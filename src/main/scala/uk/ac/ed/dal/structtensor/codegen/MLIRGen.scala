package uk.ac.ed.dal
package structtensor
package codegen

import scala.collection.mutable

import compiler.*
import scair.ir.*

import Codegen.reorder
import scair.dialects.{func, scf, arith, memref}
import scair.dialects.builtin.*
import scair.dialects.arith.AnyIntegerType
import uk.ac.ed.dal.structtensor.parser.Convertor.getAllTensors

case class MLIRGen(symbols: Seq[Variable], iters_map: Map[String, Seq[Variable]], dimMap: Map[String, Seq[Dim]], outputs: Seq[(name: String, rank: Int)]) {

    def getInputTensors(rules: Seq[Rule], defined: Seq[String] = Seq.empty): Seq[(name: String, rank: Int)] = {
        rules match
            case Nil => Seq.empty
            case h :: t => 
                val inputs = h.body.prods.flatMap(prod =>
                    prod.exps.collect {
                        case Access(name, vars, _) => (name, vars.length) 
                    })
                val head = h.head.name
                (inputs ++ getInputTensors(t, defined :+ head)).distinct
    }

    def memrefType(rank: Int) = RankedMemrefType(elementType = Float32Type(), shape = ArrayAttribute(Seq.fill(rank)(IntData(-1))))
    def genCompute(rules: Seq[Rule]): func.Func = {

        val inputTensors = getInputTensors(rules)
        val outputTensors = outputs.filterNot(inputTensors.contains)
        

        val inputTypes = symbols.map(_ => IndexType()) ++ inputTensors.map{
            case (name, rank) => memrefType(rank)
        }
        val outputTypes = outputTensors.map {
            case (name, rank) => memrefType(rank)
        }
        func.Func("stur", FunctionType(inputTypes, outputTypes), sym_visibility = None, Region(Seq(Block(
            inputTypes,
            args =>
                val (symbolArgs, tensorArgs) = args.splitAt(symbols.length)
                given values : mutable.Map[String, Value[?]] = mutable.Map.from((symbols.map(_.name) zip symbolArgs) ++ (inputTensors.map(_.name) zip tensorArgs))
                rules.flatMap(genRule(_)(using CompressedTensor)) :+ func.Return(outputTensors.map(o => values(o.name)))
        ))))
    }

    def genReconstruct(rules: Seq[Rule]): Seq[func.Func] = {
        val symbolTypes = symbols.map(_ => IndexType())
        rules.map(rule =>
            val tensorType = memrefType(rule.head.vars.length)
            val inputTypes = symbolTypes :+ tensorType
            func.Func(s"Reconstruct_${rule.head.name}", FunctionType(inputTypes, Seq()), sym_visibility = None, Region(Seq(Block(
                inputTypes,
                args =>
                    val (symbolArgs, tensorArg) = args.splitAt(symbols.length)
                    val values : mutable.Map[String, Value[?]] = mutable.Map.from((symbols.map(_.name) zip symbolArgs) ++ Seq(rule.head.name -> tensorArg.head))
                    genRule(rule)(using RedundancyMap, values) :+ func.Return(Seq())
            ))))
        )
    }

    def genRule(rule: Rule)(using kind: AccessType, values: mutable.Map[String, Value[?]]): Seq[Operation] = {
        values.get(rule.head.name) match
            // If the head is not defined, we allocate it
            case None => 
                val dims = dimMap(rule.head.name)
                val (dimValues, dimOps) = dims.map(indexGen).unzip
                val alloc = memref.Alloc(dimValues.asInstanceOf[Seq[Value[IndexType]]], Seq(), Result(memrefType(rule.head.vars.length)))
                (dimOps.flatten :+ alloc) ++ genRule(rule)(using kind, values += (rule.head.name -> alloc.memref))
            case Some(_) =>
                val computationHead = rule.head
                rule.body.prods.flatMap(genSingleProd(_, computationHead))
    }

    /** For a variable `v` and a list of conditions, extracts:
     *  - lower bounds (inclusive) as raw Index expressions,
     *  - upper bounds (exclusive) as raw Index expressions,
     *  - equality targets as raw Index expressions,
     *
     *  Strict inequalities are normalised to half-open form by wrapping
     *  the bound in `Arithmetic("+", bound, ConstantInt(1))`.
     */
    def getBoundsOnVariable(
        variable: Variable,
        conditions: Seq[Comparison]
    ): (lb: Seq[Index], ub: Seq[Index], eq: Seq[Index]) = {
        val eqs = conditions.collect {
            case Comparison("=", i, v) if v == variable => i
            case Comparison("=", i, v) if i == variable => v 
        }.distinct
        val lbs = conditions.collect {
            case Comparison("<=", i, v) if v == variable => i
            case Comparison("<", i, v) if v == variable => Arithmetic("+", i, ConstantInt(1))
            case Comparison(">=", i, v) if i == variable => v
            case Comparison(">", i, v) if i == variable => Arithmetic("+", v, ConstantInt(1)) 
        }.distinct
        val ubs = conditions.collect {
            case Comparison("<=", i, v) if i == variable => Arithmetic("+", v, ConstantInt(1))
            case Comparison("<", i, v) if i == variable => v
            case Comparison(">=", i, v) if v == variable => Arithmetic("+", i, ConstantInt(1))
            case Comparison(">", i, v) if v == variable => i
        }.distinct
        (lbs, ubs, eqs)
    }

    def genSingleProd(prod: Prod, head: Access)(using kind: AccessType, values: mutable.Map[String, Value[?]]): Seq[Operation] = {
        val conditions =
            prod.exps.collect { case condition: Comparison => condition }
        val accesses = prod.exps.collect { case access: Access => access }
        val iters = iters_map.getOrElse(head.name, Seq())
        val variablesInit =
            (iters ++ head.vars ++ accesses.flatMap(_.vars)).distinct
        val variables = reorder(variablesInit, conditions, symbols)

        given iterConds : Map[Variable, Seq[Comparison]] = conditions.groupBy { cond =>
            val condVars = Compiler.getAllVariables(cond)
            condVars.maxBy(variables.indexOf)
        }

        genSingleProdRec(prod, head, variables)
    }

    def indexGen(index: Index)(using values: mutable.Map[String, Value[?]]): (Value[AnyIntegerType], Seq[Operation]) = {
        index match
            case v: Variable => (values(v.name).asInstanceOf[Value[AnyIntegerType]], Seq())
            case ConstantInt(c) => 
                val constantOp = arith.Constant(IntegerAttr(IntData(c), IndexType()), Result(IndexType()))
                (constantOp.result.asInstanceOf[Value[AnyIntegerType]], Seq(constantOp))
            case Arithmetic(op, lhs, rhs) =>
                val (lhsVal, lhsOps) = indexGen(lhs)
                val (rhsVal, rhsOps) = indexGen(rhs)
                val arithOp = op match
                    case "+" => arith.AddI(lhsVal, rhsVal, Result(IndexType()))
                (arithOp.result, lhsOps ++ rhsOps :+ arithOp)
                
    }

    private def boundGen(lbs: Seq[Index], lower: Boolean)(using values: mutable.Map[String, Value[?]]): (value: Value[AnyIntegerType], operations: Seq[Operation]) = {
        val aggregateOp = if (lower) arith.MaxUI.apply else arith.MinUI.apply
        val (bounds, operations) = lbs.map(indexGen(_)).unzip
        val (res, aggOps) = bounds.tail.foldLeft((bounds.head, Seq.empty[Operation])) {
            case ((acc, ops), b) =>
                val res = Result(IndexType())
                val op = aggregateOp(acc, b, res)
                (res, ops :+ op)
        }
        (res, operations.flatten ++ aggOps)
    }

    def lbGen(lbs: Seq[Index])(using values: mutable.Map[String, Value[?]]): (value: Value[AnyIntegerType], operations: Seq[Operation]) = {
        boundGen(lbs, lower = true)
    }

    def ubGen(ubs: Seq[Index])(using values: mutable.Map[String, Value[?]]): (value: Value[AnyIntegerType], operations: Seq[Operation]) = {
        boundGen(ubs, lower = false)
    }

    def constantFloat(c: Double | Int): (Value[Float32Type], Operation) = {
        val d = c match
            case i: Int => i.toDouble
            case d: Double => d
        val result = Result(Float32Type())
        val constantOp = arith.Constant(FloatAttr(FloatData(d), Float32Type()), result)
        (result, constantOp)
    }
    
    def genSingleProdRec(prod: Prod, head: Access, iters: Seq[Variable])(using kind: AccessType, values: mutable.Map[String, Value[?]], iterConds: Map[Variable, Seq[Comparison]]): Seq[Operation] = {
        iters match
            case h::t =>
                val (lbs, ubs, eqs) = getBoundsOnVariable(h, iterConds.getOrElse(h, Seq()))
                eqs match
                    case Seq(to) =>
                        val (indexValue, indexOps) = indexGen(to)
                        {
                            given mutable.Map[String, Value[?]] = values += (h.name -> indexValue)
                            indexOps ++ genSingleProdRec(prod, head, t)
                        }
                    case Nil =>
                        val (lbVal, lbOps) = lbGen(lbs)
                        val (ubVal, ubOps) = ubGen(ubs)
                        val step = Result(IndexType())
                        val stepOp = arith.Constant(IntegerAttr(IntData(1), IndexType()), step)
                        val loop = scf.ForOp(lowerBound = lbVal, upperBound = ubVal, step = step, initArgs = Seq(), resultss = Seq(), region = Region(Block(IndexType(), arg =>
                            given mutable.Map[String, Value[?]] = values += (h.name -> arg)
                            genSingleProdRec(prod, head, t) :+ scf.YieldOp(Seq()))))
                        lbOps ++ ubOps :+ stepOp :+ loop
                    case _ =>
                        throw new Exception(s"Multiple equality constraints on iterator ${h.name} are not supported.")
            case Nil =>
                kind match
                    case CompressedTensor =>
                        val accesses = prod.exps.collect {
                            case Access(name, vars, _) => 
                                val result = Result(Float32Type())
                                val loadOp = memref.Load(values(name).asInstanceOf[Value[MemrefType]], vars.map(v => values(v.name).asInstanceOf[Value[IndexType]]), result)
                                (result, loadOp)
                            case ConstantInt(c) => constantFloat(c)
                            case ConstantDouble(c) => constantFloat(c)
                        }
                        val (factors, loadOps) = accesses.unzip
                        val (res, mulOps) = factors.tail.foldLeft((factors.head, Seq.empty[Operation])) {
                            case ((acc, ops), b) =>
                                val res = Result(Float32Type())
                                val mulOp = arith.MulF(acc, b, res, arith.FastMathFlagsAttr(arith.FastMathFlags.fast))
                                (res, ops :+ mulOp)
                        }
                        val headValue = values(head.name).asInstanceOf[Value[MemrefType]]
                        val headIndices = head.vars.map(v => values(v.name).asInstanceOf[Value[IndexType]])

                        val loadOp = memref.Load(headValue, headIndices, Result(Float32Type()))
                        val addOp = arith.AddF(loadOp.result.asInstanceOf[Value[Float32Type]], res, Result(Float32Type()), arith.FastMathFlagsAttr(arith.FastMathFlags.fast))
                        val storeOp = memref.Store(addOp.result, values(head.name).asInstanceOf[Value[MemrefType]], headIndices)
                        loadOps ++ mulOps :+ loadOp :+ addOp :+ storeOp
                    case RedundancyMap =>
                        val loadIndices = prod.exps.collect {
                            case a: Access => a
                        } match
                            case Seq(access) =>
                                access.vars.map(v => values(v.name).asInstanceOf[Value[IndexType]])
                            case _ =>
                                throw new Exception("Reconstruction rule should have a single rhs access.")
                        val loadOp = memref.Load(values(head.name).asInstanceOf[Value[MemrefType]], loadIndices, Result(Float32Type()))
                        val storeIndices = head.vars.map(v => values(v.name).asInstanceOf[Value[IndexType]])
                        val storeOp = memref.Store(loadOp.result, values(head.name).asInstanceOf[Value[MemrefType]], storeIndices)
                        Seq(loadOp, storeOp)

    }
}


