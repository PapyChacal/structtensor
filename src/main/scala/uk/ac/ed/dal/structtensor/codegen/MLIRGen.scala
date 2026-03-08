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
import scair.passes.cse.CSE
import scair.transformations.RewriteMethods

case class MLIRGen(symbols: Seq[Variable], iters_map: Map[String, Seq[Variable]]) {

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

    def genProgram(rules: Seq[Rule]): func.Func = {

        val inputTensors = getInputTensors(rules)
        // println(s"Input tensors: $inputTensors")
        

        val inputTypes = symbols.map(_ => IndexType()) ++ inputTensors.map{
            case (name, rank) =>
                RankedMemrefType(elementType = Float32Type(), shape = ArrayAttribute(Seq.fill(rank)(IntData(-1))))
        }
        val outputTypes = Seq()
        val f = func.Func("stur", FunctionType(inputTypes, outputTypes), sym_visibility = None, Region(Block(
            inputTypes,
            args =>
                val (symbolArgs, tensorArgs) = args.splitAt(symbols.length)
                given values : Map[String, Value[?]] = (symbols.map(_.name) zip symbolArgs).toMap ++ (inputTensors.map(_.name) zip tensorArgs).toMap
                rules.flatMap(genRule(_)(using CompressedTensor)) :+ func.Return(Seq())
        )))
        CSE()(using RewriteMethods).simplify(f.body)
        f
    }
    def genRule(rule: Rule)(using kind: AccessType, values: Map[String, Value[?]]): Seq[Operation] = {
        values.get(rule.head.name) match
            // If the head is not defined, we allocate it
            case None => 
                val alloc = memref.Alloc(Seq(), Seq(), Result(RankedMemrefType(elementType = Float32Type(), shape = ArrayAttribute(Seq.fill(rule.head.vars.length)(IntData(-1))))), alignment = IntegerAttr(IntData(0), I64))
                given Map[String, Value[?]] = values + (rule.head.name -> alloc.memref)
                alloc +: genRule(rule)
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
        }
        val lbs = conditions.collect {
            case Comparison("<=", i, v) if v == variable => i
            case Comparison("<", i, v) if v == variable => Arithmetic("+", i, ConstantInt(1))
            case Comparison(">=", i, v) if i == variable => i
            case Comparison(">", i, v) if i == variable => Arithmetic("+", i, ConstantInt(1)) 
        }
        val ubs = conditions.collect {
            case Comparison("<=", i, v) if i == variable => Arithmetic("+", i, ConstantInt(1))
            case Comparison("<", i, v) if i == variable => i
            case Comparison(">=", i, v) if v == variable => Arithmetic("+", i, ConstantInt(1))
            case Comparison(">", i, v) if v == variable => i
        }
        (lbs, ubs, eqs)
    }

    def genSingleProd(prod: Prod, head: Access)(using kind: AccessType, values: Map[String, Value[?]]): Seq[Operation] = {
        val conditions =
            prod.exps.collect { case condition: Comparison => condition }
        val accesses = prod.exps.collect { case access: Access => access }
        val iters = iters_map.getOrElse(head.name, Seq())
        val variablesInit =
            (iters ++ head.vars ++ accesses.flatMap(_.vars)).distinct
        val variables = reorder(variablesInit, conditions, symbols)

        given iterConds : Map[Variable, Seq[Comparison]] = conditions.groupBy { cond =>
            val condVars = Compiler.getAllVariables(cond)
            condVars.maxBy(v => iters.indexOf(v) match
                case -1 => 0
                case i => i
            )
        }

        genSingleProdRec(prod, head, variables)
    }

    def indexGen(index: Index)(using values: Map[String, Value[?]]): (Value[AnyIntegerType], Seq[Operation]) = {
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

    private def boundGen(lbs: Seq[Index], lower: Boolean)(using values: Map[String, Value[?]]): (value: Value[AnyIntegerType], operations: Seq[Operation]) = {
        val aggregateOp = if (lower) arith.MaxUI.apply else arith.MinUI.apply
        val (bounds, operations) = lbs.map(indexGen(_)).unzip
        bounds match
            case Seq(single) => (single, operations.flatten)
            case _ =>
                val minOp = aggregateOp(bounds(0), bounds(1), Result(IndexType()))
                val min = minOp match
                    case arith.MaxUI(_, _, res) => res
                    case arith.MinUI(_, _, res) => res
                
                bounds.drop(2).foldLeft((min, operations.flatten :+ minOp))(
                    (acc, bound) =>
                        val (min, ops) = acc
                        val newMinOp = aggregateOp(min, bound, Result(IndexType()))
                        val newMin = minOp match
                            case arith.MaxUI(_, _, res) => res
                            case arith.MinUI(_, _, res) => res
                        (newMin, ops :+ newMinOp)
                )
    }

    def lbGen(lbs: Seq[Index])(using values: Map[String, Value[?]]): (value: Value[AnyIntegerType], operations: Seq[Operation]) = {
        boundGen(lbs, lower = true)
    }

    def ubGen(ubs: Seq[Index])(using values: Map[String, Value[?]]): (value: Value[AnyIntegerType], operations: Seq[Operation]) = {
        boundGen(ubs, lower = false)
    }
    

    def genSingleProdRec(prod: Prod, head: Access, iters: Seq[Variable])(using kind: AccessType, values: Map[String, Value[?]], iterConds: Map[Variable, Seq[Comparison]]): Seq[Operation] = {
        iters match
            case h::t =>
                // println(s"Generating loop for variable $h")
                // println(s"Gathered conditions: ${iterConds.getOrElse(h, Seq())}")
                val (lbs, ubs, eqs) = getBoundsOnVariable(h, iterConds.getOrElse(h, Seq()))
                val (lbVal, lbOps) = lbGen(lbs)
                val (ubVal, ubOps) = ubGen(ubs)
                val step = Result(IndexType())
                val stepOp = arith.Constant(IntegerAttr(IntData(1), IndexType()), step)
                val loop = scf.ForOp(lowerBound = lbVal, upperBound = ubVal, step = step, initArgs = Seq(), resultss = Seq(), region = Region(Block(IndexType(), arg =>
                    given Map[String, Value[?]] = values + (h.name -> arg) 
                    genSingleProdRec(prod, head, t) :+ scf.YieldOp(Seq()))))
                lbOps ++ ubOps :+ stepOp :+ loop
            case Nil =>
                val accesses = prod.exps.collect {
                    case Access(name, vars, _) => 
                        val result = Result(Float32Type())
                        val loadOp = memref.Load(values(name).asInstanceOf[Value[MemrefType]], vars.map(v => values(v.name).asInstanceOf[Value[IndexType]]), result)
                        (result, loadOp)
                    case ConstantInt(c) =>
                        val result = Result(Float32Type())
                        val constantOp = arith.Constant(FloatAttr(FloatData(c), Float32Type()), result)
                        (result, constantOp)
                    case ConstantDouble(c) =>
                        val result = Result(Float32Type())
                        val constantOp = arith.Constant(FloatAttr(FloatData(c), Float32Type()), result)
                        (result, constantOp)
                }
                val (vals, loadOps) = accesses.unzip
                val (res, mulOps) = vals.tail.foldLeft((vals.head, Seq.empty[Operation])) {
                    case ((acc, ops), b) =>
                        val res = Result(Float32Type())
                        val mulOp = arith.MulF(acc, b, res, arith.FastMathFlagsAttr(arith.FastMathFlags.fast))
                        (res, ops :+ mulOp)
                }
                val storeOp = memref.Store(res, values(head.name).asInstanceOf[Value[MemrefType]], head.vars.map(v => values(v.name).asInstanceOf[Value[IndexType]]))
                loadOps ++ mulOps :+ storeOp

    }
}


