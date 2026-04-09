package uk.ac.ed.dal
package structtensor
package codegen

import scala.collection.mutable

import compiler.*
import scair.ir.*

import Codegen.reorder
import scair.dialects.{func, scf, arith, memref, llvm}
import scair.dialects.builtin.*
import scair.dialects.arith.AnyIntegerType
import uk.ac.ed.dal.structtensor.parser.Convertor.getAllTensors

case class MLIRGen(
    symbols: Seq[Variable],
    iters_map: Map[String, Seq[Variable]],
    dimMap: Map[String, Seq[Dim]],
    outputs: Seq[(name: String, rank: Int)],
):

  def genMain(rules: Seq[Rule]): Seq[func.Func] =
    val inputTypes = Seq(IndexType(), llvm.Ptr())
    val main = func.Func(
      "main",
      FunctionType(inputTypes, Seq(IndexType())),
      sym_visibility = None,
      Region(
        Seq(
          Block(
            inputTypes,
            args =>
              val (argc, argv) = (
                args.head.asInstanceOf[Value[IndexType]],
                args.tail.head.asInstanceOf[Value[llvm.Ptr]],
              )
              val (nSymbolsV, nSymbolsOp) = constantIndex(symbols.length + 1)
              val argcCmpV = Result(I1)
              val argcCmp = arith
                .CmpI(argc, nSymbolsV, argcCmpV, arith.CmpIPredicate.eq)
              val ifV = Result(IndexType())
              val ifOp = scf.IfOp(
                argcCmpV,
                thenRegion = Region(Block {
                  val (zeroV, zeroOp) = constantIndex(0)
                  val (symbolsV, symbolsOps) = (1 to (symbols.length)).map(i =>
                    val (offsetV, offsetOp) = constantIndex(i, I64)
                    val symbolStrPV = Result(llvm.Ptr())
                    val symbolStrPOp = llvm.GetElementPtr(
                      base = argv,
                      res = symbolStrPV,
                      rawConstantIndices = DenseArrayAttr(
                        typ = I32,
                        data = Seq(IntegerAttr(IntData(i), I32)),
                      ),
                      elem_type = llvm.Ptr(),
                    )
                    val symbolStrV = Result(llvm.Ptr())
                    val symbolStrOp = llvm.Load(symbolStrPV, symbolStrV)
                    val symbolV = Result(IndexType())
                    val symbolOp = func.Call(
                      SymbolRefAttr("atol"),
                      Seq(symbolStrV),
                      Seq(symbolV),
                    )
                    (
                      symbolV,
                      Seq(offsetOp, symbolStrPOp, symbolStrOp, symbolOp),
                    )
                  ).unzip

                  given symbolsMap: mutable.Map[String, Value[?]] = mutable.Map
                    .from(symbols.map(_.name) zip symbolsV)
                  val inputTensorsNames = getInputTensors(rules).map(_.name)
                  val (inputTensors, inputTensorsOps) = getInputTensors(rules)
                    .map { case (name, rank) =>
                      val dims = dimMap(name)
                      val (dimsV, dimsOps) = dims.map(indexGen).unzip
                      val allocV = Result(memrefType(rank))
                      val alloc = memref.Alloc(
                        dimsV.asInstanceOf[Seq[Value[IndexType]]],
                        memref = allocV,
                      )
                      (allocV, dimsOps.flatten :+ alloc)
                    }.unzip

                  val valuesMap: mutable.Map[String, Value[?]] = symbolsMap ++
                    (inputTensorsNames zip inputTensors)

                  val outputTensors = outputs
                    .filterNot(getInputTensors(rules).contains)
                  val outputTypes = outputTensors.map { case (name, rank) =>
                    memrefType(rank)
                  }
                  val results = outputTypes.map(Result.apply)
                  val call = func.Call(
                    SymbolRefAttr("compute"),
                    symbolsV ++ inputTensors,
                    results,
                  )

                  val reconstructs = outputTensors zip call.results map {
                    case ((name, rank), result) =>
                      func.Call(
                        SymbolRefAttr(s"Reconstruct_$name"),
                        symbolsV :+ result,
                      )
                  }
                  val y = scf.YieldOp(Seq(symbolsV.head))
                  (zeroOp +: (symbolsOps.flatten ++ inputTensorsOps.flatten) :+
                    call) ++ reconstructs :+ y
                }),
                elseRegion = Region(Block {
                  val (zeroV, zeroOp) = constantIndex(12)
                  val y = scf.YieldOp(Seq(zeroV))
                  Seq(zeroOp, y)
                }),
                Seq(ifV),
              )
              val ret = func.Return(Seq(ifV))
              Seq(nSymbolsOp, argcCmp, ifOp, ret),
          )
        )
      ),
    )
    Seq(
      func.Func(
        "atol",
        FunctionType(Seq(llvm.Ptr()), Seq(IndexType())),
        Some("private"),
      ),
      main,
    )

  def getInputTensors(
      rules: Seq[Rule],
      defined: Seq[String] = Seq.empty,
  ): Seq[(name: String, rank: Int)] =
    rules match
      case Nil    => Seq.empty
      case h :: t =>
        val inputs = h.body.prods.flatMap(prod =>
          prod.exps.collect { case Access(name, vars, _) =>
            (name, vars.length)
          }
        ).filterNot(t => defined.contains(t._1))
        val head = h.head.name
        (inputs ++ getInputTensors(t, defined :+ head)).distinct

  def memrefType(rank: Int) = RankedMemrefType(
    elementType = Float32Type(),
    shape = ArrayAttribute(Seq.fill(rank)(IntData(-1))),
  )

  def genCompute(rules: Seq[Rule]): func.Func =

    val inputTensors = getInputTensors(rules)
    val outputTensors = outputs.filterNot(inputTensors.contains)

    val inputTypes = symbols.map(_ => IndexType()) ++ inputTensors.map {
      case (name, rank) => memrefType(rank)
    }
    val outputTypes = outputTensors.map { case (name, rank) =>
      memrefType(rank)
    }
    func.Func(
      "compute",
      FunctionType(inputTypes, outputTypes),
      sym_visibility = Some("private"),
      Region(
        Seq(
          Block(
            inputTypes,
            args =>
              val (symbolArgs, tensorArgs) = args.splitAt(symbols.length)
              given values: mutable.Map[String, Value[?]] = mutable.Map
                .from(
                  (symbols.map(_.name) zip symbolArgs) ++
                    (inputTensors.map(_.name) zip tensorArgs)
                )
              rules.flatMap(genRule(_)(using CompressedTensor)) :+
                func.Return(outputTensors.map(o => values(o.name))),
          )
        )
      ),
    )

  def genReconstruct(rules: Seq[Rule]): Seq[func.Func] =
    val symbolTypes = symbols.map(_ => IndexType())
    rules.map(rule =>
      val tensorType = memrefType(rule.head.vars.length)
      val inputTypes = symbolTypes :+ tensorType
      func.Func(
        s"Reconstruct_${rule.head.name}",
        FunctionType(inputTypes),
        sym_visibility = Some("private"),
        Region(
          Seq(
            Block(
              inputTypes,
              args =>
                val (symbolArgs, tensorArg) = args.splitAt(symbols.length)
                val values: mutable.Map[String, Value[?]] = mutable.Map
                  .from(
                    (symbols.map(_.name) zip symbolArgs) ++
                      Seq(rule.head.name -> tensorArg.head)
                  )
                genRule(rule)(using RedundancyMap, values) :+ func.Return(),
            )
          )
        ),
      )
    )

  def genRule(rule: Rule)(using
      kind: AccessType,
      values: mutable.Map[String, Value[?]],
  ): Seq[Operation] =
    values.get(rule.head.name) match
      // If the head is not defined, we allocate it
      case None =>
        val dims = dimMap(rule.head.name)
        val (dimValues, dimOps) = dims.map(indexGen).unzip
        val alloc = memref.Alloc(
          dynamicSizes = dimValues.asInstanceOf[Seq[Value[IndexType]]],
          memref = Result(memrefType(rule.head.vars.length)),
        )
        (dimOps.flatten :+ alloc) ++
          genRule(rule)(using kind, values += (rule.head.name -> alloc.memref))
      case Some(_) =>
        val computationHead = rule.head
        rule.body.prods.flatMap(genSingleProd(_, computationHead))

  def getBoundsOnVariable(
      variable: Variable,
      conditions: Seq[Comparison],
  ): (lb: Seq[Index], ub: Seq[Index], eq: Seq[Index]) =
    val eqs = conditions.collect {
      case Comparison("=", i, v) if v == variable => i
      case Comparison("=", i, v) if i == variable => v
    }.distinct
    val lbs = conditions.collect {
      case Comparison("<=", i, v) if v == variable => i
      case Comparison("<", i, v) if v == variable  =>
        Arithmetic("+", i, ConstantInt(1))
      case Comparison(">=", i, v) if i == variable => v
      case Comparison(">", i, v) if i == variable  =>
        Arithmetic("+", v, ConstantInt(1))
    }.distinct
    val ubs = conditions.collect {
      case Comparison("<=", i, v) if i == variable =>
        Arithmetic("+", v, ConstantInt(1))
      case Comparison("<", i, v) if i == variable  => v
      case Comparison(">=", i, v) if v == variable =>
        Arithmetic("+", i, ConstantInt(1))
      case Comparison(">", i, v) if v == variable => i
    }.distinct
    (lbs, ubs, eqs)

  def genSingleProd(prod: Prod, head: Access)(using
      kind: AccessType,
      values: mutable.Map[String, Value[?]],
  ): Seq[Operation] =
    val conditions =
      prod.exps.collect { case condition: Comparison => condition }
    val accesses = prod.exps.collect { case access: Access => access }
    val iters = iters_map.getOrElse(head.name, Seq())
    val variablesInit = (iters ++ head.vars ++ accesses.flatMap(_.vars))
      .distinct
    val variables = reorder(variablesInit, conditions, symbols)

    given iterConds: Map[Variable, Seq[Comparison]] = conditions.groupBy {
      cond =>
        val condVars = Compiler.getAllVariables(cond)
        condVars.maxBy(variables.indexOf)
    }

    genSingleProdRec(prod, head, variables)

  def indexGen(index: Index)(using
      values: mutable.Map[String, Value[?]]
  ): (Value[AnyIntegerType], Seq[Operation]) =
    index match
      case v: Variable =>
        (values(v.name).asInstanceOf[Value[AnyIntegerType]], Seq())
      case ConstantInt(c) =>
        val (v, o) = constantIndex(c)
        (v, Seq(o))
      case Arithmetic(op, lhs, rhs) =>
        val (lhsVal, lhsOps) = indexGen(lhs)
        val (rhsVal, rhsOps) = indexGen(rhs)
        val arithOp = op match
          case "+" => arith.AddI(lhsVal, rhsVal, Result(IndexType()))
        (arithOp.result, lhsOps ++ rhsOps :+ arithOp)

  private def boundGen(lbs: Seq[Index], lower: Boolean)(using
      values: mutable.Map[String, Value[?]]
  ): (value: Value[AnyIntegerType], operations: Seq[Operation]) =
    val aggregateOp = if lower then arith.MaxUI.apply else arith.MinUI.apply
    val (bounds, operations) = lbs.map(indexGen(_)).unzip
    val (res, aggOps) = bounds.tail
      .foldLeft((bounds.head, Seq.empty[Operation])) { case ((acc, ops), b) =>
        val res = Result(IndexType())
        val op = aggregateOp(acc, b, res)
        (res, ops :+ op)
      }
    (res, operations.flatten ++ aggOps)

  def lbGen(lbs: Seq[Index])(using
      values: mutable.Map[String, Value[?]]
  ): (value: Value[AnyIntegerType], operations: Seq[Operation]) =
    boundGen(lbs, lower = true)

  def ubGen(ubs: Seq[Index])(using
      values: mutable.Map[String, Value[?]]
  ): (value: Value[AnyIntegerType], operations: Seq[Operation]) =
    boundGen(ubs, lower = false)

  def constantFloat(c: Double | Int): (Value[Float32Type], Operation) =
    val d = c match
      case i: Int    => i.toDouble
      case d: Double => d
    val result = Result(Float32Type())
    val constantOp = arith
      .Constant(FloatAttr(FloatData(d), Float32Type()), result)
    (result, constantOp)

  def constantIndex[T <: AnyIntegerType](
      c: Int,
      t: T = IndexType(),
  ): (Value[T], Operation) =
    val result = Result(t)
    val constantOp = arith.Constant(IntegerAttr(IntData(c), t), result)
    (result, constantOp)

  def genSingleProdRec(prod: Prod, head: Access, iters: Seq[Variable])(using
      kind: AccessType,
      values: mutable.Map[String, Value[?]],
      iterConds: Map[Variable, Seq[Comparison]],
  ): Seq[Operation] =
    iters match
      case h :: t =>
        val (lbs, ubs, eqs) =
          getBoundsOnVariable(h, iterConds.getOrElse(h, Seq()))
        eqs match
          case Seq(to) =>
            val (indexValue, indexOps) = indexGen(to)
            {
              given mutable.Map[String, Value[?]] = values +=
                (h.name -> indexValue)
              indexOps ++ genSingleProdRec(prod, head, t)
            }
          case Nil =>
            val (lbVal, lbOps) = lbGen(lbs)
            val (ubVal, ubOps) = ubGen(ubs)
            val (step, stepOp) = constantIndex(1)
            val loop = scf.ForOp(
              lowerBound = lbVal,
              upperBound = ubVal,
              step = step,
              region = Region(
                Block(
                  IndexType(),
                  arg =>
                    given mutable.Map[String, Value[?]] = values +=
                      (h.name -> arg)
                    genSingleProdRec(prod, head, t) :+ scf.YieldOp(),
                )
              ),
            )
            lbOps ++ ubOps :+ stepOp :+ loop
          case _ =>
            throw new Exception(
              s"Multiple equality constraints on iterator ${h.name} are not supported."
            )
      case Nil =>
        kind match
          case CompressedTensor =>
            val accesses = prod.exps.collect {
              case Access(name, vars, _) =>
                val result = Result(Float32Type())
                val loadOp = memref.Load(
                  values(name).asInstanceOf[Value[MemrefType]],
                  vars.map(v => values(v.name).asInstanceOf[Value[IndexType]]),
                  result,
                )
                (result, loadOp)
              case ConstantInt(c)    => constantFloat(c)
              case ConstantDouble(c) => constantFloat(c)
            }
            val (factors, loadOps) = accesses.unzip
            val (res, mulOps) = factors.tail
              .foldLeft((factors.head, Seq.empty[Operation])) {
                case ((acc, ops), b) =>
                  val res = Result(Float32Type())
                  val mulOp = arith.MulF(
                    acc,
                    b,
                    res,
                    arith.FastMathFlagsAttr(arith.FastMathFlags.fast),
                  )
                  (res, ops :+ mulOp)
              }
            val headValue = values(head.name).asInstanceOf[Value[MemrefType]]
            val headIndices = head.vars
              .map(v => values(v.name).asInstanceOf[Value[IndexType]])

            val loadOp = memref
              .Load(headValue, headIndices, Result(Float32Type()))
            val addOp = arith.AddF(
              loadOp.result.asInstanceOf[Value[Float32Type]],
              res,
              Result(Float32Type()),
              arith.FastMathFlagsAttr(arith.FastMathFlags.fast),
            )
            val storeOp = memref.Store(
              addOp.result,
              values(head.name).asInstanceOf[Value[MemrefType]],
              headIndices,
            )
            loadOps ++ mulOps :+ loadOp :+ addOp :+ storeOp
          case RedundancyMap =>
            val loadIndices = prod.exps.collect { case a: Access => a } match
              case Seq(access) =>
                access.vars
                  .map(v => values(v.name).asInstanceOf[Value[IndexType]])
              case _ =>
                throw new Exception(
                  "Reconstruction rule should have a single rhs access."
                )
            val loadOp = memref.Load(
              values(head.name).asInstanceOf[Value[MemrefType]],
              loadIndices,
              Result(Float32Type()),
            )
            val storeIndices = head.vars
              .map(v => values(v.name).asInstanceOf[Value[IndexType]])
            val storeOp = memref.Store(
              loadOp.result,
              values(head.name).asInstanceOf[Value[MemrefType]],
              storeIndices,
            )
            Seq(loadOp, storeOp)
