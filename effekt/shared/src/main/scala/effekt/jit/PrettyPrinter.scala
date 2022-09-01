package effekt
package jit

import kiama.output.ParenPrettyPrinter
import kiama.output.PrettyPrinterTypes.Document
import scala.collection.immutable.ListMap

object PrettyPrinter extends ParenPrettyPrinter {
  def toDocument(program: Program): Document = pretty(toDoc(program))

  def jsonObject(fields: Map[String, Doc]): Doc = {
    braces(
      vsep(fields.map({case (k, v) => dquotes(k) <+> ":" <+> v}).toSeq, ","))
  }
  def jsonList(elems: List[Doc]): Doc = {
    brackets(
      vsep(elems, ","))
  }
  def jsonObjectSmall(fields: Map[String, Doc]): Doc = {
    braces(hsep(fields.map({case (k,v)=> dquotes(k) <+> ":" <+> v}).toSeq, ","))
  }
  def jsonListSmall(elems: List[Doc]): Doc = {
    brackets(hsep(elems, ","))
  }

  def toDoc(program: Program): Doc = {
    jsonObject(ListMap(
      "datatypes" -> jsonList(program.datatypes.map(toDoc)),
      "blocks" -> jsonList(program.blocks.map(toDoc)),
      "frameSize" -> toDoc(program.frameSize),
    ))
  }
  def toDoc(datatype: List[List[Type]]): Doc =
    jsonList(datatype.map(pars => jsonListSmall(pars.flatMap(tpe => toDoc(tpe).toList))))

  def toDoc(tpe: Type): Option[Doc] = tpe match {
    case Type.Unit() => None
    case Type.Continuation() => Some(jsonObjectSmall(ListMap("type" -> "\"cont\"")))
    case Type.Integer() => Some(jsonObjectSmall(ListMap("type" -> "\"int\"")))
    case Type.Datatype(index) => Some(jsonObjectSmall(ListMap("type" -> "\"adt\"", "index" -> index.toString)))
  }

  def toDoc(block: BasicBlock): Doc = block match {
    case BasicBlock(BlockIndex(idx), frameDescriptor, instructions, terminator) => {
      jsonObject(ListMap(
        "label" -> idx.toString,
        "frameDescriptor" -> toDoc(frameDescriptor),
        "instructions" -> jsonList(instructions.map(toDoc) ++ List(toDoc(terminator)))
      ))
    }
    case BasicBlock(id, frameDescriptor, instructions, terminator) => {
      jsonObject(ListMap(
        "label" -> dquotes(id.toString),
        "frameDescriptor" -> toDoc(frameDescriptor),
        "instructions" -> jsonList(instructions.map(toDoc) ++ List(toDoc(terminator)))
      ))
    } // TODO: throw Error("Internal error: Unnumbered BasicBlock")
  }

  def toDoc(frameDescriptor: FrameDescriptor): Doc = {
    jsonObjectSmall(ListMap( // TODO: calculate befor pretty-printing
      "regs_int" -> frameDescriptor.locals.applyOrElse(RegisterType.Integer, x => 0).toString,
      "regs_cont" -> frameDescriptor.locals.applyOrElse(RegisterType.Continuation, x => 0).toString,
      "regs_adt" -> frameDescriptor.locals.applyOrElse(RegisterType.Datatype, x => 0).toString
    ))
  }

  def toDoc(instruction: Instruction): Doc = instruction match {
    case Const(out, value) => jsonObjectSmall(ListMap("op" -> "\"Const\"", "out" -> toDoc(out), "value" -> value.toString))
    case PrimOp(name, out, in) => jsonObjectSmall(ListMap("op" -> "\"PrimOp\"",
      "name" -> dquotes(name),
      "out" -> toDoc(out),
      "in" -> toDoc(in)))
    case Add(out, in1, in2) => jsonObjectSmall(ListMap("op" -> "\"Add\"",
      "out" -> toDoc(out), "in1" -> toDoc(in1), "in2" -> toDoc(in2)))
    case Mul(out, in1, in2) => jsonObjectSmall(ListMap("op" -> "\"Mul\"",
      "out" -> toDoc(out), "in1" -> toDoc(in1), "in2" -> toDoc(in2)))
    case Push(target, args) => jsonObjectSmall(ListMap("op" -> "\"Push\"",
      "target" -> toDoc(target), "args" -> toDoc(args)))
    case Shift(out, n) => jsonObjectSmall(ListMap("op" -> "\"Shift\"",
      "out" -> toDoc(out), "n" -> n.toString))
    case Reset() => jsonObjectSmall(ListMap("op" -> "\"Reset\""))
    case Print(arg) => jsonObjectSmall(ListMap("op" -> "\"Print\"", "arg" -> toDoc(arg)))
    case IfZero(arg, thenClause) => jsonObjectSmall(ListMap("op" -> "\"IfZero\"",
      "cond" -> toDoc(arg),
      "then" -> toDoc(thenClause)))
    case IsZero(out, arg) => jsonObjectSmall(ListMap("op" -> "\"IsZero\"", "out" -> toDoc(out), "arg" -> toDoc(arg)))
    case Subst(args) => jsonObjectSmall(ListMap("op" -> "\"Subst\"", "args" -> toDoc(args)))
    case Construct(out, adt_type, tag, args) => jsonObjectSmall(ListMap("op" -> "\"Construct\"",
      "out" -> toDoc(out),
      "type" -> adt_type.toString,
      "tag" -> tag.toString,
      "args" -> toDoc(args)
    ))
    case NewStack(out, target, args) => jsonObjectSmall(ListMap("op" -> "\"NewStack\"",
      "out" -> toDoc(out),
      "target" -> toDoc(target),
      "args" -> toDoc(args)
    ))
    case PushStack(arg) => jsonObjectSmall(ListMap("op" -> "\"PushStack\"",
      "arg" -> toDoc(arg)
    ))
  }

  def toDoc(terminator: Terminator): Doc = terminator match {
    case Return(args) => jsonObjectSmall(ListMap("op" -> "\"Return\"", "args" -> toDoc(args)))
    case Jump(target) => jsonObjectSmall(ListMap("op" -> "\"Jump\"", "target" -> toDoc(target)))
    case Resume(cont) => jsonObjectSmall(ListMap("op" -> "\"Resume\"", "cont" -> toDoc(cont)))
    case Match(adt_type, scrutinee, clauses) => jsonObjectSmall(ListMap("op" -> "\"Match\"",
      "type" -> adt_type.toString,
      "scrutinee" -> toDoc(scrutinee),
      "clauses" -> jsonListSmall(clauses.map(toDoc))))
  }

  def toDoc(clause: Clause): Doc = clause match
    case Clause(args, target) => {
      jsonObjectSmall(ListMap("target" -> toDoc(target), "args" -> toDoc(args)))
    }

  def toDoc(args: RegList): Doc = args match
    case RegList(args) => {
      jsonObjectSmall(ListMap(
        "int" -> jsonListSmall(args.applyOrElse(RegisterType.Integer, x => List()).map(toDoc)),
        "cont" -> jsonListSmall(args.applyOrElse(RegisterType.Continuation, x => List()).map(toDoc)),
        "adt" -> jsonListSmall(args.applyOrElse(RegisterType.Datatype, x => List()).map(toDoc))
      ))
    }

  def toDoc(reg: Register): Doc = reg match {
    case RegisterIndex(index) => index.toString
    case NamedRegister(name) => dquotes(name.toString) // TODO: throw Error("Internal error: Unexpected named register")
  }

  def toDoc(lbl: BlockLabel): Doc = lbl match {
    case BlockIndex(index) => index.toString
    case BlockName(name) => dquotes(name.toString) // TODO: throw Error("Internal error: Unexpected named block")
  }
}