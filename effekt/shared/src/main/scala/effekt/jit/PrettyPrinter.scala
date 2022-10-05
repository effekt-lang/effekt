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
      "datatypes" -> jsonList(program.datatypes.map{ s => "\"%s\"".format(s) }),
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
    case Type.Double() => Some(jsonObjectSmall(ListMap("type" -> "\"double\"")))
    case Type.Codata(index) => Some(jsonObjectSmall(ListMap("type" -> "\"codata\"", "index" -> index.toString)))
    case Type.Datatype(index) => Some(jsonObjectSmall(ListMap("type" -> "\"adt\"", "index" -> index.toString)))
  }

  extension(rtpe: RegisterType) {
    def name: String = rtpe match {
      case RegisterType.Integer => "int"
      case RegisterType.Continuation => "cont"
      case RegisterType.Double => "double"
      case RegisterType.Datatype => "adt"
      case RegisterType.Codata => "codata"
      case _ => sys error "Cannot print erased register type"
    }
  }
  def toDoc(rtpe: RegisterType): String = "\"%s\"".format(rtpe.name)

  def toDoc(block: BasicBlock): Doc = block match {
    case BasicBlock(id, frameDescriptor, instructions, terminator) => {
      jsonObject(ListMap(
        "label" -> dquotes(id),
        "frameDescriptor" -> toDoc(frameDescriptor),
        "instructions" -> jsonList(instructions.map(toDoc) ++ List(toDoc(terminator)))
      ))
    }
  }

  def toDoc(frameDescriptor: FrameDescriptor): Doc = {
    jsonObjectSmall(RegisterType.valuesNonErased.map(ty => ("regs_%s".format(ty.name)) ->
      (frameDescriptor.locals.getOrElse(ty, 0).toString: Doc)
    ).toMap)
  }

  def toDoc(instruction: Instruction): Doc = instruction match {
    case Const(out, value) => jsonObjectSmall(ListMap("op" -> "\"Const\"",
      "type" -> toDoc(RegisterType.Integer), "out" -> toDoc(out), "value" -> value.toString))
    case ConstDouble(out, value) => jsonObjectSmall(ListMap("op" -> "\"Const\"",
      "type" -> toDoc(RegisterType.Double), "out" -> toDoc(out), "value" -> value.toString))
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
    case Copy(tpe, from, to) => jsonObjectSmall(ListMap("op" -> "\"Copy\"",
      "type" -> toDoc(tpe),
      "from" -> toDoc(from), "to" -> toDoc(to)
    ))
    case Drop(tpe, reg) => jsonObjectSmall(ListMap("op" -> "\"Drop\"",
      "type" -> toDoc(tpe), "reg" -> toDoc(reg)
    ))
    case Swap(tpe, a, b) => jsonObjectSmall(ListMap("op" -> "\"Swap\"",
      "type" -> toDoc(tpe), "a" -> toDoc(a), "b" -> toDoc(b)
    ))
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
    case New(out, targets, args) => jsonObjectSmall(ListMap("op" -> "\"New\"",
      "out" -> toDoc(out),
      "targets" -> jsonListSmall(targets.map(toDoc)),
      "args" -> toDoc(args)
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
    case Invoke(receiver, tag, args) => jsonObjectSmall(ListMap("op" -> "\"Invoke\"",
      "receiver" -> toDoc(receiver),
      "tag" -> tag.toString,
      "args" -> toDoc(args)
    ))
  }

  def toDoc(clause: Clause): Doc = clause match
    case Clause(args, target) => {
      jsonObjectSmall(ListMap("target" -> toDoc(target), "args" -> toDoc(args)))
    }

  def toDoc(args: RegList): Doc = args match
    case RegList(args) => {
      jsonObjectSmall(RegisterType.valuesNonErased.map(ty => ty.name ->
        jsonListSmall(args.getOrElse(ty, List()).map(toDoc))
      ).toMap)
    }

  def toDoc(reg: Register): Doc = reg match {
    case RegisterIndex(index) => index.toString
    case _ => throw Error("Internal error: Unexpected non-index register")
  }

  def toDoc(lbl: BlockLabel): Doc = lbl match {
    case BlockIndex(index) => index.toString
    case _ => throw Error("Internal error: Unexpected non-index block")
  }
}
