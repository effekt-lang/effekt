package effekt
package generator
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

  def escape(s: String): String = s.flatMap(escapeChar)

  def escapeChar(ch: Char): String = ch match {
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case '"' => "\\\""
    //case '\'' => "\\\'"
    case '\\' => "\\\\"
    case _ => if (ch.isControl) "\\0" + Integer.toOctalString(ch.toInt)
    else String.valueOf(ch)
  }

  def toDoc(s: String): Doc = string(s"\"${escape(s)}\"")


  def toDoc(id: Id): Doc = id match {
    case id: core.Id => toDoc(id.show)
    case str: String => toDoc(str)
  }

  def toDoc(m: Method): Doc = m match {
    case Method(tag, params, ret) => jsonObjectSmall(ListMap(
      "tag" -> toDoc(tag),
      "params" -> jsonListSmall(params map toDoc),
      "return" -> toDoc(ret)
    ))
  }

  def toDoc(c: Constructor): Doc = c match {
    case Constructor(tag, fields) => jsonObjectSmall(ListMap(
      "tag" -> toDoc(tag),
      "fields" -> jsonListSmall(fields map toDoc)
    ))
  }

  def objName(x: Any) = x.getClass.getSimpleName.replace("$", "")

  def toDoc(t: Type): Doc = jsonObjectSmall(t match {
    case Top | Ptr | Num | Bottom => ListMap("op" -> s"\"${objName(t)}\"")
    case base: Base => ListMap("op" -> s"\"${objName(base)}\"")
    case Function(params, ret, purity) =>
      ListMap("op" -> "\"Function\"",
        "params" -> jsonList(params map toDoc),
        "return" -> toDoc(ret),
        "purity" -> s"\"${purity.toString}\"")
    case Codata(ifce_tag, methods) =>
      ListMap("op" -> "\"Codata\"",
        "interface_tag" -> toDoc(ifce_tag),
        "methods" -> jsonListSmall(methods map toDoc))
    case Data(type_tag, constructors) =>
      ListMap("op" -> "\"Data\"",
        "type_tag" -> toDoc(type_tag),
        "constructors" -> jsonListSmall(constructors map toDoc))
    case Stack(resume_ret, resume_args) =>
      ListMap("op" -> "\"Stack\"",
        "resume_return" -> toDoc(resume_ret),
        "resume_args" -> jsonListSmall(resume_args map toDoc))
    case Ref(to) => ListMap("op" -> "\"Ref\"", "to" -> toDoc(to))
  })

  def toDoc(l: LhsOperand): Doc = l match {
    case Var(name, tpe) => jsonObjectSmall(ListMap("id" -> toDoc(name), "type" -> toDoc(tpe)))
  }

  def toDoc(t: Term): Doc = t match {
    case Var(name, tpe) => jsonObjectSmall(ListMap(
      "op" -> "\"Var\"",
      "id" -> toDoc(name),
      "type" -> toDoc(tpe)))
    case Abs(params, body) => jsonObject(ListMap(
      "op" -> "\"Abs\"",
      "params" -> jsonListSmall(params map toDoc),
      "body" -> toDoc(body)))
    case App(fn, args) => jsonObjectSmall(ListMap(
      "op" -> "\"App\"",
      "fn" -> toDoc(fn),
      "args" -> jsonListSmall(args map toDoc)
    ))
    case Seq(ts) => jsonObject(ListMap(
      "op" -> "\"Seq\"",
      "elems" -> jsonList(ts map toDoc)
    ))
    case Let(defs, body) => jsonObject(ListMap(
      "op" -> "\"Let\"",
      "definitions" -> jsonList(defs map toDoc),
      "body" -> toDoc(body)
    ))
    case LetRec(defs, body) => jsonObject(ListMap(
      "op" -> "\"LetRec\"",
      "definitions" -> jsonList(defs map toDoc),
      "body" -> toDoc(body)
    ))
    case IfZero(cond, thn, els) => jsonObject(ListMap(
      "op" -> "\"IfZero\"",
      "cond" -> toDoc(cond),
      "then" -> toDoc(thn),
      "else" -> toDoc(els)
    ))
    case Construct(tpe_tag, tag, args) => jsonObjectSmall(ListMap(
      "op" -> "\"Construct\"",
      "type_tag" -> toDoc(tpe_tag),
      "tag" -> toDoc(tag),
      "args" -> jsonListSmall(args map toDoc)
    ))
    case Project(scrutinee, tpe_tag, tag, field) => jsonObjectSmall(ListMap(
      "op" -> "\"Project\"",
      "scrutinee" -> toDoc(scrutinee),
      "type_tag" -> toDoc(tpe_tag),
      "tag" -> toDoc(tag),
      "field" -> s"${field}"
    ))
    case Match(scrutinee, tpe_tag, clauses, Clause(dparams, dbody)) => jsonObject(ListMap(
      "op" -> "\"Match\"",
      "scrutinee" -> toDoc(scrutinee),
      "type_tag" -> toDoc(tpe_tag),
      "clauses" -> jsonList(clauses map { case (t, Clause(params, body)) =>
        jsonObject(ListMap("tag" -> toDoc(t),
          "params" -> jsonListSmall(params map toDoc),
          "body" -> toDoc(body)))
      }),
      "default_clause" -> jsonObject(ListMap("params" -> jsonListSmall(dparams map toDoc), "body" -> toDoc(dbody)))
    ))
    case New(ifce_tag, methods) => jsonObject(ListMap("op" -> "\"New\"",
      "ifce_tag" -> toDoc(ifce_tag),
      "methods" -> jsonList(methods map { case (t, Clause(params, body)) =>
        jsonObject(ListMap("tag" -> toDoc(t),
          "params" -> jsonListSmall(params map toDoc),
          "body" -> toDoc(body)))
      })
    ))
    case Invoke(receiver, ifce_tag, method, args) => jsonObjectSmall(ListMap(
      "op" -> "\"Invoke\"",
      "receiver" -> toDoc(receiver),
      "ifce_tag" -> toDoc(ifce_tag),
      "tag" -> toDoc(method),
      "args" -> jsonListSmall(args map toDoc)
    ))
    case LetRef(ref, region, binding, body) => jsonObject(ListMap(
      "op" -> "\"LetRef\"",
      "ref" -> toDoc(ref),
      "region" -> toDoc(region),
      "binding" -> toDoc(binding),
      "body" -> toDoc(body)
    ))
    case Load(ref) => jsonObjectSmall(ListMap("op" -> "\"Load\"", "ref" -> toDoc(ref)))
    case Store(ref, value) => jsonObjectSmall(ListMap("op" -> "\"Store\"", "ref" -> toDoc(ref), "value" -> toDoc(value)))
    case FreshLabel() => jsonObjectSmall(ListMap("op" -> "\"FreshLabel\""))
    case Reset(label, region, body, Clause(rparams, rbody)) => jsonObject(ListMap("op" -> "\"Reset\"",
      "label" -> toDoc(label),
      "region" -> toDoc(region),
      "body" -> toDoc(body),
      "return" -> jsonObject(ListMap("params" -> jsonListSmall(rparams map toDoc), "body" -> toDoc(rbody)))))
    case Shift(label, n, k, body, tpe) => jsonObject(ListMap("op" -> "\"Shift\"",
      "label" -> toDoc(label),
      "n" -> toDoc(n),
      "k" -> toDoc(k),
      "returnType" -> toDoc(tpe),
      "body" -> toDoc(body)))
    case Control(label, n, k, body, tpe) => jsonObject(ListMap("op" -> "\"Control\"",
      "label" -> toDoc(label),
      "n" -> toDoc(n),
      "k" -> toDoc(k),
      "returnType" -> toDoc(tpe),
      "body" -> toDoc(body)))
    case Resume(cont, args) => jsonObjectSmall(ListMap("op" -> "\"Resume\"", "k" -> toDoc(cont), "args" -> jsonListSmall(args map toDoc)))
    case Resumed(cont, body) => jsonObjectSmall(ListMap("op" -> "\"Resumed\"", "k" -> toDoc(cont), "body" -> toDoc(body)))
    case Primitive(name, args, returns, rest) => jsonObject(ListMap("op" -> "\"Primitive\"",
      "name" -> toDoc(name),
      "args" -> jsonListSmall(args map { a => toDoc(a) }),
      "returns" -> jsonListSmall(returns map toDoc),
      "rest" -> toDoc(rest)
    ))
    case literal: Literal => jsonObjectSmall(ListMap("op" -> string("\"Literal\""), "type" -> toDoc(literal.tpe)) + (literal match {
      case Literal.Int(value) => "value" -> string(s"${value}")
      case Literal.Double(value) => "value" -> string(s"${value}")
      case Literal.String(value) => "value" -> toDoc(value)
      case Literal.NullLabel => "value" -> string("\"null\"") // will be ignored by parser
      case Literal.Unit => "value" -> string("\"unit\"") // will be ignored by parser
    }))
    case Handle(tpe, tag, handlers, ret, body) => jsonObject(ListMap("op" -> string("\"Handle\""),
      "tpe" -> string(s"\"${tpe.productPrefix}\""),
      "tag" -> toDoc(tag),
      "handlers" -> jsonList(handlers map { case (t, v) => jsonObjectSmall(ListMap(
        "tag" -> toDoc(t), "params" -> jsonListSmall(v.params map toDoc), "body" -> toDoc(v.body)))
      }),
      "body" -> toDoc(body)
    ) ++ ret.map { r => "ret" -> toDoc(r) }.toList)
    case Op(tag, op, args, k, rtpe) => jsonObject(ListMap("op" -> "\"Op\"",
      "tag" -> toDoc(tag),
      "op_tag" -> toDoc(op),
      "args" -> jsonListSmall(args map toDoc),
      "k" -> toDoc(k),
      "rtpe" -> toDoc(rtpe)
    ))
    case DHandle(tpe, tag, handlers, ret, body) => jsonObject(ListMap("op" -> string("\"DHandle\""),
      "tpe" -> string(s"\"${tpe.productPrefix}\""),
      "tag" -> toDoc(tag),
      "handlers" -> jsonList(handlers map { case (t, v) => jsonObjectSmall(ListMap(
        "tag" -> toDoc(t), "params" -> jsonListSmall(v.params map toDoc), "body" -> toDoc(v.body)))
      }),
      "body" -> toDoc(body)
    ) ++ ret.map { r => "ret" -> toDoc(r) }.toList)
    case DOp(tag, op, args, k, rtpe) => jsonObject(ListMap("op" -> "\"DOp\"",
      "tag" -> toDoc(tag),
      "op_tag" -> toDoc(op),
      "args" -> jsonListSmall(args map toDoc),
      "k" -> toDoc(k),
      "rtpe" -> toDoc(rtpe)
    ))
    case The(tpe, body) => jsonObject(ListMap("op" -> "\"The\"", "type" -> toDoc(tpe), "term" -> toDoc(body)))
  }

  def toDoc(c: Clause): Doc = c match {
    case Clause(params, body) =>
      jsonObject(ListMap(
        "params" -> jsonListSmall(params map toDoc),
        "body" -> toDoc(body)))
  }

  def toDoc(d: Definition): Doc = d match {
    case Definition(name, binding) => jsonObject(ListMap(
      "name" -> toDoc(name), "value" -> toDoc(binding),
      "export_as" -> jsonListSmall(List(toDoc(name.name.toString)))
    ))
  }

  def toDoc(p: Program): Doc = p match {
    case Program(definitions, main) =>
      jsonObject(ListMap("definitions" -> jsonList(definitions map toDoc), "main" -> toDoc(main)))
  }

  def format(prog: Program): Document = {
    pretty(toDoc(prog))
  }
}
