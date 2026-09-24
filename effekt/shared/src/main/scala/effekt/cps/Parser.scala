package effekt
package cps

import effekt.core.{ Id, Names, Type, ValueType }
import effekt.source.Span
import effekt.util.UByte
import effekt.util.messages.{ ErrorReporter, ParseError }
import kiama.parsing.{ NoSuccess, ParseResult, Parsers, Success }
import kiama.util.{ Range, Severities, Source, StringSource }

class Parser(names: Names) extends Parsers {

  type P[T] = PackratParser[T]

  // === Lexing ===

  lazy val nameFirst = """[a-zA-Z_]""".r
  lazy val nameRest = """[a-zA-Z0-9_$]""".r
  lazy val nameBoundary = """(?!%s)""".format(nameRest).r
  lazy val name = "%s(%s)*%s".format(nameFirst, nameRest, nameBoundary).r

  lazy val ident = not(anyKeyword) ~> name | failure("Expected an identifier")

  lazy val `=` = literal("=")
  lazy val `@` = literal("@")
  lazy val `{` = literal("{")
  lazy val `}` = literal("}")
  lazy val `(` = literal("(")
  lazy val `)` = literal(")")
  lazy val `[` = literal("[")
  lazy val `]` = literal("]")
  lazy val `,` = literal(",")
  lazy val `.` = literal(".")
  lazy val `=>` = literal("=>")
  lazy val `!` = literal("!")
  lazy val `|` = literal("|")
  lazy val `;` = literal(";")
  lazy val `:` = literal(":")
  lazy val `<>` = literal("<>")

  lazy val `type` = keyword("type")
  lazy val `interface` = keyword("interface")
  lazy val `def` = keyword("def")
  lazy val `let` = keyword("let")
  lazy val `new` = keyword("new")
  lazy val `run` = keyword("run")
  lazy val `if` = keyword("if")
  lazy val `else` = keyword("else")
  lazy val `match` = keyword("match")
  lazy val `case` = keyword("case")
  lazy val `var` = keyword("var")
  lazy val `dealloc` = keyword("dealloc")
  lazy val `region` = keyword("region")
  lazy val `alloc` = keyword("alloc")
  lazy val `in` = keyword("in")
  lazy val `reset` = keyword("reset")
  lazy val `shift` = keyword("shift")
  lazy val `resume` = keyword("resume")
  lazy val `make` = keyword("make")
  lazy val `get` = keyword("get")
  lazy val `put` = keyword("put")
  lazy val `true` = keyword("true")
  lazy val `false` = keyword("false")
  lazy val `abort` = keyword("abort")
  lazy val `return` = keyword("return")
  lazy val `toplevel` = keyword("toplevel")

  lazy val `run!` : P[String] = regex("run!(?![a-zA-Z0-9_!?$])".r, "run!")
  lazy val `run~` : P[String] = regex("run~(?![a-zA-Z0-9_!?$])".r, "run~")

  def keywordStrings: List[String] = List(
    "type", "interface", "def", "let", "new", "run", "if", "else", "match", "case",
    "var", "dealloc", "region", "alloc", "in", "reset", "shift",
    "resume", "make", "get", "put", "true", "false",
    "abort", "return", "toplevel"
  )

  def keyword(kw: String): Parser[String] =
    regex((s"$kw(?!$nameRest)").r, kw)

  lazy val anyKeyword =
    keywords("[^a-zA-Z0-9_!?$]".r, keywordStrings)

  lazy val singleline = """//[^\n]*(\n|\z)""".r
  lazy val multiline = """/\*[^*]*\*+(?:[^/*][^*]*\*+)*/""".r
  lazy val simplespace = """\s+""".r

  override val whitespace = rep(simplespace | singleline | multiline)

  // === Literals ===

  lazy val integerLiteral = regex("([-+])?(0|[1-9][0-9]*)".r, "Integer literal")
  lazy val doubleLiteral =
    regex("([-+])?(0|[1-9][0-9]*)[.]([0-9]+)([eE][+-]?[0-9]+)?".r, "Double literal")
  lazy val stringLiteral =
    regex("""\"(\\.|[^\"])*\"""".r, "String literal") ^^ { s =>
      s.substring(1, s.length - 1)
    }
  lazy val byteLiteral = regex("0x([0-9A-F]{2})".r, "Byte literal") ^^ { s =>
    UByte.unsafeFromInt(Integer.parseInt(s.substring(2), 16))
  }

  // === Names ===
  lazy val id: P[Id] = ident ^^ { x => names.idFor(x) }

  // === Helpers ===

  def parens[T](p: => Parser[T]): Parser[T] = `(` ~> p <~ `)`
  def braces[T](p: => Parser[T]): Parser[T] = `{` ~> p <~ `}`
  def commaList[T](p: => Parser[T]): Parser[List[T]] = repsep(p, `,`) ^^ { _.toList }
  def many[T](p: => Parser[T]): Parser[List[T]] = rep(p) ^^ { _.toList }

  // === Module ===

  lazy val program: P[ModuleDecl] =
    many(declaration | toplevelDef) ^^ { items =>
      val declarations = items.collect { case d: core.Declaration => d }
      val definitions = items.collect { case d: ToplevelDefinition => d }
      ModuleDecl(Nil, declarations, Nil, definitions, Nil)
    }

  // === Declarations ===
  // Mirrors the core surface syntax: `type Id[T] { Ctor(f: Type) }` and
  // `interface Id[T] { op: BlockType }`.

  def brackets[T](p: => Parser[T]): Parser[T] = `[` ~> p <~ `]`
  lazy val maybeTypeParams: P[List[Id]] = brackets(commaList(id)).? ^^ (_.getOrElse(Nil))
  lazy val maybeTypeArgs: P[List[core.ValueType]] = brackets(commaList(valueType)).? ^^ (_.getOrElse(Nil))

  lazy val valueType: P[core.ValueType] =
    id ~ maybeTypeArgs ^^ { case name ~ targs => core.ValueType.Data(name, targs) }

  // A minimal block type, enough for interface operations in fixtures.
  lazy val blockType: P[core.BlockType] =
    ( parens(commaList(valueType)) ~ (`=>` ~> valueType) ^^ {
        case vparams ~ result => core.BlockType.Function(Nil, Nil, vparams, Nil, result)
      }
    | id ~ maybeTypeArgs ^^ { case name ~ targs => core.BlockType.Interface(name, targs) }
    )

  lazy val declaration: P[core.Declaration] =
    ( `type` ~> id ~ maybeTypeParams ~ braces(many(constructorDecl)) ^^ {
        case name ~ tparams ~ constructors => core.Declaration.Data(name, tparams, constructors)
      }
    | `interface` ~> id ~ maybeTypeParams ~ braces(many(propertyDecl)) ^^ {
        case name ~ tparams ~ properties => core.Declaration.Interface(name, tparams, properties)
      }
    )

  lazy val constructorDecl: P[core.Constructor] =
    id ~ maybeTypeParams ~ parens(commaList(fieldDecl)) ^^ {
      case tag ~ tparams ~ fields => core.Constructor(tag, tparams, fields)
    }

  lazy val fieldDecl: P[core.Field] =
    id ~ (`:` ~> valueType) ^^ { case name ~ tpe => core.Field(name, tpe) }

  lazy val propertyDecl: P[core.Property] =
    id ~ (`:` ~> blockType) ^^ { case name ~ tpe => core.Property(name, tpe) }

  // === Toplevel ===

  lazy val toplevelDef: P[ToplevelDefinition] =
    ( `def` ~> id ~ parens(commaList(id)) ~ braces(stmt) <~ `;`.? ^^ {
        case name ~ params ~ body => ToplevelDefinition.Def(name, params, body)
      }
    | `let` ~> id ~ (`|` ~> id <~ `,`) ~ id ~ (`=` ~> stmt) <~ `;`.? ^^ {
        case name ~ ks ~ k ~ binding => ToplevelDefinition.Val(name, ks, k, binding)
      }
    )

  // === Expressions ===

  lazy val expr: P[Expr] =
    ( `abort` ^^^ Expr.Abort
    | `toplevel` ^^^ Expr.Toplevel
    | literal("()") ^^^ Expr.Literal((), Type.TUnit)
    | `true` ^^^ Expr.Literal(true, Type.TBoolean)
    | `false` ^^^ Expr.Literal(false, Type.TBoolean)
    | byteLiteral ^^ { b => Expr.Literal(b, Type.TByte) }
    | doubleLiteral ^^ { n => Expr.Literal(n.toDouble, Type.TDouble) }
    | integerLiteral ^^ { n => Expr.Literal(n.toLong, Type.TInt) }
    | stringLiteral ^^ { s => Expr.Literal(s, Type.TString) }
    | `make` ~> id ~ parens(commaList(expr)) ^^ {
        case tag ~ args => Expr.Make(null.asInstanceOf[ValueType.Data], tag, args)
      }
    | id ^^ Expr.Variable.apply
    )

  // === Statements ===

  lazy val stmt: P[Stmt] =
    ( defStmt
    | newStmt
    | getStmt
    | putStmt
    | callStmt
    | directCallStmt
    | letStmt
    | runStmt
    | ifStmt
    | matchStmt
    | varStmt
    | deallocStmt
    | regionStmt
    | allocStmt
    | resetStmt
    | shiftStmt
    | resumeStmt
    | returnStmt
    | holeStmt
    | tailCall
    | invokeOrApp
    | braces(stmt)
    )

  // return(v1, ..., vn)
  lazy val returnStmt: P[Stmt] =
    `return` ~> parens(commaList(expr)) ^^ Stmt.Return.apply

  // def id(params) = { body } rest
  lazy val defStmt: P[Stmt] =
    `def` ~> id ~ parens(commaList(id)) ~ braces(stmt) ~ stmt ^^ {
      case name ~ params ~ body ~ rest => Stmt.Def(name, params, body, rest)
    }

  // new id : Interface { ops } rest
  lazy val newStmt: P[Stmt] =
    `new` ~> id ~ (`:` ~> id) ~ braces(many(operation)) ~ stmt ^^ {
      case name ~ iface ~ ops ~ rest => Stmt.New(name, iface, ops, rest)
    }

  // def name(params) = { body }
  lazy val operation: P[Operation] =
    `def` ~> id ~ parens(commaList(id)) ~ (`=` ~> braces(stmt)) ^^ {
      case name ~ params ~ body => Operation(name, params, body)
    }

  // get id = ref;
  lazy val getStmt: P[Stmt] =
    `get` ~> id ~ (`=` ~> id <~ `;`) ~ stmt ^^ {
      case name ~ ref ~ rest => Stmt.Get(ref, name, rest)
    }

  // put ref = value;
  lazy val putStmt: P[Stmt] =
    `put` ~> id ~ (`=` ~> expr <~ `;`) ~ stmt ^^ {
      case ref ~ value ~ rest => Stmt.Put(ref, value, rest)
    }

  // let id = expr;
  private lazy val callArguments: P[(List[Expr], Expr)] =
    parens(rep1sep(expr, `,`) <~ `,` <~ `return`) ^^ { values =>
        val expressions = values.toList
        expressions.init -> expressions.last
      }

  private lazy val callCallee: P[Callee] =
    id ~ (`.` ~> id).? ^^ {
      case receiver ~ Some(method) => Callee.Method(receiver, method)
      case function ~ None => Callee.Function(function)
    }

  // single result `x` or a parenthesized tuple of results `(x, y, ...)`
  private lazy val resultBinding: P[List[Id]] =
    ( parens(commaList(id))
    | id ^^ { List(_) }
    )

  lazy val callStmt: P[Stmt] =
    `let` ~> resultBinding ~ (`|` ~> id).? ~ (`=` ~> callCallee <~ `!`) ~ callArguments ~ (`;` ~> stmt) ^^ {
      case names ~ returnedKs ~ callee ~ (args, ks) ~ rest =>
        Stmt.Call(callee, args,
          ReturnPoint.Bind(names, returnedKs.getOrElse(Id("ks")), ks, rest))
    }

  // let results = callee(args);
  lazy val directCallStmt: P[Stmt] =
    `let` ~> resultBinding ~ (`=` ~> callCallee) ~ parens(commaList(expr)) ~ (`;` ~> stmt) ^^ {
      case names ~ callee ~ args ~ rest =>
        Stmt.Call(callee, args, ReturnPoint.Direct(names, rest))
    }

  // callee!(args) @ ks, k
  lazy val tailCall: P[Stmt] =
    (callCallee <~ `!`) ~ parens(commaList(expr)) ~
      (`@` ~> expr <~ `,`) ~ expr ^^ {
      case callee ~ args ~ ks ~ k =>
        Stmt.Call(callee, args, ReturnPoint.Tail(ks, k))
    }

  // let id = expr;
  lazy val letStmt: P[Stmt] =
    `let` ~> id ~ (`=` ~> expr <~ `;`) ~ stmt ^^ {
      case name ~ binding ~ rest => Stmt.Let(name, binding, rest)
    }

  // run id = callee(args);
  // run! id = callee(args);
  // run~ id = callee(args);
  lazy val runStmt: P[Stmt] =
    ( `run~` ~> id ~ (`=` ~> id) ~ parens(commaList(expr)) ~ (`;` ~> stmt) ^^ {
        case name ~ callee ~ args ~ rest => Stmt.Run(name, callee, args, Purity.Async, rest)
      }
    | `run!` ~> id ~ (`=` ~> id) ~ parens(commaList(expr)) ~ (`;` ~> stmt) ^^ {
        case name ~ callee ~ args ~ rest => Stmt.Run(name, callee, args, Purity.Impure, rest)
      }
    | `run` ~> id ~ (`=` ~> id) ~ parens(commaList(expr)) ~ (`;` ~> stmt) ^^ {
        case name ~ callee ~ args ~ rest => Stmt.Run(name, callee, args, Purity.Pure, rest)
      }
    )

  // if (cond) { thn } else { els }
  lazy val ifStmt: P[Stmt] =
    `if` ~> parens(expr) ~ braces(stmt) ~ (`else` ~> braces(stmt)) ^^ {
      case cond ~ thn ~ els => Stmt.If(cond, thn, els)
    }

  // scrutinee match { case Tag (params) => { body } ... } else { default }
  lazy val matchStmt: P[Stmt] =
    expr ~ (`match` ~> `{` ~> many(matchClause) <~ `}`) ~ (`else` ~> braces(stmt)).? ^^ {
      case scrutinee ~ clauses ~ default => Stmt.Match(scrutinee, clauses, default)
    }

  lazy val matchClause: P[(Id, Clause)] =
    `case` ~> id ~ clause ^^ { case tag ~ cl => (tag, cl) }

  lazy val clause: P[Clause] =
    parens(commaList(id)) ~ (`=>` ~> stmt) ^^ {
      case params ~ body => Clause(params, body)
    }

  // var id = init @ ks;
  lazy val varStmt: P[Stmt] =
    `var` ~> id ~ (`=` ~> expr) ~ (`@` ~> expr <~ `;`) ~ stmt ^^ {
      case name ~ init ~ ks ~ rest => Stmt.Var(name, init, ks, rest)
    }

  // dealloc(ref);
  lazy val deallocStmt: P[Stmt] =
    `dealloc` ~> parens(id) ~ (`;` ~> stmt) ^^ {
      case ref ~ rest => Stmt.Dealloc(ref, rest)
    }

  // region id @ ks { rest }
  lazy val regionStmt: P[Stmt] =
    `region` ~> id ~ (`@` ~> expr) ~ braces(stmt) ^^ {
      case name ~ ks ~ rest => Stmt.Region(name, ks, rest)
    }

  // alloc id in region = init;
  lazy val allocStmt: P[Stmt] =
    `alloc` ~> id ~ (`in` ~> id) ~ (`=` ~> expr <~ `;`) ~ stmt ^^ {
      case name ~ region ~ init ~ rest => Stmt.Alloc(name, init, region, rest)
    }

  // reset(p, ks, k) { body } @ ks1, k1
  lazy val resetStmt: P[Stmt] =
    `reset` ~> parens(id ~ (`,` ~> id) ~ (`,` ~> id)) ~
      braces(stmt) ~ (`@` ~> expr <~ `,`) ~ expr ^^ {
      case (p ~ ks ~ k) ~ body ~ ks1 ~ k1 =>
        Stmt.Reset(p, ks, k, body, ks1, k1)
    }

  // shift(prompt) { resume, ks, k => body } @ ks1, k1
  lazy val shiftStmt: P[Stmt] =
    `shift` ~> parens(id) ~
      (`{` ~> id <~ `,`) ~ (id <~ `,`) ~ (id <~ `=>`) ~ (stmt <~ `}`) ~
      (`@` ~> expr <~ `,`) ~ expr ^^ {
      case prompt ~ resume ~ ks ~ k ~ body ~ ks1 ~ k1 =>
        Stmt.Shift(prompt, resume, ks, k, body, ks1, k1)
    }

  // resume(r) { ks, k => body } @ ks1, k1
  lazy val resumeStmt: P[Stmt] =
    `resume` ~> parens(id) ~
      (`{` ~> id <~ `,`) ~ (id <~ `=>`) ~ (stmt <~ `}`) ~
      (`@` ~> expr <~ `,`) ~ expr ^^ {
      case r ~ ks ~ k ~ body ~ ks1 ~ k1 =>
        Stmt.Resume(r, ks, k, body, ks1, k1)
    }

  // <>
  lazy val holeStmt: P[Stmt] =
    `<>` ^^^ Stmt.Hole(Span.missing)

  // A terminal transfer: id.method(args) or id(args)
  lazy val invokeOrApp: P[Stmt] =
    callCallee ~ parens(commaList(expr)) ^^ {
      case callee ~ args => Stmt.Call(callee, args, ReturnPoint.Jump)
    }

  // === Entry points ===

  def parse(source: Source)(using C: ErrorReporter): Option[ModuleDecl] =
    parseAll(program, source) match {
      case Success(ast, _) => Some(ast)
      case res: NoSuccess =>
        val input = res.next
        val range = Range(input.position, input.nextPosition)
        C.report(ParseError(res.message, Some(range), Severities.Error))
        None
    }

  def parseStmt(input: String): ParseResult[Stmt] =
    parseAll(stmt, StringSource(input, "input-string"))
}

object Parser {
  def apply(names: Names): Parser = new Parser(names)

  def module(input: String, names: Names): ParseResult[ModuleDecl] = {
    val parsers = Parser(names)
    parsers.parseAll(parsers.program, StringSource(input, "input-string"))
  }

  def statement(input: String, names: Names): ParseResult[Stmt] = {
    val parsers = Parser(names)
    parsers.parseAll(parsers.stmt, StringSource(input, "input-string"))
  }
}
