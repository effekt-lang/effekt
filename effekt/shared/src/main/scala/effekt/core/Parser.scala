package effekt
package core

import effekt.core.Param.ValueParam
import effekt.source.NoSource
import effekt.util.messages.{ DebugMessaging, ErrorReporter, ParseError }
import kiama.parsing.{ Failure, Input, NoSuccess, ParseResult, Parsers, Success }
import kiama.util.{ Position, Positions, Range, Source, StringSource }

import scala.util.matching.Regex

class Names(var knownNames: Map[String, Id]) {
  def idFor(name: String): Id = knownNames.getOrElse(name, {
    val id = Id(name)
    knownNames = knownNames.updated(name, id)
    id
  })
}

class CoreParsers(positions: Positions, names: Names) extends EffektLexers(positions) {

  def parse(source: Source)(using C: ErrorReporter): Option[ModuleDecl] =
    parseAll(program, source) match {
      case Success(ast, _) =>
        Some(ast)

      case res: NoSuccess =>
        val input = res.next
        val range = Range(input.position, input.nextPosition)
        C.report(ParseError(res.message, Some(range)))
        None
    }

  lazy val `run` = keyword("run")
  lazy val `;`   = super.literal(";")

  /**
   * Literals
   */
  lazy val int    = integerLiteral ^^ { n => Literal(n.toInt, Type.TInt) }
  lazy val bool   = `true` ^^^ Literal(true, Type.TBoolean) | `false` ^^^ Literal(false, Type.TBoolean)
  lazy val unit   = literal("()") ^^^ Literal((), Type.TUnit)
  lazy val double = doubleLiteral ^^ { n => Literal(n.toDouble, Type.TDouble) }
  lazy val string = stringLiteral ^^ { s => Literal(s.substring(1, s.size - 1), Type.TString) }

  /**
   * Names
   */
  lazy val id = ident ^^ { name => names.idFor(name) }
  lazy val wildcard = success(names.idFor("_"))

  /**
   * Main Entry
   */
  lazy val program: P[ModuleDecl] =
    `module` ~/> moduleName ~
      many(importDecl) ~
      many(declaration) ~
      many(externDecl) ~
      many(definition) ~
      many(exportDecl) ^^ ModuleDecl.apply

  lazy val importDecl: P[String] =
    `import` ~/> moduleName


  // Externs
  // -------
  lazy val externDecl: P[Extern] =
    ( `extern` ~> externBody ^^ Extern.Include.apply
    | `extern` ~> (captures <~ `def`) ~ signature ~ (`=` ~> externBody) ^^ {
      case captures ~ (id, tparams, cparams, vparams, bparams, result) ~ body =>
        Extern.Def(id, tparams, cparams, vparams, bparams, result, captures, body)
    })

  lazy val externBody = stringLiteral | multilineString


  // Declarations
  // ------------
  lazy val declaration: P[Declaration] = dataDecl | interfaceDecl

  lazy val dataDecl: P[Declaration] =
    `type` ~> id ~ maybeTypeParams ~ (`{` ~/> many(constructor) <~ `}`) ^^ Declaration.Data.apply

  lazy val interfaceDecl: P[Declaration] =
    `interface` ~> id ~ maybeTypeParams ~ (`{` ~/> many(property) <~ `}`) ^^ Declaration.Interface.apply

  lazy val constructor: P[Constructor] =
    id ~ valueParams ^^ { case id ~ params => Constructor(id, params.map(p => Field(p.id, p.tpe))) }

  lazy val property: P[Property] =
    id ~ (`:` ~> blockType) ^^ Property.apply

  // Definitions
  // -----------
  lazy val definition: P[Definition] =
  ( `let` ~> someSep(id, `,`) ~ (`=` ~/> expr) ^^ Definition.Let.apply // TODO MRV 5
  | `def` ~> id ~ (`=` ~/> block) ^^ Definition.Def.apply
  | `def` ~> id ~ parameters ~ (`=` ~> stmt) ^^ {
      case name ~ (tparams, cparams, vparams, bparams) ~ body =>
        Definition.Def(name, BlockLit(tparams, cparams, vparams, bparams, body))
    }
  | failure("Expected a definition.")
  )


  // Statements
  // ----------
  lazy val stmt: P[Stmt] =
    ( `{` ~/> many(definition) ~ stmt <~ `}` ^^ Stmt.Scope.apply // curly braces induce scopes!
    | `return` ~> someSep(pure, `,`) ^^ Stmt.Return.apply
    | `val` ~> someSep(id, `,`) ~ (`=` ~> stmt) ~ (`;` ~> stmt) ^^ Stmt.Val.apply // TODO MRV 5
    | block ~ maybeTypeArgs ~ valueArgs ~ blockArgs ^^ Stmt.App.apply
    | (`if` ~> `(` ~/> pure <~ `)`) ~ stmt ~ (`else` ~> stmt) ^^ Stmt.If.apply
    | `region` ~> blockLit ^^ Stmt.Region.apply
    | `try` ~> blockLit ~ many(`with` ~> implementation) ^^ Stmt.Try.apply
    | `var` ~> id ~ (`in` ~> id) ~ (`=` ~> pure) ~ (`;` ~> stmt) ^^ { case id ~ region ~ init ~ body => State(id, init, region, body) }
    | `<>` ^^^ Hole()
    | (pure <~ `match`) ~/ (`{` ~> many(clause) <~ `}`) ~ (`else` ~> stmt).? ^^ Stmt.Match.apply
    )

  lazy val clause: P[(Id, BlockLit)] = (id <~ `:`) ~ blockLit ^^ { case id ~ cl => id -> cl }

  // Implementations
  // ---------------
  lazy val implementation: P[Implementation] =
    interfaceType ~ (`{` ~/> many(operation) <~ `}`) ^^ Implementation.apply

  lazy val operation: P[Operation] =
    `def` ~/> id ~ parameters ~ (`with` ~> blockParam).? ~ (`=` ~/> stmt) ^^ {
      case name ~ (tparams, cparams, vparams, bparams) ~ k ~ body =>
        Operation(name, tparams, cparams, vparams, bparams, k, body)
    }


  // Pure Expressions
  // ----------------
  lazy val pure: P[Pure] =
    pureNonAccess ~ many((`.` ~> id) ~ (`:` ~> valueType)) ^^ {
      case firstTarget ~ accesses => accesses.foldLeft(firstTarget){
        case (target, field ~ tpe) => Pure.Select(target, field, tpe)
      }
    }

  lazy val pureNonAccess: P[Pure] =
    ( literal
    | id ~ (`:` ~> valueType) ^^ Pure.ValueVar.apply
    | `box` ~> captures ~ block ^^ { case capt ~ block => Pure.Box(block, capt) }
    | block ~ maybeTypeArgs ~ valueArgs ^^ Pure.PureApp.apply
    | failure("Expected a pure expression.")
    )

  lazy val literal: P[Pure] = int | bool | string | unit | double


  // Calls
  // -----
  lazy val valueArgs: P[List[Pure]] =
    `(` ~> manySep(pure, `,`) <~ `)`

  lazy val blockArgs: P[List[Block]] =
    many(blockLit | `{` ~> block <~ `}`)


  // Expressions
  // -----------
  lazy val expr: P[Expr] =
    ( pure
    | `run` ~> stmt ^^ Run.apply
    | (`!` ~/> block) ~ maybeTypeArgs ~ valueArgs ~ blockArgs ^^ DirectApp.apply
    )


  // Blocks
  // ------
  lazy val block: P[Block] =
    ( blockNonMember ~ many((`.` ~> id) ~ (`:` ~> blockType)) ^^ {
          case firstReceiver ~ fields => fields.foldLeft(firstReceiver) {
          case (receiver, field ~ tpe) => Block.Member(receiver, field, tpe)
        }
      }
    | blockNonMember
    )

  lazy val blockNonMember: P[Block] =
    ( id ~ (`:` ~> blockType) ~ (`@` ~> captures) ^^ Block.BlockVar.apply
    | `unbox` ~> pure ^^ Block.Unbox.apply
    | `new` ~> implementation ^^ Block.New.apply
    | blockLit
    // TODO check left associative nesting (also for select)
    | `(` ~> block <~ `)`
    )

  lazy val blockLit: P[Block.BlockLit] =
    `{` ~> parameters ~ (`=>` ~/> stmt) <~ `}` ^^ {
      case (tparams, cparams, vparams, bparams) ~ body =>
        Block.BlockLit(tparams, cparams, vparams, bparams, body) : Block.BlockLit
      }

  // Exports
  // -------
  lazy val exportDecl: P[Id] = `export` ~> id


  // Signatures
  // -------
  // foo[Int, String](x: Int) { f@f2: Exc }: Int
  lazy val signature: P[(Id, List[Id], List[Id], List[Param.ValueParam], List[Param.BlockParam], ValueType)] =
    id ~ parameters ~ (`:` ~> valueType) ^^ {
      case name ~ (tparams, cparams, vparams, bparams) ~ result =>
        (name, tparams, cparams, vparams, bparams, result)
    }

  lazy val parameters: P[(List[Id], List[Id], List[Param.ValueParam], List[Param.BlockParam])] =
    maybeTypeParams ~ valueParams ~ many(trackedBlockParam) ^^ {
      case tparams ~ vparams ~ bcparams =>
        val (cparams, bparams) = bcparams.unzip
        (tparams, cparams, vparams, bparams)
    }

  lazy val typeParams: P[List[Id]] = `[` ~> someSep(typeParam, `,`) <~ `]`
  lazy val maybeTypeParams: P[List[Id]] = typeParams | success(Nil)
  lazy val typeParam: P[Id] = `'` ~> id


  lazy val valueParam: P[ValueParam] =
    id ~ (`:` ~> valueType) ^^ { case id ~ tpe => Param.ValueParam(id, tpe): Param.ValueParam }

  lazy val valueParams: P[List[ValueParam]] =
    `(` ~> manySep(valueParam, `,`) <~ `)`

  // f@f2 : Exc
  lazy val trackedBlockParam: P[(Id, BlockParam)] =
    ( `{` ~> id ~ (`@` ~> id)  ~ (`:` ~> blockType) <~ `}` ^^ {
        case id ~ capt ~ tpe => capt -> (Param.BlockParam(id, tpe): Param.BlockParam)
      }
    // abbreviation: f : Exc .= f@f : Exc
    | blockParam ^^ { p => p.id -> p }
    )

  lazy val blockParam: P[BlockParam] =
    `{` ~> id ~ (`:` ~> blockType) <~ `}` ^^ { case id ~ tpe => Param.BlockParam(id, tpe): Param.BlockParam }


  // Types
  // -----
  lazy val captures: P[Captures] = `{` ~> manySep(id, `,`) <~ `}` ^^ { ids => ids.toSet }

  lazy val valueType: P[ValueType] =
    ( nocut(blockType) ~ (`at` ~/> captures) ^^ ValueType.Boxed.apply
    | primValueType
    )

  lazy val primValueType: P[ValueType] =
    ( typeParam ^^ ValueType.Var.apply
    | id ~ maybeTypeArgs ^^ ValueType.Data.apply
    | `(` ~> valueType <~ `)`
    | failure("Expected a value type")
    )

  lazy val blockType: P[BlockType] =
    ( maybeTypeParams ~ maybeValueTypes ~ many(blockTypeParam) ~ (`=>` ~/> someSep(primValueType, `,`)) ^^ {
      case tparams ~ vparams ~ bcparams ~ results =>
        val (cparams, bparams) = bcparams.unzip
        BlockType.Function(tparams, cparams, vparams, bparams, results)
      }
    | interfaceType
    )


  lazy val maybeValueTypes: P[List[ValueType]] =
    ( `(` ~> manySep(valueType, `,`) <~ `)`
    | success(Nil)
    )

  // { f : S }
  // abbreviation { S } .= { _: S }
  lazy val blockTypeParam: P[(Id, BlockType)] =
    `{` ~> (id <~ `:` | wildcard) ~ blockType <~ `}` ^^ { case id ~ tpe => id -> tpe }

  lazy val interfaceType: P[BlockType.Interface] =
    ( id ~ maybeTypeArgs ^^ { case id ~ tpe => BlockType.Interface(id, tpe) : BlockType.Interface }
    | failure("Expected an interface")
    )

  lazy val typeArgs: P[List[ValueType]] =
    `[` ~/> manySep(valueType, `,`) <~ `]`

  lazy val maybeTypeArgs: P[List[ValueType]] =
    typeArgs.? ^^ { o => o.getOrElse(Nil) }
}

object CoreParsers {

  def apply(names: Map[String, Id]): CoreParsers = apply(Names(names))

  def apply(names: Names): CoreParsers =
    object pos extends Positions
    object parsers extends CoreParsers(pos, names)
    parsers

  // Some alternative main entry points for most common usages
  def module(input: String, names: Map[String, Id] = Map.empty): ParseResult[ModuleDecl] =
    val in = StringSource(input, "input-string")
    val parsers = CoreParsers(names)
    parsers.parseAll(parsers.program, in)

  def module(input: String, names: Names): ParseResult[ModuleDecl] =
    val parsers = CoreParsers(names)
    parsers.parseAll(parsers.program, input)

  def statement(input: String, names: Map[String, Id] = Map.empty): ParseResult[Stmt] =
    val parsers = CoreParsers(names)
    parsers.parseAll(parsers.stmt, input)

  def definition(input: String, names: Map[String, Id] = Map.empty): ParseResult[Definition] =
    val parsers = CoreParsers(names)
    parsers.parseAll(parsers.definition, input)
}
