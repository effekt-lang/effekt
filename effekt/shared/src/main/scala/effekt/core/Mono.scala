package effekt
package core

import effekt.context.Context
import effekt.lexer.TokenKind
import effekt.context.assertions.asDataType

object Mono extends Phase[CoreTransformed, CoreTransformed] {

  override val phaseName: String = "mono"

  override def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = {
    input match {
      case CoreTransformed(source, tree, mod, core) => {
        core match {
          case ModuleDecl(path, includes, declarations, externs, definitions, exports) => {
            // Find constraints in the definitions
            val monoFindContext = MonoFindContext()
            var constraints = findConstraints(definitions)(using monoFindContext)
            constraints = constraints ++ declarations.flatMap(findConstraints(_)(using monoFindContext))
            // println("Constraints")
            // constraints.foreach(c => println(c))
            // println()

            // Solve collected constraints
            val solution = solveConstraints(constraints)
            // println("Solved")
            // solution.foreach(println)
            // println()

            // Monomorphize existing definitions
            var monoNames: MonoNames = Map.empty
            solution.foreach((funId, targs) => 
              targs.foreach(vb => 
                monoNames += ((funId, vb) -> freshMonoName(funId, vb))
              )
            )

            // Collect polymorphic extern definitions
            var polyExternDefs: List[Id] = List.empty 
            externs.foreach {
              case Extern.Include(featureFlag, contents) => ()
              case Extern.Def(id, List(), cparams, vparams, bparams, ret, annotatedCapture, body) => ()
              case Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, body) => polyExternDefs :+= id
            }

            var monoContext = MonoContext(solution, monoNames, polyExternDefs)
            val monoDecls = declarations flatMap (monomorphize(_)(using monoContext))
            val monoDefs = monomorphize(definitions)(using monoContext)
            // monoDecls.foreach(decl => println(util.show(decl)))
            // println()
            // monoDefs.foreach(defn => println(util.show(defn)))
            val newModuleDecl = ModuleDecl(path, includes, monoDecls, externs, monoDefs, exports)

            // println(util.show(newModuleDecl))

            // for examples/pos/probabilistic.effekt
            // This 
            //  Vector(Base(Weighted,List(Base(Bool,List()), Base(Var,List()))))
            // Should be
            //  Vector(Base(Weighted,List(Base(Bool,List())), Base(Weighted,List(Base(Bool,List())))))

            return Some(CoreTransformed(source, tree, mod, newModuleDecl))
          }
        }
      }
    }
    Some(input)
  }
}

type FunctionId = Id
case class Constraint(lower: Vector[TypeArg], upper: FunctionId)
type Constraints = List[Constraint]

type Ground = TypeArg.Base | TypeArg.Boxed
type Solution = Map[FunctionId, Set[Vector[Ground]]]
type MonoNames = Map[(FunctionId, Vector[Ground]), FunctionId]

enum TypeArg {
  case Base(val tpe: Id, targs: List[TypeArg])
  case Var(funId: FunctionId, pos: Int)
  case Boxed(tpe: BlockType, capt: Captures)
}

// Type Id -> Var
type TypeParams = Map[Id, TypeArg.Var]

class MonoFindContext {
  var typingContext: TypeParams = Map()

  def extendTypingContext(tparam: Id, index: Int, functionId: FunctionId) =
    typingContext += (tparam -> TypeArg.Var(functionId, index))
}

case class MonoContext(solution: Solution, names: MonoNames, polyExternDefs: List[Id]) {
  var replacementTparams: Map[Id, Ground] = Map.empty
  def isPolyExtern(id: Id) = polyExternDefs.contains(id)

  def allNames(id: Id) =
    names.filter((key, _) => key._1 == id)
}

def findConstraints(definitions: List[Toplevel])(using MonoFindContext): Constraints =
  definitions flatMap findConstraints

def findConstraints(definition: Toplevel)(using ctx: MonoFindContext): Constraints = definition match
  case Toplevel.Def(id, BlockLit(tparams, cparams, vparams, bparams, body)) => 
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
    findConstraints(body)
  case Toplevel.Def(id, block) => 
    findConstraints(block)
  case Toplevel.Val(id, tpe, binding) => 
    findConstraints(binding)

def findConstraints(declaration: Declaration)(using ctx: MonoFindContext): Constraints = declaration match
  // Maybe[T] { Just[](x: T) }
  case Data(id, tparams, constructors) =>
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
    constructors.map{ constr =>
      val arity = tparams.size // + constr.tparams.size
      val constructorArgs = (0 until arity).map(index =>
        TypeArg.Var(constr.id, index) // Just.0  
      ).toVector // < Just.0 >
      Constraint(constructorArgs, id) // < Just.0 > <: Maybe
    }
  case Interface(id, tparams, properties) => 
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
    List.empty

def findConstraints(block: Block)(using ctx: MonoFindContext): Constraints = block match
  case BlockVar(id, annotatedTpe: BlockType.Interface, annotatedCapt) => findConstraints(annotatedTpe)
  case BlockVar(id, annotatedTpe: BlockType.Function, annotatedCapt) => findConstraints(annotatedTpe, id)
  case BlockLit(tparams, cparams, vparams, bparams, body) => findConstraints(body)
  case Unbox(pure) => findConstraints(pure)
  case New(impl) => findConstraints(impl)

def findConstraints(blockType: BlockType.Interface)(using ctx: MonoFindContext): Constraints = blockType match
  case BlockType.Interface(name, targs) => 
    val (newTargs, constraints) = findConstraints(targs)
    List(Constraint(newTargs.toVector, name)) ++ constraints

def findConstraints(blockType: BlockType.Function, fnId: Id)(using ctx: MonoFindContext): Constraints = blockType match
  case BlockType.Function(tparams, cparams, vparams, bparams, result) => 
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, fnId))
    List()

def findConstraints(impl: Implementation)(using ctx: MonoFindContext): Constraints = impl match 
  case Implementation(interface, operations) => 
    findConstraints(interface) ++
    (operations flatMap findConstraints)

def findConstraints(operation: Operation)(using ctx: MonoFindContext): Constraints = operation match
  case Operation(name, tparams, cparams, vparams, bparams, body) => 
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, name))
    findConstraints(body)

def findConstraints(constructor: Constructor)(using ctx: MonoFindContext): Constraints = constructor match
  case Constructor(id, tparams, List()) => List.empty
  case Constructor(id, tparams, fields) =>
    val (newTargs, constraints) = findConstraints(fields map (_.tpe))
    List(Constraint(newTargs.toVector, id)) ++ constraints

def findConstraints(stmt: Stmt)(using ctx: MonoFindContext): Constraints = stmt match
  case Let(id, annotatedTpe, binding, body) => findConstraints(binding) ++ findConstraints(body)
  case Return(expr) => findConstraints(expr)
  case Val(id, annotatedTpe, binding, body) => findConstraints(binding) ++ findConstraints(body)
  case Var(ref, init, capture, body) => findConstraints(body)
  case ImpureApp(id, callee, targs, vargs, bargs, body) =>
    val (newTargs, constraints) = findConstraints(targs)
    List(Constraint(newTargs.toVector, callee.id)) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints) ++ findConstraints(body) ++ constraints
  case App(callee: BlockVar, targs, vargs, bargs) => 
    val (newTargs, constraints) = findConstraints(targs)
    List(Constraint(newTargs.toVector, callee.id)) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints) ++ constraints
  // TODO: Very specialized, but otherwise passing an id that matches in monomorphize is hard
  //       although I'm not certain any other case can even happen 
  // TODO: part 2, also update the implementation in monomorphize if changing this
  case App(Unbox(ValueVar(id, annotatedType)), targs, vargs, bargs) => 
    val (newTargs, constraints) = findConstraints(targs)
    List(Constraint(newTargs.toVector, id)) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints) ++ constraints
  case App(callee, targs, vargs, bargs) =>
    findConstraints(callee) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints)
  // TODO: Maybe need to do something with methodTpe
  case Invoke(callee: BlockVar, method, methodTpe, targs, vargs, bargs) => 
    val (newTargs, constraints) = findConstraints(targs)
    List(Constraint(newTargs.toVector, method)) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints) ++ constraints
  case Invoke(Unbox(ValueVar(id, annotatedType)), method, methodTpe, targs, vargs, bargs) =>
    val (newTargs, constraints) = findConstraints(targs)
    List(Constraint(newTargs.toVector, method)) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints) ++ constraints
  case Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
    findConstraints(callee) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints)
  case Reset(body) => findConstraints(body)
  case If(cond, thn, els) => findConstraints(cond) ++ findConstraints(thn) ++ findConstraints(els)
  case Def(id, BlockLit(tparams, cparams, vparams, bparams, bbody), body) => 
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
    findConstraints(bbody) ++ findConstraints(body)
  case Def(id, block, body) => 
    findConstraints(block) ++ findConstraints(body)
  case Shift(prompt, body) => findConstraints(prompt) ++ findConstraints(body)
  case Match(scrutinee, clauses, default) => 
    // TODO: This is probably not technically correct, but allows for fun stuff like
    // type Foo[A] {
    //   Bar[B](x: B)
    // }
    // to be monomorphized (in special cases (?))
    var additionalConstraint: Constraints = List()
    clauses.foreach((id, bl) => { bl match {
        case BlockLit(tparams, cparams, vparams, bparams, App(callee: BlockVar, targs, vargs, bargs)) => 
          if (targs.size == tparams.size) {
            additionalConstraint +:= Constraint(tparams.zipWithIndex.map((_, paramIndex) => TypeArg.Var(id, paramIndex)).toVector, callee.id)
          }
        case _ => ()
      }
    })

    additionalConstraint ++ clauses.map(_._2).flatMap(findConstraints) ++ findConstraints(default)
  case Resume(k, body) => findConstraints(k) ++ findConstraints(body)
  case Get(id, annotatedTpe, ref, annotatedCapt, body) => findConstraints(body)
  case Put(ref, annotatedCapt, value, body) => findConstraints(value) ++ findConstraints(body)
  case Alloc(id, init, region, body) => findConstraints(init) ++ findConstraints(body)
  case Region(body) => findConstraints(body)
  case Hole(span) => List.empty

def findConstraints(opt: Option[Stmt])(using ctx: MonoFindContext): Constraints = opt match 
  case None => List.empty
  case Some(stmt) => findConstraints(stmt)

def findConstraints(expr: Expr)(using ctx: MonoFindContext): Constraints = expr match
  case PureApp(b, targs, vargs) => 
    val (newTargs, constraints) = findConstraints(targs)
    Constraint(newTargs.toVector, b.id) :: constraints
  case ValueVar(id, annotatedType) => List.empty
  case Literal(value, annotatedType) => List.empty
  case Make(data, tag, targs, vargs) => 
    // TODO: Is this the correct order?
    val combinedTargs = data.targs ++ targs
    val (newTargs, constraints) = findConstraints(combinedTargs)
    List(Constraint(newTargs.toVector, tag)) ++ // <Int> <: Just
    constraints
  case Box(b, annotatedCapture) => 
    findConstraints(b)

def findConstraints(vts: List[ValueType])(using ctx: MonoFindContext): (List[TypeArg], Constraints) = {
  val vtFindConstraints = vts map findConstraints
  val targs = vtFindConstraints.map(_._1)
  val constraints = vtFindConstraints.flatMap(_._2)
  (targs, constraints)
}

def findConstraints(vt: ValueType)(using ctx: MonoFindContext): (TypeArg, Constraints) = vt match {
  case ValueType.Boxed(tpe, capt) => {
    // TODO: Perhaps recurse into tpe
    (TypeArg.Boxed(tpe, capt), List.empty)
  }
  case ValueType.Data(name, targs) => {
    val (newTargs, constraints) = findConstraints(targs)
    val additionalConstraints = if (!newTargs.isEmpty) {
      List(Constraint(newTargs.toVector, name))
    } else {
      List.empty
    }
    (TypeArg.Base(name, newTargs), constraints ++ additionalConstraints)
  }
  case ValueType.Var(name) => (ctx.typingContext(name), List.empty)
}

def solveConstraints(constraints: Constraints): Solution =
  var solved: Solution = Map()

  val filteredConstraints = constraints.filterNot(c => c.lower.isEmpty)
  val groupedConstraints = filteredConstraints.groupBy(c => c.upper)
  val vecConstraints = groupedConstraints.map((sym, constraints) => (sym -> constraints.map(c => c.lower).toSet))

  while (true) {
    val previousSolved = solved
    vecConstraints.foreach((sym, tas) => 
        val sol = solveConstraints(sym).map(bs => bs.toVector).filter(v => !v.isEmpty)
        solved += (sym -> sol)
      )
    if (previousSolved == solved) return solved
  }

  def solveConstraints(funId: FunctionId): Set[List[Ground]] =
    val filteredConstraints = vecConstraints(funId)
    var nbs: Set[List[Ground]] = Set.empty
    filteredConstraints.foreach(b => 
      var l: List[List[Ground]] = List(List.empty)
      def listFromIndex(ind: Int) = if (ind >= l.length) List.empty else l(ind)

      def solveTypeArg(typeArg: TypeArg): List[Ground] = typeArg match {
        case TypeArg.Base(tpe, targs) => 
          val solvedTargs = targs map solveTypeArg
          var crossTargs: List[List[TypeArg]] = List(List.empty)
          solvedTargs.foreach(targ => {
            crossTargs = productAppend(crossTargs, targ)
          })
          crossTargs.map(tas => TypeArg.Base(tpe, tas))
        case TypeArg.Boxed(tpe, capt) => List(TypeArg.Boxed(tpe, capt))
        case TypeArg.Var(funId, pos) => 
          val funSolved = solved.getOrElse(funId, Set.empty)
          val posArgs = funSolved.map(v => v(pos)).toList
          posArgs
      }

      b.foreach(typeArg => l = productAppend(l, solveTypeArg(typeArg)))
      nbs ++= l
    )
    nbs

  solved

def productAppend[A](ls: List[List[A]], rs: List[A]): List[List[A]] =
  if (rs.isEmpty) return ls
  for { l <- ls; r <- rs } yield l :+ r

def monomorphize(definitions: List[Toplevel])(using ctx: MonoContext): List[Toplevel] =
  var newDefinitions: List[Toplevel] = List.empty
  definitions.foreach(definition => newDefinitions ++= monomorphize(definition))
  newDefinitions

def monomorphize(toplevel: Toplevel)(using ctx: MonoContext): List[Toplevel] = toplevel match
  case Toplevel.Def(id, BlockLit(List(), cparams, vparams, bparams, body)) => 
    List(Toplevel.Def(id, BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body))))
  case Toplevel.Def(id, BlockLit(tparams, cparams, vparams, bparams, body)) => 
    val monoTypes = ctx.solution(id).toList
    monoTypes.map(baseTypes => 
      val replacementTparams = tparams.zip(baseTypes).toMap
      ctx.replacementTparams ++= replacementTparams
      Toplevel.Def(ctx.names(id, baseTypes), BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body)))
    )
  case Toplevel.Def(id, block) => 
    List(Toplevel.Def(id, monomorphize(block)))
  case Toplevel.Val(id, tpe, binding) => 
    List(Toplevel.Val(id, monomorphize(tpe), monomorphize(binding)))

def monomorphize(decl: Declaration)(using ctx: MonoContext): List[Declaration] = decl match
  case Data(id, List(), constructors) => 
    List(Declaration.Data(id, List.empty, constructors.flatMap(monomorphize(_, 0))))
  case Data(id, tparams, constructors) => 
    val monoTypes = ctx.solution.getOrElse(id, Set.empty).toList
    monoTypes.map(baseTypes =>
      val replacementTparams = tparams.zip(baseTypes).toMap
      ctx.replacementTparams ++= replacementTparams
      Declaration.Data(ctx.names(id, baseTypes), List.empty, constructors.flatMap(monomorphize(_, tparams.size)))
    )
  case Interface(id, List(), properties) => List(decl)
  case Interface(id, tparams, properties) =>
    val monoTypes = ctx.solution.getOrElse(id, Set.empty).toList
    monoTypes.map(baseTypes =>
      val replacementTparams = tparams.zip(baseTypes).toMap
      ctx.replacementTparams ++= replacementTparams
      Declaration.Interface(ctx.names(id, baseTypes), List.empty, properties)
    )

def monomorphize(constructor: Constructor, tparamCount: Int)(using ctx: MonoContext): List[Constructor] = constructor match
  case Constructor(id, List(), fields) => 
    List(Constructor(id, List.empty, fields map monomorphize))
  case Constructor(id, tparams, fields) => 
    val monoTypes = ctx.solution.getOrElse(id, Set.empty).toList
    if (monoTypes.isEmpty) {
      // FIXME?: Assuming here tparams is empty, or at least tparams.drop(tparamCount) is
      //         This case can happen if there is no flow into a specific constructor,
      //         in which case we should still emit the Constructor, but without the tparams
      List(Constructor(id, List.empty, fields map monomorphize))
    } else {
    monoTypes.map(baseTypes =>
      val replacementTparams = tparams.drop(tparamCount).zip(baseTypes).toMap
      ctx.replacementTparams ++= replacementTparams
      Constructor(ctx.names(id, baseTypes), List.empty, fields map monomorphize) 
    )
    }

def monomorphize(block: Block)(using ctx: MonoContext): Block = block match
  case b: BlockLit => monomorphize(b)
  case b: BlockVar => monomorphize(b)
  case New(impl) => New(monomorphize(impl)) 
  case Unbox(pure) => Unbox(monomorphize(pure))

def monomorphize(impl: Implementation)(using ctx: MonoContext): Implementation = impl match
  case Implementation(interface, operations) => Implementation(monomorphize(interface), operations.flatMap(monomorphize))

def monomorphize(interface: BlockType.Interface)(using ctx: MonoContext): BlockType.Interface = interface match
  case BlockType.Interface(name, targs) => 
    val replacementData = replacementDataFromTargs(name, targs)
    BlockType.Interface(replacementData.name, replacementData.targs)

def monomorphize(operation: Operation)(using ctx: MonoContext): List[Operation] = operation match
  case Operation(name, List(), cparams, vparams, bparams, body) =>
    List(Operation(name, List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body)))
  case Operation(name, tparams, cparams, vparams, bparams, body) => 
    val monoTypes = ctx.solution.getOrElse(name, Set.empty).toList
    monoTypes.map(baseTypes =>
      val replacementTparams = tparams.zip(baseTypes).toMap
      ctx.replacementTparams ++= replacementTparams
      Operation(ctx.names(name, baseTypes), List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body))
    )
    

def monomorphize(block: BlockLit)(using ctx: MonoContext): BlockLit = block match
  case BlockLit(tparams, cparams, vparams, bparams, body) => 
    // FIXME: Is passing tparams directly here without any change correct?
    BlockLit(tparams, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body))

def monomorphize(block: BlockVar)(using ctx: MonoContext): BlockVar = block match
  case BlockVar(id, annotatedTpe, annotatedCapt) => BlockVar(id, monomorphize(annotatedTpe), annotatedCapt)

def monomorphize(field: Field)(using ctx: MonoContext): Field = field match
  case Field(id, tpe) => Field(id, monomorphize(tpe))

def monomorphize(blockVar: BlockVar, replacementId: FunctionId)(using ctx: MonoContext): BlockVar = blockVar match
  case BlockVar(id, BlockType.Function(List(), cparams, vparams, bparams, result), annotatedCapt) => blockVar
  case BlockVar(id, BlockType.Function(tparams, cparams, vparams, bparams, result), annotatedCapt) if ctx.isPolyExtern(id) => blockVar
  case BlockVar(id, BlockType.Function(tparams, cparams, vparams, bparams, result), annotatedCapt) => 
    val monoAnnotatedTpe = BlockType.Function(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(result))
    BlockVar(replacementId, monoAnnotatedTpe, annotatedCapt)
  case BlockVar(id, annotatedTpe: BlockType.Interface, annotatedCapt) =>
    BlockVar(id, monomorphize(annotatedTpe), annotatedCapt)

def monomorphize(stmt: Stmt)(using ctx: MonoContext): Stmt = stmt match
  case Return(expr) => Return(monomorphize(expr))
  case Val(id, annotatedTpe, binding, body) => 
    Val(id, monomorphize(annotatedTpe), monomorphize(binding), monomorphize(body))
  case Var(ref, init, capture, body) => 
    Var(ref, monomorphize(init), capture, monomorphize(body))
  case ImpureApp(id, callee, targs, vargs, bargs, body) =>
    ImpureApp(id, callee, targs, vargs map monomorphize, bargs map monomorphize, monomorphize(body))
  case App(callee: BlockVar, targs, vargs, bargs) => 
    val replacementData = replacementDataFromTargs(callee.id, targs)
    App(monomorphize(callee, replacementData.name), List.empty, vargs map monomorphize, bargs map monomorphize)
  // TODO: Highly specialized, see todo in findConstraints for info
  //       change at the same time as findConstraints
  case App(Unbox(ValueVar(id, annotatedTpe)), targs, vargs, bargs) =>
    val replacementData = replacementDataFromTargs(id, targs)
    App(Unbox(ValueVar(id, monomorphize(annotatedTpe))), List.empty, vargs map monomorphize, bargs map monomorphize)
  case App(callee, targs, vargs, bargs) =>
    App(monomorphize(callee), List.empty, vargs map monomorphize, bargs map monomorphize)
  case Let(id, annotatedTpe, binding, body) => Let(id, monomorphize(annotatedTpe), monomorphize(binding), monomorphize(body))
  case If(cond, thn, els) => If(monomorphize(cond), monomorphize(thn), monomorphize(els))
  case Invoke(Unbox(pure), method, methodTpe, targs, vargs, bargs) =>
    Invoke(Unbox(monomorphize(pure)), method, methodTpe, List.empty, vargs map monomorphize, bargs map monomorphize)
  case Invoke(BlockVar(id, annotatedTpe, annotatedCapt), method, BlockType.Function(tparams, cparams, vparams, bparams, result), targs, vargs, bargs) => 
    val replacementData = replacementDataFromTargs(method, targs)
    val replacementTparams = tparams.zip(targs map toTypeArg).toMap
    ctx.replacementTparams ++= replacementTparams

    val monoAnnotatedTpe = BlockType.Function(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(result))
    Invoke(BlockVar(id, monomorphize(annotatedTpe), annotatedCapt), replacementData.name, monoAnnotatedTpe, List.empty, vargs map monomorphize, bargs map monomorphize)
  case Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
    Invoke(monomorphize(callee), method, methodTpe, List.empty, vargs map monomorphize, bargs map monomorphize)
  case Resume(k, body) =>
    Resume(monomorphize(k), monomorphize(body))
  // TODO: Monomorphizing here throws an error complaining about a missing implementation
  //       Not sure what is missing, altough it does works like this
  case Reset(body) =>
    Reset(body)
  case Def(id, BlockLit(List(), cparams, vparams, bparams, bbody), body) => 
    Stmt.Def(id, BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(bbody)), monomorphize(body))
  case Def(id, BlockLit(tparams, cparams, vparams, bparams, bbody), body) =>
    val monoTypes = ctx.solution(id).toList
    // Monomorphizing inner functions may yield multiple definitions
    // which then need to be nested
    def nestDefs(defnTypes: List[Vector[Ground]]): Stmt = defnTypes match {
      case head :: next => 
        val replacementTparams = tparams.zip(head).toMap
        ctx.replacementTparams ++= replacementTparams
        Stmt.Def(ctx.names(id, head), BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(bbody)), nestDefs(next))
      case Nil => monomorphize(body)
    }
    nestDefs(monoTypes)
  case Def(id, block, body) => Def(id, monomorphize(block), monomorphize(body))
  case Shift(prompt, body) => Shift(monomorphize(prompt), monomorphize(body))
  case Match(scrutinee, clauses, default) =>
    val monoScrutinee = monomorphize(scrutinee)

    // FIXME: Not correct in all cases. Have to figure out where this is needed
    // We need the type of the scrutinee, to give each clause the correct monomorphized name based on said type
    val monoScrutineeType = exprType(scrutinee) match {
      case ValueType.Data(name, targs) => 
        targs map monomorphize
      case _ => sys error "scrutinee type was not data"
    }

    val monoClauses = clauses.flatMap(monomorphize(_, monoScrutineeType.toVector))
    Match(monomorphize(scrutinee), monoClauses, monomorphize(default))
  case Get(id, annotatedTpe, ref, annotatedCapt, body) =>
    Get(id, monomorphize(annotatedTpe), ref, annotatedCapt, monomorphize(body))
  case Put(ref, annotatedCapt, value, body) =>
    Put(ref, annotatedCapt, monomorphize(value), monomorphize(body))
  case Alloc(id, init, region, body) =>
    Alloc(id, monomorphize(init), region, monomorphize(body))
  case Region(body) => Region(monomorphize(body))
  case Hole(span) => Hole(span)

def exprType(expr: Expr): ValueType = expr match {
  case Box(b, annotatedCapture) => ValueType.Boxed(b.tpe, annotatedCapture)
  case Literal(value, annotatedType) => annotatedType
  case Make(data, tag, targs, vargs) => data
  case PureApp(b, targs, vargs) => b.annotatedTpe match {
    case effekt.core.BlockType.Function(tparams, cparams, vparams, bparams, result) => result
    case effekt.core.BlockType.Interface(name, targs) => ValueType.Data(name, targs)
  }
  case ValueVar(id, annotatedType) => annotatedType
}

def monomorphize(clause: (Id, BlockLit), scrutineeTypes: Vector[ValueType])(using ctx: MonoContext): List[(Id, BlockLit)] = clause match
  case (id, BlockLit(List(), cparams, vparams, bparams, body)) => 
    val monoName = if (scrutineeTypes.isEmpty) {
      Some(id) 
    } else {
      ctx.names.get((id, scrutineeTypes map toTypeArg))
    }

    monoName match {
      case Some(name) => List((name, BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body))))
      case None => 
        // WARN: There is no mono name for some clause in the match
        //       This will happen for example in List[T] ( Cons(head: T, rest: List[T]), Nil() )
        //       if one of the constructors is never initialized and therefore there is no Constraint flowing into it
        //       in that case we can just reuse the original name, as it is never initialized
        List((id, BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body))))
    }
  case (id, BlockLit(tparams, cparams, vparams, bparams, body)) => 
    val newClauseNameMap = ctx.names.view.filterKeys((tid, groundTypes) => tid == id)
    newClauseNameMap.map((clauseKey, monoId) => 
      val replacementTparams = tparams.zip(clauseKey._2).toMap
      ctx.replacementTparams ++= replacementTparams
      val monoBlockLit: Block.BlockLit = BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body))
      (monoId, monoBlockLit)
    ).toList

def monomorphize(opt: Option[Stmt])(using ctx: MonoContext): Option[Stmt] = opt match
  case None => None
  case Some(stmt) => Some(monomorphize(stmt))

def monomorphize(expr: Expr)(using ctx: MonoContext): Expr = expr match
  case Literal(value, annotatedType) =>
    Literal(value, monomorphize(annotatedType))
  case PureApp(b, targs, vargs) =>
    PureApp(b, targs, vargs)
  case Make(data, tag, targs, vargs) =>
    // TODO: Is this the correct order to combine the two type args or the other way around
    val combinedTargs = data.targs ++ targs
    val typeArgs = combinedTargs map toTypeArg
    val replacementTag = ctx.names.get((tag, typeArgs.toVector)) match {
      case Some(value) => value
      case None => {
        // TODO: This is for debugging, can ideally be removed later because it doesn't happen :^)
        //       (for non-empty typeArgs)
        if (typeArgs.length > 0) {
          println(s"Could not find name for ${tag} with types ${typeArgs.toVector}")
          println(s"all names: ${ctx.allNames(tag)}")
        }
        tag
      }
    }
    Make(replacementDataFromTargs(data.name, combinedTargs), replacementTag, List.empty, vargs map monomorphize)
  case Box(b, annotatedCapture) => 
    Box(monomorphize(b), annotatedCapture)
  case ValueVar(id, annotatedType) =>
    ValueVar(id, monomorphize(annotatedType))


def monomorphize(valueParam: ValueParam)(using ctx: MonoContext): ValueParam = valueParam match 
  case ValueParam(id, tpe) => ValueParam(id, monomorphize(tpe))

def monomorphize(blockParam: BlockParam)(using ctx: MonoContext): BlockParam = blockParam match
  case BlockParam(id, tpe, capt) => BlockParam(id, monomorphize(tpe), capt) 

def monomorphize(blockType: BlockType)(using ctx: MonoContext): BlockType = blockType match 
  case BlockType.Function(tparams, cparams, vparams, bparams, result) => 
    BlockType.Function(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(result))
  // FIXME: Is this correct? 
  case BlockType.Interface(name, targs) => BlockType.Interface(name, targs map monomorphize)

def monomorphize(valueType: ValueType)(using ctx: MonoContext): ValueType = valueType match
  case ValueType.Var(name) => monomorphize(ctx.replacementTparams(name))
  case ValueType.Data(name, targs) => replacementDataFromTargs(name, targs)
  case ValueType.Boxed(tpe, capt) => ValueType.Boxed(monomorphize(tpe), capt)

def monomorphize(typeArg: TypeArg)(using ctx: MonoContext): ValueType = typeArg match {
  case TypeArg.Base(tpe, targs) => ValueType.Data(tpe, targs map monomorphize)
  case TypeArg.Boxed(tpe, capt) => ValueType.Boxed(tpe, capt)
  case TypeArg.Var(funId, pos) => 
    // FIXME: Do we want to reflect this unreachability in the Data structure used for monomorphizing?
    //        we would need another version of TypeArg that disallows targs in Base to be anything other than Ground
    throw new RuntimeException(s"All the vars should have been removed in the solving stage, still got '${typeArg}'")
}

var monoCounter = 0
def freshMonoName(baseId: Id, tpe: Ground): Id =
  monoCounter += 1
  tpe match {
    case TypeArg.Base(tpe, targs) => 
      Id(baseId.name.name + tpe.name.name + monoCounter)
    case TypeArg.Boxed(tpe, capt) =>
      // TODO: Fix naming 
      Id(baseId.name.name + "BOXED" + monoCounter)
  }

def freshMonoName(baseId: Id, tpes: Vector[Ground]): Id =
  if (tpes.length == 0) return baseId

  monoCounter += 1
  val tpesString = tpes.map({
    case TypeArg.Base(tpe, targs) => tpe.name.name
    // TODO: Fix naming
    case TypeArg.Boxed(tpe, capt) => "BOXED"
  }).mkString
  Id(baseId.name.name + tpesString + monoCounter)

def replacementDataFromTargs(id: FunctionId, targs: List[ValueType])(using ctx: MonoContext): ValueType.Data =
  if (targs.isEmpty) return ValueType.Data(id, targs)

  val baseTypes: List[Ground] = targs map toTypeArg

  val nameOpt = ctx.names.get((id, baseTypes.toVector))
  nameOpt match {
    // If we have a monomorphized name stored, then this Type can be replaced with that
    case Some(name) => ValueType.Data(name, List.empty)
    // If there is a polymorphic type with no name stored, then it SHOULD be an extern type
    // We can't really do anything about those and have to leave them
    // FIXME: Currently these might also be types that are not extern but something like the `Exists` type in `exists.effekt`
    case None => 
      // println(s"expecting extern type for '${id}', '${baseTypes.toVector}'")
      ValueType.Data(id, targs)
  }

def toTypeArg(vt: ValueType)(using ctx: MonoContext): Ground = vt match 
  case ValueType.Data(name, targs) => TypeArg.Base(name, targs map toTypeArg)
  case ValueType.Var(name) => ctx.replacementTparams(name)
  case ValueType.Boxed(tpe, capt) => TypeArg.Boxed(tpe, capt)

