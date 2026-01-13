package effekt
package core

import effekt.context.Context
import effekt.util.messages.ErrorMessageReifier
import effekt.core.Type.functionType

object Mono extends Phase[CoreTransformed, CoreTransformed] {

  override val phaseName: String = "mono"

  override def run(input: CoreTransformed)(using Context): Option[CoreTransformed] = {
    input match {
      case CoreTransformed(source, tree, mod, core) => {
        core match {
          case ModuleDecl(path, includes, declarations, externs, definitions, exports) => {
            // println(util.show(core))
            // Find constraints in the definitions
            val monoFindContext = MonoFindContext()
            val dctx = DeclarationContext(declarations, externs)
            var constraints = findConstraints(definitions)(using monoFindContext)
            constraints = constraints ++ declarations.flatMap(findConstraints(_)(using monoFindContext))
            // println("Constraints")
            // constraints.filter(c => c.lower.nonEmpty).toSet.foreach(c => println(c))
            // println()

            // Solve collected constraints
            val solution = solveConstraints(constraints)
            // println("Solved")
            // solution.foreach(println)
            // println()

            // Monomorphize existing definitions
            var monoFunNames: MonoFunNames = Map.empty
            var monoTpeNames: MonoTpeNames = collection.mutable.Map.empty
            solution.foreach((id, targs) => 
              if (dctx.findExternDef(id).isDefined) {
                // We can't rename externs, so the name will always stay the same
                targs.foreach(vb => monoFunNames += ((id, vb) -> id))
              } else if (dctx.findData(id).isDefined) {
                // Id is data, do renaming of non-extern types
                // Option[Int] => Option_Int[]
                // Array[Option[Int]] => Array[Option_Int]
                val data = dctx.findData(id).get
                targs.foreach(vb => 
                  freshMonoTypeName(data.id, vb, monoTpeNames)
                )
              } else {
                // Has to be function, do normal renaming
                targs.foreach(vb => 
                  monoFunNames += ((id, vb) -> freshMonoName(id, vb))
                )
              }
            )

            // If a declaration was not in our solution it should still be added to our Fun / Tpe names unchanged
            // this way everything can be handled the same and there is no need for 'getOrElse' for names
            declarations.foreach({
              case Data(id, List(), constructors) => monoTpeNames += ((id, Vector.empty) -> ValueType.Data(id, List.empty))
              case Interface(id, List(), properties) => monoFunNames += ((id, Vector.empty) -> id)
              case _ => ()
            })

            // println("TypeNames")
            // monoTpeNames.foreach(println)
            // println()
            // println("FunNames")
            // monoFunNames.foreach(println)
            // println()

            // Collect polymorphic extern definitions
            var polyExternDefs: List[Id] = List.empty 
            externs.foreach {
              case Extern.Include(featureFlag, contents) => ()
              case Extern.Def(id, List(), cparams, vparams, bparams, ret, annotatedCapture, body) => ()
              case Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, body) => polyExternDefs :+= id
            }

            var monoContext = MonoContext(solution, monoFunNames, monoTpeNames, polyExternDefs)
            val monoDecls = declarations flatMap (monomorphize(_)(using monoContext))
            val monoDefs = monomorphize(definitions)(using monoContext)(using Context, dctx)
            // monoDecls.foreach(decl => println(util.show(decl)))
            // println()
            // monoDefs.foreach(defn => println(util.show(defn)))
            val newModuleDecl = ModuleDecl(path, includes, monoDecls, externs, monoDefs, exports)

            // println(util.show(newModuleDecl))

            return Some(CoreTransformed(source, tree, mod, newModuleDecl))
          }
        }
      }
    }
    Some(input)
  }
}

type FunctionId = Id
case class MonoConstraint(lower: Vector[TypeArg], upper: FunctionId)
type MonoConstraints = List[MonoConstraint]

type Ground = TypeArg.Base | TypeArg.Boxed
type Solution = Map[FunctionId, Set[Vector[Ground]]]
type MonoFunNames = Map[(FunctionId, Vector[Ground]), FunctionId]
// Option[Int] => Option_Int[]
// Array[Option[Int]] => Array[Option_Int]
//       -----------           ----------
//          ground                mono
type MonoTpeNames = collection.mutable.Map[(Id, Vector[Ground]), ValueType.Data]


enum TypeArg {
  case Base(tpe: Id, targs: List[TypeArg])
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

case class MonoContext(solution: Solution, funNames: MonoFunNames, tpeNames: MonoTpeNames, polyExternDefs: List[Id]) {
  var replacementTparams: Map[Id, Ground] = Map.empty
  def isPolyExtern(id: Id) = polyExternDefs.contains(id)
}

def findConstraints(definitions: List[Toplevel])(using MonoFindContext): MonoConstraints =
  definitions flatMap findConstraints

def findConstraints(definition: Toplevel)(using ctx: MonoFindContext): MonoConstraints = definition match
  case Toplevel.Def(id, BlockLit(tparams, cparams, vparams, bparams, body)) => 
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
    findConstraints(body)
  case Toplevel.Def(id, block) => 
    findConstraints(block)
  case Toplevel.Val(id, binding) => 
    findConstraints(binding)

def findConstraints(declaration: Declaration)(using ctx: MonoFindContext): MonoConstraints = declaration match
  // Maybe[T] { Just[](x: T) }
  case Data(id, tparams, constructors) =>
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
    constructors.map{ constr =>
      val arity = tparams.size // + constr.tparams.size
      val constructorArgs = (0 until arity).map(index =>
        TypeArg.Var(constr.id, index) // Just.0  
      ).toVector // < Just.0 >
      MonoConstraint(constructorArgs, id) // < Just.0 > <: Maybe
    }
  case Interface(id, tparams, properties) => 
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
    properties flatMap findConstraints

def findConstraints(property: Property)(using ctx: MonoFindContext): MonoConstraints = property match {
  case Property(id, tpe@BlockType.Function(tparams, cparams, vparams, bparams, result)) => findConstraints(tpe, id)
  case Property(id, tpe@BlockType.Interface(name, targs)) => findConstraints(tpe)
}

def findConstraints(block: Block)(using ctx: MonoFindContext): MonoConstraints = block match
  case BlockVar(id, annotatedTpe: BlockType.Interface, annotatedCapt) => findConstraints(annotatedTpe)
  case BlockVar(id, annotatedTpe: BlockType.Function, annotatedCapt) => findConstraints(annotatedTpe, id)
  case BlockLit(tparams, cparams, vparams, bparams, body) => findConstraints(body)
  case Unbox(pure) => findConstraints(pure)
  case New(impl) => findConstraints(impl)

def findConstraints(blockType: BlockType.Interface)(using ctx: MonoFindContext): MonoConstraints = blockType match
  case BlockType.Interface(name, targs) => 
    val (newTargs, constraints) = findConstraints(targs)
    List(MonoConstraint(newTargs.toVector, name)) ++ constraints

def findConstraints(blockType: BlockType.Function, fnId: Id)(using ctx: MonoFindContext): MonoConstraints = blockType match
  case BlockType.Function(tparams, cparams, vparams, bparams, result) => 
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, fnId))
    List()

def findConstraints(impl: Implementation)(using ctx: MonoFindContext): MonoConstraints = impl match 
  case Implementation(interface, operations) => 
    findConstraints(interface) ++
    (operations flatMap findConstraints)

def findConstraints(operation: Operation)(using ctx: MonoFindContext): MonoConstraints = operation match
  case Operation(name, tparams, cparams, vparams, bparams, body) => 
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, name))
    findConstraints(body)

def findConstraints(constructor: Constructor)(using ctx: MonoFindContext): MonoConstraints = constructor match
  case Constructor(id, tparams, List()) => List.empty
  case Constructor(id, tparams, fields) =>
    val (newTargs, constraints) = findConstraints(fields map (_.tpe))
    List(MonoConstraint(newTargs.toVector, id)) ++ constraints

def findConstraints(stmt: Stmt)(using ctx: MonoFindContext): MonoConstraints = stmt match
  case Let(id, binding, body) => findConstraints(binding) ++ findConstraints(body)
  case Return(expr) => findConstraints(expr)
  case Val(id, binding, body) => findConstraints(binding) ++ findConstraints(body)
  case Var(ref, init, capture, body) => findConstraints(body)
  case ImpureApp(id, callee, targs, vargs, bargs, body) =>
    val (newTargs, constraints) = findConstraints(targs)
    List(MonoConstraint(newTargs.toVector, callee.id)) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints) ++ findConstraints(body) ++ constraints
  case App(callee: BlockVar, targs, vargs, bargs) => 
    val (newTargs, constraints) = findConstraints(targs)
    List(MonoConstraint(newTargs.toVector, callee.id)) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints) ++ constraints
  // TODO: Very specialized, but otherwise passing an id that matches in monomorphize is hard
  //       although I'm not certain any other case can even happen 
  // TODO: part 2, also update the implementation in monomorphize if changing this
  case App(Unbox(ValueVar(id, annotatedType)), targs, vargs, bargs) => 
    val (newTargs, constraints) = findConstraints(targs)
    List(MonoConstraint(newTargs.toVector, id)) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints) ++ constraints
  case App(callee, targs, vargs, bargs) =>
    findConstraints(callee) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints)
  // TODO: Maybe need to do something with methodTpe
  case Invoke(callee: BlockVar, method, methodTpe, targs, vargs, bargs) => 
    val (newTargs, constraints) = findConstraints(targs)
    List(MonoConstraint(newTargs.toVector, method)) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints) ++ constraints
  case Invoke(Unbox(ValueVar(id, annotatedType)), method, methodTpe, targs, vargs, bargs) =>
    val (newTargs, constraints) = findConstraints(targs)
    List(MonoConstraint(newTargs.toVector, method)) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints) ++ constraints
  case Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
    findConstraints(callee) ++ vargs.flatMap(findConstraints) ++ bargs.flatMap(findConstraints)
  case Reset(body) => findConstraints(body)
  case If(cond, thn, els) => findConstraints(cond) ++ findConstraints(thn) ++ findConstraints(els)
  case Def(id, BlockLit(tparams, cparams, vparams, bparams, bbody), body) => 
    tparams.zipWithIndex.foreach(ctx.extendTypingContext(_, _, id))
    findConstraints(bbody) ++ findConstraints(body)
  case Def(id, block, body) => 
    findConstraints(block) ++ findConstraints(body)
    // FIXME: Handle k as well
  case Shift(prompt, k, body) => findConstraints(prompt) ++ findConstraints(body)
  case Match(scrutinee, tpe, clauses, default) => 
    // FIXME: Handle tpe ? 

    // TODO: This is probably not technically correct, but allows for fun stuff like
    // type Foo[A] {
    //   Bar[B](x: B)
    // }
    // to be monomorphized (in special cases (?))
    var additionalConstraint: MonoConstraints = List()
    clauses.foreach((id, bl) => { bl match {
        case BlockLit(tparams, cparams, vparams, bparams, App(callee: BlockVar, targs, vargs, bargs)) => 
          if (targs.size == tparams.size) {
            additionalConstraint +:= MonoConstraint(tparams.zipWithIndex.map((_, paramIndex) => TypeArg.Var(id, paramIndex)).toVector, callee.id)
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
  case Hole(tpe, span) => List.empty

def findConstraints(opt: Option[Stmt])(using ctx: MonoFindContext): MonoConstraints = opt match 
  case None => List.empty
  case Some(stmt) => findConstraints(stmt)

def findConstraints(expr: Expr)(using ctx: MonoFindContext): MonoConstraints = expr match
  case PureApp(b, targs, vargs) => 
    val (newTargs, constraints) = findConstraints(targs)
    MonoConstraint(newTargs.toVector, b.id) :: constraints
  case ValueVar(id, annotatedType) => List.empty
  case Literal(value, annotatedType) => List.empty
  case Make(data, tag, targs, vargs) => 
    val combinedTargs = data.targs ++ targs
    val (newTargs, constraints) = findConstraints(combinedTargs)
    List(MonoConstraint(newTargs.toVector, tag)) ++ // <Int> <: Just
    constraints
  case Box(b, annotatedCapture) => 
    findConstraints(b)

def findConstraints(vts: List[ValueType])(using ctx: MonoFindContext): (List[TypeArg], MonoConstraints) = {
  val vtFindConstraints = vts map findConstraints
  val targs = vtFindConstraints.map(_._1)
  val constraints = vtFindConstraints.flatMap(_._2)
  (targs, constraints)
}

def findConstraints(vt: ValueType)(using ctx: MonoFindContext): (TypeArg, MonoConstraints) = vt match {
  case ValueType.Boxed(tpe@BlockType.Function(tparams, cparams, vparams, bparams, result), capt) => {
    // TODO: Perhaps recurse into tpe
    // TODO: What do I do with a function type here? It does not have a name which does not work for my current findConstraints
    (TypeArg.Boxed(tpe, capt), List.empty)
  }
  case ValueType.Boxed(tpe@BlockType.Interface(name, targs), capt) => {
    val constraints = findConstraints(tpe)
    (TypeArg.Boxed(tpe, capt), constraints)
  }
  case ValueType.Data(name, targs) => {
    val (newTargs, constraints) = findConstraints(targs)
    val additionalConstraints = if (!newTargs.isEmpty) {
      List(MonoConstraint(newTargs.toVector, name))
    } else {
      List.empty
    }
    (TypeArg.Base(name, newTargs), constraints ++ additionalConstraints)
  }
  case ValueType.Var(name) => (ctx.typingContext(name), List.empty)
}

def filterBounds(bounds: Map[Id, Set[Vector[TypeArg]]]): Map[Id, Set[Vector[Ground]]] = bounds.view.mapValues(filterNonGround).toMap

def filterNonGround(bounds: Set[Vector[TypeArg]]): Set[Vector[Ground]] = bounds.flatMap(filterNonGround)

def filterNonGround(bound: Vector[TypeArg]): Option[Vector[Ground]] = {
  var res: Vector[Ground] = Vector.empty 
  bound.foreach({
    case TypeArg.Base(id, targs) => res :+= TypeArg.Base(id, targs)
    case TypeArg.Boxed(tpe, capt) => res :+= TypeArg.Boxed(tpe, capt)
    case TypeArg.Var(funId, pos) => ()
  })
  if (res.size == bound.size) {
    Some(res)
  } else {
    None
  }
}

def solveConstraints(constraints: MonoConstraints)(using Context): Solution = {
  val filteredConstraints = constraints.filterNot(c => c.lower.isEmpty)
  val groupedConstraints = filteredConstraints.groupBy(c => c.upper)
  var bounds = groupedConstraints.map((sym, constraints) => (sym -> constraints.map(c => c.lower).toSet))

  while (true) {
    val previousBounds = bounds
    bounds.foreach((sym, tas) => 
      val bound = solveConstraints(sym, tas).filter(v => v.nonEmpty)
      bounds += (sym -> bound)
    )
    
    if (previousBounds == bounds) return filterBounds(bounds)
  }

  def solveConstraints(funId: FunctionId, filteredConstraints: Set[Vector[TypeArg]]): Set[Vector[TypeArg]] =
    var nbs: Set[List[TypeArg]] = Set.empty
    filteredConstraints.foreach(b => 
      var l: List[List[TypeArg]] = List(List.empty)

      def solveTypeArg(typeArg: TypeArg, taPos: Int, insideTypeConstructor: Boolean): List[TypeArg] = typeArg match {
        case TypeArg.Base(tpe, targs) => 
          val solvedTargs = targs.zipWithIndex.map((ta, ind) => solveTypeArg(ta, ind, true))
          var crossTargs: List[List[TypeArg]] = List(List.empty)
          solvedTargs.foreach(targ => {
            crossTargs = productAppend(crossTargs, targ)
          })

          crossTargs.map(tas => TypeArg.Base(tpe, tas))
        case TypeArg.Boxed(tpe, capt) => List(TypeArg.Boxed(tpe, capt))
        case TypeArg.Var(fnId, pos) => 
          if (funId == fnId && taPos == pos && insideTypeConstructor) Context.abort(pretty"Detected polymorphic recursion for '${funId}' at position '${taPos}'")
          val funSolved = bounds.getOrElse(fnId, Set.empty)
          val posArgs = funSolved.map(v => v(pos)).toList
          posArgs
      }

      b.zipWithIndex.foreach((typeArg, ind) => l = productAppend(l, solveTypeArg(typeArg, ind, false)))
      
      // Only add solution vectors which match the size we expect
      // sometimes we don't have enough information in the current iteration to find a correct solution
      // nbs ++= l.filter(i => i.size == b.size)
      nbs ++= l
    )
    nbs.map(l => l.toVector)

  // we will never get here
  filterBounds(bounds)
}

def productAppend[A](ls: List[List[A]], rs: List[A]): List[List[A]] =
  if (rs.isEmpty) return ls
  for { l <- ls; r <- rs } yield l :+ r

def monomorphize(definitions: List[Toplevel])(using ctx: MonoContext)(using Context, DeclarationContext): List[Toplevel] =
  var newDefinitions: List[Toplevel] = List.empty
  definitions.foreach(definition => newDefinitions ++= monomorphize(definition))
  newDefinitions

def monomorphize(toplevel: Toplevel)(using ctx: MonoContext)(using Context, DeclarationContext): List[Toplevel] = toplevel match
  case Toplevel.Def(id, BlockLit(List(), cparams, vparams, bparams, body)) => 
    List(Toplevel.Def(id, BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body))))
  case Toplevel.Def(id, BlockLit(tparams, cparams, vparams, bparams, body)) => 
    val monoTypes = ctx.solution(id).toList
    monoTypes.map(baseTypes => 
      val replacementTparams = tparams.zip(baseTypes).toMap
      ctx.replacementTparams ++= replacementTparams
      Toplevel.Def(ctx.funNames(id, baseTypes), BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body)))
    )
  case Toplevel.Def(id, block) => 
    List(Toplevel.Def(id, monomorphize(block)))
  case Toplevel.Val(id, binding) => 
    List(Toplevel.Val(id, monomorphize(binding)))

def monomorphize(decl: Declaration)(using ctx: MonoContext): List[Declaration] = decl match
  case Data(id, tparams, constructors) => 
    val monoTypes = ctx.solution.getOrElse(id, Set.empty).toList
    monoTypes.map(baseTypes =>
      val replacementTparams = tparams.zip(baseTypes).toMap
      ctx.replacementTparams ++= replacementTparams
      Declaration.Data(ctx.tpeNames(id, baseTypes).name, List.empty, constructors.flatMap(monomorphize))
    )
  case Interface(id, tparams, properties) =>
    val monoTypes = ctx.solution.getOrElse(id, Set.empty).toList
    if (monoTypes.isEmpty) {
      List(Declaration.Interface(id, tparams, properties))
    } else {
      monoTypes.map(baseTypes =>
        val replacementTparams = tparams.zip(baseTypes).toMap
        ctx.replacementTparams ++= replacementTparams
        val monoProp = properties.flatMap(monomorphize)
        val interfaceName = ctx.funNames(id, baseTypes)
        if (interfaceName == id) {
          Declaration.Interface(interfaceName, tparams, monoProp)
        } else {
          Declaration.Interface(interfaceName, List.empty, monoProp)
        }
      )
    }

def monomorphize(property: Property)(using ctx: MonoContext): List[Property] = property match {
  case Property(id, tpe@BlockType.Function(tparams, cparams, vparams, bparams, result)) => {
    val baseTypes = ctx.solution.getOrElse(id, Set.empty).toList
    baseTypes.map(baseType => {
      Property(ctx.funNames((id, baseType)), monomorphize(tpe)) 
    })
  }
  case Property(id, tpe) => ???
}

def monomorphize(constructor: Constructor)(using ctx: MonoContext): List[Constructor] = constructor match
  case Constructor(id, tparams, fields) => 
    val baseTypes = ctx.solution.getOrElse(id, Set.empty).toList
    baseTypes.map(baseType => {
      Constructor(ctx.funNames(id, baseType), List.empty, fields map monomorphize)
    })

def monomorphize(block: Block)(using ctx: MonoContext)(using Context, DeclarationContext): Block = block match
  case b: BlockLit => monomorphize(b)
  case b: BlockVar => monomorphize(b)
  case New(impl) => New(monomorphize(impl)) 
  case Unbox(pure) => Unbox(monomorphize(pure))

def monomorphize(impl: Implementation)(using ctx: MonoContext)(using Context, DeclarationContext): Implementation = impl match
  case Implementation(interface, operations) =>
    Implementation(monomorphize(interface), operations.flatMap(monomorphize))

def monomorphize(interface: BlockType.Interface)(using ctx: MonoContext): BlockType.Interface = interface match
  case BlockType.Interface(name, targs) => 
    val funName = replacementFun(name, targs)
    BlockType.Interface(funName, List.empty)

def monomorphize(operation: Operation)(using ctx: MonoContext)(using Context, DeclarationContext): List[Operation] = operation match
  case Operation(name, tparams, cparams, vparams, bparams, body) => 
    val monoTypes = ctx.solution.getOrElse(name, Set.empty).toList
    if (monoTypes.isEmpty) {
      List(Operation(name, List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body)))
    } else {
      monoTypes.map(baseTypes =>
        val replacementTparams = tparams.zip(baseTypes).toMap
        ctx.replacementTparams ++= replacementTparams
        Operation(ctx.funNames(name, baseTypes), List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body))
      )
    }
    

def monomorphize(block: BlockLit)(using ctx: MonoContext)(using Context, DeclarationContext): BlockLit = block match
  case BlockLit(tparams, cparams, vparams, bparams, body) => 
    BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body))

def monomorphize(block: BlockVar)(using ctx: MonoContext): BlockVar = block match
  case BlockVar(id, annotatedTpe, annotatedCapt) => BlockVar(id, monomorphize(annotatedTpe), annotatedCapt)

def monomorphize(field: Field)(using ctx: MonoContext): Field = field match
  case Field(id, tpe) => Field(id, monomorphize(tpe))

// FIXME: Not a big fan of this function needing so many extra parameters
def monomorphize(blockVar: BlockVar, replacementId: FunctionId, targs: List[ValueType])(using ctx: MonoContext): BlockVar = blockVar match
  case BlockVar(id, BlockType.Function(tparams, cparams, vparams, bparams, result), annotatedCapt) if ctx.isPolyExtern(id) => 
    val annotatedTpe = BlockType.Function(tparams, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(result))
    BlockVar(id, annotatedTpe, annotatedCapt)
  case BlockVar(id, BlockType.Function(tparams, cparams, vparams, bparams, result), annotatedCapt) => 
    val replacementTparams = tparams.zip(targs map toTypeArg).toMap
    ctx.replacementTparams ++= replacementTparams
    val monoAnnotatedTpe = BlockType.Function(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(result))
    BlockVar(replacementId, monoAnnotatedTpe, annotatedCapt)
  case BlockVar(id, annotatedTpe: BlockType.Interface, annotatedCapt) =>
    BlockVar(id, monomorphize(annotatedTpe), annotatedCapt)

def monomorphize(stmt: Stmt)(using ctx: MonoContext)(using Context, DeclarationContext): Stmt = stmt match
  case Return(expr) => 
    Return(monomorphize(expr))
  case Val(id, binding, body) => 
    Val(id, monomorphize(binding), monomorphize(body))
  case Var(ref, init, capture, body) => 
    Var(ref, monomorphize(init), capture, monomorphize(body))
  case ImpureApp(id, callee, targs, vargs, bargs, body) =>
    ImpureApp(id, callee, targs map monomorphize, vargs map monomorphize, bargs map monomorphize, monomorphize(body))
  case App(callee: BlockVar, targs, vargs, bargs) => 
    val monoFnId = replacementFun(callee.id, targs)
    App(monomorphize(callee, monoFnId, targs), List.empty, vargs map monomorphize, bargs map monomorphize)
  // TODO: Highly specialized, see todo in findConstraints for info
  //       change at the same time as findConstraints
  case App(Unbox(ValueVar(id, annotatedTpe)), targs, vargs, bargs) =>
    App(Unbox(ValueVar(id, monomorphize(annotatedTpe))), List.empty, vargs map monomorphize, bargs map monomorphize)
  case App(callee, targs, vargs, bargs) =>
    App(monomorphize(callee), List.empty, vargs map monomorphize, bargs map monomorphize)
  case Let(id, binding, body) => 
    Let(id, monomorphize(binding), monomorphize(body))
  case If(cond, thn, els) => 
    If(monomorphize(cond), monomorphize(thn), monomorphize(els))
  case Invoke(Unbox(pure), method, methodTpe, targs, vargs, bargs) =>
    Invoke(Unbox(monomorphize(pure)), method, methodTpe, List.empty, vargs map monomorphize, bargs map monomorphize)
  case Invoke(BlockVar(id, annotatedTpe, annotatedCapt), method, BlockType.Function(tparams, cparams, vparams, bparams, result), targs, vargs, bargs) => 
    val monoFnId = replacementFun(method, targs)
    val replacementTparams = tparams.zip(targs map toTypeArg).toMap
    ctx.replacementTparams ++= replacementTparams

    val monoAnnotatedTpe = BlockType.Function(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(result))
    Invoke(BlockVar(id, monomorphize(annotatedTpe), annotatedCapt), monoFnId, monoAnnotatedTpe, List.empty, vargs map monomorphize, bargs map monomorphize)
  case Invoke(callee, method, methodTpe, targs, vargs, bargs) =>
    Invoke(monomorphize(callee), method, methodTpe, List.empty, vargs map monomorphize, bargs map monomorphize)
  case Resume(k, body) =>
    Resume(monomorphize(k), monomorphize(body))
  case Reset(body) =>
    Reset(monomorphize(body))
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
        Stmt.Def(ctx.funNames(id, head), BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(bbody)), nestDefs(next))
      case Nil => monomorphize(body)
    }
    nestDefs(monoTypes)
  case Def(id, block, body) => 
    val monoBlock = monomorphize(block)
    val monoBody = monomorphize(body)
    Def(id, monoBlock, monoBody)
  case Shift(prompt, k, body) => 
    Shift(monomorphize(prompt), monomorphize(k), monomorphize(body))
  case Match(scrutinee, matchTpe, clauses, default) =>
    val monoScrutinee = monomorphize(scrutinee)

    // FIXME: Not correct in all cases. Have to figure out where this is needed
    // We need the type of the scrutinee, to give each clause the correct monomorphized name based on said type
    val monoScrutineeType = exprType(scrutinee) match {
      case ValueType.Data(name, targs) => 
        targs map monomorphize
      case _ => sys error "scrutinee type was not data"
    }

    val monoClauses = clauses.flatMap(monomorphize(_, monoScrutineeType.toVector))
    Match(monomorphize(scrutinee), monomorphize(matchTpe), monoClauses, monomorphize(default))
  case Get(id, annotatedTpe, ref, annotatedCapt, body) =>
    Get(id, monomorphize(annotatedTpe), ref, annotatedCapt, monomorphize(body))
  case Put(ref, annotatedCapt, value, body) =>
    Put(ref, annotatedCapt, monomorphize(value), monomorphize(body))
  case Alloc(id, init, region, body) =>
    Alloc(id, monomorphize(init), region, monomorphize(body))
  case Region(body) => 
    Region(monomorphize(body))
  case Hole(tpe, span) => 
    Hole(monomorphize(tpe), span)

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

def monomorphize(clause: (Id, BlockLit), scrutineeTypes: Vector[ValueType])(using ctx: MonoContext)(using Context, DeclarationContext): List[(Id, BlockLit)] = clause match
  case (id, BlockLit(List(), cparams, vparams, bparams, body)) => 
    val monoName = if (scrutineeTypes.isEmpty) {
      Some(id) 
    } else {
      ctx.funNames.get((id, scrutineeTypes map toTypeArg))
    }

    monoName match {
      case Some(name) => List((name, BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body))))
      case None => 
        // WARN: There is no mono name for some clause in the match
        //       This will happen for example in List[T] ( Cons(head: T, rest: List[T]), Nil() )
        //       if one of the constructors is never initialized and therefore there is no MonoConstraint flowing into it
        //       in that case we can just reuse the original name, as it is never initialized
        List((id, BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body))))
    }
  case (id, BlockLit(tparams, cparams, vparams, bparams, body)) => 
    val newClauseNameMap = ctx.funNames.view.filterKeys((tid, groundTypes) => tid == id)
    newClauseNameMap.map((clauseKey, monoId) => 
      val replacementTparams = tparams.zip(clauseKey._2).toMap
      ctx.replacementTparams ++= replacementTparams
      val monoBlockLit: Block.BlockLit = BlockLit(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(body))
      (monoId, monoBlockLit)
    ).toList

def monomorphize(opt: Option[Stmt])(using ctx: MonoContext)(using Context, DeclarationContext): Option[Stmt] = opt match
  case None => None
  case Some(stmt) => Some(monomorphize(stmt))

def monomorphize(expr: Expr)(using ctx: MonoContext)(using Context, DeclarationContext): Expr = expr match
  case Literal(value, annotatedType) =>
    Literal(value, monomorphize(annotatedType))
  case PureApp(b, targs, vargs) =>
    // val funTpe = b.functionType
    // val replacementTparams = funTpe.tparams.zip(targs map toTypeArg).toMap
    // ctx.replacementTparams ++= replacementTparams

    // val blockTpe = BlockType.Function(funTpe.tparams, funTpe.cparams, funTpe.vparams, funTpe.bparams map monomorphize, monomorphize(funTpe.result))
    // val blockVar: BlockVar = BlockVar(b.id, blockTpe, b.annotatedCapt)
    PureApp(b, targs map monomorphize, vargs map monomorphize)
  case Make(data, tag, targs, vargs) =>
    val combinedTargs = data.targs ++ targs
    val replacementTag = replacementFun(tag, combinedTargs)
    Make(replacementData(data.name, data.targs), replacementTag, List.empty, vargs map monomorphize)
  case Box(b, annotatedCapture) => 
    Box(monomorphize(b), annotatedCapture)
  case ValueVar(id, annotatedType) =>
    ValueVar(id, monomorphize(annotatedType))


def monomorphize(valueParam: ValueParam)(using ctx: MonoContext): ValueParam = valueParam match 
  case ValueParam(id, tpe) => ValueParam(id, monomorphize(tpe))

def monomorphize(blockParam: BlockParam)(using ctx: MonoContext): BlockParam = blockParam match
  case BlockParam(id, tpe, capt) => 
    BlockParam(id, monomorphize(tpe), capt) 

def monomorphize(blockType: BlockType)(using ctx: MonoContext): BlockType = blockType match {
  case BlockType.Function(tparams, cparams, vparams, bparams, result) => 
    BlockType.Function(List.empty, cparams, vparams map monomorphize, bparams map monomorphize, monomorphize(result))
  case BlockType.Interface(name, targs) => 
    val funName = ctx.funNames(name, (targs map toTypeArg).toVector)
    // Special case here if we have 'Resume' or 'Prompt' we didn't change the name which we can detect here
    // then we don't change the targs for typechecking to work
    if (funName == name) {
      BlockType.Interface(funName, targs)
    } else {
      BlockType.Interface(funName, List.empty)
    }
}

def monomorphize(valueType: ValueType)(using ctx: MonoContext): ValueType = valueType match {
  case ValueType.Var(name) => monomorphize(ctx.replacementTparams(name))
  case ValueType.Data(name, targs) => replacementData(name, targs map monomorphize)
  case ValueType.Boxed(tpe, capt) => ValueType.Boxed(monomorphize(tpe), capt)
}

def monomorphize(typeArg: TypeArg)(using ctx: MonoContext): ValueType = typeArg match {
  case TypeArg.Base(tpe, targs) => replacementData(tpe, targs map monomorphize)
  case TypeArg.Boxed(tpe, capt) => ValueType.Boxed(monomorphize(tpe), capt)
  case TypeArg.Var(funId, pos) => 
    // FIXME: Do we want to reflect this unreachability in the Data structure used for monomorphizing?
    //        we would need another version of TypeArg that disallows targs in Base to be anything other than Ground
    throw new RuntimeException(s"All the vars should have been removed in the solving stage, still got '${typeArg}'")
}

def freshMonoTypeName(dataName: Id, tpes: Vector[Ground], monoTypeNames: MonoTpeNames): ValueType.Data = {
  monoTypeNames.getOrElse((dataName, tpes), {
    var nameBuilder = StringBuilder(dataName.name.name)
    val valueTypes = tpes map {
      case TypeArg.Base(tpe, targs) => {
        // Safe `get`, because we are handling Vector[Ground] and just re-establishing this invariant,
        // because our types do not guarantee this
        val filteredTargs = filterNonGround(targs.toVector).get
        val innerData = freshMonoTypeName(tpe, filteredTargs, monoTypeNames)
        nameBuilder.append("_" + innerData.name.name.name)
        innerData
      }
      case TypeArg.Boxed(tpe, capt) => {
        ValueType.Boxed(tpe, capt)
      }
    }
    val freshId = Id(nameBuilder.toString())
    val monoData: ValueType.Data = ValueType.Data(freshId, List.empty)
    monoTypeNames += ((dataName, tpes) -> monoData)
    monoData
  })
} 

def freshMonoName(baseId: Id, tpes: Vector[Ground]): Id = {
  if (tpes.length == 0) return baseId

  // Keep the ids of 'Resume' and 'Prompt', so we can detect this case and make typechecking work later 
  // also see monomorphize(blockType: BlockType)
  if (baseId == core.Type.ResumeSymbol || baseId == core.Type.PromptSymbol) return baseId
  
  val tpesString = tpes.map({
    case TypeArg.Base(tpe, targs) => tpe.name.name
    // TODO: Fix naming
    case TypeArg.Boxed(tpe, capt) => "BOXED"
  }).mkString
  Id(baseId.name.name + tpesString)
}

def replacementFun(id: FunctionId, targs: List[ValueType])(using ctx: MonoContext): FunctionId = {
  if (targs.isEmpty) return id
  val baseTypes: Vector[Ground] = (targs map toTypeArg).toVector
  ctx.funNames(id, baseTypes)
}

def replacementData(id: Id, targs: List[ValueType])(using ctx: MonoContext): ValueType.Data = {
  if (targs.isEmpty) return ValueType.Data(id, List.empty)
  val baseTypes: Vector[Ground] = (targs map toTypeArg).toVector

  // we do not know anything about extern types
  // therefore we need to rely on types being extern if they are not contained in the names
  // extern types should really be contained in the Module externs 
  ctx.tpeNames.getOrElse((id, baseTypes), {
    ValueType.Data(id, targs)
  })
}

def toTypeArg(vt: ValueType)(using ctx: MonoContext): Ground = vt match {
  case ValueType.Data(name, targs) => TypeArg.Base(name, targs map toTypeArg)
  case ValueType.Var(name) => ctx.replacementTparams(name)
  case ValueType.Boxed(tpe, capt) => TypeArg.Boxed(tpe, capt)
}