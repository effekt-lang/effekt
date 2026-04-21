package effekt
package core

import effekt.core.BlockType.Function
import effekt.core.ExternBody.StringExternBody
import effekt.source.FeatureFlag.Default
import effekt.symbols.Capture
import munit.Clue.generate

class TRMCTests extends CoreTests {
  
  case class ImpossibleStateError(message: String) extends RuntimeException(message: String)

  def freeInExpr(id:Id, expr: Expr): Boolean = expr.free.freeIds.contains(id)
  
  val listId = Id("List")
  val consId = Id("Cons")
  val nilId = Id("Nil")
  val headId = Id("head")
  val tailId = Id("tail")
  val TList: ValueType.Data = ValueType.Data(listId, Nil)
  
  //type List {
  // Nil
  // Cons(head: Int, tail: List)
  //}
  val listDecl = Declaration.Data(listId, Nil, List(
    Constructor(nilId, Nil, Nil),
    Constructor(consId, Nil, List(
      Field(headId, Type.TInt),
      Field(tailId, TList)
    ))
  ))
  val holeCtxId = Id("HoleContext")
  def HoleContext(a: ValueType, b: ValueType): ValueType = ValueType.Data(holeCtxId, List(a, b))
  
  val vTpeIdA = Id("A")
  val vTpeIdB = Id("B")
  val vTpeIdC = Id("C")

  val ctxEmtpyId = Id("ctx_emptyContext")
  val ctxApplyId = Id("ctx_applyContext")
  val ctxcomposeId = Id("ctx_composeContext")

  def emptyfun: Extern = Extern.Def(ctxEmtpyId, List(vTpeIdA), Nil, Nil, Nil, HoleContext(ValueType.Var(vTpeIdA), ValueType.Var(vTpeIdA)), Set.empty, StringExternBody(Default(null), null))

  def applyfun: Extern = Extern.Def(ctxApplyId, List(vTpeIdA,vTpeIdB), Nil, List(ValueParam(Id("ctx"),HoleContext(ValueType.Var(vTpeIdA),ValueType.Var(vTpeIdB))),ValueParam(Id("value"),ValueType.Var(vTpeIdB))), Nil, ValueType.Var(vTpeIdA), Set.empty, StringExternBody(Default(null), null))

  def composefun: Extern = Extern.Def(ctxcomposeId, List(vTpeIdA, vTpeIdB, vTpeIdC), Nil, List(ValueParam(Id("ctx1"), HoleContext(ValueType.Var(vTpeIdA), ValueType.Var(vTpeIdB))), ValueParam(Id("ctx2"), HoleContext(ValueType.Var(vTpeIdB),ValueType.Var(vTpeIdC)))), Nil, HoleContext(ValueType.Var(vTpeIdA),ValueType.Var(vTpeIdC)), Set.empty, StringExternBody(Default(null), null))


  val DC = DeclarationContext(List(listDecl), List(emptyfun,applyfun,composefun))

  val mainSymbol = Id("main")

  def assertTransformsTo(
                          input: String,
                          transformed: String,
                          names: Names = Names(defaultNames + ("main" -> mainSymbol))
                        )(transform: ModuleDecl => ModuleDecl)(using munit.Location) = {
    val moduleHeader =
      """module test
        |
        |""".stripMargin
    val pInput = parse(moduleHeader + input, "input", names)
    val pExpected = parse(moduleHeader + transformed, "expected", names)

    // the parser is not assigning symbols correctly, so we need to run renamer first
    val renamer = TestRenamer(names)
    val renamed = renamer(pInput)

    val obtained = transform(renamed)
    assertAlphaEquivalent(obtained, pExpected, "Not transformed to")
  }

//  def transform(input: String, expected: String)(using munit.Location) = {
//    assertTransformsTo(input, expected) { tree =>
//      tree
//    }
//  }
  enum TransformContext {
    case Outer(id: Id)
    case Val(id:Id, body: Stmt, next: TransformContext)
  }
  
  enum TailContext {
    case Empty
    case Outer(id: Id)
    case Make(data: ValueType.Data, tag: Id, targs: List[ValueType], before: List[Expr], after: List[Expr])
    case Compose(first: TailContext, second: TailContext)
  }



  def split(context: TransformContext): (TailContext, Option[TransformContext]) = context match {
    case TransformContext.Outer(id) => (TailContext.Outer(id), None)
    case TransformContext.Val(id, Stmt.Return(Expr.Make(data, tag, targs, head :: Expr.ValueVar(id2, tpe) :: Nil)), next) //TODO: leads to always a compose TailContext, even if its only one Val
          if tag == consId && id == id2 =>
      val (init, rest) = split(next)
      (TailContext.Compose(init, TailContext.Make(data, tag, targs, List(head), Nil)), rest)
    case _ => (TailContext.Empty, Some(context))
  }
  
  def transform(input: Stmt, inputfun: Id, outputfun: Id, outputTpe: ValueType, context: TransformContext, outerContextTpe: ValueType) : Stmt = input match { //TODO: is OutputTpe necessary?
    case Stmt.Def(id, block, body) => ???
    case Stmt.Let(id, binding, body) => ???
    case Stmt.ImpureApp(id, callee, targs, vargs, bargs, body) => ???
    case Stmt.Return(expr) => ???
    case Stmt.Val(id, binding, body) => transform(binding, inputfun, outputfun, outputTpe, TransformContext.Val(id,body,context), outerContextTpe)
    case Stmt.App(callee, targs, vargs, bargs) => callee match {
      case Block.BlockVar(id, annotatedTpe, annotatedCapt) => 
        if (id == inputfun) {
          val (init, rest) = split(context)
          annotatedTpe match {
            case Function(tparams, cparams, vparams, bparams, result) => 
              val inner = Stmt.App(
                Block.BlockVar(
                  outputfun,
                  Function(tparams, cparams, vparams.appended(outerContextTpe), bparams, result), annotatedCapt),//TODO:capt? 
                targs,
                vargs.appended(innerReify(init, outerContextTpe)),
                bargs)
              rest match {
                case Some(rest) => reify(inner, rest, inputfun, outputfun, outerContextTpe)
                case None => inner
              }
            case _ => throw ImpossibleStateError("in an App() Statement a Function should be called")
          }
        }else{
          reify(input, context, inputfun, outputfun, outerContextTpe)
        }
      case Block.BlockLit(tparams, cparams, vparams, bparams, body) => ???
      case Block.Unbox(pure) => ???
      case Block.New(impl) => ???
    }
    case Stmt.Invoke(callee, method, methodTpe, targs, vargs, bargs) => ???
    case Stmt.If(cond, thn, els) => 
      if(freeInExpr(inputfun, cond)){
        ??? //TODO: probably give up
      }else{
        Stmt.If(cond,
          transform(thn, inputfun, outputfun, outputTpe, context, outerContextTpe),
          transform(els, inputfun, outputfun, outputTpe, context, outerContextTpe))
      }
    case Stmt.Match(scrutinee, annotatedTpe, clauses, default) => ???
    case Stmt.Region(body) => ???
    case Stmt.Alloc(id, init, region, body) => ???
    case Stmt.Var(ref, init, capture, body) => ???
    case Stmt.Get(id, annotatedTpe, ref, annotatedCapt, body) => ???
    case Stmt.Put(ref, annotatedCapt, value, body) => ???
    case Stmt.Reset(body) => ???
    case Stmt.Shift(prompt, k, body) => ???
    case Stmt.Resume(k, body) => ???
    case Stmt.Hole(annotatedTpe, span) => ???
  }
  
  def reify(stmt: Stmt, context: TransformContext, inputfun: Id, outputfun: Id, outerContextTpe: ValueType) : Stmt = context match {
    case TransformContext.Outer(id) =>  
      val tmpId = Id("tmp")
      Stmt.Val(tmpId, stmt, Return(PureApp(blockVarFromExternDef(ctxApplyId), Nil, List(ValueVar(id, outerContextTpe), ValueVar(tmpId, stmt.tpe))))) //TODO: fix ctxType,tmpType?
    case TransformContext.Val(id, body, next) => 
      Stmt.Val(id, stmt, transform(body, inputfun, outputfun, body.tpe, next, outerContextTpe))
  }
  
  def innerReify(context: TailContext, outerContextTpe: ValueType): Expr = context match {
    case TailContext.Empty => PureApp(blockVarFromExternDef(ctxEmtpyId),Nil,Nil) //TODO: tparams should be A, but outerContextTpe is HoleContext(A,???)
    case TailContext.Outer(id) => Expr.ValueVar(id,outerContextTpe)
    case TailContext.Make(data, tag, targs, before, after) => MakeContext(data, tag, targs, before, after)
    case TailContext.Compose(first, second) => PureApp(blockVarFromExternDef(ctxcomposeId), Nil, List(innerReify(first, outerContextTpe),innerReify(second, outerContextTpe)))
  }
  
  def blockVarFromExternDef(id: Id) : Block.BlockVar = {
    DC.getExternDef(id) match {
      case Extern.Def(id, tparams, cparams, vparams, bparams, ret, annotatedCapture, body) => BlockVar(id, Function(tparams, cparams, vparams.map(getType), bparams.map(getType), ret), annotatedCapture)
    }
  }

  def getType(vparam: ValueParam): ValueType = vparam.tpe
  def getType(bparam: BlockParam): BlockType = bparam.tpe
  
  
  test("simple function call") {

    val n: Id = Id("n")
    val f: Id = Id("f")
    val f2: Id = Id("f2")
    val ctx: Id = Id("ctx")
    val A: Id = Id("A")
    val ATpe: ValueType = ValueType.Var(A)
    val fType: BlockType = Function(Nil, Nil, List(Type.TInt), Nil, Type.TInt)
    val fCapt: Captures = Set.empty

    // (n: Int) => n
    val inputTree = BlockLit(Nil, Nil, List(ValueParam(n, Type.TInt)), Nil,
      Stmt.Return(Expr.ValueVar(n, Type.TInt)))

    // return n
    //Stmt.Return(Expr.ValueVar(n, Type.TInt))

    // f(n)
    val app = Stmt.App(Block.BlockVar(f, fType, fCapt), Nil, List(Expr.ValueVar(n, Type.TInt)), Nil)
    
    // f2(n,ctx)
    val appexpected = Stmt.App(Block.BlockVar(f2, Function(Nil, Nil, List(Type.TInt, HoleContext(ATpe,Type.TInt)), Nil, Type.TInt), fCapt), Nil, List(Expr.ValueVar(n, Type.TInt),Expr.ValueVar(ctx,HoleContext(ATpe,Type.TInt))), Nil)

    val apptransformed = transform(app, f, f2, Type.TInt, TransformContext.Outer(ctx), HoleContext(ATpe,Type.TInt))

//    val input =
//      """def f(n: Int) =
//        |  (f : (Int) => Int @ {})((sub: (Int, Int) => Int @ {})(n:Int, 1))
//        |""".stripMargin
//
//    val expected =
//      """def f2(n: Int, ctx: Context[Int]) =
//        |  (f2 : (Int) => Int @ {})((sub: (Int, Int) => Int @ {})(n:Int, 1), ctx: Context[Int])
//        |""".stripMargin

    //transform(input, expected)
    assertAlphaEquivalentStatements(apptransformed,appexpected)
  }
  
  test("infinite List"){

    val n: Id = Id("n")
    val f: Id = Id("f")
    val f2: Id = Id("f2")
    val ctx: Id = Id("ctx")
    val A: Id = Id("A")
    val ATpe: ValueType = ValueType.Var(A)
    //val listType = ValueType.Var(Id("List"))
    val fType: BlockType = Function(Nil, Nil, List(Type.TInt), Nil, TList)
    val fCapt: Captures = Set.empty
    val sub: BlockVar = BlockVar(Id("infixSub"),Function(Nil,Nil,List(Type.TInt,Type.TInt),Nil,Type.TInt),Set.empty)
    //val composefun: BlockVar = BlockVar(Id("ctx_composeContext"),Function(Nil,Nil,List(ctxType,ctxType),Nil,ctxType),Set.empty)
    //val ctxfun: BlockVar = ???
    
    
    val tmpId = Id("tmp")
    //return Cons(n,tmp)
    val retStmt = Stmt.Return(Make(TList,consId,List(Type.TInt,Type.TInt),List(ValueVar(n,Type.TInt),ValueVar(tmpId,TList))))
    //f(n-1)
    val callStmt = Stmt.App(Block.BlockVar(f, fType, fCapt), Nil, List(Expr.PureApp(sub,List(Type.TInt,Type.TInt),List(ValueVar(n,Type.TInt),Literal(1,Type.TInt)))),Nil)
    //val tmp= f(n-1)
    //return Cons(n,tmp)
    val inputTree = Stmt.Val(tmpId,callStmt,retStmt)
    
    //n-1
    val minusOne = Expr.PureApp(sub,List(Type.TInt,Type.TInt),List(ValueVar(n,Type.TInt),Literal(1,Type.TInt)))

    //Cons(n,[])
    val consctx = Expr.MakeContext(TList,consId,List(Type.TInt,Type.TInt),List(ValueVar(n,Type.TInt)),Nil)
    
    //compose(ctx,Cons(n,[]))
    val composition = Expr.PureApp(blockVarFromExternDef(ctxcomposeId),Nil,List(Expr.ValueVar(ctx,HoleContext(ATpe,TList)), consctx))
    
    //f2(n-1,compose(ctx, Cons(n,[]))
    val expected = Stmt.App(Block.BlockVar(f2,Function(Nil, Nil, List(Type.TInt, HoleContext(ATpe,TList)), Nil, TList),fCapt),Nil,List(minusOne, composition),Nil)
    
    val inputtransformed = transform(inputTree,f,f2,TList,TransformContext.Outer(ctx),HoleContext(ATpe,TList))

    assertAlphaEquivalentStatements(inputtransformed,expected)
  }
//  val TFieldName = ValueType.Data(Id("FieldName"), Nil)
//  val fieldString1 = Expr.Literal(tailId.show,Type.TString) //proxy
//  val fieldString2 = Expr.Literal(tailId, TFieldName) //proxy
  test("nested Vals, not TRMC"){
    val f: Id = Id("f")
    val f2: Id = Id("f2")
    val fType: BlockType = Function(Nil, Nil, Nil, Nil, Type.TInt)
    val fCapt: Captures = Set.empty

    val ctx: Id = Id("ctx")
    val A: Id = Id("A")
    val ATpe: ValueType = ValueType.Var(A)

    val g: Id = Id("g")
    val gType: BlockType = Function(Nil, Nil, List(Type.TInt), Nil, Type.TInt)
    val gCapt: Captures = Set.empty

    val h: Id = Id("h")
    val x: Id = Id("x")
    val y: Id = Id("y")

    //f()
    val callf = Stmt.App(Block.BlockVar(f, fType, fCapt), Nil, Nil, Nil)
    //g(y)
    val callg = Stmt.App(Block.BlockVar(g, gType, gCapt), Nil, List(Expr.ValueVar(y, Type.TInt)), Nil)
    //val y = f()
    //g(y)
    val valY = Stmt.Val(y,callf,callg)
    //h(x)
    val callh = Stmt.App(Block.BlockVar(h, gType, gCapt), Nil, List(ValueVar(x, Type.TInt)), Nil)

    //val x = { val y = f()
    //          g(y)}
    //h(x)
    val inputtree = Stmt.Val(x,valY,callh)

    val tmpId = Id("tmp")
    val val3 = Stmt.Val(tmpId, callh, Return(PureApp(blockVarFromExternDef(ctxApplyId), Nil, List(ValueVar(ctx, HoleContext(ATpe,Type.TInt)), ValueVar(tmpId, Type.TInt)))))

    val val2 = Stmt.Val(x,callg,val3)

    val callf2 = Stmt.App(Block.BlockVar(f2, Function(Nil, Nil, List(HoleContext(ATpe,Type.TInt)),Nil,Type.TInt), fCapt), Nil, List(PureApp(blockVarFromExternDef(ctxEmtpyId),Nil,Nil)), Nil)

    val expected = Stmt.Val(y,callf2,val2)

    val inputtransformed = transform(inputtree,f,f2,Type.TInt,TransformContext.Outer(ctx),HoleContext(ATpe,Type.TInt))

    assertAlphaEquivalentStatements(inputtransformed,expected) //TODO: passes, even though tmpIDs are different, why?
  }

  test("nested Vals, with TRMC"){
    val f: Id = Id("f")
    val f2: Id = Id("f2")
    val fType: BlockType = Function(Nil, Nil, List(Type.TInt), Nil, TList)
    val fCapt: Captures = Set.empty

    val ctx: Id = Id("ctx")
    val A: Id = Id("A")
    val ATpe: ValueType = ValueType.Var(A)

    val sub: BlockVar = BlockVar(Id("infixSub"), Function(Nil, Nil, List(Type.TInt, Type.TInt), Nil, Type.TInt), Set.empty)

    val g: Id = Id("g")
    val gType: BlockType = Function(Nil, Nil, List(TList), Nil, TList)
    val gCapt: Captures = Set.empty

    val x: Id = Id("x")
    val y: Id = Id("y")
    val n: Id = Id("n")

    //f(n-1)
    val callf = Stmt.App(Block.BlockVar(f, fType, fCapt), Nil, List(Expr.PureApp(sub,List(Type.TInt,Type.TInt),List(ValueVar(n,Type.TInt),Literal(1,Type.TInt)))),Nil)
    //Cons(n,y)
    val retStmt = Stmt.Return(Make(TList, consId, List(Type.TInt, Type.TInt), List(ValueVar(n, Type.TInt), ValueVar(y, TList))))
    //val y = f(n-1)
    //Cons(n,y)
    val valY = Stmt.Val(y,callf,retStmt)
    //g(x)
    val callg = Stmt.App(Block.BlockVar(g, gType, gCapt), Nil, List(Expr.ValueVar(x, TList)), Nil)
    //val x = {val y = f(n-1)
    //         Cons(n,y)
    //        {
    //g(x)
    val inputtree = Stmt.Val(x,valY,callg)

    val tmpId = Id("tmp")
    //val tmp = g(x)
    //apply_ctx(ctx,tmp)
    val val2 = Stmt.Val(tmpId, callg, Return(PureApp(blockVarFromExternDef(ctxApplyId), Nil, List(ValueVar(ctx, HoleContext(ATpe, TList)), ValueVar(tmpId, TList)))))

    //n-1
    val minusone = Expr.PureApp(sub, List(Type.TInt, Type.TInt), List(ValueVar(n, Type.TInt), Literal(1, Type.TInt)))

    //Cons(n,[])
    val consctx = Expr.MakeContext(TList, consId, List(Type.TInt, Type.TInt), List(ValueVar(n, Type.TInt)), Nil)
    
    //compose([],Cons(n,[])) because Split does not see how many vals there will be
    val composition = Expr.PureApp(blockVarFromExternDef(ctxcomposeId), Nil, List(Expr.PureApp(blockVarFromExternDef(ctxEmtpyId),Nil,Nil),consctx))

    //f2(n-1,Cons(n,[]))
    val callf2 = Stmt.App(Block.BlockVar(f2, Function(Nil, Nil, List(Type.TInt, HoleContext(ATpe,TList)),Nil,TList), fCapt), Nil, List(minusone,composition), Nil)

    val expected = Stmt.Val(x, callf2, val2)

    val transformed = transform(inputtree, f, f2, TList, TransformContext.Outer(ctx), HoleContext(ATpe, TList))
    assertAlphaEquivalentStatements(transformed,expected)
  }
  
  test("if"){

    val f: Id = Id("f")
    val f2: Id = Id("f2")
    val fType: BlockType = Function(Nil, Nil, List(Type.TInt), Nil, Type.TInt)
    val fCapt: Captures = Set.empty

    val ctx: Id = Id("ctx")
    val A: Id = Id("A")
    val ATpe: ValueType = ValueType.Var(A)

    val sub: BlockVar = BlockVar(Id("infixSub"), Function(Nil, Nil, List(Type.TInt, Type.TInt), Nil, Type.TInt), Set.empty)
    val Gt: BlockVar = BlockVar(Id("infixGt"), Function(Nil, Nil, List(Type.TInt, Type.TInt), Nil, Type.TBoolean), Set.empty)

    val g: Id = Id("g")
    val gType: BlockType = Function(Nil, Nil, Nil, Nil, Type.TInt)
    val gCapt: Captures = Set.empty

    val n: Id = Id("n")
    
    //n-1
    val minusone = Expr.PureApp(sub, List(Type.TInt, Type.TInt), List(ValueVar(n, Type.TInt), Literal(1, Type.TInt)))

    //f(n-1)
    val callf = Stmt.App(Block.BlockVar(f, fType, fCapt), Nil, List(minusone), Nil)
    //g()
    val callg = Stmt.App(Block.BlockVar(g, gType, gCapt), Nil, Nil, Nil)

    //if(n>0){
    //  f(n-1)
    //}else{
    //  g()
    //}
    val inputtree = Stmt.If(Expr.PureApp(Gt, Nil, List(ValueVar(n, Type.TInt), Literal(0, Type.TInt))), callf, callg)

    //f2(n-1,ctx)
    val callf2 = Stmt.App(Block.BlockVar(f2, Function(Nil, Nil, List(Type.TInt, HoleContext(ATpe,Type.TInt)),Nil,Type.TInt), fCapt), Nil, List(minusone,ValueVar(ctx, HoleContext(ATpe, Type.TInt))), Nil)

    val tmpId = Id("tmp")
    //val tmp = g()
    //apply_ctx(ctx,tmp)
    val val1 = Stmt.Val(tmpId, callg, Return(PureApp(blockVarFromExternDef(ctxApplyId), Nil, List(ValueVar(ctx, HoleContext(ATpe, Type.TInt)), ValueVar(tmpId, Type.TInt)))))
      
    val expected = Stmt.If(Expr.PureApp(Gt, Nil, List(ValueVar(n, Type.TInt), Literal(0, Type.TInt))), callf2, val1)
    
    val transformed = transform(inputtree, f, f2, Type.TInt, TransformContext.Outer(ctx), HoleContext(ATpe, Type.TInt))
    
    assertAlphaEquivalentStatements(transformed, expected)
  }
  
}