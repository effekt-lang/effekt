var effekt = require("./effekt_runtime")

function println(x) {
    console.log(x)
    return effekt.pure(effekt.unit)
}

function e2() {
    /*
    effect Dummy(): Unit

    try {
        1
    } with Dummy {
        () => resume(())
    } on return {
        x => x + 1
    }
    */    
    return (
        effekt.handle(
            [{"op$Dummy": (resume) => resume(effekt.unit)}],
            null, null, x => effekt.pure(x + 1)
            )
            (Dummy$cap => Dummy$cap.op$Dummy().then(() => effekt.pure(1)))
    ).run()
}

function e3() {
    /*
    effect Dummy(): Unit

    var x = 0
    try {
        Dummy()
    } with Dummy {
        () => resume(())
    } on suspend {
        1
    } on resume {
        y => x = x + y
    } on return {
        println(x)
    }
    */
   return (
       effekt.pure(0).state(
            x => effekt.handle(
                    [{"op$Dummy": resume => resume(effekt.unit)}],
                    () => effekt.pure(1),
                    unwindData => x.op$get().then(x_val => x.op$put(effekt.pure(x_val + unwindData))),
                    ret => x.op$get().then(x_val => effekt.delayed(() => println(x_val)).then(_ => ret))
                )(Dummy$cap => Dummy$cap.op$Dummy())
                
       )
    ).run()
}

function e4() {
    /*
     effect Dummy(): Unit
    try {
        Dummy()
        println("done (yet?)")
    } with Dummy {
        () => resume(()); resume(())
    } on suspend {
        println("suspended")
    }
    */

    return (
        effekt.handle(
            [{"op$Dummy": resume => resume(effekt.unit).then(() => resume(effekt.unit))}],
            () => { console.log("suspended"); return effekt.pure(effekt.unit) },
            null,
            null
        )(Dummy$cap => Dummy$cap.op$Dummy().then(() => println("done (yet?)")))
    ).run()
}

function e5() {
    /*
     effect Dummy(): Unit
    try {
        Dummy()
        println("done (yet?)")
    } with Dummy {
        () => resume(()); resume(())
    } on resume {
        x => println("resumed")
    }
    */

    return (
        effekt.handle(
            [{"op$Dummy": resume => resume(effekt.unit).then(() => resume(effekt.unit))}],
            null,
            _ => println("resumed"),
            null
        )(Dummy$cap => Dummy$cap.op$Dummy().then(() => println("done (yet?)")))
    ).run()
}

function e6() {
    /*
    effect Dummy(): Unit

    try {
        Dummy()
        Dummy()
        println("done (yet?)")
    } with Dummy {
        () => resume(())
    } on suspend {
        println("suspended")
    } on resume {
        _ => println("resumed")
    } on return {
        _ => println("returned")
    }
    */
    return (
            effekt.handle(
                [{"op$Dummy": resume => resume(effekt.unit)}],
                () => println("suspended"),
                _ => println("resumed"),
                _ => println("returned")
            )(Dummy$cap => Dummy$cap.op$Dummy().then(() => Dummy$cap.op$Dummy()).then(() => println("done (yet?)")))
    ).run()
}

function e7() {
    /*
    effect Dummy(): Unit

    try {
        Dummy()
        println("done (yet?)")
    } with Dummy {
        () => resume(()); resume(())
    } on suspend {
        println("suspended")
    } on resume {
        _ => println("resumed")
    } on return {
        _ => println("returned"); 1
    }
    */
    return (
            effekt.handle(
                [{"op$Dummy": resume => resume(effekt.unit).then(() => resume(effekt.unit))}],
                () => println("suspended"),
                _ => println("resumed"),
                _ => println("returned").then(() => effekt.pure(1))
            )(Dummy$cap => Dummy$cap.op$Dummy().then(() => println("done (yet?)")))
    ).then(x => println(x)).run()
}

function e8() {
    /*
    effect Dummy(): Unit

    var x = 0
    try {
        Dummy()
    } with Dummy {
      () => resume(())  
    } on return {
        _ => x = x + 1; println(x)
    }
    */
    return (effekt.pure(0).state(x =>
        effekt.handle(
            [{"op$Dummy": resume => resume(effekt.unit)}],
            null,
            null,
            _ => x.op$get().then(x_val => x.op$put(x_val + 1)).then(() => x.op$get()).then(x_val => println(x_val))
        )(Dummy$cap => Dummy$cap.op$Dummy())
    )
    ).run()
}

function e9() {
    /*
    effect Emit(x: Int): Unit

    try {
        Emit(1)
    } with Dummy {
        x => println(x); resume(())
    } on resume {
        _ => 
        println("outer resume");
        try {
            Emit(2)
        } with Dummy {
            x => println(x); resume(())
        } on resume {
            _ => println("inner resume")
        }
    }
    */

    return (
        effekt.handle(
            [{"op$Emit": (x, resume) => println(x).then(() => resume(effekt.unit))}],
            null,
            _ => println("outer resune").then(() => (effekt.handle(
                [{"op$Emit": (x, resume) => println(x).then(() => resume(effekt.unit))}],
                null,
                _ => println("inner resume"),
                null,
            )(Emit$cap => Emit$cap.op$Emit(2)))),
            null
        )(Emit$cap => Emit$cap.op$Emit(1))
    ).run()
}

console.log(e6())

Object.keys(require.cache).forEach(key => delete require.cache[key])

module.exports = {}