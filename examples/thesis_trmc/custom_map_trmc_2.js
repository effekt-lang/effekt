import {emptyContext, HoleContext, make_context, apply_context, compose_context, empty_context} from "./HoleContext.js";

const $effekt = {  };

$effekt.show = function(obj) {
    if (!!obj && !!obj.__reflect) {
        const meta = obj.__reflect()
        return meta.__name + "(" + meta.__data.map($effekt.show).join(", ") + ")"
    }
    else if (!!obj && obj.__unit) {
        return "()";
    } else {
        return "" + obj;
    }
}

$effekt.println = function println$impl(str) {
    console.log(str); return $effekt.unit;
}

class Nil_0 {
    constructor() {
        this.__tag = 0;
    }
    __reflect() {
        return { __tag: 0, __name: "Nil", __data: [] };
    }
    __equals(other5896) {
        if (!other5896) {
            return false;
        }
        if (!($effekt.equals(this.__tag, other5896.__tag))) {
            return false;
        }
        return true;
    }
}

class Cons_0 {
    constructor(head_0, tail_0) {
        this.__tag = 1;
        this.head_0 = head_0;
        this.tail_0 = tail_0;
    }
    __reflect() {
        return { __tag: 1, __name: "Cons", __data: [this.head_0, this.tail_0] };
    }
    __equals(other5897) {
        if (!other5897) {
            return false;
        }
        if (!($effekt.equals(this.__tag, other5897.__tag))) {
            return false;
        }
        if (!($effekt.equals(this.head_0, other5897.head_0))) {
            return false;
        }
        if (!($effekt.equals(this.tail_0, other5897.tail_0))) {
            return false;
        }
        return true;
    }
}

function inspect_0(value_0) {
    $effekt.println(($effekt.show(value_0)));
}


function main_0() {
    let list = testMap_inPlace(new Cons_0(1, new Cons_0(7, new Cons_0(8, new Cons_0(4, new Nil_0())))), fibonacci_0);
    inspect_0(list);
    let hole = new Cons_0(2,undefined)
    let context1 = make_context(new Cons_0(1,hole),{obj:hole,fieldName:"tail_0"})


    /*let hole = { outer: null }
    let list2 = { head: 42, tail: hole }
    hole.outer = list2

    console.log(list2)

// plug
    hole.outer.tail = {head: 3, tail: null}
    console.log(list2)*/
}

function fibonacci_0(n_0) {
    if (n_0 === (0)) {
        return 0;
    } else if (n_0 === (1)) {
        return 1;
    } else {
        return fibonacci_0(n_0-1)+ fibonacci_0(n_0-2);
    }
}

function testMap_0(list_0, fun_0) {
    return testMapk_cps(list_0,fun_0,(n) => n);
}
//in-place
function testMap_inPlace(list_0, fun_0){
    //construct empty context for testList[B]
    let cont = empty_context()

    return testMapk_inPlace(list_0, fun_0, cont)
    //return cont.data //result
}
//pure continuation passing style
//cont is a function testList[B] => testList[B]
function testMapk_cps(list, fun, cont){
    switch(list.__tag) {
        case 0:
            return cont(new Nil_0());
        case 1:
            let y = fun(list.head_0)
            return testMapk_cps(list.tail_0, fun, (holeValue) => cont(new Cons_0(y,holeValue)))
    }
}
//in-place
//cont is a context
//context: {data: testList[B], hole: }
function testMapk_inPlace(list, fun, cont){
    switch(list.__tag) {
        case 0:
            //apply
            /*let hole = cont.hole
            hole.outer.tail_0 = new Nil_0()*/
            //cont.apply_context(new Nil_0())

            //return cont.data;
            return apply_context(cont,new Nil_0())
        case 1:
            let y = fun(list.head_0)
            let hole = undefined
            let data = new Cons_0(y,hole)
            //compose
            let innerCont = make_context(data, {obj: data, fieldName: "tail_0"})
            //cont.compose_context(innerCont)
            let composedContext = compose_context(cont, innerCont)
            /*let newHole = {outer:null}
            let newTail = new Cons_0(y,newHole)
            newHole.outer = newTail
            cont.hole.outer.tail_0 = newTail
            cont.hole = newHole*/

            return testMapk_inPlace(list.tail_0, fun, composedContext)
    }
}

main_0()