const $effekt = {  };

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

// Complexity of state:
//
//  get: O(1)
//  set: O(1)
//  capture: O(1)
//  restore: O(|write operations since capture|)
const Mem = null

function Arena() {
    const s = {
        root: { value: Mem },
        generation: 0,
        fresh: (v) => {
            const r = {
                value: v,
                generation: s.generation,
                store: s,
                set: (v) => {
                    const s = r.store
                    const r_gen = r.generation
                    const s_gen = s.generation

                    if (r_gen == s_gen) {
                        r.value = v;
                    } else {
                        const root = { value: Mem }
                        // update store
                        s.root.value = { ref: r, value: r.value, generation: r_gen, root: root }
                        s.root = root
                        r.value = v
                        r.generation = s_gen
                    }
                }
            };
            return r
        },
        // not implemented
        newRegion: () => s
    };
    return s
}

function snapshot(s) {
    const snap = { store: s, root: s.root, generation: s.generation }
    s.generation = s.generation + 1
    return snap
}

function reroot(n) {
    if (n.value === Mem) return;

    const diff = n.value
    const r = diff.ref
    const v = diff.value
    const g = diff.generation
    const n2 = diff.root
    reroot(n2)
    n.value = Mem
    n2.value = { ref: r, value: r.value, generation: r.generation, root: n}
    r.value = v
    r.generation = g
}

function restore(store, snap) {
    // linear in the number of modifications...
    reroot(snap.root)
    store.root = snap.root
    store.generation = snap.generation + 1
}

// Common Runtime
// --------------
let _prompt = 1;

const TOPLEVEL_K = (x, ks) => { throw { computationIsDone: true, result: x } }
const TOPLEVEL_KS = { prompt: 0, arena: Arena(), rest: null }

function THUNK(f) {
    f.thunk = true
    return f
}

function CAPTURE(body) {
    return (ks, k) => {
        const res = body(x => TRAMPOLINE(() => k(x, ks)))
        if (res instanceof Function) return res
        else throw { computationIsDone: true, result: $effekt.unit }
    }
}

const RETURN = (x, ks) => ks.rest.stack(x, ks.rest)

// HANDLE(ks, ks, (p, ks, k) => { STMT })
function RESET(prog, ks, k) {
    const prompt = _prompt++;
    const rest = { stack: k, prompt: ks.prompt, arena: ks.arena, rest: ks.rest }
    return prog(prompt, { prompt, arena: Arena([]), rest }, RETURN)
}

function SHIFT(p, body, ks, k) {

    // TODO avoid constructing this object
    let meta = { stack: k, prompt: ks.prompt, arena: ks.arena, rest: ks.rest }
    let cont = null

    while (!!meta && meta.prompt !== p) {
        let store = meta.arena
        cont = { stack: meta.stack, prompt: meta.prompt, arena: store, backup: snapshot(store), rest: cont }
        meta = meta.rest
    }
    if (!meta) { throw `Prompt not found ${p}` }

    // package the prompt itself
    let store = meta.arena
    cont = { stack: meta.stack, prompt: meta.prompt, arena: store, backup: snapshot(store), rest: cont }
    meta = meta.rest

    const k1 = meta.stack
    meta.stack = null
    return body(cont, meta, k1)
}

// Rewind stack `cont` back onto `k` :: `ks` and resume with c
function RESUME(cont, c, ks, k) {
    let meta = { stack: k, prompt: ks.prompt, arena: ks.arena, rest: ks.rest }
    let toRewind = cont
    while (!!toRewind) {
        restore(toRewind.arena, toRewind.backup)
        meta = { stack: toRewind.stack, prompt: toRewind.prompt, arena: toRewind.arena, rest: meta }
        toRewind = toRewind.rest
    }

    const k1 = meta.stack // TODO instead copy meta here, like elsewhere?
    meta.stack = null
    return () => c(meta, k1)
}

function RUN_TOPLEVEL(comp) {
    try {
        let a = comp(TOPLEVEL_KS, TOPLEVEL_K)
        while (true) { a = a() }
    } catch (e) {
        if (e.computationIsDone) return e.result
        else throw e
    }
}

// trampolines the given computation (like RUN_TOPLEVEL, but doesn't provide continuations)
function TRAMPOLINE(comp) {
    let a = comp;
    try {
        while (true) {
            a = a()
        }
    } catch (e) {
        if (e.computationIsDone) return e.result
        else throw e
    }
}

// keeps the current trampoline going and dispatches the given task
function RUN(task) {
    return () => task(TOPLEVEL_KS, TOPLEVEL_K)
}

// aborts the current continuation
function ABORT(value) {
    throw { computationIsDone: true, result: value }
}


// "Public API" used in FFI
// ------------------------

/**
 * Captures the current continuation as a WHOLE and makes it available
 * as argument to the passed body. For example:
 *
 *   $effekt.capture(k => ... k(42) ...)
 *
 * The body
 *
 *   $effekt.capture(k => >>> ... <<<)
 *
 * conceptually runs on the _native_ JS call stack. You can call JS functions,
 * like `setTimeout` etc., but you are not expected or required to return an
 * Effekt value like `$effekt.unit`. If you want to run an Effekt computation
 * like in `io::spawn`, you can use `$effekt.run`.
 *
 * Advanced usage details:
 *
 * The value returned by the function passed to `capture` returns
 * - a function: the returned function will be passed to the
 *   Effekt runtime, which performs trampolining.
 *   In this case, the Effekt runtime will keep running, though the
 *   continuation has been removed.
 *
 * - another value (like `undefined`): the Effekt runtime will terminate.
 */
$effekt.capture = CAPTURE

/**
 * Used to call Effekt function arguments in the JS FFI, like in `io::spawn`.
 *
 * Requires an active Effekt runtime (trampoline).
 */
$effekt.run = RUN

/**
 * Used to call Effekt function arguments in the JS FFI, like in `network::listen`.
 *
 * This function should be used when _no_ Effekt runtime is available. For instance,
 * in callbacks passed to the NodeJS eventloop.
 *
 * If a runtime is available, use `$effekt.run`, instead.
 */
$effekt.runToplevel = RUN_TOPLEVEL


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

$effekt.equals = function(obj1, obj2) {
    if (!!obj1.__equals) {
        return obj1.__equals(obj2)
    } else {
        return (obj1.__unit && obj2.__unit) || (obj1 === obj2);
    }
}

function compare$prim(n1, n2) {
    if (n1 == n2) { return 0; }
    else if (n1 > n2) { return 1; }
    else { return -1; }
}

$effekt.compare = function(obj1, obj2) {
    if ($effekt.equals(obj1, obj2)) { return 0; }

    if (!!obj1 && !!obj2) {
        if (!!obj1.__reflect && !!obj2.__reflect) {
            const tagOrdering = compare$prim(obj1.__tag, obj2.__tag)
            if (tagOrdering != 0) { return tagOrdering; }

            const meta1 = obj1.__reflect().__data
            const meta2 = obj2.__reflect().__data

            const lengthOrdering = compare$prim(meta1.length, meta2.length)
            if (lengthOrdering != 0) { return lengthOrdering; }

            for (let i = 0; i < meta1.length; i++) {
                const contentOrdering = $effekt.compare(meta1[i], meta2[i])
                if (contentOrdering != 0) { return contentOrdering; }
            }

            return 0;
        }
    }

    return compare$prim(obj1, obj2);
}

$effekt.println = function println$impl(str) {
    console.log(str); return $effekt.unit;
}

$effekt.unit = { __unit: true }

$effekt.emptyMatch = function() { throw "empty match" }

$effekt.hole = function(pos) { throw pos + " not implemented yet" }



$effekt.readln = function readln$impl(callback) {
    const readline = require('node:readline').createInterface({
        input: process.stdin,
        output: process.stdout,
    });
    readline.question('', (answer) => {
        readline.close();
        callback(answer);
    });
}



function array$set(arr, index, value) {
    arr[index] = value;
    return $effekt.unit
}



function bytearray$set(bytes, index, value) {
    bytes[index] = value;
    return $effekt.unit;
}

function bytearray$compare(arr1, arr2) {
    const len = Math.min(arr1.length, arr2.length);

    for (let i = 0; i < len; i++) {
        if (arr1[i] !== arr2[i]) {
            return arr1[i] < arr2[i] ? -1 : 1;
        }
    }

    if (arr1.length !== arr2.length) {
        return arr1.length < arr2.length ? -1 : 1;
    } else {
        return 0;
    }
}



function set$impl(ref, value) {
    ref.value = value;
    return $effekt.unit;
}


function inspect_0(value_0, ks_0, k_0) {
    const v_r_0 = $effekt.println(($effekt.show(value_0)));
    return () => k_0(v_r_0, ks_0);
}

import {emptyContext, HoleContext} from "./HoleContext.js";


function main_0(ks_1, k_1) {
   let list = testMap_inPlace(new Cons_0(1, new Cons_0(7, new Cons_0(8, new Cons_0(4, new Nil_0())))), fibonacci_0);
    inspect_0(list, ks_1, k_1);
    return testMap_inPlace(new Cons_0(1, new Cons_0(7, new Cons_0(8, new Cons_0(4, new Nil_0())))), fibonacci_0);


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
    let cont = emptyContext()

    testMapk_inPlace(list_0, fun_0, cont)
    return cont.data //result
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
            cont.apply_context(new Nil_0())

            return cont.data;
        case 1:
            let y = fun(list.head_0)
            let hole = undefined
            let data = new Cons_0(y,hole)
            //compose
            let innerCont = new HoleContext(data, {obj: data, fieldName: "tail_0"})
            cont.compose_context(innerCont)
            /*let newHole = {outer:null}
            let newTail = new Cons_0(y,newHole)
            newHole.outer = newTail
            cont.hole.outer.tail_0 = newTail
            cont.hole = newHole*/

            return testMapk_inPlace(list.tail_0, fun, cont)
    }
}

/*export function main(){
    return (() => main_0)
}*/

let $custom_map;
(typeof module != "undefined" && module !== null ? module : {}).exports = $custom_map = {
  main: () => RUN_TOPLEVEL(main_0)
};