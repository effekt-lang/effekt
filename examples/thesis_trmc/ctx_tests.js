import {emptyContext, HoleContext, make_context, apply_context, compose_context, empty_context} from "./HoleContext.js";
import {Cons} from "../../.koka/v3.2.2/js-debug-1f15ea/std_core_types.mjs";

function runAll(){
    testApplyEmpty()
}

function testApplyEmpty(){
    let ctx = empty_context()
    let res = apply_context(ctx, 1)
    if(res === 1){
        console.log("passed testApplyEmpty")
    }else{
        console.log("failed testApplyEmpty")
    }
}


runAll()