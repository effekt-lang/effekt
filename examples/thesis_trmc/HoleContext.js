
function extend_context(context,data, holePointer){
    if(context === undefined){
        return make_context(data, holePointer)
    }else{
        context.holePointer.obj[context.holePointer.fieldName] = data
        return make_context(context.data,holePointer)
    }
}
export function apply_context(context,value){
    if(context === undefined){
        return value
    }else{
        context.holePointer.obj[context.holePointer.fieldName] = value
        return context.data
    }
}
export function compose_context(context1,context2){
    if(context2 === undefined){
        return context1
    }else{
        return extend_context(context1, context2.data, context2.holePointer)
    }
}
export function make_context(data,holePointer){
    return {data: data, holePointer:holePointer}
}
export function empty_context(){
    return undefined
}