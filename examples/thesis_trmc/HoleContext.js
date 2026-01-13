

export function emptyContext(){
    return new HoleContext(undefined,{obj: undefined, fieldName: undefined})
}

export class HoleContext{
    constructor(data,holePointer) {
        this.data = data
        this.holePointer= holePointer

    }
    extend_context(data,holePointer){
        if (this.data === undefined){
            this.data = data
            this.holePointer = holePointer
        }else{
            this.holePointer.obj[this.holePointer.fieldName] = data
            this.holePointer = holePointer
        }

    }
    apply_context(value){
        if (this.data === undefined){
            this.data = value
        }else{
            this.holePointer.obj[this.holePointer.fieldName] = value
        }
    }
    compose_context(cont){
        if (cont.data !== undefined){
            this.extend_context(cont.data,cont.holePointer)
        }
    }

}
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