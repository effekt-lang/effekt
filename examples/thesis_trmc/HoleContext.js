

export function emptyContext(){
    return new HoleContext(undefined,{obj: undefined, fieldName: ""})
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