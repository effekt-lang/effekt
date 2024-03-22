package effekt
package generator
package hvm


def transform(stmt: lifted.Stmt): cps.Term = stmt match {
    case _  => ???
}

def transform(definition: lifted.Definition): cps.Term = ???

//cps to hvm
def transform(terms: List[cps.Term]): Book = ???
def transform(terms: cps.Term): Book = ???