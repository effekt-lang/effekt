const fs = require("fs")
const fsPromises = fs.promises;

function readAsyncHelper(path, f) {
    return $effekt.callcc(k => {
        fs.readFile(path, 'utf8', (err, data) => {
            // change f, so it returns true
            const comp = f(err ? Error(err.message) : Success(data)).then(x => $effekt.pure(true))
            k(comp); // restore the stack, since `f` might use effects
        });
        return k($effekt.pure(false))
    }).then(x => x).then(abort => abort ? $effekt.callcc(k => $effekt.pure(null)) : $effekt.pure(null))
}