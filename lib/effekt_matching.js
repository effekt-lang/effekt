// matchers: Any -> List[Any] | null
const $matching = (function() {

    const any = x => [x]

    const ignore = x => []

    const bind = matcher => x => {
        const matched = matcher(x)
        if (matched == null) return null;
        return [x].concat(matched)
    }

    const literal = c => x => {
        if (equals$impl(c, x))
            return []
        else
            return null
    }

    function tagged(tag) {
        const matchers = arguments
        return x => {
            if (!x || !x.__tag || x.__tag !== tag) return null;
            var extracted = [];
            // we start at 1 since matchers are shifted by 1
            for (var i = 1; i < matchers.length; i++) {
                const matched = matchers[i](x.__data[i - 1]);
                if (matched === null) return null;
                Array.prototype.push.apply(extracted, matched)
            }
            return extracted;
        }
    }

    function match(x, alternatives) {
        for (i in alternatives) {
            const alt = alternatives[i]
            const matched = alt.pattern(x)
            if (matched !== null) {
                return alt.exec.apply(null, matched)
            }
        }
    }

    return {
        any: any,
        ignore: ignore,
        tagged: tagged,
        bind: bind,
        literal: literal,
        match: match
    }
})();

Object.assign($effekt, $matching);


// p0 = bind(tagged("Nil"))
// p1 = bind(tagged("Cons", any, any))
// p2 = tagged("Cons", any, bind(tagged("Cons", any, ignore)))

// l0 = { tag: "Nil", data: [] }
// l1 = { tag: "Cons", data: [1, l0] }
// l2 = { tag: "Cons", data: [1, { tag: "Cons", data: [2, { tag: "Nil", data: [] }] }] }

// console.log(p1(l0))
// console.log(p1(l1))
// console.log(p1(l2))

// console.log(p2(l0))
// console.log(p2(l1))
// console.log(p2(l2))

// match(l2, [
//     { pattern: p0, exec: () => console.log("It is Nil!") },
//     { pattern: p2, exec: (x, y) => console.log("It has at least two elements", x, y) },
//     { pattern: p1, exec: (x, rest) => console.log("It only has one element", x) }
// ])
