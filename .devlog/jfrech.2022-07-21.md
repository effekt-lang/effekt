# [jfrech, 2022-07-21]

## Do not touch `llvm/Transformer.scala`!

## Fix all exhaustivity warnings (`asFragment`, `freeVariables`, ...).
By either shouting loudly or implementing it.

## Exhaustivity in `machine.scala`: Core -> Machine (or a catch-all)

# `libraries/llvm/effekt.effekt`: make `infixEq` work
Booleans now are ADTs:
```
adt := { i64, i8* } (one fat pointer)

false := { 0, NULL }
true := { 1, NULL }
```

# Long-term task: port benchmarks
Create `tests-llvm/benchmarks` and transpile `https://github.com/effect-handlers/effect-handlers-bench/tree/main/benchmarks/koka`.
