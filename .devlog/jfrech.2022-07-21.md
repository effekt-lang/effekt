# [jfrech, 2022-07-21]

## Bring back seized comments and add:
```
[jfrech, 2022-07-12] TODO apparently, this notion is subtly incorrect, yet its clarification requires exact specification of what we mean by an *environment* (local register representation of a
 frame, C-style environment, or Effekt-stack baked state representation)
```

## Bring back RegExp-based name sanitation

## Fix all exhaustivity warnings (`asFragment`, `freeVariables`, ...).
By either shouting loudly or implementing it.

## Exhaustivity in `machine.scala`: Core -> Machine (or a catch-all)

## Do not touch `llvm/Transformer.scala`

# `libraries/llvm/effekt.effekt`: make `infixEq` work
Booleans now are ADTs:
```
adt := { i64, i8* } (one fat pointer)

false := { 0, NULL }
true := { 1, NULL }
```

# Long-term task: port benchmarks
Create `tests-llvm/benchmarks` and transpile `https://github.com/effect-handlers/effect-handlers-bench/tree/main/benchmarks/koka`.
