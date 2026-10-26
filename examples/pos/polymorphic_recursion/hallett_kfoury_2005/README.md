# Hallett--Kfoury polymorphic-recursion corpus

Effekt translations of the named examples in J. J. Hallett and A. J.
Kfoury, *Programming Examples Needing Polymorphic Recursion* (2005).

The examples retain the type-changing calls relevant to polymorphic recursion.
Function values are represented by boxed `Fn` objects where the original uses
higher-order values. Coupled and uncoupled variants are both retained, although
top-level Effekt definitions do not form Standard ML-style binding groups.

`delay_evaluation.effekt` is intentionally ill typed, as in Section 7.1, and is
kept in the corresponding negative-test directory.

All fifteen well-typed examples run on both the polymorphic JS backend and the
LLVM backend, which executes `core.Mono`. Before selective erasure was added to
the monomorphizer, `bar.effekt` and `construct_list.effekt` reached its
`Detected polymorphic recursion` guard. The intentionally ill-typed example is
rejected earlier by the type checker because `Thunk[A] at {}` cannot be
returned as `A`.

| Paper section | Example | Result |
| --- | --- | --- |
| 3.1.1 | `double_coupled.effekt` | runs |
| 3.1.3 | `double_uncoupled.effekt` | runs |
| 3.2.1 | `mycroft_coupled.effekt` | runs |
| 3.2.2 | `mycroft_uncoupled.effekt` | runs |
| 3.3 | `sum_list.effekt` | runs |
| 3.4 | `isomorphic_compositions.effekt` | runs |
| 3.5 | `compiler_pass.effekt` | runs |
| 3.6.1 | `confusing_unalleviated.effekt` | runs |
| 3.6.3 | `confusing_alleviated.effekt` | runs |
| 4.1.1 | `matrix_transpose_unalleviated.effekt` | runs |
| 4.1.2 | `matrix_transpose_alleviated.effekt` | runs |
| 4.2 | `vector_addition.effekt` | runs |
| 5.1 | `collect.effekt` | runs |
| 5.2 | `bar.effekt` | runs |
| 6.1 | `construct_list.effekt` | runs |
| 7.1 | `delay_evaluation.effekt` (negative corpus) | intended type error |
