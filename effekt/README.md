# `/effekt`

In this directory reside all **compiler** implementations, independent of the
targeted platform.

The `js` compiler is mainly used to also target `js`, enabling effekt translation
and execution fully inside a client's browser.

For other targets such as `jvm` and `llvm`, the compiler is run on the `jvm`.
