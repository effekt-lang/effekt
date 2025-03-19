# Language Server Tests

This directory contains the test suite for the Effekt language server.

## Usage

To execute all tests, run:

```sh
sbt 'lspTest/test'
```

To see the protocol messages being exchanged, you can set the `TRACE_LSP_TESTS` environment variable.
Example invocation:

```sh
env TRACE_LSP_TESTS=1 sbt 'lspTest/test'
```

To overwrite the expected output for the test cases (the `*.check.json` files), set the `OVERWRITE_LSP_TEST_RESULTS` environment variable.
Example invocation:

```sh
env OVERWRITE_LSP_TEST_RESULTS=1 sbt 'lspTest/test'
```
