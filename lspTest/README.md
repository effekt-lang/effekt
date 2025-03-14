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
