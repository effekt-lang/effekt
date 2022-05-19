#! /bin/sh

cd "$(dirname "$(dirname "$(realpath "$0")")")"
export EFFEKT_LLVM_VERSION=11
~/.sbt-installation/bin/sbt 'project effektJVM' '~testOnly effekt.LLVMTests'
