#!/bin/bash
set -ex
export LLVMURL=https://ghp.ci/https://raw.githubusercontent.com/llvm/llvm-project/refs/heads/main
export LLVMDIR=$HOME/.emacs.d/lisp/llvm-utils
mkdir -p $LLVMDIR
curl $LLVMURL/llvm/utils/emacs/tablegen-mode.el   -o $LLVMDIR/tablegen-mode.el
curl $LLVMURL/mlir/utils/emacs/mlir-mode.el       -o $LLVMDIR/mlir-mode.el
curl $LLVMURL/mlir/utils/emacs/mlir-lsp-client.el -o $LLVMDIR/mlir-lsp-client.el
