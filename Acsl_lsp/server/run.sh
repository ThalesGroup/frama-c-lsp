#!/bin/bash

if [ "$(pwd)" != "$HOME/git/L1/T0304764/acsl_lsp/Acsl_lsp/server" ]; then
    cd "$HOME/git/L1/T0304764/acsl_lsp/Acsl_lsp/server" || exit
fi

dune build
dune exec -- frama-c