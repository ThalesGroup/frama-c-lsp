#!/bin/bash

# todo : change directory to plug-in installation directory
if [[ "$(pwd)" != "$HOME/git/L1/T0304764/acsl_lsp/Acsl_lsp/server" ]]; then
    cd "$HOME/git/L1/T0304764/acsl_lsp/Acsl_lsp/server"
fi
echo "run.sh : Frama-c plug-in launched"
dune exec -- frama-c