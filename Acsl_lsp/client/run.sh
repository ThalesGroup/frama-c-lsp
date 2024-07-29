#!/bin/bash 

# acsl lsp doesn't support unicode characters
# frama-c -acsl_lsp -no-unicode

cd "$HOME/git/L1/T0304764/acsl_lsp/Acsl_lsp/server_v3/wrapper"
# echo "run.sh : ACSL Language server launched"
dune exec wrapper