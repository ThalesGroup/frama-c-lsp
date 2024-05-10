#!bin/bash

# curl -v -H "Content-Type:application/json" -POST http://localhost:8001 -d '{"jsonrpc": 2.0, "id": 1, "method": "textDocument/definition", "params":{"textDocument":{"uri": "/home/user/.opam/4.13.1_fc28/share/frama-c/share/libc/locale.h"}, "position": {"line": 138, "character": 59}}}'
curl -v -H "Content-Type:application/json" -POST http://localhost:8001 -d '{"jsonrpc": 2.0, "id": 1, "method": "textDocument/definition", "params":{"textDocument":{"uri": "/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server/tests/test1.c"}, "position": {"line": 4, "character": 10}}}'

#curl -v -H "Content-Type:application/json" -POST http://localhost:8001 -d '{"jsonrpc": 2.0, "id": 1, "method": "textDocument/def", "params":{"textDocument":{"uri": "/home/user/.opam/4.13.1_fc28/share/frama-c/share/libc/locale.h"}, "position": {"line": 138, "character": 59}}}'
