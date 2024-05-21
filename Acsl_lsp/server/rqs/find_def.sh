#!bin/bash

curl -v -H "Content-Type:application/json" -POST http://localhost:8001 -d '{"jsonrpc": 2.0, "id": 1, "method": "textDocument/definition", "params":{"textDocument":{"uri": "/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server/tests/locale.h"}, "position": {"line": 138, "character": 60}}}'
#curl -v -H "Content-Type:application/json" -POST http://localhost:8001 -d '{"jsonrpc": 2.0, "id": 1, "method": "textDocument/definition", "params":{"textDocument":{"uri": "/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server/tests/test1.c"}, "position": {"line": 4, "character": 11}}}'
#curl -v -H "Content-Type:application/json" -POST http://localhost:8001 -d '{"jsonrpc": 2.0, "id": 1, "method": "textDocument/definition", "params":{"textDocument":{"uri": "/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server/tests/math.h"}, "position": {"line": 1522, "character": 32}}}'
#curl -v -H "Content-Type:application/json" -POST http://localhost:8001 -d '{"jsonrpc": 2.0, "id": 1, "method": "textDocument/definition", "params":{"textDocument":{"uri": "/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server/tests/math.h"}, "position": {"line": 1483, "character": 27}}}'
#curl -v -H "Content-Type:application/json" -POST http://localhost:8001 -d '{"jsonrpc": 2.0, "id": 1, "method": "textDocument/def", "params":{"textDocument":{"uri": "/home/user/.opam/4.13.1_fc28/share/frama-c/share/libc/locale.h"}, "position": {"line": 138, "character": 59}}}'

