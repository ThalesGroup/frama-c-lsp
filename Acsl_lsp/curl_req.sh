#!bin/bash

curl -v -H "Content-Type:application/json" -X POST http://localhost:8001 -d '{"jsonrpc": 2.0, "id": 1, "method": "textDocument/definition", "params":{"textDocument":{"uri": "/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/tests/quick_test/acsl_first_example.c"}, "position": {"line": 8, "character": 11}}}}}'
