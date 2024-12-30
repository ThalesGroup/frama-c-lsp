#!/bin/bash 

frama-c -lsp -lsp-debug=4 -lsp-handler || echo "Frama-C LSP is not installed." && exit 2




