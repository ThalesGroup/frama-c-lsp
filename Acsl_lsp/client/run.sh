#!/bin/bash 

frama-c -lsp -lsp-debug=4 -lsp-handler || echo "Frama-C is not installed." && exit 2




