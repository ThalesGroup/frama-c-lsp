#!/bin/bash 

#!/bin/bash

# Check if at least one argument is provided
if [ $# -lt 2 ]; then
  echo "Usage: $0 <server_port> <wrapper_port>"
  exit 1
fi

frama-c -lsp -lsp-handler $1:$2 || echo "Frama-C LSP is not installed." && exit 2




