#!/bin/bash 

# acsl lsp doesn't support unicode characters
# frama-c -acsl_lsp -no-unicode

# cd $(frama-c -print-share-path)
# cd ../..
# chmod u+x acsl_lsp_launcher/acsl_lsp_launcher.exe
# acsl_lsp_launcher/acsl_lsp_launcher.exe

frama-c -lsp -lsp-debug=4 -lsp-handler




