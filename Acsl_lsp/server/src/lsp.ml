(*
 * File Name: lsp.ml
 * Purpose: LSP module registration
 * Authors: Djamila MOHAMED
 * Licence: GNU GENERAL PUBLIC LICENSE (GPL)
*)

module Self = Plugin.Register 
(struct
  let name = "ACSL LSP"
  let shortname = "lsp"
  let help = "Activates LSP support for ACSL/C"
end)

