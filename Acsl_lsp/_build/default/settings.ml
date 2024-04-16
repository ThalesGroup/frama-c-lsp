(*include Plugin.Register
  (struct
    let name = "ACSL LSP"
    let shortname = "acsl_lsp"
    let help = "lsp support for acsl/c lanquage"
end)
(* On le met dans quel fichier ????????????????? *)
*)

module Self = Plugin.Register (
  struct
    let name = "acsl lsp plugin"
    let shortname = "acsl lsp"
    let help = "activates lsp support for acsl/c language"
end
)