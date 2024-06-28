module Self = Plugin.Register 
(struct
  let name = "ACSL LSP"
  let shortname = "acsl_lsp"
  let help = "Activates lsp support for ACSL/C"
end)

module Enabled = Self.False
(struct
  let option_name = "-acsl_lsp"
  let help = "when on (off by default), activates lsp support for ACSL/C"
end)

module Testing = Self.False
(struct
    let option_name = "-enable_tests"
    let help = "Runs tests"
end)

