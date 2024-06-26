module Self = Plugin.Register 
(struct
  let name = "ACSL LSP"
  let shortname = "acsl"
  let help = "activates lsp support for acsl/c language"
end)

module Enabled = Self.False
(struct
  let option_name = "-acsl"
  let help = ""
end)

module Testing = Self.False
(struct
    let option_name = "-enable_tests"
    let help = "Runs tests."
end)

