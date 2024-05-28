module Self = Plugin.Register 
(struct
  let name = "ACSL LSP"
  let shortname = "acsl"
  let help = "activates lsp support for acsl/c language"
end)

module Testing = Self.False
(struct
    let option_name = "-enable_tests"
    let help = "This option generates the test results file 'test_results.txt'."
end)

