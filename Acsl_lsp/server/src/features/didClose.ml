let clear_diagnostics filename = 
  Lsp_types.NotificationMessage.json_of_t (
    Lsp_types.NotificationMessage.create
      ~jsonrpc:"2.0"
      ~method_:"textDocument/publishDiagnostics"
      ~params:(Lsp_types.PublishDiagnosticsParams.json_of_t 
      (Lsp_types.PublishDiagnosticsParams.create
          ~uri:(Utils.file_str (Filepath.Normalized.of_string (Filepath.normalize filename)))
          ~diagnostics:(
            []
          )
          ()
      ))
      ()
  )

let handle filename : Json.json = 
  clear_diagnostics filename