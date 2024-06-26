let diagnostics filename msg loc = 
Json.save_string (
        Types.NotificationMessage.json_of_t (
          Types.NotificationMessage.create
            ~jsonrpc:"2.0"
            ~method_:"textDocument/publishDiagnostics"
            ~params:(Types.PublishDiagnosticsParams.json_of_t 
            (Types.PublishDiagnosticsParams.create
                ~uri:(Utils.file_str (Filepath.Normalized.of_string (Filepath.normalize filename)))
                ~diagnostics:(
                  [
                    Types.Diagnostic.create 
                      ~range:(Utils.get_lsp_range loc)
                      ~message:msg
                      ()
                  ]
                )
                ()
            ))
            ()
        )
      ) 

let clear_diagnostics filename = 
  Json.save_string (
    Types.NotificationMessage.json_of_t (
      Types.NotificationMessage.create
        ~jsonrpc:"2.0"
        ~method_:"textDocument/publishDiagnostics"
        ~params:(Types.PublishDiagnosticsParams.json_of_t 
        (Types.PublishDiagnosticsParams.create
            ~uri:(Utils.file_str (Filepath.Normalized.of_string (Filepath.normalize filename)))
            ~diagnostics:(
              []
            )
            ()
        ))
        ()
    )
  ) 