let syntax_error filename msg loc = 
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
                      ~severity:Types.DiagnosticSeverity.Error
                      ~message:msg
                      ()
                  ]
                )
                ()
            ))
            ()
        )
      ) 

let includePaths_error msg = 
  Json.save_string (
          Types.NotificationMessage.json_of_t (
            Types.NotificationMessage.create
              ~jsonrpc:"2.0"
              ~method_:"textDocument/publishDiagnostics"
              ~params:(Types.PublishDiagnosticsParams.json_of_t 
              (Types.PublishDiagnosticsParams.create
                  ~uri:((Utils.file_str (Filepath.pwd ()))^"/.vscode/settings.json")
                  ~diagnostics:(
                    [
                      Types.Diagnostic.create
                        ~range:(
                          Types.Range.create (Types.Position.create 0 0) (Types.Position.create 0 0)
                        )
                        ~severity:Types.DiagnosticSeverity.Error
                        ~message:msg
                        ()
                    ]
                  )
                  ()
              ))
              ()
          )
        ) 

let error filename msg loc = 
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
                        ~severity:Types.DiagnosticSeverity.Error
                        ~source:("ACSL LSP "^msg)
                        ~message:msg
                        ()
                    ]
                  )
                  ()
              ))
              ()
          )
        )

let warning filename msg loc = 
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
                        ~severity:Types.DiagnosticSeverity.Warning
                        ~source:("ACSL LSP "^msg)
                        ~message:"Annotation error"
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