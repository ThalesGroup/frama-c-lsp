(*
            "completionProvider": {
              "triggerCharacters": [],
              "allCommitCharacters": [],
              "resolveProvider": false,
              "completionItem": {
                "labelDetailsSupport": false
              }
            },
*)

let initialize : Json.json = 

    
    (* List.iter (fun x ->
      Lsp.Self.debug ~level:3 "warn category : %s\n%!" x
    ) PublishDiagnostics.evt_categories; *)

      let result = {|{
        "jsonrpc": "2.0",
        "id": 0,
        "result": {
          "capabilities": {
            "textDocumentSync": {
              "openClose": false,
              "change": 0,
              "save": {
                "includeText": false
              }
            },
            "definitionProvider": true,
            "declarationProvider": true,

            "diagnosticProvider": {
              "interFileDependencies": false,
              "workspaceDiagnostics": true
            },
            "workspace": {
              "workspaceFolders": {
                "supported": true,
                "changeNotifications": true
              }
            },
            "experimental": null
          },
          "serverInfo": {
            "name": "ACSL LSP",
            "version": "0.0.1"
          }
        }
      }|}
      in
      Json.load_string result
      (* todo : get uri of first and only path in workspaceFolders array *)
