(*
 * File Name: initialize.ml
 * Purpose: Definition of server capabilities compliant with the Language Server Protocol.
 * Authors: Djamila MOHAMED, Adel DJOUDI
 * Licence: GNU GENERAL PUBLIC LICENSE (GPL)
*)

let initialize : Json.json = 

      let result = {|{
        "jsonrpc": "2.0",
        "id": 0,
        "result": {
          "capabilities": {
            "textDocumentSync": {
              "openClose": false,
              "change": 0,
              "save": { "includeText": false }
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
