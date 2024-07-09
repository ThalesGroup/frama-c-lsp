let initialize (req : Types.RequestMessage.t): Json.json = 
    let req_json = (Types.RequestMessage.json_of_t req) in
    let temp =  Utils.remove_newline (Utils.remove_quotes (Json.save_string (Json.field "rootPath" (Json.field "params" req_json)))) in
    States.rootPath := States.(!rootPath) ^ temp;
    Printf.printf "rootPath = %s\n%!" States.(!rootPath);

      let result = {|{
        "jsonrpc": "2.0",
        "id": 0,
        "result": {
          "capabilities": {
            "textDocumentSync": {
              "change": 2,
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
