open Types

(* cannot be called twice, must be called first *)
let initialize (req : RequestMessage.t): Json.json = 
    (* todo : remove file scheme from root uri *)
    (*let req_json = (RequestMessage.json_of_t req) in 
    let rootUri = Json.field "params" req_json |> Json.field "rootUri" in*)
    try 
      ignore (Utils.get_ast_from_file "/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server/tests") ;
      (* todo : take dir from request (rootUri) *)
      let framac_share = Utils.file_str Fc_config.datadir in 
      Kernel.Share.set (Fc_config.datadir);
      let share = Kernel.Share.get () in
      Filepath.add_symbolic_dir framac_share share;
      Printf.printf "share path : %s\n%!" framac_share;

      (* todo : default initialize result for the moment *)
      let result = {|{
        "jsonrpc": "2.0",
        "id": 0,
        "result": {
          "capabilities": {
            "textDocumentSync": {
              "openClose": false,
              "change": 2,
              "willSave": false,
              "willSaveWaitUntil": false,
              "save": {
                "includeText": false
              }
            },
            "completionProvider": {
              "resolveProvider": false,
              "triggerCharacters": ["", ":"]
            },
            "hoverProvider": false,
            "signatureHelpProvider": {
              "triggerCharacters": ["(", ","]
            },
            "definitionProvider": true,
            "typeDefinitionProvider": false,
            "implementationProvider": false,
            "referencesProvider": false,
            "documentHighlightProvider": false,
            "documentSymbolProvider": false,
            "workspaceSymbolProvider": false,
            "codeActionProvider": false,
            "codeLensProvider": false,
            "documentFormattingProvider": false,
            "documentRangeFormattingProvider": false,
            "documentOnTypeFormattingProvider": {
              "firstTriggerCharacter": "\\n",
              "moreTriggerCharacter": [";", "}"]
            },
            "renameProvider": {
              "prepareProvider": false
            },
            "foldingRangeProvider": false,
            "executeCommandProvider": {
              "commands": [
                "editor.action.organizeImports",
                "editor.action.formatDocument"
              ]
            },
            "selectionRangeProvider": {
              "legend": {
                "tokenTypes": [],
                "tokenModifiers": []
              },
              "range": false,
              "full": { "delta": true }
            },
            "linkedEditingRangeProvider": {"workDoneProgress": false},
            "semanticTokensProvider": false,
            "monikerProvider": false,
            "callHierarchyProvider": false,
            "workspace": {
              "workspaceFolders": {
                "supported": false,
                "changeNotifications": false
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
      Json.load_string result;
      (* get uri of first and only path in workspaceFolders array *)
    with Failure _ -> 
      ResponseMessage.json_of_t (ResponseMessage.create ~jsonrpc:"2.0" ~id:req.id ~error:(ResponseError.create ~code:(-32803) ~message:"No folders with c files are open." ()) ());


