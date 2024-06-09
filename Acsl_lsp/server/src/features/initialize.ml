(* cannot be called twice, must be called first *)
let initialize (req : Types.RequestMessage.t): Json.json = 
    (* todo : remove file scheme from root uri *)
    (*let req_json = (RequestMessage.json_of_t req) in 
    let rootUri = Json.field "params" req_json |> Json.field "rootUri" in*)
    try 
      let file = File.from_filename (Filepath.Normalized.of_string "/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server/tests/test1.c") in
      ignore (File.init_from_c_files [file]) ; (* todo :*)
      ignore (Ast.get ()) ;
      (* todo : take dir from request (rootUri) *)
      let framac_share = Utils.file_str Fc_config.datadir in 
      Kernel.Share.set (Fc_config.datadir);
      let share = Kernel.Share.get () in
      Filepath.add_symbolic_dir framac_share share;

      (* todo : default initialize result for the moment *)
      let result = {|{
        "jsonrpc": "2.0",
        "id": 0,
        "result": {
          "capabilities": {
            "textDocumentSync": "None",
            "completionProvider": false,
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
            "documentOnTypeFormattingProvider": false,
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
      Types.ResponseMessage.json_of_t (Types.ResponseMessage.create ~jsonrpc:"2.0" ~id:req.id ~error:(Types.ResponseError.create ~code:(-32803) ~message:"No folders with c files are open." ()) ());


