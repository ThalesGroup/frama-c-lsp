let initialize (req : Types.RequestMessage.t): Json.json = 
    let req_json = (Types.RequestMessage.json_of_t req) in
    let temp =  Utils.remove_newline (Utils.remove_quotes (Json.save_string (Json.field "rootPath" (Json.field "params" req_json)))) in
    States.rootPath := States.(!rootPath) ^ temp ^ "/";
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
            "completionProvider": false,
            "hoverProvider": false,
            "signatureHelpProvider": {
              "triggerCharacters": ["(", ","]
            },
            "definitionProvider": true,
            "declarationProvider": false,
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
            "selectionRangeProvider": false,
            "linkedEditingRangeProvider": {"workDoneProgress": false},
            "semanticTokensProvider": false,
            "monikerProvider": false,
            "diagnosticProvider": {
              "interFileDependencies": false,
              "workspaceDiagnostics": true
            },
            "callHierarchyProvider": false,
            "workspace": {
              "workspaceFolders": {
                "supported": true,
                "changeNotifications": true
              },
              "configuration": true
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

let init_files sock = 
  try
    Configuration.set_includePaths (Configuration.(!configs));
    let filenames = Filepath.readdir (Filepath.Normalized.of_string States.(!rootPath)) in
    (* remove non source files *)
    let filtered_files = List.filter (fun x -> String.ends_with ~suffix:".c" x || String.ends_with ~suffix:".h" x) (Array.to_list filenames) in
    Printf.printf "file list size = %d\n%!" (List.length filtered_files);
    let files = 
      List.map (fun y -> 
        States.(!rootPath) ^ y) 
      (filtered_files)
      in

    (* initialize all files *)
    List.iter (fun file ->
      Load.load_file file sock
    ) files;

  with Stdlib.Sys_error err -> 
    Utils.send_error_request err sock