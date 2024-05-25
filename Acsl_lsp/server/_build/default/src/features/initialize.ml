open Types

(*
class init_visitor (params : InitializeParams.t) = object
  inherit Visitor.frama_c_inplace
  method !vglob_aux g =
    (* do something with initialize params *)
    match g with
    | _ -> ignore params;
      let capabilities = ServerCapabilities.create ~definitionProvider:(ServerCapabilities.Bool true) () in
      let serverInfo = InitializeResult.create_serverInfo ~name: "ACSL Language Server" ~version: (Some "1.0.0") () in
      InitializeResult.json_of_t (InitializeResult.create ~capabilities: (Some capabilities) ~serverInfo: (Some serverInfo) ());
      Cil.DoChildren
end
*)


(* cannot be called twice, must be called first *)
let initialize (req : RequestMessage.t): Json.json = 
    ignore req;
    (*let logic_list = Logic_env.find_all_logic_functions "__fc_nan" in 
    Printf.printf "logic_list len = %d\n%!" (List.length logic_list);
    List.iter (fun _ ->
      Printf.printf "Logic fun : %s\n%!" "e"
    ) logic_list;*)

    (*Printf.printf "initialize called\n";
    let (params : InitializeParams.t) = InitializeParams.t_of_json (get req.params) in
    let ast = get_ast_from_file (Array.get (get params.workspace_folders) 0).uri in 
    ignore ast;*)
    
    (*let capabilities = ServerCapabilities.create ~definitionProvider:(ServerCapabilities.Bool true) () in
    let serverInfo = InitializeResult.create_serverInfo ~name: "ACSL Language Server" ~version:"1.0.0" () in
    let result = InitializeResult.create ~capabilities:capabilities ~serverInfo:serverInfo () in
    ResponseMessage.json_of_t (ResponseMessage.create ~jsonrpc:req.jsonrpc ~id:req.id ?result:(Some (InitializeResult.json_of_t result)) ());*)

    (* todo : default initialize reqult for the moment *)
    let result = "{
      \"jsonrpc\": \"2.0\",
      \"id\": 0,
      \"result\": {
        \"capabilities\": {
          \"textDocumentSync\": {
            \"openClose\": true,
            \"change\": 2,
            \"willSave\": true,
            \"willSaveWaitUntil\": true,
            \"save\": {
              \"includeText\": true
            }
          },
          \"completionProvider\": {
            \"resolveProvider\": false,
            \"triggerCharacters\": [\"\", \":\"]
          },
          \"hoverProvider\": false,
          \"signatureHelpProvider\": {
            \"triggerCharacters\": [\"(\", \",\"]
          },
          \"definitionProvider\": {
            \"dynamicRegistration\": false,
            \"linkSupport\": false
          },
          \"typeDefinitionProvider\": false,
          \"implementationProvider\": false,
          \"referencesProvider\": false,
          \"documentHighlightProvider\": false,
          \"documentSymbolProvider\": false,
          \"workspaceSymbolProvider\": false,
          \"codeActionProvider\": {
            \"codeActionKinds\": [
              \"quickfix\",
              \"refactor\",
              \"refactor.extract\",
              \"refactor.inline\",
              \"refactor.rewrite\",
              \"source.organizeImports\"
            ]
          },
          \"codeLensProvider\": {
            \"resolveProvider\": false
          },
          \"documentFormattingProvider\": false,
          \"documentRangeFormattingProvider\": false,
          \"documentOnTypeFormattingProvider\": {
            \"firstTriggerCharacter\": \"\\n\",
            \"moreTriggerCharacter\": [\";\", \"}\"]
          },
          \"renameProvider\": {
            \"prepareProvider\": false
          },
          \"foldingRangeProvider\": false,
          \"executeCommandProvider\": {
            \"commands\": [
              \"editor.action.organizeImports\",
              \"editor.action.formatDocument\"
            ]
          },
          \"selectionRangeProvider\": false,
          \"linkedEditingRangeProvider\": false,
          \"semanticTokensProvider\": {
            \"legend\": {
              \"tokenTypes\": [
                \"namespace\",
                \"type\",
                \"class\",
                \"enum\",
                \"interface\",
                \"struct\",
                \"typeParameter\",
                \"parameter\",
                \"variable\",
                \"property\",
                \"enumMember\",
                \"event\",
                \"function\",
                \"method\",
                \"macro\",
                \"keyword\",
                \"modifier\",
                \"comment\",
                \"string\",
                \"number\",
                \"regexp\",
                \"operator\"
              ],
              \"tokenModifiers\": [
                \"declaration\",
                \"definition\",
                \"readonly\",
                \"static\",
                \"deprecated\",
                \"abstract\",
                \"async\",
                \"modification\",
                \"documentation\",
                \"defaultLibrary\"
              ]
            },
            \"range\": true,
            \"full\": {
              \"delta\": true
            }
          },
          \"monikerProvider\": false,
          \"callHierarchyProvider\": false,
          \"workspace\": {
            \"workspaceFolders\": {
              \"supported\": true,
              \"changeNotifications\": true
            }
          },
          \"experimental\": {}
        },
        \"serverInfo\": {
          \"name\": \"ACSL LSP\",
          \"version\": \"0.0.1\"
        }
      }
    }
    " in
    Json.load_string result;
    (* get uri of first and only path in workspaceFolders array *)

