open Types

(* Testing WorkspaceFolder module *)
let test1 () =
  let sample_workspace_folder = {
    WorkspaceFolder.uri = "file:///path/to/document";
    name = "MyWorkspace"
  } in

  let workspace_folder_json = WorkspaceFolder.json_of_t sample_workspace_folder in

  let pp_formatter = Format.std_formatter in
  Format.fprintf pp_formatter "@[<v>%a@]@." Json.pp workspace_folder_json;

  let workspace_folder = WorkspaceFolder.t_of_json workspace_folder_json in

  if (sample_workspace_folder = workspace_folder) then 
    Printf.printf "WorkspaceFolder test passed.\n"

(* Testing ProgressToken module *)
let test2 () =
  let sample_progress_token = ProgressToken.Int 123 in

  let progress_token_json = ProgressToken.json_of_t sample_progress_token in

  let pp_formatter = Format.std_formatter in
  Format.fprintf pp_formatter "@[<v>%a@]@." Json.pp progress_token_json;

  let progress_token = ProgressToken.t_of_json progress_token_json in

  if (sample_progress_token = progress_token) then 
    Printf.printf "ProgressToken test passed.\n"


(* Testing DocumentUri module *)
let test3 () =
  let sample_document_uri = "file:///path/to/document" in

  let document_uri_json = DocumentUri.json_of_t sample_document_uri in

  let pp_formatter = Format.std_formatter in
  Format.fprintf pp_formatter "@[<v>%a@]@." Json.pp document_uri_json;

  let document_uri = DocumentUri.t_of_json document_uri_json in

  if (sample_document_uri = document_uri) then 
    Printf.printf "DocumentUri test passed.\n"


(* Testing TraceValue module *)
let test4 () =
  let sample_trace_value = "off" in

  let trace_value_json = TraceValue.json_of_t sample_trace_value in

  let pp_formatter = Format.std_formatter in
  Format.fprintf pp_formatter "@[<v>%a@]@." Json.pp trace_value_json;

  let trace_value = TraceValue.t_of_json trace_value_json in

  if (sample_trace_value = trace_value) then 
    Printf.printf "TraceValue test passed.\n"

  
(* Testing DefinitionClientCapabilities module *)
let test5 () =
  let sample_capabilities = {
    DefinitionClientCapabilities.dynamicRegistration = Some true;
    linkSupport = Some false
  } in

  let capabilities_json = DefinitionClientCapabilities.json_of_t sample_capabilities in

  let pp_formatter = Format.std_formatter in
  Format.fprintf pp_formatter "@[<v>%a@]@." Json.pp capabilities_json;

  let capabilities = DefinitionClientCapabilities.t_of_json capabilities_json in

  if (sample_capabilities = capabilities) then 
    Printf.printf "DefinitionClientCapabilities test passed.\n";

  (* Json.save_string test *)
  Printf.printf "save_string\n%s\n" (Json.save_string capabilities_json)

let test6 () = 
  let sample_definition_params = {
    DefinitionParams.partialResultToken = Some (Int 42);
    textDocument = {TextDocumentIdentifier.uri = "s/s/s/d"};
    position = {Position.line = 45; character = 45};
    work_done_token = None
  } in 

  let sample_def_params_json = DefinitionParams.json_of_t sample_definition_params in 
  Printf.printf "save_string\n%s\n" (Json.save_string sample_def_params_json)

(* Json.load_string test *)
let test7 () = 
  let json_string = "{
      \"jsonrpc\": 2.0,
      \"id\": 1, 
      \"method\": \"textDocument/definition\", 
      \"result\": {
          \"textDocument\": {
              \"uri\": \"/home/file.h\"
          }, 
          \"position\": {
              \"line\": 138, \"character\": 59
          }
      }
  }" in 

  let json = Json.load_string json_string in 
  (* put it into the right type : RequestMessage and DefinitionParams *)
  let request = RequestMessage.t_of_json json in 
  let curr_method = request.method_ in 
  let request_json = Json.save_string json in
  Printf.printf "test7\nsave_string\n%s\n" json_string;
  Printf.printf "load string\n%s\n" request_json;
  Printf.printf "method\n%s\n" curr_method
