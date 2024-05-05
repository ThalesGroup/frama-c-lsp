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
  let sample_trace_value = TraceValue.Off in

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
    Printf.printf "DefinitionClientCapabilities test passed.\n"


(* RequestMessage test *)
(* Successful test case *)
let test6_1 () =
  let msg = {
    RequestMessage.id = Int 123;
    method_ = "getTextDocument";
    params = Some (Json.of_string "{ \"uri\": \"file:///path/to/document\" }")
  } in

  let json = RequestMessage.json_of_t msg in
  let reconstructed_msg = RequestMessage.t_of_json json in
  if (msg = reconstructed_msg) then
    Printf.printf "RequestMessage test 1 passed.\n"


(* Failing test case *)
let test6_2 () =
  let json = `Assoc [
    "method_", `String "getTextDocument";
  ] in
  let result = try Some (RequestMessage.t_of_json json) with _ -> None in
  if (result = None) then
    Printf.printf "RequestMessage test 2 passed.\n"

let test7 () =
  (* Create some sample registration data *)
  let registration1 = { Registration.id = "1"; method_ = "method1"; registerOptions = None } in
  let registration2 = { Registration.id = "2"; method_ = "method2"; registerOptions = Some (Json.of_string "options") } in
  let registrations = [registration1; registration2] in
  let params = RegistrationParams.{ registrations } in

  (* Serialize the data to JSON *)
  let json = RegistrationParams.json_of_t params in

  (* Deserialize the JSON back to data *)
  let parsed_params = RegistrationParams.t_of_json json in

  (* Check if the deserialized data matches the original data *)
  if (parsed_params = params) then
    Printf.printf "RegistrationParams test passed";

  (* Print the serialized JSON for inspection *)
  print_endline "Serialized JSON:";
  Json.pp Format.std_formatter json;
  Format.print_newline ()

