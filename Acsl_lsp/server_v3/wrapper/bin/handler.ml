
let receivedShutdown = ref false

let remove_file_scheme uri =
  let regex = Str.regexp {|file://|} in
  Str.global_replace regex "" uri

let remove_quotes str = 
  let regex = Str.regexp {|[\"]|} in
  Str.global_replace regex "" str

let remove_newline str = 
  let regex = Str.regexp {|.*|} in 
  ignore (Str.search_forward regex str 0);
  Str.matched_string str

let is_result json_string = 
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      (List.exists (fun (key, _) -> key = "result") fields)
    | _ -> Printf.printf "no result\n%!"; false
  with
  | Json.Error _ -> false

let is_error json_string = 
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      (List.exists (fun (key, _) -> key = "error") fields)
    | _ -> Printf.printf "no error\n%!"; false

  with
  | Json.Error _ -> false

let is_notif json_string =
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      not (List.exists (fun (key, _) -> key = "id") fields)
    | _ -> Printf.printf "no notif\n%!"; false

  with
  | Json.Error _ -> false

let is_request json_string =
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      (List.exists (fun (key, _) -> key = "id") fields)
    | _ -> Printf.printf "no request\n%!"; false

  with
  | Json.Error _ -> false
  
let rq_handler json_string vscode_sock =
  ignore vscode_sock;
  let json = Json.load_string json_string in 
  let request = Acsl_lsp.Types.RequestMessage.t_of_json json in 
  let curr_method = request.method_ in 
  let id = request.id in
  try
    match curr_method with 
    | "initialize" -> 
      Acsl_lsp.Types.RQ_RESULT (Acsl_lsp.Initialize.initialize request);
    | "textDocument/definition" -> 
      Printf.printf "definition\n%!";
      Acsl_lsp.Types.RQ_RESULT (Acsl_lsp.Find_def.find_def request);
    | "textDocument/declaration" -> 
      Printf.printf "declaration\n%!";
      Acsl_lsp.Types.RQ_RESULT (Acsl_lsp.Find_decl.find_decl request);
    (* | "textDocument/completion" -> 
      Printf.printf "completion\n%!";
      Acsl_lsp.Types.RQ_RESULT (Completion.completion_items request); *)
    | "vscodeacsl/displayCIL" -> 
      Printf.printf "displayCIL\n%!";
      Acsl_lsp.Types.RQ_RESULT (Acsl_lsp.Command.displayCIL id);
    | "shutdown" -> receivedShutdown := true; 
      Acsl_lsp.Types.RQ_RESULT (Acsl_lsp.Shutdown.shutdown request);
    | _ -> Acsl_lsp.Types.RQ_RESULT (`Null)
  with _ -> 
    Printf.printf "REQUEST ERROR \n%!";
    Acsl_lsp.Types.RQ_RESULT (Acsl_lsp.Utils.make_error "Error during request handling" (Acsl_lsp.Utils.id_to_int id))

let notif_handler json_string vscode_sock wrapper_sock =
  let json = Json.load_string json_string in 
  let notif = Acsl_lsp.Types.NotificationMessage.t_of_json json in 
  let curr_method = notif.method_ in 
  match curr_method with 
  | "initialized" -> 
    Printf.printf "initialized\n%!";
    Acsl_lsp.Utils.send_request vscode_sock (Json.save_string Configuration.request_configurations);
    Acsl_lsp.Types.RQ_RESULT (Acsl_lsp.Initialized.init)
  (* | "textDocument/didOpen" ->
    Printf.printf "didOpen\n%!";
    Acsl_lsp.Types.NTF_RESULT (DidOpen.handle notif vscode_sock); *)
  | "textDocument/didSave" ->
    Printf.printf "didSave\n%!";
    let params = match notif.params with 
      | Some p -> Acsl_lsp.Types.DidSaveTextDocumentParams.t_of_json p
      | None -> assert false
    in
    let uri = params.textDocument.uri in 
    let _file = remove_file_scheme (remove_newline (remove_quotes uri)) in
    
    let buffer = Bytes.create 4096 in
    Printf.printf "create buffer\n%!";
    Unix.listen wrapper_sock 100;

    (* let ic = Unix.open_process_in (Filename.quote_command "frama-c" ["-acsl_lsp"; ("-did_save="^_file)]) in *)
    let ic = Unix.open_process_in ("frama-c -acsl_lsp -did_save="^_file ^" -wp") in
    (* let ic = Unix.open_process_in ("frama-c " ^ _file ^ " -acsl_lsp -did_save") in *)
    Printf.printf "open in\n%!";
    Printf.printf "before accept\n%!";
    let (plugin_sock, _) = Unix.accept wrapper_sock in
    Printf.printf "accept\n%!";
    (* Printf.printf "input line : %s\n%!" (Stdlib.input_line ic); *)
    (* let _ = Unix.wait () in *)
    ignore 
    (try 
      while true do
        Printf.printf "input line : %s\n%!" (Stdlib.input_line ic);
      done;
    with End_of_file -> Printf.printf "End of ic\n%!";);

    ignore (Unix.close_process_in ic);


    let bytes_read = Unix.recv plugin_sock buffer 0 (Bytes.length buffer) [] in 
    Printf.printf "recv\n%!";
    
    let result = Bytes.sub_string buffer 0 bytes_read in
    Unix.close plugin_sock;
    Printf.printf "close sock\n%!";

    Acsl_lsp.Types.RQ_RESULT (Json.load_string result);

  | "workspace/didChangeConfiguration" ->
    Printf.printf "didChangeConfiguration\n%!";
    Acsl_lsp.Types.RQ_RESULT (Configuration.request_configurations);
  | "vscodeacsl/computeCG" -> 
    Printf.printf "computeCG\n%!";
    Acsl_lsp.Types.NTF_RESULT(
      Callgraph.Cg.compute ();
      Callgraph.Cg.dump ();
      )
  | "exit" -> if !receivedShutdown then Unix._exit 0 else Unix._exit 1
  | _ ->  Acsl_lsp.Types.NTF_RESULT ()



let result_handler json_string vscode_sock = 
  ignore vscode_sock;
  let json = Json.load_string json_string in 
  let request = Acsl_lsp.Types.ResponseMessage.t_of_json json in 
  let result = match request.result with 
    | Some r -> r
    | None -> Printf.printf "No result \n%!"; assert false
  in 

  let id = Acsl_lsp.Utils.id_to_int request.id in
  match id with 
  | 123456789 -> (* if the result is request_configurations *)
    Configuration.save_configs (result);
    Acsl_lsp.Types.EMPTY ();
  | _ -> 
    Acsl_lsp.Types.EMPTY ()

(* todo : implement client error handling with different error codes *)
let error_handler json_string = 
  let json = Json.load_string json_string in 
  let request = Acsl_lsp.Types.ResponseMessage.t_of_json json in 
  let error = 
    match request.error with 
    | Some err -> err 
    | None -> Printf.printf "No error \n%!"; assert false
  in 
  Acsl_lsp.Types.ResponseError.json_of_t (error)

let handle (json_string : string) vscode_sock wrapper_sock : Acsl_lsp.Types.lsp_result = 
  (* if !receivedShutdown then 
    Acsl_lsp.Types.RQ_RESULT (Shutdown.shutdown_error (Acsl_lsp.Types.RequestMessage.t_of_json (Json.load_string json_string))) else  *)
  if (is_result json_string) then (* todo : how to do this with a match with *)
    begin
      Printf.printf "result_handler\n%!";
      result_handler json_string vscode_sock 
    end
  else if (is_error json_string) then 
    begin
      Printf.printf "error_handler\n%!";
      Acsl_lsp.Types.RQ_RESULT (error_handler json_string)
    end
  else if (is_notif json_string) then 
    begin
      Printf.printf "notif_handler\n%!";
      (* Printf.printf "Received from client : %s\n%!" json_string; *)
      notif_handler json_string vscode_sock wrapper_sock
    end
  else if (is_request json_string) then 
    begin
      Printf.printf "rq_handler\n%!";
      rq_handler json_string vscode_sock
    end
  else 
    raise (Failure "Unknown request")
