
let receivedShutdown = ref false
let is_result json_string = 
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      (List.exists (fun (key, _) -> key = "result") fields)
    | _ -> Acsl_lsp.Settings.Self.debug ~level:1 "no result\n%!"; false
  with
  | Json.Error _ -> false

let is_error json_string = 
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      (List.exists (fun (key, _) -> key = "error") fields)
    | _ -> Acsl_lsp.Settings.Self.debug ~level:1 "no error\n%!"; false

  with
  | Json.Error _ -> false

let is_notif json_string =
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      not (List.exists (fun (key, _) -> key = "id") fields)
    | _ -> Acsl_lsp.Settings.Self.debug ~level:1 "no notif\n%!"; false

  with
  | Json.Error _ -> false

let is_request json_string =
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      (List.exists (fun (key, _) -> key = "id") fields)
    | _ -> Acsl_lsp.Settings.Self.debug ~level:1 "no request\n%!"; false

  with
  | Json.Error _ -> false
  
let rq_handler json_string sock =
  ignore sock;
  let json = Json.load_string json_string in 
  let request = Acsl_lsp.Types.RequestMessage.t_of_json json in 
  let curr_method = request.method_ in 
  let id = request.id in
  try
    match curr_method with 
    | "initialize" -> 
      Acsl_lsp.Types.RQ_RESULT (Acsl_lsp.Initialize.initialize request);
    | "textDocument/definition" -> 
      Acsl_lsp.Settings.Self.debug ~level:1 "definition\n%!";
      Acsl_lsp.Types.RQ_RESULT (Acsl_lsp.Find_def.find_def request);
    | "textDocument/declaration" -> 
      Acsl_lsp.Settings.Self.debug ~level:1 "declaration\n%!";
      Acsl_lsp.Types.RQ_RESULT (Acsl_lsp.Find_decl.find_decl request);
    (* | "textDocument/completion" -> 
      Acsl_lsp.Settings.Self.debug ~level:1 "completion\n%!";
      Acsl_lsp.Types.RQ_RESULT (Completion.completion_items request); *)
    | "vscodeacsl/displayCIL" -> 
      Acsl_lsp.Settings.Self.debug ~level:1 "displayCIL\n%!";
      Acsl_lsp.Types.RQ_RESULT (Acsl_lsp.Command.displayCIL id);
    | "shutdown" -> receivedShutdown := true; 
      Acsl_lsp.Types.RQ_RESULT (Acsl_lsp.Shutdown.shutdown request);
    | _ -> Acsl_lsp.Types.RQ_RESULT (`Null)
  with _ -> 
    Acsl_lsp.Settings.Self.debug ~level:1 "REQUEST ERROR \n%!";
    Acsl_lsp.Types.RQ_RESULT (Acsl_lsp.Utils.make_error "Error during request handling" (Acsl_lsp.Utils.id_to_int id))

let notif_handler json_string sock = ignore sock;
  let json = Json.load_string json_string in 
  let notif = Acsl_lsp.Types.NotificationMessage.t_of_json json in 
  let curr_method = notif.method_ in 
  match curr_method with 
  | "initialized" -> 
    Acsl_lsp.Settings.Self.debug ~level:1 "initialized\n%!";
    Acsl_lsp.Utils.send_request sock (Json.save_string Configuration.request_configurations);
    Acsl_lsp.Types.RQ_RESULT (Acsl_lsp.Initialized.init)
  (* | "textDocument/didOpen" ->
    Acsl_lsp.Settings.Self.debug ~level:1 "didOpen\n%!";
    Acsl_lsp.Types.NTF_RESULT (DidOpen.handle notif sock); *)
  | "textDocument/didSave" ->
    Acsl_lsp.Settings.Self.debug ~level:1 "didSave\n%!";
      Printf.printf "handler, didsave global_params length : %d\n%!" (List.length (Frama_c_kernel.Json.list !Configuration.global_params));
      Acsl_lsp.Types.RQ_RESULT (Acsl_lsp.DidSave.handle notif !Configuration.global_params);
  (* | "textDocument/didChange" ->
    Acsl_lsp.Settings.Self.debug ~level:1 "didChange\n%!";
    Acsl_lsp.Types.NTF_RESULT (); *)
  | "workspace/didChangeConfiguration" ->
    Acsl_lsp.Settings.Self.debug ~level:1 "didChangeConfiguration\n%!";
    Acsl_lsp.Types.RQ_RESULT (Configuration.request_configurations);
  | "vscodeacsl/computeCG" -> 
    Acsl_lsp.Settings.Self.debug ~level:1 "computeCG\n%!";
    Acsl_lsp.Types.NTF_RESULT(
      Callgraph.Cg.compute ();
      Callgraph.Cg.dump ();
      )
  | "exit" -> if !receivedShutdown then Unix._exit 0 else Unix._exit 1
  | _ ->  Acsl_lsp.Types.NTF_RESULT ()



let result_handler json_string sock = 
  ignore sock;
  let json = Json.load_string json_string in 
  let request = Acsl_lsp.Types.ResponseMessage.t_of_json json in 
  let result = match request.result with 
    | Some r -> r
    | None -> Acsl_lsp.Settings.Self.debug ~level:1 "No result \n%!"; assert false
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
    | None -> Acsl_lsp.Settings.Self.debug ~level:1 "No error \n%!"; assert false
  in 
  Acsl_lsp.Types.ResponseError.json_of_t (error)

let handle (json_string : string) sock : Acsl_lsp.Types.lsp_result = 
  (* if !receivedShutdown then 
    Acsl_lsp.Types.RQ_RESULT (Shutdown.shutdown_error (Acsl_lsp.Types.RequestMessage.t_of_json (Json.load_string json_string))) else  *)
  if (is_result json_string) then (* todo : how to do this with a match with *)
    begin
      Acsl_lsp.Settings.Self.debug ~level:1 "result_handler\n%!";
      result_handler json_string sock
    end
  else if (is_error json_string) then 
    begin
      Acsl_lsp.Settings.Self.debug ~level:1 "error_handler\n%!";
      Acsl_lsp.Types.RQ_RESULT (error_handler json_string)
    end
  else if (is_notif json_string) then 
    begin
      Acsl_lsp.Settings.Self.debug ~level:1 "notif_handler\n%!";
      (* Acsl_lsp.Settings.Self.debug ~level:1 "Received from client : %s\n%!" json_string; *)
      notif_handler json_string sock
    end
  else if (is_request json_string) then 
    begin
      Acsl_lsp.Settings.Self.debug ~level:1 "rq_handler\n%!";
      rq_handler json_string sock
    end
  else 
    raise (Failure "Unknown request")
