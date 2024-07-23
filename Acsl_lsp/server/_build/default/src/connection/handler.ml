

let is_result json_string = 
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      (List.exists (fun (key, _) -> key = "result") fields)
    | _ -> Settings.Self.debug ~level:1 "no result\n%!"; false
  with
  | Json.Error _ -> false

let is_error json_string = 
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      (List.exists (fun (key, _) -> key = "error") fields)
    | _ -> Settings.Self.debug ~level:1 "no error\n%!"; false

  with
  | Json.Error _ -> false

let is_notif json_string =
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      not (List.exists (fun (key, _) -> key = "id") fields)
    | _ -> Settings.Self.debug ~level:1 "no notif\n%!"; false

  with
  | Json.Error _ -> false

let is_request json_string =
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      (List.exists (fun (key, _) -> key = "id") fields)
    | _ -> Settings.Self.debug ~level:1 "no notif\n%!"; false

  with
  | Json.Error _ -> false
  
let rq_handler json_string sock =
  ignore sock;
  let json = Json.load_string json_string in 
  let request = Types.RequestMessage.t_of_json json in 
  let curr_method = request.method_ in 
  let id = request.id in
  try
    match curr_method with 
    | "initialize" -> 
      Types.RQ_RESULT (Initialize.initialize request);
    | "textDocument/definition" -> 
      Settings.Self.debug ~level:1 "definition\n%!";
      Types.RQ_RESULT (Find_def.find_def request sock);
    | "textDocument/declaration" -> 
      Settings.Self.debug ~level:1 "declaration\n%!";
      Types.RQ_RESULT (Find_decl.find_decl request sock);
    (* | "textDocument/completion" -> 
      Settings.Self.debug ~level:1 "completion\n%!";
      Types.RQ_RESULT (Completion.completion_items request); *)
    | "vscodeacsl/displayCIL" -> 
      Settings.Self.debug ~level:1 "displayCIL\n%!";
      Types.RQ_RESULT (Command.displayCIL id);
    | "shutdown" -> States.receivedShutdown := true; 
      Types.RQ_RESULT (Shutdown.shutdown request);
    | _ -> Types.RQ_RESULT (`Null)
  with _ -> 
    Settings.Self.debug ~level:1 "REQUEST ERROR \n%!";
    Types.RQ_RESULT (Utils.make_error "Error during request handling" (Utils.id_to_int id))

let notif_handler json_string sock = 
  let json = Json.load_string json_string in 
  let notif = Types.NotificationMessage.t_of_json json in 
  let curr_method = notif.method_ in 
  match curr_method with 
  | "initialized" -> 
    Settings.Self.debug ~level:1 "initialized\n%!";
    Initialized.init sock;
    Types.NTF_RESULT ();
  | "textDocument/didOpen" ->
    Settings.Self.debug ~level:1 "didOpen\n%!";
    Types.NTF_RESULT (DidOpen.handle notif sock);
  | "textDocument/didSave" ->
    Settings.Self.debug ~level:1 "didSave\n%!";
    Types.NTF_RESULT (DidSave.handle notif sock);
  (* | "textDocument/didChange" ->
    Settings.Self.debug ~level:1 "didChange\n%!";
    Types.NTF_RESULT (); *)
  | "workspace/didChangeConfiguration" ->
    Settings.Self.debug ~level:1 "didChangeConfiguration\n%!";
    Types.NTF_RESULT (Configuration.request_configurations sock);
  | "vscodeacsl/computeCG" -> 
    Settings.Self.debug ~level:1 "computeCG\n%!";
    Types.NTF_RESULT(
      Callgraph.Cg.compute ();
      Callgraph.Cg.dump ();
      )
  | "exit" -> if !States.receivedShutdown then Unix._exit 0 else Unix._exit 1
  | _ ->  Types.NTF_RESULT ()



let result_handler json_string sock = 
  ignore sock;
  let json = Json.load_string json_string in 
  let request = Types.ResponseMessage.t_of_json json in 
  let result = match request.result with 
    | Some r -> r
    | None -> Settings.Self.debug ~level:1 "No result \n%!"; assert false
  in 

  let id = Utils.id_to_int request.id in
  match id with 
  | 123456789 -> (* if the result is request_configurations *)
    Configuration.save_configs (result);
    Types.EMPTY (Configuration.set_framac_options ());
  | _ -> 
    Types.EMPTY ()

(* todo : implement client error handling with different error codes *)
let error_handler json_string = 
  let json = Json.load_string json_string in 
  let request = Types.ResponseMessage.t_of_json json in 
  let error = 
    match request.error with 
    | Some err -> err 
    | None -> Settings.Self.debug ~level:1 "No error \n%!"; assert false
  in 
  Types.ResponseError.json_of_t (error)

let handle (json_string : string) sock : Types.lsp_result = 
  (* if !States.receivedShutdown then 
    Types.RQ_RESULT (Shutdown.shutdown_error (Types.RequestMessage.t_of_json (Json.load_string json_string))) else  *)
  if (is_result json_string) then (* todo : how to do this with a match with *)
    begin
      Settings.Self.debug ~level:1 "result_handler\n%!";
      result_handler json_string sock
    end
  else if (is_error json_string) then 
    begin
      Settings.Self.debug ~level:1 "error_handler\n%!";
      Types.RQ_RESULT (error_handler json_string)
    end
  else if (is_notif json_string) then 
    begin
      Settings.Self.debug ~level:1 "notif_handler\n%!";
      (* Settings.Self.debug ~level:1 "Received from client : %s\n%!" json_string; *)
      notif_handler json_string sock
    end
  else if (is_request json_string) then 
    begin
      Settings.Self.debug ~level:1 "rq_handler\n%!";
      rq_handler json_string sock
    end
  else 
    raise (Failure "Unknown request")

