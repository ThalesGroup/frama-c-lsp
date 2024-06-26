let receivedShutdown = ref false
let initialized = ref false 

let is_result json_string = 
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      (List.exists (fun (key, _) -> key = "result") fields)
    | _ -> false
  with
  | Json.Error _ -> false

let is_error json_string = 
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      (List.exists (fun (key, _) -> key = "error") fields)
    | _ -> false
  with
  | Json.Error _ -> false

let is_notif json_string =
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      not (List.exists (fun (key, _) -> key = "id") fields)
    | _ -> false
  with
  | Json.Error _ -> false
  
let rq_handler json_string sock =
  ignore sock;
  let json = Json.load_string json_string in 
  let request = Types.RequestMessage.t_of_json json in 
  let curr_method = request.method_ in 
  match curr_method with 
  | "initialize" -> 
    Types.RQ_RESULT (Initialize.initialize request);
  | "textDocument/definition" -> 
    Printf.printf "definition\n%!";
    Types.RQ_RESULT (Find_def.find_def request);
  | "shutdown" -> receivedShutdown := true; Types.RQ_RESULT (Shutdown.shutdown request);
  | _ -> Types.RQ_RESULT (`Null)

let notif_handler json_string sock = 
  let json = Json.load_string json_string in 
  let notif = Types.NotificationMessage.t_of_json json in 
  let curr_method = notif.method_ in 
  match curr_method with 
  | "initialized" -> 
    initialized := true;
    Printf.printf "initialized\n%!";
    Initialized.init_folders sock;
    Types.NTF_RESULT ();
  | "textDocument/didOpen" ->
    Printf.printf "didOpen\n%!";
    Types.NTF_RESULT (DidOpen.handle notif);
  | "textDocument/didSave" ->
    Printf.printf "didSave\n%!";
    Types.NTF_RESULT (DidSave.handle notif sock);
  | "textDocument/didChange" ->
    Printf.printf "didChange\n%!";
    Types.NTF_RESULT ();
  (* | "workspace/didChangeConfiguration" ->
    Printf.printf "didChangeConfiguration\n%!";
    Types.NTF_RESULT (Initialize.init_files sock); *)
  | "exit" -> if !receivedShutdown then Unix._exit 0 else Unix._exit 1
  | _ ->  Types.NTF_RESULT ()

let result_handler json_string sock = 
  let json = Json.load_string json_string in 
  let request = Types.ResponseMessage.t_of_json json in 
  let result = request.result in 
  
  let id = match request.id with 
    | Str s -> Stdlib.int_of_string s 
    | Int i -> i 
    | Null -> 0
  in
  if (id = Utils.config_id) then 
    begin
      Configuration.save_config (Option.get result);
      Types.SEND_NONE (Initialize.init_files sock)
    end
  else 
    Types.SEND_NONE ()

let error_handler json_string = 
  let json = Json.load_string json_string in 
  let request = Types.ResponseMessage.t_of_json json in 
  let error = request.error in 
  Types.ResponseError.json_of_t (Option.get error)

let handle (json_string : string) sock : Types.lsp_result = 
  if !receivedShutdown then 
    Types.RQ_RESULT (Shutdown.shutdown_error (Types.RequestMessage.t_of_json (Json.load_string json_string)))
  else if (is_result json_string) then 
    begin
      Printf.printf "result_handler\n%!";
      result_handler json_string sock
    end
  else if (is_error json_string) then 
    begin
      Printf.printf "error_handler\n%!";
      Types.RQ_RESULT (error_handler json_string)
    end
  else if (is_notif json_string) then 
    begin
      Printf.printf "notif_handler\n%!";
      notif_handler json_string sock
    end
  else 
    begin
      Printf.printf "rq_handler\n%!";
      rq_handler json_string sock
    end

