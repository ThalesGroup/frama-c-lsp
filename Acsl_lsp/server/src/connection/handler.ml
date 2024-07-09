

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
  let id = request.id in
  try
    match curr_method with 
    | "initialize" -> 
      Types.RQ_RESULT (Initialize.initialize request);
    | "textDocument/definition" -> 
      Printf.printf "definition\n%!";
      Types.RQ_RESULT (Find_def.find_def request sock);
    | "textDocument/declaration" -> 
      Printf.printf "declaration\n%!";
      Types.RQ_RESULT (Find_decl.find_decl request sock);
    | "vscodeacsl/displayCIL" -> 
      Printf.printf "displayCIL\n%!";
      Types.RQ_RESULT (Command.displayCIL id);
    | "shutdown" -> States.receivedShutdown := true; 
      Types.RQ_RESULT (Shutdown.shutdown request);
    | _ -> Types.RQ_RESULT (`Null)
  with _ -> 
    Types.RQ_RESULT (Utils.make_error "Error during request handling")

let notif_handler json_string sock = 
  let json = Json.load_string json_string in 
  let notif = Types.NotificationMessage.t_of_json json in 
  let curr_method = notif.method_ in 
  try
    match curr_method with 
    | "initialized" -> 
      States.initialized := true;
      Printf.printf "initialized\n%!";
      Initialized.init_folders sock;
      Types.NTF_RESULT ();
    | "textDocument/didOpen" ->
      Printf.printf "didOpen\n%!";
      Types.NTF_RESULT ();
    | "textDocument/didSave" ->
      Printf.printf "didSave\n%!";
      Types.NTF_RESULT (DidSave.handle notif sock);
    | "textDocument/didChange" ->
      Printf.printf "didChange\n%!";
      Types.NTF_RESULT ();
    | "workspace/didChangeConfiguration" ->
      Printf.printf "didChangeConfiguration\n%!";
      Types.NTF_RESULT (Configuration.request_configurations sock);
    | "exit" -> if !States.receivedShutdown then Unix._exit 0 else Unix._exit 1
    | _ ->  Types.NTF_RESULT ()
    with _ -> 
      Types.RQ_RESULT (Utils.make_error "Error during notification handling")


let result_handler json_string sock = 
  let json = Json.load_string json_string in 
  let request = Types.ResponseMessage.t_of_json json in 
  let result = request.result in 

  let id = match request.id with 
    | Str s -> Stdlib.int_of_string s 
    | Int i -> i 
    | Null -> 0
  in
  if (id = Utils.config_id) then (* if the result is request_configurations *)
    begin
      try 
        Configuration.save_configs (Option.get result);
        Types.EMPTY     
        (Load.init_files sock);
    with _ -> 
      Types.RQ_RESULT (Utils.make_error "Error during result handling")
    end
  else 
    Types.EMPTY ()

(* todo : implement client error handling with different error codes *)
let error_handler json_string = 
  let json = Json.load_string json_string in 
  let request = Types.ResponseMessage.t_of_json json in 
  let error = request.error in 
  Types.ResponseError.json_of_t (Option.get error)

let handle (json_string : string) sock : Types.lsp_result = 
  if !States.receivedShutdown then 
    Types.RQ_RESULT (Shutdown.shutdown_error (Types.RequestMessage.t_of_json (Json.load_string json_string)))
  else if (is_result json_string) then (* todo : how to do this with a match with *)
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

