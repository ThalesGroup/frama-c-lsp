let receivedShutdown = ref false

let is_notif json_string =
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      not (List.exists (fun (key, _) -> key = "id") fields)
    | _ -> false
  with
  | Json.Error _ -> false
  
let rq_handler json_string =
  let json = Json.load_string json_string in 
  let request = Types.RequestMessage.t_of_json json in 
  let curr_method = request.method_ in 
  match curr_method with 
  | "initialize" -> 
    Printf.printf "initialize\n%!";
    Types.RQ_RESULT (Initialize.initialize request);
  | "textDocument/definition" -> 
    Printf.printf "definition\n%!";
    Types.RQ_RESULT (Find_def.find_def request);
  | "shutdown" -> receivedShutdown := true; Types.RQ_RESULT (Shutdown.shutdown request);
  | _ -> Types.RQ_RESULT (`Null)

let notif_handler json_string = 
let json = Json.load_string json_string in 
let notif = Types.NotificationMessage.t_of_json json in 
let curr_method = notif.method_ in 
match curr_method with 
| "initialized" -> 
  Types.NTF_RESULT ()
| "exit" -> if !receivedShutdown then Unix._exit 0 else Unix._exit 1
| _ ->  Types.NTF_RESULT ()

let handle (json_string : string): Types.lsp_result = 
  if !receivedShutdown then 
    Types.RQ_RESULT (Shutdown.shutdown_error (Types.RequestMessage.t_of_json (Json.load_string json_string)))
  else if (is_notif json_string) then 
    begin
      Printf.printf "notif_handler\n%!";
      notif_handler json_string
    end
  else 
    begin
      Printf.printf "rq_handler\n%!";
      rq_handler json_string
    end
