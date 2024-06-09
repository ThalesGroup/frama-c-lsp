
let is_notif json_string =
  try
    let json = Json.load_string json_string in
    match json with
    | `Assoc fields ->
      let has_method = List.exists (fun (key, _) -> key = "method") fields in
      let has_id = List.exists (fun (key, _) -> key = "id") fields in
      has_method && not has_id
    | _ -> false
  with
  | Json.Error _ -> false
  
let rq_handler json_string =
  let json = Json.load_string json_string in 
  let request = Types.RequestMessage.t_of_json json in 
  let curr_method = request.method_ in 
  match curr_method with 
  | "initialize" -> 
    Types.RQ_RESULT (Initialize.initialize request);
  | "textDocument/definition" -> 
    Types.RQ_RESULT (Find_def.find_def request);
  | _ -> Types.RQ_RESULT (`Null)

let notif_handler json_string = 
let json = Json.load_string json_string in 
let notif = Types.NotificationMessage.t_of_json json in 
let curr_method = notif.method_ in 
match curr_method with 
| "initialized" -> 
  Types.NTF_RESULT ()
| "exit" -> 
  Types.NTF_RESULT ()
| _ ->  Types.NTF_RESULT ()

let handle json_string : Types.lsp_result = 
  if (is_notif json_string) then 
    notif_handler json_string
  else 
    rq_handler json_string 
