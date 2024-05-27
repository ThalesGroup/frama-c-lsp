open Types
open Initialize
open Find_def
open Exit

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
  let request = RequestMessage.t_of_json json in 
  let curr_method = request.method_ in 
  Printf.printf "request handler\n%!";
  match curr_method with 
  | "initialize" -> 
    initialize request;
  | "textDocument/definition" -> 
    find_def (Ast.get ()) request;
  | _ -> `Null

let notif_handler json_string = 
let json = Json.load_string json_string in 
let notif = NotificationMessage.t_of_json json in 
let curr_method = notif.method_ in 
Printf.printf "notification handler\n%!";
match curr_method with 
| "initialized" -> 
  `String "Received initialized notification";
| "exit" -> 
  exit;
| _ -> `Null


let handle json_string = 
  if (is_notif json_string) then 
    notif_handler json_string
  else 
    rq_handler json_string 
