open Types
open Initialize
open Utils
  
let rq_handler json_string () =

  let json = Json.load_string json_string in 
  let request = RequestMessage.t_of_json json in 
  let curr_method = request.method_ in 

  match curr_method with 
  | "initialize" -> 
    Printf.printf "handler\n%!";
    let params = InitializeParams.t_of_json (get request.params) in
    initialize params;
  | _ -> Printf.printf "None\n%!";
