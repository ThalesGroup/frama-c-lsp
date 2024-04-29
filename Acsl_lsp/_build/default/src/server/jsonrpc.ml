
(* Define the JSON-RPC request and response types *)
type jsonrpc_request = {
  jsonrpc : Json.t;
  id : Json.t;
  method_ : Json.t;
  params : Json.t ;
}

type jsonrpc_response = {
  jsonrpc : Json.t;
  id : Json.t;
  result : Json.t option;
  error : Json.t option;
}

type jsonrpc_error = {
  code : Json.t;
  message : Json.t;
  data : Json.t option;
}

(* Extract the JSON part only from the request (removes the header part etc.) *)
let extract_json_from_request request =
  let start_index = String.index request '{' in
  let end_index = String.rindex request '}' in
  String.sub request start_index (end_index - start_index + 1)

(* Parse JSON-RPC request string into Json.json type *)
let parse_request json_str =
  try
  
  let json = Json.load_string (extract_json_from_request json_str) in
  match json with
    | `Assoc fields ->
        Printf.printf "json rpc \n%!";
        let jsonrpc = List.assoc "jsonrpc" fields  in
        Printf.printf "id\n%!";
        let id = List.assoc "id" fields  in
        Printf.printf "method name\n%!";
        let method_ = List.assoc "method" fields  in
        Printf.printf "params\n%!";
        let params = List.assoc "params" fields in
        Some { jsonrpc; method_; params; id }
    | _ -> 
        Printf.printf "NONE\n%!";
      None
  with
  | _ -> None

(* Generate JSON-RPC response *)
let generate_response ?(result = None) ?(error = None) id =
  let jsonrpc = `String "2.0" in
  { jsonrpc; result; error; id }

