(* Define the JSON-RPC request and response types *)
type jsonrpc_request = {
  jsonrpc : Json.t;
  id : Json.t;
  method_name : Json.t;
  params : Json.t;
}

type jsonrpc_response = {
  jsonrpc : Json.t;
  id : Json.t;
  result : Json.t option;
  error : Json.t option;
}

let extract_json_from_request request =
  let start_index = String.index request '{' in
  let end_index = String.rindex request '}' in
  String.sub request start_index (end_index - start_index + 1)

(* Parse JSON-RPC request *)
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
        let method_name = List.assoc "method" fields  in
        let params = List.assoc "params" fields in
        Some { jsonrpc; method_name; params; id }
    | _ -> 
        Printf.printf "NONE\n%!";
      None
  with
  | _ -> None

(* Generate JSON-RPC response *)
let generate_response ?(result = None) ?(error = None) id =
  let jsonrpc = `String "2.0" in
  { jsonrpc; result; error; id }

(* Handle JSON-RPC request *)
let handle_request request =
  (* Perform some action based on the method_name in the request *)
  match request.method_name with
  | `String "textDocument/definition" ->
      Printf.printf "handle_request\n%!";
      (* Perform some action *)
      generate_response ~result:(Some (Json.of_string "Some result")) request.id
  | _ ->
      generate_response ~error:(Some (Json.of_string "Method not found")) request.id

