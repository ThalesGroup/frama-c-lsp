(*open Unix
open Types

let server_port = 8001
let rec receive_all client_sock buffer =
  let bytes_read = recv client_sock buffer 0 (Bytes.length buffer) [] in
  if bytes_read > 0 then
    Bytes.sub buffer 0 bytes_read
  else
    receive_all client_sock buffer
let start_server () =
  let addr = ADDR_INET(inet_addr_any, server_port) in
  let server_sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt server_sock SO_REUSEADDR true;
  bind server_sock addr;
  listen server_sock 5;
  Printf.printf "Server listening on port %d\n%!" server_port;

  while true do
    let (client_sock, _) = accept server_sock in
    let buffer = Bytes.create 1024 in
    let request_data = receive_all client_sock buffer in

    (* Process the received request data *)
    let request_str = Bytes.to_string request_data in
    Printf.printf "Request received: %s\n%!" request_str;

    (* Parse the request *)
    let parsed_request = parse_request request_str in

    match parsed_request with
    | Some request ->
        (* Handle the request and generate a response *)
        let response = Rq_handler.handle_request request in
        let response_str = Json.save_string @@ `Assoc [
          "jsonrpc", response.jsonrpc;
          "result", (match response.result with Some res -> res | None -> `Null);
          "error", (match response.error with Some err -> err | None -> `Null);
          "id", response.id
        ] in
        (* Send the response back to the client *)
        let response = "HTTP/1.1 404 Not Found\r\nContent-Length: 26\r\n\r\nError: Resource not found\n" in
        let _ = Unix.send client_sock (Bytes.of_string response) 0 (String.length response) [] in
        Printf.printf "Response sent%s\n %!" response_str;
    | None ->
        Printf.eprintf "Invalid JSON-RPC request\n%!";

    (* Close the client socket *)
    close client_sock
  done

*)