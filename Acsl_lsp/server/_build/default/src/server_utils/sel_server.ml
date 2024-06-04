open Unix
open Printf

let send_jsonrpc_response client_sock response =
  let response_str = Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length response) response in
  let response_bytes = Bytes.of_string response_str in
  let bytes_sent = ref 0 in
  while !bytes_sent < Bytes.length response_bytes do
    let sent = send client_sock response_bytes !bytes_sent (Bytes.length response_bytes - !bytes_sent) [] in
    if sent <= 0 then
      failwith "Failed to send response";
    bytes_sent := !bytes_sent + sent
  done
  
let server_port = 8001 (* todo : maybe change port *)
let rec receive_all client_sock buffer =
  let bytes_read = recv client_sock buffer 0 (Bytes.length buffer) [] in
  if bytes_read > 0 then
    Bytes.sub buffer 0 bytes_read
  else
    receive_all client_sock buffer

let listen () =
  let addr = ADDR_INET(inet_addr_loopback, server_port) in
  let server_sock = socket PF_INET SOCK_STREAM 0 in
  
  setsockopt server_sock SO_REUSEADDR true;

  bind server_sock addr;
  listen server_sock 500;
  Printf.printf "Server listening on port %d\n%!" server_port;
  while true do
    let (client_sock, _) = accept server_sock in
    let request_event = Sel.On.httpcle client_sock (function
      | Ok buff ->
        begin
          printf "UI req ready";
          try Handler.handle (Bytes.to_string buff)
          with _ ->
            printf "failed to decode json";
            Json.load_string {|{
              "jsonrpc": "2.0",
              "id": 0,
              "error": {
                "code": -32601
              }
            }|}
        end
      | Error _ ->
          printf ("failed to read message");
          (* do not remove this line otherwise the server stays running in some scenarios *)
          exit 0) in 

    ignore request_event;
  close client_sock;
    
  done;

  close server_sock
