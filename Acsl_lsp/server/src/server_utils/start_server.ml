open Unix

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
  let addr = ADDR_INET(inet_addr_any, server_port) in
  let server_sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt server_sock SO_REUSEADDR true;
  bind server_sock addr;
  listen server_sock 50;
  Printf.printf "Server listening on port %d\n%!" server_port;

  let (client_sock, _) = accept ~cloexec:( true) server_sock in
  while true do
  let buffer = Bytes.create 50000 in
  let request_data = ref (receive_all client_sock buffer) in 

  (* Process the received request data *)
  Printf.printf "Received from client: %s\n%!" (Bytes.to_string !request_data);
  
  let request_str = (Bytes.to_string !request_data) in
  (* Send response *)
  let response = Handler.handle request_str in
  send_jsonrpc_response client_sock (Json.save_string response);
  
  (*close client_sock*)
  done;

  close server_sock
