open Unix

let server_port = 8001
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
  listen server_sock 5;
  Printf.printf "Server listening on port %d\n%!" server_port;

  while true do
    let (client_sock, _) = accept server_sock in
    let buffer = Bytes.create 1024 in
    let request_data = receive_all client_sock buffer in

    (* Process the received request data *)
    let request_str = Utils.extract_json_from_request (Bytes.to_string request_data) in
    Printf.printf "Request received: %s\n%!" request_str;

    (* Parse the request *)
    Handler.rq_handler request_str ();
    
    close client_sock
  done

