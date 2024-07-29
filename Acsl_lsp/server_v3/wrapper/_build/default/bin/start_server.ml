let server_port = 8001
let wrapper_port = 8002
let plugin_port = 8003
let maxContLenBufSize = 50
let maxPendingRequests = 20
let defaultProtocolType = 0
let addr = Unix.inet_addr_of_string "127.0.0.1"

let getnumber str = 
  let regex = Str.regexp {|[0-9]+|} in 
  ignore (Str.search_forward regex str 0);
  int_of_string (Str.matched_string str)

let send_request client_sock response =
  let response_str = Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length response) response in
  let response_bytes = Bytes.of_string response_str in
  let sent = Unix.send client_sock response_bytes 0 (Bytes.length response_bytes) [] in
  ignore sent

let readcontlen sock : string = 
  let contlenbuf = Bytes.create 1 in
  let res = ref "" in 
  let curr_char = ref "" in 
  while not (String.equal !curr_char "\n") do (* read the content length line character by character *)
  let data_len = Unix.read sock contlenbuf 0 1 in 
    ignore data_len;
    curr_char := (Bytes.to_string contlenbuf) ;
    res := !res ^ !curr_char;
  done;
  ignore (Unix.read sock contlenbuf 0 1); (* consume remaining "\r\n" from request header *)
  ignore (Unix.read sock contlenbuf 0 1);
  !res

let handle_data vscode_sock wrapper_sock = 
    let data_size = getnumber (readcontlen vscode_sock) in 
    let data_buf = Bytes.make data_size '0' in
    let _req_data_len = Unix.read vscode_sock data_buf 0 data_size in

    let request_str = (Bytes.to_string data_buf) in
    (* Printf.printf "Received from client : %s\n\n%!" request_str; *)
    let response : Acsl_lsp.Types.lsp_result = Handler.handle request_str vscode_sock wrapper_sock in
    match response with 
    | RQ_RESULT json -> 
      send_request vscode_sock (Json.save_string json); 
    | NTF_RESULT _ -> ()
    | EMPTY _ -> ()

let connect () =
  
  (* plugin / wrapper communication *)
  let wrapper_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in 
  Unix.bind wrapper_sock (Unix.ADDR_INET(Unix.inet_addr_loopback, wrapper_port));

  (* vs code / wrapper communication *)
  Printf.printf "Connecting on port %d\n%!" server_port;
  let (ic, oc) = Unix.open_connection (Unix.ADDR_INET (addr, server_port)) in 
  Printf.printf "Connected on port %d\n%!" server_port;

  let vscode_sock = Unix.descr_of_in_channel ic in

  while true do
    handle_data vscode_sock wrapper_sock;
    flush oc;
  done;
