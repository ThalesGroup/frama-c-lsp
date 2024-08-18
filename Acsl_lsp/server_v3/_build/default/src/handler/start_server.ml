let server_port = 8005
let wrapper_port = 8006
let maxContLenBufSize = 50
let maxPendingRequests = 20
let defaultProtocolType = 0
let addr = Unix.inet_addr_of_string "127.0.0.1"

let getnumber str = 
  let regex = Str.regexp {|[0-9]+|} in 
  ignore (Str.search_forward regex str 0);
  int_of_string (Str.matched_string str)

let send_request server_sock response =
  let response_str = Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length response) response in
  let response_bytes = Bytes.of_string response_str in
  let sent = Unix.send server_sock response_bytes 0 (Bytes.length response_bytes) [] in
  Printf.printf "Size of sent content : %d\n%!" sent

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

let handle_data server_sock = 
    try 
      let data_size = getnumber (readcontlen server_sock) in 
      let data_buf = Bytes.make data_size '0' in
      let _req_data_len = Unix.read server_sock data_buf 0 data_size in
      let request_str = (Bytes.to_string data_buf) in
      (* Printf.printf "Received from client : %s\n\n%!" request_str; *)
      let response : Lsp_types.lsp_result = Lsp_handler.handle request_str server_sock in
      match response with 
      | CONTENT string_json -> 
        Printf.printf "Sending to client : %s\n\n%!" string_json;
        send_request server_sock (string_json); 
      | EMPTY _ -> ()
    with exn -> 
      Printf.printf "Could not handle the previous request : %s, %s\n" (Printexc.exn_slot_name exn) (Printexc.get_backtrace ())
      

let connect () =
  
  (* plugin / wrapper communication *)

  (* vs code / wrapper communication *)
  Printf.printf "Connecting on port %d\n%!" server_port;
  let (ic, oc) = Unix.open_connection (Unix.ADDR_INET (addr, server_port)) in 
  Printf.printf "Connected on port %d\n%!" server_port;

  let server_sock = Unix.descr_of_in_channel ic in

  while true do
    handle_data server_sock;
    flush oc;
  done;
