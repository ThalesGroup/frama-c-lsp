let server_port = 8001
let maxContLenBufSize = 50
let maxPendingRequests = 20
let defaultProtocolType = 0
let addr = Unix.inet_addr_of_string "127.0.0.1"

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

let handle_data sock = 
    let data_size = Acsl_lsp.Utils.getnumber (readcontlen sock) in 
    let data_buf = Bytes.make data_size '0' in
    let _req_data_len = Unix.read sock data_buf 0 data_size in

    let request_str = (Bytes.to_string data_buf) in
    (* Printf.printf "Received from client : %s\n\n%!" request_str; *)
    let response : Acsl_lsp.Types.lsp_result = Handler.handle request_str sock in
    match response with 
    | RQ_RESULT json -> 
      Acsl_lsp.Utils.send_request sock (Json.save_string json); 
    | NTF_RESULT _ -> ()
    | EMPTY _ -> ()


  
let connect () =
  Acsl_lsp.Settings.Self.debug ~level:3 "Connecting on port %d\n%!" server_port;
  let (ic, oc) = Unix.open_connection (Unix.ADDR_INET (addr, server_port)) in 
  Acsl_lsp.Settings.Self.debug ~level:3 "Connected on port %d\n%!" server_port;

  let client_sock = Unix.descr_of_in_channel ic in

  while true do
    handle_data client_sock;
    flush oc;
  done;
