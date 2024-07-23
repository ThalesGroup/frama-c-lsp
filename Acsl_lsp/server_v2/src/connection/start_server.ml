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
    let data_size = Utils.getnumber (readcontlen sock) in 
    let data_buf = Bytes.make data_size '0' in
    let _req_data_len = Unix.read sock data_buf 0 data_size in

    let request_str = (Bytes.to_string data_buf) in
    let response : Types.lsp_result = Handler.handle request_str sock in
    match response with 
    | RQ_RESULT json -> 
      Utils.send_request sock (Json.save_string json); 
    | NTF_RESULT () -> ()
    | EMPTY () -> ()


  
let listen () =
  Settings.Self.debug ~level:1 "Connecting on port %d\n%!" server_port;
  (* the acsl language server is a client that connects to the server process launched by vs code on given port *)
  let (ic, oc) = Unix.open_connection (Unix.ADDR_INET (addr, server_port)) in 
  Settings.Self.debug ~level:1 "Connected on port %d\n%!" server_port;
(* 
  Kernel.Config.set (Filepath.pwd ());
  let list = Array.to_list (Filepath.readdir (Kernel.Config.get ())) in 
  List.iter (fun x ->
    Settings.Self.debug ~level:1 "Session dir has : %s\n%!" x;
  ) list; *)

  let client_sock = Unix.descr_of_in_channel ic in

  while true do
    handle_data client_sock;
    flush oc;
  done;
