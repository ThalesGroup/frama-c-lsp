let server_port = 8001
let maxContLenBufSize = 50
let maxPendingRequests = 20
let defaultProtocolType = 0
let addr = Unix.inet_addr_of_string "127.0.0.1"

let send_response client_sock response =
  let response_str = Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length response) response in
  let response_bytes = Bytes.of_string response_str in
  let sent = Unix.send client_sock response_bytes 0 (Bytes.length response_bytes) [] in
  ignore sent

let getnumber str = 
  let regex = Str.regexp {|[0-9]+|} in 
  ignore (Str.search_forward regex str 0);
  int_of_string (Str.matched_string str)

let readcontlen sock : string = 
  let contlenbuf = Bytes.create 1 in
  let res = ref "" in 
  let curr_char = ref "" in 
  while (String.equal !curr_char "\n") = false do 
  let data_len = Unix.read sock contlenbuf 0 1 in 
    ignore data_len;
    curr_char := (Bytes.to_string contlenbuf) ;
    res := !res ^ !curr_char;
  done;
  ignore (Unix.read sock contlenbuf 0 1); (* consume remaining "\r\n" from reaquest header *)
  ignore (Unix.read sock contlenbuf 0 1);
  !res

let handle_data sock = 
  let data_size = getnumber (readcontlen sock) in 
  let data_buf = Bytes.create data_size in
  let req_data_len = Unix.read sock data_buf 0 data_size in
  ignore req_data_len;
  Printf.printf "Received from client: %s bytes\n%!" (Bytes.to_string data_buf);
  
  let request_str = (Bytes.to_string data_buf) in

  let response : Types.lsp_result = Handler.handle request_str in
  match response with 
  | RQ_RESULT json -> send_response sock (Json.save_string json);
  | NTF_RESULT () -> ()

let listen () =
  Printf.printf "Connecting on port %d\n" server_port;
  let (ic, oc) = Unix.open_connection (Unix.ADDR_INET (addr, server_port)) in 
  Printf.printf "Connected on port %d\n" server_port;
  let client_sock = Unix.descr_of_in_channel ic in
  Unix.set_close_on_exec client_sock;

  while true do
    handle_data client_sock
  done;
  close_out oc


  (*
let listen1 () =
  let addr = Unix.ADDR_INET(Unix.inet_addr_any, server_port) in
  let server_sock = Unix.socket PF_INET SOCK_STREAM defaultProtocolType in
  
  Unix.bind server_sock addr;
  Unix.listen server_sock maxPendingRequests;
  Printf.printf "Server listening on port %d\n" server_port;

  let (client_sock, client_addr) = Unix.accept server_sock in
  match client_addr with 
  | ADDR_UNIX s -> Printf.printf "Accepted client sock : %s\n" s ;
  | ADDR_INET (inet, port) -> Printf.printf "Accepted client sock : %s:%d\n" (Unix.string_of_inet_addr inet) port ;

  let cpt = ref 1 in
  ignore cpt;
  while true do
    let contlenbuf = Bytes.create maxContLenBufSize in
    let data_len = Unix.recv client_sock contlenbuf 0 (Bytes.length contlenbuf) [] in (* receives first content length *)
    Printf.printf "Content length request : %s\n" (Bytes.to_string contlenbuf);
    ignore data_len;

    let data_size = getcontlen (Bytes.to_string contlenbuf) in 
    let data_buf = Bytes.create data_size in
    let req_data_len = Unix.recv client_sock data_buf 0 data_size [] in
    ignore req_data_len;
    Printf.printf "Received from client: %s\n" (Bytes.to_string data_buf);
    
    let request_str = (Bytes.to_string data_buf) in

    (* Send response *)
    let response : Types.lsp_result = Handler.handle request_str in
    match response with 
    | RQ_RESULT json -> send_response client_sock (Json.save_string json);
    | NTF_RESULT () -> ();
    
    (*close client_sock*)
    (*Printf.printf "cpt = %d\n" !cpt;
    cpt := !cpt + 1;*)
  done;

  Unix.close server_sock

let aux_func (ic : in_channel) (oc : out_channel) =
  Printf.printf "here 1\n";
  let str = input_line ic in
  Printf.printf "read : %s\n" str;
  ignore (output_string oc "test\n");
  flush oc
*)
(*let handle_socket (csock : Unix.file_descr) =
  let contlenbuf = Bytes.create maxContLenBufSize in
  let data_len = Unix.recv csock contlenbuf 0 (Bytes.length contlenbuf) [] in (* receives first content length *)
  Printf.printf "Content length request : %s\n" (Bytes.to_string contlenbuf);
  ignore data_len;

  let data_size = getcontlen (Bytes.to_string contlenbuf) in 
  let data_buf = Bytes.create data_size in
  let req_data_len = Unix.recv csock data_buf 0 data_size [] in
  ignore req_data_len;
  Printf.printf "Received from client: %s\n" (Bytes.to_string data_buf);
  
  let request_str = (Bytes.to_string data_buf) in

  (* Send response *)
  let response = Handler.handle request_str in
  send_response csock (Json.save_string response)

let handle_channels (ic : in_channel) (oc : out_channel) =
  let contlen = getcontlen (input_line ic) in 
  ignore (input_line ic);
  Printf.printf "content len : %d\n" contlen ;
  let buffer = Bytes.create contlen in 
  let data = input ic buffer 0 contlen in
  Printf.printf "data len : %d\n" data ;

  let response = Handler.handle (Bytes.to_string buffer) in 
  (*Printf.printf "handled : %s\n" (Json.save_string response);*)

  output_string oc (Json.save_string response); 
  output_string stdout ("stdout output\n");
  flush oc;
  (*let csock = Unix.getsockname (Unix.descr_of_in_channel ic) in 
  Unix.sendto (Unix.descr_of_out_channel oc) buffer 0 (Bytes.length buffer) [] csock |> ignore;
  Unix.write (Unix.descr_of_out_channel oc) buffer 0 (Bytes.length buffer) |> ignore;*)
  Printf.printf "flushed.\n"

let fx (ic: in_channel) (oc: out_channel) = 
  ignore ic; ignore oc*)


(*let listen2 () = 
  Printf.printf "Server launched.\n";
  let (ic, oc) = Unix.open_connection (Unix.ADDR_INET (addr, server_port)) in 
  Printf.printf "open connection.\n";

  (*let csock = Unix.descr_of_in_channel ic in
  let buf = (Bytes.create 10) in
  Unix.recv csock buf 0 10 [] |> ignore ;
  Printf.printf " received with recv : %s\n%!" (Bytes.to_string buf);

  let res = Bytes.of_string result in 
  Unix.send csock res 0 (Bytes.length res) [] |> ignore;
  Printf.printf "data sent with send.\n";*)
  
  handle_channels ic oc

let listen3 () = 
  Printf.printf "Server launched.\n";
  let this = Unix.socket PF_INET SOCK_STREAM 0 in 
  Unix.establish_server handle_channels (Unix.getsockname this);
  Printf.printf "established server.\n"
*)