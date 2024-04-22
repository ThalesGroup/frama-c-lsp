let server_port = 8001

let start_server out () =
  let addr = Unix.ADDR_INET(Unix.inet_addr_any, server_port) in
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt s Unix.SO_REUSEADDR true;
  Unix.bind s addr;
  Unix.listen s 5;
  Printf.printf "Server listening on port %d\n" server_port;
  while true do
    let (client_socket, _) = Unix.accept s in
    (* Handling requests *)
    let buffer = Bytes.create 1024 in
    let bytes_read = Unix.recv client_socket buffer 0 (Bytes.length buffer) [] in
    if bytes_read > 0 then begin
      let request = Bytes.sub_string buffer 0 bytes_read in
      Printf.printf "Received request: %s\n" request;
      Format.fprintf out "Received request: %s\n" request;
      Format.pp_print_flush out ();
      (* Prepare a response *)
      let response = "HTTP/1.1 200 OK\r\nContent-Length: 17\r\n\r\nHello, world! :DD" in
      let _ = Unix.send client_socket (Bytes.of_string response) 0 (String.length response) [] in
      ();
    end;
    Unix.close client_socket
  done


let launch_server () =
  let chan = open_out "server.out" in
  let fmt = Format.formatter_of_out_channel chan in
  start_server fmt ()
  let () = Find_def.browse_ast ()
  
let () = Db.Main.extend launch_server
