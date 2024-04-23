let server_port = 8001

let start_server out () =
  let addr = Unix.ADDR_INET(Unix.inet_addr_any, server_port) in
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt s Unix.SO_REUSEADDR true;
  Unix.bind s addr;
  Unix.listen s 5;
  Printf.printf "Server listening on port %d\n" server_port;
  flush stdout;
  while true do
    let (client_socket, _) = Unix.accept s in
    (* Handling requests *)
    let buffer = Bytes.create 1024 in
    let bytes_read = Unix.recv client_socket buffer 0 (Bytes.length buffer) [] in
    if bytes_read > 0 then begin
      let request = Bytes.sub_string buffer 0 bytes_read in
      (* Check if the request contains the JSON data *)
      if String.contains request '{' then begin
        (* Extract the JSON part from the request *)
        let json_start = String.index request '{' in
        let json_end = String.rindex request '}' + 1 in
        let json_part = String.sub request json_start (json_end - json_start) in
        Format.printf "Received request: %s\n" json_part;
        Format.pp_print_flush out ();
        Format.fprintf out "%s\n" json_part; 
        Format.pp_print_flush out ();
        (* Write the JSON data to the file *)
        let chan = open_out "json.out" in
        output_string chan json_part;
        close_out chan;
      end;
      (* Send a response *)
      let response = "HTTP/1.1 200 OK\r\nContent-Length: 18\r\n\r\nHello, world! :DD\n" in
      let _ = Unix.send client_socket (Bytes.of_string response) 0 (String.length response) [] in
      ();
    end;
    Unix.close client_socket
  done

let launch_server () =
  try
    let chan = open_out "json.out" in
    let fmt = Format.formatter_of_out_channel chan in

    Find_def.browse_ast ();
    start_server fmt ();
    close_out chan;
  with
  | Sys_error e ->
    Printf.eprintf "Error opening output file: %s\n" e
  | _ ->
    Printf.eprintf "Unknown error occurred while opening output file.\n"
