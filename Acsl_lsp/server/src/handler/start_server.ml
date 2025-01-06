let server_port = 8005
(* let wrapper_port = 8006 *)
(* let maxContLenBufSize = 50 *)
(* let maxPendingRequests = 20 *)
let defaultProtocolType = 0
let addr = Unix.inet_addr_of_string "127.0.0.1"

(*
module StringLoc = struct
  type t = string * Log.event

  let compare (x0, y0) (x1 , y1) : (string * Log.event) -> (string * Log.event) -> int =
    match String.compare x0 x1 with
      | 0 ->
        let msg_0 = y0.evt_message in
        let msg_1 = y1.evt_message in
        (match String.compare msg_0 msg_1 with
          | 0 ->
            let src_0 = y0.evt_source in
            let src_1 = y1.evt_source in
            (match src_0, src_1 with
            | None, None -> 0
            | None, Some _ -> -1
            | Some _, None -> 1
            | Some pos_0, Some pos_1 ->
              (match Stdlib.compare pos_0.pos_lnum pos_1.pos_lnum with
                | 0 -> (match Stdlib.compare pos_0.pos_bol pos_1.pos_bol with
                        | 0 -> Stdlib.compare pos_0.pos_cnum pos_1.pos_cnum 
                        | c -> c)
                | c -> c))
          | c -> c)
      | c -> c
end

module StringLocMap = Map.Make(StringLoc)
*)



let getnumber str = 
  let regex = Str.regexp {|[0-9]+|} in 
  ignore (Str.search_forward regex str 0);
  int_of_string (Str.matched_string str)

let send_request server_sock response =
  let response_str = Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length response) response in
  let response_bytes = Bytes.of_string response_str in
  let sent = Unix.send server_sock response_bytes 0 (Bytes.length response_bytes) [] in
  Lsp.Self.debug ~level:4 "Size of sent content : %d\n%!" sent

let update_empty_diagnostics string_json =
  let json_result = Json.load_string string_json in
   match json_result with
    | `Assoc fields ->
      if (List.exists (fun (key, _) -> key = "result") fields) then ()
      else if (List.exists (fun (key, _) -> key = "error") fields) then ()
      else if (not (List.exists (fun (key, _) -> key = "id") fields)) then
          try
            let lsp_notification = Lsp_types.NotificationMessage.t_of_json json_result in
            if lsp_notification.method_ = "textDocument/publishDiagnostics" then
              let json_params = lsp_notification.params in
              match json_params with
                | Some params ->
                  let params = Lsp_types.PublishDiagnosticsParams.t_of_json params in
                  let file_uri = params.uri in
                  DidSave.diag_map := DidSave.StringMap.add file_uri [] !DidSave.diag_map;
                  Lsp.Self.debug ~level:4 "Updated map for  %s" file_uri
                | None -> Lsp.Self.debug ~level:4 "Updated map for  1"; ()
            else Lsp.Self.debug ~level:4 "Updated map for  2"; ()
          with  exn -> 
            Lsp.Self.debug ~level:3 "Could not handle the previous request : %s, %s\n" (Printexc.exn_slot_name exn) (Printexc.get_backtrace ())        
      else ()
    | _ -> ()

let clear_diagnostics_notification file_uri = 
  let lsp_message = (Lsp_types.PublishDiagnosticsParams.create ~uri:file_uri ~diagnostics:([]) ()) in
  let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"textDocument/publishDiagnostics" ~params:(Lsp_types.PublishDiagnosticsParams.json_of_t lsp_message) () in
  Lsp_types.NotificationMessage.json_of_t lsp_notification  

let send_empty_diagnostics server_sock uri _diag_list =
  let json_notification = clear_diagnostics_notification uri in
  send_request server_sock (Json.save_string json_notification)


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

let handle_request server_sock =
    try 
      (* Lsp.Self.Debug.set (!Configuration.global_params.acslLsp); *)
      let data_size = getnumber (readcontlen server_sock) in 
      Lsp.Self.debug ~level:3 "Received from client : %d data size \n\n%!" data_size;
      let data_buf = Bytes.make data_size '0' in
      let _req_data_len = Unix.read server_sock data_buf 0 data_size in
      let request_str = (Bytes.to_string data_buf) in
      Lsp.Self.debug ~level:3 "Received from client : %s\n\n%!" request_str;
      DidSave.StringMap.iter (send_empty_diagnostics server_sock) !DidSave.diag_map;
      let (response, pid) : (Lsp_types.lsp_result * int) = Lsp_handler.handle request_str server_sock in
      match response with
      | CONTENT string_json ->
        Lsp.Self.debug ~level:3 "Sending to client : %s\n\n%!" string_json;
        let string_json_list = Str.split (Str.regexp ":::") string_json in
        List.iter update_empty_diagnostics string_json_list;
        DidSave.StringMap.iter (send_empty_diagnostics server_sock) !DidSave.diag_map;
        List.iter (send_request server_sock) (string_json_list);
        pid
      | EMPTY _ -> pid
    with 
    | Lsp_handler.UnknownRequest id as exn ->
      Lsp.Self.debug ~level:3 "Main Server is busy : %s, %s\n" (Printexc.exn_slot_name exn) (Printexc.get_backtrace ());
      let lsp_message = Lsp_types.ShowMessageParams.create ~type_: Lsp_types.MessageType.Error ~message: "Server is busy !!" () in
      let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"window/showMessage" ~params: (Lsp_types.ShowMessageParams.json_of_t lsp_message) () in
      let json_notification = Json.save_string (Lsp_types.NotificationMessage.json_of_t lsp_notification) in
      send_request server_sock json_notification;
      id
    | exn ->
      Lsp.Self.debug ~level:3 "Server is busy : %s, %s\n" (Printexc.exn_slot_name exn) (Printexc.get_backtrace ());
      let lsp_message = Lsp_types.ShowMessageParams.create ~type_: Lsp_types.MessageType.Error ~message: "Server is busy !" () in
      let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"window/showMessage" ~params: (Lsp_types.ShowMessageParams.json_of_t lsp_message) () in
      let json_notification = Json.save_string (Lsp_types.NotificationMessage.json_of_t lsp_notification) in
      send_request server_sock json_notification;
      0
      

let connect () =
  (* vs code / wrapper communication *)
  Lsp.Self.debug ~level:4 "Connecting on port %d\n%!" server_port;
  let (ic, oc) = Unix.open_connection (Unix.ADDR_INET (addr, server_port)) in 
  Lsp.Self.debug ~level:4 "Connected on port %d\n%!" server_port;
  let server_sock = Unix.descr_of_in_channel ic in
  while true do
    let pid = handle_request server_sock in
    flush oc;
    if (pid = 0) then Unix._exit 0
    else if (pid = 1) then ()
    else ignore (Unix.waitpid [Unix.WNOHANG] (-1))
  done;
