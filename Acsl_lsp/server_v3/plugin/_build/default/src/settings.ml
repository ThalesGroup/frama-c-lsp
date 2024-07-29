let server_port = 8001
let wrapper_port = 8002
let plugin_port = 8003
let maxContLenBufSize = 50
let maxPendingRequests = 20
let defaultProtocolType = 0
let addr = Unix.inet_addr_of_string "127.0.0.1"

module Self = Plugin.Register 
(struct
  let name = "ACSL LSP"
  let shortname = "acsl_lsp"
  let help = "Activates lsp support for ACSL/C"
end)

module Enabled = Self.False
(struct
  let option_name = "-acsl_lsp"
  let help = "when on (off by default), activates lsp support for ACSL/C"
end)

module Did_save = Self.String (* filename *)
(struct
  let option_name = "-did_save"
  let help = "didSave request"
  let arg_name = "did save"
  let default = ""

end)

(* module Did_save = Self.False (* filename *)
(struct
  let option_name = "-did_save"
  let help = "didSave request"

end) *)


let run () = 
  Cmdline.run_after_early_stage (fun () -> Log.add_listener ~kind:[Log.Feedback; Log.Warning; Log.Error; Log.Failure] (PublishDiagnostics.error_event_handler));
  try 
    let plugin_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in 
    (* Unix.bind plugin_sock (Unix.ADDR_INET(Unix.inet_addr_loopback, plugin_port)); *)

    if Enabled.get () then 
      (Printf.printf "Acsl lsp enable\n%!";
      (* Utils.send_request (socket) (DidSave.handle Did_save.get ()) *)
        if not (String.equal (Did_save.get ()) "") then
          (* if (Did_save.get ()) then *)
          (
            Printf.printf "did save file \n%!";
            Unix.connect plugin_sock (Unix.ADDR_INET(Unix.inet_addr_loopback, wrapper_port));
            (* let data = Json.save_string (DidSave.handle (Filepath.Normalized.to_pretty_string (List.nth (Kernel.Files.get ()) 0))) in *)
            let data = Json.save_string (DidSave.handle (Did_save.get ())) in
            ignore (Unix.sendto_substring 
              plugin_sock 
              (data) 
              0 
              (String.length data)
              []
              (Unix.ADDR_INET(Unix.inet_addr_loopback, wrapper_port))
              );
          )
      )
  with Sys_error _ as exc ->
  let msg = Printexc.to_string exc in
  Printf.eprintf "There was an error: %s\n" msg 

let () = Db.Main.extend run