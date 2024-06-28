let run () =
  try
    if Settings.Enabled.get() then 
      (*if Settings.Testing.get () = true then 
          Run_tests.run_tests ()
      else *)
      Start_server.listen ()
  with _ as exc ->
    let msg = Printexc.to_string exc in
    Printf.eprintf "[acsl-lsp] There was an error in the server : %s\n" msg
    
let () = Db.Main.extend run