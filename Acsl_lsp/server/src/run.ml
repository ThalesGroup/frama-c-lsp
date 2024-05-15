let run () =
  try
    if Settings.Testing.get () = true then (* "= true" or else it doesn't work *)
      begin
        Run_tests.run_tests ();
      end
    else 
      (*Start_server.start ();*)
      Start_server.listen ();
  with Sys_error _ as exc ->
    let msg = Printexc.to_string exc in
    Printf.eprintf "There was an error: %s\n" msg
    
let () = Db.Main.extend run