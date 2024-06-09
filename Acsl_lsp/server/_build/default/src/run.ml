let run () =
  try
    (*if Settings.Testing.get () = true then (* "= true" or else it doesn't work *)
        Run_tests.run_tests ()
    else *)
    Start_server.listen ()
  with _ as exc ->
    let msg = Printexc.to_string exc in
    Printf.eprintf "There was an error in the server : %s\n" msg
    
let () = Db.Main.extend run