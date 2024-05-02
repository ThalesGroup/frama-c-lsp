open Run_tests
let run () =
  try
    if Settings.Testing.get () = true then (* i am forced to write "= true" or else it doesn't work*)
      begin
        run_tests ();
      end
  with Sys_error _ as exc ->
    let msg = Printexc.to_string exc in
    Printf.eprintf "There was an error: %s\n" msg
    
let () = Db.Main.extend run
    