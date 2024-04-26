open Test_find_predicate_definition
let run () =
  try
    if Settings.Testing.get () = true then (* i am forced to write "= true" or else it doesn't work*)
      begin
        Printf.printf "Write the code to open a file named 'results.txt' and write the tests output in it\n";
        run_tests ();
      end
  with Sys_error _ as exc ->
    let msg = Printexc.to_string exc in
    Printf.eprintf "There was an error: %s\n" msg
    
let () = Db.Main.extend run
    