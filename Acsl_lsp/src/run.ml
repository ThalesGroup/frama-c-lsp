let run () =
  try
    if Settings.Testing.get () 
      then Printf.printf "Write the code to open a file named 'results.tx' and write the tests output in it";
  with Sys_error _ as exc ->
    let msg = Printexc.to_string exc in
    Printf.eprintf "There was an error: %s\n" msg
    
let () = Db.Main.extend run
    