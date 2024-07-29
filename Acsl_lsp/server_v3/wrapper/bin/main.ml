let run () =
  Printf.printf "Running Dune executable\n%!";
  try
    (* if Acsl_lsp.Settings.Enabled.get() then  *)
      Start_server.connect ()
  with exn ->
    Printf.printf "There was an error in the server %s:\n Backtrace : %s\n%!" (Printexc.to_string exn) (Printexc.get_backtrace ())
    
let () = run ()