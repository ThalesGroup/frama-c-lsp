let run () =
  try
    if Settings.Enabled.get() then 
      Start_server.listen ()
  with exn ->
    Settings.Self.debug ~level:1 "There was an error in the server %s:\n Backtrace : %s\n%!" (Printexc.to_string exn) (Printexc.get_backtrace ())
    
let () = Db.Main.extend run