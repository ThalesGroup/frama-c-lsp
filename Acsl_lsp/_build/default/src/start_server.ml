let pretty (fmt : Stdlib.Format.formatter) (x : 'a) : unit = 
  Format.fprintf fmt "a" ;
  x 
in

let equal (x : 'a) (y : 'a) : bool = x = y in

let my_callback (responses : 'a Server.Main.response list) : unit =
  List.iter (function
    | `Error _ -> Printf.printf "abc"
    | `Data _ -> Printf.printf "abc" 
    | `Killed _ -> ()
    | `Rejected _ -> ()
    | `Signal _ -> ()
    | `CmdLineOn -> ()
    | `CmdLineOff -> ()
  ) responses
in

(*let reqs = [
  [`Request (1234)]
] in*)

let fetch () : ('a Server.Main.message option) = Some {requests = []; callback = my_callback} in
let server = Server.Main.create ~equal:equal ~pretty:pretty ~fetch:fetch () in 
let () = Server.Main.start server in
let () = Server.Main.run server in

let my_exec_command (json : Json.t) : Json.t =
  let temp1 = Json.string json in
  Format.fprintf out "%s\n" temp1 ;
  Json.of_string "Command executed successfully"
in

let () = Server.Main.register `EXEC "Execution : " my_exec_command in
let result = Server.Main.exec "Execution : " (Json.of_string 
"{
  
 }") in

let temp2 = Json.string result in 

Printf.printf "\t\t\tRESULT = %s\n" temp2;