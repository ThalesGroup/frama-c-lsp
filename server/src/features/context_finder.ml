
open Cil_types


let distance_cursor_prop (cursor_line : int) (p : Property.t) : int =
  let (pos_start, pos_end) = Property.location p in
  let l_start = pos_start.pos_lnum in
  let l_end = pos_end.pos_lnum in
  if cursor_line >= l_start && cursor_line <= l_end then 0
  else min (abs (cursor_line - l_start)) (abs (cursor_line - l_end))


let get_wrapper_port () =
  let len = Array.length Sys.argv in
  let rec find_port i =
    if i >= len then 0 
    else 
      let arg = Sys.argv.(i) in
      if String.length arg > 13 && String.sub arg 0 13 = "-lsp-wrapper=" then
        try int_of_string (String.sub arg 13 (String.length arg - 13))
        with _ -> 0
      else find_port (i + 1)
  in
  find_port 0


let run (id: int) (file: string) (line: int) (_col: int) : unit =

  let best_prop = ref None in
  let min_distance = ref 1000 in

  let check_candidate p =
     let (pos_start, _) = Property.location p in
     let p_file = Filepath.to_string pos_start.pos_path in
     if (Filename.basename p_file) = (Filename.basename file) then begin
        let dist = distance_cursor_prop line p in
        if dist < 5 && dist < !min_distance then begin
           min_distance := dist;
           best_prop := Some p
        end
     end
  in

  Globals.Functions.iter (fun kf ->
      let spec = Annotations.funspec kf in
      let contract_props = Property.ip_of_spec kf Kglobal ~active:[] spec in
      let body_props = ref [] in
      if Kernel_function.is_definition kf then begin
         let def = Kernel_function.get_definition kf in
         List.iter (fun stmt ->
            let annots = Annotations.code_annot stmt in
            List.iter (fun annot -> 
               body_props := !body_props @ (Property.ip_of_code_annot kf stmt annot)
            ) annots
         ) def.sallstmts
      end;
      List.iter check_candidate (contract_props @ !body_props)
  );


  let (fct_name, prop_name, _needs_warning) = match !best_prop with
  | Some p ->
      let f = match Property.get_kf p with 
        | Some kf -> Kernel_function.get_name kf 
        | None -> "@axiomatic" 
      in

      let explicit_names = Property.get_names p in
      
      let (final_name, warning) = 
        match p with

        | Property.IPAssigns _ -> ("@assigns", false)


        | Property.IPPredicate { Property.ip_kind = kind; _ } -> 
             begin match explicit_names with
             | name :: _ -> (name, false) (* On a un nom explicite, super ! *)
             | [] -> 
                 match kind with
                 | Property.PKEnsures _ -> ("@ensures", true)
                 | Property.PKRequires _ -> ("@requires", true)
                 | _ -> ("@all", true)
             end


        | Property.IPDecrease _ -> ("@variant", false)

        | _ -> 
             match explicit_names with
             | name :: _ -> (name, false)
             | [] -> ("@all", true) (* Fallback ultime *)
      in
      (f, final_name, warning)

  | None -> ("@all", "@all", false)
  in


  let json_result = `Assoc [
    ("function", `String fct_name);
    ("property", `String prop_name)

  ] in

  let lsp_message = Lsp_types.ResponseMessage.create 
      ~jsonrpc:"2.0" 
      ~id:(Lsp_types.Int id) 
      ~result:json_result 
      () 
  in
  let json_str = Json.save_string (Lsp_types.ResponseMessage.json_of_t lsp_message) in

  let port = get_wrapper_port () in
  if port <> 0 then begin
      try
        let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        let addr = Unix.inet_addr_loopback in
        Unix.connect sock (Unix.ADDR_INET (addr, port));
        let response = Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length json_str) json_str in
        let _ = Unix.send sock (Bytes.of_string response) 0 (String.length response) [] in
        Unix.close sock
      with _ -> ()
  end;
  
  exit 0