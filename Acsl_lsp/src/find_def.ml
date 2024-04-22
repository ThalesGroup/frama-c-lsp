open Cil_types
module Find_def = Find_def


class print_annot out = object 
    inherit Visitor.frama_c_inplace

      method !vglob_aux g = 
        match g with 
        (GCompTag (_, _)|GCompTagDecl (_, _)|GEnumTag (_, _)|GEnumTagDecl (_, _)|
        GVarDecl (_, _)|GFun (_, _)|GVar (_, _, _)|GAsm (_, _)|
        GPragma (_, _)|GText _ |GType (_, _)) -> Format.fprintf out "\n"; Cil.DoChildren
        | GFunDecl (_, vf, (pos_start, _)) -> 
          Format.fprintf out "Function %s at line : %d\n" vf.vorig_name pos_start.pos_lnum ; Cil.DoChildren (* doesn't work *)
        | GAnnot (ga, _) -> match ga with 
          | Dfun_or_pred (_, (pos_start,pos_end)) -> 
            (*Filepath.Normalized.pp_abs out pos_start.pos_path;
            Format.fprintf out "\nhere\n";*)

            let rpcversion = 2.0 in
            let id = 1 in
            (*let mth = "textDocument/definition" in*)
            Filepath.reset_symbolic_dirs ();
            let uri = Filepath.Normalized.to_pretty_string (Filepath.Normalized.of_string (Filepath.Normalized.to_pretty_string pos_start.pos_path)) in
            let start_line = pos_start.pos_lnum in 
            let end_line = pos_end.pos_lnum in 
            let start_character = (pos_start.pos_cnum - pos_start.pos_bol) in 
            let end_character = (pos_end.pos_cnum - pos_end.pos_bol) in 

            (* Parsing data into json structure *)
            let result = (`Assoc [
              ("targetUri", `String uri);
              ("targetRange", `Assoc [
                ("start", `Assoc [
                  ("line", `Int start_line);
                  ("character", `Int start_character)
                ]);
                ("end", `Assoc [
                  ("line", `Int end_line);
                  ("character", `Int end_character)
                ])
              ]);
              ("targetSelection", `Assoc [
                ("start", `Assoc [
                  ("line",`Int start_line);
                  ("character",`Int start_character)
                ]);
                ("end", `Assoc [
                  ("line",`Int end_line); 
                  ("character", `Int end_character)
                ])
              ])
            ]) in

            let response = (`Assoc [ ("jsonrpc", `Float rpcversion);
                              ("id", `Int id);
                              ("result", result)
                            ]) in 

            let json_response = Yojson.Basic.to_string response in
            Format.fprintf out "%s\n" json_response;

            (*let json_response = "" in 
            let json_assoc_list = Json.assoc response in

            List.iter (fun elt -> 
              let json_response = json_response ^ (Json.string elt) in
            ) json_assoc_list; Cil.DoChildren*) (* TODO :  find a way to print json without using yojson *)

              
            Cil.DoChildren 
          | _ -> Format.fprintf out "other\n"; Cil.DoChildren
      
        

end

let browse_ast () =
  try
    let chan = open_out "find_def.out" in
    let fmt = Format.formatter_of_out_channel chan in
    Visitor.visitFramacFileSameGlobals (new print_annot fmt) (Ast.get());
    close_out chan
  with
  | Sys_error e ->
    Printf.eprintf "Error opening output file: %s\n" e
  | _ ->
    Printf.eprintf "Unknown error occurred while opening output file.\n"
