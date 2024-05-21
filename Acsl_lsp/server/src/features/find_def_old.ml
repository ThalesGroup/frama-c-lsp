(*open Types
open Utils

let process_annotation (params : DefinitionParams.t) (ga : Cil_types.global_annotation) : Location.t * unit = 
  match ga with 
  | Dfun_or_pred (li, (pos1, pos2)) -> 
      let uri = params.textDocument.uri in 
      let curr_pos = position_t_to_filepath_position uri params.position in
      let line = params.position.line in 
      let ch = params.position.character in 
      ignore line; ignore ch;
      (* if the uri = global def uri and position is contained within definition range *)
      Printf.printf "lv_name = %s\n%!" li.l_var_info.lv_name;
      Printf.printf "line %d, ch %d\n%!" line ch;
      (*Printf.printf "expr_name = %s\n%!" expr_name;*)
      if (pos_is_within_range (curr_pos) (pos1, pos2)) = true 
      then
        (* Replace the compared string by what we got from reading the file at the given position in the json data *)
        (*let comp_result = compare li.l_var_info.lv_name "valid" in
        if comp_result = 0 then *)
        Printf.printf "comp result : %s, [%d:%d -> %d:%d] %s\n%!"
          li.l_var_info.lv_name
          pos1.Filepath.pos_lnum
          (pos1.Filepath.pos_cnum - pos1.Filepath.pos_bol)
          pos2.Filepath.pos_lnum
          (pos2.Filepath.pos_cnum - pos2.Filepath.pos_bol)
          (Filepath.Normalized.to_pretty_string pos1.Filepath.pos_path);
      let range = Range.create (Position.create 45 45) (Position.create 45 45) () in
      (Location.create params.textDocument.uri range ()), () ;
  | _ -> 
    let range = Range.create (Position.create 72 72) (Position.create 72 72) () in
    (Location.create params.textDocument.uri range ()), ()
  
let process_global (params : DefinitionParams.t) (g : Cil_types.global) : Location.t * unit =
  match g with
  | GAnnot (ga, _) -> (process_annotation params ga);
  | _  -> 
    let range = Range.create (Position.create 12 12) (Position.create 12 12) () in
    (Location.create params.textDocument.uri range ()), ()

let find_def (file : Cil_types.file) (params : DefinitionParams.t) : Json.t = 
    print_predicates file;
    Printf.printf "find_def called\n%!";
    let result, func = process_global params in
    let () = Cil.iterGlobals file func in
    result
    


(*let print_stmt_fundec (file : Cil_types.file) = 
  match file.globinit with 
    | Some x ->  Printf.printf "%s\n%!" x.svar.vname
    | None -> Printf.printf "No\n%!"
*)
*)