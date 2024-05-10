open Utils
open Types

class def_visitor (params : DefinitionParams.t) = object
  inherit Visitor.frama_c_inplace
  val mutable json_out = None 
  (* We need to know if the character at given line in the json is located in the range of  *)
  method !vglob_aux g =
    match g with
    | GAnnot (Dfun_or_pred (li, (pos1, pos2)), _) -> 
      ignore pos1;
      ignore pos2;
        Printf.printf "li : %s, params : %s\n%!" li.l_var_info.lv_name params.textDocument.uri;
        (* Read json from input *)
        (*if pos_is_within_range (get (parse_request request)).params.position (pos1, pos2) then*)
        (* Replace the compared string by what we got from reading the file at the given position in the json data *)
        (*let comp_result = compare li.l_var_info.lv_name "valid" in
        if comp_result = 0 then *)
        (*Printf.printf "comp result : %s, [%d:%d -> %d:%d] %s\n"
          li.l_var_info.lv_name
          pos1.Filepath.pos_lnum
          (pos1.Filepath.pos_cnum - pos1.Filepath.pos_bol)
          pos2.Filepath.pos_lnum
          (pos2.Filepath.pos_cnum - pos2.Filepath.pos_bol)
          (Filepath.Normalized.to_pretty_string pos1.Filepath.pos_path)
        ;*) Cil.DoChildren
    | GAnnot (Dtype (lti, _), _) -> 
      Printf.printf "lti : %s\n" lti.lt_name
      ; Cil.DoChildren
    | _ -> Cil.DoChildren
end


let find_def params = 
    Printf.printf "find_def called\n";
    Visitor.visitFramacFileSameGlobals 
      (new def_visitor params) 
      (get_ast_from_file params.textDocument.uri ())

