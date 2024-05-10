open Utils
open Types

class def_visitor (params : DefinitionParams.t) = object
  inherit Visitor.frama_c_inplace
  val mutable json_out = None 
  (* We need to know if the character at given line in the json is located in the range of  *)
  method !vglob_aux g =
    match g with
    | GAnnot (Dfun_or_pred (li, (pos1, pos2)), _) -> 
      ignore params;
      ignore pos1;
      ignore li;
      ignore pos2;
      Cil.DoChildren
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

