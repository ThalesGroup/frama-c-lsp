open Utils
open Types

class init_visitor (params : InitializeParams.t) = object
  inherit Visitor.frama_c_inplace
  val mutable json_out = None 
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


let initialize (params : InitializeParams.t) = 
    Printf.printf "initialize called\n";
    Visitor.visitFramacFileSameGlobals 
      (new init_visitor params) 
      (get_ast_from_file(Array.get (get params.workspace_folders) 0).uri ()) (* get uri of first and only path in workspaceFolders array *)

