open Utils
open Types

class init_visitor (params : InitializeParams.t) = object
  inherit Visitor.frama_c_inplace
  val mutable json_out = None 
  method !vglob_aux g =
    (* do something with initialize params *)
    match g with
    | _ -> ignore params; Cil.DoChildren
end

(* cannot be called twice, must be called first *)
let initialize (params : InitializeParams.t) = 
    Printf.printf "initialize called\n";
    Visitor.visitFramacFileSameGlobals 
      (new init_visitor params) 
      (get_ast_from_file(Array.get (get params.workspace_folders) 0).uri ()) (* get uri of first and only path in workspaceFolders array *)

