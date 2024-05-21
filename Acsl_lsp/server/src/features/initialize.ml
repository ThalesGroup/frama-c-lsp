open Utils
open Types

(*
class init_visitor (params : InitializeParams.t) = object
  inherit Visitor.frama_c_inplace
  method !vglob_aux g =
    (* do something with initialize params *)
    match g with
    | _ -> ignore params;
      let capabilities = ServerCapabilities.create ~definitionProvider:(ServerCapabilities.Bool true) () in
      let serverInfo = InitializeResult.create_serverInfo ~name: "ACSL Language Server" ~version: (Some "1.0.0") () in
      InitializeResult.json_of_t (InitializeResult.create ~capabilities: (Some capabilities) ~serverInfo: (Some serverInfo) ());
      Cil.DoChildren
end
*)


(* cannot be called twice, must be called first *)
let initialize (params : InitializeParams.t) : Json.json = 
    (*let logic_list = Logic_env.find_all_logic_functions "__fc_nan" in 
    Printf.printf "logic_list len = %d\n%!" (List.length logic_list);
    List.iter (fun _ ->
      Printf.printf "Logic fun : %s\n%!" "e"
    ) logic_list;*)

    Printf.printf "initialize called\n";
    let ast = get_ast_from_file (Array.get (get params.workspace_folders) 0).uri in 
    ignore ast;
    
    let capabilities = ServerCapabilities.create ~definitionProvider:(ServerCapabilities.Bool true) () in
    let serverInfo = InitializeResult.create_serverInfo ~name: "ACSL Language Server" ~version:"1.0.0" () in
    let response = ResponseMessage.
    InitializeResult.json_of_t ();
       (* get uri of first and only path in workspaceFolders array *)

