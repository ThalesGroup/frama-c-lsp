open Types
open Utils
open Printer_tag
(*
let process_annotation (params : DefinitionParams.t) (ga : Cil_types.global_annotation) = 
  match ga with 
  | Dfun_or_pred (li, (pos1, pos2)) -> 
      let uri = params.textDocument.uri in 
      let curr_pos = position_t_to_filepath_position uri params.position in
      let line = params.position.line in 
      let ch = params.position.character in 
      ignore line; ignore ch;
      let target_loc = get (Printer_tag.loc_to_localizable curr_pos) in

      Printf.printf "lv_name = %s\n%!" li.l_var_info.lv_name;
     
  | _ -> Printf.printf "\n"
  *)
  
let process_global (params : DefinitionParams.t) (g : Cil_types.global) =
  ignore g;
  let uri = params.textDocument.uri in 
  let curr_pos = position_t_to_filepath_position uri params.position in
  let target_loc = Printer_tag.loc_to_localizable ?precise_col:(Some true) curr_pos in
  (*let vinfo =  Printer_tag.varinfo_of_localizable target_loc in *)
  match target_loc with 
  | Some loc -> Printf.printf "%s\n%!" (label loc)
  | None -> Printf.printf "No decl\n%!"


  (*match target_loc with 
  | PGlobal glob -> 
    (match glob with 
    | GAnnot _ ->
      Printf.printf "GAnnot\n%!"
    | _ -> Printf.printf "none\n%!"
      )
  | _ -> Printf.printf "none\n%!"*)
  (*Printf.printf "glabel : %s\n%!" (Printer_tag.glabel g)*)
  (*Printf.printf "label : %s\n%!" (Printer_tag.label target_loc)*)
  
 (* match g with
  | GAnnot (ga, _) -> 
    let ips = Property.ip_of_global_annotation ga in
    let target_loc2 = if (List.length ips) > 0 then Property.source (List.nth ips 0) else None in 
    (match target_loc2 with 
    | Some position -> Printf.printf "pos start : %d:%d\n%!" position.pos_lnum (position.pos_cnum - position.pos_bol)
    | None -> Printf.printf "");
    ignore target_loc2;
    (*match ga with 
    | Dfun_or_pred (li,_) ->  
      Printf.printf "annot : %s, location start : %d:%d\n" li.l_var_info.lv_name (fst target_loc2).pos_lnum ((fst target_loc2).pos_cnum - (fst target_loc2).pos_bol)
    | _ -> Printf.printf ""*)
  | _  -> Printf.printf "\n"*)

let find_def (file : Cil_types.file) (params : DefinitionParams.t) = 
    print_predicates file;
    Printf.printf "find_def called\n%!";

    let file = params.textDocument.uri in
    let pos = position_t_to_filepath_position file params.position in
    try
      let behaviors = retrieve_acsl_annotations pos in
      List.iter (fun _ ->
        Printf.printf "Behavior: %d\n%!" pos.pos_lnum
      ) behaviors
    with
    | Failure msg -> Printf.eprintf "Error: %s\n%!" msg
    | e -> Printf.eprintf "Unexpected error: %s\n%!" (Printexc.to_string e)  

    (*Cil.iterGlobals file (process_global params)*)
    
    
