 (*************************************************************************
 *                                                                        *
 *  This file is part of Frama-C/LSP plugin.                              *
 *                                                                        *
 *  You can redistribute it and/or modify it under the terms of the GNU   *
 *  General Public License as published by the Free Software Foundation,  *
 *  version 3.                                                            *
 *                                                                        *
 *  It is distributed in the hope that it will be useful,                 *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the          *
 *  GNU General Public License for more details.                          *
 *                                                                        *
 *  See the GNU General Public License version 3                          *
 *  for more details (enclosed in the file LICENSE.md                     *
 *                                                                        *
 *************************************************************************)

(*
    Version : 1.0
    - Finds frama_c terms and predicates (valid_read_string, valid_string, minimum, maximum, ...)
    - Finds user defined terms and predicates
    - Finds C function, struct, union and enum definitions
*)
let glob_visitor loca symbol = object 
  inherit Visitor.frama_c_inplace
    method !vglob_aux g =
      match g with 
      | GEnumTag (ei,loc) -> if (String.equal symbol ei.eorig_name) then loca := Some loc; Cil.DoChildren
      | GCompTag (ci,loc) -> if (String.equal symbol ci.corig_name) then loca := Some loc; Cil.DoChildren
      | GType (ti,loc) -> if (String.equal symbol ti.torig_name) then loca := Some loc; Cil.DoChildren
      | GVar (vi, _, loc) -> if (String.equal symbol vi.vname) then loca := Some loc; Cil.DoChildren
      | GFun (fd,loc) -> if (String.equal symbol fd.svar.vname) then loca := Some loc; Cil.DoChildren
      | GAnnot (ga, _) -> 
        (match ga with 
        | Dinvariant (li, loc) | Dtype_annot (li, loc) | Dfun_or_pred (li, loc) -> 
            if (String.equal symbol li.l_var_info.lv_name) then loca := Some loc; Cil.DoChildren
        | Dtype (lti, loc) -> if (String.equal symbol lti.lt_name) then loca := Some loc; Cil.DoChildren
        | Dlemma (str,_,_,_,_,loc) -> if (String.equal symbol str) then loca := Some loc; Cil.DoChildren
        | _ -> Cil.DoChildren)
      | _ -> Cil.DoChildren
end

(* Utile pour les variables locales aux fonctions et aux contrats *)
let vrbl_visitor loca symbol = object 
  inherit Visitor.frama_c_inplace
  method! vvdec v = 
    if (String.equal symbol v.vname) then (loca := Some v.vdecl; Cil.DoChildren)
    else Cil.DoChildren
end 

let retrieve_location (pos : Filepath.position) =
  let loca = ref None in 
  let current_file = Filepath.to_string pos.pos_path in
  let symbol = Utils.retrieve_symbol pos.pos_lnum (pos.pos_cnum - pos.pos_bol) current_file in  
  
  (* 1. On interroge l'AST de Frama-C (Objets C et Annotations ACSL) *)
  Visitor.visitFramacFile (glob_visitor loca symbol) (Ast.get ()); 
  
  match !loca with
  | Some loc -> loc
  | None -> 
      (* 2. Si non trouvé dans l'AST, on renvoie (pos, pos). 
         Cela forcera la réponse `null` dans la fonction `find` ci-dessous,
         permettant à Clangd ou CppTools de prendre le relais. *)
      (pos, pos) 

let find id definitionFile line ch : string =
  let pos = Utils.to_filepath_position definitionFile line ch in
  try 
    let (pos1, pos2) = retrieve_location pos in
    
    (* CONDITION CRITIQUE : Si pos1 = pos2, on répond `null` pour déléguer *)
    if pos1 = pos2 then 
      let lsp_response = Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:`Null () in
      Json.save_string (Lsp_types.ResponseMessage.json_of_t lsp_response)
    else
      (* On a trouvé un objet précis (ACSL ou C parsé) *)
      let lsp_position_1 = Lsp_types.Position.create (pos1.pos_lnum - 1) (pos1.pos_cnum - pos1.pos_bol) in
      let lsp_position_2 = Lsp_types.Position.create (pos2.pos_lnum - 1) (pos2.pos_cnum - pos2.pos_bol) in
      let lsp_range = Lsp_types.Range.create lsp_position_1 lsp_position_2 in
      let filename_str = Filepath.to_string pos1.pos_path in
      let lsp_location = Lsp_types.Location.create (Utils.to_lsp_uri filename_str) lsp_range in
      let json_location = Lsp_types.Location.json_of_t lsp_location in
      let lsp_response = Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:json_location () in
      Json.save_string (Lsp_types.ResponseMessage.json_of_t lsp_response)
  with exn -> 
    (* En cas d'erreur interne, on préfère renvoyer null que de bloquer le client *)
    Json.save_string (Utils.make_error (Printexc.to_string exn) id)