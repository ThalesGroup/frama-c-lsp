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
      | GEnumTag (ei,loc) -> 
        if (String.equal symbol ei.eorig_name) then
          loca := Some loc;
        Cil.DoChildren
      | GCompTag (ci,loc) -> 
        if (String.equal symbol ci.corig_name) then
          loca := Some loc;
        Cil.DoChildren
      | GType (ti,loc) -> 
        if (String.equal symbol ti.torig_name) then
          loca := Some loc;
        Cil.DoChildren
      | GVar (vi, _, loc) -> 
          Options.Self.debug ~level:1 "var : %s, symbol : %s \n%!" vi.vname symbol;
          if (String.equal symbol vi.vname) then
          begin
            loca := Some loc;
          end;
        Cil.DoChildren
      | GText _ -> 
        Cil.DoChildren
      | GFun (fd,loc) -> 
        if (String.equal symbol fd.svar.vname;) then
          begin
            Options.Self.debug ~level:1 "fun : %s\n%!" fd.svar.vname;
            loca := Some loc;
          end;
        Cil.DoChildren
      | GAsm (s,loc) -> 
        if (String.equal symbol s) then
          loca := Some loc;
        Cil.DoChildren
      | GPragma (_,_) -> 
        Options.Self.debug ~level:1 "Pragma : %s\n%!" (Pretty_utils.to_string Printer.pp_global g);
        Cil.DoChildren
      | GAnnot (ga, _) -> 
        (match ga with 
        | Dinvariant (li, loc) -> 
          if (String.equal symbol li.l_var_info.lv_name) then
            begin
              Options.Self.debug ~level:1 "invariant : %s\n%!" li.l_var_info.lv_name;
              loca := Some loc;
            end;
          Cil.DoChildren;
        | Dtype (lti, loc) -> 
          if (String.equal symbol lti.lt_name) then
            begin
              Options.Self.debug ~level:1 "logic type : %s\n%!" lti.lt_name;
              loca := Some loc;
            end;
          Cil.DoChildren
        | Dtype_annot (li, loc) -> 
          if (String.equal symbol li.l_var_info.lv_name) then
            begin
              Options.Self.debug ~level:1 "type annot : %s\n%!" li.l_var_info.lv_name;
              loca := Some loc;
            end;
          Cil.DoChildren
        | Dfun_or_pred (li,loc) ->
          if (String.equal symbol li.l_var_info.lv_name) then
            begin
              Options.Self.debug ~level:1 "fun or pred : %s\n%!" li.l_var_info.lv_name;
              loca := Some loc;
            end;
          Cil.DoChildren
        | Dlemma (str,_,_,_,_,loc) ->
          Options.Self.debug ~level:1 "lemma : %s\n%!" str;
          if (String.equal symbol str) then
            begin
              loca := Some loc;
            end;
          Cil.DoChildren
        | _ -> ();
        Cil.DoChildren)
      | _ -> Cil.DoChildren
  end

let vrbl_visitor loca symbol = object 
  inherit Visitor.frama_c_inplace
  method! vvdec v = 
    if (String.equal symbol v.vname) = true then 
      begin
        loca := Some v.vdecl; Cil.DoChildren
      end
    else
    Cil.DoChildren;
  end 
 

let func_visitor loca symbol = object 
  inherit Visitor.frama_c_inplace
  method! vfunc v = 
    if (String.equal symbol v.svar.vname) = true then 
      begin
        loca := Some v.svar.vdecl; Cil.DoChildren
      end
    else
    Cil.DoChildren;
  end 

let print_attrs () = 
  Cil.iterGlobals (Ast.get ()) (fun glob -> 
    List.iter (fun (attr : Cil_types.attribute) -> 
      match attr with 
      | Attr (name, params) -> 
        Options.Self.debug ~level:1 "attribute : %s\n%!" name;
        List.iter (fun (param : Cil_types.attrparam) ->
          Options.Self.debug ~level:1 "Attr param : %s\n%!" (Pretty_utils.to_string Printer.pp_attrparam param)
        ) params;
      | _ -> ()
    ) (Cil.global_attributes glob)
  )


let retrieve_location (pos : Filepath.position) =
  let loca = ref None in 
  let symbol = Utils.retrieve_symbol pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (Filepath.Normalized.to_pretty_string pos.pos_path) in  
  
  Visitor.visitFramacFile (glob_visitor loca symbol) (Ast.get ()); 
  
  match !loca with
  | Some loc -> loc
  | None -> (pos,pos) 

let find id definitionFile line ch : string =
  let pos = Utils.to_filepath_position definitionFile line ch in
  try 
    let (pos1, pos2) = retrieve_location pos in
    if pos1 = pos2 then 
      let lsp_response = Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:`Null () in
      let json_response = Lsp_types.ResponseMessage.json_of_t lsp_response in
      Json.save_string json_response
    else
      let lsp_position_1 = Lsp_types.Position.create (pos1.pos_lnum - 1) (pos1.pos_cnum - pos1.pos_bol) in
      let lsp_position_2 = Lsp_types.Position.create (pos2.pos_lnum - 1) (pos2.pos_cnum - pos2.pos_bol) in
      let lsp_range = Lsp_types.Range.create lsp_position_1 lsp_position_2 in
      let lsp_location = Lsp_types.Location.create (Filepath.normalize (Filepath.Normalized.to_pretty_string pos1.pos_path)) lsp_range in
      let json_location = Lsp_types.Location.json_of_t lsp_location in
      let lsp_response = Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:json_location () in
      let json_response = Lsp_types.ResponseMessage.json_of_t lsp_response in
      Json.save_string json_response
  with exn -> Json.save_string (Utils.make_error (Printexc.to_string (exn)) (id))
