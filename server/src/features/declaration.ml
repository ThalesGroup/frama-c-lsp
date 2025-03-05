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
    - Finds C function, variable, struct, union and enum declarations
    - If several declarations exist for the same func/var, it will go to the first include file listed at the beginning
*)

let glob_visitor symbol declarations = object 
  inherit Visitor.frama_c_inplace
    method !vglob_aux g =
      match g with 
      | GEnumTagDecl (ei,loc) -> 
        if (String.equal symbol ei.eorig_name) then
          declarations := loc :: !declarations;
        Cil.DoChildren
      
      | GCompTagDecl (ci,loc) -> 
        if (String.equal symbol ci.corig_name) then
          declarations := loc :: !declarations;
        Cil.DoChildren
      
      | GVarDecl (vi, loc) -> 
        if (String.equal symbol vi.vname) then 
        begin
          declarations := loc :: !declarations;
        end; 
        Cil.DoChildren
      
      | GFunDecl (_,vi,loc) -> 
        if (String.equal symbol vi.vname) then 
          begin
            declarations := loc :: !declarations;
          end;
        Cil.DoChildren

      | _ -> Cil.DoChildren
  end


let vrbl_visitor symbol declarations = object 
  inherit Visitor.frama_c_inplace
  method! vlval (lh,_) = 
    match lh with 
    | Var v -> 
      if (String.equal symbol v.vname) && (not v.vformal) then 
        declarations := v.vdecl :: !declarations;
      Cil.DoChildren;
    | Mem _ -> Cil.DoChildren;
  end 

  

let decl_visitor symbol declarations = object 
  inherit Visitor.frama_c_inplace
  method! vvdec vi = 
    if (String.equal symbol vi.vname) then 
      declarations := vi.vdecl :: !declarations; Cil.DoChildren
  end

let iter_decl () = 
  Globals.Vars.iter_in_file_order (fun v _ -> 
      Options.Self.debug ~level:1 "Iter file order : %s\n%!" v.vname
    )   

let print_fxs file = 
  Options.Self.debug ~level:1 "Printing fxs ...\n%!";
  let fxs = Globals.FileIndex.get_functions ~declarations:true file in
  List.iter (fun (f : Cil_types.kernel_function) ->
    match f.fundec with 
    | Definition (fd, _) -> 
      Options.Self.debug ~level:1 "function definition name : %s\n%!" fd.svar.vname
    | Declaration (_,vi,_,_) -> 
      Options.Self.debug ~level:1 "function declaration name : %s\n%!" vi.vname
  ) fxs

let retrieve_location (pos : Filepath.position) =
  let declarations = ref [] in
  let symbol = Utils.retrieve_symbol pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (Filepath.Normalized.to_pretty_string pos.pos_path) in  

  Visitor.visitFramacFile (glob_visitor symbol declarations) (Ast.get ()); 
  Visitor.visitFramacFile (vrbl_visitor symbol declarations) (Ast.get ()); 

  !declarations

let retrieve_location2 (pos : Filepath.position) (cil_files : Cil_types.file list) =
  let declarations = ref [] in
  let symbol = Utils.retrieve_symbol pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (Filepath.Normalized.to_pretty_string pos.pos_path) in  
  let counter = ref 0 in
  List.iter (fun f ->
    counter := !counter + 1;
    Options.Self.debug ~level:1 "retrieve_location setting project : %s\n%!" ("file"^(Stdlib.string_of_int !counter));
    Project.set_current (Project.from_unique_name ("file"^(Stdlib.string_of_int !counter)));
    Visitor.visitFramacFile (glob_visitor symbol declarations) (f); 
    (* Options.Self.debug ~level:1 "Ast of %s : %s\n%!" ("file"^(Stdlib.string_of_int !counter)) (Pretty_utils.to_string Printer.pp_file f); *)

  ) cil_files;
  !declarations

let retrieve_location3 (pos : Filepath.position) new_globals =
  let declarations = ref [] in
  let symbol = Utils.retrieve_symbol pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (Filepath.Normalized.to_pretty_string pos.pos_path) in  

    List.iter(fun (v : Cil_types.varinfo) ->
      if (String.equal symbol v.vname) then 
        (
          Options.Self.debug ~level:1 "Symbol : %s, vname : %s\n%!" symbol v.vname;
          declarations := v.vdecl :: !declarations;
          match v.vdefined with 
          | true -> Options.Self.debug ~level:1 "New defined global : %s with location : %s\n%!" (Pretty_utils.to_string Printer.pp_varinfo v) (Pretty_utils.to_string Printer.pp_location v.vdecl)
          | false -> Options.Self.debug ~level:1 "New global : %s with location : %s\n%!" (Pretty_utils.to_string Printer.pp_varinfo v) (Pretty_utils.to_string Printer.pp_location v.vdecl)
        )
    ) !new_globals;

  !declarations

let create_json_location (loc : Cil_types.location) = 
  let lsp_position_1 = Lsp_types.Position.create ((Stdlib.fst loc).pos_lnum - 1) ((Stdlib.fst loc).pos_cnum - (Stdlib.fst loc).pos_bol) in
  let lsp_position_2 = Lsp_types.Position.create ((Stdlib.snd loc).pos_lnum - 1) ((Stdlib.snd loc).pos_cnum - (Stdlib.snd loc).pos_bol) in
  let lsp_range = Lsp_types.Range.create lsp_position_1 lsp_position_2 in
  let lsp_location = Lsp_types.Location.create (Filepath.normalize (Filepath.Normalized.to_pretty_string (Stdlib.fst loc).pos_path)) lsp_range in
  let json_location = Lsp_types.Location.json_of_t lsp_location in
  json_location

let create_json_locations locations =
  let res = List.map create_json_location locations in
  Json.of_list res
  

let find id declarationFile line ch : string = 
  let pos = Utils.to_filepath_position declarationFile line ch in
  try
    let locations = retrieve_location pos in
    if (List.length locations) = 0 then
      let lsp_response = Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:`Null () in
      let json_response = Lsp_types.ResponseMessage.json_of_t lsp_response in
      Json.save_string json_response
    else
      let lsp_response = Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:(create_json_locations locations) () in
      let json_response = Lsp_types.ResponseMessage.json_of_t lsp_response in
      Json.save_string json_response
  with exn ->
    let json_response = Utils.make_error (Printexc.to_string (exn)) (id) in
    Json.save_string json_response

    