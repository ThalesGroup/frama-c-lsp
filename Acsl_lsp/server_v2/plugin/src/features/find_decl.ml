
(** 
    Description : 
    Module for Go To Declaration feature of the Language Server Protocol.

    Version : 1.0
    - Finds C function, variable, struct, union and enum declarations
    - If several declarations exist for the same func/var, it will go to the first include file listed at the beginning
*)

let glob_visitor loca symbol = object 
  inherit Visitor.frama_c_inplace
    method !vglob_aux g =
      match g with 
      | GEnumTagDecl (ei,loc) -> 
        if (String.equal symbol ei.eorig_name) then
          loca := Some loc;
        Cil.DoChildren
      
      | GCompTagDecl (ci,loc) -> 
        if (String.equal symbol ci.corig_name) then
          loca := Some loc;
        Cil.DoChildren
      
      | GVarDecl (vi, loc) -> 
          if (String.equal symbol vi.vname) then
          begin
            loca := Some loc;
          end;
        Cil.DoChildren
      
      | GFunDecl (_,vi,loc) -> 
        if (String.equal symbol vi.vname) then
          begin
            loca := Some loc;
          end;
        Cil.DoChildren
    
      | _ -> Cil.DoChildren
  end


let vrbl_visitor loca symbol = object 
  inherit Visitor.frama_c_inplace
  method! vlval (lh,_) = 
    match lh with 
    | Var v -> 
      if (String.equal symbol v.vname) = true then 
        loca := Some v.vdecl; 
      Cil.DoChildren;
    | Mem _ -> 
      (* match e.enode with 
      | Const c -> 
          (match c with 
          | CStr s -> 
            if (String.equal symbol s) = true then 
              loca := Some e.eloc; Cil.DoChildren
          | _ -> Cil.DoChildren)
      | _ ->  *)
        Cil.DoChildren;
  end 



let retrieve_location (pos : Filepath.position) =
  let loca = ref None in 
  let symbol = Utils.retrieve_symbol pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (Filepath.Normalized.to_pretty_string pos.pos_path) in  
  
  Visitor.visitFramacFile (glob_visitor loca symbol) (Ast.get ()); 
  Visitor.visitFramacFile (vrbl_visitor loca symbol) (Ast.get ()); 
  
  match !loca with
  | Some loc -> loc
  | None -> (pos,pos) 

let find_decl (req : Types.RequestMessage.t) : Json.json = 
  let params = match req.params with 
    | Some p -> Types.DeclarationParams.t_of_json p
    | None -> Settings.Self.debug ~level:3 "No declaration params \n%!"; assert false
  in
    let uri = params.textDocument.uri in 
    let file = Utils.remove_file_scheme (Utils.remove_newline (Utils.remove_quotes uri)) in
    let pos = Utils.position_t_to_filepath_position file params.position in
    (* TODO : init files *)

      let (pos1, pos2) = retrieve_location pos in

      if pos1 = pos2 then 
        Types.ResponseMessage.json_of_t (Types.ResponseMessage.create ~jsonrpc:"2.0" ~id:req.id ~result:`Null ())
      else
        Types.ResponseMessage.json_of_t (Types.ResponseMessage.create ~jsonrpc:"2.0" ~id:req.id ~result:
          (Types.Location.json_of_t
            (Types.Location.create 
              (Filepath.normalize (Filepath.Normalized.to_pretty_string pos1.pos_path))
              (Types.Range.create (Types.Position.create (pos1.pos_lnum - 1) (pos1.pos_cnum - pos1.pos_bol))
                (Types.Position.create (pos2.pos_lnum - 1) (pos2.pos_cnum - pos2.pos_bol))
              )
            )
          )
          ()
        )
    

    