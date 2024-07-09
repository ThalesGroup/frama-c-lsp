
(** 
    Description : 
    Module for Go To Definition feature of the Language Server Protocol.

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
          (* Printf.printf "var : %s, symbol : %s \n%!" vi.vname symbol; *)
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
            (* Printf.printf "fun : %s\n%!" fd.svar.vname; *)
            loca := Some loc;
          end;
        Cil.DoChildren
      | GAsm (s,loc) -> 
        if (String.equal symbol s) then
          loca := Some loc;
        Cil.DoChildren
      | GPragma (_,_) -> 
        Cil.DoChildren
      | GAnnot (ga, _) -> 
        (match ga with 
        | Dinvariant (li, loc) -> 
          if (String.equal symbol li.l_var_info.lv_name) then
            begin
              (* Printf.printf "invariant : %s\n%!" li.l_var_info.lv_name; *)
              loca := Some loc;
            end;
          Cil.DoChildren;
        | Dtype (lti, loc) -> 
          if (String.equal symbol lti.lt_name) then
            begin
              (* Printf.printf "logic type : %s\n%!" lti.lt_name; *)
              loca := Some loc;
            end;
          Cil.DoChildren
        | Dtype_annot (li, loc) -> 
          if (String.equal symbol li.l_var_info.lv_name) then
            begin
              (* Printf.printf "type annot : %s\n%!" li.l_var_info.lv_name; *)
              loca := Some loc;
            end;
          Cil.DoChildren
        | Dfun_or_pred (li,loc) ->
          if (String.equal symbol li.l_var_info.lv_name) then
            begin
              (* Printf.printf "fun or pred : %s\n%!" li.l_var_info.lv_name; *)
              loca := Some loc;
            end;
          Cil.DoChildren
        | Dlemma (str,_,_,_,_,loc) ->
          (* Printf.printf "lemma : %s\n%!" str; *)
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

let retrieve_location (pos : Filepath.position) =
  let loca = ref None in 
  let symbol = Utils.retrieve_symbol pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (Utils.file_str pos.pos_path) in  
  
  Visitor.visitFramacFile (glob_visitor loca symbol) (Ast.get ()); 
  
  match !loca with
  | Some loc -> loc
  | None -> (pos,pos) 

let find_def (req : Types.RequestMessage.t) sock : Json.json = 
    let params = Types.DefinitionParams.t_of_json (Option.get req.params) in
    let uri = params.textDocument.uri in 
    let file = Utils.remove_file_scheme (Utils.remove_newline (Utils.remove_quotes uri)) in
    let pos = Utils.position_t_to_filepath_position file params.position in
    Load.init_files sock;

    if !States.erroring then (* todo : for the moment : we cannot have go to def feature while the file has errors *)
      Types.ResponseMessage.json_of_t (Types.ResponseMessage.create ~jsonrpc:"2.0" ~id:req.id ~result:`Null ())
    else
      let (pos1, pos2) = retrieve_location pos in

      if pos1 = pos2 then 
        Types.ResponseMessage.json_of_t (Types.ResponseMessage.create ~jsonrpc:"2.0" ~id:req.id ~result:`Null ())
      else
        Types.ResponseMessage.json_of_t (Types.ResponseMessage.create ~jsonrpc:"2.0" ~id:req.id ~result:
          (Types.Location.json_of_t
            (Types.Location.create 
              (Filepath.normalize (Utils.file_str pos1.pos_path))
              (Types.Range.create (Types.Position.create (pos1.pos_lnum - 1) (pos1.pos_cnum - pos1.pos_bol))
                (Types.Position.create (pos2.pos_lnum - 1) (pos2.pos_cnum - pos2.pos_bol))
              )
            )
          )
          ()
        )
    

    