
(** 
    Description : 
    Module for Go To Definition feature of the Language Server Protocol.

    Version : 1.0
    - Finds frama_c terms and predicates (valid_read_string, valid_string, minimum, maximum, ...)
    - Finds user defined terms and predicates
    - Finds C function and type definitions
    - For the moment : includes Go To Declaration feature (but should not) : finds variable and function declarations

*)

let glob_visitor loca symbol = object 
  inherit Visitor.frama_c_inplace
    method !vglob_aux g =
      match g with 
      | GEnumTagDecl (ei,loc) -> 
        if (String.equal symbol ei.eorig_name) then
          loca := Some loc;
        SkipChildren
      | GEnumTag (ei,loc) -> 
        if (String.equal symbol ei.eorig_name) then
          loca := Some loc;
        SkipChildren
      | GCompTagDecl (ci,loc) -> 
        if (String.equal symbol ci.corig_name) then
          loca := Some loc;
        SkipChildren
      | GCompTag (ci,loc) -> 
        if (String.equal symbol ci.corig_name) then
          loca := Some loc;
        SkipChildren
      | GType (ti,loc) -> 
        if (String.equal symbol ti.torig_name) then
          loca := Some loc;
        SkipChildren
      | GVar (vi, _, loc) -> 
          Printf.printf "var : %s, symbol : %s \n%!" vi.vname symbol;
          if (String.equal symbol vi.vname) then
          begin
            loca := Some loc;
          end;
        SkipChildren
      | GVarDecl (vi, loc) -> 
          Printf.printf "var decl : %s, symbol : %s \n%!" vi.vname symbol;
          if (String.equal symbol vi.vname) then
          begin
            loca := Some loc;
          end;
        SkipChildren
      | GText _ -> 
        SkipChildren
      | GFun (fd,loc) -> 
        if (String.equal symbol fd.svar.vname;) then
          begin
            Printf.printf "fun : %s\n%!" fd.svar.vname;
            loca := Some loc;
          end;
        SkipChildren
      | GFunDecl (_,vi,loc) -> 
        if (String.equal symbol vi.vname) then
          begin
            Printf.printf "fun decl : %s\n%!" vi.vname;
            loca := Some loc;
          end;
        SkipChildren
      | GAsm (s,loc) -> 
        if (String.equal symbol s) then
          loca := Some loc;
        SkipChildren
      | GPragma (_,_) -> 
        SkipChildren
      | GAnnot (ga, _) -> 
        (match ga with 
        | Dinvariant (li, loc) -> 
          if (String.equal symbol li.l_var_info.lv_name) then
            begin
              Printf.printf "invariant : %s\n%!" li.l_var_info.lv_name;
              loca := Some loc;
            end;
          SkipChildren;
        | Dtype (lti, loc) -> 
          if (String.equal symbol lti.lt_name) then
            begin
              Printf.printf "logic type : %s\n%!" lti.lt_name;
              loca := Some loc;
            end;
          SkipChildren
        | Dtype_annot (li, loc) -> 
          if (String.equal symbol li.l_var_info.lv_name) then
            begin
              Printf.printf "type annot : %s\n%!" li.l_var_info.lv_name;
              loca := Some loc;
            end;
          SkipChildren
        | Dfun_or_pred (li,loc) ->
          if (String.equal symbol li.l_var_info.lv_name) then
            begin
              Printf.printf "fun or pred : %s\n%!" li.l_var_info.lv_name;
              loca := Some loc;
            end;
          SkipChildren
        | Dlemma (str,_,_,_,_,loc) ->
          Printf.printf "lemma : %s\n%!" str;
          if (String.equal symbol str) then
            begin
              loca := Some loc;
            end;
          SkipChildren
        | _ -> ();
        SkipChildren)
      (*| _ -> SkipChildren*)
  end

let vrbl_visitor loca symbol = object 
  inherit Visitor.frama_c_inplace
  method! vvdec v = 
    if (String.equal symbol v.vname) = true then 
      begin
        loca := Some v.vdecl; SkipChildren
      end
    else
    SkipChildren;
  end 

let retrieve_location (pos : Filepath.position) =
  let loca = ref None in 
  let symbol = Utils.retrieve_symbol pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (Utils.file_str pos.pos_path) in  

  Visitor.visitFramacFileSameGlobals (glob_visitor loca symbol) (Ast.get ()); 
  
  match !loca with
  | Some loc -> loc
  | None -> (pos,pos) 

let find_def (req : Types.RequestMessage.t) : Json.json = 
    let params = Types.DefinitionParams.t_of_json (Option.get req.params) in
    let uri = params.textDocument.uri in 
    let file = Utils.remove_file_scheme uri in
    let pos = Utils.position_t_to_filepath_position file params.position in

    if !States.erroring then (* todo : for the moement : we cannot have go to def feature until the file has no errors *)
      Types.ResponseMessage.json_of_t (Types.ResponseMessage.create ~jsonrpc:"2.0" ~id:req.id ~result:`Null ())
    else

    let (pos1, pos2) = retrieve_location pos in
    (* dummy position below for debugging purposes *)
    (*let pos1 : Filepath.position = {pos_path=(Filepath.Normalized.of_string "/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server/tests/test1.c"); pos_lnum=1;  pos_bol=2; pos_cnum=1} in
    let pos2 : Filepath.position = {pos_path=(Filepath.Normalized.of_string "/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server/tests/test1.c"); pos_lnum=1;  pos_bol=2; pos_cnum=1} in*)

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
    

    