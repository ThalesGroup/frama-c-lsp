(** 
    Description : 
    Module for Go To Definition feature of the Language Server Protocol.

    Version : 1.0
    - Finds frama_c builtin(?) predicates (valid_read_string, valid_string, minimum, maximum, ...)
    - Finds C function, type definitions and variable declarations

*)

(* todo : should send *)
let retrieve_location (pos : Filepath.position) =
  let loca = ref None in 

  let fx = Utils.retrieve_function_call pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (Utils.file_str pos.pos_path) in  

  let li_visitor = object 
    inherit Visitor.frama_c_inplace
    method !vlogic_info_use li = 
      match li.l_body with 
      | LBpred pred -> 
        if (Utils.compare_retrieved_function_name pos li.l_var_info.lv_name) = 0 then
          begin
            loca := Some pred.pred_loc; DoChildren
          end
        else 
        DoChildren
      | _ -> ();
      DoChildren
    end 
  in
  Visitor.visitFramacFileSameGlobals li_visitor (Ast.get ());

  let glob_visitor = object 
    inherit Visitor.frama_c_inplace
      method !vglob_aux g =
        match g with 
        | GEnumTagDecl (ei,loc) -> 
          if (Utils.compare_retrieved_function_name pos ei.eorig_name) = 0 then
            loca := Some loc;
          DoChildren
        | GEnumTag (ei,loc) -> 
          if (Utils.compare_retrieved_function_name pos ei.eorig_name) = 0 then
            loca := Some loc;
          DoChildren
        | GCompTagDecl (ci,loc) -> 
          if (Utils.compare_retrieved_function_name pos ci.corig_name) = 0 then
            loca := Some loc;
          DoChildren
        | GCompTag (ci,loc) -> 
          if (Utils.compare_retrieved_function_name pos ci.corig_name) = 0 then
            loca := Some loc;
          DoChildren
        | GType (ti,loc) -> 
          if (Utils.compare_retrieved_function_name pos ti.torig_name) = 0 then
            loca := Some loc;
          DoChildren
        | GVar (vi, _, loc) -> 
          if (Utils.compare_retrieved_function_name pos vi.vname) = 0 then
            loca := Some loc;
          DoChildren
        | GVarDecl (vi, loc) -> 
          if (Utils.compare_retrieved_function_name pos vi.vname) = 0 then
            loca := Some loc;
          DoChildren
        | GText _ -> 
          DoChildren
        | GFun (fd,loc) -> 
          if (Utils.compare_retrieved_function_name pos fd.svar.vname;) = 0 then
            loca := Some loc;
          DoChildren
        | GFunDecl (_,vi,loc) -> 
          if (Utils.compare_retrieved_function_name pos vi.vname) = 0 then
            loca := Some loc;
          DoChildren
        | GAsm (s,loc) -> 
          if (Utils.compare_retrieved_function_name pos s) = 0 then
            loca := Some loc;
          DoChildren
        | GPragma (_,_) -> 
          DoChildren
        | GAnnot (_, _) -> 
          DoChildren
      end
    in
    Visitor.visitFramacFileSameGlobals glob_visitor (Ast.get ());

  let pred_visitor = object 
    inherit Visitor.frama_c_inplace
    method! vpredicate pred = 
        match pred.pred_content with 
        | Papp (li,_,_) -> 
          if (Utils.compare_retrieved_function_name pos li.l_var_info.lv_name) = 0 then 
            begin
              let target_loc = Globals.Syntactic_search.find_in_scope li.l_var_info.lv_name Global in
              match target_loc with 
              | Some x -> Cil_printer.pp_location Format.std_formatter x.vdecl; let loc = x.vdecl in loca := Some loc ; DoChildren
              | None -> ();
              Format.pp_print_flush Format.std_formatter ();
              DoChildren
            end
          
        else DoChildren
      | _ -> DoChildren
    end 
  in
  Visitor.visitFramacFileSameGlobals pred_visitor (Ast.get ());

  let vrbl_visitor = object 
    inherit Visitor.frama_c_inplace
    method! vvdec v = 
      if (String.equal fx v.vname) = true then 
        begin
          loca := Some v.vdecl; DoChildren
        end
      else
      SkipChildren;
    end 
  in
  Visitor.visitFramacFileSameGlobals vrbl_visitor (Ast.get ());

  match !loca with
  | Some loc -> loc
  | None -> (pos,pos) 


let find_def (req : Types.RequestMessage.t) : Json.json = 
    let params = Types.DefinitionParams.t_of_json (Utils.get req.params) in
    let uri = params.textDocument.uri in 
    let file = Utils.remove_file_scheme uri in
    let pos = Utils.position_t_to_filepath_position file params.position in

    let (pos1, pos2) = retrieve_location pos in
    (*let pos1 : Filepath.position = {pos_path=(Filepath.Normalized.of_string "/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server/tests/test1.c"); pos_lnum=1;  pos_bol=2; pos_cnum=1} in
    let pos2 : Filepath.position = {pos_path=(Filepath.Normalized.of_string "/home/user/git/L1/T0304764/acsl_lsp/Acsl_lsp/server/tests/test1.c"); pos_lnum=1;  pos_bol=2; pos_cnum=1} in*)

    if pos1 = pos2 then 
      Types.ResponseMessage.json_of_t (Types.ResponseMessage.create ~jsonrpc:"2.0" ~id:req.id ~result:`Null ())
    else
      Types.ResponseMessage.json_of_t (Types.ResponseMessage.create ~jsonrpc:"2.0" ~id:req.id ~result:
        (Types.Location.json_of_t
          (Types.Location.create 
            (Utils.file_str pos1.pos_path |> Filepath.normalize)
            (Types.Range.create (Types.Position.create (pos1.pos_lnum - 1) (pos1.pos_cnum - pos1.pos_bol))
              (Types.Position.create (pos2.pos_lnum - 1) (pos2.pos_cnum - pos2.pos_bol))
            )
          )
        )
        ()
      )
    

    
    
