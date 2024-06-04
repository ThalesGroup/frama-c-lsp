open Types
open Utils
  
(** 
    Description : 
    Module for Go To Definition feature of the Language Server Protocol.
    The main function here is find_def defined in Utils.

    Version : 1.0
    - Finds the definition of an ACSL logic function (called in the source code).
    - Only finds frama_c builtin(?) predicates (valid_read_string, valid_string, minimum, maximum, ...)

*)

let retrieve_location (pos : Filepath.position) =
  let loca = ref None in 

  let fx = retrieve_function_call pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (file_str pos.pos_path) in  
  if (Logic_lexer.is_acsl_keyword fx) = true then Printf.printf "IS ACSL KEYWORD %s\n%!" fx else 
    Printf.printf "IS NOT ACSL KEYWORD %s\n%!" fx;

  let li_visitor = object 
    inherit Visitor.frama_c_inplace
    method !vlogic_info_use li = 
      match li.l_body with 
      | LBpred pred -> 
        if (compare_retrieved_function_name pos li.l_var_info.lv_name) = 0 then
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
          if (compare_retrieved_function_name pos ei.eorig_name) = 0 then
            loca := Some loc;
          DoChildren
        | GEnumTag (ei,loc) -> 
          if (compare_retrieved_function_name pos ei.eorig_name) = 0 then
            loca := Some loc;
          DoChildren
        | GCompTagDecl (ci,loc) -> 
          if (compare_retrieved_function_name pos ci.corig_name) = 0 then
            loca := Some loc;
          DoChildren
        | GCompTag (ci,loc) -> 
          if (compare_retrieved_function_name pos ci.corig_name) = 0 then
            loca := Some loc;
          DoChildren
        | GType (ti,loc) -> 
          if (compare_retrieved_function_name pos ti.torig_name) = 0 then
            loca := Some loc;
          DoChildren
        | GVar (vi, _, loc) -> 
          if (compare_retrieved_function_name pos vi.vname) = 0 then
            loca := Some loc;
          DoChildren
        | GVarDecl (vi, loc) -> 
          if (compare_retrieved_function_name pos vi.vname) = 0 then
            loca := Some loc;
          DoChildren
        | GText _ -> 
          DoChildren
        | GFun (fd,loc) -> 
          if (compare_retrieved_function_name pos fd.svar.vname;) = 0 then
            loca := Some loc;
          DoChildren
        | GFunDecl (_,vi,loc) -> 
          if (compare_retrieved_function_name pos vi.vname) = 0 then
            loca := Some loc;
          DoChildren
        | GAsm (s,loc) -> 
          if (compare_retrieved_function_name pos s) = 0 then
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
          if (compare_retrieved_function_name pos li.l_var_info.lv_name) = 0 then 
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

  if (Logic_lexer.is_acsl_keyword fx) = true then Printf.printf "IS ACSL KEYWORD %s\n%!" fx else 
    Printf.printf "IS NOT ACSL KEYWORD %s\n%!" fx;
    
  match !loca with
  | Some loc -> loc
  | None -> (pos,pos) 


let find_def (file : Cil_types.file) (req : RequestMessage.t) : Json.json = 
    print_predicates file;
    Printf.printf "find_def called\n%!";
    let params = DefinitionParams.t_of_json (get req.params) in
    let uri = params.textDocument.uri in 
    let file = remove_file_scheme uri in
    let pos = position_t_to_filepath_position file params.position in

    let (pos1, pos2) = retrieve_location pos in

    (* todo : should send little "No definition found for x" popup *)
    if pos1 = pos2 then 
      ResponseMessage.json_of_t (ResponseMessage.create ~jsonrpc:"2.0" ~id:req.id ~result:`Null ())
    else
      ResponseMessage.json_of_t (ResponseMessage.create ~jsonrpc:"2.0" ~id:req.id ~result:
        (Location.json_of_t
          (Location.create 
            ((file_str pos1.pos_path) |> Filepath.normalize)
            (Range.create (Position.create pos1.pos_lnum (pos1.pos_cnum - pos1.pos_bol))
              (Position.create pos2.pos_lnum (pos2.pos_cnum - pos2.pos_bol))
            )
          )
        )
        ()
      )
    

    
    
