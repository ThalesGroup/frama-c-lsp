open Cil_types
(*open Logic_const
open Logic_utils
open Logic_parse_string
open Logic_to_c
*)

(* if the value has type "option" *)
let extract_x x =
  match x with
  | Some x -> x
  | None -> failwith "extract_x: expected Some"

class print_annot out = object 
    inherit Visitor.frama_c_inplace

    (*method !vglob_aux g = 
        match g with 
        (GCompTag (_, _)|GCompTagDecl (_, _)|GEnumTag (_, _)|GEnumTagDecl (_, _)|
        GVarDecl (_, _)|GFun (_, _)|GVar (_, _, _)|GAnnot (_, _)|GAsm (_, _)|
        GPragma (_, _)|GText _ |GType (_, _)) -> Format.fprintf out "\n"; Cil.DoChildren
        | GFunDecl (_, vf, (pos1, _)) -> 
          Format.fprintf out "Function %s at line : %d\n" vf.vorig_name pos1.pos_lnum ; Cil.DoChildren*)
      method !vglob_aux g = 
        match g with 
        (GCompTag (_, _)|GCompTagDecl (_, _)|GEnumTag (_, _)|GEnumTagDecl (_, _)|
        GVarDecl (_, _)|GFun (_, _)|GVar (_, _, _)|GAsm (_, _)|
        GPragma (_, _)|GText _ |GType (_, _)) -> Format.fprintf out "\n"; Cil.DoChildren
        | GFunDecl (_, vf, (pos1, _)) -> 
          Format.fprintf out "Function %s at line : %d\n" vf.vorig_name pos1.pos_lnum ; Cil.DoChildren (* MARCHE PAAAAAAAAAAAAAS*)
        | GAnnot (ga, _) -> match ga with 
          | Dfun_or_pred (li, (pos1,_)) -> Format.fprintf out "var info : %s, path : %s, line : %d\n" li.l_var_info.lv_name (Filepath.normalize (Filepath.basename pos1.pos_path)) pos1.pos_lnum; Cil.DoChildren 
          | _ -> Format.fprintf out "other\n"; Cil.DoChildren
          
      (*
      method !vstmt_aux s = 
        let annots = Annotations.code_annot s in 
        let anleng = List.length annots in
        if anleng <= 0 then Format.fprintf out "no annotations\n"
        else
          List.iter (fun annot -> 
            match annot.annot_content with 
            | AAssert (_, _) -> Format.fprintf out "assert\n"
            | AStmtSpec (_, _) -> Format.fprintf out "stmtspec\n"
            | AInvariant (_, _, _) -> Format.fprintf out "inv\n"
            | AVariant ({term_loc=(pos1, _); _ }, _) -> Format.fprintf out "loop variant at line : %d\n" pos1.pos_lnum 
            | AAssigns (_, _) -> Format.fprintf out "assigns\n"
            | AAllocation (_, _) ->Format.fprintf out "alloc\n"
            | APragma _ -> Format.fprintf out "pragma\n"
            | AExtended (_, _, {ext_loc=(_, _); _ }) ->Format.fprintf out "ext\n"
          ) annots;
        Cil.DoChildren
            *)

        method !vstmt_aux s = 
          let annots = Annotations.code_annot s in 
          let anleng = List.length annots in
          if anleng <= 0 then Format.fprintf out "no annotations\n"
          else
            List.iter (fun annot -> 
              match annot.annot_content with 
              | AAssert (_, tp) -> List.iter (
                  fun name -> Format.fprintf out "predicate %s\n" name;
              ) tp.tp_statement.pred_name;
              | _ -> Format.fprintf out "other\n"
            
            ) annots;
          Cil.DoChildren

end

let run () =
  
let chan = open_out "funcs.out" in
let fmt = Format.formatter_of_out_channel chan in
Visitor.visitFramacFile (new print_annot fmt) (Ast.get());


close_out chan

let () = Db.Main.extend run
