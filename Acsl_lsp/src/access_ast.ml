open Cil_types
(*open Logic_const
open Logic_utils
open Logic_parse_string
open Logic_to_c
*)

class print_annot out = object 
    inherit Visitor.frama_c_inplace

    (*
  method !vstmt_aux s =
    let annots = Annotations.code_annot s in
    let anleng = List.length annots in
    if anleng <= 0 then Format.fprintf out "Empty List\n"
    else
      List.iter (fun annot -> if Logic_utils.is_assigns annot then Format.fprintf out "Assigns %d\n" annot.annot_id) annots;
 
      let x = Visitor_behavior.Get.varinfo self#behavior in
      Format.fprintf out "varinfo %s\n" x.vname
      (*List.iter (fun annot -> 
        if Logic_utils.is_same_code_annotation annot (Logic_parse_string.code_annot Visitor_behavior.Get.kernel_function) then
          Format.fprintf out "is same code annot%d\n" annot.annot_id
        ) annots ;*)
      Cil.DoChildren
      *)

      (*
      method !vstmt_aux s = 
        List.iter (fun attr -> 
          match attr with 
          | Attr (name, params) -> 
            Format.fprintf out "attr name = %s\n" name;
            List.iter (fun param -> 
              match param with 
              | AStr (str_const) -> Format.fprintf out "String constant %s\n" str_const
              | AInt (int_const) -> Format.fprintf out "Integer constant %d\n" (Integer.to_int_exn int_const)
              | _ -> Format.fprintf out "unsupported param"
          ) params;
          | _ -> Format.fprintf out "other"
        ) s.sattr;
      Cil.DoChildren   
      *)

      method !vglob_aux g = 
        match g with 
        (GCompTag (_, _)|GCompTagDecl (_, _)|GEnumTag (_, _)|GEnumTagDecl (_, _)|
        GVarDecl (_, _)|GFunDecl (_, _, _)|GVar (_, _, _)|GFun (_, _)|GAsm (_, _)|
        GPragma (_, _)|GText _) -> Cil.DoChildren
        | GType (_, _) -> Cil.DoChildren
        | GAnnot (ga, _) -> 
          match ga with 
              | Dvolatile (_, _, _, _, (_, _)) -> Format.fprintf out "\n"; Cil.DoChildren
              | _ -> Cil.DoChildren
          
      method !vstmt_aux s = 
        let annots = Annotations.code_annot s in 
        let anleng = List.length annots in
        if anleng <= 0 then Format.fprintf out "no annotations\n"
        else
          List.iter (fun annot -> 
            match annot.annot_content with 
            |AAssert (_, _) -> Format.fprintf out "assert\n"
            |AStmtSpec (_, _) -> Format.fprintf out "stmtspec\n"
            |AInvariant (_, _, _) -> Format.fprintf out "inv\n"
            |AVariant ({term_loc=(pos1, _); _ }, _) -> Format.fprintf out "loop variant at line : %d\n" pos1.pos_lnum 
            |AAssigns (_, _) -> Format.fprintf out "assigns\n"
            |AAllocation (_, _) ->Format.fprintf out "alloc\n"
            |APragma _ -> Format.fprintf out "pragma\n"
            |AExtended (_, _, {ext_loc=(_, _); _ }) ->Format.fprintf out "ext\n"
          ) annots;
        Cil.DoChildren


end

let run () =
  
let chan = open_out "annots.out" in
let fmt = Format.formatter_of_out_channel chan in
Visitor.visitFramacFileFunctions (new print_annot fmt) (Ast.get());

close_out chan

let () = Db.Main.extend run
