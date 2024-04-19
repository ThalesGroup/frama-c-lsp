open Cil_types
open Server.Data
(*open Logic_const
open Logic_utils
open Logic_parse_string
open Logic_to_c
*)

module J = Jany

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
        | GFunDecl (_, vf, (pos_start, _)) -> 
          Format.fprintf out "Function %s at line : %d\n" vf.vorig_name pos_start.pos_lnum ; Cil.DoChildren*)
      method !vglob_aux g = 
        match g with 
        (GCompTag (_, _)|GCompTagDecl (_, _)|GEnumTag (_, _)|GEnumTagDecl (_, _)|
        GVarDecl (_, _)|GFun (_, _)|GVar (_, _, _)|GAsm (_, _)|
        GPragma (_, _)|GText _ |GType (_, _)) -> Format.fprintf out "\n"; Cil.DoChildren
        | GFunDecl (_, vf, (pos_start, _)) -> 
          Format.fprintf out "Function %s at line : %d\n" vf.vorig_name pos_start.pos_lnum ; Cil.DoChildren (* doesn't work *)
        | GAnnot (ga, _) -> match ga with 
          | Dfun_or_pred (_, (pos_start,pos_end)) -> 
            (*Filepath.Normalized.pp_abs out pos_start.pos_path;
            Format.fprintf out "\nhere\n";*)

            let rpcversion = 2.0 in
            let id = 1 in
            (*let mth = "textDocument/definition" in*)
            Filepath.reset_symbolic_dirs ();
            let uri = Filepath.Normalized.to_pretty_string (Filepath.Normalized.of_string (Filepath.Normalized.to_pretty_string pos_start.pos_path)) in
            let start_line = pos_start.pos_lnum in 
            let end_line = pos_end.pos_lnum in 
            let start_character = (pos_start.pos_cnum - pos_start.pos_bol) in 
            let end_character = (pos_end.pos_cnum - pos_end.pos_bol) in 

            (* Parsing data into json structure *)
            let result = (`Assoc [
              ("targetUri", `String uri);
              ("targetRange", `Assoc [
                ("start", `Assoc [
                  ("line", `Int start_line);
                  ("character", `Int start_character)
                ]);
                ("end", `Assoc [
                  ("line", `Int end_line);
                  ("character", `Int end_character)
                ])
              ]);
              ("targetSelection", `Assoc [
                ("start", `Assoc [
                  ("line",`Int start_line);
                  ("character",`Int start_character)
                ]);
                ("end", `Assoc [
                  ("line",`Int end_line); 
                  ("character", `Int end_character)
                ])
              ])
            ]) in

            let response = (`Assoc [ ("jsonrpc", `Float rpcversion);
                              ("id", `Int id);
                              ("result", result)
                            ]) in 

            let json_response = Yojson.Basic.to_string response in
            Format.fprintf out "%s\n" json_response;

            (* creating json with Server.Data lib *)
            

            (*let json_response = "" in 
            let json_assoc_list = Json.assoc response in

            List.iter (fun elt -> 
              let json_response = json_response ^ (Json.string elt) in
            ) json_assoc_list; Cil.DoChildren*) (* TODO :  find a way to print json without using yojson *)

              
            Cil.DoChildren 
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
            | AVariant ({term_loc=(pos_start, _); _ }, _) -> Format.fprintf out "loop variant at line : %d\n" pos_start.pos_lnum 
            | AAssigns (_, _) -> Format.fprintf out "assigns\n"
            | AAllocation (_, _) ->Format.fprintf out "alloc\n"
            | APragma _ -> Format.fprintf out "pragma\n"
            | AExtended (_, _, {ext_loc=(_, _); _ }) ->Format.fprintf out "ext\n"
          ) annots;
        Cil.DoChildren
            *)

            (*
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
          *)
        

end

let run () =
  (*
  let ip = Unix.((gethostbyname "caml.inria.fr").h_addr_list.(0)) in
  let addr = Unix.ADDR_INET (ip, 80) in

  let sock = Unix.(socket PF_INET SOCK_STREAM 0) in
  let _ = Unix.connect sock addr in

  let in_ch = Unix.in_channel_of_descr sock in
  let out_ch = Unix.out_channel_of_descr sock in

  let _ =
    output_string out_ch
      "GET /pub/docs/manual-ocaml/index.html HTTP/1.1\r\n\
      Host: caml.inria.fr\r\n\
      User-Agent: OCaml\r\n\
      Connection: close\r\n\
      \r\n";
    flush out_ch in

  let _ =
    try
      while true do
        print_string (input_line in_ch)
      done
    with End_of_file ->
      Unix.close sock in
*)



let chan = open_out "funcs.out" in
let fmt = Format.formatter_of_out_channel chan in
Visitor.visitFramacFileSameGlobals (new print_annot fmt) (Ast.get());


close_out chan

let () = Db.Main.extend run
