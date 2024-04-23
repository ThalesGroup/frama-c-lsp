class find_def output = object
  inherit Visitor.frama_c_inplace

  method !vglob_aux g =
       match g with
    | GAnnot (Dfun_or_pred (li, (pos1, pos2)), _) -> (* Replace the compared string by what we got from reading the file at the given position in the json data *)
        let comp_result = compare li.l_var_info.lv_name "minimum" in
        if comp_result = 0 then 
        Format.fprintf output "comp result : %d,  %s, %d, %d, %s\n"
          comp_result
          li.l_var_info.lv_name
          pos1.Filepath.pos_lnum
          pos2.Filepath.pos_lnum
          (Filepath.Normalized.to_pretty_string pos1.Filepath.pos_path)
        ;Cil.DoChildren
    | _ -> Cil.DoChildren
end
let browse_ast () =
  try
    let output_channel = open_out "result.out" in
    let output = Format.formatter_of_out_channel output_channel in
    let visitor = new find_def output in
    Visitor.visitFramacFile visitor (Ast.get());
    close_out output_channel;
  with
  | Sys_error e ->
    Printf.eprintf "Error with file operation: %s\n" e
  | _ ->
    Printf.eprintf "An unknown error occurred.\n"

let () = Db.Main.extend browse_ast
