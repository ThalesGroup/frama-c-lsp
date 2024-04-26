let parse_json_request json out =
  let open Yojson.Basic.Util in
  let jsonrpc = json |> member "jsonrpc" |> to_float in
  let id = json |> member "id" |> to_int in
  let mth = json |> member "method" |> to_string in
  let uri = json |> member "params" |> member "textDocument" |> member "uri" |> to_string in
  Format.fprintf out "json rpc = %f\n id = %d\n method = %s\n uri = %s\n" jsonrpc id mth uri;

class find_def input output = object
  inherit Visitor.frama_c_inplace

  (* We need to know if the character at given line in the json is located in the range of  *)
  method !vglob_aux g =
       match g with
    | GAnnot (Dfun_or_pred (li, (pos1, pos2)), _) -> 
        
        (* Read json from input file *)
        let json_obj = Json.load_channel input in

        let () = parse_json_request json_obj output in

        flush stdout;
        close_in input;

        (* Replace the compared string by what we got from reading the file at the given position in the json data *)
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


let find_def () =
  try
    let input = open_in "json.out" in
    let output_channel = open_out "result.out" in
    let output = Format.formatter_of_out_channel output_channel in
    let visitor = new find_def input output in
    Visitor.visitFramacFile visitor (Ast.get());
    close_out output_channel;
    close_in input;
  with
  | Sys_error e ->
    Printf.eprintf "Error with file operation: %s\n" e
  | _ ->
    Printf.eprintf "An unknown error occurred.\n"

(*let () = Db.Main.extend find_def*)
