(*let parse_json_request json =
  let open Yojson.Basic.Util in
  let jsonrpc = json |> member "jsonrpc" |> to_float in
  let id = json |> member "id" |> to_int in
  let mth = json |> member "method" |> to_string in
  let uri = json |> member "params" |> member "textDocument" |> member "uri" |> to_string in
  Printf.printf "json rpc = %f\n id = %d\n method = %s\n uri = %s\n" jsonrpc id mth uri;
*)

class find_def json_in = object
  inherit Visitor.frama_c_inplace
  val mutable json_out = None 
  (* We need to know if the character at given line in the json is located in the range of  *)
  method !vglob_aux g =
    match g with
    | GAnnot (Dfun_or_pred (li, (pos1, pos2)), _) -> 
        ignore json_in;
        (* Read json from input *)
        (*if pos_is_within_range (get (parse_request json_in)).params.position (pos1, pos2) then*)
        (* Replace the compared string by what we got from reading the file at the given position in the json data *)
        (*let comp_result = compare li.l_var_info.lv_name "valid" in
        if comp_result = 0 then *)
        Printf.printf "comp result : %s, [%d:%d -> %d:%d] %s\n"
          li.l_var_info.lv_name
          pos1.Filepath.pos_lnum
          (pos1.Filepath.pos_cnum - pos1.Filepath.pos_bol)
          pos2.Filepath.pos_lnum
          (pos2.Filepath.pos_cnum - pos2.Filepath.pos_bol)
          (Filepath.Normalized.to_pretty_string pos1.Filepath.pos_path)
        ;Cil.DoChildren
    | _ -> Cil.DoChildren
end


let find_def json_in = 
    Printf.printf "find_def called\n";
    Visitor.visitFramacFileSameGlobals (new find_def json_in) (Ast.get())

(*let () = Db.Main.extend (find_def "{}") *)
