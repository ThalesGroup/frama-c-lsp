open Cil_types

exception FoundProp of Property.t

(* Vérifie si une ligne est comprise dans une location *)
let is_line_inside (target_line : int) (loc : location) : bool =
  let (pos_start, pos_end) = loc in
  let l_start = pos_start.pos_lnum in
  let l_end = pos_end.pos_lnum in
  target_line >= l_start && target_line <= l_end

(* Fonction pure : Analyse l'AST existant *)
let get_context (target_file : string) (line : int) : (string * string) =
  let line = line + 1 in (* Conversion VSCode (0-based) -> Frama-C (1-based) *)
  let hit_prop = ref None in
  let hit_func = ref None in

  begin
    try
      Globals.Functions.iter (fun kf ->
          let kf_name = Kernel_function.get_name kf in
          let kf_loc = Kernel_function.get_location kf in
          
          (* Est-ce qu'on est dans cette fonction ? *)
          if is_line_inside line kf_loc then hit_func := Some kf_name;

          let check_p p =
             let (p_start, _) = Property.location p in
             let p_file = Filepath.to_string p_start.pos_path in
             
             (* Est-ce le bon fichier et la bonne ligne ? *)
             if (Filename.basename p_file) = (Filename.basename target_file) then
                if is_line_inside line (Property.location p) then begin
                   let p_name = String.concat "," (Property.get_names p) in
                   if p_name <> "default!" then begin
                      hit_prop := Some p;
                      raise (FoundProp p)
                   end
                end
          in

          (* On scanne les spécifications de fonction *)
          let spec = Annotations.funspec kf in
          let contract_props = Property.ip_of_spec kf Kglobal ~active:[] spec in
          
          (* On scanne les annotations dans le code (assert, invariants...) *)
          let body_props = ref [] in
          if Kernel_function.is_definition kf then begin
             let def = Kernel_function.get_definition kf in
             List.iter (fun stmt ->
                let annots = Annotations.code_annot stmt in
                List.iter (fun annot -> 
                   body_props := !body_props @ (Property.ip_of_code_annot kf stmt annot)
                ) annots
             ) def.sallstmts
          end;
          
          List.iter check_p (contract_props @ !body_props)
      );
    with FoundProp _ -> ()
  end;

  (* On formate le résultat pour le renvoyer *)
  match !hit_prop with
  | Some p -> 
      let f = match Property.get_kf p with Some kf -> Kernel_function.get_name kf | None -> "@axiomatic" in
      let explicit_names = Property.get_names p in
      let final_name = match p with
        | Property.IPAssigns _ -> "@assigns"
        | Property.IPDecrease _ -> "@variant"
        | Property.IPPredicate { Property.ip_kind = kind; _ } -> 
             (match explicit_names with n::_ -> n | [] -> 
                match kind with Property.PKEnsures _ -> "@ensures" | Property.PKRequires _ -> "@requires" | _ -> "@all")
        | _ -> (match explicit_names with n::_ -> n | [] -> "@all")
      in
      (f, final_name)
  | None -> 
      match !hit_func with
      | Some f_name -> (f_name, "@all")
      | None -> ("@none", "@none")