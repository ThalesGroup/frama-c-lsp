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
  if (Filename.basename p_file) = (Filename.basename target_file) then begin
    let exact_match = p_start.pos_lnum = line in
    let range_match = is_line_inside line (Property.location p) in
    if exact_match || range_match then begin
      let is_code_annot = match p with
        | Property.IPPredicate { Property.ip_kind = Property.PKAssumes _; _ } -> false
        | Property.IPPredicate _ -> true
        | Property.IPAssigns _ -> true
        | Property.IPDecrease _ -> true
        | _ -> false
      in
      let p_name = String.concat "," (Property.get_names p) in
      if p_name <> "default!" || is_code_annot then begin
        match !hit_prop with
        | None -> 
            hit_prop := Some p;
            if exact_match then raise (FoundProp p)
        | Some prev ->
            let (prev_start, _) = Property.location prev in
            let prev_dist = abs (prev_start.pos_lnum - line) in
            let curr_dist = abs (p_start.pos_lnum - line) in
            if curr_dist < prev_dist then begin
              hit_prop := Some p;
              if exact_match then raise (FoundProp p)
            end
      end
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
    let f = match Property.get_kf p with
      | Some kf -> Kernel_function.get_name kf
      | None -> "@axiomatic"
    in
    let explicit_names = Property.get_names p in
    let final_name = match p with
      | Property.IPAssigns _ -> "@assigns"
      | Property.IPDecrease _ -> "@variant"
      | Property.IPPredicate { Property.ip_kind = kind; _ } -> 
          (match explicit_names with
           | n :: _ when n <> "default!" -> n
           | _ ->
               match kind with
               | Property.PKEnsures _ -> "@ensures"
               | Property.PKRequires _ -> "@requires"
               | Property.PKTerminates -> "@terminates"
               | _ -> "@assert")  (* assert sans label → @assert *)
      | _ ->
          (match explicit_names with
           | n :: _ when n <> "default!" -> n
           | _ -> "@all")
    in
    (f, final_name)
  | None -> 
      match !hit_func with
      | Some f_name -> (f_name, "@all")
      | None -> ("@none", "@none")