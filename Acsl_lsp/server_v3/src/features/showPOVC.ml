

let get_all () : Json.json = 
  let proof_oblgs = ref [] in
  Wp.Wpo.iter_on_goals (fun po ->
    proof_oblgs := (Pretty_utils.to_string (Wp.Wpo.pp_goal) po) :: !proof_oblgs
  );
  Json.of_string (String.concat "\n----------------------------\n" !proof_oblgs)

let is_position_between (line_check, char_check) (line1, char1) (line2, char2) =
  let (line1, char1, line2, char2) =
    if (line1, char1) > (line2, char2) then
      (line2, char2, line1, char1)
    else
      (line1, char1, line2, char2)
  in

  if line1 < line_check && line_check < line2 then
    true
  else if line_check = line1 then
    char1 <= char_check
  else if line_check = line2 then
    char_check <= char2
  else if line1 = line_check && line_check = line2 then
    char1 <= char_check && char_check <= char2
  else
    false

let get_property rootPath file line ch : Json.json =
  let proof_oblgs = ref [] in
  Wp.Wpo.iter_on_goals (fun po ->
    let (start,end_) = Property.location (Wp.Wpo.get_property po) in
    let po_file = Filepath.Normalized.to_pretty_string start.pos_path in
    let line1 = start.pos_lnum in 
    let line2 = end_.pos_lnum in 
    let char1 = start.pos_cnum - start.pos_bol in 
    let char2 = end_.pos_cnum - end_.pos_bol in 
    Printf.printf "po_file %s, begin : %d:%d, end : %d:%d,\ncurr_file : cursor : %d:%d\n%!" (rootPath^"/"^po_file) line1 char1 line2 char2 (line+1) ch;
    if (
        (String.equal file (rootPath^"/"^po_file)) 
        && is_position_between (line+1,ch) (line1, char1) (line2,char2)
      ) 
    then 
      proof_oblgs := (Pretty_utils.to_string (Wp.Wpo.pp_goal) po) :: !proof_oblgs
  );
  Json.of_string (String.concat "\n----------------------------\n" !proof_oblgs)


