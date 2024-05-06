let pos_is_within_range pos (pos1, pos2 : (Filepath.position * Filepath.position)) = 
  let curr_pos = pos.Filepath.pos_lnum + (pos.Filepath.pos_cnum - pos.Filepath.pos_bol) in 
  let min = pos1.Filepath.pos_lnum + (pos1.Filepath.pos_cnum - pos1.Filepath.pos_bol) in 
  let max = pos2.Filepath.pos_lnum + (pos2.Filepath.pos_cnum - pos2.Filepath.pos_bol) in 
  curr_pos >= min && curr_pos <= max

let get = function Some v -> v | None -> invalid_arg "option is None";

(*
(* transform json position to filepath position *)
let to_filepath_position () = 
*)