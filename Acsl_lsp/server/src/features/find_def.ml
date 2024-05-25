open Types
open Utils
open Printer_tag
  
(** 
    Description : 
    Module for Go To Definition feature of the Language Server Protocol.
    The main function here is retrieve_acsl_annotations defined in Utils.

    Version : 1.0
    - Finds the definition of an ACSL logic function (called in the source code).
    - Only finds frama_c builtin predicates (valid_read_string, valid_string, minimum, maximum, ...)

*)
let process_global (params : DefinitionParams.t) (g : Cil_types.global) =
  ignore g;
  let uri = params.textDocument.uri in 
  let curr_pos = position_t_to_filepath_position uri params.position in
  let target_loc = Printer_tag.loc_to_localizable ?precise_col:(Some true) curr_pos in
  (*let vinfo =  Printer_tag.varinfo_of_localizable target_loc in *)
  match target_loc with 
  | Some loc -> Printf.printf "%s\n%!" (label loc)
  | None -> Printf.printf "No decl\n%!"

let find_def (file : Cil_types.file) (req : RequestMessage.t) : Json.json = 
    print_predicates file;
    Printf.printf "find_def called\n%!";

    let (params : DefinitionParams.t) = DefinitionParams.t_of_json (get req.params) in
    let file = params.textDocument.uri in
    let pos = position_t_to_filepath_position file params.position in
    let (pos1,pos2) = retrieve_acsl_annotations pos in
    let start = Position.create pos1.pos_lnum (pos1.pos_cnum - pos1.pos_bol) in
    let end_ = Position.create pos2.pos_lnum (pos2.pos_cnum - pos2.pos_bol) in
    let range = Range.create start end_ in
    let result_loc = Location.create (file_str pos1.pos_path) range in
    ResponseMessage.json_of_t (ResponseMessage.create ~jsonrpc:req.jsonrpc ~id:req.id ?result:(Some (Location.json_of_t result_loc)) ());
    

    
    
