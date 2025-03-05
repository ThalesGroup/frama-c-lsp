 (*************************************************************************
 *                                                                        *
 *  This file is part of Frama-C/LSP plugin.                              *
 *                                                                        *
 *  You can redistribute it and/or modify it under the terms of the GNU   *
 *  General Public License as published by the Free Software Foundation,  *
 *  version 3.                                                            *
 *                                                                        *
 *  It is distributed in the hope that it will be useful,                 *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the          *
 *  GNU General Public License for more details.                          *
 *                                                                        *
 *  See the GNU General Public License version 3                          *
 *  for more details (enclosed in the file LICENSE.md                     *
 *                                                                        *
 *************************************************************************)

module StringMap = Map.Make(String)

let diag_map : Lsp_types.Diagnostic.t list StringMap.t ref = ref StringMap.empty

let warn_categories = 
  List.map (fun x -> 
    Kernel.wkey_name x
  ) (Kernel.get_all_warn_categories ())

let is_a_warn_category cat : bool = 
  let res = ref false in 
  List.iter (fun x ->
    res := !res || (String.equal cat x)
  ) warn_categories;
  !res

let publishResult id result : Json.json =
  let lsp_message = (Lsp_types.ResponseMessage.create ~jsonrpc:"2.0" ~id:(Lsp_types.Int id) ~result:(`String result) ()) in
  Lsp_types.ResponseMessage.json_of_t lsp_message


let rec remove_duplicate_diagnostics dlist =
  match dlist with
  | [] -> []
  | elem :: l when (List.mem elem l) -> (remove_duplicate_diagnostics l)
  | elem :: l -> elem :: (remove_duplicate_diagnostics l)


let rec extract_hd_elems dlist nb res =
  match dlist with
    | [] -> res
    | elem :: l when (nb > 0) -> extract_hd_elems l (nb - 1) (elem :: res)
    | _ -> res


let publishDiagnostics_notification filename dlist accumulated_list : Json.json list =
  let dlist = remove_duplicate_diagnostics dlist in
  (* let dlist = extract_hd_elems dlist 20 [] in *)
  let lsp_notification_params = Lsp_types.PublishDiagnosticsParams.create ~uri:filename ~diagnostics:dlist () in
  let json_notification_params = Lsp_types.PublishDiagnosticsParams.json_of_t lsp_notification_params in
  let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"textDocument/publishDiagnostics" ~params:json_notification_params () in
  let json_notification = Lsp_types.NotificationMessage.json_of_t lsp_notification in
  json_notification :: accumulated_list

let clear_diagnostics_no_uri =
  let lsp_notification_params = (Lsp_types.PublishDiagnosticsParams.create ~uri:("") ~diagnostics:([]) ()) in
  let lsp_notification = (Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"textDocument/publishDiagnostics" ~params:(Lsp_types.PublishDiagnosticsParams.json_of_t lsp_notification_params) ()) in
  Lsp_types.NotificationMessage.json_of_t lsp_notification

let clear_diagnostics filename = 
  let lsp_notification_params = (Lsp_types.PublishDiagnosticsParams.create ~uri:(Utils.file_str (Filepath.Normalized.of_string (Filepath.normalize filename))) ~diagnostics:([]) ()) in
  let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"textDocument/publishDiagnostics" ~params:(Lsp_types.PublishDiagnosticsParams.json_of_t lsp_notification_params) () in
  Lsp_types.NotificationMessage.json_of_t lsp_notification

let escape_double_quotes str = 
  let regex = Str.regexp {|[\"]|} in
  Str.global_replace regex {|\"|} str

let remove_file_scheme uri =
  let regex = Str.regexp {|file://|} in
  Str.global_replace regex "" uri

let remove_quotes str = 
  let regex = Str.regexp {|[\"]|} in
  Str.global_replace regex "" str

let remove_newline str = 
  let regex = Str.regexp {|.*|} in 
  ignore (Str.search_forward regex str 0);
  Str.matched_string str

let handle () : Json.json list =
  if StringMap.is_empty !diag_map then
    (
    Options.Self.debug ~level:1 "Updated Empty Diagnostics !\n%!";
    let lsp_notification_params = Lsp_types.ShowMessageParams.create ~type_: Lsp_types.MessageType.Info ~message: (Printf.sprintf "Updated diagnostics !") () in
    let json_notification_params = Lsp_types.ShowMessageParams.json_of_t lsp_notification_params in
    let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"window/showMessage" ~params:json_notification_params () in
    let json_notification = Lsp_types.NotificationMessage.json_of_t lsp_notification in
    [json_notification]
    )
  else (
    let data = StringMap.fold publishDiagnostics_notification !diag_map [] in
    data
  )

  

