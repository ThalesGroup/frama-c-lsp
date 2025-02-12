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

let clear_diagnostics filename =
  let normalized_filename = Utils.file_str (Filepath.Normalized.of_string (Filepath.normalize filename)) in
  
  let lsp_notification_params = Lsp_types.PublishDiagnosticsParams.create ~uri:(normalized_filename) ~diagnostics:([]) () in
  let json_notification_params = Lsp_types.PublishDiagnosticsParams.json_of_t (lsp_notification_params) in
  let lsp_notification = Lsp_types.NotificationMessage.create ~jsonrpc:"2.0" ~method_:"textDocument/publishDiagnostics" ~params:(json_notification_params) () in
  Lsp_types.NotificationMessage.json_of_t lsp_notification

let handle filename : Json.json = 
  clear_diagnostics filename