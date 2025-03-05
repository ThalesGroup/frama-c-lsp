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

 module Self = Plugin.Register 
 (struct
   let name = "LSP"
   let shortname = "lsp"
   let help = "Activates LSP support for ACSL/C"
 end)
 
 let () = Parameter_customize.do_not_projectify ()
 module Enabled = Self.False
 (struct
   let option_name = "-lsp"
   let help = "when on (off by default), activates lsp support for ACSL/C"
 end)
 
 
 let () = Parameter_customize.do_not_projectify ()
 module Did_save = Self.False (* filename *)
 (struct
   let option_name = "-lsp-did-save"
   let help = "Publish diagnostics each time a file is saved"
 end)
 
 
 let () = Parameter_customize.do_not_projectify ()
 module Handler_opt = Self.String
 (struct
   let option_name = "-lsp-handler"
   let help = "activates handler mode"
   let arg_name = "server_port:wrapper_port"
   let default = ""
 end)
 

 let () = Parameter_customize.do_not_projectify ()
 module Wrapper_opt = Self.Int
 (struct
   let option_name = "-lsp-wrapper"
   let help = "activates wrapper mode"
   let arg_name = "server_port"
   let default = 0
 end)


 module Find_def = Self.String 
 (struct
   let option_name = "-lsp-definition"
   let help = "definition request"
   let arg_name = "file:line:character"
   let default = ""
 end)
 
 module Find_decl = Self.String 
 (struct
   let option_name = "-lsp-declaration"
   let help = "declaration request"
   let arg_name = "file:line:character"
   let default = ""
 end)
 
 
 module Id = Self.Int
 (struct
   let option_name = "-lsp-id"
   let help = "id of the request"
   let arg_name = "id"
   let default = 0
 end)
 
 module Root_path = Self.String
 (struct
   let option_name = "-lsp-root-path"
   let help = "path to the workspace folder"
   let arg_name = "path"
   let default = ""
 end)
 
 module Show_POVC = Self.String
 (struct
   let option_name = "-lsp-show-povc"
   let help = "send back the povc of the located property"
   let arg_name = "file:line:character"
   let default = ""
 end)
 
 module Show_PO = Self.String
 (struct
   let option_name = "-lsp-show-po"
   let help = "send back the po of the located property"
   let arg_name = "goal_id"
   let default = ""
 end)
 
 module Prove = Self.String
 (struct
   let option_name = "-lsp-prove"
   let help = "send back the proof status"
   let arg_name = "fct and prop"
   let default = ""
 end) 
