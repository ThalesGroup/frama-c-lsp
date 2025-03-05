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

let initialize : Json.json = 

      let result = {|{
        "jsonrpc": "2.0",
        "id": 0,
        "result": {
          "capabilities": {
            "textDocumentSync": {
              "openClose": false,
              "change": 0,
              "save": { "includeText": false }
            },
            "definitionProvider": true,
            "declarationProvider": true,

            "diagnosticProvider": {
              "interFileDependencies": false,
              "workspaceDiagnostics": true
            },
            "workspace": {
              "workspaceFolders": {
                "supported": true,
                "changeNotifications": true
              }
            },
            "experimental": null
          },
          "serverInfo": {
            "name": "ACSL LSP",
            "version": "0.0.1"
          }
        }
      }|}
      in
      Json.load_string result
