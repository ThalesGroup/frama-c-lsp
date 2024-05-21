#!/bin/bash

curl --http0.9 -v -H "Content-Type:application/json" -POST http://localhost:8001 -d '{
  "jsonrpc": 2.0,
  "method": "initialize",
  "id" : 1,
  "params": {
    "workDoneToken" : null,
    "processId": null,
    "clientInfo": {
      "name": "Visual Studio Code ACSL Client",
      "version": "1.0.0"
    },
    "locale": "en",
    "initializationOptions" : null,
    "capabilities": {
	    "definition" : {
            "linkSupport" : true
        }
    },
    "trace": "off",
    "workspaceFolders": [
      {
        "uri": ".",
        "name": "Project workspace"
      }
    ]
  }
}'
