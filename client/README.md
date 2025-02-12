# VS Code Extension for ACSL/C Language Server

This Vs Code extension implements the client part of the Language Server Protocol (LSP) for C and ACSl languages.
It also provides additional specific Non-LSP features.

## LSP Features

- Go To Definition
- Go To Declaration
- Diagnostics

## Non LSP features

- Display CIL (preprocessed code by Frama-C)
- Display Callgraph
- Display Metrics
- Prove property (specific to Frama-C/WP plugin)
- Show proof obligations (specific to Frama-C/WP plugin)

## Structure
Code adapted from https://github.com/microsoft/vscode-extension-samples/tree/main/lsp-sample

```
.
├── client // Language Client
│   ├── src
│   │   └── extension.ts // Language Client entry point
└── package.json // The extension manifest.
```

## Installation 
Simply import the vscodeacsl-0.1.0.vsix file present in this directory in VS Code
Or run this command: code --install-extension vscodeacsl-0.1.0.vsix


## nvm installation:
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.1/install.sh | bash
nvm install 20
nvm use 20

## For developpers (compilation)
npm install
vsce package

## For developpers (debug):
- Run `npm install` in this folder. This installs all necessary npm modules in the client folder
- Open VS Code on this folder.
- Press Ctrl+Shift+B to start compiling the client and server in [watch mode](https://code.visualstudio.com/docs/editor/tasks#:~:text=The%20first%20entry%20executes,the%20HelloWorld.js%20file.).
- Switch to the Run and Debug View in the Sidebar (Ctrl+Shift+D).
- Select `Launch Client` from the drop down (if it is not already).
- Press ▷ to run the launch config (F5).