'use strict';

import * as path from 'path';
import * as cp from 'child_process';
import { workspace, ExtensionContext } from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient/node';

// Define the client and server process variables in the module scope
let client: LanguageClient | undefined;
let serverProcess: cp.ChildProcess | undefined;

export function activate(context: ExtensionContext) {
    // Path to the .sh script
    let serverScript = context.asAbsolutePath(path.join('..', 'server', 'run.sh'));

    console.log(serverScript);

    // Start the server process
    serverProcess = cp.spawn(serverScript, [], { stdio: 'pipe' });

    // Server options
    let serverOptions: ServerOptions = {
        run: { command: serverScript, transport: TransportKind.stdio },
        debug: { command: serverScript, transport: TransportKind.stdio }
    };

    // Options to control the language client
    let clientOptions: LanguageClientOptions = {
        // Register the server for C documents
        documentSelector: [{ scheme: 'file', language: 'c' }],
        synchronize: {
            // Notify the server about file changes to '.clientrc files contained in the workspace
            fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
        }
    };

    // Create the language client
    client = new LanguageClient('ACSL Language Server VS Code Extension', serverOptions, clientOptions);

    // Start the client and add it to the context's subscriptions
    client.start().then(disposable => {
        // Push the disposable to the context's subscriptions so that the
        // client can be deactivated on extension deactivation
        context.subscriptions.push();
    }).catch(error => {
        console.error(`Failed to start the language client: ${error}`);
    });

    // Add a dispose handler to clean up the server process on extension deactivation
    context.subscriptions.push({
        dispose: () => {
            console.log("kill 1")
            console.log(serverProcess.pid)
            if (serverProcess) {
                serverProcess.kill('SIGTERM');
                serverProcess = undefined;
            }
        }
    });
}

export function deactivate(): Thenable<void> | undefined {
    if (client) {
        return client.stop().then(() => {
            if (serverProcess) {
                console.log("kill 2")
                serverProcess.kill('SIGTERM');
                serverProcess = undefined;
            }
        });
    } else if (serverProcess) {
        console.log("kill 3")
        serverProcess.kill('SIGTERM');
        serverProcess = undefined;
    }
    return undefined;
}
