'use strict';

import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient/node';

export async function activate(context: ExtensionContext) {
    // Path to the .sh script
    let serverScript = context.asAbsolutePath(path.join('..', 'server', 'run.sh'));

	console.log(serverScript)

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
            // Synchronize the setting section 'acsllsp' to the server
            configurationSection: 'acsllsp',
            // Notify the server about file changes to '.clientrc files contained in the workspace
            fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
        }
    };

    // Create the language client
    const client = new LanguageClient('Language Server Example', serverOptions, clientOptions);

    // Start the client
    client.start().then(disposable => {
        // Push the disposable to the context's subscriptions so that the
        // client can be deactivated on extension deactivation
        context.subscriptions.push();

        // Register a handler for the onDidChangeState event
        client.onDidChangeState((stateChangeEvent) => {
            if (stateChangeEvent.newState === 2) { // State 2 means 'Running'
                // Send an initialization request
                client.sendRequest('initialize', /* your initialization parameters here */).then((result) => {
                    // Handle the response
                    console.log('Initialization response:', result);
                }).catch((error) => {
                    console.error('Error sending initialization request:', error);
                });
            }
        });
		
    }).catch(error => {
        console.error(`Failed to start the language client: ${error}`);
    });
}
