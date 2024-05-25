'use strict';

import * as path from 'path';
import * as cp from 'child_process';
import * as net from 'net';
import { workspace, ExtensionContext } from 'vscode';
import { 
    LanguageClient, 
    LanguageClientOptions, 
    ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';

// Define the client and server process variables in the module scope
let client: LanguageClient | undefined;
let serverProcess: cp.ChildProcess | undefined;
let connectionPromise: Promise<net.Socket> | undefined;

export function activate(context: ExtensionContext) {
    // Path to the .sh script
    let serverScript = context.asAbsolutePath(path.join('..', 'server', 'run.sh'));
    console.log(serverScript);

		// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	const serverOptions: ServerOptions = {
		run: { module: serverScript, transport: TransportKind.socket },
		debug: {
			module: serverScript,
			transport: TransportKind.socket,
		}
	};

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [{ scheme: 'file', language: 'acsl' }, { scheme: 'file', language: 'acsl' }],
		synchronize: {
			// Notify the server about file changes to '.clientrc files contained in the workspace
			fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
		}
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'acslLsp',
		'ACSL Language Server VS Code Extension',
		serverOptions,
		clientOptions
	);

	// Start the client. This will also launch the server
	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
