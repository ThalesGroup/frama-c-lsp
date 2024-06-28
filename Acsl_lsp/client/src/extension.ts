import * as path from 'path';
import { workspace, ExtensionContext, commands, extensions, window } from 'vscode';
import { DefinitionFeature } from 'vscode-languageclient/lib/common/definition';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind,
	VersionedTextDocumentIdentifier
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	// The server is implemented in OCaml
	const extension = context.extension.packageJSON.publisher 
		+ "." 
		+ context.extension.packageJSON.name
		+ "-"
		+ context.extension.packageJSON.version;

	const serverModuleRun = context.asAbsolutePath(
		path.join('run.sh')
	);

	const serverModuleDebug = context.asAbsolutePath(
		path.join('run.sh')
	);

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	const serverOptions: ServerOptions = {
		run: { 
			command: serverModuleRun, 
			transport: {kind: TransportKind.socket, port: 8001},
			options: {shell: true}
		},
		debug: {
			command: serverModuleDebug,
			transport: {kind: TransportKind.socket, port: 8001},
			options: {shell: true}
		}
	};

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [{ scheme: 'file', language: 'acsl' }
		],
		synchronize: {
			// Notify the server about file changes to '.clientrc files contained in the workspace
			fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
		}
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'vscodeacsl',
		'ACSL Language Server',
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
