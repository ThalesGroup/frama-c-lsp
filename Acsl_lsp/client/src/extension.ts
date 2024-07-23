import * as path from 'path';
import { unescape } from 'querystring';
import { workspace, ExtensionContext, commands, extensions, window, ViewColumn, TabInputWebview, TextEditor, Uri, languages, Position, Range } from 'vscode';
import { DefinitionFeature } from 'vscode-languageclient/lib/common/definition';

import {
	DidChangeConfigurationNotification,
	LanguageClient,
	LanguageClientOptions,
	RequestType,
	ServerOptions,
	TextDocumentIdentifier,
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
			transport: { kind: TransportKind.socket, port: 8001 },
			options: { shell: true }
		},
		debug: {
			command: serverModuleDebug,
			transport: { kind: TransportKind.socket, port: 8001 },
			options: { shell: true }
		}
	};

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for c files containing acsl annotations
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

	const displayCILCommand = commands.registerCommand('vscodeacsl.displayCIL', async () => {
		try {
			const res = await client.sendRequest('vscodeacsl/displayCIL');
			let cilContent = JSON.stringify(res, null, 1);
			cilContent = escapeCharacters(cilContent);
			cilContent = cilContent.slice(1, cilContent.length - 1); // remove first and last quotes
			// .replace("\\\\\\", ''); 

			// create a new untitled document in a new tab
			const newUri = Uri.parse('untitled:CIL Representation');
			const document = await workspace.openTextDocument(newUri);
			const editor = await window.showTextDocument(document, ViewColumn.Beside, true);

			// delete previous content if any and set the content of the new document
			editor.edit(editBuilder => {
				const start = new Position(0, 0);
				const end = new Position(document.lineCount, 0);
				const fullRange = new Range(start, end);
				editBuilder.delete(fullRange);
				editBuilder.insert(editor.selection.start, cilContent);
			});
			await languages.setTextDocumentLanguage(document, 'acsl');

		} catch (err) {
			window.showErrorMessage('Failed to fetch and display CIL data: ' + err.message);
			console.error('Error fetching CIL data:', err);
		}
	});
	
	const computeCGCommand = commands.registerCommand('vscodeacsl.computeCG', async () => {
		try {
			client.sendNotification('vscodeacsl/computeCG');

		} catch (err) {
			window.showErrorMessage('Failed to compute callgraph: ' + err.message);
			console.error('Error computing callgraph:', err);
		}
	});

	context.subscriptions.push(displayCILCommand, computeCGCommand);

	// Start the client. This will also launch the server
	client.start();
}

function escapeCharacters(cCode: string): string {
	const escapedCode = cCode
		.replace(/\\\\\\/g, '')  // escape triple backslashes
		.replace(/\\\\"/g, '"')    // escape double quotes
		.replace(/(\\\\n)/g, '\n') // remove " \\n "
		.replace(/\\\\r/g, '\r')   // escape carriage return characters
		.replace(/\\\\t/g, '\t')   // escape tab characters
		.replace(/\\\\f/g, '\f')   // escape form feed characters
		.replace(/\\\\v/g, '\v');  // escape vertical tab characters
	return escapedCode;
}


export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
