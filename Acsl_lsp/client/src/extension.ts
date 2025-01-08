import * as path from 'path';
import { workspace, ExtensionContext, commands, extensions, window, ViewColumn, TabInputWebview, TextEditor, Uri, languages, Position, Range, env } from 'vscode';
import * as vscode from 'vscode';
import * as fs from 'fs';
import {LanguageClient,	LanguageClientOptions, ServerOptions, TransportKind} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	// The server is implemented in OCaml
	const serverModuleRun = context.asAbsolutePath(path.join('run.sh'));
	const serverModuleDebug = context.asAbsolutePath(path.join('run.sh'));

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	const serverOptions: ServerOptions = {
		run: {command: serverModuleRun,	transport: { kind: TransportKind.socket, port: 8005 }, options: { shell: true }},
		debug: {command: serverModuleDebug,	transport: { kind: TransportKind.socket, port: 8005 }, options: { shell: true }}
	};

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for c files containing acsl annotations
		documentSelector: [{ scheme: 'file', language: 'acsl' }],
		// Notify the server about file changes to '.clientrc files contained in the workspace
		synchronize: {fileEvents: workspace.createFileSystemWatcher('**/.clientrc')}
	};

	// Create the language client and start the client.
	client = new LanguageClient('vscodeacsl', 'ACSL Language Server', serverOptions, clientOptions);

	const smokeTests = commands.registerCommand('smokeTests', async () => {
		try {
			await client.sendNotification('smokeTests', window.activeTextEditor.document.fileName);
			window.showInformationMessage('Smoke tests computed');
		} catch (err) {
			window.showErrorMessage('Failed to run smoke tests: ' + err.message);
			console.error('Error computing smoke tests:', err);
		}
	});

	const displayCIL = commands.registerCommand('displayCIL', async () => {
		try {
			await client.sendNotification('displayCIL', window.activeTextEditor.document.fileName);
			window.showInformationMessage('CIL file generated');
		} catch (err) {
			window.showErrorMessage('Failed to compute displayCIL: ' + err.message);
			console.error('Error computing displayCIL:', err);
		}
	});

	const displayCIL_noannot = commands.registerCommand('displayCIL_noannot', async () => {
		try {
			await client.sendNotification('displayCIL_noannot', window.activeTextEditor.document.fileName);
			window.showInformationMessage('CIL file generated');
		} catch (err) {
			window.showErrorMessage('Failed to compute displayCIL_noannot: ' + err.message);
			console.error('Error computing displayCIL_noannot:', err);
		}
	});

	const computeCG = commands.registerCommand('computeCG', async () => {
		try {
			await client.sendNotification('computeCG', window.activeTextEditor.document.fileName);
			window.showInformationMessage('CallGraph generated');
		} catch (err) {
			window.showErrorMessage('Failed to compute callgraph: ' + err.message);
			console.error('Error computing callgraph:', err);
		}
	});

	const showPOVC = commands.registerCommand('showPOVC', async () => {
		try {
			const res = await client.sendRequest('showPOVC', [window.activeTextEditor.document.fileName, window.activeTextEditor.selection.active]);
			const wpResult = JSON.parse(JSON.stringify(res, null, 1));

			// create a new untitled document in a new tab
			const newUri = Uri.parse('untitled:Proof Obligation');
			const document = await workspace.openTextDocument(newUri);
			await languages.setTextDocumentLanguage(document, 'plaintext');
			const editor = await window.showTextDocument(document, ViewColumn.Beside, true);

			// delete previous content if any and set the content of the new document
			editor.edit(editBuilder => {
				const start = new Position(0, 0);
				const end = new Position(document.lineCount, 0);
				const fullRange = new Range(start, end);
				editBuilder.delete(fullRange);
				editBuilder.insert(editor.selection.start, wpResult);
			});
			window.showInformationMessage('Proof obligation computed');

		} catch (err) {
			window.showErrorMessage('Failed to fetch and display WP proof obligation: ' + err.message);
			console.error('Error fetching WP proof obligation:', err);
		}
	});

	const wpResults = new MyTreeDataProvider();

    const provePO = commands.registerCommand('provePO', async () => {
		try {
            const function_name = await window.showInputBox({
                placeHolder: 'function', // Placeholder text in the input box
                prompt: 'Please specify functions to prove (c.f. -wp-fct )', // The prompt message
                validateInput: (input) => {
                    if (input.length === 0) {return 'Input cannot be empty!';}
                    return null; // Return null to indicate valid input
            }});
			const property_name = await window.showInputBox({
                placeHolder: 'property', // Placeholder text in the input box
                prompt: 'Please specify properties to prove (c.f. -wp-prop )', // The prompt message
                validateInput: (input) => {
                    if (input.length === 0) {return 'Input cannot be empty!';}
                    return null; // Return null to indicate valid input
            }});
			const proof_timeout = await window.showInputBox({
                placeHolder: 'timeout', // Placeholder text in the input box
                prompt: 'Please specify timeout for provers (c.f. -wp-timeout )', // The prompt message
                validateInput: (input) => {
                    if (input.length === 0) {return 'Input cannot be empty!';}
					if (!/^\d+$/.test(input)) {return 'Please enter a valid integer';}
                    return null; // Return null to indicate valid input
            }});
			const int_proof_timeout = parseInt(proof_timeout, 10);
			wpResults.update([]);
			wpResults.refresh();
            const res = await client.sendRequest('provePO', [window.activeTextEditor.document.fileName, function_name, property_name, int_proof_timeout]);
			wpResults.update(JSON.parse(JSON.stringify(res, null, 1)));
			wpResults.refresh();
			window.showInformationMessage('Proof results updated');
        }
        catch (err) {
            window.showErrorMessage('Failed to fetch and display WP proof: ' + err.message);
            console.error('Error fetching WP proof:', err);
        }
    });

    const provePOStrategies = commands.registerCommand('provePOStrategies', async () => {
		try {
            const function_name = await window.showInputBox({
                placeHolder: 'function', // Placeholder text in the input box
                prompt: 'Please specify functions to prove (c.f. -wp-fct )', // The prompt message
                validateInput: (input) => {
                    if (input.length === 0) {return 'Input cannot be empty!';}
                    return null; // Return null to indicate valid input
            }});
			const property_name = await window.showInputBox({
                placeHolder: 'property', // Placeholder text in the input box
                prompt: 'Please specify properties to prove (c.f. -wp-prop )', // The prompt message
                validateInput: (input) => {
                    if (input.length === 0) {return 'Input cannot be empty!';}
                    return null; // Return null to indicate valid input
            }});
			const proof_timeout = await window.showInputBox({
                placeHolder: 'timeout', // Placeholder text in the input box
                prompt: 'Please specify timeout for provers (c.f. -wp-timeout )', // The prompt message
                validateInput: (input) => {
                    if (input.length === 0) {return 'Input cannot be empty!';}
					if (!/^\d+$/.test(input)) {return 'Please enter a valid integer';}
                    return null; // Return null to indicate valid input
            }});
			const int_proof_timeout = parseInt(proof_timeout, 10);
			wpResults.update([]);
			wpResults.refresh();
            const res = await client.sendRequest('provePOStrategies', [window.activeTextEditor.document.fileName, function_name, property_name, int_proof_timeout]);
			wpResults.update(JSON.parse(JSON.stringify(res, null, 1)));
			wpResults.refresh();
			window.showInformationMessage('Proof results updated');
        }
        catch (err) {
            window.showErrorMessage('Failed to fetch and display WP proof: ' + err.message);
            console.error('Error fetching WP proof:', err);
        }
    });

	const showLocalMetrics = commands.registerCommand('showLocalMetrics', async () => {
		try {
			client.sendNotification('showLocalMetrics', window.activeTextEditor.document.fileName);
			window.showInformationMessage('Metrics file generated');
		} catch (err) {
			window.showErrorMessage('Failed to get local metrics: ' + err.message);
			console.error('Error getting local metrics:', err);
		}
	});

	const showGlobalMetrics = commands.registerCommand('showGlobalMetrics', async () => {
		try {
			client.sendNotification('showGlobalMetrics');
			window.showInformationMessage('Metrics file generated');
		} catch (err) {
			window.showErrorMessage('Failed to get global metrics: ' + err.message);
			console.error('Error getting global metrics:', err);
		}
	});

	window.registerTreeDataProvider('WPPan', wpResults);
	context.subscriptions.push(smokeTests, displayCIL, displayCIL_noannot, computeCG, showPOVC, provePO, showGlobalMetrics, showLocalMetrics);

	// Start the client. This will also launch the server
	client.start();
}

class MyTreeDataProvider implements vscode.TreeDataProvider<TreeItem> {
	private _onDidChangeTreeData: vscode.EventEmitter<TreeItem | undefined | null | void> = new vscode.EventEmitter<TreeItem | undefined | null | void>();
    readonly onDidChangeTreeData: vscode.Event<TreeItem | undefined | null | void> = this._onDidChangeTreeData.event;
	private data: TreeItem[];

	constructor() {this.data = [new TreeItem("No goals !")];}

	update(jsonData) {
		// Check if the data is an array (list)
        if (Array.isArray(jsonData)) {
			if (jsonData.length == 0) {this.data = [new TreeItem("No goals !")];}
			else {
			// Iterate over each item in the list
			this.data = [];
			jsonData.forEach((item, index) => {
				let item_list = item.trim().split(":");
				let verdict = item_list[0];
				let property = item_list[1];
				let file = item_list[2];
				let line = item_list[3];
				let stats = item_list[4];
				let script = item_list[5];
				let t_item = new TreeItem(verdict, property + " " + stats, 'itemContext');
				//let t_item = new TreeItem(item.trim(), 'itemContext');
				const workspacePath = workspace.workspaceFolders[0].uri.fsPath;
				t_item.command = {
					command: 'vscode.open',
					arguments: [vscode.Uri.parse(workspacePath + "/" + file + "#L" + line)]
				} as vscode.Command;
				this.data.push(t_item);
			});
		  }} else {
			vscode.window.showErrorMessage('Parsed JSON is not an array.');
		  }
	}
  
	getTreeItem(element: TreeItem): vscode.TreeItem|Thenable<vscode.TreeItem> {
	  return element;
	}
  
	getChildren(element?: TreeItem|undefined): vscode.ProviderResult<TreeItem[]> {
	  if (element === undefined) {
		return this.data;
	  }
	  return element.children;
	}

	refresh(): void {
		// Trigger the update by emitting the change event
		this._onDidChangeTreeData.fire();
	}
	
	addItem(newItem: TreeItem): void {
		this.data.push(newItem);
		this.refresh(); // Update the tree when an item is added
	}
	
	removeItem(itemToRemove: TreeItem): void {
		this.data = this.data.filter(item => item !== itemToRemove);
		this.refresh(); // Update the tree when an item is removed
	}

  }
  
  class TreeItem extends vscode.TreeItem {
	children: TreeItem[]|undefined;
  
	constructor(label: string, description?:string, context?:string, children?: TreeItem[]) {
	  	super(label, children === undefined ? vscode.TreeItemCollapsibleState.None : vscode.TreeItemCollapsibleState.Expanded);
		this.description = description;
	  	this.children = children;
	  	this.tooltip = `${this.label}`;
		if (this.label == "passed") {this.iconPath = new vscode.ThemeIcon('check');}
		else {this.iconPath = new vscode.ThemeIcon('error');}
	  	this.contextValue = context;
	}
  }

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
