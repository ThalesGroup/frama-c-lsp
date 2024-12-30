import * as path from 'path';
import { workspace, ExtensionContext, commands, extensions, window, ViewColumn, TabInputWebview, TextEditor, Uri, languages, Position, Range, env } from 'vscode';
import * as vscode from 'vscode';
import * as fs from 'fs';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind,
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
			transport: { kind: TransportKind.socket, port: 8005 },
			options: { shell: true }
		},
		debug: {
			command: serverModuleDebug,
			transport: { kind: TransportKind.socket, port: 8005 },
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

	/*
	const displayCIL = commands.registerCommand('displayCIL', async () => {
		try {
			const res = await client.sendRequest('displayCIL', window.activeTextEditor.document.fileName);
			const cilContent = JSON.parse(JSON.stringify(res, null, 1));

			// create a new untitled document in a new tab
			const newUri = Uri.parse('untitled:CIL Representation');
			const document = await workspace.openTextDocument(newUri);
			const editor = await window.showTextDocument(document, ViewColumn.Beside, true);
			await languages.setTextDocumentLanguage(document, 'acsl');

			// delete previous content if any and set the content of the new document
			editor.edit(editBuilder => {
				const start = new Position(0, 0);
				const end = new Position(document.lineCount, 0);
				const fullRange = new Range(start, end);
				editBuilder.delete(fullRange);
				editBuilder.insert(editor.selection.start, cilContent);
			});

		} catch (err) {
			window.showErrorMessage('Failed to fetch and display CIL data: ' + err.message);
			console.error('Error fetching CIL data:', err);
		}
	});
	*/

	const smokeTests = commands.registerCommand('smokeTests', async () => {
		try {
			await client.sendNotification('smokeTests', window.activeTextEditor.document.fileName);

		} catch (err) {
			window.showErrorMessage('Failed to run smoke tests: ' + err.message);
			console.error('Error computing smoke tests:', err);
		}
	});

	const displayCIL = commands.registerCommand('displayCIL', async () => {
		try {
			await client.sendNotification('displayCIL', window.activeTextEditor.document.fileName);

		} catch (err) {
			window.showErrorMessage('Failed to compute displayCIL: ' + err.message);
			console.error('Error computing displayCIL:', err);
		}
	});

	const displayCIL_noannot = commands.registerCommand('displayCIL_noannot', async () => {
		try {
			await client.sendNotification('displayCIL_noannot', window.activeTextEditor.document.fileName);

		} catch (err) {
			window.showErrorMessage('Failed to compute displayCIL_noannot: ' + err.message);
			console.error('Error computing displayCIL_noannot:', err);
		}
	});

	const computeCG = commands.registerCommand('computeCG', async () => {
		try {
			await client.sendNotification('computeCG', window.activeTextEditor.document.fileName);

		} catch (err) {
			window.showErrorMessage('Failed to compute callgraph: ' + err.message);
			console.error('Error computing callgraph:', err);
		}
	});

	const showPOVC = commands.registerCommand('showPOVC', async () => {
		try {
			const res = await client.sendRequest(
				'showPOVC', 
				[
					window.activeTextEditor.document.fileName, 
					window.activeTextEditor.selection.active
				]
			);
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
                prompt: 'Please specify properties to prove', // The prompt message
                validateInput: (input) => {
                    if (input.length === 0) {
                        return 'Input cannot be empty!';
                    }
                    return null; // Return null to indicate valid input
            }});
			const property_name = await window.showInputBox({
                placeHolder: 'property', // Placeholder text in the input box
                prompt: 'Please specify properties to prove', // The prompt message
                validateInput: (input) => {
                    if (input.length === 0) {
                        return 'Input cannot be empty!';
                    }
                    return null; // Return null to indicate valid input
            }});
            const res = await client.sendRequest('provePO', [window.activeTextEditor.document.fileName, function_name, property_name]);
            
			wpResults.update(JSON.parse(JSON.stringify(res, null, 1)));
			wpResults.refresh();
			window.showInformationMessage('Proof results updated in WP panel');

			// const wpResult = JSON.parse(JSON.stringify(res, null, 1));
            // // create a new untitled document in a new tab
            // const newUri = Uri.parse('untitled:Proof');
            // const document = await workspace.openTextDocument(newUri);
            // await languages.setTextDocumentLanguage(document, 'plaintext');
            // const editor_2 = await window.showTextDocument(document, ViewColumn.Beside, true);
            // // delete previous content if any and set the content of the new document
            // editor_2.edit(editBuilder => {
            //    const start = new Position(0, 0);
            //    const end = new Position(document.lineCount, 0);
            //    const fullRange = new Range(start, end);
            //    editBuilder.delete(fullRange);
            //    editBuilder.insert(editor_2.selection.start, wpResult);
            // });
        }
        catch (err) {
            window.showErrorMessage('Failed to fetch and display WP proof: ' + err.message);
            console.error('Error fetching WP proof:', err);
        }
    });
	const showLocalMetrics = commands.registerCommand('showLocalMetrics', async () => {
		try {
			client.sendNotification('showLocalMetrics', window.activeTextEditor.document.fileName);

		} catch (err) {
			window.showErrorMessage('Failed to get local metrics: ' + err.message);
			console.error('Error getting local metrics:', err);
		}
	});

	const showGlobalMetrics = commands.registerCommand('showGlobalMetrics', async () => {
		try {
			client.sendNotification('showGlobalMetrics');

		} catch (err) {
			window.showErrorMessage('Failed to get global metrics: ' + err.message);
			console.error('Error getting global metrics:', err);
		}
	});

	// does not work 
	const showCG = commands.registerCommand('showCG', async () => {
        const panel = window.createWebviewPanel(
            'pdfPreview', 
            'PDF Preview', 
            ViewColumn.Beside
        );
		console.log(Uri.file(window.activeTextEditor.document.uri.fsPath).toString());
        //const pdfFilePath = path.join((workspace.workspaceFolders[0].uri.fsPath), path.parse(window.activeTextEditor.document.uri.fsPath).name+".pdf");
		const workspacePath = workspace.workspaceFolders[0].uri.fsPath;
		const filePath = window.activeTextEditor.document.uri.fsPath;
		const pdfFilePath = path.join(workspacePath, ".frama-c/fc_"+path.basename(filePath, path.extname(filePath))+".dot.pdf");
		console.log (pdfFilePath);
        if (!fs.existsSync(pdfFilePath)) {
            window.showErrorMessage('PDF file'+ pdfFilePath +'not found');
            return;
        }
        const pdfFileUri = panel.webview.asWebviewUri(Uri.parse(pdfFilePath));
		console.log (pdfFileUri.toString());
        panel.webview.html = getWebviewContent(pdfFileUri);
    });

	window.registerTreeDataProvider('WPPan', wpResults);
	context.subscriptions.push(smokeTests, displayCIL, displayCIL_noannot, computeCG, showPOVC, provePO, showGlobalMetrics, showLocalMetrics, showCG);

	// Start the client. This will also launch the server
	client.start();
}

function getWebviewContent(pdfFileUri: Uri): string {
	const strpdf = pdfFileUri.toString();
    return `
        <!DOCTYPE html>
        <html lang="en">
        <head>
            <meta charset="UTF-8">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <title>PDF Preview</title>
        </head>
        <body style="margin: 0; padding: 0;">
            <iframe src=${strpdf} type="application/pdf" width="100%" height="100%"></iframe>
        </body>
        </html>
    `;
}

class MyTreeDataProvider implements vscode.TreeDataProvider<TreeItem> {
	private _onDidChangeTreeData: vscode.EventEmitter<TreeItem | undefined | null | void> = new vscode.EventEmitter<TreeItem | undefined | null | void>();
    readonly onDidChangeTreeData: vscode.Event<TreeItem | undefined | null | void> = this._onDidChangeTreeData.event;
	// onDidChangeTreeData?: vscode.Event<TreeItem|null|undefined>|undefined;
  
	private data: TreeItem[];
	// private items: TreeItem[] = [];

	constructor() {
		const i1 = new TreeItem('Fiesta');
		i1.command = {
			command: 'vscode.open',
			arguments: [vscode.Uri.parse("/home/user/git/L1/T0244734/acsl_lsp/Acsl_lsp/server/test_files/test1.c#L2")]
		} as vscode.Command;

		this.data = [new TreeItem("No goals !")];
	  // this.data = [new TreeItem('cars', [
		// new TreeItem(
		// 	'Ford', [i1, new TreeItem('Focus'), new TreeItem('Mustang')]),
		// new TreeItem(
		//	'BMW', [new TreeItem('320'), new TreeItem('X3'), new TreeItem('X5')])
	  // ])];
	}

	update(jsonData) {
		// Check if the data is an array (list)
        if (Array.isArray(jsonData)) {
			// Iterate over each item in the list
			this.data = [];
			jsonData.forEach((item, index) => {
				let item_list = item.trim().split(":");
				let verdict = item_list[0];
				let property = item_list[1];
				let file = item_list[2];
				let line = item_list[3];
				let t_item = new TreeItem(verdict + " : " + property);
				const workspacePath = workspace.workspaceFolders[0].uri.fsPath;
				t_item.command = {
					command: 'vscode.open',
					arguments: [vscode.Uri.parse(workspacePath + "/" + file + "#L" + line)]
				} as vscode.Command;
				this.data.push(t_item);
			  	// vscode.window.showInformationMessage(`Item ${index + 1}: ${JSON.stringify(item)}`);
				vscode.window.showInformationMessage(file + "#L" + line);
			});
		  } else {
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
  
	constructor(label: string, children?: TreeItem[]) {
	  super(
		  label,
		  children === undefined ? vscode.TreeItemCollapsibleState.None :
								   vscode.TreeItemCollapsibleState.Expanded);
	  this.children = children;
	}
  }

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
