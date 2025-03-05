import * as path from 'path';
import { workspace, ExtensionContext, commands, extensions, window, ViewColumn, TabInputWebview, TextEditor, Uri, languages, Position, Range, env } from 'vscode';
import * as vscode from 'vscode';
import * as fs from 'fs';
import {LanguageClient,	LanguageClientOptions, ServerOptions, TransportKind} from 'vscode-languageclient/node';
import { exec } from 'child_process';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	// The server is implemented in OCaml
	const serverPort = vscode.workspace.getConfiguration('vscodeacsl').get<number>('serverPort') || 8005;
	const wrapperPort = vscode.workspace.getConfiguration('vscodeacsl').get<number>('wrapperPort') || (serverPort + 1);
	const serverModuleRun = context.asAbsolutePath(path.join('run.sh')) + " " + serverPort + " " + wrapperPort;
	const serverModuleDebug = context.asAbsolutePath(path.join('run.sh')) + " " + serverPort + " " + wrapperPort;

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	const serverOptions: ServerOptions = {
		run: {command: serverModuleRun,	transport: { kind: TransportKind.socket, port: serverPort }, options: { shell: true }},
		debug: {command: serverModuleDebug,	transport: { kind: TransportKind.socket, port: serverPort }, options: { shell: true }}
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
			const editor = window.activeTextEditor;
       		if (!editor) {
           		window.showErrorMessage('No active editor found.');
           		return;
       		}
			await client.sendNotification('smokeTests', editor.document.fileName);
		} catch (err) {
			const errorMessage = err instanceof Error ? err.message : String(err);
			window.showErrorMessage('Failed to run smoke tests: ' + errorMessage);
			console.error('Error computing smoke tests:', err);
		}
	});

	const displayCIL = commands.registerCommand('displayCIL', async () => {
		try {
			const editor = window.activeTextEditor;
       		if (!editor) {
           		window.showErrorMessage('No active editor found.');
           		return;
       		}
			const filePath = editor.document.fileName;
    		const fileName = path.basename(filePath);
			await create_file(fileName, 'acsl');
			await client.sendNotification('displayCIL', filePath);
		} catch (err) {
			const errorMessage = err instanceof Error ? err.message : String(err);
			window.showErrorMessage('Failed to compute displayCIL: ' + errorMessage);
			console.error('Error computing displayCIL:', err);
		}
	});

	const displayCIL_noannot = commands.registerCommand('displayCIL_noannot', async () => {
		try {
			const editor = window.activeTextEditor;
       		if (!editor) {
           		window.showErrorMessage('No active editor found.');
           		return;
       		}
			const filePath = editor.document.fileName;
    		const fileName = path.basename(filePath);
			await create_file(fileName, 'acsl');
			await client.sendNotification('displayCIL_noannot', filePath);
		} catch (err) {
			const errorMessage = err instanceof Error ? err.message : String(err);
			window.showErrorMessage('Failed to compute displayCIL_noannot: ' + errorMessage);
			console.error('Error computing displayCIL_noannot:', err);
		}
	});

	const displayCILProject = commands.registerCommand('displayCILProject', async () => {
		try {
    		const fileName = "project.c";
			await create_file(fileName, 'acsl');
			await client.sendNotification('displayCILProject');
		} catch (err) {
			const errorMessage = err instanceof Error ? err.message : String(err);
			window.showErrorMessage('Failed to compute displayCILProject: ' + errorMessage);
			console.error('Error computing displayCILProject:', err);
		}
	});

	const displayCILProject_noannot = commands.registerCommand('displayCILProject_noannot', async () => {
		try {
    		const fileName = "project.c";
			await create_file(fileName, 'acsl');
			await client.sendNotification('displayCILProject_noannot');
		} catch (err) {
			const errorMessage = err instanceof Error ? err.message : String(err);
			window.showErrorMessage('Failed to compute displayCILProject_noannot: ' + errorMessage);
			console.error('Error computing displayCILProject_noannot:', err);
		}
	});

	const computeCG = commands.registerCommand('computeCG', async () => {
		try {
			const editor = window.activeTextEditor;
       		if (!editor) {
           		window.showErrorMessage('No active editor found.');
           		return;
       		}
			const filePath = editor.document.fileName;
			const dirPath = path.dirname(filePath);
    		const fileName = path.basename(filePath);
			const extension = path.extname(filePath);
			const fileNameBase = fileName.slice(0, -extension.length);
			const workspacePath = get_workspace ();
			const filePathOut = path.join(workspacePath, ".frama-c", "fc_" + fileNameBase + ".dot.pdf");
			if (!fs.existsSync(filePathOut)) {
				try {fs.writeFileSync(filePathOut, 'Task in progress ...')}
				catch (error) {vscode.window.showErrorMessage(`Failed to create the file: ${error.message}`);}
			}
			const fileUri = vscode.Uri.parse(filePathOut);
			await vscode.commands.executeCommand('revealInExplorer', fileUri);

			await client.sendNotification('computeCG', filePath);
		} catch (err) {
			const errorMessage = err instanceof Error ? err.message : String(err);
			window.showErrorMessage('Failed to compute callgraph: ' + errorMessage);
			console.error('Error computing callgraph:', err);
		}
	});

	const showPOVC = commands.registerCommand('showPOVC', async () => {
		try {
			const editor = window.activeTextEditor;
       		if (!editor) {
           		window.showErrorMessage('No active editor found.');
           		return;
       		}
			const res = await client.sendRequest('showPOVC', [editor.document.fileName, editor.selection.active]);
			const wpResult = JSON.parse(JSON.stringify(res, null, 1));
			const newUri = Uri.parse('untitled:Proof Obligation');
			const document = await workspace.openTextDocument(newUri);
			await languages.setTextDocumentLanguage(document, 'plaintext');
			const textDocument = await window.showTextDocument(document, ViewColumn.One, true);

			textDocument.edit(editBuilder => {
				const start = new Position(0, 0);
				const end = new Position(document.lineCount, 0);
				const fullRange = new Range(start, end);
				editBuilder.delete(fullRange);
				editBuilder.insert(textDocument.selection.start, wpResult);
			});
			window.showInformationMessage('Proof obligation computed');

		} catch (err) {
			const errorMessage = err instanceof Error ? err.message : String(err);
			window.showErrorMessage('Failed to fetch and display WP proof obligation: ' + errorMessage);
			console.error('Error fetching WP proof obligation:', err);
		}
	});


	const showPO = commands.registerCommand('showPO', async () => {
		try {
			const selectedItems = wpResultsView.selection;
        	if (selectedItems.length > 0) {
				const selectedItem = selectedItems[0];
				const workspacePath = get_workspace ();
				let fileUri: vscode.Uri;
				const path_name = path.join(workspacePath, ".frama-c", `${selectedItem.goal_id}.txt`);
				fileUri = vscode.Uri.parse(path_name);
				const document = await workspace.openTextDocument(fileUri);
				await languages.setTextDocumentLanguage(document, 'plaintext');
				const editor = await window.showTextDocument(document, ViewColumn.One, true);
			}
			else {vscode.window.showInformationMessage('No item selected');}
		} catch (err) {
			const errorMessage = err instanceof Error ? err.message : String(err);
			window.showErrorMessage('Failed to fetch and display WP proof obligation: ' + errorMessage);
			console.error('Error fetching WP proof obligation:', err);
		}
	});

	const wpResults = new MyTreeDataProvider();
	const wpResultsView = window.createTreeView('WPPan', {treeDataProvider: wpResults,
        showCollapseAll: true,      // Show "Collapse All" button
        canSelectMany: true,        // Allow multiple selection in the tree
        //contextValue: 'myTree',     // A context value for filtering actions/commands
		});

	const showScript = commands.registerCommand('showScript', async (item: TreeItem) => {
		try {
			const selectedItems = wpResultsView.selection;
        	if (selectedItems.length > 0) {
				const selectedItem = selectedItems[0];
				const workspacePath = get_workspace ();
				let fileUri: vscode.Uri;
				fileUri = vscode.Uri.parse(`${workspacePath}/${selectedItem.script}`);
				const document = await workspace.openTextDocument(fileUri);
				await languages.setTextDocumentLanguage(document, 'plaintext');
				const editor = await window.showTextDocument(document, ViewColumn.One, true);
			}
			else {vscode.window.showInformationMessage('No item selected');}
		} catch (err) {
			const errorMessage = err instanceof Error ? err.message : String(err);
			window.showErrorMessage('Failed to fetch and display script: ' + errorMessage);
			console.error('Error fetching script:', err);
		}
	});

	const runAgain = commands.registerCommand('runAgain', async (item: TreeItem) => {
		try {
			const selectedItems = wpResultsView.selection;
        	if (selectedItems.length > 0) {
				const selectedItem = selectedItems[0];
				const args = await get_args_from_item(selectedItem, false);
				const res = await client.sendRequest('provePO', args);
				wpResults.update(JSON.parse(JSON.stringify(res, null, 1)));
				wpResults.refresh();
				window.showInformationMessage('Proof results updated');

			}
			else {vscode.window.showInformationMessage('No item selected');}
		} catch (err) {
			const errorMessage = err instanceof Error ? err.message : String(err);
			window.showErrorMessage('Failed to fetch and display script: ' + errorMessage);
			console.error('Error fetching script:', err);
		}
	});

	const runAgainGui = commands.registerCommand('runAgainGui', async (item: TreeItem) => {
		try {
			const selectedItems = wpResultsView.selection;
        	if (selectedItems.length > 0) {
				const selectedItem = selectedItems[0];
				const args = await get_args_from_item(selectedItem, true);
				const res = await client.sendRequest('provePO', args);
				wpResults.update(JSON.parse(JSON.stringify(res, null, 1)));
				wpResults.refresh();
				window.showInformationMessage('Proof results updated');

			}
			else {vscode.window.showInformationMessage('No item selected');}
		} catch (err) {
			const errorMessage = err instanceof Error ? err.message : String(err);
			window.showErrorMessage('Failed to fetch and display script: ' + errorMessage);
			console.error('Error fetching script:', err);
		}
	});

	const runAgainStrategies = commands.registerCommand('runAgainStrategies', async (item: TreeItem) => {
		try {
			const selectedItems = wpResultsView.selection;
        	if (selectedItems.length > 0) {
				const selectedItem = selectedItems[0];
				const args = await get_args_from_item(selectedItem, false);
				const res = await client.sendRequest('provePOStrategies', args);
				wpResults.update(JSON.parse(JSON.stringify(res, null, 1)));
				wpResults.refresh();
				window.showInformationMessage('Proof results updated');

			}
			else {vscode.window.showInformationMessage('No item selected');}
		} catch (err) {
			const errorMessage = err instanceof Error ? err.message : String(err);
			window.showErrorMessage('Failed to fetch and display script: ' + errorMessage);
			console.error('Error fetching script:', err);
		}
	});

	const runAgainStrategiesGui = commands.registerCommand('runAgainStrategiesGui', async (item: TreeItem) => {
		try {
			const selectedItems = wpResultsView.selection;
        	if (selectedItems.length > 0) {
				const selectedItem = selectedItems[0];
				const args = await get_args_from_item(selectedItem, true);
				const res = await client.sendRequest('provePOStrategies', args);
				wpResults.update(JSON.parse(JSON.stringify(res, null, 1)));
				wpResults.refresh();
				window.showInformationMessage('Proof results updated');

			}
			else {vscode.window.showInformationMessage('No item selected');}
		} catch (err) {
			const errorMessage = err instanceof Error ? err.message : String(err);
			window.showErrorMessage('Failed to fetch and display script: ' + errorMessage);
			console.error('Error fetching script:', err);
		}
	});

    const provePO = commands.registerCommand('provePO', async () => {
		try {
			const args = await get_proof_args(false);
            const res = await client.sendRequest('provePO', args);
			wpResults.update(JSON.parse(JSON.stringify(res, null, 1)));
			wpResults.refresh();
			window.showInformationMessage('Proof results updated');
        }
        catch (err) {
			const errorMessage = err instanceof Error ? err.message : String(err);
            window.showErrorMessage('Failed to fetch and display WP proof: ' + errorMessage);
            console.error('Error fetching WP proof:', err);
        }
    });

	const provePOGUI = commands.registerCommand('provePOGUI', async () => {
		try {
            const args = await get_proof_args(true);
            const res = await client.sendRequest('provePO', args);
			window.showInformationMessage('frama-c-gui closed');
        }
        catch (err) {
			const errorMessage = err instanceof Error ? err.message : String(err);
            window.showErrorMessage('Failed to fetch and display WP proof: ' + errorMessage);
            console.error('Error fetching WP proof:', err);
        }
    });

    const provePOStrategies = commands.registerCommand('provePOStrategies', async () => {
		try {
            const args = await get_proof_args(false);
            const res = await client.sendRequest('provePOStrategies', args);
			wpResults.update(JSON.parse(JSON.stringify(res, null, 1)));
			wpResults.refresh();
			window.showInformationMessage('Proof results updated');
        }
        catch (err) {
			const errorMessage = err instanceof Error ? err.message : String(err);
            window.showErrorMessage('Failed to fetch and display WP proof: ' + errorMessage);
            console.error('Error fetching WP proof:', err);
        }
    });

    const provePOStrategiesGUI = commands.registerCommand('provePOStrategiesGUI', async () => {
		try {
            const args = await get_proof_args(true);
            const res = await client.sendRequest('provePOStrategies', args);
			window.showInformationMessage('frama-c-gui closed');
        }
        catch (err) {
			const errorMessage = err instanceof Error ? err.message : String(err);
            window.showErrorMessage('Failed to fetch and display WP proof: ' + errorMessage);
            console.error('Error fetching WP proof:', err);
        }
    });

    const stop = commands.registerCommand('stop', async () => {
		try {
            const res = await client.sendRequest('stop');
			window.showInformationMessage('Stopped processes');
        }
        catch (err) {
			const errorMessage = err instanceof Error ? err.message : String(err);
            window.showErrorMessage('Failed to stop Frama-C: ' + errorMessage);
            console.error('Error stropping Frama-C:', err);
        }
    });


	const showLocalMetrics = commands.registerCommand('showLocalMetrics', async () => {
		try {
			const editor = window.activeTextEditor;
       		if (!editor) {
           		window.showErrorMessage('No active editor found.');
           		return;
       		}
			const filePath = editor.document.fileName;
			const file_name = "metrics.txt";
			await create_file(file_name, 'plaintext');
			client.sendNotification('showLocalMetrics', filePath);
		} catch (err) {
			const errorMessage = err instanceof Error ? err.message : String(err);
			window.showErrorMessage('Failed to get local metrics: ' + errorMessage);
			console.error('Error getting local metrics:', err);
		}
	});

	const showGlobalMetrics = commands.registerCommand('showGlobalMetrics', async () => {
		try {
			const file_name = "metrics.txt";
			await create_file(file_name, 'plaintext');
			client.sendNotification('showGlobalMetrics');
		} catch (err) {
			const errorMessage = err instanceof Error ? err.message : String(err);
			window.showErrorMessage('Failed to get global metrics: ' + errorMessage);
			console.error('Error getting global metrics:', err);
		}
	});

	context.subscriptions.push(smokeTests, displayCIL, displayCIL_noannot, displayCILProject, displayCILProject_noannot, computeCG, showPOVC, showPO, runAgain, runAgainGui, runAgainStrategies, runAgainStrategiesGui, provePO, provePOGUI, provePOStrategies, provePOStrategiesGUI, showGlobalMetrics, showLocalMetrics);

	// Start the client. This will also launch the server
	client.start();
}

class MyTreeDataProvider implements vscode.TreeDataProvider<TreeItem> {
	private _onDidChangeTreeData: vscode.EventEmitter<TreeItem | undefined | null | void> = new vscode.EventEmitter<TreeItem | undefined | null | void>();
    readonly onDidChangeTreeData: vscode.Event<TreeItem | undefined | null | void> = this._onDidChangeTreeData.event;
	private data: TreeItem[];

	constructor() {this.data = [new TreeItem("No goals !")];}

	update(data) {
		// Check if the data is an array (list)
        if (Array.isArray(data)) {
			let [filename_id, fct_id, prop_id, jsonData] = data;
			if (jsonData.length == 0) {this.data = [new TreeItem("No goals !")];}
			else {
			// Iterate over each item in the list
			this.data = [];
			jsonData.forEach((item, index) => {
				let item_list = item.trim().split(":");
				let verdict = item_list[0].trim();
				let goal_id = item_list[1].trim();
				let file_id = item_list[2].trim();
				let line = item_list[3].trim();
				let stats = item_list[4].trim();
				let script = item_list[5].trim();
				let function_id = item_list[6].trim();
				let property_id = item_list[7].trim();
				let t_item = new TreeItem(verdict, goal_id + " " + stats, file_id, function_id, goal_id, script, filename_id, fct_id, prop_id, 'itemContext');
				const workspacePath = get_workspace ();
				t_item.command = {
					command: 'vscode.open',
					arguments: [vscode.Uri.parse(path.join(workspacePath, file_id) + "#L" + line)]
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

	dispose() {
		this._onDidChangeTreeData.dispose();
	}

  }
  
class TreeItem extends vscode.TreeItem {
	children: TreeItem[]|undefined;
  
	constructor(label: string, description?:string, public file_id?:string, public function_id?:string, public goal_id?:string, public script?:string, public filename_id?:string, public fct_id?:string, public prop_id?:string, context?:string, children?: TreeItem[]) {
	  	super(label, children === undefined ? vscode.TreeItemCollapsibleState.None : vscode.TreeItemCollapsibleState.Expanded);
		this.description = description;
	  	this.children = children;
	  	this.tooltip = `${this.label}`;
		
		if (this.label == "passed") {this.iconPath = new vscode.ThemeIcon('check');}
		else {this.iconPath = new vscode.ThemeIcon('error');}
	  	this.contextValue = context;
	}
}


async function create_file(fileName:string, type:string){
	const workspacePath = get_workspace ();
	const fileNameOut = path.join(workspacePath, ".frama-c", `fc_${fileName}`);
	if (!fs.existsSync(fileNameOut)) {
		try {fs.writeFileSync(fileNameOut, 'Task in progress ...')}
		catch (error) {vscode.window.showErrorMessage(`Failed to create the file: ${error.message}`);}
	}
	const fileUri = vscode.Uri.parse(fileNameOut);
	const document = await workspace.openTextDocument(fileUri);
	await languages.setTextDocumentLanguage(document, type);
	const editor = await window.showTextDocument(document, ViewColumn.One, true);
}

async function get_args_from_item(selectedItem:TreeItem, gui:boolean){
	const workspacePath = get_workspace ();
	const file_name = selectedItem.file_id;
	const function_name = selectedItem.fct_id;
	const property_name = selectedItem.prop_id;
	const proof_timeout = await window.showInputBox({
		placeHolder: 'timeout',
		prompt: 'Please specify timeout for provers (c.f. -wp-timeout )',
		validateInput: (input) => {
			if (input.length === 0) {return 'Input cannot be empty!';}
			if (!/^\d+$/.test(input)) {return 'Please enter a valid integer';}
			return null; // Return null to indicate valid input
	}});
	const int_proof_timeout = parseInt(proof_timeout, 10);
	// If need to empty the list of item:
	// wpResults.update(["","","",[]]);
	// wpResults.refresh();
	return [file_name, function_name, property_name, int_proof_timeout, gui]
}

async function get_proof_args(gui:boolean){
	const function_name = await window.showInputBox({
		placeHolder: 'function',
		prompt: 'Please specify functions to prove (c.f. -wp-fct ) (@all for all functions)',
		validateInput: (input) => {
			if (input.length === 0) {return 'Input cannot be empty!';}
			return null; // Return null to indicate valid input
	}});
	const property_name = await window.showInputBox({
		placeHolder: 'property',
		prompt: 'Please specify properties to prove (c.f. -wp-prop (@all for all properties))',
		validateInput: (input) => {
			if (input.length === 0) {return 'Input cannot be empty!';}
			return null; // Return null to indicate valid input
	}});
	const proof_timeout = await window.showInputBox({
		placeHolder: 'timeout',
		prompt: 'Please specify timeout for provers (c.f. -wp-timeout )',
		validateInput: (input) => {
			if (input.length === 0) {return 'Input cannot be empty!';}
			if (!/^\d+$/.test(input)) {return 'Please enter a valid integer';}
			return null; // Return null to indicate valid input
	}});
	const int_proof_timeout = parseInt(proof_timeout, 10);
	// If need to empty the list of item:
	// wpResults.update(["","","",[]]);
	// wpResults.refresh();
	const editor = window.activeTextEditor;
    if (!editor) {
    	window.showErrorMessage('No active editor found.');
        return;
    }
	return([editor.document.fileName, function_name, property_name, int_proof_timeout, gui]);
}

function get_workspace(){
	if (vscode.workspace.workspaceFolders && vscode.workspace.workspaceFolders.length > 0) {
		const workspacePath = workspace.workspaceFolders[0].uri.fsPath;
		create_frama_c_folder(workspacePath);
		return workspacePath;
	} else {
		const currentFolder = process.cwd();
		create_frama_c_folder(currentFolder);
		return currentFolder
	}
}


async function create_frama_c_folder(workspace:string){
	try {
		const path_name = path.join(workspace, ".frama-c");
		await fs.promises.mkdir(path_name, {recursive: true})
	} catch(err) {
	}
}

/*
export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
*/

export function deactivate(context: ExtensionContext): Thenable<void> | undefined {
	if (client) {
		client.stop();
	}
	context.subscriptions.forEach(subscription => subscription.dispose());
	return undefined;
}