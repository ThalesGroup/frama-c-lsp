import * as path from 'path';
import { workspace, ExtensionContext, commands, extensions, window, ViewColumn, TabInputWebview, TextEditor, Uri, languages, Position, Range, env } from 'vscode';
import * as vscode from 'vscode';
import * as fs from 'fs';
import {LanguageClient,	LanguageClientOptions, ServerOptions, TransportKind} from 'vscode-languageclient/node';
import { exec } from 'child_process';
import { ProjectImporter } from './projectImporter';
let client: LanguageClient;
let framaCProvider: FramaCProvider; 
let isDelegating = false;

export function activate(context: ExtensionContext) {
	// The server is implemented in OCaml
	const config = vscode.workspace.getConfiguration();
	const serverPort = config.get<number>('kernel.serverPort') || 8005;
	const wrapperPort = config.get<number>('kernel.wrapperPort') || (serverPort + 1);
	const serverModuleRun = context.asAbsolutePath(path.join('run.sh')) + " " + serverPort + " " + wrapperPort;
	const serverModuleDebug = context.asAbsolutePath(path.join('run.sh')) + " " + serverPort + " " + wrapperPort;
	const importer = new ProjectImporter();

    
    let disposable = vscode.commands.registerCommand('acsl.importProjectConfig', async () => {
        await importer.showProjectSelector();
    });
	const importStatusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100);
    importStatusBarItem.command = 'acsl.importProjectConfig'; 
    importStatusBarItem.text = '$(project) JCAT: Import IDE';
    importStatusBarItem.tooltip = 'Cliquer to Import Config File';
    importStatusBarItem.show();

    context.subscriptions.push(disposable);

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	const serverOptions: ServerOptions = {
		run: {command: serverModuleRun,	transport: { kind: TransportKind.socket, port: serverPort }, options: { shell: true }},
		debug: {command: serverModuleDebug,	transport: { kind: TransportKind.socket, port: serverPort }, options: { shell: true }}
	};

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for c files containing acsl annotations
		documentSelector: [{ scheme: 'file', language: 'c' },
			{ scheme: 'file', language: 'acsl' }
		],
		// Notify the server about file changes to '.clientrc files contained in the workspace
		synchronize: {fileEvents: workspace.createFileSystemWatcher('**/.clientrc')}
	};

	// Create the language client and start the client.
	client = new LanguageClient('vscodeacsl', 'ACSL Language Server', serverOptions, clientOptions);

framaCProvider = new FramaCProvider();


context.subscriptions.push(
    vscode.commands.registerCommand('framaCExplorer.search', async () => {
        const val = await vscode.window.showInputBox({ prompt: "Filtrer l'arborescence..." });
        framaCProvider.setFilter(val || "");
    }),
   vscode.commands.registerCommand('framaC.toggleFunctions', () => framaCProvider.toggleFunctions()),
    vscode.commands.registerCommand('framaC.toggleVariables', () => framaCProvider.toggleVariables()),
    vscode.commands.registerCommand('framaC.toggleTypes', () => framaCProvider.toggleTypes()),
    vscode.commands.registerCommand('framaC.toggleAnnotations', () => framaCProvider.toggleAnnotations())
);
    let wpResultsView = vscode.window.createTreeView('framaCExplorer', {
        treeDataProvider: framaCProvider,
        showCollapseAll: true,
        canSelectMany: true
    });

	client.onNotification("custom/getWPResponse", (data) => {
		framaCProvider.updateWP(data);
	});
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
	
	let debugAstCommand = commands.registerCommand('acsl-lsp.debugAST', async () => {
    const editor = window.activeTextEditor;
    if (!editor) return;

    const uri = editor.document.uri.toString();
    
    try {
        const response = await client.sendRequest("custom/getAST", { uri: uri });
        
        if (response) {
            framaCProvider.updateAST(uri, response);
            window.showInformationMessage("AST Charged!");
        } else {
            window.showWarningMessage("No AST Found");
        }
    } catch (error) {
        console.error(error);
    }
});
	
	const ccdoc = commands.registerCommand('ccdoc', async () => {
		try {
			await client.sendNotification('ccdoc');
		} catch (err) {
			const errorMessage = err instanceof Error ? err.message : String(err);
			window.showErrorMessage('Failed to run ccdoc: ' + errorMessage);
			console.error('Error computing ccdoc:', err);
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
			await create_file(fileName, 'c');
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
			await create_file(fileName, 'c');
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
			await create_file(fileName, 'c');
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
			await create_file(fileName, 'c');
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
				const selectedItem = selectedItems[0] as FramaCItem;
				const args = await get_args_from_item(selectedItem, false);
				const res = await client.sendRequest('provePO', args);
				framaCProvider.updateWP(JSON.parse(JSON.stringify(res, null, 1)));
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
				const selectedItem = selectedItems[0] as FramaCItem;
				const args = await get_args_from_item(selectedItem, true);
				const res = await client.sendRequest('provePO', args);
				framaCProvider.updateWP(JSON.parse(JSON.stringify(res, null, 1)));
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
				framaCProvider.updateWP(JSON.parse(JSON.stringify(res, null, 1)));
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
				framaCProvider.updateWP(JSON.parse(JSON.stringify(res, null, 1)));
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
			framaCProvider.updateWP(JSON.parse(JSON.stringify(res, null, 1)));
			window.showInformationMessage('Proof results updated');
        }
        catch (err) {
			const errorMessage = err instanceof Error ? err.message : String(err);
            window.showErrorMessage('Failed to fetch and display WP proof: ' + errorMessage);
            console.error('Error fetching WP proof:', err);
        }
    });
	let provePOCursor = vscode.commands.registerCommand('provePOCursor', async () => {
        try {
            const editor = window.activeTextEditor;
            if (!editor) return;

            const proof_timeout = await window.showInputBox({
                placeHolder: 'timeout',
                prompt: `Launch Auto-Proof (Timeout in s)`,
                value: '10',
                validateInput: (input) => /^\d+$/.test(input) ? null : 'Int required'
            });

            if (!proof_timeout) return;

            const args = [
                editor.document.fileName,
                editor.selection.active.line,
                parseInt(proof_timeout, 10)
            ];

            const res: any = await client.sendRequest('custom/proveAuto', args);
    
    if (res && Array.isArray(res) && res.length >= 3) {
        const funcName = res[1];
        const propName = res[2];
        const goals = res[3];

        
        if (propName === "@all" || propName === "") {
            window.showInformationMessage(`Function '${funcName}' selected. Proving ALL properties.`);
        } 
        else if (propName.startsWith('@')) {
            window.showInformationMessage(`Unnamed property (${propName}) in '${funcName}'. Proving similar properties. (Tip: name it for better precision).`);
        } 
        else {
            window.showInformationMessage(`Targeting specific property '${propName}' in '${funcName}'.`);
        }

        framaCProvider.updateWP(res);
    

    } else {
        window.showWarningMessage("No context detected. Please click on a function name or an ACSL property.");
    }

        }
        catch (err) {
            window.showErrorMessage('Error during Auto-Proof: ' + err);
        }
    });
    const refreshDetails = commands.registerCommand('acsl-lsp.refreshDetails', async (item: FramaCItem) => {
    if (item.contextValue === "function") {
        framaCProvider.refresh(); 
    }
});
context.subscriptions.push(refreshDetails);
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
			framaCProvider.updateWP(JSON.parse(JSON.stringify(res, null, 1)));
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
	

	context.subscriptions.push(smokeTests, ccdoc, displayCIL, displayCIL_noannot, displayCILProject, displayCILProject_noannot, computeCG, showPOVC, showPO, runAgain, runAgainGui, runAgainStrategies, runAgainStrategiesGui, provePO, provePOGUI, provePOStrategies, provePOStrategiesGUI, showGlobalMetrics, showLocalMetrics,provePOCursor,debugAstCommand);

	
    // ----------------------------------------------------------------
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

async function get_args_from_item(selectedItem:FramaCItem, gui:boolean){
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

interface AcslContext {
    function: string;
    property: string;
}

interface AcslResponse {
    jsonrpc?: string;
    id?: number;
    result?: AcslContext;
    function?: string; 
}

async function get_acsl_context(client: LanguageClient): Promise<AcslContext | null> {
    const editor = window.activeTextEditor;
    if (!editor) { return null; }

    const position = editor.selection.active;
    const filename = editor.document.fileName;

    try {
        
        const res = await client.sendRequest('getAcslContext', [
            filename, 
            position.line + 1, 
            position.character
        ]) as AcslResponse;

        if (res && res.result) {
            return res.result; 
        } 
        
        else if (res && (res as any).function) {
            return res as any as AcslContext;
        }

        return null;

    } catch (err) {
        console.error('Error getAcslContext:', err);
        return null;
    }
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
		prompt: 'Please specify properties to prove (c.f. -wp-prop ) (@all for all properites)',
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



export function deactivate(context: ExtensionContext): Thenable<void> | undefined {
	if (client) {
		client.stop();
	}
	context.subscriptions.forEach(subscription => subscription.dispose());
	return undefined;
}

export class FramaCProvider implements vscode.TreeDataProvider<FramaCItem> {
    private _onDidChangeTreeData = new vscode.EventEmitter<FramaCItem | undefined | void>();
    readonly onDidChangeTreeData = this._onDidChangeTreeData.event;

    private astData: Map<string, any> = new Map();
    private wpDataByFile: Map<string, FramaCItem[]> = new Map();
    
    private searchQuery: string = "";
    private hideVariables: boolean = false;
    private hideFunctions: boolean = false;
    private hideTypes: boolean = false;
    private hideAnnotations: boolean = false;

    refresh(): void {
        this._onDidChangeTreeData.fire();
    }

    setFilter(query: string) {
        this.searchQuery = query.toLowerCase().trim();
        this.refresh();
    }
    toggleVariables() { this.hideVariables = !this.hideVariables; this.refresh(); }
    toggleFunctions() { this.hideFunctions = !this.hideFunctions; this.refresh(); }
    toggleTypes() { this.hideTypes = !this.hideTypes; this.refresh(); }
    toggleAnnotations() { this.hideAnnotations = !this.hideAnnotations; this.refresh(); }
    updateAST(uri: string, data: any) {
        if (data) {
            this.astData.set(uri, data);
            this.refresh();
        }}
    updateWP(data: any) {
        if (Array.isArray(data)) {
            const [filename_id, , , jsonData] = data;
            const fileUri = vscode.Uri.file(filename_id).toString();

            const items = jsonData.map((item: string) => {
                const p = item.trim().split(":");
                return new FramaCItem(
                    `${p[1]} [Line ${p[3]}]`,
                    vscode.TreeItemCollapsibleState.None,
                    "goal",
                    false,
                    vscode.Uri.file(filename_id),
                    p[1], p[5], p[2], p[6], p[7], p[0]
                );
            });
            this.wpDataByFile.set(fileUri, items);
        }
        this.refresh();
    }

    getTreeItem(element: FramaCItem): vscode.TreeItem { return element; }

    async getChildren(element?: FramaCItem): Promise<FramaCItem[]> {
        const matches = (name: string) => name.toLowerCase().includes(this.searchQuery);

        if (!element || element.contextValue === "folder") {
            const rootPath = vscode.workspace.workspaceFolders?.[0].uri.fsPath || "";
            const currentDir = element ? element.resourceUri!.fsPath : rootPath;
            
            try {
                const entries = await fs.promises.readdir(currentDir, { withFileTypes: true });
                let items: FramaCItem[] = [];

                for (const entry of entries) {
                    const fullPath = path.join(currentDir, entry.name);
                    const uri = vscode.Uri.file(fullPath);

                    if (entry.isDirectory() && !entry.name.startsWith('.')) {
                        items.push(new FramaCItem(entry.name, vscode.TreeItemCollapsibleState.Collapsed, "folder", false, uri));
                    } 
                    else if (entry.isFile() && (entry.name.endsWith('.c') || entry.name.endsWith('.h'))) {
                        if (this.searchQuery === "" || this.fileHasMatch(uri.toString(), matches)) {
                            items.push(new FramaCItem(entry.name, vscode.TreeItemCollapsibleState.Collapsed, "file", false, uri));
                        }
                    }
                }
                return items.sort((a, b) => a.label.toString().localeCompare(b.label.toString()));
            } catch (err) {
                return [];
            }
        }

        if (element.contextValue === "file") {
            const fileUri = element.resourceUri?.toString() || "";
            const data = this.astData.get(fileUri);
            let children: FramaCItem[] = [];

            if (data) {
				
                if (!this.hideFunctions && data.functions) {
                    data.functions.filter((f: any) => matches(f.name)).forEach((f: any) => {
                        children.push(new FramaCItem(f.name, vscode.TreeItemCollapsibleState.None, "function", true, element.resourceUri));
                    });
                }
                if (!this.hideVariables && data.globals) {
                    data.globals.filter((g: any) => matches(g.name)).forEach((g: any) => {
                        children.push(new FramaCItem(g.name, vscode.TreeItemCollapsibleState.None, "variable", false, element.resourceUri));
                    });
                }
                if (!this.hideTypes && data.types) {
                    data.types.filter((t: any) => matches(t.name)).forEach((t: any) => {
                        children.push(new FramaCItem(t.name, vscode.TreeItemCollapsibleState.None, "type", false, element.resourceUri));
                    });
                }
                if (!this.hideAnnotations && data.annotations) {
                    data.annotations.filter((a: any) => matches(a.name)).forEach((a: any) => {
                        children.push(new FramaCItem(a.name, vscode.TreeItemCollapsibleState.None, "predicate", false, element.resourceUri));
                    });
                }
            }

            const fileWp = this.wpDataByFile.get(fileUri) || [];
            const filteredWp = fileWp.filter(goal => matches(goal.label.toString()));
            if (filteredWp.length > 0) {
                const wpRoot = new FramaCItem(`WP Goals (${filteredWp.length})`, vscode.TreeItemCollapsibleState.Collapsed, "cat_wp", false, element.resourceUri);
                wpRoot.extraData = filteredWp;
                children.push(wpRoot);
            }

            return children.sort((a, b) => a.label.toString().localeCompare(b.label.toString()));
        }

        if (element.contextValue === "cat_wp") {
            return element.extraData || [];
        }

        return [];
    }

    
    private fileHasMatch(uri: string, matches: (n: string) => boolean): boolean {
        const data = this.astData.get(uri);
        const wp = this.wpDataByFile.get(uri) || [];
        
        const matchAst = data ? (
            (data.functions?.some((f: any) => matches(f.name))) || 
            (data.globals?.some((g: any) => matches(g.name))) ||
            (data.types?.some((t: any) => matches(t.name))) ||
            (data.annotations?.some((a: any) => matches(a.name)))
        ) : false;

        const matchWp = wp.some(goal => matches(goal.label.toString()));

        return matchAst || matchWp;
    }
}

export class FramaCItem extends vscode.TreeItem {
    public extraData?: any[]; 

    constructor(
        public readonly label: string,
        public readonly collapsibleState: vscode.TreeItemCollapsibleState,
        public readonly contextValue: string,
        private underlined: boolean = false, 
        public resourceUri?: vscode.Uri,
        public goal_id?: string, 
        public script?: string, 
        public file_id?: string,
        public fct_id?: string, 
        public prop_id?: string, 
        public verdict?: string
    ) {
        super(label, collapsibleState);
        this.assignIcon();
    }

    private assignIcon() {
        if (this.contextValue === "folder") {
            this.iconPath = vscode.ThemeIcon.Folder;
        } else if (this.contextValue === "file") {
            this.iconPath = vscode.ThemeIcon.File;
        } else if (this.contextValue === "function") {
            this.iconPath = new vscode.ThemeIcon("symbol-function");
            if (this.underlined) this.description = "ƒ";
        } else if (this.contextValue === "variable") {
            this.iconPath = new vscode.ThemeIcon("symbol-variable");
        } else if (this.contextValue === "type") {
            this.iconPath = new vscode.ThemeIcon("symbol-type-parameter");
        } else if (this.contextValue === "predicate") {
            this.iconPath = new vscode.ThemeIcon("shield");
        } else if (this.contextValue === "goal") {
            // Icônes de statut de preuve (Passed / Failed / Pending)
            const icon = this.verdict === "passed" ? "pass" : (this.verdict === "failed" ? "error" : "circle-outline");
            const color = this.verdict === "passed" ? "testing.iconPassed" : (this.verdict === "failed" ? "testing.iconFailed" : "testing.iconQueued");
            this.iconPath = new vscode.ThemeIcon(icon, new vscode.ThemeColor(color));
        }
    }
}