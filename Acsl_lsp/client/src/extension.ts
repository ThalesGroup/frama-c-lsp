import * as path from 'path';
import { workspace, ExtensionContext, commands, extensions, window, ViewColumn, TabInputWebview, TextEditor, Uri, languages, Position, Range, env } from 'vscode';
import * as vscode from 'vscode';
import * as fs from 'fs';
import {LanguageClient,	LanguageClientOptions, ServerOptions, TransportKind} from 'vscode-languageclient/node';
import { exec } from 'child_process';

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
		} catch (err) {
			window.showErrorMessage('Failed to run smoke tests: ' + err.message);
			console.error('Error computing smoke tests:', err);
		}
	});

	const ccdoc = commands.registerCommand('ccdoc', async () => {
		try {
			await client.sendNotification('ccdoc');
		} catch (err) {
			window.showErrorMessage('Failed to run ccdoc: ' + err.message);
			console.error('Error computing ccdoc:', err);
		}
	});

	const displayCIL = commands.registerCommand('displayCIL', async () => {
		try {
			const filePath = window.activeTextEditor.document.fileName;
    		const fileName = path.basename(filePath);   // Extract the file name
			const workspacePath = get_workspace ();
			//  const workspacePath = workspace.workspaceFolders[0].uri.fsPath;
			const fileNameOut = workspacePath + "/.frama-c/fc_" + fileName;
			if (!fs.existsSync(fileNameOut)) {
				try {fs.writeFileSync(fileNameOut, 'Task in progress ...')}
				catch (error) {vscode.window.showErrorMessage(`Failed to create the file: ${error.message}`);}
			}
			const fileUri = vscode.Uri.parse(fileNameOut);
			const document = await workspace.openTextDocument(fileUri);
			await languages.setTextDocumentLanguage(document, 'acsl');
			const editor = await window.showTextDocument(document, ViewColumn.One, true);
			await client.sendNotification('displayCIL', filePath);
		} catch (err) {
			window.showErrorMessage('Failed to compute displayCIL: ' + err.message);
			console.error('Error computing displayCIL:', err);
		}
	});

	const displayCIL_noannot = commands.registerCommand('displayCIL_noannot', async () => {
		try {
			const filePath = window.activeTextEditor.document.fileName;
    		const fileName = path.basename(filePath);   // Extract the file name
			const workspacePath = get_workspace ();
			// const workspacePath = workspace.workspaceFolders[0].uri.fsPath;
			const fileNameOut = workspacePath + "/.frama-c/fc_" + fileName;
			if (!fs.existsSync(fileNameOut)) {
				try {fs.writeFileSync(fileNameOut, 'Task in progress ...')}
				catch (error) {vscode.window.showErrorMessage(`Failed to create the file: ${error.message}`);}
			}
			const fileUri = vscode.Uri.parse(fileNameOut);
			const document = await workspace.openTextDocument(fileUri);
			await languages.setTextDocumentLanguage(document, 'acsl');
			const editor = await window.showTextDocument(document, ViewColumn.One, true);
			await client.sendNotification('displayCIL_noannot', filePath);
		} catch (err) {
			window.showErrorMessage('Failed to compute displayCIL_noannot: ' + err.message);
			console.error('Error computing displayCIL_noannot:', err);
		}
	});

	const displayCILProject = commands.registerCommand('displayCILProject', async () => {
		try {
    		const fileName = "project.c";   // Extract the file name
			const workspacePath = get_workspace ();
			// const workspacePath = workspace.workspaceFolders[0].uri.fsPath;
			const fileNameOut = workspacePath + "/.frama-c/fc_" + fileName;
			if (!fs.existsSync(fileNameOut)) {
				try {fs.writeFileSync(fileNameOut, 'Task in progress ...')}
				catch (error) {vscode.window.showErrorMessage(`Failed to create the file: ${error.message}`);}
			}
			const fileUri = vscode.Uri.parse(fileNameOut);
			const document = await workspace.openTextDocument(fileUri);
			await languages.setTextDocumentLanguage(document, 'acsl');
			const editor = await window.showTextDocument(document, ViewColumn.One, true);
			await client.sendNotification('displayCILProject');
		} catch (err) {
			window.showErrorMessage('Failed to compute displayCILProject: ' + err.message);
			console.error('Error computing displayCILProject:', err);
		}
	});

	const displayCILProject_noannot = commands.registerCommand('displayCILProject_noannot', async () => {
		try {
    		const fileName = "project.c";   // Extract the file name
			const workspacePath = get_workspace ();
			// const workspacePath = workspace.workspaceFolders[0].uri.fsPath;
			const fileNameOut = workspacePath + "/.frama-c/fc_" + fileName;
			if (!fs.existsSync(fileNameOut)) {
				try {fs.writeFileSync(fileNameOut, 'Task in progress ...')}
				catch (error) {vscode.window.showErrorMessage(`Failed to create the file: ${error.message}`);}
			}
			const fileUri = vscode.Uri.parse(fileNameOut);
			const document = await workspace.openTextDocument(fileUri);
			await languages.setTextDocumentLanguage(document, 'acsl');
			const editor = await window.showTextDocument(document, ViewColumn.One, true);
			await client.sendNotification('displayCILProject_noannot');
		} catch (err) {
			window.showErrorMessage('Failed to compute displayCILProject_noannot: ' + err.message);
			console.error('Error computing displayCILProject_noannot:', err);
		}
	});

	const computeCG = commands.registerCommand('computeCG', async () => {
		try {
			const filePath = window.activeTextEditor.document.fileName;
			const dirPath = path.dirname(filePath);     // Extract the directory path
    		const fileName = path.basename(filePath);   // Extract the file name
			const extension = path.extname(filePath);  // Get the file extension
			const fileNameBase = fileName.slice(0, -extension.length); // Remove the extension
			const workspacePath = get_workspace ();
			// const workspacePath = workspace.workspaceFolders[0].uri.fsPath;
			const filePathOut = workspacePath + "/.frama-c/fc_" + fileNameBase + ".dot.pdf";
			// const filePathOut = workspacePath + "/.frama-c";
			if (!fs.existsSync(filePathOut)) {
				try {fs.writeFileSync(filePathOut, 'Task in progress ...')}
				catch (error) {vscode.window.showErrorMessage(`Failed to create the file: ${error.message}`);}
			}
			const fileUri = vscode.Uri.parse(filePathOut);
			await vscode.commands.executeCommand('revealInExplorer', fileUri);
			// vscode.window.showTextDocument(fileUri, { preview: false });
			// openDirectoryExternally(filePathOut)

			await client.sendNotification('computeCG', filePath);
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
			const editor = await window.showTextDocument(document, ViewColumn.One, true);

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


	const showPO = commands.registerCommand('showPO', async () => {
		try {
			const selectedItems = wpResultsView.selection;
        	if (selectedItems.length > 0) {
				const selectedItem = selectedItems[0];
				const workspacePath = get_workspace ();
				// const workspacePath = workspace.workspaceFolders[0].uri.fsPath;
				// const uriScheme = vscode.env.remoteName;
				let fileUri: vscode.Uri;
				fileUri = vscode.Uri.parse(`${workspacePath}/.frama-c/${selectedItem.goal_id}.txt`);
				const document = await workspace.openTextDocument(fileUri);
				await languages.setTextDocumentLanguage(document, 'plaintext');
				const editor = await window.showTextDocument(document, ViewColumn.One, true);
			}
			else {vscode.window.showInformationMessage('No item selected');}
		} catch (err) {
			window.showErrorMessage('Failed to fetch and display WP proof obligation: ' + err.message);
			console.error('Error fetching WP proof obligation:', err);
		}
	});
		/*
		try {
			const selectedItems = wpResultsView.selection;
			if (selectedItems.length > 0) {
				const selectedItem = selectedItems[0];
				const file_id = selectedItem.file_id;
				const function_id = selectedItem.function_id;
				const goal_id = selectedItem.goal_id;
				const res = await client.sendRequest('showPO', [file_id, function_id, goal_id]);
				const wpResult = JSON.parse(JSON.stringify(res, null, 1));

				// create a new untitled document in a new tab
				const newUri = Uri.parse('untitled:Proof Obligation');
				const document = await workspace.openTextDocument(newUri);
				await languages.setTextDocumentLanguage(document, 'plaintext');
				const editor = await window.showTextDocument(document, ViewColumn.One, true);

				// delete previous content if any and set the content of the new document
				editor.edit(editBuilder => {
					const start = new Position(0, 0);
					const end = new Position(document.lineCount, 0);
					const fullRange = new Range(start, end);
					editBuilder.delete(fullRange);
					editBuilder.insert(editor.selection.start, wpResult);
				});
				window.showInformationMessage('Proof obligation computed');
			}
			else {vscode.window.showInformationMessage('No item selected');}
		} catch (err) {
			window.showErrorMessage('Failed to fetch and display WP proof obligation: ' + err.message);
			console.error('Error fetching WP proof obligation:', err);
		}
	});
	*/


	const wpResults = new MyTreeDataProvider();
	// window.registerTreeDataProvider('WPPan', wpResults);
	const wpResultsView = window.createTreeView('WPPan', {treeDataProvider: wpResults,
        //id: 'WPPan',           // Unique identifier for the tree view
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
				// const workspacePath = workspace.workspaceFolders[0].uri.fsPath;
				// const uriScheme = vscode.env.remoteName;
				let fileUri: vscode.Uri;
				fileUri = vscode.Uri.parse(`${workspacePath}/${selectedItem.script}`);
				const document = await workspace.openTextDocument(fileUri);
				await languages.setTextDocumentLanguage(document, 'plaintext');
				const editor = await window.showTextDocument(document, ViewColumn.One, true);
			}
			else {vscode.window.showInformationMessage('No item selected');}
		} catch (err) {
			window.showErrorMessage('Failed to fetch and display script: ' + err.message);
			console.error('Error fetching script:', err);
		}
	});

	const runAgain = commands.registerCommand('runAgain', async (item: TreeItem) => {
		try {
			const selectedItems = wpResultsView.selection;
        	if (selectedItems.length > 0) {
				const selectedItem = selectedItems[0];
				const workspacePath = get_workspace ();
				// const workspacePath = workspace.workspaceFolders[0].uri.fsPath;
				const file_name = selectedItem.file_id;
				const function_name = selectedItem.fct_id;
				const property_name = selectedItem.prop_id;
				const proof_timeout = await window.showInputBox({
					placeHolder: 'timeout', // Placeholder text in the input box
					prompt: 'Please specify timeout for provers (c.f. -wp-timeout )', // The prompt message
					validateInput: (input) => {
						if (input.length === 0) {return 'Input cannot be empty!';}
						if (!/^\d+$/.test(input)) {return 'Please enter a valid integer';}
						return null; // Return null to indicate valid input
				}});
				const int_proof_timeout = parseInt(proof_timeout, 10);
				// wpResults.update(["","","",[]]);
				// wpResults.refresh();
				const gui = false;
				const res = await client.sendRequest('provePO', [file_name, function_name, property_name, int_proof_timeout, gui]);
				wpResults.update(JSON.parse(JSON.stringify(res, null, 1)));
				wpResults.refresh();
				window.showInformationMessage('Proof results updated');

			}
			else {vscode.window.showInformationMessage('No item selected');}
		} catch (err) {
			window.showErrorMessage('Failed to fetch and display script: ' + err.message);
			console.error('Error fetching script:', err);
		}
	});

	const runAgainGui = commands.registerCommand('runAgainGui', async (item: TreeItem) => {
		try {
			const selectedItems = wpResultsView.selection;
        	if (selectedItems.length > 0) {
				const selectedItem = selectedItems[0];
				const workspacePath = get_workspace ();
				// const workspacePath = workspace.workspaceFolders[0].uri.fsPath;
				const file_name = selectedItem.file_id;
				const function_name = selectedItem.fct_id;
				const property_name = selectedItem.prop_id;
				const proof_timeout = await window.showInputBox({
					placeHolder: 'timeout', // Placeholder text in the input box
					prompt: 'Please specify timeout for provers (c.f. -wp-timeout )', // The prompt message
					validateInput: (input) => {
						if (input.length === 0) {return 'Input cannot be empty!';}
						if (!/^\d+$/.test(input)) {return 'Please enter a valid integer';}
						return null; // Return null to indicate valid input
				}});
				const int_proof_timeout = parseInt(proof_timeout, 10);
				// wpResults.update(["","","",[]]);
				// wpResults.refresh();
				const gui = true;
				const res = await client.sendRequest('provePO', [file_name, function_name, property_name, int_proof_timeout, gui]);
				wpResults.update(JSON.parse(JSON.stringify(res, null, 1)));
				wpResults.refresh();
				window.showInformationMessage('Proof results updated');

			}
			else {vscode.window.showInformationMessage('No item selected');}
		} catch (err) {
			window.showErrorMessage('Failed to fetch and display script: ' + err.message);
			console.error('Error fetching script:', err);
		}
	});

	const runAgainStrategies = commands.registerCommand('runAgainStrategies', async (item: TreeItem) => {
		try {
			const selectedItems = wpResultsView.selection;
        	if (selectedItems.length > 0) {
				const selectedItem = selectedItems[0];
				const workspacePath = get_workspace ();
				// const workspacePath = workspace.workspaceFolders[0].uri.fsPath;
				const file_name = selectedItem.file_id;
				const function_name = selectedItem.fct_id;
				const property_name = selectedItem.prop_id;
				const proof_timeout = await window.showInputBox({
					placeHolder: 'timeout', // Placeholder text in the input box
					prompt: 'Please specify timeout for provers (c.f. -wp-timeout )', // The prompt message
					validateInput: (input) => {
						if (input.length === 0) {return 'Input cannot be empty!';}
						if (!/^\d+$/.test(input)) {return 'Please enter a valid integer';}
						return null; // Return null to indicate valid input
				}});
				const int_proof_timeout = parseInt(proof_timeout, 10);
				// wpResults.update(["","","",[]]);
				// wpResults.refresh();
				const gui = false;
				const res = await client.sendRequest('provePOStrategies', [file_name, function_name, property_name, int_proof_timeout, gui]);
				wpResults.update(JSON.parse(JSON.stringify(res, null, 1)));
				wpResults.refresh();
				window.showInformationMessage('Proof results updated');

			}
			else {vscode.window.showInformationMessage('No item selected');}
		} catch (err) {
			window.showErrorMessage('Failed to fetch and display script: ' + err.message);
			console.error('Error fetching script:', err);
		}
	});

	const runAgainStrategiesGui = commands.registerCommand('runAgainStrategiesGui', async (item: TreeItem) => {
		try {
			const selectedItems = wpResultsView.selection;
        	if (selectedItems.length > 0) {
				const selectedItem = selectedItems[0];
				const workspacePath = get_workspace ();
				// const workspacePath = workspace.workspaceFolders[0].uri.fsPath;
				const file_name = selectedItem.file_id;
				const function_name = selectedItem.fct_id;
				const property_name = selectedItem.prop_id;
				const proof_timeout = await window.showInputBox({
					placeHolder: 'timeout', // Placeholder text in the input box
					prompt: 'Please specify timeout for provers (c.f. -wp-timeout )', // The prompt message
					validateInput: (input) => {
						if (input.length === 0) {return 'Input cannot be empty!';}
						if (!/^\d+$/.test(input)) {return 'Please enter a valid integer';}
						return null; // Return null to indicate valid input
				}});
				const int_proof_timeout = parseInt(proof_timeout, 10);
				// wpResults.update(["","","",[]]);
				// wpResults.refresh();
				const gui = true;
				const res = await client.sendRequest('provePOStrategies', [file_name, function_name, property_name, int_proof_timeout, gui]);
				wpResults.update(JSON.parse(JSON.stringify(res, null, 1)));
				wpResults.refresh();
				window.showInformationMessage('Proof results updated');

			}
			else {vscode.window.showInformationMessage('No item selected');}
		} catch (err) {
			window.showErrorMessage('Failed to fetch and display script: ' + err.message);
			console.error('Error fetching script:', err);
		}
	});

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
			// wpResults.update(["","","",[]]);
			// wpResults.refresh();
			const gui = false;
            const res = await client.sendRequest('provePO', [window.activeTextEditor.document.fileName, function_name, property_name, int_proof_timeout, gui]);
			wpResults.update(JSON.parse(JSON.stringify(res, null, 1)));
			wpResults.refresh();
			window.showInformationMessage('Proof results updated');
        }
        catch (err) {
            window.showErrorMessage('Failed to fetch and display WP proof: ' + err.message);
            console.error('Error fetching WP proof:', err);
        }
    });

	const provePOGUI = commands.registerCommand('provePOGUI', async () => {
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
			// wpResults.update(["","","",[]]);
			// wpResults.refresh();
			const gui = true;
            const res = await client.sendRequest('provePO', [window.activeTextEditor.document.fileName, function_name, property_name, int_proof_timeout, gui]);
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
			// wpResults.update(["","","",[]]);
			// wpResults.refresh();
			const gui = false;
            const res = await client.sendRequest('provePOStrategies', [window.activeTextEditor.document.fileName, function_name, property_name, int_proof_timeout, gui]);
			wpResults.update(JSON.parse(JSON.stringify(res, null, 1)));
			wpResults.refresh();
			window.showInformationMessage('Proof results updated');
        }
        catch (err) {
            window.showErrorMessage('Failed to fetch and display WP proof: ' + err.message);
            console.error('Error fetching WP proof:', err);
        }
    });

    const provePOStrategiesGUI = commands.registerCommand('provePOStrategiesGUI', async () => {
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
			// wpResults.update(["","","",[]]);
			// wpResults.refresh();
			const gui = true;
            const res = await client.sendRequest('provePOStrategies', [window.activeTextEditor.document.fileName, function_name, property_name, int_proof_timeout, gui]);
			wpResults.update(JSON.parse(JSON.stringify(res, null, 1)));
			wpResults.refresh();
			window.showInformationMessage('Proof results updated');
        }
        catch (err) {
            window.showErrorMessage('Failed to fetch and display WP proof: ' + err.message);
            console.error('Error fetching WP proof:', err);
        }
    });

    const stop = commands.registerCommand('stop', async () => {
		try {
            const res = await client.sendRequest('stop');
			window.showInformationMessage('Stopped processes');
        }
        catch (err) {
            window.showErrorMessage('Failed to stop Frama-C: ' + err.message);
            console.error('Error stropping Frama-C:', err);
        }
    });


	const showLocalMetrics = commands.registerCommand('showLocalMetrics', async () => {
		try {
			const filePath = window.activeTextEditor.document.fileName;
			const file_name = "fc_metrics.txt";
			const workspacePath = get_workspace ();
			// const workspacePath = workspace.workspaceFolders[0].uri.fsPath;
			const filePath_metrics = workspacePath + "/.frama-c/" + file_name;
			if (!fs.existsSync(filePath_metrics)) {
				try {fs.writeFileSync(filePath_metrics, 'Task in progress ...')}
				catch (error) {vscode.window.showErrorMessage(`Failed to create the file: ${error.message}`);}
			}
			const dirPath = path.dirname(filePath);     // Extract the directory path
			const fileUri = vscode.Uri.parse(filePath_metrics);
			const document = await workspace.openTextDocument(fileUri);
			await languages.setTextDocumentLanguage(document, 'plaintext');
			const editor = await window.showTextDocument(document, ViewColumn.One, true);
			client.sendNotification('showLocalMetrics', filePath);
		} catch (err) {
			window.showErrorMessage('Failed to get local metrics: ' + err.message);
			console.error('Error getting local metrics:', err);
		}
	});

	const showGlobalMetrics = commands.registerCommand('showGlobalMetrics', async () => {
		try {
			const file_name = "fc_metrics.txt";
			const workspacePath = get_workspace ();
			// const workspacePath = workspace.workspaceFolders[0].uri.fsPath;
			const filePath = workspacePath + "/.frama-c/" + file_name;
			if (!fs.existsSync(filePath)) {
				try {fs.writeFileSync(filePath, 'Task in progress ...')}
				catch (error) {vscode.window.showErrorMessage(`Failed to create the file: ${error.message}`);}
			}
			const fileUri = vscode.Uri.parse(filePath);
			const document = await workspace.openTextDocument(fileUri);
			await languages.setTextDocumentLanguage(document, 'plaintext');
			const editor = await window.showTextDocument(document, ViewColumn.One, true);
			client.sendNotification('showGlobalMetrics');
		} catch (err) {
			window.showErrorMessage('Failed to get global metrics: ' + err.message);
			console.error('Error getting global metrics:', err);
		}
	});

	context.subscriptions.push(smokeTests, ccdoc, displayCIL, displayCIL_noannot, displayCILProject, displayCILProject_noannot, computeCG, showPOVC, showPO, runAgain, runAgainGui, runAgainStrategies, runAgainStrategiesGui, provePO, provePOGUI, provePOStrategies, provePOStrategiesGUI, showGlobalMetrics, showLocalMetrics);

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
				// const workspacePath = workspace.workspaceFolders[0].uri.fsPath;
				t_item.command = {
					command: 'vscode.open',
					arguments: [vscode.Uri.parse(workspacePath + "/" + file_id + "#L" + line)]
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



function get_workspace(){
	if (vscode.workspace.workspaceFolders && vscode.workspace.workspaceFolders.length > 0) {
		const workspacePath = workspace.workspaceFolders[0].uri.fsPath;
		create_frama_c_folder(workspacePath);
		return workspacePath;
	} else {
		const currentFolder = process.cwd();
		create_frama_c_folder(currentFolder);
		return currentFolder

/*		const editor = vscode.window.activeTextEditor;
		if (editor) {
			const fileUri = editor.document.uri;
			const filePath = fileUri.fsPath;
			const dirname = path.dirname(filePath);
			const workspaceDir = {uri: vscode.Uri.file(dirname)};
			vscode.workspace.updateWorkspaceFolders(workspace.workspaceFolders ? workspace.workspaceFolders.length : 0, null, workspaceDir);
			const workspacePath = workspace.workspaceFolders[0].uri.fsPath;
			create_frama_c_folder(workspacePath);
			return workspaceDir;
		} else {
			const dirname = path.resolve(__dirname);
			const workspaceDir = {uri: vscode.Uri.file(dirname)};
			vscode.workspace.updateWorkspaceFolders(workspace.workspaceFolders ? workspace.workspaceFolders.length : 0, null, workspaceDir);
			const workspacePath = workspace.workspaceFolders[0].uri.fsPath;
			create_frama_c_folder(workspacePath);
			return workspaceDir;
		} */
	}
}


async function create_frama_c_folder(workspace){
	try {
		await fs.promises.mkdir(workspace + "/.frama-c", {recursive: true})
	} catch(err) {
	}
}

// Function to open the directory in the system's file explorer
function openDirectoryExternally(folderPath: string) {
    const platform = process.platform;

    if (platform === 'win32') {
        exec(`explorer "${folderPath}"`);
    } else if (platform === 'darwin') {
        exec(`open "${folderPath}"`);
    } else if (platform === 'linux') {
        exec(`explorer.exe "${folderPath}"`);
    } else {
        console.error('Unsupported platform');
    }
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
