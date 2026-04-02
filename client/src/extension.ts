import * as path from 'path';
import * as vscode from 'vscode';
import * as fs from 'fs';
import { workspace, ExtensionContext, commands, window, ViewColumn, Uri, languages, Position, Range } from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient/node';
import { ProjectImporter } from './projectImporter';
import { exec } from 'child_process';

let client: LanguageClient;
let framaCProvider: FramaCProvider; 
let wpDataProvider: MyTreeDataProvider; 
let wpResultsView: vscode.TreeView<TreeItem>;

const createGutterIcon = (color: string) => {
    const svg = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16"><circle cx="8" cy="8" r="5" fill="${color}"/></svg>`;
    const encoded = Buffer.from(svg).toString('base64');
    return vscode.Uri.parse(`data:image/svg+xml;base64,${encoded}`);
};

const passedDecoration = vscode.window.createTextEditorDecorationType({
    gutterIconPath: createGutterIcon('mediumseagreen'),
    backgroundColor: 'rgba(60, 179, 113, 0.2)' // Fond vert 
});

const failedDecoration = vscode.window.createTextEditorDecorationType({
    gutterIconPath: createGutterIcon('crimson'),
    backgroundColor: 'rgba(220, 20, 60, 0.2)' // Fond rouge
});

const unknownDecoration = vscode.window.createTextEditorDecorationType({
    gutterIconPath: createGutterIcon('darkorange'),
    backgroundColor: 'rgba(255, 140, 0, 0.2)' // Fond orange
});

let lastWpData: string[] = [];
function updateDecorations() {

    const editors = vscode.window.visibleTextEditors;

    editors.forEach(editor => {
        const passedRanges: vscode.Range[] = [];
        const failedRanges: vscode.Range[] = [];
        const unknownRanges: vscode.Range[] = [];

        const currentFileName = path.basename(editor.document.fileName);

        lastWpData.forEach(item => {
            const p = item.trim().split(":");
            if (p.length < 4) return;

            const status = p[0].trim().toLowerCase();
            const itemFileName = path.basename(p[2].trim());
            const line = parseInt(p[3].trim(), 10) - 1;

            if (itemFileName === currentFileName && !isNaN(line)) {
                const range = new vscode.Range(line, 0, line, 0);
                if (status === "passed") passedRanges.push(range);
                else if (status === "failed") failedRanges.push(range);
                else unknownRanges.push(range);
            }
        });

       

        editor.setDecorations(passedDecoration, passedRanges);
        editor.setDecorations(failedDecoration, failedRanges);
        editor.setDecorations(unknownDecoration, unknownRanges);
    });
}

export function activate(context: ExtensionContext) {
	// The server is implemented in OCaml
    const config = vscode.workspace.getConfiguration();
    const serverPort = config.get<number>('kernel.serverPort') || 8005;
    const wrapperPort = config.get<number>('kernel.wrapperPort') || (serverPort + 1);
    const serverModuleRun = context.asAbsolutePath(path.join('run.sh')) + " " + serverPort + " " + wrapperPort;
    const serverModuleDebug = context.asAbsolutePath(path.join('run.sh')) + " " + serverPort + " " + wrapperPort;
    const importer = new ProjectImporter();

    // 2. Project Importer Command & Status Bar
    let importDisposable = vscode.commands.registerCommand('acsl.importProjectConfig', async () => {
        await importer.showProjectSelector();
    });
    context.subscriptions.push(importDisposable);

    const importStatusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100);
    importStatusBarItem.command = 'acsl.importProjectConfig'; 
    importStatusBarItem.text = '$(project) JCAT: Import IDE';
    importStatusBarItem.tooltip = 'Click to Import Config File';
    importStatusBarItem.show();
    context.subscriptions.push(importStatusBarItem);
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
    // 3. TreeViews Initialization (Sidebar + Bottom Panel)
    wpDataProvider = new MyTreeDataProvider();
    wpResultsView = vscode.window.createTreeView('wpGoalsView', { 
        treeDataProvider: wpDataProvider,
        showCollapseAll: true,
        canSelectMany: true 
    });

    framaCProvider = new FramaCProvider();
    vscode.window.createTreeView('framaCExplorer', {
        treeDataProvider: framaCProvider
    });



    

    // 5. Handle Server Notifications (Redirect WP Goals to the bottom panel)
    client.onNotification("custom/getWPResponse", (data) => {
        wpDataProvider.update(data);
        wpDataProvider.refresh();
        commands.executeCommand('wpGoalsView.focus');
    });
	vscode.window.onDidChangeActiveTextEditor(editor => {
        updateDecorations();
    }, null, context.subscriptions);

    // 6. Register all Frama-C Commands
    registerAllExtensionCommands(context);

    client.start();
}

/**
 * Register all extension commands without touching original logic or error handling
 */
function registerAllExtensionCommands(context: ExtensionContext) {
    context.subscriptions.push(
        // --- Sidebar Exploration & Toggles ---
        commands.registerCommand('framaCExplorer.search', async () => {
            const val = await window.showInputBox({ prompt: "Filtrer l'arborescence..." });
            framaCProvider.setFilter(val || "");
        }),
        commands.registerCommand('framaC.toggleFunctions', () => framaCProvider.toggleFunctions()),
        commands.registerCommand('framaC.toggleVariables', () => framaCProvider.toggleVariables()),
        commands.registerCommand('framaC.toggleTypes', () => framaCProvider.toggleTypes()),
        commands.registerCommand('framaC.toggleAnnotations', () => framaCProvider.toggleAnnotations()),

        // --- Analysis & AST Commands ---
        commands.registerCommand('acsl-lsp.debugAST', async () => {
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
            } catch (error) { console.error(error); }
        }),
	
// Command to handle clicking on any Sidebar element
commands.registerCommand('framaC.openAndDetail', async (item: FramaCItem) => {
    if (!item.resourceUri) return;

    try {    // 1. Open and show the file

   
        const document = await workspace.openTextDocument(item.resourceUri);
        const editor = await window.showTextDocument(document);
        
         // 2. Search for the label in the text to jump to the right line

        if (item.line !== undefined) {
            const pos = new Position(item.line - 1, 0); // Frama-C (1-based) -> VSCode (0-based)
            editor.selection = new vscode.Selection(pos, pos);
            editor.revealRange(new Range(pos, pos), vscode.TextEditorRevealType.InCenter);
        } else {
          
            const text = document.getText();
            const lines = text.split('\n');
            let lineIndex = lines.findIndex(l => l.includes(item.label));

            if (lineIndex !== -1) {
                const pos = new Position(lineIndex, 0);
                editor.selection = new vscode.Selection(pos, pos);
                editor.revealRange(new vscode.Range(pos, pos), vscode.TextEditorRevealType.InCenter);
            }
        }

           // 3. Specific logic for functions: fetch requires/ensures from server

        if (item.contextValue === "function") {
            const response: any = await client.sendRequest("custom/getFunctionDetails", { 
                uri: item.resourceUri.toString(),
                functionName: item.label 
            });
            if (response) {
                framaCProvider.updateFunctionDetails(item.label, response);
            }
        }
    } catch (e) {
            console.error("Error fetching function details:", e);
    }
}),

        commands.registerCommand('smokeTests', async () => {
            try {
                const editor = window.activeTextEditor;
                if (!editor) { window.showErrorMessage('No active editor found.'); return; }
                await client.sendNotification('smokeTests', editor.document.fileName);
            } catch (err) {
                const msg = err instanceof Error ? err.message : String(err);
                window.showErrorMessage('Failed to run smoke tests: ' + msg);
            }
        }),

        commands.registerCommand('ccdoc', async () => {
            try { await client.sendNotification('ccdoc'); }
            catch (err) {
                const msg = err instanceof Error ? err.message : String(err);
                window.showErrorMessage('Failed to run ccdoc: ' + msg);
            }
        }),

        // --- CIL Display Commands ---
        commands.registerCommand('displayCIL', async () => {
            try {
                const editor = window.activeTextEditor;
                if (!editor) { window.showErrorMessage('No active editor found.'); return; }
                const filePath = editor.document.fileName;
                await create_file(path.basename(filePath), 'c');
                await client.sendNotification('displayCIL', filePath);
            } catch (err) {
                const msg = err instanceof Error ? err.message : String(err);
                window.showErrorMessage('Failed to compute displayCIL: ' + msg);
            }
        }),

        commands.registerCommand('displayCIL_noannot', async () => {
            try {
                const editor = window.activeTextEditor;
                if (!editor) { window.showErrorMessage('No active editor found.'); return; }
                const filePath = editor.document.fileName;
                await create_file(path.basename(filePath), 'c');
                await client.sendNotification('displayCIL_noannot', filePath);
            } catch (err) {
                const msg = err instanceof Error ? err.message : String(err);
                window.showErrorMessage('Failed to compute displayCIL_noannot: ' + msg);
            }
        }),

        commands.registerCommand('displayCILProject', async () => {
            try {
                await create_file("project.c", 'c');
                await client.sendNotification('displayCILProject');
            } catch (err) {
                const msg = err instanceof Error ? err.message : String(err);
                window.showErrorMessage('Failed to compute displayCILProject: ' + msg);
            }
        }),

        commands.registerCommand('displayCILProject_noannot', async () => {
            try {
                await create_file("project.c", 'c');
                await client.sendNotification('displayCILProject_noannot');
            } catch (err) {
                const msg = err instanceof Error ? err.message : String(err);
                window.showErrorMessage('Failed to compute displayCILProject_noannot: ' + msg);
            }
        }),

        // --- Metrics & Graphs ---
        commands.registerCommand('computeCG', async () => {
            try {
                const editor = window.activeTextEditor;
                if (!editor) { window.showErrorMessage('No active editor found.'); return; }
                const filePath = editor.document.fileName;
                const workspacePath = get_workspace();
                const fileNameBase = path.basename(filePath, path.extname(filePath));
                const filePathOut = path.join(workspacePath, ".frama-c", "fc_" + fileNameBase + ".dot.pdf");
                if (!fs.existsSync(filePathOut)) {
                    try { fs.writeFileSync(filePathOut, 'Task in progress ...'); }
                    catch (error) { vscode.window.showErrorMessage(`Failed to create file: ${error.message}`); }
                }
                await vscode.commands.executeCommand('revealInExplorer', vscode.Uri.file(filePathOut));
                await client.sendNotification('computeCG', filePath);
            } catch (err) {
                const msg = err instanceof Error ? err.message : String(err);
                window.showErrorMessage('Failed to compute callgraph: ' + msg);
            }
        }),

        commands.registerCommand('showLocalMetrics', async () => {
            try {
                const editor = window.activeTextEditor;
                if (!editor) { window.showErrorMessage('No active editor found.'); return; }
                await create_file("metrics.txt", 'plaintext');
                client.sendNotification('showLocalMetrics', editor.document.fileName);
            } catch (err) {
                const msg = err instanceof Error ? err.message : String(err);
                window.showErrorMessage('Failed to get local metrics: ' + msg);
            }
        }),

        commands.registerCommand('showGlobalMetrics', async () => {
            try {
                await create_file("metrics.txt", 'plaintext');
                client.sendNotification('showGlobalMetrics');
            } catch (err) {
                const msg = err instanceof Error ? err.message : String(err);
                window.showErrorMessage('Failed to get global metrics: ' + msg);
            }
        }),

        commands.registerCommand('computeIncludeGraph', async () => {
            await generateRecursiveIncludeGraph();
			
        }),
        // --- Proof Commands ---
        commands.registerCommand('showPOVC', async () => {
            try {
                const editor = window.activeTextEditor;
                if (!editor) { window.showErrorMessage('No active editor found.'); return; }
                const res = await client.sendRequest('showPOVC', [editor.document.fileName, editor.selection.active]);
                const wpResult = JSON.parse(JSON.stringify(res, null, 1));
                const doc = await workspace.openTextDocument({ content: wpResult, language: 'plaintext' });
                await window.showTextDocument(doc, ViewColumn.One, true);
            } catch (err) {
                const msg = err instanceof Error ? err.message : String(err);
                window.showErrorMessage('Failed to display WP PO: ' + msg);
            }
        }),

        commands.registerCommand('showPO', async () => {
            try {
                const selected = wpResultsView.selection[0];
                if (selected && selected.goal_id) {
                    const fileUri = vscode.Uri.file(path.join(get_workspace(), ".frama-c", `${selected.goal_id}.txt`));
                    const doc = await workspace.openTextDocument(fileUri);
                    await window.showTextDocument(doc, ViewColumn.One, true);
                } else { window.showInformationMessage('No item selected'); }
            } catch (err) {
                window.showErrorMessage('Failed to display PO: ' + err);
            }
        }),

        commands.registerCommand('showScript', async () => {
            try {
                const selected = wpResultsView.selection[0];
                if (selected && selected.script) {
                    const fileUri = vscode.Uri.file(path.join(get_workspace(), selected.script));
                    const doc = await workspace.openTextDocument(fileUri);
                    await window.showTextDocument(doc, ViewColumn.One, true);
                } else { window.showInformationMessage('No item selected'); }
            } catch (err) {
                window.showErrorMessage('Failed to fetch script: ' + err);
            }
        }),

        commands.registerCommand('provePO', async () => {
            try {
                const args = await get_proof_args(false);
                const res = await client.sendRequest('provePO', args);
                wpDataProvider.update(res);
                wpDataProvider.refresh();
                window.showInformationMessage('Proof results updated');
            } catch (err) {
                window.showErrorMessage('Failed to run WP proof: ' + err);
            }
        }),

        commands.registerCommand('provePOStrategies', async () => {
            try {
                const args = await get_proof_args(false);
                const res = await client.sendRequest('provePOStrategies', args);
                wpDataProvider.update(res);
                wpDataProvider.refresh();
                window.showInformationMessage('Proof results updated');
            } catch (err) {
                window.showErrorMessage('Failed to run Strategies proof: ' + err);
            }
        }),

        commands.registerCommand('provePOGUI', async () => {
            try {
                const args = await get_proof_args(true);
                await client.sendRequest('provePO', args);
                window.showInformationMessage('frama-c-gui closed');
            } catch (err) { window.showErrorMessage('Failed to run WP GUI proof: ' + err); }
        }),

        commands.registerCommand('provePOStrategiesGUI', async () => {
            try {
                const args = await get_proof_args(true);
                await client.sendRequest('provePOStrategies', args);
                window.showInformationMessage('frama-c-gui closed');
            } catch (err) { window.showErrorMessage('Failed to run Strategies GUI proof: ' + err); }
        }),

        // --- Run Again (Bottom Panel selection) ---
        commands.registerCommand('runAgain', async () => {
            const selected = wpResultsView.selection[0];
            if (selected) {
                try {
                    const args = await get_args_from_item(selected, false);
                    const res = await client.sendRequest('provePO', args);
                    wpDataProvider.update(res);
                    wpDataProvider.refresh();
                    window.showInformationMessage('Proof results updated');
                } catch (err) { window.showErrorMessage('Error: ' + err); }
            }
        }),

        commands.registerCommand('runAgainGui', async () => {
            const selected = wpResultsView.selection[0];
            if (selected) {
                try {
                    const args = await get_args_from_item(selected, true);
                    const res = await client.sendRequest('provePO', args);
                    wpDataProvider.update(res);
                    wpDataProvider.refresh();
                    window.showInformationMessage('Proof results updated');
                } catch (err) { window.showErrorMessage('Error: ' + err); }
            }
        }),

        commands.registerCommand('runAgainStrategies', async () => {
            const selected = wpResultsView.selection[0];
            if (selected) {
                try {
                    const args = await get_args_from_item(selected, false);
                    const res = await client.sendRequest('provePOStrategies', args);
                    wpDataProvider.update(res);
                    wpDataProvider.refresh();
                    window.showInformationMessage('Proof results updated');
                } catch (err) { window.showErrorMessage('Error: ' + err); }
            }
        }),

        commands.registerCommand('runAgainStrategiesGui', async () => {
            const selected = wpResultsView.selection[0];
            if (selected) {
                try {
                    const args = await get_args_from_item(selected, true);
                    const res = await client.sendRequest('provePOStrategies', args);
                    wpDataProvider.update(res);
                    wpDataProvider.refresh();
                    window.showInformationMessage('Proof results updated');
                } catch (err) { window.showErrorMessage('Error: ' + err); }
            }
        }),

        // --- Specialized Cursor Commands ---
        commands.registerCommand('provePOCursor', async () => {
            try {
                const editor = window.activeTextEditor;
                if (!editor) return;
                const timeout = await window.showInputBox({ prompt: `Launch Auto-Proof (Timeout in s)`, value: '10' });
                if (!timeout) return;
                const args = [editor.document.fileName, editor.selection.active.line, parseInt(timeout, 10)];
                const res: any = await client.sendRequest('custom/proveAuto', args);
                if (res && Array.isArray(res)) {
                    window.showInformationMessage(`Targeting '${res[2]}' in '${res[1]}'.`);
                    wpDataProvider.update(res);
                    wpDataProvider.refresh();
                } else { window.showWarningMessage("No context detected."); }
            } catch (err) { window.showErrorMessage('Error: ' + err); }
        }),

        commands.registerCommand('stop', async () => {
            try { await client.sendRequest('stop'); window.showInformationMessage('Stopped processes'); }
            catch (err) { window.showErrorMessage('Failed to stop: ' + err); }
        })
    );
}

// --- DATA PROVIDER: BOTTOM PANEL (GOALS) ---
class MyTreeDataProvider implements vscode.TreeDataProvider<TreeItem> {
    private _onDidChangeTreeData = new vscode.EventEmitter<TreeItem | undefined | null | void>();
    readonly onDidChangeTreeData = this._onDidChangeTreeData.event;
    private data: TreeItem[] = [new TreeItem("No goals !")];

    update(data: any) {
        if (Array.isArray(data)) {
            let [filename_id, fct_id, prop_id, jsonData] = data;
			if (jsonData && Array.isArray(jsonData)) {
                lastWpData = jsonData;
            } else {
                lastWpData = [];
            }
            updateDecorations();
            if (!jsonData || jsonData.length === 0) {
                this.data = [new TreeItem("No goals !")];
            } else {
                this.data = jsonData.map((item: string) => {
                    const p = item.trim().split(":");
                    const t_item = new TreeItem(p[0].trim(), `${p[1]} ${p[4]}`, p[2], p[6], p[1], p[5], filename_id, fct_id, prop_id, 'itemContext');
                    const workspacePath = get_workspace();
                    t_item.command = {
                        command: 'vscode.open',
                        title: 'Open File',
                        arguments: [vscode.Uri.file(path.join(workspacePath, p[2])).with({ fragment: `L${p[3]}` })]
                    };
                    return t_item;
                });
            }
        }
    }
    refresh() { this._onDidChangeTreeData.fire(); }
    getTreeItem(element: TreeItem) { return element; }
    getChildren(element?: TreeItem) { return element ? [] : this.data; }
}

class TreeItem extends vscode.TreeItem {
    constructor(label: string, description?: string, public file_id?: string, public function_id?: string, public goal_id?: string, public script?: string, public filename_id?: string, public fct_id?: string, public prop_id?: string, context?: string) {
        super(label, vscode.TreeItemCollapsibleState.None);
        this.description = description;
        this.contextValue = context;
        const passed = label.toLowerCase() === "passed";
        this.iconPath = new vscode.ThemeIcon(passed ? 'check' : 'error', new vscode.ThemeColor(passed ? "testing.iconPassed" : "testing.iconFailed"));
    }
}

// --- DATA PROVIDER: SIDEBAR (EXPLORER/AST) ---
export class FramaCProvider implements vscode.TreeDataProvider<FramaCItem> {
    private _onDidChangeTreeData = new vscode.EventEmitter<FramaCItem | undefined | void>();
    readonly onDidChangeTreeData = this._onDidChangeTreeData.event;

    private astData: Map<string, any> = new Map();
    private functionDetails: Map<string, any[]> = new Map(); 
    private searchQuery = "";
    private hideVariables = false; 
    private hideFunctions = false;
    private hideTypes = false; 
    private hideAnnotations = false;

    refresh(): void {
        this._onDidChangeTreeData.fire();
    }

    // --- Configuration methods ---
    setFilter(q: string) { this.searchQuery = q.toLowerCase(); this.refresh(); }
    toggleFunctions() { this.hideFunctions = !this.hideFunctions; this.refresh(); }
    toggleVariables() { this.hideVariables = !this.hideVariables; this.refresh(); }
    toggleTypes() { this.hideTypes = !this.hideTypes; this.refresh(); }
    toggleAnnotations() { this.hideAnnotations = !this.hideAnnotations; this.refresh(); }

    // --- Data update methods ---
    updateAST(uri: string, data: any) {
        this.astData.set(uri, data);
        this.refresh();
    }

    updateFunctionDetails(funcName: string, details: any[]) {
        this.functionDetails.set(funcName, details);
        this.refresh();
    }

    getTreeItem(element: FramaCItem): vscode.TreeItem {
        return element;
    }

    async getChildren(element?: FramaCItem): Promise<FramaCItem[]> {
        const matches = (name: string) => name.toLowerCase().includes(this.searchQuery);
        const rootPath = workspace.workspaceFolders ? workspace.workspaceFolders[0].uri.fsPath : process.cwd();

        // CASE 1: ROOT or FOLDER - Browse file system
        if (!element || element.contextValue === "folder") {
            const currentDir = element ? element.resourceUri!.fsPath : rootPath;
            try {
                const entries = await fs.promises.readdir(currentDir, { withFileTypes: true });
                return entries
                    .filter(e => !e.name.startsWith('.') && (e.isDirectory() || e.name.endsWith('.c') || e.name.endsWith('.h')))
                    .map(e => new FramaCItem(
                        e.name, 
                        vscode.TreeItemCollapsibleState.Collapsed, 
                        e.isDirectory() ? "folder" : "file", 
                        vscode.Uri.file(path.join(currentDir, e.name))
                    ));
            } catch (e) { return []; }
        }

        // CASE 2: FILE - Show AST elements (functions, variables, types)
        if (element.contextValue === "file") {
            const data = this.astData.get(element.resourceUri!.toString());
            if (!data) return [];
            
            let children: FramaCItem[] = [];

            // Functions
            if (!this.hideFunctions && data.functions) {
                data.functions.filter((f: any) => matches(f.name)).forEach((f: any) => 
                    children.push(new FramaCItem(f.name, vscode.TreeItemCollapsibleState.Collapsed, "function", element.resourceUri)));
            }

            // Variables
            if (!this.hideVariables && data.globals) {
                data.globals.filter((g: any) => matches(g.name)).forEach((g: any) => 
                    children.push(new FramaCItem(g.name, vscode.TreeItemCollapsibleState.None, "variable", element.resourceUri)));
            }

            // Types
            if (!this.hideTypes && data.types) {
                data.types.filter((t: any) => matches(t.name)).forEach((t: any) => 
                    children.push(new FramaCItem(t.name, vscode.TreeItemCollapsibleState.None, "type", element.resourceUri)));
            }

            // Predicates/Annotations
            if (!this.hideAnnotations && data.annotations) {
                data.annotations.filter((a: any) => matches(a.name)).forEach((a: any) => 
                    children.push(new FramaCItem(a.name, vscode.TreeItemCollapsibleState.None, "predicate", element.resourceUri)));
            }

            return children;
        }

        // CASE 3: FUNCTION - Show its requires/ensures (ACSL Details)
        if (element.contextValue === "function") {
    const details = this.functionDetails.get(element.label);
    if (details) {
        return details.map(d => {
            const targetUri = d.file ? vscode.Uri.file(d.file) : element.resourceUri;
            
            return new FramaCItem(
                d.name, 
                vscode.TreeItemCollapsibleState.None, 
                d.type === "requires" ? "requires" : "ensures", 
                targetUri, 
                d.line     
            );
        });
    }
}
        return [];
    }
}

export class FramaCItem extends vscode.TreeItem {
    constructor(
        public readonly label: string, 
        public readonly collapsibleState: vscode.TreeItemCollapsibleState, 
        public readonly contextValue: string, 
		
        public resourceUri?: vscode.Uri,
		public readonly line?: number
    ) {
        super(label, collapsibleState);
        
        // Assign the click command to any item that is not a folder
        if (contextValue !== "folder") {
            this.command = { 
                command: 'framaC.openAndDetail', 
                title: 'Open Definition', 
                arguments: [this] 
            };
        }

        // Icon logic
        if (contextValue === "folder") this.iconPath = vscode.ThemeIcon.Folder;
        else if (contextValue === "file") this.iconPath = vscode.ThemeIcon.File;
        else if (contextValue === "function") this.iconPath = new vscode.ThemeIcon("symbol-function");
        else if (contextValue === "variable") this.iconPath = new vscode.ThemeIcon("symbol-variable");
        else if (contextValue === "type") this.iconPath = new vscode.ThemeIcon("symbol-type-parameter");
        else if (contextValue === "requires") this.iconPath = new vscode.ThemeIcon("shield-check", new vscode.ThemeColor("charts.blue"));
        else if (contextValue === "ensures") this.iconPath = new vscode.ThemeIcon("shield-lock", new vscode.ThemeColor("charts.green"));
    }
}

// --- UTILS ---
function get_workspace() { return workspace.workspaceFolders ? workspace.workspaceFolders[0].uri.fsPath : process.cwd(); }

async function create_file(fileName:string, type:string){
    const workspacePath = get_workspace();
    const fileNameOut = path.join(workspacePath, ".frama-c", `fc_${fileName}`);
    if (!fs.existsSync(fileNameOut)) {
        if (!fs.existsSync(path.dirname(fileNameOut))) fs.mkdirSync(path.dirname(fileNameOut), {recursive: true});
        fs.writeFileSync(fileNameOut, 'Task in progress ...');
    }
    const doc = await workspace.openTextDocument(vscode.Uri.file(fileNameOut));
    await languages.setTextDocumentLanguage(doc, type);
    await window.showTextDocument(doc, ViewColumn.One, true);
}

async function get_args_from_item(item: TreeItem, gui: boolean) {
    const t = await window.showInputBox({ prompt: 'Timeout', value: '10' });
    return [item.filename_id, item.fct_id, item.prop_id, parseInt(t || "10"), gui];
}

async function get_proof_args(gui: boolean) {
    const f = await window.showInputBox({ prompt: 'Function (@all)' });
    const p = await window.showInputBox({ prompt: 'Property (@all)' });
    const t = await window.showInputBox({ prompt: 'Timeout', value: '10' });
    const editor = window.activeTextEditor;
    if (!editor || !f || !p) return null;
    return [editor.document.fileName, f, p, parseInt(t || "10"), gui];
}
async function generateRecursiveIncludeGraph() {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
        return;
    }

    const workspacePath = get_workspace();
    const startFilePath = editor.document.fileName;
    const startFileName = path.basename(startFilePath);

    try {
        const allLocalFiles = await vscode.workspace.findFiles('**/*.{c,h}', '**/{node_modules,.git,build,obj}/**');
        const fileMap = new Map<string, string>();
        for (const file of allLocalFiles) {
            fileMap.set(path.basename(file.fsPath), file.fsPath);
        }

        const visited = new Set<string>(); 
        const edges = new Set<string>();   
        const systemNodes = new Set<string>();
        const nodesToProcess: string[] = [startFilePath]; 
        const includeRegex = /^\s*#\s*include\s*(["<])([^">]+)([">])/gm;

       
        while (nodesToProcess.length > 0) {
            const currentPath = nodesToProcess.shift()!;
            const currentName = path.basename(currentPath);

            if (visited.has(currentName)) continue;
            visited.add(currentName);

            try {
                const content = await fs.promises.readFile(currentPath, 'utf-8');
                let match;

                while ((match = includeRegex.exec(content)) !== null) {
                    const includedName = path.basename(match[2]);
                    
                    edges.add(`    "${currentName}" -> "${includedName}";`);

                    if (fileMap.has(includedName)) {
                        if (!visited.has(includedName)) {
                            nodesToProcess.push(fileMap.get(includedName)!);
                        }
                    } else {
                        systemNodes.add(includedName);
                    }
                }
            } catch (err) {  }
        }

        let dotContent = "digraph RecursiveIncludeGraph {\n";
        dotContent += "    rankdir=LR; overlap=false; splines=true;\n";
        dotContent += "    node [shape=box, style=\"rounded,filled\", fontname=\"Helvetica\", fontsize=10];\n";
        dotContent += "    edge [color=\"#7f8c8d\", penwidth=1.0, arrowsize=0.7];\n\n";

        dotContent += `    "${startFileName}" [fillcolor="#fcf3cf", color="#f1c40f", penwidth=2.0];\n`;

        visited.forEach(node => {
            if (node !== startFileName) {
                dotContent += `    "${node}" [fillcolor="#d4efdf", color="#27ae60"];\n`;
            }
        });

        systemNodes.forEach(node => {
            dotContent += `    "${node}" [fillcolor="#ebedef", color="#a6acaf", style=\"rounded,dashed,filled\"];\n`;
        });

        edges.forEach(edge => dotContent += edge + "\n");
        dotContent += "}\n";

        const outDir = path.join(workspacePath, ".frama-c");
        if (!fs.existsSync(outDir)) fs.mkdirSync(outDir, { recursive: true });
        
        const fileNameBase = path.parse(startFileName).name;
        const dotPath = path.join(outDir, `includes_cascade_${fileNameBase}.dot`);
        const dotUri = vscode.Uri.file(dotPath);

        fs.writeFileSync(dotPath, dotContent);

        
        const allCommands = await vscode.commands.getCommands();
        
       
        const doc = await vscode.workspace.openTextDocument(dotUri);
        
     
        if (allCommands.includes('graphviz-interactive-preview.preview.beside')) {
            await vscode.window.showTextDocument(doc, vscode.ViewColumn.Active);
            
            await vscode.commands.executeCommand('graphviz-interactive-preview.preview.beside', dotUri);
        } else {
           
            await vscode.window.showTextDocument(doc, vscode.ViewColumn.Beside);
            vscode.window.showWarningMessage("Installe l'extension 'Graphviz Interactive Preview' pour voir le schéma visuel !");
        }

    } catch (error) { 
        vscode.window.showErrorMessage("Erreur : " + error); 
    }
}
export function deactivate() { if (client) return client.stop(); }