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



let allGoalsRaw: any[] = []; 
let activeFilters = { status: "all", smokeOnly: false, prover: "all", search: "" };

// Store last used path for import/export
let lastImportPath: string = "";
let lastExportPath: string = "";


async function uploadWpJson() {
    const fileUri = await vscode.window.showOpenDialog({
        canSelectMany: false,
        openLabel: 'Import Frama-C results (JSON)',
        defaultUri: lastImportPath ? vscode.Uri.file(lastImportPath) : undefined,
        filters: { 'JSON': ['json'] }
    });

    if (!fileUri || !fileUri[0]) return;

    lastImportPath = fileUri[0].fsPath;

    try {
        const content = fs.readFileSync(fileUri[0].fsPath, 'utf8');
        const rawData = JSON.parse(content);

        if (!rawData || rawData.length === 0) {
            vscode.window.showWarningMessage("The JSON file is empty.");
            return;
        }

        const replaceChoice = await window.showQuickPick(
            [
                { label: "Replace path prefix", description: "Replace a path prefix with local workspace" },
                { label: "Keep original paths", description: "Keep file paths as they are in the JSON" }
            ],
            { placeHolder: "How should file paths be handled?" }
        );

        if (!replaceChoice) return;

        const keepOriginalPaths = replaceChoice.label === "Keep original paths";
        let normalizedPath = "";
        const localWorkspace = get_workspace().replace(/\\/g, '/').replace(/\/$/, '');

        if (!keepOriginalPaths) {
            const pathToReplace = await vscode.window.showInputBox({
                prompt: "Confirm the path prefix to be replaced by your current workspace:",
                value: "",
                ignoreFocusOut: true
            });

            if (pathToReplace === undefined || pathToReplace === "") return;
            normalizedPath = pathToReplace.replace(/\\/g, '/').replace(/\/$/, '');
        }

        let missingFilesCount = 0;
        allGoalsRaw = []; 

        await vscode.window.withProgress({
            location: vscode.ProgressLocation.Notification,
            title: "Processing goals...",
            cancellable: false
        }, async (progress) => {
            for (let i = 0; i < rawData.length; i++) {
                const item = rawData[i];
                let originalPath = (item.file || "").replace(/\\/g, '/');
                
                if (keepOriginalPaths) {
                    item._localPath = originalPath;
                } else if (normalizedPath && originalPath.startsWith(normalizedPath)) {
                    const absoluteLocalPath = originalPath.replace(normalizedPath, localWorkspace);
                    
                    if (i % 100 === 0 && !fs.existsSync(absoluteLocalPath)) {
                        missingFilesCount++;
                    }

                    const relativePart = absoluteLocalPath.replace(localWorkspace + '/', '');
                    item._localPath = "./" + relativePart;
                } else {
                    item._localPath = originalPath;
                }

                allGoalsRaw.push(item);


                if (i % 5000 === 0) {
                    progress.report({ increment: (5000 / rawData.length) * 100 });
                    await new Promise(resolve => setTimeout(resolve, 1));
                }
            }
        });


        activeFilters = { status: "all", smokeOnly: false, prover: "all", search: "" };
        applyFiltersAndRefreshUI();
        
        vscode.commands.executeCommand('wpGoalsView.focus');
        vscode.window.showInformationMessage(`Imported ${allGoalsRaw.length} goals successfully.`);

    } catch (err) {
        vscode.window.showErrorMessage("Error during import: " + err);
    }
}


function applyFiltersAndRefreshUI() {
    if (allGoalsRaw.length === 0) return;

    const fullyFilteredDataset = allGoalsRaw.filter(item => {
        const status = item.passed ? "passed" : (item.verdict === "timeout" ? "unknown" : "failed");
        if (activeFilters.status !== "all" && status !== activeFilters.status) return false;
        if (activeFilters.smokeOnly && item.smoke !== true) return false;
        
        const provers = (item.provers && item.provers.length > 0) ? item.provers : [{ prover: "qed", time: 0 }];
        const hasMatchingProver = provers.some((p: any) => 
            (p.prover || "qed").toLowerCase().includes(activeFilters.prover.toLowerCase())
        );
        if (activeFilters.prover !== "all" && !hasMatchingProver) return false;

        if (activeFilters.search) {
            const term = activeFilters.search.toLowerCase();
            const funcMatch = (item.function || "").toLowerCase().includes(term);
            const fileMatch = (item._localPath || item.file || "").toLowerCase().includes(term);
            if (!funcMatch && !fileMatch) return false;
        }

        return true; 
    });

    
    const MAX_DISPLAY = 10000;
    const toDisplay = fullyFilteredDataset.slice(0, MAX_DISPLAY);

    
    const formatted = toDisplay.map(item => {
        const status = item.passed ? "passed" : (item.verdict === "timeout" ? "unknown" : "failed");
        
        let proverInfo = "(qed 0)";
        if (item.provers && item.provers.length > 0 && item.provers[0]) {
            const proverParts: string[] = [];
            for (const p of item.provers) {
                if (p && p.prover) {
                    const proverName = p.prover;
                    const proverTime = (p.time !== undefined && p.time !== null) ? p.time : "?";
                    proverParts.push(`(${proverName} ${proverTime})`);
                }
            }
            if (proverParts.length > 0) {
                proverInfo = proverParts.join(" ");
            }
        }
        
   
        return {
            status: status,
            goal: item.goal || "goal",
            file: item._localPath || "",
            line: item.line || 1,
            proverInfo: proverInfo,
            script: item.script || "",
            function: item.function || ""
        };
    });

    if (fullyFilteredDataset.length > MAX_DISPLAY) {
        formatted.unshift({
            status: "⚠️ SYSTEM",
            goal: `SHOWING ${MAX_DISPLAY} OUT OF ${fullyFilteredDataset.length} GOALS`,
            file: "./",
            line: 0,
            proverInfo: "Please refine your search.",
            script: "",
            function: ""
        });
    }

    lastWpData = formatted as any;
    wpDataProvider.update(["Filtered", "All", "All", formatted]);
    wpDataProvider.refresh();
    setTimeout(() => updateDecorations(), 200);}

async function downloadWpJson() {
    const workspacePath = get_workspace();
   
    const hiddenArtifactPath = path.join(workspacePath, '.frama-c', 'latest_results.json');

    if (!fs.existsSync(hiddenArtifactPath)) {
        vscode.window.showWarningMessage("No proof results available. Please run a proof first.");
        return;
    }

    const now = new Date();
    const timestamp = now.toISOString().replace(/[:.]/g, '-').substring(0, 19);
    const defaultFileName = `proof_results_${timestamp}.json`;

    const defaultUri = lastExportPath 
        ? vscode.Uri.file(lastExportPath) 
        : vscode.Uri.file(path.join(workspacePath, defaultFileName));

    const saveUri = await vscode.window.showSaveDialog({
        defaultUri: defaultUri,
        filters: { 'JSON': ['json'] },
        saveLabel: 'Export Frama-C Results'
    });

    if (!saveUri) return;

    lastExportPath = path.dirname(saveUri.fsPath);

    try {
        fs.copyFileSync(hiddenArtifactPath, saveUri.fsPath);
        vscode.window.showInformationMessage("Results successfully exported!");
    } catch (err) {
        vscode.window.showErrorMessage("Failed to export JSON: " + err);
    }
}

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


async function initializeDefaultSettings() {
    const workspaceFolders = vscode.workspace.workspaceFolders;
    if (!workspaceFolders) {
        return; 
    }

    const workspacePath = workspaceFolders[0].uri.fsPath;
    const vscodeFolder = path.join(workspacePath, '.vscode');
    const settingsPath = path.join(vscodeFolder, 'settings.json');

    if (fs.existsSync(settingsPath)) {
        return;
    }

    if (!fs.existsSync(vscodeFolder)) {
        fs.mkdirSync(vscodeFolder, { recursive: true });
    }

    const defaultSettings = {
        "kernel.includePaths": [],
        "kernel.sourceFiles": [],
        
        "kernel.macros": [
            "__FRAMAC_SPM", 
            "__FRAMAC_STRATEGIES_", 
            "FRAMA_C_INCOMPLETE_STRATEGIES", 
            "__FRAMAC_CCDOC", 
            "__FRAMAC_METACSL",
            "__FRAMAC_VERSION=30",
            "FRAMA_C_TSFI_FSP"
        ],

        "vscodeacsl.maxNumberOfProblems": 100000,
        "kernel.lspDebug": 0,
        "kernel.macroStrategiesFunctionPrefix": "__FRAMAC_STRATEGIES_",
        "kernel.sourceFileStrategies": [],
        "kernel.sourceFileMetacsl": [],
        "kernel.aggressiveMerging": false,
        "kernel.removeUnusedSpecifiedFunctions": false,
        "kernel.machdep": "x86_32",
        "kernel.inlineCalls": ["@inline"],
        "kernel.removeInlined": ["@inline"],
        "kernel.noAnnot": false,
        "kernel.generatedSpecCustom": [],

        "wp.checkMemoryModel": true,
        "wp.noVolatile": false,
        "wp.rte": true,
        "wp.prover": "script,alt-ergo",
        "wp.timeout": 2,
        "wp.model": "Typed+var+int+float",
        "wp.par": 8,
        "wp.cache": "update",
        "wp.script": "batch",
        "wp.session": "frama_c/sessions",
        "wp.autoDepth": 20,
        "wp.autoWidth": 1,

        "metrics.byFunction": true,
        "callgraph.roots": [],
        "callgraph.services": false,
        "uncast.active": true,
        "uncast.lshiftAsMul": true,
        "uncast.rshiftAsDiv": true,
        "uncast.endianness": "little",
        "metacsl.active": false,
        "metacsl.checks": true,
        "metacsl.noCheckExt": true,
        "metacsl.noSimpl": true,
        "metacsl.numberAssertions": true,
        "ccdoc.active": true,
        "ccdoc.coverageVerif": true,
        "ccdoc.latex": true
    };

    try {
        fs.writeFileSync(settingsPath, JSON.stringify(defaultSettings, null, 4), 'utf-8');
        vscode.window.showInformationMessage("Configuration JCAT initialisée avec succès.");
    } catch (error) {
        console.error("Erreur d'initialisation :", error);
    }
}
function updateDecorations() {
    const editors = vscode.window.visibleTextEditors;

    editors.forEach(editor => {
        const currentFileName = path.basename(editor.document.fileName);

        interface LineInfo {
            worstStatus: string;
            goals: { status: string; goalId: string; proverInfo: string }[];
        }
        const lineStatusMap = new Map<number, LineInfo>();

        lastWpData.forEach((item: any) => {
            let status: string, goalId: string, itemFileName: string, line: number, proverInfo: string;

            if (typeof item === 'string') {
                const p = item.trim().split(":");
                if (p.length < 5) return; 
                status = p[0].trim().toLowerCase();
                goalId = p[1].trim(); 
                itemFileName = path.basename(p[2].trim());
                line = parseInt(p[3].trim(), 10) - 1;
                proverInfo = p[4] ? p[4].trim() : ""; 
            } else {
                if (!item || !item.status) return; 
                status = item.status.toLowerCase();
                goalId = item.goal;
                itemFileName = path.basename(item.file);
                line = item.line - 1;
                proverInfo = item.proverInfo;
            }

            if (itemFileName === currentFileName && !isNaN(line)) {
                if (!lineStatusMap.has(line)) {
                    lineStatusMap.set(line, { worstStatus: status, goals: [] });
                }
                
                const lineInfo = lineStatusMap.get(line)!;
                
                lineInfo.goals.push({ status, goalId, proverInfo });

                if (status === "failed") {
                    lineInfo.worstStatus = "failed"; 
                } else if (status === "unknown" && lineInfo.worstStatus !== "failed") {
                    lineInfo.worstStatus = "unknown"; 
                } else if (status === "passed" && !lineInfo.worstStatus) {
                    lineInfo.worstStatus = "passed"; 
                }
            }
        });

        const passedOpts: vscode.DecorationOptions[] = [];
        const failedOpts: vscode.DecorationOptions[] = [];
        const unknownOpts: vscode.DecorationOptions[] = [];

        lineStatusMap.forEach((info, line) => {
            const range = editor.document.lineAt(line).range;
            
            const hoverMessage = new vscode.MarkdownString();
            hoverMessage.isTrusted = true;
            hoverMessage.appendMarkdown(`**WP Proof Goals**\n\n`);
            
            info.goals.forEach(g => {
                let icon = "✅";
                if (g.status === "failed") icon = "❌";
                if (g.status === "unknown") icon = "⚠️";
                
                hoverMessage.appendMarkdown(`- ${icon} **${g.goalId}** _${g.proverInfo}_\n`);
            });

            const decoration = { range, hoverMessage };

            if (info.worstStatus === "passed") passedOpts.push(decoration);
            else if (info.worstStatus === "failed") failedOpts.push(decoration);
            else if (info.worstStatus === "unknown") unknownOpts.push(decoration);
        });

        editor.setDecorations(passedDecoration, passedOpts);
        editor.setDecorations(failedDecoration, failedOpts);
        editor.setDecorations(unknownDecoration, unknownOpts);
    });
}

export async function activate(context: ExtensionContext) {
	// The server is implemented in OCaml
    await initializeDefaultSettings();
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
    



    vscode.workspace.onDidSaveTextDocument(async (document) => {
        if (document.languageId === 'c') {
            await vscode.commands.executeCommand('acsl-lsp.debugAST');
        }
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
                    return response;
                } else {
                    window.showWarningMessage("No AST Found");
                }
            } catch (error) { console.error(error); }
            
        }),
	
// Command to handle clicking on any Sidebar element

commands.registerCommand('framaC.openAndDetail', async (item: FramaCItem) => {
    if (!item.resourceUri || item.line === undefined) return;

   
        const document = await workspace.openTextDocument(item.resourceUri);
        const editor = await window.showTextDocument(document);

        if (item.line > 0) {
            const pos = new Position(item.line - 1, 0);
            const lineText = document.lineAt(pos.line).text;
            const charIndex = lineText.indexOf(item.label);
            const finalPos = new Position(pos.line, charIndex !== -1 ? charIndex : 0);

            editor.selection = new vscode.Selection(finalPos, finalPos);
            editor.revealRange(new Range(finalPos, finalPos), vscode.TextEditorRevealType.InCenter);

    } 
}),
commands.registerCommand('framaC.refreshAST', () => {
    vscode.commands.executeCommand('acsl-lsp.debugAST');
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
            await showCallgraphMatrix();

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
        }),
       
        commands.registerCommand('wpFilters.search', async () => {
            const query = await window.showInputBox({ 
                prompt: "Search by function or file name...",
                value: activeFilters.search
            });
            if (query !== undefined) {
                activeFilters.search = query.trim();
                applyFiltersAndRefreshUI();
            }
        }),

    
        commands.registerCommand('wpFilters.filterStatus', async () => {
            const options = ["all", "passed", "failed", "unknown"];
            const choice = await window.showQuickPick(options, { placeHolder: "Select Goal Status" });
            if (choice) {
                activeFilters.status = choice;
                applyFiltersAndRefreshUI();
            }
        }),

        
        commands.registerCommand('wpFilters.toggleSmoke', () => {
            activeFilters.smokeOnly = !activeFilters.smokeOnly;
            window.showInformationMessage(`Smoke tests only: ${activeFilters.smokeOnly ? "ON" : "OFF"}`);
            applyFiltersAndRefreshUI();
        }),


        commands.registerCommand('wpFilters.filterProver', async () => {
            const options = ["all", "qed", "Alt-Ergo", "Z3"];
            const choice = await window.showQuickPick(options, { placeHolder: "Select Prover" });
            if (choice) {
                activeFilters.prover = choice;
                applyFiltersAndRefreshUI();
            }
        }),

    
        commands.registerCommand('wpFilters.clearAll', () => {
            activeFilters = { status: "all", smokeOnly: false, prover: "all", search: "" };
            applyFiltersAndRefreshUI();
            window.showInformationMessage("All filters cleared.");
        }),
        commands.registerCommand('wpGoalsView.downloadJson', () => downloadWpJson()),
        commands.registerCommand('wpGoalsView.uploadJson', () => uploadWpJson()),
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
                if (filename_id !== "Filtered" && filename_id !== "Imported") {
                    allGoalsRaw = jsonData.map((item: string) => {
                        const p = item.trim().split(":");
                        return {
                            passed: p[0] === "passed",
                            verdict: p[0] === "unknown" ? "timeout" : (p[0] === "passed" ? "valid" : "failed"),
                            goal: p[1] || "goal",
                            file: p[2] || "",
                            _localPath: p[2] || "",
                            line: parseInt(p[3] || "1", 10),
                            provers: [{ prover: p[4] || "qed" }],
                            script: p[5] || "",
                            function: p[6] || "",
                            smoke: false // (By default)
                        };
                    });
                    activeFilters = { status: "all", smokeOnly: false, prover: "all", search: "" };
                }

            } else {
                lastWpData = [];
                if (filename_id !== "Filtered" && filename_id !== "Imported") {
                    allGoalsRaw = [];
                }
            }
            
            updateDecorations();
            
            if (!jsonData || jsonData.length === 0) {
                this.data = [new TreeItem("No goals !")];
            } else {
                this.data = jsonData.map((item: any) => {
                    let status: string, goal: string, file: string, line: string, proverInfo: string, script: string, func: string;

                    if (typeof item === 'string') {
                        const p = item.trim().split(":");
                        status = p[0].trim();
                        goal = p[1] || "";
                        file = p[2] || "";
                        line = p[3] || "1";
                        proverInfo = p[4] || "";
                        script = p[5] || "";
                        func = p[6] || "";
                    } else {
                        status = item.status;
                        goal = item.goal;
                        file = item.file;
                        line = String(item.line);
                        proverInfo = item.proverInfo;
                        script = item.script;
                        func = item.function;
                    }

                    const t_item = new TreeItem(status, `${goal} ${proverInfo}`, file, func, goal, script, filename_id, fct_id, prop_id, 'itemContext');
                    const workspacePath = get_workspace();
                    
                    const absolutePath = path.resolve(workspacePath, file);
                    
                    t_item.command = {
                        command: 'vscode.open',
                        title: 'Open File',
                        arguments: [vscode.Uri.file(absolutePath).with({ fragment: `L${line}` })]
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
    private currentAstUri: string | undefined;
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
        this.currentAstUri = uri;
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
    if (!element) {
        return [
            new FramaCItem("Functions", vscode.TreeItemCollapsibleState.Expanded, "cat_func"),
            new FramaCItem("Variables", vscode.TreeItemCollapsibleState.Collapsed, "cat_var"),
            new FramaCItem("Types", vscode.TreeItemCollapsibleState.Collapsed, "cat_type"),
            new FramaCItem("Annotations", vscode.TreeItemCollapsibleState.Collapsed, "cat_annot")
        ];
    }

    if (!this.currentAstUri) return [];
    
    const data = this.astData.get(this.currentAstUri);
    if (!data) return [];

    let children: FramaCItem[] = [];

    const resolveUri = (filePath: string) => {
        if (!filePath) return vscode.Uri.parse(this.currentAstUri!); // Fallback sécurisé
        
        const workspacePath = workspace.workspaceFolders ? workspace.workspaceFolders[0].uri.fsPath : process.cwd();
        let cleanPath = filePath;
        if (cleanPath.startsWith('./')) {
            cleanPath = cleanPath.substring(2);
        }
        const absolutePath = path.resolve(workspacePath, cleanPath);
        return vscode.Uri.file(absolutePath);
    };

    switch (element.contextValue) {
        case "cat_func":
            if (!this.hideFunctions && data.functions) {
                data.functions.forEach((f: any) => {
                    children.push(new FramaCItem(f.name, vscode.TreeItemCollapsibleState.None, "function", resolveUri(f.file), f.line));
                });
            }
            break;
        case "cat_var":
            if (!this.hideVariables && data.globals) {
                data.globals.forEach((g: any) => {
                    children.push(new FramaCItem(g.name, vscode.TreeItemCollapsibleState.None, "variable", resolveUri(g.file), g.line));
                });
            }
            break;
        case "cat_type":
            if (!this.hideTypes && data.types) {
                data.types.forEach((t: any) => {
                    children.push(new FramaCItem(t.name, vscode.TreeItemCollapsibleState.None, "type", resolveUri(t.file), t.line));
                });
            }
            break;
        case "cat_annot":
            if (!this.hideAnnotations && data.annotations) {
                data.annotations.forEach((a: any) => {
                    children.push(new FramaCItem(a.name, vscode.TreeItemCollapsibleState.None, "predicate", resolveUri(a.file), a.line));
                });
            }
            break;
    }
    return children;
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
if (contextValue.startsWith("cat_")) {
    this.iconPath = new vscode.ThemeIcon("list-flat"); 
} else if (contextValue === "function") {
    this.iconPath = new vscode.ThemeIcon("symbol-method");
} else if (contextValue === "variable") {
    this.iconPath = new vscode.ThemeIcon("symbol-variable");
} else if (contextValue === "type") {
    this.iconPath = new vscode.ThemeIcon("symbol-parameter");
}
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
async function showCallgraphMatrix() {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
        return;
    }

    const workspacePath = get_workspace();
    const filePath = editor.document.fileName;
    const fileNameBase = path.basename(filePath, path.extname(filePath));
    const dotFilePath = path.join(workspacePath, ".frama-c", `fc_${fileNameBase}.dot`);

    if (!fs.existsSync(dotFilePath)) {
        vscode.window.showWarningMessage(`Le fichier ${dotFilePath} n'existe pas.`);
        return;
    }

    try {
       
        const dotContent = fs.readFileSync(dotFilePath, 'utf-8');

        const funcCalls = new Map<string, Set<string>>(); 
        const dotRegex = /(?:"?([a-zA-Z0-9_]+)"?)\s*->\s*(?:"?([a-zA-Z0-9_]+)"?)/g;
        let match;
        let dotLinkCount = 0;
        while ((match = dotRegex.exec(dotContent)) !== null) {
            const caller = match[1];
            const callee = match[2];
            if (!funcCalls.has(caller)) funcCalls.set(caller, new Set());
            funcCalls.get(caller)!.add(callee);
            dotLinkCount++;
        }
        
       

        
        const allFiles = await vscode.workspace.findFiles('**/*.{c,h}', '**/{node_modules,.git,build,obj}/**');
        const funcToFile = new Map<string, string>(); 
        
        const cFuncRegex = /(?:^|\n)(?:\w+\s+)+\**([a-zA-Z_][a-zA-Z0-9_]*)\s*\([^;]*\)\s*(?:\{|;)/g;

        for (const file of allFiles) {
            const fileName = path.basename(file.fsPath);
            const content = fs.readFileSync(file.fsPath, 'utf-8');
            let funcMatch;
            while ((funcMatch = cFuncRegex.exec(content)) !== null) {
                const funcName = funcMatch[1];
                if (!['if', 'while', 'for', 'switch', 'return'].includes(funcName)) {
                    if (!funcToFile.has(funcName) || fileName.endsWith('.c')) {
                        funcToFile.set(funcName, fileName);
                    }
                }
            }
        }
        const fileDependencies = new Map<string, Set<string>>(); 
        const allCallerFiles = new Set<string>(); 
        const allCalleeFiles = new Set<string>(); 

        let matchCount = 0;
        let missingFuncs = new Set<string>();

        funcCalls.forEach((callees, callerFunc) => {
            const callerFile = funcToFile.get(callerFunc);
            
            if (!callerFile) {
                missingFuncs.add(callerFunc);
                return;
            }
            if (!callerFile.endsWith('.c')) return; 

            allCallerFiles.add(callerFile);
            if (!fileDependencies.has(callerFile)) fileDependencies.set(callerFile, new Set());

            callees.forEach(calleeFunc => {
                const calleeFile = funcToFile.get(calleeFunc);
                if (calleeFile) {
                    allCalleeFiles.add(calleeFile);
                    fileDependencies.get(callerFile)!.add(calleeFile);
                    matchCount++;
                } else {
                    missingFuncs.add(calleeFunc);
                }
            });
        });

       
        const columns = Array.from(allCallerFiles).sort();
        const rows = Array.from(allCalleeFiles).sort();

       

        if (columns.length === 0 || rows.length === 0) {
            vscode.window.showErrorMessage("Matrice vide");
            return;
        }

        
        const panel = vscode.window.createWebviewPanel(
            'callgraphMatrix',
            `Matrix: ${fileNameBase}`,
            vscode.ViewColumn.Active,
            { enableScripts: true }
        );

        const htmlChunks: string[] = [];
        htmlChunks.push(`
        <!DOCTYPE html>
        <html lang="fr">
        <head>
            <meta charset="UTF-8">
            <style>
                body { font-family: var(--vscode-font-family); padding: 20px; color: var(--vscode-editor-foreground); background: var(--vscode-editor-background); }
                .table-container { overflow: auto; max-height: 85vh; border: 1px solid var(--vscode-panel-border); }
                table { border-spacing: 0; width: max-content; }
                th, td { border-bottom: 1px solid var(--vscode-panel-border); border-right: 1px solid var(--vscode-panel-border); padding: 8px; text-align: center; }
                th { background-color: var(--vscode-editor-inactiveSelectionBackground); position: sticky; top: 0; z-index: 10; }
                th:first-child { left: 0; z-index: 20; text-align: right; background-color: var(--vscode-editor-selectionBackground); }
                .col-header { writing-mode: vertical-rl; transform: rotate(180deg); white-space: nowrap; padding-bottom: 10px; }
                .yes { background-color: rgba(60, 179, 113, 0.5); color: white; font-weight: bold; }
                .no { color: transparent; }
                tr:hover td { background-color: var(--vscode-list-hoverBackground); }
            </style>
        </head>
        <body>
            <h2>Matrice de Dépendance d'Appels (Callgraph)</h2>
            <div class="table-container">
                <table>
                    <thead>
                        <tr>
                            <th>Fichier appelé \\ Fichiers appelants ➜</th>
        `);

        for (const col of columns) htmlChunks.push(`<th><div class="col-header">${col}</div></th>`);
        htmlChunks.push('</tr></thead><tbody>');

        for (const row of rows) {
            htmlChunks.push(`<tr><th>${row}</th>`);
            for (const col of columns) {
                const isCalling = fileDependencies.get(col)?.has(row);
                if (isCalling) htmlChunks.push(`<td class="yes">✔</td>`);
                else htmlChunks.push(`<td class="no"></td>`);
            }
            htmlChunks.push('</tr>');
        }

        htmlChunks.push(`</tbody></table></div></body></html>`);
        panel.webview.html = htmlChunks.join('');

    } catch (error) {
        vscode.window.showErrorMessage("Error: " + error);
    }
}
export function deactivate() { if (client) return client.stop(); }