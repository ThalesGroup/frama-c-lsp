import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';

/** Racine du workspace ouvert par test-electron. */
export function getWorkspacePath(): string {
    const folders = vscode.workspace.workspaceFolders;
    if (!folders || folders.length === 0) throw new Error('Pas de workspace ouvert');
    return folders[0].uri.fsPath;
}

/** Ouvre un fichier benchmark et le rend actif. */
export async function openBenchmark(relPath: string) {
    const abs = path.resolve(__dirname, '../../../benchmarks', relPath);
    const doc = await vscode.workspace.openTextDocument(vscode.Uri.file(abs));
    await vscode.window.showTextDocument(doc);
    return abs;
}

/** Active l'extension et attend l'init du serveur OCaml. */
export async function activateExtension(waitMs = 10000) {
    const ext = vscode.extensions.getExtension('innov-org.acsl-lsp')!;
    await ext.activate();
    await new Promise(r => setTimeout(r, waitMs));
}
/** Attend que le LSP soit stable (plus de processus en cours) */
export async function waitForLspStable(ms = 5000): Promise<void> {
    await new Promise(r => setTimeout(r, ms));
}
/**
 * Attend que le fichier existe ET que son contenu ne soit plus le placeholder
 * "Task in progress ..." écrit par create_file() côté extension.
 */
export async function waitForFileWritten(filePath: string, timeoutMs = 45000): Promise<string> {
    const start = Date.now();
    while (Date.now() - start < timeoutMs) {
        if (fs.existsSync(filePath)) {
            const content = fs.readFileSync(filePath, 'utf-8');
            if (content.length > 0 && !content.startsWith('Task in progress')) {
                return content;
            }
        }
        await new Promise(r => setTimeout(r, 500));
    }
    throw new Error(`Timeout: ${filePath} pas ecrit apres ${timeoutMs}ms`);
}

/** Supprime un fichier si existant (silencieux si absent). */
export function cleanFile(filePath: string) {
    if (fs.existsSync(filePath)) fs.unlinkSync(filePath);
}