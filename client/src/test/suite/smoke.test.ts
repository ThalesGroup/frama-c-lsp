import * as assert from 'assert';
import * as vscode from 'vscode';
import { openBenchmark, activateExtension } from './helpers';

suite('smokeTests', () => {

    test('publie des diagnostics smoke sur le fichier', async function() {
        this.timeout(90000);  // WP smoke peut etre lent

        const src = await openBenchmark('smoke/test.c');
        await activateExtension();

        const uri = vscode.Uri.file(src);
        const initialCount = vscode.languages.getDiagnostics(uri).length;

        // trigger smoke tests
        await vscode.commands.executeCommand('smokeTests');

        // polling : les diagnostics arrivent en asynchrone via LSP publishDiagnostics
        const start = Date.now();
        let diags: readonly vscode.Diagnostic[] = [];
        while (Date.now() - start < 60000) {
            diags = vscode.languages.getDiagnostics(uri);
            if (diags.length > initialCount) break;
            await new Promise(r => setTimeout(r, 1000));
        }

        console.log(`Diagnostics apres smokeTests : ${diags.length} (initial: ${initialCount})`);
        diags.forEach(d => console.log(`  - [${d.severity}] ${d.message}`));

        assert.ok(diags.length > initialCount,
            `aucun nouveau diagnostic publie apres smokeTests (attendu au moins 1)`);
    });
});