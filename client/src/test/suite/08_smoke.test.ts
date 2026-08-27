import * as assert from 'assert';
import * as vscode from 'vscode';
import { openBenchmark, activateExtension } from './helpers';

suite('smokeTests', () => {

    test('publie des diagnostics WP smoke sur impossible()', async function() {
        this.timeout(120000);

        const src = await openBenchmark('smoke/test.c');
        await activateExtension();

        const uri = vscode.Uri.file(src);
        const before = vscode.languages.getDiagnostics(uri).length;
        console.log(`Diagnostics avant smokeTests : ${before}`);

        await vscode.commands.executeCommand('smokeTests');

       
        const start = Date.now();
        let diags: readonly vscode.Diagnostic[] = [];
        while (Date.now() - start < 90000) {
            diags = vscode.languages.getDiagnostics(uri);
            if (diags.length > before) break;
            await new Promise(r => setTimeout(r, 1000));
        }

        console.log(`Diagnostics apres smokeTests : ${diags.length}`);
        diags.forEach(d =>
            console.log(`  [sev:${d.severity}] ligne ${d.range.start.line + 1} : ${d.message.substring(0, 100)}`)
        );

        assert.ok(
            diags.length > before,
            `aucun diagnostic smoke recu — attendu au moins 1 sur impossible()`
        );

        const hasSmokeDiag = diags.some(d =>
            d.message.toLowerCase().includes('smoke') ||
            d.message.toLowerCase().includes('failed') ||
            d.message.toLowerCase().includes('doomed') ||
            d.message.toLowerCase().includes('impossible')
        );
        assert.ok(hasSmokeDiag, 'aucun diagnostic smoke/failed/doomed trouve');
    });
});