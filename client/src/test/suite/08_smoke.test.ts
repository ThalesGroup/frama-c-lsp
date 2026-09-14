import * as assert from 'assert';
import * as vscode from 'vscode';
import { openBenchmark, activateExtension } from './helpers';

suite('smokeTests', () => {

    test('publishes WP smoke diagnostics on impossible()', async function() {
        this.timeout(120000);

        const src = await openBenchmark('smoke/test.c');
        await activateExtension();

        const uri = vscode.Uri.file(src);
        const before = vscode.languages.getDiagnostics(uri).length;
        console.log(`Diagnostics before smokeTests: ${before}`);

        await vscode.commands.executeCommand('smokeTests');

        // poll diagnostics — server uses synchronous execute_command
        // diagnostics arrive after analysis completes
        const start = Date.now();
        let diags: readonly vscode.Diagnostic[] = [];
        while (Date.now() - start < 90000) {
            diags = vscode.languages.getDiagnostics(uri);
            if (diags.length > before) break;
            await new Promise(r => setTimeout(r, 1000));
        }

        console.log(`Diagnostics after smokeTests: ${diags.length}`);
        diags.forEach(d =>
            console.log(`  [sev:${d.severity}] line ${d.range.start.line + 1} : ${d.message.substring(0, 100)}`)
        );

        assert.ok(
            diags.length > before,
            `no smoke diagnostic received — expected at least 1 on impossible()`
        );

        const hasSmokeDiag = diags.some(d =>
            d.message.toLowerCase().includes('smoke') ||
            d.message.toLowerCase().includes('failed') ||
            d.message.toLowerCase().includes('doomed') ||
            d.message.toLowerCase().includes('impossible')
        );
        assert.ok(hasSmokeDiag, 'no smoke/failed/doomed diagnostic found');
    });
});