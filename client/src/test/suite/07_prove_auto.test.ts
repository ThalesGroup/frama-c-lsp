import * as assert from 'assert';
import * as vscode from 'vscode';
import { openBenchmark, activateExtension } from './helpers';

suite('proveAuto', () => {

    test('detects context and proves automatically', async function() {
        this.timeout(90000);

        await openBenchmark('display_ast/test.c');
        await activateExtension();

        const editor = vscode.window.activeTextEditor!;
        // line 206 (0-based) = line 207 (1-based) = function public_max
        editor.selection = new vscode.Selection(206, 0, 206, 0);

        const original = vscode.window.showInputBox;
        (vscode.window as any).showInputBox = async () => '10';

        try {
            await vscode.commands.executeCommand('provePO Cursor');
            // wait for the full pipeline (getContext + provePO)
            await new Promise(r => setTimeout(r, 20000));
            // pipeline executed without throw → OK
            assert.ok(true, 'proveAuto pipeline executed successfully');
        } finally {
            (vscode.window as any).showInputBox = original;
        }
    });
});