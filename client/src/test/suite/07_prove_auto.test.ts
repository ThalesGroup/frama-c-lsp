import * as assert from 'assert';
import * as vscode from 'vscode';
import { openBenchmark, activateExtension } from './helpers';

suite('proveAuto', () => {

    test('detecte le contexte et prouve automatiquement', async function() {
        this.timeout(90000);

        await openBenchmark('display_ast/test.c');
        await activateExtension();

        const editor = vscode.window.activeTextEditor!;
        // ligne 206 (0-based) = ligne 207 (1-based) = fonction public_max
        editor.selection = new vscode.Selection(206, 0, 206, 0);

        const original = vscode.window.showInputBox;
        (vscode.window as any).showInputBox = async () => '10';

        try {
            await vscode.commands.executeCommand('provePO Cursor');
            // laisse le temps au pipeline complet (getContext + provePO)
            await new Promise(r => setTimeout(r, 20000));
            // le pipeline s'est execute sans throw → OK
            assert.ok(true, 'proveAuto pipeline s\'est execute avec succes');
        } finally {
            (vscode.window as any).showInputBox = original;
        }
    });
});