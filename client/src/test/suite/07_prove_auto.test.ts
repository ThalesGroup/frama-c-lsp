import * as assert from 'assert';
import * as vscode from 'vscode';
import { openBenchmark, activateExtension } from './helpers';

suite('proveAuto', () => {

    test('prouve automatiquement au curseur', async function() {
        this.timeout(90000);

        const src = await openBenchmark('wp_pass/test.c');
        await activateExtension();

        const editor = vscode.window.activeTextEditor!;
        assert.ok(editor, 'aucun editor actif');
        assert.ok(editor.document.fileName.endsWith('test.c'), 'mauvais fichier ouvert');

        // curseur sur une ligne dans une fonction connue (ex. swap ligne 77)
        editor.selection = new vscode.Selection(76, 0, 76, 0);

        // mock du inputBox
        const original = vscode.window.showInputBox;
        (vscode.window as any).showInputBox = async () => '10';

        let commandThrew = false;
        try {
            await vscode.commands.executeCommand('provePO Cursor');
        } catch (e) {
            commandThrew = true;
            console.error('provePO Cursor a throw:', e);
        } finally {
            (vscode.window as any).showInputBox = original;
        }

        assert.ok(!commandThrew, 'provePO Cursor ne doit pas throw');
    });
});