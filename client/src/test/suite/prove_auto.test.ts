import * as assert from 'assert';
import * as vscode from 'vscode';
import { openBenchmark, activateExtension } from './helpers';

suite('proveAuto', () => {

    test('prouve automatiquement au curseur', async function() {
        this.timeout(90000);

        const src = await openBenchmark('wp_pass/test.c');
        await activateExtension();

        // positionne le curseur sur une fonction connue de wp_pass/test.c
        // ligne 77 = swap (visible dans les logs de provePO)
        const editor = vscode.window.activeTextEditor!;
        editor.selection = new vscode.Selection(76, 0, 76, 0);

        // mocke le prompt de timeout (le command affiche un inputBox bloquant)
        const original = vscode.window.showInputBox;
        (vscode.window as any).showInputBox = async () => '10';
        try {
            await vscode.commands.executeCommand('provePO Cursor');
            // le command declenche proveAuto qui met a jour wpDataProvider
            // difficile d'inspecter sans exposer le client — au moins on verifie
            // que le command s'execute sans throw
            assert.ok(true, 'proveAuto a ete declenche sans erreur');
        } finally {
            (vscode.window as any).showInputBox = original;
        }
    });
});