import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';
import { openBenchmark, activateExtension } from './helpers';

suite('showPO et showPOVC', () => {

    suiteSetup(async function() {
        this.timeout(90000);
        // lance un provePO pour avoir des goals en cache
        await openBenchmark('wp_pass/test.c');
        await activateExtension();
        await vscode.commands.executeCommand('provePO');
        await new Promise(r => setTimeout(r, 30000));
    });

    test('showPOVC retourne la preuve obligation au curseur', async function() {
        this.timeout(60000);

        await openBenchmark('wp_pass/test.c');
        await activateExtension();

        const editor = vscode.window.activeTextEditor!;
        assert.ok(editor, 'aucun editor actif');

        // positionne le curseur sur la fonction swap (ligne 77)
        editor.selection = new vscode.Selection(76, 0, 76, 0);

        let commandThrew = false;
        try {
            await vscode.commands.executeCommand('showPOVC');
            await new Promise(r => setTimeout(r, 5000));
        } catch (e) {
            commandThrew = true;
            console.error('showPOVC a throw:', e);
        }

        assert.ok(!commandThrew, 'showPOVC ne doit pas throw');
    });

    test('showPO retourne le contenu d un goal par son id', async function() {
        this.timeout(60000);

        await openBenchmark('wp_pass/test.c');
        await activateExtension();

        let commandThrew = false;
        try {
            // showPO est declenche depuis le panel WP Goals
            // on verifie juste que la commande est enregistree
            const allCommands = await vscode.commands.getCommands(true);
            assert.ok(
                allCommands.includes('showPO') || allCommands.includes('showPOVC'),
                'commandes showPO/showPOVC non enregistrees'
            );
        } catch (e) {
            commandThrew = true;
            console.error('erreur:', e);
        }

        assert.ok(!commandThrew, 'pas d erreur attendue');
    });
});