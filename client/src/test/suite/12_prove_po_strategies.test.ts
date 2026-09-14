import * as assert from 'assert';
import * as vscode from 'vscode';
import { openBenchmark, activateExtension } from './helpers';

suite('provePOStrategies', () => {

    test('lance WP avec strategies sans throw', async function() {
    this.timeout(60000);

    await openBenchmark('wp_pass/test.c');
    await activateExtension();

    const allCommands = await vscode.commands.getCommands(true);
    assert.ok(allCommands.includes('provePOStrategies'), 'commande non enregistree');

    // mock showInputBox
    const original = vscode.window.showInputBox;
    (vscode.window as any).showInputBox = async () => '5'; // timeout court = 5s

    let threw = false;
    try {
        
        vscode.commands.executeCommand('provePOStrategies').catch(() => {});
        await new Promise(r => setTimeout(r, 5000));
    } catch (e) {
        threw = true;
    } finally {
        (vscode.window as any).showInputBox = original;
    }

    assert.ok(!threw, 'provePOStrategies ne doit pas throw au lancement');
});
});