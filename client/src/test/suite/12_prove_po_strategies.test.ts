import * as assert from 'assert';
import * as vscode from 'vscode';
import { openBenchmark, activateExtension } from './helpers';

suite('provePOStrategies', () => {

    test('lance WP avec strategies sans throw', async function() {
    this.timeout(60000);

    await openBenchmark('wp_pass/test.c');
    await activateExtension();

    // vérifie que la commande est enregistrée
    const allCommands = await vscode.commands.getCommands(true);
    console.log('provePOStrategies enregistre:', allCommands.includes('provePOStrategies'));
    assert.ok(
        allCommands.includes('provePOStrategies'),
        'commande provePOStrategies non enregistree'
    );

    // déclenche la commande — notification, retourne immédiatement
    let threw = false;
    try {
        await vscode.commands.executeCommand('provePOStrategies');
        // attente courte — juste vérifier que ça part sans throw
        await new Promise(r => setTimeout(r, 5000));
    } catch (e) {
        threw = true;
        console.error('provePOStrategies a throw:', e);
    }

    assert.ok(!threw, 'provePOStrategies ne doit pas throw');
});
});