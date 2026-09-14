import * as assert from 'assert';
import * as vscode from 'vscode';
import { openBenchmark, activateExtension } from './helpers';

suite('provePOStrategies', () => {

    test('lance WP avec strategies sans throw', async function() {
        this.timeout(90000);

        await openBenchmark('wp_pass/test.c');
        await activateExtension();

        let threw = false;
        try {
            await vscode.commands.executeCommand('provePOStrategies');
           
            await new Promise(r => setTimeout(r, 20000));
        } catch (e) {
            threw = true;
            console.error('provePOStrategies a throw:', e);
        }

        assert.ok(!threw, 'provePOStrategies ne doit pas throw');
    });
});