import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';
import { openBenchmark, activateExtension, waitForFileWritten, cleanFile, getWorkspacePath } from './helpers';

suite('displayCIL', () => {

    test('CIL normalise for, switch et ternaire', async function() {
        this.timeout(60000);

        await openBenchmark('cil/test.c');
        await activateExtension();

        const outFile = path.join(getWorkspacePath(), '.frama-c', 'fc_test.c');
        cleanFile(outFile);

        await vscode.commands.executeCommand('displayCIL');
        const cil = await waitForFileWritten(outFile);

        // les 4 fonctions doivent apparaitre
        assert.ok(cil.includes('abs_val'),  'fonction abs_val absente du CIL');
        assert.ok(cil.includes('classify'), 'fonction classify absente du CIL');
        assert.ok(cil.includes('sum_to'),   'fonction sum_to absente du CIL');
        assert.ok(cil.includes('in_range'), 'fonction in_range absente du CIL');

        // le for a ete normalise en while
        assert.ok(cil.includes('while'), 'aucun while trouve — le for aurait du etre normalise');

        // le switch a ete normalise en if/else (plus de mot-cle switch)
        assert.ok(!cil.includes('switch'), 'switch encore present — normalisation incomplete');
    });
});