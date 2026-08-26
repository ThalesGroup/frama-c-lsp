import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';
import { openBenchmark, activateExtension, waitForFileWritten, cleanFile, getWorkspacePath } from './helpers';

suite('displayCIL_noannot', () => {

    test('supprime les annotations ACSL du CIL', async function() {
        this.timeout(60000);

        // wp_pass/test.c est deja tres annote en ACSL
        await openBenchmark('wp_pass/test.c');
        await activateExtension();

        const outFile = path.join(getWorkspacePath(), '.frama-c', 'fc_test.c');
        cleanFile(outFile);

        await vscode.commands.executeCommand('displayCIL_noannot');
        const cil = await waitForFileWritten(outFile);

        // le code C reste
        assert.ok(cil.includes('swap'),      'fonction swap absente');
        assert.ok(cil.includes('increment'), 'fonction increment absente');

        // mais AUCUNE annotation ACSL
        assert.ok(!cil.includes('/*@'),      'marqueur /*@ encore present');
        assert.ok(!cil.includes('requires'), 'clause requires encore presente');
        assert.ok(!cil.includes('ensures'),  'clause ensures encore presente');
        assert.ok(!cil.includes('assigns'),  'clause assigns encore presente');
    });
});