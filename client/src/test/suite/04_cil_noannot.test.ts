import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';
import { openBenchmark, activateExtension, waitForFileWritten, cleanFile, getWorkspacePath } from './helpers';

suite('displayCIL_noannot', () => {

    test('removes ACSL annotations from CIL', async function() {
        this.timeout(60000);

        // wp_pass/test.c is heavily annotated with ACSL
        await openBenchmark('wp_pass/test.c');
        await activateExtension();

        const outFile = path.join(getWorkspacePath(), '.frama-c', 'fc_test.c');
        cleanFile(outFile);

        await vscode.commands.executeCommand('displayCIL_noannot');
        const cil = await waitForFileWritten(outFile);

        // C code is preserved
        assert.ok(cil.includes('swap'),      'function swap missing');
        assert.ok(cil.includes('increment'), 'function increment missing');

        // but NO ACSL annotations
        assert.ok(!cil.includes('/*@'),      'ACSL marker /*@ still present');
        assert.ok(!cil.includes('requires'), 'requires clause still present');
        assert.ok(!cil.includes('ensures'),  'ensures clause still present');
        assert.ok(!cil.includes('assigns'),  'assigns clause still present');
    });
});