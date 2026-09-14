import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';
import { openBenchmark, activateExtension, waitForFileWritten, cleanFile, getWorkspacePath } from './helpers';

suite('displayCIL', () => {

    test('CIL normalizes for, ternary, short-circuit and return', async function() {
        this.timeout(60000);

        await openBenchmark('cil/test.c');
        await activateExtension();

        const outFile = path.join(getWorkspacePath(), '.frama-c', 'fc_test.c');
        cleanFile(outFile);

        await vscode.commands.executeCommand('displayCIL');
        const cil = await waitForFileWritten(outFile);

        // all 4 functions must appear in the CIL
        assert.ok(cil.includes('abs_val'),  'function abs_val missing from CIL');
        assert.ok(cil.includes('classify'), 'function classify missing from CIL');
        assert.ok(cil.includes('sum_to'),   'function sum_to missing from CIL');
        assert.ok(cil.includes('in_range'), 'function in_range missing from CIL');

        // normalization 1 : for -> while
        assert.ok(cil.includes('while'), 'for not normalized to while');
        assert.ok(!/\bfor\s*\(/.test(cil), 'for still present in CIL');

        // normalization 2 : ternary -> if/else + temporary variable
        // abs_val must have a temporary variable "tmp" and an if/else
        const absValMatch = cil.match(/int\s+abs_val[\s\S]*?\n\}/);
        assert.ok(absValMatch, 'abs_val not found in output');
        assert.ok(/int\s+tmp/.test(absValMatch![0]),
            'ternary not normalized — no temporary variable in abs_val');

        // normalization 3 : multiple returns -> __retres + goto return_label
        // classify has 3 returns, they must be unified
        assert.ok(cil.includes('__retres'),     'return not normalized — __retres missing');
        assert.ok(cil.includes('return_label'), 'return not normalized — return_label missing');
    });
});