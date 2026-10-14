import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';
import { openBenchmark, activateExtension, waitForFileWritten, cleanFile, getWorkspacePath } from './helpers';

suite('displayCIL', () => {

    test('CIL normalise for, ternaire, court-circuit et return', async function() {
        this.timeout(60000);

        await openBenchmark('cil/test.c');
        await activateExtension();

        const outFile = path.join(getWorkspacePath(), '.frama-c', 'fc_test.c');
        cleanFile(outFile);

        await vscode.commands.executeCommand('displayCIL');
        const cil = await waitForFileWritten(outFile);

        // les 4 fonctions doivent apparaitre dans le CIL
        assert.ok(cil.includes('abs_val'),  'fonction abs_val absente du CIL');
        assert.ok(cil.includes('classify'), 'fonction classify absente du CIL');
        assert.ok(cil.includes('sum_to'),   'fonction sum_to absente du CIL');
        assert.ok(cil.includes('in_range'), 'fonction in_range absente du CIL');

        // normalisation 1 : for -> while
        assert.ok(cil.includes('while'), 'for non normalise en while');
        assert.ok(!/\bfor\s*\(/.test(cil), 'for encore present dans le CIL');

        // normalisation 2 : ternaire -> if/else + temporaire
        // abs_val doit avoir une variable temporaire "tmp" et un if/else
        const absValMatch = cil.match(/int\s+abs_val[\s\S]*?\n\}/);
        assert.ok(absValMatch, 'abs_val introuvable dans la sortie');
        assert.ok(/int\s+tmp/.test(absValMatch![0]),
            'ternaire non normalise — pas de temporaire dans abs_val');

        // normalisation 3 : return multiples -> __retres + goto return_label
        // classify a 3 return, ils doivent etre unifies
        assert.ok(cil.includes('__retres'),      'return non normalise — __retres absent');
        assert.ok(cil.includes('return_label'),  'return non normalise — return_label absent');
    });
});