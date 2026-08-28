import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';
import { openBenchmark, activateExtension, waitForFileWritten, cleanFile, getWorkspacePath } from './helpers';

suite('ccdoc', () => {

    test('genere le fichier LaTeX fc_ccdoc.tex', async function() {
        this.timeout(120000);

        await openBenchmark('ccdoc/test.c');
        await activateExtension();

        const outFile = path.join(getWorkspacePath(), '.frama-c', 'fc_ccdoc.tex');
        cleanFile(outFile);

        await vscode.commands.executeCommand('ccdoc');

       
        const start = Date.now();
        let content = '';
        while (Date.now() - start < 90000) {
            try {
                const fs = require('fs');
                if (fs.existsSync(outFile)) {
                    content = fs.readFileSync(outFile, 'utf-8');
                    if (content.length > 0) break;
                }
            } catch (_) {}
            await new Promise(r => setTimeout(r, 1000));
        }

        console.log(`fc_ccdoc.tex taille : ${content.length} chars`);

        assert.ok(content.length > 0, 'fc_ccdoc.tex vide ou absent');
        assert.ok(
            content.includes('\\') || content.includes('section') || content.includes('begin'),
            'fc_ccdoc.tex ne semble pas etre du LaTeX valide'
        );
    });
});