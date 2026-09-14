import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';
import { openBenchmark, activateExtension, cleanFile, getWorkspacePath } from './helpers';

suite('ccdoc', () => {

    test('generates the LaTeX file fc_ccdoc.tex', async function() {
        this.timeout(120000);

        await openBenchmark('ccdoc/test.c');
        await activateExtension();

        const outFile = path.join(getWorkspacePath(), '.frama-c', 'fc_ccdoc.tex');
        cleanFile(outFile);

        await vscode.commands.executeCommand('ccdoc');

        // poll the file — ccdoc uses synchronous execute_command
        // the .tex file is written directly by the ccdoc plugin (no placeholder)
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

        console.log(`fc_ccdoc.tex size: ${content.length} chars`);

        assert.ok(content.length > 0, 'fc_ccdoc.tex is empty or missing');
        assert.ok(
            content.includes('\\') || content.includes('section') || content.includes('begin'),
            'fc_ccdoc.tex does not appear to be valid LaTeX'
        );
    });
});