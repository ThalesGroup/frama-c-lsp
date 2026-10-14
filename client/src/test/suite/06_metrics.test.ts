import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';
import { openBenchmark, activateExtension, waitForFileWritten, cleanFile, getWorkspacePath } from './helpers';

suite('showLocalMetrics', () => {

    test('produit un rapport de metriques par fonction', async function() {
        this.timeout(60000);

        await openBenchmark('metrics/test.c');
        await activateExtension();

        const outFile = path.join(getWorkspacePath(), '.frama-c', 'fc_metrics.txt');
        cleanFile(outFile);

        await vscode.commands.executeCommand('showLocalMetrics');
        const report = await waitForFileWritten(outFile);

        // les 4 fonctions doivent etre mentionnees
        assert.ok(report.includes('trivial'),   'fonction trivial absente du rapport');
        assert.ok(report.includes('branchy'),   'fonction branchy absente du rapport');
        assert.ok(report.includes('with_loop'), 'fonction with_loop absente du rapport');
        assert.ok(report.includes('ptr_ops'),   'fonction ptr_ops absente du rapport');

        // le rapport doit contenir des indicateurs classiques
        // (frama-c metrics genere: sloc, ifs, loops, calls, pointer dereferencing, cyclomatic)
        const hasMetricKeyword = /sloc|cyclomatic|ifs|loops|calls/i.test(report);
        assert.ok(hasMetricKeyword, 'aucun indicateur classique de metrics trouve');
    });
});