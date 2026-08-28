import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import { openBenchmark, activateExtension, waitForFileWritten, cleanFile, getWorkspacePath } from './helpers';

suite('showGlobalMetrics', () => {

    test('produit un rapport de metriques global sur plusieurs fichiers', async function() {
    this.timeout(120000);


    await openBenchmark('global_metrics/main.c');
    await activateExtension();

    const outFile = path.join(getWorkspacePath(), '.frama-c', 'fc_metrics.txt');
    cleanFile(outFile);

    await vscode.commands.executeCommand('showGlobalMetrics');
    const report = await waitForFileWritten(outFile, 90000);

    console.log(`fc_metrics.txt taille : ${report.length} chars`);
    console.log(report.substring(0, 500));

    assert.ok(report.length > 0, 'fc_metrics.txt vide');
    assert.ok(
        report.includes('add') || report.includes('multiply') || report.includes('compute'),
        'aucune fonction de global_metrics dans le rapport'
    );
    const hasMetricKeyword = /sloc|cyclomatic|ifs|loops|calls|function/i.test(report);
    assert.ok(hasMetricKeyword, 'aucun indicateur de metrics trouve');
});
});