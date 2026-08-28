
import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import { openBenchmark, activateExtension, waitForFileWritten, cleanFile, getWorkspacePath } from './helpers';

suite('showGlobalMetrics', () => {
test('produit un rapport de metriques global sur plusieurs fichiers', async function() {
    this.timeout(150000);

    const workspacePath = getWorkspacePath();
    const mainFile = path.join(workspacePath, 'benchmarks', 'global_metrics', 'main.c');
    const utilsFile = path.join(workspacePath, 'benchmarks', 'global_metrics', 'utils.c');

    // verifie que les fichiers existent
    const fs = require('fs');
    console.log('mainFile existe:', fs.existsSync(mainFile));
    console.log('utilsFile existe:', fs.existsSync(utilsFile));

    await vscode.workspace.getConfiguration().update(
        'kernel.sourceFiles',
        [mainFile, utilsFile],
        vscode.ConfigurationTarget.Workspace
    );

    // attente longue pour que le serveur OCaml recharge la config
    await new Promise(r => setTimeout(r, 15000));

    await openBenchmark('global_metrics/main.c');
    await activateExtension();

    const outFile = path.join(workspacePath, '.frama-c', 'fc_metrics.txt');
    cleanFile(outFile);

    await vscode.commands.executeCommand('showGlobalMetrics');
    const report = await waitForFileWritten(outFile, 90000);

    console.log(`fc_metrics.txt (global) taille : ${report.length} chars`);
    console.log(report.substring(0, 400));

    assert.ok(
        report.includes('add') || report.includes('multiply') || report.includes('compute'),
        'aucune fonction de global_metrics dans le rapport'
    );

    const hasMetricKeyword = /sloc|cyclomatic|ifs|loops|calls|function/i.test(report);
    assert.ok(hasMetricKeyword, 'aucun indicateur de metrics trouve');

    // restore
    await vscode.workspace.getConfiguration().update(
        'kernel.sourceFiles',
        [path.join(workspacePath, 'benchmarks', 'wp_pass', 'test.c')],
        vscode.ConfigurationTarget.Workspace
    );
});})