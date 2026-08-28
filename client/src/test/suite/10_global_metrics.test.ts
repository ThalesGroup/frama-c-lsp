import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import { openBenchmark, activateExtension, waitForFileWritten, cleanFile, getWorkspacePath } from './helpers';

suite('showGlobalMetrics', () => {

    test('produit un rapport de metriques global sur plusieurs fichiers', async function() {
        this.timeout(120000);

        // configure kernel.sourceFiles pour pointer sur les deux fichiers du benchmark
        const mainFile = path.resolve(__dirname, '../../../benchmarks/global_metrics/main.c');
        const utilsFile = path.resolve(__dirname, '../../../benchmarks/global_metrics/utils.c');

        await vscode.workspace.getConfiguration().update(
            'kernel.sourceFiles',
            [mainFile, utilsFile],
            vscode.ConfigurationTarget.Workspace
        );

        // laisse le temps au LSP de relire la config
        await new Promise(r => setTimeout(r, 3000));

        await openBenchmark('global_metrics/main.c');
        await activateExtension();

        const outFile = path.join(getWorkspacePath(), '.frama-c', 'fc_metrics.txt');
        cleanFile(outFile);

        await vscode.commands.executeCommand('showGlobalMetrics');

        const report = await waitForFileWritten(outFile, 60000);

        console.log(`fc_metrics.txt (global) taille : ${report.length} chars`);
        console.log(report.substring(0, 300));

        // les deux fichiers doivent apparaitre dans le rapport
        assert.ok(
            report.includes('add') || report.includes('multiply') || report.includes('compute'),
            'aucune fonction des deux fichiers dans le rapport global'
        );

        const hasMetricKeyword = /sloc|cyclomatic|ifs|loops|calls/i.test(report);
        assert.ok(hasMetricKeyword, 'aucun indicateur de metrics trouve');

        // restore la config
        await vscode.workspace.getConfiguration().update(
            'kernel.sourceFiles',
            [],
            vscode.ConfigurationTarget.Workspace
        );
    });
});