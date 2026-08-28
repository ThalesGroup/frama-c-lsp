import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import { openBenchmark, activateExtension, waitForFileWritten, cleanFile, getWorkspacePath } from './helpers';

suite('showGlobalMetrics', () => {

    test('produit un rapport de metriques global sur plusieurs fichiers', async function() {
        this.timeout(150000);

        const mainFile = path.resolve(__dirname, '../../../benchmarks/global_metrics/main.c');
        const utilsFile = path.resolve(__dirname, '../../../benchmarks/global_metrics/utils.c');
        const wpPassFile = path.resolve(__dirname, '../../../benchmarks/wp_pass/test.c');

        console.log('mainFile:', mainFile);
        console.log('mainFile existe:', fs.existsSync(mainFile));

        // modifie directement le fichier settings.json sur disque
        // c'est la façon la plus fiable de déclencher didChangeConfiguration
        const settingsPath = path.join(getWorkspacePath(), '.vscode', 'settings.json');
        console.log('settingsPath:', settingsPath);

        // lit les settings actuels
        const currentSettings = JSON.parse(fs.readFileSync(settingsPath, 'utf-8'));
        const originalSourceFiles = currentSettings['kernel.sourceFiles'];

        // écrit les nouveaux settings avec les fichiers global_metrics
        const newSettings = {
            ...currentSettings,
            'kernel.sourceFiles': [mainFile, utilsFile]
        };
        fs.writeFileSync(settingsPath, JSON.stringify(newSettings, null, 4));
        console.log('settings.json mis à jour');

        // attend que VSCode détecte le changement et envoie didChangeConfiguration au serveur
        await new Promise(r => setTimeout(r, 10000));

        await openBenchmark('global_metrics/main.c');
        await activateExtension();

        const outFile = path.join(getWorkspacePath(), '.frama-c', 'fc_metrics.txt');
        cleanFile(outFile);

        await vscode.commands.executeCommand('showGlobalMetrics');
        const report = await waitForFileWritten(outFile, 90000);

        console.log(`fc_metrics.txt (global) taille : ${report.length} chars`);
        console.log(report.substring(0, 500));

        // restore les settings originaux
        const restoredSettings = {
            ...newSettings,
            'kernel.sourceFiles': originalSourceFiles
        };
        fs.writeFileSync(settingsPath, JSON.stringify(restoredSettings, null, 4));
        console.log('settings.json restauré');

        assert.ok(report.length > 0, 'fc_metrics.txt vide');
        assert.ok(
            report.includes('add') || report.includes('multiply') || report.includes('compute'),
            'aucune fonction de global_metrics dans le rapport'
        );
        const hasMetricKeyword = /sloc|cyclomatic|ifs|loops|calls|function/i.test(report);
        assert.ok(hasMetricKeyword, 'aucun indicateur de metrics trouve');
    });
});