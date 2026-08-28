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

    console.log('mainFile existe:', fs.existsSync(mainFile));
    console.log('workspace:', getWorkspacePath());

    // le settings.json du workspace de test est dans testFixture/.vscode/
    const settingsDir = path.join(getWorkspacePath(), '.vscode');
    const settingsPath = path.join(settingsDir, 'settings.json');
    console.log('settingsPath:', settingsPath);

    // crée le dossier .vscode si absent
    if (!fs.existsSync(settingsDir)) {
        fs.mkdirSync(settingsDir, { recursive: true });
    }

    // lit ou crée les settings
    let currentSettings: any = {};
    if (fs.existsSync(settingsPath)) {
        currentSettings = JSON.parse(fs.readFileSync(settingsPath, 'utf-8'));
    }
    const originalSourceFiles = currentSettings['kernel.sourceFiles'];

    // écrit les nouveaux settings
    const newSettings = {
        ...currentSettings,
        'kernel.sourceFiles': [mainFile, utilsFile]
    };
    fs.writeFileSync(settingsPath, JSON.stringify(newSettings, null, 4));
    console.log('settings.json écrit dans:', settingsPath);

    // attend le rechargement
    await new Promise(r => setTimeout(r, 12000));

    await openBenchmark('global_metrics/main.c');
    await activateExtension();

    const outFile = path.join(getWorkspacePath(), '.frama-c', 'fc_metrics.txt');
    cleanFile(outFile);

    await vscode.commands.executeCommand('showGlobalMetrics');
    const report = await waitForFileWritten(outFile, 90000);

    console.log(`fc_metrics.txt taille : ${report.length} chars`);
    console.log(report.substring(0, 500));

    // restore
    const restoredSettings = { ...newSettings, 'kernel.sourceFiles': originalSourceFiles };
    fs.writeFileSync(settingsPath, JSON.stringify(restoredSettings, null, 4));

    assert.ok(report.length > 0, 'fc_metrics.txt vide');
    assert.ok(
        report.includes('add') || report.includes('multiply') || report.includes('compute'),
        'aucune fonction de global_metrics dans le rapport'
    );
    const hasMetricKeyword = /sloc|cyclomatic|ifs|loops|calls|function/i.test(report);
    assert.ok(hasMetricKeyword, 'aucun indicateur de metrics trouve');
});
});