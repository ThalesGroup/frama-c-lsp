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

    // logs de diagnostic
    console.log('workspace:', getWorkspacePath());
    console.log('__dirname:', __dirname);

    const settingsPath = path.join(getWorkspacePath(), '.vscode', 'settings.json');
    const fs = require('fs');
    console.log('settingsPath:', settingsPath);
    console.log('settings.json existe:', fs.existsSync(settingsPath));
    if (fs.existsSync(settingsPath)) {
        const content = fs.readFileSync(settingsPath, 'utf-8');
        console.log('settings.json contenu:', content);
    }

    // liste les fichiers dans le workspace
    console.log('contenu workspace:', fs.readdirSync(getWorkspacePath()));

    const outFile = path.join(getWorkspacePath(), '.frama-c', 'fc_metrics.txt');
    console.log('outFile attendu:', outFile);
    cleanFile(outFile);

    // declenche la commande
    console.log('lancement showGlobalMetrics...');
    await vscode.commands.executeCommand('showGlobalMetrics');
    console.log('showGlobalMetrics retourne');

    // attend 5 secondes et liste .frama-c
    await new Promise(r => setTimeout(r, 5000));
    const framaCDir = path.join(getWorkspacePath(), '.frama-c');
    if (fs.existsSync(framaCDir)) {
        console.log('contenu .frama-c:', fs.readdirSync(framaCDir));
    } else {
        console.log('.frama-c nexiste pas dans le workspace');
    }

    const report = await waitForFileWritten(outFile, 90000);
    console.log(`fc_metrics.txt taille : ${report.length} chars`);
    assert.ok(report.length > 0, 'fc_metrics.txt vide');
});
});