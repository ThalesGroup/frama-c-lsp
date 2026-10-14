import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import { openBenchmark, activateExtension, cleanFile, getWorkspacePath } from './helpers';

suite('showGlobalMetrics', () => {

    test('produit un rapport de metriques global sur plusieurs fichiers', async function() {
        this.timeout(120000);

        await openBenchmark('global_metrics/main.c');
        await activateExtension();

        console.log('workspace:', getWorkspacePath());

        const outFile = path.join(getWorkspacePath(), '.frama-c', 'fc_metrics.txt');
        console.log('outFile:', outFile);
        cleanFile(outFile);
        console.log('fichier supprime, existe:', fs.existsSync(outFile));

        await vscode.commands.executeCommand('showGlobalMetrics');
        console.log('showGlobalMetrics retourne');

        // poll manuel avec logs detailles
        const start = Date.now();
        let report = '';
        while (Date.now() - start < 90000) {
            if (fs.existsSync(outFile)) {
                const content = fs.readFileSync(outFile, 'utf-8');
                console.log(`[${Math.round((Date.now() - start) / 1000)}s] fichier existe, taille: ${content.length}`);
                if (content.length > 0) {
                    console.log('debut brut:', JSON.stringify(content.substring(0, 100)));
                    console.log('startsWith Task:', content.startsWith('Task in progress'));
                    if (!content.startsWith('Task in progress')) {
                        report = content;
                        break;
                    }
                }
            } else {
                if (Math.round((Date.now() - start) / 1000) % 10 === 0) {
                    console.log(`[${Math.round((Date.now() - start) / 1000)}s] fichier absent`);
                }
            }
            await new Promise(r => setTimeout(r, 1000));
        }

        console.log(`rapport final taille: ${report.length}`);
        console.log(report.substring(0, 500));

        assert.ok(report.length > 0, 'fc_metrics.txt vide ou jamais ecrit correctement');
        assert.ok(
            report.includes('add') || report.includes('multiply') || report.includes('compute'),
            'aucune fonction de global_metrics dans le rapport'
        );
        const hasMetricKeyword = /sloc|cyclomatic|ifs|loops|calls|function/i.test(report);
        assert.ok(hasMetricKeyword, 'aucun indicateur de metrics trouve');
    });
});