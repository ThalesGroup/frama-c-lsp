test('produit un rapport de metriques global sur plusieurs fichiers', async function() {
    this.timeout(120000);

    // chemins relatifs par rapport au workspace (comme dans settings.json)
    await vscode.workspace.getConfiguration().update(
        'kernel.sourceFiles',
        [
            './benchmarks/global_metrics/main.c',
            './benchmarks/global_metrics/utils.c'
        ],
        vscode.ConfigurationTarget.Workspace
    );

    // attente plus longue pour que le serveur OCaml recharge la config
    await new Promise(r => setTimeout(r, 8000));

    await openBenchmark('global_metrics/main.c');
    await activateExtension();

    const outFile = path.join(getWorkspacePath(), '.frama-c', 'fc_metrics.txt');
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
        ['./wp_pass/test.c'],
        vscode.ConfigurationTarget.Workspace
    );
});