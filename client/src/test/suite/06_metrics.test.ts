import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';
import { openBenchmark, activateExtension, waitForFileWritten, cleanFile, getWorkspacePath } from './helpers';

suite('showLocalMetrics', () => {

    test('produces a per-function metrics report', async function() {
        this.timeout(60000);

        await openBenchmark('metrics/test.c');
        await activateExtension();

        const outFile = path.join(getWorkspacePath(), '.frama-c', 'fc_metrics.txt');
        cleanFile(outFile);

        await vscode.commands.executeCommand('showLocalMetrics');
        const report = await waitForFileWritten(outFile);

        // all 4 functions must appear in the report
        assert.ok(report.includes('trivial'),   'function trivial missing from report');
        assert.ok(report.includes('branchy'),   'function branchy missing from report');
        assert.ok(report.includes('with_loop'), 'function with_loop missing from report');
        assert.ok(report.includes('ptr_ops'),   'function ptr_ops missing from report');

        // report must contain standard metric indicators
        // (frama-c metrics generates: sloc, ifs, loops, calls, pointer dereferencing, cyclomatic)
        const hasMetricKeyword = /sloc|cyclomatic|ifs|loops|calls/i.test(report);
        assert.ok(hasMetricKeyword, 'no standard metric indicator found');
    });
});