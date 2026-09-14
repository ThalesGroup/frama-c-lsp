import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';
import { openBenchmark, activateExtension, waitForFileWritten, cleanFile, getWorkspacePath } from './helpers';

suite('computeCG', () => {

    test('generates .dot graph with correct nodes and edges', async function() {
        this.timeout(60000);

        await openBenchmark('callgraph/test.c');
        await activateExtension();

        // server writes the .dot file (text) then converts to .dot.pdf
        // we test the .dot file which is parsable
        const dotFile = path.join(getWorkspacePath(), '.frama-c', 'fc_test.dot');
        cleanFile(dotFile);
        cleanFile(dotFile + '.pdf');

        await vscode.commands.executeCommand('computeCG');
        const dot = await waitForFileWritten(dotFile);

        // graphviz format
        assert.ok(dot.includes('digraph') || dot.includes('graph'), 'not a valid .dot file');

        // the 4 connected functions must appear as nodes
        assert.ok(dot.includes('helper'),    'node helper missing');
        assert.ok(dot.includes('process_a'), 'node process_a missing');
        assert.ok(dot.includes('process_b'), 'node process_b missing');
        assert.ok(dot.includes('entry'),     'node entry missing');

        // edge entry -> process_a (dot syntax: "->" between the two)
        const hasEntryToA = /entry.*->.*process_a|process_a.*<-.*entry/.test(dot);
        assert.ok(hasEntryToA, 'edge entry -> process_a missing');
    });
});