import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';
import { openBenchmark, activateExtension, waitForFileWritten, cleanFile, getWorkspacePath } from './helpers';

suite('computeCG', () => {

    test('genere le graphe .dot avec les bons noeuds et aretes', async function() {
        this.timeout(60000);

        await openBenchmark('callgraph/test.c');
        await activateExtension();

        // le serveur ecrit le .dot (texte) puis convertit en .dot.pdf.
        // on teste le .dot qui est parsable.
        const dotFile = path.join(getWorkspacePath(), '.frama-c', 'fc_test.dot');
        cleanFile(dotFile);
        cleanFile(dotFile + '.pdf');

        await vscode.commands.executeCommand('computeCG');
        const dot = await waitForFileWritten(dotFile);

        // format graphviz
        assert.ok(dot.includes('digraph') || dot.includes('graph'), 'pas un fichier .dot valide');

        // les 5 fonctions doivent etre des noeuds
        assert.ok(dot.includes('helper'),    'noeud helper absent');
        assert.ok(dot.includes('process_a'), 'noeud process_a absent');
        assert.ok(dot.includes('process_b'), 'noeud process_b absent');
        assert.ok(dot.includes('entry'),     'noeud entry absent');

        // arete entry -> process_a (syntaxe dot : "->" entre les deux)
        const hasEntryToA = /entry.*->.*process_a|process_a.*<-.*entry/.test(dot);
        assert.ok(hasEntryToA, 'arete entry -> process_a absente');
    });
});