import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';

suite('Test DisplayAST', () => {

    test('DisplayAST retourne les 4 catégories AST', async function() {
        this.timeout(60000);

        const testFilePath = path.resolve(__dirname, '../../../../benchmarks/01_display_ast/test.c');
        const uri = vscode.Uri.file(testFilePath);
        const document = await vscode.workspace.openTextDocument(uri);
        await vscode.window.showTextDocument(document);

        const extension = vscode.extensions.getExtension('innov-org.acsl-lsp')!;
        await extension.activate();

        console.log("Attente initialisation serveur OCaml...");
        await new Promise(resolve => setTimeout(resolve, 10000));

        console.log("--- DÉBUT TEST DisplayAST ---");

        const response: any = await vscode.commands.executeCommand('DisplayAST');

        console.log("Réponse brute:", JSON.stringify(response, null, 2));

        // ── Assertions ──────────────────────────────────────────────

        assert.ok(response, "La réponse ne doit pas être undefined");

        // 1. Les 4 catégories existent
        assert.ok(response.functions,   "La catégorie 'functions' doit exister");
        assert.ok(response.globals,     "La catégorie 'globals' doit exister");
        assert.ok(response.types,       "La catégorie 'types' doit exister");
        assert.ok(response.annotations, "La catégorie 'annotations' doit exister");

        // 2. Les catégories ne sont pas vides
        assert.ok(response.functions.length > 0,
            `functions vide - reçu ${response.functions.length}`);
        assert.ok(response.globals.length > 0,
            `globals vide - reçu ${response.globals.length}`);
        assert.ok(response.types.length > 0,
            `types vide - reçu ${response.types.length}`);
        assert.ok(response.annotations.length > 0,
            `annotations vide - reçu ${response.annotations.length}`);

        // 3. Fonctions spécifiques présentes
        const funcNames = response.functions.map((f: any) => f.name);
        assert.ok(funcNames.includes('add'),       "Fonction 'add' manquante");
        assert.ok(funcNames.includes('fill_zero'), "Fonction 'fill_zero' manquante");
        assert.ok(funcNames.includes('factorial'), "Fonction 'factorial' manquante");
        assert.ok(funcNames.includes('max'),       "Fonction 'max' manquante");

        // 4. Globals spécifiques présents
        const globalNames = response.globals.map((g: any) => g.name);
        assert.ok(globalNames.includes('g_counter'), "Global 'g_counter' manquant");
        assert.ok(globalNames.includes('g_MAX'),     "Global 'g_MAX' manquant");

        // 5. Types spécifiques présents
        const typeNames = response.types.map((t: any) => t.name);
        assert.ok(typeNames.includes('point_t'),  "Type 'point_t' manquant");
        assert.ok(typeNames.includes('status_t'), "Type 'status_t' manquant");
        assert.ok(typeNames.includes('data_t'),   "Type 'data_t' manquant");

        // 6. Annotations spécifiques présentes
        const annotNames = response.annotations.map((a: any) => a.name);
        assert.ok(annotNames.includes('valid_buffer'), "Predicate 'valid_buffer' manquant");
        assert.ok(annotNames.includes('max_commut'),   "Lemma 'max_commut' manquant");
        assert.ok(annotNames.includes('SumProps'),     "Axiomatic 'SumProps' manquant");

        console.log(`✔ functions  : ${response.functions.length}`);
        console.log(`✔ globals    : ${response.globals.length}`);
        console.log(`✔ types      : ${response.types.length}`);
        console.log(`✔ annotations: ${response.annotations.length}`);
        console.log("--- FIN TEST DisplayAST ---");
    });
});