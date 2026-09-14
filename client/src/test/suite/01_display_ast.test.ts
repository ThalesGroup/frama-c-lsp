import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';

suite('Test DisplayAST', () => {

    test('DisplayAST returns the 4 AST categories', async function() {
        this.timeout(60000);

        const testFilePath = path.resolve(__dirname, '../../../benchmarks/display_ast/test.c');
        const uri = vscode.Uri.file(testFilePath);
        const document = await vscode.workspace.openTextDocument(uri);
        await vscode.window.showTextDocument(document);

        const extension = vscode.extensions.getExtension('innov-org.acsl-lsp')!;
        await extension.activate();

        console.log("Waiting for OCaml server initialization...");
        await new Promise(resolve => setTimeout(resolve, 10000));

        console.log("--- START DisplayAST TEST ---");

        const response: any = await vscode.commands.executeCommand('DisplayAST');

        console.log("Raw response:", JSON.stringify(response, null, 2));

        // ── Assertions ──────────────────────────────────────────────

        assert.ok(response, "Response must not be undefined");

        // 1. All 4 categories exist
        assert.ok(response.functions,   "Category 'functions' must exist");
        assert.ok(response.globals,     "Category 'globals' must exist");
        assert.ok(response.types,       "Category 'types' must exist");
        assert.ok(response.annotations, "Category 'annotations' must exist");

        // 2. Categories are not empty
        assert.ok(response.functions.length > 0,
            `functions is empty - received ${response.functions.length}`);
        assert.ok(response.globals.length > 0,
            `globals is empty - received ${response.globals.length}`);
        assert.ok(response.types.length > 0,
            `types is empty - received ${response.types.length}`);
        assert.ok(response.annotations.length > 0,
            `annotations is empty - received ${response.annotations.length}`);

        // 3. Specific functions present
        const funcNames = response.functions.map((f: any) => f.name);
        assert.ok(funcNames.includes('add'),        "Function 'add' missing");
        assert.ok(funcNames.includes('fill_zero'),  "Function 'fill_zero' missing");
        assert.ok(funcNames.includes('factorial'),  "Function 'factorial' missing");
        assert.ok(funcNames.includes('public_max'), "Function 'public_max' missing");

        // static function called by public_max → present in AST
        assert.ok(funcNames.includes('max'),
            "Function 'max' (static, called by public_max) must appear");

        // declaration without body → absent from AST (documented Frama-C behavior)
        assert.ok(!funcNames.includes('external_func'),
            "Declaration without body must not appear");

        // 4. Specific globals present
        const globalNames = response.globals.map((g: any) => g.name);
        assert.ok(globalNames.includes('g_counter'), "Global 'g_counter' missing");
        assert.ok(globalNames.includes('g_MAX'),     "Global 'g_MAX' missing");

        // 5. Specific types present
        const typeNames = response.types.map((t: any) => t.name);
        assert.ok(typeNames.includes('point_t'),  "Type 'point_t' missing");
        assert.ok(typeNames.includes('status_t'), "Type 'status_t' missing");
        assert.ok(typeNames.includes('data_t'),   "Type 'data_t' missing");

        // 6. Specific annotations present
        const annotNames = response.annotations.map((a: any) => a.name);
        assert.ok(annotNames.includes('valid_buffer'), "Predicate 'valid_buffer' missing");
        assert.ok(annotNames.includes('max_commut'),   "Lemma 'max_commut' missing");
        assert.ok(annotNames.includes('SumProps'),     "Axiomatic 'SumProps' missing");

        console.log(`✔ functions  : ${response.functions.length}`);
        console.log(`✔ globals    : ${response.globals.length}`);
        console.log(`✔ types      : ${response.types.length}`);
        console.log(`✔ annotations: ${response.annotations.length}`);
        console.log("--- END DisplayAST TEST ---");
    });
});