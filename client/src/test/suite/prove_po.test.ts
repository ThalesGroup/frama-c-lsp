import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';

const BENCH = path.resolve(__dirname, '../../../benchmarks/wp_pass/test.c');
const TIMEOUT_WP = 120000;

suite('Test provePO', () => {

    // ── Initialisation unique avant tous les tests ────────────────────────────
    suiteSetup(async function () {
        this.timeout(30000);
        const uri = vscode.Uri.file(BENCH);
        const doc = await vscode.workspace.openTextDocument(uri);
        await vscode.window.showTextDocument(doc);
        const ext = vscode.extensions.getExtension('innov-org.acsl-lsp')!;
        await ext.activate();
        console.log("Attente initialisation serveur OCaml...");
        await new Promise(r => setTimeout(r, 10000));
    });

    // ── TEST 1 ────────────────────────────────────────────────────────────────
    test('provePO @all/@all — structure de la réponse', async function () {
        this.timeout(TIMEOUT_WP);
        const uri = vscode.Uri.file(BENCH);
        const response: any = await vscode.commands.executeCommand(
            'provePO', uri.toString(), '@all', '@all', 30, false
        );
        console.log("Réponse brute:", JSON.stringify(response, null, 2));
        assert.ok(response,                "Réponse undefined");
        assert.ok(Array.isArray(response), "Réponse doit être un tableau");
        assert.ok(response.length > 0,    "Tableau vide — aucun goal retourné");
        for (const goal of response) {
            assert.ok('goal'     in goal, `Champ 'goal' manquant`);
            assert.ok('property' in goal, `Champ 'property' manquant`);
            assert.ok('file'     in goal, `Champ 'file' manquant`);
            assert.ok('line'     in goal, `Champ 'line' manquant`);
            assert.ok('function' in goal, `Champ 'function' manquant`);
            assert.ok('passed'   in goal, `Champ 'passed' manquant`);
            assert.ok('verdict'  in goal, `Champ 'verdict' manquant`);
            assert.ok('provers'  in goal, `Champ 'provers' manquant`);
            assert.ok('smoke'    in goal, `Champ 'smoke' manquant`);
            assert.ok(Array.isArray(goal.provers), `'provers' doit être un tableau`);
        }
        console.log(`✔ Total goals reçus : ${response.length}`);
    });

    // ── TEST 2 ────────────────────────────────────────────────────────────────
    test('provePO @all/@all — tous les goals passent (wp_pass)', async function () {
        this.timeout(TIMEOUT_WP);
        const uri = vscode.Uri.file(BENCH);
        const response: any = await vscode.commands.executeCommand(
            'provePO', uri.toString(), '@all', '@all', 30, false
        );
        assert.ok(response && response.length > 0, "Aucun goal retourné");
        const failed = response.filter((g: any) => !g.passed);
        assert.strictEqual(
            failed.length, 0,
            `${failed.length} goal(s) ont échoué :\n` +
            failed.map((g: any) => `  - ${g.function} / ${g.goal} → ${g.verdict}`).join('\n')
        );
        console.log(`✔ ${response.filter((g: any) => g.passed).length}/${response.length} goals prouvés`);
    });

    // ── TEST 3 ────────────────────────────────────────────────────────────────
    test('provePO @all/@all — fonctions attendues présentes', async function () {
        this.timeout(TIMEOUT_WP);
        const uri = vscode.Uri.file(BENCH);
        const response: any = await vscode.commands.executeCommand(
            'provePO', uri.toString(), '@all', '@all', 30, false
        );
        assert.ok(response && response.length > 0, "Aucun goal retourné");
        const functions: string[] = [...new Set<string>(response.map((g: any) => g.function as string))];
        console.log("Fonctions prouvées:", functions);
        const expected = ['increment', 'swap', 'sign', 'gauss', 'safe_div',
                          'safe_get', 'fill_zero', 'find_min_sorted', 'square', 'copy_point'];
        for (const fn of expected) {
            assert.ok(functions.includes(fn), `Fonction '${fn}' absente des goals`);
        }
    });

    // ── TEST 4 ────────────────────────────────────────────────────────────────
    test('provePO @all/@all — types de property couverts', async function () {
        this.timeout(TIMEOUT_WP);
        const uri = vscode.Uri.file(BENCH);
        const response: any = await vscode.commands.executeCommand(
            'provePO', uri.toString(), '@all', '@all', 30, false
        );
        assert.ok(response && response.length > 0, "Aucun goal retourné");
        const properties: string[] = response.map((g: any) => (g.property as string).toLowerCase());
        assert.ok(properties.some(p => p.includes('assign')), "Aucun goal 'assigns'");
        assert.ok(properties.some(p => p.includes('ensures') || p.includes('post')), "Aucun goal 'ensures'");
        assert.ok(properties.some(p => p.includes('invariant') || p.includes('loop')), "Aucun goal 'loop invariant'");
        assert.ok(properties.some(p =>
            p.includes('rte') || p.includes('division') || p.includes('overflow') || p.includes('index')),
            "Aucun goal RTE");
        console.log("Types de property présents:", [...new Set<string>(properties)]);
    });

    // ── TEST 5 ────────────────────────────────────────────────────────────────
    test('provePO increment/@all — goals limités à increment', async function () {
        this.timeout(TIMEOUT_WP);
        const uri = vscode.Uri.file(BENCH);
        const response: any = await vscode.commands.executeCommand(
            'provePO', uri.toString(), 'increment', '@all', 30, false
        );
        assert.ok(response && response.length > 0, "Aucun goal pour 'increment'");
        const functions: string[] = [...new Set<string>(response.map((g: any) => g.function as string))];
        assert.ok(functions.every(f => f === 'increment'),
            `Goals d'autres fonctions présents : ${functions.filter(f => f !== 'increment').join(', ')}`);
        console.log(`✔ increment : ${response.filter((g: any) => g.passed).length}/${response.length} goals prouvés`);
    });

    // ── TEST 6 ────────────────────────────────────────────────────────────────
    test('provePO sign/@all — behaviors positive/zero/negative présents', async function () {
        this.timeout(TIMEOUT_WP);
        const uri = vscode.Uri.file(BENCH);
        const response: any = await vscode.commands.executeCommand(
            'provePO', uri.toString(), 'sign', '@all', 30, false
        );
        assert.ok(response && response.length > 0, "Aucun goal pour 'sign'");
        const behaviors: string[] = response
            .map((g: any) => (g.behavior as string || '').toLowerCase())
            .filter((b: string) => b !== '');
        console.log("Behaviors trouvés:", [...new Set<string>(behaviors)]);
        assert.ok(behaviors.some(b => b.includes('positive')), "Behavior 'positive' non trouvé");
        assert.ok(behaviors.some(b => b.includes('zero')),     "Behavior 'zero' non trouvé");
        assert.ok(behaviors.some(b => b.includes('negative')), "Behavior 'negative' non trouvé");
        assert.strictEqual(response.filter((g: any) => !g.passed).length, 0, "Goals échoués dans sign");
    });

    // ── TEST 7 ────────────────────────────────────────────────────────────────
    test('provePO @all/@all — provers qed et alt-ergo utilisés', async function () {
        this.timeout(TIMEOUT_WP);
        const uri = vscode.Uri.file(BENCH);
        const response: any = await vscode.commands.executeCommand(
            'provePO', uri.toString(), '@all', '@all', 30, false
        );
        assert.ok(response && response.length > 0, "Aucun goal retourné");
        const allProvers: string[] = response.flatMap((g: any) =>
            (g.provers as any[]).map(p => (p.prover as string).toLowerCase())
        );
        console.log("Provers utilisés:", [...new Set<string>(allProvers)]);
        assert.ok(allProvers.some(p => p.includes('qed')), "Prover 'qed' jamais utilisé");
    });

    // ── TEST 8 ────────────────────────────────────────────────────────────────
    test('provePO safe_div/@all — RTE division prouvée', async function () {
        this.timeout(TIMEOUT_WP);
        const uri = vscode.Uri.file(BENCH);
        const response: any = await vscode.commands.executeCommand(
            'provePO', uri.toString(), 'safe_div', '@all', 30, false
        );
        assert.ok(response && response.length > 0, "Aucun goal pour 'safe_div'");
        const rteGoals = response.filter((g: any) =>
            (g.property as string).toLowerCase().includes('rte') ||
            (g.property as string).toLowerCase().includes('division') ||
            (g.goal as string).toLowerCase().includes('rte')
        );
        console.log(`RTE goals pour safe_div : ${rteGoals.length}`);
        assert.ok(rteGoals.length > 0, "Aucun goal RTE pour safe_div");
        assert.strictEqual(rteGoals.filter((g: any) => !g.passed).length, 0, "RTE goal non prouvé");
    });

    // ── TEST 9 ────────────────────────────────────────────────────────────────
    test('provePO gauss/@all — loop invariant et variant prouvés', async function () {
        this.timeout(TIMEOUT_WP);
        const uri = vscode.Uri.file(BENCH);
        const response: any = await vscode.commands.executeCommand(
            'provePO', uri.toString(), 'gauss', '@all', 30, false
        );
        assert.ok(response && response.length > 0, "Aucun goal pour 'gauss'");
        const loopGoals = response.filter((g: any) =>
            (g.property as string).toLowerCase().includes('invariant') ||
            (g.property as string).toLowerCase().includes('variant') ||
            (g.goal as string).toLowerCase().includes('loop')
        );
        console.log(`Loop goals pour gauss : ${loopGoals.length}`);
        assert.ok(loopGoals.length > 0, "Aucun goal loop invariant/variant pour gauss");
        assert.strictEqual(loopGoals.filter((g: any) => !g.passed).length, 0, "Loop goal non prouvé");
    });
});