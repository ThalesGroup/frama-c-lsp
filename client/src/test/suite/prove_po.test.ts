import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';

const BENCH = path.resolve(__dirname, '../../../benchmarks/wp_pass/test.c');
const TIMEOUT_WP = 120000;

// Cache du résultat @all/@all
let cachedAll: any = null;

suite('Test provePO', () => {

    suiteSetup(async function () {
        this.timeout(300000);

        // Ouvrir le fichier et activer l'extension
        const uri = vscode.Uri.file(BENCH);
        const doc = await vscode.workspace.openTextDocument(uri);
        await vscode.window.showTextDocument(doc);
        const ext = vscode.extensions.getExtension('innov-org.acsl-lsp')!;
        await ext.activate();
        console.log("Attente initialisation serveur OCaml...");
        await new Promise(r => setTimeout(r, 10000));

        // Mock showInputBox pour éviter l'attente utilisateur
        const originalShowInputBox = vscode.window.showInputBox;
        (vscode.window as any).showInputBox = async (opts?: any) => {
            if (opts?.prompt?.includes('Function')) return '@all';
            if (opts?.prompt?.includes('Property')) return '@all';
            if (opts?.prompt?.includes('Timeout')) return '30';
            return '30';
        };

        // Lancer provePO une seule fois et cacher le résultat
        console.log("Lancement provePO @all/@all (cache)...");
        cachedAll = await vscode.commands.executeCommand('provePO');
        console.log("Cache prêt:", cachedAll ? `${cachedAll.length} goals` : "undefined");

        // Restaurer showInputBox
        (vscode.window as any).showInputBox = originalShowInputBox;
    });

    // ── TEST 1 ────────────────────────────────────────────────────────────────
    test('provePO @all/@all — structure de la réponse', async function () {
        this.timeout(TIMEOUT_WP);
        assert.ok(cachedAll,                "Réponse undefined");
        assert.ok(Array.isArray(cachedAll), "Réponse doit être un tableau");
        assert.ok(cachedAll.length > 0,    "Tableau vide — aucun goal retourné");
        for (const goal of cachedAll) {
            assert.ok('goal'     in goal, `Champ 'goal' manquant`);
            assert.ok('property' in goal, `Champ 'property' manquant`);
            assert.ok('file'     in goal, `Champ 'file' manquant`);
            assert.ok('line'     in goal, `Champ 'line' manquant`);
            assert.ok('function' in goal, `Champ 'function' manquant`);
            assert.ok('passed'   in goal, `Champ 'passed' manquant`);
            assert.ok('verdict'  in goal, `Champ 'verdict' manquant`);
            assert.ok('provers'  in goal, `Champ 'provers' manquant`);
            assert.ok('smoke'    in goal, `Champ 'smoke' manquant`);
        }
        console.log(`✔ Total goals : ${cachedAll.length}`);
    });

    // ── TEST 2 ────────────────────────────────────────────────────────────────
    test('provePO @all/@all — tous les goals passent', async function () {
        this.timeout(TIMEOUT_WP);
        assert.ok(cachedAll && cachedAll.length > 0, "Aucun goal");
        const failed = cachedAll.filter((g: any) => !g.passed);
        assert.strictEqual(failed.length, 0,
            `${failed.length} goal(s) échoués :\n` +
            failed.map((g: any) => `  - ${g.function} / ${g.goal} → ${g.verdict}`).join('\n')
        );
        console.log(`✔ ${cachedAll.filter((g: any) => g.passed).length}/${cachedAll.length} goals prouvés`);
    });

    // ── TEST 3 ────────────────────────────────────────────────────────────────
    test('provePO @all/@all — fonctions attendues présentes', async function () {
        this.timeout(TIMEOUT_WP);
        assert.ok(cachedAll && cachedAll.length > 0, "Aucun goal");
        const functions: string[] = [...new Set<string>(cachedAll.map((g: any) => g.function as string))];
        console.log("Fonctions prouvées:", functions);
        const expected = ['increment', 'swap', 'sign', 'gauss', 'safe_div',
                          'safe_get', 'fill_zero', 'find_min_sorted', 'square', 'copy_point'];
        for (const fn of expected) {
            assert.ok(functions.includes(fn), `Fonction '${fn}' absente`);
        }
    });

    // ── TEST 4 ────────────────────────────────────────────────────────────────
    test('provePO @all/@all — types de property couverts', async function () {
        this.timeout(TIMEOUT_WP);
        assert.ok(cachedAll && cachedAll.length > 0, "Aucun goal");
        const properties: string[] = cachedAll.map((g: any) => (g.property as string).toLowerCase());
        assert.ok(properties.some(p => p.includes('assign')),   "Aucun goal 'assigns'");
        assert.ok(properties.some(p => p.includes('ensures') || p.includes('post')), "Aucun goal 'ensures'");
        assert.ok(properties.some(p => p.includes('invariant') || p.includes('loop')), "Aucun goal 'loop invariant'");
        assert.ok(properties.some(p =>
            p.includes('rte') || p.includes('division') || p.includes('overflow') || p.includes('index')),
            "Aucun goal RTE");
        console.log("Types:", [...new Set<string>(properties)]);
    });

    // ── TEST 5 ────────────────────────────────────────────────────────────────
    test('provePO increment — isolation par fonction', async function () {
        this.timeout(TIMEOUT_WP);

        const originalShowInputBox = vscode.window.showInputBox;
        (vscode.window as any).showInputBox = async (opts?: any) => {
            if (opts?.prompt?.includes('Function')) return 'increment';
            if (opts?.prompt?.includes('Property')) return '@all';
            if (opts?.prompt?.includes('Timeout')) return '30';
            return '30';
        };

        const response: any = await vscode.commands.executeCommand('provePO');
        (vscode.window as any).showInputBox = originalShowInputBox;

        assert.ok(response && response.length > 0, "Aucun goal pour 'increment'");
        const functions: string[] = [...new Set<string>(response.map((g: any) => g.function as string))];
        assert.ok(functions.every(f => f === 'increment'),
            `Goals d'autres fonctions : ${functions.filter(f => f !== 'increment').join(', ')}`);
        console.log(`✔ increment : ${response.filter((g: any) => g.passed).length}/${response.length} goals`);
    });

    // ── TEST 6 ────────────────────────────────────────────────────────────────
    test('provePO sign — behaviors nommés présents', async function () {
        this.timeout(TIMEOUT_WP);

        const originalShowInputBox = vscode.window.showInputBox;
        (vscode.window as any).showInputBox = async (opts?: any) => {
            if (opts?.prompt?.includes('Function')) return 'sign';
            if (opts?.prompt?.includes('Property')) return '@all';
            if (opts?.prompt?.includes('Timeout')) return '30';
            return '30';
        };

        const response: any = await vscode.commands.executeCommand('provePO');
        (vscode.window as any).showInputBox = originalShowInputBox;

        assert.ok(response && response.length > 0, "Aucun goal pour 'sign'");
        const behaviors: string[] = response
            .map((g: any) => (g.behavior as string || '').toLowerCase())
            .filter((b: string) => b !== '');
        console.log("Behaviors:", [...new Set<string>(behaviors)]);
        assert.ok(behaviors.some(b => b.includes('positive')), "Behavior 'positive' non trouvé");
        assert.ok(behaviors.some(b => b.includes('zero')),     "Behavior 'zero' non trouvé");
        assert.ok(behaviors.some(b => b.includes('negative')), "Behavior 'negative' non trouvé");
        assert.strictEqual(response.filter((g: any) => !g.passed).length, 0, "Goals échoués dans sign");
    });

    // ── TEST 7 ────────────────────────────────────────────────────────────────
    test('provePO @all — provers qed utilisé', async function () {
        this.timeout(TIMEOUT_WP);
        assert.ok(cachedAll && cachedAll.length > 0, "Aucun goal");
        const allProvers: string[] = cachedAll.flatMap((g: any) =>
            (g.provers as any[]).map(p => (p.prover as string).toLowerCase())
        );
        console.log("Provers:", [...new Set<string>(allProvers)]);
        assert.ok(allProvers.some(p => p.includes('qed')), "Prover 'qed' jamais utilisé");
    });

    // ── TEST 8 ────────────────────────────────────────────────────────────────
    test('provePO safe_div — RTE division prouvée', async function () {
        this.timeout(TIMEOUT_WP);

        const originalShowInputBox = vscode.window.showInputBox;
        (vscode.window as any).showInputBox = async (opts?: any) => {
            if (opts?.prompt?.includes('Function')) return 'safe_div';
            if (opts?.prompt?.includes('Property')) return '@all';
            if (opts?.prompt?.includes('Timeout')) return '30';
            return '30';
        };

        const response: any = await vscode.commands.executeCommand('provePO');
        (vscode.window as any).showInputBox = originalShowInputBox;

        assert.ok(response && response.length > 0, "Aucun goal pour 'safe_div'");
        const rteGoals = response.filter((g: any) =>
            (g.property as string).toLowerCase().includes('rte') ||
            (g.property as string).toLowerCase().includes('division') ||
            (g.goal as string).toLowerCase().includes('rte')
        );
        assert.ok(rteGoals.length > 0, "Aucun goal RTE pour safe_div");
        assert.strictEqual(rteGoals.filter((g: any) => !g.passed).length, 0, "RTE non prouvé");
        console.log(`✔ RTE goals safe_div : ${rteGoals.length}`);
    });

    // ── TEST 9 ────────────────────────────────────────────────────────────────
    test('provePO gauss — loop invariant prouvé', async function () {
        this.timeout(TIMEOUT_WP);

        const originalShowInputBox = vscode.window.showInputBox;
        (vscode.window as any).showInputBox = async (opts?: any) => {
            if (opts?.prompt?.includes('Function')) return 'gauss';
            if (opts?.prompt?.includes('Property')) return '@all';
            if (opts?.prompt?.includes('Timeout')) return '30';
            return '30';
        };

        const response: any = await vscode.commands.executeCommand('provePO');
        (vscode.window as any).showInputBox = originalShowInputBox;

        assert.ok(response && response.length > 0, "Aucun goal pour 'gauss'");
        const loopGoals = response.filter((g: any) =>
            (g.property as string).toLowerCase().includes('invariant') ||
            (g.property as string).toLowerCase().includes('variant') ||
            (g.goal as string).toLowerCase().includes('loop')
        );
        assert.ok(loopGoals.length > 0, "Aucun goal loop pour gauss");
        assert.strictEqual(loopGoals.filter((g: any) => !g.passed).length, 0, "Loop goal non prouvé");
        console.log(`✔ Loop goals gauss : ${loopGoals.length}`);
    });
});