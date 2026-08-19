import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';

const BENCH = path.resolve(__dirname, '../../../benchmarks/wp_pass/test.c');
const TIMEOUT_WP = 120000;

let cachedAll: any[] = [];

// ── Parser le format string du serveur OCaml ──────────────────────────────────
// Format: "status:goal_id:file_path:line: (provers):script:function:behavior\n"
function parseGoal(item: any): any {
    if (typeof item !== 'string') return item;
    const s = item.trim();
    // Split sur : mais on sait que les champs sont dans cet ordre fixe
    const firstColon  = s.indexOf(':');                          // après status
    const secondColon = s.indexOf(':', firstColon + 1);         // après goal_id
    const thirdColon  = s.indexOf(':', secondColon + 1);        // après file
    const fourthColon = s.indexOf(':', thirdColon + 1);         // après line
    const fifthColon  = s.indexOf(':', fourthColon + 1);        // après proverInfo
    const sixthColon  = s.indexOf(':', fifthColon + 1);         // après script
    const seventhColon = s.indexOf(':', sixthColon + 1);        // après function

    const status    = s.substring(0, firstColon).trim();
    const goal      = s.substring(firstColon + 1, secondColon).trim();
    const file      = s.substring(secondColon + 1, thirdColon).trim();
    const line      = parseInt(s.substring(thirdColon + 1, fourthColon).trim(), 10);
    const proverInfo = fourthColon > 0 ? s.substring(fourthColon + 1, fifthColon > 0 ? fifthColon : undefined).trim() : "";
    const script    = fifthColon > 0 ? s.substring(fifthColon + 1, sixthColon > 0 ? sixthColon : undefined).trim() : "";
    const func      = sixthColon > 0 ? s.substring(sixthColon + 1, seventhColon > 0 ? seventhColon : undefined).trim() : "";
    const behavior  = seventhColon > 0 ? s.substring(seventhColon + 1).trim() : "";

    // Détecter les provers depuis proverInfo ex: "(Qed 490ms) (Z3 70ms)"
    const provers: any[] = [];
    const proverMatches = proverInfo.matchAll(/\((\w[\w\s\-\.]*?)\s*(?:\d+ms)?\)/g);
    for (const m of proverMatches) {
        provers.push({ prover: m[1].trim().toLowerCase(), time: 0 });
    }
    if (provers.length === 0) provers.push({ prover: 'qed', time: 0 });

    return {
        passed:    status === 'passed',
        goal:      goal,
        file:      file,
        line:      isNaN(line) ? 0 : line,
        proverInfo: proverInfo,
        script:    script,
        function:  func,
        property:  goal,   // le goal_id contient le type (ensures, assigns, rte...)
        verdict:   status === 'passed' ? 'valid' : 'unknown',
        provers:   provers,
        smoke:     goal.toLowerCase().includes('smoke'),
        behavior:  behavior
    };
}

// ── Helper pour extraire et parser la réponse brute ───────────────────────────
// La réponse du serveur est [file, function, property, [strings]]
function extractGoals(raw: any): any[] {
    if (!raw) return [];
    if (Array.isArray(raw) && raw.length === 4 && Array.isArray(raw[3])) {
        return raw[3].map(parseGoal);
    }
    if (Array.isArray(raw)) return raw.map(parseGoal);
    return [];
}

// ── Mock showInputBox ──────────────────────────────────────────────────────────
function mockInputBox(fct: string, prop: string, timeout: string) {
    const original = vscode.window.showInputBox;
    (vscode.window as any).showInputBox = async (opts?: any) => {
        if (opts?.prompt?.includes('Function')) return fct;
        if (opts?.prompt?.includes('Property')) return prop;
        if (opts?.prompt?.includes('Timeout'))  return timeout;
        return timeout;
    };
    return original;
}

suite('Test provePO', () => {

    suiteSetup(async function () {
        this.timeout(300000);

        const uri = vscode.Uri.file(BENCH);
        const doc = await vscode.workspace.openTextDocument(uri);
        await vscode.window.showTextDocument(doc);
        const ext = vscode.extensions.getExtension('innov-org.acsl-lsp')!;
        await ext.activate();
        console.log("Attente initialisation serveur OCaml...");
        await new Promise(r => setTimeout(r, 10000));

        const original = mockInputBox('@all', '@all', '30');
        console.log("Lancement provePO @all/@all (cache)...");
        const raw = await vscode.commands.executeCommand('provePO');
        (vscode.window as any).showInputBox = original;

        cachedAll = extractGoals(raw);
        console.log(`Cache prêt: ${cachedAll.length} goals parsés`);
        if (cachedAll.length > 0) {
            console.log("Exemple goal[0]:", JSON.stringify(cachedAll[0], null, 2));
        }
    });

    // ── TEST 1 ────────────────────────────────────────────────────────────────
    test('provePO @all/@all — structure de la réponse', async function () {
        this.timeout(TIMEOUT_WP);
        assert.ok(cachedAll.length > 0, "Aucun goal retourné");
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
        assert.ok(cachedAll.length > 0, "Aucun goal");
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
        assert.ok(cachedAll.length > 0, "Aucun goal");
        const functions: string[] = [...new Set<string>(cachedAll.map((g: any) => g.function as string))];
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
        assert.ok(cachedAll.length > 0, "Aucun goal");
        const goals: string[] = cachedAll.map((g: any) => (g.goal as string).toLowerCase());
        assert.ok(goals.some(g => g.includes('assign')),   "Aucun goal 'assigns'");
        assert.ok(goals.some(g => g.includes('ensures') || g.includes('post')), "Aucun goal 'ensures'");
        assert.ok(goals.some(g => g.includes('invariant') || g.includes('loop')), "Aucun goal 'loop invariant'");
        assert.ok(goals.some(g =>
            g.includes('rte') || g.includes('division') || g.includes('overflow') || g.includes('index')),
            "Aucun goal RTE");
        console.log("Types présents:", [...new Set<string>(goals.map(g => {
            if (g.includes('ensures')) return 'ensures';
            if (g.includes('assign')) return 'assigns';
            if (g.includes('invariant') || g.includes('loop')) return 'loop';
            if (g.includes('rte')) return 'rte';
            return 'other';
        }))]);
    });

    // ── TEST 5 ────────────────────────────────────────────────────────────────
    test('provePO increment — isolation par fonction', async function () {
        this.timeout(TIMEOUT_WP);
        const original = mockInputBox('increment', '@all', '30');
        const raw = await vscode.commands.executeCommand('provePO');
        (vscode.window as any).showInputBox = original;

        const goals = extractGoals(raw);
        assert.ok(goals.length > 0, "Aucun goal pour 'increment'");
        const functions: string[] = [...new Set<string>(goals.map((g: any) => g.function as string))];
        assert.ok(functions.every(f => f === 'increment'),
            `Goals d'autres fonctions : ${functions.filter(f => f !== 'increment').join(', ')}`);
        console.log(`✔ increment : ${goals.filter((g: any) => g.passed).length}/${goals.length} goals`);
    });

    // ── TEST 6 ────────────────────────────────────────────────────────────────
    test('provePO sign — behaviors nommés présents', async function () {
        this.timeout(TIMEOUT_WP);
        const original = mockInputBox('sign', '@all', '30');
        const raw = await vscode.commands.executeCommand('provePO');
        (vscode.window as any).showInputBox = original;

        const goals = extractGoals(raw);
        assert.ok(goals.length > 0, "Aucun goal pour 'sign'");

        // Le behavior est dans le goal_id ex: "typed_sign_positive_ensures_..."
        const behaviors: string[] = goals.map((g: any) => (g.goal as string).toLowerCase());
        console.log("Goals sign:", behaviors.slice(0, 3));
        assert.ok(behaviors.some(b => b.includes('positive')), "Behavior 'positive' non trouvé dans goals");
        assert.ok(behaviors.some(b => b.includes('zero')),     "Behavior 'zero' non trouvé dans goals");
        assert.ok(behaviors.some(b => b.includes('negative')), "Behavior 'negative' non trouvé dans goals");
        assert.strictEqual(goals.filter((g: any) => !g.passed).length, 0, "Goals échoués dans sign");
    });

    // ── TEST 7 ────────────────────────────────────────────────────────────────
    test('provePO @all — provers qed utilisé', async function () {
        this.timeout(TIMEOUT_WP);
        assert.ok(cachedAll.length > 0, "Aucun goal");
        const allProvers: string[] = cachedAll.flatMap((g: any) =>
            (g.provers as any[]).map((p: any) => (p.prover as string).toLowerCase())
        );
        console.log("Provers:", [...new Set<string>(allProvers)]);
        assert.ok(allProvers.some(p => p.includes('qed')), "Prover 'qed' jamais utilisé");
    });

    // ── TEST 8 ────────────────────────────────────────────────────────────────
    test('provePO safe_div — RTE division prouvée', async function () {
        this.timeout(TIMEOUT_WP);
        const original = mockInputBox('safe_div', '@all', '30');
        const raw = await vscode.commands.executeCommand('provePO');
        (vscode.window as any).showInputBox = original;

        const goals = extractGoals(raw);
        assert.ok(goals.length > 0, "Aucun goal pour 'safe_div'");

        const rteGoals = goals.filter((g: any) =>
            (g.goal as string).toLowerCase().includes('rte') ||
            (g.goal as string).toLowerCase().includes('division') ||
            (g.proverInfo as string).toLowerCase().includes('rte')
        );
        console.log(`RTE goals safe_div : ${rteGoals.length}/${goals.length}`);
        assert.ok(rteGoals.length > 0, "Aucun goal RTE pour safe_div");
        assert.strictEqual(rteGoals.filter((g: any) => !g.passed).length, 0, "RTE non prouvé");
    });

    // ── TEST 9 ────────────────────────────────────────────────────────────────
    test('provePO gauss — loop invariant prouvé', async function () {
        this.timeout(TIMEOUT_WP);
        const original = mockInputBox('gauss', '@all', '30');
        const raw = await vscode.commands.executeCommand('provePO');
        (vscode.window as any).showInputBox = original;

        const goals = extractGoals(raw);
        assert.ok(goals.length > 0, "Aucun goal pour 'gauss'");

        const loopGoals = goals.filter((g: any) =>
            (g.goal as string).toLowerCase().includes('invariant') ||
            (g.goal as string).toLowerCase().includes('variant') ||
            (g.goal as string).toLowerCase().includes('loop')
        );
        console.log(`Loop goals gauss : ${loopGoals.length}/${goals.length}`);
        assert.ok(loopGoals.length > 0, "Aucun goal loop invariant/variant pour gauss");
        assert.strictEqual(loopGoals.filter((g: any) => !g.passed).length, 0, "Loop goal non prouvé");
    });
});