import * as assert from 'assert';
const rewire = require('rewire');

// On charge le fichier compilé (out/extension.js)
// rewire nous permet d'accéder aux fonctions privées et d'injecter des mocks
const ext = rewire('../../../../out/extension');

// ─── MOCK VSCODE ────────────────────────────────────────────────────────────
// On remplace l'API vscode par un objet minimal qui ne fait rien
// pour éviter que les appels à vscode.window.show* plantent les tests
const vscodeMock = {
    window: {
        showWarningMessage: () => {},
        showInformationMessage: () => {},
        showErrorMessage: () => {},
        visibleTextEditors: []
    },
    workspace: {
        workspaceFolders: [{
            uri: { fsPath: '/workspace/test' }
        }]
    },
    commands: {
        executeCommand: () => Promise.resolve()
    },
    Uri: {
        file: (p: string) => ({ fsPath: p })
    }
};

ext.__set__('vscode', vscodeMock);

// ─── MOCK wpDataProvider ─────────────────────────────────────────────────────
// applyFiltersAndRefreshUI appelle wpDataProvider.update() et .refresh()
// on le remplace par un spy pour capturer ce qui lui est passé
let lastUpdateCall: any = null;
const wpDataProviderMock = {
    update: (data: any) => { lastUpdateCall = data; },
    refresh: () => {}
};
ext.__set__('wpDataProvider', wpDataProviderMock);

// ─── ACCÈS AUX FONCTIONS ET VARIABLES PRIVÉES ───────────────────────────────
const setAllGoalsRaw   = (v: any[]) => ext.__set__('allGoalsRaw', v);
const setActiveFilters = (v: any)   => ext.__set__('activeFilters', v);
const getLastWpData    = ()         => ext.__get__('lastWpData');

// Les fonctions qu'on veut tester
const applyFiltersAndRefreshUI = ext.__get__('applyFiltersAndRefreshUI');
const processWpData            = ext.__get__('processWpData');
const buildLineStatusMap       = null; // intégré dans updateDecorations, testé via résultat

// ─── DONNÉES DE TEST ─────────────────────────────────────────────────────────
const makeGoal = (overrides: any = {}) => ({
    passed: true,
    verdict: 'valid',
    goal: 'goal_ensures_foo',
    file: '/workspace/test/src/foo.c',
    _localPath: './src/foo.c',
    line: 10,
    provers: [{ prover: 'alt-ergo', time: 1.2 }],
    script: '',
    function: 'foo',
    smoke: false,
    property: 'ensures',
    ...overrides
});

// ─── HELPERS ─────────────────────────────────────────────────────────────────
const resetFilters = () => setActiveFilters({
    status: 'all', smokeOnly: false, verdict: 'all',
    prover: 'all', search: '', function: '', file: '',
    type: '', hasScript: false, sortByTime: false
});

// ════════════════════════════════════════════════════════════════════════════
// SUITE 1 — applyFiltersAndRefreshUI : filtres de base
// ════════════════════════════════════════════════════════════════════════════
suite('applyFiltersAndRefreshUI — filtres de base', () => {

    setup(() => {
        lastUpdateCall = null;
        resetFilters();
    });

    test('retourne tous les goals quand aucun filtre actif', () => {
        const goals = [makeGoal(), makeGoal({ function: 'bar', passed: false })];
        setAllGoalsRaw(goals);

        applyFiltersAndRefreshUI();

        assert.ok(lastUpdateCall, 'wpDataProvider.update() doit être appelé');
        const [tag, , , data] = lastUpdateCall;
        assert.strictEqual(tag, 'Filtered');
        assert.strictEqual(data.length, 2);
    });

    test('filtre par status "passed" — ne retourne que les goals passés', () => {
        setAllGoalsRaw([
            makeGoal({ passed: true }),
            makeGoal({ passed: false }),
            makeGoal({ passed: true })
        ]);
        setActiveFilters({ ...ext.__get__('activeFilters'), status: 'passed' });

        applyFiltersAndRefreshUI();

        const [, , , data] = lastUpdateCall;
        assert.strictEqual(data.length, 2);
        assert.ok(data.every((d: any) => d.status === 'passed'));
    });

    test('filtre par status "failed" — ne retourne que les goals échoués', () => {
        setAllGoalsRaw([
            makeGoal({ passed: true }),
            makeGoal({ passed: false }),
        ]);
        setActiveFilters({ ...ext.__get__('activeFilters'), status: 'failed' });

        applyFiltersAndRefreshUI();

        const [, , , data] = lastUpdateCall;
        assert.strictEqual(data.length, 1);
        assert.strictEqual(data[0].status, 'failed');
    });

    test('filtre par function — correspondance partielle (case insensitive)', () => {
        setAllGoalsRaw([
            makeGoal({ function: 'foo_init' }),
            makeGoal({ function: 'bar_reset' }),
            makeGoal({ function: 'FOO_check' })
        ]);
        setActiveFilters({ ...ext.__get__('activeFilters'), function: 'foo' });

        applyFiltersAndRefreshUI();

        const [, , , data] = lastUpdateCall;
        assert.strictEqual(data.length, 2);
    });

    test('filtre par file — correspondance partielle', () => {
        setAllGoalsRaw([
            makeGoal({ _localPath: './src/foo.c' }),
            makeGoal({ _localPath: './src/bar.c' }),
            makeGoal({ _localPath: './src/foo_utils.c' })
        ]);
        setActiveFilters({ ...ext.__get__('activeFilters'), file: 'foo' });

        applyFiltersAndRefreshUI();

        const [, , , data] = lastUpdateCall;
        assert.strictEqual(data.length, 2);
    });

    test('filtre par type ACSL (property)', () => {
        setAllGoalsRaw([
            makeGoal({ property: 'ensures' }),
            makeGoal({ property: 'requires' }),
            makeGoal({ property: 'ensures' })
        ]);
        setActiveFilters({ ...ext.__get__('activeFilters'), type: 'ensures' });

        applyFiltersAndRefreshUI();

        const [, , , data] = lastUpdateCall;
        assert.strictEqual(data.length, 2);
    });

    test('filtre par verdict', () => {
        setAllGoalsRaw([
            makeGoal({ verdict: 'valid' }),
            makeGoal({ verdict: 'timeout' }),
            makeGoal({ verdict: 'valid' })
        ]);
        setActiveFilters({ ...ext.__get__('activeFilters'), verdict: 'timeout' });

        applyFiltersAndRefreshUI();

        const [, , , data] = lastUpdateCall;
        assert.strictEqual(data.length, 1);
    });

    test('filtre smokeOnly — ne retourne que les smoke tests', () => {
        setAllGoalsRaw([
            makeGoal({ smoke: true }),
            makeGoal({ smoke: false }),
            makeGoal({ smoke: true })
        ]);
        setActiveFilters({ ...ext.__get__('activeFilters'), smokeOnly: true });

        applyFiltersAndRefreshUI();

        const [, , , data] = lastUpdateCall;
        assert.strictEqual(data.length, 2);
    });

    test('filtre hasScript — ne retourne que les goals avec script', () => {
        setAllGoalsRaw([
            makeGoal({ script: 'proof/foo.script' }),
            makeGoal({ script: '' }),
            makeGoal({ script: '   ' })  // whitespace = pas de script
        ]);
        setActiveFilters({ ...ext.__get__('activeFilters'), hasScript: true });

        applyFiltersAndRefreshUI();

        const [, , , data] = lastUpdateCall;
        assert.strictEqual(data.length, 1);
    });
});

// ════════════════════════════════════════════════════════════════════════════
// SUITE 2 — applyFiltersAndRefreshUI : filtre prover
// ════════════════════════════════════════════════════════════════════════════
suite('applyFiltersAndRefreshUI — filtre prover', () => {

    setup(() => { lastUpdateCall = null; resetFilters(); });

    test('filtre par prover alt-ergo', () => {
        setAllGoalsRaw([
            makeGoal({ provers: [{ prover: 'alt-ergo', time: 1 }] }),
            makeGoal({ provers: [{ prover: 'z3', time: 2 }] }),
            makeGoal({ provers: [{ prover: 'alt-ergo', time: 0.5 }] })
        ]);
        setActiveFilters({ ...ext.__get__('activeFilters'), prover: 'alt-ergo' });

        applyFiltersAndRefreshUI();

        const [, , , data] = lastUpdateCall;
        assert.strictEqual(data.length, 2);
    });

    test('goal sans provers utilise qed par défaut et matche le filtre "qed"', () => {
        setAllGoalsRaw([
            makeGoal({ provers: [] }),   // pas de provers → défaut qed
            makeGoal({ provers: [{ prover: 'alt-ergo', time: 1 }] })
        ]);
        setActiveFilters({ ...ext.__get__('activeFilters'), prover: 'qed' });

        applyFiltersAndRefreshUI();

        const [, , , data] = lastUpdateCall;
        assert.strictEqual(data.length, 1);
    });
});

// ════════════════════════════════════════════════════════════════════════════
// SUITE 3 — applyFiltersAndRefreshUI : recherche globale
// ════════════════════════════════════════════════════════════════════════════
suite('applyFiltersAndRefreshUI — recherche globale (search)', () => {

    setup(() => { lastUpdateCall = null; resetFilters(); });

    test('search matche sur le nom de fonction', () => {
        setAllGoalsRaw([
            makeGoal({ function: 'authenticate_user' }),
            makeGoal({ function: 'reset_password' })
        ]);
        setActiveFilters({ ...ext.__get__('activeFilters'), search: 'auth' });

        applyFiltersAndRefreshUI();

        const [, , , data] = lastUpdateCall;
        assert.strictEqual(data.length, 1);
    });

    test('search matche sur le chemin de fichier', () => {
        setAllGoalsRaw([
            makeGoal({ _localPath: './src/auth/login.c' }),
            makeGoal({ _localPath: './src/ui/button.c' })
        ]);
        setActiveFilters({ ...ext.__get__('activeFilters'), search: 'auth' });

        applyFiltersAndRefreshUI();

        const [, , , data] = lastUpdateCall;
        assert.strictEqual(data.length, 1);
    });

    test('search matche sur le goal/property', () => {
        setAllGoalsRaw([
            makeGoal({ goal: 'ensures_result_positive' }),
            makeGoal({ goal: 'requires_not_null' })
        ]);
        setActiveFilters({ ...ext.__get__('activeFilters'), search: 'ensures' });

        applyFiltersAndRefreshUI();

        const [, , , data] = lastUpdateCall;
        assert.strictEqual(data.length, 1);
    });

    test('search vide retourne tout', () => {
        setAllGoalsRaw([makeGoal(), makeGoal(), makeGoal()]);
        setActiveFilters({ ...ext.__get__('activeFilters'), search: '' });

        applyFiltersAndRefreshUI();

        const [, , , data] = lastUpdateCall;
        assert.strictEqual(data.length, 3);
    });
});

// ════════════════════════════════════════════════════════════════════════════
// SUITE 4 — applyFiltersAndRefreshUI : tri par temps
// ════════════════════════════════════════════════════════════════════════════
suite('applyFiltersAndRefreshUI — tri par temps (sortByTime)', () => {

    setup(() => { lastUpdateCall = null; resetFilters(); });

    test('sortByTime trie du plus lent au plus rapide', () => {
        setAllGoalsRaw([
            makeGoal({ provers: [{ prover: 'alt-ergo', time: 0.5 }], goal: 'fast' }),
            makeGoal({ provers: [{ prover: 'alt-ergo', time: 5.0 }], goal: 'slow' }),
            makeGoal({ provers: [{ prover: 'alt-ergo', time: 2.0 }], goal: 'medium' })
        ]);
        setActiveFilters({ ...ext.__get__('activeFilters'), sortByTime: true });

        applyFiltersAndRefreshUI();

        const [, , , data] = lastUpdateCall;
        assert.strictEqual(data[0].goal, 'slow');
        assert.strictEqual(data[1].goal, 'medium');
        assert.strictEqual(data[2].goal, 'fast');
    });

    test('sans sortByTime, l\'ordre original est conservé', () => {
        setAllGoalsRaw([
            makeGoal({ provers: [{ prover: 'alt-ergo', time: 0.5 }], goal: 'first' }),
            makeGoal({ provers: [{ prover: 'alt-ergo', time: 5.0 }], goal: 'second' }),
        ]);
        setActiveFilters({ ...ext.__get__('activeFilters'), sortByTime: false });

        applyFiltersAndRefreshUI();

        const [, , , data] = lastUpdateCall;
        assert.strictEqual(data[0].goal, 'first');
        assert.strictEqual(data[1].goal, 'second');
    });
});

// ════════════════════════════════════════════════════════════════════════════
// SUITE 5 — applyFiltersAndRefreshUI : cas limites
// ════════════════════════════════════════════════════════════════════════════
suite('applyFiltersAndRefreshUI — cas limites', () => {

    setup(() => { lastUpdateCall = null; resetFilters(); });

    test('liste vide → wpDataProvider.update() n\'est pas appelé', () => {
        setAllGoalsRaw([]);
        applyFiltersAndRefreshUI();
        assert.strictEqual(lastUpdateCall, null, 'update() ne doit pas être appelé si pas de goals');
    });

    test('filtres combinés status + function', () => {
        setAllGoalsRaw([
            makeGoal({ passed: true,  function: 'foo' }),
            makeGoal({ passed: false, function: 'foo' }),
            makeGoal({ passed: true,  function: 'bar' })
        ]);
        setActiveFilters({ ...ext.__get__('activeFilters'), status: 'passed', function: 'foo' });

        applyFiltersAndRefreshUI();

        const [, , , data] = lastUpdateCall;
        assert.strictEqual(data.length, 1);
        assert.strictEqual(data[0].function, 'foo');
        assert.strictEqual(data[0].status, 'passed');
    });

    test('le résultat formaté contient les bons champs', () => {
        setAllGoalsRaw([
            makeGoal({
                passed: true,
                goal: 'ensures_foo',
                _localPath: './src/foo.c',
                line: 42,
                provers: [{ prover: 'alt-ergo', time: 1.5 }],
                script: 'proof/foo.script',
                function: 'foo'
            })
        ]);

        applyFiltersAndRefreshUI();

        const [, , , data] = lastUpdateCall;
        const item = data[0];
        assert.strictEqual(item.status, 'passed');
        assert.strictEqual(item.goal, 'ensures_foo');
        assert.strictEqual(item.file, './src/foo.c');
        assert.strictEqual(item.line, 42);
        assert.strictEqual(item.function, 'foo');
        assert.strictEqual(item.script, 'proof/foo.script');
        assert.ok(item.proverInfo.includes('alt-ergo'));
    });
});

// ════════════════════════════════════════════════════════════════════════════
// SUITE 6 — processWpData : normalisation des chemins
// ════════════════════════════════════════════════════════════════════════════
suite('processWpData — normalisation des chemins', () => {

    setup(() => { lastUpdateCall = null; resetFilters(); });

    test('chemin absolu qui commence par le workspace → converti en chemin relatif ./', () => {
        const rawData = [
            { file: '/workspace/test/src/foo.c', passed: true, goal: 'g1', line: 1, provers: [] }
        ];

        // processWpData appelle get_workspace() → workspace/test (via notre mock)
        processWpData(rawData, 'Auto-update');

        const goals: any[] = ext.__get__('allGoalsRaw');
        assert.ok(goals[0]._localPath.startsWith('./'), 'Le chemin doit commencer par ./');
        assert.strictEqual(goals[0]._localPath, './src/foo.c');
    });

    test('chemin absolu extérieur au workspace → conservé tel quel', () => {
        const rawData = [
            { file: '/other/project/file.c', passed: true, goal: 'g1', line: 1, provers: [] }
        ];

        processWpData(rawData, 'Auto-update');

        const goals: any[] = ext.__get__('allGoalsRaw');
        assert.strictEqual(goals[0]._localPath, '/other/project/file.c');
    });

    test('backslashes Windows convertis en forward slashes', () => {
        const rawData = [
            { file: '/workspace/test\\src\\foo.c', passed: true, goal: 'g1', line: 1, provers: [] }
        ];

        processWpData(rawData, 'Auto-update');

        const goals: any[] = ext.__get__('allGoalsRaw');
        assert.ok(!goals[0]._localPath.includes('\\'), 'Pas de backslash dans le chemin');
    });

    test('file vide → _localPath vide', () => {
        const rawData = [
            { file: '', passed: true, goal: 'g1', line: 1, provers: [] }
        ];

        processWpData(rawData, 'Auto-update');

        const goals: any[] = ext.__get__('allGoalsRaw');
        assert.strictEqual(goals[0]._localPath, '');
    });
});
