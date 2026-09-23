import * as assert from 'assert';
import * as vscode from 'vscode';

type DepEdge = { caller: string; callee: string; symbol: string };
type DepCell = { from: string; to: string; count: number; edges: DepEdge[] };
type DepResult = {
    groups: string[];
    matrix: DepCell[];
    violations: DepCell[];
    unassigned: string[];
    unresolved_calls: { file: string; line: number }[];
};

const findCell = (cells: DepCell[], from: string, to: string) =>
    cells.find(c => c.from === from && c.to === to);

suite('Dependency matrix (groups)', () => {
    const TIMEOUT = 60_000;
    let result: DepResult;

    suiteSetup(async function () {
        this.timeout(TIMEOUT);

        const ext = vscode.extensions.getExtension('innov-org.acsl-lsp');
        assert.ok(ext, 'Extension innov-org.acsl-lsp not found');
        await ext!.activate();

        await new Promise(res => setTimeout(res, 5_000));

        try {
            result = await vscode.commands.executeCommand<DepResult>('acsl.computeDependencies');
        } catch (e) {
            console.error('computeDependencies threw:', e);
            throw e;
        }
        console.log('computeDependencies returned:', JSON.stringify(result));
        assert.ok(result, 'Command returned nothing');
    });

    test('declares the four groups', () => {
        assert.deepStrictEqual([...result.groups].sort(), ['auth', 'billing', 'core', 'libs']);
    });

    test('allows auth -> core', () => {
        const cell = findCell(result.matrix, 'auth', 'core');
        assert.ok(cell, 'auth -> core missing from matrix');
        assert.ok(cell!.edges.some(e => e.symbol === 'core_init'),
            'auth -> core should include core_init');
    });

    test('allows core -> libs', () => {
        const cell = findCell(result.matrix, 'core', 'libs');
        assert.ok(cell, 'core -> libs missing from matrix');
        assert.ok(cell!.edges.some(e => e.symbol === 'util_add'),
            'core -> libs should include util_add');
    });

    test('flags billing -> auth as a violation, not in matrix', () => {
        const violation = findCell(result.violations, 'billing', 'auth');
        assert.ok(violation, 'billing -> auth should be a violation');
        assert.ok(violation!.edges.every(e => e.symbol === 'auth_login'),
            'every billing -> auth edge should go through auth_login');
        assert.strictEqual(findCell(result.matrix, 'billing', 'auth'), undefined,
            'a violation must not also appear in matrix');
    });

    test('reports exactly one violation cell', () => {
        assert.strictEqual(result.violations.length, 1);
    });

    test('leaves nothing unassigned', () => {
        assert.deepStrictEqual(result.unassigned, []);
    });


    test('resolves the annotated pointer via //@ calls', () => {
        const violation = findCell(result.violations, 'billing', 'auth');
        assert.ok(violation, 'billing -> auth missing');
        const callers = violation!.edges.map(e => e.caller);
        assert.ok(callers.some(c => c.endsWith('pointeur_test.c')),
            'the //@ calls clause in pointeur_test.c should create a billing -> auth edge');
        assert.strictEqual(violation!.count, 2,
            'billing -> auth should have 2 edges: 1 direct + 1 from //@ calls');
    });

    test('reports the unannotated pointer as unresolved', () => {
        assert.strictEqual(result.unresolved_calls.length, 1,
            'exactly one unresolved call expected (ptr_stress.c)');
        assert.ok(result.unresolved_calls[0].file.endsWith('ptr_stress.c'),
            'the unresolved call should be in ptr_stress.c');
    });

    test('the unannotated pointer produces no billing -> auth edge', () => {
        const violation = findCell(result.violations, 'billing', 'auth');
        const callers = (violation?.edges ?? []).map(e => e.caller);
        assert.ok(!callers.some(c => c.endsWith('ptr_stress.c')),
            'ptr_stress.c has no clause, so it must not create an edge');
    });
});