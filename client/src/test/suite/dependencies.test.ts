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

        // Let the OCaml LSP server start up.
        await new Promise(res => setTimeout(res, 5_000));

        // The command runs the dependency analysis and returns the raw result.
        result = await vscode.commands.executeCommand<DepResult>('acsl.computeDependencies');
        assert.ok(result, 'Command returned nothing');
    });

    test('declares the four groups', () => {
        assert.deepStrictEqual([...result.groups].sort(), ['auth', 'billing', 'core', 'libs']);
    });

    test('allows auth -> core', () => {
        const cell = findCell(result.matrix, 'auth', 'core');
        assert.ok(cell, 'auth -> core missing from matrix');
        assert.strictEqual(cell!.edges[0].symbol, 'core_init');
    });

    test('allows core -> libs', () => {
        const cell = findCell(result.matrix, 'core', 'libs');
        assert.ok(cell, 'core -> libs missing from matrix');
        assert.strictEqual(cell!.edges[0].symbol, 'util_add');
    });

    test('flags billing -> auth as a violation', () => {
        const violation = findCell(result.violations, 'billing', 'auth');
        assert.ok(violation, 'billing -> auth should be a violation');
        assert.strictEqual(violation!.edges[0].symbol, 'auth_login');
        assert.strictEqual(findCell(result.matrix, 'billing', 'auth'), undefined,
            'a violation must not also appear in matrix');
    });

    test('reports exactly one violation', () => {
        assert.strictEqual(result.violations.length, 1);
    });

    test('leaves nothing unassigned (all analysed files belong to a group)', () => {
    assert.deepStrictEqual(result.unassigned, []);
});
});