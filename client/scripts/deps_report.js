
'use strict';
const fs = require('fs');
const path = require('path');

const argv = process.argv.slice(2);
const ci = argv.indexOf('--config');
const CONFIG = ci >= 0 ? argv.splice(ci, 2)[1] : '.vscode/settings.json';
const [MODE, wsArg, rawArg, outArg] = argv;
const WS = path.resolve(wsArg || '.');

const log = (...m) => console.log('[deps]', ...m);
const fail = (m) => { console.error('[deps] ERROR:', m); process.exit(1); };

// ---------- config (JSON with comments / trailing commas) ----------
function readJsonc(file) {
    const txt = fs.readFileSync(file, 'utf8');
    let out = '', inStr = false;
    for (let i = 0; i < txt.length; i++) {
        const c = txt[i], n = txt[i + 1];
        if (inStr) { out += c; if (c === '\\') out += txt[++i]; else if (c === '"') inStr = false; continue; }
        if (c === '"') { inStr = true; out += c; }
        else if (c === '/' && n === '/') { while (i < txt.length && txt[i] !== '\n') i++; out += '\n'; }
        else if (c === '/' && n === '*') { i = txt.indexOf('*/', i + 2) + 1; }
        else out += c;
    }
    return JSON.parse(out.replace(/,(\s*[}\]])/g, '$1'));
}

const configPath = path.resolve(WS, CONFIG);
if (!fs.existsSync(configPath)) fail(`config not found: ${configPath}`);
const settings = readJsonc(configPath);
const groups = settings['dependencies.groups'] || [];
const kinds = Object.fromEntries(groups.map(g => [g.name, g.kind]));
const abs = p => path.resolve(WS, p);
const rel = f => path.relative(WS, f).replace(/\\/g, '/');

// ---------- groups (same logic as resolveFileGroups in extension.ts) ----------

function collectCFiles(dir, acc = []) {
    if (!fs.existsSync(dir)) return acc;
    for (const e of fs.readdirSync(dir, { withFileTypes: true })) {
        const full = path.join(dir, e.name);
        if (e.isDirectory()) collectCFiles(full, acc);
        else if (e.name.endsWith('.c')) acc.push(full);
    }
    return acc;
}

function resolvePattern(p) {
    if (p.endsWith('/')) return collectCFiles(p);
    if (p.includes('*')) {
        const dir = path.dirname(p);
        const re = new RegExp('^' + path.basename(p).replace(/[.+^${}()|[\]\\]/g, '\\$&').replace(/\*/g, '[^/]*') + '$');
        return fs.existsSync(dir) ? fs.readdirSync(dir).filter(f => f.endsWith('.c') && re.test(f)).map(f => path.join(dir, f)) : [];
    }
    // explicit file: only C sources are analysed (.s/.S assembly, .h, .lib... are skipped)
    return fs.existsSync(p) && p.endsWith('.c') ? [p] : [];
}

function cmdGroups() {
    if (groups.length === 0) fail(`'dependencies.groups' is empty in ${CONFIG}`);
    const fileGroups = {};
    for (const g of groups) {
        if (!['core', 'feature', 'lib'].includes(g.kind)) fail(`group '${g.name}': invalid kind '${g.kind}' (core|feature|lib)`);
        const files = g.paths.flatMap(p => resolvePattern(p.endsWith('/') ? abs(p) + '/' : abs(p)));
        files.forEach(f => { fileGroups[f] = g.name; });
        log(`group ${g.name} (${g.kind}): ${files.length} file(s)` + (files.length ? '' : '  <-- no file!'));
    }
    const files = Object.keys(fileGroups);
    if (files.length === 0) fail('no .c file resolved');

    const inc = (settings['kernel.includePaths'] || []).map(p => '-I' + abs(p));
    const mac = (settings['kernel.macros'] || []).map(m => '-D' + m);
    const dir = path.join(WS, '.frama-c');
    fs.mkdirSync(dir, { recursive: true });
    fs.writeFileSync(path.join(dir, 'dep_groups.json'), JSON.stringify({ fileGroups, kinds }, null, 1));
    fs.writeFileSync(path.join(dir, 'files.txt'), files.join('\n') + '\n');
    fs.writeFileSync(path.join(dir, 'cpp_args.txt'), ['-CC', ...inc, ...mac].join(' '));
    log(`${files.length} file(s) -> .frama-c/dep_groups.json, files.txt, cpp_args.txt`);
}

// ---------- render ----------
const esc = s => String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');

// Frama-C prints the response pretty-printed after "[lsp] ": extract the balanced JSON block.
function extractResponse(raw) {
    const m = /\[lsp\]\s*\{\s*"(jsonrpc|id|result)"/.exec(raw);
    if (!m) fail('no JSON response in frama-c output (parsing failed? see deps.err)');
    const start = raw.indexOf('{', m.index);
    let depth = 0, inStr = false;
    for (let i = start; i < raw.length; i++) {
        const c = raw[i];
        if (inStr) { if (c === '\\') i++; else if (c === '"') inStr = false; continue; }
        if (c === '"') inStr = true;
        else if (c === '{') depth++;
        else if (c === '}' && --depth === 0) return JSON.parse(raw.slice(start, i + 1));
    }
    fail('truncated JSON in frama-c output');
}

function renderHtml(res) {
    const cells = new Map();
    res.matrix.forEach(c => cells.set(`${c.from}|${c.to}`, { c, bad: false }));
    res.violations.forEach(c => cells.set(`${c.from}|${c.to}`, { c, bad: true }));
    const names = [...res.groups].sort();
    const id = c => esc(`${c.from}__${c.to}`);

    const head = names.map(n => `<th>${esc(n)}<br><small>${esc(kinds[n] || '')}</small></th>`).join('');
    const rows = names.map(from => `<tr><th class="row">${esc(from)}</th>` + names.map(to => {
        if (from === to) return '<td class="self">–</td>';
        const h = cells.get(`${from}|${to}`);
        return h ? `<td class="${h.bad ? 'bad' : 'ok'}"><a href="#${id(h.c)}">${h.c.count}</a></td>` : '<td></td>';
    }).join('') + '</tr>').join('');

    const details = [...cells.values()].sort((a, b) => b.bad - a.bad).map(({ c, bad }) =>
        `<details id="${id(c)}"${bad ? ' open' : ''}><summary class="${bad ? 'red' : ''}">` +
        `${esc(c.from)} → ${esc(c.to)}: ${c.count} call(s)${bad ? ' — VIOLATION' : ''}</summary><table>` +
        `<tr><th>Function</th><th>Defined in</th><th>Used in</th></tr>` +
        [...c.edges]
            .sort((a, b) => a.symbol.localeCompare(b.symbol) || rel(a.caller).localeCompare(rel(b.caller)))
            .map(e => `<tr><td>${esc(e.symbol)}</td><td>${esc(rel(e.callee))}</td><td>${esc(rel(e.caller))}</td></tr>`).join('') +
        `</table></details>`).join('');

    const list = (title, items) => items.length
        ? `<h3 class="warn">${title} (${items.length})</h3><ul>${items.map(i => `<li>${esc(i)}</li>`).join('')}</ul>` : '';

    return `<!DOCTYPE html><html><head><meta charset="utf-8"><title>Dependency Matrix</title><style>
body{font-family:sans-serif;padding:20px}table{border-collapse:collapse;margin:8px 0}
th,td{border:1px solid #ccc;padding:6px 10px;text-align:center}th{background:#f3f3f3}th.row{text-align:right}
.self{opacity:.4}.ok{background:#2e7d32}.bad{background:#c62828;font-weight:bold}.ok a,.bad a{color:#fff}
.red{color:#c62828;font-weight:bold}.warn{color:#e65100}details td{text-align:left;font-family:monospace;font-size:12px}
</style></head><body>
<h2>Dependency Matrix</h2>
<p>${esc(new Date().toISOString())} · <span class="red">Violations: ${res.violations.length}</span> ·
<span class="warn">Unassigned: ${res.unassigned.length}</span> · <span class="warn">Unresolved calls: ${res.unresolved_calls.length}</span></p>
<p style="opacity:.7">Rows: callers · Columns: callees · Red = architecture violation · Click a number for details</p>
<table><tr><th>from \\ to</th>${head}</tr>${rows}</table>
${list('Unassigned files (add them to dependencies.groups)', res.unassigned.map(rel))}
${list('Unresolved indirect calls (add //@ calls)', res.unresolved_calls.map(u => `${rel(u.file)}:${u.line}`))}
<h3>Edges</h3>${details}</body></html>`;
}

// Warnings NG native JSON format
function renderIssues(res) {
    const issues = [
        ...res.violations.flatMap(v => v.edges.map(e => ({
            fileName: e.caller, lineStart: 1, severity: 'ERROR', category: 'Architecture violation', type: `${v.from} -> ${v.to}`,
            message: `${kinds[v.from]} '${v.from}' must not depend on ${kinds[v.to]} '${v.to}' (calls ${e.symbol} from ${rel(e.callee)})`,
        }))),
        ...res.unassigned.map(f => ({
            fileName: f, lineStart: 1, severity: 'NORMAL', category: 'Unassigned file', type: 'dependencies.groups',
            message: 'File involved in a dependency but not in any group',
        })),
        ...res.unresolved_calls.map(u => ({
            fileName: u.file, lineStart: u.line, severity: 'LOW', category: 'Unresolved indirect call', type: '//@ calls',
            message: 'Unresolved function pointer call: add a //@ calls clause',
        })),
    ];
    return { _class: 'io.jenkins.plugins.analysis.core.restapi.ReportApi', issues };
}

function cmdRender() {
    if (!rawArg || !fs.existsSync(rawArg)) fail(`frama-c output not found: ${rawArg}`);
    const msg = extractResponse(fs.readFileSync(rawArg, 'utf8'));
    if (msg.error) fail('frama-c returned an error: ' + JSON.stringify(msg.error));
    const res = msg.result;
    if (!res || !Array.isArray(res.groups)) fail('unexpected result (frama-c diagnostics instead of the matrix?)');
    // Only .c files are grouped: headers are not reported as unassigned.
    res.unassigned = res.unassigned.filter(f => f.endsWith('.c'));

    const out = path.resolve(WS, outArg || 'report');
    fs.mkdirSync(out, { recursive: true });
    fs.writeFileSync(path.join(out, 'deps.json'), JSON.stringify(res, null, 2));
    fs.writeFileSync(path.join(out, 'matrix.html'), renderHtml(res));
    fs.writeFileSync(path.join(out, 'deps-issues.json'), JSON.stringify(renderIssues(res), null, 2));
    log(`OK: ${res.violations.length} violation(s), ${res.unassigned.length} unassigned, ${res.unresolved_calls.length} unresolved -> ${out}`);
}

if (MODE === 'groups') cmdGroups();
else if (MODE === 'render') cmdRender();
else fail('usage: deps_report.js groups <ws> | render <ws> <frama-c-output> [outDir]  [--config <file>]');