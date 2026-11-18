
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

// Stylesheet written next to matrix.html: Jenkins' default CSP blocks inline <style> and JS.
const CSS = `
*{box-sizing:border-box}
body{margin:0;padding:28px 36px;font-family:"Segoe UI",Roboto,Arial,sans-serif;font-size:14px;color:#1f2430;background:#f6f7fb}
h1{margin:0 0 4px;font-size:22px;color:#242a75}
.sub{color:#6b7084;font-size:12px;margin-bottom:20px}
.cards{display:flex;gap:14px;margin-bottom:24px;flex-wrap:wrap}
.card{background:#fff;border-radius:8px;padding:14px 20px;min-width:150px;box-shadow:0 1px 3px rgba(0,0,0,.08)}
.card b{display:block;font-size:26px;color:#242a75}
.card span{font-size:12px;color:#6b7084;text-transform:uppercase;letter-spacing:.04em}
.card.bad b{color:#c62828}.card.warn b{color:#e65100}
.panel{background:#fff;border-radius:8px;padding:18px 20px;margin-bottom:22px;box-shadow:0 1px 3px rgba(0,0,0,.08)}
.panel h2{margin:0 0 12px;font-size:16px;color:#242a75}
.legend{font-size:12px;color:#6b7084;margin:0 0 12px}
.legend i{display:inline-block;width:12px;height:12px;border-radius:2px;vertical-align:-2px;margin:0 4px 0 12px}
.legend i.ok{background:#2e7d32}.legend i.bad{background:#c62828}
.scroll{overflow-x:auto}
table{border-collapse:collapse}
.matrix th,.matrix td{border:1px solid #e3e5ee;padding:8px 10px;text-align:center;white-space:nowrap}
.matrix thead th{background:#242a75;color:#fff;font-weight:600;font-size:12px}
.matrix thead th small{display:block;font-weight:400;opacity:.75}
.matrix th.row{background:#eef0f7;color:#242a75;text-align:left;font-size:12px}
.matrix th.row small{color:#6b7084;font-weight:400;margin-left:6px}
.matrix td.self{background:#f3f4f8;color:#b8bccb}
.matrix td a{display:block;color:#fff;font-weight:700;text-decoration:none;border-radius:4px;padding:2px 0}
.matrix td.ok{background:#2e7d32}.matrix td.bad{background:#c62828}
.matrix td.ok:hover,.matrix td.bad:hover{filter:brightness(1.15)}
.edge{border:1px solid #e3e5ee;border-radius:6px;margin-bottom:10px;scroll-margin-top:12px}
.edge .hd{display:block;padding:10px 14px;font-weight:600;color:#242a75;text-decoration:none}
.edge .hd:before{content:"\\25B8";display:inline-block;margin-right:8px}
.edge.bad .hd{color:#c62828}
.edge .body{display:none}
.edge.bad .body,.edge:target .body{display:block}
.edge.bad .hd:before,.edge:target .hd:before{content:"\\25BE"}
.tag{font-size:11px;font-weight:600;padding:2px 8px;border-radius:10px;margin-left:10px;background:#eef0f7;color:#242a75}
.edge.bad .tag{background:#fdecea;color:#c62828}
.edge table{table-layout:fixed}.edge th:nth-child(1){width:34%}.edge th:nth-child(2),.edge th:nth-child(3){width:33%}
.edge:target{border:2px solid #00bbdd;box-shadow:0 0 0 3px rgba(0,187,221,.15)}
.edge table{width:100%;margin:0 0 4px;border-collapse:collapse}
.edge th,.edge td{text-align:left;padding:7px 14px;border-top:1px solid #eef0f7;font-size:13px}
.edge th{font-size:11px;text-transform:uppercase;color:#6b7084;background:#fafbfd}
.edge td.fn{font-family:Consolas,Menlo,monospace;color:#1f2430;font-weight:600}
.edge td.file{font-family:Consolas,Menlo,monospace;color:#4a4f63}
.back{display:inline-block;margin:6px 14px 10px;font-size:12px;color:#00a0c0;text-decoration:none}
ul.files{margin:0;padding-left:18px;font-family:Consolas,Menlo,monospace;font-size:13px;color:#4a4f63}
.empty{color:#2e7d32;font-weight:600}
`;

function renderHtml(res) {
    const cells = new Map();
    res.matrix.forEach(c => cells.set(`${c.from}|${c.to}`, { c, bad: false }));
    res.violations.forEach(c => cells.set(`${c.from}|${c.to}`, { c, bad: true }));
    const names = [...res.groups].sort();
    const id = c => esc(`${c.from}__${c.to}`);
    const kind = n => kinds[n] ? `<small>${esc(kinds[n])}</small>` : '';

    const head = names.map(n => `<th>${esc(n)}${kind(n)}</th>`).join('');
    const rows = names.map(from => `<tr><th class="row">${esc(from)}${kind(from)}</th>` + names.map(to => {
        if (from === to) return '<td class="self">–</td>';
        const h = cells.get(`${from}|${to}`);
        return h ? `<td class="${h.bad ? 'bad' : 'ok'}"><a href="#${id(h.c)}" title="${esc(from)} → ${esc(to)}">${h.c.count}</a></td>` : '<td></td>';
    }).join('') + '</tr>').join('');

    // Violations first and always expanded; other edges open when targeted (matrix cell or header click). Pure CSS, no JS.
    const edges = [...cells.values()]
        .sort((a, b) => b.bad - a.bad || a.c.from.localeCompare(b.c.from) || a.c.to.localeCompare(b.c.to))
        .map(({ c, bad }) =>
            `<div class="edge${bad ? ' bad' : ''}" id="${id(c)}">` +
            `<a class="hd" href="#${id(c)}">${esc(c.from)} → ${esc(c.to)}<span class="tag">${c.count} call(s)${bad ? ' · violation' : ''}</span></a>` +
            `<div class="body"><table><tr><th>Function</th><th>Defined in</th><th>Used in</th></tr>` +
            [...c.edges]
                .sort((a, b) => a.symbol.localeCompare(b.symbol) || rel(a.caller).localeCompare(rel(b.caller)))
                .map(e => `<tr><td class="fn">${esc(e.symbol)}</td><td class="file">${esc(rel(e.callee))}</td><td class="file">${esc(rel(e.caller))}</td></tr>`).join('') +
            `</table><a class="back" href="#top">↑ Back to matrix</a></div></div>`).join('');

    const panel = (title, items) => items.length
        ? `<div class="panel"><h2>${title} (${items.length})</h2><ul class="files">${items.map(i => `<li>${esc(i)}</li>`).join('')}</ul></div>` : '';

    const card = (n, label, cls = '') => `<div class="card ${n ? cls : ''}"><b>${n}</b><span>${label}</span></div>`;
    const nCalls = res.matrix.concat(res.violations).reduce((s, c) => s + c.count, 0);

    return `<!DOCTYPE html><html lang="en"><head><meta charset="utf-8"><title>Dependency Matrix</title>
<link rel="stylesheet" href="style.css"></head><body>
<h1 id="top">Dependency Matrix</h1>
<div class="sub">Generated ${esc(new Date().toISOString().replace('T', ' ').slice(0, 16))} UTC · ${names.length} groups</div>
<div class="cards">
${card(res.violations.length, 'Violations', 'bad')}
${card(nCalls, 'Cross-group calls')}
${card(res.unassigned.length, 'Unassigned files', 'warn')}
${card(res.unresolved_calls.length, 'Unresolved calls', 'warn')}
</div>
<div class="panel"><h2>Group dependencies</h2>
<p class="legend">Rows: caller group · Columns: called group ·<i class="ok"></i>Allowed<i class="bad"></i>Violation · Click a number to see the calls</p>
<div class="scroll"><table class="matrix"><thead><tr><th>caller \\ called</th>${head}</tr></thead><tbody>${rows}</tbody></table></div></div>
<div class="panel"><h2>Calls between groups</h2>${edges || '<p class="empty">No cross-group calls.</p>'}</div>
${panel('Unassigned files — add them to dependencies.groups', res.unassigned.map(rel))}
${panel('Unresolved indirect calls — add a //@ calls clause', res.unresolved_calls.map(u => `${rel(u.file)}:${u.line}`))}
</body></html>`;
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
    fs.writeFileSync(path.join(out, 'style.css'), CSS);
    fs.writeFileSync(path.join(out, 'deps-issues.json'), JSON.stringify(renderIssues(res), null, 2));
    log(`OK: ${res.violations.length} violation(s), ${res.unassigned.length} unassigned, ${res.unresolved_calls.length} unresolved -> ${out}`);
}

if (MODE === 'groups') cmdGroups();
else if (MODE === 'render') cmdRender();
else fail('usage: deps_report.js groups <ws> | render <ws> <frama-c-output> [outDir]  [--config <file>]');