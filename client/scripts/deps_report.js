#!/usr/bin/env node

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
// Accepted dependencies, applied by the Frama-C plugin (real_deps):
//   group level     { from, to }
//   function level  { caller, callee, fromCaller, fromCallee }: accepted if fromCaller calls fromCallee
//                   and caller is reachable from fromCallee in the callgraph.
const exclusions = settings['dependencies.exclusions'] || [];
// .c files placed in several execution regions of different groups (written by gen_groups.py).
const sharedFiles = settings['dependencies.sharedFiles'] || [];
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

// Frama-C kernel options built from the config, mirroring what the LSP server passes locally.
function frama_c_options() {
    const opts = [];
    if (settings['kernel.machdep']) opts.push(`-machdep=${settings['kernel.machdep']}`);
    if (settings['kernel.noAnnot']) opts.push('-no-annot');
    // Always added by the server (KernelOpt), so CI matches VS Code.
    opts.push('-keep-unused-functions=none', '-inline-calls=@inline', '-remove-inlined=@inline', '-no-unicode');
    // Optional extra kernel arguments, e.g. ["-aggressive-merging", "-kernel-warn-key=annot-error=active"].
    opts.push(...(settings['kernel.extraArgs'] || []));
    return opts;
}

function cmdGroups() {
    if (groups.length === 0) fail(`'dependencies.groups' is empty in ${CONFIG}`);
    checkExclusions();
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
    fs.writeFileSync(path.join(dir, 'dep_groups.json'), JSON.stringify({ fileGroups, kinds, exclusions }, null, 1));
    fs.writeFileSync(path.join(dir, 'files.txt'), files.join('\n') + '\n');
    fs.writeFileSync(path.join(dir, 'cpp_args.txt'), ['-CC', ...inc, ...mac].join(' '));
    fs.writeFileSync(path.join(dir, 'fc_opts.txt'), frama_c_options().join(' '));
    log(`${files.length} file(s) -> .frama-c/dep_groups.json, files.txt, cpp_args.txt, fc_opts.txt`);
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

// ---------- exclusions ----------
// Applied by the plugin; checkExclusions only validates the format, applyExclusions is the fallback
// for an older plugin that does not return "excluded".
function matches(x, from, to, e) {
    return (x.from === undefined || x.from === from)
        && (x.to === undefined || x.to === to)
        && (x.caller === undefined || x.caller === e.caller_fn)
        && (x.callee === undefined || x.callee === e.symbol);
}

function checkExclusions() {
    exclusions.forEach((x, i) => {
        const groupLevel = x.from !== undefined && x.to !== undefined;
        const functionLevel = x.caller !== undefined && x.callee !== undefined;
        if (!groupLevel && !functionLevel)
            fail(`dependencies.exclusions[${i}]: expected { from, to } or { caller, callee, fromCaller, fromCallee }`);
        if ((x.fromCaller === undefined) !== (x.fromCallee === undefined))
            fail(`dependencies.exclusions[${i}]: set both fromCaller and fromCallee, or neither`);
        // A group with no .c file is not in dependencies.groups: the exclusion simply never matches.
        for (const g of [x.from, x.to])
            if (g !== undefined && !(g in kinds)) log(`note: dependencies.exclusions[${i}]: group '${g}' has no file`);
    });
}

// Moves excluded calls out of the matrix and the violations (shown as allowed, in green, in the report).
function applyExclusions(res) {
    const excluded = [];
    const used = new Set();
    const filter = (cells, violation) => cells.flatMap(c => {
        const kept = c.edges.filter(e => {
            const i = exclusions.findIndex(x => matches(x, c.from, c.to, e));
            if (i < 0) return true;
            used.add(i);
            excluded.push({ from: c.from, to: c.to, violation, ...e, reason: exclusions[i].reason || '' });
            return false;
        });
        return kept.length ? [{ ...c, edges: kept, count: kept.length }] : [];
    });
    res.matrix = filter(res.matrix, false);
    res.violations = filter(res.violations, true);
    res.excluded = excluded;
    // Hand-written exclusions that matched nothing are probably stale (renamed function, merged group).
    // platform.cfg requires are permissions, not obligations: an unused one is normal.
    res.unused_exclusions = exclusions.filter((x, i) => !used.has(i) && !String(x.reason || '').startsWith('platform.cfg:'));
}

const describeExclusion = x =>
    (x.caller !== undefined ? `${x.caller} → ${x.callee}` : `${x.from} → ${x.to}`)
    + (x.fromCaller ? `, from ${x.fromCaller} → ${x.fromCallee}` : '') + (x.reason ? ` — ${x.reason}` : '');
const describeCall = e => `${e.from} → ${e.to} : ${e.caller_fn || '?'} → ${e.symbol}` + (e.context ? `, from ${e.context}` : '');

// Stylesheet embedded in matrix.html.
const CSS = `
*{box-sizing:border-box}
body{margin:0;padding:28px 36px;font-family:"Segoe UI",Roboto,Arial,sans-serif;font-size:14px;color:#1f2430;background:#f6f7fb}
h1{margin:0 0 4px;font-size:22px;color:#242a75}
.sub{color:#6b7084;font-size:12px;margin-bottom:20px}
.cards{display:flex;gap:14px;margin-bottom:24px;flex-wrap:wrap}
.card{background:#fff;border-radius:8px;padding:14px 20px;min-width:150px;box-shadow:0 1px 3px rgba(0,0,0,.08)}
.card b{display:block;font-size:26px;color:#242a75}
.card span{font-size:12px;color:#6b7084;text-transform:uppercase;letter-spacing:.04em}
.card.bad b{color:#c62828}.card.warn b{color:#e65100}.card.excl b{color:#2e7d32}
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
.matrix td.ok{background:#2e7d32}.matrix td.bad{background:#c62828}.matrix td.excl{background:#2e7d32}
.matrix td.ok:hover,.matrix td.bad:hover,.matrix td.excl:hover{filter:brightness(1.15)}
.matrix td small{display:block;font-size:10px;font-weight:400;opacity:.9}
.edge.excl .hd{color:#2e7d32}.edge.excl .tag{background:#e8f5e9;color:#2e7d32}
.edge tr.excl td{background:#f3faf3}.edge tr.excl td.why{color:#2e7d32;font-style:italic}
.edge{border:1px solid #e3e5ee;border-radius:6px;margin-bottom:10px;scroll-margin-top:12px}
.edge .hd{display:block;padding:10px 14px;font-weight:600;color:#242a75;text-decoration:none}
.edge .hd:before{content:"\\25B8";display:inline-block;margin-right:8px}
.edge.bad .hd{color:#c62828}
.edge .body{display:none}
.edge.bad .body,.edge:target .body{display:block}
.edge.bad .hd:before,.edge:target .hd:before{content:"\\25BE"}
.tag{font-size:11px;font-weight:600;padding:2px 8px;border-radius:10px;margin-left:10px;background:#eef0f7;color:#242a75}
.edge.bad .tag{background:#fdecea;color:#c62828}
.edge table{table-layout:fixed}.edge th:nth-child(1),.edge th:nth-child(2){width:20%}.edge th:nth-child(3),.edge th:nth-child(4){width:30%}
.edge table.note th:nth-child(1),.edge table.note th:nth-child(2){width:17%}.edge table.note th:nth-child(3),.edge table.note th:nth-child(4){width:25%}.edge table.note th:nth-child(5){width:16%}
.edge td.fn{overflow-wrap:break-word}.edge td.file{overflow-wrap:normal}
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
    // One entry per (from, to): kept calls (green/red) and excluded calls (green, marked 'excluded').
    const pairs = new Map();
    const pair = (from, to) => {
        const k = `${from}|${to}`;
        if (!pairs.has(k)) pairs.set(k, { from, to, kept: [], excluded: [], bad: false });
        return pairs.get(k);
    };
    res.matrix.forEach(c => pair(c.from, c.to).kept.push(...c.edges));
    res.violations.forEach(c => { const p = pair(c.from, c.to); p.kept.push(...c.edges); p.bad = true; });
    res.excluded.forEach(e => pair(e.from, e.to).excluded.push(e));
    const names = [...res.groups].sort();
    const id = p => esc(`${p.from}__${p.to}`);
    const kind = n => kinds[n] ? `<small>${esc(kinds[n])}</small>` : '';
    const state = p => p.kept.length ? (p.bad ? 'bad' : 'ok') : 'excl';

    const head = names.map(n => `<th>${esc(n)}${kind(n)}</th>`).join('');
    const rows = names.map(from => `<tr><th class="row">${esc(from)}${kind(from)}</th>` + names.map(to => {
        if (from === to) return '<td class="self">–</td>';
        const p = pairs.get(`${from}|${to}`);
        if (!p) return '<td></td>';
        const n = p.kept.length || p.excluded.length;
        const extra = p.kept.length && p.excluded.length ? `<small>+${p.excluded.length} excl.</small>` : '';
        return `<td class="${state(p)}"><a href="#${id(p)}" title="${esc(from)} → ${esc(to)}">${n}${extra}</a></td>`;
    }).join('') + '</tr>').join('');

    // Violations first and always expanded; other edges open when targeted. Pure CSS, no JS.
    const order = { bad: 0, excl: 1, ok: 2 };
    const filePath = f => esc(rel(f)).replace(/\//g, '/<wbr>');          // line breaks only after '/'
    const row = (e, excl, note) => `<tr${excl ? ' class="excl"' : ''}><td class="fn">${esc(e.caller_fn || '—')}</td><td class="fn">${esc(e.symbol)}</td>` +
        `<td class="file">${filePath(e.caller)}</td><td class="file">${filePath(e.callee)}</td>` +
        (note ? `<td class="why">${excl ? 'excluded' + (e.context ? ', from ' + esc(e.context) : '') + (e.reason ? ': ' + esc(e.reason) : '') : ''}</td>` : '') + '</tr>';
    // Violations/allowed calls first, then excluded ones; sorted by file then function.
    const callRows = (kept, excl) => [...kept.map(e => [e, false]), ...excl.map(e => [e, true])]
        .sort(([a, xa], [b, xb]) => xa - xb || rel(a.caller).localeCompare(rel(b.caller))
              || rel(a.callee).localeCompare(rel(b.callee)) || a.symbol.localeCompare(b.symbol))
        .map(([e, x]) => row(e, x, excl.length > 0)).join('');
    const edges = [...pairs.values()]
        .sort((a, b) => order[state(a)] - order[state(b)] || a.from.localeCompare(b.from) || a.to.localeCompare(b.to))
        .map(p => {
            const tag = [p.kept.length && `${p.kept.length} call(s)`, p.bad && 'violation',
                         p.excluded.length && `${p.excluded.length} excluded`].filter(Boolean).join(' · ');
            return `<div class="edge ${state(p)}" id="${id(p)}">` +
                `<a class="hd" href="#${id(p)}">${esc(p.from)} → ${esc(p.to)}<span class="tag">${tag}</span></a>` +
                `<div class="body"><table class="${p.excluded.length ? 'note' : ''}"><tr><th>Caller</th><th>Function</th><th>Used in</th><th>Defined in</th>${p.excluded.length ? '<th></th>' : ''}</tr>` + callRows(p.kept, p.excluded) +
                `</table><a class="back" href="#top">↑ Back to matrix</a></div></div>`;
        }).join('');

    const panel = (title, items) => items.length
        ? `<div class="panel"><h2>${title} (${items.length})</h2><ul class="files">${items.map(i => `<li>${esc(i)}</li>`).join('')}</ul></div>` : '';

    const card = (n, label, cls = '') => `<div class="card ${n ? cls : ''}"><b>${n}</b><span>${label}</span></div>`;
    const nCalls = res.matrix.concat(res.violations).reduce((s, c) => s + c.count, 0);

    return `<!DOCTYPE html><html lang="en"><head><meta charset="utf-8"><title>Dependency Matrix</title>
<style>${CSS}</style></head><body>
<h1 id="top">Dependency Matrix</h1>
<div class="sub">Generated ${esc(new Date().toISOString().replace('T', ' ').slice(0, 16))} UTC · ${names.length} groups</div>
<div class="cards">
${card(res.violations.length, 'Violations', 'bad')}
${card(nCalls, 'Cross-group calls')}
${card(res.unassigned.length, 'Unassigned files', 'warn')}
${card(res.unresolved_calls.length, 'Unresolved calls', 'warn')}
${card(res.excluded.length, 'Excluded calls', 'excl')}
${card(sharedFiles.length, 'Files in several groups', 'warn')}
</div>
<div class="panel"><h2>Group dependencies</h2>
<p class="legend">Rows: caller group · Columns: called group ·<i class="ok"></i>Allowed (rule or dependencies.exclusions)<i class="bad"></i>Violation · Click a number to see the calls</p>
<div class="scroll"><table class="matrix"><thead><tr><th>caller \\ called</th>${head}</tr></thead><tbody>${rows}</tbody></table></div></div>
<div class="panel"><h2>Calls between groups</h2>${edges || '<p class="empty">No cross-group calls.</p>'}</div>
${panel('Files in several groups — placed in execution regions of different features (counted in the first one)',
    sharedFiles.map(f => `${f.file} — ${f.groups.join(', ')} (counted in ${f.kept})`))}
${panel('Unassigned files — add them to dependencies.groups', res.unassigned.map(rel))}
${panel('Unresolved indirect calls — add a //@ calls clause', res.unresolved_calls.map(u => `${rel(u.file)}:${u.line}`))}
${panel('Excluded calls — accepted in dependencies.exclusions', res.excluded.map(e =>
    `${describeCall(e)} (${rel(e.caller)})` + (e.reason ? ` — ${e.reason}` : '')
    + ((e.callers || []).length ? ` [no fromCaller/fromCallee yet — callers of ${e.caller_fn}: ${e.callers.join(', ')}]` : '')))}
${panel('Exclusions not applied — no callgraph path from fromCaller → fromCallee to the caller (the call stays in the matrix)',
    res.invalid_exclusions.map(e => describeCall(e) + (e.callers.length ? ` (callers of ${e.caller_fn}: ${e.callers.join(', ')})` : '')))}
${panel('Unused exclusions — matched no call, check dependencies.exclusions', res.unused_exclusions.map(describeExclusion))}
</body></html>`;
}

// Warnings NG native JSON format
function renderIssues(res) {
    const issues = [
        ...res.violations.flatMap(v => v.edges.map(e => ({
            fileName: e.caller, lineStart: 1, severity: 'ERROR', category: 'Architecture violation', type: `${v.from} -> ${v.to}`,
            message: `${kinds[v.from]} '${v.from}' must not depend on ${kinds[v.to]} '${v.to}' (${e.caller_fn || '?'} calls ${e.symbol} from ${rel(e.callee)})`,
        }))),
        ...sharedFiles.map(f => ({
            fileName: abs(f.file), lineStart: 1, severity: 'NORMAL', category: 'File in several groups', type: f.groups.join(', '),
            message: `File placed in several groups (${f.groups.join(', ')}); counted in ${f.kept}`,
        })),
        ...res.invalid_exclusions.map(e => ({
            fileName: e.caller, lineStart: 1, severity: 'NORMAL', category: 'Exclusion not applied', type: `${e.caller_fn} -> ${e.symbol}`,
            message: `Exclusion ${e.caller_fn} -> ${e.symbol}${e.context ? ', from ' + e.context : ''}: no callgraph path, call kept`,
        })),
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
    checkExclusions();
    if (Array.isArray(res.excluded)) {
        // Exclusions applied by the plugin. platform.cfg requires are permissions: an unused one is normal.
        res.invalid_exclusions = res.invalid_exclusions || [];
        res.unused_exclusions = (res.unused_exclusions || []).filter(x => !String(x.reason || '').startsWith('platform.cfg:'));
    } else {
        log('note: the plugin did not apply the exclusions (old version): applied here, without the path check');
        applyExclusions(res);
        res.invalid_exclusions = [];
    }

    const out = path.resolve(WS, outArg || 'report');
    fs.mkdirSync(out, { recursive: true });
    fs.writeFileSync(path.join(out, 'deps.json'), JSON.stringify(res, null, 2));
    fs.writeFileSync(path.join(out, 'matrix.html'), renderHtml(res));
    fs.writeFileSync(path.join(out, 'deps-issues.json'), JSON.stringify(renderIssues(res), null, 2));
    log(`OK: ${res.violations.length} violation(s), ${res.excluded.length} excluded, ${res.invalid_exclusions.length} not applied, ${sharedFiles.length} shared file(s), ${res.unassigned.length} unassigned, ${res.unresolved_calls.length} unresolved -> ${out}`);
    if (res.unused_exclusions.length) log(`WARNING: ${res.unused_exclusions.length} exclusion(s) matched no call`);
}

if (MODE === 'groups') cmdGroups();
else if (MODE === 'render') cmdRender();
else fail('usage: deps_report.js groups <ws> | render <ws> <frama-c-output> [outDir]  [--config <file>]');