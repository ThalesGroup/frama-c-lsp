
'use strict';
const fs = require('fs');
const path = require('path');
const [, , MODE, wsArg, rawArg, outArg] = process.argv;
const WS = path.resolve(wsArg || process.cwd());
const log = (...m) => console.log('[deps]', ...m);
const fail = (m) => { console.error('[deps] ERREUR:', m); process.exit(1); };

function readJsonc(file) {
    const txt = fs.readFileSync(file, 'utf8');
    let out = '', i = 0, inStr = false;
    while (i < txt.length) {
        const c = txt[i], n = txt[i + 1];
        if (inStr) { out += c; if (c === '\\') { out += n; i += 2; continue; } if (c === '"') inStr = false; i++; continue; }
        if (c === '"') { inStr = true; out += c; i++; continue; }
        if (c === '/' && n === '/') { while (i < txt.length && txt[i] !== '\n') i++; continue; }
        if (c === '/' && n === '*') { i = txt.indexOf('*/', i + 2) + 2; continue; }
        out += c; i++;
    }
    return JSON.parse(out.replace(/,(\s*[}\]])/g, '$1'));
}
const settingsPath = path.join(WS, '.vscode', 'settings.json');
if (!fs.existsSync(settingsPath)) fail(`introuvable : ${settingsPath}`);
const settings = readJsonc(settingsPath);
const groups = settings['dependencies.groups'] || [];
const kinds = {};
for (const g of groups) kinds[g.name] = g.kind;

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
        const dir = path.dirname(p), base = path.basename(p);
        const re = new RegExp('^' + base.replace(/[.+^${}()|[\]\\]/g, '\\$&').replace(/\*/g, '[^/]*') + '$');
        if (!fs.existsSync(dir)) return [];
        return fs.readdirSync(dir).filter(f => f.endsWith('.c') && re.test(f)).map(f => path.join(dir, f));
    }
    return fs.existsSync(p) ? [p] : [];
}

function cmdGroups() {
    if (groups.length === 0) fail("'dependencies.groups' vide dans settings.json");
    const fileGroups = {};
    for (const g of groups) {
        if (!['core', 'feature', 'lib'].includes(g.kind)) fail(`groupe '${g.name}' : kind '${g.kind}' invalide (core|feature|lib)`);
        let n = 0;
        for (const p of g.paths) {
            const abs = path.isAbsolute(p) ? p : path.join(WS, p);
            for (const f of resolvePattern(abs)) { fileGroups[path.resolve(f)] = g.name; n++; }
        }
        log(`groupe ${g.name} (${g.kind}) : ${n} fichier(s)` + (n === 0 ? '  <-- aucun fichier !' : ''));
    }
    const files = Object.keys(fileGroups);
    if (files.length === 0) fail('aucun fichier .c résolu');
    const dir = path.join(WS, '.frama-c');
    fs.mkdirSync(dir, { recursive: true });
    fs.writeFileSync(path.join(dir, 'dep_groups.json'), JSON.stringify({ fileGroups, kinds }, null, 1));
    fs.writeFileSync(path.join(dir, 'files.txt'), files.join('\n') + '\n');
    // même construction que KernelOpt.string_of_t (lsp_handler.ml)
    const inc = (settings['kernel.includePaths'] || []).map(p => '-I' + (path.isAbsolute(p) ? p : path.join(WS, p)));
    const mac = (settings['kernel.macros'] || []).map(m => '-D' + m);
    fs.writeFileSync(path.join(dir, 'cpp_args.txt'), ['-CC', ...inc, ...mac].join(' '));
    log(`${files.length} fichier(s) -> .frama-c/dep_groups.json, files.txt, cpp_args.txt`);
}

const esc = s => String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
const rel = f => path.relative(WS, f).replace(/\\/g, '/');

function renderHtml(res) {
    const cells = new Map();
    res.matrix.forEach(c => cells.set(`${c.from}|${c.to}`, { c, bad: false }));
    res.violations.forEach(c => cells.set(`${c.from}|${c.to}`, { c, bad: true }));
    const names = res.groups.slice().sort();
    const head = names.map(n => `<th>${esc(n)}<br><small>${esc(kinds[n] || '')}</small></th>`).join('');
    const rows = names.map(from => `<tr><th class="row">${esc(from)}</th>` + names.map(to => {
        if (from === to) return '<td class="self">–</td>';
        const h = cells.get(`${from}|${to}`);
        return h ? `<td class="${h.bad ? 'bad' : 'ok'}"><a href="#${esc(from)}__${esc(to)}">${h.c.count}</a></td>` : '<td></td>';
    }).join('') + '</tr>').join('');
    const details = [...cells.values()].sort((a, b) => b.bad - a.bad).map(({ c, bad }) =>
        `<details id="${esc(c.from)}__${esc(c.to)}"${bad ? ' open' : ''}><summary class="${bad ? 'tbad' : ''}">` +
        `${esc(c.from)} → ${esc(c.to)} : ${c.count} appel(s)${bad ? ' — VIOLATION' : ''}</summary>` +
        `<table><tr><th>caller</th><th>callee</th><th>symbol</th></tr>` +
        c.edges.map(e => `<tr><td>${esc(rel(e.caller))}</td><td>${esc(rel(e.callee))}</td><td>${esc(e.symbol)}</td></tr>`).join('') +
        `</table></details>`).join('');
    const list = (t, items, cls) => items.length ? `<h3 class="${cls}">${t} (${items.length})</h3><ul>${items.map(i => `<li>${esc(i)}</li>`).join('')}</ul>` : '';
    return `<!DOCTYPE html><html><head><meta charset="utf-8"><title>Dependency Matrix</title><style>
body{font-family:sans-serif;padding:20px}table{border-collapse:collapse;margin:8px 0}
th,td{border:1px solid #ccc;padding:6px 10px;text-align:center}th{background:#f3f3f3}th.row{text-align:right}
.self{opacity:.4}.ok{background:#2e7d32}.bad{background:#c62828;font-weight:bold}.ok a,.bad a{color:#fff}
.tbad{color:#c62828;font-weight:bold}.warn{color:#e65100}details{margin:6px 0}details td{text-align:left;font-family:monospace;font-size:12px}
.sum span{display:inline-block;margin-right:18px}</style></head><body>
<h2>Dependency Matrix (groups)</h2>
<p class="sum"><span>Généré : ${esc(new Date().toISOString())}</span><span class="tbad">Violations : ${res.violations.length}</span><span class="warn">Non assignés : ${res.unassigned.length}</span>
<span class="warn">Appels non résolus : ${res.unresolved_calls.length}</span></p>
<p style="opacity:.7">Lignes : appelants · Colonnes : appelés · Rouge = violation d'architecture · Cliquer un nombre pour le détail</p>
<table><tr><th>from \\ to</th>${head}</tr>${rows}</table>
${list('Fichiers non assignés (compléter dependencies.groups)', res.unassigned.map(rel), 'warn')}
${list('Appels indirects non résolus (ajouter //@ calls)', res.unresolved_calls.map(u => `${rel(u.file)}:${u.line}`), 'warn')}
<h3>Détail des arêtes</h3>${details}</body></html>`;
}

// Format natif Warnings NG : https://github.com/jenkinsci/warnings-ng-plugin/blob/main/doc/Documentation.md
function renderIssues(res) {
    const issues = [];
    for (const v of res.violations) for (const e of v.edges) issues.push({
        fileName: e.caller, lineStart: 1, severity: 'ERROR', category: 'Architecture violation',
        type: `${v.from} -> ${v.to}`,
        message: `${kinds[v.from]} '${v.from}' ne doit pas dépendre de ${kinds[v.to]} '${v.to}' (appel de ${e.symbol} défini dans ${rel(e.callee)})`,
    });
    for (const f of res.unassigned) issues.push({
        fileName: f, lineStart: 1, severity: 'NORMAL', category: 'Unassigned file', type: 'dependencies.groups',
        message: 'Fichier impliqué dans une dépendance mais absent de dependencies.groups',
    });
    for (const u of res.unresolved_calls) issues.push({
        fileName: u.file, lineStart: u.line, severity: 'LOW', category: 'Unresolved indirect call', type: '//@ calls',
        message: "Appel par pointeur de fonction non résolu : ajouter une clause //@ calls",
    });
    return { _class: 'io.jenkins.plugins.analysis.core.restapi.ReportApi', issues };
}


function cmdRender() {
    if (!rawArg || !fs.existsSync(rawArg)) fail(`sortie frama-c introuvable : ${rawArg}`);
    // send_result sans -lsp-wrapper => Options.Self.result "%s" : une ligne "[lsp] {\"jsonrpc\":...}"
    // Frama-C imprime le JSON indenté sur plusieurs lignes : on prend le bloc qui suit "[lsp] {"
    // et on le délimite en comptant les accolades (hors chaînes).
    const raw = fs.readFileSync(rawArg, 'utf8');
    const m = /\[lsp\]\s*\{\s*"(jsonrpc|id|result)"/.exec(raw);
    if (!m) fail('aucune réponse JSON dans la sortie de frama-c (parsing raté ? voir le log)');
    const start = raw.indexOf('{', m.index);
    let depth = 0, inStr = false, end = -1;
    for (let i = start; i < raw.length; i++) {
        const c = raw[i];
        if (inStr) { if (c === '\\') i++; else if (c === '"') inStr = false; continue; }
        if (c === '"') inStr = true;
        else if (c === '{') depth++;
        else if (c === '}' && --depth === 0) { end = i; break; }
    }
    if (end < 0) fail('JSON tronqué dans la sortie de frama-c');
    const line = raw.slice(start, end + 1);
    const msg = JSON.parse(line);
    if (msg.error) fail('frama-c a répondu une erreur : ' + JSON.stringify(msg.error));
    const res = msg.result;
    if (!res || !Array.isArray(res.groups)) fail('résultat inattendu : ' + line.slice(0, 300));
    const OUT = path.resolve(WS, outArg || 'report');
    fs.mkdirSync(OUT, { recursive: true });
    fs.writeFileSync(path.join(OUT, 'deps.json'), JSON.stringify(res, null, 2));
    fs.writeFileSync(path.join(OUT, 'matrix.html'), renderHtml(res));
    fs.writeFileSync(path.join(OUT, 'deps-issues.json'), JSON.stringify(renderIssues(res), null, 2));
    log(`OK : ${res.violations.length} violation(s), ${res.unassigned.length} non assigné(s), ${res.unresolved_calls.length} non résolu(s) -> ${OUT}`);
}

if (MODE === 'groups') cmdGroups();
else if (MODE === 'render') cmdRender();
else fail('usage : deps_report.js groups <ws> | render <ws> <frama-c-stdout> [outDir]');