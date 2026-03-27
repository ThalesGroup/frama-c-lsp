import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import { XMLParser } from 'fast-xml-parser';

export class ProjectImporter {
    private readonly parser = new XMLParser({
        ignoreAttributes: false,
        attributeNamePrefix: "@_",
        processEntities: false
    });

    private linkedPaths: Map<string, string> = new Map();
    private globalExclusions: string[] = [];

    private decodeAndClean(str: string): string {
        if (!str) return "";
        return str.replace(/&quot;/g, '"').replace(/&apos;/g, "'").replace(/&lt;/g, '<').replace(/&gt;/g, '>').replace(/&amp;/g, '&')
            .replace(/['"]/g, '').trim();
    }

    public async showProjectSelector() {
        const files = await vscode.workspace.findFiles('**/*.{vcxproj,uvprojx,uvproj,cproject}', '**/node_modules/**');
        
        const items: any[] = files.map(uri => ({
            label: `$(project) ${path.basename(uri.fsPath)}`,
            description: vscode.workspace.asRelativePath(uri),
            uri: uri
        }));

        items.unshift({
            label: "$(folder-opened) Choisir un fichier externe...",
            description: "Sélectionner un projet IDE en dehors du workspace",
            action: "browse"
        });

        const selected = await vscode.window.showQuickPick(items, { 
            placeHolder: "Sélectionnez un projet IDE ou parcourez votre disque" 
        });

        if (!selected) return;

        if (selected.action === "browse") {
            const fileUri = await vscode.window.showOpenDialog({
                canSelectMany: false,
                openLabel: 'Importer ce projet',
                filters: {
                    'Projets IDE': ['vcxproj', 'uvprojx', 'uvproj', 'cproject']
                }
            });

            if (fileUri && fileUri[0]) {
                await this.processProjectFile(fileUri[0]);
            }
        } else {
            await this.processProjectFile(selected.uri);
        }
    }

    private async processProjectFile(uri: vscode.Uri) {
        try {
            const projectFileDir = path.dirname(uri.fsPath);
            const workspaceFolder = vscode.workspace.getWorkspaceFolder(uri);
            const workspaceRoot = workspaceFolder ? workspaceFolder.uri.fsPath : projectFileDir;

            this.linkedPaths.clear();
            this.globalExclusions = [];

            const dotProject = path.join(projectFileDir, '.project');
            if (fs.existsSync(dotProject)) {
                this.parseEclipseLinks(dotProject, projectFileDir);
            }

            const xmlContent = fs.readFileSync(uri.fsPath, 'utf-8');
            const jsonObj = this.parser.parse(xmlContent);

            let data = {
                includes: [] as string[],
                macros: [] as string[],
                sources: [] as string[],
                projectDir: projectFileDir,
                rootDir: workspaceRoot
            };

            const filePath = uri.fsPath.toLowerCase();
            if (filePath.endsWith('.vcxproj')) {
                this.extractVcxproj(jsonObj, data);
            } else if (filePath.endsWith('.uvprojx')) {
                this.extractUvprojx(jsonObj, data);
            } else if (filePath.endsWith('.uvproj')) {
                this.extractUvproj(jsonObj, data);
            } else if (filePath.endsWith('.cproject')) {
                this.extractCProject(jsonObj, data);
            }

            await this.applyFinalSettings(data);
        } catch (error) {
            vscode.window.showErrorMessage("Import error : " + error);
        }
    }

    private parseEclipseLinks(filePath: string, projectDir: string) {
        const obj = this.parser.parse(fs.readFileSync(filePath, 'utf-8'));
        const links = this.toArray(obj.projectDescription?.linkedResources?.link);

        links.forEach(l => {
            const locUri = l.locationURI || "";
            const match = locUri.match(/PARENT-(\d+)-PROJECT_LOC\/(.*)/);
            if (match) {
                const depth = parseInt(match[1]);
                let resolvedBase = projectDir;
                for (let i = 0; i < depth; i++) resolvedBase = path.dirname(resolvedBase);
                this.linkedPaths.set(l.name, path.join(resolvedBase, match[2]));
            }
        });
    }

    private extractCProject(obj: any, data: any) {
        const entries = this.findAllRecursive(obj, "entry");
        entries.forEach(e => {
            if (e["@_kind"] === "sourcePath") {
                const excluding = e["@_excluding"] || "";
                if (excluding) this.globalExclusions.push(...excluding.split('|'));
            }
        });

        const options = this.findAllRecursive(obj, "option");
        options.forEach((opt: any) => {
            const sc = opt["@_superClass"] || "";
            const isInc = sc.includes("include.paths");
            const isDef = sc.includes("preprocessor.def.symbols");

            if (isInc || isDef) {
                this.toArray(opt.listOptionValue).forEach(v => {
                    let val = this.decodeAndClean(v["@_value"]);
                    if (!val) return;
                    let resolved = val;
                    this.linkedPaths.forEach((abs, virt) => {
                        const marker = `/${virt}/`;
                        if (val.includes(marker)) resolved = path.join(abs, val.split(marker)[1]);
                        else if (val.endsWith(`/${virt}`)) resolved = abs;
                    });
                    if (isInc) data.includes.push(resolved);
                    else data.macros.push(resolved);
                });
            }
        });

        this.linkedPaths.forEach((absPath, virtualName) => {
            if (fs.existsSync(absPath)) {
                const stat = fs.statSync(absPath);
                if (stat.isDirectory()) {
                    data.sources.push(...this.scanForCFiles(absPath, virtualName));
                } else if (absPath.endsWith('.c')) {
                    data.sources.push(absPath);
                }
            }
        });
    }

    private extractUvproj(obj: any, data: any) {
        this.toArray(obj.Project?.Targets?.Target).forEach(t => {
            const ctrl = t.TargetOption?.TargetArmAds?.Cads?.VariousControls;
            if (ctrl?.IncludePath) ctrl.IncludePath.split(';').forEach((p: string) => data.includes.push(p));
            if (ctrl?.Define) data.macros.push(...ctrl.Define.split(','));
            this.toArray(t.Groups?.Group).forEach(g => {
                this.toArray(g.Files?.File).forEach(f => {
                    if (f.FileType === 1 && f.FilePath) data.sources.push(f.FilePath);
                });
            });
        });
    }

    private extractUvprojx(obj: any, data: any) {
        this.toArray(obj.Project?.Targets?.Target).forEach(t => {
            const ctrl = t.TargetOption?.TargetArmAds?.Cads?.VariousControls;
            if (ctrl?.IncludePath) ctrl.IncludePath.split(';').forEach((p: string) => data.includes.push(p));
            if (ctrl?.Define) data.macros.push(...ctrl.Define.split(','));
            this.toArray(t.Groups?.Group).forEach(g => {
                this.toArray(g.Files?.File).forEach(f => {
                    if (f.FilePath) data.sources.push(f.FilePath);
                });
            });
        });
    }

    private extractVcxproj(obj: any, data: any) {
        this.toArray(obj.Project?.ItemDefinitionGroup).forEach(g => {
            const cl = g?.ClCompile;
            if (cl?.AdditionalIncludeDirectories) {
                cl.AdditionalIncludeDirectories.split(';').forEach((p: string) => {
                    if (p && !p.includes("%(")) data.includes.push(p);
                });
            }
            if (cl?.PreprocessorDefinitions) {
                cl.PreprocessorDefinitions.split(';').forEach((m: string) => data.macros.push(m));
            }
        });
        this.toArray(obj.Project?.ItemGroup).forEach(ig => {
            this.toArray(ig?.ClCompile).forEach(c => {
                const inc = c?.["@_Include"];
                if (inc) data.sources.push(inc);
            });
        });
    }

    private async applyFinalSettings(data: any) {
        const config = vscode.workspace.getConfiguration();
        const root = data.rootDir;

        const clean = (list: string[]): string[] => {
            const unique = new Set<string>();
            list.forEach(item => {
                if (!item) return;
                let p = item.replace(/\\/g, '/');
                p = p.replace(/\${workspace_loc:\/.*?}/g, '').replace(/}/g, '').trim();
                const absolutePath = path.resolve(data.projectDir, p);
                const normalizedPath = path.normalize(absolutePath);
                let rel = path.relative(root, normalizedPath).replace(/\\/g, '/');
                if (!rel.startsWith('.') && !rel.startsWith('/')) rel = './' + rel;
                if (rel && !rel.includes('home/user')) unique.add(rel);
            });
            return Array.from(unique);
        };

        const finalIncludes = clean(data.includes);
        const finalSources = clean(data.sources);
        const finalMacros: string[] = Array.from(new Set(data.macros.map((m: any) =>
            m.toString().split('=')[0].trim()).filter((m: any) => m && !m.includes('%'))
        ));

        await config.update("kernel.includePaths", finalIncludes, vscode.ConfigurationTarget.Workspace);
        await config.update("kernel.sourceFiles", finalSources, vscode.ConfigurationTarget.Workspace);
        await config.update("kernel.macros", finalMacros, vscode.ConfigurationTarget.Workspace);
        await config.update("kernel.machdep", "x86_32", vscode.ConfigurationTarget.Workspace);

        await this.updateIntelliSense(root, finalIncludes, finalMacros);
        vscode.window.showInformationMessage(` Import réussi : ${finalSources.length} sources.`);
    }

    private async updateIntelliSense(root: string, includes: string[], defines: string[]) {
        const vscodeDir = path.join(root, '.vscode');
        const configPath = path.join(vscodeDir, 'c_cpp_properties.json');
        if (!fs.existsSync(vscodeDir)) fs.mkdirSync(vscodeDir, { recursive: true });

        let cppConfig = {
            configurations: [{
                name: "JCAT-Imported",
                includePath: ["${workspaceFolder}/**", ...includes],
                defines: defines,
                intelliSenseMode: process.platform === 'win32' ? "windows-gcc-x86" : "linux-gcc-x86",
                compilerPath: "/usr/bin/gcc",
                cStandard: "c11"
            }],
            version: 4
        };
        if (fs.existsSync(configPath)) {
            try {
                const existing = JSON.parse(fs.readFileSync(configPath, 'utf-8'));
                const idx = existing.configurations.findIndex((c: any) => c.name === "JCAT-Imported");
                if (idx !== -1) {
                    existing.configurations[idx].includePath = cppConfig.configurations[0].includePath;
                    existing.configurations[idx].defines = cppConfig.configurations[0].defines;
                } else existing.configurations.push(cppConfig.configurations[0]);
                cppConfig = existing;
            } catch (e) { }
        }
        fs.writeFileSync(configPath, JSON.stringify(cppConfig, null, 4), 'utf-8');
    }

    private scanForCFiles(dir: string, virtualRoot: string): string[] {
        let results: string[] = [];
        if (!fs.existsSync(dir)) return results;
        const list = fs.readdirSync(dir);
        for (const file of list) {
            const fullPath = path.join(dir, file);
            const currentRelPath = (virtualRoot + "/" + path.relative(dir, fullPath)).replace(/\\/g, '/');
            const isExcluded = this.globalExclusions.some(pattern => {
                const cleanPattern = pattern.replace(/\/$/, "");
                return currentRelPath === cleanPattern || currentRelPath.startsWith(cleanPattern + "/");
            });
            if (isExcluded) continue;

            const stat = fs.statSync(fullPath);
            if (stat.isDirectory()) {
                if (!['.git', 'node_modules', 'Debug', 'Release', 'obj'].includes(file)) {
                    results = results.concat(this.scanForCFiles(fullPath, currentRelPath));
                }
            } else if (file.endsWith('.c')) results.push(fullPath);
        }
        return results;
    }

    private toArray(val: any): any[] { return val ? (Array.isArray(val) ? val : [val]) : []; }
    private findAllRecursive(node: any, key: string): any[] {
        let results: any[] = [];
        if (!node || typeof node !== 'object') return results;
        if (node[key]) results = results.concat(this.toArray(node[key]));
        for (const k in node) results = results.concat(this.findAllRecursive(node[k], key));
        return results;
    }
}