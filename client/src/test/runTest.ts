import * as path from 'path';
import * as cp from 'child_process';
import * as fs from 'fs';
import {
    runTests,
    downloadAndUnzipVSCode,
    resolveCliArgsFromVSCodeExecutablePath
} from '@vscode/test-electron';

async function main() {
    try {
        const clientRoot = path.resolve(__dirname, '../../');
        const extensionTestsPath = path.resolve(__dirname, './suite/index');
        const workspacePath = path.resolve(clientRoot, 'benchmarks');

        const pkg = JSON.parse(fs.readFileSync(path.join(clientRoot, 'package.json'), 'utf8'));
        const vsixName = `${pkg.name}-${pkg.version}.vsix`;
        const vsixPath = path.join(clientRoot, vsixName);

        
        const emptyExtDir = path.join(clientRoot, '.empty-ext-dev');
        if (!fs.existsSync(emptyExtDir)) fs.mkdirSync(emptyExtDir, { recursive: true });

        console.log('== Compilation ==');
        cp.execSync('npm run compile', { cwd: clientRoot, stdio: 'inherit' });

        console.log('== Packaging du VSIX ==');
        cp.execSync(`npx @vscode/vsce package -o ${vsixName}`, { cwd: clientRoot, stdio: 'inherit' });
        if (!fs.existsSync(vsixPath)) {
            throw new Error(`VSIX non généré : ${vsixPath}`);
        }

        console.log('== VS Code de test ==');
        const vscodeExecutablePath = await downloadAndUnzipVSCode('1.115.0');
        const [cliPath, ...cliArgs] =
            resolveCliArgsFromVSCodeExecutablePath(vscodeExecutablePath);

        console.log('== Installation du VSIX ==');
        const install = cp.spawnSync(
            cliPath,
            [...cliArgs, '--install-extension', vsixPath],
            { encoding: 'utf-8', stdio: 'inherit' }
        );
        if (install.status !== 0) {
            throw new Error('Échec de l\'installation du VSIX');
        }

      
        console.log('== Lancement des tests ==');
        await runTests({
            vscodeExecutablePath,
            extensionDevelopmentPath: emptyExtDir,
            extensionTestsPath,
            extensionTestsEnv: {
                ...process.env,
                VSCODE_SKIP_DOWNLOAD: 'true'
            },
            launchArgs: [
                workspacePath,
                '--disable-workspace-trust'
            ]
        });
    } catch (err) {
        console.error('Failed to run tests', err);
        process.exit(1);
    }
}

main();