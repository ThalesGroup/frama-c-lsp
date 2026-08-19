import * as path from 'path';
import { runTests } from '@vscode/test-electron';

async function main() {
    try {
        const extensionDevelopmentPath = path.resolve(__dirname, '../../');
        const extensionTestsPath = path.resolve(__dirname, './suite/index');

        // Workspace = le dossier benchmark, avec son .vscode/settings.json
        const workspacePath = path.resolve(__dirname, '../../../../benchmarks/01_display_ast');

        await runTests({
            extensionDevelopmentPath,
            extensionTestsPath,
            version: '1.115.0',
            extensionTestsEnv: {
                ...process.env,
                "VSCODE_SKIP_DOWNLOAD": "true"
            },
            launchArgs: [
                workspacePath,
                '--disable-extensions'
            ]
        });
    } catch (err) {
        console.error('Failed to run tests');
        process.exit(1);
    }
}

main();