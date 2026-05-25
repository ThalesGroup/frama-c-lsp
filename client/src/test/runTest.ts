import * as path from 'path';
import { runTests } from '@vscode/test-electron';

async function main() {
    try {
        const extensionDevelopmentPath = path.resolve(__dirname, '../../');
        const extensionTestsPath = path.resolve(__dirname, './suite/index');

        const workspacePath = '/home/user/git/L2/code';

        await runTests({
            extensionDevelopmentPath,
            extensionTestsPath,
            
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

