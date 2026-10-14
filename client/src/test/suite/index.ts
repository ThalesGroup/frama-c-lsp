import * as path from 'path';
import * as Mocha from 'mocha';
import { glob } from 'glob';

export async function run(): Promise<void> {
    const mocha = new Mocha({
        ui: 'tdd',
        color: true,
        timeout: 90000,
    });

    const testsRoot = path.resolve(__dirname, '..');

    const files = await glob('**/**.test.js', { cwd: testsRoot });

    // tri explicite pour garantir l'ordre d'execution
    files.sort();
    files.forEach(f => mocha.addFile(path.resolve(testsRoot, f)));

    // delai entre chaque test pour laisser les processus forkes se terminer
    mocha.suite.afterEach(function(done) {
        setTimeout(done, 6000);
    });

    try {
        return new Promise((c, e) => {
            mocha.run(failures => {
                if (failures > 0) {
                    e(new Error(`${failures} tests failed.`));
                } else {
                    c();
                }
            });
        });
    } catch (err) {
        console.error(err);
        throw err;
    }
}