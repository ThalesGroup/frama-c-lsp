import * as path from 'path';
import * as cp from 'child_process';
import * as net from 'net';
import { workspace, ExtensionContext } from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    Definition,
    LocationLink,
    Middleware
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;
let serverProcess: cp.ChildProcess | undefined;
let connectionPromise: Promise<net.Socket> | undefined;

export function activate(context: ExtensionContext) {
    let serverScript = context.asAbsolutePath(path.join('..', 'server', 'run.sh'));

    console.log(`Server script path: ${serverScript}`);

    serverProcess = cp.spawn(serverScript, []);
    serverProcess.on('error', (err?) => {
        console.error(`Failed to start server process: ${err}`);
    });

    serverProcess.stderr.on('data', (data?) => {
        console.error(`Server stderr: ${data.toString()}`);
    });

    serverProcess.stdout.on('data', (data?) => {
        console.log(`[acsl-lsp]: ${data.toString()}`);
        if (data.toString().includes('Server listening on port 8001')) {
            console.log('Server started successfully. Initializing client...');
            initializeClient(); 
        }
    });

    function initializeClient() {
        connectionPromise = new Promise<net.Socket>((resolve, reject) => {
            const socket = net.connect(8001, '127.0.0.1', () => {
                console.log('Socket connected');
                resolve(socket);
            });

            socket.on('data', (data?) => {
                console.log(`Received data: ${data.toString()}`);
            });

            socket.on('close', (hadError?) => {
                console.log(`Socket closed, hadError: ${hadError}`);
            });

            socket.on('error', (err?) => {
                console.error(`Socket error: ${err}`);
                reject(err);
            });
        });

        let serverOptions: ServerOptions = () => connectionPromise!.then((socket) => ({
            reader: socket,
            writer: socket
        }));

        const middleware: Middleware = {
            provideDefinition: (document, position, token, next) => {
                console.log(`provideDefinition called: ${document.uri.toString()} at ${position.line}:${position.character}`);
                const result = next(document, position, token);
                if (result) {
                    if (result instanceof Promise) {
                        return result.then((resolvedResult) => {
                            console.log(`Definition result: ${JSON.stringify(resolvedResult)}`);
                            return resolvedResult;
                        }).catch((error) => {
                            console.error(`Error in provideDefinition: ${error}`);
                            throw error; // Rethrow error for further debugging
                        });
                    } else {
                        console.log(`Definition result: ${JSON.stringify(result)}`);
                        return result;
                    }
                } else {
                    console.log('No definition result');
                    return null;
                }
            }
        };

        let clientOptions: LanguageClientOptions = {
            documentSelector: [
                { scheme: 'file', language: 'c' },
                { scheme: 'file', language: 'acsl' }
            ],
            synchronize: {
                fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
            },
            middleware
        };

        client = new LanguageClient('ACSL Language Server VS Code Extension', serverOptions, clientOptions);

        client.start().then(() => {
            context.subscriptions.push(client);
            console.log('Client started successfully');
        }).catch(error => {
            console.error(`Failed to start the language client: ${error}`);
        });
    }
}

export function deactivate(): Thenable<void> | undefined {
    if (client) {
        return client.stop().then(() => {
            if (serverProcess) {
                console.log("Killing server process");
                serverProcess.kill('SIGTERM');
                serverProcess = undefined;
            }
        });
    } else if (serverProcess) {
        console.log("Killing server process");
        serverProcess.kill('SIGTERM');
        serverProcess = undefined;
    }
    return undefined;
}
