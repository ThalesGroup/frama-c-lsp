import * as path from 'path';
import * as cp from 'child_process';
import * as net from 'net';
import { workspace, ExtensionContext } from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    Middleware,
    URI
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;
let serverProcess: cp.ChildProcess | undefined;
let connectionPromise: Promise<net.Socket> | undefined;

export function activate(context: ExtensionContext) {
    const serverScript = context.asAbsolutePath(path.join('..', 'server', 'run.sh'));

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
                console.log(`Received from server: ${data.toString()}`);
            });

            socket.on('close', (hadError?) => {
                console.log(`Socket closed, hadError: ${hadError}`);
            });

            socket.on('error', (err?) => {
                console.error(`Socket error: ${err}`);
                reject(err);
            });
        });

        connectionPromise.then(() => {
            console.log('Socket connection promise resolved');
        }).catch((err) => {
            console.error(`Socket connection promise rejected: ${err}`);
        });

        const serverOptions: ServerOptions = () => connectionPromise!.then((socket) => ({
            reader: socket,
            writer: socket
        }));

        const middleware: Middleware = {
            sendRequest: async (type, params, token, next) => {
                console.log('Sending request:', type, params);
                return next(type, params, token);
            },
            sendNotification: (type, next, params) => {
                console.log('Sending notification:', type, params);
                return next(type, params);
            }
        };

        const clientOptions: LanguageClientOptions = {
            documentSelector: [
                { scheme: 'file', language: 'c' },
                { scheme: 'file', language: 'acsl' }
            ],
            synchronize: {
                fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
            },
            middleware
        };

        client = new LanguageClient('ACSLClient','ACSL Language Server VS Code Extension', serverOptions, clientOptions);

        client.start().then(() => {
            context.subscriptions.push(client);
            console.log('Client started successfully and is ready');
        }).catch(error => {
            console.error(`Failed to start the language client: ${error}`);
        });
    }
}

export function deactivate(): Thenable<void> | undefined {
    return undefined;
}
