pipeline {
    agent {label 'DBOUCHENNA_VM1_Linux'}

    environment {
        NODE_TLS_REJECT_UNAUTHORIZED = '0'
        VSCODE_SKIP_DOWNLOAD = 'true'
    }

    stages {
        stage('Recuperation des dependances') {
            steps {
                dir('client') {
                    withCredentials([
                        usernamePassword(credentialsId: "eddc7593-09ea-4939-96f8-6d455dfa4101", usernameVariable: 'ARTIFACTORYL1_EMEA_USERNAME', passwordVariable: 'ARTIFACTORYL1_EMEA_API_KEY'),
                        usernamePassword(credentialsId: "22c9bebb-a044-4183-bbe5-53c052ac9201", usernameVariable: 'ARTIFACTORYL2_EMEA_USERNAME', passwordVariable: 'ARTIFACTORYL2_EMEA_API_KEY'),
                    ]){
                        echo "Telechargement des artefacts depuis Artifactory..."
                        sh 'bash ./downloadartifacts.sh'
                    }
                }
            }
        }

        stage('Build et Reparation') {
            steps {
                dir('client') {
                    echo "Nettoyage et reparation des liens symboliques..."
                    sh 'rm -f node_modules/.bin/tsc'
                    sh 'ln -s ../typescript/bin/tsc node_modules/.bin/tsc || true'

                    echo "Attribution des droits d'execution..."
                    sh 'chmod +x node_modules/typescript/bin/tsc || true'
                    sh 'chmod +x node_modules/.bin/tsc || true'

                    echo "Lancement de la compilation TypeScript..."
                    sh 'npm run compile'
                    sh 'chmod +x run.sh'
                }
            }
        }

        stage('Build et Install Serveur (OCaml)') {
            steps {
                dir('server'){
                    echo "Compilation et installation du serveur LSP..."
                    sh 'eval $(opam env) && dune build'
                    sh 'eval $(opam env) && dune install'
                    sh 'eval $(opam env) && which frama-c-lsp || echo "ERREUR : Serveur non installe"'
                }
            }
        }

       stage('Tests Unitaires (sans VSCode)') {
    steps {
        dir('client') {
            echo "Installation de rewire..."
            sh 'npm install rewire --no-save --legacy-peer-deps 2>/dev/null || true'

            echo "Creation du stub vscode en memoire temporaire..."
            sh '''
                mkdir -p /tmp/stubs/vscode
                cat > /tmp/stubs/vscode/index.js << 'EOF'
module.exports = {
    window: {
        showWarningMessage: () => {},
        showInformationMessage: () => {},
        showErrorMessage: () => {},
        visibleTextEditors: [],
        createTextEditorDecorationType: () => ({ dispose: () => {} })
    },
    workspace: {
        workspaceFolders: [{ uri: { fsPath: '/workspace/test' } }],
        createFileSystemWatcher: () => ({ onDidChange: () => {}, dispose: () => {} })
    },
    commands: { executeCommand: () => Promise.resolve() },
    Uri: { file: (p) => ({ fsPath: p }), parse: (p) => ({ fsPath: p }) },
    EventEmitter: class { fire() {} },
    TreeItemCollapsibleState: { None: 0, Collapsed: 1, Expanded: 2 },
    TreeItem: class { constructor(l) { this.label = l; } },
    ThemeIcon: class { constructor(id) { this.id = id; } },
    ThemeColor: class { constructor(id) { this.id = id; } },
    RelativePattern: class { constructor(b, p) {} }
};
EOF
                echo '{"name":"vscode","main":"index.js"}' > /tmp/stubs/vscode/package.json
            '''

            echo "Lancement des tests unitaires Mocha..."
            sh 'NODE_PATH=/tmp/stubs node node_modules/mocha/bin/mocha.js --timeout 10000 --ui tdd --reporter spec "out/test/suite/unit/**/*.test.js"'
        }
    }
}

        stage('Tests E2E avec Ecran Virtuel') {
            steps {
                dir('client') {
                    echo "Demarrage de xvfb et lancement des tests..."
                    sh 'find .vscode-test -type f -exec chmod +x {} +'
                    sh 'eval $(opam env) && xvfb-run -a npm test -- --logLevel=off'
                }
            }
        }
    }

    post {
        success {
            echo "RESULTAT : Pipeline termine avec succes."
        }
        failure {
            echo "RESULTAT : Le pipeline a echoue. Verifiez les logs ci-dessus."
        }
    }
}