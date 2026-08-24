pipeline {
    agent { label 'DBOUCHENNA_VM1_Linux' }

    environment {
        NODE_TLS_REJECT_UNAUTHORIZED = '0'
        VSCODE_SKIP_DOWNLOAD = 'true'
    }

    stages {

        stage('Installation des dependances') {
            steps {
                dir('client') {
                    echo "Installation propre des dependances npm..."
                    sh 'rm -rf node_modules'
                    sh 'npm ci'

                    echo "Verification des paquets critiques..."
                    sh '''
                        for pkg in "@vscode/test-electron" "mocha" "typescript" "vscode-languageclient"; do
                            if [ -d "node_modules/$pkg" ]; then
                                echo "OK     : $pkg"
                            else
                                echo "MANQUE : $pkg"
                                missing=1
                            fi
                        done
                        [ -z "$missing" ] || exit 1
                    '''
                }
            }
        }

        stage('Build client TypeScript') {
            steps {
                dir('client') {
                    echo "Compilation du client TypeScript..."
                    sh 'rm -rf out/'
                    sh 'npm run compile'
                    sh 'chmod +x run.sh'
                }
            }
        }

        stage('Build serveur OCaml') {
            steps {
                dir('server') {
                    echo "Compilation et installation du serveur LSP..."
                    sh '''
                        eval $(opam env)
                        dune build
                        dune install
                        which frama-c && frama-c -version
                    '''
                }
            }
        }

        stage('Tests E2E') {
            steps {
                dir('client') {
                    echo "Lancement des tests E2E avec xvfb..."
                    sh 'find .vscode-test -type f -exec chmod +x {} + 2>/dev/null || true'
                    sh '''
                        eval $(opam env)
                        xvfb-run -a npm test -- --logLevel=off
                    '''
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