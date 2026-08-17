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

        stage('Tests E2E avec Ecran Virtuel') {
            steps {
                dir('client') {
                    echo "Recherche du VSCode deja installe..."
                    sh '''
                        VSCODE_PATH=$(find .vscode-test -name "code" -type f 2>/dev/null | head -1)
                        if [ -z "$VSCODE_PATH" ]; then
                            echo "ERREUR : VSCode introuvable dans .vscode-test/"
                            echo "Veuillez lancer le pipeline une premiere fois avec internet pour telecharger VSCode"
                            exit 1
                        fi
                        echo "VSCode trouve : $VSCODE_PATH"
                    '''

                    echo "Attribution des droits d'execution..."
                    sh 'find .vscode-test -type f -exec chmod +x {} +'

                    echo "Demarrage de xvfb et lancement des tests..."
                    sh '''
                        VSCODE_EXECUTABLE_PATH=$(find .vscode-test -name "code" -type f | head -1)
                        export VSCODE_EXECUTABLE_PATH
                        eval $(opam env) && xvfb-run -a npm test -- --logLevel=off
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