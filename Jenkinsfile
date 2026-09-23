pipeline {
    agent {label 'DBOUCHENNA_VM2_Linux'}

    environment {
        NODE_TLS_REJECT_UNAUTHORIZED = '0'
        VSCODE_SKIP_DOWNLOAD = 'true'
        PATH = "/opt/ant/bin:/home/user/.nvm/versions/node/v22.14.0/bin:${env.PATH}"
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
stage('Diagnostic zip') {
    steps {
        dir('client') {
            sh 'echo "=== .bin (tsc/vsce) ==="; ls -la node_modules/.bin/ | grep -E "tsc|vsce" || echo "absents de .bin"'
            sh 'echo "=== typescript/bin ==="; ls -la node_modules/typescript/bin/ || echo "PAS de node_modules/typescript/bin"'
            sh 'echo "=== @vscode/vsce ==="; ls -la node_modules/@vscode/vsce/ | head || echo "PAS de @vscode/vsce"'
            sh 'echo "=== racine ==="; ls -la'
        }
    }
}
     stage('Build et Reparation') {
    steps {
        dir('client') {
            echo "Nettoyage des anciens fichiers compiles..."
            sh 'rm -rf out/'

            echo "Lancement de la compilation TypeScript..."
            sh 'npm run compile'
            
            echo "Attribution des droits d'execution sur run.sh..."
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
            sh 'eval $(opam env) && which frama-c && frama-c -version || echo "ERREUR : Frama-C non installe"'
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