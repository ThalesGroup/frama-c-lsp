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

        stage('Build et Reparation') {
            steps {
                dir('client') {
                    echo "Nettoyage des anciens fichiers compiles..."
                    sh 'rm -rf out/'

                    echo "Reparation propre des binaires node_modules..."
                    // 1. Forcer le package.json à utiliser la compilation directe via node si besoin, ou réparer le lien tsc
                    sh 'rm -f node_modules/.bin/tsc'
                    sh 'ln -s ../typescript/bin/tsc node_modules/.bin/tsc || true'
                    sh 'chmod +x node_modules/typescript/bin/tsc || true'

                    // 2. Réparer le binaire vsce qui pose souvent problème avec les archives Artifactory
                    sh 'rm -f node_modules/.bin/vsce'
                    sh 'if [ -f node_modules/@vscode/vsce/vsce ]; then ln -s ../@vscode/vsce/vsce node_modules/.bin/vsce; fi || true'
                    sh 'chmod +x node_modules/.bin/vsce || true'

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