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
                    // Utilisation des identifiants stockes dans Jenkins pour Artifactory
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
                }
            }
        }

        stage('Tests E2E avec Ecran Virtuel') {
            steps {
                dir('client') {
                    echo "Demarrage de xvfb et lancement des tests..."
                    sh 'find .vscode-test -name "code" -exec chmod +x {} +'
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