pipeline {
    agent {label 'DBOUCHENNA_VM1_Linux'}

    environment {
        NODE_TLS_REJECT_UNAUTHORIZED = '0'
        VSCODE_SKIP_DOWNLOAD = 'true'
    }

    stages {
        stage('Installation des dependances') {
    steps {
        dir('client') {
            echo "Suppression de l'ancien node_modules pour repartir de zero..."
            sh 'rm -rf node_modules package-lock.json'

            echo "Installation des dependances via npm install..."
            sh 'npm install'
            sh 'ls node_modules/@vscode/ || echo "pas de dossier @vscode"'
sh 'ls node_modules/ | grep -i test || echo "aucun paquet test"'
sh 'cat package.json | grep -i test'

            echo "Verification que les paquets critiques sont bien la..."
            sh 'test -d node_modules/@vscode/test-electron'
            sh 'test -d node_modules/mocha'
            sh 'test -d node_modules/typescript'
            sh 'test -d node_modules/vscode-languageclient'
            sh 'test -x node_modules/.bin/tsc'
            sh 'test -x node_modules/.bin/mocha'

            echo "Dependances installees avec succes."
        }
    }
}

        stage('Build et Reparation') {
    steps {
        dir('client') {
            echo "Nettoyage des anciens fichiers compiles..."
            sh 'rm -rf out/'

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