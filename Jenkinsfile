pipeline {
    agent { label 'linux' } // 

    stages {
        stage('Install') {
            steps {
                sh 'npm install'
                sh 'opam install . --deps-only' 
            }
        }

        stage('Compile') {
            steps {
                sh 'npm run compile' 
                sh 'dune build' 
            }
        }

        stage('Run Extension Benchmark') {
            steps {
                
                sh 'xvfb-run npm test' 
            }
        }
    }

    post {
        always {
            junit '**/test-results.xml' 
        }
    }
}