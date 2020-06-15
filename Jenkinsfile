pipeline { 
    agent { 
        docker {
            image 'alpine:3'
            label 'exxmidebldprd05' 
        }
    }
    stages {
        stage('Build') { 
            steps { 
				sh 'ls -l'
            }
        }
    }
}
