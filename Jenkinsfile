pipeline { 
    agent { 
        docker {
            image 'alpine:3'
            label 'exxbuildprd1' 
        }
    }
    options {
        skipStagesAfterUnstable()
    }
    stages {
        stage('Build') { 
            steps { 
				sh 'cd ..'
				sh 'mkdir build'
				sh 'cd build'
                sh 'cmake ../cxvarobs -DCMAKE_BUILD_TYPE=Debug' 
				sh 'make'
            }
        }
        stage('Test'){
            steps {
				sh 'ctest'
            }
        }
    }
}
