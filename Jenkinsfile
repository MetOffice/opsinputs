pipeline { 
    agent { 
        docker {
            image 'maven:3-alpine'
            label 'exxbuildprd1' 
        }
    }
    options {
        skipStagesAfterUnstable()
    }
    stages {
        stage('Build') { 
            steps { 
				bat 'cd ..'
				bat 'mkdir build'
				bat 'cd build'
                bat 'cmake ../cxvarobs -DCMAKE_BUILD_TYPE=Debug' 
				bat 'make'
            }
        }
        stage('Test'){
            steps {
				bat 'ctest'
            }
        }
    }
}
