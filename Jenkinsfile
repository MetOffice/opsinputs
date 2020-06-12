pipeline { 
    agent any 
    options {
        skipStagesAfterUnstable()
    }
    stages {
        stage('Build') { 
            steps { 
				cmakeBuild buildType: 'Debug', cleanBuild: true
            }
        }
        stage('Test'){
            steps {
				ctest
            }
        }
    }
}
