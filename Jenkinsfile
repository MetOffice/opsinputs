pipeline { 
    agent { label 'exxbuildprd1' }
    options {
        skipStagesAfterUnstable()
    }
    stages {
        stage('Build') { 
            steps { 
				cmakeBuild buildType: 'Debug', cleanBuild: true, sourceDir: '.', buildDir: '../build', installation: '../install'
            }
        }
        stage('Test'){
            steps {
				ctest installation: '../install'
            }
        }
    }
}
