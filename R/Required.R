# RAM: Returns available RAM in GB.
RAM = function() {
    kb = as.numeric(gsub("\r","",gsub("FreePhysicalMemory=","",system('wmic OS get FreePhysicalMemory /Value',intern=TRUE)[3])))
    return(kb/1024/1024)
}

# Setup: Installs and loads required libraries.
Setup = function() {
    TensorFlowDoc = function() {
        doc = "
=============================== IMPORTANT ===============================
Requirements for TensorFlow and Keras:
1) Pyhon,
2) Miniconda,
3) CUDA and cuDNN for NVIDIA GPU.

ERROR: Could not find a version that satisfies the requirement tensorflow
  Fix: Check TensorFlow-PythoncuDNN-CUDA compatibility at https://www.tensorflow.org/install/source#gpu
  
ERROR: Can't connect to HTTPS URL because the SSL module is not available.
  Fix:
  1) Add minconda environment paths:
     <miniconda> 
     <miniconda>\\Scripts
     <miniconda>\\Library\\bin 
  2) Restart PC
  3) Call Setup() again

ERROR: No module named 'rpytools':
  Fix:
  1) Copy rpytools folder
     from <user>\\Documents\\R\\win-library\\<version>\\reticulate\\python
       to <user>\\AppData\\Local\\r-miniconda\\envs\\r-reticulate
  2) Restart R session 
  3) Call tensorflow::install_tensorflow(version = \"gpu\")
  3) Call keras::install_keras(version = \"gpu\")

========================================================================="
        return(doc)
    }
    GluonTSDoc = function() {
        doc = "
=============================== IMPORTANT ===============================
Requirements for GluonTS:
1) Pyhon,
2) Miniconda,
3) Microsoft Visual C++ 14.0,
4) CUDA and cuDNN for NVIDIA GPU.

NOTES:
Installing GluonTS library will enable the usage of DeepAR. However, the 
installation will also add r-gluonts enviroment to r-miniconda. This new
enviroment will then be used instead of r-reticulate, which will disable
tensorflow and keras. If you later wish to restore them, you may need to
delete '<user>\\Documents\\R' and '<user>\\AppData\\Local\\r-miniconda',
and then renstall R, Python, Miniconda and all the R libraries.

ERROR: Could not find a version that satisfies the requirement
  Fix: Install CUDA 9.2 and cuDNN 7.6.5.32
  
ERROR: Error in rlang::env_get(get_model_env(), cls)
  Fix: Repeat the call to SetupGluonTS.

ERROR: error in pkg.env$gluonts$distribution$student_t$StudentTOutput()
  Fix: Call modeltime.gluonts::install_gluonts()
========================================================================="
        return(doc)
    }
    SetupGluonTS = function(py.version, cuda.version) {
        Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
        devtools::install_github("business-science/modeltime.gluonts", force = TRUE)
        Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="false")
        
        install.packages("https://s3.ca-central-1.amazonaws.com/jeremiedb/share/mxnet/GPU/mxnet.zip", repos = NULL)
        
        mx = paste("mxnet-cu", gsub("[.]", "", cuda.version), sep="")
        reticulate::py_install(
            envname  = "r-gluonts",
            python_version = py.version,
            packages = c(mx, "gluonts", "pandas", "numpy", "pathlib"),
            method = "conda",
            pip = TRUE
        )
        
        modeltime.gluonts::install_gluonts()
    }
    LoadPackages = function() {
        packages = c("rlang",
                     "Rcpp",
                     "ps",
                     "processx",
                     "curl",
                     "jsonlite",
                     "backports",
                     "devtools",
                     "reticulate",
                     "tidymodels",
                     "tidyverse",
                     "timetk",
                     "ellipsis",
                     "tidyverse",
                     "modeltime.gluonts",
                     "tensorflow",
                     "keras",
                     "doParallel",
                     "RODBC",
                     "Hmisc",
                     "caret",
                     "lubridate",
                     "akmedoids",
                     "clusterSim",
                     "fastcluster",
                     "ClusterR",
                     "kohonen",
                     "RWeka",
                     "dtwclust",
                     "speccalt",
                     "Rssa",
                     "EMD",
                     "praznik",
                     "e1071",
                     "KScorrect",
                     "verification",
                     "predictionInterval",
                     "metaheuristicOpt")
        for (package in packages) {
            if (!require(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
                install.packages(package)

                if (package == "lubridate") {
                    devtools::install_github("tidyverse/lubridate")
                } else if (package == "tensorflow") {
                    write(TensorFlowDoc(), stdout())
                    reticulate::install_miniconda()
                    devtools::install_github("rstudio/tensorflow", force = TRUE)
                    tensorflow::install_tensorflow(version = "gpu")
                } else if (package == "keras") {
                    devtools::install_github("rstudio/keras", force = TRUE)
                    keras::install_keras(version = "gpu")
                } else if (package == "modeltime.gluonts") {
                    write(GluonTSDoc(), stdout())
                    sure = menu(c("Yes", "No"), title="Are you sure you want to install GluonTS and switch from r-reticulate to r-gluonts enviroment?")
                    if (sure == 1) {
                        py = readline("Please enter your Python version:")
                        cuda = readline("Please enter your CUDA version:")
                        SetupGluonTS(py, cuda)
                    }
                }

                library(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
            }
        }
    }

    # Suppressing warnings.
    options(warn = -1)

    # Changing default 256MB JVM memory limit before loading Java packages:
    options(java.parameters = paste("-Xmx", round(RAM() * 0.8), "g", sep = ""))

    # Avoding the need for user inputs:
    options(install.packages.compile.from.source = "always")

    # Loading required libraries without warnings:
    suppressWarnings(suppressMessages({ LoadPackages() }))

    # Disabling TensorFlow info messages:
    py_run_string("import os; os.environ['TF_CPP_MIN_LOG_LEVEL'] = '2';")
}

Setup()