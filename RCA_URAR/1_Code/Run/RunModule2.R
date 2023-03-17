#'##############################################   Run.R #########################################
#' Author:       Wm. Bert Craytor
#' Location:     242 Clifton Rd., Pacifica, CA 94044, USA
#' Date:         07/02/2021
#' Name:         RunModule2.R
#' Submodule:    Alamode_URAR.R, Alamode_2025.R, Alamode_HOA.r, --- or any other submodules targeted towards specific forms
#' Description:  This is the main R executable that should run Alamode Input scripts
#'            
#'              "projEnv" is the name you give the project environment.  This is variable that stores the central
#'               project information throughout the execution.
#'              "codeFolder" is the parent folder for all of this KVEsfr code (separte from your working project folder)
#'              "projectParentFolder" is the path to the parent folder for all of your projects
#'              "projectID", is the name of the sub-folder under the projectParentFolder that contains your project (property appraisal) fieles
#'                        - it is assumed that under this folder is another folder called "Earth" that contains all of your R/earth project
#'                          and data files.  It is further assumed that your Mapping file will be in the folder under this one called "Config",
#'                          that the data input and output files will be under the folder called "Mls"
#'                          That you should have created a folder called "DB" for the SQLite database, a folder called "Log" for log files
#'                          and a folder called Out for spurious output files. The log file can be useful for seeing how long the various steps
#'                          take, in particular the call to R/earth.
#'              "alamodeDataFile", is the name of the MLS data file, without the .xlsx extension.
#'              "mlsSheet", is the name of the data sheet in the data file, where the MLS data is stored
#'              "subjectCQA", is the CQA value you want to assign to the  subject property.
#'
#'               So, below replace "CITY", "ROAD" and "NUMBER" with the appropriate data:
#'
#'        
#'
#' Notes:        1.  This program is free software; you can redistribute it and/or modify
#'                   it under the terms of the GNU General Public License as published by
#'                   the Free Software Foundation; either version 3 of the License, or
#'                   (at your option) any later version.
#'
#'               2.  This program is distributed in the hope that it will be useful,
#'                   but WITHOUT ANY WARRANTY; without even the implied warranty of
#'                   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#'                   GNU General Public License for more details.
#'
#'               3.   A copy of the GNU General Public License is available at
#'                   http://www.r-project.org/Licenses
#'
#'####################################################################################################
# 
#  Purpose:  Transfer the previous RunModule1.R output to a worksheet template for upload into Alamode
#
#            You need to:
#
#            1.  Make a copy of the MlsData.xlsx file in your version output folder to "MlsComps.xlsx".  Then rename "Sheet1" to "X". 
#
#            2.  Then rename "Sheet1" to "X".  - As you may wind-up making a lot of other worksheets for other calcalations.  The "X" sheet is to create the 
#                "AlamodeInput" worksheet for input to the Alamode URAR form, using Alamode's worksheet feature.
#
#            3.  The first part of the RunModul2.R script is almost the same as the first part of the RunModule1.R script.   
#                Basically it sets up the global projEnv variable values, loads the libraries and source files and then calls the common  
#                Setup_0() function in Setup_0.R to setup the common variables.
#
#            4.  Then it reads in MlsComps.xlsx and transfers the data to the AlamodeInput.xlsx file to be uploaded into the Alamode Worksheet.
#
#            5.  Note, I usually have separate scripts for Alamode Input, "Alamode_URAR.R" for URAR forms, "Alamode_2025.R" for 2025 forms and "Alamdoe_HOA"
#                for HOA forms.  You will need to create your own 2025, HOA and other forms (as an exercise).
#
########################################################################################################

  
##############  R/C++ Package ####################
library(Rcpp)



###############  CONSTANTS ########################

effDate <- "02/26/2022"
projEnv <- new.env()
projEnv$MlsFile <- "Appraisal_2.xlsx"
##projEnv$MlsFile <- "Terrel2.xlsx"

#projEnv$ResidualMethod <- "Residual"
projEnv$ResidualMethod <- "ResidualPerSF"

projEnv$RootPath <- "C:/Order_1/B/A/PVN2023/"
projEnv$Project  <- "KVE23_02_HMB_PhantomRd_999_All/"
projEnv$ProjectFolder  <- paste(projEnv$RootPath,projEnv$Project,sep="")
projEnv$EarthFolder <- paste(projEnv$ProjectFolder,"Earth/",sep="")
projEnv$CodeFolder <- paste(projEnv$EarthFolder,         "1_Code/",sep="")
projEnv$DataFolder <- paste(projEnv$EarthFolder,         "2_Data/",sep="")
projEnv$TemplatesFolder <-   paste(projEnv$EarthFolder,     "3_Templates/",sep="")


projEnv$Module <- "Module1/"
projEnv$ModuleFolder <- paste(projEnv$CodeFolder,projEnv$Module,sep="")

projEnv$RModuleFolder <- paste(projEnv$ModuleFolder,"R/",sep="")

projEnv$RunFolder <- paste(projEnv$CodeFolder,"Run/",sep="")
projEnv$RunModuleFolder <- paste(projEnv$RunFolder,projEnv$Module,sep="")
projEnv$DbFolder <- paste(projEnv$DataFolder,        "DB/", sep = '')
projEnv$MlsFolder <- paste(projEnv$DataFolder,       "Mls/",sep="")
projEnv$MlsDataFile <- paste(projEnv$MlsFolder,projEnv$MlsFile,sep="")

## For Alamode Input Processing
projEnv$MlsCompsFileName <- "MlsComps.xlsx"
projEnv$AlamodeInputFileName <- "AlamodeInput.xlsx"

projEnv$MlsCompsFile <- paste(projEnv$TemplatesFolder,projEnv$MlsCompsFileName , sep="")
projEnv$AlamodeInputFile <- paste(projEnv$TemplatesFolder,projEnv$AlamodeInputFileName, sep="")
## 


projEnv$LogFolder <- paste(projEnv$CodeFolder,"Log/",sep="")
library(Rcpp)
projEnv$CppModuleFolder <- paste(projEnv$ModuleFolder,"CPP/",sep="")
cppFile <- paste(projEnv$CppModuleFolder,"CqaMap.cpp",sep="")
sourceCpp(cppFile)

	
################# LIBRARIES #################
library(stats)
library(foreach)
library(Matrix)
library(openxlsx)
library(ggplotAssist)
library(writexl)
library(splines)
library(Formula)
library(plotrix)
library(rpart)
library(rpart.plot)
library(TeachingDemos)
library(gam)
library(gbm)
library(glmnet)
library(ParamHelpers)
library(mlr)
library(mlr3)
library(neuralnet)
library(glmnetUtils)
library(mgcv)
library(mda)
library(partykit)
library(MASS)
library(stringi)
library(base64enc)
library(backports)
library(Hmisc)
library(R.utils)
library(collections)
library(utils)
library(R.oo)
library(inline)
library(RSQLite)
library(devtools)
library(roxygen2)
library(here)
library(rgl)
library(scatterplot3d)
library(ggThemeAssist)
library(futile.logger)
library(rstudioapi)
library(DescTools)
library(simEd)
library(methods)
#library(parallel)
#library(doParallel)
library(gridExtra)
library(tictoc)

library(futile.logger)
library(recipes)

library(timetk)
library(lubridate)
library(furrr)
library(tictoc)
library(blob)
library(hms)
library(DBI)
library(readxl)
library(googlesheets4)
library(googledrive)
library(jsonlite)
library(xml2)
library(rtf)

# CORE TIDYVERSE
library(tidyverse)  #ggplot2,dplyr,tidr,readr,purrr,tibble;j hms,stringr,lubridate,forcats,DBI,haven,httr,
library(ggplot2)    # A system for declaratively creating graphics
library(dplyr)      # A grammar of data manipulation, providing a consistent set of verbs that help you solve the most common data manipulation challenges
                    # mutate()    adds new variables that are functions of existing variables
					# select()    picks variables based on their names.
					# filter()    picks cases based on their values.
					# summarize() reduces multiple values down to a single summary.
					# arrange()   changes the ordering of the rows.
library(tidyr)      # Create tidy data, 1. Every column is variable, row is observation, cell single value
                    # 5 categories of funcs: Pivoting, Rectangling, Nesting, Splitting/combining, treat missing values ...
library(readr)      # A fast and friendly way to read rectangular data from delimited files
                    # read_csv(), read_tsv(), read_csv2, read_delim(), read_fwf(),read_table(),read_log()
library(purrr)      # A complete and consistent set of tools for working with functions and vectors
                    # nest(),  mutate(), map()
library(tibble)     # Tibbles are data.frames that are lazy and surly: they do less (i.e. they don’t change variable names or types, and don’t do partial matching) and complain more (e.g. when a variable does not exist). This forces you to confront problems earlier, typically leading to cleaner, more expressive code
library(stringr)    # A cohesive set of functions designed to make working with strings as easy as possible
library(forcats)    # A suite of tools that solve common problems with factors, including changing the order of levels or the values
library(timetk)     # Time-series
library(lubridate)  # For dates and date-times
library(hms)        # For time-of-day values
library(magrittr)   # the pipe, %>% used throughout the tidyverse. It also provide a number of more specialised piping operators (like %$% and %<>%) that can be useful in other places.
library(furrr)
library(tictoc)     # Extended timing functions tic/toc, as well as stack and list structures. See http://jabiru.github.io/tictoc/ for more detail.
library(blob)       #  For binary data

library(DBI)            # For relational databases
library(readxl)         # .xls and .xlsx sheets

library(googlesheets4)  # Google Sheets via Sheets API v4
library(googledrive)    # GoogleDrive
library(jsonlite)
library(xml2)


# CORE TIDYMODELS
library(tidymodels)  # A meta-package that installs and load the core packages listed below that you need for modeling and machine learning
library(rsample)     # Infrastructure for efficient data splitting and resampling
library(parsnip)     # Can be used to try a range of models without getting bogged down in the syntactical minutiae of the underlying packages
library(recipes)     # A tidy interface to data pre-processing tools for feature engineering
library(workflows)   # Bundle your pre-processing, modeling, and post-processing together
library(tune)        # Helps you optimize the hyperparameters of your model and pre-processing steps
library(yardstick)   # Measures the effectiveness of models using performance metrics
library(broom)       # Converts the information in common statistical R objects into user-friendly, predictable formats
library(dials)       # Creates and manages tuning parameters and parameter grids

# TIDYMODEL SPECIALIZED PACKAGES
## Perform statistical analysis
library(infer)  # A high-level API for tidyverse-friendly statistical inference.
library(corrr)   # Gas tidy interfaces for working with correlation matrices
## Create robust models
library(spatialsample)  # Provides resampling functions and classes like rsample, but specialized for spatial data.
library(discrim)  # Contains definitions for discriminant analysis models
library(rules)    # Does the same for rule-based classification and regression models.
library(baguette) # Creates ensemble models via bagging
library(multilevelmod) # Provides support for multilevel models (also known as mixed models or hierarchical models).
library(embed)         # Contains steps to create embeddings or projections of predictors
library(textrecipes)   # Has extra steps for text processing
library(themis)        # Can help alleviate class imbalance using sampling methods
library(tidypredict)   # Can convert prediction equations to different languages (e.g. SQL) and fit some models in-database
library(modeldb)       #
## Tune, compare and work with your models
library(workflowsets)   # Lets you create sets of workflow objects for tuning and resampling.
library(stacks)   		# To integrate predictions from many models,provides tools for stacked ensemble modeling
library(finetune)   	# Extends the tune package with more approaches such as racing and simulated annealing.
library(usemodels)   	# Creates templates and automatically generates code to fit and tune models.
library(probably)   	# Tools for post-processing class probability estimates.
library(tidyposterior) 	# Make formal statistical comparisons between models using resampling and Bayesian methods.
library(butcher)   		# Reduce the size of large objects by removing the sub-components.
library(applicable)   	# Whether the data that you are predicting are extrapolations from the training set
library(shinymodels)   	# Explore tuning or resampling results via a Shiny app.

#Develop custom modeling tools

# CARET
library(caret)
# Earth
library(plotmo)
library(earth)
	
# Setup log console
flog.appender(appender.console(), name="Log2Console")
# Setup file console "Log" folder under "Earth"
flog.appender(appender.file(paste(projEnv$LogFolder,  "Log1.log", sep="")) ,name="Log2File")
# Source the relevant files
sourceDirectory(projEnv$RModuleFolder)
setwd(projEnv$EarthFolder)
source("1_Code/Module1/R/Setup_0.R")
source("1_Code/Module1/R/KVEsfr_Utilities.R")
source("1_Code/Module1/R/A.R")


# Set up Version Folder System
projEnv$MlsVersionFolder  <- "C:/Order_1/B/A/PVN2023/KVE23_02_HMB_PhantomRd_999_All/Earth/2_Data/Mls/VER20230304_152209_Final"

proceed <- TRUE

if (!file.exists(projEnv$MlsVersionFolder)){
       flog.info( "Specified Version Folder Does Not Exist." )  
       proceed <- FALSE
}

projEnv$MlsCompsFile <- paste(projEnv$MlsVersionFolder,"/MlsComps.xlsx", sep="")
projEnv$AlamodeInputFile <- paste(projEnv$MlsVersionFolder,"/AlamodeInput.xlsx",sep="")

 if (!file.exists(projEnv$MlsCompsFile)){
       flog.info( "Specified MlsComps.xlsx File Does Not Exist." )  
	    flog.info( "Copy MlsData.xlsx to MlsComps.xlsx and rename Sheet1 to 'X'." )  
		 flog.info( "Then modify Sheet X as necessary for input to Alamode Worksheet" )  
       proceed <- FALSE
}
if (!file.exists(projEnv$MlsVersionFolder)){
       flog.info( "Specified AlamodeInput.xlsx Worksheet Does Not Exist." )  
       proceed <- FALSE
}

if(!proceed) {
    stop("Errors prevent proceeding.")
}

gctorture(FALSE)
set.seed(9921)
set.seed(1823450)
flog.info( "Libraries loaded, code sourced",name = "Log2Console")
flog.info( "Libraries loaded, code sourced",name = "Log2File")

#SetUp_0 <-
# function(
#           projectID,
#           mlsData,
#           mlsSheet,
#           targetVariable,
#           bootstrap,
#           pmethod,
#           ncross,
#           nfold,
#           keepxy,
#           filterInteractions,
#           penalty_1D,
#           penalty_2D,
#           trace,
#           maxterms,
#           nprune,
#           minspan,
#           endspan,
#           degree,
#           subjectCQA
#           subjectCQA2)

#  For this particular R script, most of the parameters below are NOT used, but for convenience are the same as for RunModule1.R,
#  so that you can use them for any modifications you might want to make to the routines.
SetUp_0(
        "",
        "Appraisal_2",
        "MLSData",
        "SalePrice",
        60,
        "backward",
        80,
        5,
        TRUE,
        TRUE,
        1,
        5,
        4,
        120,
        90,
        30,
        0,
        1,
        5.25,
	    5.5)
# SetUpProjectEnvironment <-
#   function(projEnv,
#            codeFolder,
#            projectParentFolder,
#            projectID,
#            alamodeDataFile,
#            compSheet

Alamode_URAR(   )

#
# codeFolder <-"C:/RPackages/Alamode/"
# projectParentFolder <- "C:/Users/MaxTask/Documents/B/A/PVN2021/"
# compSheet <- "MlsComps"
# almodeInput <- "AlamodeInput"
#

# End of forest plot test


