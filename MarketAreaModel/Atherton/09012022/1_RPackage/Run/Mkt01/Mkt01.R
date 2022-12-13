
###############################################   Run.R #########################################
#' Author:       Wm. Bert Craytor
#' Location:     242 Clifton Rd., Pacifica, CA 94044, USA
#' Date:         07/02/2021
#' License:      MIT License
#' Description:  This is the main R executable that should run everything else.  You should make
#'
#'
#'              "projEnv" is the name you give the project environment.  This is variable that stores the central
#'              project information throughout the execution.
#'              "codeFolder" is the parent folder for all of this KVEsfr code (separte from your working project folder)
#'              "projectParentFolder" is the path to the parent folder for all of your projects
#'              "projectID", is the name of the sub-folder under the projectParentFolder that contains your project (property appraisal) fieles
#'                        - it is assumed that under this folder is another folder called "Earth" that contains all of your R/earth project
#'                          and data files.  It is further assumed that your Mapping file will be in the folder under this one called "Config",
#'                          that the data input and output files will be under the folder called "Mls"
#'                          That you should have created a folder called "DB" for the SQLite database, a folder called "Log" for log files
#'                          and a folder called Out for spurious output files. The log file can be useful for seeing how long the various steps
#'                          take, in particular the call to R/earth.
#'              "mlsDataFile", is the name of the MLS data file, without the .xlsx extension.
#'              "mlsSheet", is the name of the data sheet in the data file, where the MLS data is stored
#'              "subjectCQA", is the CQA value you want to assign to the  subject property.
#'
#'               So, below replace "CITY", "ROAD" and "NUMBER" with the appropriate data:
#'
#'               SetUp_0(projEnv,"C:/RPackages/KVEsfr/","C:/Users/XXXXXXX/Documents/YYYY/PVcnN2021/","CITY_ROAD_NUMBER","CITY_ROAD_NUMBER","MLSData", 1.1)
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
##############  DIRECTORY INFO ####################
library(Rcpp)

thisCity <- "Atherton"
countyPath <- "C:/Order_1/Market/SanMateoCounty/"
datePath <- "09012022"

projectParentFolder  <- paste(countyPath,thisCity,"/",datePath,"/2_Earth/",sep="")
codeFolder <- paste(countyPath,thisCity,"/",datePath,"/1_RPackage/",sep="")

###############  LIBRARIES ########################

library(Rcpp)
cppFilePath <- paste(codeFolder,"src/CqaMap.cpp",sep="")
sourceCpp(cppFilePath)
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
library(TeachingDemos)
library(gam)
library(gbm)
library(glmnet)
library(ParamHelpers)
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
library(parallel)
library(doParallel)
library(gridExtra)
#library(ipred)
#library(e1071)
#library(futile.logger)
library(tictoc)
library(stringr)
library(futile.logger)
library(recipes)
library(tidymodels)  #ggplot2,dplyr,tidr,readr,purrr,tibble;j hms,stringr,lubridate,forcats,DBI,haven,httr,
library(caret)
library(earth)

projEnv <- new.env()
rFolder <- paste(codeFolder,"R/",sep="")
# Source the relevant files
sourceDirectory(rFolder)
setwd(codeFolder)

flog.appender(appender.console(), name="FC")
flog.appender(appender.file("C:/Earth/Projects/Mkt01.log"),name="F1")

# Source the relevant files
sourceDirectory(rFolder)

source("R/Setup_0.R")
source("R/KVEsfr_Utilities.R")
source("R/Stage_1.R")

gctorture(FALSE)
set.seed(249334217)
  
SetUp_0(projEnv,
        codeFolder,
        projectParentFolder,
        "",
        "Appraisal_2",
        "MLSData",
        "SalePrice",
        60,
        "backward",
        60,
        5,
        TRUE,
        -1,
        4,
        1,
        120,
        70,
        0,
        0,
        1,
        5.0)

flog.info( "Stage I",name="F1")
Stage_1(projEnv)
flog.info("Stage I Finished",name="F1")


 