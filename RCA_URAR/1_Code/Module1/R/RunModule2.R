#'##############################################   Run.R #########################################
#' Author:       Wm. Bert Craytor
#' Location:     242 Clifton Rd., Pacifica, CA 94044, USA
#' Date:         07/02/2021
#' Description:  This is the main R executable that should run everything else.  You should make
#'               a copy of this for each project.  I use a name like ...Pacifica_CliftonRd_1234 for
#'               the end of the path to the project data files and project name.
#'
#'                SetUpProjectEnvironment( projEnv,
#'                                        codeFolder,
#'                                        projectParentFolder,
#'                                        projectID,
#'                                        alamodeDataFile,
#'                                        mlsSheet,
#'                                        subjectCQA)
#'
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
#'               SetUp_0(projEnv,"C:/RPackages/KVEsfr/","C:/Users/XXXXXXX/Documents/YYYY/PVN2021/","CITY_ROAD_NUMBER","CITY_ROAD_NUMBER","alamodeData", 1.1)
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
library(readxl)
library(readr)
library(openxlsx)
library(magrittr)
library(tidyverse)
library(lubridate)
library(tibble)
library(ggplotAssist)
library(writexl)
library(Formula)
library(plotrix)
library(rpart.plot)
library(TeachingDemos)
library(gam)
library(gbm)
library(glmnet)
library(mlr)
library(neuralnet)
library(glmnetUtils)
library(mgcv)
library(mda)
library(partykit)
library(MASS)
library(rpart)
library(stringi)
library(Hmisc)
library(dplyr)
library(R.utils)
library(pryr)
library(earth)
library(collections)
library(Rcpp)
library(utils)
library(R.oo)
library(inline)
library(DBI)
library(RSQLite)
library(devtools)
library(roxygen2)
library(here)
library(rgl)
library(scatterplot3d)
library(ggThemeAssist)
library(futile.logger)
library(rstudioapi)
library(rtf)
library(lattice)
library(data.table)

#################### KRIGING ###########
# library(sf) # processing spatial vector data - the easy way
# library(sp) # processing spatial vector data - the way gstat needs it
# library(raster) # processing spatial raster data. !!!overwrites dplyr::select!!!
#
# # Packages for geostatistics
# library(gstat)   # The most popular R-Package for Kriging (imho)
# library(automap) # Automatize some (or all) parts of the gstat-workflow
# library(geoR)
# library(scales)====
# # Finally, some packages to make pretty plots
# library(patchwork)
# library(viridis)
################### END OF KRIGING LIBRARY LOAD ################

projEnv <- new.env()
#traceback()
#browser()

codeFolder <- "C:/Order_1/RPackages/AlamodeInput/";
rFolder <- paste(codeFolder,"R/",sep="")
cppFilePath <- paste(codeFolder,"src/CqaMap.cpp",sep="")
setwd(codeFolder)

# Source the relevant files
flog.appender(appender.console(), name="FC")
flog.appender(appender.file("C:/Earth/Projects/Klog"),name="F1")



source("R/KVEsfr_Utilities.R")

source("R/Alamode_HOA_0.R")


gctorture(FALSE)

# SetUpProjectEnvironment <-
#   function(projEnv,
#            codeFolder,
#            projectParentFolder,
#            projectID,
#            alamodeDataFile,
#            compSheet

Alamode_0(projEnv,
          "C:/Order_1/RPackages/Alamode/",
          "C:/Order_1/B/A/PVN2022/",
          "Kve22_02_BoulderCreek",
          "MlsComps",
          "AlamodeInput"
          )

#
# codeFolder <-"C:/RPackages/Alamode/"
# projectParentFolder <- "C:/Users/MaxTask/Documents/B/A/PVN2021/"
# projectID <- Berkeley_HillcrestRd_120"
# alamodeDataFile <-"DalyCity_FairlawnAve_152"
# compSheet <- "MlsComps"
# almodeInput <- "AlamodeInput"
#

# End of forest plot test


