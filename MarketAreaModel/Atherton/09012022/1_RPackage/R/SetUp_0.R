################################################   Setup #########################################
# Author:       Wm. Bert Craytor
# Location:     242 Clifton Rd., Pacifica, CA 94044, USA
# Date:         07/02/2021
# License:      MIT License
# Description:  Setup script to generate MARS (earth)  analysis for appraisal data using R:earth
 
# library(readxl)
# library(readr)
# library(openxlsx)
# library(magrittr)
# library(dplyr)
# library(pryr)
# library(tidyverse)
# library(lubridate)
# library(tibble)
# library(ggplotAssist)
# library("writexl")
# library("Formula")
# library("partykit")
# library("MASS")
# library(collections)
# library(Rcpp)
# library(DBI)
# library(RSQLite)
# library(here)

#'  Loads the initial MLS Excel Data File to Data Frames
#'  @projEnv Project Environment
#'  @return  Project Environment
#'
#' #export
LoadMlsExcelToDataFrame <- function(projEnv) {
  print("LoadMlsExcelToDataFrame")
  print(paste("MLSData File: ",projEnv$MlsFile," ",projEnv$MlsSheet))
  # Load MLS File into MLS Data Frame
  projEnv$MlsDataDF <-
    read.xlsx(projEnv$MlsFile,
              sheet = projEnv$MlsSheet ,
              detectDates = TRUE)
  assign("MLS", projEnv$MlsDataDF)
  projEnv
}

#'  Load the Mappings (or configuation file) to a Data Frame
#'  @projEnv Project Environment
#'  @return  Project Environment
LoadMappingsExcelToDataFrame <- function(projEnv) {
  print("LoadMappingsExcelToDataFrame")

  # Load MLS File into MLS Data Frame
  print("Load Project")
 
  projEnv$ProjectDF <-
    read.xlsx(projEnv$MlsFile,
              sheet = "Project" ,
              detectDates = TRUE)
  # Assign the variables in the "Project" sheet to projEnv variables of same name, with the associated values in that
  # spreadsheet
  nr <- nrow(projEnv$ProjectDF)
  for (i in 1:nr) {
    var1 <-  projEnv$ProjectDF[i, 1]
    assign(var1, as.character(projEnv$ProjectDF[i, 2]), envir = as.environment(projEnv))
  }

  projEnv$ProjectID <- projEnv$ProjectrDF$ProjectID

  print(
    paste(
      "#1 projEnv$EffDate: ",
      projEnv$EffDate,
      " projEnv$ProjectDF$EffDate: " ,
      projEnv$ProjectDF$EffDate
    )
  )
  projEnv$MLS <- projEnv$ProjectDF$MLS

  print("Load OneWayAggregation")
  projEnv$OneWayAggregationDF <-
    read.xlsx(projEnv$MlsFile,
              sheet = "OneWayAggregation" ,
              detectDates = TRUE)
  print("Load InteractionAggregation")
  projEnv$InteractionAggregationDF <-
    read.xlsx(projEnv$MlsFile,
              sheet = "InteractionAggregation" ,
              detectDates = TRUE)
  print("Load URARMapping")
  
  print("Load AllowedInteractions")
  projEnv$AllowedInteractionsDF <-
    read.xlsx(projEnv$MlsFile,
              sheet = "AllowedInteractions" ,
              detectDates = TRUE)
  print("Load MlsMapping")
  projEnv$MlsMappingDF <-
    read.xlsx(projEnv$MlsFile,
              sheet = "MlsMapping" ,
              detectDates = TRUE)
 
  print("Load HelpFields")
  projEnv$HelpFieldsDF <-
    read.xlsx(projEnv$MlsFile,
              sheet = "HelpFields" ,
              detectDates = TRUE)
  print("Load FieldsRecalculated")
  projEnv$FieldsRecalculatedDF <-
    read.xlsx(projEnv$MlsFile,
              sheet = "FieldsRecalculated" ,
              detectDates = TRUE)
  print("Load FieldsCalcStage1DF")
  projEnv$FieldsCalcStage1DF <-
    read.xlsx(projEnv$MlsFile,
              sheet = "FieldsCalcStage1" ,
              detectDates = TRUE)
  print("Load FieldsCalcStage2DF")
  projEnv$FieldsCalcStage2DF <-
    read.xlsx(projEnv$MlsFile,
              sheet = "FieldsCalcStage2" ,
              detectDates = TRUE)
  print("Load RegressionFields")
  projEnv$RegressionFieldsDF <-
    read.xlsx(projEnv$MlsFile,
              sheet = "RegressionFields" ,
              detectDates = TRUE)
  projEnv
}

#'  Replace periods in name vector with spaces
#'  @projEnv Name vector
#'  @return  Name vector
RepPeriodWithSpaceColumnNames <- function(cnms) {
  nc <- length(cnms)
  # Remove spaces, period, parentheses and pound signs from column names
  for (i in 1:nc) {
    cnmsI <- cnms[i]
    # replace period with space
    cnms[i] <- gsub("\\.", " ", cnmsI)
  }
  cnms
}

#'  Compress/clean column names
#'  Replace some characters with more descriptive names and remove unwanted characters such as spaces and periods
#'  @projEnv Name vector
#'  @return  Name vector
CompressColumnNames <- function(cnms) {
  nc <- length(cnms)

  print(paste("nc: ", nc))

  # Remove spaces, period, parentheses and pound signs from column names
  for (i in 1:nc) {
    cnmsI <- cnms[i]
    # replace pound sign with Nbr
    cnmsI <- gsub('#', "Nbr", cnmsI)
    # remove any characters not in the range A-z
    cnmsI <- gsub('[^A-z0-9]*', '', cnmsI)
    cnms[i] <- cnmsI
  }
  return (cnms)
}

#'  Apply CleanColumNames() to all project Data Frames
#'  @projEnv Name vector
#'  @return  Name vector
CompressColumnNamesAllDF <- function(projEnv) {
  print("Compress MlsDataDF")
  colnames(projEnv$MlsDataDF) <-
    CompressColumnNames(colnames(projEnv$MlsDataDF))
  print("Compress ProjectDF")
  colnames(projEnv$ProjectDF) <-
    CompressColumnNames(colnames(projEnv$ProjectDF))
  print("Compress OneWayAggregationDF")
  colnames(projEnv$OneWayAggregationDF)   <-
    CompressColumnNames(colnames(projEnv$OneWayAggregationDF))
  print("Compress InteractionAggregationDF")
  colnames(projEnv$InteractionAggregationDF)  <-
    CompressColumnNames(colnames(projEnv$InteractionAggregationDF))


  print("Compress AllowedInteractionsDF")
  colnames(projEnv$AllowedInteractionsDF)  <-
    CompressColumnNames(colnames(projEnv$AllowedInteractionsDF))
  print("Compress MlsMappingDF")
  colnames(projEnv$MlsMappingDF) <-
    CompressColumnNames(colnames(projEnv$MlsMappingDF))
  print("Compress ProjectDF")
  colnames(projEnv$ProjectDF) <-
    CompressColumnNames(colnames(projEnv$ProjectDF))
  print("Compress FieldsRecalculated")
  colnames(projEnv$FieldsRecalculated) <-
    CompressColumnNames(colnames(projEnv$FieldsRecalculated))
  print("Compress FieldsCalcStage1")
  colnames(projEnv$FieldsCalcStage1) <-
    CompressColumnNames(colnames(projEnv$FieldsCalcStage1))
  print("Compress FieldsCalcStage2")
  colnames(projEnv$FieldsCalcStage2) <-
    CompressColumnNames(colnames(projEnv$FieldsCalcStage2))


  print("Compress HelpFields")
  colnames(projEnv$HelpFields) <-
    CompressColumnNames(colnames(projEnv$HelpFields))

  print("Compress RegressionFields")
  colnames(projEnv$RegressionFields) <-
    CompressColumnNames(colnames(projEnv$RegressionFields))

  print("Finished compression")
  print(projEnv)
  print("Finished compressionA")
  return(projEnv)
}

#' We need to convert Data Frame Dates to Char/string for the upload to SQLite to work correctly
#'  @projEnv Name vector
#'  @return  Name vector
ConvertDatesToChar <- function(projEnv, df) {
  print("ConvertDatesToChar")

  # Get the number of columns
  nc <- ncol(df)

  # Get number of rows
  nr <- nrow(df)

  # Put all column types in array
  t <- sapply(df, class)

  # Find and convert Dates to character for upload into SQLite
  for (i in 1:nc) {
    if (t[i] == "Date") {
      print(paste("i/t[i]: ", i, t[i]))
      asChar <-
        as.character(projEnv$MlsDataDF[, i], format = "%m/%d/%Y")
      df[, i] <- asChar
      print(df[, i])
    }
  }
  return (projEnv)
}

#' We need to convert Data Frame Dates to Char/string for the upload to SQLite to work correctly
#'  @projEnv Project Environment
#'  @return  Project Environment
ConvertDatesToCharAllDF <- function(projEnv) {
  print("ConvertDatesToChar MlsDataDF")
  projEnv <- ConvertDatesToChar(projEnv, projEnv$MlsDataDF)
  print("ConvertDatesToChar ProjectDF")
  projEnv <- ConvertDatesToChar(projEnv, projEnv$ProjectDF)
  print("ConvertDatesToChar OneWayAggregationDF")
  projEnv <- ConvertDatesToChar(projEnv, projEnv$OneWayAggregationDF)
  print("ConvertDatesToChar InteractionAggregationDF")
  projEnv <-
    ConvertDatesToChar(projEnv, projEnv$InteractionAggregationDF)
  print("ConvertDatesToChar URARMlsMappingDF")

  print("ConvertDatesToChar AllowedInteractionsDF")
  projEnv <-
    ConvertDatesToChar(projEnv, projEnv$AllowedInteractionsDF)
  print("ConvertDatesToChar MlsMappingDF")
  projEnv <- ConvertDatesToChar(projEnv, projEnv$MlsMappingDF)
  projEnv
}

#' Store the MLS Data Frame to SQLite for ease of use, reliability
WriteMlsDfToSqlite <- function(projEnv)
{
  print("WriteMlsDfToSqlite")
  # Setup SQLite Folder and Filename Paths

  # Write MLS Data Frame to SQLite
  kveDB <- dbConnect(RSQLite::SQLite(), projEnv$DbFile)
  print("A1")
  dbWriteTable(kveDB, "mls", projEnv$MlsDataDF, overwrite = TRUE)
  print("A2")
  dbDisconnect(kveDB)
  print("A3")
  unlink("dbFile")
  print("A4")
  projEnv
}

#'  Write Config Mappings To SQLite
#'  @projEnv Name vector
#'  @return  Name vector
WriteConfigMappingsDfToSqlite <- function(projEnv)
{
  kveDB <- dbConnect(RSQLite::SQLite(), projEnv$DbFile)
  dbWriteTable(kveDB, "Project", projEnv$ProjectDF, overwrite = TRUE)
  dbWriteTable(kveDB,
               "OneWayAggregation",
               projEnv$OneWayAggregationDF,
               overwrite = TRUE)
  dbWriteTable(kveDB,
               "InteractionAggregation",
               projEnv$InteractionAggregationDF,
               overwrite = TRUE)
  #dbWriteTable(kveDB, "URARMapping", projEnv$URARMlsMappingDF, overwrite =
  #               TRUE)
  dbWriteTable(kveDB,
               "AllowedInteractions",
               projEnv$AllowedInteractionsDF,
               overwrite = TRUE)
  dbWriteTable(kveDB, "MlsMapping", projEnv$MlsMappingDF, overwrite = TRUE)
  dbWriteTable(kveDB, "HelpFields", projEnv$MlsMappingDF, overwrite = TRUE)
  dbWriteTable(kveDB, "FieldsCalcStage1", projEnv$MlsMappingDF, overwrite =
                 TRUE)
  dbWriteTable(kveDB, "FieldsCalcStage2", projEnv$MlsMappingDF, overwrite =
                 TRUE)

  dbWriteTable(kveDB,
               "FieldsRecalculated",
               projEnv$MlsMappingDF,
               overwrite = TRUE)
  dbWriteTable(kveDB,
               "RegressionFields",
               projEnv$MlsMappingDF,
               overwrite = TRUE)
  dbDisconnect(kveDB)
  unlink("dbFile")
  projEnv
}

#' Initial call is here to set up the environment under projEnv
#'  @projEnv Project Environment
#'  @codeFolder codeFolder,
#'  @projectParentFolder Project files parent folder
#'  @projectID Project ID
#'  @mlsDataFile MLS Data File
#'  @mlsSheet
#'  @targetVariable
#'  @pmethod
#'  @maxterms
#'  @mlsSheet MLS Data File Sheet Name
#'  @minspan
#'  @endspan
#'  @degree Earth degree arg
#'  @subjectCQA  Subject CQA
#' Initial call is here to set up the environment under projEnv
#' #export
SetUpProjectEnvironment <-
  function(projEnv,
           codeFolder,
           projectParentFolder,
           projectID,
           mlsDataFile,
           mlsSheet,
           targetVariable,
           bootstrap,
           pmethod,
           ncross,
           nfold,
           keepxy,
           penalty_1D,
           penalty_2D,
           trace,
           maxterms,
           nprune,
           minspan,
           endspan,
           degree,
           subjectCQA) {

             
           

    print("SetUpProjectEnvironment")

    projEnv$ProjectID <- projectID
    projEnv$ProjectParentFolder <- projectParentFolder
    projEnv$CodeFolder <- codeFolder
    projEnv$Maxterms <- maxterms
    projEnv$Nprune <- nprune
    projEnv$Degree <- degree
    projEnv$TargetVariable <- targetVariable
    projEnv$Minspan <- minspan
    projEnv$Endspan <- endspan
    projEnv$Bootstrap <- bootstrap
    projEnv$Pmethod <- pmethod
    projEnv$Ncross <- ncross
    projEnv$Nfold <- nfold
    projEnv$Keepxy <- keepxy
    projEnv$Penalty_1D <- penalty_1D
    projEnv$Penalty_2D <- penalty_2D
    projEnv$Trace <- trace
    projEnv$Cnames <- c(0)
    projEnv$INTERACTION_TABLE <- data.frame()
    print(paste("ProjectParentFolder: ", projEnv$ProjectParentFolder))
    # Project Folder Path
    projEnv$ProjectFolder <- projectParentFolder

    projEnv$AdjGrid <- data.frame()
    projEnv$AgMap <- data.frame()
    projEnv$LinPreds <- ""
    # MLS Data Folder Path
    projEnv$MlsFolder <- paste(projEnv$ProjectFolder, "Mls/", sep = "")
    projEnv$RFolder <- paste(projEnv$CodeFolder, "R/", sep = "")
    projEnv$SrcFolder <- paste(projEnv$CodeFolder, "src/", sep = "")
    projEnv$SubjectCQA <- subjectCQA
    projEnv$lmvOrig <- c(0)

    ts <- as.character(Sys.time())
    ts <- gsub("-","",ts)
    ts <- gsub(":","",ts)
    ts <- gsub(" ","_",ts)

    projEnv$MlsVersionFolder <- str_c(projEnv$MlsFolder, "VER", ts,"/")


    print(paste("MlsVersionFolder: ", projEnv$MlsVersionFolder))

    if (!file.exists(projEnv$MlsVersionFolder)){

      dir.create(projEnv$MlsVersionFolder)

      print("created")
    }

    # MLS File

    projEnv$MlsFile <- paste(projEnv$MlsFolder,mlsDataFile,".xlsx",sep="")

    # Add "2" for the prepared data file that is input to Stage I
    projEnv$MlsFileStageI <-
      paste(projEnv$MlsVersionFolder, mlsDataFile, "_StageI.xlsx", sep = "")
    projEnv$MlsFileStageIa <-
      paste(projEnv$MlsVersionFolder, mlsDataFile, "_StageIa.xlsx", sep = "")
    projEnv$MlsFileStageIb <-
      paste(projEnv$MlsVersionFolder, mlsDataFile, "_StageIb.xlsx", sep = "")
    projEnv$MlsFileStageIc <-
      paste(projEnv$MlsFolder, mlsDataFile, "_StageIc.xlsx", sep = "")
    projEnv$MlsFileStageII <-
      paste(projEnv$MlsVersionFolder, mlsDataFile, "_StageII.xlsx", sep = "")
    projEnv$MlsFileStageIIa <-
      paste(projEnv$MlsVersionFolder, mlsDataFile, "_StageIIa.xlsx", sep = "")
    projEnv$MlsFileStageIIb <-
      paste(projEnv$MlsVersionFolder, mlsDataFile, "_StageIIb.xlsx", sep = "")
    projEnv$MlsDataFileName <-
      paste(projEnv$MlsVersionFolder, mlsDataFile, "MlsData.xlsx", sep = "")


    projEnv$MlsSheet <- mlsSheet

    # Folder for SQLite DB
    projEnv$DbFolder <- paste(projEnv$ProjectFolder, "DB/", sep = '')

    projEnv$Log1 <- paste(projEnv$ProjectFolder, "Log/Log1", sep = '')
    projEnv$Log0 <- paste(projEnv$ProjectFolder, "Log/Log1", sep = '')
    projEnv$Log2 <- paste(projEnv$ProjectFolder, "Log/Log1", sep = '')

    # SQLite DB File Path and Name
    projEnv$DbFile <-
      paste(projEnv$DbFolder,  "DB.sqlite", sep = "")
    print(paste("DBFile: ", projEnv$DbFile))

    # Config   Folder Path
    projEnv$ConfigFolder <-
      paste(projEnv$ProjectFolder, "Earth/Config/", sep = '')

    # Source folder
    projEnv$SourceR <- here::here()
    projEnv$SourceUtilities <-
      paste(projEnv$SourceR, "/R/", "KVEsfr_Utilities.R", sep = "")

    projEnv$ListModelVars <- c()
    projEnv$ListModelFactors <- c()    
    projEnv$ResidualTable <- data.frame(    )
    projEnv$Mars <- c()
    #projEnv$RandomForest <- c()
    projEnv$ListModel <- c()
    projEnv$MlsDataDF$Adress <- NULL
    
    # Access value (0.0-10.0) in residual fuction dictionary
    projEnv$CqaToResidual <- object
    projEnv$VCqa <- as.vector(c(1,2,3))
    projEnv$VResiduals <- as.vector(c(1,2,3))
   
    # Number of times to trace vars in AllowedInteractions filter by earth()
    projEnv$AllowedInteractionsTrace <- 0  
    return (projEnv)  # our global environment
  }

#' Cross check MlsMapping Original Names are in the MLS Data
#' @projEnv Project Environment
#' @return None
CheckMlsMappingOriginalNamesAreInMlsData <- function(projEnv) {
  colnames(projEnv$MlsDataDF) <-
    RepPeriodWithSpaceColumnNames(colnames(projEnv$MlsDataDF))
  # All projEnv$MlsMappingDF.MLSFields should be in colnames(projEnv$MlsDataDF
  cns <- colnames(projEnv$MlsDataDF)
  flds <- projEnv$MlsMappingDF[, 1]
  AbortRun <- FALSE
  nf <- length(flds)
  for (i in 1:nf) {
    if (!flds[i] %in% cns) {
      print(
        paste(
          "MlsMappingDF.OriginalMLSFieldName",
          "[",
          i,
          "] is missing in MlsDataDF ",
          flds[i]
        )
      )
      AbortRun <- TRUE
    }
  }
  if (AbortRun) {
    print("Processing aborted due to errors.")
    return(FALSE)
  }
  print("Required MLS fields are in data.")
  return(TRUE)
}

#' Do any field renaming as set in the Mappings:MlsMapping sheet and then print as the initial AvailableFields in same named sheet
#' @projEnv Project Environment
#' @return None
RenameFieldsAndSetAvailable  <- function(projEnv) {

  renamed <-  projEnv$MlsMappingDF[["Renamed"]]
  mlsFields <-  projEnv$MlsMappingDF[["MLSFields"]]
  renameString <- paste( " plyr::rename(projEnv$MlsDataDF, replace = c(  \"" , projEnv$MlsMappingDF$MLSFields[1],"\"=\"",renamed[1],"\"",sep="")
  renamed <-
    trimws(renamed,
           which = c("both", "left", "right"),
           whitespace = "[ \t\r\n]")
  nr <- nrow(projEnv$MlsMappingDF)

  for (i in 2:nr) {
    if (is.na(renamed[i]) || renamed[i] == "") {
      renamed[i] <- "."
    }
    if (renamed[i] != ".") {
      renameString <-
        paste(renameString,
              ",\"",
              projEnv$MlsMappingDF$MLSFields[i],
              "\"=\"",
              renamed[i],"\"",sep="")
      projEnv$MlsMappingDF$MLSRenamedFields[i] <- renamed[i]
    } else {

    }
  }

  #print(paste("renameString: ",renameString))
  renameString <- paste(renameString, "))",sep="")
  print(renameString)
  projEnv$MlsDataDF <- eval(parse(text = renameString))
  return(projEnv)
}

#'  Add fields to the MLS Data Data Frame
#'  @projEnv Project Environment
#'  @return None
#' #export
AddFields <- function(projEnv) {
  flds <- projEnv$FieldsCalcStage1DF[["Fields"]]
  nf <- length(flds)
  nr <- nrow(projEnv$MlsDataDF)
  for (i in 1:nf) {
    cc <- flds[i]
    projEnv$MlsDataDF[cc] <- rep(0, nr)
  }

  flds <- projEnv$FieldsCalcStage2DF[["Fields"]]
  nf <- length(flds)

  for (i in 1:nf) {
    cc <- flds[i]
    # parm <- paste(flds[i],"=1:",nr)
    projEnv$MlsDataDF[cc] <- rep(0, nr)
  }

  flds <- projEnv$HelpFields[["Fields"]]
  nf <- length(flds)

  for (i in 1:nf) {
    cc <- flds[i]
    projEnv$MlsDataDF[cc] <- rep(0, nr)
  }
}

#' Do the calculations for calculated fields
#' @projEnv Project Environment
#' @return None
#' #export
DoCalculations1 <- function(projEnv) {
  #print("M1")
  flds <- projEnv$FieldsCalcStage1DF[["Fields"]]
  calcs <- projEnv$FieldsCalcStage1DF[["Calculation"]]
  vars <- projEnv$FieldsCalcStage1DF[["Variable"]]
  EffDate <- projEnv$EffDate
  # We just need to set this var onsce
  # print(paste("EffDate: ", EffDate))
  nf <- length(flds)
  nr <- nrow(projEnv$MlsDataDF)

  for (c in 1:nf) {
    # go through FieldsCalcStage1
    for (row in 1:nr) {
      # for each field go through all rows in data
      cc <- flds[c]
      calc <- calcs[c]
      vars1 <- vars[c]
      varList <- as.vector(strsplit(vars1, ",")[[1]])
      vLen <- length(varList)

      for (v in 1:vLen) {
        varX <- varList[v]
        if (varX != "EffDate") {
          # Check var is not a global var such as EffDate
          assign(varX, projEnv$MlsDataDF[row, varX])
        }
      }

      projEnv$MlsDataDF[row, cc] <- eval(parse(text = calc))
    }
  }

  flds <- projEnv$FieldsRecalculatedDF[["Fields"]]
  calcs <- projEnv$FieldsRecalculatedDF[["Calculation"]]
  vars <- projEnv$FieldsRecalculatedDF[["Variables"]]
  nf <- length(flds)
  nr <- nrow(projEnv$MlsDataDF)

  if(nf >= 1) { 
    for (c in 1:nf) {

    # go through FieldsRecalculated
    for (row in 1:nr) {

      # for each field go through all rows in data
      cc <- flds[c]
      calc <- calcs[c]
      vars1 <- vars[c]
      varList <- as.vector(strsplit(vars1, ",")[[1]])
      vLen <- length(varList)

      for (v in 1:vLen) {
        varX <- varList[v]
        if (varX != "EffDate") {
          # Check var is not a global var such as EffDate
          assign(varX, projEnv$MlsDataDF[row, varX])
        }
      }
      projEnv$MlsDataDF[row, cc] <- eval(parse(text = calc))
     }
   }
  }
}

cppFunction('int one() {
  return 1;
}')

cppFunction('int signC(int x) {
  if (x > 0) {
    return 1;
  } else if (x == 0) {
    return 0;
  } else {
    return -1;
  }
}')

AbortRun <- FALSE

#' SetUp
#'  @projEnv Project Environment
#'  @codeFolder codeFolder,
#'  @projectParentFolder Project files parent folder
#'  @projectID Project ID
#'  @mlsData MLS Data File
#'  @mlsSheet MLS Data File Sheet Name
#'  @subjectCQA  Subject CQA
#'
#'  @example
#'  Stage_1(projEnv)
#'
#' Out main Stage I function:
#' #export
SetUp_0 <-
  function(projEnv,
           codeFolder,
           projectParentFolder,
           projectID,
           mlsData,
           mlsSheet,
           targetVariable,
           bootstrap,
           pmethod,
           ncross,
           nfold,
           keepxy,
           penalty_1D,
           penalty_2D,
           trace,
           maxterms,
           nprune,
           minspan,
           endspan,
           degree,
           subjectCQA) {

    flog.appender(appender.console(), name="SC")
    flog.appender(appender.file("C:/Order_1/Market/_SetUp.log"),name="S1")

    projectParentFolder <-  projectParentFolder
    projectID <- projectID
    mlsData <- mlsData
    mlsSheet <- mlsSheet

   # flog.info( "SetUpProjectEnvironment",name="S1")
    SetUpProjectEnvironment(projEnv, codeFolder, projectParentFolder, projectID, mlsData, 
                            mlsSheet, targetVariable,bootstrap, pmethod,      ncross,
                            nfold,   keepxy,  penalty_1D,  penalty_2D,trace,maxterms,nprune,minspan,
                            endspan,degree,subjectCQA)
 
    flog.info( "Load MLS Excel and Mapping ",name="S1")

    # Create the MLS Data Frame
    projEnv <- LoadMlsExcelToDataFrame(projEnv)
    projEnv <- LoadMappingsExcelToDataFrame(projEnv)
    #print(paste("projEnv$EffDate1", projEnv$EffDate))


    flog.info( "Check Original Nameps in MLSData ",name="S1")
    if (!CheckMlsMappingOriginalNamesAreInMlsData(projEnv)) {
      print("Run aborted")
      return
    }

    flog.info( "Compress Column Names ",name="S1")
    projEnv <- CompressColumnNamesAllDF(projEnv)
    
    flog.info( "Convert Dates To Char ",name="S1")
    projEnv <- ConvertDatesToCharAllDF(projEnv)

    flog.info( "Write DataFrames to SQLite ",name="S1")
    # Store the MLS Data Frame to the SQLite DB
    print("WriteMlsDfToSqlite")
    projEnv <- WriteMlsDfToSqlite(projEnv)
    print("WriteMlsDfToSqlite")
    projEnv <- WriteConfigMappingsDfToSqlite(projEnv)
    print("Add Fields ")
    flog.info( "Add Fields ",name="S1")
    # Add fields
    AddFields(projEnv)
    print("Rename Fields")
    flog.info( "Rename Fields ",name="S1")
    RenameFieldsAndSetAvailable (projEnv)
    print("Convert DF Fields Bool To Binary")
    flog.info( "Convert DF Fields Bool To Binary ",name="S1")

    # This needs to be set in configuration TODO
    ConvertDfFieldBoolToBinary(projEnv, "PoolYN")
    flog.info( "Do Calculations ",name="S1")

    # Do calculations for calculated fields
    DoCalculations1(projEnv)
    flog.info( paste("Write data to Excel file: ",projEnv$MlsFileStageI),name="S1")

    write_xlsx(
      projEnv$MlsDataDF,
      projEnv$MlsFileStageI,
      col_names = TRUE,
      format_headers = TRUE,
      use_zip64 = FALSE
    )

    flog.info( paste("Generated file: ",projEnv$MlsFileStageI),name="S1")

    flog.info( "Done with Set-Up",name="S1")

  }
