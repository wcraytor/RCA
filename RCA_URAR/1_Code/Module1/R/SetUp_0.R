################################################   Setup #########################################
# Author:       Wm. Bert Craytor
# Location:     242 Clifton Rd., Pacifica, CA 94044, USA
# License:      MIT License
# Description:  Setup script to generate MARS (earth)  analysis for appraisal data using R:earth 
 
#'  Loads the initial MLS Excel Data File to Data Frames
#'  @return  Project Environment
#'
#' #export
LoadMlsExcelToDataFrame <- function() {
flog.info("Stage I Finished",name="Log2File")

  print("LoadMlsExcelToDataFrame")
  flog.info(paste("MLSData File: ",projEnv$MlsDataFile," ",projEnv$MlsSheet),name="Log2File")

 # Load MLS File into MLS Data Frame
  projEnv$MlsDataDF <-
    read.xlsx(projEnv$MlsDataFile,
              sheet = projEnv$MlsSheet ,
              detectDates = TRUE)

  # Keep Orignal copy for version folder
  projEnv$MlsDataDFOriginal <-
    read.xlsx(projEnv$MlsDataFile,
              sheet = projEnv$MlsSheet ,
              detectDates = TRUE)

  assign("MLS", projEnv$MlsDataDF)

}

#'  Load the Mappings (or configuation file) to a Data Frame
#'  @return  Project Environment
LoadMappingsExcelToDataFrame <- function() {
  flog.info("LoadMappingsExcelToDataFrame",name="Log2File")

  # Load MLS File into MLS Data Frame
  flog.info("Load Project",name="Log2File")
 
  projEnv$ProjectDF <-
    read.xlsx(projEnv$MlsDataFile,
              sheet = "Project" ,
              detectDates = TRUE)
			  
  # Assign the variables in the "Project" sheet to projEnv variables of same name, with the associated values in that spreadsheet
  nr <- nrow(projEnv$ProjectDF)
  for (i in 1:nr) {
    var1 <-  projEnv$ProjectDF[i, 1]
    assign(var1, as.character(projEnv$ProjectDF[i, 2]), envir = as.environment(projEnv))
  }

  projEnv$ProjectID <- projEnv$ProjectrDF$ProjectID

  flog.info(
    paste(
      "#1 projEnv$EffDate: ",
      projEnv$EffDate,
      " projEnv$ProjectDF$EffDate: " ,
      projEnv$ProjectDF$EffDate,name="Log2File")
  )
  projEnv$MLS <- projEnv$ProjectDF$MLS

  flog.info("Load OneWayAggregation",name="Log2File")
  projEnv$OneWayAggregationDF <-
    read.xlsx(projEnv$MlsDataFile,
              sheet = "OneWayAggregation" ,
              detectDates = TRUE)
  flog.info("Load InteractionAggregation",name="Log2File")
  projEnv$InteractionAggregationDF <-
    read.xlsx(projEnv$MlsDataFile,
              sheet = "InteractionAggregation" ,
              detectDates = TRUE)
  flog.info("Load URARMapping",name="Log2File")
  
  flog.info("Load AllowedInteractions",name="Log2File")
  projEnv$AllowedInteractionsDF <-
    read.xlsx(projEnv$MlsDataFile,
              sheet = "AllowedInteractions" ,
              detectDates = TRUE)
  flog.info("Load MlsMapping",name="Log2File")
  projEnv$MlsMappingDF <-
    read.xlsx(projEnv$MlsDataFile,
              sheet = "MlsMapping" ,
              detectDates = TRUE)
 
  flog.info("Load HelpFields",name="Log2File")
  projEnv$HelpFieldsDF <-
    read.xlsx(projEnv$MlsDataFile,
              sheet = "HelpFields" ,
              detectDates = TRUE)
  flog.info("Load FieldsRecalculated",name="Log2File")
  projEnv$FieldsRecalculatedDF <-
    read.xlsx(projEnv$MlsDataFile,
              sheet = "FieldsRecalculated" ,
              detectDates = TRUE)
  flog.info("Load FieldsCalcStage1DF",name="Log2File")
  projEnv$FieldsCalcStage1DF <-
    read.xlsx(projEnv$MlsDataFile,
              sheet = "FieldsCalcStage1" ,
              detectDates = TRUE)
  flog.info("Load FieldsCalcStage2DF",name="Log2File")
  projEnv$FieldsCalcStage2DF <-
    read.xlsx(projEnv$MlsDataFile,
              sheet = "FieldsCalcStage2" ,
              detectDates = TRUE)
  flog.info("Load RegressionFields",name="Log2File")
  projEnv$RegressionFieldsDF <-
    read.xlsx(projEnv$MlsDataFile,
              sheet = "RegressionFields" ,
              detectDates = TRUE)
  
}

#'  Replace periods in name vector with spaces
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
#'  @return  Name vector
CompressColumnNames <- function(cnms) {
  nc <- length(cnms)

  if(nc == 0) 
    return (NULL)
	
  flog.info(paste("nc: ", nc),name="Log2File")

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
#'  @return  Name vector
CompressColumnNamesAllDF <- function() {

  flog.info("Compress MlsDataDF",name="Log2File")

  colnames(projEnv$MlsDataDF) <-
    CompressColumnNames(colnames(projEnv$MlsDataDF))

  flog.info("Compress ProjectDF",name="Log2File")
  colnames(projEnv$ProjectDF) <-
    CompressColumnNames(colnames(projEnv$ProjectDF))
  flog.info("Compress OneWayAggregationDF",name="Log2File")

  colnames(projEnv$OneWayAggregationDF)   <-
    CompressColumnNames(colnames(projEnv$OneWayAggregationDF))
  flog.info("Compress InteractionAggregationDF",name="Log2File")

  colnames(projEnv$InteractionAggregationDF)  <-
    CompressColumnNames(colnames(projEnv$InteractionAggregationDF))

  flog.info("Compress AllowedInteractionsDF",name="Log2File")
  colnames(projEnv$AllowedInteractionsDF)  <-
    CompressColumnNames(colnames(projEnv$AllowedInteractionsDF))

  flog.info("Compress MlsMappingDF",name="Log2File")
  colnames(projEnv$MlsMappingDF) <-
    CompressColumnNames(colnames(projEnv$MlsMappingDF))

  flog.info("Compress ProjectDF",name="Log2File")
  colnames(projEnv$ProjectDF) <-
    CompressColumnNames(colnames(projEnv$ProjectDF))

  flog.info("Compress FieldsRecalculated",name="Log2File")

  colnames(projEnv$FieldsRecalculatedDF) <-
    CompressColumnNames(colnames(projEnv$FieldsRecalculatedDF))

  flog.info("Compress FieldsCalcStage1",name="Log2File")
  colnames(projEnv$FieldsCalcStage1DF) <-
       CompressColumnNames(colnames(projEnv$FieldsCalcStage1DF))

  flog.info("Compress FieldsCalcStage2",name="Log2File")
  colnames(projEnv$FieldsCalcStage2DF) <-
        CompressColumnNames(colnames(projEnv$FieldsCalcStage2DF))

   flog.info("Compress HelpFields",name="Log2File")
   colnames(projEnv$HelpFieldsDF) <-
     CompressColumnNames(colnames(projEnv$HelpFieldsDF))

  flog.info("Compress RegressionFields",name="Log2File")
  colnames(projEnv$RegressionFieldsDF) <-
    CompressColumnNames(colnames(projEnv$RegressionFieldsDF))
}

#' We need to convert Data Frame Dates to Char/string for the upload to SQLite to work correctly
#'  @return  Name vector
ConvertDatesToChar <- function(  df) {
  flog.info("ConvertDatesToChar",name="Log2File")

  # Get the number of columns
  nc <- ncol(df)

  # Get number of rows
  nr <- nrow(df)

  # Put all column types in array
  t <- sapply(df, class)

  # Find and convert Dates to character for upload into SQLite
  for (i in 1:nc) {
    if (t[i] == "Date") {
      flog.info(paste("i/t[i]: ", i, t[i]),name="Log2File")
      asChar <-
        as.character(projEnv$MlsDataDF[, i], format = "%m/%d/%Y")
      df[, i] <- asChar
      flog.info(df[, i],name="Log2File")
    }
  }
 
}

#' We need to convert Data Frame Dates to Char/string for the upload to SQLite to work correctly
#'  @return  Project Environment
ConvertDatesToCharAllDF <- function( ) {
  flog.info("ConvertDatesToChar MlsDataDF",name="Log2File")
  ConvertDatesToChar(  projEnv$MlsDataDF)

  flog.info("ConvertDatesToChar ProjectDF",name="Log2File")
  ConvertDatesToChar(  projEnv$ProjectDF)

  flog.info("ConvertDatesToChar OneWayAggregationDF",name="Log2File")
  ConvertDatesToChar(   projEnv$OneWayAggregationDF)

  flog.info("ConvertDatesToChar InteractionAggregationDF",name="Log2File")
  ConvertDatesToChar(  projEnv$InteractionAggregationDF)

  flog.info("ConvertDatesToChar URAR MlsMappingDF",name="Log2File")

  flog.info("ConvertDatesToChar AllowedInteractionsDF",name="Log2File")
  ConvertDatesToChar( projEnv$AllowedInteractionsDF)

  flog.info("ConvertDatesToChar MlsMappingDF",name="Log2File")
  ConvertDatesToChar(  projEnv$MlsMappingDF)
  
}

#' Store the MLS Data Frame to SQLite for ease of use, reliability
WriteMlsDfToSqlite <- function()
{
  flog.info("WriteMlsDfToSqlite",name="Log2File")
  # Setup SQLite Folder and Filename Paths

  # Write MLS Data Frame to SQLite
  kveDB <- dbConnect(RSQLite::SQLite(), projEnv$DbFile)

  flog.info("A1",name="Log2File")
  dbWriteTable(kveDB, "mls", projEnv$MlsDataDF, overwrite = TRUE)
  flog.info("A2",name="Log2File")
  dbDisconnect(kveDB)
  flog.info("A3",name="Log2File")
  #unlink("dbFile")
  flog.info("A4",name="Log2File")
}

#'  Write Config Mappings To SQLite
#'  @return  Name vector
WriteConfigMappingsDfToSqlite <- function()
{
  flog.info("WriteConfigMappintsDfToSqlite 1",name="Log2File")
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
    flog.info("WriteConfigMappintsDfToSqlite 2",name="Log2File")
  unlink("dbFile")
    flog.info("WriteConfigMappintsDfToSqlite 3",name="Log2File")
}

#' Initial call is here to set up the environment under projEnv
#'  @projEnv Project Environment
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
  function( 
           projectID,
           mlsDataFile,
           mlsSheet,
           targetVariable,
           bootstrap,
           pmethod,
           ncross,
           nfold,
           keepxy,
		   filterInteractions,
           penalty_1D,
           penalty_2D,
           trace,
           maxterms,
           nprune,
           minspan,
           endspan,
           degree,
           subjectCQA,
		   subjectCQA2) {

    flog.info("SetUpProjectEnvironment",name="Log2File")

    projEnv$ProjectID <- projectID
  
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
	  projEnv$FilterInteractions <- filterInteractions
    projEnv$Penalty_1D <- penalty_1D
    projEnv$Penalty_2D <- penalty_2D
    projEnv$Trace <- trace
    projEnv$Cnames <- c(0)
    projEnv$INTERACTION_TABLE <- data.frame()
 
    # Project Folder Path
    
    projEnv$AdjGrid <- data.frame()
    projEnv$AgMap <- data.frame()
    projEnv$LinPreds <- ""
    projEnv$SubjectCQA <- subjectCQA
    projEnv$SubjectCQA2 <- subjectCQA2
    projEnv$LmvOrig <- c(0)

   

    if (!file.exists(projEnv$MlsVersionFolder)){

      dir.create(projEnv$MlsVersionFolder)

      flog.info("created",name="Log2File")
    }

    # MLS File
 
    projEnv$MlsFile <-     projEnv$MlsDataFile

    # Add "2" for the prepared data file that is input to Stage I
     projEnv$MlsFileOriginal <-
      paste(projEnv$MlsVersionFolder, mlsDataFile, "_Original.xlsx", sep = "")
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
 



    # SQLite DB File Path and Name
    projEnv$DbFile <-
      paste(projEnv$DbFolder,  "DB.sqlite", sep = "")
    flog.info(paste("DBFile: ", projEnv$DbFile),name="Log2File")

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
CheckMlsMappingOriginalNamesAreInMlsData <- function() {
  colnames(projEnv$MlsDataDF) <-
    RepPeriodWithSpaceColumnNames(colnames(projEnv$MlsDataDF))
  # All projEnv$MlsMappingDF.MLSFields should be in colnames(projEnv$MlsDataDF
  cns <- colnames(projEnv$MlsDataDF)
  flds <- projEnv$MlsMappingDF[, 1]
  AbortRun <- FALSE
  nf <- length(flds)
  for (i in 1:nf) {
    if (!flds[i] %in% cns) {
      flog.info(
        paste(
          "MlsMappingDF.OriginalMLSFieldName",
          "[",
          i,
          "] is missing in MlsDataDF ",
          flds[i]
        ),name="Log2File")
      
      AbortRun <- TRUE
    }
  }
  if (AbortRun) {
    flog.info("Processing aborted due to errors.",name="Log2File")
    return(FALSE)
  }
  flog.info("Required MLS fields are in data.",name="Log2File")
  return(TRUE)
}

#' Do any field renaming as set in the Mappings:MlsMapping sheet and then print as the initial AvailableFields in same named sheet
#' @return None
RenameFieldsAndSetAvailable  <- function() {

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

  #flog.info(paste("renameString: ",renameString,name="Log2File"))
  renameString <- paste(renameString, "))",sep="")
  flog.info(renameString,name="Log2File")
  projEnv$MlsDataDF <- eval(parse(text = renameString))

}

#'  Add fields to the MLS Data Data Frame
#'  @projEnv Project Environment
#'  @return None
#' #export
AddFields <- function() {
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
#' @return None
#' #export
DoCalculations1 <- function() {
  flds <- projEnv$FieldsCalcStage1DF[["Fields"]]
  calcs <- projEnv$FieldsCalcStage1DF[["Calculation"]]
  vars <- projEnv$FieldsCalcStage1DF[["Variable"]]
  EffDate <- projEnv$EffDate
  # We just need to set this var onsce
  # flog.info(paste("EffDate: ", EffDate,name="Log2File"))
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


#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#' SetUp
#'  @codeFolder codeFolder,
#'  @projectFolder Project files parent folder
#'  @projectID Project ID
#'  @mlsData MLS Data File
#'  @mlsSheet MLS Data File Sheet Name
#'  @subjectCQA  Subject CQA
#'
#'  @example
#'
#' Out main Stage I function:
#' #export
SetUp_0 <-
  function( 
           projectID,
           mlsData,
           mlsSheet,
           targetVariable,
           bootstrap,
           pmethod,
           ncross,
           nfold,
           keepxy,
		       filterInteractions,
           penalty_1D,
           penalty_2D,
           trace,
           maxterms,
           nprune,
           minspan,
           endspan,
           degree,
           subjectCQA,
		   subjectCQA2) 
{
    flog.info( "Starting Setup_0")	
    projectFolder <-  projEnv$ProjectFolder
    projectID <- projectID
    mlsData <- mlsData
    mlsSheet <- mlsSheet
 
    SetUpProjectEnvironment(    projectID, mlsData, 
                            mlsSheet, targetVariable,bootstrap, pmethod,      ncross,
                            nfold,   keepxy, filterInteractions, penalty_1D,  penalty_2D,trace,maxterms,nprune,minspan,
                            endspan,degree,subjectCQA,subjectCQA2)
 
    flog.info( "Load MLS Excel and Mapping ",name = "Log2Console")

    # Create the MLS Data Frame
    LoadMlsExcelToDataFrame()
    LoadMappingsExcelToDataFrame()

    flog.info( "Check Original Nameps in MLSData ")
    if (!CheckMlsMappingOriginalNamesAreInMlsData( )) {
      flog.info("Run aborted",name="Log2File")
      return
    }

    flog.info( "Compress Column Names ")
    CompressColumnNamesAllDF( )
    
    flog.info( "Convert Dates To Char " )
    ConvertDatesToCharAllDF( )

    flog.info( "Write DataFrames to SQLite " )
    # Store the MLS Data Frame to the SQLite DB
    flog.info("WriteMlsDfToSqlite" )
    WriteMlsDfToSqlite( )
    WriteConfigMappingsDfToSqlite( )
    flog.info("Add Fields ")
 
    # Add fields
    AddFields( )
    flog.info("Rename Fields" )
    RenameFieldsAndSetAvailable ( )
    flog.info("Convert DF Fields Bool To Binary" )
  
    # This needs to be set in configuration TODO
    ConvertDfFieldBoolToBinary(   "PoolYN")
    flog.info( "Do Calculations ")

    # Do calculations for calculated fields
    DoCalculations1( )
    flog.info( paste("Write data to Excel file: ",projEnv$MlsFileStageI))

    write_xlsx(
      projEnv$MlsDataDFOriginal,
      projEnv$MlsFileOriginal,
      col_names  =  TRUE,
      format_headers  =  TRUE,
      use_zip64  =  FALSE
    )
    write_xlsx(
      projEnv$MlsDataDF,
      projEnv$MlsFileStageI,
      col_names  =  TRUE,
      format_headers  =  TRUE,
      use_zip64  =  FALSE
    )

    flog.info( paste("Generated file: ",projEnv$MlsFileStageI))

    flog.info( "Done with Set-Up")

  }
