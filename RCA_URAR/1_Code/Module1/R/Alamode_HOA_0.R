################################################   Setup ##########################################
# Author:       Wm. Bert Craytor
# Location:     242 Clifton Rd., Pacifica, CA 94044, USA
# Date:         07/02/2021
# Description:  Setup script to generate MARS (earth)  analysis for appraisal data using R:earth
################################################ KVEsfr_Utilities.R ##############################
#
# Notes:        1.  This program is free software; you can redistribute it and/or modify
#                   it under the terms of the GNU General Public License as published by
#                   the Free Software Foundation; either version 3 of the License, or
#                   (at your option) any later version.
#
#               2.  This program is distributed in the hope that it will be useful,
#                   but WITHOUT ANY WARRANTY; without even the implied warranty of
#                   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#                   GNU General Public License for more details.
#
#               3.  A copy of the GNU General Public License is available at
#                   http://www.r-project.org/Licenses
#
###################################################################################################
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

#'  Load the Mappings (or configuation file) to a Data Frame
#'  @projEnv Project Environment
#'  @return  Project Environment
LoadCompsToDataFrame <- function(projEnv) {

  print("LoadCompsToDataFrame")

  print(paste("Comps File: ",projEnv$MlsCompsFile))

  projEnv$MlsCompsDF <-
    read.xlsx(projEnv$MlsCompsFile,
              sheet = "X" ,
              detectDates = TRUE)

  # Assign the variables in the "Project" sheet to projEnv variables of same name, with the associated values in that
  # spreadsheet

  projEnv$ProjectID <- projEnv$ProjectrDF$ProjectID

  projEnv$AlamodeInputDF <-
    read.xlsx(projEnv$AlamodeInputFile,
              sheet = "X" ,
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
  cnms
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
        as.character(projEnv$alamodeInputDF[, i], format = "%m/%d/%Y")
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
  print("ConvertDatesToChar alamodeInputDF")
   ConvertDatesToChar(projEnv, projEnv$AlamodeInputDF)
    ConvertDatesToChar(projEnv, projEnv$MlsCompsDF)
  projEnv
}

#' Initial call is here to set up the environment under projEnv
#'  @projEnv Project Environment
#'  @codeFolder codeFolder,
#'  @projectParentFolder Project files parent folder
#'  @projectID Project ID
#'  @mlsComps MLS Data File
#'  @alamodeInput Alamode Input File
#' Initial call is here to set up the environment under projEnv
#' #export
SetUpProjectEnvironment <-
  function(projEnv,
           codeFolder,
           projectParentFolder,
           projectID,
           mlsComps,
           alamodeInput ) {

    print("SetUpProjectEnvironment")

    projEnv$ProjectID <- projectID
    projEnv$ProjectParentFolder <- projectParentFolder
    projEnv$CodeFolder <- codeFolder

    print(paste("ProjectParentFolder: ", projEnv$ProjectParentFolder))
    # Project Folder Path
    projEnv$ProjectFolder <-
      paste(projectParentFolder, projEnv$ProjectID, "/", sep = "" )
    # MLS Data Folder Path
    projEnv$AlamodeFolder <- paste(projEnv$ProjectFolder, "Earth/Alamode/", sep = "")
    projEnv$RFolder <- paste(projEnv$CodeFolder, "R/", sep = "")
    projEnv$SrcFolder <- paste(projEnv$CodeFolder, "src/", sep = "")
    projEnv$MlsCompsDF <- data.frame()
    projEnv$AlamodeInputDF <- data.frame()

    # MLS File
    projEnv$MlsCompsFile <-
      paste(projEnv$AlamodeFolder, mlsComps, ".xlsx", sep = "")

    projEnv$AlamodeInputFile <-
      paste(projEnv$AlamodeFolder, alamodeInput, ".xlsx", sep = "")




    projEnv$Log1 <- paste(projEnv$ProjectFolder, "Earth/Log/Log1", sep = '')
    projEnv$Log0 <- paste(projEnv$ProjectFolder, "Earth/Log/Log1", sep = '')
    projEnv$Log2 <- paste(projEnv$ProjectFolder, "Earth/Log/Log1", sep = '')


    # Config   Folder Path
    projEnv$ConfigFolder <-
      paste(projEnv$ProjectFolder, "Earth/Config/", sep = '')


    # Source folder
    projEnv$SourceR <- here()
    projEnv$SourceUtilities <-
      paste(projEnv$SourceR, "/R/", "KVEsfr_Utilities.R", sep = "")



    return (projEnv)  # our global environment
  }

AbortRun <- FALSE

#' SetUp
#'  @projEnv Project Environment
#'  @codeFolder codeFolder,
#'  @projectParentFolder Project files parent folder
#'  @projectID Project ID
#'  @mlsComps MLS Data File
#'  @alamodeInput MLS Data File Sheet Name
#'
#'  @example
#'  Stage_1(projEnv)
#'
#' Out main Stage I function:
#' #export
Alamode_HOA_0 <-
  function(projEnv,
           codeFolder,
           projectParentFolder,
           projectID,
           mlsComps,
           alamodeInput
 ) {

    codeFolder <- "C:/RPackages/Alamode/"
    projectParentFolder<-  "C:/Order_1/B/A/PVN2022/"
    projectID<- "KVE22_02_BoulderCreek"
    mlsComps<- "MlsComps"
    alamodeInput<- "AlamodeInput"

    flog.appender(appender.console(), name="SC")
    flog.appender(appender.file("C:/Earth/Projects/AlamodeSetUp.log"),name="S1")

    projectParentFolder <-  projectParentFolder
    projectID <- projectID
    alamodeInput <- alamodeInput

    SetUpProjectEnvironment(projEnv, codeFolder, projectParentFolder, projectID, mlsComps, alamodeInput)

    projEnv <- LoadCompsToDataFrame(projEnv)




    # Transfer Street Addresses Address 1:9 -> B2,C2,E2,G2,H2,I2,K2,M2,O2,Q2,S2,V2
    #                                     Subj,1,2,3,4,5,6,7,8,9
    projEnv$AlamodeInputDF[1,"Subj"] <- projEnv$MlsCompsDF[1,"Address"]
    projEnv$AlamodeInputDF[2,"Subj"] <- projEnv$MlsCompsDF[1,"CityStateZip"]
   # projEnv$AlamodeInputDF[3,"Subj"] <- projEnv$MlsCompsDF[1,"CityStateZip"]
   # projEnv$AlamodeInputDF[4,"Subj"] <- projEnv$MlsCompsDF[1,"CityStateZip"]
   # projEnv$AlamodeInputDF[5,"Subj"] <- projEnv$MlsCompsDF[1,"CityStateZip"]
    projEnv$AlamodeInputDF[6,"Subj"] <- str_c("MLSL #",projEnv$MlsCompsDF[1,"MlsNbr"],";DOM ",projEnv$MlsCompsDF[1,"DOM"])
    projEnv$AlamodeInputDF[7,"Subj"] <- str_c("Realist/Doc #",projEnv$MlsCompsDF[1,"Doc"] )
    #projEnv$AlamodeInputDF[8,"Subj"] <- projEnv$MlsCompsDF[1,"CityStateZip"]
    projEnv$AlamodeInputDF[9,"Subj"] <- projEnv$MlsCompsDF[1,"SaleConc1"]
    projEnv$AlamodeInputDF[10,"Subj"] <- projEnv$MlsCompsDF[1,"SaleConc2"]
    projEnv$AlamodeInputDF  [11,"Subj"] <- format(projEnv$MlsCompsDF[1,"SaleDate"],format="%m/%d/%Y")
    projEnv$AlamodeInputDF[12,"Subj"] <- str_c(projEnv$MlsCompsDF[1,"LocNBA"],";",projEnv$MlsCompsDF[1,"LocDsc"] )
    projEnv$AlamodeInputDF[13,"Subj"] <- "Fee Simple"
    projEnv$AlamodeInputDF[14,"Subj"] <- str_c(projEnv$MlsCompsDF[1,"LotSize"]," sf" )
    projEnv$AlamodeInputDF[15,"Subj"] <- str_c(projEnv$MlsCompsDF[1,"ViewNBA"] )
    projEnv$AlamodeInputDF[16,"Subj"] <-  str_c(projEnv$MlsCompsDF[1,"AttDet"], projEnv$MlsCompsDF[1,"Stories"] ,";",projEnv$MlsCompsDF[1,"Style"])
    projEnv$AlamodeInputDF[17,"Subj"] <- projEnv$MlsCompsDF[1,"Qual"]
    projEnv$AlamodeInputDF[18,"Subj"] <- projEnv$MlsCompsDF[1,"Age"]
    projEnv$AlamodeInputDF[19,"Subj"] <- projEnv$MlsCompsDF[1,"Cond"]
    #projEnv$AlamodeInputDF[20,"Subj"] <- projEnv$MlsCompsDF[1,"CityStateZip"]
    projEnv$AlamodeInputDF[21,"Subj"] <- projEnv$MlsCompsDF[1,"Total"]
    projEnv$AlamodeInputDF[22,"Subj"] <- projEnv$MlsCompsDF[1,"Beds"]
    projEnv$AlamodeInputDF[23,"Subj"] <- projEnv$MlsCompsDF[1,"Baths"]
    projEnv$AlamodeInputDF[24,"Subj"] <- projEnv$MlsCompsDF[1,"GLA"]
    #projEnv$AlamodeInputDF[25,"Subj"] <- projEnv$MlsCompsDF[1,"XXX"]
    #projEnv$AlamodeInputDF[26,"Subj"] <- projEnv$MlsCompsDF[1,"CityStateZip"]
    projEnv$AlamodeInputDF[27,"Subj"] <- projEnv$MlsCompsDF[1,"FU"]
    projEnv$AlamodeInputDF[28,"Subj"] <- projEnv$MlsCompsDF[1,"Heat"]
    projEnv$AlamodeInputDF[29,"Subj"] <- projEnv$MlsCompsDF[1,"Energy"]
    projEnv$AlamodeInputDF[30,"Subj"] <- projEnv$MlsCompsDF[1,"GarDscGaGdGbiDw"]
   # projEnv$AlamodeInputDF[31,"Subj"] <- projEnv$MlsCompsDF[1,"CityStateZip"]
   # projEnv$AlamodeInputDF[32,"Subj"] <- projEnv$MlsCompsDF[1,"CityStateZip"]
   # projEnv$AlamodeInputDF[33,"Subj"] <- projEnv$MlsCompsDF[1,"CityStateZip"]
    projEnv$AlamodeInputDF[34,"Subj"] <- str_c("CQA",projEnv$MlsCompsDF[1,"CQA"],":$",round(projEnv$MlsCompsDF[1,"URAR_CQA"],digits=0))

    for(i in 1:6) {
      print(paste("i: ",i))
      projEnv$AlamodeInputDF[1,toString(i)] <- projEnv$MlsCompsDF[i+1,"Address"]
      unitNumber <- projEnv$MlsCompsDF[i+1,"UnitNumber"]
      if(is.na(unitNumber)) {
        unitNumber <- ""
      } else  {
         unitNumber <- str_c(unitNumber,",")
      }
      projEnv$AlamodeInputDF[2,toString(i)] <-  str_c(unitNumber, projEnv$MlsCompsDF[i+1,"CityStateZip"])
      projEnv$AlamodeInputDF[3,toString(i)] <- projEnv$MlsCompsDF[i+1,"ProjName"]
      projEnv$AlamodeInputDF[4,toString(i)] <- projEnv$MlsCompsDF[i+1,"Phase"]
       projEnv$AlamodeInputDF[5,str_c(toString(i),"b")] <- projEnv$MlsCompsDF[i+1,"SalePrice"]
       projEnv$AlamodeInputDF[7,toString(i)] <- str_c("MLSL ",projEnv$MlsCompsDF[i+1,"MlsNbr"],";DOM ",projEnv$MlsCompsDF[i+1,"DOM"])
       projEnv$AlamodeInputDF[8,toString(i)] <- str_c("Realist/Doc ",projEnv$MlsCompsDF[i+1,"Doc"] )
       projEnv$AlamodeInputDF[10,toString(i)] <- projEnv$MlsCompsDF[i+1,"SaleConc1"]
       projEnv$AlamodeInputDF[11,toString(i)] <- projEnv$MlsCompsDF[i+1,"SaleConc2"]

       saleDate  <-  projEnv$MlsCompsDF[i+1, "SaleDate"]
       closeDate <- projEnv$MlsCompsDF[i+1, "CloseDate"]

       saleMonth <-str_pad(month(as.POSIXlt(saleDate,format="%m/%d/%Y")),2,pad="0")
       closeMonth <-str_pad(month(as.POSIXlt(closeDate,format="%m/%d/%Y")),2,pad="0")
       saleYear <-str_pad(year(as.POSIXlt(saleDate,format="%m/%d/%Y"))-2000,2,pad="0")
       closeYear <-str_pad(year(as.POSIXlt(closeDate,format="%m/%d/%Y"))-2000,2,pad="0")
       dot <- str_c("s",closeMonth,"/",closeYear,";c",saleMonth,"/",saleYear)

       projEnv$AlamodeInputDF[12,toString(i)] <- dot
       projEnv$AlamodeInputDF[12,str_c(toString(i),"b")] <- round(projEnv$MlsCompsDF[i+1,"URAR_Date_Of_Sale"],digits=0)
       projEnv$AlamodeInputDF[13,toString(i)] <- str_c(projEnv$MlsCompsDF[i+1,"LocNBA"],";",projEnv$MlsCompsDF[i+1,"LocDsc"] )
       projEnv$AlamodeInputDF[13,str_c(toString(i),"b")] <- round(projEnv$MlsCompsDF[i+1,"URAR_Location"]  ,digits=0)
       projEnv$AlamodeInputDF[14,toString(i)] <- "Fee Simple"

       projEnv$AlamodeInputDF[15,toString(i)] <- str_c(projEnv$MlsCompsDF[i+1,"HOAFee"],"" )
       projEnv$AlamodeInputDF[15,str_c(toString(i),"b")] <- str_c(projEnv$MlsCompsDF[i+1,"Other.Fees"],"" )
       projEnv$AlamodeInputDF[16,toString(i)] <- str_c(projEnv$MlsCompsDF[i+1,"CommonElem"],"" )
       projEnv$AlamodeInputDF[17,toString(i)] <- str_c(projEnv$MlsCompsDF[i+1,"Recreation"],"" )
       projEnv$AlamodeInputDF[18,toString(i)] <-  projEnv$MlsCompsDF[i+1,"Floor"]
       projEnv$AlamodeInputDF[19,toString(i)] <- str_c(projEnv$MlsCompsDF[i+1,"ViewNBA"] )
       projEnv$AlamodeInputDF[19,str_c(toString(i),"b")] <- round(projEnv$MlsCompsDF[i+1,"CQA_V"] * projEnv$MlsCompsDF[i+1,"URAR_CQA"],digits=0)
       projEnv$AlamodeInputDF[20,toString(i)] <- str_c(projEnv$MlsCompsDF[i+1,"Style"])
       projEnv$AlamodeInputDF[20,str_c(toString(i),"b")] <- round( projEnv$MlsCompsDF[i+1,"CQA_D"] * projEnv$MlsCompsDF[i+1,"URAR_CQA"],digits=0)
       projEnv$AlamodeInputDF[21,toString(i)] <- projEnv$MlsCompsDF[i+1,"Qual"]
       projEnv$AlamodeInputDF[21,str_c(toString(i),"b")] <- round(projEnv$MlsCompsDF[i+1,"CQA_Q"] * projEnv$MlsCompsDF[i+1,"URAR_CQA"],digits=0)
       projEnv$AlamodeInputDF[22,toString(i)] <- projEnv$MlsCompsDF[i+1,"Age"]
       projEnv$AlamodeInputDF[22,str_c(toString(i),"b")] <- round(projEnv$MlsCompsDF[i+1,"URAR_ActualAge"],digits=0)
       projEnv$AlamodeInputDF[23,toString(i)] <- projEnv$MlsCompsDF[i+1,"Cond"]
       projEnv$AlamodeInputDF[23,str_c(toString(i),"b")] <- round(projEnv$MlsCompsDF[i+1,"CQA_C"] * projEnv$MlsCompsDF[i+1,"URAR_CQA"],digits=0)
       projEnv$AlamodeInputDF[24,toString(i)] <- projEnv$MlsCompsDF[i+1,"Total"]
       projEnv$AlamodeInputDF[25,toString(i)] <- projEnv$MlsCompsDF[i+1,"Beds"]
       projEnv$AlamodeInputDF[26,toString(i)] <- projEnv$MlsCompsDF[i+1,"Baths"]
       projEnv$AlamodeInputDF[26,str_c(toString(i),"b")] <- round(projEnv$MlsCompsDF[i+1,"URAR_RoomCount"],digits=0)

       projEnv$AlamodeInputDF[27,toString(i)] <- projEnv$MlsCompsDF[i+1,"GLA"]
       projEnv$AlamodeInputDF[27,str_c(toString(i),"b")] <- round(projEnv$MlsCompsDF[i+1,"URAR_GLA"],digits=0)
       projEnv$AlamodeInputDF[30,toString(i)] <- projEnv$MlsCompsDF[i+1,"FU"]
       projEnv$AlamodeInputDF[30,str_c(toString(i),"b")] <- round(projEnv$MlsCompsDF[i+1,"CQA_FU"] * projEnv$MlsCompsDF[i+1,"URAR_CQA"],digits=0)
       projEnv$AlamodeInputDF[31,toString(i)] <- projEnv$MlsCompsDF[i+1,"Heat"]
       projEnv$AlamodeInputDF[32,toString(i)] <- projEnv$MlsCompsDF[i+1,"Energy"]
       projEnv$AlamodeInputDF[33,toString(i)] <- projEnv$MlsCompsDF[i+1,"GarDscGaGdGbiDw"]
       projEnv$AlamodeInputDF[33,str_c(toString(i),"b")] <- round(projEnv$MlsCompsDF[i+1,"URAR_Garage"],digits=0)
       projEnv$AlamodeInputDF[37,toString(i)] <- str_c("CQA",projEnv$MlsCompsDF[i+1,"CQA"],":$",round(projEnv$MlsCompsDF[i+1,"URAR_CQA"],digits=0))
      #

   }
    # Transfer City Addresses CityStateZip 2:10   -> B3,C3,E3,G3,H3,I3,K3,M3,O3,Q3,S3,V3
    #                                     SUbj,1,2,3,4,5,6,7,8,9
    # Transfer Sale Prices  SalePrice 2:10


    write_xlsx(projEnv$AlamodeInputDF, projEnv$AlamodeInputFile, col_names=TRUE,format_headers=TRUE,use_zip64=FALSE)

}
