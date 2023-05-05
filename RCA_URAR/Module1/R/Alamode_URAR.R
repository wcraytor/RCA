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
# Purpose:  You need to:
#            1.  Make a copy of the MlsData.xlsx file in your version output folder to "MlsComps.xlsx".  Then rename "Sheet1" to "X". 
#            2.  Then rename "Sheet1" to "X".  - As you may wind-up making a lot of other worksheets for other calcalations.  The "X" sheet is to create the 
#                "AlamodeInput" worksheet for input to the Alamode URAR form, using Alamode's worksheet feature.
#            3.  The first part of the RunModul2.R script is almost the same as the first part of the RunModule1.R script.  Basically it set sup
#                the proj
#
#
#
#
#
#
#
#
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
library(openxlsx)
#'  @return  Project Environment
LoadCompsToDataFrame2 <- function() {

  print("LoadCompsToDataFrame")
   
 
  #cat(projEnv$SGPrepDataFile)
	
   

  projEnv$MlsCompsDF <-
    read.xlsx(projEnv$SGPrepDataFile,
              sheet = "Sheet1" ,
              detectDates = TRUE)
  

  # Assign the variables in the "Project" sheet to projEnv variables of same name, with the associated values in that
  # spreadsheet

  #projEnv$ProjectID <- projEnv$ProjectrDF$ProjectID

  projEnv$AlamodeInputDF <-
    read.xlsx(projEnv$AlamodeInputTemplateFile,
              sheet = "Sheet1" ,
              detectDates = TRUE)

 
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

 
#' We need to convert Data Frame Dates to Char/string for the upload to SQLite to work correctly
#'  @projEnv Name vector
#'  @return  Name vector
ConvertDatesToChar <- function( df) {
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
        as.character(projEnv$AlamodeInputDF[, i], format = "%m/%d/%Y")
      df[, i] <- asChar
      print(df[, i])
    }
  }
   
}

#' We need to convert Data Frame Dates to Char/string for the upload to SQLite to work correctly
#'  @projEnv Project Environment
#'  @return  Project Environment
# ConvertDatesToCharAllDF <- function( ) {
  # print("ConvertDatesToChar alamodeInputDF")
   # ConvertDatesToChar(projEnv$AlamodeInputDF)
    # ConvertDatesToChar( projEnv$MlsCompsDF)
  
# }

#' Initial call is here to set up the environment under projEnv
#'  @projEnv Project Environment
#'  @codeFolder codeFolder,
#'  @projectParentFolder Project files parent folder
#'  @projectID Project ID
#'  @mlsComps MLS Data File
#'  @alamodeInput Alamode Input File
#' Initial call is here to set up the environment under projEnv
#' #export
SetUpAlamodeEnvironment <-
  function(    ) {

    print("SetUpAlamodeEnvironment")


    projEnv$MlsCompsDF <- data.frame()
    projEnv$AlamodeInputDF <- data.frame()

     
  }

ColumnExists2 <- function(r,nm) {

 a <- ""
 if(nm %in% colnames(projEnv$MlsCompsDF) )
 { 
    a <- projEnv$MlsCompsDF[r,nm] 
	
 }  
 return (a)

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
#'dev
#' Out main Stage I function:
Alamode_URAR <-
  function( ) {
    ##  !!!!!!   YOU need to set the Version Folder int RunModuleto Use for the Import (Usually the last one created)

    SetUpAlamodeEnvironment(  )
    dummy <- LoadCompsToDataFrame2( )

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
    projEnv$AlamodeInputDF[9,"Subj"] <- ColumnExists2(1,"SaleConc1") 
    projEnv$AlamodeInputDF[10,"Subj"] <- ColumnExists2(1,"SaleConc2") 
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
    projEnv$AlamodeInputDF[34,"Subj"] <- str_c("CQA",projEnv$MlsCompsDF[1,"CQA"],":$",round(projEnv$MlsCompsDF[1,"SG_CQA"],digits=0))

    for(i in 1:9) {
      print(paste("i:",i))
      
      projEnv$AlamodeInputDF[1,toString(i)] <- projEnv$MlsCompsDF[i+1,"Address"]
      print(paste("ia:",i))
      projEnv$AlamodeInputDF[2,toString(i)] <- projEnv$MlsCompsDF[i+1,"CityStateZip"]
      print(paste("ib:",i))
      projEnv$AlamodeInputDF[4,str_c(toString(i),"b")] <- projEnv$MlsCompsDF[i+1,"SalePrice"]
      print(paste("ic:",i))
      projEnv$AlamodeInputDF[6,toString(i)] <- str_c("MLSL #",projEnv$MlsCompsDF[i+1,"MlsNbr"],";DOM ",projEnv$MlsCompsDF[i+1,"DOM"])
      print(paste("id:",i))
      projEnv$AlamodeInputDF[7,toString(i)] <- str_c("Realist/Doc #",projEnv$MlsCompsDF[i+1,"Doc"] )
      print(paste("id:",i))
      projEnv$AlamodeInputDF[9,toString(i)] <- projEnv$MlsCompsDF[i+1,"SaleConc1"]
      print(paste("ide:",i))
      projEnv$AlamodeInputDF[10,toString(i)] <- ColumnExists2(i+1,"SaleConc2") 
      print(paste("idf:",i))
      saleDate  <-  projEnv$MlsCompsDF[i+1, "SaleDate"]
      print(paste("idg:",i))
      closeDate <- projEnv$MlsCompsDF[i+1, "CloseDate"]
      print(paste("idh:",i))
      saleMonth <-str_pad(month(as.POSIXlt(as.Date(saleDate,origin="1899-12-31"),format="%m/%d/%Y") ),width=2,pad="0")
      print(paste("idi:",i))
      closeMonth <-str_pad(month(as.POSIXlt(as.Date(closeDate,origin="1899-12-31"),format="%m/%d/%Y")),width=2,pad="0")
      print(paste("id:",i))
      saleYear <-str_pad(year(as.POSIXlt(as.Date(saleDate,origin="1899-12-31"),format="%m/%d/%Y"))-2000,width=2,pad="0")
      print(paste("idj:",i))
      closeYear <-str_pad(year(as.POSIXlt(as.Date(closeDate,origin="1899-12-31"),format="%m/%d/%Y"))-2000,width=2,pad="0")
      print(paste("idk:",i))
      dot <- str_c("s",closeMonth,"/",closeYear,";c",saleMonth,"/",saleYear)
      print(paste("idl:",i))
      projEnv$AlamodeInputDF[11,toString(i)] <- dot
      projEnv$AlamodeInputDF[11,str_c(toString(i),"b")] <- round(projEnv$MlsCompsDF[i+1,"SG_Date_Of_Sale"],digits=0)
      projEnv$AlamodeInputDF[12,toString(i)] <- str_c(projEnv$MlsCompsDF[i+1,"LocNBA"],";",projEnv$MlsCompsDF[i+1,"LocDsc"] )
      projEnv$AlamodeInputDF[12,str_c(toString(i),"b")] <- round(projEnv$MlsCompsDF[i+1,"SG_Location"]  ,digits=0)
      projEnv$AlamodeInputDF[13,toString(i)] <- "Fee Simple"
      print(paste("idm:",i))
      projEnv$AlamodeInputDF[14,toString(i)] <- str_c(projEnv$MlsCompsDF[i+1,"LotSize"]," sf" )
      projEnv$AlamodeInputDF[14,str_c(toString(i),"b")] <-  projEnv$MlsCompsDF[i+1,"SG_Site"]
      projEnv$AlamodeInputDF[15,toString(i)] <- str_c(projEnv$MlsCompsDF[i+1,"ViewNBA"] )
      print(paste("ido:",i))
      projEnv$AlamodeInputDF[15,str_c(toString(i),"b")] <- round(projEnv$MlsCompsDF[i+1,"CQA_V"] * projEnv$MlsCompsDF[i+1,"SG_CQA"] ,digits=0)
      print(paste("ido1:",i))
       projEnv$AlamodeInputDF[16,toString(i)] <- str_c(projEnv$MlsCompsDF[i+1,"AttDet"], projEnv$MlsCompsDF[i+1,"Story"] ,";",projEnv$MlsCompsDF[i+1,"Style"])
      print(paste("idp:",i))
        projEnv$AlamodeInputDF[16,str_c(toString(i),"b")] <- round( projEnv$MlsCompsDF[i+1,"CQA_D"] * projEnv$MlsCompsDF[i+1,"SG_CQA"],digits=0)
      projEnv$AlamodeInputDF[17,toString(i)] <- projEnv$MlsCompsDF[i+1,"Qual"]
      print(paste("i: ",i))
      projEnv$AlamodeInputDF[17,str_c(toString(i),"b")] <- round(projEnv$MlsCompsDF[i+1,"CQA_Q"] * projEnv$MlsCompsDF[i+1,"SG_CQA"],digits=0)
      projEnv$AlamodeInputDF[18,toString(i)] <- projEnv$MlsCompsDF[i+1,"Age"]
      projEnv$AlamodeInputDF[18,str_c(toString(i),"b")] <- round(projEnv$MlsCompsDF[i+1,"SG_ActualAge"],digits=0)
      projEnv$AlamodeInputDF[19,toString(i)] <- projEnv$MlsCompsDF[i+1,"Cond"]
      projEnv$AlamodeInputDF[19,str_c(toString(i),"b")] <- round(projEnv$MlsCompsDF[i+1,"CQA_C"] * projEnv$MlsCompsDF[i+1,"SG_CQA"],digits=0)
      projEnv$AlamodeInputDF[21,toString(i)] <- projEnv$MlsCompsDF[i+1,"Total"]
      projEnv$AlamodeInputDF[22,toString(i)] <- projEnv$MlsCompsDF[i+1,"Beds"]
      projEnv$AlamodeInputDF[23,toString(i)] <- projEnv$MlsCompsDF[i+1,"Baths"]
      projEnv$AlamodeInputDF[23,str_c(toString(i),"b")] <- round(projEnv$MlsCompsDF[i+1,"SG_RoomCount"],digits=0)

      projEnv$AlamodeInputDF[24,toString(i)] <- projEnv$MlsCompsDF[i+1,"GLA"]
      projEnv$AlamodeInputDF[24,str_c(toString(i),"b")] <- round(projEnv$MlsCompsDF[i+1,"SG_GLA"],digits=0)
      projEnv$AlamodeInputDF[27,toString(i)] <- projEnv$MlsCompsDF[i+1,"FU"]
      projEnv$AlamodeInputDF[27,str_c(toString(i),"b")] <- round(projEnv$MlsCompsDF[i+1,"CQA_FU"] * projEnv$MlsCompsDF[i+1,"SG_CQA"],digits=0)
      projEnv$AlamodeInputDF[28,toString(i)] <- projEnv$MlsCompsDF[i+1,"Heat"]
      projEnv$AlamodeInputDF[29,toString(i)] <- projEnv$MlsCompsDF[i+1,"Energy"]
      projEnv$AlamodeInputDF[30,toString(i)] <- projEnv$MlsCompsDF[i+1,"GarDscGaGdGbiDw"]
      projEnv$AlamodeInputDF[30,str_c(toString(i),"b")] <- round(projEnv$MlsCompsDF[i+1,"SG_Garage"],digits=0)

      projEnv$AlamodeInputDF[33,toString(i)] <- projEnv$MlsCompsDF[i+1,"FrPlcNbr"]
      projEnv$AlamodeInputDF[33,str_c(toString(i),"b")] <- str_c( round(projEnv$MlsCompsDF[i+1,"SG_FrPlcNbr"],digits=0)," Fireplace")

      projEnv$AlamodeInputDF[34,toString(i)] <- str_c("CQA",projEnv$MlsCompsDF[i+1,"CQA"],":$",round(projEnv$MlsCompsDF[i+1,"SG_CQA"],digits=0))
      #projEnv$AlamodeInputDF[34,str_c(toString(i),"b")] <- round( projEnv$MlsCompsDF[i+1,"SG_CQA"],digits=0)

   }
 
    ## Write the AlamodeInput Worksheet to disk
    write_xlsx(projEnv$AlamodeInputDF, projEnv$AlamodeInputFile, col_names=TRUE,format_headers=TRUE,use_zip64=FALSE)
  }


