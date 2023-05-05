################################################   STAGE II ##########################################
# Author:       Wm. Bert Craytor
# Location:     242 Clifton Rd., Pacifica, CA 94044, USA
# Date:         08/12/2021
# Description:  Stage II script to generate MARS analysis for appraisal data using R:earth\
#
# Notes:        1.  This program is free software; you can redistribute it and/or modify
#                   it under the terms of the GNU General Public License as published by
#                   the Free Software Foundation; either version 2 of the License, or
#                   (at your option) any later version.
#
#               2.  This program is distributed in the hope that it will be useful,
#                   but WITHOUT ANY WARRANTY; without even the implied warranty of
#                   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#                   GNU General Public License for more details.
#
#              3.   A copy of the GNU General Public License is available at
#                   http://www.r-project.org/Licenses
#
######################################################################################################
#'  Stage3
#'  This program reads in the MlsData
#'  @return     None
#'
#'  @example
#'  Stage_3()
#'
Stage_3 <- function( ) {
  flog.info("###################   STAGE III   ######################",name="Log2File")

  #  Create an intermediate Excel spreadsheet MlsData.xlsx to manually breakdown the residual into unmeasured feature value contributions 
  #  such as Condition, Quality, Design, Functional Utility.  Also add some default UAD compliant values for Condition, Quality, Funtional Utility, etc.
  
  # Read in previous spreadsheet.
  outputFileName <- projEnv$MlsFileStageIIb
  projEnv$MlsDataDF <- readxl::read_excel(outputFileName,sheet="Sheet1")
  
  # Copy MlsDataDf to SGPrepDF
  projEnv$SGPrepDF <- projEnv$MlsDataDF
  
  SGPrepDF <- head(  projEnv$SGPrepDF,13)
  ##names(proprojEnv$DjEnv$SGPrepDF)[names(projEnv$SGPrepDF)=="NoOfStories"] <- "Stories"
  newColumns <- projEnv$SGPrepDF[,c("Comp","MlsNbr","Address")]
  newColumns$CityStateZip <- c(0)
  newColumns <- cbind(newColumns, projEnv$SGPrepDF["CQA"])
  newColumns$Doc <- c(0)
  newColumns$CQA_Q <- c(0)
  newColumns$CQA_C <- c(0)
  newColumns$CQA_V <- c(0)
  newColumns$CQA_D <- c(0)
  newColumns$CQA_FU <- c(0)
  newColumns$SaleConc1 <- c(0)
  newColumns$SaleConc2 <- c(0)
  newColumns$LocNBA <- c(0)
  newColumns$LocDsc <- c(0)
  newColumns$ViewNBA <- c(0)
  newColumns$AttDet <- c(0)

  newColumns <- cbind(newColumns,projEnv$SGPrepDF["Story"])
  newColumns$Style <- c(0)
  newColumns$Qual <- c(0)
  newColumns$Cond <- c(0)
  newColumns$FU <- c(0)
  newColumns$Heat <- c(0)
  newColumns$Energy <- c(0)
  newColumns$GarDscGaGdGbiDw <- c(0)
 
  # Adding existing columns in two step x1, x2
  x1<- projEnv$SGPrepDF[,c("SalePrice","SaleDate","GLA")]
  newColumns <- cbind(newColumns,x1)
  newColumns$Total <- c(0)

  
  x2 <-projEnv$SGPrepDF[c("Beds","Baths","BathsFull","BathsHalf","Age","FrPlcNbr","Garage","DaysOffMkt", "PoolYN", "Longitude",
  "Concessions", "ContractSP","Proximity", "HazScore","Earthquake","Liquifaction","Landslide",   "Latitude","LotSize", "CloseDate", "AreaNbr","Kitchen","Bathroom","PropertyType", "Fireplaces","PropertySubType",
              "Bedrooms", "ListPrice","ListingDate","Zoning","Status","ParcelNbr","County","Carport","Parking","BGLA","ULA","GarageSF") ]
 
  newColumns <- cbind(newColumns,x2)
  
  nn <- ncol(projEnv$SGPrepDF)
  nEstimate <- which(colnames(projEnv$SGPrepDF)=="Estimate")
  x1 <- projEnv$SGPrepDF[c(nEstimate:nn)]
  projEnv$SGPrepDF <- cbind(newColumns,x1)

  nr <- nrow(projEnv$SGPrepDF)
  for(i in 1:nr) {

    projEnv$SGPrepDF[i,"SaleConc1"] <- "ArmLth"
    projEnv$SGPrepDF[i,"SaleConc2"] <- "Conv;0"
    projEnv$SGPrepDF[i,"LocNBA"] <- "N"
    projEnv$SGPrepDF[i,"LocDsc"] <- "Res"
    projEnv$SGPrepDF[i,"ViewNBA"] <- "N;Res"
    projEnv$SGPrepDF[i,"AttDet"] <- "DT"
    projEnv$SGPrepDF[i,"Style"] <- "Contemp"
    projEnv$SGPrepDF[i,"Qual"] <- "C4"
    projEnv$SGPrepDF[i,"Cond"] <- "C4"
    projEnv$SGPrepDF[i,"FU"] <- "Average"
    projEnv$SGPrepDF[i,"Heat"] <- "FAU/NoAC"
    projEnv$SGPrepDF[i,"Energy"] <- "DPW"
    projEnv$SGPrepDF[i,"GarDscGaGdGbiDw"] <- "2gbi2dw"
    projEnv$SGPrepDF[i,"CityStateZip"] =str_c(projEnv$City,",CA"," " ,projEnv$Zip)
  }


  # Let's write SGPrepDF to SGPrepData.xlsx
  # This file is to be manually updated for pasting into the Alamode SG spreadsheet import
  mlsDataFileName <- str_c(projEnv$MlsVersionFolder,"/SGPrepData.xlsx")
  flog.info(paste("Output file:  ",mlsDataFileName,sep=""),name="Log2File")
  write_xlsx(projEnv$SGPrepDF, mlsDataFileName, col_names=TRUE,format_headers=TRUE,use_zip64=FALSE)

  #  Now let's create the Stats.xlsx spreadhsset  with sheets Stats, projEnv$AgMap, AdjGrid, Calculations - which can be used
  #  in the report to explain aggregation and calculations
  
  nr = length(projEnv$ListModel)
  projEnv$StatDF <- data.frame(matrix(NA,nrow=nr,ncol=3))
  colnames(projEnv$StatDF ) <- c("Variables", "Func", "Desc")
  projEnv$StatDF$FuncName <- c(0)
  projEnv$StatDF$Func <- c(0)
  projEnv$StatDF$Desc <- c("")
  lmv <- projEnv$LmvOrig
  lmv <- insert(lmv,ats=1,value="Base")
  
  for(i in 1:nr) {
    x <- projEnv$ListModel[i]
    x <- str_replace(gsub("\\s+"," ",str_trim(x)),"B","b")
    projEnv$StatDF$Func[i] <- x
    projEnv$StatDF$Variables[i] <- as.character(lmv[i])
  }

  
  wb <- createWorkbook()
  addWorksheet(wb,"Stats")
  writeData(wb,"Stats",projEnv$StatDF)
  addWorksheet(wb,"projEnv$AgMap")
  writeData(wb,"projEnv$AgMap",projEnv$AgMap)
  addWorksheet(wb,"AdjGrid")
  writeData(wb,"AdjGrid",projEnv$AdjGrid )
  addWorksheet(wb,"Calculations")

  modifyBaseFont(wb, fontSize = 11, fontColour = "darkblue", fontName = "Futura LtCn BT")
  setColWidths(wb,"Stats",1:4,widths="auto")
  setColWidths(wb,"projEnv$AgMap",1:4,widths="auto")
  setColWidths(wb,"AdjGrid",1:4,widths="auto")
  setColWidths(wb,"Calculations",1:4,widths="auto")
  mlsStatsFileName <- str_c(projEnv$MlsVersionFolder,"/Stats.xlsx")
  nrows <- nrow(projEnv$MlsDataDF)
  adjGrid1 <- projEnv$AdjGrid
  adjRow <- 2
  nlmv <- length(lmv)
  maxRows <- 18 + nrows * nlmv

  displayCalcs <-  data.frame(Comp="",SGField="",ValueName="",Value=rep(0,maxRows))
  dispIdx <- 0

  for(comp in 1:12) {
    flog.info(paste("COMP ", comp),name="Log2File")
    lastSGName <- ""

    while(adjGrid1$Comp[adjRow] == comp) {

      flog.info(paste("adjRow: ",adjRow," lastComp: ", comp),name="Log2File")
      flog.info(paste("Comp ",comp," SG Field ", adjGrid1$Address[adjRow]),name="Log2File")
      dispIdx <- dispIdx + 1
      displayCalcs$Comp[dispIdx] <- paste("Comp ", comp," Address: ", adjGrid1$Address[adjRow])
      displayCalcs$Value[dispIdx] <- ""

      while(adjGrid1$Comp[adjRow] == comp) {
        if(adjGrid1$SGName[adjRow] != lastSGName && adjGrid1$SGName[adjRow] != "") {

          lastSGName <- adjGrid1$SGName[adjRow]
          dispIdx <- dispIdx + 1
          displayCalcs$SGField[dispIdx] <-   adjGrid1$SGName[adjRow]
          displayCalcs$Value[dispIdx] <- ""

        }
        SGTotal <- 0.0
        while(adjGrid1$SGName[adjRow] == lastSGName) {
          flog.info(paste("U1: ", adjGrid1$SGName[adjRow]," U2: ",lastSGName),name="Log2File")
          if(is.numeric(adjGrid1$AdjValue[adjRow]) && abs(adjGrid1$AdjValue[adjRow]) > 0.0001) {
            dispIdx <- dispIdx + 1
            displayCalcs$ValueName[dispIdx] <- paste(   adjGrid1$AdjName[adjRow])
            displayCalcs$Value[dispIdx] <- adjGrid1$AdjValue[adjRow]
            flog.info(paste("Comp: ",adjGrid1$Comp[adjRow], "  ValueName: ",adjGrid1$AdjName[adjRow]," Value: ",adjGrid1$AdjValue[adjRow]),name="Log2File")
            SGTotal <- adjGrid1$SGValue[adjRow]   # Get latest
            flog.info("T1",name="Log2File")
          }
          adjRow <- adjRow+1
            flog.info("T1a",name="Log2File")
        }
        dispIdx <- dispIdx+1
           flog.info("T2",name="Log2File")
        displayCalcs$ValueName[dispIdx] <- "   Total: "
           flog.info("T3",name="Log2File")
        displayCalcs$Value[dispIdx] <- SGTotal
           flog.info("T4",name="Log2File")

      }
    }

  }

  writeData(wb,"Calculations",displayCalcs)
  saveWorkbook(wb,mlsStatsFileName,overwrite=TRUE)
  
file.copy(from=projEnv$CodeFolder,to=projEnv$MlsVersionFolder,,   recursive=TRUE,copy.date=TRUE, copy.mode=TRUE)

}



