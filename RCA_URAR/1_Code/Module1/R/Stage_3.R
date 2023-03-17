################################################   STAGE II ##########################################
#' Author:       Wm. Bert Craytor
#' Location:     242 Clifton Rd., Pacifica, CA 94044, USA
#' License:      MIT License
#' Date:         08/12/2021
#' Description:  Stage II script to generate MARS analysis for appraisal data using R:earth\
#'
#' Notes:        1.  This program is free software; you can redistribute it and/or modify
#'                   it under the terms of the GNU General Public License as published by
#'                   the Free Software Foundation; either version 2 of the License, or
#'                   (at your option) any later version.
#'
#'              2.  This program is distributed in the hope that it will be useful,
#'                   but WITHOUT ANY WARRANTY; without even the implied warranty of
#'                   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#'                   GNU General Public License for more details.
#'
#'             3.   A copy of the GNU General Public License is available at
#'                   http://www.r-project.org/Licenses
#'
######################################################################################################
#'  Stage3
#'   
#'  @return     None
#'
#'  @example
#'  Stage_3()
#'
Stage_3 <- function( ) {
#
  flog.info("###################   STAGE III   ######################",name="Log2File")

  #  Create an intermediate Excel spreadsheet MlsData.xlsx to manually breakdown the residual into unmeasured feature value contributions 
  #  such as Condition, Quality, Design, Functional Utility.  Also add some default UAD compliant values for Condition, Quality, Funtional Utility, etc.
  
  # Read in previous spreadsheet.
  outputFileName <- projEnv$MlsFileStageIIb
  projEnv$MlsDataDF <- readxl::read_excel(outputFileName,sheet="Sheet1")
  
  # Copy MlsDataDf to UrarPrepDF
  projEnv$UrarPrepDF <- projEnv$MlsDataDF
  
  UrarPrepDF <- head(  projEnv$UrarPrepDF,13)
  ##names(proprojEnv$DjEnv$UrarPrepDF)[names(projEnv$UrarPrepDF)=="NoOfStories"] <- "Stories"
  newColumns <- projEnv$UrarPrepDF[,c("Comp","MlsNbr","Address")]
  newColumns$CityStateZip <- c(0)
  newColumns <- cbind(newColumns, projEnv$UrarPrepDF["CQA"])
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

  newColumns <- cbind(newColumns,projEnv$UrarPrepDF["Story"])
  newColumns$Style <- c(0)
  newColumns$Qual <- c(0)
  newColumns$Cond <- c(0)
  newColumns$FU <- c(0)
  newColumns$Heat <- c(0)
  newColumns$Energy <- c(0)
  newColumns$GarDscGaGdGbiDw <- c(0)
 
  # Adding existing columns in two step x1, x2
  x1<- projEnv$UrarPrepDF[,c("SalePrice","SaleDate","GLA")]
  newColumns <- cbind(newColumns,x1)
  newColumns$Total <- c(0)

  
  x2 <-projEnv$UrarPrepDF[c("Beds","Baths","BathsFull","BathsHalf","Age","FrPlcNbr","Garage","DaysOffMkt", "PoolYN", "Longitude",
              "Latitude", "CloseDate", "AreaNbr","Kitchen","Bathroom","PropertyType", "Fireplaces","PropertySubType",
              "Bedrooms", "ListPrice","ListingDate","Zoning","Status","ParcelNbr","County","Carport","Parking","OceanView","Hwy101","GolfCourse","Attached" ) ]
 
  newColumns <- cbind(newColumns,x2)
  
  nn <- ncol(projEnv$UrarPrepDF)
  nEstimate <- which(colnames(projEnv$UrarPrepDF)=="Estimate")
  x1 <- projEnv$UrarPrepDF[c(nEstimate:nn)]
  projEnv$UrarPrepDF <- cbind(newColumns,x1)

  nr <- nrow(projEnv$UrarPrepDF)
  for(i in 1:nr) {

    projEnv$UrarPrepDF[i,"SaleConc1"] <- "ArmLth"
    projEnv$UrarPrepDF[i,"SaleConc2"] <- "Conv;0"
    projEnv$UrarPrepDF[i,"LocNBA"] <- "N"
    projEnv$UrarPrepDF[i,"LocDsc"] <- "Res"
    projEnv$UrarPrepDF[i,"ViewNBA"] <- "N;Res"
    projEnv$UrarPrepDF[i,"AttDet"] <- "DT"
    projEnv$UrarPrepDF[i,"Style"] <- "Contemp"
    projEnv$UrarPrepDF[i,"Qual"] <- "C4"
    projEnv$UrarPrepDF[i,"Cond"] <- "C4"
    projEnv$UrarPrepDF[i,"FU"] <- "Average"
    projEnv$UrarPrepDF[i,"Heat"] <- "FAU/NoAC"
    projEnv$UrarPrepDF[i,"Energy"] <- "DPW"
    projEnv$UrarPrepDF[i,"GarDscGaGdGbiDw"] <- "2gbi2dw"
    projEnv$UrarPrepDF[i,"CityStateZip"] =str_c(projEnv$City,",CA"," " ,projEnv$Zip)
  }


  # Let's write UrarPrepDF to UrarPrepData.xlsx
  # This file is to be manually updated for pasting into the Alamode URAR spreadsheet import
  mlsDataFileName <- str_c(projEnv$MlsVersionFolder,"/UrarPrepData.xlsx")
  flog.info(paste("Output file:  ",mlsDataFileName,sep=""),name="Log2File")
  write_xlsx(projEnv$UrarPrepDF, mlsDataFileName, col_names=TRUE,format_headers=TRUE,use_zip64=FALSE)

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

  displayCalcs <-  data.frame(Comp="",URARField="",ValueName="",Value=rep(0,maxRows))
  dispIdx <- 0

  for(comp in 1:12) {
    flog.info(paste("COMP ", comp),name="Log2File")
    lastURARName <- ""

    while(adjGrid1$Comp[adjRow] == comp) {

      flog.info(paste("adjRow: ",adjRow," lastComp: ", comp),name="Log2File")
      flog.info(paste("Comp ",comp," URAR Field ", adjGrid1$Address[adjRow]),name="Log2File")
      dispIdx <- dispIdx + 1
      displayCalcs$Comp[dispIdx] <- paste("Comp ", comp," Address: ", adjGrid1$Address[adjRow])
      displayCalcs$Value[dispIdx] <- ""

      while(adjGrid1$Comp[adjRow] == comp) {
        if(adjGrid1$URARName[adjRow] != lastURARName && adjGrid1$URARName[adjRow] != "") {

          lastURARName <- adjGrid1$URARName[adjRow]
          dispIdx <- dispIdx + 1
          displayCalcs$URARField[dispIdx] <-   adjGrid1$URARName[adjRow]
          displayCalcs$Value[dispIdx] <- ""

        }
        urarTotal <- 0.0
        while(adjGrid1$URARName[adjRow] == lastURARName) {
          flog.info(paste("U1: ", adjGrid1$URARName[adjRow]," U2: ",lastURARName),name="Log2File")
          if(is.numeric(adjGrid1$AdjValue[adjRow]) && abs(adjGrid1$AdjValue[adjRow]) > 0.0001) {
            dispIdx <- dispIdx + 1
            displayCalcs$ValueName[dispIdx] <- paste(   adjGrid1$AdjName[adjRow])
            displayCalcs$Value[dispIdx] <- adjGrid1$AdjValue[adjRow]
            flog.info(paste("Comp: ",adjGrid1$Comp[adjRow], "  ValueName: ",adjGrid1$AdjName[adjRow]," Value: ",adjGrid1$AdjValue[adjRow]),name="Log2File")
            urarTotal <- adjGrid1$URARValue[adjRow]   # Get latest
            flog.info("T1",name="Log2File")
          }
          adjRow <- adjRow+1
            flog.info("T1a",name="Log2File")
        }
        dispIdx <- dispIdx+1
           flog.info("T2",name="Log2File")
        displayCalcs$ValueName[dispIdx] <- "   Total: "
           flog.info("T3",name="Log2File")
        displayCalcs$Value[dispIdx] <- urarTotal
           flog.info("T4",name="Log2File")

      }
    }

  }

  writeData(wb,"Calculations",displayCalcs)
  saveWorkbook(wb,mlsStatsFileName,overwrite=TRUE)
  file.copy(from=mlsStatsFileName,to=
  file.copy(from=projEnv$CodeFolder,to=projEnv$MlsVersionFolder,,   recursive=TRUE,copy.date=TRUE, copy.mode=TRUE)

}



