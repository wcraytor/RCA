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
#'  Stage2
#'
#'  @return     None
#'
#'  @example
#'  Stage_2()
#'
#' Out main Stage II function:
#' #export
Stage_2 <- function(  ) {

  print("###################   STAGE II   ######################")
 

  flog.info( "Load files",name="Log2File")

  # Reopen data file
  outputFileName <- projEnv$MlsFileStageIb
  projEnv$MlsDataDF <- readxl::read_excel(outputFileName,sheet="Sheet1")
   projEnv$MlsDataDF <-
      projEnv$MlsDataDF[order(projEnv$MlsDataDF$Comp), ]
  nrows <- nrow(projEnv$MlsDataDF)

  flog.info( "Find Contribution Variables From Model List",name="Log2File")
  # Find and Add Required Contribution Columns --  Look at Aggregation shseets
  cList <- findContribVars(  projEnv$Mars$modelList)
  nl <- length(cList)
  contribVars <- c()

  flog.info( "Create MARS Model Switches",name="Log2File")
  # Create a list "lmv" of switches for each of the items in listModel
  nm <- length(projEnv$ListModelVars)
  lmv <- projEnv$ListModelVars
  lmvF <- projEnv$ListModelFactors

  # lmvF is only for creating the final model formula with "F" switches to turn on and off
  # certain parts of the model
  # lmv is to allow us to iterate through the contribution variables (for each row)
  for(m in 1:nm) {

    factor <-lmv[[m]][[1]]

    if(is.null(factor)) {
      lmv[m] = ""
      next
    }

    if(length(factor) == 1) {
      lmv[m] <- paste(factor[1],sep="")
      lmvF[m] <- paste(factor[1],"F",sep="")
    } else if(length(factor) == 2) {
      lmv[m] <- paste(factor[1],"_",factor[2],sep="")
      lmvF[m] <- paste(factor[1],"_",factor[2],"F",sep="")
    } else if (length(factor) == 3) {
      lmv[m] <- paste(factor[1],"_",factor[2],"_",factor[3],sep="")
      lmvF[m] <- paste(factor[1],"_",factor[2],"_",factor[3],"F",sep="")
    }
  }

  flog.info( "Eliminate duplicates from model list",name="Log2File")
  # For iterating through the list of contribution variables, we don't need duplicates

  projEnv$LmvOrig <- lmv
  lmv <- unique(lmv)

  flog.info( "Store unique factors in uLmvF",name="Log2File")
  # We will put the unique version of lmvF in uLmvF
  uLmvF <- unique(lmvF)

  nl <- length(lmv)
  #print(paste("lmv: ",lmv))
  flog.info( "Now Add Contribution Columns To Spreadsheet",name="Log2File")

  # Add Contribution Columns to Spreadsheet
  # And initiate columns to zero
  contribVars <- c()
  for(i in 1:nl) {
    valueInitCmd <- ""
    var <-  paste("data$Contrib_",lmv[i],sep="")

    cVar <- paste("add_column(projEnv$MlsDataDF,","Contrib_",lmv[i],"=1:nrows)",sep="")
   print(cVar)
    contribVars <- append(contribVars,var)
    projEnv$MlsDataDF <- eval(parse(text=cVar))
    valueInitCmd <- paste("projEnv$MlsDataDF$Contrib_",lmv[i]," <- rep(0,nrows)",sep="")
    eval(parse(text=valueInitCmd))
  }

  add_column(projEnv$MlsDataDF,"Contrib_CQA=1:nrows")

  flog.info( "Add Calculated Residual",name="Log2File")
  add_column(projEnv$MlsDataDF,"CalculatedResidual=1:nrows")
  projEnv$MlsDataDF$CalculatedResidual = rep(0,nrows)

  # Compute CalculatedResidual from CqaToResidual Map
  for(i in 1:nrows) {
    paste("t ",i)
    projEnv$MlsDataDF$CalculatedResidual[i] <- projEnv$CqaToResidual$Find( projEnv$MlsDataDF$CQA[i])
  }
  projEnv$MlsDataDF$Contrib_CQA <- rep(0,nrows)


  flog.info( "Add Adjustment Columns",name="Log2File")
  # Same thing for Adjustment columns, one per contribution column
  for(i in 1:nl) {
    valueInitCmd <- ""
    var <-  paste("projEnv$MlsDataDF$Adjstmts_",lmv[i],sep="")
    cVar <- paste("add_column(projEnv$MlsDataDF,","Adjstmts_",lmv[i],"=1:",nrows,")",sep="")
    projEnv$MlsDataDF <- eval(parse(text=cVar))
    #print(paste("valueInitCmd:  ",valueInitCmd,sep=""))
  }

  # Initialize Adjustment Columns to zero
  for(i in 1:nl) {
    valueInitCmd <- ""
    valueInitCmd <- paste("projEnv$MlsDataDF$Adjstmts_",lmv[i]," <- rep(0,nrows)",sep="")
    eval(parse(text=valueInitCmd))

  }

  projEnv$MlsDataDF <- add_column(projEnv$MlsDataDF,Adjstmts_CQA=1:nrows)
   projEnv$MlsDataDF <- add_column(projEnv$MlsDataDF,Adjstmts_CQA2=1:nrows)
  print("Add and Initialize Net, Gros Total")
  flog.info( "Add and Initialize Net, Gros Total",name="Log2File")
  projEnv$MlsDataDF <- add_column(projEnv$MlsDataDF,Adjstmts_Net=1:nrows)
  projEnv$MlsDataDF <- add_column(projEnv$MlsDataDF,Adjstmts_Gross=1:nrows)

  projEnv$MlsDataDF <- add_column(projEnv$MlsDataDF,Adjsted_SalePrice=1:nrows)
  projEnv$MlsDataDF$Adjstmts_CQA <- rep(0,nrows)
    projEnv$MlsDataDF$Adjstmts_CQA2 <- rep(0,nrows)
  projEnv$MlsDataDF$Adjstmts_Net <- rep(0,nrows)
  projEnv$MlsDataDF$Adjstmts_Gross <- rep(0,nrows)
  projEnv$MlsDataDF$Adjstmts_Total <- rep(0,nrows)
  projEnv$MlsDataDF$Adjsted_SalePrice <- rep(0,nrows)


  flog.info( "Save Data to StageII",name="Log2File")
  # Save current data file
  outputFileName <- projEnv$MlsFileStageII
  write_xlsx(projEnv$MlsDataDF, outputFileName, col_names=TRUE,format_headers=TRUE,use_zip64=FALSE)

  ################################################  STEP 3  #########################
  # Reopen data file
  outputFileName <- projEnv$MlsFileStageII
  projEnv$MlsDataDF <- readxl::read_excel(outputFileName,sheet="Sheet1")
  projEnv$ListModel <- projEnv$Mars$modelList
  listModelLen <- length(projEnv$ListModel)
  # Attach property feature switches to get contributions by feature
  for(j in 1:listModelLen) {
    if(j>1)  {
      projEnv$ListModel[j] <- paste("*(",projEnv$ListModel[j],")")
      projEnv$ListModel[j] <- paste(lmvF[j-1],projEnv$ListModel[j],sep="")
    } else   {

      projEnv$ListModel[1] <- paste("BasisF*",projEnv$ListModel[1])
    }
  }
  #####################   agMap ########################
  flog.info( "Create Data Frame 'agMap' to Map Adjustments to URAR fields - START",name="Log2File")
  ir <- nrow(projEnv$InteractionAggregationDF)
  nm <- names(projEnv$InteractionAggregationDF[1,])
  agLen <- (ir)*(ir-1)/2

  projEnv$AgMap <- data.frame()
  row <- 0
  for(i in 1:(ir-1)) {
    n1 <- nm[i+1]
    for(j in (i+1):ir) {

      row <- row+1
      n2 <- nm[j+1]
      cell <- projEnv$InteractionAggregationDF[i,j+1]
      projEnv$AgMap[row,1] <- paste(n1,"_",n2,sep="")
      projEnv$AgMap[row,2] <- cell

    }
  }
  io <- nrow(projEnv$OneWayAggregationDF)
  for(i in 1:io) {
    row <- row + 1
    projEnv$AgMap[row,1] <- projEnv$OneWayAggregationDF[i,1]
    projEnv$AgMap[row,2] <- projEnv$OneWayAggregationDF[i,2]
  }
  projEnv$AgMap <- projEnv$AgMap[order(projEnv$AgMap$V1),]

  # Find by:  subset(projEnv$AgMap,grepl("LatScaled_LotSize",V1)) gives row (V1,V2)
  flog.info( "Create Data Frame to Map Adjustments to URAR fields - END",name="Log2File")


  # Concatenate the elements of the model list into a single
  # equation:
  listModelLen <- length(projEnv$ListModel)
  modelFunc <- capture.output(cat(projEnv$ListModel,sep="+"))
  #print(modelFunc)

  #print("New Data")
  #print(data)


  #################################### EVALATE CONTRIUBTIONS #########################


  flog.info( "Calculate Contributions",name="Log2File")
  # Evaluate the feature contributions by setting feature flags to turn
  # on and off features contributions in the regression function
  for(r in 1:nrows) {

    # SaleAge <- data$SaleAge[i]
    # LotSize <- data$LotSize[i]
    # GLA <- data$GLA[i]
    # Age <- data$Age[i]
    # Baths <- data$Baths[i]
    # Beds <- data$Beds[i]
    # LotSize <- data$LotSize[i]
    # LongScaled <- data$LongScaled[i]
    # LatScaled <- data$LatScaled[i]
    # Garage <- data$Garage[i]
    # Pool <- data$Pool[i]
    # FrPlc <- data$FrPlc[i]



    # Set switches to 0:
    # e.g. SaleAgeF <- 0
    lmvLen <- length(lmv)
    BasisF <- 0
    # Make list of unique valuesin lmv
    fList <- unique(lmv)
    fListLen <- length(fList)
    print("XX1")
    for(i in 1:lmvLen) {

      equ <- paste(uLmvF[i]," <- 0",sep="")
      print(equ)
      eval(parse(text=equ))
    }
    print("XX2")
    nr <- nrow(projEnv$RegressionFieldsDF)
    # Set variable values, e.g. SaleAge <- data$SaleAge[i]
    for(i in 1:nr) {
      valSrc <- paste("projEnv$MlsDataDF$",projEnv$RegressionFieldsDF[i,1],"[",r,"]",sep="")
      equ <- paste(projEnv$RegressionFieldsDF[i,1]," <- projEnv$MlsDataDF$",projEnv$RegressionFieldsDF[i,1],"[",r,"]",sep="")
      print(equ)
      eval(parse(text=equ))
      #print(valSrc)
      #print(paste(projEnv$RegressionFieldsDF[i,1],"=",eval(parse(text=valSrc))))
    }
    print("XX3")
    # Iterate through flags, we are setting the Contributions
    for(i in 1:lmvLen) {
      #print(paste("i: ",i,uLmvF[i]))
      setOne <- paste(uLmvF[i]," <- 1",sep="")
      print(paste("setOne: ", setOne))
      eval(parse(text=setOne))
      #print(paste("Before: " , uLmvF[i]," = ",eval(parse(text=uLmvF[i]))))
      #print(paste("SaleAge= ",SaleAge))
      #print(paste("GLA= ",GLA))
      #print(modelFunc)
      equ <- paste("projEnv$MlsDataDF$Contrib_",lmv[i],"[",r,"] <- eval(parse(text=modelFunc))",sep="")
      print(equ)
      print("XX3a")
      print(paste("EQU: ",eval(parse(text=modelFunc))))
      print("XX3b")
      eval(parse(text=equ))
      print("XX3c")
      setZero <- paste(uLmvF[i]," <- 0",sep="")
      print("XX3d")
      eval(parse(text=setZero))
      print("XX3f")

      #print(paste("After=", uLmvF[i]," = ",eval(parse(text=uLmvF[i],sep=""))))
    }

    # LotSizeF <- 1
    # data$ContribSite[i] <- eval(parse(text=modelFunc))
    # LotSizeF <- 0
    # GLAF <- 1
    # data$ContribGLA[i] <-  eval(parse(text=modelFunc))
    # GLAF <- 0
    # AgeF <- 1
    # data$ContribAge[i] <- eval(parse(text=modelFunc))
    # AgeF <- 0
    # BathsF <- 1
  }

  outputFileName <- projEnv$MlsFileStageII
  write_xlsx(projEnv$MlsDataDF, outputFileName, col_names=TRUE,format_headers=TRUE,use_zip64=FALSE)

  ############################################# CALCULATE ADJUSTMENTS #################################
  outputFileName <- projEnv$MlsFileStageII
  projEnv$MlsDataDF <- readxl::read_excel(outputFileName,sheet="Sheet1")
  # Calculate Adjustments
  nlmv <- length(lmv)

  for(r in 2:nrows) {
    for(n in 1:nlmv){
      aName = as.character(lmv[[n]])
      print(aName)
      contName <- paste("projEnv$MlsDataDF$Contrib_",aName,"[",r,"]",sep="")
      adjName <- paste("projEnv$MlsDataDF$Adjstmts_",aName,"[",r,"]",sep="")
      subjName <- paste("projEnv$MlsDataDF$Contrib_",aName,"[",1,"]",sep="")
      print(paste("contName: ",contName," subjName: ",subjName," adjName: ",adjName))
      calc <- paste(adjName," <- ", subjName," - ", contName)
      print(calc)
      eval(parse(text=calc))
    }

  }

  ###########******
  subjResidual <- projEnv$MlsDataDF$Residual[1]

  # Calculate the Residual Adjustment
  for(r in 2:nrows) {
    calResid <- paste("projEnv$MlsDataDF$Residual","[",r,"]",sep="")
    adjResid  <-  paste("projEnv$MlsDataDF$Adjstmts_CQA","[",r,"]",sep="")

    calc <- paste(adjResid," <- ", subjResidual,
                  " - ", calResid)
    print(calc)
    res <- eval(parse(text=calc))
    print(paste("res: ",res))
  }
  
  ###### ResidualSF Option
  subjResidualUsingSF <- projEnv$MlsDataDF$ResidualSF[1] * projEnv$MlsDataDF$GLA[1]

  # Calculate the Residual Adjustment Using SF method
  for(r in 2:nrows) {
    compResid <- paste("projEnv$MlsDataDF$Residual","[",r,"]",sep="")
    adjResidSF  <-  paste("projEnv$MlsDataDF$Adjstmts_CQA2","[",r,"]",sep="")

    compResid <- paste(adjResidSF," <- ", subjResidualUsingSF,
                  " - ", compResid)
    print(compResid)
    resSF <- eval(parse(text=compResid))
    print(paste("res: ",resSF))
  }

  # Now get Adjustment Totals
  for(r in 2:nrows) {
    adjNetFormula <-  ""
    adjGrossFormula <- ""
    adjSalePriceFormula <- ""

    adjSalePrice <- 0

    for(n in 1:nlmv){
      aName = as.character(lmv[[n]])
      #print(aName)

      adjNameFormula <- paste("projEnv$MlsDataDF$Adjstmts_",aName,"[",r,"]",sep="")
      if(n>1) {
        adjNetFormula <- paste(adjNetFormula,"+",sep="")
        adjGrossFormula <- paste(adjGrossFormula,"+",sep="")
      }

      adjNetFormula <- paste(adjNetFormula,adjNameFormula)
      adjGrossFormula <- paste(adjGrossFormula,"abs(",adjNameFormula,")")
    }
	
	if(projEnv$ResidualMethod  == "Residual") {
    adjNetFormula <- paste(adjNetFormula,"+ projEnv$MlsDataDF$Adjstmts_CQA[r]")
    adjGrossFormula <- paste(adjGrossFormula,"+ abs(projEnv$MlsDataDF$Adjstmts_CQA[r])")
    } else { 
	 adjNetFormula <- paste(adjNetFormula,"+ projEnv$MlsDataDF$Adjstmts_CQA2[r]")
    adjGrossFormula <- paste(adjGrossFormula,"+ abs(projEnv$MlsDataDF$Adjstmts_CQA2[r])")
	}
    adjNetFormula <- paste("projEnv$MlsDataDF$Adjstmts_Net[",r,"]  <- ", adjNetFormula,sep="")
    adjGrossFormula <- paste("projEnv$MlsDataDF$Adjstmts_Gross[",r,"]  <- ", adjGrossFormula,sep="")

    #print(adjNetFormula)
    #print(adjGrossFormula)
    eval(parse(text=adjNetFormula))
    eval(parse(text=adjGrossFormula))
    projEnv$MlsDataDF$Adjsted_SalePrice[r] <- projEnv$MlsDataDF$SalePrice[r] + projEnv$MlsDataDF$Adjstmts_Net[r]

  }

  outputFileName <- projEnv$MlsFileStageIIa
  write_xlsx(projEnv$MlsDataDF, outputFileName, col_names=TRUE,format_headers=TRUE,use_zip64=FALSE)

  ###################################################################  CACULATE URAR VALUES #####################
  outputFileName <- projEnv$MlsFileStageIIa
  projEnv$MlsDataDF <- readxl::read_excel(outputFileName,sheet="Sheet1")
  projEnv$MlsDataDF <- projEnv$MlsDataDF[with(projEnv$MlsDataDF,order(Comp)),]
  # now go through lmv, refer to projEnv$AgMap to get URAR columns and add
  nl <- length(lmv)

  nrows <- nrow(projEnv$MlsDataDF)
  uv <- projEnv$AgMap[,2]
  uv <- unique(uv)
  uv <- sort(uv)
  nu <- length(uv)
  urarSum <- ""
  
  for(u in 1:nu) {
    nm <- uv[u]
    if(nm != "." && nm != " ") {
      var <-  paste("projEnv$MlsDataDF$URAR_",nm,sep="")
      if(u>1) {
        urarSum <- paste(urarSum,"+",sep="")
      }
      #print(var)
      urarSum <- paste(urarSum,var,"[r]",sep="")

      cVar <- paste("add_column(projEnv$MlsDataDF,","URAR_",nm,"=1:nrows)",sep="")
      projEnv$MlsDataDF <- eval(parse(text=cVar))
      valueInitCmd <- paste("projEnv$MlsDataDF$URAR_",nm," <- rep(0,nrows)",sep="")
      eval(parse(text=valueInitCmd))
    }
  }

  for(u in 1:nu) {
    nm <- uv[u]
    if(nm != "." && nm != " ") {
      valueInitCmd <- paste("projEnv$MlsDataDF$URAR_",nm," <- rep(0,nrows)",sep="")
      eval(parse(text=valueInitCmd))
    }
  }

  nlmv <- length(lmv)
  maxRows <- 18 + nrows * nlmv
  projEnv$AdjGrid <-  data.frame(Comp=1:maxRows,Address="",AdjName="",AdjValue=0,URARName="",URARValue=0,AdjRow=0)
  adjRow <- 1


  for(r in 2:nrows) {
    # subset(projEnv$AgMap,grepl("LatScaled_LotSize",V1)) gives row (V1,V2)

    for(n in 1:nlmv){
      # Get Adjustment Name
      aName = as.character(lmv[[n]])
      #print(aName)

      # Get URAR name to aggregate to
      uName <- subset(projEnv$AgMap,grepl(paste("^",aName,"$",sep=""),V1))[2]
      uName <- uName[[1]]
      print(paste("aName: ",aName))
      # Make Adjustment Name
      adjNameFormula <- paste("projEnv$MlsDataDF$Adjstmts_",aName,"[",r,"]",sep="")
      print(paste(n,"  adjNameFormulaM: ",adjNameFormula))
      #print(uName)
      urarNameFormula <- paste("projEnv$MlsDataDF$URAR_",uName,"[",r,"]",sep="")
      #print(paste("urarNameForumula: ", urarNameFormula,sep=""))
      urarNameFormula1 <- paste(urarNameFormula," <- ",urarNameFormula," + ",adjNameFormula,sep="")
      #print(paste("URAR Formula: ",urarNameFormula1))
      eval(parse(text=urarNameFormula1))
      if(r < 18) {
        print("r < 18 ....")
        adjRow <- adjRow + 1
        projEnv$AdjGrid$AdjRow[adjRow] <- adjRow
        projEnv$AdjGrid$Address[adjRow] <- projEnv$MlsDataDF$Address[r]
        projEnv$AdjGrid$Comp[adjRow]  <- r -1
        projEnv$AdjGrid$AdjName[adjRow]  <- aName
        projEnv$AdjGrid$AdjValue[adjRow]  <- eval(parse(text=adjNameFormula))
        projEnv$AdjGrid$URARName[adjRow] <- uName
        projEnv$AdjGrid$URARValue[adjRow] <- eval(parse(text=urarNameFormula))
      }
    }

    print("done loop x")

    urarNameFormula1 <- paste("projEnv$MlsDataDF$URAR_CQA","[",r,"]"," <- projEnv$MlsDataDF$URAR_CQA","[",r,"]"," + projEnv$MlsDataDF$Adjstmts_CQA","[",r,"]",sep="")
    print("done loop y")

    if(r<18) {
      print("r < 18")

      adjRow <- adjRow + 1
      projEnv$AdjGrid$AdjRow[adjRow] = adjRow
      projEnv$AdjGrid$Address[adjRow] = projEnv$MlsDataDF$Address[r]
      #print(paste("uName: ",uName))
      # Make Adjustment Name
      adjNameFormula <- paste("projEnv$MlsDataDF$Adjstmts_CQA", "[",r,"]",sep="")
      print(paste("adjNameFormulaY: ",adjNameFormula))
      print(uName)
      print(paste("rX: ",r))

      print(paste("urarNameFormula1",urarNameFormula1))
      projEnv$AdjGrid$Comp[adjRow]  <- r -1
      projEnv$AdjGrid$AdjName[adjRow] <- "Adjstmts_CQA"

      print(paste("adjNameForumulaX: ",adjNameFormula))
      projEnv$AdjGrid$AdjValue[adjRow] <-  eval(parse(text=adjNameFormula))

      print("mm")
      projEnv$AdjGrid$URARName[adjRow]  <- "URAR_CQA"
      print("mn")
    }

    print(paste("URAR Formula: ",urarNameFormula1))
    eval(parse(text=urarNameFormula1))
    print(paste("r: ",r))

    if(r < 18) {
        print("xx")
        projEnv$AdjGrid$URARValue[adjRow]  <- eval(parse(text="projEnv$MlsDataDF$URAR_CQA[r]"))
        print("xy")
    }
  }

  projEnv$AdjGrid  <- projEnv$AdjGrid [with(projEnv$AdjGrid ,order(Comp,URARName,AdjRow)),]
  flog.info( "Add and Initialize Net, Gros Total",name="Log2File")
  projEnv$MlsDataDF <- add_column(projEnv$MlsDataDF,URAR_Net=1:nrows)
  projEnv$MlsDataDF <- add_column(projEnv$MlsDataDF,URAR_Gross=1:nrows)
  projEnv$MlsDataDF <- add_column(projEnv$MlsDataDF,URAR_SalePrice=1:nrows)
  projEnv$MlsDataDF$URAR_Net <- rep(0,nrows)
  projEnv$MlsDataDF$URAR_Gross <- rep(0,nrows)

  nu <- length(uv)
  flog.info(paste("uv Len: ",length(uv)),name="Log2File")
  projEnv$MlsDataDF$URAR_SalePrice <- rep(0,nrows)
 
  for(r in 2:nrows) {
    urarNetFormula <-  ""
    urarGrossFormula <- ""
    urarSalePriceFormula <- ""
    urarSalePrice <- 0

    for(n in 1:nu){
      aName <- as.character(uv[[n]])
      urarNameFormula <- paste("projEnv$MlsDataDF$URAR_",aName,"[",r,"]",sep="")

      if(n>1) {
        urarNetFormula <- paste(urarNetFormula,"+",sep="")
        urarGrossFormula <- paste(urarGrossFormula,"+",sep="")
      }
      urarNetFormula <- paste(urarNetFormula,urarNameFormula)
      urarGrossFormula <- paste(urarGrossFormula,"abs(",urarNameFormula,")")
    }

    urarNetFormula <- paste("projEnv$MlsDataDF$URAR_Net[",r,"]  <- ", urarNetFormula,sep="")
    urarGrossFormula <- paste("projEnv$MlsDataDF$URAR_Gross[",r,"]  <- ", urarGrossFormula,sep="")
    flog.info(paste("Net Formula",urarNetFormula),name="Log2File")
    flog.info(paste("Gross Formula",urarGrossFormula),name="Log2File")
    eval(parse(text=urarNetFormula))
    eval(parse(text=urarGrossFormula))
    flog.info(paste("SalePrice:", projEnv$MlsDataDF$SalePrice[r]), name="Log2File")
    flog.info(paste("URAR_Net: ",projEnv$MlsDataDF$URAR_Net[r]),name="Log2File")
    
    projEnv$MlsDataDF$URAR_SalePrice[r] <- projEnv$MlsDataDF$SalePrice[r] + projEnv$MlsDataDF$URAR_Net[r]

  }
  
  outputFileName <- projEnv$MlsFileStageIIb
  projEnv$MlsDataDF <- arrange(projEnv$MlsDataDF, Comp)

  # Write completed input and contribution data to outputFileName
  flog.info(paste("Output file:  ",outputFileName,sep=""), name="Log2File ")
  write_xlsx(projEnv$MlsDataDF, outputFileName, col_names=TRUE,format_headers=TRUE,use_zip64=FALSE)


}


