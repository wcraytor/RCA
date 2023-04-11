################################################   STAGE I #########################################
# Author:       Wm. Bert Craytor
# Location:     242 Clifton Rd., Pacifica, CA 94044, USA
# Date:         08/12/2021
# Description:  Stage I script to generate MARS analysis for appraisal data using R:earth\
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
#              3.   A copy of the GNU General Public License is available at
#                   http://www.r-project.org/Licenses
#
######################################################################################################



#' Initialize the projEnv$INTERACTION_TABLE
#'  @return     TRUE/FALSE
#'
#'  @example
#'  initialize_InteractionTable()_
#'
#' p1_initialize loads the Excel AllowedInteractions sheet and uses it to create
#' a table for deciding which interactions to allow, essentially ensuring symmetry
#' and removing row-columns that are not going to be input into the regeression per the
#' specification in column "Include" of sheet "RegressionFields" of the input spreadsheet
initialize_InteractionTable <- function( ) {

  # Lets get the table of allowed interactions for 2 degree MARS
  interactionDf <- projEnv$AllowedInteractionsDF
  # Order them by name
  interactionDf <- interactionDf[order(names( interactionDf))]
  # Get the column names (names of the features for which interactions are defined
  onames <- colnames(interactionDf)
  # Column 1 is a dummy column named "AAA Regression Feature" - to be removed
  onames <-  onames[2:length(onames)]  # Remove "RegressionFeatures" first item

  # Transpose interaction Data Frame into a Matrix
  # Moves names from first row to column
  INTERACTION_MATRIX <<- t(interactionDf)
  #flog.infoINTERACTION_MATRIX),name="Log2File")
  INTERACTION_ROWS <<- nrow(INTERACTION_MATRIX)
  INTERACTION_COLS <<- ncol(INTERACTION_MATRIX)
 
  # Any x's in the lower left are copied to the upper right. And vice versa.
  for (row1 in 3:INTERACTION_ROWS) {
    for (col1 in 2:(row1 - 1)) {
      flog.info(paste("row: ", row1, " col: ", col1, "  ", INTERACTION_MATRIX[row1,col1]),name="Log2File")
      if(INTERACTION_MATRIX[row1,col1] == "x") {
        INTERACTION_MATRIX[col1 + 1, row1 - 1] <-
        INTERACTION_MATRIX[row1, col1]
      }
	  
	  if(INTERACTION_MATRIX[col1+1,row1 - 1] == "x") {
        INTERACTION_MATRIX[row1, col1] <-
        INTERACTION_MATRIX[col1 + 1, row1 - 1]
      }
	 
    }
  }

  # Convert R Matrix back into a Data Frame (Table)
  projEnv$INTERACTION_TABLE <- as.data.frame(INTERACTION_MATRIX)

  # Store number of regression fields in nRegrsFlds
  nRegrsFlds <- nrow(projEnv$RegressionFieldsDF)

  # Intialize regression input field names vector to 0
  projEnv$RegrsInputFldNames <- c(0)
  regrsInputFldNames_count <- 0
  factor_cnt <- 0
    projEnv$RegressionFieldsDF <-
      projEnv$RegressionFieldsDF[order(projEnv$RegressionFieldsDF$Stage1Regression), ]
  # Sort regression input field names alphabetically
  #projEnv$RegressionFieldsDF  <- projEnv$RegressionFieldsDF %>% arrange(Stage1Regression)
 
  # Select those regression fields were Include = "x"
  for (i in 1:nRegrsFlds) {
    fld <- projEnv$RegressionFieldsDF[i, "Include"]
    
    if (fld == "x")
    {
      regrsInputFldNames_count <- regrsInputFldNames_count + 1
      projEnv$RegrsInputFldNames[regrsInputFldNames_count] <- projEnv$RegressionFieldsDF[i, 1]
    }	
  }
  
  #  So, now we want remove the row-columns from the INTERACTION_TABLE that are for
  #  property features not selected for input in the RegressionFields sheet Include column
  
  # A couple of vectors to store column and row names with AAARegressionFeature added
  regFldsC <- append(projEnv$RegrsInputFldNames, "AAARegressionFeature")
  regFldsR <- append(projEnv$RegrsInputFldNames, "AAARegressionFeature")

  
  #n1 <- ncol(projEnv$INTERACTION_TABLE)
  #n2 <- length(projEnv$RegrsInputFldNames)
  rnms <-  rownames(projEnv$INTERACTION_TABLE)
 
  cv <- as.vector(onames %in% regFldsC)
  cvL <- length(cv)
  cv <- cv[1:cvL]

  projEnv$INTERACTION_TABLE <-  projEnv$INTERACTION_TABLE[rnms %in% regFldsR, ]
  projEnv$INTERACTION_TABLE <-   projEnv$INTERACTION_TABLE[,cv]

  # projEnv$INTERACTION_TABLE is now initialized
}  # END OF initialze_InteractionTable


#'  Filter for allowing interactions
#'
#'  @return     TRUE/FALSE
#'
#'  @example
#'  This is called by R/earth
#'
#' This is the "allowed" function for R:earth that returns TRUE for allowed
#' interactions
make.allowedInteractions <- function() {
  fldNames <-projEnv$RegrsInputFldNames
  allowedInteractions <- function(degree, pred, parents, namesx,first) {
    #flog.infopaste("DEGREE:",degree),name="Log2File"))

    if (degree < 2)
      return(TRUE)

    predictor <- namesx[pred]        # name of the predictor
    parent <- namesx[parents != 0]   # name of the parent
    #flog.infopaste(parent,"-",predictor),name="Log2File"))
    #flog.infofldNames),name="Log2File")
    ridx <- -1
    cidx <- -1
    ridx <- which(fldNames %in% c(parent)) + 1
    cidx <- which(fldNames %in% c(predictor))
    #flog.infopaste(ridx,"-",cidx),name="Log2File"))
    checked1  <- "."
    checked1 <- projEnv$INTERACTION_TABLE[ridx, cidx]
    #cat(paste("checked1:",checked1))
    #cat(":")
    if (identical(checked1, "x")) {
      #cat("t")
      return(TRUE)     # Allowed
    }
     #cat("f")
    return(FALSE)   # Not Allowed
  }
  return(allowedInteractions)
}

#'  Find the unique set of contribution variables and interactions.  These may be one-way with only one variable name
#'  or two or more way to represent interactions.  The variable names will be sorted alphabetically
#'  and the final list of vectors will have duplicates removed.
#'  @listModel  This is a vector of the additive components of the model
#'              Each component will have one or more variable terms combined with constants and operators where
#'              variables are combined with a multiplicative operator
#'  @return     A list of unique vectors, where each vector contains the sorted names of the variables used in one
#'              or more of the component terms  in listModel
#'  @example
#'  findContribVars(["3107.608","-0.2457188*SaleAge","-2.02277e-05*pmax(0,LatScaled - -444488)*pmax(0, Longitude - -328751)"] )
#'  returns [["SaleAge"],["LatScaled","Longitude"]] which would indicate the model will provide contributions for SaleAge towards
#'  SalePrice_K (the taraget variable) and the interaction between LatScaled/Longitude and SalePrice_K
#'
findContribVars <- function(  listModel) {
  # Get the number of fields to be used for regression. Remember the first field
  # is the target or dependent variable
  nr <- nrow(projEnv$RegressionFieldsDF)
  listModelLen <- length(listModel)

  contribList <- list()
  projEnv$ListModelVars <- c()

  # Generate the regression functions
  for (j in 1:listModelLen) {
    contribs <- c()
    #flog.infopaste("ListModel: ", listModel[]),name="Log2File"))
    for (n in 1:nr)  {
      val <- trimws(projEnv$RegressionFieldsDF[n, 1])
      fld <-  projEnv$RegressionFieldsDF[n, 1]
      if(is.na(fld)){
        next
      }

      searchFld <-  paste("[^A-z]", projEnv$RegressionFieldsDF[n, 1], sep = "")
      result <- regexpr(searchFld, listModel[j], perl=TRUE,ignore.case = TRUE)[1]

      if (result[1] > 0) {
        if(length(fld) > 0) {
            contribs <- append(contribs, fld)
        }
      } else {
        #flog.info"no match"),name="Log2File")
      }
    }
    contribs <- sort(contribs)
    if(length(contribs) == 0) {
      next
    }
    contribList[[length(contribList) + 1]] <- list(contribs)
    #flog.infopaste("contriblist: ",contribList),name="Log2File"))
    #contribList <- list(contribList,contribs)
  }
  # remove first empty cell
  #contribList <- contribList[2:length(contribList)]
  projEnv$ListModelVars <- contribList
  contribList <- unique(contribList)

  contribList
}




#'  Call earth and return the resulting earth model as projEnv$Mars
#'  @return     earth output
#'
#'  @example
#'     callEarth()
#'
callEarth <- function( ) {

  # Make sure to sort the MLS data matrix by comp, so that earth output from earth.predict() is
  # consistent with subsequent calls
   projEnv$MlsDataDF <-
      projEnv$MlsDataDF[order(projEnv$MlsDataDF$Comp), ]

  nr <- nrow(projEnv$RegressionFieldsDF)
  RegrsInputFldNames <- append(projEnv$RegrsInputFldNames,"SalePrice")
  earthTarget <-  paste(projEnv$TargetVariable, " ~ ")
  linPreds <- c()
  lpCnt <- 0
  v_count <- 0
  
  housePredictors <- c()

  
  for (i in 1:nr) {
    fld <- projEnv$RegressionFieldsDF[i, 8]
    if (fld == "x") {
      v_count <- v_count + 1
      fldName <- projEnv$RegressionFieldsDF[i, 1]

      if (v_count > 1) {
        # if variable count more than one, then add "+"
        earthTarget <-
          paste(earthTarget, "+", projEnv$RegressionFieldsDF[i, 1])
      } else {
        earthTarget <- paste(earthTarget, projEnv$RegressionFieldsDF[i, 1])
      }
    }

    if (projEnv$RegressionFieldsDF[i, 2] != ".") {
      linPreds <-
        append(linPreds,
               paste(projEnv$RegressionFieldsDF[i, 1], "$", sep = ""))
    }

    testStr <- gsub("^\\s+|\\s+$", "", projEnv$RegressionFieldsDF[i, 7])  #Factor should be in column 7
  
  if (testStr != ".") {
      flog.info("T1",name="Log2File")
      fieldName <- projEnv$RegressionFieldsDF[i, 1]
      flog.info("T2",name="Log2File")
      makeFactor <-
        paste(
          "projEnv$MlsDataDF$",
          fieldName,
          " <- factor(projEnv$MlsDataDF$",
          fieldName,
          ", levels=sort(unique(projEnv$MlsDataDF$",
          fieldName,
          ")))",
          sep = ""
        )
      flog.infopaste("T3: ", makeFactor, sep = "",name="Log2File")
      eval(parse(text = makeFactor))
      flog.info("T4",name="Log2File")   

	  }
  }

  nrows= nrow(projEnv$MlsDataDF)
  weightsVector <- rep(1, nrows)
  weightsVector[1] <- 0  # set the weight of the subject in first row to zero
  flog.info(paste("Earth Target: ", earthTarget),name="Log2File")
  flog.info(paste("LinPreds: ", linPreds),name="Log2File")
  projEnv$LinPreds <- linPreds
  flog.info(paste("Earth Target:", earthTarget))
  options(digits = 14)
 
 
  no_cores <- 24
  targetName <- "SalePrice"   
  
  ################ For Comparison Create LM Model ####################
  allnames <- append(projEnv$RegrsInputFldNames, "SalePrice")
  X <- as.data.frame( projEnv$MlsDataDF[allnames])

  modlm <- 1
  
  try( modlm <- lm(SalePrice ~ . , data = X), silent=TRUE)
  
  if(class(modlm) == "lm") { 
    pdf(paste(projEnv$MlsVersionFolder, '/LinearModelx.pdf', sep = "")) # make a PDF file to store plots
    
    par(bg= "lightgrey",col.lab="darkblue", col.axis="darkblue",col.main="darkblue",
        col.sub="darkblue",col="darkblue" )
    
    plotmo(modlm,   all1=FALSE,
           all2=FALSE,
           clip = FALSE,
           col.degree1="white",
           col.image=grey(0:9/10),
           col.persp="red",
           col.response="yellow",
           cex.response=1, 
           col.shade="orange",
           col.se="red",
           degree1=TRUE,
           degree2=TRUE,
           density.col = "red",
           density.adjust = 5,
           density.lty = 1,
           # One graph per page.
           grid.col="yellow",
           jitter = .5,
           level = 0.80,
           level.shade="darkblue",
           level.shade2=1,
           ngrid2 = 50,
           npoints = TRUE,
           nresponse=NA,  
           nrug = "density",
           pch.response=2,
           persp.col=terrain.colors(50),
           persp.ticktype = "detailed",
           pmethod = "plotmo",
           prednames.abbreviate = FALSE,
           pt.col = "orange2",
           persp.shade=0.50,
           smooth.col = 6,
           smooth.f = 0.3,
           trace = -1,
           type=NULL,
           type2 = "persp",
           ylab = "Partial Value Contribution" ,
           ylim = NA)

    title("Multi-Linear Regression    \n")
    
    dev.off()
  }
  #############################  WORK AREA GBM ##########################
    seeds <- vector(mode = "list", length = 22)
  for(i in 1:22) seeds[[i]] <- sample.int(1000, 22)
  xx <-  projEnv$MlsDataDF[[targetName]]
  
  trainX <- projEnv$MlsDataDF[,RegrsInputFldNames]
  trainY <- projEnv$MlsDataDF[, targetName]
  tX <- as.matrix(trainX)
  tY <- as.matrix(trainY)
  
  nRegrsFlds <- nrow(tY)
  for(i in 1:nRegrsFlds) { tY[i] <- as.numeric(tY[i])}
  
   gbmGrid <- expand.grid(interaction.depth = 1,
                          shrinkage = 0.1,
                 
                          n.trees = as.numeric(c(10, 50, 100,150)),
                          n.minobsinnode = as.numeric(8))                   #### Check this was 10
  
  tC <- trainControl(method="cv",number=10 )                               ####   3?

  #############################  WORK AREA EARTH ########################t
  REQUIRE_NAMESPACE_QUIET_STOP <<- ""
  
  SSCount <<- 0
  SStateP <<- 0
  
  
  set.seed(123)
  seeds <- vector(mode = "list", length = 41)
  for(i in 1:41) seeds[[i]] <- sample.int(1000, 41)
  
  RegrsInputFldNames <-  projEnv$RegrsInputFldNames 
  
  # We want to set digits to 14 so that GSI coorditnates are treated with the necessary precision:
  options(digits = 14)
  
  # Target Variable
  Y <- projEnv$MlsDataDF$SalePrice
  
  # Input Variables
  X <- as.data.frame( projEnv$MlsDataDF[RegrsInputFldNames] )
  
  interactions <- make.allowedInteractions()

  if(!projEnv$FilterInteractions)
      interactions <- NULL

  interactions <- make.allowedInteractions()

  # This is the call to earth():
  projEnv$MarsModel <-
    earth(
      y=Y,
      x = as.data.frame( X ),
      allowed = interactions,
      nprune = projEnv$Nprune,
      pmethod = projEnv$Pmethod,
      nk = projEnv$Maxterms,
      nfold = projEnv$Nfold,
      trace=projEnv$Trace,
      ncross = projEnv$Ncross,   
      degree =projEnv$Degree,
      keepxy = projEnv$Keepxy,
      Auto.linpreds = FALSE,
      penalty = if(projEnv$Degree > 1) projEnv$Penalty_2D else projEnv$Penalty_1D,
      minspan = projEnv$Minspan,
      endspan = projEnv$Endspan,
      fast.beta = 0,
      fast.k = 0,
      newvar.penalty = 0,
      linpreds = linPreds,
      varmod.method = "lm"
    )
  
  
  toc(log=TRUE, quiet=TRUE)
  tic.log(format = TRUE)
 
  maxRS <- 0
  idx <- 0
  fldName = ""
 
  projEnv$EvimpMatrix = evimp(projEnv$MarsModel,trim=TRUE,sqrt.=TRUE)
 
  projEnv$MarsSummary <- summary(projEnv$MarsModel, digits =max(3,getOption("digits")-3),decomp="anova")

  thisModel <- format(projEnv$MarsModel, style = "pmax", digits = max(3,getOption("digits")-3))
  listModel <- string.break.line(thisModel)
  listModel <- listModel[[1]]
  listModelLen <- length(listModel)
  
  projEnv$Mars <-
    list(
      model=projEnv$MarsModel ,
      summary = projEnv$MarsSummary,
      modelList = listModel,
      digits = max(3,getOption("digits")-3)
    )
	
}    # END OF callEarth()


#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#'  Stage I
#'  @return     None
#'
#'  @example
#'  Stage_1( )
#'
#' Out main Stage I function:
#' #export
Stage_1 <- function() {
 
  flog.info("Create Estimate and Residual", name = "Log2File")
  flog.info("###################   STAGE I   ######################",name="Log2File")

  flog.info("Load files", name = "Log2File")
  inputFileName <- projEnv$MlsFileStageI
  outputFileName <- projEnv$MlsFileStageI

  # Read data from previous output
  projEnv$MlsDataDF <-
    readxl::read_excel(inputFileName, sheet = "Sheet1")

  flog.info("Load Interaction Table", name = "Log2File")
  interactionDf <- projEnv$AllowedInteractionsDF
  initialize_InteractionTable( )
  flog.info("Returned interaction table: ",name="Log2File")
  flog.info(projEnv$INTERACTION_TABLE,name="Log2File")

  nrows  <- nrow(projEnv$MlsDataDF)

 
  flog.info(
    ">>> CALLING R/earth.  This may take some time, depending on how fast your computers is and how large the MLS file. "
  ,name="Log2File")
  flog.info("Calling earth.", name = "Log2File")
  flog.info("Calling R/earth.  This may take some time. ", name = "Log2Console")
  
  ###################
  callEarth( )
  ###################
  
  flog.info("R/earth finsihed." )
 
  Sys.sleep(3)
  
  # Order the MLS property grid by Comp:
   projEnv$MlsDataDF <-
      projEnv$MlsDataDF[order(projEnv$MlsDataDF$Comp), ]
	  
  # Set the first residual which is now the subject (Comp 0) to -9999999, so that ordering will keep it first
  projEnv$MlsDataDF$Residual[1] <- (-999999999)
     projEnv$MlsDataDF <-
      projEnv$MlsDataDF[order(projEnv$MlsDataDF$Residual), ]
	  
  # Fill Estimates, Residual and Calculate CQA
  flog.info("Call predict to get estimate.", name = "Log2File")
  projEnv$MlsDataDF$Estimate <-
    as.vector(predict(projEnv$Mars$model))
  projEnv$MlsDataDF$Residual <- c(0)
  projEnv$MlsDataDF$ResidualSF <- c(0)
 
  flog.info("Calc residual.", name = "Log2File")
  #projEnv$MlsDataDF$Residual <- as.vector(projEnv$MlsDataDF["SalePrice_K"] - projEnv$MlsDataDF["Estimate"])
  nr <- nrow(projEnv$MlsDataDF)
  for (m in 2:nr) {
    flog.info("T2",name="Log2File")
    projEnv$MlsDataDF$Residual[m] <-
      projEnv$MlsDataDF$SalePrice[m] - projEnv$MlsDataDF$Estimate[m]
	  
    projEnv$MlsDataDF$ResidualSF[m] <- projEnv$MlsDataDF$Residual[m] / 
      projEnv$MlsDataDF$GLA[m]
  }
   
  Sys.sleep(3)
 
 

  #outputFileName <- projEnv$MlsFileStageIb
  #write_xlsx(projEnv$MlsDataDF, outputFileName, col_names=TRUE,format_headers=TRUE,use_zip64=FALSE)

  flog.info("Calculate CQA", name = "Log2File")
  # Order data by Residuals, smallest to largest, with -9999999999 in first/subject row
  # first order by Comp to get subject on to -999999999
  projEnv$MlsDataDF <-
      projEnv$MlsDataDF[order(projEnv$MlsDataDF$Comp), ]
  # then set subject residual to
  projEnv$MlsDataDF$Residual[1] <- (-999999999)

  projEnv$MlsDataDF <-
      projEnv$MlsDataDF[order(projEnv$MlsDataDF$Residual), ]

  flog.info("Calculate CQA", name = "F1")
  # Now calculate CQA starting with the 2nd row
  for (i in 2:nrows) {

    projEnv$MlsDataDF$CQA[i] = floor((((i - 1) / ((
      nrows - 1
    ) / 100)) / 10) / 0.1) * 0.1
  }

  flog.info("Print MARS plot from earth.", name = "Log2File")
  flog.info("T7a",name="Log2File")
  options(scipen = 999) 
  
  # Plot the regression functions
  pdf(paste(projEnv$MlsVersionFolder, '/MarsFit.pdf', sep = "")) # make a PDF file to store plots

  ## Below col.degree1 - is the function line
  ## density.col is the density
  ## density.adjust is the weight of the line
  
  par(bg="#002060",col.lab="white", col.axis="white",col.main="white",   col.sub="white",col="white")
  
  MarsFit.pdf <- plotmo(
    object=projEnv$Mars$model,
    all1=FALSE,
    all2=FALSE,
    caption = "MARS OUTPUT",
    clip = FALSE,
    col.degree1="white",
    col.image=grey(0:9/10),
    col.persp="red",
    col.response="yellow",
    cex.response=1, 
    col.shade="orange",
    col.se="red",
    degree1=TRUE,
    degree2=TRUE,
    density.col = "red",
    density.adjust = 5,
    density.lty = 1,
    do.par = 0,  # One graph per page.
    grid.col="yellow",
    jitter = .5,
    level = 0.80,
    level.shade="darkblue",
    level.shade2=1,
    ngrid2 = 50,
    npoints = TRUE,
    nresponse=NA,  
    nrug = "density",
    pch.response=2,
    persp.col=terrain.colors(50),
    persp.ticktype = "detailed",
    pmethod = "partdep",
    prednames.abbreviate = FALSE,
    pt.col = "orange2",
    persp.shade=0.50,
    smooth.col = 6,
    smooth.f = 0.3,
    trace = -1,
    type=NULL,
    type2 = "persp",
    ylab = "Partial Value Contribution" ,
   ylim = NA
  )

  dev.off()
  
   
  #######################################################
  
  # Print Mars Model
  pdf(paste(projEnv$MlsVersionFolder, '/Residuals2a.pdf', sep = "")) # make a PDF file to store plots

  par(bg= "lightgrey",col.lab="darkblue", col.axis="darkblue",col.main="darkblue",
      col.sub="darkblue",col="darkblue" )
  plotres( projEnv$Mars$model,
           which=1:8,
           col.response="red",
           col.mean.infold.rsq="blue", col.infold.rsq="lightblue",
           col.grsq=0, col.rsq=0, col.vline=0, col.oof.vline=0,
           col.degree1="darkyellow",
           pt.col="darkgreen",
           info=TRUE,
           versus=1,
           do.par=0,
           trace=1,
           npoints=3000)
  
  dev.off()
  
  # Plot part dependencies
  pdf(paste(projEnv$MlsVersionFolder, '/PartDep.pdf', sep = "")) # make a PDF file to store plots

  par(bg= "lightgrey",col.lab="darkblue", col.axis="darkblue",col.main="darkblue",
      col.sub="darkblue",col="darkblue" )
  plotmo(projEnv$Mars$model, 
         pmethod="partdep",
         col.response="red",
        
         col.grsq=0, col.rsq=0, col.vline=0, col.oof.vline=0,
         col.degree1="yellow",
         pt.col="darkgreen",
         info=TRUE,
        
         do.par=0 
         
         )
  
  dev.off()
    
  # Plot "Model Comparison" and "Cumulative Distribution" for Final MARS Model
  pdf(paste(projEnv$MlsVersionFolder, '/FinalModel_Comp_CumDist.pdf', sep = "")) # make a PDF file to store plots
  par(bg="#002060",col.lab="white", col.axis="white",col.main="white",
      col.sub="white",col="white" )
  plot.earth.models(projEnv$MarsModel, which=1:2,
                    pt.col="white",  smooth.col = 6,
                    smooth.f = 0.3,   density.col = "red",
                    jitter=0.01,
                    density.adjust = 5,
                    grid.col="white",
                    col.cv="lightred",
                    density.lty = 1,  col.image=grey(0:9/10),
                    do.par=0,
                    col.persp="red",
                    col.npres="white",
                    col.oof.vline="white",
                    col.response="yellow",
                    cex.response=1,  #size oftrian
                    col.shade="orange",
                    col.grsq = "white", col.rsq =  "yellow", col.infold.rsq = "orange2",
                    col.mean.infold.rsq = 4, col.mean.oof.rsq = "yellow",
                    col.se="red", legend.pos= )

  dev.off()
  
  # print a comparison of the cross-validation models created showing various GR2's produced
  # vs number of terms used
  pdf(paste(projEnv$MlsVersionFolder, '/CrossValidation_GR2vsNbrTerms.pdf', sep = "")) # make a PDF file to store plots
  par(bg= "lightgrey",col.lab="darkblue", col.axis="darkblue",col.main="darkblue",
      col.sub="darkblue",col="darkblue" )

  plot.earth.models(projEnv$MarsModel$cv.list, 
                    which=1,
                   caption="XYZ",
                   col.grsq=4,
                   lty.grsq = 2,
                   col.rsq = 0, 
                   lty.rsq = 4,
                   col.vline = 4, 
                   lty.vline = "12",
                   jitter=0.01,
                   col.npreds = 0,
                   lty.npreds  = 2,
                   legend.text = NULL,
                    trace = 0,
                    level.shade="gray",
                    level.shade2="lightblue",
                    legend.cex=0.9,
                    pt.col="white",  
                    smooth.col = 6,
                    smooth.f = 0.3,   
                    density.col = "red",
                    density.adjust = 5,
                    grid.col="darkblue",
                    text.col="yellow",
                    mtext.col="red",
                    col.cv="lightred",
                    density.lty = 1,  
                    col.image=grey(0:9/10),
                    col.persp="red",
                    col.lab="darkblue",
                    col.main="darkblue",
                    col.sub="darkblue",
                    col.axis="darkblue",
                    col.npres="darkblue",
                    col.oof.vline="orange2",
                    col.response="orange",
                    cex.response=4,
                    col.shade="orange",
                    col.infold.rsq = "orange2",
                    col.mean.infold.rsq = 4,
                    col.mean.oof.rsq = "yellow",
                    col.se="red"
                   )
  
  dev.off()
  
  # Print "Cumulative Distribution"  for Cross Validation runs
  pdf(paste(projEnv$MlsVersionFolder, '/CrossValidation_CumDist.pdf', sep = "")) # make a PDF file to store plots
  par(bg= "lightgrey",col.lab="darkblue", col.axis="darkblue",col.main="darkblue",
      col.sub="darkblue",col="darkblue" )
  
  plot.earth.models(projEnv$MarsModel$cv.list, 
                    which=2,
                    caption="XYZ",
                    col.grsq=4,
                    lty.grsq = 2,
                    col.rsq = 3, 
                    lty.rsq = 4,
                    col.vline = 4, 
                    lty.vline = "12",
                    col.npreds = 3,  lty.npreds  = 2,
                    legend.text = NULL,  trace = 0,
                    pt.col = c( "lightyellow","yellow",  
                                "lightgreen", "green","darkgreen", 
                                "orange", "darkorange",
                                "red","darkred",
                                "tan","purple","lightblue","blue","darkblue","black"),  
                    smooth.col = 6,
                    smooth.f = 0.3,  
                    do.par=0,
                    density.col = "red",
                    density.adjust = 5,
                    grid.col="darkblue",
                    jitter=0.01,,
                    text.col="yellow",
                    mtext.col="red",
                    col.cv="lightred",
                    density.lty = 1,  
                    col.image=grey(0:9/10),
                    col.persp="red",
                    col.lab="darkblue",
                    col.main="darkblue",
                    col.sub="darkblue",
                    col.axis="darkblue",
                    col.npres="darkblue",
                    col.oof.vline="orange2",
                    col.response="orange",
                    cex.response=4,
                    col.shade="orange",
                    col.infold.rsq = "orange2",
                    col.mean.infold.rsq = 4,
                    col.mean.oof.rsq = "yellow",
                    col.se="red")
  
  dev.off()
  
  
 
 #######################  CQA1 STUFF ###################
  # projEnv$MlsDataDF <-
   #  projEnv$MlsDataDF[order(projEnv$MlsDataDF$Comp), ]
  #rojEnv$MlsDataDF$CQA[1] <- (-1)

  nr <- nrow(projEnv$MlsDataDF)
    # Get number of columns
  ncols <- ncol(projEnv$MlsDataDF)
 
  nrm1 <- nr-1
  projEnv$MlsDataDF$CQA[1] <- -1
  z <- subset(projEnv$MlsDataDF, select = c(Residual, CQA))
  z <- arrange(z,CQA)

  z <- z[-1, ]
  residual <- z[, 1]
  cqa <- z[, 2]
  flog.info("Create CQA Mapping", name = "Log2File")
  # Create CQA mapping
  vCqa <- as.vector(cqa$CQA)
  vResidual <- as.vector(residual$Residual)
  projEnv$VCqa <- vCqa
  projEnv$VResiduals <- vResidual
  res <- as.vector(residual)
  projEnv$CqaToResidual <- new(CqaMap, cqa$CQA, res$Residual)
  # So, now this sets the subject first row residual to the value based on its CQA
  projEnv$MlsDataDF$Residual[1] <-
    projEnv$CqaToResidual$Find(projEnv$SubjectCQA)

  projEnv$MlsDataDF$CQA[1] <- projEnv$SubjectCQA
   nrm1 <- nr-1
    options(scipen = 999) 
    minz <- c(13)
    nrm20 <- round(nrm1/20)
    nrm10 <- round(nrm1/10)
    
    minz[1] <- round(z[1,1])       #lowest
    minz[2] <- round(z[nrm20,1])   # score 0.5
    minz[12] <- round(z[nrm1-nrm20,1])  #score 9.5
    minz[13] <- round(z[nrm1,1])     # score 10.0
    for(k in 1:9) {
      minz[k+2] <-  round(z[nrm10*k+1,1])
    }
    
    
    png(file=paste(projEnv$MlsVersionFolder,"Residual.png"),width=1000,height=1200)
    par(bg="#002060",col.lab="white", col.axis="white",col.main="white",
        col.sub="white",col="white",mar=c(6,8,4,4) , mai= c(1,2,1,0) )
    plot(
      z$CQA,
      z$Residual,
      main = "CQA-Residual Function",
      ylab = "Residual\n",
      xlab = "\nCQA",
      
      
      type = "l",
      pch=19,
      cex.lab=3,
      
      cex.main=3,
      axes=FALSE,
      
      col.lab="white",
      col = "white"
    )
    
    
    axis(side = 2, at= minz, labels=format(minz, big.mark=",", format="d"),cex.axis=1.25, las=2, col.ticks="white",col.axis="white",col="white" )
    
    axis(side = 1, at = c(0,0.5, 1, 2, 3, 4, 5, 6, 7, 8, 9, 9.5, 10), cex.axis=1.25,col.ticks="white",col.axis="white",col="white")
    
    dev.off()
    
  ######################## CQA2 STUFF #####################
   projEnv$MlsDataDF <-
      projEnv$MlsDataDF[order(projEnv$MlsDataDF$Comp), ]
	  
 
   
    flog.info("Calculate CQA", name = "Log2File")
    # Now calculate CQA2 starting with the 2nd row
    for (i in 1:nr) {
      
      projEnv$MlsDataDF$CQA2[i] = floor((((i - 1) / ((
        nr - 1
      ) / 1000)) / 1)  ) * 0.01
    }
    
    ncols <- ncol(projEnv$MlsDataDF)
 
    nrm1 <- nr-1
    
    z <- subset(projEnv$MlsDataDF, select = c(ResidualSF, CQA2))
    z <- arrange(z,CQA2)
    z <- z[-1, ]
    residual <- z[, 1]
 
    cqa <- z[, 2]
    flog.info("Create CQA Mapping", name = "Log2File")
    # Create CQA mapping
    vCqa <- as.vector(cqa$CQA)
    vResidual <- as.vector(residual$Residual)
    projEnv$VCqa <- vCqa

    projEnv$VResiduals <- vResidual
    res <- as.vector(residual)
    projEnv$CqaToResidualSF <- new(CqaMap, cqa$CQA2, res$ResidualSF)
	
	
    # So, now this sets the subject first row residual to the value based on its CQA
	projEnv$MlsDataDF$CQA2[1] <- projEnv$SubjectCQA2
	   
    projEnv$MlsDataDF$ResidualSF[1] <-
      projEnv$CqaToResidualSF$Find(projEnv$SubjectCQA2)
	  
 
	 
   projEnv$MlsDataDF$ResidualSF[1] <-
     projEnv$MlsDataDF$ResidualSF[1] * projEnv$MlsDataDF$GLA[1]

 
    projEnv$MlsDataDF <-
      projEnv$MlsDataDF[order(projEnv$MlsDataDF$ResidualSF), ]
	  
    nrm1 <- nr-1
    options(scipen = 999)  
    minz <- c(13)
    nrm20 <- round(nrm1/20)
    nrm10 <- round(nrm1/10)
    
    minz[1] <- round(z[1,1],2)       #lowest
    minz[2] <- round(z[nrm20,1],2)   # score 0.5
    minz[12] <- round(z[nrm1-nrm20,1],2)  #score 9.5
    minz[13] <- round(z[nrm1,1],2)     # score 10.0
	
    for(k in 1:9) {
      minz[k+2] <-  round(z[nrm10*k+1,1],2)
    }
    
    png(file=paste(projEnv$MlsVersionFolder,"ResidualSF.png"),width=1000,height=1200)
    par(bg="#002060",col.lab="white", col.axis="white",col.main="white",
        col.sub="white",col="white",mar=c(6,8,4,4) , mai= c(1,2,1,0) )
    plot(
      z$CQA2,
      z$ResidualSF,
      main = "CQA-Residual/SF Function",
      ylab = "Residual/SF\n",
      xlab = "\nCQA",
      
      
      type = "l",
      pch=19,
      cex.lab=3,
      
      cex.main=3,
      axes=FALSE,
      
      col.lab="white",
      col = "white"
    )
    
    
    axis(side = 2, at= minz, labels=sprintf("$%s",format(minz, big.mark=",", format="d")),cex.axis=1.25, las=1, col.ticks="white",col.axis="white",col="white" )
    
    axis(side = 1, at = c(0,0.5, 1, 2, 3, 4, 5, 6, 7, 8, 9, 9.5, 10), cex.axis=1.25,col.ticks="white",col.axis="white",col="white")
    
    dev.off()
    
    
  ######################## END OF CQA #################
    
  ############ Pairs.pdf ###############
  pdf(paste(projEnv$MlsVersionFolder, '/Pairs.pdf', sep = ""),width=18,height=18)
  par(bg="lightblue",col.lab="darkblue", col.axis="darkblue",col.main="darkblue",
      col.sub="darkblue",col="darkblue" )
  pairs(projEnv$MarsModel$x, pch=".",panel=panel.smooth, gap=1/10,   row1attop=TRUE)
  title( "Variable Pair Relationship\n\n" )
  dev.off()
  ############# VariableImportance1.pdf ##############
  # manually plot SalePrice against all variables (less crowded than above pairplot)
  myt <- ttheme_default(
    # Use hjust and x to left justify the text
    # Alternate the row fill colours
    core = list(fg_params=list(hjust = 1, x=1),
                bg_params=list(fill=c("lightgrey", "grey"))),
    
    # Change column header to white text and red background
    colhead = list(fg_params=list(col="darkblue"),
                   bg_params=list(fill="grey"))
  )
  
  pdf(paste(projEnv$MlsVersionFolder, 'VariableImportance1.pdf', sep = ""))
  par(bg="grey",col.lab="black", col.axis="white",col.main="black",
      col.sub="grey",col="black" )
 
  plot(projEnv$EvimpMatrix)
  grid.newpage()
  par(bg="grey",col.lab="black", col.axis="white",col.main="black",
      col.sub="grey",col="black" )
  grid.table(round(projEnv$EvimpMatrix[,c(2,3,4)],2),theme=myt)
  title("Varialble Importance Table")                             
  dev.off()

  ############# VariableVsSalePrice.pdf ##############
  # manually plot SalePrice against all variables (less crowded than above pairplot)
  pdf(paste(projEnv$MlsVersionFolder, 'VariableVsSalePrice.pdf', sep = ""))
  par(bg="#002060",col.lab="white", col.axis="white",col.main="white",  col.sub="white",col="white" )
  
  par(mfrow=c(3, 3), mar=c(3, 3, 3, 1), mgp=c(1.5, 0.5, 0)) # multiple plots on same page
  SalePrice <- projEnv$MarsModel$y
  data <- projEnv$MarsModel$x

  for(i in 1:ncol(data)) {
    if(colnames(data)[i] != "SalePrice") { # skip plot of SalesPrice against SalesPrice
      x <- data[,i]
      plot(x, SalePrice, pch=20, col="darkgray",
           xlab=colnames(data)[i], ylab="SalePrice")
      lines(lowess(x, SalePrice), col="red")
      title(paste("\n\nSalePrice vs ", colnames(data[i]),"\n" ))
    }
  }
 
  dev.off()

  ################### Histograms.pdf #############################
  pdf(paste(projEnv$MlsVersionFolder, '/Histogramx.pdf', sep = ""))
  par(bg="#002060",col.lab="white", col.axis="white",col.main="white",
      col.sub="white",col="white" )
  
  par(bg="#002060",col.lab="white", col.axis="white",col.main="white",
      col.sub="white",col="white" )
  
  par(mfrow=c(4, 3), mar=c(3, 3, 3, 1), mgp=c(1.5, 0.5, 0))
  for(i in 1:ncol(data)) {
    hist(as.numeric(data[,i]), # as.numeric needed for hist with factor variables (AreaID)
         col="darkgray", border="darkgray", 
         xlab=colnames(data)[i], main="")
    title(paste("\n\nHistogram ", colnames(data[i]),"\n" ))
  }
  title(paste("Histogram \n\n" ))
  dev.off() 

  modelOutput <- paste(projEnv$MlsVersionFolder, '/Model.txt', sep = "")

  sink(
    file = modelOutput,
    append = FALSE,
    type = c("output", "message"),
    split = TRUE
  )

  print(projEnv$Mars)
  print(paste("CodeFolder: ",projEnv$CodeFolder))
  print(paste("VersionFolder: ",projEnv$VersionFolder))
  print(paste("MaxTerms: ",projEnv$Maxterms))
  print(paste("nprune: ",projEnv$Nprune))
  print(paste("ncross: ",projEnv$Ncross))
  print(paste("nfold: ",projEnv$Nfold))
  print(paste("keepxy: ",projEnv$Keepxy))
  print(paste("Penalty_1D: ",projEnv$Penalty_1D))
  print(paste("Penalty_2D: ",projEnv$Penalty_2D))
  print(paste("Trace: ",projEnv$Trace))
  print(paste("RegrsInputFldNames: ",projEnv$RegrsInputFldNames))
  print(paste("Pmethod: ", projEnv$Pmethod))
  print(paste("Degree: ",  projEnv$Degree))
  print(paste("Minspan: ", projEnv$Minspan))
  print(paste("Endspan: ", projEnv$Endspan))
  print(paste("Maxterms: ", projEnv$Maxterms))
  print(paste("Linpreds:",projEnv$LinPreds))
  sink()
   projEnv$MlsDataDF <-
      projEnv$MlsDataDF[order(projEnv$MlsDataDF$Comp), ]
  # Save current data file
  outputFileName <- projEnv$MlsFileStageIb
  write_xlsx(
    projEnv$MlsDataDF,
    outputFileName,
    col_names = TRUE,
    format_headers = TRUE,
    use_zip64 = FALSE
  )

}

