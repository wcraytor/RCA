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



#' Filter for allowing interactions
#'  @projEnv    Project Environment
#'  @return     TRUE/FALSE
#'
#'  @example
#'  p1_initiallize
#'
#' p1_initialize loads the Excel Interaction Matrix and uses it to create
#' a table for deciding which interactions to allow
p1_initialize <- function(projEnv) {


  interactionDf <- projEnv$AllowedInteractionsDF
  interactionDf <- interactionDf[order(names( interactionDf))]
  onames <- colnames(interactionDf)
  onames <-  onames[1:length(onames)]  # Remove "RegressionFeatures" first item

  interactionDf  <- interactionDf[with(interactionDf, order(interactionDf$`AAARegressionFeature`)),]

  # Transpose interaction Data Frame into a Matrix
  # Moves names from first row to column
  INTERACTION_MATRIX <<- t(interactionDf)
  #print(INTERACTION_MATRIX)
  INTERACTION_ROWS <<- nrow(INTERACTION_MATRIX)
  INTERACTION_COLS <<- ncol(INTERACTION_MATRIX)

  for (row1 in 3:INTERACTION_ROWS) {
    for (col1 in 1:(row1 - 1)) {
      #print(paste("row: ", row1, " col: ", col1, "  ", INTERACTION_MATRIX[row1,col1]))

      INTERACTION_MATRIX[col1 + 1, row1 - 1] <-
        INTERACTION_MATRIX[row1, col1]
    }
  }

  # Convert R Matrix back into a Data Frame (Table)
  projEnv$INTERACTION_TABLE <- as.data.frame(INTERACTION_MATRIX)

  # Reset the column names
  #colnames(INTERACTION_TABLE) <<- c("Age", "Baths" ,"Beds", "FrPlcNbr", "Garage" ,"GLA" ,"Latitude", "Longitude", "LotSize" ,"PoolYN", "SaleAge", "Stories")

   nr1 <- nrow(projEnv$RegressionFieldsDF)


  projEnv$Cnames <- c(0)
  cname_cnt <- 0
  factor_cnt <- 0
  
  projEnv$RegressionFieldsDF  <- projEnv$RegressionFieldsDF %>% arrange(Stage1Regression)

  for (i in 1:nr1) {
    fld <- projEnv$RegressionFieldsDF[i, 8]
    print(paste("A",fld))
    if (fld == "x")
    {
      cname_cnt <- cname_cnt + 1
      projEnv$Cnames[cname_cnt] <- projEnv$RegressionFieldsDF[i, 1]
    }
 
    
  }

  regFldsC <- append(projEnv$Cnames, "AAARegressionFeature")
  regFldsR <- append(projEnv$Cnames, "AAARegressionFeature")

  n1 <- ncol(projEnv$INTERACTION_TABLE)
  n2 <- length(projEnv$Cnames)
  rnms <-  rownames(projEnv$INTERACTION_TABLE)
  # What a nightmare! Bug fix or feature?
  cv <- as.vector(onames %in% regFldsC)
  cvL <- length(cv)
  cv <- cv[2:cvL]

  projEnv$INTERACTION_TABLE <-  projEnv$INTERACTION_TABLE[rnms %in% regFldsR, ]
  projEnv$INTERACTION_TABLE <-   projEnv$INTERACTION_TABLE[,cv]

  # Return the Interaction Table (Data Frame)
  projEnv$INTERACTION_TABLE
}


#'  Filter for allowing interactions
#'

#'  @projEnv    Project Environment
#'  @return     TRUE/FALSE
#'
#'  @example
#'  This is called by R/earth
#'
#' This is the "allowed" function for R:earth that returns TRUE for supported
#' interactions
allowedInteractions <-
  function(degree, pred, parents, namesx) {

    # Trivial case
    if (degree < 2)
      return(TRUE)

    predictor <- namesx[pred]        # name of the predictor
    parent <- namesx[parents != 0]   # name of the parent

    ridx <- -1
    cidx <- -1
    ridx <- which(projEnv$Cnames %in% c(parent)) + 1
    cidx <- which(projEnv$Cnames %in% c(predictor))

    checked1  <- "."
    checked1 <- projEnv$INTERACTION_TABLE[ridx, cidx]

    if (identical(checked1, "x")) {
      return(TRUE)     # Allowed
    }

    return(FALSE)   # Not Allowed
  }

#'  Find the unique set of contribution variables and interactions.  These may be one-way with only one variable name
#'  or two or more way to represent interactions.  The variable names will be sorted alphabetically
#'  and the final list of vectors will have duplicates removed.
#'  @projEnv    Project Environment
#'  @listModel  This is a vector of the additive components of the model
#'              Each component will have one or more variable terms combined with constants and operators where
#'              variables are combined with a multiplicative operator
#'  @return     A list of unique vectors, where each vector contains the sorted names of the variables used in one
#'              or more of the component terms  in listModel
#'  @example
#'  findContribVars(projEnv,["3107.608","-0.2457188*SaleAge","-2.02277e-05*pmax(0,LatScaled - -444488)*pmax(0, Longitude - -328751)"] )
#'  returns [["SaleAge"],["LatScaled","Longitude"]] which would indicate the model will provide contributions for SaleAge towards
#'  SalePrice_K (the taraget variable) and the interaction between LatScaled/Longitude and SalePrice_K
#'
findContribVars <- function(projEnv, listModel) {
  # Get the number of fields to be used for regression. Remember the first field
  # is the target or dependent variable
  nr <- nrow(projEnv$RegressionFieldsDF)
  listModelLen <- length(listModel)

  contribList <- list()
  projEnv$ListModelVars <- c()

  # Generate the regression functions
  for (j in 1:listModelLen) {
    contribs <- c()
    #print(paste("ListModel: ", listModel[]))
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
        #print("no match")
      }
    }
    contribs <- sort(contribs)
    if(length(contribs) == 0) {
      next
    }
    contribList[[length(contribList) + 1]] <- list(contribs)
    #print(paste("contriblist: ",contribList))
    #contribList <- list(contribList,contribs)
  }
  # remove first empty cell
  #contribList <- contribList[2:length(contribList)]
  projEnv$ListModelVars <- contribList
  contribList <- unique(contribList)

  contribList
}




#'  Call earth
#'  @projEnv    Project Environment
#'  @return     earth output
#'
#'  @example
#'  findContribVars(projEnv,["3107.608","-0.2457188*SaleAge","-2.02277e-05*pmax(0,LatScaled - -444488)*pmax(0, Longitude - -328751)"] )
#'  returns [["SaleAge"],["LatScaled","Longitude"]] which would indicate the model will provide contributions for SaleAge towards
#'  SalePrice_K (the taraget variable) and the interaction between LatScaled/Longitude and SalePrice_K
#'
callEarth <- function(projEnv) {
  nr <- nrow(projEnv$RegressionFieldsDF)
  cnames <- append(projEnv$Cnames,"SalePrice")
  earthTarget <-  paste(projEnv$TargetVariable, " ~ ")
  linPreds <- c()
  lpCnt <- 0
  v_count <- 0
  
  housePredictors <- c()
  projEnv$MlsDataDF$Comp <- NULL
  projEnv$MlsDataDF$MlsNbr <- NULL
  projEnv$MlsDataDF$Address <- NULL
  projEnv$MlsDataDF$ParcelNbr <- NULL
  
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
      print("T1")
      fieldName <- projEnv$RegressionFieldsDF[i, 1]
      print("T2")
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
      print(paste("T3: ", makeFactor, sep = ""))
      eval(parse(text = makeFactor))
      print("T4")    }
  }

  nrows= nrow(projEnv$MlsDataDF)
  weightsVector <- rep(1, nrows)
  weightsVector[1] <- 0  # set the weight of the subject in first row to zero
  print(paste("Earth Target: ", earthTarget))
  print(paste("LinPreds: ", linPreds))
  projEnv$LinPreds <- linPreds
  flog.info(paste("Earth Target:", earthTarget))
  options(digits = 14)
 
 
  no_cores <- 24
  targetName <- "SalePrice"   
  
  ################ For Comparison Create LM Model ####################
  allnames <- append(projEnv$Cnames, "SalePrice")
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
  
  trainX <- projEnv$MlsDataDF[,cnames]
  trainY <- projEnv$MlsDataDF[, targetName]
  tX <- as.matrix(trainX)
  tY <- as.matrix(trainY)
  
  nr1 <- nrow(tY)
  for(i in 1:nr1) { tY[i] <- as.numeric(tY[i])}
  
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
  
  cnames <-  projEnv$Cnames 
  
  options(digits = 14)
  
  Y <- projEnv$MlsDataDF$SalePrice
  X <- as.data.frame( projEnv$MlsDataDF[cnames] )
  
  projEnv$marsModel <-
    earth(
      y=Y,
      x = as.data.frame( X ),
      NULL, #allowed = allowedInteractions,
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
      linpreds = linPreds,
      varmod.method = "lm"
    )
  
  
  toc(log=TRUE, quiet=TRUE)
  tic.log(format = TRUE)
 
  maxRS <- 0
  idx <- 0
  fldName = ""
 
  projEnv$evimpMatrix = evimp(projEnv$marsModel,trim=TRUE,sqrt.=TRUE)
 
  projEnv$marsSummary <- summary(projEnv$marsModel, digits =max(3,getOption("digits")-3),decomp="anova")


  thisModel <- format(projEnv$marsModel, style = "pmax", digits = max(3,getOption("digits")-3))
  listModel <- string.break.line(thisModel)
  listModel <- listModel[[1]]
  listModelLen <- length(listModel)
  projEnv$Mars <-
    list(
      model=projEnv$marsModel ,
      summary = projEnv$marsSummary,
      modelList = listModel,
      digits = max(3,getOption("digits")-3)
    )
}

#'  Stage I
#'
#'  @projEnv    Project Environment
#'  @return     None
#'
#'  @example
#'  Stage_1(projEnv)
#'
#' Out main Stage I function:
#' #export
Stage_1 <- function(projEnv) {
 
  flog.info("Create Estimate and Residual", name = "S1")
  print("###################   STAGE I   ######################")

  flog.appender(appender.console(), name = "GC")
  flog.appender(appender.file(projEnv$Log1), name = "S1")

  flog.info("Load files", name = "S1")
  inputFileName <- projEnv$MlsFileStageI
  outputFileName <- projEnv$MlsFileStageI

  # Read data from previous output
  projEnv$MlsDataDF <-
    readxl::read_excel(inputFileName, sheet = "Sheet1")

  flog.info("Load Interaction Table", name = "S1")
  interactionDf <- projEnv$AllowedInteractionsDF
  projEnv$INTERACTION_TABLE <- p1_initialize(projEnv)
  print("Returned interaction table: ")
  print(projEnv$INTERACTION_TABLE)

  nrows  <- nrow(projEnv$MlsDataDF)

  cnames <- colnames(projEnv$MlsDataDF)
  print(cnames)
  print(
    "####### CALLING R/earth.  This may take some time, depending on how fast your computers is and how large the MLS file. "
  )
  flog.info("Calling earth.", name = "S1")
  flog.info("Calling R/earth.  This may take some time. ", name = "S1")
  
  callEarth(projEnv)
  
  ######################################
  flog.info("R/earth finsihed.", name = "S1")
  print("R/earth finished.")
  Sys.sleep(3)

  # Fill Estimates, Residual and Calculate CQA
  flog.info("Call predict to get estimate.", name = "S1")
  projEnv$MlsDataDF$Estimate <-
    as.vector(predict(projEnv$Mars$model))
  projEnv$MlsDataDF$Residual <- c(0)
  projEnv$MlsDataDF$ResidualSF <- c(0)
  print("T1")
  flog.info("Calc residual.", name = "S1")
  #projEnv$MlsDataDF$Residual <- as.vector(projEnv$MlsDataDF["SalePrice_K"] - projEnv$MlsDataDF["Estimate"])
  nr <- nrow(projEnv$MlsDataDF)
  for (m in 1:nr) {
    print("T2")
    projEnv$MlsDataDF$Residual[m] <-
      projEnv$MlsDataDF$SalePrice[m] - projEnv$MlsDataDF$Estimate[m]
    projEnv$MlsDataDF$ResidualSF[m] <- projEnv$MlsDataDF$Residual[m] / 
      projEnv$MlsDataDF$GLA[m]
  }
    print("T3")
  Sys.sleep(3)
  # Set the Subject Residual temporarily to -1 for the sort
  #projEnv$MlsDataDF$Residual[1] <- (-999999999)
  print("T4")

  #outputFileName <- projEnv$MlsFileStageIb
  #write_xlsx(projEnv$MlsDataDF, outputFileName, col_names=TRUE,format_headers=TRUE,use_zip64=FALSE)

  flog.info("Order MLS Data By Residual", name = "S1")

  flog.info("Print MARS plot from earth.", name = "S1")
  print("T7a")
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
  
  ########################## CQA #############################
  # Order data by Residuals, smallest to largest
  projEnv$MlsDataDF <-
    projEnv$MlsDataDF[order(projEnv$MlsDataDF$Residual), ]
  print("T5")
  flog.info("Calculate CQA", name = "S1")
  # Now calculate CQA starting with the 2nd row
  for (i in 1:nr) {
 
    projEnv$MlsDataDF$CQA[i] = floor((((i - 1) / ((
      nr - 1
    ) / 1000)) / 1)  ) * 0.01
  }
  
  #######################################################
 
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
    
  print("XXX0a") 
  pdf(paste(projEnv$MlsVersionFolder, '/ModelComp.pdf', sep = "")) # make a PDF file to store plots
  par(bg="#002060",col.lab="white", col.axis="white",col.main="white",
      col.sub="white",col="white" )
  plot.earth.models(projEnv$marsModel, which=1:2,   
                    pt.col="white",  smooth.col = 6,
                    smooth.f = 0.3,   density.col = "red",
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
  
  print("XXX0b")
  
  pdf(paste(projEnv$MlsVersionFolder, '/ModelComp2.pdf', sep = "")) # make a PDF file to store plots
  par(bg= "lightgrey",col.lab="darkblue", col.axis="darkblue",col.main="darkblue",
      col.sub="darkblue",col="darkblue" )

  plot.earth.models(projEnv$marsModel$cv.list, 
                    which=1,
                   caption="XYZ",
                   col.grsq=4,
                   lty.grsq = 2,
                   col.rsq = 0, 
                   lty.rsq = 4,
                   col.vline = 4, 
                   lty.vline = "12",
                   col.npreds = 0,  lty.npreds  = 2,
                   legend.text = NULL,  trace = 0,
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
  
  pdf(paste(projEnv$MlsVersionFolder, '/ModelComp3.pdf', sep = "")) # make a PDF file to store plots
  par(bg= "lightgrey",col.lab="darkblue", col.axis="darkblue",col.main="darkblue",
      col.sub="darkblue",col="darkblue" )
  
  plot.earth.models(projEnv$marsModel$cv.list, 
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
  nr <- nrow(projEnv$MlsDataDF)
    # Get number of columns
  ncols <- ncol(projEnv$MlsDataDF)
  print("T9a")
  nrm1 <- nr-1
  
  z <- subset(projEnv$MlsDataDF, select = c(Residual, CQA))
  z <- arrange(z,CQA)

  z <- z[-1, ]
  residual <- z[, 1]
  cqa <- z[, 2]
  flog.info("Create CQA Mapping", name = "S1")
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
	  print("17a")
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
      projEnv$MlsDataDF[order(projEnv$MlsDataDF$ResidualSF), ]
   
    flog.info("Calculate CQA", name = "S1")
    # Now calculate CQA starting with the 2nd row
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
    flog.info("Create CQA Mapping", name = "S1")
    # Create CQA mapping
    vCqa <- as.vector(cqa$CQA)
    vResidual <- as.vector(residual$Residual)
    projEnv$VCqa <- vCqa

    projEnv$VResiduals <- vResidual
    res <- as.vector(residual)
    projEnv$CqaToResidualSF <- new(CqaMap, cqa$CQA2, res$ResidualSF)
    # So, now this sets the subject first row residual to the value based on its CQA
    projEnv$MlsDataDF$ResidualSF[1] <-
      projEnv$CqaToResidualSF$Find(projEnv$SubjectCQA)
    projEnv$MlsDataDF$CQA2[1] <- projEnv$SubjectCQA
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
  pairs(projEnv$marsModel$x, pch=".",panel=panel.smooth, gap=1/10,   row1attop=TRUE)
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
 
  plot(projEnv$evimpMatrix)
  grid.newpage()
  par(bg="grey",col.lab="black", col.axis="white",col.main="black",
      col.sub="grey",col="black" )
  grid.table(round(projEnv$evimpMatrix[,c(2,3,4)],2),theme=myt)
  title("Varialble Importance Table")                             
  dev.off()

  ############# VariableVsSalePrice.pdf ##############
  # manually plot SalePrice against all variables (less crowded than above pairplot)
  pdf(paste(projEnv$MlsVersionFolder, 'VariableVsSalePrice.pdf', sep = ""))
  par(bg="#002060",col.lab="white", col.axis="white",col.main="white",  col.sub="white",col="white" )
  
  par(mfrow=c(3, 3), mar=c(3, 3, 3, 1), mgp=c(1.5, 0.5, 0)) # multiple plots on same page
  SalePrice <- projEnv$marsModel$y
  data <- projEnv$marsModel$x

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
  print(paste("nprune: ",projEnv$Nprune))
  print(paste("ncross: ",projEnv$Ncross))
  print(paste("nfold: ",projEnv$Nfold))
  print(paste("keepxy: ",projEnv$Keepxy))
  print(paste("Penalty_1D: ",projEnv$Penalty_1D))
  print(paste("Penalty_2D: ",projEnv$Penalty_2D))
  print(paste("Trace: ",projEnv$Trace))
  print(paste("Cnames: ",projEnv$Cnames))
  print(paste("Pmethod: ", projEnv$Pmethod))
  print(paste("Degree: ",  projEnv$Degree))
  print(paste("Minspan: ", projEnv$Minspan))
  print(paste("Endspan: ", projEnv$Endspan))
  print(paste("Maxterms: ", projEnv$Maxterms))
  sink()

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

