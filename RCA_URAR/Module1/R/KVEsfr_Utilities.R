#'###############################################   KVEsfr_Utilities.R #########################################
#' Author:       Wm. Bert Craytor
#' Location:     242 Clifton Rd., Pacifica, CA 94044, USA
#' Date:         08/02/2021
#' Description:  Utilities script to generate MARS (earth)  analysis for appraisal data using R:earth
#' License:      MIT License
#' Notes:        1.  This program is free software; you can redistribute it and/or modify
#'
#####################################################################################################
# KVEsfr Utilities
library(readxl)
library(readr)
library(magrittr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(tibble)
library("writexl")
library("openxlsx")
library(collections)
library(utils)
library(Rcpp)
library(DBI)
library(RSQLite)

#' Convert a boolean value to 1 if true, otherwise 0
ConvertBoolenToBinary <- function(boolVal) {

  if(boolVal) {
    return (1)
  } else {
    return (0)
  }
}


#' Convert an null, NA and 0 value to Boolean FALSE otherwise TRUE
ConvertNaOrZeroOrEmptyToZeroElseOne <- function(naVal) {
 if(is.null(naVal) || is.na(naVal) || naVal==0)
   naVal <- 1
 else
   naVal <- 2
 res <- switch(naVal,    0,  1  )
 res
 }

#' Comresses column names for MLSData Data Frame and then
#' converts NA feilds to FALSE and Boolean fields to binary
ConvertDfFieldBoolToBinary <- function(fldName) {
  CompressColumnNamesAllDF()
  nr <- nrow(projEnv$MlsDataDF)

  for(i in 1:nr) {   
    varA <- projEnv$MlsDataDF[i,fldName]
    projEnv$MlsDataDF[i,fldName] <- ConvertNaOrZeroOrEmptyToZeroElseOne(  varA)
   # varA <- projEnv$MlsDataDF[i,fldName]
   # projEnv$MlsDataDF[i,fldName] <- ConvertBoolenToBinary(  varA)
  }

}

IsDebugMode <- function()
{
  exists(".DEBUG", envir = globalenv()) &&
    get(".DEBUG", envir = globalenv())
}

SetDebugMode <- function(on = FALSE)
{
  old_value <- is.debug.mode()
  .DEBUG <<- on
  invisible(old_value)
}
# SetDebugMode(TRUE)   #turn debug mode on
# SetDebugMode+(FALSE)  #turn debug mode off

