# Copyright notice:
# This script is provided with a Creative Commons - Attribution license, as defined on:
# http://creativecommons.org/licenses/by/3.0/us/
#
#
# Author Contact:
# Daniel Godwin
# danielg7@gmail.com
# Savanna Ecology Lab
# Division of Biological Sciences
# University of Missouri - Columbia
#
# Script Intent:
# This script creates two functions for reading in HOBO-brand data and iButton data.
#
# Completeness: Complete
#
# Inputs:
# None
#
# Outputs: 
# Functions: read.ibutton, read.hobo
# 
##################################

require(lubridate)

read.ibutton <- function( iButtonFile,Date=TRUE,RemoveUnit=TRUE)
{
  ibutton <- read.csv(iButtonFile,skip=19,col.names=c("Time","Unit","Temperature"))
  
  if(Date==TRUE)
  {
  ibutton$Time <- as.character(ibutton$Time)
  ibutton$Time <- mdy_hms(ibutton$Time)
  }
  
  if(RemoveUnit==TRUE)
    ibutton$Unit <- NULL
  return(ibutton)
}

read.hobo <- function(HoboFile,Date=TRUE,RemoveObs=TRUE)
{
  hobo <- read.csv(HoboFile,skip=2,col.names=c("Obs","Time","Temperature"),na.strings = "-888.88",)
  
  if(RemoveObs==TRUE)
    hobo$Obs <- NULL
  
  if(Date==TRUE){
    hobo$Time <- as.character(hobo$Time)
    hobo$Time <- mdy_hms(hobo$Time)
  }
  return(hobo)
}


