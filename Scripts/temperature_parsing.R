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
# This script creates two functions useful for processing fire temperature data.
#
# Completeness: Complete
#
# Inputs:
# None
#
# Outputs: 
# Functions: max_subset, fire_duration
# 
##################################

#Create a function that subsets data that is above a critical temperature
#df = dataframe where values are stored
#PlotID = unique ID of each plot
#tempcrit = critical temperature above which values are accepted
#tempvar = column name of temperature values
#df_new = value of time code
#min_subset <- function(df,tempcrit,tempvar,time,PlotID){
 # df_tcrit <- subset(df,tempvar > tempcrit)
  #as.data.frame(df_tcrit)
  #return(df_tcrit)
#}

max_subset <- function(dframe,tempvar,time,PlotID){
  #Find max temps
  maxDF <- aggregate(tempvar~PlotID,data=dframe,FUN="max")
  newDF <- data.frame(PlotID,time,tempvar)
  newDF <- merge(newDF,maxDF,by="PlotID")
  colnames(newDF) <- c("PlotID","t","Temp","TempMax")
  
  #Find rows where temp=maxtemp
  timeMid <- subset(newDF,Temp==TempMax)
  
  #Make a df as a lookup table for maximum time values where temperature is maximized
  splitDF <- aggregate(t~PlotID,data=timeMid,FUN="max")
  
  #Make a new DF combining the lookup table
  scratchDF <- data.frame(PlotID=PlotID,timestamp=time,Temp=tempvar)
  scratchDF <- merge(scratchDF,splitDF,by="PlotID")
  
  #Subset by time values that are less than t
  outputDF <- ddply(scratchDF,"PlotID",subset,timestamp < t)
  
  #Drop extra column
  outputDF <- outputDF[-4]
  
  return(outputDF)
}

idPeak <- function(dframe,tempvar,time,PlotID){
  #Find max temps
  maxDF <- aggregate(tempvar~PlotID,data=dframe,FUN="max")
  newDF <- data.frame(PlotID,time,tempvar)
  newDF <- merge(newDF,maxDF,by="PlotID")
  colnames(newDF) <- c("PlotID","t","Temp","TempMax")
  
  #Find rows where temp=maxtemp
  timeMid <- subset(newDF,Temp==TempMax)
  
  #Make a df as a lookup table for maximum time values where temperature is maximized
  splitDF <- aggregate(t~PlotID,data=timeMid,FUN="max")
  
  #Make a new DF combining the lookup table
  scratchDF <- data.frame(PlotID=PlotID,timestamp=time,Temp=tempvar)
  scratchDF <- merge(scratchDF,splitDF,by="PlotID")
  
  #Subset by time values that are less than t
 # outputDF <- ddply(scratchDF,"PlotID",subset,timestamp < t)
  
  #Drop extra column
#  outputDF <- outputDF[-4]
  
  #Okay, let's lop off the bottom tails
 # outputDF <- ddply(outputDF,"PlotID",subset,Temp>=tcrit)
  
  return(scratchDF)
}



fire_duration <- function(dframe,time,PlotID){
  
  outputDF <- ddply(dframe,.(PlotID),
                    mutate,
                    flamefront = max(unlist(time)) - min(unlist(time)))
  
  return(outputDF)
}

