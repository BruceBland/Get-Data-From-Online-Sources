################################################################
#
# Function: GetData 
#
# Purpose: Loads data from Stooq and stores file in fst format
#          Will not re-fetch data if its the same day as today
#          reduces load requests from Stooq which has a daily limit
#
# Parameters: 
#
#       Instrument - Stooq instrument identifier
#       DataPath - Where to store fst files
#       Debug - Boolean to give debug messages on its operations
#
# Requires:
#
#       fst library for fast re-loading and space saving storage
#
# Example:
#       # Test the GetData function -make sure you have the data path
#       InstData <- GetData("AAPL.US","C:\\DATA")
#
################################################################

GetData <- function(Instrument="",DataPath="C:\\Data",DebugThis=TRUE)
{
  
  library(fst)
  
  # Lets store the Instrument with todays date
  Today = Sys.Date()
  destfile=paste(DataPath,"\\",Instrument,"-",Today,".CSV",sep="")
  
  
  # Do we already have the data
  if (file.exists(destfile)) {
    
    if (DebugThis==TRUE) {print(paste("Found file already exists so loading now ...",destfile))}
    
    InstrumentData <- read.fst(destfile,as.data.table = TRUE)
    
    return(InstrumentData)
    
  } else {
    
    if (DebugThis==TRUE) {print(paste("Loading from Stooq ..."))}
    
    InstrumentData <- read.csv(paste("https://stooq.com/q/d/l/?s=",Instrument,"&i=d",sep=""))
    
    # Create rownames
    rownames(InstrumentData) <- InstrumentData$Date
    InstrumentData$Date <- NULL
    
    # Convert the data format
    InstrumentData <- as.data.frame(InstrumentData)
    InstrumentData$Date <- as.Date(rownames(InstrumentData),"%Y-%m-%d")
    
    # Write file to FST format and then read back  
    if (DebugThis==TRUE) {print(paste("Found data now writing file for next time ...",destfile))}
    write.fst(InstrumentData,destfile)
    
    if (DebugThis==TRUE) {print(paste("Reading file back in now ...",destfile))}
    InstrumentData <- read.fst(destfile,as.data.table = TRUE)
    
    return(InstrumentData)
    
  }
  
}

#InstData <- GetData("AAPL.US","C:\\DATA",DebugThis=FALSE)
