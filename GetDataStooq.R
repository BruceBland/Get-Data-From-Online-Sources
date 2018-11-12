



####################################################################################
#
# Functions - Edit the Data Load and Format Function to ingest the data
#
#####################################################################################
GetData <- function(Instrument="",DataPath="D:\\Data",DebugThis=TRUE,Historic=TRUE)
{
  
  library(fst)
  
  if (Historic == TRUE)
  {
  
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
      
      if (nrow(InstrumentData) > 0)
      {
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
      } else {
        InstrumentData <- NULL
      }
      
      return(InstrumentData)
    }
      
  } else {
      
      
    if (DebugThis==TRUE) {print(paste("Loading LIVE DATA from Stooq ..."))}
      
    InstrumentData <- read.csv(paste("https://stooq.com/q/l/?s=",Instrument,"&f=sd2t2ohlcv&h&e=csv",sep=""))
    
    if (nrow(InstrumentData) > 0)
    {
      # Create rownames
      rownames(InstrumentData) <- InstrumentData$Date
      InstrumentData$Date <- NULL
      
      # Convert the data format
      InstrumentData <- as.data.frame(InstrumentData)
      InstrumentData$Date <- as.Date(rownames(InstrumentData),"%Y-%m-%d")
      
      if (DebugThis==TRUE) {print(paste("Found LIVE data",nrow(InstrumentData),"rows returned"))}
      
      InstrumentData <- as.data.frame(InstrumentData)
      
    } else {
      InstrumentData <- NULL
    }
    
    return(InstrumentData)
  }
  
}

# Example code to get LIVE prices
ExampleData <- GetData("LLOY.UK",DataPath="D:\\Data",DebugThis=TRUE,Historic=FALSE)
