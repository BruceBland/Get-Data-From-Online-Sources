
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

GetHistoricAndLiveData <- function(Instrument="",DataPath="D:\\Data",DebugThis=TRUE)
{
  # Get historical and live data
  HistoricExampleData <- GetData(Instrument,DataPath=DataPath,DebugThis=DebugThis,Historic=TRUE)
  LiveExampleData <- GetData(Instrument,DataPath=DataPath,DebugThis=DebugThis,Historic=FALSE)
  
  # Add the columns that are missing from the historical one
  HistoricExampleData$Symbol <- LiveExampleData$Symbol[1]
  HistoricExampleData$Time <- "01:00:00"
  
  # Add the data together
  HistoricExampleData <- rbind(HistoricExampleData,LiveExampleData)
}


#ExampleData <- GetHistoricAndLiveData("LLOY.UK",DataPath="D:\\Data",DebugThis=TRUE)

GetMultiInstrumentData <- function(Instruments="",DataPath="D:\\Data",DebugThis=TRUE)
{
  for (Instrument in Instruments)
  {
    ExampleData <- GetHistoricAndLiveData(Instrument,DataPath="D:\\Data",DebugThis=TRUE)
    
    ExampleData$LastClose <- c(0,head(ExampleData$Close,nrow(ExampleData)-1))
    ExampleData$PercentReturns <- (ExampleData$Close - ExampleData$LastClose) / ExampleData$LastClose
    
    ExampleData <- tail(ExampleData,nrow(ExampleData)-1)
    
    ExampleData <- data.frame(Date=ExampleData$Date,
                              Return=ExampleData$PercentReturns)
    colnames(ExampleData) <- c("Date",Instrument)
    
    if (Instrument == Instruments[1])
    {
      Results <- ExampleData
    } else {
      
      Results <- merge(Results,ExampleData,by="Date",all.x = TRUE,all.y=TRUE,sort = TRUE)
    }
  }
  return(Results)
}


ExampleData <- GetMultiInstrumentData(Instruments=c("AAPL.US","IBM.US"),DataPath="D:\\Data",DebugThis=TRUE)
ExampleData <- na.omit(ExampleData)

# Now plot the results
LastBit = tail(ExampleData,100)
ResultsPlot <- ggplot(LastBit,aes(x=AAPL.US,y=IBM.US)) +
  geom_point(
             colour="Blue",
             fill="DarkBlue") +
  geom_smooth(method="loess") +
  xlab("AAPL.US") +
  ylab("IBM.US") +
  theme(plot.title = element_text(size = 12),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        text = element_text(size = 8)) +
  ggtitle(paste("Apple versus IBM"))
print(ResultsPlot)

# Historic Plot
HistoricPlot <- ggplot(LastBit, aes(Date, AAPL.US)) +
  geom_step(col="Darkgreen",aes(y=AAPL.US)) +
  geom_step(col="Darkblue",aes(y=IBM.US)) +
  theme(legend.position = "top") +
  labs(
    x = "Date",
    y = "Price",
    title = paste("Apple and IBM"),
    subtitle = paste("Historical Chart"))
print(HistoricPlot)
