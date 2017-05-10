#remove old objects for safety resons
rm(list=ls(all=TRUE))
#set seed to make analysis reproducible if any pseudo random number generator is used by any function
set.seed(123)
#utility function to glue together text without separator
glue<-function(...){paste(...,sep="")}
#read the local paths to different directories from an external file
source("workingDir.R")
#change to the data directory
setwd(dataDir)

header<-c("Date","Hour","Name","IP","Port","Version","Bandwidth","Authority","BadExit","Exit","Fast","Guard","HSDir","NoEdConsensus","Running","Stable","V2Dir","Valid")
col.type<-c(rep("character",4),"integer","character",rep("integer",12))

#Read additional data set with IPs belonging to Sybills
IPs.Sybill <- readLines("IpInfo.txt")

#Now let's run this analysis for all years:
for(year in seq(2008,2017)){
  msg<-glue("Year: ", year)
  logging(msg, outputfile=logFile) 
  
  setwd(dataDir)
  fileName<-glue("LogInfos",year,".txt")
  d<-read.table(fileName, header=FALSE, sep=";",quote="",  
                stringsAsFactors=FALSE,comment.char="",                   
                colClasses=col.type)
  names(d)<-header
  d$isSybill  <- ifelse(d$IP %in% IPs.Sybill,1,0)

  #######################################################
  #Aggregate observations from the same IP address
  #######################################################
 
  BadExit <- with(d, tapply(BadExit, IP, function(x){sum(x)>0})) 
  isSybill <- with(d, tapply(isSybill, IP, function(x){sum(x)>0})) 
  IP<-names(BadExit)
  #length(BadExit)==length(IP)
  predictors<-names(d)[-c(1:4,9,7)]
  for(predictor in predictors){
    eval(parse(text=glue(
      predictor," <- with(d, tapply(",predictor,", IP, function(x){names(sort(table(x),decreasing=TRUE)[1])}))"
    )))  
  }
  
  Bandwidth <- with(d, tapply(Bandwidth, IP, mean))
  num.observations <- with(d, tapply(Bandwidth, IP, length))
  
  d.aggregated<-data.frame(
    BadExit,
    isSybill,
    IP,
    Bandwidth,
    Port,
    Version,
    Authority,
    Exit,
    Fast,
    Guard,
    HSDir,
    NoEdConsensus,
    Running,
    Stable,
    V2Dir,
    Valid,
    num.observations)
    
  outputFileName<-glue("AggregatedDataSet",year,".txt")
  setwd(dataDir)
  write.table(d.aggregated, file=outputFileName,append=FALSE,col.names=FALSE)
  rm(d,d.aggregated);
}