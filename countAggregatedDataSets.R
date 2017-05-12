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

#Read additional data set with IPs belonging to Sybills
IPs.Sybill <- readLines("IPSybilLogInfo.txt")

#Read additional data set describing how often the server has changed his fingerprint:
lines<-readLines("CountFingerprints.txt")
trim.leading <- function (x)  sub("^\\s+", "", x)
getFirstColumn<-function(x){
  unlist(trim.leading(substr(trim.leading(x), 1, 2)))
} 
getSecondColumn<-function(x){
  unlist(substr(trim.leading(x), 3, 16))
} 
count <- as.numeric(sapply(lines,getFirstColumn))
IP <- as.character(sapply(lines,getSecondColumn))
d.fp<-data.frame(count,IP)

years<-seq(2008,2017)
N<-length(years)
num.obs<-rep(-1,N)
num.ips<-rep(-1,N)
num.ips.flag.bad<-rep(-1,N)
num.sybills<-rep(-1,N)
num.obs.fingerprint<-rep(-1,N)

#/data$ head AggregatedDataSet2008.txt 
#"113.128.138.3" FALSE "0" "113.128.138.3" 14.6153846153846 "9001" "0.2.0.32" "0" "1" "0" "0" "0" "0" "1" "0" "0" "1" 13
header<-c(
  "IP",
  "BadExit",
  "isSybill",
  "IP",
  "Bandwidth",
  "Port",
  "Version",
  "Authority",
  "Exit",
  "Fast",
  "Guard",
  "HSDir",
  "NoEdConsensus",
  "Running",
  "Stable",
  "V2Dir",
  "Valid",
  "num.observations")

#loop index
index<-1

#Now let's run this analysis for all years:
for(year in years){

  setwd(dataDir)
  fileName<-glue("AggregatedDataSet",year,".txt")
  d<-read.table(fileName, header=FALSE, sep=" ",  
                stringsAsFactors=FALSE,comment.char="")
  names(d)<-header
  d<-d[,-1]
  
  #set correct data type for inputs
  d$isSybill   <- as.factor(d$isSybill)
  d$Exit   <- as.factor(d$Exit)
  d$BadExit   <- as.factor(d$BadExit)
  d$Port      <- as.factor(d$Port)
  d$Version   <- as.factor(d$Version)
  d$Bandwidth <- as.numeric(d$Bandwidth)
  d$IP        <- as.factor(d$IP)
  d$Authority       <- as.factor(d$Authority)
  d$Fast       <- as.factor(d$Fast)
  d$Guard       <- as.factor(d$Guard)
  d$HSDir       <- as.factor(d$HSDir)
  d$NoEdConsensus   <- as.factor(d$NoEdConsensus)
  d$Running   <- as.factor(d$Running)
  d$Stable   <- as.factor(d$Stable)
  d$V2Dir   <- as.factor(d$V2Dir)
  d$Valid   <- as.factor(d$Valid)
  
  num.obs[index]<-sum(d$num.observations)
  num.ips[index]<-dim(d)[1]
  num.ips.flag.bad[index]<-length(d$BadExit[d$BadExit==TRUE])
  d$isSybill  <- as.factor(d$IP %in% IPs.Sybill)
  num.sybills[index]<-length(d$isSybill[d$isSybill==TRUE])
  #perform a join with the data set containing the information about change of fingerprints 
  d<-merge(d, d.fp, all.x=TRUE, by="IP")
  num.obs.fingerprint[index]<-sum(table(d$count))
  
  index <- index + 1
}

setwd(dataDir)
write.table(
  data.frame(
            years,
            num.obs,
            num.ips,
            num.ips.flag.bad,
            num.sybills,
            num.obs.fingerprint), 
  "timeSeries.txt",
  row.names = FALSE, sep =";")

  