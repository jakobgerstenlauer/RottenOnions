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

#create a logfile
logFile<-"ProcessAllYears.log"
if(!file.exists(logFile)){
  file.create(logFile)
}

#a logging function
#Warning: the logFile has to exist already!
logging<-function(logMessage, outputfile){
  oldPath<-getwd()
  setwd(logDir)
  stopifnot(is.character(logMessage))
  stopifnot(is.character(outputfile))
  stopifnot(file.exists(outputfile))
  cat(logMessage,file=outputfile,append=TRUE,fill=TRUE)
  setwd(oldPath)
}

#TODO Increase after first test run!
num.trees<-10

#Now let's run this analysis for all years:
for(year in seq(2008,2017)){
 
  msg<-glue("Year: ", year)
  logging(msg, outputfile=logFile) 
  
  fileName<-glue("LogInfos",year,".txt")
  header<-c("Date","Hour","Name","IP","Port","Version","Bandwidth","Authority","BadExit","Exit","Fast","Guard","HSDir","NoEdConsensus","Running","Stable","V2Dir","Valid")
  col.type<-c(rep("character",4),"integer","character",rep("integer",12))
  rm(d,d1,dfp)
  
  setwd(dataDir)
  d<-read.table(fileName, header=FALSE, sep=";",quote="",  
                stringsAsFactors=FALSE,comment.char="",                   
                colClasses=col.type)
  names(d)<-header
  
  #set correct data type for inputs
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
  
  bad.ips<-unique(d[d$BadExit==1,1])
  msg<-glue("The IPs with flag Bad Exit are: ")
  logging(msg, outputfile=logFile)
  msg<-bad.ips
  logging(msg, outputfile=logFile)
  
  #Plot the distribution of IP addresses: 
  setwd(plotDir)
  plotName<-glue("Histogram_IP_addresses_",year,".jpeg")
  jpeg(plotName)
  require("lattice")
  print(histogram(~log(table(d$IP))))
  dev.off()
  
  #perform a left join with the data set containing the information about change of fingerprints 
  d<-merge(d, d.fp, by="IP",all.x = TRUE)
  
  #subset of data with information about fingerprint changes
  d1<-d[!is.na(d$count),]
  
  dx<-d1[d1$count>1,]
  max.count<-max(dx$count)
  dx<-dx[dx$count==max.count,]
  most.suspicious.ports<-unique(dx$IP)
  
  msg<-glue("The maximum number of fingerprint changes is: ", 
            max.count)
  logging(msg, outputfile=logFile)
  
  logging("The port(s) with the maximum changes of fingerprints: ", outputfile=logFile) 
  logging(most.suspicious.ports, outputfile=logFile) 
  
  #Plot the distribution of fingerprint changes: 
  setwd(plotDir)
  plotName<-glue("Histogram_Fingerprint_Changes_",year,".jpeg")
  jpeg(plotName)
  require("lattice")
  print(histogram(~d1$count))
  dev.off()
  
  require("gbm")
  #fit the model only for subset of data with data about fingerprint changes
  m1.gbm <- gbm (count ~ . ,
                 distribution="poisson",
                 verbose=FALSE,
                 interaction.depth=3,
                 shrinkage=0.001,
                 n.trees = num.trees,
                 data=d1[,names(d)[-c(1:4,10:11,14)]])
  
  ri<-summary(m1.gbm)
  outputFileName<-glue("VariableImportanceBoostedRegressionTrees_",year,".txt")
  setwd(dataDir)
  write.table(ri, file=outputFileName,append=FALSE,col.names=FALSE)
  
  logging("The most important variables (var importance > 3%)for the prediction of suspicious IPs based on fingerprint changes are:",outputfile=logFile)
  logging(ri[ri$rel.inf>3 , 1], outputfile=logFile) 
  
  d1$predictions<-predict(m1.gbm, d1, n.trees=num.trees, type="response")
  
  #number of observations
  N<-dim(d1)[1]
 
  port.observations<-with(d1,tapply(predictions, Port, length))
  #Let's restrict the analysis to ports with at least 5 observations:
  ports<-(names(port.observations[port.observations>4]))
  ports<-ports[!is.na(ports)]
  dx<-subset(d1, Port %in% ports)
  suspicious.ports<-sort(with(dx, tapply(predictions, Port, mean)))
  n<-length(suspicious.ports)
  most.suspicious.ports<-suspicious.ports[seq((n-20),n)]
  #9080     9002    49000       21     9005    20017 
  #1.338371 1.418677 1.508181 1.510889 1.516973 1.529711 
  
  msg<-glue("The 10 most suspicious ports based on fingerprint changes (model results): ", names(most.suspicious.ports))
  logging(msg, outputfile=logFile) 
  
  bad.ips<-unique(d[d$BadExit==1,4])
  logging("IPs with bad exit flag : ", outputfile=logFile)
  logging(bad.ips, outputfile=logFile) 
  
  d.bad<-d[d$BadExit==1,]
  #eval(parse(text=glue("d_",year,"<-d.bad")))
  
  if(exists("d.bad.old")){
    d.bad.old<-rbind(d.bad.old,d.bad)  
  }else{
    d.bad.old<-d.bad  
  }
}

dim(d.bad.old)
#[1] 79272    18

summary(d.bad.old)

setwd(dataDir)
write.table(d.bad.old, "BadRelays.txt", row.names=FALSE, sep=";")

#require("lme4")
#m1.glmer <- glmer(formula = BadExit ~  Port + Version + Bandwidth + (1 | IP) ,
#                  family=binomial(link='logit'), data = d)
