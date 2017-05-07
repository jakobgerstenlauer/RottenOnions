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

#work with data from 2008
year<-2008
fileName<-glue("LogInfos",year,".txt")
header<-c("Date","Hour","Name","IP","Port","Version","Bandwidth","Authority","BadExit","Exit","Fast","Guard","HSDir","NoEdConsensus","Running","Stable","V2Dir","Valid")
col.type<-c(rep("character",4),"integer","character",rep("integer",12))
d<-read.table(fileName, header=FALSE, sep=";",quote="",  
                              stringsAsFactors=FALSE,comment.char="",                   
                              colClasses=col.type)
names(d)<-header
dim(d)
#[1] 3762726      18
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

table(d$BadExit)
# 0       1 
# 3762358     368 

#How is the distribution of IP addresses? 
require("lattice")
histogram(~log(table(d$IP)))

(bad.ips<-unique(d[d$BadExit==1,4]))
#[1] "64.191.56.197"  "217.172.60.111"
#Wow, there were only 2 nodes which were labeled bad exit!

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
#perform a left join 
d<-merge(d, d.fp, by="IP",all.x = TRUE)
dim(d)
#[1] 3762726      20

table(d$count, useNA = "always")
#1       2       3       4       5       7    <NA> 
#2238837  117371   32378    4233    6155      18 1363817 

#There are 1363817 observations for which no information about changes of the fingerprint is available from the logfiles.
#I assume that these are short lived relays. Let's check this assumption:

d.missingFingerprintChange<-d[is.na(d$count),]
table(d$Stable)
# 0       1 
# 2209496 1553230
table(d.missingFingerprintChange$Stable)
# 0       1 
# 1028610  335207 

#**************************************************************************************************************
#Conclusion: 
#Yes, the assumption is true, the proportion of stable relays
#is much lower in the subset of relays with missing information about fingerprint changes
#compared with the whole data set!
#Still, based on the fact that we have only partial information about fingerprint changes,
#we should restrict the data set to those observations
#for which fingerprint change information is available if we 
#include fingerprint changes in our analysis .
#**************************************************************************************************************

#subset of data with information about fingerprint changes
d1<-d[!is.na(d$count),]
require("gbm")
num.trees<-400
#fit the model only for subset of data with data about fingerprint changes
m1.gbm <- gbm (count ~ . ,
               distribution="poisson",
               verbose=FALSE,
               interaction.depth=3,
               shrinkage=0.001,
               n.trees = num.trees,
               data=d1[,names(d)[-c(1:4,10:11,14)]])

(ri<-summary(m1.gbm))
# var    rel.inf
# Port           Port 68.9281325
# Version     Version 30.5450762
# V2Dir         V2Dir  0.5267913

#plot the predicted values of label 
plot(m1.gbm,  type="response", i.var = "Port")
plot(m1.gbm,  type="response", i.var = "Version")

#fit the model again but only with the selected features
m2.gbm <- gbm (count ~ . ,
               distribution="poisson",
               verbose=FALSE,
               interaction.depth=3,
               shrinkage=0.001,
               n.trees = num.trees,
               data=d1[,names(d)[c(5,6,19)]])

d1$predictions<-predict(m2.gbm, d1, n.trees=num.trees, type="response")

#number of observations
N<-dim(d1)[1]

#the ports with the highest predicted probabilities of being used by a malitious node: 
sort(with(d1,tapply(predictions, Port, mean)))  

port.observations<-with(d1,tapply(predictions, Port, length))
table(port.observations)
hist(log(port.observations))

#The problem is that for many ports there are only very few observations in the data set!
#Let's restrict the analysis to ports with at least 5 observations:
ports<-(names(port.observations[port.observations>4]))
ports<-ports[!is.na(ports)]
dx<-subset(d1, Port %in% ports)
suspicious.ports<-sort(with(dx, tapply(predictions, Port, mean)))
n<-length(suspicious.ports)
suspicious.ports[seq((n-5),n)]
#9080     9002    49000       21     9005    20017 
#1.338371 1.418677 1.508181 1.510889 1.516973 1.529711 





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
  msg<-glue("The IPs with flag Bad Exit are: ", 
            bad.ips)
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
  
  msg<-glue("The port(s) with the maximum changes of fingerprints: ", 
            most.suspicious.ports)
  logging(msg, outputfile=logFile) 
  
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
  write.table(ri, file=outputFileName,append=FALSE,col.names=FALSE)
  
  msg<-glue("The most important variables (var importance > 3%)for the prediction of suspicious IPs based on fingerprint changes are:",
            ri[ri$rel.inf>3 , 1])
  logging(msg, outputfile=logFile) 
  
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
  msg<-glue("IPs with bad exit flag : ", bad.ips)
  logging(msg, outputfile=logFile) 
  
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
