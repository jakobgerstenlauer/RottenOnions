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
IPs.Sybill <- readLines("SybillIpList.txt")

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

setwd(logDir)
#create a logfile
logFile<-"ProcessAllYears.log"
if(!file.exists(logFile)){
  file.create(logFile)
}

#create a logfile
logFilePorts<-"SuspiciousPorts.log"
if(!file.exists(logFilePorts)){
  file.create(logFilePorts)
}

#create a logfile
logFileIPs<-"SuspiciousIPs.log"
if(!file.exists(logFileIPs)){
  file.create(logFileIPs)
}

#a logging function
#Warning: the logFile has to exist already!
logging<-function(logMessage, outputfile){
  oldPath<-getwd()
  setwd(logDir)
  if(!(is.character(logMessage))){
    logMessage<-as.character(logMessage)
  }
  stopifnot(is.character(outputfile))
  stopifnot(file.exists(outputfile))
  cat(logMessage,file=outputfile,append=TRUE,fill=TRUE)
  setwd(oldPath)
}

#TODO Set TRUE to run statistical analyses!
runStats <- FALSE

#TODO Increase after first test run!
num.trees <- 300

#set up a new data set that collects key infos for time series from 2017 to 2018
n <- length(seq(2008,2017))

#total number of ips flaged as bad exit
num.ips.flag.bad <- rep(-1, n)

#total number of ips with more than one fingerprint
num.ips.changing.fingerprint <- rep(-1, n)

#total number of observations
num.obs <- rep(-1, n)

#total number of observations with fingerprint info
num.obs.known.fingerprint <- rep(-1, n)

#total number of ips observed
num.ips <- rep(-1, n)

#total number of ips with known fingerprint
num.ips.known.fingerprint <- rep(-1, n)

#number of Sybills
num.sybills <- rep(-1, n)

header<-c("Date","Hour","Name","IP","Port","Version","Bandwidth","Authority","BadExit","Exit","Fast","Guard","HSDir","NoEdConsensus","Running","Stable","V2Dir","Valid")
col.type<-c(rep("character",4),"integer","character",rep("integer",12))

index<-1

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
  
  num.obs[index]<-dim(d)[1]
  num.ips[index]<-length(unique(d$IP))
  
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
  
  bad.ips<-unique(d[d$BadExit==1,"IP"])
  msg<-glue("The IPs with flag Bad Exit are: ")
  logging(msg, outputfile=logFile)
  msg<-as.character(bad.ips)
  logging(msg, outputfile=logFile)
  num.ips.flag.bad[index]<-length(bad.ips)
  
  d$isSybill  <- as.factor(d$IP %in% IPs.Sybill)
  index.ip<-which(names(d)=="IP")
  num.sybills[index] <- length(unique(d[d$isSybill,index.ip])) 
  gc()
  
  outputFileName<-glue("IPs_BadExit_",year,".txt")
  setwd(dataDir)
  write.table(bad.ips, file=outputFileName,append=FALSE,col.names=FALSE)
  
  #Plot the distribution of IP addresses: 
  setwd(plotDir)
  plotName<-glue("Histogram_IP_addresses_",year,".jpeg")
  jpeg(plotName)
  require("lattice")
  print(histogram(~log(table(d$IP))))
  dev.off()
  
  if(runStats){
    #logistic regression of BadExit
    if("BadExit" %in% names(d)){
      #Automatically remove inputs without variance:
      calculate.variance<-function(x){
        var(as.numeric(x), na.rm = TRUE)
      }
      variances<-as.numeric(lapply(d[,8:18], calculate.variance))
      positive.variances <-ifelse(is.na(variances),FALSE,variances>0)
      indices<-c(rep(TRUE,7), positive.variances) 
      d<-d[,indices]
      
      msg<-glue("Logistic regression models: ")
      logging(msg, outputfile=logFile)
      
      predictors<-names(d)[-c(1:4)]
      predictors<-predictors[predictors!="BadExit"]
      #There is a problem with this predictor (with the contrasts)
      predictors<-predictors[predictors!="NoEdConsensus"]
      
      #take a subsample of the observations which don't have the "BadExit" flag:
      indicesBad<-which(d$BadExit==1)
      indicesGood<-which(d$BadExit==0)
      
      #I want to have a balanced sample!
      #Therefore I retain all observations with the flag BadExit,
      #and I randomly sample the same number of observation from the subset 
      #of 
      N<-length(indicesBad)
      if(N > 20000){
        N <- 20000;
        indicesBad<-sample(indicesBad, size = N, replace = FALSE);
      }
      samplesGood <- sample(indicesGood, size = N, replace = FALSE);
      sampleIndices <- sort(c(samplesGood, indicesBad));
      
      bestPredictor <- predictors[1]
      bestAIC<- AIC(glm(BadExit ~ 1, family= binomial(), data=d[sampleIndices,]))
      
      for(predictor in predictors){
        eval(parse(text=glue(
          "m1.glm <- glm(BadExit ~ ",predictor," , family= binomial(), data=d[sampleIndices,])"
        )))
        aic<-AIC(m1.glm)
        msg<-glue(predictor," : AIC:",aic)
        logging(msg, outputfile=logFile)
        if(aic < bestAIC){
          bestAIC<-aic;
          bestPredictor <- predictor;
        }
      }
      FirstPredictor<-bestPredictor
      predictors<-predictors[predictors!=FirstPredictor]
      
      msg<-glue("Best predictor in round 1 is: ", FirstPredictor)
      logging(msg, outputfile=logFile)
      
      #Round 2
      for(predictor in predictors){
        eval(parse(text=glue(
          "m1.glm <- glm(BadExit ~ ",FirstPredictor," + ",predictor," , family= binomial(), data=d[sampleIndices,])"
        )))
        aic<-AIC(m1.glm)
        msg<-glue(predictor," : AIC:",aic)
        logging(msg, outputfile=logFile)
        if(aic < bestAIC){
          bestAIC<-aic;
          bestPredictor <- predictor;
        }
      }
      
      SecondPredictor<-bestPredictor
      predictors<-predictors[predictors!=SecondPredictor]
      
      msg<-glue("Best predictor in round 2 is: ", SecondPredictor)
      logging(msg, outputfile=logFile)
      
      #Round 3
      for(predictor in predictors){
        eval(parse(text=glue(
          "m1.glm <- glm(BadExit ~ ",FirstPredictor," + ",SecondPredictor," +",predictor," , family= binomial(), data=d[sampleIndices,])"
        )))
        aic<-AIC(m1.glm)
        msg<-glue(predictor," : AIC:",aic)
        logging(msg, outputfile=logFile)
        if(aic < bestAIC){
          bestAIC<-aic;
          bestPredictor <- predictor;
        }
      }
      
      ThirdPredictor<-bestPredictor
      predictors<-predictors[predictors!=ThirdPredictor]
      
      msg<-glue("Best predictor in round 3 is: ", ThirdPredictor)
      logging(msg, outputfile=logFile)
      
      #Round 4
      for(predictor in predictors){
        eval(parse(text=glue(
          "m1.glm <- glm(BadExit ~ ",FirstPredictor," + ",SecondPredictor," + ",ThirdPredictor," +",predictor," , family= binomial(), data=d[sampleIndices,])"
        )))
        aic<-AIC(m1.glm)
        msg<-glue(predictor," : AIC:",aic)
        logging(msg, outputfile=logFile)
        if(aic < bestAIC){
          bestAIC<-aic;
          bestPredictor <- predictor;
        }
      }
      
      #Write final model with four predictors to text output file:
      FourthPredictor<-bestPredictor
      eval(parse(text=glue(
        "m1.glm <- glm(BadExit ~ ",FirstPredictor," + ",SecondPredictor," + ",ThirdPredictor," +",FourthPredictor," , family= binomial(), data=d[sampleIndices,])"
      )))
      fileName<-glue("ModelSummary_",year,".txt")
      setwd(dataDir)
      sink(fileName)
      summary(m1.glm)
      sink()  
    }
  }
  
  #perform a join with the data set containing the information about change of fingerprints 
  d<-merge(d, d.fp, by="IP")
  gc();
  
  #subset of data with information about fingerprint changes
  d<-d[!is.na(d$count),]
  
  num.obs.known.fingerprint[index]<-dim(d)[1]
  num.ips.known.fingerprint[index]<-length(unique(d$IP))
  
  dx<-d[d$count>1,]
  num.ips.changing.fingerprint[index] <- length(unique(dx$IP))
  max.count<-max(dx$count)
  dx<-dx[dx$count==max.count,]
  most.suspicious.ips<-unique(dx$IP)
  rm(dx)
  
  msg<-glue("The maximum number of fingerprint changes is: ", 
            max.count)
  logging(msg, outputfile=logFile)
  
  logging("The port(s) with the maximum changes of fingerprints: ", outputfile=logFile) 
  logging(as.character(most.suspicious.ips), outputfile=logFile) 
  logging(as.character(most.suspicious.ips), outputfile=logFileIPs)
  
  #Plot the distribution of fingerprint changes: 
  setwd(plotDir)
  plotName<-glue("Histogram_Fingerprint_Changes_",year,".jpeg")
  jpeg(plotName)
  require("lattice")
  print(histogram(~d$count))
  dev.off()
  
  if(runStats){
    require("gbm")
    #check the number of factors of gbm, only 1024 are allowed!
    d$Port <- factor(d$Port)
    numOfFactors<-length(levels(d$Port))
    if(numOfFactors>1024){
      stop("There are more than 1024 factor levels / ports! Thus the influence of the ports has to be ignored.")
    }
    
    #fit the model only for subset of data with data about fingerprint changes
    tryCatch({
      m1.gbm <- gbm (count ~ . ,
                     distribution="poisson",
                     verbose=FALSE,
                     interaction.depth=3,
                     shrinkage=0.001,
                     n.trees = num.trees,
                     #ignore ports, because there are to many levels!
                     data=d[,names(d)[-c(1:4)]])
      ri<-summary(m1.gbm, plotit=FALSE)
      outputFileName<-glue("VariableImportanceBoostedRegressionTrees_Fingerprint_Count",year,".txt")
      setwd(dataDir)
      write.table(ri, file=outputFileName,append=FALSE,col.names=FALSE)
      
      logging("The most important variables (var importance > 3%)for the prediction of suspicious IPs based on fingerprint changes are:",outputfile=logFile)
      logging(as.character(ri[ri$rel.inf>3 , 1]), outputfile=logFile) 
      
      d$predictions<-predict(m1.gbm, d, n.trees=num.trees, type="response")
      rm(m2.gbm)
    }, error = function(e) {
      rm(m2.gbm);
    }, finally = {
      rm(m2.gbm);
    })
    
    #number of observations
    N<-dim(d)[1]
    
    port.observations<-with(d,tapply(predictions, Port, length))
    #Let's restrict the analysis to ports with at least 5 observations:
    ports<-(names(port.observations[port.observations>4]))
    ports<-ports[!is.na(ports)]
    dx<-subset(d, Port %in% ports)
    suspicious.ports<-sort(with(dx, tapply(predictions, Port, mean)))
    rm(dx)
    n<-length(suspicious.ports)
    most.suspicious.ports<-suspicious.ports[seq((n-20),n)]
    #9080     9002    49000       21     9005    20017 
    #1.338371 1.418677 1.508181 1.510889 1.516973 1.529711 
    
    logging("The 10 most suspicious ports based on fingerprint changes (model results): ",outputfile=logFile) 
    logging(names(most.suspicious.ports), outputfile=logFile) 
    rm(m1.gbm)
    
    #************************************************************************************************
    #if there is no variance in BadExit it is removed!
    if(exists("d$BadExit")){
      
      tryCatch({
        m2.gbm <- gbm (BadExit ~ . ,
                       distribution="bernoulli",
                       verbose=FALSE,
                       interaction.depth=3,#6
                       shrinkage=0.001,#0.001
                       n.trees = num.trees,#3000
                       data=d[1,names(d)[-c(1:4)]])
        
        ri<-summary(m2.gbm, plotit=FALSE)
        outputFileName<-glue("VariableImportanceBoostedRegressionTrees_BadExit",year,".txt")
        setwd(dataDir)
        write.table(ri, file=outputFileName,append=FALSE,col.names=FALSE)
        
        logging("The most important variables (var importance > 3%)for the prediction of BadExit IPs:",outputfile=logFile)
        logging(as.character(ri[ri$rel.inf>3 , 1]), outputfile=logFile) 
        
        d$predictions2<-predict(m2.gbm, d, n.trees=num.trees, type="response")
        rm(m2.gbm)
      }, error = function(e) {
        rm(m2.gbm);
      }, finally = {
        rm(m2.gbm);
      })
      
      port.observations<-with(d,tapply(predictions2, Port, length))
      #Let's restrict the analysis to ports with at least 5 observations:
      ports<-(names(port.observations[port.observations>4]))
      ports<-ports[!is.na(ports)]
      dx<-subset(d, Port %in% ports)
      suspicious.ports<-sort(with(dx, tapply(predictions, Port, mean)))
      rm(dx)
      n<-length(suspicious.ports)
      most.suspicious.ports<-suspicious.ports[seq((n-20),n)]
      
      logging("The 10 most suspicious ports based on BadExit flags (model results): ",outputfile=logFile) 
      logging(names(most.suspicious.ports), outputfile=logFile) 
    }
  }
    rm(d);gc();
    index<-index+1;
}

time.series<-data.frame(
year<-seq(2008,2017),
#total number of ips flaged as bad exit
num.ips.flag.bad,
#total number of ips with more than one fingerprint
num.ips.changing.fingerprint,
#total number of observations
num.obs,
#total number of observations with fingerprint info
 num.obs.known.fingerprint,
#total number of ips observed
num.ips,
#total number of ips with known fingerprint
num.ips.known.fingerprint,
#number of observations of sybills
num.sybills)
  
setwd(dataDir)
write.table(time.series, "timeSeries.txt", row.names=FALSE, sep=";")
  
plot(year,num.ips)
plot(year,num.ips.flag.bad/num.ips)
#extraordinary peak of IPs with flag "BadExit" in 2012
  
plot(year,num.ips.known.fingerprint/num.ips)
plot(year,num.obs/num.obs.known.fingerprint)
plot(year,num.obs.known.fingerprint/num.obs)
plot(year,num.sybills)
plot(year,num.ips.changing.fingerprint/num.ips.known.fingerprint)
plot(year,num.ips.changing.fingerprint)
plot(year,num.ips.changing.fingerprint/num.ips.known.fingerprint)
  