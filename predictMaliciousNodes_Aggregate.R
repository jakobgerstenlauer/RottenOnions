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

uniq.length<-function(x){
  length(unique(x))
}

#Automatically remove inputs without variance:
calculate.variance<-function(x){
  var(as.numeric(x), na.rm = TRUE)
}

#Read additional data set with IPs belonging to Sybills
IPs.Sybill <- readLines("IpInfo.txt")

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

#This parameter determines the maximum sample size of nodes with flag BadExit when running the glm.
#The total sample sample size is 2 times maxN because we take the same number of observations from the subset of 
#nodes without the flag.
#TODO increase to 40000
maxN<-10000;

#TODO Set TRUE to run statistical analyses!
runStats <- TRUE

#TODO Increase after first test run!
num.trees <- 1000

#set up a new data set that collects key infos for time series from 2017 to 2018
n <- length(seq(2008,2017))

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
    Valid)
    
  dim(d.aggregated)
  #[1] 110128     16
  
  rm(d)
  d<-d.aggregated
  
  if(runStats){
    variances<-as.numeric(lapply(d[,8:18], calculate.variance))
    positive.variances <-ifelse(is.na(variances),FALSE,variances>0.01)
    indices<-c(rep(TRUE,7), positive.variances) 
    #keep "BadExit"!
    index.BadExit<-which(names(d)=="BadExit")
    indices[index.BadExit]<-TRUE
    d<-d[,indices]
    
    predictors<-names(d)[-c(1:6)]
    predictors<-predictors[predictors!="BadExit"]
    #There are problems with these predictors (with the contrasts)
    predictors<-predictors[predictors!="NoEdConsensus"]
    predictors<-predictors[predictors!="isSybill"]
    
    #take a subsample of the observations which don't have the "BadExit" flag:
    indicesBad<-which(d$BadExit==1)
    indicesGood<-which(d$BadExit==0)
    
    #I want to have a balanced sample!
    #Therefore I retain all observations with the flag BadExit,
    #and I randomly sample the same number of observation from the subset 
    #of 
    N<-length(indicesBad)
    if(N > maxN){
      N <- maxN;
      indicesBad<-sample(indicesBad, size = N, replace = FALSE);
    }
    samplesGood <- sample(indicesGood, size = N, replace = FALSE);
    sampleIndices <- sort(c(samplesGood, indicesBad));
    
    #create subset for modelling of BadExit flag
    dx<-d[sampleIndices,]
    
    #Repeat: Automatically remove inputs without variance:
    variances<-as.numeric(lapply(dx[,-(1:6)], calculate.variance))
    positive.variances <-ifelse(is.na(variances),FALSE,variances>0.01)
    indices<-c(rep(TRUE,6), positive.variances) 
    dx<-dx[,indices]
    
    #Check factor port
    pc<-with(dx, tapply(IP,Port,uniq.length))
    port.num.restricted<-length(pc[pc>2]);
    ports.restricted<-names(pc[pc>2])
    dx$Port <- as.factor(ifelse(dx$Port %in% ports.restricted, ports.restricted, "000"))
    numOfFactors<-length(levels(dx$Port))
    
    if(numOfFactors>1024){
      stop("There are more than 1024 factor levels / ports! Thus the influence of the ports has to be ignored.")
    }
    
    #Check factor version
    pc<-with(dx, tapply(IP,Version,uniq.length))
    version.num.restricted<-length(pc[pc>2]);
    version.restricted<-names(pc[pc>2])
    dx$Version <- as.factor(ifelse(dx$Version %in% version.restricted, version.restricted, "000"))
    numOfFactors<-length(levels(dx$Version))
    
    if(numOfFactors>1024){
      stop("There are more than 1024 factor levels / versions!")
    }
    
    predictors<-names(dx)[-c(1:4)]
    predictors<-predictors[predictors!="BadExit"]
    #There are problems with these predictors (with the contrasts)
    predictors<-predictors[predictors!="NoEdConsensus"]
    predictors<-predictors[predictors!="isSybill"]
    
    for(predictor in predictors){
      #set correct data type for inputs
      if(mode(predictor)=="character"){
        eval(parse(text=glue(
          "dx$",predictor,"<-factor(dx$",predictor,")"
        )))
      }
    }
    dx$Bandwidth<-as.numeric(dx$Bandwidth)
    
    #logistic regression of BadExit
    if("BadExit" %in% names(dx)){
      
      msg<-glue("Logistic regression models: ")
      logging(msg, outputfile=logFile)
      
      bestPredictor <- predictors[1]
      bestAIC<- AIC(glm(BadExit ~ 1, family= binomial(), data=dx))
      
      #fit the full model without Version and Port
      index.Port<-which(names(d)=="Port")
      index.Version<-which(names(d)=="Version")
      
      #m1.glm <- glm(BadExit ~ . , family= binomial(), data=dx[,-c(1:4)])
      m1.glm <- glm(BadExit ~ . , family= binomial(), data=dx[,-c(1:4,index.Port,index.Version)])
      
      fileName<-glue("GLM_FullModel_Summary_",year,".txt")
      setwd(dataDir)
      sink(fileName)
      summary(m1.glm)
      sink()  
      
      #############################################
      #Aggregate all observations with the same IP
      #############################################
      #require(lme4)
      #m1.lme <- lme(BadExit ~ Bandwidth + Fast + Guard + HSDir + Stable + V2Dir1 + (1|IP), family= binomial(), data=dx[,-c(1:4,index.Port,index.Version)])
      
      for(predictor in predictors){
        eval(parse(text=glue(
          "m1.glm <- glm(BadExit ~ ",predictor," , family= binomial(), data=dx)"
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
          "m1.glm <- glm(BadExit ~ ",FirstPredictor," + ",predictor," , family= binomial(), data=dx)"
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
          "m1.glm <- glm(BadExit ~ ",FirstPredictor," + ",SecondPredictor," +",predictor," , family= binomial(), data=dx)"
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
          "m1.glm <- glm(BadExit ~ ",FirstPredictor," + ",SecondPredictor," + ",ThirdPredictor," +",predictor," , family= binomial(), data=dx)"
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
        "m1.glm <- glm(BadExit ~ ",FirstPredictor," + ",SecondPredictor," + ",ThirdPredictor," +",FourthPredictor," , family= binomial(), data=dx)"
      )))
      fileName<-glue("ModelSummary_",year,".txt")
      setwd(dataDir)
      sink(fileName)
      summary(m1.glm)
      sink()  
      
      #************************************************************************************************
      #Fit gbm model
      
      require("gbm")
      
      
        tryCatch({
          m2.gbm <- gbm (BadExit ~ . ,
                         distribution="bernoulli",
                         verbose=FALSE,
                         interaction.depth=3,#6
                         shrinkage=0.001,#0.001
                         n.trees = num.trees,#3000
                         data=dx)
          
          ri<-summary(m2.gbm, plotit=FALSE)
          outputFileName<-glue("VariableImportanceBoostedRegressionTrees_BadExit",year,".txt")
          setwd(dataDir)
          write.table(ri, file=outputFileName,append=FALSE,col.names=FALSE)
          
          logging("The most important variables (var importance > 3%)for the prediction of BadExit IPs:",outputfile=logFile)
          logging(as.character(ri[ri$rel.inf>3 , 1]), outputfile=logFile) 
          
          dx$predictions2<-predict(m2.gbm, d, n.trees=num.trees, type="response")
          rm(m2.gbm)
          
          suspicious.ports<-sort(with(dx, tapply(predictions2, Port, mean)))
          rm(dx)
          n<-length(suspicious.ports)
          most.suspicious.ports<-suspicious.ports[seq((n-20),n)]
          
          logging("The 10 most suspicious ports based on BadExit flags (model results): ",outputfile=logFile) 
          logging(names(most.suspicious.ports), outputfile=logFile)
        }, error = function(e) {
          
        }, finally = {
          
        })
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
