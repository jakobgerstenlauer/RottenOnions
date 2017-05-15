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

#Automatically remove inputs without variance:
calculate.variance<-function(x){
  var(as.numeric(x), na.rm = TRUE)
}

maxN<-50000
min.num.bad.exit<- 100 
num.trees <- 5000

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

# variables to store the matrixes for the final outputs
glm.d<-NULL
glm.rownames<-c("Year", "Bandwidth", "Fast","Guard","HSDir","Stable","V2Dir","Num Observations","Null Deviance","Residual Deviance","R2")
glm.d<-cbind(glm.d,glm.rownames)

gbm.d<-NULL
gbm.rownames<-c("Year","Fast", "num.observations", "Bandwidth","V2Dir","Port","Version","Guard","Stable","HSDir")
gbm.d<-cbind(gbm.d,gbm.rownames)

check.significance<-function(x){
  return(ifelse(x!="",ifelse(x<0.05,"+","-"),"N/A"))
}

glm.create.frame<-function(inputmodel,year){
  p.values <- inputmodel$coefficients[,4]
  z<-c(year)
  z[2]=check.significance(p.values["Bandwidth"])
  z[3]=check.significance(p.values["Fast1"])
  z[4]=check.significance(p.values["Guard1"])
  z[5]=check.significance(p.values["HSDir1"])
  z[6]=check.significance(p.values["Stable1"])
  z[7]=check.significance(p.values["V2Dir1"])        
  z[8]=check.significance(p.values["num.observations"])
  z[9]=inputmodel$null.deviance
  z[10]=inputmodel$deviance 
  z[11]=100 * round(1-(inputmodel$deviance/inputmodel$null.deviance),3)
  return(z)
}

gbm.create.frame<-function(inputmodel,year,digits){
  z<-c(year)
  z[2]=round(inputmodel["Fast",]$rel.inf,digits)
  z[3]=round(inputmodel["num.observations",]$rel.inf,digits)
  z[4]=round(inputmodel["Bandwidth",]$rel.inf,digits)
  z[5]=round(inputmodel["V2Dir",]$rel.inf,digits)
  z[6]=round(inputmodel["Port",]$rel.inf,digits)
  z[7]=round(inputmodel["Version",]$rel.inf,digits)      
  z[8]=round(inputmodel["Guard",]$rel.inf,digits)
  z[9]=round(inputmodel["Stable",]$rel.inf,digits)
  z[10]=round(inputmodel["HSDir",]$rel.inf,digits)
  return(z)
}

#Now let's run this analysis for all years:
for(year in seq(2008,2017)){

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
  
  index.Exit<-which(names(d)=="Exit")
  #only select nodes which are configured as exit nodes
  d<-d[d$Exit==1,]
  
  #take a subsample of the observations which don't have the "BadExit" flag:
  indicesBad<-which(d$BadExit==TRUE)
  indicesGood<-which(d$BadExit==FALSE)
    
  if(length(indicesBad) > min.num.bad.exit){

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
    
  #Check factor port
  pc<-table(dx$Port)
  ports.restricted<-names(pc[pc>3])
  dx$Port <- as.factor(ifelse(dx$Port %in% ports.restricted, ports.restricted, "000"))
  numOfFactors<-length(levels(dx$Port))
  
  if(numOfFactors>1024){
    stop("There are more than 1024 factor levels / ports! Thus the influence of the ports has to be ignored.")
  }
    
  #Check factor version
  pc<-table(dx$Version)
  version.restricted<-names(pc[pc>3])
  dx$Version <- as.factor(ifelse(dx$Version %in% version.restricted, version.restricted, "000"))
  numOfFactors<-length(levels(dx$Version))
    
  if(numOfFactors>1024){
    stop("There are more than 1024 factor levels / versions!")
  }
  
  predictors<-names(dx)[-c(1:3,12,17)]
    
  for(predictor in predictors){
    #set correct data type for inputs
    if(mode(predictor)=="character"){
      eval(parse(text=glue(
        "dx$",predictor,"<-factor(dx$",predictor,")"
      )))
    }
  }
  dx$Bandwidth<-as.numeric(dx$Bandwidth)
  
  variances<-as.numeric(lapply(dx[,3:16], calculate.variance))
  positive.variances <-ifelse(is.na(variances),FALSE,variances>0.01)
  indices<-c(rep(TRUE,2), positive.variances, TRUE) 
  dx<-dx[,indices]
  
  #fit the full model without Version and Port
  index.Port<-which(names(d)=="Port")
  index.Version<-which(names(d)=="Version")
  index.IP<-which(names(d)=="IP")
  index.isSybill<-which(names(d)=="isSybill")
  
  m1.glm <- glm(BadExit ~ . , family= binomial(), data=dx[,-c(index.Port,index.Version,index.IP,index.isSybill)])
  
  setwd(dataDir)
  fileName<-glue("GLM_FullModel_Summary_Aggregated_",year,".txt")
  sink(fileName)
  print(summary(m1.glm))
  sink()  
  
  glm.d<-cbind(glm.d,glm.create.frame(summary(m1.glm),year))
  
  #************************************************************************************************
  #Fit gbm model
   require("gbm")
   m2.gbm <- gbm ((as.numeric(BadExit)-1) ~ . ,
                   distribution="bernoulli",
                   verbose=FALSE,
                   interaction.depth=3,#6
                   shrinkage=0.001,#0.001
                   n.trees = num.trees,#3000
                   data=dx[,-c(index.IP,index.isSybill)])

    ri<-summary(m2.gbm, plotit=FALSE)
    
    outputFileName<-glue("VariableImportanceBoostedRegressionTrees_BadExit_agg_",year,".txt")
    setwd(dataDir)
    write.table(ri, file=outputFileName,append=FALSE,col.names=FALSE)
    
    gbm.d<-cbind(gbm.d,gbm.create.frame(summary(m2.gbm, plotit=FALSE),year,1))
  }
}

# save tables to latex file
require(xtable)
colnames(glm.d)<- as.character(unlist(glm.d[1,]))
glm.d<-glm.d[-1,]

colnames(gbm.d)<- as.character(unlist(gbm.d[1,]))
gbm.d<-gbm.d[-1,]

setwd(dataDir)
print.xtable(xtable(glm.d), type="latex", file="glmtable.tex")
print.xtable(xtable(gbm.d), type="latex", file="gbmtable.tex")

#R2 negative? check the formula
