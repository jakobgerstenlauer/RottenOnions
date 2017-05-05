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
d$BadExit   <- as.factor(d$BadExit)
d$Port      <- as.factor(d$Port)
d$Version   <- as.factor(d$Version)
d$Bandwidth <- as.numeric(d$Bandwidth)
d$IP        <- as.factor(d$IP)

#How is the distribution of IP addresses? 
require("lattice")
histogram(~log(table(d$IP)))

(bad.ips<-unique(d[d$BadExit==1,4]))
#[1] "64.191.56.197"  "217.172.60.111"
#Wow, there were only 2 nodes which were labeled bad exit!

#Now let's run this analysis for all years:
for(year in seq(2008,2017)){

  fileName<-glue("LogInfos",year,".txt")
  header<-c("Date","Hour","Name","IP","Port","Version","Bandwidth","Authority","BadExit","Exit","Fast","Guard","HSDir","NoEdConsensus","Running","Stable","V2Dir","Valid")
  col.type<-c(rep("character",4),"integer","character",rep("integer",12))
  setwd(dataDir)
  d<-read.table(fileName, header=FALSE, sep=";",quote="",  
                stringsAsFactors=FALSE,comment.char="",                   
                colClasses=col.type)
  
  names(d)<-header
  d$BadExit   <- as.factor(d$BadExit)
  d$Port      <- as.factor(d$Port)
  d$Version   <- as.factor(d$Version)
  d$Bandwidth <- as.numeric(d$Bandwidth)
  d$IP        <- as.factor(d$IP)

  #How is the distribution of IP addresses? 
  setwd(plotDir)
  plotName<-glue("Histogram_IP_addresses_",year,".jpeg")
  jpeg(plotName)
  require("lattice")
  print(histogram(~log(table(d$IP))))
  dev.off()
  
  bad.ips<-unique(d[d$BadExit==1,4])
  print(bad.ips)
  
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