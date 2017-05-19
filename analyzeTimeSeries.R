#############################################################################################
#                 Analyze Time Series ratio for each year
#
#############################################################################################

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

d<-read.table("timeSeries.txt", header=TRUE,sep=";")
names(d)[1]<-"year"
with(d, plot(year,num.ips))

setwd(plotDir)

jpeg("ObservationsIPs.jpeg")
with(d,plot(year,num.obs/num.ips, 
            pch="+",type="b",
            xlab="Year",
            ylab="Number of observations / IP"))
dev.off()

jpeg("BadExitProportion.jpeg")
with(d,plot(year,num.ips.flag.bad/num.ips, 
            pch="+",type="b",
            xlab="Year",
            ylab="Ratio of nodes with \"BadExit\" flag"))
dev.off()

jpeg("NumSybils.jpeg")
with(d,plot(year,num.sybills, 
            pch="+",type="b",
            xlab="Year",
            ylab="Number of sybils"))
dev.off()

#Extraordinary peak of IPs with flag "BadExit" in 2012

with(d, plot(year,num.obs.fingerprint/num.ips))
with(d, plot(year,num.obs.fingerprint))
#Why are there so few IPs sampled in the files from which we extracted the fingerprints?

with(d, plot(year, num.obs))
#The number of observations per increases over time

with(d, plot(year, num.obs/num.ips))
#The number of observations /IP increases

with(d, plot(year, num.obs.fingerprint/num.obs))

with(d, plot(year, num.sybills))
with(d, plot(year,num.obs.fingerprint))

#scale to view all the different values in one plot
ds<-as.data.frame(scale(d[,-1]))
ds<-cbind(year=d$year,ds)

#we will use melt from reshape2 to create a better plot
require(ggplot2)
require(reshape2)
ds.melt<-melt(ds,id="year")

#function to add new values to the dataframe
calculate.value<-function(dataframe,operation){
  temp.dataframe<-NULL
  temp.dataframe$year<-dataframe$year
  temp.dataframe$variable<-as.factor(rep(deparse(substitute(operation)),nrow(dataframe)))
  temp.dataframe$value<-eval(parse(text=glue("with(",deparse(substitute(dataframe)),",",deparse(substitute(operation)),")")))#with(dataframe,operation)
  return(as.data.frame(temp.dataframe))
}

#finally we row join the values
ds.melt<-rbind(ds.melt,
      calculate.value(ds,num.ips.flag.bad/num.ips),
      calculate.value(ds,num.obs/num.ips),
      calculate.value(ds,num.obs.fingerprint/num.obs))

#drop values that are not needed in plot
ds.reduced<-(droplevels( ds.melt[-which(ds.melt$variable %in% c("num.ips.flag.bad","num.obs","num.ips","num.obs.known.fingerprint")), ] ))

#and the plot is grouped by variable
ggplot(ds.reduced, aes(x=year, y=value, color=variable)) + 
  geom_line(aes(linetype=variable), size=1) +
  geom_point(aes(shape=variable, size=4)) +
  scale_linetype_manual(values =sample(1:10,nlevels(ds.reduced$variable),replace=T)) +
  scale_shape_manual(values=sample(1:10,nlevels(ds.reduced$variable),replace=T)) 

levels(ds.melt$variable)

#drop values that are not needed in plot
ds.reduced<-(droplevels( ds.melt[-which(ds.melt$variable %in% c("num.obs.fingerprint/num.obs","num.ips.flag.bad","num.obs","num.ips","num.obs.known.fingerprint")), ] ))

#and the plot is grouped by variable
ggplot(ds.reduced, aes(x=year, y=value, color=variable)) + 
  geom_line(aes(linetype=variable), size=1) +
  geom_point(aes(shape=variable, size=4)) +
  scale_linetype_manual(values =sample(1:10,nlevels(ds.reduced$variable),replace=T)) +
  scale_shape_manual(values=sample(1:10,nlevels(ds.reduced$variable),replace=T)) 

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

jpeg("BadExitProportion.jpeg",width = 900,height = 600,units = "px",res = 150)
ggplot(d, aes(x=year, y=num.ips.flag.bad/num.ips))+
  geom_line(size=0.9,color=cbPalette[1])+
  geom_point(aes(shape=as.factor(d$year), size=4,color=as.factor(d$year))) +
  scale_shape_manual(values=sample(1:10,nlevels(as.factor(d$year))))+
  labs(x="Year", y="Ratio of nodes with \"BadExit\" flag",shape="Year", colour="Year") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12,face="bold"))+
  scale_x_continuous(breaks=d$year)
dev.off()

jpeg("NumSybils.jpeg",width = 900,height = 600,units = "px",res = 150)
ggplot(d, aes(x=year, y=num.sybills))+
  geom_line(size=0.9,color=cbPalette[5])+
  geom_point(aes(shape=as.factor(d$year), size=4,color=as.factor(d$year))) +
  scale_shape_manual(values=sample(1:10,nlevels(as.factor(d$year))))+
  labs(x="Year", y="Number of sybils",shape="Year", colour="Year") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12,face="bold"))+
  scale_x_continuous(breaks=d$year)
dev.off()
  
jpeg("ObservationsIPs.jpeg",width = 900,height = 600,units = "px",res = 150)
ggplot(d, aes(x=year, y=num.obs/num.ips))+
  geom_line(size=0.9,color=cbPalette[3])+
  geom_point(aes(shape=as.factor(d$year), size=4,color=as.factor(d$year))) +
  scale_shape_manual(values=sample(1:10,nlevels(as.factor(d$year))))+
  labs(x="Year", y="Number of observations / IP",shape="Year", colour="Year") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12,face="bold"))+
  scale_x_continuous(breaks=d$year)
dev.off()

# Cross information of IP from counted fingerprints and each year
setwd(dataDir)
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

d.fpy<-NULL
# Get all ips by year
for(year in seq(2008,2017)){
  
  setwd(dataDir)
  fileName<-glue("AggregatedDataSet",year,".txt")
  d<-read.table(fileName, header=FALSE, sep=" ",  
                stringsAsFactors=FALSE,comment.char="")
  names(d)<-header
  d<-d[,-1]
  
  d$IP<- as.factor(d$IP)
  d<-cbind.data.frame(IP=d$IP)
  
  d.aux<-merge(d,d.fp,by="IP")
  d.aux<-cbind(d.aux,year)
  d.fpy<-rbind(d.fpy,d.aux)
  
  # outputFileName<-glue("IPCountFingerPrintByYear.txt")
  # setwd(dataDir)
  # write.table(d.aux, file=outputFileName,append=TRUE,col.names=FALSE, row.names = F)
}

library(dplyr)
set.seed(1)
d.count.fpy<-d.fpy %>% group_by(year) %>% summarise("IP Count" = length(year))

setwd(plotDir)
jpeg("ObservationsFingerprintsYear.jpeg",width = 900,height = 600,units = "px",res = 150)
ggplot(d.count.fpy, aes(x=d.count.fpy$year, y=d.count.fpy$`IP Count`))+
  geom_line(size=0.9,color=cbPalette[3])+
  geom_point(aes(shape=as.factor(d.count.fpy$year), size=4,color=as.factor(d.count.fpy$year))) +
  scale_shape_manual(values=sample(1:10,nlevels(as.factor(d.count.fpy$year))))+
  labs(x="Year", y="Number of observations changed fingerprints",shape="Year", colour="Year") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12,face="bold"))+
  scale_x_continuous(breaks=d.count.fpy$year)
dev.off()

