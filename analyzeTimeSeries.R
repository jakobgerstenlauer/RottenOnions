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
setwd(codeDir)

d<-read.table("timeSeries.txt", header=TRUE,sep=";")
names(d)[1]<-"year"
with(d, plot(year,num.ips))
with(d,plot(year,num.ips.flag.bad/num.ips))
#Extraordinary peak of IPs with flag "BadExit" in 2012

with(d, plot(year,num.ips.known.fingerprint/num.ips))
with(d, plot(year,num.ips.known.fingerprint))
#Why are there so few IPs sampled in the files from which we extracted the fingerprints?

with(d, plot(year, num.obs))
#The number of observations per increases over time

with(d, plot(year, num.obs/num.ips))
#The number of observations /IP increases

with(d, plot(year,num.obs.known.fingerprint/num.obs))

with(d, plot(year, num.sybills))
#Here s.th is wrong, there must be more Sybills!
#TODO Check numbers!

with(d, plot(year,num.ips.changing.fingerprint/num.ips.known.fingerprint))
with(d, plot(year,num.ips.changing.fingerprint))

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
ds.melt<-rbind(ds.melt,calculate.value(ds,num.ips.changing.fingerprint/num.ips.known.fingerprint),
      calculate.value(ds,num.ips.flag.bad/num.ips),
      calculate.value(ds,num.obs/num.ips),
      calculate.value(ds,num.obs.known.fingerprint/num.obs))

#drop values that are not needed in plot
ds.reduced<-(droplevels( ds.melt[-which(ds.melt$variable %in% c("num.ips.flag.bad","num.obs","num.ips","num.obs.known.fingerprint")), ] ))

#and the plot is grouped by variable
ggplot(ds.reduced, aes(x=year, y=value, color=variable)) + 
  geom_line(aes(linetype=variable), size=1) +
  geom_point(aes(shape=variable, size=4)) +
  scale_linetype_manual(values =sample(1:10,nlevels(ds.reduced$variable),replace=T)) +
  scale_shape_manual(values=sample(1:10,nlevels(ds.reduced$variable),replace=T)) 
