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

require(reshape2)
ds<-melt(ds,id="year")

ggplot(ds, aes(x=year, y=value, color=variable)) + 
  geom_line(aes(linetype=variable), size=1) +
  geom_point(aes(shape=variable, size=4)) +
  scale_linetype_manual(values = c(1,2,1,1,2,1,2)) +
  scale_shape_manual(values=c(0,1,2,3,4,5,6))

ggplot(ds, aes(x=year)) + 
  geom_line(aes(y = num.ips, color="num.ips")) +
  geom_line(aes(y = num.obs, color = "num.obs")) +
  geom_line(aes(y = num.ips.changing.fingerprint/num.ips.known.fingerprint, color="num.ips.changing.fingerprint/num.ips.known.fingerprint")) +
  geom_line(aes(y = num.ips.changing.fingerprint, color = "num.ips.changing.fingerprint"))+
  geom_line(aes(y = num.ips.flag.bad/num.ips, color = "num.ips.flag.bad/num.ips")) +
  geom_line(aes(y = num.ips.known.fingerprint, color = "num.ips.known.fingerprint")) +
  geom_line(aes(y = num.obs/num.ips, color = "num.obs/num.ips")) +
  geom_line(aes(y = num.obs.known.fingerprint/num.obs, color = "num.obs.known.fingerprint/num.obs")) +
  geom_line(aes(y = num.sybills, color = "num.sybills"))+
  scale_linetype_manual(values = c(1,2,1,1)) +
  scale_shape_manual(values=c(0,1,2,3))
