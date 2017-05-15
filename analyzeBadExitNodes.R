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

#open local file just to analyze
d <- read.table("BadRelays.txt", sep = ";", header = TRUE)
# names
# [1] "Date"          "Hour"          "Name"          "IP"            "Port"          "Version"       "Bandwidth"    
# [8] "Authority"     "BadExit"       "Exit"          "Fast"          "Guard"         "HSDir"         "NoEdConsensus"
# [15] "Running"       "Stable"        "V2Dir"         "Valid" 

#TODO Question: What does this function do? I don't see a plot but it is also no storing a plot to a file.
draw.plot<-function(input.data,type){
  require(ggplot2)
  require(gridExtra)
  
  l.data<-length(input.data)
  rounded<-round(sqrt(l.data),0)
  par(mar=c(3,3,2,2))
  par(mfrow=c(rounded-1, rounded+1))
  # for(i in 1:l.data){
  out.plot<-array(dim = l.data)
  #   eval(parse(text=glue(type,"(input.data[,i],main = names(input.data)[i])")))}
  # Note: some of the values are negative, so in that case the credit with the positive values is used
  switch(type,
         histogram={for(i in 1:l.data){hist(input.data[,i],main = names(input.data)[i],prob=TRUE);lines(density(input.data[,i]),col="blue", lwd=2)}},
         # histogram={out.plot <- lapply(1:14, function(i) ggplot(data=input.data, aes(input.data[,i])) +
         #                                 geom_histogram(aes(y =..density..),breaks=seq(20, 50, by = 2),col="red",fill="green",alpha = .2) +
         #                                 geom_density(col=i) +labs(title=names(input.data)[i],x=element_blank()))},
         plot={for(i in 1:l.data){plot(input.data[,i],main = names(input.data)[i])}},
         stop("Valid plot types are 'histogram', 'plot'"))
  #marrangeGrob(input.data, nrow=rounded, ncol=rounded)
  #set values to default
  par(mar= c(5, 4, 4, 2))
  par(mfrow=c(1,1))
}

draw.plot(d[,sapply(d, is.numeric)],"histogram")
#from the plot we can see that port around 10000 has something to do with bad nodes, we will check this

require(plyr)
require(xtable)
head(arrange(as.data.frame(table(d$Port)),desc(as.data.frame(table(d$Port))$Freq)), n = 10)
# just 8 ports are used by bad exit nodes, being the most frequent one 9001 and 443 
# Explanation:
# from https://trac.torproject.org/projects/tor/wiki/doc/ReducedExitPolicy talking about a configuration of an 
# exit node with reduced policy : "It should be noted that to avoid Tor DNSBL an exit nodes ORPort and/or DirPort 
# must not use the 'default' ports 9001 or 9030. If your computer isn't running a webserver, and you haven't set 
# AccountingMax, please consider changing your ORPort to 443 and/or your DirPort to 80."
# So 443 is stated as a configuration alternative, probably this is  the case for this as it is the port
# for https.
# TODO Question: What does this mean in practical terms for our research?

# read file "taggedips.txt" to get how many countries are marked as bad exit relays according from sybil hunter data
dc<-read.table("SybillIpListCountry.txt",sep = " ")
colnames(dc) <- c("IP Address","Country")
freq.countries<-head(arrange(as.data.frame(table(dc$Country)),desc(as.data.frame(table(dc$Country))$Freq)), n = 10)
print.xtable(xtable(freq.countries), type="latex", file="freq.sybil.countries.tex",caption= "Frequency of countries reported in SybilHunter")
# Var1 Freq
# 1    HR  253
# 2    RU  206
# 3    US  172
# 4    NL  134
# 5    SI  116
# 6    DE   68
# 7    ES   58
# 8    GB   24
# 9    BA   16
# 10   FR   14
# Top ten countries marked in sybil hunter
