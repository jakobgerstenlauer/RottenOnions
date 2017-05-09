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
