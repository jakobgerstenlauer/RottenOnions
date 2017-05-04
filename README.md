# RottenOnions
Find malicious Tor exit nodes.


## Downloading and Processing the Data
### Data Description
The data was downloaded from the [CollecTor] (https://collector.torproject.org/) website of the Torproject. 


https://collector.torproject.org/recent/relay-descriptors/consensuses/2017-05-03-19-00-00-consensus


r seele AAoQ1DAR6kkoo19hBAX5K0QztNw vCMD4ZbahM/MbQvO23jaxuafqFE 2017-05-03 03:10:12 50.174.62.220 9001 0
s Running Stable V2Dir Valid
v Tor 0.2.9.10
pr Cons=1-2 Desc=1-2 DirCache=1 HSDir=1 HSIntro=3 HSRend=1-2 Link=1-4 LinkAuth=1 Microdesc=1-2 Relay=1-2
w Bandwidth=23
p reject 1-65535

r 
"name": seele
date: 2017-05-03 03:10:12
IP adress: 50.174.62.220
port of host: 9001

s
Flags: Running Stable V2Dir Valid

v
Tor version: 0.2.9.10

w
Bandwidth:23

Archive data:
https://collector.torproject.org/archive/relay-descriptors/consensuses/
Contains data from 2007-10 to 2017-05 with 20-30 MB per file

```bash
for year in {2007..2017}; do
	for month in 01 02 03 04 05 06 07 08 09 10 11 12; do
		wget --reject "index.html*" --no-parent --no-host-directories https://collector.torproject.org/archive/relay-descriptors/consensuses/consensuses-${year}-${month}.tar.xz		
		tar -xpvf consensuses-${year}-${month}.tar.xz --to-stdout | ./extractLogInfos.awk >> LogInfos.txt
		rm consensuses-${year}-${month}.tar.xz
	done;
done;
```

#Split the file into smaller files (for each year)
data$ ./SplitLogInfos.awk LogInfos.txt 
