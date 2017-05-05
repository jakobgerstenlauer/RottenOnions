# RottenOnions
Find malicious Tor exit nodes.


## Downloading and Processing the Data
### Data Description
Data was downloaded from the [CollecTor] (https://collector.torproject.org/) website of the Tor project.
It describes Tor relay servers and was gather over eleven years from 2007 to 2017. 

A glossary of Tor terminology can be found here: https://gitweb.torproject.org/torspec.git/tree/glossary.txt

To understand the structure of the data, let's have a look at the following recent log file:
https://collector.torproject.org/recent/relay-descriptors/consensuses/2017-05-03-19-00-00-consensus

The following six lines describe one relay server at a given point in time:

r seele AAoQ1DAR6kkoo19hBAX5K0QztNw vCMD4ZbahM/MbQvO23jaxuafqFE 2017-05-03 03:10:12 50.174.62.220 9001 0

s Running Stable V2Dir Valid

v Tor 0.2.9.10

pr Cons=1-2 Desc=1-2 DirCache=1 HSDir=1 HSIntro=3 HSRend=1-2 Link=1-4 LinkAuth=1 Microdesc=1-2 Relay=1-2

w Bandwidth=23

p reject 1-65535

The first line, starting with "r" includes a descriptive name (element 2), the date (element 5), the hour (element 6), the IP  address (element 7) and the port used by the server (element 8).

r 
"name": seele
date: 2017-05-03 03:10:12
IP adress: 50.174.62.220
port of host: 9001

The second line, starting with "s",contains a list of valid flags. A list of possible flags can be retrieved from the header of the lofg  file.

s Running Stable V2Dir Valid

The third line, starting with "v", contains the version of the Tor software running on the server.

v Tor version: 0.2.9.10

The fourth line, starting with "w", contains the bandwidth in kB of the server.

w Bandwidth=23

The sixth line contains additional concerning the "exit policy" of the relay. Here it is specified which ports (and thus associated internet protocols) can be used for outgoing traffic.

The archived data was obtained from the following folder:
https://collector.torproject.org/archive/relay-descriptors/consensuses/
The data was collected from 2007-10 to 2017-05 with 20-30 MB per file.

### Data Processing


Awk-script that extracts the essential information from the original logfiles:
```awk
#!/usr/bin/awk -f
BEGIN{
FS=" ";
OFS=";";
}

{

if($1=="r"){
        Name=$2;
        Date=$5;
        Hour=$6;
        IP=$7;
        Port=$8;
}

if($1=="s"){
        Authority=0;
        if ( $0 ~ /Authority/ ){Authority=1}
        BadExit=0;
        if ( $0 ~ /BadExit/ ){BadExit=1}
        Exit=0;
        if ( $0 ~ /Exit/ ){Exit=1}
        Fast=0;
        if ( $0 ~ /Fast/ ){Fast=1}
        Guard=0;
        if ( $0 ~ /Guard/ ){Guard=1}
        HSDir=0;
        if ( $0 ~ /HSDir/ ){HSDir=1}
        NoEdConsensus=0;
        if ( $0 ~ /NoEdConsensus/ ){NoEdConsensus=1}
        Running=0;
        if ( $0 ~ /Running/ ){Running=1}
        Stable=0;
        if ( $0 ~ /Stable/ ){Stable=1}
        V2Dir=0;
        if ( $0 ~ /V2Dir/ ){V2Dir=1}
        Valid=0;        
        if ( $0 ~ /Valid/ ){Valid=1}
}

if($1=="v"){
        Version=$3;
}

if($1=="w"){ 
        search="="
        split($2,array,search);
        Bandwidth=array[2]; 
} 

if($1=="p"){ 
        printf("%s;%s;%s;%s;%d;%s;%d;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s\n",Date,Hour,Name,IP,Port,Version,Bandwidth,Authority,BadExit,Exit,Fast,Guard,HSDir,NoEdConsensus,Running,Stable,V2Dir,Valid)
}
```
                      
Awk-script that splits the data into separate files (for each year one file):
```awk
#!/usr/bin/awk -f
BEGIN{
FS=";";
OFS=";";
}

{
        year = substr($0,1,4);
        print $0 >> "LogInfos" year ".txt"
}
```
Running everything together in the bash shell: 

```bash
for year in {2007..2017}; do
	for month in 01 02 03 04 05 06 07 08 09 10 11 12; do
		wget --reject "index.html*" --no-parent --no-host-directories https://collector.torproject.org/archive/relay-descriptors/consensuses/consensuses-${year}-${month}.tar.xz		
		tar -xpvf consensuses-${year}-${month}.tar.xz --to-stdout | ./extractLogInfos.awk >> LogInfos.txt
		rm consensuses-${year}-${month}.tar.xz
	done;
done;
#Split the file into smaller files (for each year)
data$ ./SplitLogInfos.awk LogInfos.txt 
```
Extract the exit policy:
```bash
for year in {2007..2017}; do
	for month in 01 02 03 04 05 06 07 08 09 10 11 12; do
		wget --reject "index.html*" --no-parent --no-host-directories https://collector.torproject.org/archive/relay-descriptors/consensuses/consensuses-${year}-${month}.tar.xz		
		tar -xpvf consensuses-${year}-${month}.tar.xz --to-stdout | ./extractExitPolicy.awk 
		rm consensuses-${year}-${month}.tar.xz
	done;
done;
```

### Extracting the Fingerprints
In order to extract the fingerprints of relay servers we have to access the archive files at:
https://collector.torproject.org/archive/relay-descriptors/server-descriptors/



Running everything together in the bash shell: 

```bash
for year in {2007..2017}; do
	for month in 01 02 03 04 05 06 07 08 09 10 11 12; do
		wget --reject "index.html*" --no-parent --no-host-directories https://collector.torproject.org/archive/relay-descriptors/server-descriptors/server-descriptors-${year}-${month}.tar.xz		
		tar -xpvf server-descriptors-${year}-${month}.tar.xz --to-stdout | ./extractFingerprint.awk >> LogFingerprints.txt
		rm server-descriptors-${year}-${month}.tar.xz
	done;
done;
```
