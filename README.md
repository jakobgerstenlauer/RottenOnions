# RottenOnions
In this project, we use archive data from the Tor project to build predictive models for malicious Tor relays.

### Background Information
The Tor network is increasingly used by privacy aware Internet users around the world. Especially journalists, whistle-blowers and NGOs use the network to protect their communications from government surveillance. In some countries, such as China, were the state restricts his subjects access to the world wide web using DNS poisoning, access to the Tor network via so-called *bridges*, i.e. Tor nodes that are not public and thus cannot be blocked by governments, enables users to circumvent government restrictions and explore the wider Internet. The Tor network's primary objective is to provide anonymity to Internet users (i.e. clients) and content providers (i.e. servers) via hidden-services. It is however, important to note that there is a fundamental difference between anonymity and privacy, and that anonymity without privacy is often worthless, because transmitted data may contain personal information that enables snooping organizations to identify users. Traffic in the Tor network is only encrypted until it reaches the exit node. If the user does not use end-to-end encryption, the exit node and all downstream machines are able to read the clear-text of the packets sent. As a result, the Tor protocol does not provide privacy and it is thus of paramount importance for the network to detect malicious exit nodes. In this work, we will we will integrate and analyse data from the Tor archive and previous research projects. Our objective is to build statistical models predicting malicious nodes in the Tor network.    

## Data Gathering and Processing
### List of Data Sources
The following public data sources were used:
- [Tor consensus files](https://collector.torproject.org/archive/relay-descriptors/consensuses/)
- [Tor server descriptor files](https://collector.torproject.org/archive/relay-descriptors/server-descriptors/)
- [Sybill files: sybil-groups.tar.bz2](https://nymity.ch/sybilhunting/)

The first two sources describes Tor relay servers and was gathered over eleven years from 2007 to 2017.
The last data source is a research project that was focused on hunting Sybills, i.e. secret groups of Tor nodes run by a single adversary. A glossary of Tor terminology can be found [here](https://gitweb.torproject.org/torspec.git/tree/glossary.txt).

### Processing of Tor Consensus Files

#### File Structure
To understand the structure of the data, let's have a look at the following recent log file:
https://collector.torproject.org/recent/relay-descriptors/consensuses/2017-05-03-19-00-00-consensus

The following six lines describe one relay server at a given point in time:

```
r seele AAoQ1DAR6kkoo19hBAX5K0QztNw vCMD4ZbahM/MbQvO23jaxuafqFE 2017-05-03 03:10:12 50.174.62.220 9001 0

s Running Stable V2Dir Valid

v Tor 0.2.9.10

pr Cons=1-2 Desc=1-2 DirCache=1 HSDir=1 HSIntro=3 HSRend=1-2 Link=1-4 LinkAuth=1 Microdesc=1-2 Relay=1-2

w Bandwidth=23

p reject 1-65535
```


The first line of each record, starts with "r" and has the following pattern (SP means space, NL stands for newline):

SP nickname SP identity SP digest SP publication SP IP SP ORPort SP DirPort NL

"Nickname" is the OR's nickname.  

"Identity" is a hash of its identity key, encoded in base64, with trailing equals sign(s) removed.  

"Digest" is a hash of its most recent descriptor as signed (that is, not including the signature), encoded in base64.

"Publication" is the publication time of its most recent descriptor, in the form YYYY-MM-DD HH:MM:SS, in UTC.  

"IP" is its current IP address; 

"ORPort" is its current OR port, "

"DirPort" is its current directory port, or "0" for "none".

Source???

The second line, starting with "s",contains a list of valid flags. 

A series of space-separated status flags, in lexical order (as ASCII byte strings).  
Currently documented flags are:

"Authority" if the router is a directory authority.

"BadExit" if the router is believed to be useless as an exit node (because its ISP censors it, because it is behind a restrictive proxy, or for some similar reason).

"Exit" if the router is more useful for building general-purpose exit circuits than for relay circuits.  The path building algorithm uses this flag; see path-spec.txt.

"Fast" if the router is suitable for high-bandwidth circuits.

"Guard" if the router is suitable for use as an entry guard.

"HSDir" if the router is considered a v2 hidden service directory.

"NoEdConsensus" if any Ed25519 key in the router's descriptor or microdesriptor does not reflect authority consensus.

"Stable" if the router is suitable for long-lived circuits.

"Running" if the router is currently usable. Relays without this flag are omitted from the consensus, and current clients (since 0.2.9.4-alpha) assume that every listed relay has this flag.

"Valid" if the router has been 'validated'. Clients before 0.2.9.4-alpha would not use routers without this flag by default. Currently, relays without this flag are omitted fromthe consensus, and current (post-0.2.9.4-alpha) clients assume that every listed relay has this flag.

"V2Dir" if the router implements the v2 directory protocol or higher.

Source: ??? 

The third line, starting with "v", contains the version of the Tor software running on the server.

v Tor version: 0.2.9.10

The fourth line, starting with "w", contains the bandwidth in kB of the server.

w Bandwidth=23

The sixth line contains additional concerning the "exit policy" of the relay. Here it is specified which ports (and thus associated internet protocols) can be used for outgoing traffic.

#### Data Processing


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
		tar -xpvf server-descriptors-${year}-${month}.tar.xz --to-stdout | ./extractFingerprint.awk | sort -k1,13 | uniq  >> LogFingerprints.txt
		rm server-descriptors-${year}-${month}.tar.xz
	done;
done;
cut -c1-13 LogFingerprints.txt | uniq -c | sort -n -k1,7 > CountFingerprints.txt
```
# Retrieving the data from Sybil groups in SybilHunting 
The files are located in https://nymity.ch/sybilhunting/
Execute the follow bahs to get all the data from the Sybil groups in a file
```bash
#!/bin/bash
wget --reject "index.html*" --no-parent --no-host-directories https://nymity.ch/sybilhunting/sybil-groups.tar.bz2	
tar -xpvf sybil-groups.tar.bz2
cwd=$(pwd)
COUNTER=0

#getting into each folder
for D in sybil-groups/*; do
    if [ -d "${D}" ]; then	
	mkdir ${D}/consensuses
	tar -xvf ${D}/consensuses.tar -C ${D}/consensuses	
	for F in ${D}/consensuses/*; do
		COUNTER=$((COUNTER + 1))
		# using awk script to retrieve data, in this case ip address
		$cwd/extractip.awk $F >> IpInfo.txt
	done	
    fi
done
echo "$COUNTER files succesfully decompressed"

#cleaning up the house
echo "Cleaning up the house...."
rm sybil-groups.tar.bz2
rm -rf sybil-groups
```

For research purproses, we tagged the ip to its corresponding country, using the endpoint proveded by ipinfo.io/

```bash
#!/bin/bash

urlString="curl ipinfo.io/"
suffix="/country"
COUNTER=0
sep=" "

while read STRING
do
    ip=$STRING
    evalString=$urlString$ip$suffix
    rai=$($evalString)
    out=$STRING$sep$rai 
    echo "$out">>temp.txt
    COUNTER=$((COUNTER + 1))
    
done
# unique is not needed here, it is just a check to be sure
sort temp.txt | uniq > taggedips.txt 
echo "Succesfully tagged $COUNTER countries"
rm temp.txt
```
	
