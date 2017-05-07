#!/usr/bin/awk -f
BEGIN{
FS=" ";
OFS=";";
ExitPolicy[""]=0;
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
	ExitPolicy[ IP "," $0 ]++; 
	printf("%s;%s;%s;%s;%d;%s;%d;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s\n",Date,Hour,Name,IP,Port,Version,Bandwidth,Authority,BadExit,Exit,Fast,Guard,HSDir,NoEdConsensus,Running,Stable,V2Dir,Valid)        
} 

}

END{
for (i in ExitPolicy) {
        if (i != "") {
                printf("%s;%s;\n",i,ExitPolicy[i]) >>  exitPolicy.log
        }
}
}
