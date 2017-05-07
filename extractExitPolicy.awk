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

if($1=="p"){
	ExitPolicy[ IP "," $0 ]++; 
} 

}

END{
for (i in ExitPolicy) {
        if (i != "") {
                printf("%s;%s;\n",i,ExitPolicy[i]) >>  "exitPolicy.log"
        }
}
}

