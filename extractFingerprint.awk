#!/usr/bin/awk -f
BEGIN{
FS=" ";
OFS=" ";
IP="missingIP"
}

{
if ( $0 ~ /router/ ){
	IP=$3;
}

if ( $0 ~ /fingerprint/ ){
	search="fingerprint"
        split($0, array, search);
        fingerprint = array[2]; 
	print IP, fingerprint 
}
}

