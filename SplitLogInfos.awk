#!/usr/bin/awk -f
BEGIN{
FS=";";
OFS=";";
}

{
	year = substr($0,1,4);
	print $0 >> "LogInfos" year ".txt"
}
