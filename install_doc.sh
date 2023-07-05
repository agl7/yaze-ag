#!/bin/sh
# a hack by agl (15 January 2002)
# $1 is the directory in which the documentation will be installed
#

echo "Begin install dokumentation"

mkdir -p $1
for dfile in `cat docfiles.txt` ; do
   echo "install doc file $dfile"
   install -c -m 644 $dfile $1
done

echo "End install dokumentation"
