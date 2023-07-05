#!/bin/sh
# a hack by agl (15 January 2002)
# $1 is the directory in which the documentation will be installed
#

echo "Begin install HTML dokumentation"

mkdir -p $1 $1/cpmhelp
for dfile in `cat docfiles_html.txt` ; do
   echo "install html file $dfile"
   install -c -m 644 $dfile $1
done

for dfile in `cat doc_cpmhelp_html.txt` ; do
   echo "install html file $dfile"
   install -c -m 644 $dfile $1/cpmhelp
done

echo "End install HTML dokumentation"
