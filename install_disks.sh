#!/bin/sh
# a hack by agl (08 June 2008)
# $1 is the directory in which the disks will be installed
#

echo "Begin install disks"

mkdir -p $1
for ydsk in `cat doc_ydskfiles.txt` ; do
   echo "install yaze disk $1/$ydsk.gz"
   gzip -c9 $ydsk >$1/$ydsk.gz
done

echo "make $1/disksort.tar"
tar cf $1/disksort.tar disksort
echo "compress $1/disksort.tar with gzip"
gzip -f9 $1/disksort.tar

echo "generate $1/yazerc"
cp .yazerc $1/yazerc

echo "End install disks"

