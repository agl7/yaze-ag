#!/bin/sh
# a hack by agl (08 June 2008)
# $1 is the directory in which the disks will be installed
#

echo "Begin install disks"

mkdir -p $1
for ydsk in `cat doc_ydskfiles.txt` ; do
   echo "install yaze disk $1/$ydsk"
   cp $ydsk $1/$ydsk
done

echo "copy the source of the disksort project"
cp -av disksort $1
chown -R root:root $1/disksort

echo "generate $1/yazerc"
cp .yazerc $1/yazerc

echo "End install disks"

