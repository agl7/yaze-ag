#!/bin/sh

S_YAZEFILES=/usr/local/lib/yaze
S_CPMDSKS=/usr/local/lib/yaze/disks
S_DOCFILES=/usr/local/lib/yaze/doc
S_DOCFILES_html=/usr/local/lib/yaze/doc_html

if [ ! -f .yazerc ]
then
  if [ ! -d $HOME/cpm ]
  then
    echo
    echo Creating $HOME/cpm ...
    echo
    mkdir $HOME/cpm
    echo "copy $S_CPMDSKS/yazerc  to  $HOME/cpm/.yazerc"
    cp $S_CPMDSKS/yazerc $HOME/cpm/.yazerc
    echo
    echo "copy $S_YAZEFILES/*.ktt  to  $HOME/cpm/"
    echo
    cp -v $S_YAZEFILES/*.ktt $HOME/cpm
    echo
    echo "Install some yaze-disks to run CP/M 3.1 ..."
    echo
    for ydsk in $S_CPMDSKS/*.gz ; do
	echo -n "`basename $ydsk .gz`	<----	"
	gzip -vdc $ydsk >$HOME/cpm/`basename $ydsk .gz`
    done
    cd $HOME/cpm
    tar xf disksort.tar
    rm disksort.tar
    echo
    read -p "Pause press enter to continue ... "
    echo
    echo
    echo "Creating $HOME/cpm/doc ... (Here is the complete documentation)"
    echo
    mkdir $HOME/cpm/doc
    for dfile in $S_DOCFILES/* ; do
	echo "set link for `basename $dfile`"
	# gzip -vdc $dfile >$HOME/cpm/doc/`basename $dfile`
	ln -f -s $dfile $HOME/cpm/doc/`basename $dfile`
    done
    echo
    read -p "Pause press enter to continue ... "
    echo
    echo "Creating $HOME/cpm/doc_html ... (Here is the complete documentation in HTML)"
    echo "............................... (Klick on the file index.html)"
    echo
    mkdir $HOME/cpm/doc_html
    for dfile in $S_DOCFILES_html/* ; do
	echo "set link for `basename $dfile`"
	# gzip -vdc $dfile >$HOME/cpm/doc_html/`basename $dfile`
	ln -f -s $dfile $HOME/cpm/doc_html/`basename $dfile`
    done
    echo
    echo "Have a look also to \"man yaze\" and \"man cdm\""
    echo
    echo -n "syncing..."
    sync
    echo "ok"
    read -p "Pause press enter to continue ... "
  fi
  cd $HOME/cpm
  if [ ! -f .yazerc ]
  then
    echo
    echo "$HOME/cpm exists but"
    echo "$HOME/cpm/.yazerc is not presend --> do not run yaze_bin !!!"
    echo
    echo "Read yaze(1) and yaze.doc and write a .yazerc !!!"
    echo "You can use $S_CPMDSKS/yazerc as a pattern."
    echo
    echo "Or you can remove $HOME/cpm complete and restart with \"yaze\"."
    echo
    exit 1
  fi
fi

echo
echo pwd=`pwd`

if [ -f yaze_bin ]
then
   echo "starting ./yaze_bin $*"
   exec ./yaze_bin $*
else
   echo "starting yaze_bin $*"
   exec yaze_bin $*
fi
