#!/bin/bash

DIR=$(cd $(dirname $0); echo $PWD)

NEW=$(cd $DIR; cd ../lisp; echo $PWD)
OLD=$(cd $DIR; cd ../../metaproject-original.git; echo $PWD)

cd ~/lib/emacs
rm -f metaproject
ln -s $NEW metaproject

cd $DIR
export NO_LOAD_MP=1

# Set it if isn't already
[ -z "$EMACS" ] && EMACS="$1" && shift

if [ -z "$EMACS" ]; then
    EMACS=/usr/bin/emacs
else
    echo "emacs is $EMACS"
fi

$EMACS --debug-init test.el --eval "(eval-buffer)"

cd ~/lib/emacs
rm -f metaproject
ln -s $OLD metaproject
