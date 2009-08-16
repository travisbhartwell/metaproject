#!/bin/bash

DIR=$(cd $(dirname $0); echo $PWD)

NEW=/home/nafai/Projects/metaproject/metaproject
OLD=/home/nafai/Projects/metaproject/metaproject-original.git

cd ~/lib/emacs
rm -f metaproject
ln -s $NEW metaproject

cd $DIR
export NO_LOAD_MP=1
/usr/bin/emacs --debug-init test.el --eval "(eval-buffer)"

cd ~/lib/emacs
rm -f metaproject
ln -s $OLD metaproject



