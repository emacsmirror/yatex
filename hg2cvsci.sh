#!/bin/sh
# This script does not support automatic "cvs add".
# If new file is added by "hg add", don't forget to "cvs add" in
# CVS working dir.
#
# This script should be called via incoming hook in .hg/hgrc as below.
# [hooks]
# incoming.cvsci = ./hg2cvsci.sh

node=${HG_NODE:-tip}
msg=`hg log -r $node --template '{desc}\n'`
cd `dirname $0`
$HG up -r $node
cvs ci -m "$msg"
