#!/usr/bin/env bash

RAW_DATA=/Users/danton/Dropbox/ra-work/spped/RawData
OUTDIR=$RAW_DATA/CPS_Personnel/cooked

rm -r $OUTDIR
mkdir $OUTDIR

# search is left broad. refine later.

# ** MUST USE [[:space:]] no just [ ]
# 2008 - 2011
F1=$(ls $RAW_DATA/CPS_Personnel/ | grep txt | egrep 20[01][0189])
for i in $F1
do
    sed -n -E "/[^(\\&|and)][[:space:]]Security/Ip" $RAW_DATA/CPS_Personnel/${i} |
    sed -E "s/^([0-9]{6})[[:space:]]*([^[:digit:]]+)[[:space:]]*([0-9]+)/\\1\|\\2\|\\3\|/Ig" |
    sed -n -E "/[^(Vacant)]/Ip" > $OUTDIR/${i}
done


# 2012 - 2013
F2=$(ls $RAW_DATA/CPS_Personnel/ | grep txt | egrep 20[1][23])
for i in $F2
do
    sed -n -E "/[^(\\&|and)][[:space:]]Security/Ip" $RAW_DATA/CPS_Personnel/${i} |
    sed -E "s/^([0-9]{6})[[:space:]]*([^[:digit:]]+)[[:space:]]*([0-9]+)/\\1\|\\2\|\\3\|/Ig" |
    sed -n -E "/[^(Vacant)]/Ip" > $OUTDIR/${i}
done


# 2014 - 2016.
F3=$(ls $RAW_DATA/CPS_Personnel/ | grep csv | egrep 20[1][456])
for i in $F3
do
    sed -n -E "/[^(\\&|and)][[:space:]]Security/Ip" $RAW_DATA/CPS_Personnel/${i} |
    sed -n -E "/[^(Vacant)]/Ip" > $OUTDIR/${i}
done