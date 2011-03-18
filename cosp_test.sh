#!/usr/bin/sh

# arg1 = 1D or 2D
# arg2 must be one of these options:
#   0: use cmp
#   1: use nccmp
#   2: use nco + diff
#   3: use nco to create differences

# (c) British Crown Copyright 2008, the Met Office.
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without modification, are permitted 
# provided that the following conditions are met:
# 
#     * Redistributions of source code must retain the above copyright notice, this list 
#       of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright notice, this list
#       of conditions and the following disclaimer in the documentation and/or other materials 
#       provided with the distribution.
#     * Neither the name of the Met Office nor the names of its contributors may be used 
#       to endorse or promote products derived from this software without specific prior written 
#       permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR 
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND 
# FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR 
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER 
# IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
# OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# 
# History:
# Dec 2008 - A. Bodas-Salcedo - Initial version
# Jan 2009 - A. Bodas-Salcedo - Added options 2 and 3
# Feb 2010 - A. Bodas-Salcedo - Added arg3 and arg4

if [ $# -ne 4 ]
then
   echo "Usage: " $0 " arg1 arg2 arg3 arg4"
   echo "arg1 = 1D or 2D"
   echo "arg2 must be one of thee options:"
   echo " 0: use cmp"
   echo " 1: use nccmp"
   echo " 2: use nco + diff"
   echo " 3: use nco to create differences"
   echo "arg3 is the directory with the test files (e.g. ./outputs_test.mlev)"
   echo "arg4 is the directory with the output files (e.g. ./outputs)"
   exit 1
fi

DIR_TEST=$3
DIR_OUTS=$4

# Check inputs
if [  $1 != "1D"  -a  $1 != "2D"  ]; then
    echo "arg1 must be 1D or 2D"
    exit 1
fi

if [  $2 != 0  -a  $2 != 1  -a  $2 != 2  -a  $2 != 3 ]; then
    echo "arg2 must be one of these options:"
    echo "0: use cmp"
    echo "1: use nccmp"
    echo "2: use nco + diff"
    echo "3: use nco"
    exit 1
fi

if [ $2 = 0 ]; then
  CMPPRG=cmp
  OPTS="-l"
fi
if [ $2 = 1 ]; then
   CMPPRG=nccmp
   OPTS="-dmqf -A history"
fi
if [ $2 = 2 -o $2 = 3 ]; then
   TMPDIR=./temp
   mkdir $TMPDIR
fi

# Compare files
PRECISION=4
NLINES=10000
LENGTH=100
LIST_TEST=$(ls $DIR_TEST/*_$1*.nc)
# LIST_TEST=./$DIR_TEST/ctpisccp_1D.nc
for FILE in $LIST_TEST; do
  FILE=$(echo $FILE | cut -d/ -f3)
  F1=$DIR_TEST/$FILE
  F2=$DIR_OUTS/$FILE
#   VAR=$(echo $FILE | cut -d_ -f1)
  VAR=$(echo $FILE | sed 's/_'$1'.nc//')
  echo '------------------------- Comparing ' $FILE '...'
  if [  $2 == 0  -o  $2 == 1  ]; then
    $CMPPRG $OPTS $F1 $F2
  fi
  if [  $2 == 2  ]; then
    TMP1=cosp.tmp.1
    TMP2=cosp.tmp.2
    TMP3=cosp.tmp.3
    ncdump -l $LENGTH -p $PRECISION -v $VAR $F1 > $TMPDIR/$TMP3
    grep -A $NLINES -i "data:" $TMPDIR/$TMP3 > $TMPDIR/$TMP1
    ncdump -l $LENGTH -p $PRECISION -v $VAR $F2 > $TMPDIR/$TMP3
    grep -A $NLINES -i "data:" $TMPDIR/$TMP3 > $TMPDIR/$TMP2
    diff $TMPDIR/$TMP1 $TMPDIR/$TMP2
  fi
  if [  $2 == 3  ]; then
    TMP1=$VAR.tmp.nc
    TMP2=$VAR.diff.nc
    F3=$TMPDIR/$TMP1
    F4=$TMPDIR/$TMP2
    # Compute difference
    ncdiff -O $F2 $F1 $F4
#     # Copy test file to temp directory
#     cp $F1 $TMPDIR/$TMP1
#     # Rename variable to 'old'
#     ncrename -v $VAR,old $F3
#     # Append new variable
#     ncks -A -v $VAR $F2 $F3
#     # Rename variable to 'new'
#     ncrename -v $VAR,new $F3
#     # Compute d=new-old
#     ncap -O -s 'd=new-old;' $F3 $F4
    #++++++++ I don't know why the last line doesn't work.
    # Add attribute max
    # ncatted -a max,$VAR,c,f,-5.0 $F4
    # Compute maximum with ncap (does not work)
    # ncap -O -s 'd@max=d.max();' $F4 $F4
    #----------
  fi
done

if [ $2 = 2 ]; then
    rm -rd $TMPDIR
fi

