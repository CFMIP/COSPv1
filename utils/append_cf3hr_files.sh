#!/bin/sh
# ! (c) British Crown Copyright 2010, the Met Office.
# ! All rights reserved.
# ! 
# ! Redistribution and use in source and binary forms, with or without modification, are permitted 
# ! provided that the following conditions are met:
# ! 
# !     * Redistributions of source code must retain the above copyright notice, this list 
# !       of conditions and the following disclaimer.
# !     * Redistributions in binary form must reproduce the above copyright notice, this list
# !       of conditions and the following disclaimer in the documentation and/or other materials 
# !       provided with the distribution.
# !     * Neither the name of the Met Office nor the names of its contributors may be used 
# !       to endorse or promote products derived from this software without specific prior written 
# !       permission.
# ! 
# ! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR 
# ! IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND 
# ! FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR 
# ! CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
# ! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
# ! DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER 
# ! IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
# ! OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# !
# ! History:
# ! Oct 2010 - A. Bodas-Salcedo - Initial version
# !
# !

# $1 is the directory with the input files
# $2 is the directory where the outputs will be written. This needs to be the absolute path, not relative.

DIRIN=$1 
DIROUT=$2
TABLE='cf3hr'
# cfadDbze94_cf3hr_HadGEM2-ES_amip_r1i1p1_200801010130-200801010130.nc
# cfadLidarsr532_cf3hr_HadGEM2-ES_amip_r1i1p1_200801010130-200801010130.nc
# clcalipso_cf3hr_HadGEM2-ES_amip_r1i1p1_200801010130-200801010130.nc
# clhcalipso_cf3hr_HadGEM2-ES_amip_r1i1p1_200801010130-200801010130.nc
# cllcalipso_cf3hr_HadGEM2-ES_amip_r1i1p1_200801010130-200801010130.nc
# clmcalipso_cf3hr_HadGEM2-ES_amip_r1i1p1_200801010130-200801010130.nc
# cltcalipso_cf3hr_HadGEM2-ES_amip_r1i1p1_200801010130-200801010130.nc
# parasolRefl_cf3hr_HadGEM2-ES_amip_r1i1p1_200801010130-200801010130.nc

cd ${DIRIN}
VNAME=(cfadDbze94 \
    cfadLidarsr532 \
    clcalipso \
    clhcalipso \
    cllcalipso \
    clmcalipso \
    cltcalipso \
    parasolRefl)
N=8
for ((i = 0;i < $N;i++))
do
    echo ${VNAME[$i]}
    FLIST=$(/bin/ls ${VNAME[$i]}_${TABLE}_*.nc)
    M=$(/bin/ls ${VNAME[$i]}_${TABLE}_*.nc -w3 | wc -l)
    # Find first and last time
    j=0
    for FILE in $FLIST; do
       TIME=$(echo ${FILE} | cut -d_ -f6)
       T1=$(echo ${TIME} | cut -d- -f1)
       T2=$(echo ${TIME} | cut -d- -f2)
       T2=$(echo ${T2} | cut -d. -f1)
       if [[ $j == 0 ]] ; then
         FNAME=$(echo ${FILE} | cut -d_ -f1-5)
         TIME1=$T1
       fi
       j=$j+1
    done
    TIME2=$T2
    echo "Time 1: "$TIME1
    echo "Time 2: "$TIME2
    # Create output file name
    FNAME=${FNAME}_${TIME1}-${TIME2}.nc
    echo "Output: "$FNAME
    # Concatenate files
    ncrcat ${FLIST} ${DIROUT}/${FNAME}
done
