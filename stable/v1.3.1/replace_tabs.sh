#!/bin/ksh

for iter in *.F90
do
     echo ${iter}
     mv ${iter} ${iter}.tabs 
     expand -t 4 ${iter}.tabs > ${iter}
done