#!/bin/ksh

for iter in quickbeam/*.f90.fixed ;
do
     echo ${iter}
     mv ${iter} ${iter}.tabs 
     expand -t 4 ${iter}.tabs > ${iter}
done