#!/bin/bash 

# usage:
# ./build.sh  [ hsi|hse ]
# hsi -> flash 16Mhz internal oscillator version after build
# hse -> flash external crystal version after build
# no option -> build both versions without flashing 

if [ -z $1 ]; then 
# build HSI version 
sed -i 's/HSI=0/HSI=1/' inc/config.inc 
make -B hsi

# build HSE 24Mhz version 
sed -i 's/HSI=1/HSI=0/' inc/config.inc
make -B hse
fi 

if [  ! -z $1 ]; then 
    if [ $1 == "hsi" ]; then 
            make flash_hsi 
    elif [ $1 == "hse" ]; then
            make flash_hse  
    fi
fi 

