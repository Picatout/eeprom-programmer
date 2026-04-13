;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  configuration parameters 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    MAJOR=3
    MINOR=0
    REV=1

    FMSTR=16 ; master clock frequency in Mhz 

    AT28=0  ; AT28 memory type 
    SST39=1 ; SST39 memory type 
    W25Q=2  ; W25Q memory type 
    
    .include "inc/stm8s208.inc" 
    .include "inc/nucleo_8s208.inc"
    .include "inc/ascii.inc"
	.include "inc/gen_macros.inc" 
    .include "inc/app_macros.inc"

