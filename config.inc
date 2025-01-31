;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  configuration parameters 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DEBUG=0 ; set to 1 to include debugging code 

HSI=1 ; set this to 1 if using internal high speed oscillator  
.if HSI 
HSE=0
.else
HSE=1  
.endif 

FMSTR=16 ; master clock frequency in Mhz 

NUCLEO_8S208RB=1

    .include "inc/stm8s208.inc" 
    .include "inc/nucleo_8s208.inc"
    .include "inc/ascii.inc"
	.include "inc/gen_macros.inc" 

