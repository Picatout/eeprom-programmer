;;
; Copyright Jacques Deschênes 2019 
; This file is part of STM8_NUCLEO 
;
;     STM8_NUCLEO is free software: you can redistribute it and/or modify
;     it under the terms of the GNU General Public License as published by
;     the Free Software Foundation, either version 3 of the License, or
;     (at your option) any later version.
;
;     STM8_NUCLEO is distributed in the hope that it will be useful,
;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;     GNU General Public License for more details.
;
;     You should have received a copy of the GNU General Public License
;     along with STM8_NUCLEO.  If not, see <http://www.gnu.org/licenses/>.
;;
;--------------------------------------
        TAB_WIDTH=4 ; default tabulation width 
        EOF=0xff ; end of file marker 
        NLEN_MASK=0xf  ; mask to extract name len 


	STACK_EMPTY=RAM_SIZE-1  
 
	RX_QUEUE_SIZE=8 

    TIM2_CLK_FREQ=62500

;--------------------------------------
;   assembler flags 
;-------------------------------------

  
    ; jump back to interp_loop 
    .macro _next 
        jp interp_loop 
    .endm ; 2 cycles 
    
;------------------------------------
;  board user LED control macros 
;------------------------------------

    .macro _led_on 
        bset LED_PORT,#LED_BIT 
    .endm 

    .macro _led_off 
        bres LED_PORT,#LED_BIT 
    .endm 

    .macro _led_toggle 
        bcpl LED_PORT,#LED_BIT 
    .endm 


   