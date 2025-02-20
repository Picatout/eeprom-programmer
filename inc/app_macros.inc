;;
; Copyright Jacques Deschênes 2025
; This file is part of eeProg 
;
;     eeProg is free software: you can redistribute it and/or modify
;     it under the terms of the GNU General Public License as published by
;     the Free Software Foundation, either version 3 of the License, or
;     (at your option) any later version.
;
;     eeProg is distributed in the hope that it will be useful,
;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;     GNU General Public License for more details.
;
;     You should have received a copy of the GNU General Public License
;     along with eeProg.  If not, see <http://www.gnu.org/licenses/>.
;;
;--------------------------------------
	STACK_EMPTY=RAM_SIZE-1  
    STACK_SIZE=128 
    PAD_SIZE=64 ; size of EEPROM programming buffer 
    TIB_SIZE=240 ; input buffer size, enough to accept 64 hex bytes for programming a complete EEPROM row.
    RX_QUEUE_SIZE==32 ; UART receive queue size 

    TIM2_CLK_FREQ=62500

;--------------------------------------
;   flags 
;-------------------------------------

    FUPPER=0 
    FTIMER=1
    
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


   