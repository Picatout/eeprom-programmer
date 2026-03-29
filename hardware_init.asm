;;
; Copyright Jacques Deschênes 2025  
; This file is part of eeprom-programmer 
;
;     eeprom-programmer is free software: you can redistribute it and/or modify
;     it under the terms of the GNU General Public License as published by
;     the Free Software Foundation, either version 3 of the License, or
;     (at your option) any later version.
;
;     eeprom-programmer is distributed in the hope that it will be useful,
;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;     GNU General Public License for more details.
;
;     You should have received a copy of the GNU General Public License
;     along with eeprom-programmer.  If not, see <http://www.gnu.org/licenses/>.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hardware initialisation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;------------------------
; if unified compilation 
; must be first in list 
;-----------------------

    .module HW_INIT 

    .include "config.inc"

  
;;-----------------------------------
    .area SSEG (ABS)
;; working buffers and stack at end of RAM. 	
;;-----------------------------------
    .org RAM_SIZE-STACK_SIZE-TIB_SIZE-PAD_SIZE 
tib: .ds TIB_SIZE ; input buffer 
write_buffer::                 ; use to write FLASH block (alias for pad )
pad:: .ds PAD_SIZE             ; working buffer
stack_full:: .ds STACK_SIZE   ; control stack 
stack_unf: ; stack underflow ; control_stack bottom 

;;--------------------------------------
    .area HOME 
;; interrupt vector table at 0x8000
;;--------------------------------------

    int cold_start			; RESET vector 
	int NonHandledInterrupt ; trap instruction 
	int NonHandledInterrupt ;int0 TLI   external top level interrupt
	int NonHandledInterrupt ;int1 AWU   auto wake up from halt
	int NonHandledInterrupt ;int2 CLK   clock controller
	int NonHandledInterrupt ;int3 EXTI0 gpio A external interrupts
	int NonHandledInterrupt ;int4 EXTI1 gpio B external interrupts
	int NonHandledInterrupt ;int5 EXTI2 gpio C external interrupts
	int NonHandledInterrupt ;int6 EXTI3 gpio D external interrupts
	int NonHandledInterrupt
	int NonHandledInterrupt ;int8 beCAN RX interrupt
	int NonHandledInterrupt ;int9 beCAN TX/ER/SC interrupt
	int NonHandledInterrupt ;int10 SPI End of transfer
	int NonHandledInterrupt ;int11 TIM1 update/overflow/underflow/trigger/break
	int NonHandledInterrupt ; int12 TIM1 capture/compare
	int NonHandledInterrupt ;int13 TIM2 update /overflow
	int NonHandledInterrupt ;int14 TIM2 capture/compare
	int NonHandledInterrupt ;int15 TIM3 Update/overflow
	int NonHandledInterrupt ;int16 TIM3 Capture/compare
	int NonHandledInterrupt ;int17 UART1 TX completed
	int UartRxHandler		;int18 UART1 RX full 
	int NonHandledInterrupt ;int19 I2C 
	int NonHandledInterrupt ;int20 UART3 TX completed
	int NonHandledInterrupt ;int21 UART3 RX full
	int NonHandledInterrupt ;int22 ADC2 end of conversion
	int Timer4UpdateHandler	;int23 TIM4 update/overflow ; used as msec ticks counter
	int NonHandledInterrupt ;int24 flash writing EOP/WR_PG_DIS
	int NonHandledInterrupt ;int25  not used
	int NonHandledInterrupt ;int26  not used
	int NonHandledInterrupt ;int27  not used
	int NonHandledInterrupt ;int28  not used
	int NonHandledInterrupt ;int29  not used


;--------------------------------------
    .area DATA (ABS)
	.org 0 
;--------------------------------------	

; keep the following 3 variables in this order 
base::  .blkb 1 ; nemeric base used to print integer 
fmstr:: .blkb 1 ; frequency in Mhz of Fmaster
timer:: .blkw 1 ;  milliseconds count down timer 
farptr: .blkb 1 ; 24 bits pointer used by file system, upper-byte
ptr16::  .blkb 1 ; 16 bits pointer , farptr high-byte 
ptr8:   .blkb 1 ; 8 bits pointer, farptr low-byte  
flags:: .blkb 1 ; various boolean flags
rx1_head::  .blkb 1 ; rx1_queue head pointer
rx1_tail::   .blkb 1 ; rx1_queue tail pointer  
mode: .blkb 1 ; command mode 
; version 2 update these variable size to 24 bits 
xamadr: .blkb 3 ; examine address 
storadr: .blkb 3 ; store address 
last: .blkb 3   ; last address parsed from input 
limit: .blkb 3 ; eeprom last address 0x1fff for 8KO, 0x7fff for 32KO, etc 
count: .blkb 1 ; count bytes written
eeType: .blkb 1; programming type AT28 =0, 39SF = 1 
page_size: .blkb 1 ;  how many bytes can be programmed in one shot, 0=256 
RowDelay: .blkb 1 ; delay msec between row send in exam_block

	.org 0x80 
rx1_queue:: .ds RX_QUEUE_SIZE ; UART receive circular queue 

	.area CODE 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; non handled interrupt 
; reset MCU
;;;;;;;;;;;;;;;;;;;;;;;;;;;
NonHandledInterrupt:
	_swreset ; see "inc/gen_macros.inc"


;------------------------------
; TIMER 4 is used to maintain 
; a milliseconds 'ticks' counter
; and decrement 'timer' varaiable
; ticks range {0..2^16-1}
; timer range {0..65535}
;--------------------------------
Timer4UpdateHandler:
	clr TIM4_SR 
	btjf flags,#FTIMER,1$ 
	_ldxz timer
	decw x 
	_strxz timer 
	jrne 1$ 
	bres flags,#FTIMER  
1$:	
	iret 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    peripherals initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;---------------------------------
; TIM4 is configured to generate an 
; interrupt every millisecond 
;----------------------------------
timer4_init:
	bset CLK_PCKENR1,#CLK_PCKENR1_TIM4
	bres TIM4_CR1,#TIM4_CR1_CEN 
	ld a,fmstr 
	ldw x,#0xe8 
	mul x,a
	pushw x 
	ldw x,#3 
	mul x,a 
	swapw x 
	addw x,(1,sp) 
	_drop 2  
	clr a 
0$:	 
	cpw x,#256 
	jrmi 1$ 
	inc a 
	srlw x 
	jra 0$ 
1$:
	ld TIM4_PSCR,a 
	ld a,xl 
	ld TIM4_ARR,a
	mov TIM4_CR1,#((1<<TIM4_CR1_CEN)|(1<<TIM4_CR1_URS))
	bset TIM4_IER,#TIM4_IER_UIE
	ret


;-------------------------------------
;  initialization entry point 
;-------------------------------------
cold_start:
;set stack 
	ldw x,#STACK_EMPTY
	ldw sp,x
; clear all ram 
0$: clr (x)
	decw x 
	jrne 0$
; activate pull up on all inputs 
	ld a,#255 
	ld PA_CR1,a 
	ld PB_CR1,a 
	ld PC_CR1,a 
	ld PD_CR1,a 
	ld PE_CR1,a 
	ld PF_CR1,a 
	ld PG_CR1,a 
	ld PI_CR1,a
; set EEPROM Vcc control on PF0 
	BSET VCC_CTRL+2,#VCC_BIT ; DDR registe bit 0 as output 
	BSET VCC_CTRL,#VCC_BIT ; EEPROM VCC_OFF  
; set user LED pin as output 
    bset LED_PORT+GPIO_CR1,#LED_BIT
    bset LED_PORT+GPIO_CR2,#LED_BIT
    bset LED_PORT+ GPIO_DDR,#LED_BIT
; select internal clock no divisor: 16 Mhz 	
	clr CLK_CKDIVR ; 16Mhz HSI 
	mov fmstr,#16
	call timer4_init ; msec ticks timer 
	call spi_config  ; used with memory with SPI interface 
; UART at 115200 BAUD
; used for user interface 
	call uart_init
	_clrz flags 
	rim ; enable interrupts 
	jp eeProg 
	jra . 

;-------------------------------
; pause execution for 
; some msec 
; input:
;    X    delay msec 
;-------------------------------
pause:
	_strxz timer 
	bset flags,#FTIMER 
	btjt flags,#FTIMER,. 
	ret 