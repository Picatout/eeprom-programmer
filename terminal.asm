;;
; Copyright Jacques Deschênes 2019,2022,2025 
; This file is part of eBasic 
;
;     eBasic is free software: you can redistribute it and/or modify
;     it under the terms of the GNU General Public License as published by
;     the Free Software Foundation, either version 3 of the License, or
;     (at your option) any later version.
;
;     eBasic is distributed in the hope that it will be useful,
;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;     GNU General Public License for more details.
;
;     You should have received a copy of the GNU General Public License
;     along with eBasic.  If not, see <http://www.gnu.org/licenses/>.
;;
;------------------------------
; This file is for functions 
; interfacing with VT100 terminal
; emulator.
;------------------------------

    .module TERMINAL  

    .area CODE 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   UART subroutines
;;   used for user interface 
;;   communication channel.
;;   settings: 
;;		115200 8N1 no flow control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



	.area CODE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Uart1 intterrupt handler 
;;; on receive character 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;--------------------------
; UART receive character
; in a FIFO buffer 
; CTRL+C (ASCII 3)
; cancel program execution
; and fall back to command line
; CTRL+X reboot system 
; CTLR+Z erase EEPROM autorun 
;        information and reboot
;--------------------------
UartRxHandler: ; console receive char 
	btjf UART_SR,#UART_SR_RXNE,5$ 
	ld a,UART_DR 
	push a 
	ld a,#rx1_queue 
	add a,rx1_tail 
	clrw x 
	ld xl,a 
	pop a 
	ld (x),a 
	ld a,rx1_tail 
	inc a 
	and a,#RX_QUEUE_SIZE-1
	ld rx1_tail,a 
5$:	
	iret 


;---------------------------------------------
; initialize UART, 115200 8N1
; called from cold_start in hardware_init.asm 
; input:
;	none
; output:
;   none
;---------------------------------------------
BAUD_RATE=115200
; BRR value = 16Mhz/115200 = 0x8B  
BRR1_VAL=8 
BRR2_VAL=0xB
uart_init:
	ld a,#BRR2_VAL
	ld UART_BRR2,a 
	ld a,#BRR1_VAL  
	ld UART_BRR1,a
    clr UART_DR
	mov UART_CR2,#((1<<UART_CR2_TEN)|(1<<UART_CR2_REN)|(1<<UART_CR2_RIEN));
	bset UART_CR2,#UART_CR2_SBK
    btjf UART_SR,#UART_SR_TC,.
	call clear_queue
	ret

;---------------------------
;  clear rx1_queue 
;---------------------------
clear_queue:
    _clrz rx1_head 
	_clrz rx1_tail 
	ret 


;---------------------------------
; uart_putc
; send a character via UART
; input:
;    A  	character to send
;---------------------------------
putc::
uart_putc:: 
	btjf UART_SR,#UART_SR_TXE,.
	ld UART_DR,a 
	ret 


;---------------------------------
; Query for character in rx1_queue
; input:
;   none 
; output:
;   A     0 no charcter available
;   Z     1 no character available
;---------------------------------
qgetc::
uart_qgetc::
	_ldaz rx1_head 
	sub a,rx1_tail 
	ret 

;---------------------------------
; wait character from UART 
; input:
;   none
; output:
;   A 			char  
;--------------------------------	
getc:: ;console input
uart_getc::
	call uart_qgetc
	jreq uart_getc 
	pushw x 
;; rx1_queue must be in page 0 	
	ld a,#rx1_queue
	add a,rx1_head 
	clrw x  
	ld xl,a 
	ld a,(x)
	push a
	_ldaz rx1_head 
	inc a 
	and a,#RX_QUEUE_SIZE-1
	_straz rx1_head 
	pop a 
	btjf flags,#FUPPER,1$
	cp a,#'a 
	jrmi 1$
	cp a,#'z+1 
	jrmi 1$ 
	and a,#0xDF  
1$: 
	popw x
	ret 

;-----------------------------
; send an ASCIZ string to UART 
; input: 
;   x 		char * 
; output:
;   none 
;-------------------------------
puts::
    ld a,(x)
	jreq 1$
	call putc 
	incw x 
	jra puts 
1$:	incw x 
	ret 

;---------------------------
; delete character at left 
; of cursor on terminal 
; input:
;   none 
; output:
;	none 
;---------------------------
uart_bksp::
bksp::
	push a 
	ld a,#BS 
	call putc  
	ld a,#SPACE 
	call putc 
	ld a,#BS 
	call putc 
	pop a 
	ret 
 

;---------------------------
; send LF character 
; terminal interpret it 
; as CRLF 
;---------------------------
new_line:: 
	ld a,#CR  
	call putc 
	ret 

;--------------------------
; erase terminal screen 
;--------------------------
clr_screen::
	ld a,#ESC 
	call putc 
	ld a,#'c 
	call putc 
	ret 

;--------------------------
; output a single space
;--------------------------
space::
	push a 
	ld a,#SPACE 
	call putc 
	pop a 
	ret 

;--------------------------
; print n spaces on terminal
; input:
;  X 		number of spaces 
; output:
;	none 
;---------------------------
spaces::
	ld a,#SPACE 
1$:	tnzw x
	jreq 9$
	call putc 
	decw x
	jra 1$
9$: 
	ret 

;--------------------------
; this version of readln 
; if to be used with 
; non ANSI terminal 
; like STM8_terminal 
; 
; BS      delete last character 
; input:
;   A     initial line length
; output:
;   A     line length 
;   X     tib address 
;--------------------------
MAX_LEN=TIB_SIZE-1
	HI_LL=1
	LN_LEN=2
	CHAR=3 
	VSIZE=CHAR  
readln::
	_vars VSIZE
	clr (CHAR,sp) 
	clr (HI_LL,sp)
	ld (LN_LEN,sp),a
	ldw x,#tib 
	tnz a 
	jreq 1$
	call puts 
	ldw x,#tib
	addw x,(HI_LL,sp)
1$:
	call uart_getc
	call to_upper
	ld (CHAR,sp),a 
	cp a,#SPACE 
	jruge 4$
	cp a,#CR 
	jrne 2$
	jra 9$ 
2$:
	cp a,#LF 
	jrne 3$
	jra 9$
3$:
	cp a,#BS 
	jrne 1$ 
	tnz (LN_LEN,sp)
	jreq 1$ 
	call bksp 
	decw x 
	clr (x)
	dec (LN_LEN,sp)
	jra 1$ 
4$:	
; append character to end of line 
	ld a,(LN_LEN,sp)
	cp a,#MAX_LEN 
	jrmi 5$
    jra 1$ 
5$:
	ld a,(CHAR,sp)
	call uart_putc 
	ld (x),a 
	incw x 
	clr (x)
	inc (LN_LEN,sp)
	jra 1$ 
9$:	call uart_putc  
10$: 
	ldw x,#tib 
	ld a,(LN_LEN,sp)
	_drop VSIZE 
	ret 

;----------------------------------
; convert to hexadecimal digit 
; input:
;   A       digit to convert 
; output:
;   A       hexdecimal character 
;----------------------------------
to_hex_char::
	and a,#15 
	cp a,#10 
	jrmi 1$ 
	add a,#7
1$: add a,#'0 
	ret 

;------------------------------
; print byte  in hexadecimal 
; on console
; no space separator 
; input:
;    A		byte to print
;------------------------------
print_hex::
	push a 
	swap a 
	call to_hex_char 
	call putc 
    pop a  
	call to_hex_char
	call putc   
	ret 

;------------------------------
; print A in decimal base 
; not space after, no leading 
; zero.
; input:
;    A    int8 to print 
;-------------------------------
print_dec:
	pushw x 
	clrw x 
	ld xl,a 
	ld a,#10
	div x,a  
	push a 
	ld a,#10 
	div x,a 
	push a 
	ld a,xl 
	tnz a 
	jreq 1$ 
	call prt_digit 
1$: pop a 
	tnz a 
	jreq 2$ 
	call prt_digit 
2$:	pop a 
	call prt_digit 
	popw x 
	ret 
prt_digit:
	add a,#'0 
	call putc 
	ret 


;------------------------
; print int8 
; input:
;    A    int8 
; output:
;    none 
;-----------------------
prt_i8:
	clrw x 
	ld xl,a  


;------------------------------------
; print integer  
; input:
;	X  		    integer to print 
;	'base' 		numerical base for conversion 
;    A 			signed||unsigned conversion
;  output:
;    A          string length
;------------------------------------
print_int::
	_ldaz base 
	sub a,#16 
	jreq 1$
	ld a,#255  ; signed conversion  when base 10 
1$:
    call itoa  ; conversion entier en  .asciz
	push a 
	call puts
	pop a 
    ret	

;------------------------------------
; convert integer in x to string
; input:
;   'base'	conversion base 
;	X   	integer to convert
;   A       0=unsigned, else signed 
; output:
;   X  		pointer to first char of string
;   A       string length
; use:
;   pad     to build string 
;------------------------------------
	SIGN=1  ; 1 byte, integer sign 
	LEN=SIGN+1   ; 1 byte, string length 
	VSIZE=2 ;locals size
itoa::
	pushw y 
	_vars VSIZE
	clr (LEN,sp) ; string length  
	clr (SIGN,sp)    ; sign
	tnz a
	jreq 1$ ; unsigned conversion  
	tnzw x 
	jrpl 1$ 
	cpl (SIGN,sp)
	negw x 
1$:
; initialize string pointer 
; build string at end of pad  
	ldw y,#pad 
	addw y,#PAD_SIZE 
	decw y 
	clr (y)
	ld a,#SPACE
	decw y
	ld (y),a 
	inc (LEN,sp)
itoa_loop:
    _ldaz base 
    div x,a 
    add a,#'0  ; remainder of division
    cp a,#'9+1
    jrmi 2$
    add a,#7 
2$:	
	decw y
    ld (y),a
	inc (LEN,sp)
; if x==0 conversion done
	tnzw x 
    jrne itoa_loop
	_ldaz base 
	sub a,#10 
	jreq 3$
	ld a,#'$ 
	jra 4$
3$:
	ld a,(SIGN,sp)
    jreq 10$
    ld a,#'-
4$:
    decw y
    ld (y),a
	inc (LEN,sp)
10$:
	ld a,(LEN,sp)
	ldw x,y 
	_drop VSIZE
	popw y 
	ret

;--------------------------
; convert lower letters 
; to upper case 
; input:
;    A 
; output:
;    A 
;-------------------------
to_upper:
	cp a,#'a 
	jrmi 9$ 
	cp a,#'z+1 
	jrpl 9$ 
	and a,#0xDF 
9$:
	ret 
