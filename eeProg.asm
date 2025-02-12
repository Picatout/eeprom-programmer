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

;;---------------------------------------
;; at28C64B || at28c256  EEPROM programmer 
;;--------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   COMMENTS 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; 1) Apple I keyboard interface was setting 
;;    setting bit 7 to 1 
;;     no need for it here 
;; 2) STM8 have 16 bits X,Y registers 
;;    I use this facility to avoid page 0
;;    pointers  
;; 3) STM8 have stack relative addressing 
;;    also very helpfull to avoid global 
;;    variables in RAM.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    .module EEPROG  


    ADDR_HIGH=PG_ODR 
    ADDR_LOW=PD_ODR 
    DATA_ODR=PB_ODR
    DATA_IDR=PB_IDR 
    DATA_DDR=PB_DDR
    DATA_CR1=PB_CR1 
    DATA_CR2=PB_CR2  
    EEPROM_CTRL=PC_ODR 
    EEPROM_NCE=BIT1 ; eeprom enable 
    EEPROM_NOE=BIT2 ; eeprom output enable 
    EEPROM_NWE=BIT3 ; eeprom write enable 

    EEPROM_PAGE_SIZE=64

;--------------------------------
;     MACROS 
;--------------------------------

    ; reset eeprom ~CE bit 
    ; activate eeprom
    ; must be in this state 
    ; for read/prog operations  
    .macro _eeprom_enable  
        bres EEPROM_CTRL,#EEPROM_NCE 
    .endm 

    ; set eeprom ~CE bit 
    ; put eeprom in low power mode 
    ; data pin are in hi-z state 
    ; can't be read/prog in this state 
    .macro _eeprom_disable 
        bset EEPROM_CTRL,#EEPROM_NCE
    .endm 

    ; reset eeprom ~OE bit
    ; must be low to read eeprom  
    .macro _eeprom_enabe_output 
        bres EEPROM_CTRL,#EEPROM_NOE 
    .endm 

    ; set eeprom ~OE bit
    ; must be high to program eeprom  
    .macro _eeprom_disable_output 
        bset EEPROM_CTRL,#EEPROM_NOE
    .endm 

    ; reset eeprom ~WE bit 
    .macro _eeprom_we_low  
        bres EEPROM_CTRL,#EEPROM_NWE
    .endm 

    ; set eeprom ~WE bit 
    .macro _eeprom_we_high 
        bset EEPROM_CTRL,#EEPROM_NWE
    .endm 

    ; eeprom programming delay 
    ; 11msec per 64 bytes page 
    .macro _prog_delay 
        ld a,#10
        _straz timer+1
        _clrz timer  
        bset flags,#FTIMER 
        btjt flags,#FTIMER,.
    .endm 

    ; set DATA port as output 
    .macro _data_output 
        ld a,#255 
        ld DATA_CR1,a ; push pull output mode 
        ld DATA_CR2,a ; high_speed  
        ld DATA_DDR,a ; output mode 
    .endm 

    ; set DATA port as input 
    .macro _data_input
        clr DATA_CR1  ; floating input
        clr DATA_CR2  ; disable ineterrupt
        clr DATA_DDR  ; input mode
    .endm 

    ;configure data port for output 
    ; and enable eeprom ~OE bit.
    .macro _config_write 
        _data_output
        _eeprom_disable_output
    .endm 

    ; configure data port for input 
    ; and disable eeprom ~OE bit 
    .macro _config_read 
        _data_input
        _eeprom_enabe_output
    .endm 

    ; data in A 
    .macro _eeprom_write  
        _eeprom_we_low 
        nop 
        nop 
        ld DATA_ODR,a
        nop
        nop 
        _eeprom_we_high   
    .endm 

    ; read eeprom data in A 
    .macro _eeprom_read  
        ld a,DATA_IDR  
    .endm 


;;--------------------------------------
    .area CODE
;;--------------------------------------


;--------------------------------------------------
; command line interface
; input formats:
;       hex_number  -> display byte at that address 
;       hex_number.hex_number -> display bytes in that range 
;       hex_number: hex_byte [hex_byte]*  -> write to EEPROM data bytes  
;       hex_number"STRING   write string in EEPROM 
;       hex_numberXhex_number  erase range filling eeprom with FF 
;----------------------------------------------------
; operatiing modes 
    NOP=0
    READ=1 ; single address or block
    STORE=2 
    ERASE=3 ; fill range with 0xFF 

    ; get next character from input buffer 
    .macro _next_char 
    ld a,(y)
    incw y 
    .endm ; 4 bytes, 2 cy 


;---------------------------
; initialize ports used 
; to interface to EEPROM 
; PORT G  address bits 15:8 
; PORT D  address bits 7:0 
; PORT B  data bits 
; PORT C  bits 1,2,3 as controls lines 
;----------------------------  
init_ports:
; PORT G (ADDR_HIGH) as output push-pull 
    ld a,#255 
    ld PG_DDR,a ; output 
    ld PG_CR1,a ; push-pull 
    ld PG_CR2,a ; high speed 
    clr ADDR_HIGH     
; PORT D (ADDR_LOW) as outpout push-pull 
    ld PD_DDR,a ; output 
    ld PD_CR1,a ; push-pull 
    ld PD_CR2,a ; high speed 
    clr ADDR_LOW  
; PORT C (control lines) bits 1,2,3 as output push-pull 
    ld a,#(1<<EEPROM_NCE)+(1<<EEPROM_NOE)+(1<<EEPROM_NWE)
    ld PC_ODR,a ; all control lines to high 
    ld PC_CR1,a ; push-pull 
    ld PC_CR2,a ; high-speed 
    ld PC_DDR,a ; output 
    _eeprom_enable 
    _config_read
    ret 

;----------------------
;  eeProg entry point 
;---------------------
EEPROG_tibFO: .asciz "eeProg, Copyright Jacques Deschenes, 2025\nversion "
eeProg:
    call clr_screen
    ldw x,#EEPROG_tibFO 
    call puts 
    ld a,#MAJOR
    call print_dec 
    ld a,#'. 
    call putc 
    ld a,#MINOR 
    call print_dec
    ld a,#'. 
    call putc 
    ld a,#REV 
    call print_dec
    call new_line 
    call init_ports 
    clr a 
    _clrz xamadr 
    _clrz storadr 
    _clrz last  
cli: 
    call new_line
    ld a,#'# 
    call putc ; prompt character 
    clr a
    clr tib
    call readln
; analyze input line      
    ldw y,x  
    _clrz mode 
next_char:     
    _next_char
    tnz a     
    jrne parse01
; at end of line 
     tnz mode 
     jreq cli 
    call exam_block 
    jra cli 
parse01:
    cp a,#'" 
    jrne 1$ 
    call write_string
    jra cli 
1$: 
    cp a,#'X 
    jrne 2$ 
    ld a,#ERASE 
    _straz mode 
    jra next_char 
2$:    
    cp a,#':
    jrne 5$ 
    call write_eeprom 
    jra cli     
5$:
    cp a,#'. 
    jrne 7$ 
    ld a,#READ 
    _straz mode  
;    tnz mode 
;    jreq cli ; here mode should be set to 1 
    jra next_char 
7$: 
    cp a,#SPACE 
    jreq next_char ; skip separator and invalids characters  
    call parse_hex ; maybe an hexadecimal number 
    tnz a ; unknown token ignore rest of line
    jreq cli 
    tnz mode 
    jreq 9$
    ld a,#ERASE 
    cp a,mode 
    jrne 8$
    call erase_range 
    jra cli
8$:
    call exam_block
    jra next_char
9$:
    _strxz xamadr 
    _strxz storadr
    _incz mode
    jra next_char 


;-------------------------------------
; write to eeprom 
; write data to pad then transfert to eeprom  
; read byte list from input buffer
; all bytes must be in same page.
; i.e. only bit 5:0 of address change
; maximum 64 bytes at once.
; if delay between _eeprom_write >150µSec 
; programming phase start.
;--------------------------------------
    PAGE_CNTR=1
write_eeprom:
    push #EEPROM_PAGE_SIZE ; bytes per eeprom page 
; load data in pad 
    ldw x,#pad 
    _strxz ptr16
1$: 
; skip spaces 
    _next_char 
    cp a,#SPACE 
    jreq 1$
    cp a,#'; 
    jreq 9$  
    call parse_hex
    tnz a 
    jreq 9$
    ld a,xl 
    ld [ptr16],a 
    inc ptr8  
    dec (PAGE_CNTR,sp)
    jreq 9$ 
    jra 1$ 
9$: ld a,#EEPROM_PAGE_SIZE 
    sub a,(PAGE_CNTR,sp)
    jreq 10$
    call prog_eeprom 
10$:
    _clrz mode 
    _drop 1 
    ret 

;-------------------------------------------
; display memory in range 'xamadr'...'last' 
;-------------------------------------------    
    ROW_SIZE=1
    VSIZE=1
exam_block:
    _vars VSIZE
    _config_read ; to read data from eeprom  
    _ldxz xamadr
new_row: 
    ld a,#16
    ld (ROW_SIZE,sp),a ; bytes per row 
    call print_adr ; display address and first byte of row 
    ldw y,#tib 
row:
    call print_mem ; display byte at address  
    cpw x,last 
    jrmi 2$
1$:
    call print_text 
    jra 9$ 
2$:     
    incw x 
    jreq 1$ ; overflow 
    dec (ROW_SIZE,sp)
    jrne row
    call print_text 
    jra new_row 
9$: incw x
    _strxz xamadr
    _strxz last 
    _clrz mode 
    _drop VSIZE 
    ret  

;--------------------------------
; print ASCII chr for this row  
;--------------------------------
print_text:
    pushw x 
    ldw x,#2 
    call spaces 
    clr (y)
    ldw x,#tib 
    call puts 
    call new_line       
    popw x 
    ret 

;----------------------------
; parse hexadecimal number 
; from input buffer 
; input:
;    A   first character 
;    Y   pointer to TIB 
; output: 
;    X     number 
;    Y     point after number 
;-----------------------------      
parse_hex:
    push #0 ; digits count 
    clrw x
1$:    
    cp a,#'G 
    jrpl 9$ 
    sub a,#'0
    jrmi 9$ 
    cp a,#10 
    jrmi 2$   ; 0..9 
    cp a,#17 
    jrmi 9$ 
    sub a,#7 
2$: ; shift hex digit in X 
    push #4 ; bit count 
    swap a ; bits in 7:4 
3$:
    sll a 
    rlcw x 
    dec (1,sp)
    jrne 3$
    pop a
    inc (1,sp) ; digits count  
    _next_char 
    tnz a 
    jrne 1$
9$: ; end of hex number
    decw y  ; put back last character  
    pop a ; hex digits count
    tnz a 
    jreq 10$ ; no hex number 
    _strxz last 
10$:
    ret 

;-----------------------------------
;  print address in xamadr variable
;  followed by ': '  
;  input: 
;    X     address to print 
;  output:
;   X      not modified 
;-------------------------------------
print_adr: 
    callr print_word 
    ld a,#': 
    call putc 
    call space
    ret 

;-------------------------------
;  print hexadecimal number 
; input:
;    X  number to print 
; output:
;    none 
;--------------------------------
print_word: 
    ld a,xh
    call print_hex  
    ld a,xl 
    call print_hex  
    ret 

;-------------------------------------
;  print byte at memory location 
;  pointed by X followed by ' ' 
;  input:
;     X     memory address 
;  output:
;    X      not modified 
;-------------------------------------
print_mem:
    call eeprom_addr 
    _eeprom_read
    push a 
    cp a,#SPACE  
    jrmi 1$ 
    cp a,#127
    jrmi 2$ 
1$:
    ld a,#SPACE
2$:     
    ld (y),a 
    incw y
    pop a 
    call print_hex  
    call space 
    ret 


;------------------------------
; program data in pad to eeprom 
; input:
;    A     byte count 
;    pad   data 
;    storadr  where to store data 
;-------------------------------
prog_eeprom:
    push a ; bytes to program 
    _config_write
    ldw y,#pad 
    _ldxz storadr 
1$:
    call eeprom_addr 
    incw x 
    ld a,(y)
    incw y 
    _eeprom_write 
    dec (1,sp)
    jrne 1$ 
    _strxz storadr 
    _config_read 
    _prog_delay
    _drop 1
    ret 

;---------------------------
; set eeprom address 
; input:
;    X     address 
; output:
;    X     preserved 
;---------------------------
eeprom_addr:
    push a 
    ld a,xh 
    ld ADDR_HIGH,a 
    ld a,xl 
    ld ADDR_LOW,a 
    pop a
    ret 

;-----------------------------
; copy tib to eeprom as 
; .asciz 
; input: 
;   tib 
; string max length 63 char.
;-----------------------------
write_string:
    _config_write 
    ldw x,y 
    call strlen
    tnz a 
    jreq 10$
    inc a 
    cp a,#EEPROM_PAGE_SIZE
    jrmi 1$ 
    ld a,#PAD_SIZE 
1$: push a 
    _ldxz storadr 
2$:
    ld a,(y)
    incw y 
    call eeprom_addr 
    incw x 
    _eeprom_write 
    dec (1,sp)
    jrne 2$
    _strxz storadr
    _prog_delay
    _config_read 
    _drop 1  
10$:
    ret 

;----------------------------
;  erasse EEPROM range 
;  filling with 0xFF value 
;  cmd format: addr1Xaddr2 
;----------------------------
    COUNT=1
    VSIZE=2
erase_range:
    _vars VSIZE 
; fill pad with 0xFF
    clr (COUNT,sp)
    ld a,#EEPROM_PAGE_SIZE
    ld (COUNT+1,sp),a
    ldw x,#pad 
    ld a,#0xff 
1$: 
    ld (x),a 
    incw x 
    dec (2,sp)
    jrne 1$ 
    _ldxz last 
    subw x,storadr 
    incw x 
    ldw (COUNT,sp),x ; count to erase 
2$:
    ldw x,#EEPROM_PAGE_SIZE 
    cpw x,(COUNT,sp)
    jrmi 4$ 
    ldw x,(COUNT,sp)
4$: ld a,xl 
    push a 
    call prog_eeprom 
    pop ptr8 
    clr ptr16 
    ldw x,(COUNT,sp) 
    subw x,ptr16
    ldw (COUNT,sp),x  
    jrne 2$ 
    _drop VSIZE 
    ret 

;--------------------------
; return lenght of string
; input:
;    X   *.asciz 
; output:
;    A     length 
;    X     not changed 
;--------------------------
strlen:
    pushw x
    clr a 
1$:
    tnz (x)
    jreq 9$
    inc a 
    incw x 
    jra 1$
9$: popw x
    ret 
