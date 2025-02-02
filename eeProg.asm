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

    .macro _enable_eeprom 
        bres EEPROM_CTRL,#EEPROM_NCE 
    .endm 

    .macro _disable_eeprom 
        bset EEPROM_CTRL,#EEPROM_NCE
    .endm 

    .macro _enabe_eeprom_output 
        bres EEPROM_CTRL,#EEPROM_NOE 
    .endm 

    .macro _disable_eeprom_output 
        bset EEPROM_CTRL,#EEPROM_NOE
    .endm 

    .macro _enable_eeprom_write 
        bres EEPROM_CTRL,#EEPROM_NWE
    .endm 

    .macro _disable_eeprom_write 
        bset EEPROM_CTRL,#EEPROM_NWE
    .endm 

    ; addres in X 
    .macro _set_eeprom_addr  
        ld a,xh 
        ld ADDR_HIGH,a 
        ld a,xl 
        ld ADDR_LOW,a 
    .endm 

    ; set DATA port as output 
    .macro _data_output 
        ld a,#255 
         DATA_DDR,a ; output mode 
         ld DATA_CR2,a ; high_speed  
    .endm 

    ; set DATA port as input 
    .macro _data_input
        clr DATA_CR2  ; disable ineterrupt
        clr DATA_DDR  ; input mode 
    .endm 

    ; data in A 
    .macro _write_eeprom 
        ld DATA_ODR,a 
    .endm 

    ; read eeprom data in A 
    .macro _read_eeprom 
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
;----------------------------------------------------
; operatiing modes 
    NOP=0
    READ=1 ; single address or block
    STORE=2 

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
; PORT B is bidirectionnal, default to input 
    clr DATA_CR2  ; disable ineterrupt
    clr DATA_DDR  ; input mode 
    ld a,#255
    ld DATA_CR1,a ; pullup  
; PORT C (control lines) bits 1,2,3 as output push-pull 
    ld a,#(1<<EEPROM_NCE)+(1<<EEPROM_NOE)+(1<<EEPROM_NWE)
    ld PC_ODR,a ; all control lines to high 
    ld PC_CR1,a ; push-pull 
    ld PC_CR2,a ; high-speed 
    ld PC_DDR,a ; output 
    _enable_eeprom
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

cli: 
    ld a,#CR 
    call putc
    ld a,#'# 
    call putc ; prompt character 
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
    cp a,#':
    jrne 5$ 
    call write_eeprom 
    jra cli     
5$:
    cp a,#'. 
    jrne 8$ 
    tnz mode 
    jreq cli ; here mode should be set to 1 
    jra next_char 
8$: 
    cp a,#SPACE 
    jreq next_char ; skip separator and invalids characters  
    call parse_hex ; maybe an hexadecimal number 
    tnz a ; unknown token ignore rest of line
    jreq cli 
    tnz mode 
    jreq 9$
    call exam_block
    jra next_char
9$:
    _strxz xamadr 
    _strxz storadr
    _incz mode
    jra next_char 

;-------------------------------------
; write to eeprom  
; read byte list from input buffer
;--------------------------------------
write_eeprom:
1$: 
; skip spaces 
    _next_char 
    cp a,#SPACE 
    jreq 1$ 
    call parse_hex
    tnz a 
    jreq 9$ 
    ld a,xl 
    _ldxz storadr 
    ld (x),a 
    incw x 
    _strxz storadr
    jra 1$ 
9$: _clrz mode 
    ret 

;-------------------------------------------
; display memory in range 'xamadr'...'last' 
;-------------------------------------------    
    ROW_SIZE=1
    VSIZE=1
exam_block:
    _vars VSIZE
    _ldxz xamadr
new_row: 
    ld a,#16
    ld (ROW_SIZE,sp),a ; bytes per row 
    call new_line  
    call print_adr ; display address and first byte of row 
    call print_mem ; display byte at address  
row:
    incw x 
    jreq 9$ ; overflow 
    cpw x,last 
    jrugt 9$ 
    dec (ROW_SIZE,sp)
    jreq new_row  
    call print_mem  
    jra row 
9$:
    _clrz mode 
    _drop VSIZE 
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
    ld a,(x) 
    call print_hex  
    call space 
    ret 


