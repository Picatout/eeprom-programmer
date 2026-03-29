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


;;--------------------------------------------
;; at28C64B || at28c256  EEPROM programmer 
;; version 3 add support for W25Q80 spi flash 
;;--------------------------------------------

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

; version 3 update 
; support w25q80dv spi flash memory 
; W25Q80DV (20 bits address) 1MB 

; version 2 update 
; can work with 19 bits PLCC-32 EEPROM
; SST39SF010 (17 bits address) 128KB
; SST39SF020 (18 bits address) 256KB 
; SST39SF040 (19 bits address) 512KB 

    ADDR_UPPER=PE_ODR  ; bits 16..18
    ADDR_HIGH=PG_ODR   ; bits 8..15
    ADDR_LOW=PD_ODR    ; bits 0..7 
    DATA_ODR=PB_ODR
    DATA_IDR=PB_IDR 
    DATA_DDR=PB_DDR
    DATA_CR1=PB_CR1 
    DATA_CR2=PB_CR2  
    EEPROM_CTRL=PC_ODR 
    EEPROM_NCE=BIT1 ; eeprom enable 
    EEPROM_NOE=BIT2 ; eeprom output enable 
    EEPROM_NWE=BIT3 ; eeprom write enable 
    VCC_CTRL=PF_ODR ; control EEPROM Vcc on PF0 
    VCC_BIT=0 

;--------------------------------
;     MACROS 
;--------------------------------

    ; reset eeprom ~CE bit 
    ; activate eeprom
    ; must be in this state 
    ; for read/prog operations  
    .macro _eeprom_nce_low  
        bres EEPROM_CTRL,#EEPROM_NCE 
    .endm 

    ; set eeprom ~CE bit 
    ; put eeprom in low power mode 
    ; data pin are in hi-z state 
    ; can't be read/prog in this state 
    .macro _eeprom_nce_high 
        bset EEPROM_CTRL,#EEPROM_NCE
    .endm 

    ; reset eeprom ~OE bit
    ; must be low to read eeprom  
    .macro _eeprom_noe_low 
        bres EEPROM_CTRL,#EEPROM_NOE
        nop  
    .endm 

    ; set eeprom ~OE bit
    ; must be high to program eeprom  
    .macro _eeprom_noe_high 
        bset EEPROM_CTRL,#EEPROM_NOE
        nop 
    .endm 

    ; reset eeprom ~WE bit 
    .macro _eeprom_nwe_low  
        bres EEPROM_CTRL,#EEPROM_NWE
    .endm 

    ; set eeprom ~WE bit 
    .macro _eeprom_nwe_high 
        bset EEPROM_CTRL,#EEPROM_NWE
    .endm 

    ; AT28C eeprom programming delay 
    ; 10msec per 64 bytes page 
    .macro _prog_delay 
        ld a,#10
        _straz timer+1
        clr  timer  
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
    ; and disable eeprom ~OE bit.
    .macro _config_write 
        _eeprom_noe_high
        _data_output
    .endm 

    ; configure data port for input 
    ; and enable eeprom ~OE bit 
    .macro _config_read 
        _eeprom_nwe_high 
        _data_input
    .endm 

    ; data in A 
    .macro _eeprom_write  
        _eeprom_nwe_low 
        nop 
        ld DATA_ODR,a
        nop
        _eeprom_nwe_high
    .endm 

    ; read eeprom data in A 
    .macro _eeprom_read  
        _eeprom_noe_low 
        ld a,DATA_IDR   
        _eeprom_noe_high 
    .endm 

    ; power EEPROM on  
    .macro _eeprom_on 
        bres VCC_CTRL,#VCC_BIT 
    .endm 

    ; power EEPOM off
    .macro _eeprom_off  
        bset VCC_CTRL,#VCC_BIT 
    .endm 

;-----------------------------
;  24 bits variables operations
;-----------------------------
    ; increment 24 bits value 
    ; value in A:X 
    .macro _inc24
    addw x,#1 
    adc a,#0
    .endm 

    ; incremeent 24 bits variable 
    .macro _inc_v24 v 
    ld a, v+2
    add a,#1 
    ld v+2,a 
    ld a, v+1 
    adc a,#0 
    ld v+1,a  
    ld a, v 
    adc a,#0 
    ld v,a  
    .endm 

    ; load 24 bits variable in A:X 
    .macro _ld24 addr 
    ld a,addr 
    ldw x, addr+1
    .endm 

    ; store 24 bits variable from A:X 
    .macro _str24 addr 
    _straz addr 
    ldw addr+1, x
    .endm 

    ; move 24 bits variable dest src 
    .macro _mov_v24 dest,src 
    ld a,src
    ldw x, src+1 
    ld  dest, a 
    ldw dest+1,x  
    .endm 

    .macro _cp_v24 v1 v2 
    ld a,v1 
    ldw x, v1+1 
    subw x,v2+1
    sbc a,v2
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
;       hex_numberS eeprom size in bytes   
;       {0,1}T  set eeprom type  AT28 or SST39
;       {0,1}V  eeprom Vcc off,on 
;----------------------------------------------------
; operatiing modes 
    NOP=0
    READ=1 ; single address or block
    STORE=2 
    ERASE=3 ; fill range with 0xFF 

; eeprom programming buffe size 
    DEFAULT_PAGE_SIZE=64 ; AT28C(BV)64B and AT28C(BV)256 

; eeprom types 
    AT28=0   ; AT28Cxxx (Vcc=5v), AT28BVxxx (Vcc=3.3V)
    SST39=1  ; SST39SFxxx (Vcc=5V), SST39LFxxx (Vcc=3.3V), SST39VFxxx (Vcc=3.3V)
; spi flash type 
    W25Q=2  ; W25Q80DV spi flash memory (vCC=3.2V) page size 256 bytes, minimum erase sector 4KB  


DEFAULT_LIMIT = 0x1FFF ; 8KO eeprom 


    ; get next character from input buffer 
    .macro _next_char 
    ld a,(y)
    incw y 
    .endm ; 4 bytes, 2 cy 


;---------------------------
; initialize ports used 
; to interface to EEPROM 
; PORT E  address bits 18:16 
; PORT G  address bits 15:8 
; PORT D  address bits 7:0 
; PORT B  data bits 
; PORT C  bits 1,2,3 as controls lines 
;----------------------------  
init_ports:
; PORT E (ADDR_UPPER) as output push-pull
; bits 18:16 
   ld a,PE_DDR 
   or a,limit 
   ld PE_DDR,a ; bits 0..2 as output
   ld  a,PE_CR1  
   or  a, limit  
   ld PE_CR1,a ; bits 0..2 push pull output 
   ld  a,PE_CR2  
   or  a, limit 
   ld PE_CR2,a ; bits 0..2 high speed 
   clr ADDR_UPPER       
; PORT G (ADDR_HIGH) as output push-pull 
; bits 15:8 
    ld a,limit+1  
    ld PG_DDR,a ; output 
    ld PG_CR1,a ; push-pull 
    ld PG_CR2,a ; high speed 
    clr ADDR_HIGH     
; PORT D (ADDR_LOW) as outpout push-pull 
; bits 7:0 
    ld a,limit+2 
    ld PD_DDR,a ; output 
    ld PD_CR1,a ; push-pull 
    ld PD_CR2,a ; high speed 
    clr ADDR_LOW  
; PORT C (control lines) bits 1,2,3 as output push-pull 
    ld a,PC_ODR 
    or a,#(1<<EEPROM_NCE)+(1<<EEPROM_NOE)+(1<<EEPROM_NWE)
    ld PC_ODR,a ; all control lines to high 
    ld a,PC_CR1 
    or a,#(1<<EEPROM_NCE)+(1<<EEPROM_NOE)+(1<<EEPROM_NWE)
    ld PC_CR1,a ; push-pull 
    ld a,PC_CR2  
    or a,#(1<<EEPROM_NCE)+(1<<EEPROM_NOE)+(1<<EEPROM_NWE)
    ld PC_CR2,a ; high-speed
    ld a,PC_DDR 
    or a,#(1<<EEPROM_NCE)+(1<<EEPROM_NOE)+(1<<EEPROM_NWE)    
    ld PC_DDR,a ; output 
    _eeprom_nce_low  
    _config_read
    ret 

;----------------------
;  eeProg entry point 
;---------------------
EEPROG_INFO: .asciz "\reeProg, Copyright Jacques Deschenes, 2025\rversion "
eeProg:
    mov base,#16 
    call clr_screen
    ldw x,#EEPROG_INFO 
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
    clr  limit 
    ldw x,#DEFAULT_LIMIT
    ldw limit+1,x 
    call init_ports 
; set default limit for 8KB eeprom     
    mov page_size, #DEFAULT_PAGE_SIZE
; row delay default to 4 msec 
    mov RowDelay,#4     
; set eeprom type to AT28xxxx 
    mov eeType,#AT28     
; clear pointer variables 
    clr a 
    clrw x 
    _str24 xamadr 
    _str24 storadr 
    _str24 last 
eeProg_1:
	ldw x,#STACK_EMPTY ; in case CTRL_C was used 
	ldw sp,x
    bset flags,#FUPPER ; commands all upper case 
cli: 
    _eeprom_off 
    call new_line
    ld a,#'# 
    call putc ; prompt character 
    clr a
    clr tib
    call readln
    jreq cli 
    ldw y,x 
    _eeprom_on
; Vcc eeprom stabilisation delay      
    ldw x,#50 
    call pause
; analyze input line       
    clr  mode 
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
    cp a,#SPACE
    jrule next_char
; write string test
    cp a,#'" 
    jrne 1$
    _mov_v24 storadr, xamadr 
    call write_string
    tnz a
    jreq 44$ 
    jra  42$ 
1$: ; erase range test
    cp a,#'X 
    jrne 2$ 
    ld a,#ERASE 
    _straz mode 
    _mov_v24 storadr, xamadr
    jra next_char 
2$:
; erase all test 
    cp a,#'*
    jrne 24$ 
    call erase_all 
    jra cli 
24$:
    cp a,#'S ; eeprom size in bytes   
    jrne 3$
;'limit' test     
    ld a,xamadr+2 
    sub a,#1 
    _straz limit+2
    ld a,xamadr+1 
    sbc a,#0 
    _straz limit+1
    ld a,xamadr 
    sbc a,#0
    _straz limit
    call init_ports ; adjust address bits to eeprom size 
    clr  mode 
    jra next_char 
3$: 
    cp a,#'M ; row delay msec 
    jrne 34$
    ld a,xamadr+2 
    and a,#15 
    _straz RowDelay
    clr  mode 
    jp cli 
34$:
    cp a,#'T ; eeprom type: 0->AT28,1->SST39, 2-> W25Q80  
    jrne 4$
    ld a,xamadr+2 
    and a,#3
    _straz eeType
    clr  mode
    jp next_char
4$: ; store test 
    cp a,#':
    jrne 5$ 
    call write_eeprom
    tnz a  
    jreq 44$
42$: 
    ld a,#NAK 
    call uart_putc 
    jp cli 
44$: 
    ld a,#ACK 
    call uart_putc
    jp cli    
5$: ; block exam test 
    cp a,#'. 
    jrne 7$ 
    ld a,#READ 
    _straz mode  
    jp next_char 
7$:
    call parse_hex ; maybe an hexadecimal number 
    tnz a ; unknown token ignore rest of line
    jrne 64$
    jp next_char 
64$: 
    tnz mode 
    jreq 9$
    ld a,#ERASE 
    cp a,mode 
    jrne 8$
    call erase_range 
    jp cli     
8$:
    call exam_block
    jp cli
9$:
    _mov_v24 xamadr, last  
    inc mode
    jp next_char 


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
write_eeprom:
    _mov_v24 storadr, xamadr 
    ld a,page_size 
; copy data to pad 
    ldw x,#pad 
    ldw ptr16,x  
    clr count 
1$: 
; skip spaces 
    _next_char 
    cp a,#SPACE 
    jreq 1$
    call parse_hex
    tnz a 
    jreq 9$
    ld a,xl 
    ld [ptr16],a 
    inc ptr8
    jrne 2$ 
    inc ptr16 
2$:   
    inc count 
    jreq 9$ 
    jra 1$ 
9$: ld a,count  
    cp a,page_size 
    jrule 10$
    ld a,page_size 
    _straz count 
10$:
    tnz   a 
    jreq  at28_prog_eeprom
    cp    a,#SST39  
    jreq  12$
    jp    w25q_write_buffer
12$:
    jp    sst39sf0xx_prog_eeprom 


;------------------------------
; program data in pad to AT28 eeprom 
; limited to 64 bytes  
; input:
;    count byte count 
;    pad   data 
;    storadr  where to store data 
;-------------------------------
at28_prog_eeprom:
    push count ; bytes to program 
    _config_write
    ldw y,#pad 
1$: clr a 
    ldw x, storadr+1 
    call eeprom_addr 
    incw x 
    ldw storadr+1, x
    ld a,(y)
    incw y 
    _eeprom_write 
    ld a,page_size 
    dec a
    and a,storadr+2
    jrne 2$
; this page is full program it.     
    _config_read
    call toggle_polling
    _config_write  
2$:
    dec (1,sp)
    jrne 1$ 
    _config_read
.if 0
; delay
    _prog_delay
.else
    call toggle_polling
.endif 
    _drop 1
    jp verify_prog

;-----------------------------
; copy tib to eeprom as 
; .asciz 
; input: 
;   tib 
; string max length 63 char.
;-----------------------------
write_string:
; copy string to pad 
    ldw x,#pad 
    clr  count 
1$:
    _next_char
    jreq 2$ 
    cp a,#'"' 
    jreq 2$ 
    ld (x),a 
    incw x 
    inc count 
    jra 1$ 
2$: clr (x)
    inc count 
    btjt eeType,#0,3$ 
    jp at28_prog_eeprom 
3$:
    jp sst39sf0xx_prog_eeprom  


;-------------------------------------------
; display memory in range 'xamadr'...'last' 
;-------------------------------------------    
    ROW_SIZE=1
    VSIZE=1
exam_block:
    ld  a,#W25Q 
    cp  a,eeType
    jrne 1$
    jp w25q_dump 
1$:
    _vars VSIZE
    _config_read ; to read data from eeprom  
new_row: 
    ld a,#16
    ld (ROW_SIZE,sp),a ; bytes per row 
    ld a,xamadr 
    ldw x, xamadr+1
    call print_adr ; display address and first byte of row 
    ldw y,#tib 
row:
    ld a,xamadr 
    ldw x, xamadr+1
    call print_mem ; display byte at address  
    _cp_v24  xamadr last
    jrult 2$   
1$:
    call print_text 
    jra 9$ 
2$:     
    _inc_v24 xamadr 
    _cp_v24 limit xamadr 
    jrult 1$
3$:
    dec (ROW_SIZE,sp)
    jrne row
    call print_text
; 2 msec delay between lines     
    ld a,RowDelay
    jreq new_row 
    ld timer+1,a 
    bset flags,#FTIMER 
    btjt flags,#FTIMER,. 
    jra new_row 
9$: 
    _inc_v24 xamadr
    _mov_v24 last xamadr 
    _drop VSIZE 
    ret  

;--------------------------------
; print ASCII chr for this row  
;--------------------------------
print_text:
    pushw x 
    ld a,#';
    call putc 
    call space 
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
;    last parsed hexnumber if any
;    A     number of digits  
;    Y     point after number 
;-----------------------------      
    DIGITS_CNT=2 
    UPPER_BYTE=1
    VAR_SIZE=2
parse_hex:
    _vars VAR_SIZE
    clrw x 
    ldw (UPPER_BYTE,sp),x 
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
2$: ; rotate A -> X ->  UPPER_BYTE 
    swap a
    sll a  
    rlcw x 
    rlc (UPPER_BYTE,sp)  
    sll a  
    rlcw x 
    rlc (UPPER_BYTE,sp)  
    sll a  
    rlcw x 
    rlc (UPPER_BYTE,sp)  
    sll a  
    rlcw x 
    rlc (UPPER_BYTE,sp)  
    inc (DIGITS_CNT,sp) ; digits count  
    _next_char 
    tnz a 
    jrne 1$
9$: ; end of hex number
    decw y  ; put back last character  
    ld a,(UPPER_BYTE,sp)
    tnz (DIGITS_CNT,sp) ; hex digits count
    jreq 10$
    _str24 last
10$: 
    ld a,(DIGITS_CNT,sp)
    _drop VAR_SIZE 
    ret 

;-----------------------------------
;  print address in xamadr variable
;  followed by ': '  
;  input: 
;    A     upper byte 
;    X     address to print 
;  output:
;   A:X      not modified 
;-------------------------------------
print_adr:
    push a 
    call print_hex 
    callr print_word 
    ld a,#': 
    call putc 
    call space
    pop a 
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
;     A:X     memory address 
;  output:
;    A:X      not modified 
;-------------------------------------
print_mem:
    push a 
    pushw x 
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
    popw x 
    pop a 
    ret 

;--------------------------
; read back data 
; and compare to pad 
; input:
;    count   byte count 
;    pad     reference data 
;    xamadr  flash_address range start  
; outpu:
;    A    0 ok, otherwise failed  
;--------------------------
verify_prog:
    push count 
    ldw y,#pad 
1$:
    ldw x, xamadr+1 
    ld a, xamadr  
    call eeprom_addr  
    _eeprom_read 
    xor a,(y)
    jrne 9$
    dec (1,sp)
    jreq 9$
    incw y 
    _inc_v24 xamadr
    jra 1$ 
9$:  
    _drop 1     
    ret 

;---------------------------
; set eeprom address 
; input:
;    A:X     address 
; output:
;    A:X     preserved 
;---------------------------
eeprom_addr:
    push a 
    ld ADDR_UPPER,a  
    ld a,xh 
    ld ADDR_HIGH,a 
    ld a,xl 
    ld ADDR_LOW,a 
    pop a
    ret 

;----------------------------
;  erase EEPROM range 
;  filling with 0xFF value 
;  cmd format: addr1Xaddr2 
;----------------------------
erase_range:
    tnz  eeType 
    jreq at28_range_erase 
    ld  a,#SST39 
    cp  a,eeType 
    jrne 1$
    jp sst39sf0xx_range_erase
1$: jp w25q_erase_range 

;------------------------------
; AT28 type range erase  
;------------------------------
    COUNT=1  
    VSIZE=2 
at28_range_erase:
    push #0
    push #0  
; fill pad with 0xFF
    ldw x,#pad 
    ld a,#0xff 
1$: 
    ld (x),a 
    incw x 
    dec (COUNT,sp)
    jrne 1$ 
    ldw x,last+1 
    subw x,storadr+1 
    incw x 
    ldw (COUNT,sp),x ; count to erase 
2$:
    clrw x 
    ld a, page_size 
    ld xl,a 
    cpw x,(COUNT,sp)
    jrmi 4$ 
    ldw x,(COUNT,sp)
4$: ld a,xl 
    ld count, a 
    call at28_prog_eeprom 
    ld a, count 
    ld ptr8, a 
    clr ptr16 
    ldw x,(COUNT,sp) 
    subw x,ptr16
    ldw (COUNT,sp),x  
    jrne 2$ 
    _drop VSIZE 
    ret 

;-------------------------
; erase all eeprom 
;-------------------------
erase_all:
    tnz   eeType 
    jrne  1$ 
; AT28 EEPROM type 
    clr  storadr 
    clr  storadr+1
    clr  storadr+2 
    _mov_v24 last, limit 
    jra erase_range 
1$: ld  a,#SST39 
    cp  a,eeType 
    jrne 2$ 
    ; 39SF0xx eeprom type 
    jp sst39sf0xx_chip_erase
2$:    ; w25q80dv 
    jp w25q_erase_chip

;-----------------------------
; bit toggle at each read 
; while programming or erase 
; loop until bit 6 stable 
; NOTE:
;  expect _config_read done 
;  before call.
;-----------------------------
    B6_MASK=(1<<6)
    LAST_READ=1
toggle_polling:
    push #0
; set timemout limit 101msec
; according to specs sst39sf* chip erase 
; take 100msec maximum.
    ldw x,#101 
    ldw timer,x
    bset flags,#FTIMER
    _eeprom_read 
    and a,#B6_MASK
1$: 
    ld (LAST_READ,sp),a 
; ~ 1µSec delay 
    ld a,#5 
3$: dec a 
    jrne 3$
    btjt flags,#FTIMER,2$ 
; if timeout 
; print message 
    ldw x,#timeout 
    call puts 
    jp eeProg_1 
2$: ; 2 consecutive read must be equal 
    _eeprom_read
    and a,#B6_MASK 
    cp a,(LAST_READ,sp)
    jrne 1$ 
    _eeprom_read
    and a,#B6_MASK
    cp a,(LAST_READ,sp)
    jrne 1$ 
9$:
    _drop 1 
    ret 
timeout: .asciz "operation timeout"

;---------------------------
; substract 24 bits values
; A:X with value on stack 
; result= A:X-(3,SP)
; input:
;   A:X   v1 
;   (3,SP) v2 
; output:
;   flags Z,N,C 
;   A:X 
;-------------------------
    V2=3
sub24:
    subw x,(V2+1,sp)
    sbc a,(V2,sp)
    ret 


;---------------------------------
;---------------------------------
;  SST39SF0XX eeprom procedures
;
; these eeprom can be programmed 
; only one byte at a time 
; 15 µSec delay between each byte
; special sequences are required 
; for:
;   write byte 
;   sector erase 
;   chip erase.
;---------------------------------
;---------------------------------

; sst39sf0xx commands bytes 
SST39_KEY_ADDR=0x5555
SST39_NKEY_ADDR=0x2AAA 
SST39_KEY=0xAA 
SST39_NKEY=0x55
SST39_CMD_ADDR=SST39_KEY_ADDR  
SST39_WRITE_CMD=0xA0
SST39_ERASE_CMD=0x80 
SST39_SECTOR_ERASE=0x30
SST39_CHIP_ERASE=0x10
SST39_ID_ENTER=0x90 
SST39_ID_EXIT=0xFF 

SST39_SECTOR_SIZE=4096 ; erase sector size in bytes 

;--------------------------------
; send  command byte to sst39sf0xx eeprom 
; input:
;    a    cmd byte 
;    x    address (0x5555||0x2AAA) 
;--------------------------------
sst39sf0xx_send_cmd:
    push a 
    clr a 
    call eeprom_addr 
    pop a 
    _eeprom_write 
    ret 

;--------------------------
; write/erase unlock 
; 5555 AA 2AAA 55
;-------------------------- 
sst39sf0xx_send_prefix:
    ld a,#SST39_KEY 
    ldw x,#SST39_KEY_ADDR 
    call sst39sf0xx_send_cmd  
    ld a,#SST39_NKEY 
    ldw x,#SST39_NKEY_ADDR 
    call sst39sf0xx_send_cmd 
    ret 

;-------------------------------
; SST39SF0xx write byte 
; input:
;   storadr  byte address 
;   A        data 
;-------------------------------
sst39sf0xx_write_byte:
    push a 
    _config_write
    call sst39sf0xx_send_prefix 
    ld a,#SST39_WRITE_CMD
    ldw x,#SST39_CMD_ADDR
    call sst39sf0xx_send_cmd    
    ld a,storadr 
    ldw x, storadr+1
    call eeprom_addr  
    pop a 
    _eeprom_write
    _config_read 
    call toggle_polling
    ret 

;--------------------------------
; write data to eeprom 
;input:
;   storadr   first address 
;             auto increment 
;   count     of bytes to write
;--------------------------------
sst39sf0xx_prog_eeprom: 
    push count 
    ldw y,#pad 
1$:
    ld a,(y)
    incw y  
    call sst39sf0xx_write_byte
    dec (1,sp) 
    jreq 4$
    _inc_v24 storadr
    _cp_v24 limit, storadr 
    jruge 1$     
4$: ; verify write 
    _drop 1
    jp verify_prog


;---------------------------------
; SST39SF0XX chip erase 
; command sequence:
; prefix 0x5555 0x80
; prefix 0x5555 0x10 
; erase time: 70msec 
;---------------------------------
sst39sf0xx_chip_erase:
; save used registers 
    push a 
    pushw x 
; send prefix  
    _config_write 
   call sst39sf0xx_send_prefix 
; erase command first byte  
    ld a,#SST39_ERASE_CMD
    ldw x,#SST39_KEY_ADDR
    call sst39sf0xx_send_cmd 
; prefix sequence again 
    call sst39sf0xx_send_prefix 
; erase chip command 2d byte 
    ld a,#SST39_CHIP_ERASE
    ldw x,#SST39_KEY_ADDR
    call sst39sf0xx_send_cmd 
    _config_read 
    ldw x,#100 
    ldw timer,x  
    bset flags,#FTIMER 
    btjt flags,#FTIMER,.
;restore saved registers     
    popw x 
    pop a 
    ret 


;----------------------------
; minimum erase is 4096 bytes 
; sector  (0x1000)
;----------------------------
sst39sf0xx_range_erase:
; sector align address 
; clear storadr bits 11:0 
    clr  storadr+2
    ld a,storadr+1
    and a,#0xf0 
    _straz storadr+1
1$:
    call sst39sf0xx_sector_erase  
    ldw x, storadr 
    addw x,#SST39_SECTOR_SIZE 
    ldw storadr,x 
    cpw x,#last 
    jrult 1$
    ret 

;-------------------------------
; SST39SF0XX sector erase 
; a sector is 4096 bytes 
; command sequence:
; prefix 0x5555 0x80 
; prefix SA 0x30
; SA is sector address
; input:
;    storadr+1 Ams=A12  
;------------------------------
sst39sf0xx_sector_erase:
; send prefix  
    _config_write 
   call sst39sf0xx_send_prefix 
; erase command  byte  
    ld a,#SST39_ERASE_CMD
    ldw x,#SST39_KEY_ADDR
    call sst39sf0xx_send_cmd 
; prefix sequence again 
    call sst39sf0xx_send_prefix 
; set sector address + 0x30 
    ld a,storadr 
    ldw x,storadr+1
    call eeprom_addr 
    ld a,#SST39_SECTOR_ERASE
    _eeprom_write 
    _config_read 
    ldw x,#25 ; maximum for sst39sf0xx 
    ldw timer,x  
    bset flags,#FTIMER 
    btjt flags,#FTIMER,.
    ret 

