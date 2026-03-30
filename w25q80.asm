;-------------------------------
;  W25180DV flash memory 
;  programming interface 
;-------------------------------

CS_BIT = 5  ; W25180DV select bit on PE 
CMD_WRITE = 2 ; W25180DV write command 
CMD_READ = 3  ; W25180DV read command 
CMD_READ_SR1 = 5 ; read status register 1   
CMD_WR_EN = 6 ; W25180DV write enable command 
CMD_SECT_ERASE = 0x20 ; erase 4KB sector 
CMD_CHIP_ERASE = 0x60  ; erase chip 
BUSY_BIT = 0 ; SR1 busy bit 
WEL_BIT = 1 ; SR1 WEL bit  

;------------------------
; select device 
;------------------------
w25q_select:
    bres    PE_ODR,#CS_BIT
    bset    PE_DDR,#CS_BIT 
    ret 

;-----------------------
; deselect device 
;------------------------
w25q_deselect: 
    call    spi_wait 
    bset    PE_ODR,#CS_BIT 
    bres    PE_DDR,#CS_BIT
    ret 

;------------------------
; align address on 
; sector 
; input:
;   A:X  
; output:
;   A:X 
;------------------------
w25q_sect_algn:
    addw    x,#4096
    adc     a,#0 
    ret 

;-----------------------
; move address to next 
; page
; W25Q80DV programming 
; page are 256 bytes 
; input:
;   A:X 
; output:
;   A:X 
;----------------------
w25q_next_page:
    addw    x,#256 
    adc     a,#0
    ret

;--------------------------
; send w25q address 
; device allready selected 
; input:
;   A:X 
;--------------------------
w25q_addr: 
    call    spi_wr_byte 
    rlwa    x 
    call    spi_wr_byte 
    rlwa    x 
    call    spi_wr_byte 
    ret 

;---------------------------
; send write enable command
;---------------------------
w25q_wr_en:
    call    w25q_select 
    ld      a,#CMD_WR_EN 
    call    spi_wr_byte
    call    w25q_deselect 
    ret 

;---------------------------
; read W25Q80DV status 
; register 1 
; output:
;   A 
;----------------------------
w25q_rd_sr1:
    call    w25q_select 
    ld      a,#CMD_READ_SR1 
    call    spi_wr_byte 
    call    spi_rd_byte 
    call    w25q_deselect 
    ret 

;-----------------------------
; wait operation completion 
;-----------------------------
w25q_wait_eop:
1$:
    callr   w25q_rd_sr1
    and     a,#(1<<BUSY_BIT)+(1<<WEL_BIT)
    jrne    1$
    ret 

;-----------------------------
; erase whole W25Q80DV chip 
;-----------------------------
w25q_erase_chip:
    call    w25q_wr_en 
    call    w25q_select 
    ld      a,#CMD_CHIP_ERASE
    call    spi_wr_byte 
    call    w25q_deselect
    callr   w25q_wait_eop 
    ret 

;---------------------------
; erase W25Q80DV sector 
; input:
;   A:X   sector address 
;---------------------------
w25q_erase_sector:
    push    a 
    pushw   x 
    call    w25q_wr_en 
    call    w25q_select 
    ld      a,#CMD_SECT_ERASE 
    call    spi_wr_byte 
    popw    x 
    pop     a 
    call    w25q_addr
    call    w25q_deselect 
    call    w25q_wait_eop 
    ret 

;-----------------------------
; erase multiple sector 
; input:
;   storadr  first sector addr 
;   last     last sector adr 
;-----------------------------
w25q_erase_range:
; align storadr to beginning of sector 
    clr   storadr+2 
    ld    a, storadr+1
    and   a,#0xF0 
    ld    storadr+1,a 
; align last to beginning of sector 
    clr    last+2
    ld     a,last+1
    and    a,#0xF0 
    ld     last+1,a 
1$: 
    ldw    x,storadr+1 
    ld     a,storadr 
    callr  w25q_erase_sector 
    ldw    x,storadr+1 
    ld     a,storadr 
    addw   x,#4096 ; W25180DV sector size 
    adc    a,#0 
    ld     storadr,a 
    ldw    storadr+1,x 
    _cp_v24  storadr, last
    jrult  1$     
    ret 


;--------------------------
; read w25q80 data 
; compare it with pad data 
; if same ok else error 
;--------------------------
w25q_verify:
    call w25q_select
    ld  a,#CMD_READ
    call spi_wr_byte
    ldw x, xamadr+1 
    ld a, xamadr  
    call w25q_addr 
    push count
    ldw  y,#pad  
1$:
    call spi_rd_byte  
    xor a,(y)
    jrne 9$
    dec (1,sp)
    jreq 9$
    incw y 
    jra 1$ 
9$:     
    call    w25q_deselect 
    ret 


;--------------------------
; write buffer to w25q80dv  
; input:
;   storaddr   device address 
;   
;--------------------------
w25q_write_buffer:
    call    w25q_wr_en 
    call    w25q_select 
    ld      a,#CMD_WRITE 
    call    spi_wr_byte
    ld      a,storadr 
    ldw     x,storadr+1 
    call    w25q_addr 
    ldw     y,#pad
    push    count  
1$:
    ld      a,(y)
    incw    y  
    call    spi_wr_byte
    dec     (1,sp) 
    jreq    4$
    _inc_v24 storadr
    jra     1$ 
4$: 
    _drop   1 
    call    w25q_deselect 
    call    w25q_verify
    ret 


;--------------------------
; dump w25q80 memory range  
; input:
;  xamadr   device address 
;  last     last address  
;---------------------------
w25q_dump:
    call    w25q_select
    btjt    PE_IDR,#CS_BIT,. 
    ld      a,#CMD_READ 
    call    spi_wr_byte 
    ldw     x,xamadr+1 
    ld      a,xamadr
    call    w25q_addr     
w25_read_row: ; read 16 bytes 
    ldw     x,xamadr+1 
    ld      a,xamadr 
    call    print_adr
    push    #16
    ldw     y,#pad
    ldw     x,#tib   
1$: 
    call    spi_rd_byte 
    ld      (y),a 
    incw    y 
    cp      a,#32 
    jrpl    2$ 
    ld      a,#32
2$: cp      a,#127 
    jrmi    3$ 
    ld      a,#32 
3$: ld      (x),a 
    incw    x 
    dec     (1,sp)
    jrne    1$
    _drop   1
    clr     (x)
w25_hex_dump: ; display bytes in hexadecimal 
    ldw     y,#pad 
    push    #16
1$:    
    ld      a,(y) 
    call    print_hex
    call    space  
    incw    y 
    dec     (1,sp)
    jrne    1$ 
    _drop   1 
    ldw     x,#tib 
    call    puts
    call    new_line  
    ldw     x,xamadr+1 
    ld      a,xamadr  
    addw    x,#16 
    adc     a,#0 
    ld      xamadr,a 
    ldw     xamadr+1,x 
    _cp_v24 xamadr,last 
    jrmi    w25_read_row   
9$:
    call    w25q_deselect
    ret 

