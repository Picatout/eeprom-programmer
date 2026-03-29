;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  SPI peripheral interface 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;----------------------------
; SPI peripheral 
; configuration 
;----------------------------
spi_config:
; initialize SPI     
	bset    CLK_PCKENR1,#CLK_PCKENR1_SPI 
	ld		a,#(1<<SPI_CR2_SSI)+(1<<SPI_CR2_SSM)
	ld		SPI_CR2,a 
	ld		a,#(1<<SPI_CR1_MSTR)+(1<<SPI_CR1_SPE)
	ld		SPI_CR1,a 
; set PE5 as output 
    bset    PE_ODR,#5 
    bset    PE_DDR,#5 
    bset    PE_CR2,#5 ; fast mode 
	ret 

;------------------------------
; wait transaction completion 
;------------------------------
spi_wait:
    btjt    SPI_SR,#SPI_SR_BSY,.     
    ret

;------------------------------
; wait for data register to be 
; empty to avoid overwrite 
;------------------------------
wait_txe:
    btjf    SPI_SR,#SPI_SR_TXE,.
    ret 

;------------------------
; send byte via SPI 
; input:
;    A 
;------------------------
spi_wr_byte:
    callr   wait_txe 
    ld      SPI_DR,a 
    btjf    SPI_SR,#SPI_SR_RXNE,.
    ld      a, SPI_DR ; clear RXNE bit 
    ret 

;---------------------
; read byte from SPI 
; output:
;    A 
;---------------------
spi_rd_byte:
    callr   wait_txe 
    mov     SPI_DR,#0 
    btjf    SPI_SR,#SPI_SR_RXNE,.
    ld      a,SPI_DR 
    ret 

