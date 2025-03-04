;;
; Copyright Jacques Deschênes 2019 
; This file is part of MONA 
;
;     MONA is free software: you can redistribute it and/or modify
;     it under the terms of the GNU General Public License as published by
;     the Free Software Foundation, either version 3 of the License, or
;     (at your option) any later version.
;
;     MONA is distributed in the hope that it will be useful,
;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;     GNU General Public License for more details.
;
;     You should have received a copy of the GNU General Public License
;     along with MONA.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NUCLEO-8S208RB board specific definitions
; Date: 2019/10/29
; author: Jacques Deschênes, copyright 2018,2019
; licence: GPLv3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; mcu on board is stm8s208rbt6

; crystal on board is 8Mhz
FHSE = 8000000

; LED2 is user LED
; connected to PC5 via Q2 -> 2N7002 MOSFET
LED_PORT = PC_BASE ;port C  ODR
LED_BIT = 5
LED_MASK = (1<<LED_BIT) ;bit 5 mask

     
;  user interface UART via ST-LINK , (T-VCP)

UART=UART1
; port used by  UART1  
UART_PORT_ODR=PA_ODR 
UART_PORT_DDR=PA_DDR 
UART_PORT_IDR=PA_IDR 
UART_PORT_CR1=PA_CR1 
UART_PORT_CR2=PA_CR2 

; clock enable bit 
UART_PCKEN=CLK_PCKENR1_UART1

; UART1 registers 
UART_SR=UART1_SR
UART_DR=UART1_DR
UART_BRR1=UART1_BRR1
UART_BRR2=UART1_BRR2
UART_CR1=UART1_CR1
UART_CR2=UART1_CR2

; TX, RX pin
UART_TX_PIN=UART1_TX_PIN
UART_RX_PIN=UART1_RX_PIN 


