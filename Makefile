#############################
# eeProg make file
#############################
#############################
# Make file for NUCLEO-8S207K8 board
#############################
BOARD=stm8s208k8
PROGRAMMER=stlinkv21
FLASH_SIZE=65536
BOARD_INC=inc/stm8s208.inc inc/nucleo_8s208.inc
NAME=eeProg
SDAS=sdasstm8
SDCC=sdcc
SDAR=sdar
OBJCPY=objcpy 
CFLAGS=-mstm8 -lstm8
INC=inc/
INCLUDES=$(BOARD_INC) $(INC)ascii.inc $(INC)gen_macros.inc $(INC)app_macros.inc
BUILD=build/
SRC=hardware_init.asm terminal.asm eeProg.asm 
OBJ=$(BUILD)eeProg.rel
FLASH=stm8flash

.PHONY: eeProg

eeProg: clean $(SRC) $(INCLUDES)
	#
	# "*************************************"
	# "compiling $(NAME)  for $(BOARD)      "
	# "*************************************"
	$(SDAS) -g -l -o $(BUILD)$(NAME).rel $(SRC) 
	$(SDCC) $(CFLAGS) -Wl-u -o $(BUILD)$(NAME).ihx $(OBJ) 
	objcopy -Iihex -Obinary  $(BUILD)$(NAME).ihx $(BUILD)$(NAME).bin 
	cp $(BUILD)$(NAME).bin dist/eeProg.bin 
	# 
	@ls -l  $(BUILD)$(NAME).bin 
	# 

.PHONY: clean 
clean:
	#
	# "***************"
	# "cleaning files"
	# "***************"
	rm -f $(BUILD)*


flash:
	$(FLASH) -c $(PROGRAMMER) -p $(BOARD) -s flash -w dist/eeProg.bin 	 


# read flash memory 
read: 
	$(FLASH) -c $(PROGRAMMER) -p $(BOARD) -s flash -b 32768 -r flash.dat 

# erase flash memory from 0x8000-0xffff 
erase:
	dd if=/dev/zero bs=1 count=32768 of=zero.bin
	$(FLASH) -c $(PROGRAMMER) -p$(BOARD) -s flash -b 32768 -w zero.bin 
	rm -f zero.bin 

.PHONY: ee_clear 
# erase eeprom first 16 bytes 
ee_clear: 
	dd if=/dev/zero bs=1 count=16 of=zero.bin
	$(FLASH) -c $(PROGRAMMER) -p$(BOARD) -s eeprom -b 16 -w zero.bin 
	rm -f zero.bin 
 
 
