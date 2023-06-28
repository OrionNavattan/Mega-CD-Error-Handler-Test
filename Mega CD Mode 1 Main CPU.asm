; ---------------------------------------------------------------------------
; Mega CD Mode 1 Main CPU hardware addresses
; ---------------------------------------------------------------------------

expansion:		equ $400000
expansion_end:	equ $800000


cd_bios:			equ expansion 	 ; Mega CD BIOS ROM (executed when booting in mode 2; in Mode 1 we're simply reading its header and decompressing the sub CPU BIOS payload)
cd_bios_signature:	equ cd_bios+$100 ; $400100 ; SEGA signature in BIOS header
cd_bios_name:		equ cd_bios+$120 ; $400120 ; Name of Sub-CPU device in BIOS header (Mega CD, CDX, WonderMega. etc.)
cd_bios_sw_type:	equ cd_bios+$180 ; $400180 ; Software type in BIOS header (should be "BR")
cd_bios_region:		equ cd_bios+$1F0 ; $4001F0 ; CD BIOS region

_CDBIOS_SetVDPRegs:	equ	cd_bios+$2B0 ; $4002B0 ; main CPU bios call to set up VDP registers
_CDBIOS_DMA:		equ	cd_bios+$2D4 ; $4002D4 ; main CPU bios call to DMA to VDP memory

cd_bios_end:	equ $420000


program_ram:		equ	$420000 ; Mega CD program RAM window
program_ram_end:	equ	$440000

word_ram:			equ $600000 ; Mega CD word RAM
word_ram_1M:		equ	word_ram	; MCD Word RAM start (1M/1M)
word_ram_1M_end:	equ	$620000	; MCD Word RAM end (1M/1M)

word_ram_2M:		equ	word_ram	; MCD Word RAM start (2M)
word_ram_2M_end:	equ	$640000	; MCD Word RAM end (2M)

word_ram_IMG:		equ	$620000	; when wordram is used as output space for MCD graphics operations
word_ram_IMG_end:	equ	$640000

sizeof_cd_bios:		equ cd_bios_end-cd_bios
sizeof_program_ram_window:	equ	$20000
sizeof_word_ram_1M:	equ	word_ram_1M_end-word_ram_1M	; MCD Word RAM size (1M/1M)
sizeof_word_ram_2M:	equ	word_ram_2M_end-word_ram_2M	; MCD Word RAM size (2M)
sizeof_word_ram_IMG:	equ	word_ram_IMG_end-word_ram_IMG	; MCD VRAM image of Word RAM size (1M/1M)


mcd_control_registers:	equ $A12000 			; Mega CD gate array
mcd_md_interrupt:	equ	mcd_control_registers		; $A12000 ; MD interrupt, triggers IRQ2 on sub CPU when set to 1

mcd_reset:				equ	$A12001		; $A12001 ; Sub CPU bus request and reset 
	sub_reset_bit:			equ 0		; set to 0 to reset sub CPU, 1 to run
	sub_bus_request_bit:	equ 1		; set to 1 to request sub CPU bus, 0 to return, when read, returns 1 once bus has been granted
	sub_reset:				equ 1<<sub_reset_bit		; reset sub CPU, 1 to run
	sub_bus_request:		equ 1<<sub_bus_request_bit	; set to 1 to request sub CPU bus, 0 to return, when read, returns 1 once bus has been granted
mcd_write_protect:	equ	$A12002 ; write protection; enable write protection for program RAM addresses 0-$FEFF in $100 byte increments
mcd_mem_mode:		equ	$A12003 ; word RAM swap and program RAM bankswitch registers; first two bits have different meanings depending on 1M or 2M mode
	; 1M mode:
	bank_assignment_bit:	equ 0	; RET; read-only; word RAM bank assignment; 0 = bank 0 main CPU and bank 1 sub CPU, 1 - bank 0 sub CPU and bank 1 main CPU
	bank_swap_request_bit:	equ 1	; DMNA; swap word ram banks by setting to 1; returns 1 while swap is in progress and 0 once it is complete
	; 2M mode:	
	wordram_swapmain_bit:	equ 0	; RET; read-only, 0 = swap of word RAM to main CPU is in progress; 1 = swap complete
	wordram_swapsub_bit:	equ 1	; DMNA; give word RAM to sub CPU by setting to 1; returns 0 while swap is in progress and 1 once it is complete

	wordram_mode_bit:		equ 2	; MODE; read only, 0 = 2M mode, 1 = 1M mode
	program_ram_bank_1:		equ 6	; program RAM bank bits, sets program RAM bank to access
	program_ram_bank_2:		equ 7
	program_ram_bank:		equ (1<<program_ram_bank_1)|(1<<program_ram_bank_2) ; $C0
	
mcd_cd_controller_mode:		equ	$A12004 ; CD data controller mode and destination select register
	cd_destination:		equ 7	; bits 0-2, destination of CD data read
	cd_dest_main:		equ	2	; main CPU read from mcd_cdc_data
	cd_dest_sub:		equ 3	; sub CPU read from mcd_cdc_data
	cd_dest_pcm:		equ 4	; DMA to PCM sound source
	cd_dest_prgram:		equ 5	; DMA to program RAM
	cd_dest_wordram:	equ 7	; DMA to word RAM
	
	hibyte_ready_bit:	equ 5	; set when upper byte is sent from CD controller, cleared once full word is ready
	data_ready_bit:		equ 6	; set once full word of data is ready
	data_end_bit:		equ 7	; set once the data read is finished


mcd_user_hblank:	equ	$A12006 ; override default HBlank vector (useless in Mode 1), new address consists of $FF0000 or'ed with contents of this register
mcd_cdc_data:		equ	$A12008 ; CD data output for main CPU read
mcd_stopwatch:		equ	$A1200C ; general purpose 12-bit timer

mcd_com_flags:		equ	$A1200E ; Communication flags
mcd_main_flag:		equ	mcd_com_flags	; $A1200E ; Main CPU communication flag
mcd_sub_flag:		equ	$A1200F ; Sub CPU communication flag

mcd_maincoms:		equ	$A12010 ; Communication to sub CPU
mcd_maincom_0:		equ	mcd_maincoms	; $A12010 ; Communication command 0
mcd_maincom_0_lo:	equ	$A12011 ; Communication command 0
mcd_maincom_1:		equ	$A12012 ; Communication command 1
mcd_maincom_1_lo:	equ	$A12013 ; Communication command 1
mcd_maincom_2:		equ $A12014 ; Communication command 2
mcd_maincom_2_lo:	equ $A12015 ; Communication command 2
mcd_maincom_3:		equ	$A12016 ; Communication command 3
mcd_maincom_3_lo:	equ $A12017 ; Communication command 3
mcd_maincom_4:		equ	$A12018 ; Communication command 4
mcd_maincom_4_lo:	equ	$A12019 ; Communication command 4
mcd_maincom_5:		equ $A1201A ; Communication command 5
mcd_maincom_5_lo:	equ $A1201B ; Communication command 5
mcd_maincom_6:		equ	$A1201C ; Communication command 6
mcd_maincom_6_lo:	equ	$A1201D ; Communication command 6
mcd_maincom_7:		equ	$A1201E ; Communication command 7
mcd_maincom_7_lo:	equ	$A1201F ; Communication command 7

mcd_subcoms:		equ	$A12020 ; Communication from sub CPU
mcd_subcom_0:		equ	mcd_subcoms	;	 $A12020 ; Communication status 0
mcd_subcom_0_lo:	equ	$A12021 ; Communication status 0
mcd_subcom_1:		equ	$A12022 ; Communication status 1
mcd_subcom_1_lo:	equ	$A12023 ; Communication status 1
mcd_subcom_2:		equ	$A12024 ; Communication status 2
mcd_subcom_2_lo:	equ $A12025 ; Communication status 2
mcd_subcom_3:		equ $A12026 ; Communication status 3
mcd_subcom_3_lo:	equ $A12027 ; Communication status 3
mcd_subcom_4:		equ	$A12028 ; Communication status 4
mcd_subcom_4_lo:	equ $A12029 ; Communication status 4
mcd_subcom_5:		equ $A1202A ; Communication status 5
mcd_subcom_5_lo:	equ $A1202B ; Communication status 5
mcd_subcom_6:		equ $A1202C ; Communication status 6
mcd_subcom_6_lo:	equ $A1202D ; Communication status 6
mcd_subcom_7:		equ $A1202E ; Communication status 7
mcd_subcom_7_lo:	equ $A1202F ; Communication status 7