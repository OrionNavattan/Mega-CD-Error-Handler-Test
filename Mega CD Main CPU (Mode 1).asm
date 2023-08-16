; ---------------------------------------------------------------------------
; Standard Mega Drive 68K hardware addresses, constants & function macros
; ---------------------------------------------------------------------------

; VDP addressses
vdp_data_port:		equ $C00000
vdp_control_port:	equ $C00004

	; Status register bits
	video_mode_bit:			equ 0			; PAL, 0 if NTSC, 1 if PAL
	dma_status_bit:			equ 1			; DMA, 1 if a DMA is in progress
	hblank_bit:				equ 2		; BB, 1 if HBlank is in progress
	vblank_bit:				equ 3		; VB, 1 if VBlank is in progress
	interlaced_counter_bit:	equ 4				; OD, 0 if even frame displayed in interlaced mode, 1 if odd
	hardware_collision_bit:	equ 5				; SC, 1 if any two sprites have non-transparent pixels overlapping. this is a leftover from the TMS9918, which could only display four sprites per scanline
	sprite_limit_bit:		equ 6			; SO, 1 if sprite limit (16 in H32, 20 in H40) has been reached on current scanline
	vertical_interrupt_bit: equ 7				; VI, 1 if a vertical interrupt has just occurred
	fifo_full_bit:			equ 8			; F, 1 if VDP FIFO is full
	fifo_empty_bit:			equ 9			; E, 1 if VDP FIFO is empty

	; VDP register settings
	vdp_mode_register1:	equ $8000
	vdp_left_border:	equ vdp_mode_register1+$20	; blank leftmost 8px to bg colour
	vdp_enable_hint:	equ vdp_mode_register1+$10	; enable horizontal interrupts
	vdp_md_color:		equ vdp_mode_register1+4	; Mega Drive colour mode
	vdp_freeze_hvcounter:	equ vdp_mode_register1+2	; freeze H/V counter on interrupts
	vdp_disable_display:	equ vdp_mode_register1+1
	
	vdp_mode_register2:	equ $8100
	vdp_128kb_vram:		equ vdp_mode_register2+$80	; use 128kB of VRAM, Teradrive only
	vdp_enable_display:	equ vdp_mode_register2+$40	; if not set, display is filled with background color
	vdp_enable_vint:	equ vdp_mode_register2+$20	; enable vertical interrupts
	vdp_enable_dma:		equ vdp_mode_register2+$10	; enable DMA operations
	vdp_pal_display:	equ vdp_mode_register2+8	; 240px screen height (PAL)
	vdp_ntsc_display:	equ vdp_mode_register2		; 224px screen height (NTSC)
	vdp_md_display:		equ vdp_mode_register2+4	; mode 5 Mega Drive display
	
	vdp_fg_nametable:	equ $8200			; fg (plane A) nametable setting
	vdp_window_nametable:	equ $8300			; window nametable setting
	vdp_bg_nametable:	equ $8400			; bg (plane B) nametable setting
	vdp_sprite_table:	equ $8500			; sprite table setting
	vdp_sprite_table2:	equ $8600			; sprite table setting for 128kB VRAM
	vdp_bg_color:		equ $8700			; bg colour id (+0..$3F)
	vdp_sms_hscroll:	equ $8800
	vdp_sms_vscroll:	equ $8900
	vdp_hint_counter:	equ $8A00			; number of lines between horizontal interrupts
	
	vdp_mode_register3:	equ $8B00
	vdp_enable_xint:	equ vdp_mode_register3+8	; enable external interrupts
	vdp_16px_vscroll:	equ vdp_mode_register3+4	; 16px column vertical scroll mode
	vdp_full_vscroll:	equ vdp_mode_register3		; full screen vertical scroll mode
	vdp_1px_hscroll:	equ vdp_mode_register3+3	; 1px row horizontal scroll mode
	vdp_8px_hscroll:	equ vdp_mode_register3+2	; 8px row horizontal scroll mode
	vdp_full_hscroll:	equ vdp_mode_register3		; full screen horizontal scroll mode
	
	vdp_mode_register4:	equ $8C00
	vdp_320px_screen_width:	equ vdp_mode_register4+$81	; 320px wide screen mode
	vdp_256px_screen_width:	equ vdp_mode_register4		; 256px wide screen mode
	vdp_shadow_highlight:	equ vdp_mode_register4+8	; enable shadow/highlight mode
	vdp_interlace:		equ vdp_mode_register4+2	; enable interlace mode
	vdp_interlace_x2:	equ vdp_mode_register4+6	; enable double height interlace mode (e.g. Sonic 2 two player game)
	
	vdp_hscroll_table:	equ $8D00			; horizontal scroll table setting
	vdp_nametable_hi:	equ $8E00			; high bits of fg/bg nametable settings for 128kB VRAM
	vdp_auto_inc:		equ $8F00			; value added to VDP address after each write
	
	vdp_plane_size:		equ $9000			; fg/bg plane dimensions
	vdp_plane_height_128:	equ vdp_plane_size+$30		; height = 128 cells (1024px)
	vdp_plane_height_64:	equ vdp_plane_size+$10		; height = 64 cells (512px)
	vdp_plane_height_32:	equ vdp_plane_size		; height = 32 cells (256px)
	vdp_plane_width_128:	equ vdp_plane_size+3		; width = 128 cells (1024px)
	vdp_plane_width_64:	equ vdp_plane_size+1		; width = 64 cells (512px)
	vdp_plane_width_32:	equ vdp_plane_size		; width = 32 cells (256px)
	
	vdp_window_x_pos:	equ $9100
	vdp_window_right:	equ vdp_window_x_pos+$80	; draw window from x pos to right edge of screen
	vdp_window_left:	equ vdp_window_x_pos		; draw window from x pos to left edge of screen
	
	vdp_window_y_pos:	equ $9200
	vdp_window_bottom:	equ vdp_window_y_pos+$80	; draw window from y pos to bottom edge of screen
	vdp_window_top:		equ vdp_window_y_pos		; draw window from y pos to top edge of screen
	
	vdp_dma_length_low:	equ $9300
	vdp_dma_length_hi:	equ $9400
	vdp_dma_source_low:	equ $9500
	vdp_dma_source_mid:	equ $9600
	vdp_dma_source_hi:	equ $9700
	vdp_dma_68k_copy:	equ vdp_dma_source_hi		; DMA 68k to VRAM copy mode
	vdp_dma_vram_fill:	equ vdp_dma_source_hi+$80	; DMA VRAM fill mode
	vdp_dma_vram_copy:	equ vdp_dma_source_hi+$C0	; DMA VRAM to VRAM copy mode
		
vdp_counter:		equ $C00008
psg_input:			equ $C00011
debug_reg:			equ $C0001C

; Z80 address space
z80_ram:		equ $A00000				; start of Z80 RAM
z80_ram_end:	equ $A02000					; end of non-reserved Z80 RAM
ym2612_a0:		equ $A04000
ym2612_d0:		equ $A04001
ym2612_a1:		equ $A04002
ym2612_d1:		equ $A04003

; I/O addresses
console_version:	equ $A10001
	console_region_bit:	equ 7				; 0 = Japan/Korea; 1 = overseas
	console_speed_bit:	equ 6				; 0 = NTSC; 1 = PAL
	console_mcd_bit:	equ 5				; 0 = Mega-CD attached (or other addon?), 1 = nothing attached
	console_region:		equ 1<<console_region_bit
	console_speed:		equ 1<<console_speed_bit
	console_mcd:		equ 1<<console_mcd_bit
	console_revision:	equ $F				; bitmask for revision id in bits 0-3; revision 0 has no TMSS
port_1_data_hi:		equ $A10002
port_1_data:		equ $A10003
port_2_data_hi:		equ $A10004
port_2_data:		equ $A10005
port_e_data_hi:		equ $A10006
port_e_data:		equ $A10007
port_1_control_hi:	equ $A10008
port_1_control:		equ $A10009
port_2_control_hi:	equ $A1000A
port_2_control:		equ $A1000B
port_e_control_hi:	equ $A1000C
port_e_control:		equ $A1000D

z80_bus_request:	equ $A11100
z80_reset:		equ $A11200

; Bank registers, et al
sram_access_reg:	equ $A130F1
bank_reg_1:		equ $A130F3				; Bank register for address $80000-$FFFFF
bank_reg_2:		equ $A130F5				; Bank register for address $100000-$17FFFF
bank_reg_3:		equ $A130F7				; Bank register for address $180000-$1FFFFF
bank_reg_4:		equ $A130F9				; Bank register for address $200000-$27FFFF
bank_reg_5:		equ $A130FB				; Bank register for address $280000-$2FFFFF
bank_reg_6:		equ $A130FD				; Bank register for address $300000-$37FFFF
bank_reg_7:		equ $A130FF				; Bank register for address $380000-$3FFFFF
tmss_sega:		equ $A14000				; write the string "SEGA" to unlock the VDP
tmss_reg:		equ $A14101				; bankswitch between cartridge and TMSS ROM

; Memory sizes
sizeof_ram:			equ $10000
sizeof_vram:		equ $10000
sizeof_vsram:		equ $50
sizeof_z80_ram:		equ z80_ram_end-z80_ram			; $2000
sizeof_z80_bank:	equ $8000				; size of switchable Z80 rom window


; Mega CD Expansion slot 
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
	sub_bios_end:		equ $5400	; $425400, end of sub CPU BIOS' executable code
	sp_start:			equ $6000	; $426000, start of user program in first program RAM bank
program_ram_end:	equ	$440000

wordram:			equ $600000 ; Mega CD word RAM
wordram_1M:		equ	wordram	; MCD Word RAM start (1M/1M)
wordram_1M_end:	equ	$620000	; MCD Word RAM end (1M/1M)

wordram_2M:		equ	wordram	; MCD Word RAM start (2M)
wordram_2M_end:	equ	$640000	; MCD Word RAM end (2M)

wordram_IMG:		equ	$620000	; when wordram is used as output space for MCD graphics operations
wordram_IMG_end:	equ	$640000

sizeof_cd_bios:		equ cd_bios_end-cd_bios
sizeof_program_ram_window:	equ	$20000
sizeof_wordram_1M:	equ	wordram_1M_end-wordram_1M	; MCD Word RAM size (1M/1M)
sizeof_wordram_2M:	equ	wordram_2M_end-wordram_2M	; MCD Word RAM size (2M)
sizeof_wordram_IMG:	equ	wordram_IMG_end-wordram_IMG	; MCD VRAM image of Word RAM size (1M/1M)

; Mega CD control registers; aka, the gate array
mcd_control_registers:	equ $A12000 			; Mega CD gate array
mcd_md_interrupt:	equ	mcd_control_registers		; $A12000 ; MD interrupt, triggers IRQ2 on sub CPU when set to 1
	mcd_int_bit:	equ 0
mcd_reset:				equ	$A12001		; $A12001 ; Sub CPU bus request and reset 
	sub_reset_bit:			equ 0		; set to 0 to reset sub CPU, 1 to run
	sub_bus_request_bit:	equ 1		; set to 1 to request sub CPU bus, 0 to return, when read, returns 1 once bus has been granted
	sub_reset:				equ 0
	sub_run:				equ 1<<sub_reset_bit
	sub_bus_request:		equ 1<<sub_bus_request_bit
mcd_write_protect:	equ	$A12002 ; write protection; enable write protection for program RAM addresses 0-$FEFF in $100 byte increments
mcd_mem_mode:		equ	$A12003 ; word RAM swap and program RAM bankswitch registers; first two bits have different meanings depending on 1M or 2M mode
	; 1M mode:
	bank_assignment_bit:	equ 0	; RET; read-only; word RAM bank assignment; 0 = bank 0 main CPU and bank 1 sub CPU, 1 - bank 0 sub CPU and bank 1 main CPU
	bank_swap_request_bit:	equ 1	; DMNA; swap word ram banks by setting to 1; returns 1 while swap is in progress and 0 once it is complete
	; 2M mode:	
	wordram_swapmain_bit:	equ 0	; RET; read-only, 0 = word RAM is assigned to sub CPU; 1 = word RAM is assigned to main CPU
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

; ---------------------------------------------------------------------------
; Stop the Z80
; ---------------------------------------------------------------------------

stopZ80:	macros
		move.w	#$100,(z80_bus_request).l

; ---------------------------------------------------------------------------
; Wait for Z80 to stop
; ---------------------------------------------------------------------------

waitZ80:	macro
	.wait\@:
		btst	#0,(z80_bus_request).l
		bne.s	.wait\@
		endm

; ---------------------------------------------------------------------------
; Start the Z80
; ---------------------------------------------------------------------------

startZ80:	macros
		move.w	#0,(z80_bus_request).l

; ---------------------------------------------------------------------------
; Reset the Z80
; ---------------------------------------------------------------------------

resetZ80_assert: macros
		move.w	#0,(z80_reset).l

resetZ80_release: macros
		move.w	#$100,(z80_reset).l

; ---------------------------------------------------------------------------
; Disable interrupts
; ---------------------------------------------------------------------------

disable_ints:	macros
		move	#$2700,sr

; ---------------------------------------------------------------------------
; Enable interrupts
; ---------------------------------------------------------------------------

enable_ints:	macros
		move	#$2300,sr

; ---------------------------------------------------------------------------
; Bankswitch between SRAM and ROM
; (remember to enable SRAM in the header first!)
; ---------------------------------------------------------------------------

gotoSRAM:	macros
		move.b  #1,(sram_access_reg).l

gotoROM:	macros
		move.b  #0,(sram_access_reg).l

; ---------------------------------------------------------------------------
; Wait for word RAM access
; ---------------------------------------------------------------------------

waitwordram:	macro
	.waitwordram\@:
		cmpi.b	#$FF,(mcd_sub_flag).l	; is sub CPU OK?
		bne.s	.ok\@					; branch if so
		trap #0
		
	.ok@\:	
		btst	#wordram_swapmain_bit,(mcd_mem_mode).l	; has sub CPU given us the word RAM?
		beq.s	.waitwordram		; if not, wait
		endm
		
; ---------------------------------------------------------------------------
; Give word RAM to the sub CPU
; ---------------------------------------------------------------------------

givewordram:	macro
	.givewordram\@:
		cmpi.b	#$FF,(mcd_sub_flag).l	; is sub CPU OK?
		bne.s	.ok\@					; branch if so
		trap #0
		
	.ok@\:
		bset	#wordram_swapsub_bit,GAMEMMODE			; give word ram to the sub CPU
		beq.s	GiveWordRAMAccess				; wait until it is given
		endm