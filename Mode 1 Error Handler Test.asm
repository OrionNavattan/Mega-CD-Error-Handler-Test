
		opt l.					; . is the local label symbol
		opt ae-					; automatic evens are disabled by default
		opt ws+					; allow statements to contain white-spaces
		opt w+					; print warnings
		opt m+					; do not expand macros - if enabled, this can break assembling
		
		include "Debugger.asm"
		include "Mega Drive.asm"
		include "Mega CD Mode 1 Main CPU.asm"
		
workram:		equ $FF0000	
sizeof_workram:	equ $10000	
countof_color:		equ 16					; colors per palette line
countof_pal:		equ 4					; total palette lines
sizeof_pal:		equ countof_color*2			; total bytes in 1 palette line (32 bytes)
sizeof_pal_all:		equ sizeof_pal*countof_pal		; bytes in all palette lines (128 bytes)		
vram_window:		equ $A000				; window nametable - unused
vram_fg:			equ $C000			; foreground nametable ($1000 bytes); extends until $CFFF
vram_bg:			equ $E000			; background nametable ($1000 bytes); extends until $EFFF
vram_sprites:			equ $F800			; sprite attribute table ($280 bytes)			
vram_hscroll:			equ $FC00			; horizontal scroll table ($380 bytes); extends until $FF7F	
	
; ---------------------------------------------------------------------------
; Test if macro argument is used
; ---------------------------------------------------------------------------

ifarg		macros
		if strlen("\1")>0

ifnotarg	macros
		if strlen("\1")=0

; ---------------------------------------------------------------------------
; Save and restore registers from the stack.
; ---------------------------------------------------------------------------

chkifreg:	macro
		isreg: = 1					; assume string is register
		isregm: = 0					; assume single register
		regtmp: equs \1					; copy input
		rept strlen(\1)
		regchr:	substr ,1,"\regtmp"			; get first character
		regtmp:	substr 2,,"\regtmp"			; remove first character
		if instr("ad01234567/-","\regchr")
		else
		isreg: = 0					; string isn't register if it contains characters besides those listed
		endc
		if instr("/-","\regchr")
		isregm: = 1					; string is multi-register
		endc
		endr
		endm

pushr:		macro
		chkifreg "\1"
		if (isreg=1)&(isregm=1)
			ifarg \0				; check if size is specified
			movem.\0	\1,-(sp)		; save multiple registers (b/w)
			else
			movem.l	\1,-(sp)			; save multiple registers
			endc
		else
			ifarg \0				; check if size is specified
			move.\0	\1,-(sp)			; save one register (b/w)
			else
			move.l	\1,-(sp)			; save one whole register
			endc
		endc
		endm

popr:		macro
		chkifreg "\1"
		if (isreg=1)&(isregm=1)
			ifarg \0				; check if size is specified
			movem.\0	(sp)+,\1		; restore multiple registers (b/w)
			else
			movem.l	(sp)+,\1			; restore multiple whole registers
			endc
		else
			ifarg \0				; check if size is specified
			move.\0	(sp)+,\1			; restore one register (b/w)
			else
			move.l	(sp)+,\1			; restore one whole register
			endc
		endc
		endm	
			
ROM_Start:
Vectors:	
		dc.l 0			; Initial stack pointer value
		dc.l EntryPoint					; Start of program
		dc.l BusError					; Bus error
		dc.l AddressError				; Address error
		dc.l IllegalInstr				; Illegal instruction
		dc.l ZeroDivide					; Division by zero
		dc.l ChkInstr					; CHK exception
		dc.l TrapvInstr					; TRAPV exception
		dc.l PrivilegeViol				; Privilege violation
		dc.l Trace					; TRACE exception
		dc.l Line1010Emu				; Line-A emulator
		dc.l Line1111Emu				; Line-F emulator
		dcb.l 2,ErrorExcept				; Unused (reserved)
		dc.l ErrorExcept				; Format error
		dc.l ErrorExcept				; Uninitialized interrupt
		dcb.l 8,ErrorExcept				; Unused (reserved)
		dc.l ErrorExcept				; Spurious exception
		dc.l ErrorExcept					; IRQ level 1
		dc.l ErrorExcept					; IRQ level 2
		dc.l ErrorExcept					; IRQ level 3
		dc.l ErrorExcept					; IRQ level 4 (horizontal retrace interrupt)
		dc.l ErrorExcept					; IRQ level 5
		dc.l ErrorExcept					; IRQ level 6 (vertical retrace interrupt)
		dc.l ErrorExcept					; IRQ level 7
		dcb.l 16,ErrorExcept				; TRAP #00..#15 exceptions
		dcb.l 16,ErrorExcept				; Unused (reserved)
Header:
		dc.b "SEGA GENESIS"		; Hardware system ID (Console name)
		dc.b "ORIONNAVATTAN 2023.JUNE"				; Copyright holder and release date (generally year)
		dc.b "ORION'S MODE 1 ERROR HANDLER TEST                      " ; Domestic name
		dc.b "ORION'S MODE 1 ERROR HANDLER TEST                      " ; International name


		dc.b "FFFFFFFFFFFFF"				; Serial/version number (Rev non-0)

Checksum: 	dc.w $0
		dc.b "JC              "				; I/O support
ROM_Start_Ptr:	dc.l ROM_Start					; Start address of ROM
ROM_End_Ptr:	dc.l ROM_End-1					; End address of ROM
		dc.l $FF0000					; Start address of RAM
		dc.l $FFFFFF					; End address of RAM

		dc.l $20202020					; dummy values (SRAM disabled)
		dc.l $20202020					; SRAM start
		dc.l $20202020					; SRAM end

		dc.b "                                                    " ; Notes (unused, anything can be put in this space, but it has to be 52 bytes.)
		dc.b "JUE             "				; Region (Country code)
EndOfHeader:
		
EntryPoint:
		lea	SetupValues(pc),a0			; load setup array
		move.w	(a0)+,sr				; disable interrupts during setup; they will be reenabled by the Sega Screen
		movem.l (a0)+,a1-a3/a5/a6			; Z80 RAM start, work RAM start, Z80 bus request register, VDP data port, VDP control port
		movem.w (a0)+,d1/d2				; VDP register increment/value for Z80 stop and reset release ($100),  first VDP register value ($8004)
		moveq	#SetupVDP_end-SetupVDP-1,d5		; VDP registers loop counter
		moveq	#0,d4					; DMA fill/memory clear/Z80 stop bit test value
		movea.l d4,a4					; clear a4
		move.l	a4,usp					; clear user stack pointer

		move.b	console_version-z80_bus_request(a3),d6	; load hardware version
		move.b	d6,d3					; copy to d3 for checking revision (d6 will be used later to set region and speed)
		andi.b	#console_revision,d3			; get only hardware version ID
		beq.s	.wait_dma				; if Model 1 VA4 or earlier (ID = 0), branch
		move.l	Header(pc),tmss_sega-z80_bus_request(a3)	; satisfy the TMSS
   
   .wait_dma:
		move.w	(a6),ccr				; copy status register to CCR, clearing the VDP write latch and setting the overflow flag if a DMA is in progress
		bvs.s	.wait_dma				; if a DMA was in progress during a soft reset, wait until it is finished
   
   .loop_vdp:
		move.w	d2,(a6)					; set VDP register
		add.w	d1,d2					; advance register ID
		move.b	(a0)+,d2				; load next register value
		dbf	d5,.loop_vdp				; repeat for all registers; final value loaded will be used later to initialize I/0 ports
   
		move.l	(a0)+,(a6)				; set DMA fill destination
		move.w	d4,(a5)					; set DMA fill value (0000), clearing the VRAM

		move.w	(a0)+,d5	
   .loop_ram:
		move.l	d4,(a2)+				; a2 = start of 68K RAM
		dbf	d5,.loop_ram				; clear all RAM

		move.w	d1,(a3)					; stop the Z80 (we will clear the VSRAM and CRAM while waiting for it to stop)
		move.w	d1,z80_reset-z80_bus_request(a3)	; deassert Z80 reset (ZRES is held high on console reset until we clear it)

		move.w	(a0)+,(a6)				; set VDP increment to 2

		move.l	(a0)+,(a6)				; set VDP to VSRAM write
		moveq	#(sizeof_vsram/4)-1,d5			; set repeat times
   .loop_vsram:
		move.l	d4,(a5)					; clear 4 bytes of VSRAM
		dbf	d5,.loop_vsram				; repeat until entire VSRAM has been cleared

		move.l	(a0)+,(a6)				; set VDP to CRAM write
		moveq	#(sizeof_pal_all/4)-1,d5		; set repeat times
   .loop_cram:
		move.l	d4,(a5)					; clear two palette entries
		dbf	d5,.loop_cram				; repeat until entire CRAM has been cleared

   .waitz80:
		btst	d4,(a3)					; has the Z80 stopped?
		bne.s	.waitz80				; if not, branch

		move.w #sizeof_z80_ram-1,d5			; size of Z80 ram
   .clear_Z80_ram:
		move.b 	d4,(a1)+				; clear the Z80 RAM
		dbf	d5,.clear_Z80_ram
		
		moveq	#4-1,d5					; set number of PSG channels to mute
   .psg_loop:
		move.b	(a0)+,psg_input-vdp_data_port(a6)	; set the PSG channel volume to null (no sound)
		dbf	d5,.psg_loop				; repeat for all channels

		; if soft reset, skip finding the BIOS, instead test mcd_bios_id, 0 = no BIOS found

		btst	#console_mcd_bit,d6	; is there anything in the expansion slot?
		bne.s	.notfound			; branch if not
		;andi.b	 #console_region+console_speed,d6	; get region and speed settings
		;move.b	 d6,(v_console_region).w		; set in RAM	
		
		cmpi.l	#"SEGA",cd_bios_signature	; is the "SEGA" signature present?
		bne.s	.notfound					; if not, branch
		cmpi.w	#"BR",cd_bios_sw_type		; is the "Boot ROM" software type present?
		bne.s	.notfound					; if not, branch

		; Determine which MEGA CD device is attached.
		movea.l	a0,a1				; a1 & a2 = pointer index to BIOS data
		movea.l a0,a2
		moveq	#(sizeof_MCDBIOSList/2)-1,d0
		moveq	#0,d6

	.findloop:
		adda.w	(a2)+,a1			; a1 = pointer to BIOS data

		;movea.l	(a1)+,a0			; get address of compressed BIOS payload
		addq	#4,a1					; skip over BIOS payload address
		lea	(cd_bios_name).l,a3			; get BIOS name

	.checkname:
		move.b	(a1)+,d1			; get character
		beq.s	.namematch			; branch if we've reached the end of the name
		cmp.b	(a3)+,d1			; does the BIOS name match so far?
		bne.s	.nextBIOS			; if not, go check the next BIOS
		bra.s	.checkname			; loop until name is fully checked

	.namematch:
		move.b	(a1)+,d1			; is this Sub CPU BIOS address region specific?
		beq.s	.mcd_init				; branch if not
		cmp.b	(cd_bios_region).l,d1			; does the BIOS region match?
		beq.s	.mcd_init				; branch if so

	.nextBIOS:
		addq.w	#1,d6				; increment BIOS ID 
		dbf	d0,.findloop			; loop until all BIOSes are checked

	.notfound:
		moveq	#0,d0				; error code for BIOS not found
		bra.w	InitFailure

	.mcd_init:
		move.b	d6,(mcd_bios_id).w				; save BIOS ID
		move.w	#$FF00,(ga_write_protect).l		; reset the sub CPU's gate array
		move.b	#3,(ga_reset).l					; these four instructions in this specific sequence trigger the reset
		move.b	#2,(ga_reset).l					; this will also give the wordram to the main CPU
		move.b	#0,(ga_reset).l

		moveq	#$80-1,d2			; wait for gate array reset to complete
		dbf	d2,*

		lea	(mcd_reset).l,a5
	
		move.w	#$100-1,d2			; max time to wait for MCD response
	.reset_sub:
		bclr	#sub_reset_bit,(a0)			; reset the sub CPU
		dbeq	d2,.reset_sub			; wait for reset to complete
		bne.s	.no_response			; branch if no response

		move.w	#$100-1,d2			; max time to wait for MCD response
	.req_bus:
		bset	#sub_bus_request_bit,(a0)	; request sub CPU bus
		dbne	d2,.req_bus			; wait for bus request to complete
		bne.s	.Clear_wordram		; branch if successful

	.no_response:
		moveq	#1,d0	; error code for timeout
		bra.w	InitFailure
		
.Clear_wordram:
		; Clear the word RAM.
		lea (wordram_2M).l,a0
		move.w (sizeof_word_ram_2M/4)-1,d5
		;moveq #0,d7

	.clearloop1:
		move.l d4,(a0)+		; clear 4 bytes of wordram
		dbf d5,.clearloop1	; repeat for entire wordram
		
;ClearPrgRAM:
		; Clear the program RAM.
		lea (mcd_mem_mode),a6
		move.b	#0,mcd_write_protect-mcd_mem_mode(a1)			; disable write protect on Sub CPU memory
		move.b	(a6),d6			; get current bank setting
		andi.b	#(~program_ram_bank)&$FF,d3		; set program ram bank to 0
		move.b	d6,(a6)
		bsr.s	.clearbank
		
		move.b	(a6),d6			; get current bank setting
		addi.b	#$40,d6
		move.b	d6,(a6)		; advance to second bank
		bsr.s	.clearbank
		
		move.b	(a6),d6			; get current bank setting
		addi.b	#$40,d6
		move.b	d6,(a6)		; advance to third bank
		bsr.s	.clearbank
		
		move.b	(a6),d6			; get current bank setting
		addi.b	#$40,d6
		move.b	d6,(a6)		; advance to fourth and final bank
		pea	.clearcoms(pc)	; 'return' to next step of init
		
	.clearbank:
		lea (program_ram).l,a0
		move.w  #(sizeof_program_ram_window/4)-1,d5

	.clearloop2:
		move.l  d4,(a0)+	; clear 4 bytes of the program ram bank
		dbf d5,.clearloop2	; repeat for whole bank
		rts
		
	.clearcoms:	
		; Clear the main CPU communication registers.
		lea (mcd_maincoms).l,a0
		move.b	d4,mcd_com_flags-mcd_maincoms(a0)	; clear main CPU communication bits
		move.l	d4,(a0)+		; clear main CPU communication registers
		move.l	d4,(a0)+
		move.l	d4,(a0)+
		move.l	d4,(a0)
		
;LoadSubCPUBIOS:
		; Decompress the sub CPU BIOS.
		move.b	(a6),d6			; get current bank setting
		andi.b	#(~program_ram_bank)&$FF,d3		; set program ram bank to 0
		move.b	d6,(a6)
		lea	(program_ram).l,a1		; start of program RAM
		move.b	(mcd_bios_id).w,d7	; get BIOS ID
		add.w	d7,d7				; make index
		move.w	MCDBIOSList-2(pc,d7.w),d1	; IDs start at 1
		movea.l	MCDBIOSList(pc,d1.w),a0	; a0 = start of compressed BIOS payload
		bsr.w	KosDec					; decompress the sub CPU BIOS

;LoadSubCPUProgram:	
		; Decompress the sub CPU program.
		lea	(SubCPU_Program).l,a0			; a0 = compressed sub CPU program
		;lea (program_ram+$6000),a1		; a1 = start of user Sub CPU program in first program RAM bank (unnecessary if this is run immediately after decompressing the BIOS)
		move.w	(a0)+,d0				; d0 = total number of modules

		moveq	#(sizeof_program_ram_window-sizeof_sub_bios)/sizeof_module,d5	
		cmp.b	d5,d0					; will we need to bankswitch at all?
		bls.s	.nobankswitch			; branch if not
		sub.b	d5,d0					; deincrement module counter
		moveq	#(sizeof_program_ram_window-sizeof_sub_bios)/sizeof_module-1,d7

		pushr.w	d0	
		bsr.s	.decompress				; decompress the first 13 modules to first bank
		popr.w	d0
		
		move.b	(a6),d6		; get current bank setting
		addi.b	#$40,d6
		move.b	d6,(a6)		; advance to next bank
	
		moveq	#sizeof_program_ram_window/sizeof_module,d5	
		cmp.b	d5,d0					; will we need to bankswitch to third bank?
		bls.s	.nobankswitch			; branch if not
		sub.b	d5,d0					; deincrement module counter
		moveq	#(sizeof_program_ram_window/sizeof_module)-1,d7
		
		pushr.w	d0	
		bsr.s	.decompress				; decompress the next 16 modules to second bank
		popr.w	d0
		
		move.b	(a6),d6		; get current bank setting
		addi.b	#$40,d6
		move.b	d6,(a6)		; advance to next bank
		
		moveq	#sizeof_program_ram_window/sizeof_module,d5	
		cmp.b	d5,d0					; will we need to bankswitch to fourth bank?
		bls.s	.nobankswitch			; branch if not
		sub.b	d5,d0					; deincrement module counter
		moveq	#(sizeof_program_ram_window/sizeof_module)-1,d7
		
		pushr.w	d0	
		bsr.s	.decompress				; decompress the next 16 modules to third bank
		popr.w	d0
				
		move.b	(a6),d6		; get current bank setting
		addi.b	#$40,d6
		move.b	d6,(a6)		; advance to final bank
		
	.nobankswitch:
		subq.b	#1,d0					
		move.w	d0,d7					; decompress all remaining modules
		pea	.decompdone(pc)

	.decompress:
		bsr.w	KosDec			; decompress the module
		dbf	d7,.decompress		; repeat until end of bank or end of data
		rts
		
		
	.decompdone:	
		move.b	#($5400>>9),(ga_write_protect).l			; enable write protect on program RAM

		lea	(mcd_reset).l,a0
		move.w	#$100-1,d2			; max time to wait for MCD response
	.reset_release:
		bset	#sub_reset_bit,(a0)			; release sub CPU reset
		dbne	d2,.reset_release			; wait for completion
		bne.w	.no_response			; branch if we timed out

		move.w	#$100-1,d2			; max time to wait for MCD response
	.release_bus:
		bclr	#sub_bus_request_bit,(a0)	; release sub CPU bus
		dbeq	d2,.release_bus			; wait for completion
		beq.w	.no_response		; branch if we timed out
		
		
		move.w	#id_VBlank_Init,(v_vblank_routine).w
		enable_ints
		
		
SetupValues:
		dc.w	$2700					; disable interrupts
		dc.l	z80_ram
		dc.l	workram				; ram_start
		dc.l	z80_bus_request
		dc.l	vdp_data_port
		dc.l	vdp_control_port

		dc.w	vdp_mode_register2-vdp_mode_register1	; VDP Reg increment value & opposite initialisation flag for Z80
		dc.w	vdp_md_color				; $8004; normal color mode, horizontal interrupts disabled
	SetupVDP:
		dc.b	(vdp_enable_vint|vdp_enable_dma|vdp_ntsc_display|vdp_md_display)&$FF ;  $8134; mode 5, NTSC, vertical interrupts and DMA enabled 
		dc.b	(vdp_fg_nametable+(vram_fg>>10))&$FF	; $8230; foreground nametable starts at $C000
		dc.b	(vdp_window_nametable+(vram_window>>10))&$FF ; $8328; window nametable starts at $A000
		dc.b	(vdp_bg_nametable+(vram_bg>>13))&$FF	; $8407; background nametable starts at $E000
		dc.b	(vdp_sprite_table+(vram_sprites>>9))&$FF ; $857C; sprite attribute table starts at $F800
		dc.b	vdp_sprite_table2&$FF			; $8600; unused (high bit of sprite attribute table address for 128KB VRAM)
		dc.b	(vdp_bg_color+0)&$FF			; $8700; background color (palette line 0 color 0)
		dc.b	vdp_sms_hscroll&$FF			; $8800; unused (mode 4 hscroll register)
		dc.b	vdp_sms_vscroll&$FF			; $8900; unused (mode 4 vscroll register)
		dc.b	(vdp_hint_counter+0)&$FF		; $8A00; horizontal interrupt register (set to 0 for now)
		dc.b	(vdp_full_vscroll|vdp_full_hscroll)&$FF	; $8B00; full-screen vertical/horizontal scrolling
		dc.b	vdp_320px_screen_width&$FF		; $8C81; H40 display mode
		dc.b	(vdp_hscroll_table+(vram_hscroll>>10))&$FF ; $8D3F; hscroll table starts at $FC00
		dc.b	vdp_nametable_hi&$FF			; $8E00: unused (high bits of fg and bg nametable addresses for 128KB VRAM)
		dc.b	(vdp_auto_inc+1)&$FF			; $8F01; VDP increment size (will be changed to 2 later)
		dc.b	(vdp_plane_width_64|vdp_plane_height_32)&$FF ; $9001; 64x32 plane size
		dc.b	vdp_window_x_pos&$FF			; $9100; unused (window horizontal position)
		dc.b	vdp_window_y_pos&$FF			; $9200; unused (window vertical position)

		dc.w	sizeof_vram-1				; $93FF/$94FF - DMA length
		dc.w	0					; VDP $9500/9600 - DMA source
		dc.b	vdp_dma_vram_fill&$FF			; VDP $9780 - DMA fill VRAM

		dc.b	$40					; I/O port initialization value
   
	SetupVDP_end:

		dc.l	$40000080				; DMA fill VRAM
		dc.w	(sizeof_workram/4)-1
		dc.w	vdp_auto_inc+2				; VDP increment
		dc.l	$40000010				; VSRAM write mode
		dc.l	$C0000000				; CRAM write mode
   

		dc.b	$9F,$BF,$DF,$FF				; PSG mute values (PSG 1 to 4) 

MCDBIOSList:	index offset(*),1
		ptr	MCDBIOS_JP1			; 1
		ptr	MCDBIOS_US1			; 2
		ptr	MCDBIOS_EU1			; 3
		ptr	MCDBIOS_CD2			; 4
		ptr	MCDBIOS_CDX			; 5
		ptr	MCDBIOS_LaserActive	; 6
		ptr	MCDBIOS_Wondermega1	; 7
		ptr	MCDBIOS_Wondermega2	; 8
		arraysize MCDBIOSList

MCDBIOS_JP1:
		dc.l	$416000
		dc.b	"MEGA-CD BOOT ROM",0		; Japanese MCD Model 1
		dc.b	"J"
		even

MCDBIOS_US1:
		dc.l	$415800
		dc.b	"SEGA-CD BOOT ROM",0		; North American SCD Model 1
		dc.b	0
		even

MCDBIOS_EU1:
		dc.l	$415800
		dc.b	"MEGA-CD BOOT ROM",0		; PAL MCD Model 1
		dc.b	"E"
		even

MCDBIOS_CD2:
		dc.l	$416000
		dc.b	"CD2 BOOT ROM    ",0		; All MCD/SCD Model 2s and Aiwa Mega-CD
		dc.b	0
		even

MCDBIOS_CDX:
		dc.l	$416000
		dc.b	"CDX BOOT ROM    ",0		; Sega MultiMega and CDX
		dc.b	0
		even

MCDBIOS_LaserActive:
		dc.l	$41AD00
		dc.b	"MEGA-LD BOOT ROM",0		; Pioneer LaserActive MEGA-LD Pak
		dc.b	0
		even

MCDBIOS_Wondermega1:
		dc.l	$416000
		dc.b	"WONDER-MEGA BOOTROM",0	; Victor WonderMega 1 and Sega WonderMega
		dc.b	0
		even

MCDBIOS_Wondermega2:
		dc.l	$416000
		dc.b	"WONDERMEGA2 BOOTROM",0	; Victor WonderMega 2 and JVC X'Eye
		dc.b	0
		even		
