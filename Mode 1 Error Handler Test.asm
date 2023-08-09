
		opt l.					; . is the local label symbol
		opt ae-					; automatic evens are disabled by default
		opt ws+					; allow statements to contain white-spaces
		opt w+					; print warnings
		
		include "Debugger Macros and Common Defs.asm"
		include "Mega CD Main CPU (Mode 1).asm"
		include "Common Macros.asm"		
		
workram:		equ $FF0000	

stack_pointer:		equ $FFFFFFFE
v_console_region:	equ $FFFFFFFE
v_bios_id:	equ $FFFFFFFF			
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

cGreen:		equ $0E0					; color green
cRed:		equ $00E					; color red	
cBlue:		equ $E00					; color blue
			
ROM_Start:
Vectors:	
		dc.l stack_pointer			; Initial stack pointer value
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
		dc.l VBlank					; IRQ level 6 (vertical retrace interrupt)
		dc.l ErrorExcept					; IRQ level 7
		dc.l SubCPUError
		dcb.l 15,ErrorExcept				; TRAP #01..#15 exceptions
		dcb.l 16,ErrorExcept				; Unused (reserved)
Header:
		dc.b "SEGA GENESIS    "		; Hardware system ID (Console name)
		dc.b "ORIONNA 2023.AUG"				; Copyright holder and release date
		dc.b "ORION'S MODE 1 ERROR HANDLER TEST               " ; Domestic name
		dc.b "ORION'S MODE 1 ERROR HANDLER TEST               " ; International name


		dc.b "FFFFFFFFFFFFFF"				; Serial/version number (Rev non-0)

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
		
		include "Mega CD Initialization.asm"	; EntryPoint
			
MainLoop:
		cmpi.b	#$FF,(mcd_sub_flag).l	; is sub CPU OK?
		bne.s	.subOK				; branch if it is
		trap #0
		
	.subOK:	
		cmpi.b	#'R',(mcd_subcom_0).l	; is sub CPU done initializing?	
		bne.s	MainLoop				; branch if not
		
		move.l (mcd_subcom_2).l,-(sp)
		
		move.w	#$FF00,mcd_write_protect-mcd_mem_mode(a3)		; reset the sub CPU's gate array
		move.b	#3,mcd_reset-mcd_mem_mode(a3)					; these four values written to these address trigger the reset
		move.b	#2,mcd_reset-mcd_mem_mode(a3)
		move.b	#0,mcd_reset-mcd_mem_mode(a3)

		moveq	#$80-1,d2			; wait for gate array reset to complete
		dbf	d2,*
		
		move.l (mcd_subcom_2).l,-(sp)
		
		illegal			; trigger illegal instruction exception
		;move.w 1(a0),d0	; crash the CPU with word operation at odd address 
		
		move.w	#cGreen,(vdp_data_port).l	; signal success
		bra.s *
		
VBlank:
		bset #mcd_int_bit,(mcd_md_interrupt).l	; trigger VBlank on sub CPU
		rte	
			
		include "KosM to PrgRAM.asm"
		include "Kosinski Decompression.asm"	
		
SubCPU_Program:
		incbin "Sub CPU Program.kosm"	
		even
	
		include "Mega CD Exception Handler (Main CPU).asm"

ROM_End:	
		end