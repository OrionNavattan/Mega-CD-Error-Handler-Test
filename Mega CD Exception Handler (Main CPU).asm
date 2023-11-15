; ----------------------------------------------------------------------------
; Mega CD Error Handler - Main CPU module

; ----------------------------------------------------------------------------
; Original code by Vladikcomper 2016-2023
; Modified by Orion Navattan 2023

; Must be placed at the very end of the ROM for symbol table
; support, but otherwise can be located anywhere in the ROM.
; ----------------------------------------------------------------------------
; Exception entry points
; ----------------------------------------------------------------------------

BusError:
	__ErrorMessage "MAIN CPU: BUS ERROR", _eh_show_sr_usp|_eh_address_error

AddressError:
	__ErrorMessage "MAIN CPU: ADDRESS ERROR", _eh_show_sr_usp|_eh_address_error

IllegalInstr:
	__ErrorMessage "MAIN CPU: ILLEGAL INSTRUCTION", _eh_show_sr_usp

ZeroDivide:
	__ErrorMessage "MAIN CPU: ZERO DIVIDE", _eh_show_sr_usp

ChkInstr:
	__ErrorMessage "MAIN CPU: CHK INSTRUCTION", _eh_show_sr_usp

TrapvInstr:
	__ErrorMessage "MAIN CPU: TRAPV INSTRUCTION", _eh_show_sr_usp

PrivilegeViol:
	__ErrorMessage "MAIN CPU: PRIVILEGE VIOLATION", _eh_show_sr_usp

Trace:
	__ErrorMessage "MAIN CPU: TRACE", _eh_show_sr_usp

Line1010Emu:
	__ErrorMessage "MAIN CPU: LINE 1010 EMULATOR", _eh_show_sr_usp

Line1111Emu:
	__ErrorMessage "MAIN CPU: LINE 1111 EMULATOR", _eh_show_sr_usp

ErrorExcept:
	__ErrorMessage "MAIN CPU: ERROR EXCEPTION", _eh_show_sr_usp

SubCPUTimeout:
	__ErrorMessage "MAIN CPU: TIMED OUT WAITING FOR SUB CPU", _eh_show_sr_usp

; ============================================================================
; ----------------------------------------------------------------------------
; Macro definitions
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
; Sign-extend a value and use it with moveq
; Replicates the signextendB function in Sonic 2 AS; required to prevent the
; assembler from generating a sign-extension warning.
; ----------------------------------------------------------------------------

	if ~def(moveq_)
moveq_:		macro
 		moveq	#(\1\+-((-(\1\&(1<<(8-1))))&(1<<(8-1))))!-((-(\1\&(1<<(8-1))))&(1<<(8-1))),\2
		endm
		endc

; ----------------------------------------------------------------------------
; Save and restore registers from the stack.
; ----------------------------------------------------------------------------

	if ~def(chkifreg)
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
	endc

	if ~def(pushr)
pushr:		macro
	chkifreg "\1"
	if (isreg=1)&(isregm=1)
		if strlen("\0")>0					; check if size is specified
		movem.\0	\1,-(sp)			; save multiple registers (b/w)
		else
		movem.l	\1,-(sp)				; save multiple registers
		endc
	else
		if strlen("\0")>0					; check if size is specified
		move.\0	\1,-(sp)				; save one register (b/w)
		else
			move.l	\1,-(sp)			; save one whole register
		endc
	endc
	endm
	endc

	if ~def(popr)
popr:		macro
	chkifreg "\1"
	if (isreg=1)&(isregm=1)
		if strlen("\0")>0					; check if size is specified
		movem.\0	(sp)+,\1			; restore multiple registers (b/w)
		else
		movem.l	(sp)+,\1				; restore multiple whole registers
		endc
	else
		if strlen("\0")>0					; check if size is specified
		move.\0	(sp)+,\1				; restore one register (b/w)
		else
		move.l	(sp)+,\1				; restore one whole register
		endc
	endc
	endm
	endc


; ----------------------------------------------------------------------------
; Constants
; ----------------------------------------------------------------------------

VRAM_Font:		equ 	(('!'-1)*$20)
VRAM_PlaneA: 	equ		$8000
VRAM_PlaneB: 	equ		VRAM_PlaneA

VRAM_ErrorScreen:	equ		VRAM_PlaneA
VRAM_DebuggerPage:	equ		$C000



_white:			equ 	0
_yellow: 		equ 	1<<13
_blue:			equ 	2<<13
_blue2:			equ 	3<<13

sizeof_dumpedregs:		equ 4*(8+7)	; $3C

initial_sp:				equ 	0 ; location of initial stack pointer value
hblank_vector:			equ 	$70
vblank_vector:			equ		$78

port_1_data:			equ 	$A10003
vdp_data_port:			equ 	$C00000
vdp_control_port:		equ 	$C00004
mcd_reset:				equ		$A12001
	sub_bus_request_bit:	equ 1		; set to 1 to request sub CPU bus, 0 to return, when read, returns 1 once bus has been granted
mcd_mem_mode:			equ		$A12003
	program_ram_bank_1:		equ 6	; program RAM bank bits, sets program RAM bank to access
	program_ram_bank_2:		equ 7
	program_ram_bank:		equ (1<<program_ram_bank_1)|(1<<program_ram_bank_2) ; $C0
mcd_main_flag:			equ 	$A1200E
mcd_sub_flag:			equ		$A1200F
mcd_subcom_0:			equ		$A12020
mcd_subcom_2:			equ		$A12024
program_ram:			equ 	$420000

; ----------------------------------------------------------------------------
; VDP register settings
; ----------------------------------------------------------------------------
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

; --------------------------------------------------------------------------
; VDP colors
; --------------------------------------------------------------------------

cWhite:	equ $EEE
cBlack:	equ 0

; --------------------------------------------------------------------------
; Joypad input
; --------------------------------------------------------------------------

bitStart:	equ 7
bitA:		equ 6
bitC:		equ 5
bitB:		equ 4
bitR:		equ 3
bitL:		equ 2
bitDn:		equ 1
bitUp:		equ 0
btnStart:	equ 1<<bitStart					; Start button	($80)
btnA:		equ 1<<bitA					; A		($40)
btnC:		equ 1<<bitC					; C		($20)
btnB:		equ 1<<bitB					; B		($10)
btnR:		equ 1<<bitR					; Right		($08)
btnL:		equ 1<<bitL					; Left		($04)
btnDn:		equ 1<<bitDn					; Down		($02)
btnUp:		equ 1<<bitUp					; Up		($01)
btnDir:		equ btnL+btnR+btnDn+btnUp			; Any direction	($0F)
btnABC:		equ btnA+btnB+btnC				; A, B or C	($70)


; ----------------------------------------------------------------------------
; Console RAM
; ----------------------------------------------------------------------------

; RAM structure
			rsreset
Console_ScreenPosReq:	rs.l	1				;		screen position request for VDP
Console_CharsPerLine:	rs.w	1				; d2	number of characters per line
Console_CharsRemaining:	rs.w	1				; d3	remaining number of characters
Console_BasePattern:	rs.w	1				; d4	base pattern
Console_ScreenRowSz:	rs.w	1				; d6	row size within screen position
Console_Magic:			rs.b	1				;		should contain a magic number to ensure this is valid console memory area
						rs.b	1				;		<Reserved>
sizeof_Console_RAM:		equ		__rs

_ConsoleMagic:	equ	$5D	; magic value stored in Console_Magic

; ----------------------------------------------------------------------------
; Instruction opcodes
; ----------------------------------------------------------------------------

ins_bsr:	equ $61	; high byte of opcode for bsr
ins_jsr:	equ $4E	; high byte of opcode for jsr
	addr_reg_indir:	equ $90 ; low byte of jsr address register indirect
	abs_long:		equ $B9	; low byte of jsr absolute long
ins_jmp_abs_long:	equ $4EF9


; ===========================================================================
; ---------------------------------------------------------------------------
; Address registers debugger
; ---------------------------------------------------------------------------

	if DebuggerExtensions

Debugger_AddressRegisters:
		pushr.l	a0-a6						; dump registers

		; Setup screen header (position and "Address Registers:" text)
		lea	Str_AddrScreenHeader(pc),a0
		jsr	Console_Write(pc)

		; Render registers table
		lea	(sp),a4							; get registers dump in the stack ...
		moveq	#7-1,d6							; number of registers to process (minus one)

		move.w	#' '<<8,-(sp)						; STACK => dc.b	pal0, 'a0: ', 0
		move.l	#(pal0<<24)|'a0:',-(sp)			; ''

	.loop:
		lea	(sp),a0							; a0 = label
		move.l	(a4)+,d1							; d1 = address register value

		jsr	Error_DrawOffsetLocation(pc)

		addq.b	#1,2(sp)							; add 1 to register's digit ASCII
		dbf	d6,.loop

		lea	6+(4*7)(sp),sp						; STACK => free string buffer and registers themselves
		rts
; ===========================================================================

Str_AddrScreenHeader:
		dc.b	endl,setx,1,setw,38
		dc.b	pal1,'Address Registers:',endl,endl,0
		even

; ----------------------------------------------------------------------------
; Backtrace debugger
; ----------------------------------------------------------------------------

Debugger_Backtrace:
		; Setup screen header (position and "Backtrace:" text)
		lea	Str_BacktrceScreenHeader(pc),a0
		jsr	Console_Write(pc)

		; ----------------------------------------
		; Build backtrace routine
		; ----------------------------------------

		; Registers layout:
		;.data0:		equr	d0
		;.data1:		equr	d1
		;.addr0:		equr	a0
		;.stack_top:	equr	a1
		;.stack_curr:	equr	a2

		movea.l	(initial_sp).w,a1
		subq.l	#4,a1					; set final longword to read
		lea	(sp),a2
		jsr	Error_MaskStackBoundaries(pc)

		cmpa.l	a2,a1			; are we within stack?
		bcs.s	.done							; if not, branch

	.try_offset_loop:
		cmpi.w	#$40,(a2)				; is address within ROM ($000000..$3FFFFF)?
		bcc.s	.try_next_offset				; if not, branch
		move.l	(a2),d0					; d0 = possible return address
		beq.s	.try_next_offset				; if address is zero, branch
		movea.l	d0,a0					; a0 = possible return address
		andi.w	#1,d0						; is address even?
		bne.s	.try_next_offset				; if not, branch

		; Trying to find jsr/BSR instructions before the return address
	.chk_2byte:
		; 2-byte instructions:
		move.b	-(a0),d1
		move.b	-(a0),d0

	.chk_2byte_bsr:
		; bsr.s = %01000001 XXXXXXXX
		cmpi.b	#ins_bsr,d0					; is instruction bsr.s?
		bne.s	.chk_2byte_jsr				; if not, branch
		tst.b	d1							; bsr.s must use non-zero displacement
		bne.s	.offset_is_caller				; branch if it is indeed non-zero

	.chk_2byte_jsr:
		; jsr (an) = %01001110 10010XXX
		cmpi.b	#ins_jsr,d0					; is instruction jsr?
		bne.s	.chk_4byte						; if not, branch
		andi.b	#%11111000,d1				; clear out "EARegister" part
		cmpi.b	#addr_reg_indir,d1				; is mode (an)?
		beq.s	.offset_is_caller				; if yes, branch

	.chk_4byte:
		move.w	-(a0),d0

	.chk_4byte_bsr:
		; bsr.w	= %01000001 00000000 XXXXXXXX XXXXXXXX
		cmpi.w	#ins_bsr<<8,d0					; is instruction BSR.w?
		beq.s	.offset_is_caller				; if yes, branch

	.chk_4byte_jsr:
		; jsr d16(an)	= %01001110 10101XXX XXXXXXXX XXXXXXXX
		; jsr d8(an,xn)	= %01001110 10110XXX XXXXXXXX XXXXXXXX
		; jsr (xxx).w	= %01001110 10111000 XXXXXXXX XXXXXXXX
		; jsr d16(pc)	= %01001110 10111010 XXXXXXXX XXXXXXXX
		; jsr d8(pc,xn)	= %01001110 10111011 XXXXXXXX XXXXXXXX
		move.b	d0,d1						; copy to d1
		clr.b	d0							; clear addressing mode byte
		cmpi.w	#ins_jsr<<8,d0					; is instruction jsr?
		bne.s	.chk_6byte						; if not, branch
		cmpi.b	#%10101000,d1				; low byte should be between %10101000
		bcs.s	.chk_6byte
		cmpi.b	#%10111011,d1				; ... and %10111011
		bhi.s	.chk_6byte
		cmpi.b	#%10111001,d1				; jsr (xxx).l is invalid, because it's not 4 bytes!
		bne.s	.offset_is_caller

	.chk_6byte:
		; jsr (xxx).l	= %01001110 10111001 XXXXXXXX XXXXXXXX
		cmpi.w	#(ins_jsr<<8)|abs_long,-(a0)	; is instruction jsr (xxx).l?
		bne.s	.try_next_offset				; if not, branch

	.offset_is_caller:
		pushr.l	a2
		pushr.l	a1
		move.l	a0,d1						; d1 = offset
		jsr	Error_DrawOffsetLocation2(pc)
		popr.l	a1
		popr.l	a2

		addq.l	#2,a2					; for +4 (see below)

	.try_next_offset:
		addq.l	#2,a2
		cmpa.l	a2,a1
		bcc.s	.try_offset_loop

	.done:
		rts
; ===========================================================================

Str_BacktrceScreenHeader:
		dc.b	endl,setx,1,setw,38
		dc.b	pal1,'Backtrace:',endl,endl,0
		even


; ----------------------------------------------------------------------------
; Enters a console program specified after the subroutine call
; ----------------------------------------------------------------------------

ErrorHandler_ConsoleOnly:
		disable_ints
		lea	-sizeof_Console_RAM(sp),sp		; allocate memory for console
		pushr.l	d0-a6
		lea	sizeof_dumpedregs(sp),a3						; a3 = Console RAM pointer
		jsr	ErrorHandler_SetupVDP(pc)
		jsr	Error_InitConsole(pc)
		popr.l	d0-a6
		pea	Error_IdleLoop(pc)
		pushr.l	sizeof_Console_RAM+4(sp)	; retrieve return address
		rts										; jump to return address

; ----------------------------------------------------------------------------
; Clears currently used console
; ----------------------------------------------------------------------------

ErrorHandler_ClearConsole:
		pushr.l	a3
		move.l	usp,a3
		cmpi.b	#_ConsoleMagic,Console_Magic(a3)
		bne.s	.quit

		pushr.l	d0-d1/d5/a1/a5-a6
		lea	(vdp_control_port).l,a5					; a5 = VDP control port
		lea	vdp_data_port-vdp_control_port(a5),a6				; a6 = VDP data port
		lea	ErrorHandler_ConsoleConfig_Initial(pc),a1
		jsr	Console_Reset(pc)
		popr.l	d0-d1/d5/a1/a5-a6

	.quit:
		popr.l	a3
		rts

; ----------------------------------------------------------------------------
; Write formatted strings to KDebug message buffer

; input:
;	a1	= pointer to source formatted string
;	a2	= arguments buffer pointer

; 	uses d7.l, a0, a1, a2
; ----------------------------------------------------------------------------

KDebug_WriteLine_Formatted:
		pea		KDebug_FlushLine(pc)

KDebug_Write_Formatted:

sizeof_stringbuffer = $10

		move.l	usp,a0
		cmpi.b	#_ConsoleMagic,Console_Magic(a0)	; are we running console?
		beq.s	.quit						; if yes, disable KDebug output, because it breaks VDP address

		pushr.l	a4
		lea	KDebug_FlushBuffer(pc),a4		; flushing function
		lea	-sizeof_stringbuffer(sp),sp		; allocate string buffer
		lea	(sp),a0					; a0 = string buffer
		moveq	#sizeof_stringbuffer-2,d7			; d7 = number of characters before flush -1

		jsr	FormatString(pc)
		lea	sizeof_stringbuffer(sp),sp		; free string buffer

		popr.l	a4

	.quit:
		rts

; ----------------------------------------------------------------------------
; Flush buffer callback raised by FormatString

; input:
;	a0	= buffer position
;	d7.w = number of characters remaining in buffer - 1

; output:
;	a0	= buffer position after flushing
;	d7.w = number of characters before next flush - 1
;	carry	0 = continue operation
;			1 = terminate FormatString with error condition

; WARNING: This function shouldn't modify d0-d4 / a1-a3!
; ----------------------------------------------------------------------------

KDebug_FlushBuffer:
		clr.b	(a0)+					; finalize buffer

		neg.w	d7
		addi.w	#sizeof_stringbuffer-1,d7
		suba.w	d7,a0					; a0 = start of the buffer

		pushr.l	a0
		pushr.l	a5

		lea	(vdp_control_port).l,a5
		move.w	#$9E00,d7
		bra.s	.write_buffer_next
; ===========================================================================

	.write_buffer:
		move.w	d7,(a5)

	.write_buffer_next:
		move.b	(a0)+,d7
		bgt.s	.write_buffer			; if not null-terminator or flag, branch
		beq.s	.write_buffer_done		; if null-terminator, branch
		subi.b	#endl,d7				; is flag "new line"?
		beq.s	.write_buffer			; if yes, branch
		bra.s	.write_buffer_next		; otherwise, skip writing
; ===========================================================================

	.write_buffer_done:
		popr.l	a5
		popr.l	a0
		moveq	#sizeof_stringbuffer-2,d7		; d7 = number of characters before flush -1
		rts								; WARNING! Must return carry=0

; ----------------------------------------------------------------------------
; Finishes the current line and flushes KDebug message buffer
; ----------------------------------------------------------------------------

KDebug_FlushLine:
		pushr.l	a0
		move.l	usp,a0
		cmpi.b	#_ConsoleMagic,Console_Magic(a0)	; are we running console?
		beq.s	.quit						; if yes, disable KDebug output, because it breaks VDP address

		move.w	#$9E00,(vdp_control_port).l			; send null-terminator

	.quit:
		popr.l	a0
		rts

; ----------------------------------------------------------------------------
; Pause console program executions until A/B/C are pressed
; ----------------------------------------------------------------------------

ErrorHandler_PauseConsole:
		pushr.l	d0-d1/a0-a1/a3
		move.l	usp,a3
		cmpi.b	#_ConsoleMagic,Console_Magic(a3)
		bne.s	.quit

		pushr.w	#0					; allocate joypad memory
		bsr.s	Joypad_Wait_ABCStart		; extra call to initialize joypad bitfield and avoid misdetecting pressed buttons

	.loop:
		bsr.s	Joypad_Wait_ABCStart		; is A/B/C pressed?
		beq.s	.loop						; if not, branch

		addq.w	#2,sp

	.quit:
		popr.l	d0-d1/a0-a1/a3
		rts

; ----------------------------------------------------------------------------
; Pause console program executions until A/B/C or Start are pressed

; input:
;	4(sp) =	pointer to a word that stores pressed/held buttons

; output:
;	d0.b = pressed A/B/C/Start state; fFormat: %SACB0000

;	uses d0.l, d1.l, a0, a1
; ----------------------------------------------------------------------------

Joypad_Wait_ABCStart:
		bsr.s	VSync
		lea	4(sp),a0					; a0 = Joypad memory
		lea	(port_1_data).l,a1					; a1 = poypad 1 Port
		bsr.s	ReadJoypad
		moveq_	btnStart|btnABC,d0
		and.b	5(sp),d0					; start/A/B/C pressed?
		rts									; return Z=0 if pressed

; ----------------------------------------------------------------------------
; A simple controller that allows switching between debuggers
; ----------------------------------------------------------------------------

ErrorHandler_PagesController:
		pushr.l	d0-a6				; back up all the registers ...
		pushr.w	#0					; allocate joypad memory
		bsr.s	Joypad_Wait_ABCStart		; extra call to initialize joypad bitfield and avoid misdetecting pressed buttons

	.main_loop:
		lea	(vdp_control_port).l,a5				; a5 = VDP control port
		lea	vdp_data_port-vdp_control_port(a5),a6					; a6 = VDP data port

		bsr.s	Joypad_Wait_ABCStart		; start/A/B/C pressed?
		beq.s	.main_loop					; if not, branch
		bmi.s	.show_main_error			; if start pressed, branch

		; Detect debugger to run depending on currently pressed button (A/B/C)
		lea	ErrorHandler_ExtraDebuggerList-4-4(pc),a0		; another "-4" to skip always-reset Start button

	.chkbutton:
		addq.l	#4,a0						; next debugger
		add.b	d0,d0
		bcc.s	.chkbutton

		move.l	(a0),d0					; d0 = debugger address
		ble.s	.show_main_error		; if address is zero or negative, branch
		movea.l	d0,a0

		; Initialize console for the debugger
		lea	-sizeof_Console_RAM(sp),sp	; allocate memory for console
		lea	ErrorHandler_ConsoleConfig_Shared(pc),a1
		lea	(sp),a3					; a3 = console RAM
		vdp_comm.l	move,VRAM_DebuggerPage,vram,write,d5		; d5 = screen start address
		jsr	Console_InitShared(pc)

		; Display debugger's own console
		move.l	#((vdp_fg_nametable+VRAM_DebuggerPage/$400)<<16)|(vdp_bg_nametable+VRAM_DebuggerPage/$2000),(a5)
		move.l	d5,(a5)					; restore last VDP write address

		; Execute the debugger
		pea	.destroy_debugger(pc)
		pea	(a0)						; use debbuger's context upon return
		movem.l	sizeof_Console_RAM+2+4(sp),d0-a6	; switch to original registers ...
		rts									; switch to debugger's context ...
; ===========================================================================

	.destroy_debugger:
		lea	sizeof_Console_RAM(sp),sp	; deallocate console memory
		bra.s	.main_loop
; ===========================================================================

	.show_main_error:
		; WARNING! Make sure a5 is "vdp_control_port"!
		move.l	ErrorHandler_VDPConfig_Nametables(pc),(a5)
		bra.s	.main_loop

; ----------------------------------------------------------------------------
; Performs VSync
; ----------------------------------------------------------------------------

VSync:
		lea	(vdp_control_port).l,a0

	.loop0:
		move.w	(a0),ccr	; get VDP status register, setting N bit if VBlank is occurring
		bmi.s	.loop0		; branch if VBlank has not yet occured

	.loop1:
		move.w	(a0),ccr	; same as above
		bpl.s	.loop1		; branch if VBlank is not finished

		rts

; ----------------------------------------------------------------------------
; Reads input from joypad
; ----------------------------------------------------------------------------

ReadJoypad:
		move.b	#0,(a1)			; command to poll for A/Start
		nop							; wait for port (0/1)
		moveq_	btnStart|btnA,d1		; wait for port (1/1) ... and do useful work (0/1)
		move.b	(a1),d0			; get data for A/Start
		lsl.b	#2,d0
		move.b	#$40,(a1)			; command to poll for B/C/UDLR
		nop							; wait for port (0/1)
		and.b	d1,d0				; wait for port (1/1) ... and do useful work (1/1)
		move.b	(a1),d1				; get data for B/C/UDLR
		andi.b	#btnDir|btnB|btnC,d1
		or.b	d1,d0				; d0 = held buttons bitfield (negated)
		not.b	d0					; d0 = held buttons bitfield (normal)
		move.b	(a0),d1			; d1 = previously held buttons
		eor.b	d0,d1				; toggle off buttons that are being pressed
		move.b	d0,(a0)+			; put raw controller input (for held buttons)
		and.b	d0,d1
		move.b	d1,(a0)+			; put pressed controller input
		rts

; ----------------------------------------------------------------------------
; Debugger extensions table
; ----------------------------------------------------------------------------

ErrorHandler_ExtraDebuggerList:
		dc.l	Debugger_AddressRegisters	; for button A
		dc.l	0	; for button C (not B)
		dc.l	Debugger_Backtrace	; for button B (not C)

	endc ; DebuggerExtensions

; ----------------------------------------------------------------------------
; Main CPU handler for sub CPU exceptions. Entered by means of one of the trap
; vectors when it detects that the sub CPU has crashed.

; Nearly the same as that used for processing main CPU exceptions, except it
; is reading the exception arguments, register dump and stack frame from the
; sub CPU's stack.

; This assumes that the initial stack set by the BIOS is still being used, and
; that the stack is in the first program RAM bank (which it is by default).
; Both Mode 1 and Mode 2 should be supported; the only change being the value
; of 'program_ram'.

; GLOBAL REGISTERS:
;		d6.b = error handler flags bitfield
;		a3	= pointer to additional parameters
;		a4	= bottom of sub CPU stack (after exception frame)
; ----------------------------------------------------------------------------

SubCPUError:
		disable_ints				; disable interrupts
		move.b	(mcd_sub_flag).l,(mcd_main_flag).l	; let sub CPU know we've noticed

	.waitsub:
		tst.b	(mcd_sub_flag).l	; is the sub CPU done?
		bne.s	.waitsub			; if not, branch

	.waitsubbus:
		bset	#sub_bus_request_bit,(mcd_reset).l			; request the sub CPU bus
		beq.s	.waitsubbus									; branch if it has been granted

		lea	-sizeof_Console_RAM(sp),sp		; allocate memory for console on main CPU stack
		jsr	ErrorHandler_SetupVDP(pc)

		; Initialize console subsystem
		lea	4(sp),a3
		jsr	Error_InitConsole(pc)

		; ----------------
		; Screen header
		; ----------------

		lea	Str_SetErrorScreen(pc),a0
		jsr	Console_Write(pc)

		move.b	(mcd_mem_mode).l,d3
		andi.b	#(~program_ram_bank)&$FF,d3			; set program ram bank to 0
		move.b	d3,(mcd_mem_mode).l

		movea.l	(mcd_subcom_0).l,a4	; get sub CPU stack bottom address (the start of the dumped registers)
		adda.l	#program_ram,a4	; convert to main CPU address

		pushr.l a4	; back up sub CPU stack bottom address for later

		lea sizeof_dumpedregs+4(a4),a4	; a4 = arguments and exception stack frame

		; Print error description
		movea.l	(a4)+,a1						; get error text
		adda.l	#program_ram,a1					; convert to main CPU address
		lea	(a4),a2								; a2 = load arguments buffer (if present)
		jsr	Console_WriteLine_Formatted(pc)
		jsr	Console_StartNewLine(pc)

		lea	(a2),a4						; a4 = sub CPU stack frame (after arguments buffer was processed by Console_Write)

		; Load screen configuration bitfield
		move.b	(a1)+,d6						; d6 = configuration bitfield
		bpl.s	.align_ok						; if "_eh_align_offset" is not set, branch
		addq.w	#1,a1							; skip a byte to avoid address error on reading the next word

	.align_ok:
		lea	(a1),a3							; a3 may be used to fetch console program address later

		; Print error address (for Address error only)
		btst	#extended_frame_bit,d6							; does error has extended stack frame (Address Error only)?
		beq.s	.skip1							; if not, branch

		lea Str_Address(pc),a0				; a0 = label string
		move.l	2(a4),d1						; d1 = address error offset
		jsr	Error_DrawOffsetLocation(pc)
		addq.w	#8,a4							; skip extension part of the stack frame

	.skip1:
		; Print exception offset
		lea Str_Offset(pc),a0				; a0 = label string
		move.l	2(a4),d1						; d1 = last return offset
		jsr	Error_DrawOffsetLocation(pc)

		; Print caller
		movea.l	(program_ram).l,a1			; a1 = sub CPU initial stack pointer value
		adda.l	#program_ram,a1				; convert to main CPU address
		lea	6(a4),a2						; a2 = call stack (after exception stack frame)
		jsr	Error_GuessCaller(pc)			; d1 = caller
		lea Str_Caller(pc),a0				; a0 = label string
		jsr	Error_DrawOffsetLocation(pc)
		jsr	Console_StartNewLine(pc)

		; ----------------
		; Registers
		; ----------------

		popr.l a4							; restore sub CPU stack bottom address
		lea	4(a4),a2						; use register buffer as arguments

		; Print data registers
		jsr	Console_GetPosAsXY(pc)			; d0/d1 = XY-pos
		move.w	d1,-(sp)						; remember line
		moveq	#3,d0							; left margin for data registers
		jsr	Console_SetPosAsXY(pc)
		move.w	#'d0',d0						; d0 = 'd0', what a twist !!!
		moveq	#8-1,d5						; number of registers - 1
		jsr	Error_DrawRegisters(pc)

		; Print address registers
		move.w	(sp)+,d1					; restore line
		moveq	#$11,d0						; left margin for address registers
		jsr	Console_SetPosAsXY(pc)
		move.w	#'a0',d0
		moveq	#7-1,d5						; number of registers - 1
		jsr	Error_DrawRegisters(pc)

		; Special case : stack pointer (SP)
		move.w	#'sp',d0
		moveq	#0,d5					; number of registers - 1
		lea	sizeof_dumpedregs(a4),a2		; a2 = top of stack frame
		move.l	a2,d4
		subi.l	#program_ram,d4		; convert to sub CPU address
		move.l	d4,-(sp)
		lea	(sp),a2						; a2 = pointer to where address of frame bottom is written
		jsr	Error_DrawRegisters(pc)
		addq.w	#4,sp

		; Display USP and SR (if requested)
		btst	#show_sr_usp_bit,d6
		beq.s	.skip2

   		; Draw 'USP'
		lea	Str_USP(pc),a1
		lea	(a4),a2						; a2 = USP dumped by sub CPU
		jsr	Console_Write_Formatted(pc)

		; Draw 'SR'
		lea	Str_SR(pc),a1
		lea	sizeof_dumpedregs+4+4(a4),a2					; a2 = top of exception frame
		btst	#extended_frame_bit,d6							; does error has extended stack frame (Address Error only)?
		beq.s	.notaddrerr							; if not, branch
		addq.w	#8,a2					; skip extended frame

	.notaddrerr:
		jsr	Console_WriteLine_Formatted(pc)

	.skip2:
		jsr	Console_GetPosAsXY(pc)			; d0/d1 = XY-pos
		addq.w	#1,d1							; skip a line
		moveq	#1,d0							; left margin for data registers
		jsr	Console_SetPosAsXY(pc)

		jsr	Console_StartNewLine(pc)

		; -----------------
		; Stack contents
		; -----------------

		movea.l (program_ram).l,a1			; a1 = sub CPU initial stack pointer
		adda.l	#program_ram,a1				; convert to main CPU address

		lea	sizeof_dumpedregs+4+4(a4),a2			; a2 = top of exception frame
		;subq.l	#1,a1								; unnecessary here as sub CPU stack top can never be zero if BIOS is used
		btst	#extended_frame_bit,d6							; does error has extended stack frame (Address Error only)?
		beq.s	.notaddrerr2							; if not, branch
		addq.w	#8,a2					; skip extended frame

	.notaddrerr2:
		bsr.w	Error_MaskStackBoundaries

		jsr	Console_GetPosAsXY(pc)			; d0/d1 = XY-pos
		moveq	#28-3,d5
		sub.w	d1,d5
		bmi.s	.stack_done

		bsr.w	Error_DrawStackRow_First

	.stack_loop:
		jsr	Error_DrawStackRow(pc)
		dbf	d5,.stack_loop

	.stack_done:
		bra.w	Error_IdleLoop

; ----------------------------------------------------------------------------
; Main error handler
; ----------------------------------------------------------------------------
; GLOBAL REGISTERS:
;		d6.b	Error handler flags bitfield
;		a3		Pointer to additional parameters
;		a4		Stack pointer (after exception frame)

; NOTE:	It should be called via jsr/BSR exclusively with error
;		information following the jsr/BSR opcode.

; ERROR DATA FORMAT:
;		dc.b	"<Error formatted message>", 0
;		dc.b	<Error Handler flags>
;		even
;		jmp	<ConsoleProgram> (optional)

;	Flags bitfield uses the following format:
;		bit #0:	If set, loads extended stack frame (used for
;				Address and Bus errors only)
;		bit #1: If set, displays SR and USP registers
;		bit #2:	<UNUSED>
;		bit #3:	<UNUSED>
;		bit #4:	<UNUSED>
;		bit #5:	If set, displays full screen, but then calls
;				console program (via "jmp <ConsoleProgram>")
;		bit #6:	If set, displays error header only, then calls
;				console program (via "jmp <ConsoleProgram>")
;		bit #7:	If set, skips a byte after this byte, so
;				jmp <ConsoleProgram> is word-aligned.
; ----------------------------------------------------------------------------

ErrorHandler:
		disable_ints						; disable interrupts for good
		lea	-sizeof_Console_RAM(sp),sp		; STACK => allocate memory for console
		pushr.l d0-a6 					; STACK => dump registers ($3C bytes)

.waitsubbus:
		; Halt the Sub CPU
		bset	#sub_bus_request_bit,(mcd_reset).l			; request the sub CPU bus
		beq.s	.waitsubbus									; branch if it has been granted

		jsr	ErrorHandler_SetupVDP(pc)
		lea sizeof_dumpedregs+sizeof_Console_RAM(sp),a4	; a4 = arguments, stack frame

		move.l	usp,a0
		pushr.l	a0						; save USP if needed to display later (as it's overwritten by the console subsystem)

		; Initialize console subsystem
		lea	sizeof_dumpedregs+4(sp),a3					; a3 = Console RAM
		jsr	Error_InitConsole(pc)

		; ----------------
		; Screen header
		; ----------------

		lea	Str_SetErrorScreen(pc),a0
		jsr	Console_Write(pc)

		; Print error description
		movea.l	(a4)+,a1						; get error text
		lea	(a4),a2						; a2 = load arguments buffer (if present)
		jsr Console_WriteLine_Formatted(pc)
		jsr	Console_StartNewLine(pc)

		lea	(a2),a4						; a4 = stack frame (after arguments buffer was processed by Console_Write)

		; Load screen configuration bitfield
		move.b	(a1)+,d6						; d6 = configuration bitfield
		bpl.s	.align_ok						; if "_eh_align_offset" is not set, branch
		addq.w	#1,a1							; skip a byte to avoid address error on reading the next word

	.align_ok:
		lea	(a1),a3						; a3 may be used to fetch console program address later

		; Print error address (for Address error and Bus Error only)
		btst	#extended_frame_bit,d6							; does error has extended stack frame (Address Error and Bus Error only)?
		beq.s	.skip							; if not, branch

		lea Str_Address(pc),a0				; a0 = label string
		move.l	2(a4),d1						; d1 = address error offset
		jsr	Error_DrawOffsetLocation(pc)
		addq.w	#8,a4							; skip extension part of the stack frame

	.skip:
		; Print exception offset
		lea Str_Offset(pc),a0				; a0 = label string
		move.l	2(a4),d1						; d1 = last return offset
		jsr	Error_DrawOffsetLocation(pc)

		; Print caller
		movea.l	(initial_sp).w,a1							; a1 = stack top boundary
		lea	6(a4),a2						; a2 = call stack (after exception stack frame)
		jsr	Error_GuessCaller(pc)			; d1 = caller
		lea Str_Caller(pc),a0				; a0 = label string
		jsr	Error_DrawOffsetLocation(pc)
		jsr	Console_StartNewLine(pc)

		btst	#console_bit,d6							; is execute console program bit set?
		bne.w	Error_EnterConsoleProgram		; if yes, branch

		; ----------------
		; Registers
		; ----------------

		lea	4(sp),a2						; use register buffer as arguments

		; Print data registers
		jsr	Console_GetPosAsXY(pc)			; d0/d1 = XY-pos
		move.w	d1,-(sp)						; remember line
		moveq	#3,d0							; left margin for data registers
		jsr	Console_SetPosAsXY(pc)
		move.w	#'d0',d0						; d0 = 'd0', what a twist !!!
		moveq	#8-1,d5						; number of registers - 1
		jsr	Error_DrawRegisters(pc)

		; Print address registers
		move.w	(sp)+,d1						; restore line
		moveq	#$11,d0						; left margin for address registers
		jsr	Console_SetPosAsXY(pc)
		move.w	#'a0',d0
		moveq	#7-1,d5						; number of registers - 1
		jsr	Error_DrawRegisters(pc)

		; Special case : stack pointer (SP)
		move.w	#'sp',d0
		moveq	#0,d5							; number of registers - 1
		move.l	a4,-(sp)
		lea	(sp),a2
		jsr	Error_DrawRegisters(pc)
		addq.w	#4,sp

		; Display USP and SR (if requested)
		btst	#show_sr_usp_bit,d6
		beq.s	.skip2

  	 	; Draw 'USP'
		lea	Str_USP(pc),a1
		lea	(sp),a2						; a2 = USP saved in stack (how convy!)
		jsr	Console_Write_Formatted(pc)

		; Draw 'SR'
		lea	Str_SR(pc),a1
		lea	(a4),a2
		jsr	Console_WriteLine_Formatted(pc)

	.skip2:
		addq.w	#4,sp							; free USP copy from the stack (we don't need it anymore)

		jsr	Console_GetPosAsXY(pc)			; d0/d1 = XY-pos
		addq.w	#1,d1							; skip a line
		moveq	#1,d0							; left margin for data registers
		jsr	Console_SetPosAsXY(pc)


		; --------------------
		; Interrupt handlers
		; --------------------

		; Print vertical and horizontal interrupt handlers, if available
		move.l	(vblank_vector).w,d0						; d0 = VInt vector address
		lea	Str_VInt(pc),a0
		jsr	Error_DrawInterruptHandler(pc)

		move.l	(hblank_vector).w,d0						; d0 = HInt vector address
		lea	Str_HInt(pc),a0
		jsr	Error_DrawInterruptHandler(pc)

		jsr	Console_StartNewLine(pc)		; newline

		; -----------------
		; Stack contents
		; -----------------

		movea.l (initial_sp).w,a1							; a1 = stack top
		lea	(a4),a2						; a2 = stack bottom
		subq.l	#1,a1							; hotfix to convert stack pointer $0000 to $FFFF, decrement by 1 shouldn't make any difference otherwise
		bsr.s	Error_MaskStackBoundaries

		jsr	Console_GetPosAsXY(pc)			; d0/d1 = XY-pos
		moveq	#28-3,d5
		sub.w	d1,d5
		bmi.s	.stack_done

		bsr.s	Error_DrawStackRow_First

	.stack_loop:
		jsr	Error_DrawStackRow(pc)
		dbf	d5,.stack_loop

	.stack_done:
		btst	#return_bit,d6							; is execute console program (at the end) bit set?
		bne.s	Error_RunConsoleProgram

Error_IdleLoop:
		nop
		bra.s	Error_IdleLoop

; ----------------------------------------------------------------------------
; Routine to enter console mode after writting error header
; ----------------------------------------------------------------------------

Error_EnterConsoleProgram:
		moveq	#0,d1
		jsr	Console_SetBasePattern(pc)

Error_RunConsoleProgram:
		move.l	a3,(sp)+						; replace USP in stack with return address
		popr.l	d0-a6					; restore registers
		pea	Error_IdleLoop(pc)				; set return address
		pushr.l	-sizeof_dumpedregs(sp),-(sp)					; retrieve "a3" saved earlier
		rts										; jump to a3
; ===========================================================================

Error_InitConsole:
		lea	ErrorHandler_ConsoleConfig(pc),a1
		lea	Art1bpp_Font(pc),a2
		jmp	Console_Init(pc)				; d5 = On-screen position

; ----------------------------------------------------------------------------
; Subroutine to mask top and bottom stack boundaries to 24-bit

; input:
;	a1 = stack top boundary
;	a2 = current stack pointer

; 	uses d1.l, d2.l
; ----------------------------------------------------------------------------

Error_MaskStackBoundaries:
		move.l 	#$FFFFFF,d1

		move.l	a1,d2
		and.l	d1,d2
		movea.l	d2,a1

		move.l	a2,d2
		and.l	d1,d2
		movea.l	d2,a2
		rts

; ----------------------------------------------------------------------------
; Subroutine to draw contents of stack row

; input:
;	a0 = string buffer
;	a1 = top of stack pointer
;	a2 = arguments (stack contents)

;	uses d1.w, d7.l
; ----------------------------------------------------------------------------

sizeof_stringbuffer: = $30

Error_DrawStackRow_First:
		lea	-sizeof_stringbuffer(sp),sp
		lea	(sp),a0				; a0 = string buffer
		moveq	#-1,d7					; size of the buffer for formatter functions (we assume buffer will never overflow)

		move.l	#'(SP)',(a0)+
		move.w	#': ',(a0)+
		bra.s	Error_DrawStackRow_Continue
; ===========================================================================

Error_DrawStackRow:
		lea	-sizeof_stringbuffer(sp),sp
		lea	(sp),a0				; a0 = string buffer
		moveq	#-1,d7					; size of the buffer for formatter functions (we assume buffer will never overflow)

		move.w	#' +',(a0)+
		move.w	a2,d1
		sub.w	a4,d1					; d1 = stack displacement
		jsr 	FormatHex_Byte(pc)
		move.w	#': ',(a0)+

Error_DrawStackRow_Continue:
		moveq	#5,d0					; number of words to display

	.loop:
		moveq_	pal2,d1	; use light blue
		cmpa.l	a1,a2					; is current word out of stack?
		bcs.s	.in_stack						; if not, branch
		moveq_	pal3,d1	; use dark blue

	.in_stack:
		move.b	d1,(a0)+				; setup color
		move.w	(a2)+,d1
		jsr	FormatHex_Word(pc)
		move.b	#' ',(a0)+
		dbf d0,.loop

		clr.b	(a0)+					; finalize string

		; Draw string on screen
		lea	(sp),a0
		moveq	#0,d1
		jsr	Console_WriteLine_WithPattern(pc)
		lea	sizeof_stringbuffer(sp),sp
		rts

; ----------------------------------------------------------------------------
; Utility function to draw exception location

; input:
;	d1.l = Exception offset
;	a0 = label

;	uses a2
; ----------------------------------------------------------------------------

Error_DrawOffsetLocation:
		jsr	Console_Write(pc)				; display label
		; fallthrough

Error_DrawOffsetLocation2:
		pushr.l	d1
		pushr.l	d1
		lea	(sp),a2						; a2 = arguments buffer
		lea	Str_OffsetLocation(pc),a1
		jsr	Console_WriteLine_Formatted(pc)
		addq.w	#8,sp							; free arguments buffer
		rts

; ----------------------------------------------------------------------------
; Subroutine to draw series of registers

; input:
;	d0.w = name of the first register ('d0' or 'a0')
;	d5.w = number of registers
;	a2	 = registers buffer

;	uses d1.l, a0
; ----------------------------------------------------------------------------

sizeof_stringbuffer: = $10

Error_DrawRegisters:
		lea	-sizeof_stringbuffer(sp),sp				; allocate string buffer
		moveq	#-1,d7						; size of the buffer for formatter functions (we assume buffer will never overflow)

	.regloop:
		lea	(sp),a0						; use allocated stack space as string buffer
		move.w	d0,(a0)+						; put register name
		move.w	#': ',(a0)+					; put ": "
		move.b	#pal2,(a0)+					; put palette flag
		move.l	(a2)+,d1
		jsr	FormatHex_LongWord(pc)			; put register contents
		clr.b	(a0)+							; finalize string

		lea	(sp),a0						; use allocated stack space as string buffer
		moveq	#0,d1							; default pattern
		jsr	Console_WriteLine_WithPattern(pc)
		addq.w	#1,d0							; next register name
		dbf	d5,.regloop

		lea	sizeof_stringbuffer(sp),sp

Error_Return:
		rts

; ----------------------------------------------------------------------------
; Subroutine to draw series of registers

; input:
;	d0.l = interrupt handler address
;	a0 = handler name string

;	uses d1.l, a1, a2
; ----------------------------------------------------------------------------

Error_DrawInterruptHandler:
		move.l	d0,d1
		swap	d1
		not.b	d1							; does handler address point to RAM (block $FF)?
		bne.s	Error_Return				; if not, branch

		movea.l	d0,a2						; a2 = handler routine
		cmpi.w	#ins_jmp_abs_long,(a2)+				; does routine include jmp (xxx).l opcode?
		bne.s	.uknown_handler_address		; if not, process "Str_IntHandler_Unknown"
		move.l	(a2),d1					; d1 = interrupt handler offset
		bra.s	Error_DrawOffsetLocation
; ===========================================================================

.uknown_handler_address:
		jsr	Console_Write_Formatted(pc)
		lea	Str_Undefined(pc),a1
		jmp	Console_WriteLine_Formatted(pc)

; ----------------------------------------------------------------------------
; Subroutine to guess caller by inspecting stack

; input:
;	a1 = stack top boundary
;	a2 = stack bottom boundary (after stack frame)

; output:
;	d1.l = caller offset or 0, if not found

; uses a1, a2
; ----------------------------------------------------------------------------

Error_GuessCaller:
		subq.l	#4,a1					; set a final longword to read
		jsr	Error_MaskStackBoundaries(pc)
		cmpa.l	a2,a1
		bcs.s	.nocaller

	.try_offset:
		cmpi.w	#$40,(a2)				; does this seem like an offset?
		bcs.s	.caller_found			; if yes, branch

	.try_next_offset:
		addq.l	#2,a2					; try some next offsets
		cmpa.l	a2,a1
		bcc.s	.try_offset

	.nocaller:
		moveq	#0,d1
		rts
; ===========================================================================

.caller_found:
		move.l	(a2),d1
		beq.s	.try_next_offset		; if offset is zero, branch
		btst	#0,d1					; is this offset even?
		bne.s	.try_next_offset		; if not, branch
		rts

; ----------------------------------------------------------------------------
; Subroutine to setup/reset VDP in order to display properly

; 	uses d0.l
; ----------------------------------------------------------------------------

ErrorHandler_SetupVDP:
		lea (vdp_control_port).l,a5				; a5 = VDP control port
		lea vdp_data_port-vdp_control_port(a5),a6		; a6 = VDP data port

		; Make sure there are no pending writes to VDP (unnecessary in this context)
		tst.w	(a5)

		; Make sure there are no DMAs occuring, otherwise wait
	.wait_dma:
		move.w	(a5),ccr				; is a DMA occuring?
		bvs.s	.wait_dma				;if so,  wait until it's finished

		; Setup VDP registers for Error Handler screen
		lea 	ErrorHandler_VDPConfig(pc),a0

	.setup_regs:
		move.w	(a0)+,d0
		bpl.s	.done
		move.w	d0,(a5)
		bra.s	.setup_regs
; ===========================================================================

	.done:
		; Remove all sprites, reset horizontal and vertical scrolling
		moveq	#0,d0
		vdp_comm.l	move,0,vram,write,(a5)	; reset sprites and horizontal scrolling (HSRAM)
		move.l	d0,(a6)				; ''
		vdp_comm.l	move,0,vsram,write,(a5)	; reset vertical scrolling
		move.l	d0,(a6)				; ''

		; Fill screen with black
		vdp_comm.l	move,0,cram,write,(a5)
		move.w	d0,(a6)

		rts

; ----------------------------------------------------------------------------
; Error screen's VDP configuration
; ----------------------------------------------------------------------------

ErrorHandler_VDPConfig:
		dc.w	vdp_md_color													; $8004; normal color mode, horizontal interrupts disabled
		dc.w 	vdp_enable_vint|vdp_enable_dma|vdp_ntsc_display|vdp_md_display 	; $8134; Mode 5, NTSC, DMA and vertical interrupts enabled, display disabled
		dc.w	vdp_sprite_table						; $8500; sprite attribute table at 0
		dc.w	vdp_bg_color+0							; $8700; BG color (palete line 0 color 0)
		dc.w	vdp_full_vscroll|vdp_full_hscroll		; $8B00; full screen HScroll and VScroll
		dc.w	vdp_320px_screen_width					; $8C81; 320px wide screen mode
		dc.w	vdp_hscroll_table						; $8D00; HScroll table at 0
		dc.w	vdp_auto_inc+2							; $8F02; set VDP auto-increment to 2
		dc.w	vdp_plane_height_64|vdp_plane_width_64	; $9011; 512x512 cell plane size
		dc.w	vdp_window_x_pos						; $9100; reset Window X-position
		dc.w	vdp_window_y_pos						; $9200; reset Window Y-position
		; fallthrough

ErrorHandler_VDPConfig_Nametables:
		dc.w	vdp_fg_nametable+(VRAM_ErrorScreen>>10)	; $8202, set Plane A nametable offset in VRAM
		dc.w	vdp_bg_nametable+(VRAM_ErrorScreen>>13)	; $8404, set Plane B nametable offset in VRAM
		dc.w	0	; terminator

; ----------------------------------------------------------------------------
; Console loading programme for Error Handler
; ----------------------------------------------------------------------------

ErrorHandler_ConsoleConfig:

	; ----------------------------------------------------------------------------
	; Font decompression programme
	; ----------------------------------------------------------------------------
	; NOTICE: It's possible to generate several "samples" of font
	;	with different color indecies at different VRAM locations.
	;	However, this is not used for this Error Handler
	; ----------------------------------------------------------------------------

		vdp_comm.l	dc,VRAM_Font,vram,write	; font offset in VRAM
		dc.w	$0000, $0001, $0010, $0011	; decompression table for 1bpp nibbles
		dc.w	$0100, $0101, $0110, $0111	; ''
		dc.w	$1000, $1001, $1010, $1011	; ''
		dc.w	$1100, $1101, $1110, $1111	; ''

		dc.w	-1							; end marker

	; ----------------------------------------------------------------------------
	; CRAM data
	; ----------------------------------------------------------------------------
	; FORMAT:
	;	dc.w	Color1, ..., ColorN, -X*2
	;		X = Number of longwords to fill until line ends
	;
	; NOTICE: Transparent color at the beginning of a palette line is
	;	auto-filled with $000 (black), hence Color1 is index #1, etc
	;
	; WARNING: Caution is required when calculating -X*2 as it's used
	;	for a jump offset directly in Console_Init code.
	;
	; WARNING: Make sure size of colors you pass (+automatic
	;	transparency color) and fill size sums to $20 bytes strictly!
	;	-- You can only fill with 4 bytes precision!
	;	-- Use dummy colors if neccessary.
	; ----------------------------------------------------------------------------

		dc.w	cWhite,-7*2					; line 0: white text
		dc.w	$00CE,-7*2					; line 1: yellow text
		dc.w	$0EEA,-7*2					; line 2: lighter blue text
		dc.w	$0E86,-7*2					; line 3: darker blue text
		; fallthrough

	; ----------------------------------------------------------------------------
	; Console RAM initial config
	; ----------------------------------------------------------------------------

ErrorHandler_ConsoleConfig_Initial:
		vdp_comm.l	dc,VRAM_ErrorScreen,vram,write	; screen start address / plane nametable pointer
		; fallthrough

ErrorHandler_ConsoleConfig_Shared:
		dc.w	40							; number of characters per line
		dc.w	40							; number of characters on the first line (meant to be the same as the above)
		dc.w	0							; base font pattern (tile id for ASCII $00 + palette flags)
		dc.w	$80							; size of screen row (in bytes)

		dc.w	$2000/$20-1					; size of screen (in tiles - 1)


; ----------------------------------------------------------------------------
; Error Handler interface data
; ----------------------------------------------------------------------------

Str_SetErrorScreen:
		dc.b	pal1,endl,setx,1,setw,38,0

Str_Address:
		dc.b	pal1,'Address: ',0

Str_Offset:
		dc.b	pal1,'Offset: ',0

Str_Caller:
 		dc.b	pal1,'Caller: ',0

Str_OffsetLocation:
		dc.b	pal2,hex|long,' ',pal0,sym|long|split|forced,pal2,symdisp|weak,0

Str_USP:
		dc.b	setx,$10,pal0,'usp: ',pal2,hex|long,0

Str_SR:
		dc.b	setx,$03,pal0,'sr: ',pal2,hex|word,0

Str_VInt:
		dc.b	pal1,'VInt: ',0

Str_HInt:
		dc.b	pal1,'HInt: ',0

Str_Undefined:
		dc.b	pal0,'<undefined>',0
		even

; ----------------------------------------------------------------------------
; Error Handler 1bpp font graphics
; ----------------------------------------------------------------------------

Art1bpp_Font:
	dc.w	Art1bpp_Font_End-Art1bpp_Font_Start-1			; font size - 1

Art1bpp_Font_Start:
	dc.l	$00000000, $00000000, $183C3C18, $18001800, $6C6C6C00, $00000000, $6C6CFE6C, $FE6C6C00
	dc.l	$187EC07C, $06FC1800, $00C60C18, $3060C600, $386C3876, $CCCC7600, $18183000, $00000000
	dc.l	$18306060, $60301800, $60301818, $18306000, $00EE7CFE, $7CEE0000, $0018187E, $18180000
	dc.l	$00000000, $18183000, $000000FE, $00000000, $00000000, $00383800, $060C1830, $60C08000
	dc.l	$7CC6CEDE, $F6E67C00, $18781818, $18187E00, $7CC60C18, $3066FE00, $7CC6063C, $06C67C00
	dc.l	$0C1C3C6C, $FE0C0C00, $FEC0FC06, $06C67C00, $7CC6C0FC, $C6C67C00, $FEC6060C, $18181800
	dc.l	$7CC6C67C, $C6C67C00, $7CC6C67E, $06C67C00, $001C1C00, $001C1C00, $00181800, $00181830
	dc.l	$0C183060, $30180C00, $0000FE00, $00FE0000, $6030180C, $18306000, $7CC6060C, $18001800
	dc.l	$7CC6C6DE, $DCC07E00, $386CC6C6, $FEC6C600, $FC66667C, $6666FC00, $3C66C0C0, $C0663C00
	dc.l	$F86C6666, $666CF800, $FEC2C0F8, $C0C2FE00, $FE62607C, $6060F000, $7CC6C0C0, $DEC67C00
	dc.l	$C6C6C6FE, $C6C6C600, $3C181818, $18183C00, $3C181818, $D8D87000, $C6CCD8F0, $D8CCC600
	dc.l	$F0606060, $6062FE00, $C6EEFED6, $D6C6C600, $C6E6E6F6, $DECEC600, $7CC6C6C6, $C6C67C00
	dc.l	$FC66667C, $6060F000, $7CC6C6C6, $C6D67C06, $FCC6C6FC, $D8CCC600, $7CC6C07C, $06C67C00
	dc.l	$7E5A1818, $18183C00, $C6C6C6C6, $C6C67C00, $C6C6C6C6, $6C381000, $C6C6D6D6, $FEEEC600
	dc.l	$C66C3838, $386CC600, $6666663C, $18183C00, $FE860C18, $3062FE00, $7C606060, $60607C00
	dc.l	$C0603018, $0C060200, $7C0C0C0C, $0C0C7C00, $10386CC6, $00000000, $00000000, $000000FF
	dc.l	$30301800, $00000000, $0000780C, $7CCC7E00, $E0607C66, $6666FC00, $00007CC6, $C0C67C00
	dc.l	$1C0C7CCC, $CCCC7E00, $00007CC6, $FEC07C00, $1C3630FC, $30307800, $000076CE, $C67E067C
	dc.l	$E0607C66, $6666E600, $18003818, $18183C00, $0C001C0C, $0C0CCC78, $E060666C, $786CE600
	dc.l	$18181818, $18181C00, $00006CFE, $D6D6C600, $0000DC66, $66666600, $00007CC6, $C6C67C00
	dc.l	$0000DC66, $667C60F0, $000076CC, $CC7C0C1E, $0000DC66, $6060F000, $00007CC0, $7C067C00
	dc.l	$3030FC30, $30361C00, $0000CCCC, $CCCC7600, $0000C6C6, $6C381000, $0000C6C6, $D6FE6C00
	dc.l	$0000C66C, $386CC600, $0000C6C6, $CE76067C, $0000FC98, $3064FC00, $0E181870, $18180E00
	dc.l	$18181800, $18181800, $7018180E, $18187000, $76DC0000, $00000000

Art1bpp_Font_End:

_ValidHeader = $DEB2

; ----------------------------------------------------------------------------
; Subroutine to find nearest symbol for given offset

; input:
;	d1.l = offset

; output:
;	d0.w = status (0 = ok, -1 = error)
;	d1.l = offset displacement
;	a1 = pointer to compressed symbol text

; 	uses d0.l, d1.l, d2.l, d3.l, a1, a2, a3
; ----------------------------------------------------------------------------

GetSymbolByOffset:
		lea	SymbolData(pc),a1
		cmpi.w	#_ValidHeader,(a1)+	; verify header
		bne.s	.return_error

		moveq	#-2,d0
		add.w	(a1)+,d0				; d0 = (lastBlock+1)*4
		moveq	#-4,d2					; d2 will be 4-byte boundary mask
		moveq	#0,d3					; d3 will be gain value

		swap	d1						; d1 = block
		andi.w	#$FF,d1				; mask higher 8-bits of block id, since MD has 24-bit address bus anyways ...
		add.w	d1,d1					; d1 = block*2
		add.w	d1,d1					; d1 = block*4
		cmp.w	d0,d1					; is the offset's block within [0..lastBlock+1]?
		bhi.s	.return_error			; if not, branch
		beq.s	.load_prev_block		; if strictly lastBlock+1, fetch the previous one ...

	.load_block:
		move.l	(a1,d1.w),d0 			; d0 = relative offset
		beq.s	.load_prev_block		; if block is empty, branch
		lea 	(a1,d0.l),a3			; a3 = Block structure
		swap	d1						; d1 = offset

		moveq	#0,d0
		move.w	(a3)+,d0				; d0 = symbols heap relative offset
		cmp.w	(a3),d1				; compare the requested offset with the lowest in the block
		bcs.s	.load_prev_block_2		; if even lower, find nearest offset in the previous block

		; WARNING: The following instruction assumes blocks will not be reloaded anymore
		lea	-2(a3,d0.l),a1			; a1 = symbols heap
										; d0 = (high - low)
		lea -4(a1),a2 				; a2 = high
										; a3 = low
	.search_loop:
		lsr.w	#1,d0					; 8		; d0 = (high - low) / 2
		and.w	d2,d0					; 4		; find nearest 4-byte struct for the displacement

		cmp.w	(a3,d0.w),d1			; 14	; compare the requested offset with the guessed entry
		bcs.s	.search_lower_half		; 8/10
		bhi.s	.search_higher_half		; 8/10

		adda.w	d0,a3
		bra.s	.load_symbol
; ===========================================================================

	.search_higher_half:
		lea 	4(a3,d0.w),a3			; 12	; limit "low" to "middle"+1 of previously observed area
		move.l	a2,d0					; 4
		sub.l	a3,d0					; 8		; d0 = (high - low)
		bpl.s	.search_loop			; 8/10	; if (low >= high),branch

		subq.w	#4,a3
		bra.s	.load_symbol
; ===========================================================================

	.search_lower_half:
		lea -4(a3,d0.w),a2			; 12	; limit "high" to "middle"-1 of previously observed area
		move.l	a2,d0					; 4		;
		sub.l	a3,d0					; 8		; d0 = (high - low)
		bpl.s	.search_loop			; 8/10	; if (low >= high),branch

		lea	(a2),a3

.load_symbol:
		sub.w	(a3)+,d1				; d1 = displacement
		moveq	#0,d2
		move.w	(a3)+,d2				; d2 = symbol pointer, relative to the heap
		adda.l	d2,a1

		swap	d1						; ''
		; NOTICE: You should be able to access SymbolData+4(pc,d1.w) now ...
		clr.w	d1						; ''
		swap	d1						; andi.l #$FFFF,d1
		add.l	d3,d1
		moveq	#0,d0					; return success
		rts
; ===========================================================================

.return_error:
		moveq	#-1,d0				; return -1
		rts
; ===========================================================================

	.load_prev_block:
		swap	d1

	.load_prev_block_2:
		moveq	#0,d0
		move.w	d1,d0
		add.l	d0,d3				; increase offset gain by the offset within the previous block
		addq.l	#1,d3				; also increase offset gain by 1 to compensate for ...
		move.w	#$FFFF,d1			; ... setting offset to $FFFF instead of $10000
		swap	d1
		subq.w	#4,d1				; decrease block number
		bpl.s	.load_block			; if block is positive, branch
		moveq	#-1,d0				; return -1
		rts

; ----------------------------------------------------------------------------
; Subroutine to decode compressed symbol name to string buffer

; input:
;	a0 = string buffer pointer
;	a1 = pointer to the compressed symbol data
;	a4 = buffer flush function
;	d7.w = number of bytes left in buffer, minus one

; output:
;	(a0)++	ASCII characters for the converted value
;
;	uses  d1.l, d2.l, d3.b, d4.l, a1, a2, a3
; ----------------------------------------------------------------------------

DecodeSymbol:

		lea	SymbolData(pc),a3
		cmpi.w	#_ValidHeader,(a3)+			; verify the header
		bne.s	.return_cc
		adda.w	(a3),a3						; a3 = Huffman code table

		moveq	#0,d4							; d4 will handle byte feeding from bitstream


.decode_new_node:
		moveq	#0,d1							; d1 will hold code
		moveq	#0,d2							; d2 will hold code length (in bits)
		lea	(a3),a2						; a2 will hold current position in the decode table

	.code_extend:
		dbf d4,.stream_ok					; if bits remain in byte, branch
		move.b	(a1)+,d3
		moveq	#7,d4

	.stream_ok:
		add.b	d3,d3							; get a bit from the bitstream ...
		addx.w	d1,d1							; ... add it to current code
		addq.w	#1,d2							; count this bit

	.code_check_loop:
		cmp.w	(a2),d1 						; does this node has the same code?
		bhi.s	.code_check_next				; if not, check next
		bcs.s	.code_extend					; if no nodes are found, branch
		cmp.b	2(a2),d2						; is this code of the same length?
		beq.s	.code_found 					; if not, branch
		bcs.s	.code_extend					; if length is lower, append code

	.code_check_next:
		addq.w	#4,a2
		cmp.w	(a2),d1 						; does this node has the same code?
		bhi.s	.code_check_next				; if not, check next
		bcs.s	.code_extend					; if no nodes are found, branch
		cmp.b	2(a2),d2						; is this code of the same length?
		bcs.s	.code_extend					; if length is lower, append code
		bne.s	.code_check_next

	.code_found:
		move.b	3(a2),(a0)+					; get decoded character
		beq.s	.decode_done					; if it's null character, branch

		dbf	d7,.decode_new_node
		jsr	(a4)
		bcc.s	.decode_new_node
		rts
; ===========================================================================

.decode_done:
		subq.w	#1,a0				; put the last character back
		rts
; ===========================================================================

.return_cc:						; return with carry clear (cc)
		moveq	#0,d0
		rts

; ----------------------------------------------------------------------------
; Hexadecimal number string formatter

; input:
;	d1	= value

;	d7.w = number of bytes left in buffer, minus one
;	a0 = string buffer
;	a4 = buffer flush function

; output:
;	(a0)++	ASCII characters for the converted value
;	carry=0 if buffer is not terminated, carry=1 otherwise.

;	uses d2.l, d3.l, d4.w

; WARNING!
;	1) Formatters can only use registers a3/d0-d4
;	2) Formatters should decrement d7 after each symbol write.
;	3) When d7 decrements below 0, a buffer flush function
;		loaded in a4 should be called. The standard function
;		usually renders buffer's contents on the screen (see
;		"Console_FlushBuffer"), then resets the buffer.
;		This function will reload d7, a0 and carry flag.
;	4) If carry flag is set after calling buffer flush function,
;		formatter should halt all further processing and return,
;		retaining the returned carry bit.
; ----------------------------------------------------------------------------

FormatHex_Handlers:
		jmp	FormatHex_Word(pc)			; $00		; handler for word
		jmp	FormatHex_LongWord(pc)		; $04		; handler for longword
	;	jmp	FormatHex_Byte(pc)			; $08		; handler for byte

FormatHex_Byte:
		moveq	#$F,d3
		move.w	d1,d2
		lsr.w	#4,d2
		and.w	d3,d2						; get nibble
		move.b	HexDigitToChar(pc,d2.w),(a0)+

		dbf	d7,FormatHex_Word_WriteLastNibble
		jsr	(a4)
		bcc.s	FormatHex_Word_WriteLastNibble
		rts		; return carry=1
; ===========================================================================

FormatHex_LongWord:
		swap	d1

FormatHex_LongWord_Swapped:
		bsr.s	FormatHex_Word
		bcs.s	FormatHex_Return			; if buffer terminated, branch

FormatHex_Word_Swapped:
		swap	d1
		;fallthrough


FormatHex_Word:
		moveq	#4,d2
		moveq	#$F,d3

		rept 4-1
		rol.w	d2,d1
		move.b	d1,d4
		and.w	d3,d4						; get nibble
		move.b	HexDigitToChar(pc,d4.w),(a0)+
		dbf	d7,*+6						; if buffer is not exhausted, branch
		jsr	(a4)						; otherwise, call buffer flush function
		bcs.s	FormatHex_Return			; if buffer is terminated, branch
		endr

		rol.w	d2,d1

FormatHex_Word_WriteLastNibble:
		and.w	d3,d1						; get nibble
		move.b	HexDigitToChar(pc,d1.w),(a0)+
		dbf	d7,FormatHex_Return
		jmp	(a4)						; call buffer flush function and return buffer status
; ===========================================================================

FormatHex_Return:
		rts									; return buffer status
; ===========================================================================

HexDigitToChar:
		dc.b	'0123456789ABCDEF'


; ----------------------------------------------------------------------------
; Binary number String formatter

; input:
;	d1	= value

;	d7.w = number of bytes left in buffer, minus one
;	a0 = string buffer
;	a4 = buffer flush function
;
; output:
;	(a0)++	ASCII characters for the converted value
;	carry=0 if buffer is not terminated, carry=1 otherwise.

;	uses d0.l, d2.l

; WARNING!
;	1) Formatters can only use registers a3 / d0-d4
;	2) Formatters should decrement d7 after each symbol write.
;	3) When d7 decrements below 0, a buffer flush function
;		loaded in a4 should be called. The standard function
;		usually renders buffer's contents on the screen (see
;		"Console_FlushBuffer"), then resets the buffer.
;		This function will reload d7, a0 and carry flag.
;	4) If carry flag is set after calling buffer flush function,
;		formatter should halt all further processing and return,
;		retaining the returned carry bit.
; ----------------------------------------------------------------------------

FormatBin_Handlers:
		jmp FormatBin_Word(pc)	 			; $00	Word display handler
		jmp FormatBin_LongWord(pc) 			; $04	Longword display handler
	;	jmp	FormatBin_Byte(pc)				; $08	Byte display handler

;FormatBin_Byte:
		moveq	#8-1,d2

	.loop:
		moveq	#'0'/2,d0
		add.b	d1,d1
		addx.b	d0,d0
		move.b	d0,(a0)+

		dbf	d7,.buffer_ok
		jsr	(a4)
		bcs.s	.quit

	.buffer_ok:
		dbf	d2,.loop

	.quit:
		rts
; ===========================================================================

FormatBin_LongWord:
		swap	d1
		bsr.s	FormatBin_Word
		bcs.s	FormatBin_Return
		swap	d1

FormatBin_Word:
		moveq	#16-1,d2

	.loop:
		moveq	#'0'/2,d0
		add.w	d1,d1
		addx.b	d0,d0
		move.b	d0,(a0)+

		dbf	d7,.buffer_ok
		jsr	(a4)
		bcs.s	FormatBin_Return

	.buffer_ok:
		dbf	d2,.loop

FormatBin_Return:
		rts

; ----------------------------------------------------------------------------
; Decimal number string formatter

; input:
;	d1	= value

;	d7.w = number of bytes left in buffer, minus one
;	a0 = string buffer
;	a4 = buffer flush function
;
; output:
;	(a0)++	ASCII characters for the converted value
;	carry=0 if buffer is not terminated, carry=1 otherwise.

;	uses d2.w, d3.l, d4.w

; WARNING!
;	1) Formatters can only use registers a3 / d0-d4
;	2) Formatters should decrement d7 after each symbol write.
;	3) When d7 decrements below 0, a buffer flush function
;		loaded in a4 should be called. The standard function
;		usually renders buffer's contents on the screen (see
;		"Console_FlushBuffer"), then resets the buffer.
;		This function will reload d7, a0 and carry flag.
;	4) If carry flag is set after calling buffer flush function,
;		formatter should halt all further processing and return,
;		retaining the returned carry bit.
; ----------------------------------------------------------------------------

FormatDec_Handlers:
		jmp FormatDec_Word(pc)	 			; $00	Word display handler
		jmp FormatDec_LongWord(pc) 			; $04	Longword display handler
	;	jmp	FormatDec_Byte(pc)				; $08	Byte display handler

;FormatDec_Byte:
		lea DecimalBase_Byte(pc),a3			; $08	Byte display handler
		andi.w	#$FF,d1								; ...
		bra.s 	FormatDec			 				; ...
; ===========================================================================

FormatDec_Word:
		lea 	DecimalBase_Word(pc),a3

FormatDec:
		clr.b	d0						; d0 will be trim zeros switcher
		moveq	#10-1,d3					; d3 will be DBcc iterator base
		move.w	(a3)+,d4				; d4 = decimal base

FormatDec_Cont:

	.process_digit:
		move.w	d3,d2

	.find_digit:
		sub.w	d4,d1
		dbcs	d2,.find_digit

		add.w	d4,d1							; restore digit
		sub.w	d3,d2
		neg.w	d2								; d2 = digit
		or.b	d2,d0							; have we met non-zero digit yet?
		beq.s	.next_digit						; if not, branch
		add.b	#'0',d2
		move.b	d2,(a0)+

		dbf	d7,.next_digit
		jsr	(a4)
		bcs.s	FormatDec_Return

	.next_digit:
		move.w	(a3)+,d4
		bpl.s	.process_digit

		; The last digit is done manually
		addi.b	#'0',d1
		move.b	d1,(a0)+
		dbf	d7,FormatDec_Return
		jmp	(a4)
; ===========================================================================

FormatDec_Return:
		rts
; ===========================================================================

FormatDec_LongWord:
		lea 	DecimalBase_Long(pc),a3
		clr.b	d0						; d0 will be trim zeros switcher
		moveq	#9,d3					; d3 will be DBcc iterator base
		move.l	(a3)+,d4				; d4 = decimal base

	.process_digit:
		move.w	d3,d2

	.find_digit:
		sub.l	d4,d1
		dbcs	d2,.find_digit

		add.l	d4,d1							; restore digit
		sub.w	d3,d2
		neg.w	d2								; d2 = digit
		or.b	d2,d0							; have we met non-zero digit yet?
		beq.s	.next_digit						; if not, branch
		addi.b	#'0',d2
		move.b	d2,(a0)+

		dbf	d7,.next_digit
		jsr	(a4)
		bcs.s	FormatDec_Return

	.next_digit:
		move.l	(a3)+,d4				; load next decimal base
		bpl.s	.process_digit			; if base is positive, branch
										; otherwise, base is word-sized ...
		bra.s	FormatDec_Cont		; continue drawing with word-sized version
		; note that lower word of d4 already contains next decimal base ...
; ===========================================================================


DecimalBase_Long:
	dc.l	1000000000
	dc.l	100000000
	dc.l	10000000
	dc.l	1000000
	dc.l	100000
	dc.l	10000
	dc.w	-1				; marks switching between 'long' and 'word' modes
	dc.w	1000
	dc.w	100
	dc.w	10
	dc.w	-1				; marks end of digit searching
; ===========================================================================

DecimalBase_Word:
	dc.w	10000
	dc.w	1000
; ===========================================================================

DecimalBase_Byte:
	dc.w	100
	dc.w	10
	dc.w	-1				; marks end of digit searching

; ----------------------------------------------------------------------------
; Symbol String formatter

; input:
;	d1	= value

;	d7.w = number of bytes left in buffer, minus one
;	a0 = string buffer
;	a4 = buffer flush function
;
; output:
;	(a0)++	ASCII characters for the converted value
;	carry=0 if buffer is not terminated, carry=1 otherwise.
;
; WARNING!
;	1) Formatters can only use registers a3 / d0-d4
;	2) Formatters should decrement d7 after each symbol write.
;	3) When d7 decrements below 0, a buffer flush function
;		loaded in a4 should be called. The standard function
;		usually renders buffer's contents on the screen (see
;		"Console_FlushBuffer"), then resets the buffer.
;		This function will reload d7, a0 and carry flag.
;	4) If carry flag is set after calling buffer flush function,
;		formatter should halt all further processing and return,
;		retaining the returned carry bit.
; ----------------------------------------------------------------------------

FormatSym_Handlers:
		ext.l	d1							; $00		; handler for word
		bra.s	FormatSym					; $02
; ===========================================================================

		jmp	FormatSym(pc)				; $04		; handler for longword
; ===========================================================================

		ext.w	d1							; $08		; handler for byte
		ext.l	d1

FormatSym:
		pushr.l	d1/d3/a1-a2
		jsr	GetSymbolByOffset(pc)			; IN:	d1 = offset
		bne.s	FormatSym_ChkUnknownSymbol		; OUT:	d0/Z = error status, d1 = displacement, a1 = symbol pointer
		move.l	d1,(sp)						; replace offset stored in stack as D1 with displacement
		jsr	DecodeSymbol(pc)				; IN:	a1 = symbol pointer
		popr.l	d1/d3/a1-a2						; NOTICE: This doesn't affect CCR, so this routine still returns carry
		bcs.s	FormatSym_Return				; if got carry (buffer termination), return immediately

FormatSym_ChkDrawDisplacement:
		btst	#split_bit,d3							; is "display just label part so far" bit set?
		bne.s	FormatSym_Return				; if yes, branch (Z=0 or C=1)
		jmp	FormatString_CodeHandlers+$40(pc); otherwise, also display displacement now
; ===========================================================================

FormatSym_Return:
		rts
; ===========================================================================

FormatSym_ChkUnknownSymbol:
		popr.l	d1/d3/a1-a2
  		btst	#forced_bit,d3							; is "draw <unknown> on error" bit set?
		beq.s	FormatSym_ReturnNC				; if not, branch
		lea	FormatSym_Str_Unknown(pc),a3
		jmp	FormatString_CodeHandlers+$52(pc)	; jump to code D0 (string) handler, but skip instruction that sets A3
; ===========================================================================

FormatSym_ReturnNC:
		moveq	#-1,d0							; reset carry, keep D0 an error code
		bra.s	FormatSym_ChkDrawDisplacement
; ===========================================================================

FormatSym_Str_Unknown:
		dc.b	'<unknown>',0
		even

; ----------------------------------------------------------------------------
; input:
;	d1.l = displacement
; ----------------------------------------------------------------------------

FormatSym_Displacement:
		move.b	#'+',(a0)+
		dbf	d7,.buffer_ok
		jsr	(a4)
		bcs.s	FormatSym_Return

	.buffer_ok:
		swap	d1								; swap displacement longword
		tst.w	d1								; test higher 16-bits of displacement
		beq.w		FormatHex_Word_Swapped			; if bits are empty, display displacement as word
		bra.w		FormatHex_LongWord_Swapped		; otherwise, display longword

; ----------------------------------------------------------------------------
; input:
;	d1.l = offset
;	d3.b = control byte
; ----------------------------------------------------------------------------

FormatSym_Offset:
		btst	#weak_bit,d3							; is "don't draw offset" flag set?
		bne.s	FormatSym_Return				; WARNING: Should return NC
		jmp	FormatHex_LongWord(pc)

; ----------------------------------------------------------------------------
; String formatter module

; input:
;	a0	= pointer to a string buffer
;	a1	= pointer to format sequence
;	a2	= pointer to arguments list
;	a4	= buffer flush function
;	d7.w = number of bytes left in buffer, minus one

;	uses d2.w, d3.b, a0, a1, a2
; ----------------------------------------------------------------------------

FormatString:
		pushr.l	d0-d4/a3,-(sp)

	; NOTICE: This loop shouldn't use registers D0/D1, as control codes B0..BF, C0..CF
	;	that are executed consequently use it to pass parameters inbetween.
	.copy_loop:
		move.b	(a1)+,(a0)+
		dble	d7,.copy_loop				; if character's code is below $80 and not $00, copy string ...
		bgt.s	.flush
		beq.s	.quit						; if char $00 was fetched, quit

	.flag:
		; Process special character
		move.b	-(a0),d3					; d3 = special character that was pushed out of the string
		moveq	#$70,d2						; d2 = $00, $10, $20, $30, $40, $60, $70
		and.b	d3,d2						; d2 = code offset based on character's code, aligned on $10-byte boundary
		jsr	FormatString_CodeHandlers(pc,d2.w)	; jump to an appropriate special character handler
		bcc.s	.copy_loop					; if string buffer is good, branch

	.quit_no_flush:
		popr.l	d0-d4/a3
		rts
; ===========================================================================

	.flush:
		jsr	(a4)						; flush buffer
		bcc.s	.copy_loop					; if flushing was ok, branch
		bra.s	.quit_no_flush
; ===========================================================================

	.quit:
		subq.w	#1,a0		; because D7 wasn't decremented?
		jsr	(a4)							; call flush buffer function
		popr.l	d0-d4/a3
		rts
; ===========================================================================

FormatString_CodeHandlers:
		; codes 80..8F : Display hex number
		lea	FormatHex_Handlers(pc),a3			; $00
		eor.b	d3,d2								; $04	; d2 = lower 4 bits of char code, encodes argument size (valid values are: 0, 1, 3, see below)
		add.b	d2,d2								; $06	; multiply 4-bit code by 2 as instructions in the code handlers below are word-sized
		jmp	.argument_fetch(pc,d2.w)			; $08	; jump to an appropriate insturction (note that even invalid codes won't crash)
; ===========================================================================
		nop											; $0C
		nop											; $0E
		; codes 90..9F : Display decimal number
		lea	FormatDec_Handlers(pc),a3			; $00
		eor.b	d3,d2								; $04	; d2 = lower 4 bits of char code, encodes argument size (valid values are: 0, 1, 3, see below)
		add.b	d2,d2								; $06	; multiply 4-bit code by 2 as instructions in the code handlers below are word-sized
		jmp	.argument_fetch(pc,d2.w)			; $08	; jump to an appropriate insturction (note that even invalid codes won't crash)
; ===========================================================================
		nop											; $0C
		nop											; $0E
		; codes A0..AF : Display binary number
		lea	FormatBin_Handlers(pc),a3			; $00
		eor.b	d3,d2								; $04	; d2 = lower 4 bits of char code, encodes argument size (valid values are: 0, 1, 3, see below)
		add.b	d2,d2								; $06	; multiply 4-bit code by 2 as instructions in the code handlers below are word-sized
		jmp	.argument_fetch(pc,d2.w)			; $08	; jump to an appropriate instruction (note that even invalid codes won't crash)
; ===========================================================================
	.null_terminator:
		subq.w	#1,a0								; $0C	; overwrite null-terminator (part of "String" section, see below)
		rts											; $0E
; ===========================================================================
		; codes B0..BF : Display symbol
		lea	FormatSym_Handlers(pc),a3			; $00
		move.b	d3,d2								; $04
		andi.w	#3,d2								; $06	; d2 = 0, 1, 3 ... (ignore handlers for signed values)
		add.w	d2,d2								; $0A	; multiply 4-bit code by 2 as instructions in the code handlers below are word-sized
		jmp	.argument_fetch(pc,d2.w)			; $0C	; jump to an appropriate instruction (note that even invalid codes won't crash)
; ===========================================================================
		; codes C0..CF : Display symbol's displacement (to be used after codes B0..BF, if extra formatting is due)
		tst.w	d0									; $00	; check "GetSymbolByOffset" (see "FormatSym" code)
		bmi.s	.display_offset									; $02	; if return code is -1 (error), assume d1 is OFFSET, display it directly
		tst.l	d1									; $04	; assume d1 is DISPLACEMENT, test it
		beq.s	.return2							; $06	; if displacement is zero, branch
		jmp	FormatSym_Displacement(pc)			; $08
; ===========================================================================
	.display_offset:
		jmp	FormatSym_Offset(pc)				; $0C
; ===========================================================================
		; codes D0..DF : String
		movea.l	(a2)+,a3						; $00	; a3 = string ptr

	.string_loop:
		move.b	(a3)+,(a0)+						; $02	; copy char
		dbeq	d7,.string_loop					; $04	; loop until either buffer ends or zero-terminator is met
		beq.s	.null_terminator				; $08	; if met zero-terminator, branch
		jsr	(a4)								; $0A	; flush buffer
		bcc.s	.string_loop					; $0C	; if buffer is ok, branch

	 .return2:
		rts										; $0E	; return C
; ===========================================================================

		; codes E0..EF : Drawing command (ignore)
		addq.w	#1,a0								; $00	; restore control character back
		bra.s	.after_restore_char				; $02
; ===========================================================================

	; NOTICE: Code handlers continue below and overlap with the following code ...

; --------------------------------------------------------------
; WARNING!
; The code in the following blocks are critical and shouldn't
; be altered. Each instruction MUST take 2 bytes, so even the
; invalid codes won't crash, but only break the flow ...
; --------------------------------------------------------------

.argument_fetch:
		addq.w	#8,a3							; $00 :$04	; code 0 : Display byte
		move.w	(a2)+,d1						; $02 :$06	; code 1 : Display word
		jmp	(a3)							; $04 :$08	; code 2 : ## invalid : displays garbage word
; ===========================================================================
		addq.w	#4,a3							; $06 :$0A	; code 3 : Display longword
		move.l	(a2)+,d1						; $08 :$0C	; code 4 : ## invalid ##: displays word, but loads longword
		jmp	(a3)							; $0A :$0E	; code 5 : ## invalid ##: displays garbage word
; ===========================================================================
		; codes F0..FF : Drawing command, one-byte argument (ignore)
		addq.w	#1,a0							; $0C :$00	; code 6 : ## invalid ##: restores control character and puts another one
		bra.s	.after_restore_char2			; $0E :$02	; code 7 : ## invalid ##: does nothing
; ===========================================================================
		addq.w	#8,a3							; $10		; code 8 : Display signed byte
		move.w	(a2)+,d1						; $12		; code 9 : Display signed word
		bra.s	.checksign					; $14		; code A : ## invalid ##: displays garbage signed word
; ===========================================================================
		addq.w	#4,a3							; $16		; code B : Display signed longword
		move.l	(a2)+,d1						; $18		; code C : ## invalid ##: displays signed word, but loads longword
; ===========================================================================
.checksign:
		bpl.s	.positive					; $1A		; code D : ## invalid ##: displays garbage signed word
		neg.l	d1								; $1C		; code E : ## invalid ##: displays gargage pseudo-negative word
		move.b	#'-',(a0)+						; $1E		; code F : ## invalid ##: displays gargage pseudo-non-negative word
		bra.s	.aftersendsign
; ===========================================================================

.positive:
		move.b	#'+',(a0)+

.aftersendsign:
		dbf	d7,.sign_ok					; if there are characters left in the buffer, branch
		jsr	(a4)							; call buffer flush function
		bcs.s	.return2						; if there's no space left in buffer, quit

.sign_ok:
		jmp	(a3)							; draw the actual value using an appropriate handler
; ===========================================================================

.after_restore_char2:
		dbf	d7,.after_restore_char3
		jsr	(a4)
		bcs.s	.return2

.after_restore_char3:
		move.b	(a1)+,(a0)+

.after_restore_char:
		dbf	d7,.return2
		jmp	(a4)

; ===========================================================================
; Console Module
; ----------------------------------------------------------------------------
; Subroutine to initialize the console system

; input:
;	a1 = console config
;	a2 = console font graphics (1bpp)
;	a3 = console RAM pointer

; output:
;	d5.l	current on-screen position

;	uses d0.l, d1.l, d2.w, d3.l, d4.w, a0, a5, a6
; ----------------------------------------------------------------------------

Console_Init:
		lea	vdp_control_port,a5
		lea	vdp_data_port-vdp_control_port(a5),a6

	; Load console font
	.font_prg_loop:
		tst.w	(a1)
		bmi.s	.font_done
		move.l	(a1)+,(a5)				; VDP => Setup font offset in VRAM
		lea	(a2),a0				; load font offset
		move.w	(a0)+,d4				; load font size - 1
		jsr	Decomp1bpp(pc)			; decompress font (input: a0-a1/a6, uses: a0/d0-d4)
		lea	$20(a1),a1
		bra.s	.font_prg_loop
; ===========================================================================

.font_done:
		addq.w	#2,a1					; skip end marker

		; Load palette
		lea	Console_FillTile(pc),a0
		vdp_comm.l	move,0,cram,write,(a5)	; VDP => Setup CRAM write at offset $00
		moveq	#cBlack,d0					; d0 = black color
		moveq	#4-1,d3				; d3 = number of palette lines - 1

	.fill_palette_line:
		move.w	d0,(a6)			; transparent color is always black
		move.w	(a1)+,d2			; get CRAM data entry

	.iscolor:
		move.w	d2,(a6)			; write to CRAM
		move.w	(a1)+,d2			; get next CRAM data entry
		bpl.s	.iscolor					; if color, branch

		moveq	#0,d1
		jsr	$10(a0,d2.w)			; fill the rest of the line by a clever jump (WARNING! Precision required!)
		dbf	d3,.fill_palette_line
	; fallthrough

; ----------------------------------------------------------------------------
; Subroutine to clear and reset console to the initial config

; input:
;	a1 = initial console config
;	a3 = console RAM pointer
;	a5 = VDP control port ($C00004)
;	a6 = VDP data port ($C00000)

; output:
;	d5.l	Current on-screen position

;	uses d0.l, d1.l, a1, a3
; ----------------------------------------------------------------------------

Console_Reset:
		move.l	(a1)+,d5				; d5 = VDP command with start on-screen position
		; fallthrough

; ----------------------------------------------------------------------------
; A shorter initialization sequence used by sub-consoles sharing
; the same palette and graphics, but using a different nametable

; input:
;	a1 = initial console config
;	a3 = console RAM pointer
;	a5 = VDP control port ($C00004)
;	a6 = VDP data port ($C00000)
;	d5.l = VDP command with start on-screen position

; output:
;	d5.l	Current on-screen position

;	uses d0.l, d1.l, a1, a3
; ----------------------------------------------------------------------------

Console_InitShared:
		; WARNING! Make sure a5 and a6 are properly set when calling this fragment separately

		; Init Console RAM
		move.l	a3,usp					; remember Console RAM pointer in USP to restore it in later calls
		move.l	d5,(a3)+				; Console RAM => copy screen position (long)
		move.l	(a1)+,(a3)+			; Console RAM => copy number of characters per line (word) + characters remaining for the current line (word)
		move.l	(a1)+,(a3)+			; Console RAM => copy base pattern (word) + screen row size (word)
		move.w	#_ConsoleMagic<<8,(a3)+; Console RAM => set magic number, clear reserved byte

		; Clear screen
		move.l	d5,(a5)				; VDP => Setup VRAM for screen namespace
		moveq	#0,d0					; d0 = fill pattern
		move.w	(a1)+,d1				; d1 = size of screen in tiles - 1
		bsr.s	Console_FillTile		; fill screen

		vdp_comm.l	move,0,vram,write,(a5)	; VDP => Setup VRAM at tile 0
		;moveq	#0,d0					; d0 = fill pattern		-- OPTIMIZED OUT
		moveq	#0,d1					; d1 = number of tiles to fill - 1
		bsr.s	Console_FillTile		; clear first tile

		; Finalize
		move.w	#vdp_md_display|vdp_enable_vint|vdp_enable_dma|vdp_enable_display,(a5)			; VDP => enable display
		move.l	d5,(a5)				; VDP => enable console for writing
		rts
; ===========================================================================

Console_FillTile:
		rept 8
		move.l	d0,(a6)
		endr
		dbf	d1,Console_FillTile
		rts

; ----------------------------------------------------------------------------
; Subroutine to set console cursor position based on XY coordinates

; input:
;	d0.w = x pos
;	d1.w = y pos
; ----------------------------------------------------------------------------

Console_SetPosAsXY_Stack:
		movem.w	4(sp),d0-d1

Console_SetPosAsXY:
		pushr.l	d1-d2/a3
		move.l	usp,a3
		cmpi.b	#_ConsoleMagic,Console_Magic(a3)
		bne.s	.quit

		move.w	(a3),d2
		andi.w	#$E000,d2				; clear out displacement, leave base offset only
		mulu.w	Console_ScreenRowSz(a3),d1
		add.w	d1,d2
		add.w	d0,d2
		add.w	d0,d2
		move.w	d2,(a3)
		move.l	(a3)+,(vdp_control_port).l

		move.w	(a3)+,(a3)+			; reset remaining characters counter

	.quit:
		popr.l	d1-d2/a3
		rts

; ----------------------------------------------------------------------------
; Subroutine to get current line position in XY-coordinates

; output:
;	d0.w = x pos
;	d1.w = y pos
; ----------------------------------------------------------------------------

Console_GetPosAsXY:
		pushr.l	a3
		move.l	usp,a3
		cmpi.b	#_ConsoleMagic,Console_Magic(a3)
		bne.s	.quit
		moveq	#0,d1
		move.w	(a3),d1
		andi.w	#$1FFF,d1						; clear out base offset, leave displacement only
		divu.w	Console_ScreenRowSz(a3),d1		; d1 = row
		move.l	d1,d0
		swap	d0
		lsr.w	d0

	.quit:
		popr.l	a3
		rts

; ----------------------------------------------------------------------------
; Subroutine to transfer console to a new line
; ----------------------------------------------------------------------------

Console_StartNewLine:
		pushr.l	a3
		move.l	usp,a3
		cmpi.b	#_ConsoleMagic,Console_Magic(a3)
		bne.s	.quit

		pushr.w	d0
		move.w	(a3),d0
		add.w	Console_ScreenRowSz(a3),d0
		; TODO: Check if offset is out of plane boundaries
		andi.w	#$5FFF,d0			; make sure line stays within plane
		move.w	d0,(a3)			; save new position
		move.l	(a3)+,(vdp_control_port).l
		move.w	(a3)+,(a3)+		; reset characters on line counter (copy "CharsPerLine" to "CharsRemaining")

		popr.w	d0

	.quit:
		popr.l	a3
		rts

; ----------------------------------------------------------------------------
; Subroutine to set console's base pattern

; input:
;	d1.w = base pattern
; ----------------------------------------------------------------------------

Console_SetBasePattern:
		pushr.l	a3
		move.l	usp,a3
		cmpi.b	#_ConsoleMagic,Console_Magic(a3)
		bne.s	.quit
		move.w	d1,Console_BasePattern(a3)

	.quit:
		popr.l	a3
		rts

; ----------------------------------------------------------------------------
; Subroutine to set console's width

; input:
;	d1.w = width
; ----------------------------------------------------------------------------

Console_SetWidth:
		pushr.l	a3
		move.l	usp,a3
		cmpi.b	#_ConsoleMagic,Console_Magic(a3)
		bne.s	.quit
		addq.w	#4,a3
		move.w	d1,(a3)+
		move.w	d1,(a3)+

	.quit:
		popr.l	a3
		rts

; ----------------------------------------------------------------------------
; Subroutine to draw string on screen

; input:
;	a0 = pointer to null-terminated string
;	d1.w = base pattern (*_WriteLine_WithPattern only)

; output:
;	a0	= pointer to the end of string
; ----------------------------------------------------------------------------

Console_WriteLine_WithPattern:
		bsr.s	Console_SetBasePattern	; set new base pattern

Console_WriteLine:
		pea	Console_StartNewLine(pc)	; start new line before returning to caller

Console_Write:
		pushr.l	d1-d6/a3/a6
		move.l	usp,a3
		cmpi.b	#_ConsoleMagic,Console_Magic(a3)
		bne.s	.quit

		; Load console variables
		move.l	(a3)+,d5			; d5 = VDP screen position request
		movem.w	(a3),d2-d4/d6		; number of characters per line, number of characters remaining until next line, base pattern, screen position increment value
		swap	d6
		lea	(vdp_data_port).l,a6		; a6 = VDP data port

		; First iteration in .loop, unrolled
		moveq	#0,d1
		move.b	(a0)+,d1			; load first char
		bgt.s	.loop				; if not a null-terminator or flag, branch
		bmi.s	.flag				; if char is a flag, branch

	.done:
		movem.w	d2-d4,(a3)			; save d2-d4 (ignore d6 as it won't get changed anyways ...)
		move.l	d5,-(a3)			; save screen position

	.quit:
		popr.l	d1-d6/a3/a6
		rts
; ===========================================================================

	.loop:
		dbf	d3,.writechar
		add.w	d2,d3				; restore number of characters per line
		add.l	d6,d5
		bclr	#29,d5
		move.l	d5,vdp_control_port-vdp_data_port(a6)			; setup screen position

	.writechar:
		add.w	d4,d1  			; add base pattern
		move.w	d1,(a6)			; draw

	.nextchar:
		moveq	#0,d1
		move.b	(a0)+,d1			; load next char
		bgt.s	.loop				; if not a null-terminator or flag, branch
		beq.s	.done				; if null-terminator, branch

	; Process drawing flag
.flag:
		andi.w	#$1E,d1					; d2 = $00, $02, $04, $06, $08, $0A, $0C, $0E, $10, $12, $14, $16, $18, $1A, $1C, $1E
		jmp	.command_handlers(pc,d1.w)
; ===========================================================================

.command_handlers:
		; For flags E0-EF (no arguments)
		add.l	d6,d5						; $00	; codes E0-E1 : start a new line
		moveq	#29,d1 					; $02	; codes E2-E3 : <<UNUSED>>
		bclr	d1,d5						; $04	; codes E4-E5 : <<UNUSED>>
		bra.s	.reset_line					; $06	; codes E6-E7 : reset position to the beginning of line
; ===========================================================================

		bra.s	.set_palette_line_0			; $08	; codes E8-E9 : set palette line #0
; ===========================================================================
		bra.s	.set_palette_line_1			; $0A	; codes EA-EB : set palette line #1
; ===========================================================================

		bra.s	.set_palette_line_2			; $0C	; codes EC-ED : set palette line #2
; ===========================================================================

		bra.s	.set_palette_line_3			; $0E	; codes EE-EF : set palette line #3
; ===========================================================================
		; For flags F0-FF (one-byte arguments)
		move.b	(a0)+,d2					; $10	; codes F0-F1 : set characters per line, reset line
		bra.s	.reset_line					; $12	; codes F2-F3 : <<UNUSED>>
; ===========================================================================

		move.b	(a0)+,d4					; $14	; codes F4-F5 : set low byte of base pattern (raw)
		bra.s	.nextchar					; $16	; codes F6-F7 : <<UNUSED>>
; ===========================================================================

		bra.s	.set_base_pattern_high_byte	; $18	; codes F8-F9 : set high byte of base pattern (raw)
; ===========================================================================

		move.b	(a0)+,d1					; $1A	; codes FA-FB : set x-position
		add.w	d1,d1						; $1C	; codes FC-FD : <<UNUSED>>
		moveq	#-$80,d3					; $1E	; codes FE-FF : <<UNUSED>>
		swap	d3							;
		and.l	d3,d5						;
		swap	d1							;
		or.l	d1,d5						;
;		bra.s	.reset_line					; restore d3 anyways, as it's corrupted

.reset_line:
		move.w	d2,d3
		move.l	d5,vdp_control_port-vdp_data_port(a6)
		bra.s	.nextchar
; ===========================================================================

.set_palette_line_0:
		andi.w	#$7FF,d4
		bra.s	.nextchar
; ===========================================================================

.set_palette_line_1:
		andi.w	#$7FF,d4
		ori.w	#$2000,d4
		bra.s	.nextchar
; ===========================================================================

.set_palette_line_2:
		andi.w	#$7FF,d4
		ori.w	#$4000,d4
		bra.s	.nextchar
; ===========================================================================

.set_palette_line_3:
		ori.w	#$6000,d4
		bra.s	.nextchar
; ===========================================================================

.set_base_pattern_high_byte:
		pushr.w	d4
		move.b	(a0)+,(sp)
		popr.w	d4
		bra.s	.nextchar

; ----------------------------------------------------------------------------
; Subroutine to write formatted strings

; input:
;	a1 = pointer to source formatted string
;	a2 = arguments buffer pointer

;	uses d7.l, a0, a1, a2
; ----------------------------------------------------------------------------

Console_WriteLine_Formatted:
		pea		Console_StartNewLine(pc)	; start new line before returning to caller

Console_Write_Formatted:

sizeof_stringbuffer: = $10

		pushr.l	a4

		lea	Console_FlushBuffer(pc),a4		; flushing function
		lea	-sizeof_stringbuffer(sp),sp		; allocate string buffer
		lea	(sp),a0					; a0 = string buffer

		moveq	#sizeof_stringbuffer-2,d7			; d7 = number of characters before flush -1
		jsr	FormatString(pc)
		lea	sizeof_stringbuffer(sp),sp		; free string buffer

		popr.l	a4
		rts

; ----------------------------------------------------------------------------
; Flush buffer callback raised by FormatString

; input:
;	a0 = buffer position
;	d7.w = number of characters remaining in buffer - 1

; output:
;	a0 = buffer position after flushing
;	d7.w = number of characters before next flush - 1
;	carry	0 = continue operation
;			1 = terminate FormatString with error condition

; WARNING: This function shouldn't modify d0-d4 / a1-a3!
; ----------------------------------------------------------------------------

Console_FlushBuffer:
		clr.b	(a0)+					; finalize buffer

		neg.w	d7
		addi.w	#sizeof_stringbuffer-1,d7
		suba.w	d7,a0					; a0 = start of the buffer

		pushr.l	a0,-(sp)
		jsr	Console_Write(pc)		; call the real flush function
		popr.l	a0
		moveq	#sizeof_stringbuffer-2,d7		; d7 = number of characters before flush -1
		rts								; WARNING! Must return carry=0

; ----------------------------------------------------------------------------
; Fast 1bpp decompressor

; input:
;	a0 = source 1bpp art
;	a1 = decode table (generated or manual)
;	a6 = VDP Data Port
;	d4.w = size of art in bytes - 1

;	uses d0.w, d1.w, d2.w, d4.w, a0
; ----------------------------------------------------------------------------

Decomp1bpp:
		moveq	#$1E,d2

	.row:
		move.b	(a0)+,d0				; d0 = %aaaa bbbb
		move.b	d0,d1
		lsr.b	#3,d1					; d1 = %000a aaab
		and.w	d2,d1					; d1 = %000a aaa0
		move.w	(a1,d1.w),(a6)			; decompress first nibble

		add.b	d0,d0					; d0 = %aaab bbb0
		and.w	d2,d0					; d0 = %000b bbb0
		move.w	(a1,d0.w),(a6)			; decompress second nibble

		dbf	d4,.row

		rts
; ===========================================================================

SymbolData:
