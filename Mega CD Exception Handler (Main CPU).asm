; ---------------------------------------------------------------
; Mega CD Error Handler - Main CPU module

; ---------------------------------------------------------------
; Original code by Vladikcomper 2016-2017
; Modified by Orion Navattan 2023

; Must be placed at the very end of the ROM for symbol table 
; support, but otherwise can be located anywhere in the ROM.
; ---------------------------------------------------------------
; Exception entry points
; ---------------------------------------------------------------

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


; ---------------------------------------------------------------
; Macro definitions
; ---------------------------------------------------------------
; Generate VRAM write command
vram	macro
			if (narg=1)
					move.l	#($40000000+((\1&$3FFF)<<16)+((\1&$C000)>>14)),($C00004).l
			else
					move.l	#($40000000+((\1&$3FFF)<<16)+((\1&$C000)>>14)),\2
			endc
			endm

; Generate dc.l constant with VRAM write command
dcvram	macro
			dc.l	($40000000+((\1&$3FFF)<<16)+((\1&$C000)>>14))
			endm


; Generate CRAM write command
cram	macro	offset,operand
			if (narg=1)
					move.l	#($C0000000+(\1<<16)),(vdp_control_port).l
			else
					move.l	#($C0000000+(\1<<16)),\operand
			endc
			endm
			
; Special macro to define externally visible symbols
__global macro	*
__global__\*:
\*:
			endm


; ---------------------------------------------------------------
; Constants
; ---------------------------------------------------------------

VRAM_Font		equ 	(('!'-1)*$20)
VRAM_PlaneA 	equ		$8000
VRAM_PlaneB 	equ		VRAM_PlaneA

_white			equ 	0
_yellow 		equ 	1<<13
_blue			equ 	2<<13
_blue2			equ 	3<<13

sizeof_dumpedregs:		equ 4*(8+7)	; $3C

; ---------------------------------------------------------------
; Main CPU handler for sub CPU exceptions. Entered by means of 
; one of the trap vectors when it detects that the sub CPU has
; crashed.

; Nearly the same as that used for processing main
; CPU exceptions, except it is reading the exception arguments,
; register dump and stack frame from the sub CPU's stack.

; This assumes that the initial stack set by the BIOS is
; still being used, and that the stack is in the first program
; RAM bank (which it is by default). Both Mode 1 and Mode 2 should
; be supported; the only change being the value of 'program_ram'.

; GLOBAL REGISTERS:
;		d6.b	Error handler flags bitfield
;		a3		Pointer to additional parameters
;		a4		sub CPU stack pointer (after exception frame)
; ---------------------------------------------------------------
	
SubCPUError:
		disable_ints				; disable interrupts
		move.b	(mcd_sub_flag).l,(mcd_main_flag).l	; let sub CPU know we've noticed

	.waitsub:
		tst.b	(mcd_sub_flag).l	; is the sub CPU done?
		bne.s	.waitsub			; if not, branch	
		
	.waitsubbus:	
		bset	#sub_bus_request_bit,(mcd_reset).l			; request the sub CPU bus
		beq.s	.waitsubbus					; if it has not been granted, wait
		
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
		
		move.l a4,-(sp)	; back up sub CPU stack bottom address for later
		
		lea sizeof_dumpedregs+4(a4),a4	; a4 = arguments and exception stack frame 

		; Print error description
		movea.l	(a4)+,a1						; get error text pointer
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

		; Print error address (for address error only)
		btst	#extended_frame_bit,d6							; does error has extended stack frame (Address Error only)?
		beq.s	.skip1							; if not, branch

		lea	Str_Address(pc),a1					; a1 = formatted string
		lea	2(a4),a2							; a2 = arguments buffer
		jsr	Console_WriteLine_Formatted(pc)
		addq.w	#8,a4							; skip extension part of the stack frame				
		
	.skip1:	
		; Print error location
		lea	Str_Location(pc),a1				; a1 = formatted string
		lea	2(a4),a2						; a2 = arguments buffer
		jsr	Console_WriteLine_Formatted(pc)

		; Print module name error occurred in
		lea	Str_Module(pc),a1				; a1 = formatted string
		lea	2(a4),a2						; a2 = arguments buffer
		jsr	Console_WriteLine_Formatted(pc)

		; Print caller
		movea.l	(program_ram),a1			; a1 = sub CPU initial stack pointer value
		adda.l	#program_ram,a1				; make into main CPU address		
		lea	6(a4),a2						; a2 = call stack (after exception stack frame)
		jsr	Error_MaskStackBoundaries(pc)

		jsr	Error_GuessCaller(pc)			; d1 = caller
		lea	Str_Caller(pc),a1				; a1 = formatted string
		move.l	d1,-(sp)
		lea	(sp),a2						; a2 = arguments buffer
		jsr	Console_WriteLine_Formatted(pc)
		jsr	Console_StartNewLine(pc)
		addq.w	#4,sp							; free argument
	
		movea.l (sp)+,a4							; restore sub CPU stack bottom address
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
		sub.l	#program_ram,d4		; convert to sub CPU address	
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
		btst	#0,d6							; does error has extended stack frame (Address Error only)?
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

		movea.l (program_ram).l,a1		; get sub CPU initial stack pointer
		adda.l	#program_ram,a1				; make into main CPU address
		
	
		lea	sizeof_dumpedregs+4+4(a4),a2							; a2 = top of exception frame
		;subq.l	#1,a1							; unnecessary here as sub CPU stack top can never be zero if BIOS is used
		btst	#extended_frame_bit,d6							; does error has extended stack frame (Address Error only)?
		beq.s	.notaddrerr2							; if not, branch	
		addq.w	#8,a2					; skip extended frame
	
	.notaddrerr2:				
		bsr.w	Error_MaskStackBoundaries

		jsr		Console_GetPosAsXY(pc)			; d0/d1 = XY-pos
		moveq	#28-3,d5
		sub.w	d1,d5
		bmi.s	.stack_done

		bsr.w	Error_DrawStackRow_First

	.stack_loop:
		jsr	Error_DrawStackRow(pc)
		dbf	d5,.stack_loop

	.stack_done:
		bra.w	Error_IdleLoop
		
		
		
; ---------------------------------------------------------------
; Main error handler
; ---------------------------------------------------------------
; GLOBAL REGISTERS:
;		d6	.b	Error handler flags bitfield
;		a3		Pointer to additional parameters
;		a4		Stack pointer (after exception frame)
;
; NOTE:	It should be called via JSR/BSR exclusively with error
;		information following the JSR/BSR opcode.
;
; ERROR DATA FORMAT:
;		dc.b	"<Error formatted message>", 0
;		dc.b	<Error Handler flags>
;		even
;		jmp		<ConsoleProgram> (optional)
;
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
; ---------------------------------------------------------------

ErrorHandler:
		disable_ints							; disable interrupts for good

		; Halt the sub CPU.
	.waitsubbus:	
		bset	#sub_bus_request_bit,(mcd_reset).l			; request the sub CPU bus
	
		; Beyond here is unmodified from original.
				
		lea	-sizeof_Console_RAM(sp),sp		; allocate memory for console on stack
		movem.l	d0-a6,-(sp) 							; dump all registers

		jsr	ErrorHandler_SetupVDP(pc)
		lea	sizeof_dumpedregs+sizeof_Console_RAM(sp),a4		; a4 = arguments and exception stack frame

		move.l	usp,a0
		move.l	a0,-(sp)								; save USP if needed to display later (as it's overwritten by the console subsystem)

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
		lea	(a4),a2								; a2 = load arguments buffer (if present)
		jsr	Console_WriteLine_Formatted(pc)
		jsr	Console_StartNewLine(pc)

		lea	(a2),a4						; a4 = stack frame (after arguments buffer was processed by Console_Write)

		; Load screen configuration bitfield
		move.b	(a1)+,d6						; d6 = configuration bitfield
		bpl.s	.align_ok						; if "_eh_align_offset" is not set, branch
		addq.w	#1,	a1							; skip a byte to avoid address error on reading the next word
		
	.align_ok:
		lea		(a1),a3							; a3 may be used to fetch console program address later

		; Print error address (for Address error and Bus Error only)
		btst	#0,d6							; does error has extended stack frame (Address Error and Bus Error only)?
		beq.s	.skip							; if not, branch

		lea	Str_Address(pc),a1					; a1 = formatted string
		lea	2(a4),a2							; a2 = arguments buffer
		jsr	Console_WriteLine_Formatted(pc)
		addq.w	#8,a4							; skip extension part of the stack frame

	.skip:
		; Print error location
		lea	Str_Location(pc),a1				; a1 = formatted string
		lea	2(a4),a2						; a2 = arguments buffer
		jsr	Console_WriteLine_Formatted(pc)

		; Print module name error occured in
		lea	Str_Module(pc),a1				; a1 = formatted string
		lea	2(a4),a2						; a2 = arguments buffer
		jsr	Console_WriteLine_Formatted(pc)

		; Print caller
		movea.l	0.w,a1							; a1 = initial stack pointer value
		lea	6(a4),a2						; a2 = call stack (after exception stack frame)
		jsr	Error_MaskStackBoundaries(pc)

		jsr	Error_GuessCaller(pc)			; d1 = caller
		lea	Str_Caller(pc),a1				; a1 = formatted string
		move.l	d1,-(sp)
		lea	(sp),a2						; a2 = arguments buffer
		jsr	Console_WriteLine_Formatted(pc)
		jsr	Console_StartNewLine(pc)
		addq.w	#4,sp							; free argument

		btst	#6, d6							; is execute console program bit set?
		bne.w	Error_EnterConsoleProgram		; if yes, branch to error trap

		; ----------------
		; Registers
		; ----------------

		lea	4(sp),a2						; use register buffer as arguments

		; Print data registers
		jsr	Console_GetPosAsXY(pc)			; d0/d1 = XY-pos
		move.w	d1,-(sp)						; remember line
		moveq	#3,d0							; left margin for data registers
		jsr	Console_SetPosAsXY(pc)
		move.w	#'d0', d0						; d0 = 'd0', what a twist !!!
		moveq	#8-1, d5						; number of registers - 1
		jsr	Error_DrawRegisters(pc)

		; Print address registers
		move.w	(sp)+,d1					; restore line
		moveq	#$11,d0						; left margin for address registers
		jsr	Console_SetPosAsXY(pc)
		move.w	#'a0',d0
		moveq	#7-1, d5						; number of registers - 1
		jsr	Error_DrawRegisters(pc)

		; Special case : stack pointer (SP)
		move.w	#'sp',d0
		moveq	#0,d5							; number of registers - 1
		move.l	a4,-(sp)
		lea	(sp),a2
		jsr	Error_DrawRegisters(pc)
		addq.w	#4,sp

		; Display USP and SR (if requested)
		btst	#1,d6
		beq.s	.skip2

   		; Draw 'USP'
		lea	Str_USP(pc),a1
		lea	(sp), a2						; a2 = USP saved in stack (how convy!)
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
		move.l	$78.w,d0						; d0 = VInt vector address
		lea	Str_VInt(pc),a0
		jsr	Error_DrawInterruptHandler(pc)

		move.l	$70.w,d0						; d0 = HInt vector address
		lea	Str_HInt(pc),a0
		jsr	Error_DrawInterruptHandler(pc)

		jsr	Console_StartNewLine(pc)		; newline

		; -----------------
		; Stack contents
		; -----------------

		movea.l 0.w,a1							; a1 = stack top
		lea		(a4),a2						; a2 = stack bottom
		subq.l	#1,a1							; hotfix to convert stack pointer $0000 to $FFFF, decrement by 1 shouldn't make any difference otherwise
		bsr.s	Error_MaskStackBoundaries

		jsr		Console_GetPosAsXY(pc)			; d0/d1 = XY-pos
		moveq	#28-3,d5
		sub.w	d1,d5
		bmi.s	.stack_done

		bsr.s	Error_DrawStackRow_First

	.stack_loop:
		jsr	Error_DrawStackRow(pc)
		dbf	d5,.stack_loop

	.stack_done:
		btst	#5,d6							; is execute console program (at the end) bit set?
		bne.s	Error_RunConsoleProgram

; ---------------------------------------------------------------
Error_IdleLoop:
		bra.s	*

; ---------------------------------------------------------------
; Routine to enter console mode after writting error header
; ---------------------------------------------------------------

Error_EnterConsoleProgram:
		moveq	#0,d1
		jsr	Console_SetBasePattern(pc)

Error_RunConsoleProgram:
		move.l	a3,(sp)+						; replace USP in stack with return address
		movem.l	(sp)+,d0-a6					; restore registers
		pea	Error_IdleLoop(pc)				; set return address
		move.l	-$3C(sp),-(sp)					; retrieve "a3" saved earlier
		rts										; jump to a3

; ---------------------------------------------------------------
Error_InitConsole:	__global
		lea	ErrorHandler_ConsoleConfig(pc),a1
		lea	Art1bpp_Font(pc),a2
		jmp	Console_Init(pc)				; d5 = On-screen position


; ===============================================================
; ---------------------------------------------------------------
; Masks top and bottom stack boundaries to 24-bit
; ---------------------------------------------------------------
; INPUT:
;		a1		Stack top boundary
;		a2		Current stack pointer
;
; USES:
;		d1, d2
; ---------------------------------------------------------------

Error_MaskStackBoundaries:
		move.l 	#$FFFFFF,d1

		move.l	a1,d2
		and.l	d1,d2
		movea.l	d2,a1

		move.l	a2,d2
		and.l	d1,d2
		movea.l	d2,a2
		rts


; ===============================================================
; ---------------------------------------------------------------
; Subroutine to draw contents of stack row
; ---------------------------------------------------------------
; INPUT:
;		a0		String buffero
;		a1		Top of stack pointer
;		a2		Arguments (stack contents)
; ---------------------------------------------------------------

Error_DrawStackRow_First:
		lea	-$30(sp),sp
		lea	(sp),a0				; a0 = string buffer
		moveq	#-1,d7					; size of the buffer for formatter functions (we assume buffer will never overflow)

		move.l	#'(SP)',(a0)+
		move.w	#': ',(a0)+
		bra.s	Error_DrawStackRow_Continue

; ---------------------------------------------------------------
Error_DrawStackRow:
		lea	-$30(sp),sp
		lea	(sp),a0				; a0 = string buffer
		moveq	#-1,d7					; size of the buffer for formatter functions (we assume buffer will never overflow)

		move.w	#' +',(a0)+
		move.w	a2,d1
		sub.w	a4,d1					; d1 = stack displacement
		jsr	FormatHex_Byte(pc)
		move.w	#': ',(a0)+

; ---------------------------------------------------------------
Error_DrawStackRow_Continue:
		moveq	#5, d0					; number of words to display

	.loop:
		moveq	#$FFFFFF00|_pal2,d1		; use light blue
		cmp.l	a1,a2					; is current word out of stack?
		blo.s	.0						; if not, branch
		moveq	#$FFFFFF00|_pal3,d1		; use dark blue
	.0:
		move.b	d1,(a0)+				; setup color
		move.w	(a2)+,d1
		jsr	FormatHex_Word(pc)
		move.b	#' ',(a0)+
		dbf	d0,.loop

		clr.b	(a0)+					; finalize string

		; Draw string on screen
		lea	(sp),a0
		moveq	#0,d1
		jsr	Console_WriteLine_WithPattern(pc)
		lea	$30(sp),sp
		rts


; ===============================================================
; ---------------------------------------------------------------
; Subroutine to draw series of registers
; ---------------------------------------------------------------
; INPUT:
;		d0	.w	Name of the first register ('d0' or 'a0')
;		d5	.w	Number of registers
;		a2		Registers buffer
; ---------------------------------------------------------------

Error_DrawRegisters:     
		lea	-$10(sp),sp				; allocate string buffaro
		moveq	#-1,d7						; size of the buffer for formatter functions (we assume buffer will never overflow)

	.regloop:
		lea	(sp),a0						; use allocated stack space as string buffer
		move.w	d0,(a0)+						; put register name
		move.w	#': ',(a0)+					; put ": "
		move.b	#_pal2,(a0)+					; put palette flag
		move.l	(a2)+,d1
		jsr	FormatHex_LongWord(pc)			; put register contents
		clr.b	(a0)+							; finalize string

		lea	(sp),a0						; use allocated stack space as string buffer
		moveq	#0,d1							; default pattern
		jsr	Console_WriteLine_WithPattern(pc)
		addq.w	#1,d0							; next register name
		dbf	d5,.regloop

		lea		$10(sp), sp
		rts



; ===============================================================
; ---------------------------------------------------------------
; Subroutine to draw series of registers
; ---------------------------------------------------------------
; INPUT:
;		d0	.l	Interrupt handler address
;		a0		Handler name string
; ---------------------------------------------------------------

Error_DrawInterruptHandler:
		move.l	d0,d1
		swap	d1
		not.b	d1							; does handler address point to RAM (block $FF)?
		bne.s	.ret						; if not, branch

		subq.w	#8,sp
		move.l	a0,(sp)					; Argument #0 : String pointer
		movea.l	d0,a2						; a2 = handler routine
		lea		Str_IntHandler_Unknown(pc),a1
		cmp.w	#$4EF9,(a2)+				; does routine include jmp (xxx).l opcode?
		bne.s	.0							; if not, process "Str_IntHandler_Unknown"
		lea		Str_IntHandler(pc),a1		; otherwise, process "Str_IntHandler"
		move.l	(a2),4(sp)					; Argument #1 : Jump offset
	.0:	lea		(sp),a2
		jsr		Console_Write_Formatted(pc)
		addq.w	#8,sp

	.ret:
		rts

; ---------------------------------------------------------------
Str_IntHandler:
		dc.b	_str, _pal0, _sym|long|split|forced, _pal2, _disp|weak, _newl, 0
		
Str_IntHandler_Unknown:
		dc.b	_str, _pal0, '<undefined>', _newl, 0
		even


; ===============================================================
; ---------------------------------------------------------------
; Subroutine to guess caller by inspecting stack
; ---------------------------------------------------------------
; INPUT:
;		a1				Stack top boundary
;		a2				Stack bottom boundary (after stack frame)
;
; OUTPUT:
;		d1		.l		Caller offset or 0, if not found
;
; USES:
;		a1-a2
; ---------------------------------------------------------------

Error_GuessCaller:
		subq.l	#4,a1					; set a final longword to read
		cmpa.l	a2,a1
		blo.s	.nocaller

.try_offset:
		cmp.w	#$40,(a2)				; does this seem like an offset?
		blo.s	.caller_found			; if yes, branch

.try_next_offset:
		addq.l	#2,a2					; try some next offsets
		cmpa.l	a2,a1
		bhs.s	.try_offset

.nocaller:
		moveq	#0,d1
		rts

; ---------------------------------------------------------------
.caller_found:
		move.l	(a2),d1
		beq.s	.try_next_offset		; if offset is zero, branch
		btst	#0,d1					; is this offset even?
		bne.s	.try_next_offset		; if not, branch
		rts


; ===============================================================
; ---------------------------------------------------------------
; Subroutine to setup/reset VDP in order to display properly
; ---------------------------------------------------------------

ErrorHandler_SetupVDP:	__global
		lea	vdp_control_port,a5 			; a5 = vdp_control_port
		lea	-4(a5),a6				; a6 = (vdp_data_port).l

		; Make sure there are no pending writes to VDP
		tst.w	(a5)

		; Make sure there are no DMA's occurring, otherwise wait
	.wait_dma:
		move.w	(a5),ccr				; is DMA occuring?
		bvs.s	.wait_dma				; wait until it's finished

		; Setup VDP registers for Error Handler screen
		lea	.VDPConfig(pc),a0

	.setup_regs:
		move.w	(a0)+,d0
		bpl.s	.done
		move.w	d0,(a5)
		bra.s	.setup_regs

		.done:

		; Remove all sprites, reset horizontal and vertical scrolling
		moveq	#0,d0
		vram	$0000,(a5)				; reset sprites and horizontal scrolling (HSRAM)
		move.l	d0,(a6)				; ''
		move.l	#$40000010,(a5) 		; reset vertical scrolling
		move.l	d0,(a6)				; ''
		
		; Fill screen with black
		cram	$00,(a5)
		move.w	d0,(a6)

		rts

; ---------------------------------------------------------------
; Error screen's VDP configuration
; ---------------------------------------------------------------

.VDPConfig:
		dc.w	$8004							; $00, disable HInts
		dc.w	$8134							; $01, disable DISP
		dc.w	$8200+VRAM_PlaneA/$400			; $02, set Plane A nametable offset in VRAM
		dc.w	$8400+VRAM_PlaneB/$2000 		; $04, set Plane B nametable offset in VRAM
		dc.w	$8500							; $05, set Sprites offset to $0000
		dc.w	$8700							; $07, set backdrop color
		dc.w	$8B00							; $0B, set VScroll=full, HScroll=full
		dc.w	$8C81							; $0C, use 320 pixels horizontal resolution
		dc.w	$8D00							; $0D, set HScroll table offset to $0000
		dc.w	$8F02							; $0F, set auto-increment to $02
		dc.w	$9011							; $10, use 512x512 plane resolution
		dc.w	$9100							; $11, reset Window X-position
		dc.w	$9200							; $12, reset Window Y-position
		dc.w	0								; WARNING! Make sure the next word is positive!


; ===============================================================
; ---------------------------------------------------------------
; Console loading programme for Error Handler
; ---------------------------------------------------------------

ErrorHandler_ConsoleConfig:

		; ---------------------------------------------------------------
		; Font decompression programme
		; ---------------------------------------------------------------
		; NOTICE: It's possible to generate several "samples" of font
		;	with different color indecies at different VRAM locations.
		;	However, this is not used for this Error Handler
		; ---------------------------------------------------------------

		dcvram	VRAM_Font					; font offset in VRAM
		dc.w	$0000, $0001, $0010, $0011	; decompression table for 1bpp nibbles
		dc.w	$0100, $0101, $0110, $0111	; ''
		dc.w	$1000, $1001, $1010, $1011	; ''
		dc.w	$1100, $1101, $1110, $1111	; ''

		dc.w	-1							; end marker

		; ---------------------------------------------------------------
		; Console RAM initial config
		; ---------------------------------------------------------------

		dcvram	VRAM_PlaneA					; screen start address / plane nametable pointer
		dc.w	40							; number of characters per line
		dc.w	40							; number of charasters on the first line (meant to be the same as the above)
		dc.w	0							; base font pattern (tile id for ASCII $00 + palette flags)
		dc.w	$80							; size of screen row (in bytes)

		dc.w	$2000/$20-1					; size of screen (in tiles - 1)

		; ---------------------------------------------------------------
		; CRAM data
		; ---------------------------------------------------------------
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
		; ---------------------------------------------------------------

		dc.w	$0EEE, -7*2					; line 0: white text
		dc.w	$00CE, -7*2					; line 1: yellow text
		dc.w	$0EEA, -7*2					; line 2: lighter blue text
		dc.w	$0E86, -7*2					; line 3: darker blue text

; ---------------------------------------------------------------
; Error Handler interface data
; ---------------------------------------------------------------

Str_SetErrorScreen:
		dc.b	_pal1, _newl, _setx, 1, _setw, 38, 0

Str_Address:
		dc.b	_pal1, 'Address: ', _pal0, _sym|long|split, _pal2, _disp, 0

Str_Location:
		dc.b	_pal1, 'Location: ', _pal2, _hex|long, 0

Str_Module:
		dc.b	_pal1, 'Module: ', _pal0, _sym|long|split|forced, _pal2, _disp|weak, 0

Str_Caller:
 		dc.b	_pal1, 'Caller: ', _pal0, _sym|long|split, _pal2, _disp, 0

Str_USP:
		dc.b	_setx, $10, _pal0, 'usp: ', _pal2, _hex|long, 0

Str_SR:
		dc.b	_setx, $03, _pal0, 'sr: ', _pal2, _hex|word, 0

Str_VInt:
		dc.b	_pal1, 'VInt: ', 0

Str_HInt:
		dc.b	_pal1, 'HInt: ', 0
		even

; ---------------------------------------------------------------
; Error Handler 1bpp font graphics
; ---------------------------------------------------------------

Art1bpp_Font:	__global
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

; ---------------------------------------------------------------
; Core modules
; ---------------------------------------------------------------


_ValidHeader = $DEB2

; ===============================================================
; ---------------------------------------------------------------
; Subroutine to find nearest symbol for given offset
; ---------------------------------------------------------------
; INPUT:
;		d1	.l		Offset
;
; OUTPUT:
;		d0	.w		Status (0 = ok, -1 = error)
;		d1	.l		Offset displacement
;		a1			Pointer to compressed symbol text
;
; USES:
;		a1-a3 / d0-d3
; ---------------------------------------------------------------

GetSymbolByOffset:
		if def(_USE_SYMBOL_DATA_REF_)
			movea.l	(SymbolData_Ptr).l, a1
		else
			lea		SymbolData(pc), a1
		endc

		cmp.w	#_ValidHeader, (a1)+	; verify header
		bne.s	.return_error

		moveq	#-2, d0
		add.w	(a1)+, d0				; d0 = (lastBlock+1)*4
		moveq	#-4, d2					; d2 will be 4-byte boundary mask
		moveq	#0, d3					; d3 will be gain value

		swap	d1						; d1 = block
		and.w	#$FF, d1				; mask higher 8-bits of block id, since MD has 24-bit address bus anyways ...
		add.w	d1, d1					; d1 = block*2
		add.w	d1, d1					; d1 = block*4
		cmp.w	d0, d1					; is the offset's block within [0..lastBlock+1]?
		bhi.s	.return_error			; if not, branch
		beq.s	.load_prev_block		; if strictly lastBlock+1, fetch the previous one ...

		.load_block:
			move.l	(a1,d1), d0 			; d0 = relative offset
			beq.s	.load_prev_block		; if block is empty, branch
			lea 	(a1,d0.l), a3			; a3 = Block structure
			swap	d1						; d1 = offset

			moveq	#0, d0
			move.w	(a3)+, d0				; d0 = symbols heap relative offset
			cmp.w	(a3), d1				; compare the requested offset with the lowest in the block
			blo.s	.load_prev_block_2		; if even lower, find nearest offset in the previous block

			; WARNING: The following instruction assumes blocks will not be reloaded anymore
			lea		-2(a3,d0.l), a1			; a1 = symbols heap
											; d0 = (high - low)
			lea 	-4(a1), a2 				; a2 = high
											; a3 = low
			.search_loop:
				lsr.w	#1, d0					; 8		; d0 = (high - low) / 2
				and.w	d2, d0					; 4		; find nearest 4-byte struct for the displacement

				cmp.w	(a3,d0), d1				; 14	; compare the requested offset with the guessed entry
				blo.s	.search_lower_half		; 8/10
				bhi.s	.search_higher_half		; 8/10

				adda.w	d0, a3
				bra.s	.load_symbol

			; -----------------------------------------------------------
			.search_higher_half:
				lea 	4(a3,d0), a3			; 12	; limit "low" to "middle"+1 of previously observed area
				move.l	a2, d0					; 4
				sub.l	a3, d0					; 8		; d0 = (high - low)
				bpl.s	.search_loop			; 8/10	; if (low >= high), branch

				subq.w	#4, a3
				bra.s	.load_symbol

			; -----------------------------------------------------------
			.search_lower_half:
				lea 	-4(a3,d0), a2			; 12	; limit "high" to "middle"-1 of previously observed area
				move.l	a2, d0					; 4		;
				sub.l	a3, d0					; 8		; d0 = (high - low)
				bpl.s	.search_loop			; 8/10	; if (low >= high), branch

				lea		(a2), a3

			.load_symbol:
				sub.w	(a3)+, d1				; d1 = displacement
				moveq	#0, d2
				move.w	(a3)+, d2				; d2 = symbol pointer, relative to the heap
				adda.l	d2, a1

				swap	d1						; ''
				; NOTICE: You should be able to access SymbolData+4(pc,d1) now ...
				clr.w	d1						; ''
				swap	d1						; andi.l #$FFFF, d1
				add.l	d3, d1
				moveq	#0, d0					; return success
				rts

; ---------------------------------------------------------------
.return_error:
		moveq	#-1, d0				; return -1
		rts

		; ---------------------------------------------------------------
		.load_prev_block:
			swap	d1
		
		.load_prev_block_2:
			moveq	#0, d0
			move.w	d1, d0
			add.l	d0, d3				; increase offset gain by the offset within the previous block
			addq.l	#1, d3				; also increase offset gain by 1 to compensate for ...
			move.w	#$FFFF, d1			; ... setting offset to $FFFF instead of $10000
			swap	d1
			subq.w	#4, d1				; decrease block number
			bpl.s	.load_block			; if block is positive, branch
			moveq	#-1, d0				; return -1
			rts


; ===============================================================
; ---------------------------------------------------------------
; Subroutine to decode compressed symbol name to string buffer
; ---------------------------------------------------------------
; INPUT:
;		a0			String buffer pointer
;		a1			Pointer to the compressed symbol data
;
;		d7	.w	Number of bytes left in buffer, minus one
;		a0		String buffer
;		a4		Buffer flush function
;
; OUTPUT:
;		(a0)++	ASCII characters for the converted value
;
; USES:
;		a1-a3, d1-d4
; ---------------------------------------------------------------

DecodeSymbol:
		if def(_USE_SYMBOL_DATA_REF_)
			movea.l	(SymbolData_Ptr).l, a3
		else
			lea		SymbolData(pc), a3
		endc
		cmp.w	#_ValidHeader, (a3)+			; verify the header
		bne.s	.return_cc
		add.w	(a3), a3						; a3 = Huffman code table

		moveq	#0,d4							; d4 will handle byte feeding from bitstream

; ---------------------------------------------------------------
		.decode_new_node:
			moveq	#0, d1							; d1 will hold code
			moveq	#0, d2							; d2 will hold code length (in bits)
			lea		(a3), a2						; a2 will hold current position in the decode table

		; ---------------------------------------------------------------
		.code_extend:
			dbf 	d4, .stream_ok					; if bits remain in byte, branch
			move.b	(a1)+, d3
			moveq	#7, d4

		.stream_ok:
			add.b	d3, d3							; get a bit from the bitstream ...
			addx.w	d1, d1							; ... add it to current code
			addq.w	#1, d2							; count this bit

			.code_check_loop:
				cmp.w	(a2), d1 						; does this node has the same code?
				bhi.s	.code_check_next				; if not, check next
				blo.s	.code_extend					; if no nodes are found, branch
				cmp.b	2(a2), d2						; is this code of the same length?
				beq.s	.code_found 					; if not, branch
				blo.s	.code_extend					; if length is lower, append code
		
			.code_check_next:
				addq.w	#4, a2
				cmp.w	(a2), d1 						; does this node has the same code?
				bhi.s	.code_check_next				; if not, check next
				blo.s	.code_extend					; if no nodes are found, branch
				cmp.b	2(a2), d2						; is this code of the same length?
				blo.s	.code_extend					; if length is lower, append code
				bne.s	.code_check_next

		.code_found:
			move.b	3(a2), (a0)+					; get decoded character
			beq.s	.decode_done					; if it's null character, branch
			
			dbf		d7, .decode_new_node
			jsr		(a4)
			bcc.s	.decode_new_node
			rts

; ---------------------------------------------------------------
.decode_done:
		subq.w	#1, a0				; put the last character back
		rts

; ---------------------------------------------------------------
.return_cc:						; return with Carry clear (cc)
		moveq	#0,d0
		rts


; ===============================================================
; ---------------------------------------------------------------
; String formatters : Hexidecimal number
; ---------------------------------------------------------------
; INPUT:
;		d1		Value
;
;		d7	.w	Number of bytes left in buffer, minus one
;		a0		String buffer
;		a4		Buffer flush function
;
; OUTPUT:
;		(a0)++	ASCII characters for the converted value
;
; WARNING!
;	1) Formatters can only use registers a3 / d0-d4
;	2) Formatters should decrement d7 after each symbol write.
;	3) When d7 decrements below 0, a buffer flush function
;		loaded in a4 should be called. The standard function
;		usually renders buffer's contents on the screen (see
;		"Console_FlushBuffer"), then resets the buffer.
;		This function will reload d7, a0 and Carry flag.
;	4) If Carry flag is set after calling buffer flush function,
;		formatter should halt all further processing and return,
;		retaining the returned Carry bit.
; ---------------------------------------------------------------

FormatHex_Handlers:
		jmp		FormatHex_Word(pc)			; $00		; handler for word
; ---------------------------------------------------------------
		jmp		FormatHex_LongWord(pc)		; $04		; handler for longword
; ---------------------------------------------------------------
;	jmp		FormatHex_Byte(pc)			; $08		; handler for byte

FormatHex_Byte:
		moveq	#$F,d3
		move.w	d1,d2
		lsr.w	#4,d2
		and.w	d3,d2						; get nibble
		move.b	HexDigitToChar(pc,d2), (a0)+
		
		dbf		d7, FormatHex_Word_WriteLastNibble
		jsr		(a4)
		bcc.s	FormatHex_Word_WriteLastNibble
		rts		; return Carry=1

; ---------------------------------------------------------------
FormatHex_LongWord:
		swap	d1

FormatHex_LongWord_Swapped:
		bsr.s	FormatHex_Word
		bcs.s	FormatHex_Return			; if buffer terminated, branch

FormatHex_Word_Swapped:
		swap	d1
		;fallthrough

; ---------------------------------------------------------------
FormatHex_Word:
		moveq	#4,d2
		moveq	#$F,d3

		rept 4-1
			rol.w	d2,d1
			move.b	d1,d4
			and.w	d3,d4						; get nibble
			move.b	HexDigitToChar(pc,d4), (a0)+
			dbf		d7, *+6						; if buffer is not exhausted, branch
			jsr		(a4)						; otherwise, call buffer flush function
			bcs.s	FormatHex_Return			; if buffer is terminated, branch
		endr

		rol.w	d2,d1

FormatHex_Word_WriteLastNibble:
		and.w	d3,d1						; get nibble
		move.b	HexDigitToChar(pc,d1), (a0)+
		dbf		d7, FormatHex_Return
		jmp		(a4)						; call buffer flush function and return buffer status

FormatHex_Return:
		rts									; return buffer status

; ---------------------------------------------------------------
HexDigitToChar:
		dc.b	'0123456789ABCDEF'


; ===============================================================
; ---------------------------------------------------------------
; String formatters : Binary number
; ---------------------------------------------------------------
; INPUT:
;		d1		Value
;
;		d7	.w	Number of bytes left in buffer, minus one
;		a0		String buffer
;		a4		Buffer flush function
;
; OUTPUT:
;		(a0)++	ASCII characters for the converted value
;		Carry=0 if buffer is not terminated, Carry=1 otherwise.
;
; WARNING!
;	1) Formatters can only use registers a3 / d0-d4
;	2) Formatters should decrement d7 after each symbol write.
;	3) When d7 decrements below 0, a buffer flush function
;		loaded in a4 should be called. The standard function
;		usually renders buffer's contents on the screen (see
;		"Console_FlushBuffer"), then resets the buffer.
;		This function will reload d7, a0 and Carry flag.
;	4) If Carry flag is set after calling buffer flush function,
;		formatter should halt all further processing and return,
;		retaining the returned Carry bit.
; ---------------------------------------------------------------

FormatBin_Handlers:
		jmp 	FormatBin_Word(pc)	 			; $00	Word display handler
; ---------------------------------------------------------------
		jmp 	FormatBin_LongWord(pc) 			; $04	Longword display handler
; ---------------------------------------------------------------
;	jmp		FormatBin_Byte(pc)				; $08	Byte display handler

FormatBin_Byte:
		moveq	#8-1, d2

		.loop:
			moveq	#'0'/2,d0
			add.b	d1,d1
			addx.b	d0,d0
			move.b	d0, (a0)+

			dbf		d7, .buffer_ok
			jsr		(a4)
			bcs.s	.quit
		.buffer_ok:

			dbf		d2, .loop

.quit:
		rts

; ---------------------------------------------------------------
FormatBin_LongWord:
		swap	d1
		bsr.s	FormatBin_Word
		bcs.s	FormatBin_Return
		swap	d1

FormatBin_Word:
		moveq	#16-1, d2

		.loop:
			moveq	#'0'/2,d0
			add.w	d1,d1
			addx.b	d0,d0
			move.b	d0, (a0)+

			dbf		d7, .buffer_ok
			jsr		(a4)
			bcs.s	FormatBin_Return
		.buffer_ok:

			dbf		d2, .loop
			
FormatBin_Return:
		rts


; ===============================================================
; ---------------------------------------------------------------
; String formatters : Decimal number
; ---------------------------------------------------------------
; INPUT:
;		d1		Value
;
;		d7	.w	Number of bytes left in buffer, minus one
;		a0		String buffer
;		a4		Buffer flush function
;
; OUTPUT:
;		(a0)++	ASCII characters for the converted value
;
; WARNING!
;	1) Formatters can only use registers a3 / d0-d4
;	2) Formatters should decrement d7 after each symbol write.
;	3) When d7 decrements below 0, a buffer flush function
;		loaded in a4 should be called. The standard function
;		usually renders buffer's contents on the screen (see
;		"Console_FlushBuffer"), then resets the buffer.
;		This function will reload d7, a0 and Carry flag.
;	4) If Carry flag is set after calling buffer flush function,
;		formatter should halt all further processing and return,
;		retaining the returned Carry bit.
; ---------------------------------------------------------------

FormatDec_Handlers:
		jmp 	FormatDec_Word(pc)	 			; $00	Word display handler
; ---------------------------------------------------------------
		jmp 	FormatDec_LongWord(pc) 			; $04	Longword display handler
; ---------------------------------------------------------------
		lea 	DecimalBase_Byte(pc), a3			; $08	Byte display handler
		andi.w	#$FF, d1								; ...
		bra.s 	FormatDec			 				; ...

; ---------------------------------------------------------------
FormatDec_Word:
		lea 	DecimalBase_Word(pc), a3

FormatDec:
		clr.b	d0						; d0 will be trim zeros switcher
		moveq	#9, d3					; d3 will be DBcc iterator base
		move.w	(a3)+, d4				; d4 = decimal base

FormatDec_Cont:
		.ProcessDigit:
			move.w	d3, d2

			.FindDigit:
				sub.w	d4, d1
				dbcs	d2, .FindDigit

			add.w	d4, d1							; restore digit
			sub.w	d3, d2
			neg.w	d2								; d2 = digit
			or.b	d2, d0							; have we met non-zero digit yet?
			beq.s	.NextDigit						; if not, branch
			add.b	#'0', d2
			move.b	d2, (a0)+

			dbf		d7, .NextDigit
			jsr		(a4)
			bcs.s	FormatDec_Return

		.NextDigit:
			move.w	(a3)+, d4
			bpl.s	.ProcessDigit

		; The last digit is done manually
		add.b	#'0', d1
		move.b	d1, (a0)+
		dbf		d7, FormatDec_Return
		jmp		(a4)

FormatDec_Return:
		rts

; ---------------------------------------------------------------
FormatDec_LongWord:
		lea 	DecimalBase_Long(pc), a3
		clr.b	d0						; d0 will be trim zeros switcher
		moveq	#9, d3					; d3 will be DBcc iterator base
		move.l	(a3)+, d4				; d4 = decimal base

		.ProcessDigit:
			move.w	d3, d2
			
			.FindDigit:
				sub.l	d4, d1
				dbcs	d2, .FindDigit

			add.l	d4, d1							; restore digit
			sub.w	d3, d2
			neg.w	d2								; d2 = digit
			or.b	d2, d0							; have we met non-zero digit yet?
			beq.s	.NextDigit						; if not, branch
			add.b	#'0', d2
			move.b	d2, (a0)+
			
			dbf		d7, .NextDigit
			jsr		(a4)
			bcs.s	FormatDec_Return

		.NextDigit:
			move.l	(a3)+, d4				; load next decimal base
			bpl.s	.ProcessDigit			; if base is positive, branch
											; otherwise, base is word-sized ...

		bra.s	FormatDec_Cont		; continue drawing with word-sized version
											; note that lower word of D4 already contains next decimal base ...

; ---------------------------------------------------------------
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

; ---------------------------------------------------------------
DecimalBase_Word:
		dc.w	10000
		dc.w	1000

DecimalBase_Byte:
		dc.w	100
		dc.w	10
		dc.w	-1				; marks end of digit searching


; ===============================================================
; ---------------------------------------------------------------
; String formatters : Symbols
; ---------------------------------------------------------------
; INPUT:
;		d1		Value
;
;		d7	.w	Number of bytes left in buffer, minus one
;		a0		String buffer
;		a4		Buffer flush function
;
; OUTPUT:
;		(a0)++	ASCII characters for the converted value
;
; WARNING!
;	1) Formatters can only use registers a3 / d0-d4
;	2) Formatters should decrement d7 after each symbol write.
;	3) When d7 decrements below 0, a buffer flush function
;		loaded in a4 should be called. The standard function
;		usually renders buffer's contents on the screen (see
;		"Console_FlushBuffer"), then resets the buffer.
;		This function will reload d7, a0 and Carry flag.
;	4) If Carry flag is set after calling buffer flush function,
;		formatter should halt all further processing and return,
;		retaining the returned Carry bit.
; ---------------------------------------------------------------

FormatSym_Handlers:
		ext.l	d1							; $00		; handler for word
		bra.s	FormatSym					; $02
; ---------------------------------------------------------------
		jmp		FormatSym(pc)				; $04		; handler for longword
; ---------------------------------------------------------------
		ext.w	d1							; $08		; handler for byte
		ext.l	d1

FormatSym:
		movem.l	d1/d3/a1-a2, -(sp)
		jsr		GetSymbolByOffset(pc)			; IN:	d1 = offset
		bne.s	FormatSym_ChkUnknownSymbol		; OUT:	d0/Z = error status, d1 = displacement, a1 = symbol pointer
		move.l	d1, (sp)						; replace offset stored in stack as D1 with displacement
		jsr		DecodeSymbol(pc)				; IN:	a1 = symbol pointer
		movem.l	(sp)+, d1/d3/a1-a2				; NOTICE: This doesn't affect CCR, so this routine still returns Carry
		bcs.s	FormatSym_Return				; if got carry (buffer termination), return immediately

FormatSym_ChkDrawDisplacement:
		btst	#3, d3							; is "display just label part so far" bit set?
		bne.s	FormatSym_Return				; if yes, branch (Z=0 or C=1)
		jmp		FormatString_CodeHandlers+$40(pc); otherwise, also display displacement now

FormatSym_Return:
		rts

; ---------------------------------------------------------------
FormatSym_ChkUnknownSymbol:
		movem.l	(sp)+, d1/d3/a1-a2
  	btst	#2, d3							; is "draw <unknown> on error" bit set?
		beq.s	FormatSym_ReturnNC				; if not, branch
		lea		FormatSym_Str_Unknown(pc), a3
		jmp		FormatString_CodeHandlers+$52(pc)	; jump to code D0 (string) handler, but skip instruction that sets A3

; ---------------------------------------------------------------
FormatSym_ReturnNC:
		moveq	#-1, d0							; reset Carry, keep D0 an error code
		bra.s	FormatSym_ChkDrawDisplacement

; ---------------------------------------------------------------
FormatSym_Str_Unknown:
		dc.b	'<unknown>',0
		even

; ---------------------------------------------------------------
; INPUT:
;		d1	.l	Displacement
; ---------------------------------------------------------------

FormatSym_Displacement:
		move.b	#'+', (a0)+
		dbf		d7, .buffer_ok
		jsr		(a4)
		bcs.s	FormatSym_Return
.buffer_ok:

		swap	d1								; swap displacement longword
		tst.w	d1								; test higher 16-bits of displacement
		beq		FormatHex_Word_Swapped			; if bits are empty, display displacement as word
		bra		FormatHex_LongWord_Swapped		; otherwise, display longword

; ---------------------------------------------------------------
; INPUT:
;		d1	.l	Offset
;		d3	.b	Control byte
; ---------------------------------------------------------------

FormatSym_Offset:
		btst	#3, d3							; is "don't draw offset" flag set?
		bne.s	FormatSym_Return				; WARNING: Should return NC
		jmp		FormatHex_LongWord(pc)



; ===============================================================
; ---------------------------------------------------------------
; Error handling and debugging modules
; 2016-2017,Vladikcomper
; ---------------------------------------------------------------
; String formatter module
; ---------------------------------------------------------------
; INPUT:
;		a0		Pointer to a string buffer
;		a1		Pointer to format sequence
;		a2		Pointer to arguments list
;		a4		Buffer flush function
;		d7	.w	Number of bytes left in buffer,minus one
;
; USES:
;		a0-a2
; ---------------------------------------------------------------

;d0-d4/a3:	reg	d0-d4/a3
FormatString_regnum		equ	7

_hex	equ		$80
_dec	equ		$90
_bin	equ		$A0
_sym	equ		$B0
_disp	equ		$C0
_str	equ		$D0

byte	equ		0
word	equ		1
long	equ		3

; for number formatters ...
signed	equ		8

; for symbol formatters ...
split	equ		8				; display symbol/offset only, don't draw displacement yet ...
forced	equ		4				; display <unknown> if symbol was not found

; for symbol displacement or offset formatters ...
weak	equ		8				; don't draw offset (for use with _sym|forced, see above)

; ---------------------------------------------------------------
FormatString:	__global
		movem.l	d0-d4/a3,-(sp)

		; NOTICE: This loop shouldn't use registers D0/D1,as control codes B0..BF,C0..CF
		;	that are executed consequently use it to pass parameters inbetween.
	.copy_loop:
		move.b	(a1)+,(a0)+
		dble	d7,.copy_loop				; if character's code is below $80 and not $00,copy string ...
		bgt.s	.flush
		beq.s	.quit						; if char $00 was fetched,quit

	.flag:
		; Process special character
		move.b	-(a0),d3					; d3 = special character that was pushed out of the string
		moveq	#$70,d2					; d2 = $00,$10,$20,$30,$40,$60,$70
		and.b	d3,d2						; d2 = code offset based on character's code, aligned on $10-byte boundary
		jsr		FormatString_CodeHandlers(pc,d2)	; jump to an appropriate special character handler
		bcc.s	.copy_loop					; if string buffer is good,branch

	.quit_no_flush:
		movem.l	(sp)+,d0-d4/a3
		rts

	.flush:
		jsr	(a4)						; flush buffer
		bcc.s	.copy_loop					; if flashing was ok,branch
		bra.s	.quit_no_flush

	.quit:
		subq.w	#1,a0		; because D7 wasn't decremented?
		jsr	(a4)							; call flush buffer function
		movem.l	(sp)+,d0-d4/a3

	.return:
		rts

; --------------------------------------------------------------
FormatString_CodeHandlers:
		; codes 80..8F : Display hex number
		lea		FormatHex_Handlers(pc),a3			; $00
		eor.b	d3,d2								; $04	; d2 = lower 4 bits of char code,encodes argument size (valid values are: 0,1,3,see below)
		add.b	d2,d2								; $06	; multiply 4-bit code by 2 as instructions in the code handlers below are word-sized
		jmp		.ArgumentFetchFlow(pc,d2)			; $08	; jump to an appropriate insturction (note that even invalid codes won't crash)
		nop											; $0C
		nop											; $0E

		; codes 90..9F : Display decimal number
		lea		FormatDec_Handlers(pc),a3			; $00
		eor.b	d3,d2								; $04	; d2 = lower 4 bits of char code,encodes argument size (valid values are: 0,1,3,see below)
		add.b	d2,d2								; $06	; multiply 4-bit code by 2 as instructions in the code handlers below are word-sized
		jmp		.ArgumentFetchFlow(pc,d2)			; $08	; jump to an appropriate insturction (note that even invalid codes won't crash)
		nop											; $0C
		nop											; $0E

		; codes A0..AF : Display binary number
		lea		FormatBin_Handlers(pc),a3			; $00
		eor.b	d3,d2								; $04	; d2 = lower 4 bits of char code,encodes argument size (valid values are: 0,1,3,see below)
		add.b	d2,d2								; $06	; multiply 4-bit code by 2 as instructions in the code handlers below are word-sized
		jmp		.ArgumentFetchFlow(pc,d2)			; $08	; jump to an appropriate instruction (note that even invalid codes won't crash)

	.d0:
		subq.w	#1,a0								; $0C	; overwrite null-terminator (part of "String" section,see below)
		rts											; $0E

		; codes B0..BF : Display symbol
		lea		FormatSym_Handlers(pc),a3			; $00
		move.b	d3,d2								; $04
		and.w	#3,d2								; $06	; d2 = 0,1,3 ... (ignore handlers for signed values)
		add.w	d2,d2								; $0A	; multiply 4-bit code by 2 as instructions in the code handlers below are word-sized
		jmp		.ArgumentFetchFlow(pc,d2)			; $0C	; jump to an appropriate instruction (note that even invalid codes won't crash)

		; codes C0..CF : Display symbol's displacement (to be used after codes B0..BF,if extra formatting is due)
		tst.w	d0									; $00	; check "GetSymbolByOffset" (see "FormatSym" code)
		bmi.s	.c0									; $02	; if return code is -1 (error),assume d1 is OFFSET,display it directly
		tst.l	d1									; $04	; assume d1 is DISPLACEMENT,test it
		beq.s	.return2							; $06	; if displacement is zero,branch
		jmp		FormatSym_Displacement(pc)			; $08
	.c0:
		jmp		FormatSym_Offset(pc)				; $0C

		; codes D0..DF : String
		movea.l	(a2)+,a3							; $00	; a3 = string ptr
	.d1:	
		move.b	(a3)+,(a0)+						; $02	; copy char
		dbeq	d7,.d1								; $04	; loop until either buffer ends or zero-terminator is met
		beq.s	.d0									; $08	; if met zero-terminator,branch
		jsr		(a4)								; $0A	; flush buffer
		bcc.s	.d1									; $0C	; if buffer is ok,branch
	.return2:
		rts											; $0E	; return C

		; codes E0..EF : Drawing command (ignore)
		addq.w	#1,a0								; $00	; restore control character back
		bra.s	.AfterRestoreCharacter				; $02

		; NOTICE: Code handlers continue below and overlap with the following code ...

; --------------------------------------------------------------
; WARNING!
;	The code in the following blocks are critical and shouldn't
;	be altered anymore. Each instruction MUST take 2 bytes,
;	so even the invalid codes won't crash, but only break
;	the flow ...
; --------------------------------------------------------------

.ArgumentFetchFlow:
		addq.w	#8,a3							; $00 :$04	; code 0 : Display byte
		move.w	(a2)+,d1						; $02 :$06	; code 1 : Display word
		jmp		(a3)							; $04 :$08	; code 2 : ## invalid : displays garbage word
; --------------------------------------------------------------
		addq.w	#4,a3							; $06 :$0A	; code 3 : Display longword
		move.l	(a2)+,d1						; $08 :$0C	; code 4 : ## invalid ##: displays word,but loads longword
		jmp		(a3)							; $0A :$0E	; code 5 : ## invalid ##: displays garbage word
; --------------------------------------------------------------
		; codes F0..FF : Drawing command,one-byte argument (ignore)
		addq.w	#1,a0							; $0C :$00	; code 6 : ## invalid ##: restores control character and puts another one
		bra.s	.AfterRestoreCharacter2			; $0E :$02	; code 7 : ## invalid ##: does nothing
; --------------------------------------------------------------
		addq.w	#8,a3							; $10		; code 8 : Display signed byte
		move.w	(a2)+,d1						; $12		; code 9 : Display signed word
		bra.s	.CheckValueSign					; $14		; code A : ## invalid ##: displays garbage signed word
; --------------------------------------------------------------
		addq.w	#4,a3							; $16		; code B : Display signed longword
		move.l	(a2)+,d1						; $18		; code C : ## invalid ##: displays signed word,but loads longword
; --------------------------------------------------------------
.CheckValueSign:
		bpl.s	.SignPositive					; $1A		; code D : ## invalid ##: displays garbage signed word
		neg.l	d1								; $1C		; code E : ## invalid ##: displays gargage pseudo-negative word
		move.b	#'-',(a0)+						; $1E		; code F : ## invalid ##: displays gargage pseudo-non-negative word
		bra.s	.AfterSendSign

; --------------------------------------------------------------
.SignPositive:
		move.b	#'+',(a0)+

.AfterSendSign:
		dbf		d7,.sign_ok					; if there are characters left in the buffer,branch
		jsr		(a4)							; call buffer flush function
		bcs.s	.return2						; if there's no space left in buffer,quit

.sign_ok:
		jmp		(a3)							; draw the actual value using an appropriate handler

; --------------------------------------------------------------
.AfterRestoreCharacter2:
		dbf		d7,.AfterRestoreCharacter3
		jsr		(a4)
		bcs.s	.return2  

.AfterRestoreCharacter3:
		move.b	(a1)+,(a0)+

.AfterRestoreCharacter:
		dbf		d7,.return2
		jmp		(a4)


; ===============================================================
; ---------------------------------------------------------------
; Error handling and debugging modules
; 2016-2017,Vladikcomper
; ---------------------------------------------------------------
; Console Module
; ---------------------------------------------------------------

; ---------------------------------------------------------------
; RAM structure
; ---------------------------------------------------------------

				rsreset
Console_RAM				equ		__rs
Console.ScreenPosReq	rs.l	1				;		screen position request for VDP
Console.CharsPerLine	rs.w	1				; d2	number of characters per line
Console.CharsRemaining	rs.w	1				; d3	remaining number of characters
Console.BasePattern		rs.w	1				; d4	base pattern
Console.ScreenRowSz		rs.w	1				; d6	row size within screen position
Console.Validator		rs.b	1				;		should contain $5D to ensure this is valid console memory area
							rs.b	1				;		<<FREE>>
sizeof_Console_RAM		equ		__rs-Console_RAM

; Drawing flags supported in strings
_newl	equ		$E0
_cr		equ		$E6
_pal0	equ		$E8
_pal1	equ		$EA
_pal2	equ		$EC
_pal3	equ		$EE

_setw	equ		$F0
_setoff	equ		$F4
_setpat	equ		$F8
_setx	equ		$FA

_ConsoleEnable	equ	$5D

; ===============================================================
; ---------------------------------------------------------------
; Initialize console module
; ---------------------------------------------------------------
; INPUT:
;		a1		Console config
;		a2		Console font graphics (1bpp)
;		a3		Console RAM pointer
;
; OUTPUT:
;		d5	.l	Current on-screen position
;
; USES:
;		d0-d4,a5-a6
; ---------------------------------------------------------------

Console_Init:
		lea		vdp_control_port,a5
		lea		-4(a5),a6

		; Load console font
	.font_prg_loop:
		tst.w	(a1)
		bmi.s	.font_done
		move.l	(a1)+,(a5)				; VDP => Setup font offset in VRAM
		lea		(a2),a0				; load font offset
		move.w	(a0)+,d4				; load font size - 1
		jsr		Decomp1bpp(pc)			; decompress font (input: a0-a1/a6,uses: a0/d0-d4)
		lea		$20(a1),a1
		bra.s	.font_prg_loop

.font_done:
		addq.w	#2,a1					; skip end marker

		; Init Console RAM
		move.l	a3,usp					; remember Console RAM pointer in USP to restore it in later calls
		move.l	(a1)+,d5				; d4 = start VRAM pos
		move.l	d5,(a3)+				; Console RAM => copy screen position (long)
		move.l	(a1)+,(a3)+			; Console RAM => copy number of characters per line (word) + characters remaining for the current line (word)
		move.l	(a1)+,(a3)+			; Console RAM => copy base pattern (word) + screen row size (word)
		move.w	#_ConsoleEnable<<8,(a3)+ ; Console RAM => set validator and clear the last byte (UNUSED)

		; WARNING! Don't touch d5 from now on

		; Clear screen
		lea		Console_FillTile(pc),a3
		move.l	d5,(a5)				; VDP => Setup VRAM for screen namespace
		moveq	#0,d0					; d0 = fill pattern
		move.w	(a1)+,d1				; d1 = size of screen in tiles - 1
		jsr		(a3)					; fill screen
		vram	$0000,(a5)				; VDP => Setup VRAM at tile 0
		moveq	#0,d1					; d1 = number of tiles to fill - 1
		jsr		(a3)					; clear first tile


Console_LoadPalette: __global

		; Load palette
		cram	$00,(a5)				; VDP => Setup CRAM write at offset $00
		moveq	#0,d0					; d0 = black color
		moveq	#4-1,d3				; d3 = number of palette lines - 1

	.fill_palette_line:
		move.w	d0,(a6)			; transparent color is always black
		move.w	(a1)+,d2			; get CRAM data entry
	.0:	
		move.w	d2,(a6)			; write to CRAM
		move.w	(a1)+,d2			; get next CRAM data entry
		bpl.s	.0					; if color,branch

		moveq	#0,d1
		jsr		$10(a3,d2)			; fill the rest of cram by a clever jump (WARNING! Precision required!)
		dbf		d3,.fill_palette_line

		; Finalize
		move.w	#$8174,(a5)			; VDP => Enable display
		move.l	d5,(a5)				; VDP => Enable console for writing
		rts

; ---------------------------------------------------------------
Console_FillTile:
		rept 8
		move.l	d0,(a6)
		endr
		dbf		d1,Console_FillTile
		rts


; ===============================================================
; ---------------------------------------------------------------
; Setup console cursor position based on XY coordinates
; ---------------------------------------------------------------
; INPUT:
;		d0	.w	X-position
;		d1	.w	Y-position
; ---------------------------------------------------------------

Console_SetPosAsXY_Stack: __global
		movem.w	4(sp),d0-d1

Console_SetPosAsXY: __global
		movem.l	d1-d2/a3,-(sp)
		move.l	usp,a3
		cmp.b	#_ConsoleEnable,Console.Validator(a3)
		bne.s	.quit

		move.w	(a3),d2
		and.w	#$E000,d2				; clear out displacement,leave base offset only
		mulu.w	Console.ScreenRowSz(a3),d1
		add.w	d1,d2
		add.w	d0,d2
		add.w	d0,d2
		move.w	d2,(a3)
		move.l	(a3)+,vdp_control_port

		move.w	(a3)+,(a3)+			; Reset remaining characters counter

.quit:
		movem.l	(sp)+,d1-d2/a3
		rts


; ===============================================================
; ---------------------------------------------------------------
; Get current line position in XY-coordinates
; ---------------------------------------------------------------
; OUTPUT:
;		d0	.w	X-position
;		d1	.w	Y-position
; ---------------------------------------------------------------

Console_GetPosAsXY: __global
		move.l	a3,-(sp)
		move.l	usp,a3
		cmp.b	#_ConsoleEnable,Console.Validator(a3)
		bne.s	.quit
		moveq	#0,d1
		move.w	(a3),d1
		and.w	#$1FFF,d1						; clear out base offset,leave displacement only
		divu.w	Console.ScreenRowSz(a3),d1		; d1 = row
		move.l	d1,d0
		swap	d0
		lsr.w	d0
.quit:
		move.l	(sp)+,a3
		rts

; ===============================================================
; ---------------------------------------------------------------
; Subroutine to transfer console to a new line
; ---------------------------------------------------------------

Console_StartNewLine: __global
		move.l	a3,-(sp)
		move.l	usp,a3
		cmp.b	#_ConsoleEnable,Console.Validator(a3)
		bne.s	.quit

		move.w	d0,-(sp)
		move.w	(a3),d0
		add.w	Console.ScreenRowSz(a3),d0
		; TODOh: Check if offset is out of plane boundaries
		and.w	#$5FFF,d0			; make sure line stays within plane
		move.w	d0,(a3)			; save new position
		move.l	(a3)+,vdp_control_port
		move.w	(a3)+,(a3)+		; reset characters on line counter (copy "CharsPerLine" to "CharsRemaining")

		move.w	(sp)+,d0
.quit:
		move.l	(sp)+,a3
		rts


; ===============================================================
; ---------------------------------------------------------------
; Subroutine to set console's base pattern
; ---------------------------------------------------------------
; INPUT:
;		d1	.w	Base pattern
; ---------------------------------------------------------------

Console_SetBasePattern: __global
		move.l	a3,-(sp)
		move.l	usp,a3
		cmp.b	#_ConsoleEnable,Console.Validator(a3)
		bne.s	.quit
		move.w	d1,Console.BasePattern(a3)
		
.quit:
		move.l	(sp)+,a3
		rts

; ===============================================================
; ---------------------------------------------------------------
; Subroutine to set console's base pattern
; ---------------------------------------------------------------
; INPUT:
;		d1	.w	Base pattern
; ---------------------------------------------------------------

Console_SetWidth: __global
		move.l	a3,-(sp)
		move.l	usp,a3
		cmp.b	#_ConsoleEnable,Console.Validator(a3)
		bne.s	.quit
		addq.w	#4,a3
		move.w	d1,(a3)+
		move.w	d1,(a3)+

.quit:
		move.l	(sp)+,a3
		rts


; ---------------------------------------------------------------
; Subroutine to draw string on screen
; ---------------------------------------------------------------
; INPUT:
;		a0		Pointer to null-terminated string
;		d1	.w	Base pattern (*_WriteLine_WithPattern only)
;
; OUTPUT:
;		a0		Pointer to the end of string
;
; MODIFIES:
;		a0
; ---------------------------------------------------------------

Console_WriteLine_WithPattern: __global
		bsr.s	Console_SetBasePattern

; ---------------------------------------------------------------
Console_WriteLine: __global
		pea		Console_StartNewLine(pc)

; ---------------------------------------------------------------
Console_Write: __global
		movem.l	d1-d6/a3/a6,-(sp)
		move.l	usp,a3
		cmp.b	#_ConsoleEnable,Console.Validator(a3)
		bne.s	.quit

		; Load console variables
		move.l	(a3)+,d5			; d5 = VDP screen position request
		movem.w	(a3),d2-d4/d6		; d2 = number of characters per line

		swap	d6					; d3 = number of characters remaining until next line
									; d4 = base pattern
									; d6 = screen position increment value
		lea	(vdp_data_port).l,a6		; a6 = (vdp_data_port).l

		; First iteration in .loop,unrolled
		moveq	#0,d1
		move.b	(a0)+,d1			; load first char
		bgt.s	.loop				; if not a null-terminator or flag,branch
		bmi.s	.flag				; if char is a flague,branch

	.done:
		movem.w	d2-d4,(a3)			; save d2-d4 (ignore d6 as it won't get changed anyways ...)
		move.l	d5,-(a3)			; save screen position
		
	.quit:
		movem.l	(sp)+,d1-d6/a3/a6
		rts

; ---------------------------------------------------------------
	.loop:
		dbf		d3,.writechar
		add.w	d2,d3				; restore number of characters per line
		add.l	d6,d5
		bclr	#29,d5
		move.l	d5,4(a6)			; setup screen position

	.writechar:
		add.w	d4,d1  			; add base pattern
		move.w	d1,(a6)			; draw

	.nextchar:
		moveq	#0,d1
		move.b	(a0)+,d1			; load next char
		bgt.s	.loop				; if not a null-terminator or flag,branch
		beq.s	.done				; if null-terminator,branch

		; Process drawing flag
	.flag:
		and.w	#$1E,d1					; d2 = $00,$02,$04,$06,$08,$0A,$0C,$0E,$10,$12,$14,$16,$18,$1A,$1C,$1E
		jmp		.CommandHandlers(pc,d1.w)

; ---------------------------------------------------------------
.CommandHandlers:
		; For flags E0-EF (no arguments)
		add.l	d6,d5						; $00	; codes E0-E1 : start a new line
		moveq	#29,d1 					; $02	; codes E2-E3 : <<UNUSED>>
		bclr	d1,d5						; $04	; codes E4-E5 : <<UNUSED>>
		bra.s	.reset_line					; $06	; codes E6-E7 : reset position to the beginning of line
		bra.s	.set_palette_line_0			; $08	; codes E8-E9 : set palette line #0
		bra.s	.set_palette_line_1			; $0A	; codes EA-EB : set palette line #1
		bra.s	.set_palette_line_2			; $0C	; codes EC-ED : set palette line #2
		bra.s	.set_palette_line_3			; $0E	; codes EE-EF : set palette line #3

		; For flags F0-FF (one-byte arguments)
		move.b	(a0)+,d2					; $10	; codes F0-F1 : set characters per line,reset line
		bra.s	.reset_line					; $12	; codes F2-F3 : <<UNUSED>>
		move.b	(a0)+,d4					; $14	; codes F4-F5 : set low byte of base pattern (raw)
		bra.s	.nextchar					; $16	; codes F6-F7 : <<UNUSED>>
		bra.s	.set_base_pattern_high_byte	; $18	; codes F8-F9 : set high byte of base pattern (raw)
		move.b	(a0)+,d1					; $1A	; codes FA-FB : set x-position
		add.w	d1,d1						; $1C	; codes FC-FD : <<UNUSED>>
		moveq	#-$80,d3					; $1E	; codes FE-FF : <<UNUSED>>
		swap	d3							;
		and.l	d3,d5						;
		swap	d1							;
		or.l	d1,d5						;
;		bra.s	.reset_line					; restore d3 anyways,as it's corrupted

.reset_line:
		move.w	d2,d3
		move.l	d5,4(a6)
		bra.s	.nextchar

; ---------------------------------------------------------------
.set_palette_line_0:
		and.w	#$7FF,d4
		bra.s	.nextchar

; ---------------------------------------------------------------
.set_palette_line_1:
		and.w	#$7FF,d4
		or.w	#$2000,d4
		bra.s	.nextchar

; ---------------------------------------------------------------
.set_palette_line_2:
		and.w	#$7FF,d4
		or.w	#$4000,d4
		bra.s	.nextchar

; ---------------------------------------------------------------
.set_palette_line_3:
		or.w	#$6000,d4
		bra.s	.nextchar

; ---------------------------------------------------------------
.set_base_pattern_high_byte:
		move.w	d4,-(sp)
		move.b	(a0)+,(sp)
		move.w	(sp)+,d4
		bra.s	.nextchar


; ---------------------------------------------------------------
; Subroutine to provide writting of formatted strings
; ---------------------------------------------------------------
; INPUT:
;		a1		Pointer to source formatted string
;		a2		Arguments buffer pointer
;		a5		VDP Control port
;		a6		VDP Data port
;
; USES:
;		a0-a2,d7
; ---------------------------------------------------------------

Console_WriteLine_Formatted: __global
		pea		Console_StartNewLine(pc)

; ---------------------------------------------------------------
Console_Write_Formatted: __global

.buffer_size = $10

		move.l	a4,-(sp)

		lea		.FlushBuffer(pc),a4		; flushing function
		lea		-.buffer_size(sp),sp		; allocate string buffer
		lea		(sp),a0					; a0 = string buffer

		if def(__DEBUG__)
			move.l	a0,-4
		endc

		moveq	#.buffer_size-2,d7			; d7 = number of characters before flush -1
		jsr		FormatString(pc)
		lea		.buffer_size(sp),sp		; free string buffer
		
		move.l	(sp)+,a4
		rts

; ---------------------------------------------------------------
; Flush buffer callback raised by FormatString
; ---------------------------------------------------------------
; INPUT:
;		a0		Buffer position
;		d7	.w	Number of characters remaining in buffer - 1
;
; OUTPUT:
;		a0		Buffer position after flushing
;		d7	.w	Number of characters before next flush - 1
;		Carry	0 = continue operation
;				1 = terminate FormatString with error condition
;
; WARNING: This function shouldn't modify d0-d4 / a1-a3!
; ---------------------------------------------------------------

.FlushBuffer:
		clr.b	(a0)+					; finalize buffer

		neg.w	d7
		add.w	#.buffer_size-1,d7
		sub.w	d7,a0					; a0 = start of the buffer

		if def(__DEBUG__)
			cmpa.l	-4,a0
			beq.s	.align_ok
			move.l	-4,d0
			illegal
		.align_ok:
		endc

		move.l	a0,-(sp)
		jsr		Console_Write(pc)		; call the real flush function
		move.l	(sp)+,a0
		moveq	#.buffer_size-2,d7		; d7 = number of characters before flush -1
		rts								; WARNING! Must return Carry=0


; ===============================================================
; ---------------------------------------------------------------
; Fast 1bpp decompressor
; ---------------------------------------------------------------
; INPUT:
;		a0		Source 1bpp art
;		a1		Decode table (generated or manual)
;		d4	.w	Size of art in bytes - 1
;		a6		VDP Data Port
;
; USES:
;		a0, d0-d2/d4
; ---------------------------------------------------------------

Decomp1bpp:	__global
		moveq	#$1E,d2

	.row:
		move.b	(a0)+,d0				; d0 = %aaaa bbbb
		move.b	d0,d1
		lsr.b	#3,d1					; d1 = %000a aaab
		and.w	d2,d1					; d1 = %000a aaa0
		move.w	(a1,d1),(a6)			; decompress first nibble
		
		add.b	d0,d0					; d0 = %aaab bbb0
		and.w	d2,d0					; d0 = %000b bbb0
		move.w	(a1,d0),(a6)			; decompress second nibble
			
		dbf		d4,.row

		rts



SymbolData: