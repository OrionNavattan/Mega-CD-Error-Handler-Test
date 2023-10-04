; ---------------------------------------------------------------
; Error handling and debugging modules
; Debugger Definitions
; 2016-2023, Vladikcomper
; Modified 2023 Orion Navattan
; ---------------------------------------------------------------

; Debug Features
; Set to 1 to enable the use of debug assertions and the KDebug interface
DebugFeatures: 		equ 1	

; Enable debugger extensions
; Pressing A/B/C on the exception screen can open other debuggers
; Pressing Start or unmapped button returns to the exception
DebuggerExtensions:	equ 1	; Set to 1 to enable 

; ---------------------------------------------------------------
; Constants
; ---------------------------------------------------------------
; ----------------------------
; Arguments formatting flags
; ----------------------------

; General arguments format flags
hex:		equ		$80				; flag to display as hexadecimal number
deci:		equ		$90				; flag to display as decimal number
bin:		equ		$A0				; flag to display as binary number
sym:		equ		$B0				; flag to display as symbol (treat as offset, decode into symbol +displacement, if present)
symdisp:	equ		$C0				; flag to display as symbol's displacement alone (DO NOT USE, unless complex formatting is required, see notes below)
str:		equ		$D0				; flag to display as string (treat as offset, insert string from that offset)

; NOTES:
;	* By default, the "sym" flag displays both symbol and displacement (e.g.: "Map_Sonic+$2E")
;		In case, you need a different formatting for the displacement part (different text color and such),
;		use "sym|split", so the displacement won't be displayed until symdisp is met
;	* The "symdisp" can only be used after the "sym|split" instance, which decodes offset, otherwise, it'll
;		display a garbage offset.
;	* No other argument format flags (hex, dec, bin, str) are allowed between "sym|split" and "symdisp",
;		otherwise, the "symdisp" results are undefined.
;	* When using "str" flag, the argument should point to string offset that will be inserted.
;		Arguments format flags CAN NOT be used in the string (as no arguments are meant to be here),
;		only console control flags (see below).


; Additional flags ...
; ... for number formatters (hex, dec, bin)
signed:	equ		8				; treat number as signed (display + or - before the number depending on sign)

; ... for symbol formatter (sym)
split_bit:	equ 3
forced_bit:	equ 2
split:	equ		8				; DO NOT write displacement (if present), skip and wait for "symdisp" flag to write it later (optional)
forced:	equ		4				; display "<unknown>" if symbol was not found, otherwise, plain offset is displayed by the displacement formatter

; ... for symbol displacement formatter (symdisp)
weak_bit:	equ 3
weak:	equ		8				; DO NOT write plain offset if symbol is displayed as "<unknown>" (for use with sym|forced, see above)

; Argument type flags:
; - DO NOT USE in formatted strings processed by macros, as these are included automatically
; - ONLY USE when writting down strings manually with DC.B
byte:	equ		0
word:	equ		1
long:	equ		3

; ---------------------------------------------------------------
; Console control flags
; ---------------------------------------------------------------

; Plain control flags: no arguments following
endl:	equ		$E0				; "End of line": flag for line break
cr:		equ		$E6				; "Carriage return": jump to the beginning of the line
pal0:	equ		$E8				; use palette line #0
pal1:	equ		$EA				; use palette line #1
pal2:	equ		$EC				; use palette line #2
pal3:	equ		$EE				; use palette line #3

; Parametrized control flags: followed by 1-byte argument
setw:	equ		$F0				; set line width: number of characters before automatic line break
setoff:	equ		$F4				; set tile offset: lower byte of base pattern, which points to tile index of ASCII character 00
setpat:	equ		$F8				; set tile pattern: high byte of base pattern, which determines palette flags and $100-tile section id
setx:	equ		$FA				; set x-position

; ---------------------------------------------------------------
; Error handler control flags
; ---------------------------------------------------------------

extended_frame_bit:	equ 0
show_sr_usp_bit:	equ 1
return_bit:			equ 5
console_bit:		equ 6
align_offset_bit:	equ 7

; Screen appearence flags
_eh_address_error:	equ	1<<extended_frame_bit	; use for address and bus errors only (tells error handler to display additional "Address" field)
_eh_show_sr_usp:	equ	1<<show_sr_usp_bit		; displays SR and USP registers content on error screen

; Advanced execution flags
; WARNING! For experts only, DO NOT USES them unless you know what you're doing
_eh_return:			equ	1<<return_bit
_eh_enter_console:	equ	1<<console_bit
_eh_align_offset:	equ	1<<align_offset_bit

; Default screen configuration
_eh_default			equ	0

; ---------------------------------------------------------------
; Disable interrupts
; ---------------------------------------------------------------

	if ~def(disable_ints)
disable_ints: macro
		move #$2700,sr
		endm
	endc
	
; ---------------------------------------------------------------
; Create assertions for debugging

; EXAMPLES:
;	assert.b	d0, eq, #1		; d0 must be $01, or else crash!
;	assert.w	d5, eq			; d5 must be $0000!
;	assert.l	a1, hi, a0		; asert a1 > a0, or else crash!
;	assert.b	MemFlag, ne		; MemFlag must be non-zero!
; ---------------------------------------------------------------

assert:	macro	src,cond,dest
	if DebugFeatures
	if narg=3
		cmp.\0	\dest,\src
	else narg=2
		tst.\0	\src
	endc
		b\cond\.s	@skip\@
		RaiseError	"Assertion failed:%<endl>\src \cond \dest"
	@skip\@:
	endc
	endm
	
; ---------------------------------------------------------------
; Raises an error with the given message

; EXAMPLES:
;	RaiseError	"Something is wrong"
;	RaiseError	"Your D0 value is BAD: %<.w d0>"
;	RaiseError	"Module crashed! Extra info:", YourMod_Debugger
; ---------------------------------------------------------------

RaiseError: macro	string,console_program,opts

	pea	*(pc)
	move.w	sr,-(sp)
	__FSTRING_GenerateArgumentsCode \string
	jsr	ErrorHandler
	__FSTRING_GenerateDecodedString \string
	if strlen("\console_program")&def(MainCPU)			; if console program offset is specified ...
		dc.b	\opts+_eh_enter_console|(((*&1)^1)*_eh_align_offset)	; add flag "_eh_align_offset" if the next byte is at odd offset ...
		even															; ... to tell Error handler to skip this byte, so it'll jump to ...
		if DebuggerExtensions
			jsr	\console_program										; ... an aligned "jsr" instruction that calls console program itself
			jmp	ErrorHandler_PagesController
		else
			jmp	\console_program										; ... an aligned "jmp" instruction that calls console program itself
		endc
	else
		if DebuggerExtensions&def(MainCPU)
			dc.b	\opts+_eh_return|(((*&1)^1)*_eh_align_offset)			; add flag "_eh_align_offset" if the next byte is at odd offset ...
			even															; ... to tell Error handler to skip this byte, so it'll jump to ...
			jmp	ErrorHandler_PagesController
		else
			dc.b	\opts+0						; otherwise, just specify \opts for error handler, +0 will generate dc.b 0 ...
			even								; ... in case \opts argument is empty or skipped
		endc
	endc
	even

	endm

; ---------------------------------------------------------------
; Console interface

; EXAMPLES:
;	Console.Run	YourConsoleProgram
;	Console.Write "Hello "
;	Console.WriteLine "...world!"
;	Console.SetXY #1, #4
;	Console.WriteLine "Your data is %<.b d0>"
;	Console.WriteLine "%<pal0>Your code pointer: %<.l a0 sym>"
; ---------------------------------------------------------------

Console: macro

	if strcmp("\0","write")|strcmp("\0","writeline")|strcmp("\0","Write")|strcmp("\0","WriteLine")
		move.w	sr,-(sp)
		__FSTRING_GenerateArgumentsCode \1
		movem.l	a0-a2/d7,-(sp)
		if (__sp>0)
			lea	4*4(sp),a2
		endc
		lea	.str\@(pc),a1
		jsr	Console_\0\_Formatted
		movem.l	(sp)+,a0-a2/d7
		if (__sp>8)
			lea	__sp(sp),sp
		elseif (__sp>0)
			addq.w	#__sp,sp
		endc
		move.w	(sp)+,sr
		bra.w	.instr_end\@
	.str\@:
		__FSTRING_GenerateDecodedString \1
		even
	.instr_end\@:

	elseif strcmp("\0","run")|strcmp("\0","Run")
		jsr	ErrorHandler_ConsoleOnly
		jsr	\1
		bra.s	*

	elseif strcmp("\0","clear")|strcmp("\0","Clear")
		move.w	sr,-(sp)
		jsr	ErrorHandler_ClearConsole
		move.w	(sp)+,sr

	elseif strcmp("\0","pause")|strcmp("\0","Pause")
		move.w	sr,-(sp)
		jsr	ErrorHandler_PauseConsole
		move.w	(sp)+,sr

	elseif strcmp("\0","sleep")|strcmp("\0","Sleep")
		move.w	sr,-(sp)
		move.w	d0,-(sp)
		move.l	a0,-(sp)
		move.w	\1,d0
		subq.w	#1,d0
		bcs.s	.sleep_done\@
		.sleep_loop\@:
			jsr	VSync
			dbf	d0, .sleep_loop\@

	.sleep_done\@:
		move.l	(sp)+,a0
		move.w	(sp)+,d0
		move.w	(sp)+,sr

	elseif strcmp("\0","setxy")|strcmp("\0","SetXY")
		move.w	sr,-(sp)
		movem.l	d0-d1,-(sp)
		move.w	\2,-(sp)
		move.w	\1,-(sp)
		jsr	Console_SetPosAsXY_Stack
		addq.w	#4,sp
		movem.l	(sp)+,d0-d1
		move.w	(sp)+,sr

	elseif strcmp("\0","breakline")|strcmp("\0","BreakLine")
		move.w	sr,-(sp)
		jsr	Console_StartNewLine
		move.w	(sp)+,sr

	else
		inform	2,"""\0"" isn't a member of ""Console"""

	endc
	endm

; ---------------------------------------------------------------
; KDebug interface
; ---------------------------------------------------------------

KDebug: macro
	if DebugFeatures
	if strcmp("\0","write")|strcmp("\0","writeline")|strcmp("\0","Write")|strcmp("\0","WriteLine")
		move.w	sr,-(sp)
		__FSTRING_GenerateArgumentsCode \1
		movem.l	a0-a2/d7,-(sp)
		if (__sp>0)
			lea	4*4(sp),a2
		endc
		lea	.str\@(pc),a1
		jsr	KDebug_\0\_Formatted
		movem.l	(sp)+,a0-a2/d7
		if (__sp>8)
			lea	__sp(sp),sp
		elseif (__sp>0)
			addq.w	#__sp,sp
		endc
		move.w	(sp)+,sr
		bra.w	@instr_end\@
	.str\@:
		__FSTRING_GenerateDecodedString \1
		even
	.instr_end\@:

	elseif strcmp("\0","breakline")|strcmp("\0","BreakLine")
		move.w	sr,-(sp)
		jsr	KDebug_FlushLine
		move.w	(sp)+,sr

	elseif strcmp("\0","starttimer")|strcmp("\0","StartTimer")
		move.w	sr,-(sp)
		move.w	#$9FC0,(vdp_control_port).l
		move.w	(sp)+,sr

	elseif strcmp("\0","endtimer")|strcmp("\0","EndTimer")
		move.w	sr,-(sp)
		move.w	#$9F00,(vdp_control_port).l
		move.w	(sp)+,sr

	elseif strcmp("\0","breakpoint")|strcmp("\0","BreakPoint")
		move.w	sr,-(sp)
		move.w	#$9D00,(vdp_control_port).l
		move.w	(sp)+,sr

	else
		inform	2,"""\0"" isn't a member of ""KDebug"""

	endc
	endc
	endm

	
; ===========================================================================

__ErrorMessage:	macro	string,opts
		__FSTRING_GenerateArgumentsCode \string
		jsr	ErrorHandler
		__FSTRING_GenerateDecodedString \string
		if DebuggerExtensions&def(MainCPU)
			dc.b	\opts+_eh_return|(((*&1)^1)*_eh_align_offset)	; add flag "_eh_align_offset" if the next byte is at odd offset ...
			even													; ... to tell Error handler to skip this byte, so it'll jump to ...
			jmp	ErrorHandler_PagesController	; ... extensions controller
		else
			dc.b	\opts+0
			even
		endc
	endm
	
; ===========================================================================

__FSTRING_GenerateArgumentsCode: macro	string

	__pos:	= 	instr(\string,'%<')		; token position
	__stack:=		0						; size of actual stack
	__sp:	=		0						; stack displacement

	; Parse string itself
	while (__pos)

		; Retrive expression in brackets following % char
    	__endpos:	=		instr(__pos+1,\string,'>')
    	__midpos:	=		instr(__pos+5,\string,' ')
    	if (__midpos<1)|(__midpos>__endpos)
			__midpos: = __endpos
    	endc
		__substr:	substr	__pos+1+1,__endpos-1,\string			; .type ea param
		__type:		substr	__pos+1+1,__pos+1+1+1,\string			; .type

		; Expression is an effective address (e.g. %(.w d0 hex) )
		if "\__type">>8="."
			__operand:	substr	__pos+1+1,__midpos-1,\string			; .type ea
			__param:	substr	__midpos+1,__endpos-1,\string			; param

			if "\__type"=".b"
				pushp	"move\__operand\,1(sp)"
				pushp	"subq.w	#2, sp"
				__stack: = __stack+2
				__sp: = __sp+2

			elseif "\__type"=".w"
				pushp	"move\__operand\,-(sp)"
				__stack: = __stack+1
				__sp: = __sp+2

			elseif "\__type"=".l"
				pushp	"move\__operand\,-(sp)"
				__stack: = __stack+1
				__sp: = __sp+4

			else
				inform 3,'Unrecognized type in string operand: %<\__substr>'
			endc
		endc

		__pos:	=		instr(__pos+1,\string,'%<')
	endw

	; Generate stack code
	rept __stack
		popp	__command
		\__command
	endr

	endm
	
; ===========================================================================

__FSTRING_GenerateDecodedString: macro string

	__lpos:	=		1						; start position
	__pos:	= 	instr(\string,'%<')		; token position

	while (__pos)

		; Write part of string before % token
		__substr:	substr	__lpos,__pos-1,\string
		dc.b	"\__substr"

		; Retrive expression in brakets following % char
    	__endpos:	=		instr(__pos+1,\string,'>')
    	__midpos:	=		instr(__pos+5,\string,' ')
    	if (__midpos<1)|(__midpos>__endpos)
			__midpos: = __endpos
    	endc
		__type:		substr	__pos+1+1,__pos+1+1+1,\string			; .type

		; Expression is an effective address (e.g. %<.w d0 hex> )
		if "\__type">>8="."    
			__param:	substr	__midpos+1,__endpos-1,\string			; param
			
			; Validate format setting ("param")
			if strlen("\__param")<1
				__param: substr ,,"hex"			; if param is ommited, set it to "hex"
			elseif strcmp("\__param","signed")
				__param: substr ,,"hex+signed"	; if param is "signed", correct it to "hex+signed"
			endc

			if (\__param < $80)
				inform	2,"Illegal operand format setting: ""\__param\"". Expected ""hex"", ""dec"", ""bin"", ""sym"", ""str"" or their derivatives."
			endc

			if "\__type"=".b"
				dc.b	\__param
			elseif "\__type"=".w"
				dc.b	\__param|1
			else
				dc.b	\__param|3
			endc

		; Expression is an inline constant (e.g. %<endl> )
		else
			__substr:	substr	__pos+1+1,__endpos-1,\string
			dc.b	\__substr
		endc

		__lpos:	=		__endpos+1
		__pos:	=		instr(__pos+1,\string,'%<')
	endw

	; Write part of string before the end
	__substr:	substr	__lpos,,\string
	dc.b	"\__substr"
	dc.b	0

	endm