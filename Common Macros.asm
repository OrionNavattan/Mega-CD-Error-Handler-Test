; ---------------------------------------------------------------------------
; Test if macro argument is used
; ---------------------------------------------------------------------------

ifarg		macros
		if strlen("\1")>0

ifnotarg	macros
		if strlen("\1")=0


; ---------------------------------------------------------------------------
; Align and pad.
; input: length to align to, value to use as padding (default is 0)
; ---------------------------------------------------------------------------

align:		macro length,value
	ifarg \value
		dcb.b (\length-(*%\length))%\length,\value
	else
		dcb.b (\length-(*%\length))%\length,0
	endc
	endm
	
; ---------------------------------------------------------------------------
; Sign-extend a value and use it with moveq
; Replicates the signextendB function in Sonic 2 AS; required to prevent the 
; assembler from generating a sign-extension warning.
; --------------------------------------------------------------------------- 
 
moveq_:		macros
 		moveq	#(\1\+-((-(\1\&(1<<(8-1))))&(1<<(8-1))))!-((-(\1\&(1<<(8-1))))&(1<<(8-1))),\2
    	
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

; ---------------------------------------------------------------------------
; Create a pointer index.
; input: start location (usually offset(*) or 0; leave blank to make pointers
; relative to themselves), id start (default 0), id increment (default 1)
; ---------------------------------------------------------------------------

index:		macro start,idstart,idinc
;		nolist
;		pusho
;		opt	m-

		ifarg \start					; check if start is defined
			index_start: = \start
		else
			index_start: = -1
		endc

		ifarg \0					; check if width is defined (b, w, l)
		index_width: equs "\0"
		else
		index_width: equs "w"				; use w by default
		endc
			
		ifarg \idstart					; check if first pointer id is defined
			ptr_id: = \idstart
		else
			ptr_id: = 0				; use 0 by default
		endc

		ifarg \idinc					; check if pointer id increment is defined
			ptr_id_inc: = \idinc
		else
			ptr_id_inc: = 1				; use 1 by default
		endc
		
;		popo
;		list
		endm
		
; ---------------------------------------------------------------------------
; Item in a pointer index.
; input: pointer target, optional alias (useful if multiple pointers point 
; to same location, such as the bubble mappings or deleted objects
; ---------------------------------------------------------------------------

ptr:		macro
;		nolist
;		pusho
;		opt	m-

		if index_start=-1
			dc.\index_width \1-offset(*)
		else
			dc.\index_width \1-index_start
		endc
		
		if ~def(prefix_id)
			prefix_id: equs "id_"
		endc
		
		if instr("\1",".")=1				; check if pointer is local
		else
		
			ifarg \2
				\prefix_id\\2: equ ptr_id	; create id for pointer using explicitly specified alias			
			elseif ~def(\prefix_id\\1)
				\prefix_id\\1: equ ptr_id	; create id for pointer
			else
				\prefix_id\\1_\$ptr_id: equ ptr_id ; if id already exists, append number
			endc
			
		endc
		
		ptr_id: = ptr_id+ptr_id_inc			; increment id

;		popo
;		list
		endm
		
; ---------------------------------------------------------------------------
; Make a size constant for an array.
; input: array start label
; ---------------------------------------------------------------------------	

arraysize:	macros
		sizeof_\1: equ	*-\1		
