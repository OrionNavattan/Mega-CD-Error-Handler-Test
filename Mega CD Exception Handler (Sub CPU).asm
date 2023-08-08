AddressError:
		__ErrorMessage "SUB CPU: ADDRESS ERROR", _eh_show_sr_usp|_eh_address_error

IllegalInstr:
		__ErrorMessage "SUB CPU: ILLEGAL INSTRUCTION", _eh_show_sr_usp

ZeroDivide:
		__ErrorMessage "SUB CPU: ZERO DIVIDE", _eh_show_sr_usp

ChkInstr:
		__ErrorMessage "SUB CPU: CHK INSTRUCTION", _eh_show_sr_usp

TrapvInstr:
		__ErrorMessage "SUB CPU: TRAPV INSTRUCTION", _eh_show_sr_usp

PrivilegeViol:
		__ErrorMessage "SUB CPU: PRIVILEGE VIOLATION", _eh_show_sr_usp

Trace:
		__ErrorMessage "SUB CPU: TRACE", _eh_show_sr_usp

Line1010Emu:
		__ErrorMessage "SUB CPU: LINE 1010 EMULATOR", _eh_show_sr_usp

Line1111Emu:
		__ErrorMessage "SUB CPU: LINE 1111 EMULATOR", _eh_show_sr_usp

ErrorExcept:
		__ErrorMessage "SUB CPU: ERROR EXCEPTION", _eh_show_sr_usp
		
ErrorHandler:
		move	#$2700,sr				; disable all interrupts
		st.b (mcd_sub_flag).w		; set flag to let main CPU know we've crashed (assumes communication protocol includes checking this flag for $FF before sending commands or while waiting for responses)
		movem.l	d0-a6,-(sp)				; dump all registers
		move.l	usp,a0
		move.l	a0,-(sp)			; dump USP (unnecessary if BIOS is being used, as user mode can not be used with it)
		
	.waitmain:
		cmpi.b	#$FF,(mcd_main_flag).w	; has the main CPU noticed?
		bne.s	.waitmain	; if not, branch
		
		; Main CPU has noticed
		move.l	sp,(mcd_subcom_0).w	; get address of bottom of stack (including dumped registers) for main CPU
		clr.b	(mcd_sub_flag).w ; clear flag to let main CPU know we are done
		bra.s	*	; stay here forever

