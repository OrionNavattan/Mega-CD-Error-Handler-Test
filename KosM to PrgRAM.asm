; ---------------------------------------------------------------------------
; Subroutine to decompress the sub CPU program. Program is compressed with a 
; custom variant of moduled Kosinski: modules are $2000 in size with no 
; padding, and the two-byte header simply contains the total number of 
; modules with no size information. Should be run immediately after 
; decompressing the sub CPU BIOS. 

; input:
;	a3 = mcd_mem_mode
; ---------------------------------------------------------------------------

		sizeof_module: 		equ $2000 ; $2000 is the maximum possible size, as it is the greatest common factor of $20000 (size of bank) and $1A000 (size of bank - $6000)

Decompress_SubCPUProgram:
		lea	(SubCPU_Program).l,a0			; a0 = compressed sub CPU program
		lea (program_ram+sp_start).l,a1				; a1 = start of user sub CPU program in first program RAM bank
		move.w	(a0)+,d0				; d0 = total number of modules

		moveq	#(sizeof_program_ram_window-sp_start)/sizeof_module,d5	
		cmp.w	d5,d0
		bls.s	.nobankswitch			; branch if module count is 13 or less (no bankswitching required)
		sub.w	d5,d0					; deincrement module counter
		moveq	#(sizeof_program_ram_window-sp_start)/sizeof_module-1,d7	; 13 modules in first bank - 1 

		pushr.w	d0	
		bsr.s	.decompress				; decompress the first 13 modules to first bank
		popr.w	d0
		
		move.b	(a3),d6		; get current bank setting
		addi.b	#$40,d6
		move.b	d6,(a3)		; advance to next bank
		lea (program_ram).l,a1		; return to start of program RAM window
	
		moveq	#sizeof_program_ram_window/sizeof_module,d5	
		cmp.w	d5,d0					; will we need to bankswitch to third bank?
		bls.s	.nobankswitch			; branch if not
		sub.w	d5,d0					; deincrement module counter
		moveq	#(sizeof_program_ram_window/sizeof_module)-1,d7	; 16 modules in second bank
		
		pushr.w	d0	
		bsr.s	.decompress				; decompress the next 16 modules to second bank
		popr.w	d0
		
		move.b	(a3),d6		; get current bank setting
		addi.b	#$40,d6
		move.b	d6,(a3)		; advance to next bank
		lea (program_ram).l,a1		; return to start of program RAM window
		
		moveq	#sizeof_program_ram_window/sizeof_module,d5	
		cmp.w	d5,d0					; will we need to bankswitch to fourth bank?
		bls.s	.nobankswitch			; branch if not
		sub.w	d5,d0					; deincrement module counter
		moveq	#(sizeof_program_ram_window/sizeof_module)-1,d7		; 16 modules in second bank
		
		pushr.w	d0	
		bsr.s	.decompress				; decompress the next 16 modules to third bank
		popr.w	d0
				
		move.b	(a3),d6		; get current bank setting
		addi.b	#$40,d6
		move.b	d6,(a3)		; advance to final bank
		lea (program_ram).l,a1		; return to start of program RAM window
		
	.nobankswitch:
		subq.w	#1,d0					; adjust for loop counter
		move.w	d0,d7					; decompress all remaining modules

	.decompress:
		bsr.w	KosDec			; decompress the module
		dbf	d7,.decompress		; repeat until end of bank or end of data
		rts

		
		
		
