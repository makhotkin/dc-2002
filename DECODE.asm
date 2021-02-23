; Project DC 2002
; Decode engine
; (c) 2002 Trance_C[RootTeam]

.model	small
.stack 	100h

include		i586_fl.inc

.data

include	i586.inc

s_OpCode	db	16 dup (0)	; quite enough for "Repne scasb"			
s_Op1		db	16 dup (0)
s_Op2		db	32 dup (0)	; Uses 24+ bytes in some cases, need reserve!
s_Op3		db	16 dup (0)	

public	FinalCode			
FinalCode	db	64 dup (0)		; used by many modules, so declared here


public	DC_Def_State, EIPLocal		; should be used only in view module
DC_Def_State	dw	State_UseDS
EIPLocal	dd	0


.code
.386
EXTRN	strcpy_x:PROC, strnext:PROC
EXTRN	Int2Hex8f: PROC, Int2Hex16f: PROC, Int2Hex32f:PROC
PUBLIC	DecodeCmd



; Procedure	Decode_Check_Prefix
; Purpose	Determine if currenly loaded 1st byte is a prefix
; Output	If it is, set "carry" flag
Decode_Check_Prefix:
		cmp	al, 66h		; Change Operands prefix
		jnz	@DCP_Address	; Jump to next check if this is not a 66h prefix
		xor 	dx, State_Oper32	; Set special flag in dx register
					; It's unknown how CPU behaves if meets two equal prefixes before 
					; a command. So, in this program, two prefixes will negate each other
		stc
		ret
@DCP_Address:
		cmp	al, 67h		; Change address prefix
		jnz	@DCP_Check_ES	; See above...
		xor 	dx, State_Addr32
		stc
		ret
@DCP_Check_ES:
		cmp	al, 26h		; Use ES prefix		
		jnz	@DCP_Check_CS
		and	dx, State_RmSegMask	; Clear out segment reg settings
		or	dx, State_UseES	; Set ES as segment
					; This command is a bit strange (State_UseES = 0)
					; However, that would allow to change constants
		stc
		ret
@DCP_Check_CS:
		cmp	al, 2Eh		; Use CS prefix		
		jnz	@DCP_Check_SS
		and	dx, State_RmSegMask	; Clear out segment reg settings
		or	dx, State_UseCS	; Set CS as segment
		stc
		ret
		
@DCP_Check_SS:
		cmp	al, 36h		; Use SS prefix		
		jnz	@DCP_Check_DS
		and	dx, State_RmSegMask	; Clear out segment reg settings
		or	dx, State_UseSS	; Set SS as segment
		stc
		ret
		
@DCP_Check_DS:
		cmp	al, 3Eh		; Use DS prefix		
		jnz	@DCP_Check_FS
		and	dx, State_RmSegMask	; Clear out segment reg settings
		or	dx, State_UseDS	; Set DS as segment (This is default)
		stc
		ret
		
@DCP_Check_FS:
		cmp	al, 64h		; Use FS prefix		
		jnz	@DCP_Check_GS
		and	dx, State_RmSegMask	; Clear out segment reg settings
		or	dx, State_UseFS	; Set FS as segment
		stc
		ret

@DCP_Check_GS:
		cmp	al, 65h		; Use GS prefix		
		jnz	@DCP_check_Ok
		and	dx, State_RmSegMask	; Clear out segment reg settings
		or	dx, State_UseGS	; Set GS as segment
		stc
		ret
@DCP_check_Ok:
		clc	; No prefix found. NOTE: Rep(z/nz) are considered ordinary commands
		ret


; Procedure	DecodeCmd
; Purpose	Convert CPU code from OpCodes into human-readable form, that is 
;		mnemonic code.
; Input		ds:si - Points to code fragment
;		es:di - Points to memory reserved for decode string
; Output	2-Strings at es:di - command and operands divided by zero-byte, 
;		si is incrememted by command length
;		cx - command length (with all prefixes included)

DecodeCmd		PROC
; One-time Initialization
;		push	bp
;		mov	bp, sp
;		sub	sp, 4		; Stack	frame needed for unBalanced stack inside procedure
		
		mov	dx, DC_Def_State	; Set default flags state
		lea	bx, dc_Table_Main	; Translate using main table (default)
		xor	ax, ax		; Zero-out ax
		push	si		; Store starting point
		
@DC_start_1:		; Here we return if met a prefix instead of command
		lodsb	; Read COP-byte from memory location
		call	Decode_Check_Prefix
		jc	@DC_start_1

@DC_Repz_Check:
		cmp	al, 0f3h		; Rep/repe/repz - prefixes
		jnz	@DC_RepNz_Check
		mov	al, byte ptr [si]	; read next command
		push	di			; save commands
		movzx	cx, nREPZCodes
		lea	di, aREPZCodes
		repne	scasb
		pop	di
		mov	al, 0f3h
		jcxz	@DC_Translate
		or	dx, State_Pref_RepZ
		jmp	@DC_start_1
@DC_RepNz_Check:
		cmp	al, 0f2h		; Repne/repnz - prefixes
		jnz	@DC_2Byte_Check		
		mov	al, byte ptr [si]	; read next command
		push	di			; save commands
		movzx	cx, nREPZCodes
		lea	di, aREPZCodes
		repne	scasb
		pop	di
		mov	al, 0f2h
		jcxz	@DC_Translate
		or	dx, State_Pref_RepNZ
		jmp	@DC_start_1
	
@DC_2Byte_Check:					
		cmp	al, 0fh		; Here we shall switch to alternative table
					; if command begins with 0Fh
		jnz	@DC_Translate	
		lea	bx, dc_Table_Alt	; switching itself
		or	dx, State_TabFFh
		lodsb	
@DC_Translate:
		push	si
		shl	ax, 2		; Multiply by 4 - size of 'CMDDESC' structure
		mov	si, ax
		mov	cx, word ptr [bx+si].cmdName
		mov	bx, word ptr [bx+si].cmdFmt
		pop	si
		
@DC_Transl_ChkFmt:
		;test	cx, Fam_8087	; Check for "uncommon formats" disabled
		; should a jump lead us away from here?
		;jnz	@DC_Transl_Uncommon

		Call	Decode_OperationEx	; Resolve states "2(8)_Names", "Query_Regs", "Extract Condition"
		
;		test	bx, Mask_NoOperands
;		jz	@DC_Merge
		push	di
		Call	Decode_Int_Cmd		; Create string @ es:di
		jmp	@DC_Merge
@DC_Transl_Uncommon:		
	;	Call	Decode_Ext_Cmd		; Decode procedure for extended commands
		
	
@DC_Merge:	; Merge operands here
		pop	di
		push	si	; will return
		lea	si, s_OpCode	; Load Opcode offset

		call	strcpy_x		; Move it to destination string
		; Ending zero set by procedure is kept to divide command and operands
		
		test	bx, Mask_NoOperands	; If both operands were absent
		jz	@DC_Merge_Done	; needn't merge anything else
		mov	ax, bx		; Check operand order
		and	ax, Spec_Mask	
		test	ax, Spec_RevOrder
		jnz	@DC_Merge_Rev	; jump for reverse
@DC_Merge_1st:
		test	bx, Op1_Mask	; Test: is 1st operand present?
		jz	@DC_Merge_2nd		
		lea	si, s_Op1		; Load Opcode offset
		call	strcpy_x		; Move it to destination string
		dec	di		; Pointer set to ending zero
		mov	ax, bx
		and	ax, Op2_Mask
		cmp	ax, Op1_isOffset	; Skip operand 2 if field was used to store 1st param modificator
		jbe	@DC_Merge_3rd
		mov	ax, " ,"	
		stosw
@DC_Merge_2nd:		
		mov	ax, bx
		and	ax, Op2_Mask
		cmp	ax, Op1_isOffset
		jbe	@DC_Merge_3rd
		lea	si, s_Op2		; Load Opcode offset
		call	strcpy_x		; Move it to destination string
		dec	di		; Pointer set to ending zero
		jmp	@DC_Merge_3rd
@DC_Merge_Rev:			
		mov	ax, bx
		and	ax, Op2_Mask
		cmp	ax, Op1_isOffset
		jz	@DC_Merge_1st_r		
		lea	si, s_Op2		; Load Opcode offset
		call	strcpy_x		; Move it to destination string
		dec	di			; Pointer set to ending zero
		test	bx, Op1_Mask
		jz	@DC_Merge_3rd
		mov	ax, " ,"	
		stosw
@DC_Merge_1st_r:		
		test	ax, Op1_Mask
		jz	@DC_Merge_3rd
		lea	si, s_Op1		; Load Opcode offset
		call	strcpy_x		; Move it to destination string
		dec	di		; Pointer set to ending zero
;		jmp	@DC_Megre_3rd
@DC_Merge_3rd:	
		mov	ax, bx		; Load command descriptor
		and	ax, Spec_Mask2
		cmp	ax, Spec_ExtraI8
		jb	@DC_Merge_Done
		test	bx, Mask_NoOperands
		jz	@DC_Merge_3rd_only
		mov	ax, " ,"	
		stosw
@DC_Merge_3rd_only:		
		lea	si, s_Op3		; Load Opcode offset
		call	strcpy_x		; Move it to destination string
		dec	di
;		jmp	@DC_Merge_Done

@DC_Merge_Done:
		xor	al, al
		stosb			; Ending zero is needed in any case
	
@DC_Done:		
		pop	si
		pop	cx		
		sub	cx, si
		neg	cx
		movsx	ecx, cx
		mov	eax, EIPLocal
		add	eax, ecx
		mov	EIPLocal, eax
;		mov	sp, bp
;		pop	bp
		ret
DecodeCmd		ENDP


; Purpose		Find out and Store Operation symbolic name into s_Op
; Input		bx 	- CmdDesc 
;		cx	- offset to corresponding symbolic data
;		ds:si	- offset to code (for post-byte checks)
; Output		s_opCode	- Opcode in ASCIIZ string
;		bx	- (store)
;		si 	- (store)

Decode_OperationEx	PROC	; Resolve "2(8)_Names", "Query_Regs", "Extract Condition", rep-prefixes
		push	bp
		mov	bp, sp
		sub	sp, 2
Ocode	equ	[bp-2]

		mov	ocode, si

		push	di
		mov	di, offset s_OpCode
@DC_OpEx_Check_Prefix:
		mov	ax, dx
		and	ax, 3	; Save only Rep(n(z)) prefixes
		cmp	ax, State_Pref_Rep
		jnz	@DC_OpEx_Repz
		lea	si, cmRep
		
@DC_OpEx_OutPref:		
		call	strcpy_x
		mov	byte ptr [di-1], ' ' 
		jmp	@DC_OpEx_Common
		
@DC_OpEx_Repz:
		cmp	ax, State_Pref_Repz
		jnz	@DC_OpEx_Repnz
		lea	si, cmRepe
		jmp	@DC_OpEx_OutPref

@DC_OpEx_Repnz:
		cmp	ax, State_Pref_Repnz
		jnz	@DC_OpEx_Common
		lea	si, cmRepnz
		jmp	@DC_OpEx_OutPref

@DC_OpEx_Common:	
		mov	si, cx	
		mov	ax, bx		; Check operand order
		and	ax, Spec_Mask2	
		cmp	ax, Spec_2Names	; Dual Names handling
		jnz	@DC_OpEx_1
		test	dx, State_Oper32
		jz	@DC_OpEx_Final	; If using 16-bit mode, choose first name
		call	strnext			; switch si to second word
		jmp	@DC_OpEx_Final	; If using 32-bit mode, choose second name	
@DC_OpEx_1:				
		cmp	ax, Spec_8Names
		jnz	@DC_OpEx_Final
		push	bx
		mov	bx, Ocode
		mov	al, byte ptr [bx]			
		pop	bx
		and	ax, 38h		; keep reg only
		shr	ax, 3
		mov	cx, ax
		jcxz	@DC_OpEx_Final
@DC_OpEx_1a:
		call	strnext
		loop	@DC_OpEx_1a
;		jmp	@DC_OpEx_Final		
		
@DC_OpEx_Final:	; need final opcode in si, final attributes @ bx
		call	strcpy_x
		Mov	ax, bx
		and	ax, Spec_Mask2
		cmp	ax, Spec_GetCond
		jnz	@DC_OpEx_Done
		dec	di
		push	bx
		mov	bx, Ocode
		mov	al, byte ptr [bx-1]			
		pop	bx
		xor	ah, ah
		and	al, 0fh
		shl	al, 2
		lea	si, cmCond		
		add	si, ax
		call	strcpy_x		
		
@DC_OpEx_Done:		
		pop	di
		mov	si, Ocode

		mov	sp, bp
		pop	bp
		ret
Decode_OperationEx	ENDP



; Procedure	Decode_Int_Cmd
; Purpose		Restore command parameters from opcode descriptors
; Input		ds:si - pointer to source code, set behind main command byte
;		bx    - cmdFmt field of CMDDESC for command being decoded
;		dx    - Prefixes and addressing modes state
; Output		Written @ s_op1, s_op2, s_op3
;		si    - (store)+offset(s)
;		di    - undefined
;		bx    - (store)
;		dx    - (store)
Decode_Int_Cmd	PROC
;
		push	bp	; Stack frame will allow us to make 
		mov	bp, sp	; what we wish on stack inside procedure
		sub	sp, 02h
b_1st_byte	equ	[bp-2]	; 1 st byte gets here
b_post_byte	equ	[bp-1]	; post byte gets here (if it's needed)
; Notes on algorithm:
;	1. Check if first operand requires post-byte
;	2. The same for second argument
; // Theese  2 steps needed to adjust si, so that it points to next function on exit
;	3. Decode 2nd parameter into string
;	4. Decode 1st operand into string
;	5. Decode 3rd parameter (if any)
;	6. Cat all 3 (or as many as we got) parameters into single line 
;	7. Exit!
		push	word ptr	[si-1]
		pop	word ptr	b_1st_byte
		
@DCI_1:
		mov	ax, bx
		and	ax, 0fh	; Mask 1-operand information
		cmp	al, Op1_reg8	; al should be >= Op1_reg8,
		jb	@DCI_2	
		cmp	al, Op1_reg1632	; al <= Op1_Op1_reg1632
		jbe	@DCI_2P
		
		; What to if RESERVED is used?	
		
		cmp	ax, Op1_SPreg	;  Special register coded bu postbyte! + Reg32 in pb
		jnz	@DCI_2
		lea	di, s_op2
		push	word ptr b_1st_byte
		call	Decode_Reg32
		inc	si	; Postbyte was used!
		or 	bx, Op2_RegExtr8	; reg decode will be skipped anyway (Used for MERGE)
		jmp	@DCI_4	; Skip to 1-st operand routines
@DCI_2:
		mov	ax, bx
		test	ax, Op2_Memory	; Query 2nd operand information
		jz	@DCI_3
@DCI_2P:
		inc	si		; to skip over post-byte		
@DCI_3:		
		; This exception is not handled by command format, so we have to handle it here
		;  Opcode D6: xx01xxxx - Not and Neg commands use 1 operand unlike test and div/mul 
		; that use the same byte D6 (Commonly 8-names command use same opernds size and quantity)
		mov	ax,  word ptr b_1st_byte
		cmp	al, 0F6h
		jb	@DCI_3_alt
		cmp	al, 0F7h
		ja	@DCI_3_alt
		and	ah, 30h	; extract bits 4,5 (00xx0000)
		cmp	ah, 10h
		jnz	@DCI_3_alt
		and	bx, 0fff0h	; Set Op1_None
@DCI_3_alt:		
		mov	ax, bx
		and	ax, Op2_Mask	; Query 2nd operand information


@DCI_3check_IO:		
		cmp	ax, Op1_isOff_al
		jz	@DCI_3_ImmOffs
		cmp	ax, Op1_isOff_ax
		jz	@DCI_3_ImmOffs
		
		test	al, Op2_Memory	; If got memory location analyse it further
		jz	@DCI_3_a
		lea	di, s_op2
		push	word ptr	b_1st_byte
		call	Decode_Int_RM	; This should return well-formed Op2 
					; (if it was in memory or reg, pointed by RM)
		jmp	@DCI_4
		
@DCI_3_ImmOffs:
		or	dx, State_Op1_MemOffset ; Set flag only 
		lea	di, s_op2
		push	word ptr b_1st_byte
		call	Decode_Int_RM	; call to Op2 decoder, with special flag set
		mov	ax, bx
		and	ax, Op2_Mask	; Query 2nd operand information		

@DCI_3_a:	; Initial check (if operand is present)
		test	al, Op2_Mask	; Compare with zero-operand state
		jz	@DCI_4
		lea	di, s_Op2
		
@DCI_3a:	; 8-bit Extract registers
		cmp	al, Op2_RegExtr8	; This code is for cmds with flag "Op2_RegExtr8"
		jnz	@DCI_3b		; Skip if command does not have that flag
		mov	al, byte ptr b_1st_byte	; Get 1st byte 
		and	al, 07h		; Mask last 3 bits (they mean register)
		shl	ax, 2		; now ax = offset in registers table
		push 	si		; Save source-code location
		lea	si, sn8bit	; 8-bit registers symbol-names table, dword aligned
		add	si, ax		; source-string ready
		call	strcpy_x		; String Copying macros
		pop	si		; restore offset of code
		jmp	@DCI_4
				
@DCI_3b:	; 16(32)-bit Extract registers
		cmp	al, Op2_RegExtr16	; Check condition
		jnz	@DCI_3c		; Keep checking if wrong case
		mov	al, byte ptr b_1st_byte
		and	al, 07h
		shl	ax, 2		; ax = offset in registers table
		push 	si
		test	dx, State_Oper32
		jnz 	@DCI_3b_32
		lea	si, sn16bit	; 16-bit registers symbol-names table, dword aligned
		jmp	@DCI_3b_common
@DCI_3b_32:		
		lea	si, sn32bit	; 32-bit registers symbol-names table, dword aligned		
@DCI_3b_common:
		add	si, ax		; source-string ready
		call	strcpy_x
		pop	si		; restore offset of code
		jmp	@DCI_4
		
@DCI_3c:	; 8-bit Accumulator used
		cmp	al, Op2_Al	; case Op2_al
		jnz	@DCI_3d		; wrong case?
@DCI_3c_e:		
		push	si
		lea	si, snAL
		call	strcpy_x		
		pop	si
		jmp	@DCI_4
				
@DCI_3d:	; 16(32)-bit Accumulator used

		cmp	al, Op2_ax	; Check condition
		jnz	@DCI_3e		; Keep checking if wrong case
@DCI_3d_e:		
		push 	si
		test	dx, State_Oper32
		jnz 	@DCI_3d_32
		lea	si, snAX		; offset for name of 16-bit accumulator
		jmp	@DCI_3d_common
@DCI_3d_32:		
		lea	si, snEAX		; offset for name of 32-bit accumulator		
@DCI_3d_common:
		call	strcpy_x		; StrCpy inside macro
		pop	si		; restore offset of code
		jmp	@DCI_4
				
@DCI_3e:	; Marker, set when operand 1 is (e)ip-relative offset
		cmp	al, Op1_isOffset	; it's about operand 1 (such a mudificator)
		jnz	@DCI_3f	
		
		or	dx, State_Op1_CodeOffset ; Set flag only
;		jmp	@DCI_4		
@DCI_3f:
		cmp	al, Op1_isOff_ax
		jnz	@DCI_3g
		lea	di, s_op1
		jmp	@DCI_3d_e

@DCI_3g:
		cmp	al, Op1_isOff_al
		jnz	@DCI_4
		lea	di, s_op1
		jmp	@DCI_3c_e		

@DCI_4:		
		mov	ax, bx		; Extract information from command descriptor
		and	ax, Op1_Mask	; Clear all, except 1-parameter info
		
		cmp	al, Op1_None
		jz	@DCI_5
		lea	di, s_op1		; Write any result to Op1 buffer
		
@DCI_4a:	; Operand 1 is equal to "1"
		cmp	al, Op1_is_1	; case Op1_is1:
		jnz	@DCI_4a_a
		mov	ax, 31h
		stosw
		jmp	@DCI_5
		
@DCI_4a_a:	; Operand 1 is "cl"
		cmp	al, Op1_Cl	; case Op1_Cl	; Needed for "Shifts"octet
		jnz	@DCI_4b
		push	si
		lea	si, snCL
		call	strcpy_x
		pop	si
		jmp	@DCI_5		

@DCI_4b:	; Operand 1 is "dx"
		cmp	al, Op1_Dx	; case Op1_Dx:
		jnz	@DCI_4c
		push	si
		lea	si, snDX	
		call	strcpy_x
		pop	si
		jmp	@DCI_5
		
@DCI_4c:	; Operand 1 is encoded in lowest bits of first byte
		cmp	al, Op1_regExtr16	; case Op1_regExtr16
		jnz	@DCI_4d		;
		
		mov	al, b_1st_byte	
		and	al, 07h		; al = (b_1st_byte) & 0x7;	// array index
		shl	ax, 2		; ax = al * sizeof(DWORD);	// array beginning offet
		push 	si		
		test	dx, State_Oper32	; if (32_bit_mode)
		jnz 	@DCI_4c_32
		lea	si, sn16bit	; si = pointer to 16-bit registers names
		jmp	@DCI_4c_common
@DCI_4c_32:		
		lea	si, sn32bit	; else choose 32-bit names array		
@DCI_4c_common:
		add	si, ax		; create address from base & offset
		call	strcpy_x		; copy what we got into s_Op1
		pop	si		; si is back, it points to code 
		jmp	@DCI_5		; break;
		
@DCI_4d:	; Operand is 8-bit register, must be extracted from 'reg' of post-byte 		
		cmp	al, Op1_reg8
		jnz	@DCI_4e
		mov	al, b_post_byte
		and	al, 38h		; bits 3-5 will be saved. we get 00xxx000
		shr	al, 1		; instd. of shr 3, shl 2
		push	si
		lea	si, sn8bit	; need 8-bits registers array 
		add	si, ax		; indexing done
		call	strcpy_x		
		pop	si
		jmp	@DCI_5		; break; 

@DCI_4e:	; Operand is 16-bit register, must be extracted from 'reg' of post-byte
		cmp	al, Op1_reg16
		jnz	@DCI_4f
		mov	al, byte ptr b_post_byte
		and	al, 38h		; bits 3-5 will be saved. we get 00xxx000
		shr	al, 1		; instd. of shr 3, shl 2
		push	si
		lea	si, sn16bit	; need 16-bits registers array 
		add	si, ax		; indexing done
		call	strcpy_x		
		pop	si
		jmp	@DCI_5		; break; 
		
@DCI_4f:	; Operand is 16(32)-bit register, must be extracted from 'reg' of post-byte 		
		cmp	al, Op1_reg1632
		jnz	@DCI_4g
		mov	al, byte ptr b_post_byte
		and	al, 38h		; bits 3-5 will be saved. we get 00xxx000
		shr	al, 1		; instd. of shr 3, shl 2
		push	si
		test	dx, State_Oper32	; if (32_bit_mode)
		jnz 	@DCI_4f_32
		lea	si, sn16bit	; si = pointer to 16-bit registers names
		jmp	@DCI_4f_common
@DCI_4f_32:		
		lea	si, sn32bit	; else choose 32-bit names array		
@DCI_4f_common:
		add	si, ax		; indexing done
		call	strcpy_x		
		pop	si
		jmp	@DCI_5		; break; 		

@DCI_4g:	
;@DCI_4h:	; Operand is control/debug/"t" register, must be extracted from 'reg' of post-byte
		cmp	al, Op1_SPreg
		jnz	@DCI_4i
		; Internal registers processing
		
		; Number of register
		xor	ax, ax
		mov	al, byte ptr b_post_byte
		and	al, 38h		; bits 3-5 will be saved. we get 00xxx000
		add	ax, 30h*8	; shifted 3-left,  ascii-zero base
		shl	eax, 13		; shift reg index down to lowest bits
		; Saved reg number in hi-word of eax
		xor	ax, ax
		mov	al, byte ptr b_1st_byte
		and	al, 5h		; save 00000x0x
		ror	ax, 1		; save x in 15-bit, another in 1st bit
		shr	al, 1		; save x in 15-bit, another in 0th bit
		rol	ax, 1		; save x's in bits 0,1
		shl	ax, 2		; get offset in dword array
		push	si
		lea	si, snCR
		add	si, ax
		lodsw
		pop	si
		; Insert register check here (Optional)
		stosd
		jmp	@DCI_5		; break; 	

@DCI_4i:	; Operand is segment register, must be extracted from 'reg' of post-byte
		cmp	al, Op1_sreg386_pb
		jnz	@DCI_4i_1
		mov	al, byte ptr b_post_byte
@DCI_4ia: ; used for jump after sreg-386 bits extracted
		and	al, 38h		; bits 3-5 will be saved. we get 00xxx000
		shr	al, 1		; instd. of shr 3, shl 2
		push	si
		lea	si, snSegReg	; need 8-bits registers array 
		add	si, ax		; indexing done
		call	strcpy_x		
		pop	si
		jmp	@DCI_5		; break; 
@DCI_4i_1:	; Sreg is locatd in "main" byte
		cmp	al, Op1_sreg386
		jnz	@DCI_4j
		mov	al, byte ptr b_1st_byte
		jmp	@DCI_4ia		
		
@DCI_4j:	; Operand is 8-bit immediate
		cmp	al, Op1_Imm8
		jnz	@DCI_4k
		lodsb			; load immediate into AL
		test	dx, State_Op1_CodeOffset ; check if it is an offset
		jz	@DCI_4jb		; jump if imm8 is simple operand
		cbw
		cwde
		add	eax, 2
@DCI_4ja:	; This point used to output 32-bit offsets	
		; Add correction by command length
		test	dx, State_TabFFh
		jz	$+4
		inc	eax	
		add	eax, EIPLocal	; Get curent pseudo-ip
		call	Int2Hex32f	; Output by procedure
		jmp	@DCI_5
@DCI_4jb:	; Immediate is just an operand (no offsets)
		call	Int2Hex8f	; pure output here
		jmp	@DCI_5
		
@DCI_4k:	; Operand is 16-bit immediate	(offset inapplicable)
		cmp	al, Op1_Imm16
		jnz	@DCI_4l
@DCI_4ka:	; Will Be Used as output routine for 16-bit numbers
		lodsw			
		call	Int2Hex16f
		jmp	@DCI_5
@DCI_4l:	; Used to parse Immediates of 2 (4) bytes
		cmp	al, Op1_Imm1632	; Check if got into correct handler
		jnz	@DCI_4m
		test	dx, State_Op1_MemOffset
		jnz	@DCI_5			; skip any output, already processed
		test	dx, State_Op1_CodeOffset	; Operand 1 is offset?
		jz	@DCI_4la		; stay if it is

		test	dx, State_Oper32	; Operating 32-bit offset?
		jnz	@DCI_4l32		; if so, there's another word
		lodsw			; Load 2-bytes from code
		cwde
		add	eax, 3
		jmp	@DCI_4ja		; if so, there's another word
@DCI_4l32:		
		lodsd			; load it
		add	eax, 5
		jmp	@DCI_4ja		; jump to abs address calculation
		
@DCI_4la:	; Non-offset 16/32 immediates processing
		test	dx, State_Oper32
		jz	@DCI_4ka
		lodsd
		call	Int2Hex32f
		jmp	@DCI_5
@DCI_4m:	; Last possible way: i16 + i32/16 (far calls and jumps, LDS)
		test	dx, State_Oper32
		jnz	@DCI_4ma
		lodsw
		call	Int2Hex16f
		jmp	@DCI_4mb
@DCI_4ma:		
		lodsd
		call	Int2Hex32f
@DCI_4mb:		
		mov	al, ':'
		stosb
		lodsw
		call	Int2Hex16f
		dec	di
		jmp	@DCI_5
		
		
@DCI_5:
		mov	ax, bx
		and	ax, Spec_Mask
		lea	di, s_op3
		
@DCI_5a:
		cmp	ax, Spec_ExtraI8
		jnz	@DCI_5b
		lodsb			; load immediate into AL
		call	Int2Hex8f
		jmp	@DCI_6
@DCI_5b:
		cmp	ax, Spec_ExtraI1632
		jnz	@DCI_5c
		lodsw
		test	dx, State_Oper32
		jz	@DCI_5ba
		push 	dx
		xchg	ax, dx
		lodsw
		xchg	ax, dx
		call	Int2Hex32f
		pop	dx
		jmp	@DCI_6
@DCI_5ba:		
		call	Int2Hex16f
		jmp	@DCI_6		
@DCI_5c:
		cmp	ax, Spec_Op1_Is_3
		jnz	@DCI_5d
		mov	ax, 33h
		stosw
		jmp	@DCI_6	
@DCI_5d:
		cmp	ax, Spec_Extra_cl	; Specific 3rd operand for double-shifts
		jnz	@DCI_6
		push	si
		lea	si, snCL
		call	strcpy_x
		pop	si		
;		jmp	@DCI_6	
	
@DCI_6:		
		mov	sp, bp
		pop	bp
		ret
Decode_Int_Cmd	endp		
		


; Special Entry point to explicitly decode rm as 32-bit register
Decode_Reg32	PROC	PASCAL, cmd:word
	or	dx, State_Oper32
	jmp	@DCIRM_1c_backdoor		; special label into reg16/32 handler
	ret	; won't get here ;)
Decode_Reg32 endp	; I don't need this... compiler does

; Procedure	Decoder_Int_RM
; Purpose	Save operand defined by Mod+R/M
; Input		ds:si 	= seg:offset for code string (post-byte already extracted)
;		bx    - cmdFmt field of CMDDESC for command being decoded
;		dx    - Prefixes and addressing modes state
; Output	Written to string @ es:di
Decode_Int_RM	PROC	PASCAL, cmd:word
; Algorithm:
;	1. Decode MD field, if it is register, write it and terminate
;	2. Memory: get pointer size, save it into string
;	3. Process immediates (if any)

;	0. test Situation "Offset in immediate"	
		test	dx, State_Op1_MemOffset
		jnz	@DCIRM_1_MemImm

@DCIRM_1:	; Checking out "11" state for MD bits	
		mov	ax, cmd
		shr	ax, 14	; leave only rm field!
		cmp	al, 3	; Mod = 11 ~ register addressing
		jnz	@DCIRM_2
		jmp	@DCIRM_1pre
@DCIRM_1_MemImm:
		; If got here, should enclose immediate into sq. brackets and set its size
		mov	al, '['
		stosb
		test	dx, State_Addr32
		jnz	@DCIRM_1_32
		lodsw
		call	Int2Hex16f
		dec	di
		jmp	@DCIRM_2x
@DCIRM_1_32:	
		lodsd
		call	Int2Hex32f
		dec	di
		jmp	@DCIRM_2x		
@DCIRM_1pre:		
		mov	ax, bx
		and	ax, Op2_Mask
		
@DCIRM_1a: ; Operand is 8-bit register, must be extracted from 'r/m' of post-byte 		
		cmp	ax, Op2_RM8
		jnz	@DCIRM_1b

		mov	ax, cmd
		mov	al, ah
		and	ax, 07h
		shl	al, 2
		push	si
		lea	si, sn8bit	; need 8-bits registers array 
		add	si, ax		; indexing done
		call	strcpy_x		
		pop	si
		jmp	@DCIRM_X		; break; 

@DCIRM_1b: ; Operand is 16-bit register, must be extracted from 'r/m' of post-byte 		
		cmp	ax, Op2_RM16
		jnz	@DCIRM_1c	
			
		mov	ax, cmd
		mov	al, ah
		and	ax, 07h
		shl	al, 2
@DCIRM_1ba:		
		push	si
		lea	si, sn16bit	; need 8-bits registers array 
		add	si, ax		; indexing done
		call	strcpy_x		
		pop	si
		jmp	@DCIRM_X		; break; 
		
@DCIRM_1c:	; 16(32)-bit register operand
		cmp	ax, Op2_RM1632
		jnz	@DCIRM_1x
@DCIRM_1c_backdoor:		
		mov	ax, cmd
		mov	al, ah
		and	ax, 07h
		shl	al, 2
		
		test	dx, State_Oper32
		jz	@DCIRM_1ba
		
		push	si
		lea	si, sn32bit	; need 8-bits registers array 
		add	si, ax		; indexing done
		call	strcpy_x		
		pop	si
		jmp	@DCIRM_X		; break; 	
		

@DCIRM_1x:	; Other combinations are Forbidden (ex. reg cannot be 64-bits on x86)
		mov	eax, "???"
		stosd
		jmp	@DCIRM_X
		
		
		
@DCIRM_2:
;	Output memory operand size if needed
	mov	ax, bx	; load command descriptor
	and	ax, 0f0h
	mov	ah, al	; Ah - 2nd operand info
	mov	al, bl
	and	al, 0fh	; Al - 1operand info
	; If thier sizes do not match or cannot be understood from disassembly,
	; We should type pointer type
	cmp	ah, Op2_RM8
	jnz	@DCIRM_2mc_1
	cmp	al, Op1_reg8
	jz	@DCIRM_2mc_1
	push	si
	lea	si, sn_byte
	call	strcpy_x
	dec	di
	pop	si
	jmp	@DCIRM_2seg

@DCIRM_2mc_1:
	cmp	ah, Op2_RM16
	jnz	@DCIRM_2mc_2
	test	dx, Spec_ExtraI8	; force if extra op/cond. action
	jnz	@DCIRM_2mc_1s	
	cmp	al, Op1_reg16
	jz	@DCIRM_2mc_2
@DCIRM_2mc_1s:
	push	si
	lea	si, sn_word
	call	strcpy_x
	dec	di
	pop	si
	jmp	@DCIRM_2seg	
	
@DCIRM_2mc_2:
	cmp	ah, Op2_RM1632	; case select
	jnz	@DCIRM_2mc_3
	test	dx, Spec_ExtraI8	; force if extra op/cond. action
	jnz	@DCIRM_2mc_2s
	cmp	al, Op1_reg1632	; skip if opreand in reg. has same size
	jz	@DCIRM_2mc_3
@DCIRM_2mc_2s:
	push	si
	test	dx, State_Oper32
	jnz	@DCIRM_2mc32
	lea	si, sn_word
	jmp	@DCIRM_2mc2com
@DCIRM_2mc32:
	lea	si, sn_dword
@DCIRM_2mc2com:
	call	strcpy_x
	dec	di
	pop	si
	jmp	@DCIRM_2seg	

@DCIRM_2mc_3:

@DCIRM_2seg:
;	Check segment subst
		mov	ax, dx
		and	ax, State_SegMask
		cmp	ax, State_UseDS
		jz	@DCIRM_2_a	; DS is default, needn't fix code
		shr	ax, 6	; Indexing ...
		push	si
		lea	si, snSegReg	; need 32-bits registers array 
		add	si, ax		; indexing done
		call	strcpy_x		; copy needed register name
		pop	si
		dec 	di
		mov	al, ':'
		stosb	
		
@DCIRM_2_a:	; done with segments, now checking addressing mode		
		mov	al, '['
		stosb
		test	dx, State_Addr32
		jnz	@DCIRM_32		; jump to 32-bit processing algorithms
		; 16-bit processing
		mov	ax, cmd
		and	ax, 700h		; save mod & r/m
		cmp	ah, 6		; exclusion for 16-bit pure-offset
		jnz	@DCIRM_2a		; If not triggered, use common rules
		mov	ax, cmd
		test	ax, 0C000h
		jnz	@DCIRM_2a		; Must be 0 in md for exception
		lodsw
		call	Int2Hex16f
		dec 	di
		mov	ax, word (0 + '[')
		stosw
		jmp	@DCIRM_2x
@DCIRM_2a:	; Exceptions handled, common rules follow
		mov	ax, cmd
		mov	al, ah
		and	ax, 7		; Store r/m only
		shl	ax, 2		; scaling by 8 - alignment of array
		push	si
		test	dx, State_Addr32
		jnz	@DCIRM_2a_32
		shl	ax, 1
		lea	si, snAddr16	; need 16-bits registers array 
		jmp	@DCIRM_2a_com
@DCIRM_2a_32:
		lea	si, sn32bit
@DCIRM_2a_com:		
		add	si, ax		; indexing done
		call	strcpy_x
		dec	di		
		pop	si
		
		mov	ax, cmd
		mov	al, ah		; store post-byte in al
		and	ax, 0C0h		; store md

	; Offset =?= 8 bit		
		cmp	ax, 40h		; 8-bit offset
		jnz	@DCIRM_2b
		mov	al, '+'
		stosb
		lodsb		
		call	Int2Hex8f
		dec	di
		jmp	@DCIRM_2x
		
@DCIRM_2b:	; Offset =?= 16 bit?
		cmp	ax, 80h		; 16-bit offset
		jnz	@DCIRM_2x
		mov	al, '+'
		stosb
		lodsw		
		call	Int2Hex16f
		dec 	di				
;		jmp	@DCIRM_2x				
@DCIRM_2x:	; finalization part
		mov	ax, ']'
		stosw
		jmp	@DCIRM_X
@DCIRM_32:	; 32-bit addresses 
		mov	ax, cmd
		and	ax, 0C700h	; save mod & r/m
		cmp	ah, 5		; exclusion for 32-bit pure-offset
		jnz	@DCIRM_32a	; If not triggered, use common rules
		lodsd
		call	Int2Hex32f
		dec 	di
		mov	ax, ']'
		stosw
		jmp	@DCIRM_X				
@DCIRM_32a:	;check for sib 
		and	ah, 0fh
		cmp	ah, 4		; using S-I-B addresses
		jnz	@DCIRM_32b
		;;; Sib HERE
		; Hint: Addr = S * I + B
		; If  I=[esp] - no index and scaling
		; If  B=[ebp] = Md=0 => [off32]
		;	  	Md=1 => [ebp+off8]
		;		Md=2 => [ebp+off32]
		;		Md=3 (impossible - checked before and means register addresation)
		push	bx
		lodsb
		movzx	bx, al
		
		and	ax, 07h	; keep bytes 0-2 to calculate base
		and	bl, 0f8h	; base bits not needed ;)
		cmp	al, 5		; ebp is not always present!
		jnz	@DCIRM_SIB_Base_ok
		mov	ax, cmd
		mov	al, ah		; store post-byte in al
		and	ax, 0C0h		; store md
		test	ax, ax 
		jz	@DCIRM_SIB_skip_EBP
		push	ax
		push	si
		lea	si, snEBP
		call	strcpy_x
		pop	si
		pop	ax
		dec	di
;		jmp	@DCIRM_SIB_IndexCheck
		or	bl, 4		; Flag that ebp was typed, may use "+" if need to
@DCIRM_SIB_skip_EBP:
		rol	al, 2		; rotate md from 6-7 to 0-1 in al
		inc	al	
		or	bl, al		; bl = SS,III,0md  (each letter=1 bit)
					; md = 0 - no offset
					; md = 1, 3 - Offset 32
					; md = 2 - offset 8
		jmp	@DCIRM_SIB_IndexCheck
@DCIRM_SIB_Base_ok:
		shl	ax, 2		; scaling by 4 - alignment of array
		push	si
		lea	si, sn32bit	; need 16-bits registers array 
		add	si, ax		; indexing done
		call	strcpy_x		
		pop	si
		dec	di		; eat ending zero
		or	bl, 4		; Flag that base was typed, may use "+" if need to

@DCIRM_SIB_IndexCheck:		
		mov	ax, bx
		and	ax, 038h	; keep bytes 3-5 to calculate index
		cmp	al, 020h	; 00100000 - index is void, scale unused
		jz	@DCIRM_Sib_Final
		
		shr	ax, 1		; ready for 32-bit register names indexing
		push	si
		lea	si, sn32bit	; need 32-bits registers array 
		add	si, ax		; indexing done
		
		test	bl, 4	
		jz	@DCIRM_SIB_NoEBP	; cannot type "plus" here
		
		mov	al, '+'		; load plus sign
		stosb			; to send into resulting string		
@DCIRM_SIB_NoEBP:		
		call	strcpy_x		; copy needed register name
		pop	si
		dec	di		; eat ending zero
		
		mov	ax, bx
		and	ax, 0C0h		; keep bytes 6-7 to calculate scale
		shl	ax, 2		; move scale to ah
		
		test	ah, ah		; if bits = 0, scale is 1, needn't multiplicaiton
		jz	@DCIRM_from_Sib	; scale = 1, needn't display it
		mov	cl, ah		; 
		mov	ah, 1
		shl	ax, cl		; ah = 2 ^ ah
		or	ax, 30h*256 + '*'	; ah = 'scale' (2, 4 or 8) in ASCII, al = '*'
		stosw	
@DCIRM_Sib_Final:
		mov	al, bl
		pop	bx	
		and	al, 3h
		cmp	al, 1h
		jnz	@DCIRM_from_Sib	; if not 1 all offsets will be set automatically
		jmp	@DCIRM_32c_s	; if 1, we need to set offset instead of [ebp] explicitly
		
			
@DCIRM_32b:
		shr	ax, 6		; scaling by 4 - alignment of array & move down from ah
		push	si
		lea	si, sn32bit	; need 16-bits registers array 
		add	si, ax		; indexing done
		call	strcpy_x
		dec	di		
		pop	si
		
@DCIRM_from_Sib:	
		mov	ax, cmd
		mov	al, ah		; store post-byte in al
		and	ax, 0C0h		; store md

		cmp	al, 040h
		jnz	@DCIRM_32c

		mov	al, '+'
		stosb
		lodsb		
		call	Int2Hex8f
		dec	di
		jmp	@DCIRM_2x		
		
@DCIRM_32c:
		cmp	al, 080h
		jnz	@DCIRM_2x
@DCIRM_32c_s:		
		mov	al, '+'
		stosb		
		lodsd
		call	Int2Hex32f
		dec 	di
		jmp	@DCIRM_2x		
					
@DCIRM_X:
		ret
Decode_Int_rm	endp

end