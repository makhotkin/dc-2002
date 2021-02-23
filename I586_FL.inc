; Project DC 2002
; Common Command format structure, definitions


CMDDESC		struc
cmdFmt		dw	?	; A pack of flags which help program to understand code
cmdName		dw	?	; Offset in data segment for ASCIIZ name of command
CMDDESC		ends



Mask_Op1	equ	0Fh	; Mask to extract information about 1st operand

Op1_None	equ	0h	; No openand
Op1_is_1	equ	1h	; Operand is 1 (used in shifts)
Op1_DX	equ	2h	; Operand is DX (IN, OUT and modifications)
Op1_CL	equ	3h	; Another free set
Op1_reg8	equ	4h	; Treat "reg"-field as 8-byte register operand
Op1_reg16	equ	5h	; Treat "reg"-field as 16-byte register operand
Op1_reg1632	equ	6h	; Same, operand is 32-bit if command prefixed by 66h
Op1_regExtr16	equ	7h	; Same as above, 3 lowest bits of 1st byte considered
Op1_Imm8	equ	8h	; After postbyte (if any) follows 1-byte immediate
Op1_Imm16	equ	9h	; Same, with 2-byte immediate
Op1_Imm1632	equ	0Ah	; Same, may be 4-bytes in 32-bit modes
Op1_Imm3248	equ	0Bh	; Same, prefixed by i16 (used by calls and jumps)
Op1_SPreg	equ	0Ch	; Postbyte "reg"-field is dr/cr/tr	; used w/rm32
Op1_Reserve	equ	0Dh	; Got free combination
Op1_sreg386_pb	equ	0Eh	; Bits 3-5 of post-byte mean segment register
Op1_sreg386	equ	0Fh	; Bits 3-5 of COP-byte mean segment register

Op1_Mask	equ	0Fh	; This combination is reserved

Op2_None	equ	00h	; No openand
Op1_isOffset	equ	10h	; Modificator, means immediate is ip-relative offset
Op1_isOff_al	equ	20h	; Immediate points to a byte in memory, second op is al
Op1_isOff_ax	equ	30h	; Immediate points to a (d)word in memory, second is (e)ax
Op2_RegExtr8	equ	40h	; Introduced due to some exceptions in COp format
Op2_RegExtr16	equ	50h	; Introduced due to some exceptions in COp format
Op2_al		equ	60h	; Operand is Accumulator low byte (al)
Op2_ax		equ	70h	; Operand is Accumulator (ax/eax)

Op2_Memory	equ	80h	; Operand 2 is memory location of undefined size
				; or user knows it's size for sure
				; The adress itself taken from post-byte
Op2_RM8	equ	90h	; "md" & "r/m" enclose 8-bit memory address or register
Op2_RM16	equ	0A0h	; Same, but 16-bit operand
Op2_RM1632	equ	0B0h	; Same, 32-bit if that mode is set
Op2_Mem3264	equ	0C0h	; Memory location: DWORD in 16-bits mode, 
				; QWORD in 32-bit one (Check where used)
Op2_Mem3248	equ	0D0h	; DWORD if 16-bit mode, FWORD in 32-bit (segment+offset)
Op2_Mem48	equ	0E0h	; 48-bit memory ( LES, LDS)
Op2_Reserve	equ	0F0h	; 64-Mem used by CMPXCHG8B (i586)

Op2_Mask	equ	0F0h	; Mask to extract 2nd parameter


Mask_NoOperands	equ	0FFh	; If theese bits set to zero, parameters are absent
				; or need no further processing


Spec_Ordinary	equ	000h	; This state is default, command is quite simple
Spec_GetCond	equ	100h	; Should Extract last 4-bits of 1st byte to get
				; command condition (Jcc, CMOVcc, SETcc)
Spec_2Names	equ	200h	; Command is called differently in 32-bit and 16-bit modes
				; CmdName points to 2 ASCIIZ strings
Spec_8Names	equ	300h	; 8 (or less) command of same format held in that code
				; CmdName points to 8 ASCIIZ strings, 
				; choice depends on 'reg' field
Spec_ExtraI8	equ	400h	; Extra 8-bits operand (used by ENTER and 3-operand IMUL)
Spec_ExtraI1632	equ	500h	; Extra Operand is 16/32 bits large (4 IMUL)
Spec_Op1_Is_3	equ	600h	; 1st operand is 3 (only for 0CCh - int 3)
Spec_Extra_cl	equ	700h	; Code holds several commands of different formats
				; CmdName is Sub-Table Offset (ex. FF - jump/call/inc)
				; Should check 'reg' field of post-byte
Spec_RevOrder	equ	800h	; In disassembly listing Op2 should go first (ARPL) 

Spec_Mask	equ	0F00h	; Mask to extract information about special conditions
Spec_Mask2	equ	0700h	; Mask to extract information about special conditions

; This is fairly advanced part of this project and is not obligatory, 
; So many records here mean "reserved for future versions" (if any)

; The main thing is 13th bit (first is number 1), if set it means Information about 
; Operands and special conditions is written in different form, so
; for this release a success in its check would mean "skip this very command"

Fam_8086	equ	0000h	; Command valid for all CPUs
Fam_8087	equ	1000h	; Command valid for all FPUs (and has special format)
Fam_80286	equ	2000h	; Command is ok for 80286 and below (no NPX)
Fam_80287	equ	3000h	; Command valid on 80286+80287 systems
Fam_80386	equ	4000h	; Same about 80386 w/o fpu
Fam_80387	equ	5000h	; Same about 80386 + 80387
Fam_i486	equ	6000h	; Valid for i486, no co-processor (that the task was)
Fam_i486DX	equ	7000h	; Valid for i486 with FPU, like 486DX2-66 
Fam_P54C	equ	8000h	; Pentium series new integer commands
Fam_P55C	equ	9000h	; MMX-commands
Fam_Piii	equ	0A000h	; Integer command introduced on Pentium Pro and later
Fam_SSE		equ	0B000h	; Streaming SIMD Extensions for Pentium III and above
Fam_Meta	equ	0E000h	; Decode all possible integer commands
Fam_MetaFPU	equ	0F000h	; Decode all known commands (same as above + SSE, MMX, FPU)
Mask_Family	equ	0F000h	; Mask to extract information about 
				; Processors supporting this command

; When Inside DecodeCmd procedure, some flags regarding command decoding are set in
; a register (variable?). Here are the symbolic meanings for each bit


; This should decribe immediate size counter
State_NoPref	equ	0h
State_Pref_Rep	equ	1h
State_Pref_Repz	equ	2h
State_Pref_Repnz	equ	3h

; bits 2 and 3 still free or reserved ;)

; Addressing mode and prefix modificators
State_Oper32	equ	010h
State_Addr32	equ	020h

; Command format (FPU, SSE, MMX)
; selects the way table is interpreted

State_Int_Cmd	equ	000h
State_FPU_Cmd	equ	040h
State_MMX_Cmd	equ	080h
State_SSE_Cmd	equ	0C0h

; Field combined with sreg386 specifiaction
; So, default setting should be 0300h
State_UseES	equ	0000h
State_UseCS	equ	0100h
State_UseSS	equ	0200h
State_UseDS	equ	0300h
State_UseFS	equ	0400h
State_UseGS	equ	0500h

State_RmSegMask	equ	0f0ffh	; Used to AND in case def. segment register is changed
State_SegMask	equ	00f00h	; Used to AND in need to know

State_Reserved1		equ	1000h
State_Op1_MemOffset	equ	2000h
State_TabFFh			equ	4000h	; #@$@! 0FFh opcode!
State_Op1_CodeOffset	equ	8000h	; Immediate is offset
