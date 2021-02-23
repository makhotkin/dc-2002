; Project 'DC 2002'
; Assembly commands descriptor structure and constants
; Assembly decode tables, command names

; NOTE: This module was built to provide further developement of 'Project DC",
; however, Extern Edition Includes only integer 486 commands, 


; Data and Index registers
; Typed by my own hands @ 15/09/02 11:55 am

; names aligned by 4-byte boundary
;ALIGN 16
sn8bit	label 	dword	
snAL	db	"al",0,0
snCL	db	"cl",0,0
snDL	db	"dl",0,0
snBL	db	"bl",0,0
snAH	db	"ah",0,0
snCH	db	"ch",0,0
snDH	db	"dh",0,0
snBH	db	"bh",0,0

; For thoose who don't understand: "sn" prefix means "Symbolic Name"


sn16bit	label	dword

snAX	db	"ax",0,0
snCX	db	"cx",0,0
snDX	db	"dx",0,0
snBX	db	"bx",0,0
snSP	db	"sp",0,0
snBP	db	"bp",0,0
snDI	db	"si",0,0
snSI	db	"di",0,0

; 32-bit registers symbloic names

sn32bit	label	dword

snEAX	db	"eax",0
snECX	db	"ecx",0
snEDX	db	"edx",0
snEBX	db	"ebx",0
snESP	db	"esp",0
snEBP	db	"ebp",0
snEDI	db	"esi",0
snESI	db	"edi",0

; Addressing modes in 16-bit systems 

snAddr16	label	QWORD
snA16_000	db	"bx+si", 3 dup (0)
snA16_001	db	"bx+di", 3 dup (0)
snA16_010	db	"bp+si", 3 dup (0)
snA16_011	db	"bp+di", 3 dup (0)
snA16_100	db	"si", 6 dup (0)
snA16_101	db	"di", 6 dup (0)
snA16_110	db	"bp", 6 dup (0)		; Remember! Bp is used only w/offset
snA16_111	db	"bx", 6 dup (0)


; Segment, Control and Debug Registers
; Made by HandS (respect to Yurov V. for his books)
; Completion moment: 15/09/02 12:11 am

; Sreg386 encoding
regES	equ	0
regCS	equ	1
regSS	equ	2
regDS	equ	3
regFS	equ 	4
regGS	equ	5

; We'll need to call them somehow inside program
snSegReg	label	dword
snES	db	"es",0,0
snCS	db	"cs",0,0
snSS	db	"ss",0,0
snDS	db	"ds",0,0
snFS	db	"fs",0,0
snGS	db	"gs",0,0
sn7	db	"???",0
sn8	db	"???",0

; Some place still remains... 
; let's type in here command counter names
snCmdCnt	label dword
snIP	db	"ip",0,0
snEIP	db	"eip",0,0

; No need to type in all names
snCR	db	"cr",0
ckCR	db	00011101b	; bytes maked as "0" mean absent registers
snDR	db	"dr",0
ckDR	db	11001111b	; The same about debug regiters
snTR	db	"tr",0
ckTR	db	0 ; Where to get this @$%#^@ table ?

sn_1	db	"1",0

sn_byte	db	"byte ptr ",0
sn_word	db	"word ptr ",0
sn_dword	db	"dword ptr ",0
sn_fword	db	"fword ptr ",0
sn_qword	db	"qword ptr ",0
sn_tbyte	db	"tbyte ptr ",0


; Conditions for jumps and similiar commands
cmCond	label	dword	
sn_o	db	"o",0,0,0
sn_no	db	"no",0,0
sn_b	db	"b",0,0,0
sn_ae	db	"ae",0,0
sn_z	db	"z",0,0,0
sn_nz	db	"nz",0,0
sn_be	db	"be",0,0
sn_a	db	"a",0,0,0

sn_s	db	"s",0,0,0
sn_ns	db	"ns",0,0
sn_p	db	"p",0,0,0
sn_np	db	"np",0,0
sn_l	db	"l",0,0,0
sn_ge	db	"ge",0,0
sn_le	db	"le",0,0
sn_g	db	"g",0,0,0


; Command Names list

; 80-83 Octet
cmCommon8	label	dword
cmADD	db	"add",0
cmOR	db	"or",0
cmADC	db	"adc",0
cmSBB	db	"sbb",0
cmAND	db	"and",0
cmSUB	db	"sub",0
cmXOR	db	"xor",0
cmCMP	db	"cmp",0

; D0-D3 Octet (and some other combinations)
cmShift8	label	dword
cmROL	db	"rol",0
cmROR	db	"ror",0
cmRCL	db	"rcl",0
cmRCR	db	"rcr",0
cmSHL	db	"shl",0
cmSHR	db	"shr",0
cmSAL	db	"sal",0
cmSAR	db	"sar",0

; F6-F7 Octet
cmTEST	db	"test",0
cmUNKNOWN	db	"???",0
cmNOT	db	"not",0
cmNEG	db	"neg",0
cmMUL	db	"mul",0
cmIMUL	db	"imul",0
cmDIV	db	"div",0
cmIDIV	db	"idiv",0

; FF deadliest octet
cmINC	db	"inc",0
cmDEC	db	"dec",0
cmCALL	db	"call",0
	db	"call",0
cmJMP	db	"jmp",0	
	db	"???",0
cmPUSH	db	"push",0
cm_0fBa	db	"???",0		; shared by 2 octets

	db	"???",0
	db	"???",0
	db	"???",0

cmBT	db	"bt",0
cmBTS	db	"bts",0
cmBTR	db	"btr",0
cmBTC	db	"btc",0

	

; Dual-names
cmCBW	db	"cbw",0
cmCWDE	db	"cwde",0

cmCWD	db	"cwd",0
cmCDQ	db	"cdq",0

cmINSB	db	"insb",0
cmINSW	db	"insw",0
cmINSD	db	"insd",0

cmOUTSB	db	"outsb",0
cmOUTSW	db	"outsw",0
cmOUTSD	db	"outsd",0

cmLODSB	db	"lodsb",0
cmLODSW	db	"lodsw",0
cmLODSD	db	"lodsd",0

cmSTOSB	db	"stosb",0
cmSTOSW	db	"stosw",0
cmSTOSD	db	"stosd",0

cmSCASB	db	"scasb",0
cmSCASW	db	"scasw",0
cmSCASD	db	"scasd",0

cmCMPSB	db	"cmpsb",0
cmCMPSW	db	"cmpsw",0
cmCMPSD	db	"cmpsd",0

cmMOVSB	db	"movsb",0
cmMOVSW	db	"movsw",0
cmMOVSD	db	"movsd",0

cmPOPA	db	"popa",0
cmPOPAD	db	"popad",0

cmPOPF	db	"popf",0
cmPOPFD	db	"popfd",0

cmPUSHA	db	"pusha",0
cmPUSHAD	db	"pushad",0

cmPUSHF	db	"pushf",0
cmPUSHFD	db	"pushfd",0

cmJCXZ	db	"jcxz",0
cmJECXZ	db	"jecxz",0


; Extract Condition group
cmJcc	db	"j",0
cmFCMOVcc	db	"fcmov",0
cmCMOVcc	db	"cmov",0
cmSETcc	db	"set",0

; Ordinary instructions
cmAAA	db	"aaa",0
cmAAD	db	"aad",0
cmAAM	db	"aam",0
cmAAS	db	"aas",0
cmARPL	db	"arpl",0
cmBOUND	db	"bound",0
cmBSF	db	"bsf",0
cmBSR	db	"bsr",0
cmBSWAP	db	"bswap",0
cmCLC	db	"clc",0
cmCLD	db	"cld",0
cmCLI	db	"cli",0
cmCLTS	db	"clts",0
cmCMC	db	"cmc",0
cmCMPXCHG	db	"cmpxchg",0
cmCMPXCHG8B	db	"cmpxchg8b",0
cmCPUID	db	"cpuid",0
cmDAA	db	"daa",0
cmDAS	db	"das",0
cmENTER	db	"enter",0
cmHLT	db	"hlt",0
cmICEBP	db	"icebp",0
cmIN	db	"in",0
cmINT	db	"int",0
cmINTO	db	"into",0
cmINVD	db	"invd",0
cmIRET	db	"iret",0
cmLAHF	db	"lahf",0
cmLAR	db	"lar",0
cmLDS	db	"lds",0
cmLEA	db	"lea",0
cmLEAVE	db	"leave",0
cmLES	db	"les",0
cmLFS	db	"lfs",0
cmLGS	db	"lgs",0
cmLSS	db	"lss",0
cmLOCK	db	"lock",0
cmLOOP	db	"loop",0
cmLOOPZ	db	"loopz",0
cmLOOPNZ	db	"loopnz",0
cmLSL	db	"lsl",0
cmMOV	db	"mov",0
cmMOVSX	db	"movsx",0
cmMOVZX	db	"movzx",0
cmNOP	db	"nop",0
cmOUT	db	"out",0
cmPOP	db	"pop",0
cmRDMSR	db	"rdmsr",0
cmRDPMC	db	"rdpmc",0
cmRDTSC	db	"rdtsc",0
cmREP	db	"rep",0
cmREPE	db	"repe",0
cmREPNZ	db	"repne",0
cmRET	db	"ret",0
cmRETF	db	"retf",0
cmRSM	db	"rsm",0
cmSAHF	db	"sahf",0
cmSALC	db	"setalc",0
cmSHLD	db	"shld",0
cmSHRD	db	"shrd",0
cmSTC	db	"stc",0
cmSTD	db	"std",0
cmSTI	db	"sti",0
cmUD2	db	"ud2",0
cmWAIT	db	"wait",0
cmFWAIT	db	"fwait",0
cmWBINVD	db	"wbinvd",0
cmWRMSR	db	"wrmsr",0
cmXADD	db	"xadd",0
cmXCHG	db	"xchg",0
cmXLAT	db	"xlat",0

; 0F octets
; 0F 00
cmSLDT	db	"sldt",0
cmSTR	db	"str",0
cmLLDT	db	"lldt",0
cmLTR	db	"ltr",0
cmVERR	db	"verr",0
cmVERW	db	"verw",0
	db	2 dup ("???",0)
	
; 0F 01
cmSGDT	db	"sgdt",0
cmSIDT	db	"sidt",0	
cmLGDT	db	"lgdt",0
cmLIDT	db	"lidt",0
cmSMSW	db	"smsw",0
	db	"???",0
cmLMSW	db	"lmsw",0
cmINVLPG	db	"invlpg",0

; Some "Hidden" instructions 
cmSYSCALL	db	"syscall",0
cmSYSRET	db	"sysret",0
cmSYSENTER	db	"sysenter",0
cmSYSEXIT	db	"sysexit",0



; FPU Instructions (not implemented yet)
cmF2XM1	db	"f2xm1",0
cmFABS	db	"fabs",0
cmFADD	db	"fadd",0
cmFADDP	db	"faddp",0
cmFIADD	db	"fiadd",0
cmFBLD	db	"fbld",0
cmFBSTP	db	"fbstp",0
cmFCHS	db	"fchs",0
cmFCLEX	db	"fclex",0
cmFNCLEX	db	"fnclex",0
cmFCOM	db	"fcom",0
cmFCOMP	db	"fcomp",0
cmFCOMPP	db	"fcompp",0
cmFCOMI	db	"fcomi",0
cmFCOMIP	db	"fcomip",0
cmFUCOMI	db	"fucomi",0
cmFUCOMIP	db	"fucomip",0
cmFCOS	db	"fcos",0
cmFDECSTP	db	"fdecstp",0
cmFDIV	db	"fdiv",0
cmFDIVP	db	"fdivp",0
cmFIDIV	db	"fidiv",0
cmFDIVR	db	"fdivr",0
cmFDIVRP	db	"fdivrp",0
cmFIDIVR	db	"FIDIVR",0
cmFFREE	db	"FFREE",0
cmFICOM	db	"ficom",0
cmFICOMP	db	"ficomp",0
cmFILD	db	"fild",0
cmFINCSTP	db	"fincstp",0
cmFINIT	db	"finit",0
cmFNINIT	db	"fninit",0
cmFIST	db	"fist",0
cmFISTP	db	"fistp",0
cmFLD	db	"fld",0
cmFLD1	db	"fld1",0
cmFLDL2T	db	"fldl2t",0
cmFLDL2E	db	"fldl2e",0
cmFLDPI	db	"fldpi",0
cmFLDLG2	db	"fldlg2",0
cmFLDLN2	db	"fldln2",0
cmFLDZ	db	"fldz",0
cmFLDCW	db	"fldcw",0
cmFLDENV	db	"fldenv",0
cmFMUL	db	"fmul",0
cmFMULP	db	"fmulp",0
cmFIMUL	db	"fimul",0
cmFNOP	db	"fnop",0
cmFPATAN	db	"fpatan",0
cmFPREM	db	"fprem",0
cmFPREM1	db	"fprem1",0
cmFPTAN	db	"fptan",0
cmFRNDINT	db	"frndint",0
cmFRSTOR	db	"frstor",0
cmFSAVE	db	"fsave",0
cmFNSAVE	db	"fnsave",0
cmFSCALE	db	"fscale",0
cmFSIN	db	"fsin",0
cmFSINCOS	db	"fsincos",0
cmFSQRT	db	"fsqrt",0
cmFST		db	"fst",0
cmFSTP	db	"fstp",0
cmFSTCW	db	"fstcw",0
cmFNSTCW	db	"fnstcw",0
cmFSTENV	db	"fstenv",0
cmFNSTENV	db	"fnstenv",0
cmFSTSW	db	"fstsw",0
cmFNSTSW	db	"fnstsw",0
cmFSUB	db	"fsub",0
cmFSUBP	db	"fsubp",0
cmFISUB	db	"fisub",0
cmFSUBR	db	"fsubr",0
cmFSUBRP	db	"fsubrp",0
cmFISUBR	db	"fisubr",0
cmFTST	db	"ftst",0
cmFUCOM	db	"fucom",0
cmFUCOMP	db	"fucomp",0
cmFUCOMPP	db	"fucompp",0
cmFXAM	db	"fxam",0
cmFXCH	db	"fxch",0
cmFXTRACT	db	"fxtract",0
cmFYL2X	db	"fyl2x",0
cmFYL2XP1	db	"fyl2xp1",0

; MMX commands (not implemented)
cmEMMS	db	"emms",0
cmMOVD	db	"movd",0
cmMOVQ	db	"movq",0
cmPACKSSWB	db	"packsswb",0
cmPACKSSDW	db	"packssdw",0
cmPACKUSWB	db	"packuswb",0
cmPADDB	db	"paddb",0
cmPADDW	db	"paddw",0
cmPADDD	db	"paddd",0
cmPADDSB	db	"paddsb",0
cmPADDSW	db	"paddsw",0
cmPADDUSB	db	"paddusb",0
cmPADDUSW	db	"paddusw",0
cmPAND	db	"pand",0
cmPANDN	db	"pandn",0
cmPCMPEQB	db	"pcmpeqb",0
cmPCMPEQW	db	"pcmpeqw",0
cmPCMPEQD	db	"pcmpeqd",0
cmPCMPGTB	db	"pcmpgtb",0
cmPCMPGTW	db	"pcmpgtw",0
cmPCMPGTD	db	"pcmpgtd",0
cmPMADDWD	db	"pmaddwd",0
cmPMULHW	db	"pmulhw",0
cmPMULLW	db	"pmullw",0
cmPOR	db	"por",0
cmPSLLW	db	"psllw",0
cmPSLLD	db	"pslld",0
cmPSLLQ	db	"psllq",0
cmPSRAW	db	"psraw",0
cmPSRAD	db	"psrad",0
cmPSRLW	db	"psrlw",0
cmPSRLD	db	"psrld",0
cmPSRLQ	db	"psrlq",0
cmPSUBB	db	"psubb",0
cmPSUBW	db	"psubw",0
cmPSUBD	db	"psubd",0
cmPSUBSB	db	"psubsb",0
cmPSUBSW	db	"psubsw",0
cmPSUBUSB	db	"psubusb",0
cmPSUBUSW	db	"psubusw",0
cmPUNPCKHBW	db	"punpckhbw",0
cmPUNPCKHWD	db	"punpckhwd",0
cmPUNPCKHDQ	db	"punpckhdq",0
cmPUNPCKLBW	db	"punpcklbw",0
cmPUNPCKLWD	db	"punpcklwd",0
cmPUNPCKLDQ	db	"punpckldq",0
cmPXOR	db	"pxor",0



; a variant for prefix imoplementation
cmUSE	db	"use",0

aREPZcodes	db	0a4h, 0a5h, 0a6h, 0a7h, 0aah, 0abh, 0ach, 0adh, 0aeh, 0afh, 6ch, 6dh, 6eh, 6fh 	; opcodes for chain commands, prefixed by rep((n)z)
nREPZCodes	db	$-aREPZcodes+1	; length



DC_TABLE_MAIN	label	DWORD	; This is main table of opcodes
i_m_00	CMDDESC	<Op1_reg8 or Op2_rm8 or Spec_RevOrder or Fam_8086, 	cmADD>
	CMDDESC	<Op1_reg1632 or Op2_rm1632 or Spec_RevOrder or Fam_8086, 	cmADD>
	CMDDESC	<Op1_reg8 or Op2_rm8 or Fam_8086,			cmADD>
	CMDDESC	<Op1_reg1632 or Op2_rm1632 or Fam_8086, 		cmADD>
	CMDDESC	<Op1_Imm8 or Op2_al or Fam_8086 or Spec_RevOrder, 	cmADD>
	CMDDESC	<Op1_Imm1632 or Op2_ax or Fam_8086 or Spec_RevOrder, 	cmADD>
	CMDDESC	<Op1_sreg386 or Fam_8086, 				cmPUSH>
	CMDDESC	<Op1_sreg386 or Fam_8086, 				cmPOP>

i_m_08	CMDDESC	<Op1_reg8 or Op2_rm8 or Spec_RevOrder or Fam_8086, 	cmOR>
	CMDDESC	<Op1_reg1632 or Op2_rm1632 or Spec_RevOrder or Fam_8086, 	cmOR>
	CMDDESC	<Op1_reg8 or Op2_rm8 or Fam_8086, 			cmOR>
	CMDDESC	<Op1_reg1632 or Op2_rm1632 or Fam_8086, 		cmOR>
	CMDDESC	<Op1_Imm8 or Op2_al or Fam_8086 or Spec_RevOrder,	cmOR>
	CMDDESC	<Op1_Imm1632 or Op2_ax or Fam_8086  or Spec_RevOrder,	cmOR>
	CMDDESC	<Op1_sreg386 or Fam_8086, 				cmPUSH>
	CMDDESC	<Fam_8086, 					cmUNKNOWN>

i_m_10	CMDDESC	<Op1_reg8 or Op2_rm8 or Spec_RevOrder or Fam_8086, 	cmADC>
	CMDDESC	<Op1_reg1632 or Op2_rm1632 or Spec_RevOrder or Fam_8086, 	cmADC>
	CMDDESC	<Op1_reg8 or Op2_rm8 or Fam_8086, 			cmADC>
	CMDDESC	<Op1_reg1632 or Op2_rm1632 or Fam_8086, 		cmADC>
	CMDDESC	<Op1_Imm8 or Op2_al or Fam_8086 or Spec_RevOrder, 	cmADC>
	CMDDESC	<Op1_Imm1632 or Op2_ax or Fam_8086 or Spec_RevOrder, 	cmADC>
	CMDDESC	<Op1_sreg386 or Fam_8086, 				cmPUSH>
	CMDDESC	<Op1_sreg386 or Fam_8086, 				cmPOP>

i_m_18	CMDDESC	<Op1_reg8 or Op2_rm8 or Spec_RevOrder or Fam_8086, 	cmSBB>
	CMDDESC	<Op1_reg1632 or Op2_rm1632 or Spec_RevOrder or Fam_8086, 	cmSBB>
	CMDDESC	<Op1_reg8 or Op2_rm8 or Fam_8086, 			cmSBB>
	CMDDESC	<Op1_reg1632 or Op2_rm1632 or Fam_8086, 		cmSBB>
	CMDDESC	<Op1_Imm8 or Op2_al or Fam_8086 or Spec_RevOrder, 	cmSBB>
	CMDDESC	<Op1_Imm1632 or Op2_ax or Fam_8086 or Spec_RevOrder, 	cmSBB>
	CMDDESC	<Op1_sreg386 or Fam_8086, 				cmPUSH>
	CMDDESC	<Op1_sreg386 or Fam_8086, 				cmPOP>

i_m_20	CMDDESC	<Op1_reg8 or Op2_rm8 or Spec_RevOrder or Fam_8086, 	cmAND>
	CMDDESC	<Op1_reg1632 or Op2_rm1632 or Spec_RevOrder or Fam_8086, 	cmAND>
	CMDDESC	<Op1_reg8 or Op2_rm8 or Fam_8086, 			cmAND>
	CMDDESC	<Op1_reg1632 or Op2_rm1632 or Fam_8086, 		cmAND>
	CMDDESC	<Op1_Imm8 or Op2_al or Fam_8086 or Spec_RevOrder, 	cmAND>
	CMDDESC	<Op1_Imm1632 or Op2_ax or Fam_8086 or Spec_RevOrder,	cmAND>
	CMDDESC	<Op1_sreg386 or Fam_8086, 				cmUSE>
	CMDDESC	<Fam_8086, 						cmDAA>

i_m_28	CMDDESC	<Op1_reg8 or Op2_rm8 or Spec_RevOrder or Fam_8086, 	cmSUB>
	CMDDESC	<Op1_reg1632 or Op2_rm1632 or Spec_RevOrder or Fam_8086, 	cmSUB>
	CMDDESC	<Op1_reg8 or Op2_rm8 or Fam_8086, 			cmSUB>
	CMDDESC	<Op1_reg1632 or Op2_rm1632 or Fam_8086, 		cmSUB>
	CMDDESC	<Op1_Imm8 or Op2_al or Fam_8086 or Spec_RevOrder, 	cmSUB>
	CMDDESC	<Op1_Imm1632 or Op2_ax or Fam_8086 or Spec_RevOrder, 	cmSUB>
	CMDDESC	<Op1_sreg386 or Fam_8086, 				cmUSE>
	CMDDESC	<Fam_8086, 						cmDAS>

i_m_30	CMDDESC	<Op1_reg8 or Op2_rm8 or Spec_RevOrder or Fam_8086, 	cmXOR>
	CMDDESC	<Op1_reg1632 or Op2_rm1632 or Spec_RevOrder or Fam_8086, 	cmXOR>
	CMDDESC	<Op1_reg8 or Op2_rm8 or Fam_8086, 			cmXOR>
	CMDDESC	<Op1_reg1632 or Op2_rm1632 or Fam_8086, 		cmXOR>
	CMDDESC	<Op1_Imm8 or Op2_al or Fam_8086 or Spec_RevOrder, 	cmXOR>
	CMDDESC	<Op1_Imm1632 or Op2_ax or Fam_8086 or Spec_RevOrder, 	cmXOR>
	CMDDESC	<Op1_sreg386 or Fam_8086, 				cmUSE>
	CMDDESC	<Fam_8086, 						cmAAA>

i_m_38	CMDDESC	<Op1_reg8 or Op2_rm8 or Spec_RevOrder or Fam_8086, 	cmCMP>
	CMDDESC	<Op1_reg1632 or Op2_rm1632 or Spec_RevOrder or Fam_8086, 	cmCMP>
	CMDDESC	<Op1_reg8 or Op2_rm8 or Fam_8086, 			cmCMP>
	CMDDESC	<Op1_reg1632 or Op2_rm1632 or Fam_8086, 		cmCMP>
	CMDDESC	<Op1_Imm8 or Op2_al or Fam_8086 or Spec_RevOrder, 	cmCMP>
	CMDDESC	<Op1_Imm1632 or Op2_ax or Fam_8086 or Spec_RevOrder, 	cmCMP>
	CMDDESC	<Op1_sreg386 or Fam_8086, 				cmUSE>
	CMDDESC	<Fam_8086, 						cmAAS>

i_m_40	label	dword
	rept	8
	CMDDESC	<Op1_regExtr16 or Fam_8086, 				cmINC>
	endm
i_m_48	label	dword
	rept	8
	CMDDESC	<Op1_regExtr16 or Fam_8086, 				cmDEC>
	endm
i_m_50	label	dword
	rept	8
	CMDDESC	<Op1_regExtr16 or Fam_8086, 				cmPUSH>
	endm
i_m_58	label	dword
	rept	8
	CMDDESC	<Op1_regExtr16 or Fam_8086, 				cmPOP>
	endm

i_m_60	CMDDESC	<Fam_8086 or Spec_2Names, 				cmPUSHA>
	CMDDESC	<Fam_8086 or Spec_2Names, 				cmPOPA>
	CMDDESC	<Op1_Reg1632 or Op2_Mem3264 or Fam_80286,	 	cmBOUND>
	CMDDESC	<Op1_reg16 or Op2_rm16 or Spec_RevOrder or Fam_80286,	cmARPL>
	CMDDESC	<Op1_sreg386 or Fam_8086, 				cmUSE>
	CMDDESC	<Op1_sreg386 or Fam_8086, 				cmUSE>
	CMDDESC	<Fam_8086, 					cmUNKNOWN>
	CMDDESC	<Fam_8086, 					cmUNKNOWN>

i_m_68	CMDDESC	<Op1_Imm1632 or Fam_8086,				cmPUSH>
	CMDDESC	<Op1_reg1632 or Op2_RM1632 or Spec_ExtraI1632 or Fam_i486,	cmIMUL>
	CMDDESC	<Op1_Imm8 or Fam_8086,				cmPUSH>
	CMDDESC	<Op1_reg1632 or Op2_RM1632 or Spec_ExtraI8 or Fam_i486,	cmIMUL>
	CMDDESC	<Op1_None or Op2_None or Fam_80286,		 	cmINSB>
	CMDDESC	<Op1_None or Op2_None or Spec_2Names or Fam_80286, 	cmINSW>
	CMDDESC	<Op1_None or Op2_None or Fam_80286, 			cmOUTSB>
	CMDDESC	<Op1_None or Op2_None or Spec_2Names or Fam_80286, 	cmOUTSW>
		
i_m_70	label	dword		
	rept	16
	CMDDESC	<Op1_Imm8 or Fam_8086 or Spec_GetCond or Op1_isOffset, 	cmJcc>
	endm

i_m_80	CMDDESC	<Op1_Imm8 or Op2_rm8 or Spec_RevOrder or Fam_8086 or Spec_8Names, 		cmADD>
	CMDDESC	<Op1_Imm1632 or Op2_rm1632 or Spec_RevOrder or Fam_8086 or Spec_8Names, 	cmADD>
	CMDDESC	<Op1_Imm8 or Op2_rm8 or Spec_RevOrder or Fam_8086 or Spec_8Names, 		cmADD>
	CMDDESC	<Op1_Imm8 or Op2_rm1632 or Spec_RevOrder or Fam_8086 or Spec_8Names, 		cmADD>
	CMDDESC	<Op1_Imm8 or Op2_rm8 or Spec_RevOrder or Fam_8086, 	cmTest>
	CMDDESC	<Op1_Imm1632 or Op2_rm1632 or Spec_RevOrder or Fam_8086, 	cmTest>
	CMDDESC	<Op1_None or Fam_8086, 				cmCMPSB>
	CMDDESC	<Op1_None or Fam_8086 or Spec_2Names,			cmCMPSW>

i_m_88	CMDDESC	<Op1_reg8 or Op2_rm8 or Spec_RevOrder or Fam_8086, 	cmMOV>
	CMDDESC	<Op1_reg1632 or Op2_rm1632 or Spec_RevOrder or Fam_8086, 	cmMOV>
	CMDDESC	<Op1_reg8 or Op2_rm8 or Fam_8086, 			cmMOV>
	CMDDESC	<Op1_reg1632 or Op2_rm1632 or Fam_8086, 		cmMOV>
	CMDDESC	<Op2_rm1632 or Op1_sreg386_pb or Fam_8086, 		cmMOV>
	CMDDESC	<Op1_reg1632 or Op2_Memory or Fam_8086, 		cmLEA>
	CMDDESC	<Op2_rm1632 or Op1_sreg386_pb or Spec_RevOrder or Fam_8086,	cmMOV>
	CMDDESC	<Op2_rm1632 or Fam_8086, 				cmPOP>

i_m_90	CMDDESC	<Op1_None or Op2_None or Fam_8086, 			cmNOP>
	rept	7
	CMDDESC	<Op1_regExtr16 or Op2_ax or Spec_RevOrder or Fam_8086, 	cmXCHG>
	endm
	
i_m_98	CMDDESC	<Fam_8086 or Spec_2Names, 		cmCBW>				
	CMDDESC	<Fam_8086 or Spec_2Names,			cmCWD>
	CMDDESC	<Op1_Imm3248 or Fam_8086,			cmCALL>
	CMDDESC	<Op1_None or Op2_None or Fam_8086, 	cmWAIT>
	CMDDESC	<Fam_8086 or Spec_2Names,			cmPUSHF>
	CMDDESC	<Fam_8086 or Spec_2Names,			cmPOPF>
	CMDDESC	<Fam_8086,					cmSAHF>
	CMDDESC	<Fam_8086,					cmLAHF>

i_m_A0	CMDDESC	<Fam_8086 or Op1_Imm1632 or Op1_isOff_al,				cmMOV>
	CMDDESC	<Fam_8086 or Op1_Imm1632 or Op1_isOff_ax,				cmMOV>
	CMDDESC	<Fam_8086 or Op1_Imm1632 or Op1_isOff_al or Spec_RevOrder,	cmMOV>
	CMDDESC	<Fam_8086 or Op1_Imm1632 or Op1_isOff_ax or Spec_RevOrder,	cmMOV>
	
	
	CMDDESC	<Fam_8086, 					cmMOVSB>				
	CMDDESC	<Fam_8086 or Spec_2Names,			cmMOVSW>
	CMDDESC	<Fam_8086, 					cmCMPSB>				
	CMDDESC	<Fam_8086 or Spec_2Names,			cmCMPSW>

i_m_A8	CMDDESC	<Op1_Imm8 or Op2_al or Spec_RevOrder or Fam_8086, 	cmTEST>				
	CMDDESC	<Op1_Imm1632 or Op2_ax or Spec_RevOrder or Fam_8086,	cmTEST>
	CMDDESC	<Fam_8086, 					cmSTOSB>	
	CMDDESC	<Fam_8086 or Spec_2Names,			cmSTOSW>
	CMDDESC	<Fam_8086, 					cmLODSB>	
	CMDDESC	<Fam_8086 or Spec_2Names,			cmLODSW>
	CMDDESC	<Fam_8086, 					cmSCASB>	
	CMDDESC	<Fam_8086 or Spec_2Names,			cmSCASW>

i_m_BO	label	dword	
	rept	8
	CMDDESC	<Fam_8086 or Op2_RegExtr8 or Op1_Imm8 or Spec_RevOrder,		cmMOV>	
	endm
i_m_B8	label	dword	
	rept	8
	CMDDESC	<Fam_8086 or Op2_RegExtr16 or Op1_Imm1632 or Spec_RevOrder,	cmMOV>	
	endm
			
i_m_C0	CMDDESC	<Op1_Imm8 or Op2_rm8 or Fam_8086 or Spec_8Names,		cmROL>
	CMDDESC	<Op1_Imm8 or Op2_rm1632 or Fam_8086 or Spec_8Names,	cmROL>
	CMDDESC	<Op1_Imm16 or Fam_8086, 				cmRET>				
	CMDDESC	<Fam_8086,					cmRET>
	CMDDESC	<Op1_Reg1632 or Op2_Mem3248 or Fam_80286,		cmLDS>				
	CMDDESC	<Op1_Reg1632 or Op2_Mem3248 or Fam_80286,		cmLES>
	CMDDESC	<Op1_Imm8 or Op2_rm8 or Spec_RevOrder or Fam_8086,	cmMOV>				
	CMDDESC	<Op1_Imm1632 or Op2_rm1632 or Spec_RevOrder or Fam_8086,	cmMOV>

i_m_C8	CMDDESC	<Op1_Imm16 or Spec_ExtraI8 or Fam_80286,	cmENTER>
	CMDDESC	<Fam_80286,					cmLEAVE>
	CMDDESC	<Op1_Imm16 or Fam_8086, 			cmRETF>
	CMDDESC	<Fam_8086, 					cmRETF>
	CMDDESC	<Spec_Op1_Is_3 or Fam_8086, 				cmINT>
	CMDDESC	<Op1_Imm8 or Fam_8086, 				cmINT>
	CMDDESC	<Fam_8086, 					cmINTO>
	CMDDESC	<Fam_8086, 					cmIRET>

i_m_D0	CMDDESC	<Op1_is_1 or Op2_rm8 or Fam_8086 or Spec_8Names or Spec_RevOrder,	cmROL>
	CMDDESC	<Op1_is_1 or Op2_rm1632 or Fam_8086 or Spec_8Names or Spec_RevOrder,	cmROL>
	CMDDESC	<Op1_CL or Op2_rm8 or Fam_8086 or Spec_8Names or Spec_RevOrder,		cmROL>
	CMDDESC	<Op1_CL or Op2_rm1632 or Fam_8086 or Spec_8Names or Spec_RevOrder,	cmROL>

	CMDDESC	<Op1_Imm8 or Fam_8086,				cmAAM>
	CMDDESC	<Op1_Imm8 or Fam_8086,				cmAAD>
	CMDDESC	<Fam_8086,					cmSALC>
	CMDDESC	<Fam_8086,					cmXLAT>

; While fpu decode is not made, we can only skip fpu instructions 
	dw 	8h dup (Op1_Imm8, cmUNKNOWN)
		
		
i_m_E0	CMDDESC	<Op1_Imm8 or Fam_8086 or Op1_isOffset,			cmLOOPNZ>
	CMDDESC	<Op1_Imm8 or Fam_8086 or Op1_isOffset,			cmLOOPZ>
	CMDDESC	<Op1_Imm8 or Fam_8086 or Op1_isOffset,			cmLOOP>
	CMDDESC	<Op1_Imm8 or Fam_8086 or Op1_isOffset or Spec_2Names,	cmJCXZ>		

i_m_E4	CMDDESC	<Op1_Imm8 or Fam_8086 or Spec_RevOrder or Op2_al,		cmIN>		
	CMDDESC	<Op1_Imm8 or Fam_8086 or Spec_RevOrder or Op2_ax,		cmIN>		
	CMDDESC	<Op1_Imm8 or Fam_8086 or Op2_al,			cmOUT>		
	CMDDESC	<Op1_Imm8 or Fam_8086 or Op2_ax,			cmOUT>		

i_m_E8	CMDDESC	<Op1_Imm1632 or Fam_8086 or Op1_isOffset,		cmCALL>
	CMDDESC	<Op1_Imm1632 or Fam_8086 or Op1_isOffset,		cmJMP>
	CMDDESC	<Op1_Imm3248 or Fam_8086,				cmJMP>
	CMDDESC	<Op1_Imm8 or Fam_8086 or Op1_isOffset,			cmJMP>

i_m_EC	CMDDESC	<Op1_DX or Fam_8086 or Spec_RevOrder or Op2_al,		cmIN>		
	CMDDESC	<Op1_DX or Fam_8086 or Spec_RevOrder or Op2_ax,		cmIN>		
	CMDDESC	<Op1_DX or Fam_8086 or Op2_al,			cmOUT>		
	CMDDESC	<Op1_DX or Fam_8086 or Op2_ax,			cmOUT>
		
i_m_F0	CMDDESC	<Fam_8086,					cmLOCK>
	CMDDESC	<Fam_8086,					cmICEBP>
	CMDDESC	<Fam_8086,					cmREPNZ>
	CMDDESC	<Fam_8086,					cmREP>
	CMDDESC	<Fam_8086,					cmHLT>
	CMDDESC	<Fam_8086,					cmCMC>
	CMDDESC	<Op1_Imm8 or Op2_Rm8 or Fam_8086 or Spec_8Names or Spec_RevOrder,	cmTEST>
	CMDDESC	<Op1_Imm1632 or Op2_Rm1632 or Fam_8086 or Spec_8Names or Spec_RevOrder,	cmTEST>
	
i_m_F8	CMDDESC	<Fam_8086,					cmCLC>
	CMDDESC	<Fam_8086,					cmSTC>
	CMDDESC	<Fam_8086,					cmCLI>
	CMDDESC	<Fam_8086,					cmSTI>
	CMDDESC	<Fam_8086,					cmCLD>
	CMDDESC	<Fam_8086,					cmSTD>
	CMDDESC	<Fam_8086 or Op2_Rm8 or Spec_8Names,	cmINC> ; NOTE: combination s except /0, /1 are forbidden
	CMDDESC	<Fam_8086 or Op2_Rm1632 or Spec_8Names,	cmINC>

DC_TABLE_ALT	label	DWORD	; Extended Opcodes (1st byte = 0Fh)

i_a_00	CMDDESC	<Fam_i486 or Op2_Memory or Spec_8Names,	cmSLDT>	; Rm16 shoud be here (used to suppress type output)
	CMDDESC	<Fam_i486 or Op2_Memory or Spec_8Names,	cmSGDT>
	
	CMDDESC	<Fam_i486 or Op2_Rm1632 or Op1_Reg1632,		cmLAR>
	CMDDESC	<Fam_i486 or Op2_Rm1632 or Op1_Reg1632,		cmLSL>
	
	dw 		0, cmUNKNOWN	
	CMDDESC	<Fam_80286,					cmSYSCALL>
	CMDDESC	<Fam_80286,					cmCLTS>
	CMDDESC	<Fam_80286,					cmSYSRET>

i_a_08	CMDDESC	<Fam_i486,					cmINVD>
i_a_09	CMDDESC	<Fam_i486,					cmWBINVD>	
	dw	0, cmUNKNOWN
i_a_0B	CMDDESC	<Fam_P54C,					cmUD2>
	dw	14h	dup (0, cmUNKNOWN)		; Skip XMM and other commands
i_a_20	CMDDESC	<Fam_80386 or Op1_SPreg or Spec_RevOrder, 	cmMOV>
	CMDDESC	<Fam_80386 or Op1_SPreg or Spec_RevOrder, 	cmMOV>
	CMDDESC	<Fam_80386 or Op1_SPreg, 			cmMOV>
	CMDDESC	<Fam_80386 or Op1_SPreg, 			cmMOV> 
	CMDDESC	<Fam_80286 or Op1_SPreg or Spec_RevOrder, 	cmMOV>
	CMDDESC	<Fam_80286 or Op1_SPreg or Spec_RevOrder, 	cmMOV>
	CMDDESC	<Fam_80386 or Op1_SPreg, 			cmMOV>
	CMDDESC	<Fam_80386 or Op1_SPreg, 			cmMOV> ; Unknown?

	dw 	8h dup (0, cmUNKNOWN)		; Skip XMM here and in some other places
	
i_a_30	CMDDESC	<Fam_i486,					cmWRMSR>
	CMDDESC	<Fam_i486,					cmRDTSC>
	CMDDESC	<Fam_i486,					cmRDMSR>
	CMDDESC	<Fam_i486,					cmRDPMC>
	CMDDESC	<Fam_Piii,					cmSYSENTER>
	CMDDESC	<Fam_Piii,					cmSYSEXIT>
	
	dw	0Ah dup (0, cmUNKNOWN)
	
i_a_40	label	DWORD
	rept	16
	CMDDESC	<Op1_Reg1632 or Fam_Piii or Spec_GetCond or Op2_Rm1632, 	cmCMOVcc>
	endm	

	dw	10h dup (0, cmUNKNOWN)	; XMM or anything else, anyway should skip this

	dw	20h dup (0, cmUNKNOWN)	; MMX - not implemented in Extern Edition
	
i_a_80	label	DWORD
	rept	16
	CMDDESC	<Op1_Imm1632 or Fam_80386 or Spec_GetCond or Op1_isOffset, 	cmJcc>
	endm		

i_a_90	label	DWORD
	rept	16
	CMDDESC	<Fam_80386 or Spec_GetCond or Op2_RM8, 	cmSETcc>
	endm	
	
i_a_A0	CMDDESC	<Op1_sreg386 or Fam_80286, 				cmPUSH>	; Should decode fs
	CMDDESC	<Op1_sreg386 or Fam_80286, 				cmPOP>	
	CMDDESC	<Fam_i486,						cmCPUID>
	CMDDESC	<Fam_i486 or Op2_Rm1632 or Op1_Reg1632 or Spec_RevOrder,	cmBT>
	CMDDESC	<Fam_80386 or Op2_Rm1632 or Op1_Reg1632 or Spec_RevOrder or Spec_ExtraI8,	cmSHLD>
	CMDDESC	<Fam_80386 or Op2_Rm1632 or Op1_Reg1632 or Spec_RevOrder or Spec_Extra_cl,	cmSHLD>
		dw 	2h dup (0, cmUNKNOWN)		; Skip CMPXCHG486 Here
i_a_A8	CMDDESC	<Op1_sreg386 or Fam_80286, 				cmPUSH>	; Should decode GS
	CMDDESC	<Op1_sreg386 or Fam_80286, 				cmPOP>	
	CMDDESC	<Fam_P54C,						cmRSM>
	CMDDESC	<Fam_i486 or Op2_Rm1632 or Op1_Reg1632 or Spec_RevOrder,	cmBTS>
	CMDDESC	<Fam_80386 or Op2_Rm1632 or Op1_Reg1632 or Spec_RevOrder or Spec_ExtraI8,	cmSHRD>
	CMDDESC	<Fam_80386 or Op2_Rm1632 or Op1_Reg1632 or Spec_RevOrder or Spec_Extra_cl,	cmSHRD>
		dw	0, cmUNKNOWN		; Skip FXSAVE/FXRSTR here (it is an XMM)
	CMDDESC	<Fam_80386 or Op2_Rm1632 or Op1_Reg1632,	cmIMUL>
	
i_a_B0	CMDDESC	<Op1_reg8 or Op2_rm8 or Spec_RevOrder or Fam_8086, 	cmCMPXCHG>
	CMDDESC	<Op1_reg1632 or Op2_rm1632 or Spec_RevOrder or Fam_8086, 	cmCMPXCHG>
	CMDDESC	<Op1_Reg1632 or Op2_Mem3248 or Fam_80286,		cmLSS>				
	CMDDESC	<Fam_i486 or Op2_Rm1632 or Op1_Reg1632 or Spec_RevOrder,	cmBTR>
	CMDDESC	<Op1_Reg1632 or Op2_Mem3248 or Fam_80286,		cmLFS>				
	CMDDESC	<Op1_Reg1632 or Op2_Mem3248 or Fam_80286,		cmLGS>
	CMDDESC	<Op1_reg1632 or  Op2_RM8 or Fam_80386, 		cmMOVZX>
	CMDDESC	<Op1_reg1632 or  Op2_RM16 or Fam_80386, 		cmMOVZX>
i_a_b8		dw 	2h dup (0, cmUNKNOWN)		; Skip invalid opcodes Here
	CMDDESC	<Op1_Imm8 or Op2_rm8 or Spec_RevOrder or Spec_8Names,	cm_0fBa>	; Bit oper w/immediate
	CMDDESC	<Fam_i486 or Op2_Rm1632 or Op1_Reg1632 or Spec_RevOrder,	cmBTC>	
	CMDDESC	<Fam_i486 or Op2_Rm1632 or Op1_Reg1632 or Spec_RevOrder,	cmBSF>
	CMDDESC	<Fam_i486 or Op2_Rm1632 or Op1_Reg1632 or Spec_RevOrder,	cmBSR>	
	CMDDESC	<Op1_reg1632 or  Op2_RM8 or Fam_80386, 		cmMOVSX>
	CMDDESC	<Op1_reg1632 or  Op2_RM16 or Fam_80386, 		cmMOVSX>
	
i_a_C0	CMDDESC	<Op1_reg8 or Op2_rm8 or Spec_RevOrder or Fam_80386, 	cmXADD>
	CMDDESC	<Op1_reg1632 or Op2_rm1632 or Spec_RevOrder or Fam_80386, 	cmXADD>
		dw 	5h dup (0, cmUNKNOWN)
	CMDDESC	<Op2_Memory or Fam_P54C, 				cmCMPXCHG8B>
i_a_C8	label	Dword
	rept	8
	CMDDESC	<Op1_regExtr16 or Fam_i486, 	cmBSWAP>
	endm
			
	dw 	30h dup (0, cmUNKNOWN)	; MMX and some undocs remaining
		
; The end of both tables		
