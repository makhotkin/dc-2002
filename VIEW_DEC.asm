; Project "DC 2002"
; "Disassembly view" module

.model small


at_Main	equ	1bh
at_Status	equ	0B1h
line_count	equ	25
CodeQuanta	equ	150h

include		i586_fl.inc

.data
CpuClass	dw	Fam_i486	; CPU class to decode commands
; Remember: FPU, MMX and SSE not implemented in extern edition

;sc_CpuSelect	db	"Choose Target CPU",0
;sc_CPU8086	db	"Intel  8086",0
;sc_CPU80286	db	"Intel 80286",0
;sc_CPU80386	db	"Intel 80386",0
;sc_CPUi486	db	"Intel  i486",0
;sc_CPUP54C	db	"Pentium (P54C)",0
;sc_CPUPiii	db	"Piii (no fpu, mmx, sse)",0

sUse		db	"Use ",0
sModeDec	db	"Decode:",0

Hintbar		db	"Help  ",0,"16..32",0,"      ",0,"Mode  ",0,"Go To ",0,"      ",0,"      ",0,"SaveDA",0,"Open  ",0,"Quit  ",0

CodeMark	dd	0	; Used for positioning and scrolling
CodeMark2	dd	0	; Used for positioning and scrolling


EXTRN	LineBuffer:Byte, ViewOffset:DWORD, Filename24:Byte
EXTRN	FinalCode:byte, EIPLocal:dword, DC_Def_State:Word,  sAppTitle:byte, RFileSize:DWORD


.code
.386

include	base.inc
EXTRN		DecodeCmd:Proc, GetDiskBuffer:PROC, InitDC:Proc, DisplayHintBaR:Proc, ChooseList: Proc

PUBLIC	UpdateDeCodeView, DecodeCpuSelect, DecodeModeSelect, DC_LineDown, DC_LineUp, DC_PageDown

DC_LineDown	proc
	mov	eax, CodeMark
	cmp	eax, RFileSize
	jae	@DCLD_Done
	mov	ViewOffset, eax
@DCLD_Done:
	ret
DC_LineDown	endp

DC_LineUp	proc
	mov	eax, ViewOffset
	dec	eax
	test	eax, eax		; cmp eax, 0
	jl	@DCLU_Done
	mov	ViewOffset, eax
@DCLU_Done:
	ret
DC_LineUp	endp

DC_PageDown	proc
	mov	eax, CodeMark2
	cmp	eax, RFileSize
	jae	@DCPD_Done
	mov	ViewOffset, eax
@DCPD_Done:
	ret
DC_PageDown	endp


DecodeCpuSelect	PROC	; Feature disabled until implemented in decode.asm
;	mov	cx, 6
;	lea	si, sc_CpuSelect
;	call	ChooseList

	ret
DecodeCpuSelect	endp


DecodeModeSelect	PROC

	xor	DC_Def_State, State_Oper32 or State_Addr32	
	
	ret
DecodeModeSelect endp


; Procedure	DeCodeView
; Purpose	Display currently open file from given offset
; Input		FileSize	- (Mem) Size of opened file
;		ViewOffset	- (Mem) Offset to start displaying from
;		fb_Buffers	- (Mem) Buffers containing 
; Output	Lines 1-23 filled with text from opened file, offset ViewOffset
UpdateDeCodeView	PROC	
		push	bp
		mov	bp, sp
RequestLoc	equ	[bp-4]	; Location to request from file buffers
CurrentLoc	equ	[bp-8]	; Offset of bytes being sent to output
BufferLimit	equ	[bp-0Ah]	; Offset in data segment: end of valid buffer block
ThisLine	equ	[bp-0Ch]
BufferPoint	equ	[bp-0Eh]
		sub	sp, 0Eh

		mov	edx, ViewOffset
		mov	CurrentLoc, edx
		mov	EIPLocal, edx		; initialize command counter in decode module
		mov	RequestLoc, edx
		call	DC_statusBar		; draw status bar 
		mov	word ptr ThisLine, 01h
		
@DecodeV_LoadBuffer:
		mov	edx, RequestLoc
		mov	cx, CodeQuanta
		call	GetDiskBuffer	; Should return adress in ds:si, characters in cx
		
		test	cx, cx
		jz	@DecodeV_Fill	; End draw if nothing else is on buffer 
					; might be caused by EOF or other errors 
@DecodeV_BufferCheck:		
		; Fix cur_loc
		movzx	eax, cx
		add	eax, RequestLoc
		mov	RequestLoc, eax
		cmp	cx, CodeQuanta
		jae	@DecodeV_Buffer_Good
		push	si
		add	si, cx
		mov	dword ptr [si],0
		mov	dword ptr [si+4],0
		mov	dword ptr [si+8],0
		pop	si
@DecodeV_Buffer_Good:
		add	cx, si		; End of valid data block
		mov	BufferLimit, cx
		
@DecodeV_NextLine:
		cmp	word ptr ThisLine, line_count-1	; if drawn enough lines, 
		jae	@DecodeV_Done		; then we are done
		cmp	si, BufferLimit
		jae	@DecodeV_LoadBuffer	; Try to load more if still needed some 

@DecodeV_LineStart:
		push	es
		push	ds			; es should keep video page offset for a while
		pop 	es 			; es is data 
		mov	LineBuffer, 0		; Clear current line buffer
		lea	di, LineBuffer
		mov	eax, CurrentLoc
		call	Int2Hex32
		dec	di			; not an end yet, should remove zero-terminator
		mov	eax, ' :'
		stosw

		push	di
		lea	di, FinalCode	; Should call decoder first, 'cause command length is unknown
		call	DecodeCmd	; Call to decoder
		pop	di

		movzx	eax, cx
		add	eax, CurrentLoc
		cmp	byte ptr ThisLine, 01h
		jnz	@DecodeV_NoSave
		mov	CodeMark, eax
@DecodeV_NoSave:
		mov	CurrentLoc, eax
		mov	CodeMark2, eax	; Last record will be used for page-down
		sub	si, cx			; Second pass is hex output

@DecodeV_HexData:
		lodsb			; Load current character
		call	Int2Hex8
		dec	di
		loop	@DecodeV_HexData
; Tablute to 32 		
		mov	BufferPoint, si
		mov	al, ' '
		mov	cx, 40
		lea	di, LineBuffer
		call	sFillUp		; Tabulation for output line
		dec	di		; That's Not an end yet
; Cat Final Code to LineBuffer	
		lea	si, FinalCode
		call	strcpy_x	; Copy command name

		mov	cx, 52
		lea	di, LineBuffer
		mov	al, ' '
		call	sFillUp		; Tabulation for output line
		dec	di
		
		call	strcpy_x	; copy operands
		
		pop	es		
		mov	ah, at_Main
		lea	si, LineBuffer
		mov	dx, ThisLine
		xchg	dh, dl
		call	FormatOutAt
		mov	al, ' '
		call	FillLineAt
		mov	si, BufferPoint
		inc	word ptr ThisLine
		jmp	@DecodeV_NextLine

@DecodeV_Fill:
		mov	ax, ThisLine
		xchg	ah, al
		call	GetVideoOffset
		mov	di, ax
		mov	ax, at_Main*256 + 32		
		mov	dx, 1800h
		call	FillUpAt

@DecodeV_Done:	
		mov	sp, bp
		pop	bp	
		lea	si, HintBar
		call	DisplayHintbar		
		ret
UpdateDeCodeView	ENDP

DC_statusBar	proc
	
	push	es
		push	ds
		pop	es
		
		; Status line 
		lea	di, LineBuffer	; Output Mode Name
		lea	si, sModeDec
		call	strcpy_x
		
		lea	di, LineBuffer	; Tabulate 8
		mov	cx, 8
		mov	al, 32
		call	sFillUp
		dec 	di
		
		lea	si, Filename24	; Output Filename short format
		call	strcpy_x

		mov	ax, 32		; tabulate 36
		mov	cx, 36
		lea	di, LineBuffer
		call	sFillUp
		dec	di
		
		mov	eax, ViewOffset 	; Out currently viewed location
		call	Int2Hex32
		
		dec	di
		mov	ax, 2020h
		stosw
		
		mov	eax, dword ptr sUse		; Display decode mode (use 16 or Use 32)
		stosd
		
		test	DC_Def_State, State_Oper32 or State_Addr32
		jnz	@DC_stat_32
		mov	al, 16h
		jmp	@DC_stat_use
@DC_stat_32:				
		mov	al, 32h
@DC_stat_use:
		call	Int2Hex8	
		
		mov	ax, 32			; Tabulate 60
		mov	cx, 60
		lea	di, LineBuffer	
		call	sFillUp
		dec	di
		
		lea 	si, sAppTitle		; Append AppName
		call	strcpy_x
		
		pop	es			; Send to video
		xor	di, di
		lea	si, LineBuffer
		mov	cx, 80
		mov	ah, at_Status
		call	OutCharsAt_x
	ret

DC_statusBar endp

end

