; Project DC 2002
; Hexadecimal View: functions, scroll, Options, etc


.model		small

.stack		80h

HexQuanta	equ	250h
line_count	equ	25
at_Main	equ	1Bh
at_Status	equ	0B1h

.data

sModeHex	db	"Hex:",0
HintBar	db	"Help  ",0,"      ",0,"      ",0,"Mode  ",0,"GoTo  ",0,"      ",0,"      ",0,"      ",0,"Open  ",0,"Quit  "
s_Separate1	db	" ",0b3h,"  "

EXTRN	LineBuffer:Byte, ViewOffset:DWORD, Filename24:Byte,  sAppTitle:byte, RFileSize:DWORD

.code
.386

include	base.inc
EXTRN		GetDiskBuffer:PROC, DisplayHintBar: PROC

PUBLIC	UpdateHexView, Hex_LineDown, Hex_LineUp, Hex_PageDown, Hex_PageUp


Hex_LineDown	PROC
	mov	eax, ViewOffset
	add	eax, 16
	mov	edx, eax
	and	dl, 0f0h
	cmp	edx, RFileSize
	ja	@HexLD_Done
	mov	ViewOffset, eax
@HexLD_Done:
	ret
Hex_LineDown	ENDP

Hex_PageDown	PROC
	mov	eax, ViewOffset
	add	eax, 16*(line_count-2)
	mov	edx, eax
	and	dl, 0f0h
	cmp	edx, RFileSize
	ja	@HexPD_Done
	mov	ViewOffset, eax
@HexPD_Done:
	ret
Hex_PageDown	ENDP

Hex_PageUp	PROC
	mov	eax, ViewOffset
	sub	eax, 16*(line_count-2)
	mov	edx, eax
	and	dl, 0f0h
	test	edx, edx
	jge	@HexPU_Done
	xor	eax, eax	
@HexPU_Done:
	mov	ViewOffset, eax
	ret
Hex_PageUp	ENDP

Hex_LineUp	PROC
	mov	eax, ViewOffset
	sub	eax, 16
	test	eax, eax
	jl	@HexLU_Done
	mov	ViewOffset, eax
@HexLU_Done:
	ret
Hex_LineUp	ENDP

; Procedure	HexView
; Purpose	Display currently open file from given offset
; Input		FileSize	- (Mem) Size of opened file
;		ViewOffset	- (Mem) Offset to start displaying from
; Output	Lines 1-23 filled with Hext from opened file, offset ViewOffset
UpdateHexView		PROC
		; Create stack frame for locals
		push	bp
		mov	bp, sp
RequestLoc	equ	[bp-4]	; Location to request from file buffers
CurrentLoc	equ	[bp-8]	; Offset of bytes being sent to output
BufferLimit	equ	[bp-0Ah]
ThisLine	equ	[bp-0Ch]
		sub	sp, 0Ch

		mov	edx, ViewOffset
		and	edx, 0fffffff0h		; lose lowest tetrad 
		mov	CurrentLoc, edx	; (output is 16 bytes-aligned)
		mov	RequestLoc, edx
		
		call	HexViewStatus
				
		mov	word ptr ThisLine, 01h

@HexV_LoadBuffer:
		mov	edx, RequestLoc
		mov	cx, HexQuanta
		call	GetDiskBuffer	; Should return adress in ds:si, characters in cx
		test	cx, cx
		jz	@HexV_Fill	; End draw if nothing else is on buffer 
					; might be caused by EOF or other errors 
@HexV_BufferCheck:		
		; Fix cur_loc
		mov	edx, RequestLoc
		cmp	cx, 10h
		jb	@HexV_LastLine
		and	ecx, 0fff0h
		add	edx, ecx
		mov	RequestLoc, edx
		add	cx, si		; End of valid data block
		mov	BufferLimit, cx
@HexV_Next16:
		cmp	word ptr ThisLine, line_count-1	; if there are enough lines on screen,
		jae	@HexV_Done		; we are done
		cmp	si, BufferLimit	; Check buffer underrun
		jae	@HexV_LoadBuffer	; Try to load more if still needed

@HexV_LineStart:
		push	es
		push	ds			; es should keep video page offset for a while
		pop 	es 			; es is data 
		mov	LineBuffer, 0		; Clear current line buffer
		lea	di, LineBuffer
		mov	eax, CurrentLoc
		call	Int2Hex32
		mov	dword ptr [di-1], '  :'	; write ":  " instead of zero-terminator
		add	di,2			; increment di accordingly
		push	si
		mov	cx, 10h
@HexV_HexData:
		lodsb	; Load current character
		call	Int2Hex8
		mov	byte ptr [di-1], ' ' ; Make space instead of zero-byte
		cmp	cl,9
		jnz	@HexV_HexDelim
		mov	al, ' '
		stosb
@HexV_HexDelim:
		loop	@HexV_HexData
;@HexV_CharData:
		dec	di
		mov	eax, dword ptr s_Separate1
		stosd
		pop	si
		mov	cx, 4h
		rep	movsd
		xor	ax, ax
		stosb
@HexV_OutputBuffer:
		mov	eax, CurrentLoc
		add	eax, 10h
		mov	CurrentLoc, eax
		pop	es
		push	si
		mov	ax, ThisLine
		xchg	al, ah
		call	GetVideoOffset
		mov	di, ax
		lea	si, LineBuffer
		mov	cx, 80
		mov	ah, at_Main
		call	OutCharsAt_x
		inc	word ptr ThisLine
		pop	si
		jmp	@HexV_Next16
@HexV_LastLine:
		push	es
		push	ds			; es should keep video page offset for a while
		pop 	es 			; es is data 
		mov	LineBuffer, 0		; Clear current line buffer
		lea	di, LineBuffer
		push	cx
		mov	eax, CurrentLoc
		call	Int2Hex32
		mov	dword ptr [di-1], '  :'	; write ":  " instead of zero-terminator
		add	di,2			; increment di accordingly
		pop	cx
		push	cx
		push	si
		mov	dx, 16
@HexV_HexLast:
		lodsb			; Load current character
		call	Int2Hex8
		mov	byte ptr [di-1], ' ' ; Make space instead of zero-byte
		cmp	dl, 9
		jnz	@HexV_HexLD
		mov	al, ' '
		stosb
@HexV_HexLD:
		dec	dx
		loop	@HexV_HexLast
@HexV_LastChars:
		xor	ax, ax			; Need to mark line-end
		stosb				; before call to "FillUp"
		lea	di, LineBuffer
		mov	cx, 59
		mov	al, ' '
		call	sFillUp
		dec	di
		mov	eax, '  | '
		stosd
		pop	si
		pop	cx
		push	cx
		rep	movsb
		xor	ax, ax
		stosb

;@HexV_OutputLastBuffer:
		mov	cx, 80-16-1	; width - 16chars area - 1 space
		pop	ax
		add	cx, ax		

		mov	ax, ThisLine
		xchg	al, ah
		call	GetVideoOffset
		mov	di, ax
		mov	ah, At_Main
		lea	si, LineBuffer
		pop	es
		call	OutCharsAt_x
		jmp	@HexV_FillEx
@HexV_Fill:
		mov	ax, ThisLine
		xchg	al, ah
		call	GetVideoOffset
		mov	di, ax
@HexV_FillEx:		
		mov	ax, at_Main*256 + 32		
		mov	dx, 1800h
		call	FillUpAt
@HexV_Done:
		mov	sp,bp
		pop	bp
		lea	si, HintBar
		call	DisplayHintbar
		ret
UpdateHexView	endp


HexViewStatus	PROC
		
		push	es
		push	ds
		pop	es
		
		; Status line 
		lea	di, LineBuffer	; Output Mode Name
		lea	si, sModeHex
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
		mov	byte ptr [di-1],'/'
		mov	eax, RFileSize
		call	Int2Hex32

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
HexViewStatus	ENDP
		
end