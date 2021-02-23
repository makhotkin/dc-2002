; Project DC 2002
; Views, scrolling routines

.model		small

LOCAL	@@

.stack		80h

cols_count	equ	80	; 80 Columns in VGA mode 3
line_count	equ	25	; 25 lines ---"---

at_Main		equ	1Bh
at_Status	equ	0B1h

VideoSegment	equ 	0B800h	; Video refresh buffer

m2m	MACRO	dest, src
	push	src
	pop	dest
	endm

.data

public	sAppTitle
sAppTitle	db	"DeCode 2002: Extern",0

; Data quantizion
TextQuanta	equ	2048
HexQuanta	equ	TextQuanta / 4
CodeQuanta	equ	HexQuanta / 4

ModeText	equ	1
ModeHex	equ	2
ModeDecode	equ	4

ViewMode	dw	2	; Set according to enumeration above
public		ViewOffset
ViewOffset	dd	0	; Offset from R-File beginning, starting point to show

sc_ModeSelect	db	"Choose View",0
;sc_ModeText	db	"Plain Text ",0
sc_ModeHex	db	"Hexadecimal",0
sc_ModeCode	db	"Disassembly",0


NoFileTitle	db	"Press F9 to Open a file",9,9,"***",9,"DeCode 2002 - Extern Edition / v 1.0.14",0
NoFileHintbar	db	8 dup ("      ",0),"Open  ",0,"Quit  ",0

public		LineBuffer
LineBuffer	db	82 dup (0)	; Dedicated to functions writing onto screen trough string buffer

ErrTitle		db	"Error",0

OpenTitle	db	"Open file",0
OpenPrompt	db	"Enter file name: ",0

SAveTitle	db	"Save DisAssembly",0
SavePrompt	db	"Enter file name, (if no path specified, current dir will be used: ",0

aFileNotFound	db	"File not found",0
aInvalidOffset	db	"Offset is out of file",0
aDisasmSaved	db 	"Disassembly Saved",0
aFileOverWrite	db	"File will be overwritten",0
aValidKeys	db	"Made by RootTeam, www.sysworld.net",0

EXTRN		RFileSize:dword

.code
.386

PUBLIC	UpdateView, OnKey_Down, OnKey_Up, OnKey_F9, OnKey_F8, OnKey_F7, OnKey_F6
PUBLIC	OnKey_F5, OnKey_F4, OnKey_F3, OnKey_F2, OnKey_F1, OnKey_Home, OnKey_End
PUBLIC	OnKey_PgUp, OnKey_PgDn, OnKey_Else


include 	base.inc	; library functions
EXTRN	isRFileOpen: PROC, DisplayHintBar: PROC, OpenFileAttempt:PROC, SaveAttempt:Proc
EXTRN	ErrorBox:Proc, ChooseList:Proc, DrawFrame:PROC, OpenFile:Proc, Pascal DefInputBox:Proc, ViewOffsetRequest:PROC
EXTRN	UpdateHexView:proc, UpdateDecodeView:Proc, DecodeCpuSelect:Proc, DecodeModeSelect:PROC

EXTRN	DC_LineDown:PROC, DC_LineUp:PROC, DC_PageDown:PROC
EXTRN	Hex_PageDown:PROC, Hex_LineDown:PROC, Hex_LineUp:PROC, Hex_PageUp:PROC


OnKey_Down	PROC
	call	isRFileOpen
	jz	@Down_Done
	
	test	ViewMode, ModeDecode
	jz	@Down_Next
	call	DC_LineDown
	; may insert break here
@Down_Next:	
	test	ViewMode, ModeHex
	jz	@Down_Text
	call	Hex_LineDown
@Down_Text:	
	; Add code, when you implement that module correctly

@Down_Done:
	ret

OnKey_Down endp

OnKey_Up	PROC
	call	isRFileOpen
	jz	@Up_Done
	
	test	ViewMode, ModeDecode
	jz	@Up_Next
	call	DC_LineUp
@Up_Next:	
	test	ViewMode, ModeHex
	jz	@Up_Text
	call	Hex_LineUp
@Up_Text:	
	; Add code, when you implement that module correctly
@Up_Done:
	ret

	
OnKey_Up	Endp



OnKey_F1	PROC
	call	isRFileOpen
	jz	@F1_Done
	
	lea	si, aValidKeys
	clc
	call	ErrorBox
@F1_Done:	
	ret

OnKey_F1 	endp

OnKey_F2	PROC
	call	isRFileOpen
	jz	@F2_Done	; option alvaliable only if a file is open
	
	test	ViewMode, ModeDecode
	jnz	DecodeModeSelect
		
@F2_Done:
	ret
OnKey_F2 	endp

OnKey_F3	PROC
	
	
	ret

OnKey_F3 	endp


OnKey_F4	PROC
	call	isRFileOpen
	jz	@F4_Done	; option alvaliable only if a file is open
	
	mov	cx, 2
	lea	si, sc_ModeSelect
	call	ChooseList
	jcxz	@F4_Done
		
	mov	ax, 1	
	shl	ax, cl
;	shr	ax, 1	- needed if text mode enabled
	mov	ViewMode, ax
@F4_Done:
	ret

OnKey_F4 	endp

OnKey_F5	PROC
	call	isRFileOpen
	jz	@F5_Done
	test	ViewMode, ModeDecode or ModeHex
	jz	@F5_Done
	call	ViewOffsetRequest
	test	ax, ax
	jz	@F5_Done
	cmp	edx, RFileSize
	jae	@F5_Error	
	mov	ViewOffset, edx
	jmp	@F5_Done	
@F5_Error:	
	lea	si, aInvalidOffset
	stc
	call	ErrorBox	
@F5_Done:	
	ret

OnKey_F5 	endp

OnKey_F6	PROC
	call	isRFileOpen
	jz	@F6_Done	
	
	test	ViewMode, ModeDecode
	jnz	DecodeCpuSelect
	
@F6_Done:
	ret

OnKey_F6 	endp

OnKey_F7	PROC
	
	
	ret

OnKey_F7 	endp

OnKey_F8	PROC
	call	isRFileOpen
	jz	@@Done	
	
	test	ViewMode, ModeDecode
	jz	@@Done
	push	offset SaveTitle
	push	offset SavePrompt
	push	offset LineBuffer
	call	DefInputBox
	test	ax, ax
	jz	@F8_NoSave
	jns	@F8_TrySave
@F8_NoSave:	
	call	HideCursor
	jmp	@@Done
@F8_TrySave:
	call	HideCursor
	lea	si, LineBuffer
	mov	cx, 1
@F8_DoSave:	
	call	SaveAttempt
	test	ax, ax
	jz	@F8_Error
	jns	@@ShowOkMessage
@F8_Confirm:
	lea	si, aFileOverWrite
	clc
	call	ErrorBox
	xor	cx, cx
	lea	si, LineBuffer	
	jmp	@F8_DoSave
		
@F8_Error:
	lea	si, aFileNotFound
	stc
	call	ErrorBox
	jmp	@@Done
@@ShowOkMessage:
	lea	si, aDisasmSaved
	clc
	call	ErrorBox
;	jmp	@@Done
		

	; Here	
@@Done:	
	ret

OnKey_F8 	endp

OnKey_F9	PROC
	push	offset OpenTitle
	push	offset OpenPrompt
	push	offset LineBuffer
	call	DefInputBox
	test	ax, ax
	jz	@F9_NoOpen
	jns	@F9_TryOpen
@F9_NoOpen:	
	call	HideCursor
	jmp	@F9_Done
@F9_TryOpen:
	call	HideCursor
	lea	si, LineBuffer
	call	OpenFileAttempt
	test	ax, ax
	jnz	@F9_Done
	lea	si, aFileNotFound
	stc
	call	ErrorBox	
@F9_Done:
	ret

OnKey_F9 	endp

OnKey_Home	proc
	xor	eax, eax
	mov	ViewOffset, eax
	ret

OnKey_Home endp


OnKey_PgUp	proc
	call	isRFileOpen
	jz	@PgUp_Done
	
	test	ViewMode, ModeHex
	jz	@PgUp_Text
	call	Hex_PageUp
	
@PgUp_Next:	
	; Not implemented, 'cause I do not have algorithm of scrolling up command-by-command
	;test	ViewMode, ModeDecode
	;jz	@PgUp_Next
	;call	DC_PageUp

@PgUp_Text:	
	; Add code, when you implement that module correctly

@PgUp_Done:		
	ret

OnKey_PgUp endp

OnKey_End	proc
	call	isRFileOpen
	jz	@@End_Done
	
	mov	eax, RFileSize
	dec	eax
	mov	ViewOffset, eax
	
@@End_Done:
	ret

OnKey_End endp


Onkey_PgDn	proc
	call	isRFileOpen
	jz	@@PgDn_Done
	
	test	ViewMode, ModeDecode
	jz	@PgDn_Next
	call	DC_PageDown
	; may insert break here
@PgDn_Next:	
	test	ViewMode, ModeHex
	jz	@PgDn_Text
	call	Hex_PageDown
@PgDn_Text:	
	; Add code, when you implement that module correctly

@@PgDn_Done:	
	ret

Onkey_PgDn endp

OnKey_Else	proc
	
	
	ret

OnKey_Else endp


UpdateView	PROC
	m2m	es, VideoSegment
	call	isRFileOpen
	jz	@@UpdateV_NoFile
	
	mov	cx, ViewMode
	test	cx, ModeHex
	jnz	UpdateHexView	; jump here is a bit doubtful... it would return to KB_loop

	mov	cx, ViewMode
	test	cx, ModeDecode
	jnz	UpdateDecodeView	; jump here is a bit doubtful... it would return to KB_loop
	jmp	@@UpdateDone

@@UpdateV_NoFile:
	xor	di, di
	mov	ax,at_Status*256 + 32
	lea	si, NoFileTitle
	call	FormatOutAt_x
	mov	dx, 100h
	call	FillUpAt
	mov	ax, at_Main*256 + 176
	mov	dx, 1750h
	call	FillUpAt
	lea	si, NoFileHintbar
	Call	DisplayHintBar
@@UpdateDone:
	ret
UpdateView	endp



end


