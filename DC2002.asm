; Project DC
; Main executable module
; Initializes and terminates application, analyses user input and handles command string
; (c) 2002 Trance_C[RootTeam]

.model small
Local	@@

; 10-10-2002
; ToDo	: Help
; OPTION: Make confirm-Box		; User recieves a warning now
; OPTION: Fill with tabs inside file
; OPTION_X: Text View

include bios.inc

; Key codes
key_ESC		equ	1Bh
key_ENTER	equ	0Dh
key_BS		equ	8
key_Down	equ	50h
key_Up		equ	48h
key_Left	equ	4Bh
key_Right	equ	4Dh
key_Home	equ	47h
key_End		equ	4Fh
key_PgUp	equ	49h
key_PgDn	equ	51h


irp	key,<1,2,3,4,5,6,7,8,9,10>
key_F&key	equ	3Ah+key	; Write scancodes for all 10 Fn-keys
endm

m2m	MACRO	dest, src
	push	src
	pop	dest
	endm
	
.data



SegmentEnv	dw	0	; Environment strings segment

DriveLetter	dw	0	; Byte: Zero-terminated, drive-letter
FillByte	db	'\'
PwdBuffer	db	64 dup (?)
CmdLine		db	127 dup (?)

NameBuffer	db	127 dup (?)

CallIndex	label	byte
		irp	key,<1,2,3,4,5,6,7,8,9,10>
		db	3Ah+key	; Write scancodes for all 10 Fn-keys.
		endm	
		db	key_Up,		key_Down	
		db	key_PgUp,	key_PgDn	
		db	key_Home,	key_End	
CallidxCnt	dw	$-CallIndex
	
	align	2			
CallTable	label	word
		irp	key,<1,2,3,4,5,6,7,8,9>
		dw	OnKey_F&key
		endm
		dw	CloseApplication		; for F10
		dw	onKey_Up, 	OnKey_Down
		dw	OnKey_PgUp, 	OnKey_PgDn
		dw	OnKey_Home, 	OnKey_End
		dw	OnKey_Else			; default case


DcTitle		db	0Dh, 0Ah, "Project DeCode 2002 :: Extern Edition (1.0.14) - i486 DisAssembler",0Dh, 0Ah
		db	"(C) 2002 Trance_C[RootTeam] www.sysworld.net",0Dh, 0Ah,"$"
UsageNote	db	0Dh, 0Ah,"Usage:",09h,"dc2002.exe [[/da:<out_file>] <file_name>]",0Dh, 0Ah
		db	09h,"file_name - File to open immediatelly after startup",0Dh, 0Ah
		db	"$"
FileNotFound	db	"dc2002: file not found: $"

EXTRN		LineBuffer:Word, FinalCode:Byte, EIPLocal:DWORD, WHandle:Word 
	
.stack	100h
.code 
.486

	include	base.inc

	EXTRN	CloseFile:PROC, ExistFile:PROC, OpenFile:PROC, isRFileOpen:PROC
	EXTRN	InitVideo:PROC, InitRBuffer:PROC, UpdateView:PROC, DecodeCmd:PROC
	EXTRN	OnKey_F1:PROC, OnKey_F2:PROC, OnKey_F3:PROC, OnKey_F4:PROC, OnKey_F5:PROC
	EXTRN	OnKey_F6:PROC, OnKey_F7:PROC, OnKey_F8:PROC, OnKey_F9:PROC
	EXTRN	OnKey_Up:PROC, OnKey_Down:PROC, OnKey_PgUp:PROC, OnKey_PgDn:PROC, OnKey_Home:PROC
	EXTRN	OnKey_End:PROC, OnKey_Else:PROC

	EXTRN		DecodeCmd:PROC, GetDiskBuffer:PROC

	PUBLIC	CloseApplication, SaveDisAsm
	PUBLIC	OpenFileAttempt, SaveAttempt
	
; Procedure	OpenFileAttempt
; Purpose	Open File
; Imput		 Given by string at ds:si
; Comment	Since, DOS OpenFile requires full path, and user will be allowed to enter only filename,
;		Here we should insert the missing path. Try current directory, PATH, and program directory
OpenFileAttempt PROC
; Algorithm
;	1. Check: if filename is given with path, pass it to OpenFile
;	2. Insert current directory instead of path, try it with OpenFile
;	5. Return with failure if none of above succeeded
	push	es	;es is not video
	m2m	es, ds	; es is data!
	
	mov	al, byte ptr [si+1]
	cmp	al, ':'
	jz	@OpenF_PathGiven
	push	si	; save string
	lea	di, NameBuffer
	lea	si, DriveLetter
	call	strcpy_x
	pop	si
	mov	byte ptr [di-1], '\'	; change ending zero by slash
	call	strcpy_x
	lea	dx, NameBuffer		; should get current directory path + filename
	jmp	@OpenF_TryOn
@OpenF_PathGiven:
	mov	dx, si
@OpenF_TryOn:
	call	ExistFile
	jz	@OpenF_NotFound
	clc	; Open file for reading
	call	OpenFile
	xor	ax, ax
	inc	ax		; Successfuly opened
	pop	es
	ret
@OpenF_NotFound:	
	pop	es
	xor	ax, ax		; failure opeining
	ret

OpenFileAttempt endp


;	jmp	start
start:
	mov	ax, @data
	mov	es, ax

	mov	si, 2Ch			; Compiler issues warning if used word ptr [2ch]
	mov	ax, word ptr [si]
	mov	es:SegmentEnv, ax
	; ds should be PSP-segment
	
	mov	bx, ds:[80h]		; Read cmdline length from PSP
	xor	bh, bh			; need pure offset to 
	mov	BYTE PTR ds:[bx+81h],0	; convert cmdline to ASCIIZ
	mov	cx, bx
	mov	si, 81h
	lea	di, es:CmdLine
	cld
	rep	movsb
	
	m2m	ds, es
	
	mov	ah, 19h	; Dos function 19:
	int	21h		; Get current drive
	add	al, 'A'
	mov	ah, ':'
	mov	DriveLetter, ax
	
	mov	ah, 47h	; Dos function 47:
	xor	dx, dx
	lea	si, pwdbuffer	; where to store path
	int	21h		; Get Current directory

	call	InitRBuffer	; Prepare for file I/O operations

; Process command line here
	
	lea	si, CmdLine
	call	strlen			; if no parameters, go directly to interactive mode
	test	cx, cx
	jz	InterActive_Mode	; cannot use jcxz here ;( - too far to jump
	push	cx
	
	; Left-Trim is done here (no library calls)
LeftTrim:
	lodsb
	cmp	al, '!'	; al should be gt '!'
	jb	n_trim	
	cmp	al, '~'	; and lt '~' 
	ja	n_trim
	jmp	trim_done	; to exit cycle
n_trim:	
	loop	LeftTrim
	jmp	InterActive_Mode	; Here we get if string is over (i.e. cmdLine doesn't contain filenames/keys, etc.)

trim_done:		; Here we get after 1st "character" is found
	dec	si	; symbol was checked, need to rollback 1 char
	pop	ax	
	sub	ax, cx	; ax = offset in cmdLine
	
	; lea	si, cmdLine + ax

	mov	ax, 0900h		; Here we are sure that something was entered
	lea	dx, DcTitle
	int	21h
	
	push	si
	call	OpenFileAttempt
	jnz	InterActive_Mode
	mov	ax, 0900h
	lea	dx, FileNotFound
	int	21h
	call	strnext			; find end of filename
	mov	byte ptr [si-1], '$'	; terminate it in DOS-style
	
	pop	si
	mov	dx, si
	mov	ax, 0900h
	int	21h

Usage_Note:	
	
	mov	ax, 0900h
	lea	dx, UsageNote
	int	21h
	jmp	CloseApplication_NoVid	; need to save text in console, so skip "cls"

; Procedure	SaveAttempt
; Purpose	Check if saving is possible, and do save if so
; Input		ds:si 	- filename
;		cx	- Check if file existed before (cx=0 means don't care about overwrites)
; Output	WFile with decoded commands, WFile closed
SaveAttempt	proc
; 1. Check if path is given (if not. use current directory)
; 2. Call to OpenFile
; 3. Return if didn't open, save DizAsm if success
	push	es	;es is not video
	m2m	es, ds	; es is data!

	push	cx	; remove if cx unmodified

	mov	al, byte ptr [si+1]
	cmp	al, ':'
	jz	@SaveA_PathGiven
	push	si	; save string
	lea	di, NameBuffer
	lea	si, DriveLetter
	call	strcpy_x
	pop	si
	mov	byte ptr [di-1], '\'	; change ending zero by slash
	call	strcpy_x
	lea	dx, NameBuffer		; should get current directory path + filename
	jmp	@SaveA_TryOn
@SaveA_PathGiven:
	mov	dx, si
@SaveA_TryOn:
	pop	cx
	jcxz	@SaveA_NewFile
	call	ExistFile
	jz	@SaveA_NewFile
	
	xor	ax, ax		; Confirmation needed
	dec	ax		; this is "-1" result
	jmp	@SaveA_LocRet
	
@SaveA_NewFile:	
	stc	
	call	OpenFile
	jz	@SaveA_GoodToGo
	xor	ax, ax
	jmp	@SaveA_LocRet
	
@SaveA_GoodToGo:	
	call	SaveDisAsm
	xor	ax, ax
	inc	ax		; Successfuly opened (1)

@SaveA_LocRet:
	pop	es
	ret

SaveAttempt endp

; Procedure	SaveDisAsm
; Purpose	Save disassembly of whole RFile into WFile
; Input		RFile is Open, WFile also open	
; Output	WFile with decoded commands, WFile closed
SaveDisAsm	proc
; NOTES:	Take care of instructions at end of file and those in adjanced blocks.
StreamQuanta	equ	400h	;  1 Kb
		push	bp
		mov	bp, sp
CurrentLoc	equ	[bp-4]	; Offset of bytes being sent to output
BufferLimit	equ	[bp-06h]	; Offset in data segment: end of valid buffer block
ThisLine		equ	[bp-08h]
BufferNow	equ	[bp-0Ah]
		sub	sp, 0Ah

		xor	edx, edx
		mov	dword ptr CurrentLoc, edx
		mov	EIPLocal, edx		; initialize command counter in decode module

		
@SaveDA_LoadBuffer:
		mov	edx, dword ptr CurrentLoc
		mov	cx, StreamQuanta
		call	GetDiskBuffer	; Should return adress in ds:si, characters in cx
		
		test	cx, cx
		jz	@SaveDA_Close	; End if nothing else is on buffer: might be caused by EOF or errors 
		
@SaveDA_BufferCheck:		
		cmp	cx, StreamQuanta
		jb	@SaveDA_LtQuanta
		mov	cx, StreamQuanta
		jmp	@SaveDA_UseBuffer
@SaveDA_LtQuanta:		
		push	si
		add	si, cx
		xor	eax, eax
		mov	dword ptr [si], eax
		mov	dword ptr [si+4], eax
		mov	dword ptr [si+8], eax
		pop	si
;		jmp	@SaveDA_UseBuffer
@SaveDA_UseBuffer:
		add	cx, si		; End of valid data block
		mov	BufferLimit, cx	; Limit = min {StreamQuanta; cx}, (cx - length of cache block )
@SaveDA_NextLine:
		cmp	si, BufferLimit
		jae	@SaveDA_LoadBuffer	; Try to load more if still needed some 
@SaveDA_LineStart:
		mov	LineBuffer, 0		; Clear current line buffer
		lea	di, LineBuffer
		mov	eax, dword ptr CurrentLoc
		call	Int2Hex32
		dec	di			; not an end yet, should remove zero-terminator
		mov	eax, ' :'
		stosw

		push	di
		lea	di, FinalCode	; Should call decoder first, 'cause command length is unknown
		call	DecodeCmd	; Call to decoder
		pop	di

		movzx	eax, cx
		add	dword ptr CurrentLoc, eax
		sub	si, cx			; Second pass is hex output

@SaveDA_HexData:
		lodsb			; Load current character
		call	Int2Hex8
		dec	di
		loop	@SaveDA_HexData
; Tablute to 32 		
		mov	BufferNow, si
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
		dec	di
		
		mov	ax, 0A0Dh	; CR+LF
		stosw
		mov	byte ptr [di], 0
		
		lea	si, LineBuffer
		call	strlen
;	Direct it to disk.asm
		mov	ax, 4000h	; save string
		mov	bx, WHandle
		; mov	cx, Length(LineBuffer)
		lea	dx, LineBuffer
		int 	21h
; Primitive error handler
		jc	@SaveDA_Close
;		

		mov	si, BufferNow
		jmp	@SaveDA_NextLine

@SaveDA_Close:	
	stc
	call	CloseFile

	mov	sp, bp
	pop	bp		
	ret

SaveDisAsm endp



		
InterActive_Mode:		
	call	InitVideo
	m2m	es, 0b800h	
@@Wait_Keys:
	call	UpdateView
	mov	ah, 08h	; Read symbol from keyboard w/o echo, wait
	int	21h		; call to DOS
	test	al,al		; Check for extended symbols 
	jz	@@Ext_ASCII	; if found, direct to special handler
	cmp	al, key_ESC	; Escape typed
	jne	kb_l1		; 
	jmp	CloseApplication	; exit program - could become near, not short
kb_l1:	
	;compare other keys...
	
	jmp	short	@@Wait_Keys
@@Ext_ASCII:	
	int	21h		; get extended part into al
	lea	di, CallIndex
	mov	cx, CallidxCnt
	mov	si, cx
	push	es
	m2m	es, ds
	repne	scasb
	setz	al 
	add	cl, al
	sub	si, cx
	shl	si, 1
	mov	ax, word ptr CallTable[si]
	pop	es
	call	ax
	jmp	short	@@Wait_Keys

CloseApplication:
	@SetMode	3
CloseApplication_NoVid:
	call	IsRFileOpen
	jz	@Close_1
	call	CloseFile
@Close_1:
	mov 	ax, 4c00h
	int 	21h

end	start
