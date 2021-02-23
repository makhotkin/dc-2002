; Project DC 2002
; Contsins basic functions as String Output (Screen and other Strings),
; Integer to string/hex conversion and back

.model		small
.stack 		80h
include 	bios.inc
.data

cols_count	equ	80	; 80 Columns in VGA mode 3
line_count	equ	25	; 25 lines ---"---

.code
.386

PUBLIC	strcat, strcat_x, strcpy, strcpy_x, strlen, strnext, sFillUp, StrPos	; String Copy/Append/GetLen/Fill
PUBLIC	Int2Hex8, Int2Hex16, Int2Hex32, Hex2IntDX			; Hex number displaying and parsing
PUBLIC	Int2Hex8f, Int2Hex16f, Int2Hex32f			; Hex number display with leading zero 
PUBLIC	OutStringAt, OutStringAt_x, OutString, OutString_x	; Display strings with attribute/positioning
PUBLIC	OutCharsAt_x, OutChars_x				; Display character chains
PUBLIC	FormatOutAt_x, FormatOutAt				; Formatted output
PUBLIC	OutStringCntr, OutStringCntrAt				; Display string in center of given line
PUBLIC	FillLine, FillLineAt, TabulateAt, Tabulate, FillUpAt		; Tabulation, line and screen filling
PUBLIC	Fill2ColumnAt, Fill2Column				; Screen filling
PUBLIC	SetAttrib_x, SetAttrib					; Set Character attributes
PUBLIC	GetVideoOffset, GetVideoCoords				; Video pointer positioning
PUBLIC	PrintAtCursor, TypeAtCursor, HideCursor, ShowCursor	; Typing Emulation 


; Great macro to convert lowest 4 bits of al into ascii-code
; Respect to Zubkov S.V.
AL4_2ASCII	MACRO
	and	al, 0fh
	cmp	al, 10
	sbb	al, 69h
	das
endm


; Function	StrCat
; Purpose	Append string at ds:si to the end of string at es:di
strcat:
	push	si
	push	di
	push	cx
	xor	cx,cx
	dec	cx	; make cycle "infinite"
	xor	al, al
	repe 	scasb	; this should deliver to end of string
	dec	di	; if got zero-byte, must overwrite it
@strcat_loop:
	lodsb		; Send byte from ds:si
	stosb		; to es:di through al
	test	al, al	
	jnz	@strcat_loop	; Repeat until got zero-byte in src string
	pop	cx
	pop	di
	pop	si
	ret

strcat_x:
	xor	cx,cx
	dec	cx	; make cycle "infinite"
	xor	al, al
	repe 	scasb	; this should deliver to end of string
	dec	di	; if got zero-byte, must overwrite it
@strcat_x_loop:
	lodsb		; Send byte from ds:si
	stosb		; to es:di through al
	test	al, al	
	jnz	@strcat_x_loop	; Repeat until got zero-byte in src string
	ret

; Function	StrNext
; Purpose	Set ds:di pointer behind current line zero-byte (ideal for searching trough strings)

strnext:
	lodsb			; repne would involve cx, sometimes we should save it
	test	al, al
	jnz	strnext
	ret	
		
; Procedure	StrCpy
; Purpose	Copy String at es:di to ds:si
; Input		ds:si source string, es:di destination string
; Output	ds:si untouched; es:di contains ds:si + ending zero-byte
strcpy:
	push	si
	push	di
	push	ax
@strcpy_loop:
	lodsb		; Send byte from ds:si
	stosb		; to es:di through al
	test	al, al	
	jnz	@strcpy_loop	; Repeat until got zero-byte in src string
	pop	ax
	pop	di
	pop	si
	ret

strcpy_x:		; The fastest (but not safest)
	lodsb		; Send byte from ds:si
	stosb		; to es:di through al
	test	al, al	
	jnz	strcpy_x	; Repeat until got zero-byte in src string
	ret

; Procedure	StrLen
; Purpose	Get length of non-zero bytes chain
; Input		ds:si	- source string
; Output	cx	- string length
StrLen:
	push	si
	xor	cx, cx		; Rep not used here
@StrLen_loop:
	lodsb
	inc	cx	
	test	al, al
	jnz	@StrLen_loop
	dec	cx
	pop	si
	ret	

; Procedure	StrPos
; Purpose	Get index of desired item in string
; Input		ds:si	- source string
; Output	cx = index if found, else -1
StrPos:
	push	di		; save di
	mov	di, si		
	push	ax		; save symbol to find
	xor	cx, cx		
	xor	ax, ax		; find zero-byte (end of string)
	dec	cx		; scan up to 65535 bytes
	push	cx		; save
	std			; go forward
	repe	scasb		; find end of line
	cld			; go backward
	pop	ax		; extract starting postion
	sub	ax, cx		; ax = string length
	xchg	cx, ax		; cx = strlen
	pop	ax		; ax = symbol to find
	repe	scasb		; find! - cx will become zero if not found!
	pop	di		; restore di
	std			; set normal direction
	ret	

; Procedure	Int2Hex8
; Purpose	Type number in AL into es:di in hexadecimal form, close with zero
Int2Hex8:
	push	dx
	xchg	ax, dx
	rol	dl, 4
	mov	al, dl
	AL4_2ASCII
	stosb		
	rol	dl, 4
	mov	al, dl
	AL4_2ASCII
	stosb		
	xor	ax, ax
	stosb
	xchg	ax, dx
	pop	dx
	ret

; Procedure	Int2Hex16
; Purpose	Type number in AX into es:di in hexadecimal form, close with zero
Int2Hex16:
	push	dx
	xchg	ax, dx
	mov	cx, 4
@Int2Hex16_loop:
	rol	dx, 4
	mov	al, dl
	AL4_2ASCII
	stosb		
	loop	@Int2Hex16_loop
	xor	ax, ax
	stosb
	xchg	ax, dx
	pop	dx

; Procedure	Int2Hex32
; Purpose	Type number in EAX into es:di in hexadecimal form, close with zero
Int2Hex32:
	push	edx
	xchg	eax, edx
	mov	cx, 8
@Int2Hex32_loop:
	rol	edx, 4
	mov	al, dl
	AL4_2ASCII
	stosb		
	loop	@Int2Hex32_loop
	xor	ax, ax
	stosb
	xchg	eax, edx
	pop	edx
	ret

; Procedure	Int2Hex8f
; Purpose	Store @ es:di 8-bit number in 0xxh format
; Input		al - byte
; Output	es:di - zero terminated string
Int2Hex8f:
	push	dx
	xchg	ax, dx
	rol	dl, 4
	mov	al, dl
	AL4_2ASCII
	cmp	al, 39h
	jbe	@I2H8f_1
	push	ax
	mov	al, '0'
	stosb
	pop	ax
@I2H8f_1:
	stosb		
	rol	dl, 4
	mov	al, dl
	AL4_2ASCII
	stosb		
	mov	al, 'h'
	stosb
	xor	ax, ax
	stosb
	xchg	ax, dx
	pop	dx
	ret
		
; Procedure	Int2Hex16f
; Purpose	Store @ es:di 16-bit number in 0xxh format
; Input		ax - word
; Output	es:di - zero terminated string
Int2Hex16f:
	push	dx
	xchg	ax, dx
	rol	dx, 4
	mov	al, dl
	AL4_2ASCII
	cmp	al, 39h
	jbe	@I2H16f_1
	push	ax
	mov	al, '0'
	stosb
	pop	ax
@I2H16f_1:
	stosb	
	mov	cx, 3
@I2H16f_2:
	rol	dx, 4		
	mov	al, dl		; 2 bytes
	AL4_2ASCII
	stosb			; 1 byte
	loop	@I2H16f_2
	mov	al, 'h'
	stosb
	xor	ax, ax
	stosb
	xchg	ax, dx
	pop	dx
	ret			

; Procedure	Int2Hex32f
; Purpose	Store @ es:di 16-bit number in 0xxh format
; Input		Eax - word
; Output	es:di - zero terminated string
Int2Hex32f:
	push	edx
	xchg	eax, edx
	rol	edx, 4
	mov	al, dl
	AL4_2ASCII
	cmp	al, 39h
	jbe	@I2H32f_1
	push	ax
	mov	al, '0'
	stosb
	pop	ax
@I2H32f_1:
	stosb	
	mov	cx, 7
@I2H32f_2:
	rol	edx, 4		
	mov	al, dl		; 2 bytes
	AL4_2ASCII
	stosb			; 1 byte
	loop	@I2H32f_2
	mov	al, 'h'
	stosb
	xor	ax, ax
	stosb
	xchg	eax, edx
	pop	edx
	ret	

; String output

; Procedure 	OutStingAt
; Purpose	Display String at given Position with attribute
; Input		ds:si  	- string adress
;		es	- video page segment
;		(dl; dh)	- screen coordinates (x, y)
;		ah	- attribute
OutStringAt:
	push	ax
	mov	ax, dx
	call	GetVideoOffset
	mov	di, ax
	pop	ax
; Procedure 	OutStingAt_x
; Purpose	Display String at current Position with attribute
; Input		ds:si  	- string adress
;		es:di	- video memory address
;		ah	- attribute
OutStringAt_x:
	lodsb			
	test	al, al		
	jz	@OutStringAt_x_Done
	stosw			
	jmp	short	OutStringAt_x
@OutStringAt_x_Done:
	ret


; Procedure 	OutSting
; Purpose	Display String at given Position w/o attribute
; Input		ds:si  	- string adress
;		es	- video page segment
;		dx	- screen coordinates (dl=x, dh=y)
; Output	ds:si points behind zero-byte, useful for sequent calls, when string arrays processed
OutString:
	mov	ax, dx
	call	GetVideoOffset
	mov	di, ax
	
; Procedure 	OutSting
; Purpose	Display String at current Position don't change attribute
; Input		ds:si  	- string adress
;		es	- video page segment
; Output	ds:si points behind zero-byte, useful for sequent calls, when string arrays processed
OutString_x:
	lodsb
	test	al, al
	jz	@OutString_x_Done
	stosb			
	inc	di
	jmp	short	OutString_x
@OutString_x_Done:
	ret

; Procedure	GetVideoOffset
; Purpose	Calculate screen output pointer offset, corresponding to needed position
; Input		AL - x-coord, AH - y-coord
; Output	AX - Buffer offset
; Registers	All preserved
GetVideoOffset:
	push	cx	; Save CX (we didn't mention it would change)
	mov	ch, al	; Save x-coordinate in counter-high
	mov	al, ah	; Move y to multiplier
	mov	cl, cols_count	; Prepare yet another multiplier
	mul	cl	; ax = cl * al (ch stays untouched)
	add	al, ch	; Add x-coordinate to offset
	adc	ah, 0	; make carry if needed
	shl	ax, 1	; must multiply, 'cause 2 bytes per character
	pop	cx
	ret

; Procedure	GetVideoCoords
; Purpose	return screen coordinates from pointer
; Input		AX - Buffer offset
; Output	AL - x-coord, AH - y-coord
; Registers	All preserved
GetVideoCoords:
	push	cx
	shr	ax, 1	; "2-byte per character" fix
	mov	cl, cols_count 
	div	cl	
	xchg	ah, al	; ah = remainder (x), must swap "x" and "y" in ax
	pop	cx
	ret

; Procedure	FillLineAt
; Purpose	Fill line, pointed by di with spaces with given attribute
; Input		ax - character with attribute
;		di - character to start with: (x + y*cols_count)*2
; Output	Line filled with spaces of given attribute.
; All Registers preserved
FillLineAt:
	push	cx
	push	ax
	mov	ax, di
	call	GetVideoCoords
	movzx	cx, al
	neg	cx
	add	cx, cols_count 
	pop	ax
	rep	stosw	
	pop	cx
	ret

; Procedure	FillLine
; Purpose	Fill line, pointed by di with spaces with given attribute
; Input		ax - character with attribute
;		di - character to start with: (x + y*cols_count)*2
; Output	Line filled with spaces of given attribute.
; All Registers preserved
FillLine:
	push	cx
	push	ax
	mov	ax, di
	call	GetVideoCoords
	movzx	cx, al
	neg	cx
	add	cx, cols_count 
	pop	ax
@FillL_loop:	
	stosb
	inc	di
	loop	@FillL_loop	
	pop	cx
	ret
	
; Procedure	SetAttrib
; Purpose	Set Attributes for given number of bytes, start @ dl, dh
; Input		ah - attribute
;		dx - coordinates to start filling
;		cx - number of bytes to change attribute
SetAttrib:
	push	ax
	mov	ax, dx
	call	GetVideoOffset
	mov	di, ax
	pop	ax	
; Procedure	SetAttrib_x
; Purpose	Set Attributes for given number of bytes at curent position
; Input		ah - attribute
;		cx - number of byte sto change attribute
SetAttrib_x:
	xchg	al, ah
@SetAttr_loop:	
	inc	di
	stosb
	loop	@SetAttr_loop
	xchg	al, ah
	ret	
	

; Procedure	Fill2ColumnAt
; Purpose	Fill with spaces, until given column reached * attribute set by ah
; Input		ax - character with attribute
;		cx - column to tabulate by
;		di - character to start with: (x + y*cols_count)*2
; Output	Line filled with spaces of given attribute.
; All Registers preserved
Fill2ColumnAt:
	push	ax
	push	cx
	mov	ax, di
	call	GetVideoCoords
	movzx	cx, al
	neg	cx
	pop	ax
	add	cx, ax
	pop	ax
	rep	stosw	
	ret

; Procedure	Fill2Column
; Purpose	Fill line, pointed by di with spaces with given attribute
; Input		ax - character with attribute
;		cx - column to tabulate by
;		di - character to start with: (x + y*cols_count)*2
; Output	Line filled with spaces of given attribute.
; All Registers preserved
Fill2Column:
	push	ax
	push	cx
	mov	ax, di
	call	GetVideoCoords
	movzx	cx, al
	neg	cx
	pop	ax
	add	cx, ax
	pop	ax
@Fill2C_Loop:
	stosb
	inc	di
	loop	@Fill2C_Loop
	ret
	

; Procedure	FillUpAt
; Purpose	Fill Screen with ax-given character and attribute, stop at dx coordinate 
; Input		ax - character with attribute
;		dx - coordinates for ending point
;		di - character to start with: (x + y*cols_count)*2
; Output	Line filled with spaces of given attribute.
; All Registers preserved
FillUpAt:
	push	cx
	push	ax
	mov	ax, dx
	call	GetVideoOffset
	sub	ax, di
	shr	ax, 1
	mov	cx, ax
	pop	ax
	rep	stosw
	pop	cx
	ret
	
; Procedure	sFillUp
; Purpose	Fill String, until its size becomes dl
; Input		al - character 
;		cx - coordinates for ending point
;		di - line beginning
; Output	Line filled with spaces of given attribute.
; All Registers preserved
sFillUp:			
	push	si		; save si
	push	ax
	mov	si, di		
	call	strnext		; find ending zero-byte of "di"
	sub	si, di		; si = length of "di"
	add	di, si		; di = coord of end for string
	dec	di	; to overwrite ending zero
	sub	cx, si		; cx = number of bytes to fill
	inc	cx	; fixed strlen
	pop	ax
	js	@sFillUp_1	; skip if negative
	jz	@sFillUp_1	; skip if zero
	rep	stosb		; fill
	xor	al, al
	stosb			; close string
@sFillUp_1:	
	pop	si		; restore si
	ret	

; Procedure	TabulateAt
; Purpose	Fill up to 8 bytes with spaces with given attribute
; Input		ax - character with attribute
; Output	Line filled with spaces of given attribute.
; All Registers preserved	
TabulateAt:
	mov	al, 32
@TabA_loop:	
	stosw
	test	di, 0fh
	jnz	@TabA_loop
	ret

; Procedure	Tabulate
; Purpose	Fill up to 8 bytes with spaces
; Input		ax - character with attribute
; Output	Line filled with spaces of given attribute.
; All Registers preserved	
Tabulate:
	mov	al, 32
@Tab_loop:
	stosb
	inc	di
	test	di, 0fh
	jnz	@Tab_loop
	ret	

; Procedure	OutStringCntr	/
; Purpose	Display an ASCIIZ string without attributes at es:di
; Input		string	- string offset in it's segment
;		ds:si	- segment & offset containing string
;		dh	- Line Number
;		es	- video page segment
; Output	String on video RAM or buffer at given coordinates
; Resiters	Kept untouched,
;		AH 	- destroyed
;		si 	- points behind string zero-byte
OutStringCntr	proc	
	push	di	; save registers - they'll get changed
	push	cx
	xor	cl, cl
	call	strlen
	xor	ch, ch
	sub	cl, Cols_Count
	neg	cl
	shr	cl, 1
	dec	cl
	movzx	ax, cl
	mov	ah, dh
	call	GetVideoOffset
	mov	di, ax
	pop	cx
	mov	al, ' '
	stosb
	inc	di
	call	OutString_x
	mov	al, ' '
	stosb
	inc	di
	pop	di
	ret
OutStringCntr	Endp

; Procedure	OutStringCntrAt
; Purpose	Display an ASCIIZ string without attributes at es:di
; Input		string	- string offset in it's segment
;		ds:si	- segment & offset containing string
;		dh	- Line Number
;		ah	- Attribute
;		es	- video page segment
; Output	String on video RAM or buffer at given coordinates
; Resiters	Kept untouched,
;		AH 	- destroyed
;		si 	- points behind string zero-byte
OutStringCntrAt	proc	; check centering!
	push	di	; save registers - they'll get changed
	push	ax
	push	cx
	xor	cl, cl
	call	strlen
	xor	ch, ch
	sub	cl, Cols_Count
	neg	cl
	shr	cl, 1
	dec	cl
	movzx	ax, cl
	mov	ah, dh
	call	GetVideoOffset
	mov	di, ax
	pop	cx
	pop	ax
	mov	al, ' '
	stosw
	call	OutStringAt_x
	mov	al, ' '
	stosw
	pop	di
	ret
OutStringCntrAt	Endp
	

PrintAtCursor:
	push	di
	push	ax	; Character is sent via 'al'
	@GetCur
	mov	ax, dx
	call	GetVideoOffset
	mov	di, ax
	pop	ax
	stosb	
	pop	di
	ret
	
TypeAtCursor:
	push	dx	; dx used is.
	push	di
	push	ax	; Character is sent via 'al'
	@GetCur
	push	dx
	inc	dl
	@SetCurPos
	pop	ax
	call	GetVideoOffset
	mov	di, ax
	pop	ax
	stosb	
	pop	di
	pop	dx
	ret	

HideCursor:
	push	ax	; maybe pusha is better?
	push	cx
	push	dx
	push	bx
	@SetCurPos	0, 25
	pop	bx
	pop	dx
	pop	cx
	pop	ax
	ret
	
; Input		(dl, dh) - (x;y)	
ShowCursor:
	push	bx
	xor	bh,bh
	mov	ah,02h
	int	10h
	pop	bx
	ret

; Print a serie of chars with attribute
; cx - length
OutCharsAt_x:
	lodsb
	stosw
	loop OutCharsAt_x
	ret

OutChars_x:
	movsb
	inc	di
	loop OutChars_x
	ret


; Procedure 	FormatOutAt_x
; Purpose	Display String at current Position with attribute
; Input		ds:si  	- string adress
;		es:	- video memory page
;		ah	- attribute
;		dx	- coordinates
FormatOutAt:
	push	ax
	mov	ax, dx
	call	GetVideoOffset
	mov	di, ax
	pop	ax
; Procedure 	FormatOutAt_x
; Purpose	Display String at current Position with attribute
; Input		ds:si  	- string adress
;		es:di	- video memory address
;		ah	- attribute
FormatOutAt_x:
	lodsb			
	test	al, al		
	jz	@FmtOutAt_done
	cmp	al, 09h		; tabulation
	jnz	@FmtOutAt_Ord
	call	TabulateAt
	jmp	short	FormatOutAt_x
@FmtOutAt_Ord:
	stosw			
	jmp	short	FormatOutAt_x
@FmtOutAt_done:
	ret
ret

; Procedure	Hex2IntDX
; Purpose	Covert ASCIIZ string (8 first chars) into CPU-understandable number.
; Input		ds:si	- string with ASCIIZ number
; Output	edx 	- decoded value
Hex2IntDX:
	xor	edx, edx
	mov	cx, 8
@H2I_Loop:	
	lodsb
	test	al, al
	jz	@H2I_Done
	shl	edx, 4
	sub	al, 30h
	cmp	al, 31h
	jbe	@H2I_CaseOk
	sub	al, 20h	
@H2I_CaseOk:
	cmp	al, 9
	jbe	@H2I_Fix
	sub	al, 7
@H2I_Fix:
	cbw
	cwde	
	add	edx, eax
	loop	@H2I_Loop
@H2I_Done:	
	ret

end	
	