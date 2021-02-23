; Project "DC 2002"
; User Interface Helper Code


at_DownNum	equ	07h	; Numbers of Fn-Keys - White on BLack
at_DownHint	equ	30h	; Command hints - black on Cyan
at_DialogBox	equ	7fh	; Dialog Box - White on gray
at_DialogBoxInv	equ	0f7h
at_InputField	equ	0fh	; input field - white on black
at_ErrorBox	equ	04fh	; Error Box - White on red
at_Button	equ	0E0h	; Button
at_ScreenFill	equ	01bh
at_Selected	equ	0Fh	; selection - white on black


; Key codes
key_ESC		equ	1Bh
key_ENTER	equ	0Dh
key_BS		equ	8
key_Down	equ	50h
key_Up		equ	48h


;Pseudographics
pg_GG		equ	0C9h		; Like Russian "G"
pg_iGG		equ	0BBh		; The same in vertical mirror (^|)
pg_LL		equ	0C8h		; L - corner
pg_JJ		equ	0BCh		; J - corner
pg_DH		equ	0CDh		; = - line
pg_II		equ	0BAh		; || - line

; Theese values can be converted into variables with code changes,
;  that might allow use of different video modes
cols_count	equ	80	; 80 Columns in VGA mode 3
line_count	equ	25	; 25 lines ---"---


InputDefX		equ	4
InputDefY		equ	7
InputDefHeight		equ	6
InputDefWidth		equ	70

InputDefPos		equ	704h
InputDefSize		equ	646h

InputDefPromptPos	equ	InputDefPos + 203h


InputDefTypePos	equ	InputDefPromptPos + 101h
InputDefTypeLen	equ	InputDefWidth - 8

InputDefTypeBack	equ	250	; a dot, like in NC

ErrorDefPos		equ	714h
ErrorDefSize		equ	828h

ErrorDefPromptPos	equ	ErrorDefPos + 203h

include 	bios.inc

.model 	small
.data


sb_Buffer0	db	3 dup (0)


sOk		db	" Ok ",0
sCancel		db	" Cancel ",0
sErrorWndTitle	db	"Error",0
sMessageWndTitle	db	"Message",0

sBuffer		db	9 dup (0)

.code
.386

include	base.inc

PUBLIC	ErrorBox, ChooseList, DrawFrame, InitVideo, DisplayHintBar, DefInputBox, ViewOffsetRequest

; set initial screen mode (text 80*25), setup high-bit of attribute byte behavior (toggle intensity instead of blinking)
InitVideo:
	@SetMode	3
	@SetIntensity
	call	HideCursor
ret

; Procedure	ViewOffsetRequest
; Purpose	Display input box at top of screen
; Input		dx - coordinates for input-field
; Output	edx - resulting output
ViewOffsetRequest	PROC
	
	mov	dx, 0100h
	mov	cx, 8
	xor	ax, ax
	call	DrawInputField
		
	lea	si, sBuffer	
	mov	cx, 8
	mov	ax, 16
	call	ReadKeyInput
	test	ax, ax
	jz	@ViewOR_Done	
@ViewOR_Decode:	
	lea	si, sBuffer
	call	Hex2IntDX
	xor	ax, ax
	inc	ax	
@ViewOR_Done:	
	ret

ViewOffsetRequest endp


; Procedure	DefInputBox
; Pupose	Display an input box with prompt and title, store what user types in buffer
DefInputBox	PROC	Pascal, WndTitle:word, WndPrompt:word, strBuffer:word
		mov	cx, InputDefPos
		mov	dx, InputDefSize
		mov	ah, at_DialogBox
		call	DrawFrame
		mov	ah, at_DialogBoxInv
		mov	dx,  InputDefPos
		mov	si, WndTitle
		Call	OutStringCntrAt
		
		mov	ah, at_DialogBox
		mov	si, WndPrompt
		mov	dx, InputDefPromptPos
		call	OutStringAt

		mov	ax, 1
		mov	dx, InputDefTypePos
		mov	cx, InputDefTypeLen
		call	DrawInputField
		

		mov	si, strBuffer
		mov	cx, InputDefTypeLen
		xor	ax, ax
		call	ReadKeyInput
		ret
DefInputBox	endp


; Procedure	DrawFrame
; Purpose	Display Rectangle Frame of given size with shadow 
; Input		ah - Attribute
;		(cl, ch): x, y - top-left corner of frame
;		(dl, dh) - dimensions
;		es - video page segment
; Output	Frame at video of other buffer pointed by es
DrawFrame	PROC
	push	bp
	mov	bp, sp
	sub	sp, 4
VidOffset	equ	[bp-2]
w_dth		equ	[bp-4]
attrib		equ	[bp-3]

ShadowAttrib	equ	07h	; gray on bleck

	push	di			; Save di (maybe it is used outside proc)
	mov	attrib, ah		; Save attribute
	mov	ax, cx			; Load parameters
	call	GetVideoOffset	; Calculate vidoe offset
	mov	VidOffset, ax		; store v/offset
	sub	dx, 202h		; minus 2 for height and width (used by cycles)
	mov	w_dth, dl		; save width in stack frame
	xor	cx, cx			; initialize counter to zero
	mov	di, ax			; load offset for first string
	mov	ah, attrib		; load attribute
	; Draw Top Frame
	mov	al, pg_GG		; Russian "G" angle
	stosw		
	mov	al, pg_DH
	mov	cl, w_dth
	rep	stosw	; Draw a line of needed width
	mov	al, pg_iGG		; inverse "G" angle
	stosw
; Move to another line	
@DrawF_NextLine:
	mov	di, VidOffset
	add	di, cols_count*2
	mov	VidOffset, di
	mov	al, pg_II
	stosw		; Type it along Left frame
	mov	cl, w_dth
	mov	al, 20h
	rep	stosw
	mov	al, pg_II
	stosw		; Type it along Right frame
; Shadow looks cool
	mov	al, ShadowAttrib
	inc	di
	stosb
	inc	di
	stosb
	dec	dh
	jg	@DrawF_NextLine
	
; Draw bottom line here
	mov	di, VidOffset
	add	di, cols_count*2
	mov	VidOffset, di

	mov	cl, w_dth
	mov	al, pg_LL	; Double L-angle
	stosw		
	mov	al, pg_DH	; double horizontal
	rep	stosw	; Draw a line of needed width
	mov	al, pg_JJ
	stosw		
; Shadow looks cool
	mov	al, ShadowAttrib	
	inc	di
	stosb
	inc	di
	stosb
; Last 'Pure Shadow' Line
	mov	di, VidOffset
	add	di, cols_count*2 + 4; 1 line down, 2 chars right
	movzx	cx, byte ptr w_dth	; w_dth doesn't include frame
	add	cx, 2
	mov	ah, ShadowAttrib	
	call	SetAttrib_x
	pop	di	; restore di  - maybe it was used before
	mov	sp, bp	; kill stack frame
	pop	bp
	ret
DrawFrame	ENDP


; Procedure	DrawInputField
; Purpose	Draw an NC-styled inputField at (x,y)
; Input		(dl, dh) - coordinates (x,y)
;		cx	- length of field
;		al	- flags	00 - no parenthesis, anything else = square par.
; Output	Drawn to video-page (segment at es)
DrawInputField	PROC
	push	dx
	xchg	ax, dx
	call	GetVideoOffset
	cmp	dl, 0
	jz	@DrawIB_Np1
	sub	ax, 2
	mov	di, ax
	mov	ax, at_InputField*256 + '['
	stosb
	inc	di
	mov	al, InputDefTypeBack
	rep	stosw
	cmp	dl, 0
	jz	@DrawIB_Np2
	mov	al, ']'
	stosb	
	jmp	@DrawIB_Np2
	
@DrawIB_Np1:	
	mov	di, ax
	mov	al, InputDefTypeBack
@DrawIB_NpLoop:	
	stosb
	inc	di
	loop	@DrawIB_NpLoop	
	
@DrawIB_Np2:
	pop	dx
	push	bx
	@SetCurPos		; clears ax!
	pop	bx	; Macro alters bx
	ret
DrawInputField	ENDP


; Procedure	ReadKeyInput
; Purpose	Query string from user: display a frame and 'textbox'
; Input		ds:si	- offset where to place read string
;		cx	- maximum length for entered string
;		ax	- input mode: -1= all chars; 10-decimal numbers; 10h-heaxadecimal
; Output	ds:si	- stores string
;		ax	- number of bytes read + cancel state
ReadKeyInput	PROC
		push	bp
		mov	bp, sp
		sub	sp, 4
MaxLen		equ	[bp-2]
InputMode	equ	[bp-4]
		mov	MaxLen, cx
		mov	InputMode, ax
		push	si
		push	bx
		xchg	bx, si		; now bx becomes base, si - index
		xor	si, si		; will count symbols
@ReadKI_Main:
		mov	ah, 08h
		int	21h		; read from KB
		test	al,al		; extended?
		jz	@ReadKI_Ext
		cmp	al, key_ESC	; ESCAPE
		jnz	@ReadKI_1	; If not, keep analyzing
		xor	si, si		; Still here?... ok, cx = 0 - nothing entered
		xor	ax, ax		; 
		dec	ax		; ax = -1 (input cancelled by user)
		jmp	@ReadKI_Done	; We got a result, must allowed
@ReadKI_1:	
		cmp	al, key_BS	; Check for BackSpace
		jnz	@ReadKI_2
		test	si, si		; if nothing has been entered - do nothing
		jz	@ReadKI_Main
		dec	si		; minus one symbol
		@MoveCursorLeft	; Move cursor (that's for user's eyes only)
		push	bx
		mov	al, InputDefTypeBack
		call	PrintAtCursor
		pop	bx
		jmp	@ReadKI_Main	; Back to kb queries
@ReadKI_2:
		cmp	al, key_ENTER	; Check for Enter
		jnz	@ReadKI_3
		test	si, si		; if nothing has been entered - do nothing
		jz	@ReadKI_Empty
		mov	ax, si		; count bytes and normal exit
		jmp	@ReadKI_Done
@ReadKI_3:
		cmp	word ptr InputMode, 10
		jz	@ReadKI_Try10
		cmp	word ptr InputMode, 16
		jz	@ReadKI_Try16
		; Here we stay for all other cases (all common symbols accepted)
		cmp	al, ' '
		jbe	@ReadKI_Main	; All special symbols already handled
		cmp	al, '~'
		ja	@ReadKI_Main	; Russian characters unsuported!
		jmp	@ReadKI_SymbolOk
@ReadKI_Try16:
		cmp	al, '0'
		jb	@ReadKI_Main
		cmp	al, '9'
		jbe	@ReadKI_SymbolOk
		and	al, 11011111b	; all chars driven to Upper-case
		cmp	al, 'A'
		jb	@ReadKI_Main
		cmp	al, 'F'
		ja	@ReadKI_Main
		jmp	@ReadKI_SymbolOk
@ReadKI_Try10:		
		cmp	al, '0'
		jb	@ReadKI_Main
		cmp	al, '9'
		ja	@ReadKI_Main
;		jmp	@ReadKI_SymbolOk
	
@ReadKI_SymbolOk:		
		cmp	si, MaxLen
		jae	@ReadKI_Main
		mov	byte ptr [si+bx], al
		push	bx
		call	TypeAtCursor
		pop	bx
		inc	si			; Advance per 1 symbol
		jmp	@ReadKI_Main
		; compare more
@ReadKI_Ext:
		int	21h		; call over to get scan code
@ReadKI_Ext1:
		; Arrow-Keys handling may be added here
		jmp	@ReadKI_Main

@ReadKI_Empty:
		xor	si, si		; Still here?... ok, bx = 0 - nothing entered
		xor	ax, ax		; 
@ReadKI_Done:
		mov	byte ptr [si+bx], 0	; terminate string with '/0'
		pop	bx
		pop	si
		mov	sp, bp
		pop	bp
		ret
ReadKeyInput	ENDP


; Procedure	DisplayHintBar
; Purpose	Draw Hint-Bar, which explains function-keys bindings
; Input		ds:si	- pointer to array of 6-byte ASCIIZ strings
;		ds	- segment to work with
; Output	You'll see it on display or in video page, currently adressed by es

DisplayHintBar	PROC			; tested ok
	mov	ax, 1800h	; (24, 0)
	call	GetVideoOffset
	mov	di, ax
	mov	cx, 9
	mov	ah, at_DownNum	; Read hints attribute for numbers
@DispHB_Loop:
	mov	al, cl		; get current loop pass
	neg	al		; numbers go in increment order, loops decrease
	add	al, 10+'0'	; fix that and convert to decimal number
	stosw			; out to video
	mov	ah, at_DownHint ; Attribute for hint itself
	call	OutStringAt_x
	mov	ax, at_DownNum*256 + ' '	; space with attribute 
	stosw
	loop	@DispHB_Loop
; Prepare to write the last Hint
	mov	al,'1'		; Load '1'
	stosw			; Write '1'
	dec	ax		; Make it zero
	stosw			; Write '0'
	mov	ah, at_DownHint ; Attribute for hint itself
	call	OutStringAt_x
	ret
DisplayHintBar	ENDP


; Procedure	MessageBoxEx
; Input		ds:si	- string to be printed (tabs, crlf are allowed) terminated by '/0'
;		ah	- color for message
;		al	- type of MessageBox
;		es	- segment for videopage drawn to.
; Output	User sees dialog box and makes his choice (if variants are possible)
;		ax	- result as defined in... user32.h (!!!)
; Comment	Growing project requieres more complex messages and interface
;		This proc is in fact emulating windows MessageBox function
MessageBoxEx	PROC
; Alghoritm
; 1. Determine frame size call to GetTextExtent
; 2. 

	ret
MessageBoxEx	ENDP



GetTextExtent	Proc
;	================ C code beings =====================:
;	RECT FrameSize (char* text)
;	(stack) char* text_cpy;
;	register line_chars = 0;
;	register word_chars = 0;
;
;	RECT frame = {0,0};
;	while(*text) {
;		switch	(*text) {
;			word_chars = 0;
;		case 0xA:
;			text++;		// ignore symbol
;			break;
;		case 0xD:
;			frame.y++;
;			frame.x = (frame.x<line_chars)? frame.x : line_chars ;
;			line_chars = 0;
;		case 0x20:
;			word_chars = -1;
;		default:
;			*text_cpy++ = *text++;
;			word_chars++;
;		}
;		if (line_chars>=60) 
;		{
;			if (word_chars =! line_chars) 
;			{
;				text =- word_chars;
;				text_cpy =- word_chars;
;			} 
;			*text_cpy++ = 0xD;
;			frame.y++;
;			frame.x = (frame.x<line_chars)? frame.x : line_chars ;
;			line_chars = 0;
;			word_chars = -1;
;		}
;	return rc;
;	}
;	rc.y =+ (sButtons_Drawn in al) ? 3 : 0;
;	================== C code Ends ====================:	
	
	ret

GetTextExtent endp



; Procedure	ErrorBox
; Input		ds:si - message
;		carry flag: set - error, clear - message
; Output	A red box with a message, button "ok" below it
; Comments	Used frequently, not a good idea to insert macro each time
ErrorBox	PROC
	mov	cx, ErrorDefPos
	mov	dx, ErrorDefSize
	jnc	@ErrorB_Message
	push	offset sErrorWndTitle
	mov	ah, at_ErrorBox
	jmp	@ErrorB_Frame
@ErrorB_Message:
	mov	ah, at_DialogBox
	push	offset sMessageWndTitle

@ErrorB_Frame:
	push	ax
	call	DrawFrame
	pop	ax

	; mov	si, message
	mov	dx, ErrorDefPromptPos
	call	OutStringCntrAt

	rol	ah, 4
	mov	dx, ErrorDefPos
	pop	si
	call	OutStringCntrAt

	mov	dx, ((ErrorDefPos+500h) and 0ff00h ) or 36
	mov	cx, 8
	lea	si,  sOk
	Call	DrawButton

	lea	si, sb_Buffer0
	mov	cx, 0
	call	ReadKeyInput

	ret
ErrorBox	endp

; Procedure	DrawButton
; Purpose	Draw a rectangluar button of given width with given text
; Input		cx - legnth of field
; 		dx - coordinates for button
;		si  - text
DrawButton	PROC
	push	cx
	mov	ax, dx
	call	GetVideoOffset
	mov	di, ax
	pop	dx
	call	strlen
	push	cx
	push	dx
	sub	dx, cx	; dx = spaces cnt (at both sides)
	jc	@DrawB_NoSpace	; dx > 0 ?
	mov	ax, cx
	mov	ax, dx	
	shr	ax, 1	; spaces/2
	sub	dx, ax	; spaces after funtion name
	mov	cx, ax	
	mov	ax, at_Button*256 + 32
	push	di
	rep	stosw
	call	OutStringAt_x
	mov	cx, dx
	mov	ax, at_Button*256 + 32
	rep	stosw
	mov	ah, es:[di+1]	; Get attribute for next character
	and	ah, 0f0h		; Set text color into black
	mov	al, 0DCh
	stosw	
	pop	di
	add	di, (cols_count + 1)* 2	; move one line down, 1 chr right
	mov	al, 0DFh
	pop	cx
	rep	stosw
	pop	cx
	ret

@DrawB_NoSpace:
	push	di
	mov	ah, at_Button
	call	OutStringAt_x
	mov	ah, es:[di+1]	; Get attribute for next character
	and	ah, 0f0h		; Set text color into black
	mov	al, 0DCh
	stosw
	pop	di
	add	di, (cols_count + 1)* 2	; move one line down, 1 chr right
	mov	al, 0DFh
	pop	cx
	pop	cx
	rep	stosw
	ret

DrawButton	Endp


; Procedure	ChooseList
; Purpose	Let User choose between several possible options
; Input		cx	- number of operands
;		stack	- offsets to strings 4 extraction and displaying
;		si	- String for dialog title, and items text
; Output	cx	- number of option selected (0 means cancel or error)
ChooseList	PROC	; PASCAL-style procedure with multiple parameters - operands extracted manually
		push	bp
		mov	bp, sp
		sub	sp, 4
ParamsCnt	equ	[bp-2]

	mov	ParamsCnt, cx
; Organize correct stack extraction

	xor	dx, dx			 
	mov	dh, cl
	add	dx, 41Eh
	mov	cx, InputDefY*256 + 25

	mov	ah, at_DialogBox
	call	DrawFrame

	mov	ah, at_DialogBox
	ror	ah, 4
	mov	dh, InputDefY
;	lea	si, sc_ModeSelect
	call	OutStringCntrAt

	mov	cx, ParamsCnt
	
@ChooseL_DrawItems:	
	mov	ah, at_DialogBox
	mov	dh, InputDefY + 2
	add	dh, byte ptr ParamsCnt
	sub	dh, cl		
;	lea 	si, nextString	
	call	OutStringCntr
	loop	@ChooseL_DrawItems

	mov	dx, (InputDefY + 2)*256 + 28
	mov	ax, 24
	mov	cx, ParamsCnt
	call	ChooseListItem
	
	mov	sp, bp
	pop	bp
	ret		; return with stack clear
	
ChooseList	ENDP

; Procedure	ChooseListItem
; Purpose	User must choose one of list items
; Input		(dl, dh)	- input box coordinates
;		ax	- width of line
;		cx	- lines count
; Output	cx 	- number of item selected, zero if cancelled
ChooseListItem	PROC
	push	bp
	mov	bp, sp
	sub	sp, 8
cnt	equ	[bp-6]
w_dth	equ	[bp-4]
sel	equ	[bp-2]
coordz	equ	[bp-8]
	mov	coordz, dx
	mov	w_dth, ax
	mov	cnt, cx
	mov	cx, 1
	jmp	@ChooseLI_LightIt
@ChooseLI_Main:
	mov	ah, 08h
	int	21h		; read from KB
	test	al,al		; extended?
	jz	@ChooseLI_Ext
	cmp	al, key_ESC	; ESCAPE
	jnz	@ChooseLI_1	; If not, keep analyzing
	xor	cx, cx		; cx = 0, user cancelled his choice
	jmp	@ChooseLI_Done
@ChooseLI_1:	
	cmp	al, key_Enter
	jnz	@ChooseLI_Main
	jmp	@ChooseLI_Done
@ChooseLI_Ext:
	int	21h
	cmp	al, key_Up	; 
	jnz	@ChooseLI_2
	dec	cx
	jnz	$+3
	inc	cx
	jmp	@ChooseLI_LightIt
@ChooseLI_2:
	cmp	al, key_Down	; 
	jnz	@ChooseLI_Main
	inc	cx
	cmp	cx, cnt
	jbe	$+3
	dec	cx
;	jmp	@ChooseLI_LightIt
@ChooseLI_LightIt:
	mov	sel, cx			; store currently active item
	mov	cx, cnt			; load item quantity for cycle 
	mov	ax, Coordz		; Read coordinates
	add	ah, cl
	dec	ah
	call	GetVideoOffset	; Calculate offset
	mov	di, ax			
	push	di			; offset on to stack
	
@ChooseLI_LightLoop:
	cmp	cx, sel			; set attributes for selected and "ordinary" lines
	mov	al, at_DialogBox
	jnz	@ChooseLI_Simple
	mov	al, at_Selected		
@ChooseLI_Simple:

	push	cx			; Save line being processed counter
	mov	cx, w_dth		; set width of hilight field
	inc	di
	stosb				
	loop	$-2
	pop	cx			; Restore line number

	pop	di			; Load
	sub	di, cols_count *2	; Fix Video RAM offset
	push	di			; Store offset
	
	loop	@ChooseLI_LightLoop	; Process all lines

	pop	di			; remove offset from stack
	mov	cx, sel			; Restore number of selected item
	
	jmp	@ChooseLI_Main
@ChooseLI_Done:
	mov	sp, bp
	pop	bp
	ret
ChooseListItem	ENDP

end