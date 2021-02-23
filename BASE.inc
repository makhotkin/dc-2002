; Project DC 2002
; Include file for library base.inc


EXTRN	strcat:PROC, strcat_x:PROC, strcpy:PROC, strcpy_x:PROC, strlen:PROC, strnext:PROC, StrPos:PROC
	; String Copy/Append/GetLen
EXTRN	Int2Hex8:PROC, Int2Hex16:PROC, Int2Hex32:PROC				; Hex number displaying
EXTRN	Int2Hex8f:PROC, Int2Hex16f:PROC, Int2Hex32f:PROC		; Hex number display with leading zero 
EXTRN	OutStringAt:PROC, OutStringAt_x:PROC, OutString:PROC, OutString_x:PROC	; Display strings with attribute/positioning
EXTRN	OutCharsAt_x:PROC, OutChars_x:PROC, sFillUp:PROC			; Display character chains
EXTRN	FormatOutAt_x:PROC, FormatOutAt:PROC					; Formatted output
EXTRN	OutStringCntr:PROC, OutStringCntrAt:PROC					; Display string in center of given line
EXTRN	FillLine:PROC, Tabulate:PROC, FillUpAt:PROC, Fill2Column:PROC		; Tabulation, line and screen filling
EXTRN	FillLineAt:PROC, TabulateAt:PROC, Fill2ColumnAt:PROC			; Filling, screen clearing
EXTRN	GetVideoOffset:PROC, GetVideoCoords:PROC					; Video pointer positioning
EXTRN	PrintAtCursor:PROC, TypeAtCursor:PROC, HideCursor:PROC, ShowCursor:PROC	; Typing Emulation 
EXTRN	SetAttrib_x:PROC, SetAttrib:PROC						; Set Character attributes
EXTRN	Hex2IntDX:PROC