; Project "DC 2002"
; Disk Operations, Library for Main and View modules
; (c) 2002 Trance_C[RootTeam]

.model 	small

FileBuffer	struc		; 8-bytes structure to describe a buffer
FileOffset	dd	?	; Offset in file, matching first charcter of buffer
SegOffset	dw	?
Filled	dw	0	; If zero - buffer unused, else its length in bytes read from disk, not just casual data
FileBuffer	ends

FileStruct	struc		; No members created, but used for offset calculation for fields below
Handle	dw	0
Extra		dw	0
FileSize	dd	0
;FileName	db	120 dup(0)	; Unused in current realization
FileStruct	ends

.data
Filename24	db	26 dup (0)	; string for filename to be typed at topmost status-line in view-mode

PUBLIC	RFileSize, Filename24

RBufferSz	equ	4096		; 4 KBytes for disk buffer would be enough
; Theese fields in fact contain 'File' object
RHandle	dw	0
RExtra		dw	0
RFileSize	dd	0
;RFilename	db	120 dup (0)		; seems to be unused
RBufferData	db	RBufferSz dup (0)	; Offset from record beginning is 128!
RBuffer		FileBuffer	<0,0,0>

; Yet another variant is Allocate Memory in Runtime
		
WBufferCnt	equ	4
WBufferSz	equ	512

Public 		WHandle

WHandle	dw	1	; Handle for saving data
;WExtra		dw	0	; Theese fields are not used 
;WFileSize	dd	0
;WFilename	db	120 dup (0)
;WBufferData	db	WBufferSz*WBufferCnt dup (0)	; Offset from record beginning is 128!
;WBuffer	FileBuffer	WBufferCnt dup(<0,0,0>)
				

.code
.386

include 	base.inc
; Export declarations
PUBLIC	ExistFile, OpenFile, GetDiskBuffer, IsRFileOpen, CloseFile, InitRBuffer

IsRFileOpen:
	mov	ax, RHandle
	test	ax, ax
	ret

InitRBuffer:
	lea	ax, RBufferData	; offset for buffers
	lea	bx, RBuffer	; 1st buffer descriptor offset
	mov	word ptr [bx].SegOffset, ax	; set a right offset
	ret


; Procedure	MakeName24
; Purpose	Convert filename into a string displayed at topmost status-line
; Input		si	- filename with path
; Output	memory: FileName24 contains a desired string
MakeName24	proc
	push	si
	push	di
	lea	di, FileName24
	call	strlen
	cmp	cx, 24
	jbe	@MakeN24_ShortName
	push	cx
	mov	cx, 9
	rep	movsb	; copy first 9 chars 
	mov	eax, '...'
	stosd		; Add 3 points
	dec	di
	pop	cx
	sub	cx, 9+12
	add	si, cx
	mov	cx, 12
	rep	movsb	; copy last  12 chars 
	xor	ax,ax
	stosb		; ending zero
	jmp	@MakeN24_Done
@MakeN24_ShortName:
	call	strcpy_x	; Copy file as is if it fits in 24 chars
@MakeN24_Done:
	pop	di
	pop	si
	ret

MakeName24 endp

; Procedure	ExistFile
; Purpose	Return state: whether file exists or not
; Input		dx	- offset for filename in ds
; Output	ax	= 1 if exist, 0 if file not found
ExistFile	Proc
	mov	ax, 3D00h		; Open Existing File function for reading only, "compatibility sharing"
	int	21h
	jnc	@ExistF_Found	; jump next if all o'kay
	Xor	ax, ax			; if error, ax = 0
	ret
@ExistF_Found:
	mov	bx, ax			; Pass handle 
	mov	ax, 3e00h		; Close file
	int	21h			
	xor	ax, ax			
	inc	ax			; set ax = 1
	ret
ExistFile endp


; Procedure	OpenFile
; Purpose	Get Information about file, store its hanlde, size, 
;		Fill buffers with first n Kbytes
; Input		ds:dx		- File Name to open
;		carry flag	- Open Mode: "clear" for read, "set" for write
; Output	dw_FileSize	- Size of opened file
;		RBuffer	- (Mem) Buffers containing first k bytes of file
;		RBufferData	- Describe information here

OpenFile	PROC		
		jc	@OpenF_Force_Write	; 1 = write anyway (don't care if file exists and so...)
@OpenF_Read:
		push	si
		mov	si, dx
		mov	ax, 3D00h		; Open Existing File funtion for reading only, "compatibility sharing"
		int	21h
		jc	@OpenF_NotOpened	; Could we open it ?	
		push	ax		; Save handle for further purposes
		mov	bx, ax
		mov	ax, 4202h		; Move file pointer to the end
		xor	cx, cx			; 0 bytes
		xor	dx, dx			; 0 bytes from end
		int	21h
		jc	@OpenF_GetSzFail
		shl	edx, 10h	
		mov	dx, ax
		mov	RFileSize, edx
@OpenF_GetSzOk:
		clc			; closing r-file...
		call	CloseFile	; Close Prevoiusly open file (if any)
		pop	RHandle
		lea	bx, RHandle
		xor	edx, edx
		mov	cx , RBufferSz		
		Call	ReadBlock
		Call	MakeName24
		pop	si
		jmp	@OpenF_Ok

@OpenF_Force_Write:			; Here we come after overwrite confirmed or file didn't exist		
		mov	ax, 3C10h		; Create New file with archive attribute
		int	21h	
		jc	@OpenF_NotOpened	; Could we open it ?	
		mov	WHandle, ax		; Save handle
@OpenF_Ok:		
		xor	ax, ax			; Return 0 - this is normal
		jmp	@OpenF_ret
@OpenF_NotOpened:		
		mov	ax, -1			; Means function failed opening file 
		jmp	@OpenF_ret		; most common reason is file_not_found
@OpenF_GetSzFail:
		pop	ax	; must leave with good stack
		mov	ax, -2			; Means function failed @ GetFileSize
						; never happened to me, but still reserved
@OpenF_ret:
		ret
OpenFile	ENDP

; Procedure	ReadBlock
; Purpose	Serve request and dispaly a portin of file into read buffer
; Input		edx	- requested offset (block is RBufferCnt * RBufferSz	bytes large)
; Output	ReadBuffers filled with data
;		edx	- buffer start
;		cx 	- length
ReadBlock	PROC
	
		and	dx, 0fe00h	; 512-bytes align
		cmp	edx, RFileSize
		jbe	Offset_inFile	; keep coordinates, if still inside file
		xor	edx, edx	; fix, if rollback lead before file beginning
Offset_inFile:
	; NOTE: Requested coordinates aren't checked to be below end-of-file, caller should care of that
		mov	bx, RHandle
		mov	ax, 4200h	; Move file pointer from the beginning to cx:dx
		push	edx		; Save offset to request
		push	edx		; Save offset to request
		pop	dx
		pop	cx
;		xchg	cx, dx		; send low-word into cx
;		shr	edx, 16		; get high-word into dx
;		xchg	cx, dx		; set cx=highword, dx = lowword
		int	21h
		; no error checking here - function shouldn't fail, once succeeded.

		mov	cx, RBufferSz		; Request data to fully fill thy buffer
		lea	bx, RBuffer
		mov	dx, [bx].SegOffset	; Load buffers' offset 
		mov	bx, RHandle
		mov	ax, 3F00h		; Read from file
		int	21h			; Call to DOS (ax = bytes received)
		lea	bx, RBuffer		; Set bx to first (and last) descriptor (there were 8 of them before)
		mov	[bx].Filled, ax
		xchg	cx, ax
		pop	edx
		mov	dword ptr [bx].FileOffset, edx

@ReadB_Done:
		ret
ReadBlock	ENDP



InvalidateRBuffer:	; may become obsolete - test it!
; Invalidate buffers (if not final close)
	lea	bx, RBuffer		; load descriptors' array offset
	mov	word ptr [bx].Filled, 0	; Declare buffer empty
	xor	eax, eax		
	mov	RFileSize, eax		; Set filesize to zero
	ret		

; Procedure	CloseFile
; Purpose	Display currently open file from given offset
; Input		carry flag	- 0 for R-file, 1 for W-file
; Output	None
CloseFile	PROC
		jnc	@CloseF_Read
		mov	bx, WHandle	; Load file handle
		cmp	bx, 2			; check if it is a file, not std(in/out/err)
		jbe	@CloseF_WasNotOpen
		mov	bx, WHandle
		jmp	@CloseF_DosCall
		
@CloseF_Read:
		mov	bx, RHandle	; Load file handle
		test	bx, bx			; check if it is a file, not a stdin
		jz	@CloseF_WasNotOpen
		Call	InvalidateRBuffer
		mov	bx, RHandle
		jmp	@CloseF_DosCall
@CloseF_DosCall:
		mov	ax, 3e00h	; Close file
		int	21h
@CloseF_WasNotOpen:
		ret		; when done, begone
CloseFile	ENDP


; Procedure	GetDiskBuffer
; Purpose	Provide procedures with fresh data from file
; Input		edx		- (Reg) Offset
;		ecx		- (Reg) Size of fragment desired
; Output	ds:si pointing to a tasty file cache
GetDiskBuffer	PROC
;	1. Look in cache and share what there is.
;	2. Read from Disk, update cache, share that part.
		push	bp
		mov	bp, sp
		sub	sp, 8h
FromOffset	equ	[bp-4]
ToOffset	equ	[bp-8]
	; Stack frame for locals created
		lea	bx, RBuffer
		mov	FromOffset, edx
		
	; Check if file bounds are violated
		test	edx, edx		; edx < 0 ? (well, it cannot be unsigned long, 'cause 2Gb files are quite rare)
		jl	@GetDB_InvRequest	; Cannot read before file beginning (bogus request? ;)
		cmp	edx, RFileSize		; This time user wishes to read something behind end-of-file
		jae	@GetDB_InvRequest	; quite stupid, but it is better to handle, than to leave that as is.

	; Check if requested data would fit in buffer
		cmp	ecx, RBufferSz*3/4	; If asked for more than 3/4 of buffer,
		ja	@GetDB_InvRequest	; it will be impossible to serve that request
		
	; Now, we are inside file for sure. So all requests will be served
	; Now need to decide if existing cache is good for this request
		sub	edx, [bx].FileOffset	; got in edx offset from buffer start
		jl	@GetDB_CacheMiss	; If need something before cache, it's a miss for sure
		cmp	edx, RBufferSz		; Requested position is behind cached data (but still inside file)
		ja	@GetDB_CacheMiss	; missed another time
		
		push	dx			; May use dx, we know that 0 <= edx < 4K
		add	dx, cx			; dx = offset of ending point rel. cache 1st bytes
		cmp	dx, RBufferSz		; if ending point outside buffer, 
		pop	dx
		jae	@GetDB_LowCache	; we should do something...
		
	; Here we stay if cache is good and contains all needed information
	@GetDB_UseExisting:
		mov	si, [bx].SegOffset	; Load cache beginning
		add	si, dx			; add index to get memory offset for data in file
		mov	cx, [bx].Filled
		sub	cx, dx
		jmp	@GetDB_KillFrame

	; This is where we get if running towards cache end
	@GetDB_LowCache:
		; Two situations possible:
		; 1. Need to read next block of data
		; 2. Buffer underrun due to end-of-file
		mov	edx, FromOffset
		add	edx, ecx
		cmp	edx, RFileSize
		jle	@GetDB_CacheMiss	; This is 1st case

		; Here we stay if eof is almost reached
		mov	edx, RFileSize		
		sub	edx, FromOffset	; edx = cache piece from request till eof
		mov	ecx, edx		; store it for function-out
		mov	edx, FromOffset	; Get requested-Offset 
		sub	edx, [bx].FileOffset	; calculate offset in cache
		jmp	@GetDB_UseExisting	; jump to cache give-away
				
	@GetDB_CacheMiss:
		mov	edx, FromOffset
		call	ReadBlock
		; No return code here? 
		lea	bx, RBuffer
		mov	edx, FromOffset
		sub	edx, [bx].FileOffset	; calculate offset in cache
		; edx = offset; ecx - length
		jmp	@GetDB_UseExisting
		
@GetDB_InvRequest:	; means we got an error or end of file - anyway function failed
		xor	ecx, ecx
		xor	si,si
;		jmp	short	@GetB_KillFrame
@GetDB_KillFrame:
		mov	sp, bp
		pop	bp
		ret
GetDiskBuffer	ENDP



end
