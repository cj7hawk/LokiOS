; Loki startup routines - COM - Can execute scripts or other activities - As a COM.
;
.EQU	BDOS,	$0005

.equ	ADRIVE,	$00
.equ	LDRIVE,	$0B
.equ	MDRIVE,	$0C

.equ	TRACKPORT	,$06
.equ	SECTORPORT	,$07
.equ	MADPORT		,$0C

.equ	MMU,$0F				; The port to read/write MMU bytes to the MMU RAM.
.equ	PID,$0E				; The PID port - Which process is the MMU reflecting?

.equ 	RETURN,$0050			; Return address. This routine restores the process ID and exits back to Process 0. Might need to change the process return later. 
.equ	RETURN_FROM_INT,$0054	; Return address for return from Interrupt.  A should hole the process ID before calling this. 

; Memory Map.
; Common Lower Page - common to all drivers. 4K in size.
; $0000 - Common memory, entry points in the Zero Page. Existing applications from $0100 to $0FFF. Can contain temporary data to send to the driver also. 
; $1000 - Driver entry, Code and Bitmaps. Resident.  8K in size. 
; $3000 - Video Map - 8 bits - 2040 bytes * 2 ( 8 bits of attributes ). 5x ADM attributes.  
;

; Drivers begin at $1000 usually ( they don't have to, but it's convenient for the OS and system to follow this convention leaving the lower page common. )

.ORG $1000		;  Into the next page. 
VIDEO_ENTRY:	;  We enter here with a character in B and a code in C that tells what to do. DE may contain coordinates, memory offsets, etc. 
				;  Note: The contents of "A" are lost due to the process by which the jump is made but is restored on exit.
				;  It cannot generally be saved reliably as there is no way to know if the stack was switched out when the call was made. 
		LD		A,C			; Collect C.
		OR		A
		JR		Z,	INITIALISE	; C=0 = initialize. 

		LD		A,B				; Restore A from B as B holds character. 

		LD		(HLSTORE),HL	; Store HL.
		LD		HL,$0000	
		ADD		HL,SP			; Move Stack Pointer to HP. 
		LD		SP,VIDEOSTACK	; Set local stack for routines.
		LD		(STACKSTORE),HL ; And preserve original SP value so we don't have too many entries in it. 

		PUSH	IX				; This routine also uses IX and DE. Save them since the routine probably thinks the BIOS is handling this. 
		PUSH	DE

		CALL	CHAR
		
		POP		DE			; Exit - Let's restore the registers. 
		POP		IX			; AF get's popped on return.....
		
		LD		HL,(STACKSTORE)	; Restore Stack Pointer. 
		LD		SP,HL
		LD		HL,(HLSTORE)	; Restore HL.
		
		XOR		A			; Set the return process ID - May be more complex later. For now, return to process 0. 
		JP		RETURN		; Exit returning things to normal. 

HLSTORE:
		DW		$0000		; Temporary HL Store. 
STACKSTORE:
		DW		$FFFF		; Temporary Stack Store.
		
INITIALISE:					; Let's initialise the screen, Character map, etc. Show something. Maybe set up the video modes too. 
		CALL	CLEARSCREEN	
		CALL	SHOWASCII		; Show some text and the character set. 

		LD		DE,INITMESS1	
		CALL	PSTRING
		
		LD		DE,SMESS
		CALL	PSTRING

		LD		A,22			; Let's start at the bottom. 
		LD		(VPOS),A
		XOR 	A
		LD		(HPOS),A		; Bottom of the screen. Left side. Waiting for input. 

		XOR		A			; Set the return process ID - May be more complex later when I nest drivers. 
		LD		(VCURSOR),A
		LD		(VATTR),A		; Clear the cursor and attribute setting. 


		JP		RETURN		; Exit via the return to set Process 0 again. ( Later to return to previous process. ) 
		
		
SHOWASCII:

		LD		A,$01
		LD		(VPOS),A
		XOR		A
		LD		(HPOS),A

TLOOP:							; TLOOP just prints the character set to the screen..... 
		PUSH	AF
		CALL	TEXT		
		LD		HL,HPOS
		INC		(HL)
		POP		AF
		INC		A
		CP		$40
		JR		NZ,TLOOP


		LD		A,$02
		LD		(VPOS),A
		XOR		A
		LD		(HPOS),A		
		LD		A,$40			; Start character. 
TLOOP2:
		PUSH	AF
		CALL	TEXT		
		LD		HL,HPOS
		INC		(HL)
		POP		AF
		INC		A
		CP		$80
		JR		NZ,TLOOP2
		


		LD		A,$03
		LD		(VPOS),A
		XOR		A
		LD		(HPOS),A		
		LD		A,$80
TLOOP3:
		PUSH	AF
		CALL	TEXT		
		LD		HL,HPOS
		INC		(HL)
		POP		AF
		INC		A
		CP		$C0
		JR		NZ,TLOOP3


		LD		A,$04
		LD		(VPOS),A
		XOR		A
		LD		(HPOS),A
		LD		A,$C0
TLOOP4:
		PUSH	AF
		CALL	TEXT		
		LD		HL,HPOS
		INC		(HL)
		POP		AF
		INC		A
		JR		NZ,TLOOP4
		
		ret




SMESS:	DB	22,60,'512x192 Mode Set$'
INITMESS1:		DB	5,0,'Installing bitmap video driver resident software.$'

PSTRING:									; Print a string pointed to by DE - $ terminates.
			LD	A,(DE)
			LD	L,A
			INC	DE
			LD	A,(DE)
			LD	H,A
			INC	DE
			LD	A,(DE)
PSLOOP:

			PUSH	DE
			PUSH	HL
			CALL	PCHAR
			POP		HL
			POP		DE
			INC		H
			INC		DE
			LD		A,(DE)
			CP		'$'
			RET		Z
			JR		PSLOOP					; Print all characters.
			

;VPOS:	DB	$00								; 24 lines.0to23 5 bits bax. 
;HPOS: 	DB	$00								; Horizontal Position. 0-84, then mult by 3 - eg, 0,1,2,3 becomes 0,3,6,9,12 byte pos - 7 bits max to 8 bits full.  

ESCAPE:		DB		$00						; If z, not in escape. Goes up for each character in the sequence.
INCHAR:		DB		$00						; Temporarily store the incoming character.
INCHAR1:	DB 		$00						; Extra Escape Characters - We might get called multiple times.
INCHAR2:	DB		$00

CHAR:		; Print the character in A, at the current cursor position and move cursor to the right
			; Install ADM-3A decoding here.

			PUSH	AF						; Store character, and work out if we should turn the cursor off. 
			LD		A,(VCURSOR)
			CP		$00
			JR		Z,ALREADYOFF
			CALL	CURSOROFF
ALREADYOFF:	
			POP		AF	
			
			LD		HL,(INCHAR)				; Pick up any previous input characters.			PUSH	AF
			LD		(INCHAR1),HL			; And shift them along 1 byte. (three byte escape character buffer - doesn't store the escape code itself )
			LD		(INCHAR),A
			LD		A,(ESCAPE)
			OR		A
			JR		Z,NOESCAPE
			CALL	INESCAPE				; Call the escape sequebce
			CALL	CURSORON
			ret

NOESCAPE:									; We are not in an escape sequence, so process the character normally.
			LD		A,(INCHAR)
			CP		32
			JP		NC,	NOCONTROL
			CALL	CONTROLCODE				; If we see a control code, process it. Don't print it. 
			CALL	CURSORON
			ret

NOCONTROL:									; It's not a control code, so print it. 
			CALL	TEXT
			LD		A,(HPOS)
			INC		A
			LD		(HPOS),A
			CP		84						; 85 characters from 0 to 84
			JR		C,CURSORON				; Not at EOL yet? Turn the cursor back on now and exit via that routine. 

			XOR		A						; Perform CR-LF combination. 
			LD		(HPOS),A
			CALL	LINE_FEED				; THen fall through to CURSORON to turn the cursor on.
			
CURSORON:									; Turn the cursor on.
											; Cursor should be a simple character printed in "XOR" over the current character.
											; Any character can be the cursor. 
											; A hardware cursor in the video card could be anything - eg, Mouse cursor, Text cursor. Give this some thought.
		LD		A,$01
		LD		(VATTR),A					; Set "Over" mode.
		LD		A,$BF						; Solid Block.
		CALL	TEXT						; Draw it.
		LD		A,$FF
		LD		(VCURSOR),A					; Note that it's on. 
		XOR		A
		LD		(VATTR),A
		ret
		
CURSOROFF:									; Turn the cursor off.
		LD		A,$01
		LD		(VATTR),A					; Set "Over" mode.
		LD		A,$BF						; Solid Block.
		CALL	TEXT						; Draw it.
		XOR		A
		LD		(VCURSOR),A					; Note that the cursor is off. 
		LD		(VATTR),A					; And turn off the XOR attribute. 
		ret

CONTROLCODE:
		; Assume it's return or newline for the moment.
		; First check if there's an escape routine going and collect all the set. 
		; Otherwise process single byte control codes.
			CP	$08					; Cursor left
			JP	Z,BACKSPACE
			CP	$0A					; Cursor Down
			JP	Z,LINE_FEED
			CP	$0B					; Cursor Up
			JP	Z,CURSOR_UP
			CP	$0C					; Cursor Right
			JP	Z,CURSOR_RIGHT
			CP	$0D
			JP	Z,CARRIAGE_RETURN
			CP	$1A					; ctrl-z - Clear screen.
			JP	Z,CLEAR_HOME_POSITION
			CP	$1B					; Escape
			JP	Z,SET_ESCAPE
			CP	$1E					; Home
			JP	Z,HOME_POSITION
			ret

SET_ESCAPE:
		LD		A,(ESCAPE)
		INC		A
		LD		(ESCAPE),A
		ret
		

INESCAPE:
		LD		A,(INCHAR2)		; Check this first. Especially since ESC = = = is a valid position. And we're looking for filled characters... 
		CP		'='
		JR		Z,SETCURSOR		; If we got = then it's a four byte sequence. 
		
		LD		A,(ESCAPE)
		INC		A
		LD		(ESCAPE),A
		CP		4				; no more than 3 characters after the escape, or we assume it's a bad sequence and kill it.
		RET		C				; But otherwise we can exit here because there's less than 3. Use NC because we don't want MORE than 3 eg, 2-3=carry, 3-3=no carry. 

ESCAPE_OUT:
		XOR		A
		LD		(ESCAPE),A		; Turn escape off. 
		ret


SETCURSOR:
		LD		A,(INCHAR1)
		AND		$1F			; Mask to 5 bits...
		LD		(VPOS),A
		LD		A,(INCHAR)		
		SUB		$20				; Remove Space.
		LD		(HPOS),A
		
		XOR		A				; And erase the characters so we don't match them again. 
		LD		(INCHAR),A
		LD		(INCHAR1),A
		LD		(INCHAR2),A
		JP		ESCAPE_OUT
	
	
BACKSPACE:
			LD		A,(HPOS)
			DEC		A
			LD		(HPOS),A
			CP		$FF
			RET		NZ
			XOR		A
			LD		(HPOS),A				; Block further movement initially. We might be at the top left. Scroll backwards????
			LD		A,(VPOS)
			OR		A
			RET		Z						; Return if we're already on the top line (0)
			DEC		A
			LD		(VPOS),A				; Otherwise up a line.
			LD		A,84
			LD		(HPOS),A				; And set the horizontal cursor to the far right of the screen... Consider 80 columns +2 either side in future.
			ret
			
LINE_FEED:
			LD		A,(VPOS)
			INC		A
			LD		(VPOS),A
			CP		24						; Line 24? ( we go from 0 to 23 )
			RET		C						; If carry, were not at the bottom line yet. 
			LD		A,23					; Will be 23 or over.
			LD		(VPOS),A				; Make sure it's never 24.
			CALL	SCROLL					; Scroll Up. 
			ret			

CURSOR_UP:	LD		A,(VPOS)
			OR		A
			RET		Z						; Can't go higher than line 0.
			DEC		A
			LD		(VPOS),A
			ret

CURSOR_RIGHT:
			LD		A,(HPOS)
			INC		A
			LD		(HPOS),A
			CP		85
			RET		C						; Move to the right. If we aren't past EOL, then exit.
			XOR		A
			LD		(HPOS),A				; Otherwise wrap around to the left, and linefeed. 
			JP		LINE_FEED				; If we went past the bottom, scroll.
			

CARRIAGE_RETURN:
			XOR		A
			LD		(HPOS),A
			ret
			
CLEAR_HOME_POSITION:										; Clear the screen. 
			CALL	CLEARSCREEN		
HOME_POSITION:
			XOR		A
			LD		(VPOS),A
			LD		(HPOS),A
			ret
			


VIDPAGE:	DB		$C0						; A page in the video ram. 

SCROLL:
			LD		A,$C0					; Start page of video ram.
			LD		(VIDPAGE),A				; Store start. 
			LD		B,$0B					; 12 double-runs (2 lines per run) and we finish with a half run for the final part since we start with half.

SCROLL1:
			PUSH	BC
			LD		B,$40						; Segment 4.
			LD		C,MMU
			LD		A,(VIDPAGE)
			OUT		(C),A						; Load Video block into segment 4.
			LD		B,$50						; Segment 5.
			INC		A
			OUT 	(C),A	

			LD		HL,$4800					; Middle of segment at 4000 - A line down video. 
			LD		DE,$4000					; Start of segment.
			LD		BC,$1000					; 4K = 2 video lines.
			LDIR

			LD		A,(VIDPAGE)
			INC		A
			LD		(VIDPAGE),A
			POP		BC
			DJNZ	SCROLL1						; And loop 11 times to scroll 22 lines. 

			LD		HL,$5800					; Middle of second 4K. Scroll final line. 
			LD		DE,$5000
			LD		BC,$0800					; Just 2K of line to shift this time. 
			LDIR
			
			LD		BC,$0800
;			LD		A,$00						; Later set to "Paper". 
SCROLL2:	LD		A,$00
			LD		(DE),A
			INC		DE
			DEC		BC
			LD		A,B
			OR		C
			RET		Z
			JP		SCROLL2
;			ret



PCHAR:		;Print a character in C at the current cursor. HL = pos = L= Vertical, H=Horizontal.  Can print non-ASCII printable (eg, 0-1F and 80-FF).
			LD	(VPOS),HL					; Location.
;				LD	A,C
			CALL	TEXT
			ret

		
		
CLEARSCREEN:
		LD		C,MADPORT			; Set the Memory as Disk port in C. 
;		LD		D,$0DF				; From page DF to page $0C0
		LD		D,$0CB				; Only 192 pixels = half page. C0000 CBFFF
		LD		A,D

CLEAR1:
		OUT		(TRACKPORT),A

			LD		E,32
	CLEAR2:
			LD		A,E
			DEC		A
			OUT 	(SECTORPORT),A		; Set the sector. 0 - 31.
			XOR		A



				LD		B,128				; Write the 0 byte then the 127 byte etc. Won't matter for erase.
		CLEAR3:
				OUT		(C),A				; B is carried in A8 to A15. 
				DJNZ	CLEAR3				; Write 256 
			


			DEC		E
			JR		NZ,CLEAR2
		
		DEC		D
		LD		A,D
		CP		$0BF						; $0C0 was the last and we're counting down. 
		JR		NZ,CLEAR1
		
		ret


;;CHAR:	DB	$00								; The current character we are printing. 
VPOS:	DB	$00								; 24 lines.0to23 5 bits bax. 
HPOS: 	DB	$00								; Horizontal Position. 0-84, then mult by 3 - eg, 0,1,2,3 becomes 0,3,6,9,12 byte pos - 7 bits max to 8 bits full.  

VTRACK:	DB	$00								; Track prefix - 8 bits. ( 8 + 5 + 7 = 20 bits of total address )
VSECT:	DB	$00								; The Sector address - 5 bits.
VBADD:	DB	$00								; The "b" address for the upper address lines - 7 bits
VBYTE:	DB	$00								; The byte to write to video memory. 
VCOLOR:	DB	$0F								; Set here - but can and will change later. 
VBACK:	DB	$00								; Background Colour. 
VATTR:	DB	$00								; Video attributes such as Over 1 ( XOR ) or OR or AND functions. 
VCURSOR:DB	$00								; Cursor state - currently off. 

;LINE RLA,RLA,RLA,AND$1F, +C00 and UPPER 8 bits= TRACK and Lower 4 bits *2 = Sector. 
;POS = MUL required. Or table. or DOUBLE + ORIGINAL = 3x.
; POS=10 - Double = 20 Add 10. H=0,L=POS. DE=HL. CCF. RLC L RL H. ADD HL, DE - Value in H to sector. ( rotate in. ) 
;
;C0000
;C0100
;C0200
;C0300
;C0400
;C0500
;C0600
;C0700
;C0800
;
;7+5 POS=0to79
;
;0	0
;3	1
;6	2
;9	3
;12	4
;15	5
;18	6
;21	7
;24	8
;27	9
;30	10
;33
;36
;
;
; Video address format.
; Routine "Text" draws a character using I/O and MAD - Does not require direct access to video display. 
; 11
TEXT:										; Draw a letter. 192 pixels mode. 
											; First locate the starting byte that matches the vertical position we want to print at.
											; Call with a letter to draw in A and LINE and POS set to something.
		LD		D,A								; Preserve the character we want. 
		LD		HL,VSECT						; Set HL to sector address.
		AND		A								; Clear Carry Flag. 
		LD		A,(VPOS)						; First convert line ( ROW - Y axis - 0-23 ) to an address for I/O.
		RRA									; only 2048 bytes/line in 192 pixels mode.
		RR		(HL)							; Move the lower bit of VSECT
		ADD		A,$C0							; Mask and Add for Video Memory.
		LD		(VTRACK),A						; And store the track location.
;;;		OUT		(TRACKPORT),A					; And latch the track in.
		
		XOR		A								; Clear A. 
		RRD		(HL)							; Nibble-shift HL (sector) so the overflow bit 7 goes to bit 3.
			
											; And calculate for the horizontal position now. 
		LD		A,(HPOS)						; Where on the line are we?
		LD		E,A								; Store the value.
		RLCA									; Double A.
		ADD		A,E								; And add it's orignal value = A*3. Now we have an 8 bit number for A.
		LD		(VBADD),A						; Now all addresses are full, except Sector.
		RLA										; Rotate the 8th bit out of A ( sub sector address is only 7 bits. )
		RL		(HL)							; Rotate this bit into A, and finally shift Bit3 to Bit4 for upper sector address from VPOS lower bit. 
												; Now all the bits are set. VTRACK, VSECT and VBADD... 
												; We've finished with HL. 
											; The "track" and "sector" for the current character are now set. 
		
		LD		IX,COL80SET						; Character map after 00. COL80 is without the 00 to 1F characters. 
		LD		A,D								; Retrieve character. 
;		SUB		$20								; reduce offset. ( Space now equals 00 instad of 20 )
		LD		E,A
		LD		D,$00							; DE now holds character number beginning at 00 for SPACE. Later I'll add video codes for sub-$20 and remove this.
		ADD		IX,DE							; IX now holds the position of the character we want. use IX because we want 2 bytes at a time. 6 bytes per character space. 
		ADD		IX,DE							; IX now holds the position of the character we want. use IX because we want 2 bytes at a time.
		ADD		IX,DE							; IX now holds the position of the character we want. use IX because we want 2 bytes at a time.
		ADD		IX,DE							; IX now holds the position of the character we want. use IX because we want 2 bytes at a time.
		ADD		IX,DE							; IX now holds the position of the character we want. use IX because we want 2 bytes at a time.
		ADD		IX,DE							; IX now holds the position of the character we want. use IX because we want 2 bytes at a time.		 6 adds is easier. 

		LD		D,3								; 3 strips to transfer in total. 




TRANS3STRIPS:
		LD		C,MADPORT						; The Memory As Disk I/O port. 
		LD		HL,VBYTE						; The video byte to send to VRAM. 
		LD		E,$8							; 8 bits per vertical character. 8 cycles. 8 bytes per 2-pixel wide strip vertically. 
												; in 512 mode, charcters are drawn two pixels at a time, top to bottom, then the next two pixels. 

	
		LD		A,(VSECT)					; Make sure VSECT vertical bits are zeroed. 
		AND		$11							; Mask out bits 1,2,3,4 - which are character row information Used to be $011 - Need to address this issue. 
		LD		(VSECT),A
		
		
			TRANS8BYTES:
															; Transfer the two pixels. 
					LD		A,(VBACK)						; Default is background colour. 		
					RRC		(IX+0)
					JR		NC,NOLEFT						; Left pixel test.
					LD		A,(VCOLOR)						; Otherwise, load pixel colour.
			NOLEFT:	RLD										; Rotate it into the video byte as a nibble.
					LD		A,(VBACK)						; Default is background colour. 		
					RRC		(IX+1)
					JR		NC,NORIGHT						; Right pixel test.
					LD		A,(VCOLOR)						; Otherwise, load pixel colour.
			NORIGHT:RLD										; Rotate it into the video byte as a nibble.

			STOREBYTE:										; Stores a byte via the MAD interface. 
					LD		A,(VTRACK)						; Set up for the external byte transfer. 
					OUT		(trackport),A
					LD		A,(VSECT)
					OUT		(sectorport),A
					LD		A,(VBADD)
					LD		B,A
					
					LD		A,(VATTR)							; Check any attributes.
					OR		A								; Test it. 
					JR		Z,NOATTR						; And bypass the calculation if there's no attributes.
					
					IN		A,(C)							; retrieve the byte if we're performing a logical operation.
					XOR		(HL)							; XOR with HL.
					JR		ATTR							; And write the attribute. 
					
			NOATTR:	LD		A,(HL)							; HL holds VBYTE. 	
			ATTR:	OUT		(C),A							; Store in Video RAM. 
					DEC		E
					JR		Z,TRANS8DONE						; Send 8 bytes for 8 pixels. Also rotates the video map back into place. Exit here if we're done. 
					LD		A,(VSECT)						
					INC		A
					INC		A
					LD		(VSECT),A	
					JR		TRANS8BYTES


TRANS8DONE:
		INC		IX
		INC		IX								; Next double-byte.
		LD		A,(VBADD)
;		ADD		A,$01							; Add 1 position to VBADD. ( lower 7 bits ) - USE ADD because we need CARRY. 
		INC		A
		AND		$7FH							; Mask - Even though we don't use Bit 7...
		LD		(VBADD),A						; This cycles... So will mess up some calculations. 

		JR		NZ,NOMIDTRANS					; Zero gets set from the AND if we overflowed ( switched screen sides )
		LD		A,(VSECT)
		XOR		$01								; Flip the 1stbit in vsect (Bit0=0 Left side, Bit0=1 right side)
		LD		(VSECT),A						; Will overflow once per line. Somewhere around character 42/43
NOMIDTRANS:		
		DEC		D								; Transfer 3 strips of 8 double-pixels ( 8 bytes ). 
		JR		NZ,TRANS3STRIPS

;		LD		HL,VCOLOR						; Rainbow Effect. Just for testing. 
;		INC		(HL)

		ret
		
		

; LOKI Character Set.\\

COL80SET:	db	%00011100	; NULL - Bullseye
			db	%00100010
			db	%01001001
			db	%01010101
			db	%01010101
			db	%01001001
			

			db	%00011100	; ^A - Copyleft
			db	%00100010
			db	%01000001
			db	%01010101
			db	%01010101
			db	%01001001			
					
			db	%00011100	; ^B - Copyright
			db	%00100010
			db	%01001001
			db	%01010101
			db	%01010101
			db	%01000001

			db	%00100010	; ^C - Half Circle (use with codes 0,1 and 2)
			db	%00011100
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000
			
			db	%00110000	; ^D - Delta
			db	%00101000
			db	%00101100
			db	%00101000
			db	%00110000
			db	%00000000
			
			db	%00000010	; ^E - Degrees
			db	%00000101
			db	%00000010
			db	%00000000
			db	%00000000
			db	%00000000
						
			db	%01011000	; ^F
			db	%01100100
			db	%00000100
			db	%01100100
			db	%01011000
			db	%00000000
			
			db	%00011100	; ^G
			db	%00100010
			db	%01111111
			db	%00100010
			db	%00011100
			db	%00000000
			
			db	%00001000	; ^H
			db	%00011100
			db	%00101010
			db	%00101010
			db	%00011100
			db	%00001000
			
			db	%00011100	; ^I
			db	%00111110
			db	%01111100
			db	%00111110
			db	%00011100
			db	%00000000
			
			db	%00001000	; ^J
			db	%00011100
			db	%00111110
			db	%00011100
			db	%00001000
			db	%00000000
			
			db	%00011000	; ^K
			db	%00011011
			db	%01111111
			db	%00011011
			db	%00011000
			db	%00000000
			
			db	%00111100	; ^L
			db	%00011110
			db	%01111111
			db	%00011110
			db	%00111100
			db	%00000000
						
			db	%01100000	; ^M
			db	%01111110
			db	%00000001
			db	%01111110
			db	%01100000
			db	%00000000
			
			db	%00011000	; ^N
			db	%00100000
			db	%00011000
			db	%00100000
			db	%00011000
			db	%00000000
				
			db	%00001000	; ^O
			db	%00111100
			db	%00000100
			db	%00111000
			db	%00000100
			db	%00000000
				
			db	%01111111	; ^P >
			db	%00111110
			db	%00011100
			db	%00001000
			db	%00000000
			db	%00000000
				
			db	%00001000	; ^Q <
			db	%00011100
			db	%00111110
			db	%01111111
			db	%00000000
			db	%00000000
				
			db	%00000000	; ^R UpDown
			db	%00100010
			db	%01111111
			db	%00100010
			db	%00000000
			db	%00000000
			
			db	%00000000	; ^S !!
			db	%01011110
			db	%00000000
			db	%01011110
			db	%00000000
			db	%00000000
		
			db	%00000100	; ^T Pi-ish
			db	%00001010
			db	%00111110
			db	%00000010
			db	%00111110
			db	%00000000
				
			db	%00001110	; ^U
			db	%01011101
			db	%00111000
			db	%00000000
			db	%00000000
			db	%00000000
				
			db	%00011100	; ^V
			db	%00011100
			db	%00011100
			db	%00011100
			db	%00011100
			db	%00000000
			
			db	%01000000	; ^W
			db	%01000010
			db	%01011111
			db	%01000010
			db	%01000000
			db	%00000000

			db	%00000000	; ^X
			db	%00000010
			db	%00111111
			db	%00000010
			db	%00000000
			db	%00000000
				
			db	%00000000	; ^Y
			db	%00010000
			db	%00111111
			db	%00010000
			db	%00000000
			db	%00000000
				
			db	%00001000	; ^Z
			db	%00001000
			db	%00001000
			db	%00011100
			db	%00001000
			db	%00000000
			
			db	%00001000	; ESC
			db	%00011100
			db	%00001000
			db	%00001000
			db	%00001000
			db	%00000000
				
			db	%01111111	; ^=
			db	%01000000
			db	%01000000
			db	%01000000
			db	%01000000
			db	%01000000
				
			db	%00001000	; ^>
			db	%00011100
			db	%00001000
			db	%00011100
			db	%00001000
			db	%00000000
			
			db	%01100000	; ^?
			db	%01111000
			db	%01111110
			db	%01111000
			db	%01100000
			db	%00000000
								
			db	%00000110	; ^<
			db	%00011110
			db	%01111110
			db	%00011110
			db	%00000110
			db	%00000000

COL80:		db	%00000000	; ' '
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000
			

			db	%00000000	; !
			db	%00000000
			db	%01011111
			db	%00000000
			db	%00000000
			db	%00000000			
					
			db	%00000000	; "
			db	%00000111
			db	%00000000
			db	%00000111
			db	%00000000
			db	%00000000

			db	%00010100	; #
			db	%01111111
			db	%00010100
			db	%01111111
			db	%00010100
			db	%00000000
			
			db	%00100100	; $
			db	%00101010
			db	%01111111
			db	%00101010
			db	%00010010
			db	%00000000
			
			db	%01000011	; %
			db	%00110000
			db	%00001100
			db	%01100011
			db	%00000000
			db	%00000000
						
			db	%00110110	; &
			db	%01001001
			db	%01010110
			db	%00100000
			db	%01000000
			db	%00000000
			
			db	%00000000	; '
			db	%00000100
			db	%00000011
			db	%00000000
			db	%00000000
			db	%00000000
			
			db	%00000000	; (
			db	%00011100
			db	%00100010
			db	%01000001
			db	%00000000
			db	%00000000
			
			db	%00000000	; )
			db	%01000001
			db	%00100010
			db	%00011100
			db	%00000000
			db	%00000000
			
			db	%00100010	; *
			db	%00010100
			db	%01111111
			db	%00010100
			db	%00100010
			db	%00000000
			
			db	%00001000	; +
			db	%00001000
			db	%00111110
			db	%00001000
			db	%00001000
			db	%00000000
			
			db	%00000000	; ,
			db	%01000000
			db	%00110000
			db	%00000000
			db	%00000000
			db	%00000000
						
			db	%00001000	; -
			db	%00001000
			db	%00001000
			db	%00001000
			db	%00001000
			db	%00000000
			
			db	%00000000	; .
			db	%00110000
			db	%00110000
			db	%00000000
			db	%00000000
			db	%00000000
				
			db	%00000000	; /
			db	%01110000
			db	%00011100
			db	%00000111
			db	%00000000
			db	%00000000
				
			db	%00111110	; 0
			db	%01000001
			db	%01001001
			db	%01000001
			db	%00111110
			db	%00000000
				
			db	%01000100	; 1
			db	%01000010
			db	%01111111
			db	%01000000
			db	%01000000
			db	%00000000
				
			db	%01100010	; 2
			db	%01010001
			db	%01001001
			db	%01001001
			db	%01000110
			db	%00000000
			
			db	%00100010	; 3
			db	%01000001
			db	%01001001
			db	%01001001
			db	%00110110
			db	%00000000
		
			db	%00011000	; 4
			db	%00010100
			db	%00010010
			db	%01111111
			db	%00010000
			db	%00000000
				
			db	%00101111	; 5
			db	%01000101
			db	%01000101
			db	%01000101
			db	%00111001
			db	%00000000
				
			db	%00111000	; 6
			db	%01001100
			db	%01001010
			db	%01001001
			db	%00110001
			db	%00000000
			
			db	%00000001	; 7
			db	%00000001
			db	%01111001
			db	%00000101
			db	%00000011
			db	%00000000

			db	%00110110	; 8
			db	%01001001
			db	%01001001
			db	%01001001
			db	%00110110
			db	%00000000
				
			db	%00000110	; 9
			db	%00001001
			db	%01101001
			db	%00011001
			db	%00000110
			db	%00000000
				
			db	%00000000	; :
			db	%00000000
			db	%00110110
			db	%00000000
			db	%00000000
			db	%00000000
			
			db	%00000000	; ;
			db	%01000000
			db	%00110110
			db	%00000000
			db	%00000000
			db	%00000000
		
			db	%00001000	; <
			db	%00010100
			db	%00010100
			db	%00100010
			db	%00100010
			db	%00000000
				
			db	%00000000	; =
			db	%00010100
			db	%00010100
			db	%00010100
			db	%00010100
			db	%00000000
				
			db	%00100010	; >
			db	%00100010
			db	%00010100
			db	%00010100
			db	%00001000
			db	%00000000
			
			db	%00000010	; ?
			db	%00000001
			db	%01011001
			db	%00000101
			db	%00000010
			db	%00000000
								
			db	%00111110	; @
			db	%01000001
			db	%01011001
			db	%01011001
			db	%01001110
			db	%00000000
				
			db	%01111100	; A
			db	%00001010
			db	%00001001
			db	%00001010
			db	%01111100
			db	%00000000
				
			db	%01111111	; B
			db	%01001001
			db	%01001001
			db	%01001001
			db	%00110110
			db	%00000000
			
			db	%00111110	; C
			db	%01000001
			db	%01000001
			db	%01000001
			db	%00100010
			db	%00000000
		
			db	%01111111	; D
			db	%01000001
			db	%01000001
			db	%00100010
			db	%00011100
			db	%00000000
				
			db	%01111111	; E
			db	%01001001
			db	%01001001
			db	%01000001
			db	%01000001
			db	%00000000
				
			db	%01111111	; F
			db	%00001001
			db	%00001001
			db	%00000001
			db	%00000001
			db	%00000000
			
			db	%00111110	; G
			db	%01000001
			db	%01000001
			db	%01010001
			db	%01110010
			db	%00000000

			db	%01111111	; H
			db	%00001000
			db	%00001000
			db	%00001000
			db	%01111111
			db	%00000000
				
			db	%00000000	; I
			db	%01000001
			db	%01111111
			db	%01000001
			db	%00000000
			db	%00000000
				
			db	%00100001	; J
			db	%01000001
			db	%01000001
			db	%00111111
			db	%00000001
			db	%00000000
			
			db	%01111111	; K
			db	%00001000
			db	%00010100
			db	%00100010
			db	%01000001
			db	%00000000
		
			db	%01111111	; L
			db	%01000000
			db	%01000000
			db	%01000000
			db	%01000000
			db	%00000000
				
			db	%01111111	; M
			db	%00000010
			db	%00000100
			db	%00000010
			db	%01111111
			db	%00000000
				
			db	%01111111	; N
			db	%00000010
			db	%00011100
			db	%00100000
			db	%01111111
			db	%00000000
			
			db	%00111110	; O
			db	%01000001
			db	%01000001
			db	%01000001
			db	%00111110
			db	%00000000
																
			db	%01111111	; P
			db	%00001001
			db	%00001001
			db	%00001001
			db	%00000110
			db	%00000000
				
			db	%00111110	; Q
			db	%01000001
			db	%01010001
			db	%00100001
			db	%01011110
			db	%00000000
			
			db	%01111111	; R
			db	%00001001
			db	%00011001
			db	%00101001
			db	%01000110
			db	%00000000
		
			db	%00100110	; S
			db	%01001001
			db	%01001001
			db	%01001001
			db	%00110010
			db	%00000000
				
			db	%00000001	; T
			db	%00000001
			db	%01111111
			db	%00000001
			db	%00000001
			db	%00000000
				
			db	%00111111	; U
			db	%01000000
			db	%01000000
			db	%01000000
			db	%00111111
			db	%00000000
			
			db	%00000111	; V
			db	%00111000
			db	%01000000
			db	%00111000
			db	%00000111
			db	%00000000

			db	%00111111	; W
			db	%01000000
			db	%00110000
			db	%01000000
			db	%00111111
			db	%00000000
				
			db	%01100011	; X
			db	%00010100
			db	%00001000
			db	%00010100
			db	%01100011
			db	%00000000
				
			db	%00000111	; Y
			db	%00001000
			db	%01110000
			db	%00001000
			db	%00000111
			db	%00000000
			
			db	%01000001	; Z
			db	%01110001
			db	%01001001
			db	%01000111
			db	%01000001
			db	%00000000
		
			db	%00000000	; [
			db	%01111111
			db	%01000001
			db	%01000001
			db	%01000000
			db	%00000000
				
			db	%00000000	; \
			db	%00000111
			db	%00011000
			db	%01100000
			db	%00000000
			db	%00000000
				
			db	%00000000	; ]
			db	%01000001
			db	%01000001
			db	%01111111
			db	%00000000
			db	%00000000
			
						
			db	%00000100	; ^
			db	%00000010
			db	%00000001
			db	%00000010
			db	%00000100
			db	%00000000
									
			db	%10000000	; _
			db	%00000000
			db	%10000000
			db	%00000000
			db	%10000000
			db	%00000000
				
			db	%00000001	; `
			db	%00000010
			db	%00000100
			db	%00000000
			db	%00000000
			db	%00000000
			
			db	%00011000	; a
			db	%00100100
			db	%00100100
			db	%00011100
			db	%00100000
			db	%00000000
		
			db	%00111111	; b
			db	%00100100
			db	%00100100
			db	%00011000
			db	%00000000
			db	%00000000
				
			db	%00011000	; c
			db	%00100100
			db	%00100100
			db	%00100100
			db	%00000000
			db	%00000000
				
			db	%00011000	; d
			db	%00100100
			db	%00100100
			db	%00011111
			db	%00100000
			db	%00000000
			
			db	%00011000	; e
			db	%00101100
			db	%00101100
			db	%00001000
			db	%00000000
			db	%00000000

			db	%00000000	; f
			db	%00111110
			db	%00000101
			db	%00000101
			db	%00000000
			db	%00000000
				
			db	%00001000	; g
			db	%01010100
			db	%01010100
			db	%00111100
			db	%00000000
			db	%00000000
				
			db	%00111111	; h
			db	%00000100
			db	%00000100
			db	%00111000
			db	%00000000
			db	%00000000
			
			db	%00000000	; i
			db	%00000000
			db	%00111101
			db	%00000000
			db	%00000000
			db	%00000000
		
			db	%01000000	; j
			db	%01000000
			db	%00111101
			db	%00000000
			db	%00000000
			db	%00000000
				
			db	%00111111	; k
			db	%00011000
			db	%00011000
			db	%00100100
			db	%00000000
			db	%00000000
				
			db	%00000000	; l
			db	%00011111
			db	%00100000
			db	%00000000
			db	%00000000
			db	%00000000
			
			db	%00111100	; m
			db	%00000100
			db	%00111000
			db	%00000100
			db	%00111000
			db	%00000000
																
			db	%00000000	; n
			db	%00111100
			db	%00000100
			db	%00000100
			db	%00111000
			db	%00000000
				
			db	%00000000	; 0
			db	%00011000
			db	%00100100
			db	%00100100
			db	%00011000
			db	%00000000
			
			db	%01111100	; p
			db	%00010100
			db	%00010100
			db	%00001000
			db	%00000000
			db	%00000000
		
			db	%00001000	; q
			db	%00010100
			db	%00010100
			db	%01111100
			db	%01000000
			db	%00000000
				
			db	%00000000	; r
			db	%00111000
			db	%00000100
			db	%00000100
			db	%00000000
			db	%00000000
				
			db	%00101000	; s
			db	%00101100
			db	%00101100
			db	%00010100
			db	%00000000
			db	%00000000
			
			db	%00000000	; t
			db	%00011111
			db	%00100100
			db	%00100100
			db	%00000000
			db	%00000000

			db	%00011100	; u
			db	%00100000
			db	%00100000
			db	%00100000
			db	%00011100
			db	%00000000
				
			db	%00000100	; v
			db	%00011000
			db	%00100000
			db	%00011000
			db	%00000100
			db	%00000000
				
			db	%00011100	; w
			db	%00100000
			db	%00011000
			db	%00100000
			db	%00011100
			db	%00000000
			
			db	%00100100	; x
			db	%00011000
			db	%00011000
			db	%00100100	
			db	%00000000
			db	%00000000
		
			db	%00000100	; y
			db	%01011000
			db	%00100000
			db	%00011000
			db	%00000100
			db	%00000000
				
			db	%00100100	; z
			db	%00110100
			db	%00101100
			db	%00100100
			db	%00000000
			db	%00000000
				
			db	%00000000	; {
			db	%00001000
			db	%00101010
			db	%01010101
			db	%01000001
			db	%00000000
			
			db	%00000000	; |
			db	%00000000
			db	%01110111
			db	%00000000
			db	%00000000
			db	%00000000
									
			db	%01000001	; }
			db	%01010101
			db	%00101010
			db	%00001000
			db	%00000000
			db	%00000000
			
			db	%00000010	; ~
			db	%00000001
			db	%00000010
			db	%00000100
			db	%00000010
			db	%00000000
									
			db	%01000110	; £
			db	%00101001
			db	%01011001
			db	%01001001
			db	%01100010
			db	%00000000
			






							
; Alternate Graphics Codes for ADM-3A
COL80G:

;G00
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000
			
;G01
			db	%00000111
			db	%00000111
			db	%00000111
			db	%00000000
			db	%00000000
			db	%00000000
			
;G02
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000111
			db	%00000111
			db	%00000111
			
;G03
			db	%00000111
			db	%00000111
			db	%00000111
			db	%00000111
			db	%00000111
			db	%00000111

;G04
			db	%00011000
			db	%00011000
			db	%00011000
			db	%00000000
			db	%00000000
			db	%00000000
			
;G05
			db	%00011111
			db	%00011111
			db	%00011111
			db	%00000000
			db	%00000000
			db	%00000000
			
;G06
			db	%00011000
			db	%00011000
			db	%00011000
			db	%00000111
			db	%00000111
			db	%00000111
			
;G07
			db	%00011111
			db	%00011111
			db	%00011111
			db	%00000111
			db	%00000111
			db	%00000111

;G08
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00011000
			db	%00011000
			db	%00011000
			
;G09
			db	%00000111
			db	%00000111
			db	%00000111
			db	%00011000
			db	%00011000
			db	%00011000
			
;G0A
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00011111
			db	%00011111
			db	%00011111
			
;G0C
			db	%00000111
			db	%00000111
			db	%00000111
			db	%00011111
			db	%00011111
			db	%00011111

;G0B
			db	%00011000
			db	%00011000
			db	%00011000
			db	%00011000
			db	%00011000
			db	%00011000
			
;G0D
			db	%00011111
			db	%00011111
			db	%00011111
			db	%00011000
			db	%00011000
			db	%00011000
			
;G0E
			db	%00011000
			db	%00011000
			db	%00011000
			db	%00011111
			db	%00011111
			db	%00011111
			
;G0F
			db	%00011111
			db	%00011111
			db	%00011111
			db	%00011111
			db	%00011111
			db	%00011111
			
;G10
			db	%11100000
			db	%11100000
			db	%11100000
			db	%00000000
			db	%00000000
			db	%00000000
			
;G11
			db	%11100111
			db	%11100111
			db	%11100111
			db	%00000000
			db	%00000000
			db	%00000000
			
;G12
			db	%11100000
			db	%11100000
			db	%11100000
			db	%00000111
			db	%00000111
			db	%00000111
			
;G13
			db	%11100111
			db	%11100111
			db	%11100111
			db	%00000111
			db	%00000111
			db	%00000111

;G14
			db	%11111000
			db	%11111000
			db	%11111000
			db	%00000000
			db	%00000000
			db	%00000000
			
;G15
			db	%11111111
			db	%11111111
			db	%11111111
			db	%00000000
			db	%00000000
			db	%00000000
			
;G16
			db	%11111000
			db	%11111000
			db	%11111000
			db	%00000111
			db	%00000111
			db	%00000111
			
;G17
			db	%11111111
			db	%11111111
			db	%11111111
			db	%00000111
			db	%00000111
			db	%00000111

;G18
			db	%11100000
			db	%11100000
			db	%11100000
			db	%00011000
			db	%00011000
			db	%00011000
			
;G19
			db	%11100111
			db	%11100111
			db	%11100111
			db	%00011000
			db	%00011000
			db	%00011000
			
;G1A
			db	%11100000
			db	%11100000
			db	%11100000
			db	%00011111
			db	%00011111
			db	%00011111
			
;G1B
			db	%11100111
			db	%11100111
			db	%11100111
			db	%00011111
			db	%00011111
			db	%00011111

;G1C
			db	%00011000
			db	%00011000
			db	%00011000
			db	%00011000
			db	%00011000
			db	%00011000
			
;G1D
			db	%11111111
			db	%11111111
			db	%11111111
			db	%00011000
			db	%00011000
			db	%00011000
			
;G1E
			db	%11111000
			db	%11111000
			db	%11111000
			db	%00011111
			db	%00011111
			db	%00011111
			
;G1F
			db	%11111111
			db	%11111111
			db	%11111111
			db	%00011111
			db	%00011111
			db	%00011111
			
;G20
			db	%00000000
			db	%00000000
			db	%00000000
			db	%11100000
			db	%11100000
			db	%11100000
			
;G21
			db	%00000111
			db	%00000111
			db	%00000111
			db	%11100000
			db	%11100000
			db	%11100000
			
;G22
			db	%00000000
			db	%00000000
			db	%00000000
			db	%11100111
			db	%11100111
			db	%11100111
			
;G23
			db	%00000111
			db	%00000111
			db	%00000111
			db	%11100111
			db	%11100111
			db	%11100111

;G24
			db	%00011000
			db	%00011000
			db	%00011000
			db	%11100000
			db	%11100000
			db	%11100000
			
;G25
			db	%00011111
			db	%00011111
			db	%00011111
			db	%11100000
			db	%11100000
			db	%11100000
			
;G26
			db	%00011000
			db	%00011000
			db	%00011000
			db	%11100111
			db	%11100111
			db	%11100111
			
;G27
			db	%00011111
			db	%00011111
			db	%00011111
			db	%11100111
			db	%11100111
			db	%11100111

;G28
			db	%00000000
			db	%00000000
			db	%00000000
			db	%11111000
			db	%11111000
			db	%11111000
			
;G29
			db	%00000111
			db	%00000111
			db	%00000111
			db	%11111000
			db	%11111000
			db	%11111000
			
;G2A
			db	%00000000
			db	%00000000
			db	%00000000
			db	%11111111
			db	%11111111
			db	%11111111
			
;G2B
			db	%00000111
			db	%00000111
			db	%00000111
			db	%11111111
			db	%11111111
			db	%11111111

;G2C
			db	%11111000
			db	%11111000
			db	%11111000
			db	%11111000
			db	%11111000
			db	%11111000
			
;G2D
			db	%00011111
			db	%00011111
			db	%00011111
			db	%11111000
			db	%11111000
			db	%11111000
			
;G2E
			db	%00011000
			db	%00011000
			db	%00011000
			db	%11111111
			db	%11111111
			db	%11111111
			
;G2F
			db	%00011111
			db	%00011111
			db	%00011111
			db	%11111111
			db	%11111111
			db	%11111111

;G30
			db	%11100000
			db	%11100000
			db	%11100000
			db	%11100000
			db	%11100000
			db	%11100000
			
;G31
			db	%11100111
			db	%11100111
			db	%11100111
			db	%11100000
			db	%11100000
			db	%11100000
			
;G32
			db	%11100000
			db	%11100000
			db	%11100000
			db	%11100111
			db	%11100111
			db	%11100111
			
;G33
			db	%11100111
			db	%11100111
			db	%11100111
			db	%11100111
			db	%11100111
			db	%11100111

;G34
			db	%11111000
			db	%11111000
			db	%11111000
			db	%11100000
			db	%11100000
			db	%11100000
			
;G35
			db	%11111111
			db	%11111111
			db	%11111111
			db	%11100000
			db	%11100000
			db	%11100000
			
;G36
			db	%11111000
			db	%11111000
			db	%11111000
			db	%11100111
			db	%11100111
			db	%11100111
			
;G37
			db	%11111111
			db	%11111111
			db	%11111111
			db	%11100111
			db	%11100111
			db	%11100111

;G38
			db	%11100000
			db	%11100000
			db	%11100000
			db	%11111000
			db	%11111000
			db	%11111000
			
;G39
			db	%11100111
			db	%11100111
			db	%11100111
			db	%11111000
			db	%11111000
			db	%11111000
			
;G3A
			db	%11100000
			db	%11100000
			db	%11100000
			db	%11111111
			db	%11111111
			db	%11111111
			
;G3B
			db	%11100111
			db	%11100111
			db	%11100111
			db	%11111111
			db	%11111111
			db	%11111111

;G3C
			db	%11111000
			db	%11111000
			db	%11111000
			db	%11111000
			db	%11111000
			db	%11111000
			
;G3D
			db	%11111111
			db	%11111111
			db	%11111111
			db	%11111000
			db	%11111000
			db	%11111000
			
;G3E
			db	%11111000
			db	%11111000
			db	%11111000
			db	%11111111
			db	%11111111
			db	%11111111
			
;G3F
			db	%11111111
			db	%11111111
			db	%11111111
			db	%11111111
			db	%11111111
			db	%11111111


PACGRAPH:
; Lines and stuff...

;PC0
			db	%00100100
			db	%11111111
			db	%00100100
			db	%00100100
			db	%11111111
			db	%00100100


;PC1
			db	%00000100
			db	%00000011
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000

;PC2
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000011
			db	%00000100

;PC3
			db	%00000100
			db	%00000011
			db	%00000000	
			db	%00000000
			db	%00000011
			db	%00000100
						

;PC4
			db	%00100000
			db	%11000000
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000	

;PC5	
			db	%00100100
			db	%11000011
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000

;PC6
			db	%00100000
			db	%11000000
			db	%00000000
			db	%00000000
			db	%00000011
			db	%00000100

;PC7
			db	%00100100
			db	%11000011
			db	%00000000	
			db	%00000000
			db	%00000011
			db	%00000100

;PC8
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000
			db	%11000000
			db	%00100000	

;PC9		
			db	%00000100
			db	%00000011
			db	%00000000				
			db	%00000000
			db	%11000000
			db	%00100000

;PCA
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000
			db	%11000011
			db	%00100100	
			
;PCB		
			db	%00000100
			db	%00000011
			db	%00000000
			db	%00000000
			db	%11000011
			db	%00100100	
			
;PCC					
			db	%00100000
			db	%11000000
			db	%00000000
			db	%00000000
			db	%11000000
			db	%00100000	

;PCD
			db	%00100100
			db	%11000011
			db	%00000000			
			db	%00000000
			db	%11000000
			db	%00100000	

;PCE
			db	%00100000
			db	%11000000
			db	%00000000
			db	%00000000
			db	%11000011
			db	%00100100
			
;PCF
			db	%00100100
			db	%11000011
			db	%00000000
			db	%00000000
			db	%11000011
			db	%00100100


;PD0		
			db	%00000100
			db	%00000100
			db	%00000100
			db	%00000100
			db	%00000100
			db	%00000100

;PD1		
			db	%00100000
			db	%00100000
			db	%00100000
			db	%00100000
			db	%00100000
			db	%00100000

;PD2
			db	%00100100
			db	%00100100
			db	%00100100
			db	%00100100
			db	%00100100
			db	%00100100

;PD3
			db	%00000000
			db	%11111111
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000

;PD4
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000
			db	%11111111
			db	%00000000

;PD5
			db	%00000000
			db	%11111111
			db	%00000000
			db	%00000000
			db	%11111111
			db	%00000000
			
;PD6		
			db	%00000100
			db	%00000011
			db	%00000000
			db	%00000000
			db	%11111111
			db	%00000000
			
;PD7					
			db	%00100000
			db	%11000000
			db	%00000000
			db	%00000000
			db	%11111111
			db	%00000000

;PD8
			db	%00100100
			db	%11000011
			db	%00000000			
			db	%00000000
			db	%11111111
			db	%00000000				

;PD9
			db	%00000000
			db	%11111111
			db	%00000000	
			db	%00000000
			db	%00000011
			db	%00000100			

;PDA			
			db	%00000000
			db	%11111111
			db	%00000000	
			db	%00000000
			db	%11000000
			db	%00100000

;PDB
			db	%00000000
			db	%11111111
			db	%00000000	
			db	%00000000
			db	%11000011
			db	%00100100

;PDC
			db	%00100100
			db	%11000100
			db	%00000100
			db	%00000100
			db	%00000100
			db	%00000100

;PDD
			db	%00000100
			db	%00000100
			db	%00000100
			db	%00000100
			db	%11000100
			db	%00100100

;PDE
			db	%00100100
			db	%11000100
			db	%00000100
			db	%00000100
			db	%11000100
			db	%00100100

;PDF
			db	%00100100
			db	%00100011
			db	%00100000	
			db	%00100000
			db	%00100000	
			db	%00100000	

;PE0
			db	%00100000	
			db	%00100000
			db	%00100000	
			db	%00100000
			db	%00100011
			db	%00100100	

;PE1
			db	%00100100
			db	%00100011
			db	%00100000	
			db	%00100000
			db	%00100011
			db	%00100100				

;PE2
			db	%00100100
			db	%00100011
			db	%00100000
			db	%00010000
			db	%00001111
			db	%00000000			

;PE3
			db	%00000000
			db	%00001111
			db	%00010000
			db	%00100000
			db	%00100011
			db	%00100100			

;PE4
			db	%00100100
			db	%11000100
			db	%00000100
			db	%00001000
			db	%11110000
			db	%00000000

;PE5
			db	%00000000
			db	%11110000
			db	%00001000
			db	%00000100
			db	%11000100
			db	%00100100
			
;PE6
			db	%00011000
			db	%00011000
			db	%00011000
			db	%00011000
			db	%00011000
			db	%00011000

;PE7
			db	%00000000
			db	%00000000
			db	%11111111
			db	%11111111
			db	%00000000
			db	%00000000

;PE8
			db	%00011000
			db	%00011000
			db	%00011111
			db	%00001111
			db	%00000000
			db	%00000000

;PE9
			db	%00000000
			db	%00000000
			db	%00001111
			db	%00011111
			db	%00011000
			db	%00011000

;PEA
			db	%00011000
			db	%00011000
			db	%11111000
			db	%11110000
			db	%00000000
			db	%00000000

;PEB
			db	%00000000
			db	%00000000
			db	%11110000
			db	%11111000
			db	%00011000
			db	%00011000

;PEC
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000

;PED
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000

;PEE
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000

;PEF
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000


;PF0
			db 	%00000000
			db	%00000000
			db 	%00000000
			db	%00000000
			db 	%00000000
			db	%00000000

;PF1
			db	%00000000	
			db	%00000011
			db	%00000100	
			db	%00000100
			db	%00000011	
			db	%00000000


;PF2
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00011000
			db	%00100100

;PF3
			db	%00000000	
			db	%00000011
			db	%00000100	
			db	%00000100
			db	%00011011	
			db	%00100100
;PF4
			db	%00100100
			db	%00011000
			db	%00000000
			db	%00000000
			db	%00000000
			db	%00000000
			
;PF5
			db	%00100100	
			db	%00011011
			db	%00000100	
			db	%00000100
			db	%00000011	
			db	%00000000

;PF6
			db	%00100100
			db  %00011000
			db  %00000000
			db	%00000000
			db  %00011000
			db  %00100100

;PF7
			db	%00100100	
			db	%00011011
			db	%00000100	
			db	%00000100
			db	%00011011	
			db	%00100100

;PF8
			db	%00000000
			db	%11000000
			db	%00100000
			db	%00100000
			db	%11000000
			db	%00000000
			
;PF9
			db	%00000000
			db	%11000011
			db	%00100100
			db  %00100100
			db  %11000011
			db  %00000000
			
;PFA		
			db	%00000000
			db  %11000000
			db	%00100000
			db	%00100000
			db	%11011000
			db	%00100100
			
;PFB
			db	%00000000
			db  %11000011
			db	%00100100
			db	%00100100
			db	%11011011
			db	%00100100
			
;PFC
			db	%00100100
			db	%11011000
			db	%00100000
			db	%00100000
			db	%11000000
			db	%00000000
			
;PFD			
			db	%00100100
			db	%11011011
			db	%00100100
			db	%00100100
			db	%11000011
			db	%00000000			
			
;PFE			
			db	%00100100
			db	%11011000
			db	%00100000
			db	%00100000
			db	%11011000
			db	%00100100
			
;PFF			
			db	%00100100
			db	%11011011
			db	%00100100
			db	%00100100
			db	%11011011
			db	%00100100			
			
			
			

			
;.ORG $2000	; Video Character Map. We use this to keep track of where characters have been printed and can use it to "refresh" the display. Useful to return in case we process a popupp t
			;    accessed the screen video directly. 
			; format - 2040 characters. 20
;BLOCK		$1000

.equ CHARMAP,$3000	; Character Map.
.equ ATTRMAP,$3800  ; Attribute Map. 
VIDEOSTACK: BLOCK 64	; Small VIDEO STACK. 
									; Wildcard for FCB....
;DR F1 F2 F3 F4 F5 F6 F7 F8 T1 T2 T3 EX S1 S2 RC  .FILENAMETYP...
;AL AL AL AL AL AL AL AL AL AL AL AL AL AL AL AL  ...............
;CR R0 R1 R2    
;
;The bytes in it have the following meanings:
;
;
;FCB+00h DR - Drive. 0 for default, 1-16 for A-P. In DOSPLUS,
;             bit 7 can be set to indicate that the operation should work with
;             subdirectories rather than files.
;           
;FCB+01h Fn - Filename, 7-bit ASCII. The top bits of the filename bytes
;             (usually referred to as F1' to F8') have the following
;             meanings:
;               F1'-F4' - User-defined attributes. Any program can use
;                        them in any way it likes. The filename in the
;                        disc directory has the corresponding bits set.
;               F5'-F8' - Interface attributes. They modify the 
;                        behaviour of various BDOS functions or 
;                        indicate error conditions. In the directory
;                        these bits are always zero.
;FCB+09h Tn - Filetype, 7-bit ASCII. T1' to T3' have the following
;             meanings:
;               T1' - Read-Only. 
;               T2' - System (hidden). System files in user 0 can be
;                    opened from other user areas.
;               T3' - Archive. Set if the file has not been changed
;                    since it was last copied.
;FCB+0Ch EX - Set this to 0 when opening a file and then leave it to
;            CP/M. You can rewind a file by setting EX, RC, S2 and CR to 0.
;FCB+0Dh S1 - Reserved.   But otherwise upper bits of RC ( Remember, Bit 7 is something else )
;FCB+0Eh S2 - Reserved.	  But otherwise upper bits of EX ( Remember, Bit 7 is something else )
;FCB+0Fh RC - Set this to 0 when opening a file and then leave it to
;            CP/M.
;FCB+10h AL - Image of the second half of the directory entry,
;            containing the file's allocation (which disc blocks it
;            owns).
;FCB+20h CR - Current record within extent. It is usually best to set 
;            this to 0 immediately after a file has been opened and 
;            then ignore it.
;FCB+21h Rn - Random access record number (not CP/M 1). A 16-bit 
;            value in CP/M 2 (with R2 used for overflow); an 18-bit
;            value in CP/M 3. 
;
; CR - Current Record ( Sequential Read )
; R0, R1, R2 - Random read ( R0, R1 = 16 bit. R2 Overflow. )


		
;**************************************************************
; I really wish I knew why this bios has a -3 jump... Would be good to figure that out. Might be due to CP/M 3???
; Jump table here - Define the BIOS entry points. 
.EQU		BIOS	,$FC03		; Location of BIOS. Though in reality, we have boot. Need to understand if BOOT=0 or BOOT=-3.
.EQU		BOOT	,BIOS-3		;-3: Cold Boot - Set up system. 
.EQU		WBOOT	,BIOS+0		; 0: Warm boot - reload command processor
.EQU		WBOOTE	,BIOS+0
.EQU		CONST	,BIOS+3		; 3: Console status - A=0 No character ready, A=FF character waiting to be read.
.EQU 		CONIN	,BIOS+6		; 6: Console input - Wait until character then A=character.
.EQU		CONOUT	,BIOS+9		; 9: Console output - Write C register  to screen.
.EQU		LIST	,BIOS+12	;12: Printer output - Wait until ready, then write C register to printer.
.EQU		PUNCH	,BIOS+15	;15: Paper tape punch output - Wait until ready then write C to Punch Reader ( or AUX ). 
.EQU		READER	,BIOS+18	;18: Paper tape reader input - Wait until ready, then return A=Character. If not implemented, return 26 ( Ctrl Z )
.EQU		HOME	,BIOS+21	;21: Move disc head to track 0
.EQU		SELDSK	,BIOS+24	;24: Select disc drive	- C Register = disk 0...F. Enter with E=0 or E=FF. 
				; If bit 0 of E is 0, then the disc is logged in as if new; if the format has to be determined from the boot sector, for example, this will be done.
				;If bit 0 if E is 1, then the disc has been logged in before. The disc is not accessed; the DPH address (or zero) is returned immediately.
				;SELDSK returns the address of a Disc Parameter Header in HL. The exact format of a DPH varies between CP/M versions; note that under CP/M 3, 
				;	the DPH is in memory bank 0 and probably not visible to programs. If the disc could not be selected it returns HL=0.
.EQU		SETTRK	,BIOS+27	;27: Set track number - Track in BC - Is a word - Starts at 0. 
.EQU		SETSEC	,BIOS+30	;30: Set sector number - Sector in BC - Is a word. Starts at 1. I think. Will find out. 
.EQU		SETDMA	,BIOS+33	;33: Set DMA address - Set DMA address. 
.EQU		READ	,BIOS+36	;36: Read a sector - To DMA address. A=0 success. A=1 unrecoverable error. A=FF Media changed.
.EQU		WRITE	,BIOS+39	;39: Write a sector - C contains blocking code.  0=deferred. 1=immediate. 2=Can be deferred. No preread necessary. A=0 success. 
				;	A=1 unrecoverable error. A=FF Media changed.
;In CP/M 2 and later, the following extra jumps appear:
.EQU		LISTST	,BIOS+42	;42: Status of list device - A=0 Printer not ready. A=FF -printer ready. 
.EQU		SECTRAN	,BIOS+45	;45: Sector translation for skewing DE=Tableaddress (IGNORE DE). BC=Incoming sector (logical). HL=Translated sector (physical) . Return BC. 
.EQU		SECTRN	,BIOS+45	;45: Sector translation for skewing DE=Tableaddress (IGNORE DE). BC=Incoming sector (logical). HL=Translated sector (physical) . Return BC. 
	
	
; CPM Calls as EQU functions.
EQU	System_Reset		,	0
EQU	Console_Input		,	1
EQU	Console_Output		,	2
EQU	Reader_Input		,	3
EQU	Punch_Output		,	4
EQU	List_Output			,	5
EQU	Direct_Console_IO 	,	6
EQU	Get_IO_Byte			,	7
EQU	Set_IO_Byte			,	8
EQU	Print_String		,	9
EQU	Read_Console_String	,	10
EQU	Get_Console_Status	,	11
EQU	Return_Version		,	12
EQU	Reset_Disk_System	,	13
EQU	Select_Disk			,	14
EQU	Open_File			,	15
EQU	Close_File			,	16
EQU	Search_For_First	,	17
EQU	Search_For_Next		,	18
EQU	Delete_File			,	19
EQU	Read_Sequential		,	20
EQU	Write_Sequential	,	21
EQU	Make_File			,	22
EQU	Rename_File			,	23
EQU	Return_Login_Vector	,	24
EQU	Return_Current_Disk	,	25
EQU	Set_DMA_Address		,	26
EQU	Get_ADDR_ALLOC		,	27
EQU	Write_Protect_Disk	,	28
EQU	Get_RO_Vector		,	29
EQU	Set_File_Attributes	,	30
EQU	Get_ADDR_DiskParms	,	31
EQU	Set_Get_User_Code 	,	32
EQU	Read_Random			,	33
EQU	Write_Random		,	34
EQU	Compute_File_Size	,	35
EQU	Set_Random_Record	,	36
EQU	Reset_Drive			,	37
EQU	Access_Drive		,	38
EQU	Free_Drive			,	39
EQU	Write_Random_Fill	,	40
; Special.
EQU	Write_Dry			,   41	; Dry write - Not a real function. 


FBC:	DW		$00			; File Control Block location Save. First is a label in memory. Rest are offset for IX.
EQU		DR,		$00			; Offset 0
EQU		EX,		$0C			; Extent counter. 00, 01 etc. First extent is 00.
EQU		S1,		$0D
EQU		S2,		$0E
EQU		RC,		$0F			; Record counter. File is RC * 128 bytes. 
EQU		ALLOC1,	$10			; First byte of allocation data or other contents. Copied from Directory for this extent.
EQU		ALLOC2, $11			; Second byte of the allocation data or other contents. 
EQU		CR,		$20			; Current record within extent. 0= first record. 
EQU		R0,		$21			; Low byte first. 16 bit counter for Random Access Record Number. Record =128 block. 
EQU		R1,		$22			; Random Access Record high byte.
EQU		R2,		$23			; Random Access Record Count Overflow. 


DPH:		DB		$00		; Disk Parameter Header file. Get temorarily to 
EQU		TRANS0,		$00		; Put in label for vector here if translation table. 00=no translate
EQU		SCRATCH1,	$02		; Scratchpad.
EQU		SCRATCH2,	$04		; Scratchpad.
EQU		SCRATCH3,	$06		; Scratchpad.
EQU		DIRBF,		$08		; 128 byte scratchpad buffer for file directory. ( used by BDOS )
EQU		DIRBF_H,	$09			; High byte. 		
EQU		DPBLK,		$0A		; Disk Parameter Block. 
EQU		CHK00,		$0C		;
EQU		ALL00,		$0E		; Allocation table. 
	EQU	ALL00_H,	$0FH	; High byte allocation table.
	
	;DISK PARAMETER BLOCK Offsets. 
EQU		SPT,	$00	; SPT Sectors per track. ie, 0 to 31. 
	EQU	SPT_H,	$01	; 	SPT High Byte Offset
EQU		BSF,	$02	; BSF Block Shift Factor - 3 bits over Bit6 ( Bit6 = 128 = allocation ). Bit7 = 256., Bit 8=512 and Bit9 is 1024 ( 0 to 1023 ). Number of bits past Bit6.
EQU		BLM,	$03	; BLM Block Mask - Related directly to the above.  =2^BSF-1...  7 is a 3 bit mask, so BSF must be 3. 
EQU 	EXM,	$04 ; EXM - Extent Mask - 
EQU		DSM,	$05	; DSM - Disk size -1 - ie, 242= 243 BLOCKS. if the block size=1024k then  248,832 bytes on disk. Block=Allocation Unit. 
	EQU	DSM_H,	$06 ;	DSM High Byte - If not zero, two byte allocations. 
EQU		DRM,	$07	; DRM - Directory Max - Directory Entries = 64 entries. If 1024K allocation holds 32 directory entries then this means 2 allocations for directory. 
	EQU	DRM_H,	$08	; 	DRM High Byte offset. 
EQU		AL0,	$09	; AL0 - Alloc 0  ROTATE LEFT, One bit set per directory allocation.  something like RL(IX+AL0) ???
EQU		AL1,	$0A	; AL1 - Yet, they used 16 bits for this -  is it to quick-set the ALL00? I think it might be.
EQU		CKS,	$0B	; CKS - Check size = (DRM+1) /4 for removable media. = 64/4 = 16... In this case, If fixed then DRM=0 = Do not check directory records - Not sure why.
EQU		OFF,	$0D	; OFF - Track Offset. Number of "Reserved" tracks.  
		
;.NOTES
;Everything after this point can just be text. It's just general notes. The assembly should stop at end or notes. 
;5.6 System Function Summary
;Function
;Number 	Function
;		Name					Input 					Output
;Dec	Hex
;0	0	System Reset			none					none
;1	1	Console Input			none					A = ASCII char
;2	2	Console Output			E = char				none
;3	3	Reader Input			none					A = ASCII char
;4	4	Punch Output			E = char				none
;5	5	List Output				E = char				none
;6	6	Direct Console I/O 		E = $FF (input)		A = char
;								E = $FE (status)		A = status
;								E = char				none
;7	7	Get I/O Byte			none					A = I/O byte value
;8	8	Set I/O Byte			E = I/O byte			none
;9	9	Print String			DE = Buffer Address		none
;10	A	Read Console String		DE = Buffer				Console characters in Buffer
;11	B	Get Console Status		none					A = 00/non zero
;12	C	Return Version #		none					HL = Version #
;13	D	Reset Disk System		none					none
;14	E	Select Disk				E = Disk #				none
;15	F	Open File				DE = FCB address		A = FF if not found
;16	10	Close File				DE = FCB address		A = FF if not found
;17	11	Search For First		DE = FCB address		A = Directory Code
;18	12	Search For Next			none					A = Directory Code
;19	13	Delete File				DE = FCB address		A = none
;20	14	Read Sequential			DE = FCB address		A = Error Code
;21	15	Write Sequential		DE = FCB Address		A = Error Code
;22	16	Make File				DE = FCB address		A = FF if no DIR Space
;23	17	Rename File				DE = FCB address		A = FF if not found
;24	18	Return Login Vector		none					HL = Login Vector*
;25	19	Return Current Disk		none					A = Current Disk Number
;26	1A	Set DMA Address			DE = DMA address		none
;27	1B	Get ADDR (ALLOC)		none					HL = ALLOC address*
;28	1C	Write Protect Disk		none					none
;29	1D	Get Read/only Vector	none					HL = ALLOC address*
;30	1E	Set File Attributes		DE = FCB address		A = none
;31	1F	Get ADDR (Disk Parms)	none					HL = DPB address
;32	20	Set/Get User Code 		E = $FF for Get		A = User Number
;								E = 00 to $0F for Set	none
;33	21	Read Random				DE = FCB address		A = Error
;34	22	Write Random			DE = FCB address		A = Error Code
;35	23	Compute File Size		DE = FCB address		r0, r1, r2
;36	24	Set Random Record		DE = FCB address		r0, r1, r2
;37	25	Reset Drive				DE = Drive Vector		A = 0
;38	26	Access Drive			not supported			not supported
;39	27	Free Drive				not supported			not supported
;40	28	Write Random w/Fill		DE = FCB				A = error code
