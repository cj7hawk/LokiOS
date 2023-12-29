; CCP I am writing. Contains some BIOS routines as well. I'll keep those at the end. 
; 19/11/2022 - Got to fix the command number selection.  - FIXED.
; 24/12/2022 - Bug - Can't run different programs within same session... Something wrong with routine. Just reruns same loaded program. FIXED.
; 28/02/2023 - Only recognizes commands locally on the selected drive, or L: - Maybe add N: later? Or some kind of path scheme?	 SEE LAST COMMENT.
; 1/3/2023   - Need to work out why I can't copy between disks. I think it's because I'm not updating all the registers. Must update each time. FIXED. 
; Consider use of EXEC_DMA buffer. 
; Need to make better use of EXEC_FCB to allow execution of commands from other drives, and maybe simplify the command function at the same time 
; while allowing drive specifiers. eg: A:COMMAND>COM - Execute COMMAND on A:, not the local.
; 19/05/2023 - Long string without whitespace crashes CCP. Due to overwriting code in buffer, which is 16 bytes, but first byte is a counter, so needed 17 bytes...... Fixed. 
; 22/06/2023 - Need to work out why autoboot command (LOKI.COM) isn't working suddenly. I am probably starting up wrong with recent changes. BOOT reset order.
; 22/06/2023 - Need to add RESET command into CCP for when things go wrong and somehow I end up back in the wrong CCP... Seems it can happen with code left
;                   behind when transitioning to DRI CCP. 
; 29-12-2023 - Moved EOF check in TYPE command to before the output stage (so it doesn't output ASCII $1A to screen)
; 29-12-2023 - Added CTRL-C on Type command

equ	DRIVE_SELECTED	, $0004	; 16 bit.
equ BDOS 			, $0005	; Address of BDOS jump. 
equ	COMMAND_BUFFER	, $0080	; This is where we will process the input buffer. 
equ	COMMAND_LENGTH	, $0040	; How long is the command buffer?
equ	FCB1			, $005C ; This is the FIRST FCB in the Zero Page.
equ	FCB2			, $006C	; This is the SECOND FCB in the Zero Page. Will be overwritten by opening or searching FCB1.


;
        org     $D000			
		JP		WITH_COMMAND					; If we use the default command. 
		JP		WITHOUT_COMMAND
		
WITH_COMMAND:

		LD		A,$FF
		LD		(SPECIAL_ERROR),A				; Special errors if these commands are not found.


		LD		DE,INIT_COMMAND1				; Nothing special for this. If we booted, it should be there. 
		CALL	COMMAND_MANUAL					; Execute the command pointed to by DE. 

		LD		HL,SPECIAL_ERROR_V
		LD		(SPECIAL_ERROR_STRING),HL
		LD		DE,INIT_COMMAND2
		CALL	COMMAND_MANUAL					; Execute the command pointed to by DE. 

		LD		HL,SPECIAL_ERROR_N
		LD		(SPECIAL_ERROR_STRING),HL
		LD 		DE,INIT_COMMAND3
		CALL	COMMAND_MANUAL					; Execute the command pointed to by DE. 

		LD		HL,SPECIAL_ERROR_U1
		LD		(SPECIAL_ERROR_STRING),HL
		LD 		DE,INIT_COMMAND4
		CALL	COMMAND_MANUAL					; Execute the command pointed to by DE. 		

		LD		HL,SPECIAL_ERROR_U2
		LD		(SPECIAL_ERROR_STRING),HL
		LD 		DE,INIT_COMMAND5
		CALL	COMMAND_MANUAL					; Execute the command pointed to by DE. 		

		LD		HL,SPECIAL_ERROR_D
		LD		(SPECIAL_ERROR_STRING),HL
		LD 		DE,INIT_COMMAND6
		CALL	COMMAND_MANUAL					; Execute the command pointed to by DE. 

		LD		HL,SPECIAL_ERROR_D1
		LD		(SPECIAL_ERROR_STRING),HL
		LD 		DE,INIT_COMMAND7
		CALL	COMMAND_MANUAL					; Execute the command pointed to by DE. 
		
		LD		HL,SPECIAL_ERROR_D2
		LD		(SPECIAL_ERROR_STRING),HL
		LD 		DE,INIT_COMMAND8
		CALL	COMMAND_MANUAL					; Execute the command pointed to by DE. 

		
		XOR		A
		LD		(SPECIAL_ERROR),A				; Note that special errors are over. 
	

WITHOUT_COMMAND:
		Call INITIALISE	
		XOR		A
		LD		(FIRST_COMMAND),A
		
REPEAT_CCP:	
		CALL	COMMAND							; Process any commands in the console line and return the value in C if correct.

		LD		A,$00
		LD		(FIRST_COMMAND),A				; Clear any buffered commands or we will repeat on no entry. 
        JR		REPEAT_CCP

LOGGED_DRIVE:									; Show the logged drive. 
;		LD		A,(DRIVE_SELECTED)
		CALL	CRLF

		LD		C,Return_Current_Disk
		CALL	BDOS							; Get the current logged disk drive. 
		AND		$0F
		ADD		A,$41
		LD		E,A
		LD		C,Console_Output
		CALL	BDOS
		LD		E,'>'
		LD		C,Console_Output
		CALL	BDOS
		ret


INITIALISE:
												; Populate debugging routines. 

		LD		HL,SHOWREG						; Show Registers routine.
		LD		A,L
		LD		($0031),A
		LD		A,H
		LD		($0032),A
		LD		A,$C3	; JUMP..
		LD		($0030),A						; Set up SHOWREG as RST30. 
		
		LD		HL,SHOWIX
		LD		A,L
		LD		($0039),A
		LD		A,H
		LD		($003A),A
		LD		A,$C3	; JUMP..
		LD		($0038),A						; Set up SHOWIX as RST38 	
		
		LD		HL,MONITOR
		LD		A,$C3
		LD		($0018),A
		LD		A,L
		LD		($0019),A
		LD		A,H
		LD		($001A),A						; Set up monitor as RST 18
		
		
;		LD		C,Reset_Disk_System
;		CALL	BDOS							; Should build the table also 

;rst 28
		LD		E,$0B							; L is 11th character
;		LD		E,$00							; A:
		LD		C,Select_Disk		
		CALL	BDOS							; We start on the L drive... For Loki. Do this with the correct BDOS call later. Boot off L: 2K BDOS max + CCP from BDOS COLD and WARM BOOT. 
		CALL	INIT_MESSAGE
		

		
		LD		DE,$0080
		LD		C,Set_DMA_Address
		CALL	BDOS							; Set DMA to 8000
		
		ret

Init_message:
        CALL	CRLF
		LD		DE,Init_message_text			; We're entering for the first time. Initialise with a short message.
        LD		C,Print_String
        call    BDOS
		ret
Init_message_text:  db      'LokiOS - Open Spectrum Project 2022 - CC-BY-NC',$0D,$0A,'CP/M 2.2 Compatible',$0D,$0A,'$'
;Init_command1:	db			$FF,$08,'LINSTALL',$00
Init_command1:	db			$04,'LOKI',$00
Init_command2:	db			$07,'J:VIDEO',$00
Init_command3:	db			$09,'K:NETWORK',$00
Init_command4:	db			$06,'O:USER',$00
Init_command5:	db			$06,'P:USER',$00
Init_command6:	db			$0A,'A:AUTOBOOT',$00
Init_command7:	db			$0A,'C:AUTOBOOT',$00
Init_command8:	db			$0A,'N:AUTOBOOT',$00

SPECIAL_ERROR_V:	db		'Video card not installed',$0D,$0A,'$'
SPECIAL_ERROR_N:	db		'Network card not installed',$0D,$0A,'$'
SPECIAL_ERROR_U1:	db		'User Card 1 not installed',$0D,$0A,'$'
SPECIAL_ERROR_U2:	db		'User Card 2 not installed',$0D,$0A,'$'
SPECIAL_ERROR_D:	db		'No A:AUTOBOOT.COM',$0D,$0A,'$'
SPECIAL_ERROR_D1:	db		'No C:AUTOBOOT.COM',$0D,$0A,'$'
SPECIAL_ERROR_D2:	db		'No N:AUTOBOOT.COM',$0D,$0A,'$'

SCANPOS:	DW  $00					; Record where in the buffer we are
SCANCHARS:	DW	$00					; variable to store the number of characters remaining.



SCANWHITE:							;Scan input whitespace ( advance HL until we get a non-Whitespace character ).
									;Step through a buffer and remove any whitespace. 
									;We enter this with DE as the input buffer, and A as the number of characters 
									;( but that's also in INPUT_BUFFER+1 )

		PUSH	DE					; Transfer DE to HL. 
		POP		HL					; What got typed in, the buffer is now in HL. 
		INC		HL					; First byte no longer ignored - Now it's the count...
		
		LD		A,(HL)				; Retrieve the number of characters in the buffer, 
		AND		A					; Check A.
		JR		Z,SCANWHITE_ERROR	; If no characters, then the command line is blank. 
		
		LD		B,A					; Store the number of characters in B. ( They come back in A also ).
		INC		HL					; Get to the start of the buffer. 

SCANWHITE1:
		LD		A,(HL)				; Get rid of white space and exit if there's no characters pressed. 00=EOL also
		LD		(SCANPOS),HL			; Reuse BPOS for storing the space, because we will call this routine later.
		CP		' '
		JR		NZ,	SCANWHITE_FIRSTCHAR	; scan through whitespace. If it's not whitespace or $00 then continue.
		DEC		B					; We also terminate on buffer expiry ( if B is reduced to zero. )
		JR		Z, SCANWHITE_ERROR		;
		INC		HL					; Step HL. It will eventually either be a non-whitespace character or a zero as the buffer ends with 0. 
		JR		SCANWHITE1			; And loop until we are at a non-whitespace or control character. 
		
SCANWHITE_FIRSTCHAR:					; Jump to here when we get our first non-whitespace 		
		OR		A
		PUSH	AF
		LD		A,B
		LD		(SCANCHARS),A		; Save the remainin characters in buffer. 
		POP		AF
		RET							; We return with HL at the start of a character we can read.
									; HL = First buffer location with a character.
									; B=number of characters left in buffer. When zero, we ran out. 
	
SCANWHITE_ERROR:
		SCF							; Carry means we had an error - Either no characters, or all space, or no nonspace.
		RET
		
SCANWHITE_NEXT:						; If we're mid-line and we need to scan to the next point, come here.
		LD		HL,(SCANPOS)
		LD		A,(SCANCHARS)
		CP		$00
		JR		Z,SCANWHITE_ERROR	; Make sure there are still characters to scan. 
		LD		B,A					; Store the number of characters left in the buffer to scan through. 
SCANWHITE_NEXT1:
		LD		A,(HL)
		LD		(SCANPOS),HL		; Update where the current pointer is ( since we return here with it changed. )
		CP		' '
		JR		NZ,	SCANWHITE_FIRSTCHAR	; scan through whitespace. If it's not whitespace or $00 then continue.
		DEC		B					; We also terminate on buffer expiry ( if B is reduced to zero. )
		JR		Z, SCANWHITE_ERROR	; If we get to zero, and it's still a space, we'll treat it as an error.
		INC		HL					; Step HL. It will eventually either be a non-whitespace character or a zero as the buffer ends with 0. 
		JR		SCANWHITE_NEXT1		; And loop until we are at a non-whitespace or control character. 	

SCANCHAR:							; Just return whatever character is here. Upper case correction. 
		LD		A,(SCANCHARS)
		OR		A
		JR		Z,SCANWHITE_ERROR	; Nothing more to scan if we are at 0 characters.  Exit now if that's the case and set carry. 		
		
		LD		B,A
		LD		HL,(SCANPOS)
		LD		A,(HL)		
		AND		%11011111			; Mask bit 5 to compare only in one case... Since the comparison case is fixed and ALPHA only, this is reliable. 
		INC		HL
		LD		(SCANPOS),HL		; Move pointer. 

		DEC		B
		JR		SCANWHITE_FIRSTCHAR	; Exit here to store number of characters left. 

GETCHAR:							; Just return whatever character is here. NO Upper case correction. 
		LD		A,(SCANCHARS)
		OR		A
		JR		Z,SCANWHITE_ERROR	; Nothing more to scan if we are at 0 characters.  Exit now if that's the case and set carry. 
		LD		B,A					; Transfer to B to use later if necessary. 

		LD		HL,(SCANPOS)
		LD		A,(HL)
		INC		HL
		LD		(SCANPOS),HL		; Move pointer. 
		
		DEC		B
		JR		SCANWHITE_FIRSTCHAR	; Exit here to store number of characters left and return the characater in A. 

		
SCANGREY:							; Rem scan through the grey characters... Until we hit Whitespace again. 
		LD		HL,(SCANPOS)		; Again, pick up the "Grey" character from Scanpos ( We probably did SCANWHITE until we got here )
		LD		DE,FIRST_COMMAND+1	; When we run SCAN GREY, we are most likely scanning through a command on the command line. Let's copy it at the same time. \\
		LD		A,$00				; Count of characters in FIRST_COMMAND - Zero it first.
		LD		(FIRST_COMMAND),A	; Start with no characters in first command. 
		
		LD		A,(SCANCHARS)		; Of course, before we start, make sure there is something to scan, or drop out. 
		CP		$00
		JR		Z,SCANWHITE_ERROR	; Make sure there are still characters to scan. 
		LD		B,A					; Store the number of characters left in the buffer to scan through. 
SCANGREY_NEXT1:
		LD		A,(HL)
		LD		(SCANPOS),HL		; Update where the current pointer is ( since we return here with it changed. )
		CP		' '
		JR		Z,	SCANWHITE_FIRSTCHAR	; scan through whitespace. If it's not whitespace or $00 then continue. ( This is the opposite of the earlier routine ). 

		LD		(DE),A				; Store the character in First_Command at DE ( a 16 byte copy of the first "grey" characters, last character is always overwritten. 

		LD		A,(FIRST_COMMAND)
		CP		$0F					; No more than 14 characters in this buffer. Doesn't really need to be longer than 11, but let's be nice. 
		JR		Z,SCANGREY_NOSTORE	; Don't increase DE if we're past length, or update the counter. Just overwrite the last character. 	
		INC		A
		LD		(FIRST_COMMAND),A	; Store the number of characters in the buffer at the start of the buffer. 
		INC		DE					; One more character in DE.

SCANGREY_NOSTORE:					; We're done storing the first command part of this routine, continue with the "Grey" scanning. 	
		DEC		B					; We also terminate on buffer expiry ( if B is reduced to zero. )
		LD		A,B					; But first save the current counter in case we exit unexpectedly.
		LD		(SCANCHARS),A		; Since it might be a valid error with GREY (eg, End of command, no more characters ) then store the number of characters. 
		JR		Z, SCANWHITE_ERROR	; If we get B to zero, it's not actually an error here, but we've saved B so exit.
		INC		HL					; Step HL. It will eventually either be a whitespace character or a zero as the buffer ends with 0. 
		JR		SCANGREY_NEXT1		; And loop until we are at a whitespace or control character. 	



SHOW_STACK:	DW	$00					; Temp store of current stack ( should be return to where we were called from )\
SHOW_DE:	DW	$00
SHOW_BC:	DW	$00
SHOW_HL:	DW	$00
SHOW_AF:	DW	$00
SHOW_DE2:	DW	$00
SHOW_BC2:	DW	$00
SHOW_HL2:	DW	$00
SHOW_AF2:	DW	$00
SHOW_IX:	DW $00
SHOW_IY:	DW $00

SHOWREG:							; Rem Show the registers ( without screwing any up if possible ) - Working - Take to CCP. 
		LD		(SHOW_STACK),SP		; Store all the registers before we start. Makes it much easier later.
		PUSH	AF
		PUSH	DE
		PUSH	HL
		PUSH	BC
									; And takes the load off of the stack.
		LD		(SHOW_DE),DE
		LD		(SHOW_HL),HL
		LD		(SHOW_BC),BC
		PUSH	AF
		POP		HL
		LD		(SHOW_AF),HL
		
		LD	(SHOW_IX),IX
		LD	(SHOW_IY),IY

		EXX
		EX	AF,AF
		
		PUSH	AF
		PUSH	DE
		PUSH	HL
		PUSH	BC
		
		LD		(SHOW_DE2),DE
		LD		(SHOW_HL2),HL
		LD		(SHOW_BC2),BC
		PUSH	AF
		POP		HL
		LD		(SHOW_AF2),HL

		
		CALL	CRLF				; New line.

		LD		E,'P'				; PC (Where the call came from to show the registers )
		LD		C,Console_Output
		CALL	BDOS
;		LD		E,'C'
;		LD		C,Console_Output
;		CALL	BDOS
		LD		E,'-'
		LD		C,Console_Output
		CALL	BDOS
		LD		HL,(SHOW_STACK)
		INC		HL
		LD		A,(HL)
		CALL	PRINTHEX
		LD		HL,(SHOW_STACK)
		LD		A,(HL)
		CALL	PRINTHEX
		LD		E,' '
		LD		C,Console_Output
		CALL	BDOS	
		
		LD		E,'S'				; SP
		LD		C,Console_Output
		CALL	BDOS
;		LD		E,'P'
;		LD		C,Console_Output
;		CALL	BDOS
		LD		E,'-'
		LD		C,Console_Output
		CALL	BDOS
		LD		A,(SHOW_STACK+1)
		CALL	PRINTHEX
		LD		A,(SHOW_STACK)
		CALL	PRINTHEX
		LD		E,' '
		LD		C,2
		CALL	BDOS	



		LD		E,'H'				; HL
		LD		C,Console_Output
		CALL	BDOS
;		LD		E,'L'
;		LD		C,Console_Output
;		CALL	BDOS
		LD		E,'-'
		LD		C,Console_Output
		CALL	BDOS
		LD		A,(SHOW_HL+1)
		CALL	PRINTHEX
		LD		A,(SHOW_HL)
		CALL	PRINTHEX
		LD		E,' '
		LD		C,Console_Output
		CALL	BDOS		

		
									; AF
		LD		E,'A'
		LD		C,Console_Output
		CALL	BDOS
		LD		E,'F'
		LD		C,Console_Output
		CALL	BDOS
		LD		E,'-'
		LD		C,Console_Output
		CALL	BDOS
		LD		A,(SHOW_AF+1)
		CALL	PRINTHEX
		LD		A,(SHOW_AF)
		CALL	PRINTHEX
		LD		E,' '
		LD		C,2
		CALL	BDOS		
		
									; DE
		LD		E,'D'
		LD		C,Console_Output
		CALL	BDOS
;		LD		E,'E'
;		LD		C,Console_Output
;		CALL	BDOS
		LD		E,'-'
		LD		C,Console_Output
		CALL	BDOS
		LD		A,(SHOW_DE+1)
		CALL	PRINTHEX
		LD		A,(SHOW_DE)
		CALL	PRINTHEX
		LD		E,' '
		LD		C,Console_Output
		CALL	BDOS		

									; BC
		LD		E,'B'
		LD		C,Console_Output
		CALL	BDOS
;		LD		E,'C'
;		LD		C,Console_Output
;		CALL	BDOS
		LD		E,'-'
		LD		C,Console_Output
		CALL	BDOS
		LD		A,(SHOW_BC+1)
		CALL	PRINTHEX
		LD		A,(SHOW_BC)
		CALL	PRINTHEX
		LD		E,' '
		LD		C,Console_Output
		CALL	BDOS		
		
		LD		E,'H'				; HL'
		LD		C,Console_Output
		CALL	BDOS
;		LD		E,'L'
;		LD		C,Console_Output
;		CALL	BDOS
		LD		E,'`'
		LD		C,Console_Output
		CALL	BDOS
		LD		A,(SHOW_HL2+1)
		CALL	PRINTHEX
		LD		A,(SHOW_HL2)
		CALL	PRINTHEX
		LD		E,' '
		LD		C,Console_Output
		CALL	BDOS		

		
									; AF'
		LD		E,'A'
		LD		C,Console_Output
		CALL	BDOS
		LD		E,'F'
		LD		C,Console_Output
		CALL	BDOS
		LD		E,'`'
		LD		C,Console_Output
		CALL	BDOS
		LD		A,(SHOW_AF2+1)
		CALL	PRINTHEX
		LD		A,(SHOW_AF2)
		CALL	PRINTHEX
		LD		E,' '
		LD		C,Console_Output
		CALL	BDOS		
		
									; DE'
		LD		E,'D'
		LD		C,Console_Output
		CALL	BDOS
;		LD		E,'E'
;		LD		C,Console_Output
;		CALL	BDOS
		LD		E,'`'
		LD		C,Console_Output
		CALL	BDOS
		LD		A,(SHOW_DE2+1)
		CALL	PRINTHEX
		LD		A,(SHOW_DE2)
		CALL	PRINTHEX
		LD		E,' '
		LD		C,Console_Output
		CALL	BDOS		

									; BC'
		LD		E,'B'
		LD		C,Console_Output
		CALL	BDOS
;		LD		E,'C'
;		LD		C,Console_Output
;		CALL	BDOS
		LD		E,'`'
		LD		C,Console_Output
		CALL	BDOS
		LD		A,(SHOW_BC2+1)
		CALL	PRINTHEX
		LD		A,(SHOW_BC2)
		CALL	PRINTHEX
		LD		E,' '				; No final trailing space. ? Unless we add flags.
		LD		C,Console_Output
		CALL	BDOS		
	
	
										; BC'
		LD		E,'X'
		LD		C,Console_Output
		CALL	BDOS
		LD		E,'`'
		LD		C,Console_Output
		CALL	BDOS
		LD		A,(SHOW_IX+1)
		CALL	PRINTHEX
		LD		A,(SHOW_IX)
		CALL	PRINTHEX
		LD		E,' '				; No final trailing space. ? Unless we add flags.
		LD		C,Console_Output
		CALL	BDOS	
		
									; BC'
		LD		E,'Y'
		LD		C,Console_Output
		CALL	BDOS
		LD		E,'`'
		LD		C,Console_Output
		CALL	BDOS
		LD		A,(SHOW_IY+1)
		CALL	PRINTHEX
		LD		A,(SHOW_IY)
		CALL	PRINTHEX
		LD		E,' '				; No final trailing space. ? Unless we add flags.
		LD		C,Console_Output
		CALL	BDOS	

		
	
									; Flags
									;Bit      7 6 5 4 3 2   1 0
									;Position S Z X H X P/V N C		
		LD		HL,(SHOW_AF)		; Positive or Minus
		PUSH	HL
		POP		AF
		JP		P,SHOWREG_POSITIVE
		LD		E,'M'
		LD		C,Console_Output
		CALL	BDOS
		JR		SHOWREG_NEGATIVE
SHOWREG_POSITIVE:
		LD		E,'P'
		LD		C,Console_Output
		CALL	BDOS
SHOWREG_NEGATIVE:
		LD		E,' '
		LD		C,Console_Output
		CALL	BDOS

		
		LD		HL,(SHOW_AF)		; Zero
		PUSH	HL
		POP		AF
		JR		Z,SHOWREG_ZERO
		LD		E,'N'
		LD		C,Console_Output
		CALL	BDOS
SHOWREG_ZERO:
		LD		E,'Z'
		LD		C,Console_Output
		CALL	BDOS
		LD		E,' '
		LD		C,Console_Output
		CALL	BDOS
		
		
		LD		A,(SHOW_AF)			; Flags in A. Test Half Carry.
		BIT		4,A
		JR		Z,SHOWREG_NOTHALF
		LD		E,'H'
		LD		C,Console_Output
		CALL	BDOS
		LD		E,' '
		LD		C,Console_Output
		CALL 	BDOS
SHOWREG_NOTHALF:							; Display nothing if nothalf.
		

		LD		E,'P'
		LD		C,Console_Output
		CALL	BDOS
		LD		HL,(SHOW_AF)		; PO/PE	
		PUSH	HL
		POP		AF
		JP		PE,SHOWREG_PE
		LD		E,'O'
		LD		C,Console_Output
		CALL	BDOS
		JR		SHOWREG_PO
SHOWREG_PE:
		LD		E,'E'
		LD		C,Console_Output
		CALL	BDOS
SHOWREG_PO:
		LD		E,' '
		LD		C,Console_Output
		CALL	BDOS
			
		LD		E,'N'
		LD		C,Console_Output
		CALL	BDOS
		LD		A,(SHOW_AF)			; Flags in A. Test N
		BIT		1,A
		JR		Z,SHOWREG_N
		LD		E,'1'
		LD		C,Console_Output
		CALL	BDOS
		JR		SHOWREG_NOTN
SHOWREG_N:
		LD		E,'0'
		LD		C,Console_Output
		CALL 	BDOS
SHOWREG_NOTN:							; Display nothing if nothalf.
		LD		E,' '
		LD		C,Console_Output
		CALL 	BDOS

		LD		HL,(SHOW_AF)		; CARRY
		PUSH	HL
		POP		AF
		JR		C,SHOWREG_CARRY
		LD		E,'N'
		LD		C,Console_Output
		CALL	BDOS
SHOWREG_CARRY:
		LD		E,'C'
		LD		C,Console_Output
		CALL	BDOS
		LD		E,' '
		LD		C,2
		CALL	BDOS

		CALL	CRLF				; New line after also .
		

SHOWREG_END:
		POP		BC
		POP		HL
		POP		DE
		POP		AF
		
		EXX
		EX	AF,AF
		
		POP		BC
		POP		HL
		POP		DE
		POP		AF
		
		ret
		
SHOWMHL:	DW	$00
		
SHOWIX:								; Rem Show the registers ( without screwing any up if possible ) - Working - Take to CCP. 
		LD		(SHOW_STACK),SP		; IX/IY/(HL) only.
		PUSH	AF
		PUSH	DE
		PUSH	HL
		PUSH	BC

		EXX
		EX	AF,AF
		
		PUSH	AF
		PUSH	DE
		PUSH	HL
		PUSH	BC
	
		
		EXX
		
		LD		E,(HL)
		INC		HL
		LD		D,(HL)
		LD		A,(DE)
		LD		(SHOWMHL),A
			

		CALL	CRLF				; New line.
		
		
		LD		E,'('				; (HL) = M
		LD		C,Console_Output
		CALL	BDOS
		LD		E,'H'
		LD		C,Console_Output
		CALL	BDOS
		LD		E,'L'				; (HL) = M
		LD		C,Console_Output
		CALL	BDOS
		LD		E,')'
		LD		C,Console_Output
		CALL	BDOS
		LD		E,'='
		LD		C,Console_Output
		CALL	BDOS
		LD		A,(SHOWMHL)
		CALL	PRINTHEX
		LD		E,' '
		LD		C,Console_Output
		CALL	BDOS	


		EXX


		PUSH	IX
		POP		HL
		LD		(SHOW_HL),HL
		

		LD		E,'I'				; HL
		LD		C,Console_Output
		CALL	BDOS
		LD		E,'X'
		LD		C,Console_Output
		CALL	BDOS
		LD		E,'-'
		LD		C,Console_Output
		CALL	BDOS
		LD		A,(SHOW_HL+1)
		CALL	PRINTHEX
		LD		A,(SHOW_HL)
		CALL	PRINTHEX
		LD		E,' '
		LD		C,Console_Output
		CALL	BDOS	

		PUSH	IY
		POP		HL
		LD		(SHOW_HL),HL
		
		LD		E,'I'				; HL
		LD		C,Console_Output
		CALL	BDOS
		LD		E,'Y'
		LD		C,Console_Output
		CALL	BDOS
		LD		E,'-'
		LD		C,Console_Output
		CALL	BDOS
		LD		A,(SHOW_HL+1)
		CALL	PRINTHEX
		LD		A,(SHOW_HL)
		CALL	PRINTHEX
		LD		E,' '
		LD		C,Console_Output
		CALL	BDOS	
		
		

		POP		BC
		POP		HL
		POP		DE
		POP		AF
		
		EXX
		EX	AF,AF
		
		POP		BC
		POP		HL
		POP		DE
		POP		AF

		ret

KEYBOARD_BUFFER:	DS	$10					; Store this outside of the main program, since we don't want to corrupt anything below the CCP.
											; Leave up to 16 characters. 

MONITOR:							; Rem "Enter Monitor Mode."

;;		CALL	SHOWREG				; SHow the current register states.
;;out ($10),A	; turn off debug. 

		LD		DE,MONITOR_PROMPT	
		LD		C,PRINT_STRING
		CALL	BDOS				; Show the "Monitor:" prompt. 
		
		LD		DE,KEYBOARD_BUFFER	; Let's use this space to record things for the moment.
		LD		A,10
		LD		(KEYBOARD_BUFFER),A				; 10 characters in first byte of buffer (size)
		LD		C,Read_Console_String					; Function 10, read in a string.
		CALL	BDOS					; Get the data.

		LD		DE,KEYBOARD_BUFFER	; OK, let's get DE pointed correctly again.
		
		CALL	SCANWHITE			; Get the console input and remove any whitespace, so we're at the command.		
		JR		C,MONITOR_ERROR		; If carry is set, we got an error.
	
		CALL	SCANCHAR			; Just get the next char. Do it through this, because we want to decrement line counter
		JR		C,MONITOR_ERROR

		;QUAD_HEXTODEC
		;DECFROMHEX
		CP		'Q'
		RET		Z
		CP		'P'
		JP		Z,MONITOR_DISK
		CP		'T'
		JP		Z,MONITOR_TRACK
		CP		'R'
		JP		Z,MONITOR_RECORD
		CP		'M'
		JP		Z,MONITOR_DMA
		CP		'L'
		JP		Z,MONITOR_READ
		CP		'G'
		JP		Z,MONITOR_EXECUTE
		CP		'D'
		JR		NZ, MONITOR1
		
		CALL	SCANWHITE_NEXT		; Get next useable character. - If it errors, treat that as just "D" by itself.
		JR		C,MONITOR_NODUMPLOC	; Treat this as a default Dumploc, will usually be 0000. Can also be an error (alternative)
		CALL	QUAD_HEXTODEC		; Convert to number
		JR		C,MONITOR_ERROR2
		
MONITOR_NODUMPLOC:
		CALL 	DUMPPAGE			; Dump the page specified in Hex.
		JR		MONITOR

MONITOR1:
		CP		'S'
		JR		NZ, MONITOR2
		
		JP		MONITOR_SHOWSTACK
MONITOR2:
		JR		MONITOR_ERROR

MONITOR_ERROR:
		LD		DE,MONITOR_ERRORM
		LD		C,Print_String
		CALL	BDOS
		JP		MONITOR				; Show the error, and repeat.
		

MONITOR_ERROR2:
		LD		DE,MONITOR_ERRORM1
		LD		C,Print_String
		CALL	BDOS
		JR		MONITOR_ERROR		; Show the other error line too. Comment out otherwise. 
		JP		MONITOR				; Show the error, and repeat.
		
MONITOR_SHOWSTACK:
		LD		DE,MONITOR_STACK
		LD		C,Print_String
		CALL	BDOS
		LD		(DECFROMHEX),SP		; Store SP to display.
		LD		IX,DECFROMHEX		; Pick up the memory location.
		LD		A,(IX+1)
		CALL	PRINTHEX			; And print it out. The page.

		LD		A,(IX+0)
		CALL	PRINTHEX			; Print the current address.
		JP		MONITOR				; Show the error, and repeat.

MONITOR_DISKVAL:	DW	$00
MONITOR_TRACKVAL:	DW  $00
MONITOR_RECORDVAL:	DW	$00
MONITOR_DMAVAL:		DW	$0080		; Set initially to 0080 just in case of "oops" stuff. 

MONITOR_DISK:
		CALL	SCANWHITE_NEXT		; Get next useable character. - If it errors, treat that as just "D" by itself.
		JR		C,MONITOR_ERROR2	; Treat this as a default Dumploc, will usually be 0000. Can also be an error (alternative)
		CALL	DOUBLE_HEXTODEC		; Convert to number
		JR		C,MONITOR_ERROR2	;
		LD		A,(DECFROMHEX)		; We got a valid word. Just get the lower byte.
		LD		(MONITOR_DISKVAL),A	; Store it.
		JP		MONITOR
		
MONITOR_TRACK:
		CALL	SCANWHITE_NEXT		; Get next useable character. - If it errors, treat that as just "D" by itself.
		JR		C,MONITOR_ERROR2	; Treat this as a default Dumploc, will usually be 0000. Can also be an error (alternative)
		CALL	DOUBLE_HEXTODEC		; Convert to number
		JR		C,MONITOR_ERROR2	;
		LD		A,(DECFROMHEX)		; We got a valid word. Just get the lower byte.
		LD		(MONITOR_TRACKVAL),A	; Store it.
		JP		MONITOR
MONITOR_RECORD:
		CALL	SCANWHITE_NEXT		; Get next useable character. - If it errors, treat that as just "D" by itself.
		JR		C,MONITOR_ERROR2	; Treat this as a default Dumploc, will usually be 0000. Can also be an error (alternative)
		CALL	DOUBLE_HEXTODEC		; Convert to number
		JR		C,MONITOR_ERROR2	;
		LD		A,(DECFROMHEX)		; We got a valid word. Just get the lower byte.
		LD		(MONITOR_RECORDVAL),A	; Store it.
		JP		MONITOR
MONITOR_DMA:
		CALL	SCANWHITE_NEXT		; Get next useable character. - If it errors, treat that as just "D" by itself.
		JR		C,MONITOR_ERROR2	; Treat this as a default Dumploc, will usually be 0000. Can also be an error (alternative)
		CALL	QUAD_HEXTODEC		; Convert to number
		JR		C,MONITOR_ERROR2	;
		LD		BC,(DECFROMHEX)		; We got a valid word. Just get the lower byte.
		LD		(MONITOR_DMAVAL),BC	; Store it.
		JP		MONITOR
		
MONITOR_EXECUTE:					; GO to address.
		CALL	SCANWHITE_NEXT		; Get next useable character. - If it errors, treat that as just "D" by itself.
		JP		C,MONITOR_ERROR2	; Treat this as a default Dumploc, will usually be 0000. Can also be an error (alternative)
		CALL	QUAD_HEXTODEC		; Convert to number
		JP		C,MONITOR_ERROR2	;
		LD		HL,MONITOR
		PUSH	HL					; Store monitor as the return point. 
		LD		HL,(DECFROMHEX)		; We got a valid word. Get the full word. 
		JP		(HL)
MONITOR_READ:	
;		LD		BC,(MONITOR_DISKVAL)
;		CALL	SELDSK	; Note - this is the BIOS. 

		LD		A,(MONITOR_DISKVAL)
		LD		E,A
		LD		C,Select_Disk
		CALL	BDOS				; Select the new current disk drive. 

		LD		BC,(MONITOR_TRACKVAL)
		CALL	SETTRK
		LD		BC,(MONITOR_RECORDVAL)
		CALL	SETSEC
		LD		BC,(MONITOR_DMAVAL)
		CALL	SETDMA
		CALL	READ
		JP		MONITOR				; And continue.

MONITOR_PROMPT:	DB $0A,$0D,'MONITOR:$'
MONITOR_ERRORM: DB $0A,$0D,'Use D NNNN (dump NNNN page), E NNNN XX (set NNNN to XX), S=Stack or Q=Quit'
MONITOR_ERRORM_:DB $0A,$0D,'P NN=DISK, T NN=Track, R NN=Record, M NNNN=DMA, L=Read Record' 
MONITOR_ERRORMA:DB $0A,$0D,'G NNNN=GotoNNNN$ I(IO-INPUT PORT VALUE)NNNNXX - O(IO-OUTPUT)NNNN XX ' 
MONITOR_ERRORM1:DB $0A,$0D,'Hex value error.$'
MONITOR_STACK:	DB $0A,$0D,'Stack:$'

DUMPTEMP:	DW	$0000				; Temporary variable store. Position in dump.
DUMPPAGE:
		CALL	DUMPRECORD			; So we do it twice. 

DUMPRECORD:							; Based on the current input buffer, read a HEX4 number and dump the page. Called from Monitor
		PUSH	HL
		POP		IX
		
		LD		B,$08				; Number of vertical lines ( $10 = 16 lines, = full page. )
		
DUMPPAGE_MULTI:						; Return point for multiple lines.		

		PUSH	BC					; Store vertical line counter.
		
		LD		IX,DECFROMHEX		; Pick up the memory location.
		LD		A,(IX+1)
		CALL	PRINTHEX			; And print it out. The page.

		LD		A,(IX+0)
		CALL	PRINTHEX			; Print the current address.

		LD		E,' '				; Print a space.
		LD		C,Console_Output
		CALL	BDOS

		LD		HL,(DECFROMHEX)		; Location of memory buffer.
		LD		(DUMPTEMP),HL
		LD		B,16					;
DUMPPAGE_LINE:
		LD		A,(HL)
		PUSH	BC
		PUSH	HL
		CALL	PRINTHEX			; Print out the memory locations. 
		
		LD		E,' '				; Print a space,
		LD		C,Console_Output
		CALL	BDOS				
		
		POP		HL
		POP		BC
		INC		HL
		DJNZ	DUMPPAGE_LINE
		LD		(DECFROMHEX),HL		; Store the new location for multiline dumps
		
		
		LD		HL,(DUMPTEMP)
		LD		B,16					;
DUMPPAGE_LINE2:
		LD		A,(HL)
		PUSH	BC
		PUSH	HL
		
		CP		' '
		JR		NC,		DUMPPAGE_PRINTABLE
		LD		A,'+'				; Instead of control codes, print a plus. 
DUMPPAGE_PRINTABLE:		
		LD		E,A
		LD		C,Console_Output
		CALL	BDOS				
		
		POP		HL
		POP		BC
		INC		HL
		DJNZ	DUMPPAGE_LINE2
		
		CALL	CRLF				; Print a new line. 

		POP		BC
		DJNZ	DUMPPAGE_MULTI


		RET

PRINTHEXCHAR:	DB $00					; Temp place to store the char.
PRINTHEX:							; Print a HEX number in A

		LD		(PRINTHEXCHAR),A
		LD		HL,PRINTHEXCHAR
		
		RLD
		AND		$0F
		OR		$30
		CP		$3A
		JR		C,		PRINTHEX1
		ADD		A,$07
PRINTHEX1:
		LD		E,A
		LD		C,Console_Output
		CALL	BDOS
		LD		HL,PRINTHEXCHAR		
		RLD
		AND		$0F
		OR		$30
		CP		$3A
		JR		C,		PRINTHEX2
		ADD		A,$07
PRINTHEX2:
		LD		E,A
		LD		C,Console_Output
		CALL	BDOS
		
		RET



		

HEXTODEC:							; Convert a hex number in A ( AS ASCII ) to a 4 bit decimal number.

		CP		'0'					; Check it's a HEX character
		JR	C,	HEXTODEC_ERROR		; Not hex, set error.
		CP		'9'+1
		JR	C,	HEXTODEC_STRIP		; If between 0 and 9, then we got the right bits in the lower nibble.
		AND		%11011111			; Mask bit 5 to compare only in one case... Since the comparison case is fixed and ALPHA only, this is reliable. 
		CP		'A'
		JR	C,	HEXTODEC_ERROR		; Not hex, set error.
		CP		'F'+1
		JR	C,	HEXTODEC_STRIP2		; Not hex, set error.
		JR	HEXTODEC_ERROR			; Number higher than "F"
HEXTODEC_STRIP2:
		ADD		A,$09				; Add 9.
HEXTODEC_STRIP:
		AND		$0F
		OR		A							; Clear carry
		RET
HEXTODEC_ERROR:
		SCF							; Set carry = ERROR - Conversion failed. 
		RET
		
		
DECFROMHEX:	DW	$00					; After a hex conversion, will have either a byte or a word in decimal from hex.		

		
DOUBLE_HEXTODEC:					; Convert Hex held in two locations at (HL) to decimal and place in A.
		PUSH	HL
		POP		IX					; Transfer location to IX. 
		LD		HL,DECFROMHEX		; Where we build the number. 
		LD		A,(IX+0)
		CALL	HEXTODEC
		JR		C,DOUBLE_HEXTODEC_FAIL
		RLD
		LD		A,(IX+1)
		CALL	HEXTODEC
		JR		C,DOUBLE_HEXTODEC_FAIL
		RLD
		OR		A
		RET
DOUBLE_HEXTODEC_FAIL:		
		RET							; Carry flag = failed conversion.

QUAD_HEXTODEC:					; Convert Hex held in four locations at (HL) to decimal and place in A.
		PUSH	HL
		POP		IX					; Transfer location to IX. 
		LD		HL,DECFROMHEX+1		; Where we build the number. 
		LD		A,(IX+0)
		CALL	HEXTODEC
		JR		C,QUAD_HEXTODEC_FAIL
		RLD
		LD		A,(IX+1)
		CALL	HEXTODEC
		JR		C,QUAD_HEXTODEC_FAIL
		RLD
		DEC		HL					; Now get the lower order bytes, and store in the lower part of the word. 
		LD		A,(IX+2)
		CALL	HEXTODEC
		JR		C,QUAD_HEXTODEC_FAIL
		RLD
		LD		A,(IX+3)
		CALL	HEXTODEC
		JR		C,QUAD_HEXTODEC_FAIL
		RLD
		OR		A
		RET
QUAD_HEXTODEC_FAIL:		
		RET							; Carry flag = failed conversion.
		

		

Showprompt:			
		CALL	CRLF
;		LD		A,(DRIVE_SELECTED)
		LD		C,Return_Current_Disk
		CALL	BDOS				; Get the current logged drive. 
		ADD		A,$41				; Make "0" and "A"
		LD		C,Console_Output
		LD		E,A	
		CALL	BDOS				; Write the drive selected.
		LD		E,'>'
		LD		C,Console_Output
		CALL	BDOS
		ret
		
		
CRLF:	LD		E,$0D
		LD		C,Console_Output
		CALL	BDOS
		LD		E,$0A
		LD		C,Console_Output
		CALL	BDOS
		ret
		

TWOSPACE:	LD		E,$20			; Print two spaces. 
		LD		C,Console_Output
		CALL	BDOS
		LD		E,$20
		LD		C,Console_Output
		CALL	BDOS
		ret

ONELINE:LD		E,' '
		LD		C,Console_Output
		CALL	BDOS
		LD		E,'|'
		LD		C,Console_Output
		CALL	BDOS
		LD		E,' '
		LD		C,Console_Output
		CALL	BDOS
		ret

DB		'THIS IS THE FIRST COMMAND HERE ----->' ; Just so we can find it in memory dumps. 

;FIRST_COMMAND:	DS	$11					; 16 character buffer to store the executive on the command line ( the command or file to execute ). No spaces. 
										; First byte is number of characters. Second is command itself. 
										; When we run POPULATEFCB it will place the first command in here, up to 16 characters. Over that, is not a command
										; If the first byte is $00, we should ignore it. 
										; Note: NEEDS TO BE $11 BYTES, since the first is the length.. Otherwise it will overwrite 1 character of code if long. 
FIRST_COMMAND:	db	4,'LOKI',0,0,0,0,0,0,0,0,0,0,0,0,0	; Set up to automatically execute the LOKI install command. 
;FIRST_COMMAND:	db	4,'DIR',0,0,0,0,0,0,0,0,0,0,0,0,0,0	; Set up to automatically execute the LOKI install command. 
	
COMMAND:							; Display a command prompt, get input, call appropriate function or load .COM file.
									; This is the MAIN routine of the CCP code. 
;	call monitor If I need to see what last happened, uncomment to run monitor before CLI. 
		CALL	LOGGED_DRIVE		; Display the A> prompt. 

		LD		A,COMMAND_LENGTH	; 64 characters input. I can change this later. 
		LD		DE,COMMAND_BUFFER		; Doesn't have to be here, but right now, I'll use the buffer at $0080. (Command_Buffer is usually $0080 )
		LD		(DE),A
		LD		C,Read_Console_string
		CALL	BDOS				; Call BDOS to Input the command line. 

COMMAND_AUTO:						; Call this when I need to transition an automatic command - Load the command line in at COMMAND_BUFFER. Zero terminate.
									; Note: Make sure we start at 0081 for the character count, and 0082 for the command. 
									; If using this automatically, limit size to 64 characters... Like the command line. 
		LD		DE,COMMAND_BUFFER+1	; Set the start of the buffer - Before we do anything, check if we got one back. ****ADJUSTED**** to +1, since the count is at 0081


COMMAND_MANUAL:
		LD		A,(DE)				; Get the input buffer length of input string. 
		AND		A					; Check A for being zero ( no characters )
		JR		Z,COMMAND_BLANK		; If no characters, then the command line is blank. 

;		LD		(BUFFER_LENGTH),A	; Store the buffer length in case we need it again. 
; Here I process a command.

		DEC		DE					; Back to 0080 or the Command Buffer = First Char=wanted size, Second char= size, third=input. 
		CALL	POPULATEFCB			; Populate the default FCBs. These are at $5C and $5D. Also fills in the command at First_Command buffer with the executive command. 
					
		LD		IX,COMMANDS			; Where we have the command table. 
		LD		C,$00				; We're on command 0.
		
COMMAND_SCAN_LOOP:								; Loop back here each time we try to match the command. 
		LD		HL,FIRST_COMMAND+1				; The start location for what got typed in. 
		LD		A,(FIRST_COMMAND)				; Number of characters.
		LD		B,A					; Store the number of characters in the command executive in B. So we don't match against missing characters. 

		OR		A					; Check number of characters stored in command at FIRST_COMMAND
		JR		Z,COMMAND_BLANK		; If the command line is blank, catch it here. 
		LD		A,(HL)				; Store first character of incoming command buffer and prepare to compare. 
									; Otherwise continue on. 
COMMAND2:							; We found valid characters, now see if they form a command. 
		AND		%11011111			; Convert to upper case and then;
		OR		%10000000           ; First check against end of command character string ( $80 + last character )
		CP		(IX+0)				; Test against the table strings.
		JR		Z,COMMAND_FOUND		; This means we got a matching character string in the command line and it's the final character = match.

		AND		%01111111			; Mask bit 5 to compare only in one case... Since the comparison case is fixed and ALPHA only, this is reliable. 
		CP		(IX+0)				; Compare to IX+0
		JR		NZ,COMMAND_NOTFOUND	; If it's not a match, continue past.

		INC		HL					; Otherwise increment both pointers...
		INC		IX					; I said 'both'
		LD		A,(HL)				; Get the next character to compare.
		DEC		B					; And make sure we have characters to spare. 
		JR		NZ,COMMAND2			; Otherwise keep scanning until we match or fail to match.
		
		JR		COMMAND_NOTFOUND	; If we get to the end of the buffer, we're done with this matching. Don't match any more characters ( or previous buffer
	 								; ... entries may form a part of the command... )

COMMAND_BLANK:
		
		LD		DE,COMMAND_HINT
		JP		ERROR_MESSAGE		; Exit via an error when encountering nothing but whitespace... Exit here. 

		
COMMAND_NOTFOUND:
		INC		C					; Look for command 2 now. And so on until we check all commands in table. 
		PUSH	DE
		POP		HL					; Reset the HL pointer to what got typed in. 
;		LD		A,(FIRST_COMMAND)	; i LOAD THIS AT SCAN LOOP NOW 
		LD		B,A					; Restore the buffer length counter. 
COMMAND_NOTFOUND1:					; Now we have to move IX to the end of the command we were matching and into the next command in the table. 
		INC		IX					; Last character failed to match.... Get next character in match table 
		LD		A,(IX+0)			; ...and load into A.
		
									; Check if we're at the end of the command list table, and get past next whitespace. 
		CP		$00					; if the table sees a zero, then we're at the end of the table.
		JR		Z,COMMAND_UNKNOWN	; ** Exit via Command Unknown after we've checked all possibilities. 
									; ** Relative jump too far. 

		LD		A,(IX-1)			; we will keep stepping through until we're at the first character after a character with Bit 8 set, which is the next command in the table 
		AND		%10000000
		AND		A					; And check if we reached the whitespace at the end of each check?
		JR		Z,	COMMAND_NOTFOUND1	; Loop here until we get to the whitespace. 

		JR		COMMAND_SCAN_LOOP			; And scan for the next command from the start of the command buffer again. 
		
COMMAND_FOUND:						; We got one - Let's just notify the user at the moment. Will change for a jump table later. 
									; First check we didn't get a partial match by mistake - eg, DIRT matches DIR
		DEC		B
		JR		Z,	COMMAND_LEGIT		; if B would have been the end anyway, then it's legitimately the end. We used up all the characters.
		INC		HL
		LD		A,(HL)
		CP		' '					
		JR		Z,	COMMAND_LEGIT		; If the next character was a space, that's OK too.
		CP		$09			
		JR		Z,COMMAND_LEGIT		; TAB is a legit character too... All whitespace is.
		JR		COMMAND_NOTFOUND	; Otherwise there was more to the match and it was a false positive - Correct it here. 

COMMAND_LEGIT:						; Command confirmed legitimate. 
		CALL	CCPVECTOR			; Execute the correct handler for the command. C holds the numerical number of the command. 
		ret		



		
;BUFFER_LENGTH:	DB	$00				; Characters in the buffer, 

TEMP_LOGGED_DRIVE:	DB	$FF			; Temporary logged drive. 
SPECIAL_ERROR:		DB	$00			; If 00 then just the normal error. 
SPECIAL_ERROR_STRING: DW $0000		; Location of special error string. 

COMMAND_UNKNOWN:
		CALL	CHANGE_LOGGED_DRIVE	; Is the command line a drive change?
		RET		C
		CALL	EXECUTE				; Is the command line something we can execute?
		RET		C

LD		A,(SPECIAL_ERROR)
OR		A
JR		Z,NORMAL_ERROR
LD		DE,(SPECIAL_ERROR_STRING)
JR		CUSTOM_ERROR


NORMAL_ERROR:
		LD		DE,COMMAND_BAD
CUSTOM_ERROR:
		LD		C,Print_String
		CALL	BDOS
		RET
		
CHANGE_LOGGED_DRIVE:
		LD		A,$FF					; FF = Default. 0 = A, 1 = B etc.  
		LD		(TEMP_LOGGED_DRIVE),A	; Set temporary logged drive for transient command as default
									; Start performing tests - to see if syntax is correct.
		LD		A,(FIRST_COMMAND+2)	; Is there a COLON?
		CP		':'
		JR		NZ,CHANGE_LOGGED_EXIT					; return if there's no colon. C is not set. 
		LD		A,(FIRST_COMMAND+1)
		AND		%01011111			; Convert to lower case.
		CP		'A'					; Check bounds - is it in the range A to P ?
		JR		C,CHANGE_LOGGED_EXIT
		CP		'Q'
		JR		NC,CHANGE_LOGGED_EXIT
		DEC		A
		AND		$0F
		LD		(TEMP_LOGGED_DRIVE),A	; Store it. 
		
		LD		A,(FIRST_COMMAND)
		CP		$02					; 2 bytes? Might be a drive change.
		JR		NZ,CHANGE_LOGGED_EXIT					; return if it's not 2 bytes. C is not set.
		
		LD		A,(TEMP_LOGGED_DRIVE)
		LD		E,A
		LD		C,Select_Disk
		CALL	BDOS				; Select the new current disk drive. 

		SCF					; set Carry Flag. 
		ret
CHANGE_LOGGED_EXIT:
		OR		A			; Clear carry flag
		ret

ERROR_MESSAGE:
		LD		C,Print_String
		CALL	BDOS
		RET
	
COMMAND_BAD: db 'Command Unknown or Executable not found',$0D,$0A,'$'	
		
FILENAME_BAD:	db 'Filename had unexpected charactors or was malformed',$0D,$0A,'$'

COMMAND_HINT:	db 'Type HELP for a list of commands',$0D,$0A,'$'	

FILEOPEN_ERROR:	db	'File could not be opened',$0D,$0A,'$'

FILEREAD_ERROR:	db	'Error reading file',$0D,$0A,'$'

NODEST_ERROR:	db	'Bad Source or Destination',$0D,$0A,'$'	

FILE_COPY_ERROR:db	'Error copying file',$0D,$0A,'$'

COMMLIST:	; What commands to we support? Let's create a simple table.
DW		$0000						; First two bytes store the location int he table
COMMSEL:	; Which command was selected?
DB		$00
; DIR ERA REN SAVE TYPE USER
; Add HALT and COPY (instead of PIP)
;


COMMANDS:		; Show with single "Whitespace" at the end That way we can match exact until Whitespace and only costs a space per character. 			
DB		'DIR'+$80			;0
DB		'ERA'+$80			;1
DB		'REN'+$80			;2
DB		'SAVE'+$80			;3
DB		'TYPE'+$80			;4
DB		'USER'+$80			;5
DB		'COPY'+$80			;6  
DB		'MONITOR'+$80		;7 	
DB		'HALT'+$80			;8 
DB		'HELP'+$80			;9
DB		'DUMP'+$80			;A
DB		'SUBMIT'+$80		;B
DB		$00					; 	$00 By itself means we've reached the end of the table.
; Future
;With the exception possibly of Monitor, I need to consider if I make any more commands override disk 
;name commands, since that will inhibit disk use. Maybe add a "L" or allow direct disk = eg, "A:FORMAT" etc.
;DB		'COPY '			; Same as DOS copy.
;DB		'FORMAT '		; Basic FORMAT command. 
;DB		'HELP '			; Help Menu ( in CCP ! )
;DB		'EDIT '			; 
;DB 		'LOKI '			; Setup initial Loki Configuration ( eg, Memory, Drives, etc. )

CCPTABLE:							; Creates a table with the jumpvectors for each routine. 
		DW	DIR				;0
		DW	ERA				;1
		DW	REN				;2
		DW	SAVE			;3
		DW	TYPE			;4
		DW	USER			;5
		DW	COPY			;6
		DW	MONITOR			;7
		DW	HALT_HERE		;8
		DW	HELP			;9
		DW	DUMP			;A
		DW	SUBMIT			;B

CCPJUMP:							; CCP Calls enter here. 
;EQU		MAXCALL ,	$08				; Matches number of Commands. Commands can be located anywhere in asm.							;
									; rem Enter this location with as as the call code.							
CCPVECTOR:							; Check C here to avoid calling too big of a table. C holds the vector.
		LD		A,C					; C has our position in the table. Transfer it to A. 
		PUSH	DE					; Store the instruction for later.
		LD		HL,CCPTABLE
		RL		A					; Double it.
		AND		$FE					; And mask any carry. We have a maximum of 128 options.
		ADD		A,L
		LD		L,A
		LD		A,$00				; Clear A.
		ADC		A,H
		LD		H,A
		LD		E,(hl)				; Move the jump vector to DE
		INC		HL
		LD		D,(hl)
		PUSH	DE					; Push DE vector onto the stack so we can use RET to get there. 
		POP		HL					; HL now contains the jump vector.
		POP		DE					; Restore the command information.
		JP		(HL)				; And jump to the location specified by DE. 
UNKNOWNCODE:
		RET							; Just exit here. 


DRIVE_SPECIFIER:					; Test if there's a drive specifier - eg, the A: in A:FILENAME.TXT
									; Return a Drive specifier in A and Update SCANPOS if there's a specifier. 
									; A:=01, B:=02 etc. If no specifier, then return Default:=00 and don't move SCANPOS.
									; Use Scanwhite, ScanGrey, etc. Uses same variable locations to locate file details. 
		LD		A,(SCANCHARS)
		OR		A
		JR		Z,DRIVE_NOTSPECIFIED	; Nothing more to scan if we are at 0 characters.  Return A as 00 for default. Don't move HL. Don't change SCANCHARS or SCANPOS
		DEC		A
		JR		Z,DRIVE_NOTSPECIFIED	; Nothing more to scan again if not 2 characters. Return A as 00 for default. Don't move HL. Don't change SCANCHARS or SCANPOS
	
		LD		HL,(SCANPOS)
		INC		HL
		LD		A,(HL)					; Pick up drive specifier delimiter. 
		CP		':'						; Drive specifier delimiter character.
		JR		NZ,DRIVE_NOTSPECIFIED	; Nothing to do if there's no : after the current character.

DRIVE_SPECIFIED:						; Label is just a placeholder. We got a drive specifier. 
		DEC		HL						; Back a character First check if it's a valid drive specifier ( eg, A: to P: )
		LD		A,(HL)
		AND		%11011111				; Mask to change upper to lower case. 
		CP		'A'
		JR		C,DRIVE_NOTSPECIFIED	; Lower than 'A' - Not a drive specifier.
		CP		'Q'
		JR		NC,DRIVE_NOTSPECIFIED	; Higher than 'P' - Not a drive specifier.
		DEC		A
		AND		%00001111				; Mask for 16 drive numbers.
		INC		A						; A 
		PUSH	AF						; Store drive number.			
		LD		A,(SCANCHARS)			; Update both Scanchars and Scanpos variables. 
		DEC		A
		DEC		A
		LD		(SCANCHARS),A			; reduce number of characters remaining by 2. ( eg, A: is two characters )
		INC		HL
		INC		HL						; Step past drive and colon.
		LD		(SCANPOS),HL			; Save new position. 
		POP		AF						; Restore drive number. 
		ret

DRIVE_NOTSPECIFIED:
		XOR		A
		RET



START_ARGUMENTS:	DW		$0000		; What is the address where the argument starts ( including spaces )
ARGUMENT_CHARS:		DB		$00			; How many characters in the argument only?

POPULATEFCB:							; Fill in the default FCBs. Prior to opening files or other things. COMMAND FILE1 FILE2 format. Doesn't need setup.
		LD		HL,FCB1
		LD		A,$00
		LD		B,$24
POPULATE_CLEAR:
		LD		(HL),A
		INC		HL
		DJNZ	POPULATE_CLEAR			; Clear all bytes in FCB to start. 
										; Next we track down the second and third field of the command line and copy them into FCB1 and FCB2

											; DE should still be set with the command buffer when we get here. DON'T RESET IT as commands can move. 
;		LD		DE,COMMAND_BUFFER			; At 0080 - Most likely. 
		CALL	SCANWHITE				; knows how long the strings are and errors if the filenames aren't there...
		JR		C,POPULATE_FCB1			; Something went wrong with the read. Just progress. 
		
		CALL	SCANGREY				; This should scan through the GREY (non white) letters.
										; Scan through the command here. Also copies the FIRST_COMMAND buffer to the command processor. 
		
		LD		HL,(SCANPOS)			; Get the current pointer to the command buffer.
		LD		(START_ARGUMENTS),HL	; Store the start of the arguments.
		LD		A,(SCANCHARS)			; Number of characters left in string entered into command buffer.
		LD		(ARGUMENT_CHARS),A		; Store that as well, so we can rewrite the arguments ( including LENGTH and FIRST SPACE) at $0080. 
		JR		C,POPULATE_FCB1			; Something went wrong with scanning the command, maybe it wasn't there?
		CALL	SCANWHITE_NEXT			; Now scan to the DIR target file.
;		JR		C,POPULATE_FCB1			; Something went wrong with the start of the process before we got to any arguments.

POPULATE_FCB1:
		LD		HL,FCB1					; Location of first temporary FCB in Zero Page @ $005C
		CALL	FILLFCB					;

										; At this point, I can detect the failed first file via (FILENAME_ERROR) and do something, or just pretend everything is OK.
										; I should test if there's greyspace next and SCANGREY if there is though... 
								
		CALL	SCANWHITE_NEXT			; Find the next character space and skip past it if necessary to the next argument. 
		
POPULATE_FCB2:
		LD		HL,FCB2
		CALL	FILLFCB
POPULATE_ARGUMENTS:						; Now we populate the arguments section.
		LD		HL,(START_ARGUMENTS)	; Get the starting pointer in memory.
		LD		A,(ARGUMENT_CHARS)		; And get the number of characters waiting.
		OR		A						; Is A = 0?
		JR		Z,POPULATE_NOARGUMENTS	; No arguments = mark it down and return. 
		LD		DE,$0080				; Where we write the arguments in memory. NOT a EQU label, becuase THIS IS A FIXED MEMORY LOCATION. 
		LD		(DE),A					; Write the number of characters in the argument. 
		INC		DE						; And progress to the first character of the arguments ( pretty much always a space ). 
		LD		C,A						; Load the number of characters to copy into BC. 
		LD		B,$00
		LDIR							; And copy the arguments. 	
		
		LD		A,$00					; Set the last post-argument to zero. null char alternative terminator since it's expected by CP/M software sometimes. 
		LD		(DE),A
		ret
POPULATE_NOARGUMENTS:
		LD		($0080),A				; Record there were no arguments. FCBs will both be all blanks = no filename. 
		ret


FCB_BLANK:								; Pre-clean the FCB 16 character space at HL. After the drive specifier. 
		LD		B,11					; 11 characters
FCB_BLANK_1:
		LD		(HL),' '				; Store space for each.
		INC		HL
		DJNZ	FCB_BLANK_1
		LD		B,4
FCB_BLANK_2:
		LD		(HL),$00
		INC		HL
		DJNZ	FCB_BLANK_2
		ret
		
FILENAME_ERROR:	DB	$00					; Let's save a place to note if there are any file errors.  0=not processed. 1=No errors. 2=??? 3=???
										; Reserved Characters are    < > . , ; : = ? * [ ] 
FILLFCB:								; Fill in the FCB from the Command Buffer. 
										; Let's fill in the FCB from the buffer,
										
		;NOTE COMMENT					; HL should hold FCB address that we are filling in... 

		LD		A,$01
		LD		(FILENAME_ERROR),A		; Start assuming there's no errors. 
		PUSH	HL						; Store FCB address
		CALL	DRIVE_SPECIFIER			; Comes back with 00 for no drive specifier, or the drive specifier - eg, A:=01 B:=02... P:=$10 etc. 
		POP		HL						; Recover FCB address.
		LD		(HL),A					; Store the drive specifier					
		INC		HL						; Move to the next character where the filename begins.
		PUSH	HL
		CALL	FCB_BLANK				; Blank out ( SPACES ) all characters ( = padding )
		POP		HL

										; FCB is now formatted and we can move on to checking what we have. 

		LD		B,8						; 8 characters
		LD		C,' '					; And the continue character = space. 
		CALL	FILLCHARS				; And fill in the first 8, and apply any necessary rules.
		RET		C						; If we have Carry set on return, there's nothing else to process.... 
		
		LD		B,3						; 3 characters for extension. 
		LD		C,' '					; And the continue character = space. 
		CALL	FILLCHARS				; And fill in the first 8, and apply any necessary rules.
		RET		C						; And once again, exit if that's all. We should not be expecting or seeing any more characters.
		
		LD		A,$03
		LD		(FILENAME_ERROR),A		; Otherwiwse we have an unspecified error on exit (eg, followed by a . )

		RET
		
	
FILLCHARS:
		PUSH	BC						; B counts iterations ( 8 or 3 )
		PUSH	HL						; HL holds location within FCB. 
		CALL	GETCHAR					; Get a character. Like SCAN but not correction for case. Returns next character in A. 
		POP		HL
		POP		BC

		JR		C,FILL_NOEXTENSION		; Carry = Nothing more, end of current search.
		
		CP		' '						; End of file matching string is whitespace?
		JR		Z,FILL_NOEXTENSION		; Filename without extension.	
		CP		'<'
		JR		Z,FILL_NOEXTENSION		; Reserved Characters are    < > . , ; : = ? * [ ] 	- Treat as whitespace.
		CP		'>'
		JR		Z,FILL_NOEXTENSION		; Reserved Characters are    < > . , ; : = ? * [ ] 	- Treat as whitespace.		
		CP		','
		JR		Z,FILL_NOEXTENSION		; Reserved Characters are    < > . , ; : = ? * [ ] 	- Treat as whitespace.	
		CP		';'
		JR		Z,FILL_NOEXTENSION		; Reserved Characters are    < > . , ; : = ? * [ ] 	- Treat as whitespace.	
		CP		':'
		JR		Z,FILL_NOEXTENSION		; Reserved Characters are    < > . , ; : = ? * [ ] 	- Treat as whitespace.
		CP		'='
		JR		Z,FILL_NOEXTENSION		; Reserved Characters are    < > . , ; : = ? * [ ] 	- Treat as whitespace.	
		CP		'['
		JR		Z,FILL_NOEXTENSION		; Reserved Characters are    < > . , ; : = ? * [ ] 	- Treat as whitespace.	
		CP		']'
		JR		Z,FILL_NOEXTENSION		; Reserved Characters are    < > . , ; : = ? * [ ] 	- Treat as whitespace.
		
				
		CP		'*'					; Global Wildcard.
		JR		Z,FILL_FIRSTWILDCARD	; Fill with ? to the end of the current buffer section.
		
		CP		'.'					; End of 8 char part early ( before 8 characters are input )	
		JR		Z,FILL_WITHEXTENSION			; Fill remain with spaces. 
		LD		(HL),A					; Store Character
		INC		HL
		DJNZ	FILLCHARS
		
		PUSH	HL
		CALL	GETCHAR					; Since we got to the last character of 8 real characters, let's read what comes next... It's relevant to what we do next. 
		POP		HL
		
;FILLCHARS_POSTCHECK:					; Enter here if the following character is still unknown. ( = character following the filename chars )		
;		RET		C						; If the carry is set, then there' nothing left to do... We've used all of the input buffer

FILLCHARS_POSTCHECK2:					; Enter here if the following character was already known.		
		CP		'.'						; Maybe the '.' came at the end of 8 characters?
		RET		Z						; Carry is clear (since we just tested for it) so return with no carry to indicate more might be available.
		
		CP 		' '						; Whitespace is OK, but there's nothing left to get, so set the carry.
		SCF								; Set the carry before we return on the Z. 
		RET		Z						; Carry = nothing more to get, and Z means it was Whitespace. 
		
		LD		A,2						; At this point, we got a bad filename. 
		LD		(FILENAME_ERROR),A		; Store the error code for unexpected characters after filename or extension. 
		RET								; We're at the end... Let's return - Carry is still set. 

FILL_NOEXTENSION:
		PUSH	AF						; Save flags.

FILL_NOEXTENSION2:
		LD		(HL),C
		INC		HL
		DJNZ	FILL_NOEXTENSION2	; Write all remaining characters as spaces, 	

		POP		AF						; Recover flags.
		JR		NC, FILLCHARS_POSTCHECK2		; And exit via routine to check nothing strange came next unless we were at the end of the buffer.
		RET									; Or just exit here if the carry brought us here. 
		
FILL_FIRSTWILDCARD:
		LD		C,'?'					; Change default fill to ? (Wildcard).	
FILL_FIRSTWILDCARD_LP:
		LD		(HL),C
		INC		HL
		DJNZ	FILL_FIRSTWILDCARD_LP	; Fill in with '?'s
		PUSH	HL						; Store FCB location. 
		CALL	GETCHAR					; Since * = remaining 8 of '?'s. its like we got ???????? so we need the next character. 
		POP		HL						; Restore FCB location.
		JR		FILLCHARS_POSTCHECK2	; Which will check if there's anything more. 

FILL_WITHEXTENSION:
		LD		(HL),C
		INC		HL
		DJNZ	FILL_WITHEXTENSION	; Write all remaining characters as spaces, 		
		JR		FILLCHARS_POSTCHECK2	; And exit via routine to process out the . character



;###################################################################################
;###############
;###############
;###############             DIR
;###############
;###############
;###################################################################################	
DIRENTRIES:	DB	$05						; Directory entries per line. 
DIRCOUNT:	DB	$05						; Count of directory entries. 
; CRLF here. 
	
DIR:									; We know the command was "DIR" and we have the FCB at $005C. 	
		LD		A,$05
		LD		(DIRCOUNT),A			; Clear current count of entries displayed. 
		
		CALL	CRLF					; Simple start, because the system we're on might use CR instead of LF for Enter. 

		LD		DE,$0080
		LD		C,$1A					; Function 1A - Set the DMA address to the buffer at 0080.
		CALL	BDOS					; Set DMA to 0080 - This is because the directory will show up here. Saves us copying the filenames too. 

										; First check for case where there were no filenames and convert to ??????????? otherwise.

		LD		A,($0080)				; FIXED - This is where the command arguments got stored by Populatefcb.
		OR		A						; If there's no arguments... 
		JR		Z,DIR_ALLFILES			; Then rewrite as a wildcard.
		LD		A,(FCB1+1)				; Also, might be spaces ( no file ).
		CP		' '
		JR		Z,DIR_ALLFILES			; And if the filename is just spaces, or leading spaces, then rewrite as a wildcard.
		JP		DIR_SEARCH_FCB			; Otherwise search for what is in the FCB. 
DIR_ALLFILES:
		CALL	DIR_WILDCARD		
DIR_SEARCH_FCB:
		LD		DE,FCB1					; Call the BDOS with the default FCB.
		LD		C,Search_For_First					; Find first.
		CALL	BDOS					; Get the first matching file. 

		
DIR_LOOP:
;		CALL	SHOWREG
		CP		$FF						; A should hold the index of the extent in the record. FF means end of extents to search. 
		RET		Z
;		JR		Z,DIR_END

		CALL	PRINTEXTENT				; A holds the index, and it's somewhere after $0080 so print the filename within it. 
		CALL	ONELINE					; Print two spaces or one line  after entry. 

		LD		A,(DIRCOUNT)			; New code added he until DIR_NOCRLF to control spacing of directory entries printed. 
		DEC		A
		LD		(DIRCOUNT),A
		JR		NZ,DIR_NOCRLF
		
		LD		A,(DIRENTRIES)
		LD		(DIRCOUNT),A			; RESET. 
		CALL	CRLF					; Insert an EOL. 

DIR_NOCRLF:
		LD		DE,FCB1					; And we search again....  
		LD		C,Search_For_Next					; First next.			
		CALL	BDOS					; Get the next file. 
			
		JR		DIR_LOOP				; Loop until there's no more files. 
		
;DIR_END:
;		CALL MONITOR
;		RET

DIR_WILDCARD:							; Fill the remaining FCB with Wildcards ???????????
		LD		HL,FCB1+1				; Not the temp_fcb which gets written on each file. 
		LD		B,11						; 8 characters
DIR_WILDCARD_1:
		LD		(HL),'?'				; Store Character
		INC		HL
		DJNZ	DIR_WILDCARD_1
		ret
		

DIRERROR:								; Something went wrong.
		LD		DE,FILENAME_BAD
		JP		ERROR_MESSAGE			; Send out the error message. 



;###################################################################################
;###############
;###############
;###############             ERASE
;###############
;###############
;###################################################################################
ERA:
		LD		C,Delete_File
		LD		DE,$005C				; The lower FCB 
		CALL	BDOS
		ret								; Delete the file. 




;###################################################################################
;###############
;###############
;###############             RENAME FILE
;###############
;###############
;###################################################################################
REN:
		LD		C,Rename_File
		LD		DE,$005C
		CALL	BDOS
		ret
		
		
		
;###################################################################################
;###############
;###############
;###############             SAVE
;###############
;###############
;###################################################################################		
SAVE_RECORD:
		LD		HL,(SAVE_DMA)
		LD		DE,$0080
		ADD		HL,DE
		LD		(SAVE_DMA),HL
		EX		DE,HL
		LD		C,Set_DMA_Address
		CALL	BDOS
		LD		DE,$005C
		LD		C,Write_Sequential
		CALL	BDOS
		ret
		
SAVE_BLOCKS:	DB	$00				; Temp save of iterations of write.
SAVE_DMA:		DW	$0000			; Where are we saving?
SAVE:		; Two things to do here - first get the number into B ( from 0 to 255 )
			; Then move the filename into FCB 1, and open the file to write and sequential save it.
			
		LD	DE,$005D						; First character of the number of blocks to save.
		CALL	COUNT_NUMERALS				; How many numerals? Returns in A and C. 
				
		LD		A,C						; If no numerals?
		OR		A
		JP		Z,DECIMAL_ERROR			; If there's zero, then exit with error. 
		
		CALL	GET_NUMBER
		OR		A
		JP		Z,DECIMAL_ERROR			; Don't accept zero as a valid number. 
		
		LD		(SAVE_BLOCKS),A			; Store it while we check the filename
		
		LD		DE,$005C
		LD		HL,$006C
		LD		BC,$0010
		LDIR							; Copy the second filename to FCB1.
		
		LD		DE,$005C				; Let's open a new file. 
		LD		C,Make_File
		CALL	BDOS
		
		PUSH	BC						; Push because we otherwise call bad save from within the loop - so make sure we can pop it. 
		INC		A
		JR		Z,BAD_SAVE				; If A was $FF from Open File then it didn't create. 
		POP		BC
		
		LD		A,(SAVE_BLOCKS)
		LD		B,A						; How much are we going to save?
		
		LD		DE,$0080				; Save the address prior to adding 80 and setting DMA ( will set DMA to $0100 on first call )
		LD		(SAVE_DMA),DE

SAVE_LOOP:	
		PUSH	BC
		CALL	SAVE_RECORD
		OR		A
		JR		NZ,BAD_SAVE

		CALL	SAVE_RECORD
		OR		A
		JR		NZ,BAD_SAVE
		
		POP		BC
		DJNZ	SAVE_LOOP

		LD		DE,$005C
		LD		C,Close_File
		CALL	BDOS					; Close the file.
		INC		A
		JR		Z,BAD_SAVE				; Alert that the file didn't close properly. 

		ret







		
	
GET_NUMBER:
		DEC		DE						; First character 
		LD		A,(DE)
		AND		$0F					; Mask to get a binary value of the character.('0' = 0)
		DEC		C						; C holds count of characters. 
		RET		Z						; If there was only 1 character, exit here. We now have the lower digit.

			; Second Digit.
		LD		H,A						; Temp store of A. 
		DEC		DE
		LD		A,(DE)
		AND		$0F					; Convert to binary.
		OR		A						; Is it zero? Bypass the add then
		JR		Z,GET_NUMBER_SKIP
		LD		B,A
		LD		A,H
GET_NUMBER_MID:
		ADD		A,$0A					; Add 10 B times.
		DJNZ	GET_NUMBER_MID
GET_NUMBER_SKIP:
		DEC		C						; C holds count of characters. 
		RET		Z						; If there was only 2 characters, exit here. We now have the lower two digits.	
			
			; Third Digit.
GET_NUMBER_HIGH:						; OK there was at least 3 characters. 
		DEC		DE
		LD		H,A						; Temp store A.
		LD		A,(DE)
		AND		$0F
		DEC		A						; Check for out of bounds at same time, which is why we do this the long way, instead of a general routine.
		JR		Z,GET_NUMBER_100
		DEC		A
		JR		NZ,DECIMAL_ERROR
		LD		A,H
		ADD		A,$C8
		JR		C,DECIMAL_ERROR 		; Went past 255. That's too big. 
		ret								; or exit with a full number. 
GET_NUMBER_100:
		LD		A,H
		ADD		A,$64
		ret
		
		
		
COUNT_NUMERALS:							; Count how many numerals are there?
		LD		C,0						; Number count. How many digits are there that we found?
		LD		L,4						; L = Limit of how many we'll check. 4 means fail out. 
COUNT_NUMERALS_LP:

		LD		A,(DE)
		CP		' '						; Blank space.		
		ret		Z						; if we hit a space, exit. 	

		CALL	IS_IT_DECIMAL			; Is it a decimal number?

		JR		C,COUNT_NOT_NUMBER
		DEC		L						; Number counter.
		JR		Z,COUNT_NOT_NUMBER	
		
		INC		DE						; Go to the next space.
		INC		C
		JR		COUNT_NUMERALS_LP		; And loop. Up to 7 times. 
		
		
COUNT_NOT_NUMBER:
		LD		C,0						; Clear A to show no numerals.
		ret
		

IS_IT_DECIMAL:							; Check A - If it's decimal, return CARRY SET if it's not, otherwise return NO CARRY if it's decimal.
		CP		'0'
		RET		C						; Carry means the value of A was less than '0' so it's not decimal.
		CP		'9'+1					
		CCF								; Complement Carry Flag. 0-9 is Carry, otherwise is NC. 
		RET

DECIMAL_ERROR:
		LD		DE,BAD_DECIMAL_NUMBER
		JP		ERROR_MESSAGE
BAD_SAVE:
		POP		BC
		LD		DE,BAD_SAVE_MESSAGE
		JP		ERROR_MESSAGE
BAD_DECIMAL_NUMBER:	db 'Not a valid command. Type SAVE <BLOCKS> FILENAME.EXT where <BLOCKS> is a decimal number from 1 to 255 - Each block is 256 bytes starting from (HEX0100)',$0A,$0D,'$'
BAD_SAVE_MESSAGE:	db 'Issue saving file - bad filename or disk full.',$0A,$0D,'$'

;###################################################################################
;###############
;###############
;###############             TYPE
;###############
;###############
;###################################################################################
TYPE:					
						; Type out a file.
						; Opens a file, then 
		LD		DE,$0080
		LD		C,Set_DMA_Address		; Function 1A - Set the DMA address to the buffer at 0080.
		CALL	BDOS					; Set DMA to 0080 - This is because the directory will show up here. Saves us copying the filenames too. 

		LD		DE,FCB1					; The filename we want is located in the first FCB in lower memory from the Command Line
		LD		C,Open_File				; Open File.
		CALL	BDOS					; Get the first matching file. 

		AND		%11111100				; Mask the result bytes.
		JR		NZ,FILE_OPEN_ERROR
TYPE_READ:
		LD		DE,FCB1					; The filename we want remains located in the first FCB in lower memory from the Command Line
		LD		C,Read_Sequential					; read record
		CALL	BDOS					; Get the first matching file. 
		OR		A						; Check for file status - And exit if we're at EOF ( we already exited earlier for other result other than read OK )

;ld d,(IY+CR)
;ld e,(IY+RC)
;ld b,(IY+EX)
;call showreg

		JR		NZ,TYPE_EXIT				
		
		
		LD	E,$FF						; Read the keyboard once per pass in case of CTRL C - Maybe add a pause here later too.  
		LD	C,Direct_Console_IO			; Keyboard Interrupt on type added 29-12-2023
		CALL	BDOS
		CP	$03
		JP	Z,TYPE_EXIT								; Break point. Once per pass.
		
		
		CALL	PRINT_RECORD			; Print the record
		JR		TYPE_READ
		
TYPE_EXIT:		
		CALL	CRLF					; Start next on a new line. 
		ret

PRINT_RECORD:
		LD		B,128
		LD		HL,$0080				; DMA address.
PRINT_RECORD_LOOP:

		LD		A,(HL)					; Bugfix 29-12-2023 - This was occuring AFTER the output. Don't output EOF
		CP		$1A						; CTRL'Z'
		RET		Z						; Is end of file. Exit here now if that happens. 
		
		PUSH	HL
		PUSH	BC
		LD		E,(HL)
		LD		C,Console_Output
		CALL	BDOS
		POP		BC
		POP		HL

		
		INC		HL
		DJNZ	PRINT_RECORD_LOOP
		
		ret


DESTINATION_MISSING_ERROR:
		LD		DE,NODEST_ERROR
		JP		ERROR_MESSAGE

FILE_READ_ERROR:
		LD		DE,FILEREAD_ERROR
		JP		ERROR_MESSAGE

FILE_OPEN_ERROR:
		LD		DE,FILEOPEN_ERROR
		JP		ERROR_MESSAGE			; Exit via the error message.
		

;###################################################################################
;###############
;###############
;###############             USER CHANGE ID
;###############
;###############
;###################################################################################

USER:
		ret
		
		
		
		
;###################################################################################
;###############
;###############
;###############             COPY FILE
;###############
;###############
;###################################################################################	
SECOND_FCB:			DB	$00					; Similar names to BDOS so we can use the same code. 
SECOND_FILENAME:	DB	'nullfiledat'		
SECOND_FILENAME_CNT:DB $00,$00,$00,$00
SECOND_FILENAME_ALL:DB $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$00
SECOND_FILENAME_HND:DB $00,$00,$00,$00	
EXTENT_POINTER:		DW	$0000					; The current extent pointer is the DMA record location - we will copy to and from here for 128 bytes. 
EQU		EXTENT_MEMORY_START,$8000				; We're going to read up to 16K at a time. Read and Write extent by extent. 
FCBLOC:				DW	$0000					; Store of the FCB location for IX. 


COPY:
						; COPY a file.
						; Opens a file, then 
		LD		DE,EXTENT_MEMORY_START
		LD 		(EXTENT_POINTER),DE		; Store the current extent memory location where we'll copy the record to. 
		LD		C,Set_DMA_Address		; Function 1A - Set the DMA address to the buffer at 0080.
		CALL	BDOS					; Set DMA to 0080 - This is because the directory will show up here. Saves us copying the filenames too. 
		
		LD		HL,FCB2					; Second Filename Location in memory. 
		LD		DE,SECOND_FCB			; Second FCB to use to open file. 
		LD		BC,12					; Twelve characters since destination might be on another drive, or original might not come from default. 
		LDIR							; Set up the second FCB, since we're going to copy the entire thing. 
	
		LD		HL,SECOND_FILENAME		; Check if the destination filename was specified. 
		LD		A,(HL)
		CP		$20						; Check if the start of the filename is a space character.
;		JR		Z,DESTINATION_MISSING_ERROR	; No destination filename specified. 
		JR		NZ,SECOND_FILENAME_OK	; Continue if it was specified. 
										; Otherwise let's assume the first filename should be used for the second also. 
		
		LD		HL,FCB1+1				; Filename section of FCB1.
		LD		A,(HL)					; Check briefly to make sure the first exists, or the second will not exist. 
		CP		$20						; Make sure the first filename exists.
		JR		Z,DESTINATION_MISSING_ERROR	; Error out if there's nothing to work with. 
		
		LD		DE,SECOND_FCB+1			; Name section of second FCB. ( if we get here, the first filename existed ) - And the destination drive will be default or known. 
		LD		BC,11					; 8+3
		LDIR
		
		


SECOND_FILENAME_OK:	
		LD		HL,SECOND_FILENAME_CNT		; Zero out the rest if it's not already zero ( eg, after we already do a copy previously )
		LD		B,24					; 24 bytes to zero. - Note - BDOS Create Filename also does this. 
COPY_CLEAN_LP:
		LD		(HL),$00
		INC		HL
		DJNZ	COPY_CLEAN_LP			; And clean up the remaining 24 characters of FCB. 

		LD		DE,FCB1					; The filename we want is located in the first FCB in lower memory from the Command Line
		LD		C,Open_File				; Open File.
		CALL	BDOS					; Get the first matching file. 
		AND		%11111100				; Mask the result bytes.
		JR		NZ,FILE_OPEN_ERROR		; An error here means DO NOT CONTINUE. 	
										; Need to check the file isn't empty. Look for at least an allocation.
										;###################################################################################################################										
		
		LD		DE,SECOND_FCB
		LD		C,Make_File				; Need to add error handling in here later. In case we can't make the file. 
		CALL	BDOS					; Create the second file
										; Error test here and exit if we can't open the other file.
										;###################################################################################################################


COPY_AGAIN:		
		LD		IX,FCB1					; Time to read the FCB data post-open. 
		LD		B,(IX+RC)				; Let's get the number of records we have to copy. This will change with each iteration of FCB Extent. 

COPY_READ:
		PUSH	BC
		LD		DE,FCB1					; The filename we want remains located in the first FCB in lower memory from the Command Line
		LD		C,Read_Sequential					; read record
		CALL	BDOS					; Get the first matching file. 
		OR		A						; Check for file status - And exit if we're at EOF ( we already exited earlier for other result other than read OK )
		JP		NZ,COPY_ERROR			; Something went wrong if the read response isn't zero. 				
COPY_REENTRY:					; reentry point if we need to restart for another extent. 
		LD		HL,(EXTENT_POINTER)		; Get the current DMA. 
		LD		DE,$80					; Record Length.
		ADD		HL,DE					;
		LD		(EXTENT_POINTER),HL
		LD		DE,(EXTENT_POINTER)
		LD		C,Set_DMA_Address		; Function 1A - Set the DMA address to the buffer at 0080.
		CALL	BDOS					; Set DMA to 0080 - This is because the directory will show up here. Saves us copying the filenames too. 		
		POP		BC
		DJNZ	COPY_READ				; Use DJNZ because we don't want to read more than 80 records... Otherwise we will go past the extent. If there's less, that's OK. 


COPY_WRITE:		
		LD		DE,EXTENT_MEMORY_START
		LD 		(EXTENT_POINTER),DE		; Store the current extent memory location where we'll copy the record from and send to disk. 
		
		LD		C,Set_DMA_Address		; Function 1A - Set the DMA address to the buffer at 0080.
		CALL	BDOS					; Set DMA to 0080 - This is because the directory will show up here. Saves us copying the filenames too. 


		LD		IX,FCB1					; Time to read the FCB data post-open. 
		LD		B,(IX+RC)				; Let's get the number of records we have to copy. 		
		
COPY_WRITE_LOOP:
		PUSH	BC

		LD		DE,SECOND_FCB
		LD		C,Write_Sequential
		CALL	BDOS	

		OR		A
	
		JR		NZ,COPY_ERROR			; Something went wrong if the response isn't zero. 
		
		LD		HL,(EXTENT_POINTER)		; Get the current DMA. 
		LD		DE,$80					; Record Length.
		ADD		HL,DE						;
		LD		(EXTENT_POINTER),HL	
		LD		DE,(EXTENT_POINTER)
		LD		C,Set_DMA_Address		; Function 1A - Set the DMA address to the buffer at 0080.
		CALL	BDOS					; Set DMA to 0080 - This is because the directory will show up here. Saves us copying the filenames too. 		
	
		POP		BC
		DJNZ	COPY_WRITE_LOOP			; We won't have written more than an extent now...
		
		
		;************************************* Drop out of the loop and see if iteration required. 
		
		LD		IX,FCB1					; Once more, let's read the extent record to see if we need to do another.  
		LD		A,(IX+RC)				; Let's get the number of records we have to copy. This will change with each iteration of FCB Extent. 
		CP		$80						; If it's $80 then we shoud read another one, so set up and start again.
		JR		NZ,COPY_WRITE_DONE
		
		LD		DE,EXTENT_MEMORY_START	; Reset the memory pointer for another extent. 
		LD 		(EXTENT_POINTER),DE		; Store the current extent memory location where we'll copy the record from and send to disk. 
		
		LD		C,Set_DMA_Address		; Function 1A - Set the DMA address to the buffer at 0080.
		CALL	BDOS					; Set DMA to 0080 - This is because the directory will show up here. Saves us copying the filenames too. 

		LD		DE,FCB1					; The filename we want remains located in the first FCB in lower memory from the Command Line
		LD		C,Read_Sequential					; read record
		CALL	BDOS					; Get the first matching file. 

		CP		$01						; End of file, in case there's nothing more...
		JR		Z,COPY_WRITE_DONE
		CP		$04
		JR		Z,COPY_WRITE_DONE		; Next extent doesn't exist so we read them all. 		
		
		OR		A						; Check for file status - And exit if we're at EOF ( we already exited earlier for other result other than read OK )
		
		JR		NZ,COPY_ERROR			; Something went wrong if the read response isn't zero and it wasn't EOF.

		LD		IX,FCB1					; Once more, let's read the extent record to see if we need to do another.  
		LD		B,(IX+RC)				; Let's get the number of records we have to copy. This will change with each iteration of FCB Extent. 
		PUSH	BC						; Because the main routine pops it to get this back. 
		
		JP		COPY_REENTRY			; And reenter the main loop.
		
										
COPY_WRITE_DONE:						; We should be done now.... Say something?
		LD		DE,SECOND_FCB
		LD		C,CLOSE_FILE
		CALL	BDOS
		ret

COPY_ERROR:
		POP		BC						; Because it was already pushed. 
		
		LD		DE,FILE_COPY_ERROR	
		CALL	ERROR_MESSAGE
		LD		DE,SECOND_FCB
		
		LD		C,CLOSE_FILE			; Close the extent we just copied. 
		CALL	BDOS
		ret						; Ret at the end of the list. 



;###################################################################################
;###############
;###############
;###############            HALT
;###############
;###############
;###################################################################################


HALT_HERE:

;	CALL	SHOWREG
;	CALL	SHOWIX
	
	LD		DE,HALTMESSAGE
	LD		C,9
	CALL	BDOS
	
;	CALL MONITOR
	HALT
	JP	HALT_HERE		; I just stop here. Will cause emulator to exit. 

HALTMESSAGE: DB $0D,$0A,'System error: Halting.',$0D,$0A,'$'

;###################################################################################
;###############
;###############
;###############             HELP
;###############
;###############
;###################################################################################


HELP:					; Show command list. 
	CALL	CRLF			; New line. 
	LD		HL,COMMANDS	; Find the commands list.
HELP1:
	LD		A,(HL)
	AND		%0111 1111	; Just putting a space in here to remind me I can space variables. Two nibbles are easier to see. 
	RET		Z				; Use this as a test point - Return if A is zero = End of table. 
	
	PUSH	AF
	PUSH	HL
	LD		E,A
	LD		C,Console_Output
	CALL	BDOS
	POP		HL
	POP		AF
	
	CP		(HL)			; Was it an End Of Command flag? (Bit 7)
	JR		Z,HELP2			; If it's the same, it was a normal character.
	
	PUSH	HL
	CALL	CRLF			; New line. 
	POP		HL

HELP2:
	INC		HL
	JR		HELP1			; And repeat. 
	

;###################################################################################
;###############
;###############
;###############             DUMP
;###############
;###############
;###################################################################################

DUMP_FILE_OFFSET: dw $0000	; Where we store the offset into the file. 

DUMP:					; Like TYPE but 
						; DUMPS out a file in HEX.
						; Opens a file, then 

		LD		DE,$0080
		LD		C,Set_DMA_Address		; Function 1A - Set the DMA address to the buffer at 0080.
		CALL	BDOS					; Set DMA to 0080 - This is because the directory will show up here. Saves us copying the filenames too. 

		LD		DE,FCB1					; The filename we want is located in the first FCB in lower memory from the Command Line
		LD		C,Open_File				; Open File.
		CALL	BDOS					; Get the first matching file. 

		AND		%11111100				; Mask the result bytes.
		JP		NZ,FILE_OPEN_ERROR

		LD		HL,$0000				; Set up index of file position. 
		LD		(DUMP_FILE_OFFSET),HL	; An index of how far we are into the file. 

DUMP_READ:
		LD		DE,FCB1					; The filename we want remains located in the first FCB in lower memory from the Command Line
		LD		C,Read_Sequential					; read record
		CALL	BDOS					; Get the first matching file. 
		OR		A						; Check for file status - And exit if we're at EOF ( we already exited earlier for other result other than read OK )
		JR		NZ,DUMP_EXIT				

		CALL	DUMP_RECORD			; Print the record

LD	HL,$0080
LD	(DECFROMHEX),HL
CALL	DUMPRECORD		
		
		JR		DUMP_READ			; Don't loop while testing... Just one sector is enough. 
		
DUMP_EXIT:		
		CALL	CRLF					; Start next on a new line. 
		ret
		
DUMP_RECORD:	


		LD		HL,$0080
		LD		B,$10

;		CALL	DUMPPAGE_LINE			; LD HL,data, B,number of bytes

		LD		HL,$0090
		LD		B,$10
		
;		CALL	DUMPPAGE_LINE			; LD HL,data, B,number of bytes
;		CALL	DUMPPAGE_LINE			; LD HL,data, B,number of bytes
;		CALL	DUMPPAGE_LINE			; LD HL,data, B,number of bytes

		ret
		
DUMP_INDEX:
		LD		A,(DUMP_FILE_OFFSET+1)	; How far are we into the file?
		CALL	PRINTHEX			; And print it out. The page.

		LD		A,(DUMP_FILE_OFFSET)	; Lower bytes of how far we're into the file. 
		CALL	PRINTHEX			; Print the current address.

		LD		HL,(DUMP_FILE_OFFSET)
		LD		DE,$0010
		ADD		HL,DE
		; Do something here to change the index in future... 
		
		LD		E,' '				; Print a space.
		LD		C,Console_Output
		CALL	BDOS
		ret

;###################################################################################
;###############
;###############
;###############             SUBMIT - Batch File Processing. 
;###############
;###############
;###################################################################################

SUBMIT:					
						; Execute batch files from a text file - terminate line on 0A/0D or otherwise 
						; Opens a file, then 
		LD		DE,$0080
		LD		C,Set_DMA_Address		; Function 1A - Set the DMA address to the buffer at 0080.
		CALL	BDOS					; Set DMA to 0080 - This is because the directory will show up here. Saves us copying the filenames too. 

		LD		DE,FCB1					; The filename we want is located in the first FCB in lower memory from the Command Line
		LD		C,Open_File				; Open File.
		CALL	BDOS					; Get the first matching file. 

		AND		%11111100				; Mask the result bytes.
		JP		NZ,FILE_OPEN_ERROR		; Use the errors from other routines -
SUBMIT_READ:
		LD		DE,FCB1					; The filename we want remains located in the first FCB in lower memory from the Command Line
		LD		C,Read_Sequential					; read record
		CALL	BDOS					; Get the first matching file. 
		OR		A						; Check for file status - And exit if we're at EOF ( we already exited earlier for other result other than read OK )

		JR		NZ,SUBMIT_EXIT				
		CALL	PRINT_RECORD			; Print the record
		JR		SUBMIT_READ
		
SUBMIT_EXIT:		
		CALL	CRLF					; Start next on a new line. 
		ret

		
;###################################################################################
;###############
;###############
;###############             No more internal commands. 
;###############
;###############
;###################################################################################


PRINT_FCB:								; Print the filename in the current FCB. 
										; Enter with HL pointing to the FCB.
		INC		HL
		LD		B,11					

PRINTFCB_LOOP:								; Do something here. Printing via BIOS only affects A and C at the moment.								
		PUSH	BC						; Print the found filename. 
			LD		A,(HL)
			push	HL
			LD		C,Console_Output
			AND		$7F					; Convert to ASCII
			LD		E,A
;			CP		' '						; Don't print spaces. (padding)
			push	bc
;			CALL	NZ,BDOS					; Only call for non-? characters.
			CALL	BDOS
			pop 	bc
			LD		A,B
			CP		04						; On the pass where B=4, print a dot.
			LD		E,'.'
			LD		C,Console_Output
			CALL	Z,BDOS					; Only when B=4 though, print a dot.  
		POP		HL
		POP		BC						; Will need to protect DE and HL later. Or maybe adjust BIOS to protect what it touches. 
		INC		HL						; Update pointers. 
		DJNZ	PRINTFCB_LOOP
;		CALL	CRLF					; Need to see what CRLF messes with. Actually, at this point, who cares. We reset the registers later anyway. 
		ret


PRINTEXTENT:						; Print out directory contents for a directory structure. 
		LD		HL,$0080			; We can use different addresses later - For the moment, this is where... Called with A holding the index of the extent we want. 
		RRCA
		RRCA
		RRCA						; Multiply by 32. 
		LD		C,A					; Enter with A=Extent Index.	C = A * 32 ( won't exceed 96 ) 
		LD		B,$00				; BC = A * 32.
		ADD		HL,BC				; Get the address of the start of the record in HL. 4 extents per record. 
		INC		HL
		LD		B,11
		JR		PRINTFCB_LOOP		; And use the FCB print routine. 


EXECUTE_NOT:
		OR		A					; Clear Carry Flag. No executables. Return to error message handler. 
		RET						

EXEC_COM:	db $00,'        COM'	; Default drive, COM. Later will make multi-search and L: is home drive ( will search )	
EXEC_FILE_PTR: dw $0100				: Where in memory we are writing... 

ARGBUFFER:	block 128				; Store the arguments while we corrupt them with searches... 
									; this is a kludge - need a better way.

EXECUTE:							; Is the command line something we can execute?

		XOR		A
		LD		(EXEC_COM),A		; Unless it changes, set default drive. 
		
									; First check if it's on the default drive we're logged into or not?
		LD		A,(TEMP_LOGGED_DRIVE)	; Are we on a temporary logged drive? 
		INC		A					; Turn FF in 0 (default) and 0 into 1 ( drive A in FCB) etc. 
		JR		Z,EXEC_DEFAULT		; If it's zero, leave it at default drive. 
		LD		(EXEC_COM),A		; Set the drive in case it's relevant. ( Might be transitory )
		LD		HL,FIRST_COMMAND+3
		LD		DE,FIRST_COMMAND+1	; Remove the drive specifier.
		LD		BC,12				; Possible 8+3+1
		LDIR						; Adjust command to remove drive specifier. 
		LD		A,(FIRST_COMMAND)
		DEC		A
		DEC		A
		LD		(FIRST_COMMAND),A	; Doesn't matter if ithe number of letters is out of bounds - we check that next. 
;		LD		A,$FF
;		LD		(TEMP_LOGGED_DRIVE),A	; Make sure we reset this now, if not elsewhere. 

EXEC_DEFAULT:						; Default drive assumed or already set for the transitory drive if we are here.

		LD		A,(FIRST_COMMAND)
		OR		A
		RET		Z					; if no characters, just exit, and carry is already cleared ( is fail ).
		CP		$09					; 8 characters or less max.
		JP		NC,EXECUTE_NOT		; Nothing to execute. 

LD		HL,$0080
LD		DE,ARGBUFFER
LD		BC,$0080
LDIR	 ; backup arguments. 


									; Clear the execution FCB space.
		LD		HL,EXEC_FCB			; We need to clear the exec FCB first, since it might have carry over from other files loaded.
		LD		B,$24				; 36 characters in DMA.
EXECUTE_CLEAR_FCB:
		LD		(HL),$00
		INC		HL		
		DJNZ	EXECUTE_CLEAR_FCB	; and it's clean.
		
		
		LD		BC,12				; Add the ".COM" and spaces to the FCB. 
		LD		DE,EXEC_FCB			; Let's leave the lower FCB to the application. 
		LD		HL,EXEC_COM
		LDIR						; Copy the COM default.
		
		LD		A,(FIRST_COMMAND)
		LD		C,A					; Get the character count from the first transfer.
		LD		B,$00				; Clear B ( BC holds count )
		LD		HL,FIRST_COMMAND+1	; Transfer the command characters from First Command to EXEC FCB
		LD		DE,EXEC_FCB+1			; The character section.
		LDIR						; Move the command line as is... The match routine won't work if it's garbage.
									; Since Wildcards will be ignored, and we aren't writing any filenames. 

		LD		DE,EXEC_DMA	
		LD		C,Set_DMA_Address		; Function 1A - Set the DMA address to the buffer at 0080.
		CALL	BDOS					; Set DMA to 0080 - This is because the directory will show up here. Saves us copying the filenames too. 


		LD		DE,EXEC_FCB					; The filename we want is located in the first FCB in lower memory from the Command Line
		LD		C,Open_File				; Open File.
		CALL	BDOS					; Get the first matching file. 
		
		AND		%11111100				; Mask the result bytes.
;;;		RET		NZ						; If the file didn't open, then exit.  AND will clear CF = error. FIRST CHECK PATH.  
		JR		Z,EXEC_OK				; If we got a file we can execute, then execute. 

								; Extend on here to check other drives ( eg: L: N: O: etc.  )
		LD		A,$0C					; L Drive may contain alternative commands. 
		LD		(EXEC_FCB),A			; Set EXEC FCB manually to look at L:

		LD		DE,EXEC_FCB				; Redo the check with L: this time. 
		LD		C,Open_File				; Open File.
		CALL	BDOS					; Get the first matching file. 
		
		AND		%11111100				; Mask the result bytes.
		RET		NZ						; If the file didn't open, then exit.  AND will clear CF = error. 		

EXEC_OK:								; We got a command we can execute..  So do so. 
		LD		DE,$0100
		LD		(EXEC_FILE_PTR),DE		; Store where we will store it. 
EXEC_READ:
		LD		DE,EXEC_FCB					; The filename we want remains located in the first FCB in lower memory from the Command Line
		LD		C,Read_Sequential					; read record
		CALL	BDOS					; Get the first matching file. 
		OR		A						; Check for file status - And exit if we're at EOF ( we already exited earlier for other result other than read OK )
		JR		NZ,EXEC_EXIT				
		CALL	EXEC_WRITECOM			; Copy the record to the file. 
		JR		EXEC_READ

EXEC_EXIT:		
;;CALL	CRLF					; Start next on a new line. 
;;call showreg							; Note, will need to CALL $0100 - since we must set CF
;call monitor

LD		DE,$0080
LD		HL,ARGBUFFER
LD		BC,$0080
LDIR	 ; restore arguments. 

;out ($13),A	; debug on.
		call	$0100
;out (10),A

		SCF								; Don't print error. 
		ret

EXEC_WRITECOM:
;		LD		B,128
		LD		BC,$80
		LD		HL,EXEC_DMA				; DMA address.
		LD		DE,(EXEC_FILE_PTR)		; File location
		LDIR							; Transfer direct to memory. 
;EPRINT_RECORD_LOOP:
;		LD		A,(HL)
;		LD		(DE),A
;		INC		HL
;		INC		DE
;		DJNZ	EPRINT_RECORD_LOOP
		LD		(EXEC_FILE_PTR),DE		; Store new pointer location. (Cumulative)
		ret

EXEC_FCB:	DS	36						; Blank space for FCB.
;EXEC_DMA:	DS	128						; DMA for Executable. Leave the $0080 alone. 

;.equ EXEC_FCB,$005C
.equ EXEC_DMA,$0080						; test moving these to the zero page. 

EXEC_EOF:	DB	'<END'					; mark the end of this in memory. 

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
;6	6	Direct Console I/O 		E = $FF (input)			A = char
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



 
