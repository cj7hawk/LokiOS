; LOKI OS. Logical Output and Keyboard Interface. 
; LOKI OS is CP/M Compatible. At least it uses the same code calls, but is intended to "obfuscate" the hardware through a TTL Serial Bus @ 1Mbps.
; The "Megabus" is a serial shared networking protocol that allows calls for any function to be made over serial - eg, Disk over serial...  Console over serial.
; Protocol is basically SLIP with some new concepts. So it's talking IP. 
;
; Stuff I still have to deal with -
;  BDOS uses it's own stack. - OK, Didn't know this. I need to figure out a clean way to do this. 
;  CP/M is *not* case insensitive. It converts the command line to uppercase, but that's not the same thing.

; BUGS - In Editor ( key SHIFT-9 key - screws up input and makes go back to line start. 
; BUG 25/12/2022	- Fixed the console input bug, ctrl codes were marked with 2 upper nibble instead of 1 (eg, $28 instead of $18 )
; BUG 9/3/2023 - Allocation Tables incorrect - Need to scale to disk size. 
; BUG 4/4/2023 - Rename function was fixed to lower FCB, needed to make work with ANY fcb with two name - Caused WS to fail. 
;
;
;LD		B,H
;LD		A,L
;LD		C,$13			; Starts DEBUG mode in emulator ( LOKI, Not Loki2 )
;OUT (C),A				; Show BIOS DMA area. 
;
; Out ($0A),A			; STOPS debug mode in emulator. 
;
;

; CPM Calls.
;
;
;Function                           Entry Value to     Return Value from
; Number                            BDOS Passed in       BDOS Passed in 
;DEC  HEX     Function             (DE) or (E) regs   (HL) or (A) register    STATUS OF CODE.
;-------------------------------------------------------------------------
; 0    00 | System Reset           |      ****        |      ****        | Done
; 1    01 | Console Input          |      ****        | (A)=character    | Done
; 2    02 | Console Output         | (E)=character    |      ****        | Done
; 3    03 | Reader Input           |      ****        | (A)=character    | Nothing further at this time
; 4    04 | Punch Output           | (E)=character    |      ****        | Nothing further at this time
; 5    05 | Printer Output         | (E)=character    |      ****        | Nothing further at this time
; 6    06 | Direct Console I/O     | (E)=0FFH is input| (A)=character    | Nothing further at this time
;         |                        | (E)=chr is output|      ****        |
; 7    07 | Get IOBYTE             |      ****        | (A)=IOBYTE       | Nothing further at this time
; 8    08 | Set IOBYTE             | (E)=IOBYTE       |      ****        | Nothing further at this time
; 9    09 | Display Console String | (DE)=string addr |      ****        | Done
;10    0A | Input Console String   | (DE)=string addr | (A)=# chr input  | Done
;11    0B | Get Console Status     |      ****        | (A)=000H idle    | Done
;         |                        |                  | (A)=0FFH ready   |
;12    0C | Get CP/M Version Number|      ****        | (HL)=Version #   | Done
;13    0D | Reset Disk Subsystem   |      ****        |      ****        | Nothing further at this time
;14    0E | Select Disk Drive      | (E)=disk number  |      ****        | Done
;15    0F | Open a File            | (DE)=FCB address | (A)=dir code     | Done
;16    10 | Close a File           | (DE)=FCB address | (A)=dir code     | Done
;17    11 | Search for File        | (DE)=FCB address | (A)=dir code     | Done
;18    12 | Search for Next        |      ****        | (A)=dir code     | Done
;19    13 | Delete File            | (DE)=FCB address | (A)=dir code     | Done
;20    14 | Read next Record       | (DE)=FCB address | (A)=error code   | Done
;21    15 | Write next Record      | (DE)=FCB address | (A)=error code   | Done
;22    16 | Create New File        | (DE)=FCB address | (A)=dir code     | Done
;23    17 | Rename File            | (DE)=FCB address | (A)=dir code     | Done
;24    18 | Get Login Vector       |      ****        | (HL)=login vector| Nothing further at this time
;25    19 | Get Logged Disk Number |      ****        | (A)=logged disk  | Done
;26    1A | Set R/W Data Buff Addr | (DE)=buffer addr |      ****        | Done
;27    1B | Get Allocation Vector  |      ****        | (HL)=alloc vector| Done
;         |                        |                  |      address     |
;28    1C | Write Protect Disk     | (E)=disk number  |      ****        | Nothing further at this time
;29    1D | Get Read Only Vector   |      ****        | (HL)=R/O vector  | Nothing further at this time
;30    1E | Set File Attributes    | (DE)=FCB address | (A)=dir code     | Nothing further at this time
;31    1F | Get Addr of Disk Parms |      ****        | (HL)=parm addr   | Done 
;32    20 | Get/Set User Select    | (E)=0FFH get     | (A)=current user | MISSING
;33    21 | Read Random Record     | (DE)=long FCB adr| (A)=error code   | Done - Issue with setting CR - Should be the current, not the next.
;34    22 | Write Random Record    | (DE)=long FCB adr| (A)=error code   | Done - Issue with setting CR - Should be the current, not the next.
;35    23 | Get Size of File       | (DE)=long FCB adr| (r0-2=rec cnt)   | Written for Sparse files, does not align with CP/M standards. 
;36    24 | Set Random Record Num  | (DE)=long FCB adr| (r0-2=rec numb)  | Complete but untested and unvalidated
;37    25 | Reset Drive            | (DE)=drive vector|      ****        | Nothing further at this time- should add Allocation Vector Generation 
;38    26 | Not used               |                  |                  |
;39    27 | Not used               |                  |                  |
;40    28 | Write Random with      | (DE)=long FCB adr| (A)=error code   | MISSING
;-------------------------------------------------------------------------
;    The  technical means required to "use" or interface  to  the 
;CP/M system for each function contains a certain common structure 
;that  will  be discussed here.  The base memory page  of  a  CP/M 
;system memory map includes,  at a specific memory address, a JUMP 
;instruction  to the CP/M BDOS entry point.  For most CP/M systems 
;this is address 00005H.  To accomplish BDOS I/O the number of the 
;function  is  placed  into the (C)  register.  If  the  parameter 
;requires  input  parameters,  then they are passed  in  the  (DE) 
;register  pair  or  the individual (E)  register  depending  upon 
;whether the parameter is a word or byte value. Result information 
;returned  by some functions is sent back to the users program  in 
;either  the (A) register or the (HL) register pair depending upon 
;if  the  value is a byte or word.  The following  simple  program 
;segment demonstrates the scheme used to output the 26  characters 
;A-Z to the console screen through the use of function number 2.


EQU	DEFAULT_DISK,$0004						; Where we store the currently selected disk. 0=A, 1=B, 11=L and so on. 
EQU BDOS,$0005
.ORG $F000



EQU		MAXCALL ,	$30				; highest call.		Set to one above the hightest call number. 	Later will add special calls. 
EQU		DEBUG ,		$0A				; Debug port. Track calls. 

BDOSENTRY:							; BDOS Calls enter here. 
			
					; rem Enter this location with as as the call code.							
BIOSVECTOR:							; Check C here to avoid calling too big of a table.
		LD		HL,$0000
		ADD		HL,SP				; save the SP
		LD		(SAVESTACK),HL
		LD		SP,$FF80			; New stack location for BDOS call.
		LD		HL,UNKNOWNCODE		; Where we want to exit to ( to restore stack )
		PUSH	HL

		LD		A,C
		CP		MAXCALL				; Highest routine that can be called.
		RET		NC					; Just return if the call doesn't exist. 
		PUSH	DE					; Store the instruction for later.
		LD		HL,BDOSTABLE
;		RL		A					; Double it.
;		AND		$FE					; And mask any carry. We have a maximum of 128 options.
		SLA		A					; replace RL A and AND $FE with this - SLA will zero the incomming bit. 
		ADD		A,L
		LD		L,A
		XOR		A					; Clear A.
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

		LD		(SAVEHL),HL
		LD		HL,(SAVESTACK)
		LD		SP,HL				; Restore the SP. 
		LD		HL,(SAVEHL)
		RET							; Just exit here. 

SAVESTACK:	DW	$0000				; Place to save the stack just in case. 
SAVEHL:		DW	$0000				; Place to save HL while restoring the stack. 
	
BDOSTABLE:
		DW	RESET				;0
		DW	CONSOLEIN			;1
		DW	CONSOLEOUT			;2
		DW	READERIN			;3
		DW	PUNCHOUT			;4
		DW	LISTOUT				;5
		DW	DIRECTCONSOLE		;6
		DW	GETIO				;7
		DW	SETIO				;8
		DW	PRINTSTRING			;9
		DW	READCONSOLESTRING	;0A
		DW	GETCONSOLESTATUS 	;0B
		DW	RETURNVERSION		;0C
		
		;						Disk Systems. 
		
		DW	RESET_DISK			;0D
		DW	SELECT_DISK			;0E
		DW	OPEN_FILE			;0F
		DW	CLOSE_FILE			;10
		DW  MATCH_FIRST			;11
		DW	MATCH_NEXT			;12
		DW	DELETE_FILE			;13
		DW READ_NEXT			;14
		DW WRITE_NEXT			;15
		DW CREATE_NEW_FILE		;16
		DW RENAME_FILE			;17
		DW Get_Login_Vector		;18
		DW Get_Logged_Disk		;19
		DW Set_DMA_Address		;1A
		DW Get_Allocation_Vector;1B

		DW	Write_Protect_Disk	;1C
		DW	Get_RO_Vector		;1D
		DW	Set_File_Attributes	;1E
		DW	Get_DPB_ADDR		;1F
		DW	User_Code			;20
		DW	Read_Random			;21
		DW	Write_Random		;22
		DW	Compute_File_Size	;23
		DW	Set_Random_Record	;24
		DW	Reset_Drive			;25
		DW	Access_Drive		;26
		DW	Free_Drive			;27
		DW	Write_Random		;28
		
		; End of BDOS routines, let's temporarily create my own.
		
		DW	Dry_Write			;29 Same as Write_Next (Function $15) but doesn't store the data. 
;		DW	Allocate			;30 Similar to Write Random, but allows the allocation to be specifically written to the file. 


.equ	ADRIVE,	$00
.equ	LDRIVE,	$0B
.equ	MDRIVE,	$0C
.equ	NDRIVE,	$0D
		
				
RESET:
XOR		A
OUT		(14),A						; Force PID to zero. 
LD		A,$01						; Allocation 1 will go into lowest page.
LD		B,$00						; Page 0
LD		C,$0F						; Port for paging - Current ID is set. 
OUT		(C),A						; Page out the lower ROM and page in RAM at 01000 - Leave at F0000 for the moment. It's all presently RAM anyway, 
 
LD		HL,BDOSENTRY
LD		A,$C3
LD		($0005),A					; Set jumps
LD		($0000),A
LD		($0006),HL					; Install BDOS call vector into location 0005. 
LD		HL,$FC03					; BIOS WARM REBOOT.
LD		($0001),HL					;  For RST0. 

LD		A,LDRIVE					; System reset. 
LD		(DEFAULT_DISK),A			; Select drive L: as default.

LD		SP,$FFFF					; Set up Stack Pointer for the moment.
								
									; Clear up RSTs to automatically return if called, including 40 and 48. 
		LD	A,$C9					; Return Opcode.
		LD	($0008),A				; Unassigned.
		LD	($0010),A				; Unassigned.
		LD	($0018),A				; Unassigned.
		LD	($0020),A				; Unassigned.
		LD	($0028),A				; Unassigned.
		LD	($0030),A				; Unassigned.
		LD	($0038),A				; Unassigned.
		LD	($0040),A				; Video
		LD	($0048),A				; Network
		LD	($0050),A				; Return
								; Default page-0 returns done. 	
								; We can load the CCP now...
	
		CALL	CCP_LOAD			; Load the CCP here - Add any debug statements next, since the debug routines are in the CCP. 

		
		LD		E,MDRIVE			; Disk M
		CALL	SELECT_DISK
		CALL	RESET_DISK		
		
		LD		E,LDRIVE			; Disk L
		CALL	SELECT_DISK
		CALL	RESET_DISK	

		LD		E,ADRIVE			; Disk A
		CALL	SELECT_DISK
		CALL	RESET_DISK			
		
		LD		E,NDRIVE			; Disk N:
		CALL	SELECT_DISK
		CALL	RESET_DISK

;		JP 		$D003 ;				; start up the CCP		
		JP		$D000				; Start up the CCP with initialise command. 


;;;JP		BOOT					; Via the BIOS Cold boot. 


;INPUT FROM CONSOLE KEYBOARD: Function 1.
;
;     This  function waits for and reads in a character  from  the 
;console  device keyboard.  The operator typed character is echoed 
;automatically back to the console display if the character is  an 
;ASCII  printable  character  (020H to 07EH) or it is  a  carriage 
;return,  line  feed,  back  space,  or tab.  Note that  the  BDOS 
;automatically expands tabs to columns of eight  characters.  Upon 
;outputting  the  character  for the echo,  a check  is  made  for 
;console  start/stop,  CTL-S,  and if so the console input routine 
;does not return to the users program until another arbitrary  key 
;is depressed.

CONSOLEIN:
	CALL	CONIN						; Later need to do some more, and call this, so we can analyse the character. 
	LD		C,A
	PUSH	AF
	CALL	CONOUT						; Need to do some checks here later.
	POP		AF
	RET
	;  
	
CONSOLEOUT:
	LD		C,E						; 02 - Console Outs. 
	JP		CONOUT					; 	Call Bios. 
READERIN:							; 03 - Additional serial input... Might be able to use this for network files.
	IN		A,(2)
	ret
PUNCHOUT:							; 04 - Punch card output... Might be able to use this for network files.
	LD		A,E
	OUT		(2),A
	ret
LISTOUT:							; 05 - List device - use for default printer. 
	LD		A,E
	OUT		(3),A
	ret
	
	
	

;DIRECT USER INTERFACE TO CONSOLE: Function 6.
;
;     Some  programming  applications require that  the  BDOS  not 
;monitor  the  input/output  character  stream  as  is  done  with 
;functions  1  & 2.  To allow for these functions the  direct  I/O 
;function is supported. 
;  Function to perform direct console i/o. If (C) contains (FF)
; then this is an input request. If (C) contains (FE) then
; this is a status request. Otherwise we are to output (C).
; 6    06 | Direct Console I/O     | (E)=0FFH is input| (A)=character    |
;         |                        | (E)=chr is output|      ****        |

DIRECTCONSOLE:					; 06 - Direct Console Access - Need to understand how this works. 

;OUT (10),A			; Let's see what is going on here.  (debug port )

	INC		E					; Let's test C. If we get a carry, it's input.
	JR		Z,DIRECTCONSOLEINPUT	; There won't be a carry - but ZERO gets changed. If it was FF, then it's input 
DIRECtCONSOLEOUTPUT:
	DEC		E
	LD		A,E
	AND		$7F			; mask high bits.
	LD		C,A

	CALL	CONOUT					; Send out the character in E. 

	ret							

DIRECTCONSOLEINPUT:	

	CALL	CONST					; Check the console status.
	OR		A						; Test for zero.
	RET		Z						; exit if so.
	CALL	CONIN					; We know something is there.

	RET								; Return it. 
	
	
	ret
GETIO:							; 07 - Get I/O - Need to understand how this works. 
								; OK, I am going to change this, because I want location $0003 so I can DI, then JUMP. 
	ret
SETIO:							; 08 - Set I/O - Need to understand how this works. 
								; I'll work out where to stick this byte later 
	ret
	
	
;PRINTING STRINGS OF CHARACTERS TO THE CONSOLE: Function 9.
;
;     Message  string  sequences of characters to be sent  to  the 
;console  are quite common in  applications  programming.  Typical 
;uses  may be for user prompt messages,  program sign-on  messages 
;etc.  The  BDOS  provides  a convenient mechanism  to  allow  the 
;programmer  to  output a whole string of characters  rather  than 
;having  to  loop with single character  outputs.  The  string  is 
;intended  to  be stored in consecutive memory locations  and  end 
;with  the  ASCII '$' character.  The (DE) registers are  used  to 
;point to the start of the string.  The '$' signals the end of the 
;string  to  display and is not sent to the  console.  The  output 
;bytes  may be any 8-bit value but many times the hardware  driver 
;BIOS  routines automatically strip off the upper bit of the byte. 
;Upon  output of each character the printer echo flag within  BDOS 
;is  checked (CTL-P) and if set the character is also sent to  the 
;printer  peripheral  device.  Note that  the  BDOS  automatically 
;expands  output  tabs  to  columns  of  eight  characters.   Upon 
;outputting  each  character a check is made for input of  console 
;start/stop,  CTL-S,  and if so the console string output  routine 
;does  not return to the users program until another arbitrary key 
;is depressed.	
	
PRINTSTRING:					; 09 - Display a string ending with $ on the console. String in DE
	LD		A,(DE)
	CP		'$'
	RET		Z
	PUSH	DE
	LD		C,A
	CALL	CONOUT
	POP		DE
	INC		DE
	JP		PRINTSTRING



;READING A STRING OF CHARACTERS IN FROM KEYBOARD: Function 10.
;
;     The CP/M console command processor (CCP) assumed to be  vary 
;familiar  to  most CP/M system operators allows buffered  command 
;input with editing features.  It turns out that this operation is 
;a  much needed function for getting in strings of text  from  the 
;operator console.  Use of this function allows standardization of 
;the command input functions so that the operator can easily learn 
;the  editing key functions.  It also removes the pain of  writing 
;the  same  function  over  and over  again  by  the  applications 
;programmer.  The  read string command inputs the edited text to a 
;buffer  pointerd  to  by  the  (DE)  register  pair.  The  caller 
;specifies  the  maximum length desired and the BDOS  returns  the 
;actual  length  of string entered if carriage return  is  entered 
;prior to exceeding the maximum input length.  The input length is 
;returned  in  both the (A) register and as part  of  the  buffer. 
;Bytes  in the string buffer past the end of the entered text  are 
;uninitialized. The example shown below gives an assembly language 
;view  point  of the buffer structure and how to program an  input 
;function.
;
;     The  editing functions supported are the  following  control 
;and/or special characters:
;
;              rub/del        removes and echos the last entered char
; prob works    ctl-C   3      initiates system reboot if first char
;   done        ctl-E   5      echos a CR & LF to console without
;                               putting them into buffer
;   done        ctl-H   8      (or back space key) back spaces one char
;                               removing last entered character
;	done		ctl-I	9		TAB - Goes to next block of 8. 
;   done        ctl-J   10      (or line feed key) terminates line input
;   done        ctl-M   13      (or carriage return) terminates input
;  untested     ctl-R   18      retypes currently entered characters 
;                               under current line
;  untested     ctl-U   21      deletes all of currently entered data
;                               and restarts buffer input on new line
;  untested     ctl-X   24      deletes all of currently entered data
;                               and restarts buffer input on same line
;
; Enter with DE pointing to the buffer ( will be same on exit ).
; Exit with A showing number of characters


CRLF:
OUTCRLF:
		LD		C,$0D
		CALL	CONOUT			; Call the BIOS direct.
		LD		C,$0A
		CALL	CONOUT				; Call the BIOS direct.
		RET


PRINT_INPUT_BUFFER:
		LD		A,(IX+1)			; Check number of characters in line.
		OR		A
		RET		Z					; Just exit straight away if there's no characters. 

		CALL	CLEAR_CONSOLE_LINE
		LD		HL,(INPUT_BUFFER_START)	; We call this from CONSOLE_LINE so IX+0=len, IX+1=chars.
		
		INC		HL
		INC		HL					;Add two to reach the first character.

PRINT_INPUT_BUFFER3:
		LD		C,(HL)				;Print each character.
		CALL	CONOUT				; Doesn't affect HL... At least not yet. 
		INC		HL
		DJNZ	PRINT_INPUT_BUFFER3		
		RET							; Reprint the current input buffer. 

CLEAR_CONSOLE_LINE:						   
		LD		A,(IX+1)			; Number of characters in line.
		OR		A					; Check character count here, because we call it from elsewhere. 
		RET		Z					; No characters, just return

		LD		C,$0DH				; Carriage Return ( without line feed ). 
		CALL	CONOUT

		LD		B,(IX+1)			; Number of characters in print buffer.
PRINT_INPUT_BUFFER2:
		LD		C,' '
		CALL	CONOUT
		DJNZ	PRINT_INPUT_BUFFER2
		RET

; Input Buffer Format - Get it in DE.
; DE+Offset
; 00	-	Should be preset with the length of the input buffer. Simulator CR on hitting end.
; 01	- 	Character Count. 
; 02-nn -	Characters. 

INPUT_BUFFER_START:	DW		$00		; Start location of buffer. 
BPOS:				DW		$00		; Start with the start of the input buffer, though this will move.

READCONSOLESTRING:					; Function 0A - Need to understand how this works. 
CONSOLE_LINE:						; Get a line from the console. 
		LD		(INPUT_BUFFER_START),DE
		PUSH	DE
		POP		IX					; Copy to IX so we can reference the length(+0) and count(+1)

CONSOLE_LINE_RESTART:
		LD		(IX+1),0			; Clear character counter. As of now, there are 0 characters stored. 
		LD		HL,(INPUT_BUFFER_START)	; Reset the counter in memory.
		INC		HL
		INC		HL					; Now point it at the first character location.
		LD		(BPOS),HL			; Store current buffer position. 	
		
CONSOLE_LINE_NEXT:
;;;		LD		(BPOS),HL			; Store current buffer position. 
		
CONSOLE_REDO_CHAR:
		LD		HL,(BPOS)			; Retrieve place in buffer. 
		CALL	CONIN				; Use the BIOS routine - since we will make this routine BDOS later. 

		CP		$0D					; Did we get an EOL? Let's check a few characters.
		JP		Z,CONSOLE_LINE_ENTER					; EXIT if ENTER DETECTED.
		CP		$0A
		JP		Z,CONSOLE_LINE_ENTER					; Treat 0A same as 0D. 
		CP		$03					; Ctrl-C
		JP		Z,$0000				; Warm Boot ( Ctrl-C )
		CP		$05					; Echo two characters.
		JR		NZ,INPUT_BUFFER1
		CALL	CRLF
		JR		CONSOLE_REDO_CHAR	; Print CR/LF and ignore current line and the extra input 

INPUT_BUFFER1:
		CP		$12					; Ctrl-R - Retype the buffer on the next line.
		JR		NZ,INPUT_BUFFER2
		CALL	CRLF				; Head to new line
		CALL	PRINT_INPUT_BUFFER			; Print the current line
		JR		CONSOLE_REDO_CHAR			; Abd

INPUT_BUFFER2:
		CP		$15					; Ctrl-U - Restart Input on a new line.
		JR		NZ,INPUT_BUFFER3
		CALL	CRLF
		JR		CONSOLE_LINE_RESTART		; Restart from the beginning.

INPUT_BUFFER3:
		CP		$18					; Ctrl-X - Restart Input on SAME line. 
		JR		NZ,INPUT_BUFFER4
		CALL	CLEAR_CONSOLE_LINE
		LD		C,$0DH
		CALL	CONOUT						; Restart to the current line.
		JR		CONSOLE_LINE_RESTART		; And restart from the beginning. 

INPUT_BUFFER4:		
		CP		$09					; Tab. Jump to the 1st character of the next 8-character column. 
		JR		NZ,	CONSOLE_NOTAB	

		LD		A,(IX+0)
		SUB		(IX+1)				; Check how close we are to the end of the buffer.
		CP		$08					; Compare to 8... If we're not at least 8 characters from the end of the buffer, exit.
									; 7-8 = carry.  So a carry means 8 
		JR		C,CONSOLE_TOO_LONG	; Won't be a carry if we're past the buffersize-5.
	
		LD		A,(IX+1)
		DEC		A
		AND		$07
		CPL		A					; Make the difference negative.
;		INC		A					; IMPORTANT, if I don't put this here, it stops every 8th character and waits... eg,12345..8........8
									; If I INC here, then it stops on the 9th position, eg, 8 locations, and I'm in the next column on the 9th. 
									;	eg, One.. A>One.....Two.....Three...Four....Five....Six.....Seven...Eight...Nine....Ten.....Eleven..Twelve..Thir
		
		AND		$07					; Mask for 8 spaces. $0F would be 16 spaces/tab. $03 would be 4 spaces/tab. 
		JR		NZ,	CONSOLE_NO_TAB_STUFFING	; Don't stuff an extra space there if it's just one space to go.
		LD		A,$08				; Otherwise space until we reach the 8th character
CONSOLE_NO_TAB_STUFFING:
		LD		B,A					; Store the count for the next op. 

CONSOLE_TAB_REPEAT:					; Tab is repeated spaces. 
		LD		HL,(BPOS)			; Retrieve place in buffer. 
		LD		A,' '				; reset A to be the space. 
		LD		(HL),A				; Draw the space.
		INC		HL					; Increment storage place location. 
		LD 		(BPOS),HL			; And update the pointer. 
		INC		(IX+1)				; One more character to record. 

		PUSH	BC
		LD		C,' '				; To see automatic TABs put a different character here, eg, '>' or '.'
		CALL	CONOUT				; Send to the console 
		POP		BC

		DJNZ	CONSOLE_TAB_REPEAT
		
		LD		HL,(BPOS)
		JR		CONSOLE_LINE_CHECKBUFFER	; Just bypass all other processing. We've done enough. 		

CONSOLE_TOO_LONG:					; Just single space if we drop out of the above. 		
		LD		A,' '				; Turn a TAB into a space. Now I have to deal with the local echo. 

CONSOLE_NOTAB:	
		CP		$08					; Was it a backspace?
		JR		NZ,CONSOLE_LINE_STORE	; if it's not a Backspace, then store it.
		
CONSOLE_BACKSPACE:					; Null label. We don't jump here, but it separates code. Backspace below. 
		LD		A,(IX+1)			; Number of characters in buffer.
		OR		A	
		JP		Z,CONSOLE_LINE_NEXT		; If so, ignore the key and wait again.
		
		LD		HL,(BPOS)
		DEC		HL					; And go back a full character to the prior one. 
		LD		(HL),$0				; And delete it.
		LD		(BPOS),HL			; Store the updated pointer
		DEC		(IX+1)				; Character count gets shorter. 

		LD		C,$08
		CALL	CONOUT				; Echo the BS character we just accepted. Use the BIOS output routine. 
		LD		C,' '
		CALL	CONOUT				; Print a space to mask what we just erased.
		LD		C,$08
		CALL	CONOUT				; And then backspace again
	
		JR		CONSOLE_LINE_CHECKBUFFER	; Do not store the character in A at the moment. Do not increment HL. 
		
CONSOLE_LINE_STORE:		
		LD		HL,(BPOS)			; Get current buffer position.
		LD		(HL),A				; Store the character.
		INC		HL					; Move to next place in buffer. 
		LD		(BPOS),HL
		INC		(IX+1)				; Increment character counter.
	
		LD		C,A
		CALL	CONOUT				; Echo the character we just accepted. Use the BIOS output routine. 
		
CONSOLE_LINE_CHECKBUFFER:			; Jump here to avoid storing the character. Just check for end of buffer as a final test.

		LD		A,(IX+0)			; Length of buffer
		SUB		(IX+1)				; Characters in buffer. 
		JR		Z,CONSOLE_LINE_ENTER	; Let's treat end of buffer as an unplanned enter. Either that or  we block more characters or make it bigger. 

		JP		CONSOLE_LINE_NEXT	; Then repeat from the top. 


CONSOLE_LINE_ENTER:
		CALL	CRLF				; Return and feed. ( next line )
		LD		DE,(INPUT_BUFFER_START)	; Start of character input buffer. 					
		LD		A,(IX+1)				; Number of characters in input buffer. 
		ret





;11    0B | Get Console Status     |      ****        | (A)=000H idle    |
;         |                        |                  | (A)=0FFH ready   |
GETCONSOLESTATUS: 				; 0B - Neet to understand how this works. 
		CALL	CONST					; CHeck the console status.
		ret								; Return the BIOS output.




;12    0C | Get CP/M Version Number|      ****        | (HL)=Version #   |
RETURNVERSION:					; 0C - Return version. Need to know what I should return.
		LD		HL,$0022		; Version 2.2, CP/M. Might make this different later for LokiOS. 
		ret



;13    0D | Reset Disk Subsystem   |      ****        |      ****        |
RESET_DISK:
		LD		A,(DEFAULT_DISK)	; Make sure we're working with the current default disk. 
		LD		C,A
		LD		B,0
;;;		LD		BC,$0B 			;Because L: is the default drive of the Loki system . 
		CALL	SELDSK_CALL		; Select the disk and remember the location of the DPH and DPB
		
		LD		BC,$00
		CALL	SETTRK_CALL
		LD		BC,$00
		CALL	SETSEC
		LD		BC,$0080
		CALL	SETDMA
;		CALL	READ

		CALL	Get_Allocation_Vector			; Build the tables for the selected drive and return the information here. I should probably do this on reset, not get. 

		ret						;Add to this later. Disks are handled differently here. Load defaults. 
		
	
; Not a function, but it's relevant from here on...
; BUILD THE ALLOCATION TABLE... Store all the allocations in the file table.
; L: Drive = 32 allocations of 
; Find Directory Sector of Disk....
; Maybe drop this in below. 

;14    0E | Select Disk Drive      | (E)=disk number  |      ****        |
Disk_Parameter_Header:	DW		$00
Disk_Parameter_Block:	DW		$00
SELECT_DISK:
		LD		A,E					; Disks start at 0 for A ( not like the FCB byte )
		AND		$0F
		LD		(DEFAULT_DISK),A	; Store the default disk. ( or repeat it )
		LD		C,A		
		LD		B,$0
		CALL	SELDSK_CALL		; Select the disk and remember the location of the DPH and DPB
		
;;;		CALL	READ_DIRECTORY		;  Don't read the directory here. Do it as another function, eg , the next 15 - Open File.
		
		ret

;15 
;Function 15: Open File
;	Register	Value
;Entry	C	0FH
;DE	FCB Address
;Return	A	Directory Code

;The Open File operation is used to activate a file that currently exists in the disk directory for the currently active user number. 
;The FDOS scans the referenced disk directory for a match in positions 1 through 14 of the FCB referenced by DE (byte s1 is automatically zeroed)
; where an ASCII question mark (3FH) matches any directory character in any of these positions. Normally, no question marks are included, 
;and bytes ex and s2 of the FCB are zero.
;
;If a directory element is matched, the relevant directory information is copied into bytes d0 through dn of FCB, 
;thus allowing access to the files through subsequent read and write operations. The user should note that an existing file 
;must not be accessed until a successful open operation is completed. Upon return, the open function returns a directory code 
;with the value 0 through 3 if the open was successful or 0FFH (255 decimal) if the file cannot be found. If question marks occur 
;in the FCB, the first matching FCB is activated. Note that the current record, (cr) must be zeroed by the program if the file is 
;to be accessed sequentially from the first record. 

;dir code:  directory code:
;           0FFH=failed (e.g. file not found, directory full)
;           0,1,2,3 = success: offset into current DMA buffer, which
;           contains a directory sector, where the FCB can be found
;
;ret code:  return code -- 0=success, non-zero=failed
;
;
;  *  V1.4 none
; ** V1.4 initializes system and selects A: drive
;*** ret codes:
;    00 - no error
;    01 - reading unwritten data
;    03 - cannot close current extent
;    04 - seek to unwritten extent
;    05 - directory overflow (write only)
;    06 - seek past physical end of disk


;15    0F | Open a File            | (DE)=FCB address | (A)=dir code     |
OPEN_FILE:
		CALL	 NO_AFN				; Test for Ambiguous file names for open. Doesn't affect DE
		LD		A,$FF				; Preset for "Fail" code 
		RET		C					; Carry = Ambiguous file. Exit here now - Don't match further. Now Match First. 
		CALL	MATCH_FIRST_FILENAME	; Match the filename. A holds the return code. 0-3 is OK, FF is failed. 
		CP		$FF					; Did we get FF? FailFail?
		RET		Z					; Exit now with the fail code if it failed to open.
		CALL	MATCH_WRITE_FCB		; Otherwise copy the extent allocations back to the FCB
		LD		A,(DIR_CODE)		; Get the index back in A. It will be 0-3 or FF.

		ret
									

MATCH_FIRST_FILENAME:
		LD		(MATCH_FCB),DE		; Set this now - We read it back shortly, but that routine MATCH_SELECTED_DISK is called from elsewhere. STORE THE FCB location. 
		CALL	MATCH_SELECTED_DISK	; Set up the correct disk for the read, based on the FCB. ALSO sets up IX/IY for DPH and DPB. 
		CALL	PREREAD_DIRECTORY 	; First Entry of directory and set up variables for subsequent reads. 
		LD		DE,(MATCH_FCB)		; FCB for matching. Temp FCB since we're not calling it with this set up. 
		LD		A,$00				; First extent in record.  
	 	CALL	MATCH_FILENAME		; Now call the Match_Filename routine. If A=0,1,2,3 on exit, then it was found. If A=FF then it wasn't found. 					
		ret

MATCH_SELECTED_DISK:				; Select the disk we want to read the directory from. 
		LD		DE,(MATCH_FCB)		; Locate the current start of the FCB which details which disk we want - 0 = logged, otherwise 1=a, 2=b etc. 	
								
		LD		A,(DE)			;first byte of FCB we're trying to match tells us which drive to read. 00=default
		CP		'?'					; FCB ALL MATCHES... NO EXCEPTIONS. 
		JR		Z,OPEN_FILE_STAY	; Don't select another disk if there's a ? in the first byte of the FCB
		CP		$E5					; BLANK EXTENT - when we want to write a new one.
		JR		Z,OPEN_FILE_NOSELECTION	; Don't select any disk if it's a blank extent you're looking for - assume it's already selected. 
		
		OR		A
		JR		NZ,OPEN_FILE_SPECIFIED
		
OPEN_FILE_STAY:					; Special case- we have a ? or a E5 so we don't want to change.
		LD		A,(DEFAULT_DISK)
		INC		A
OPEN_FILE_SPECIFIED:
		DEC		A
		LD		C,A				; Transfer the selected disk to C. 
		CALL	SELDSK_CALL		; Select the Disk via the BIOS.						
		ret
OPEN_FILE_NOSELECTION:
		ret


SELDSK_CALL:
		CALL	SELDSK			; Select the Disk via the BIOS.
								; Now IY should hold the location of the DPH. 
								; Now IX Should hold the location of the DPB. 	
		LD		(Disk_Parameter_Header),IY			; We may want these later. 
		LD		(Disk_Parameter_Block),IX			; So store the Disk Parameter Header and the Disk Parameter Block 	
		
		ret

SETTRK_CALL:					; Called with BC = TRACK - Now we must add the offset from the DPB
					
	LD		IX,(Disk_Parameter_Block)			; So store the Disk Parameter Header and the Disk Parameter Block 	
;	LD		A,(IX+13)			; Location of OFF (Track Offset) for the selected disk. 
;	LD		(TRACKOFF_L),A
;	LD		A,(IX+14)
;	LD		(TRACKOFF_H),A


;	LD		(TRACK),BC
;	LD		IX,TRACKOFF_L
;	LD		A,(IX+0)
;	ADD		A,C
;	LD		C,A

LD	A,(IX+13)					; Lower byte of track offset.
LD	B,$00
ADD	A,C
LD	C,A							; Just for 256 tracks for test. 

	CALL	SETTRK				; Call the BIOS routine.
	ret
	



;
; NO AMBIGUOUS FILENAMES - Call to filter for $ and * etc. 
; ######################
; MAKE SURE WE DON'T AFFECT DE.
NO_AFN:								; test for ambiguous file name. 
		PUSH	DE
		POP		HL					; Move the FCB pointer to HL
		INC		HL					; First character of filename in FCB
		LD		B,11				; 11 characters in filename
NO_AFN_LOOP:
		LD		A,(HL)				; Retrieve filename,
		CP		'?'					; Is it a question mark????
		JR		Z,NO_AFN_AMBIG		; If we find any ?s there's a problem - Can't open wildcards.
		INC		HL
		DJNZ	NO_AFN_LOOP
		OR		A							; Return outcome in carry
		ret
NO_AFN_AMBIG:
		SCF							;If carry, then there was a ?
		LD		A,$FF				; ERROR OUT, because we can't find a file we can't match due to ??s
		ret		



;16    10 | Close a File           | (DE)=FCB address | (A)=dir code     |
CLOSE_FCB:	DW	$0000				; We can temporarily store the FCB of the extent we want to close. 
CLOSE_FILE:							; See if the extent exists,
									; Search for valid location... Need to adapt file search.
									; Then need to modify DMA area and WRITE FCB to indexed location.
									;
		LD		(CLOSE_FCB),DE		; Let's not lose our original handle for the FCB we want to write to the directory space. 	
		
		LD		A,(DE)				; Let's change disk first. Note: We're 1 higher than the SELDSK value. 
		LD		(MATCH_FCB),DE
		CALL	MATCH_SELECTED_DISK		; Call Select Disk to select the correct drive. 
		
		
		LD		DE,(CLOSE_FCB)			; Recover FCB pointer. 	
		CALL	MATCH_FIRST_FILENAME	;	See if the extent already exists.
		AND		%1111 1100				; 	mask for the response (remove the index)
		JR		Z,CLOSE_FILE_OK			; If the extent already exists, we can just rewrite and close it. No need to find a blank to write. 
		
									; If we got any kind of an error from the search for the extent, then we need a new blank extent to write to. 
									; Search for a blank extent to close. 
		LD		DE,MATCH_NO_FILE	; We don't need a full FCB, just one starting with a $e5 character.
		CALL	MATCH_FIRST_FILENAME ; Match the first filename that is not a file. HL = extent. 		
		CP		$FF					; Make sure got an extent to write, otherwise CLOSE ERROR, DISK FULL.
		JP		NZ,CLOSE_FILE_OK
		
CLOSE_FILE_DISKFULL_ERROR:			; If there was an error, Bomb-out here. 
		LD		A,$FF				; Indicate Disk Close Error or Write Error.
		ret							; Return with the error?
		JP		DISKFULL_ERROR		; Or bomb out?
		
CLOSE_FILE_OK:		
		LD		DE,(MATCH_EXTENT)		; The "empty" extent should be in Match_Extent in the DMA area, and we should be able to change and write it.
		LD		HL,(CLOSE_FCB)		; HL now holds the FCB we want to write to the empty extent. 
		LD		IX,(CLOSE_FCB)		; Use IX also - we need to change the FCB before writing it.
		
										; OK, I can't work out why I commented out the next two lines. I'll put them back in. 
										; I think it was related to testing against a non-DRI BDOS. 
		LD		A,(IX+CR)				; Copy the Count of Records to the Record Counter. Don't do this here - otherwise write random will cause issues. 
		LD		(IX+RC),A				; Make any other changes to extent here if desired. 
										; This sets the number in the file to the last written in the block.
										; Note sure how it handles multiple extent entries with less than full allocations. 
										; Seems to work like DRI BDOS now. Testing gives correct result. Especially with multiple extents per entry. 

		XOR		A						; Can add in user number here later since we're writing the first byte of the directory entry.
;		LD		(IX+DR),A				; THIS CAUSES A BUG - CLEARS THE DRIVE NUMBER IN THE FCB... 
;		LD		A,(HL)					; Transfer it. 
		LD		(DE),A
		INC		HL
		INC		DE
		
		
		LD		BC,$001F			; 32 bytes. - Minus the byte we did earlier manually. ( We transferred a zero instead )
		LDIR						; Change the extent ( copy the new FCB into it )
		
		CALL		WRITE		
		LD			A,(DIR_CODE)		; And return the code so we know it closed OK. 	
		ret

MATCH_NO_FILE:						; Default FCB that matches nothing. We use this to find an empty extent entry in the directory. 
		DB		$E5		; We'll write this via Select disk, and maybe some other places. 00 = Default. $0C= L: drive. Use default for this example. 
		DB		'********','***',0,0,0,0			; '*' should never appear in a filename and won't match by itself. 
		DB		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0		; Leave the rest blank - since anything still might modify it. 
		DB		0,0,0,0		
		
		
;17    11 | Search for File        | (DE)=FCB address | (A)=dir code     |
MATCH_FIRST:
		JP		MATCH_FIRST_FILENAME ; Like Match Next Filename, but also reads the directory first record and sets the extent index for the record. 

;18    12 | Search for Next        |      ****        | (A)=dir code     |
MATCH_NEXT:
	 	CALL	MATCH_NEXT_FILENAME	; Now call the Match_Filename routine. If A=0,1,2,3 then it was found. If A=FF then it wasn't found. 			
		ret

;19    13 | Delete File            | (DE)=FCB address | (A)=dir code     |
DELETE_FILE:
		LD		(MATCH_FCB),DE		; Set this now - We read it back shortly, but that routine MATCH_SELECTED_DISK is called from elsewhere. STORE THE FCB location. 
		CALL	MATCH_SELECTED_DISK	; Set up the correct disk for the read, based on the FCB. ALSO sets up IX/IY for DPH and DPB. 
									; We don't need the DPH from here - We can set IY to the File Control Block
									; Calling the first time sets up the registers we want to locate the DMA area. 
		LD		IX,(MATCH_FCB)		; locate the extent identifier.
		LD		(IX+EX),'?'			; And write a wildcard into the extent identifier to identify all extents of the match. 
		LD		DE,(MATCH_FCB)
		CALL	MATCH_FIRST_FILENAME	; Match the first. 
DELETE_FILES_LP:
		AND		%1111 1100
		RET		NZ					; Return if there's no valid file entry.
		LD		IX,(MATCH_EXTENT)
		LD		(IX+DR)	,$E5		; Mark as deleted. 
		CALL	WRITE				; And write the record back to the disk. Since the last operation from Match Extent read it, we can rewrite it. 
		CALL	SHOW_FILENAME
		LD		DE,(MATCH_FCB)
		CALL	MATCH_NEXT_FILENAME	; Match the first. 
		JR		DELETE_FILES_LP



SHOW_FILENAME:						; Display the file that matched. This is really only for debugging. 
		LD		HL,(MATCH_EXTENT)
		INC		HL
		LD		B,11				; 11 characters.
SHOW_FILENAME_LP:
		LD		C,(HL)
		PUSH	HL
		CALL	CONOUT
		POP		HL
		INC		HL
		DJNZ	SHOW_FILENAME_LP
		CALL	CRLF
		ret
		
		
;20    14 | Read next Record       | (DE)=FCB address | (A)=error code   |

THIS_RECORD:			DW	$00		; Which record in the allocation.
THIS_ALLOCATION:		DB	$00		; Which allocation in extent. ( 0 to 127 ) 
THIS_LOGICAL_EXTENT:	DB	$00		; Which LOGICAL extent in the physical extent. 
LOGICAL_BLOCK:			DW	$00		; Which Logical Block on the disk do we want?
LOGICAL_RECORD:			DW	$00		; Which Logical Record is this? 
PHYSICAL_TRACK:			DW	$00		; What physical track? only 8 bits, but make it 16 because we set it with BC. 
PHYSICAL_RECORD:		DW	$00		; What physical record? 16 bits. 


READ_NEXT:						; Read Sequential. Use FCB+20 for the counter. 
								; As we return from calling SELDSK in the BIOS.
								; Also IY is the DPH, IX is the DPB.  This gets called via Match Selected Disk.
								; On Entry, DE=FCB. 


		LD		(MATCH_FCB),DE		; Set this now - We read it back shortly, but that routine MATCH_SELECTED_DISK is called from elsewhere. STORE THE FCB location. 
		CALL	MATCH_SELECTED_DISK	; Set up the correct disk for the read, based on the FCB. ALSO sets up IX/IY for DPH and DPB. 
									; We don't need the DPH from here - We can set IY to the File Control Block

		LD		IY,(MATCH_FCB)		; Now we have the FCB in IY and the DPB in IX.
		LD		IX,(Disk_Parameter_Block)

		LD		A,(IY+CR)			; Retrieve the Current Record. 
		CP		(IY+RC)				; Compare to the Record Counter ( number of records in the extent )
		JP		Z,READ_NEXT_PASTEND	; If zero, we read past the end of the current file.
		JP		NC,READ_NEXT_PASTEND	; We should never already be looking to read right past the end of the file - check eg, Looking to read Record 8 of a 5 record file. 
		

		; Otherwise A holds the record we want. Let's set up the disk for READ now (convert from record to Track and Sector).
		CALL	ACCESS_RECORD		; Set up the allocation we want in THIS_ALLOCATION. 
									; Access_record will set Z flag if we want a single, or NZ if we want a double. 
		CALL	Z,READ_SINGLEALLOCATION	; If there's less than 256 allocations, read a single byte allocation. It will ensure we don't get NZ next. 
		CALL	NZ,READ_DOUBLEALLOCATION ; If there's more than 255 allocations, read a double byte allocation. 
		
		CALL	ACCESS_DISK			; Use THIS_ALLOCATION to identify which block we want, and locate it on the DISK surface.  
		
		CALL	READ					; And READ the sector(record). 	

		LD		IY,(MATCH_FCB)			; We want to update the current record now, since we read the last. 
		INC		(IY+CR)					; Increment the Current Record Counter in the FCB now before we make calls to set track and sector and exit. 
										; Be aware this was moved in WRITE operations to the last read, since IY gets changed. 
		ret



ACCESS_RECORD:						; Originally READ routine, common to READ and WRITE, takes a record and sets up the correct Track and Sector.
									; Takes a record number and returns with the Track and Sector selected and the DMA set up.
									; After that, either a read or write can occur. 
									; To call...
									; 	The record number ( within extent ) in A. 
		LD		A,(IY+CR)			; Look both ways, retrieve number here anyway JUST TO MAKE SURE WE HAVE IT. A= Current Record in FCB. 
									; Retrieve the Current Record. 									
									;	The File Control Block (FCB) pointer in MATCH_FCB and IY
									; 	The Disk Parameter Block (DPB) in IX
									; While it does iterate, it's primarily a drop-through routine, and a READ or WRITE can occur immediately after.
									; It will identify the correct allocation from a record, either 8 or 16 bit, then locate that allocation
									; on the disk surface, and align everything. 
									;							
		AND		(IX+BLM)			; AND record number with the BLOCK MASK - Which record in the allocation do we want?
		LD		(THIS_RECORD),A		; Store the record we want within the block.... ( Upper bits define which block ).
		XOR		A
		LD		(THIS_RECORD+1),A	; Make it a 16 bit number. Zero out the high byte. Now we know which record we want within the block or allocation.
	
		LD		A,(IY+EX)
		AND		(IX+EXM)			; Let's add the extent with the extent mask so we know how many extents should be in this directory entry.
		LD		(THIS_LOGICAL_EXTENT),A	; And store the logical extent number.


		LD		B,(IX+BSF)			; Block Shift Factor.
		LD		A,(IY+CR)			; Get hold of FCB again.
		RLA							; it's a 7 bit number and we want to connect it to the THIS LOGICAL RECORD number before shifting right.
		LD		HL,THIS_LOGICAL_EXTENT	; Get the location of This Logical Extent into HL.
		RR		(HL)				; Rotate it and get the lower bit in carry.
		RRA							; And rotate this into Bit7 (8th bit) of A, which holds the logical record to create a 12 bit record number = CR+EX.
		
ACCESS_NEXT_ALLOCATION:				; Work out which allocation in the record.
									; We want to add bits for the EXM ( Extent Mask ) here since we might choose later allocations in the extent table. 
									; FCB+EX AND EXM rotate in over 
		RR		(HL)				; Remember the Record Number is now a 12 bit number formed by CR (bit0-6) and EX(bit0-4)
		RRA
		DJNZ	ACCESS_NEXT_ALLOCATION	; Get the allocation number in the extent.
									; Remember we need DRM, If high byte <> 0 then we must use two bytes per allocation. 
									; 		DW	63		; DRM - Directory Max - Directory Entries = 64 entries. If 1024K allocation holds 32 directory entries then this means 2 allocations for directory. 
									; It's DRM_H... (IX+DRM_H)
									; Anyway, A holds which allocation we want.
									; And we divide Allocation / Sectors per track for Sectors and Tracks to reference record. ( BIOS is currently 128 byte sectors )
									;
		AND		$0F					; There are only 16 allocations in the FCB index / Extent table. Mask any higher bits. 
		LD		(THIS_ALLOCATION),A	; Store the allocation index. Which allocation in the table will we read/write. 
									; Now we're mostly done, but let's check if it's 8 bit or 16 bit allocation group size.
		LD		A,(IX+DSM_H)		;	If there's more than 256 blocks in Disk Size, then we need to use 16 bit entries.
		OR		A					; 	test for this and the zero flag will tell us whether it's 8 bit or 16 bit. (zero = 8 bit - any other value = 16 bit )
		ret							; We have the zero flag set or reset based on whether we want a double or single read. 
		
		
ACCESS_DISK:							; Now BC holds the disk block we want. Let's access the right track and sector. 
		LD		(LOGICAL_BLOCK),BC		; Store it for now. 							
		PUSH	BC
		POP		HL						; Put Disk Block into HL so we can calculate track and sector. 
		LD		B,(IX+BSF)				; First we need to convert to allocations.
ACCESS_NEXT_SECCALC:						; Work out what the logical sector is by multiplying by BSF Block Shift Factor
		OR		A						; Clear carry
		RL		L
		RL		H
		DJNZ	ACCESS_NEXT_SECCALC		; Multiply blocks by 128 byte sectors in the block. 
		LD		(LOGICAL_RECORD),HL		; Store the logical record ( place on disk as a record number )

		
		LD		E,(IX+SPT)				; SPT = Sectors Per Track. Really Records per track though. 
		LD		D,(IX+SPT_H)			; Put the number of allocations in a block into DE.  
										; so 32 sectors/track would be $20 in DE. 	
		XOR		A						; Clear carry and zero a.
ACCESS_NEXT_TRACKCALC:					; I'm going to be lazy for this calculation.
		SBC		HL,DE					; Count down one track at a time... This will be slow. Up to 40 typical cycles.
		JR		C,ACCESS_NEXT_GOTTRACK	; Exit when we get carry, means A holds the track number.
		INC		A
		JR		ACCESS_NEXT_TRACKCALC
ACCESS_NEXT_GOTTRACK:
		LD		(PHYSICAL_TRACK),A		; Which track?
		XOR		A
		LD		(PHYSICAL_TRACK+1),A	; 	It's a 16 bit number, but we only accept 8 bits, so zero the next byte. 
		ADD		HL,DE					; Undo last subtract that went below 0.
		LD		DE,(THIS_RECORD)		; Get the record within the allocation. (we determined this at the start)
		ADD		HL,DE					; Add this to HL
		LD		(PHYSICAL_RECORD),HL	; The remainder in HL is the record on that track.
										; At this point, we should have what we need to read a sector(record) from disk.
										
										; Disk was already selected, We just need to set Track and Sector and DMA
										;     should already be set, so then do a READ
		LD		BC,(PHYSICAL_TRACK)		; Get the track in BC
		CALL	SETTRK_CALL					; Call the BIOS to set it.
		LD		BC,(PHYSICAL_RECORD)	; Get the record in BC
		CALL	SETSEC					; Call the BIOS to set it.
		
		LD		BC,(CURRENT_DMA)				; DEBUG because I am not sure I set this?
		CALL	SETDMA	
		ret								; And exit. 




		
READ_SINGLEALLOCATION:				; Single byte allocations. 
		CALL	GET_SINGLE_VECTOR
		JP		SINGLE_RETURN
		
	
READ_DOUBLEALLOCATION:				; Two byte allocations
		CALL	GET_DOUBLE_VECTOR
		JP		DOUBLE_RETURN

; Include the write versions here as well so as to use JR instead of JP
		
WRITE_SINGLEALLOCATION:				; Single byte allocations. 
		CALL	GET_SINGLE_VECTOR

		LD		A,(HL)				; Is an allocation already existing? If we didn't pick up a valid allocation
		OR		A
		JR		NZ,SINGLE_RETURN	; If there's already a block in the allocation, then don't get another one.
		
		PUSH	HL					; Otherwise write a new valid block into the allocation. 
		CALL	BORROW_ALLOCATION	; Get a free block from the disk in DE
		POP		HL
		LD		(HL),E				; Just a single byte here. 	

		JP		SINGLE_RETURN
		
		
WRITE_DOUBLEALLOCATION:
		CALL	GET_DOUBLE_VECTOR	; Get HL to point to the allocation in the FCB. 
		
		LD		E,(HL)				; Check if it's a valid ( not zero ) allocation.
		INC		HL
		LD		D,(HL)
		DEC		HL
		LD		A,D
		OR		E
		JR		NZ,DOUBLE_RETURN	; If there's already an allocation in the FCB, just return it, otherwise get an allocation.
		
		PUSH	HL					; Otherwise get a valid allocation and write it in.. 
		CALL	BORROW_ALLOCATION	; Find the next free block on disk and get it in DE.
		POP		HL
		LD		(HL),E
		INC		HL
		LD		(HL),D
		DEC 	HL					; And write it into the FCB at (HL) as two bytes. 
		
		JP		DOUBLE_RETURN
		

GET_SINGLE_VECTOR:					; Get the vector for a single byte allocation in the FCB. 
		LD		HL,(MATCH_FCB)		; Get the base address of the FCB.
		LD		A,(THIS_ALLOCATION)	; Which allocation in the table?
		LD		DE,$10				; Allocation base of the FCB offset
		ADD		HL,DE				; Get the current address of the FCB allocation table. 
		AND		$0F					; There shouldn't be any upper nibble bits, but we don't want this screwing up. Let's look both ways.
		ADD		A,L					; And add the allocation offset that's in A. 
		LD		L,A
		XOR		A					; Clear A.
		ADC		A,H
		LD		H,A					; HL now points to a single-byte allocation.
		RET

SINGLE_RETURN:						; Transfer the contents of the vector in (HL) to BC.
		XOR		A					; Make sure A is ZERO, because we don't want READ DOUBLE following READ SINGLE. 
		LD		C,(HL)
		LD		B,A					; BC is the Logical Block.  Zero it out since we only really have an 8 bit number. 
		ret



GET_DOUBLE_VECTOR:					; Get the vector for a double byte allocation in the FCB. 
		LD		HL,(MATCH_FCB)		; Get the base address of the FCB.
		LD		A,(THIS_ALLOCATION)	; Which allocation in the table?
		LD		DE,$10				; Allocation base of the FCB offset
		ADD		HL,DE				; Get the current address of the FCB allocation table. 
		RLC		A					; Double A.
		AND		$0E					; There shouldn't be any upper nibble bits or bit 0, but we don't want this screwing up. Let's look both ways. 
		ADD		A,L					; And add the allocation offset that's in A. 
		LD		L,A
		XOR		A					; Clear A.
		ADC		A,H
		LD		H,A					; HL now points to the double-byte allocation. 
		ret
		
		
DOUBLE_RETURN:						; Transfer the contents of the double byte vector in (HL) to BC. 
		LD		C,(HL)
		inc		HL
		LD		B,(HL)				; BC is the Logical Block. 
		ret	
	
READ_NEXT_PASTEND:					; This is called when we either read the last record of the extent, or we read the last record used in the extent.
									; If there's more extents ( indicated by $80 in the (IY+RC) then open the next extent and increase the extent counter in FCB.
		LD		A,(IY+RC)			; Let's see if the extent is full.
		CP		$80
		LD		A,$01				; 01 = end of file.
		RET		NZ					; Return if not a full extent. 

									; Code to find new extent goes here. Update the Extent Counter, and do another search of the directory. 
		INC		(IY+EX)				; IY holds the FCB index. EX is the extent counter.

		LD		DE,(MATCH_FCB)
		CALL	OPEN_FILE			; "open" then next extent, just like we opened the first. 
		AND		%11111100			; Did we succeed? Check the return code, and should be between 00 and 03 if successful. 
;		JR		NZ,READ_NEXT_FILEERROR	; Exit if we didn't match the next extent. 
		JR		NZ,SEEK_TO_UNWRITTEN_EXTENT	; Exit if we didn't match the next extent. 

		XOR		A
		LD		(IY+CR),A			; Clear the current record in the FCB back to zero since we started a new extent. 
		
		LD		DE,(MATCH_FCB)		; Current FCB. Since we rewrite it, and we're a little recursive here. 		
		JP		READ_NEXT			; And try to read it again now we incremented the extent counter. 

READ_NEXT_FILEERROR:
		LD		A,$FF
		ret
		
SEEK_TO_UNWRITTEN_EXTENT:
		LD		A,$04				; Error code. 
		ret

;dir code:  directory code:
;           0FFH=failed (e.g. file not found, directory full)
;           0,1,2,3 = success: offset into current DMA buffer, which
;           contains a directory sector, where the FCB can be found
;
;ret code:  return code -- 0=success, non-zero=failed
;
;
;  *  V1.4 none
; ** V1.4 initializes system and selects A: drive
;*** ret codes:
;    00 - no error
;    01 - reading unwritten data
;    03 - cannot close current extent
;    04 - seek to unwritten extent
;    05 - directory overflow (write only)
;    06 - seek past physical end of disk


;These are defined within READ - Reuse them. Copy here for reference. 
;THIS_RECORD:	DW	$00		; Which record in the allocation.
;THIS_ALLOCATION:DB	$00		; Which allocation in extent. ( 0 to 127 ) 
;LOGICAL_BLOCK:	DW	$00		; Which Logical Block on the disk do we want?
;LOGICAL_RECORD:	DW	$00		; Which Logical Record is this? 
;PHYSICAL_TRACK:	DW	$00		; What physical track? only 8 bits, but make it 16 because we set it with BC. 
;PHYSICAL_RECORD:DW	$00		; What physical record? 16 bits. 
;
; additional variables for write. 
DISK_FCB_TO_CLOSE:		DW	$00		; Because the multiple changes and searches will affect which FCB we're looking at. 
WRITE_PROTECT_FCB:		DW	$00		; When we close and write new extents, we need to protect the original FCB location because MATCH_FCB will get reused. 
		
;21    15 | Write next Record      | (DE)=FCB address | (A)=error code   |
;DR F1 F2 F3 F4 F5 F6 F7 F8 T1 T2 T3 EX S1 S2 RC  .FILENAMETYP...
;AL AL AL AL AL AL AL AL AL AL AL AL AL AL AL AL  ...............
;CR R0 R1 R2  
WRITE_NEXT:
										; Algorythm 
										; Locate the correct allocation for the counter.
										; Check if there's a block in the allocation
										;		If not, get an allocation from the AllVect routine and put it in the allocation.
										;		Update the Allocation Vector at the same time as the allocation is assigned. 
										; Locate the sector/track for the block.
										; Write the DMA to the Disk
										; Update the counter in the extent. 
										; Exit. 
	CALL	DISK_GETPARAS				; Get the Track, Sector and Record for the write. Currently write is in code below. 

	CALL	WRITE						; And WRITE the track... Since it's all set up now. this is the BIOS write. 
	ret
	
	
DRY_WRITE:								;Alternative routine that does all the work, but doesn't write.
	CALL	DISK_GETPARAS				; Everything except the actual data write. 
	ret	

DISK_GETPARAS:
		LD		(MATCH_FCB),DE		; Set this now - We read it back shortly, but that routine MATCH_SELECTED_DISK is called from elsewhere. STORE THE FCB location. 
		CALL	MATCH_SELECTED_DISK	; Set up the correct disk for the read, based on the FCB. ALSO sets up IX/IY for DPH and DPB. 
									; We don't need the DPH from here - We can set IY to the File Control Block
		LD		IY,(MATCH_FCB)		; Now we have the FCB in IY and the DPB in IX.

		LD		A,(IY+CR)			; Retrieve the Current Record. 
		CP		$80					; if it's 80, we're at the end of the extent. Need to write the extent and open a new one. 
		JP		NZ,THIS_RECORDOK	; If we don't have 80 records, keep writing records into the current extent.
		
									; Now check if the physical extent is full of logical extent.s
		LD		A,(IX+EXM)			; Get EXM Extent Mask in A.
		AND		(IY+EX)				; Mask applied to the Extent Counter.
		XOR		(IX+EXM)			; Will become zero post-XOR if the extent lower bits were the same as Extent Mask, which means Physical Extent is FULL of logical extents. 
		JR		Z,DISK_EXTENT_FULL	; If it was full, we need to do something else. 
		
		INC		(IY+EX)				; Otherwise we can store another logical extent, so increase the counter.
		LD		(IY+CR),$00			; And clear the Current Record counter ( for this logical extent ). 
		JR		THIS_RECORDOK		; And continue with the next logical extent empty. 

DISK_EXTENT_FULL:					; Oops - extent is full. Let's write the FCB to disk. 

		LD		DE,(MATCH_FCB)		; Get the correct FCB. 
		LD		(WRITE_PROTECT_FCB),DE 	; Store the FCB we really want to operate from, because WE WILL CORRUPT THIS VARIABLE WITH RECURSIVE CALLS TO FILE OPERATIONS.
		PUSH	DE					; Calling CLOSE will corrupt the value of MATCH_FCB	

		LD		A,(IY+CR)				; Copy the Count of Records to the Record Counter. Otherwise we won't save file size. 
		LD		(IY+RC),A				; Make any other changes to extent here if desired. 
		
		CALL	CLOSE_FILE			; We can use this to write the current extent also... Close the current extent. ( Doesn't close the file as much as it writes the current extent )

		POP		IY					; I think IX already has it, but that might change. Retrieve the FCB pointer into IT. 
									; Let's clean up the FCB. 
		LD		(MATCH_FCB),IY		; Resave the current FCB location. 
		INC		(IY+EX)				; Increment the extent counter in the current FCB.
		LD		B,$14				; Clear the FCB. ( Random write might need to change this - let's leave the extent counter and 3 random bytes alone )
DISK_EXTENT_FULL1:
		INC		IY
		LD		(IY+EX),$00			; Maintain the offset as we step IY through the bytes, 
		DJNZ	DISK_EXTENT_FULL1	; And cycle through the next $14 bytes, which also clear the CR Current Record byte. 
		
		LD		DE,(WRITE_PROTECT_FCB)
		CALL	CREATE_NEW_FILE		; Reserve the new extent for this FCB. 
		
		LD		DE,(WRITE_PROTECT_FCB)	; Recover the original FCB one more time and store in DE. 

		JR		DISK_GETPARAS		; And restart this routine to begin writing the next extent. 
		
THIS_RECORDOK:						; A holds the record we want. First locate the allocation number ( numerical, not logical ).

		CALL	ACCESS_RECORD		; Set up the aim at the current record. Same as read - Locate the allocation vector int he FCB.

		CALL	Z,WRITE_SINGLEALLOCATION	; If there's less than 256 allocations, read a single byte allocation. We need to see if there's already an allocation to write. 
		CALL	NZ,WRITE_DOUBLEALLOCATION ; If there's more than 255 allocations, read a double byte allocation. We need to see if there's already an allocation to write. 		
									; This bit is different
		CALL	ACCESS_DISK 
		
		LD		IY,(MATCH_FCB)
		INC		(IY+CR)				; Increment the current record in the FCB to the next.
		ret
		

;22    16 | Create New File        | (DE)=FCB address | (A)=dir code     |
CREATE_NEW_FILE:					; I need to fix this later so that I check there's space on the disk.  Otherwise, write will assume the file is open. 
									; I might also need to write the initial BLANK FCB to disk.... That's probably a good idea actually.
									; in case another open operation tries to take that space. 
									; Also need to call this each time I open a new extent following a write operation. 
									
									; Should I check if the file exists before creating it? 
									; Maybe I should only "Force" a create with a close when it's called intentionally?
		CALL	 NO_AFN				; Test for Ambiguous file names for open. Doesn't affect DE
		LD		A,$FF				; Preset for "Fail" code 
		RET		C					; Carry = Ambiguous file. Exit here now - Don't match further. Now Match First. 

		PUSH	DE
		POP		HL					; Copy DE to HL to get FCB into HL.
		LD		BC,$0010
		ADD		HL,BC				; Get to the allocations in the FCB. 
		LD		B,$14				; Zero out the remainder when we create this file. 
CREATE_CLEAR:
		LD		(HL),$00
		INC		HL
		DJNZ	CREATE_CLEAR		; Zero out the allocations and the CR and R0,R1,R2
									
FORCE_CREATE_NEW_FILE:									
		CALL	CLOSE_FILE			; Ironic isn't it? I use Close File with a new blank FCB to open a new file... Can't close without opening I guess.
		ret
		
		
		
		
		
;DR F1 F2 F3 F4 F5 F6 F7 F8 T1 T2 T3 EX S1 S2 RC  .FILENAMETYP...
;AL AL AL AL AL AL AL AL AL AL AL AL AL AL AL AL  ...............
;CR R0 R1 R2                                      ....		
SECOND_FCB:			DB	$00		
SECOND_FILENAME:	DB	'nullfiledat'		
SECOND_FILENAME_CNT:DB $00,$00,$00,$00
SECOND_FILENAME_ALL:DB $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$00
SECOND_FILENAME_HND:DB $00,$00,$00,$00
		
;23    17 | Rename File            | (DE)=FCB address | (A)=dir code     |
RENAME_FILE:						; Similar to DELETE function, but different rewrite.
		LD		(MATCH_FCB),DE		; Set this now - We read it back shortly, but that routine MATCH_SELECTED_DISK is called from elsewhere. STORE THE FCB location. 
		CALL	NO_AFN				; No Ambiguous Filenames. 
		RET		C					; Carry = Ambiguous file. Exit here now - Don't match further. Now Match First if it was OK. 

;;;		LD		HL,$006D			; Second Filename Location in memory. 
		LD		HL,(MATCH_FCB)		; Point HL to the FCB which contains both filenames. 
		INC		HL					; And now to the filename itself.
		LD		DE,$10				; The second filename is $10 (16) characters later. 
		ADD		HL,DE				; HL now points to second filename in extent area of FCB.
		LD		DE,SECOND_FILENAME	; Where we store a temporary copy of the second filename. 
		LD		BC,11
		LDIR						; Set up the second FCB, though we only use the filename section here. 

		LD		DE,SECOND_FCB
		CALL	NO_AFN				; Copy "to" name must not be ambiguous.
		RET		C					; Exit if that's faulty also. 
		
		CALL	MATCH_SELECTED_DISK	; Set up the correct disk for the read, based on the FCB. ALSO sets up IX/IY for DPH and DPB. 
									; We don't need the DPH from here - We can set IY to the File Control Block
									; Calling the first time sets up the registers we want to locate the DMA area. 
		LD		IX,(MATCH_FCB)		; locate the extent identifier.
		LD		(IX+EX),'?'			; And write a wildcard into the extent identifier to identify all extents of the match. 
		LD		DE,(MATCH_FCB)
		CALL	MATCH_FIRST_FILENAME	; Match the first. 
RENAME_FILE_LP:
		AND		%1111 1100
		RET		NZ					; Return if there's no valid further file entry to rename. .

		LD		DE,(MATCH_EXTENT)	; The extent that matched the search function. 
		INC		DE					; Align the destination with the first character of the filename.	
		LD		HL,SECOND_FILENAME	; The filename we want to rewrite it as...
		LD		BC,11				; 11 characters
		LDIR						; Copy the filename
		

		CALL	WRITE				; And write the record back to the disk. Since the last operation from Match Extent read it, we can rewrite it. 
		CALL	SHOW_FILENAME
		LD		DE,(MATCH_FCB)
		CALL	MATCH_NEXT_FILENAME	; Match the first. 
		JR		RENAME_FILE_LP
		
		
;24    18 | Get Login Vector       |      ****        | (HL)=login vector|
Login_Vector: DEFB %0000 0001, %1111 1110 ; Low order first. 
Get_Login_Vector:
;		LD		HL,%1111 1110 0000 0001	; Drives A and L and M at the moment. Added N for NVM mid-memory. Add J K O and P 
										; Bit 0=A: Bit 15=P, 
										; Need to store this in memory so it can change. 
		LD		HL,(Login_Vector)
		LD		DE,Login_Vector
		ret
;25    19 | Get Logged Disk Number |      ****        | (A)=logged disk  |
Get_Logged_Disk:
		LD		A,(DEFAULT_DISK)
		ret
;26    1A | Set R/W Data Buff Addr | (DE)=buffer addr |      ****        |
Set_DMA_Address:
		LD		(CURRENT_DMA),DE	;
		LD		BC,(CURRENT_DMA)	; Move DMA address to BC and call BIOS. 
		JP		SETDMA
CURRENT_DMA:	DW	$00				; Store the current DMA as the BIOS and BDOS may move it. 



;27    1B | Get Allocation Vector  |      ****        | (HL)=alloc vector|
Get_Allocation_Vector:
		LD		DE,MATCH_ALL_FILES
	
		CALL	MATCH_FIRST_FILENAME		; Match the first file.

		CALL	CLEARALLOC					; After the match, we should have the right registers set up.
											; Also makes sure we know where the DPH and DPB is. 
													
		LD		A,(DIR_CODE)				; DIR_CODE (response) MATCH_EXTENT (Extent location in DMA )
		
Get_AV_Loop:								; We just loop here until we exit. 
		AND		%11111100					; Check that we got something?
;		RET		NZ							; If the directory is empty
		JR		NZ,GET_AV_DONE
		CALL	BUILD_TABLE					; Build the allocation table if there are files.
		CALL	MATCH_NEXT_FILENAME
		JR		Get_AV_Loop					; And keep searching until there are no more valid entries. 

GET_AV_DONE:
		CALL	Get_Next_Allocation		; Will have next available allocation in DE and HL. 
		
										; What if it's full? Do we do anything here? Not sure... Why are we calling Get_Next_Allocation - not sure why I did this. 
										; Think it's because we need to reset the table, but need to see if the routines write out entries in the table. 
										; See other calls for Get_Next_Allocation to check. Only Borrow_Allocation seems to write the allocation as used so this is OK.
										; Just need to consider efficiency of calling Get_next_allocation and find out why I did it this way since I forgot. 
		
		LD		HL,(ALLOCATION_VECTOR)	; Collect the vector table start vector into HL on the way out... 

		ret								; And exit. 

ALLOCATION_VECTOR:	DW	$0000			; Where we store the value we return. 
BUILD_TABLE_EP:	DW 	$0000				; Build Table Extent Pointer ( Will be in the extent )	
BUILD_TABLE:

		LD		IY,(Disk_Parameter_Header)	; Retrieve the DPH into IY. 
		LD		IX,(Disk_Parameter_Block)	; Retreive the DPB into IX. 
	
		LD		HL,(START_DMA)			; The Directory Scratchpad DMA. 
		LD		A,(DIR_CODE)			; Which entry in the scratchpad?
		RRCA
		RRCA
		RRCA							; Multiply by 32.
		ADD		A,$10					; Get to the allocations in the extent. 
		LD		E,A
		LD		D,$00
		ADD		HL,DE					; Add DE to HL, HL now holds the start of allocations in a valid extent. 

		LD		(BUILD_TABLE_EP),HL
		

		
		LD		B,8
		LD		A,(IX+DSM_H)			; Is it one or two bytes per allocation?
		OR		A						; Test A
		JR		NZ,BUILD_TABLE_LOOP
		RLC		B						; Double B if there's only one byte per allocation. 
BUILD_TABLE_LOOP:	
		LD		HL,(BUILD_TABLE_EP)
		LD		A,(IX+DSM_H)			; Is it one or two bytes per allocation?
		OR		A						; Test A
		CALL	Z,BUILD_SINGLEALLOCATION	; If there's less than 256 allocations, read a single byte allocation.
		CALL	NZ,BUILD_DOUBLEALLOCATION ; If there's more than 255 allocations, read a double byte allocation. 	
										; DE now holds the relevant allocation block, HL the next allocation vector.  
		LD		A,D						; Check the last allocation. 
		OR		E
		JR		Z,BUILD_TABLE_DONE		; If DE is 0, that is NOT an allocation. (Block 0 is always directory). 
		LD		(BUILD_TABLE_EP),HL

		PUSH	BC
		CALL	SET_ALLOC				; Set the allocations into the vector table. 
		POP		BC						; REcover counters.

		DJNZ	BUILD_TABLE_LOOP		; no more than 8 or 16. 	
BUILD_TABLE_DONE:
		ret								;



BUILD_SINGLEALLOCATION:					; Just one byte allocation. DONT TOUCH BC. 
		LD		E,(HL)
		LD		D,$00
		INC		HL
		XOR		A						; Make sure A is still cleared and is zero. So we don't call DoubleAllocation. 
		ret
BUILD_DOUBLEALLOCATION:					; DON"T TOUCH BC. Get a double allocation in DE. 
		LD		E,(HL)
		INC		HL
		LD		D,(HL)
		INC		HL
		ret

MATCH_ALL_FILES:						; Default FCB that matches everything, eg, DIR command. 
		DB		$00		; We'll write this via Select disk, and maybe some other places. 00 = Default. $0C= L: drive. Use default for this example. 
		DB		'????????','???','?',0,0,0			; Extra '?' = match all matching extents. 
		DB		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		DB		0,0,0,0

; Note		LD		(Disk_Parameter_Header),IY			; We may want these later. 
; Note		LD		(Disk_Parameter_Block),IX			; So store the Disk Parameter Header and the Disk Parameter Block 

CLEARALLOC:							
;		LD		B,64						; 64 bytes in table for 512 allocations max. 
		CALL	GET_AV_TABLE_SIZE			; Load BC with the AV table size. 

		PUSH	HL							; Temp store HL = Allocation table base. 
CLEARALLOC_LOOP:
		LD		(HL),$00					; Clear the table entry.
		INC		HL							; And step through the table. 
		DEC		BC
		
		LD		A,B							; Do a 16 bit "DJNZ" here. 
		OR		C
		JR		NZ,CLEARALLOC_LOOP
		
;		DJNZ	CLEARALLOC_LOOP
		POP		HL							; Retrieve allocation base.
		LD		A,(IX+AL0)					; First allocation byte for directory use.
		LD		(HL),A						; Add the directory allocations to start. 
		INC		HL
		LD		A,(IX+AL1)
		LD		(HL),A						; And the base table is now set up. 

		RET

GET_AV_TABLE_SIZE:							; Return number of bytes in AV table in BC.
		LD		IY,(Disk_Parameter_Header)	; Retrieve the DPH into IY. 
		LD		IX,(Disk_Parameter_Block)	; Retreive the DPB into IX. 
		LD		L,(IX+DSM)
		LD		H,(IX+DSM_H)
		LD		(DISKSIZE),HL				; Store the total number of allocations on the disk here. 
		LD		L,(IY+ALL00)
		LD		H,(IY+ALL00_H)				; Get the vector table base address from the DPH. HL=ALLOC VECTOR TABLE. 
		LD		(ALLOCATION_VECTOR),HL		; Store the vector. 

;EQU		DSM,	$05	; DSM - Disk size -1 - ie, 242= 243 BLOCKS. if the block size=1024k then  248,832 bytes on disk. Block=Allocation Unit. 
;EQU		DSM_H,	$06 ;	DSM High Byte - If not zero, two byte allocations.

		LD		A,(IX+DSM_H)					; High byte of Disk Size.
		LD		B,A
		LD		A,(IX+DSM)					; Low byte of Disk Size.							
											; Now BA 'pair' hold the full number of blocks on disk device (-1). 
		AND		%1111 1000				; Mask with F8 to eliminate lower 3 bits.
		RR		B
		RRA
		RR		B
		RRA
		RR		B
		RRA									; Reduce count by number of bytes... 
		LD		C,A							; And make 'BC' pair now - BC holds number of bytes in allocation vector. 
		INC		BC							; And add one to make up for "rounding error" when we got rid of low order bits ( we need to cover ALL bits )
		ret

Build_Allocation_Table:
ALLOCATION:		DW		$0000
ALLVECT:		DB		$00	
DISKSIZE:		DW		$0000				; How many allocations on the disk? ( minus one )			


SET_ALLOC:									; Set a bit in the allocation table.
											; Enter with DE holding the allocation. 
		CALL	FIND_ALLOC					; Locate the correct byte in table.
		OR		(HL)						; Add the bits already in the table.
		LD		(HL),A						; Store the byte back in the table with the extra bit
		ret
		
FIND_ALLOC:									; We come into this with an allocation in DE 
		LD		(ALLOCATION),DE				; DE holds the allocation we want to add to the vector table. 
		LD		HL,(ALLOCATION_VECTOR)		; Base location of the vector table is now in HL. 
		
		LD		A,(ALLOCATION+1)			; Store allocation we're adding. We got 9 bits in total.
		RRCA								; We only have 512 allocations max, which will give us up to 1.44 Mb with 4Kb allocations.
		LD		A,(ALLOCATION)				; Get the lower order bytes.
		RRA
		RRA
		RRA									; Divide what is there by 8 ( and we picked up a bit from high order bits ).
		AND		%0011 1111					; Won't be more than 63... 
		LD		(ALLVECT),A					; Now we have the vector to add to HL.
		LD		E,A
		LD		D,0
		ADD		HL,DE						; Get the correct byte reference in HL.
		LD		A,(ALLOCATION)				; Now get the BIT reference.
		AND		$07							; Mask for 3 bits
		LD		B,A							; Move to B. 
		INC		B							; And make a number from 1 to 8.
		XOR		A							
		SCF									; Clear A and Set Carry.
FIND_ALLOC_LOOP:
		RR		A							; Move the bit through Bit7 to Bit 0. First allocation is high order bit. 
		DJNZ	FIND_ALLOC_LOOP				; As many times as necessary.
		RET									; On Exit, A holds the bit, and HL holds the location.
		


;EQU		DSM,	$05	; DSM - Disk size -1 - ie, 242= 243 BLOCKS. if the block size=1024k then  248,832 bytes on disk. Block=Allocation Unit. 
;EQU		DSM_H,	$06 ;	DSM High Byte - If not zero, two byte allocations. 
Found_Allocation:	DW	$0000				; Store where the found allocation was in the vector table. 
Mask_Allocation:	DB	$00					; Store the mask we want if we need to mark the allocation as used. 

								; Also IY is the DPH, IX is the DPB.  This gets called via Match Selected Disk.
								; DPB +05, +06 = number of allocations - 
								;EQU		DSM,	$05	; DSM - Disk size -1 - ie, 242= 243 BLOCKS. if the block size=1024k then  248,832 bytes on disk. Block=Allocation Unit. 
								;EQU		DSM_H,	$06 ;	DSM High Byte - If not zero, two byte allocations. 
								; Number of elements in AV table = (DSM+1)/8
								
; Note: we need to loop for the size of the allocation table. 

Get_Next_Allocation:						; Return the number of the next free allocation in the DE pair. 0000 = No Free Allocations.
		LD		HL,(ALLOCATION_VECTOR)	; Collect the vector table start vector on the way out... 
;		LD		B,64					; 64 max iterations. Max size of allocation vector table... Need a better way of doing this. 
										; Needs to be (allocations+1) /8.... 
		CALL	GET_AV_TABLE_SIZE		; Load BC with the size of the allocation table. 			
										; IX is now DPB pointer, +09, +0A are the directory allocation bits.



;		LD		A,(IX+AL0)				; First two predetermined directory allocatiob bytes of allocation table are in the DPB.
;		LD		(HL),A					; Write them into the first two bytes of the allocation table itse. 
;		INC		HL
;		LD		A,(IX+AL1)				
;		LD		(HL),A
;		DEC		HL

		
		LD		DE,$0000					; Set DE to hold the allocation number. We can start at 0 because we've written the Directory Allocations. 
Get_NA_LOOP:
		LD		A,(HL)
		CP		$FF
		JR		NZ,Get_NA_Found				; If the value in HL isn't FF then we found a free block to allocate.
		LD		A,E
		ADD		A,$08
		LD		E,A
		LD		A,$00
		ADC		A,D
		LD		D,A
		INC		HL

;		DJNZ	GET_NA_LOOP					; Let's do this with 16 bits now. 
		LD		A,B							; Do a 16 bit "DJNZ" here. 
		OR		C
		JR		NZ,GET_NA_LOOP


GET_NA_FULL:		
		LD		DE,$0000					; No allocations available. 
		ret
		
Get_NA_Found:
		LD		B,8							; Let's do a bit scan.
		LD		C,$80						; Set the high bit in C to get an "or" target if we want to mark the bit. 
		OR		A							; Clear Carry
Get_NA_Scan:
		RL		A							; We're looking for NO CARRY to signify a free allocation.
		JR		NC,GET_NA_This_Block		; If no carry, we have the right block.
		RRC		C							; And rotate C ( the "or" if we mark the byte later as used )
		INC		DE
		DJNZ	Get_NA_SCAN

halt										; And halt - We should NEVER get to this code... But if we want to continue, we can delete this and just error out. 		
		
		JR		GET_NA_FULL					; Something went seriously wrong... So I'm setting a break point to know the code isn't working.
GET_NA_This_Block:
		LD		(Found_Allocation),HL		; Store this for later if we want to mark the block as used.
		LD		A,C
		LD		(Mask_Allocation),A			; Store the mask necessary to mark this block as used.  ( or with block ). 
		PUSH	DE							; Transfer the allocation number from DE to HL. 
		POP		HL							; T. Move it to HL
		LD		BC,(DISKSIZE)
		INC		BC							; We want DISKSIZE to be larger, not same size as last allocation. 
		SBC		HL,BC						; Should be no carry heading into this - If we get a carry (borrow) out, it's because DISKSIZE was larger. 
		JR		NC,GET_NA_FULL				; Found allocation was not on the disk.
		ret

Borrow_Allocation:

		CALL	Get_Next_Allocation			; Find the next free allocation in the table.

		LD		A,D
		OR		E							; If both D and E are 00 then the allocation could not be provided. 

		JR		Z,DISKFULL_ERROR			; Disk full means BOMB OUT and RESTART with an error.
			
											; Since we're asking to borrow the allocation for a file however, let's mark it now as used... If we don't close the file, we can
											; always recover the lost memory with a disk reset. 
		LD		HL,(Found_Allocation)		; Let's mark the place we found the space allocation as used... 
		LD		A,(Mask_Allocation)
		OR		(HL)
		LD		(HL),A						; Allocation is now marked as used in the vector table. 

		ret
		

DISKFULL_ERROR:

		LD		DE,DFERROR
BOMBOUT:
;		LD		C,9
		CALL	PRINTSTRING
		JP		$0000						; Restart... Unrecoverable write error.  Should I close the current extent to save what is possible?
	
DFERROR:	db	'BDOS Error. Reboot.',$0A,$0D,'$'


;DISKSIZE
		ret

Write_Protect_Disk:
		ret

RO_VECTOR:	DEFB  %0000 0000, %1100 1110	; Lower order byte first. 
Get_RO_Vector:
;		LD		HL,%1100 1110 0000 0000		; L drive is RO... Or it is supposed to be. Added JKO and P BIOS drives. M(emory) is RAMDISK, N is NVM disk and I is Internet. 
		LD		HL,(RO_VECTOR)
		LD		DE,RO_VECTOR				; ADDED OVER CP/M - Return the location of the vector in DE in case it needs to be changed. 
		ret
		
		
		
Set_File_Attributes:
		ret
Get_DPB_ADDR:
		LD		HL,(Disk_Parameter_Block)
		ret


User_Code:									; Loki OS uses the user code to defined which process this software should be installed for.
		ret

Compute_File_Size:		; Let's locate all parts of the file, and determine how many allocations exist. 
						; This will include sparse files, so search until the file is no longer found. IX will be DPB. (Disk Parameter Block)
		PUSH	DE
		POP		IY						; Store FCB location in IY.
		LD		(IY+EX),'?'				; Set the FCB match extent to wildcard.
		LD		(IY+R0),$00
		LD		(IY+R1),$00
		LD		(IY+R2),$00				; Clear filesize info.
		CALL	MATCH_FIRST_FILENAME	; And locate the file if it exists. 
Compute_FS_Loop:
		AND		%1111 1100 			; Mask out lower bits.
		RET		NZ						; If there's a response other than 0 to 3, then exit.

		LD		IX,(DISK_PARAMETER_BLOCK)	; Where we stored the location of the DPB. 
		LD		C,(IX+BLM)				; Block Length Mask. 
		INC		C						; So that C now holds the records in a block.
		
		LD		IY,(MATCH_FCB)			; Pick up FCB location
		
		LD		IX,(MATCH_EXTENT)		; Which extent matched?
		LD		A,(IX+DSM_H)			; After MATCH operation, IX is the Disk Parameter Block (DPB)
		OR		A
		CALL	Z,Compute_Single		; If there's less than 256 allocations, read a single byte allocation.
		CALL	NZ,Compute_Double 		; If there's more than 255 allocations, read a double byte allocation. 		
		
		LD		DE,(MATCH_FCB)			; Make sure DE holds the FCB again.
		CALL	MATCH_NEXT_FILENAME
		JR		Compute_FS_Loop			; And repeat until all filenames have been read. 
		
Compute_Single:
		LD		B,16
		LD		IY,(MATCH_FCB)			; Retrieve the start of the Disk Parameter block. 
Compute_Single_LP:
		LD		A,(IX+Alloc1) 
;		OR		A,(IX+Alloc2)			; Anything other than a zero? Might be a sparse file.
		OR		A
		CALL	NZ,Compute_Add_Allocation
		INC		IX
		DJNZ	Compute_Single_LP
		XOR		A						; Clear A to make sure Double doesn't happen. 
		ret

Compute_Double:	
		LD		IY,(MATCH_FCB)			; Retrieve the start of the Disk Parameter block. 
		LD		B,8
Compute_Double_LP:
		LD		A,(IX+Alloc1) 
		OR		(IX+Alloc2)			; Anything other than a zero? Might be a sparse file.
		OR		A
		CALL	NZ,Compute_Add_Allocation
		INC		IX
		INC		IX
		DJNZ	Compute_Double_LP
		XOR		A						; Clear A to make sure Double doesn't happen. 
		ret
		
Compute_Add_Allocation:					; BSF is Block Shift Factor - ADD 1 and add this to total records used per allocation. 
;		PUSH	IY						; Store index - it's used byt he routine that calls this one. 
;		LD		IY,(MATCH_FCB)			; Retrieve the start of the FCB 
		LD		A,(IY+R0)				; IY is the File COntrol Block.
		ADD		A,C						; And Disk Parameter Block BSF+1 to A.
		LD		(IY+R0),A				; Update the record pointer. 
		LD		A,(IY+R1)
		ADC		A,$00
		LD		(IY+R1),A				; Carry over to R1
		LD		A,(IY+R2)
		ADC		A,$00
		LD		(IY+R2),A				; Carry over to R2
;		POP		IY
		ret								; Final result will be 1 over the last block - ie, will be the next block. Not sure if I like this?
										; Need to check what the real CP/M does. 
		


Set_Random_Record:						; Opposite of SET_RANDOM function - basically the same in reverse. Set the R0 R1 R2 record number this time. 
		PUSH	DE					; Get the FCB Location.
		POP		IY					; Move the FCB location to IY. 

		LD		A,(IY+CR)			; Do I need a "dec A" here?
;		OR		%0111 1111			; Mask for 7 bits.
		RLC		A					; Shift the bits up.
		LD		(IY+R0),A			; Store in R0.
		LD		A,(IY+EX)			; Get extent byte.
		RRC		A
		RRC		(IY+R0)				; And rotate 1 bit from extent into R0.
		RLC		A
		RLC		A					; Move A bits ( 6 remaining ) to top  of A.
		AND		%1111 1100			; Mask remaining 6 bits of A. Might need to reduce number of bits. Need to check max valid extents.
		LD		(IY+R1),A			; Store in R1.
		LD		A,(IY+S2)			; Get overflow.
		RRC		A
		RRC		(IY+R1)				; Move a bit from S2 to R1
		RRC		A
		RRC		(IY+R1)				; Move a bit from S2 to R1
		AND		%0001 1111			; Mask to 5 bits
		LD		(IY+R2),A			; Store in R2.
		
		ret							; And we've shuffled the CR and EX and S2 into R0,R1 and R2. Need to validate this with real CP/M.


Reset_Drive:
Access_Drive:
Free_Drive:
			ret


;         |                        |                  |      address     |
;28    1C | Write Protect Disk     | (E)=disk number  |      ****        |
;29    1D | Get Read Only Vector   |      ****        | (HL)=R/O vector  |
;30    1E | Set File Attributes    | (DE)=FCB address | (A)=dir code     |
;31    1F | Get Addr of Disk Parms |      ****        | (HL)=parm addr   |
;32    20 | Get/Set User Select    | (E)=0FFH get     | (A)=current user |

;33    21 | Read Random Record     | (DE)=long FCB adr| (A)=error code   |


; Set Random is just a subroutine of READ and WRITE random. 
SET_RANDOM:						; SETS RANDOM RECORD AND EXTENT AND S2. WRITE/READ Random. 
								; Use FCB+21,22,23 or R0,R1,R2 for the counter. 
								; ALSO SETS UP EX AND S2... 
								; As we return from calling SELDSK in the BIOS.
								; Also IY is the DPH, IX is the DPB.  This gets called via Match Selected Disk.
								; On Entry, DE=FCB. 
								; We set up the "record" and "extent" in the FCB then jump to READ_NEXT.

		PUSH	DE					; Get the FCB Location.
		POP		IY					; Move the FCB location to IY.
;		LD		(MATCH_FCB),DE		; Set this now - We read it back shortly, but that routine MATCH_SELECTED_DISK is called from elsewhere. STORE THE FCB location. 
;		LD		IY,(MATCH_FCB)		; Get the FCB in IY
		LD		A,(IY+R0)			; lower byte.
		AND		%0111 1111			; 128 records per extent. Mask the value.
		LD		(IY+CR),A			; Set the Current Record.
		
		LD		A,(IY+R0)			; Once more get the lower R0 byte - We want the upper extent.
		RL		A					; Get the upper bit in carry.
		LD		A,(IY+R1)
		RL		A					; And move into upper A.
		AND		%0001 1111			; &h7F (spaces don't count )
									; NOTE - We now have the "new" EX value in A, limited to 5 bits as per CP/M requirements.
									; Before we change it, we MUST make sure we haven't changed the current EX outside of what it was - otherwise we need to 
									; open the correct extent.
		CP		(IY+EX)				; Compare to the existing value...		
		JR		Z,SET_RANDOM_SAME_EXTENT	; if they are the same, nothing to fix... 
									; But now we need to re-open the file to the correct extent as noted in the modified FCB.
		LD		(IY+EX),A			; First, modify the FCB by loading the upper 5 bits into Extent extent field. 							
									
		PUSH	DE
		CALL	OPEN_FILE			; "open" then next extent, just like we opened the first. 
		POP		DE
		
		AND		%11111100			; Did we succeed? Check the return code, and should be between 00 and 03 if successful. 
		JR		Z,SET_RANDOM_SAME_EXTENT	; File open was OK.

		;;; What do I do with the error here? The extent wasn't available.... I should error out somehow. Need to figure out what to do. 
		NOP
		NOP
		NOP

SET_RANDOM_SAME_EXTENT:
									; Before I mess with R1, I need to check what my serial routines do first with R1. (I think they just use EX)
;		LD		A,(IY+R1)			; R1 is all we care about. R2 would give us above 8Mb per file. We will ignore it. 
;		RLCA
;		RLCA						; Circular move the upper bits of R1 to D0 and D1 of A.
;		AND		%0000 0011			; Mask.
;		LD		(IY+S2),A			; And store the upper bytes.	
		RET
		





		
READ_RANDOM:
		CALL	SET_RANDOM			; Set up the FCB by changing the Current Record, Extent and S2
		JP		READ_NEXT			; Then treat this like a read next
		

;34    22 | Write Random Record    | (DE)=long FCB adr| (A)=error code   |
WRITE_RANDOM:
		PUSH	DE
		POP		IY
		LD		A,(IY+ALLOC1)		; First allocation.
		OR		(IY+ALLOC2)			; Second allocation byte ( might be a single or double allocation.) If it's already open and written to
									; one of them won't be '00'. If either have data, file was already opened and possibly written to. CLOSE THE FILE
									; FIRST AND WRITE ANY EXISTING allocations in the FCB to the extent. 
		JR		Z,WRITE_RANDOM_OK	; If there was nothing (eg, new file, then it's OK to change the CR and EX and S2 so just set and write.
		
									; If we fell through here, then we need to close the file to write the extent to the disk with any changes.
		PUSH	DE					; I might need to delete all of the above and determine if the write is in the same extent via something else. Probably EX. 
		CALL	CLOSE_FILE			; Close file really just writes the current FCB to an extent. It would be possible to write/close/write/close... 
		POP		DE		

WRITE_RANDOM_OK:		
		CALL	SET_RANDOM			; Somewhere I need to check if the extent has changed and close/reopen the file. 
		JP		WRITE_NEXT			;


; ALL Stuff I still have to write. 
;35    23 | Get Size of File       | (DE)=long FCB adr| (r0-2=rec cnt)   |
;36    24 | Set Random Record Num  | (DE)=long FCB adr| (r0-2=rec numb)  |
;37    25 | Reset Drive            | (DE)=drive vector|      ****        |
;38    26 | Not used               |                  |                  |
;39    27 | Not used               |                  |                  |
;40    28 | Write Random with      | (DE)=long FCB adr| (A)=error code   |
;-------------------------------------------------------------------------


; Wildcard for FCB....
;DR F1 F2 F3 F4 F5 F6 F7 F8 T1 T2 T3 EX S1 S2 RC  .FILENAMETYP...
;AL AL AL AL AL AL AL AL AL AL AL AL AL AL AL AL  ...............
;CR R0 R1 R2          



PREREAD_DIRECTORY:							; First set up of directory flags. 		
		LD		L,(IX+DRM)
		LD		H,(IX+DRM_H)				; Get HL from DPB DRM field ( Directory Max Entries ) 
		OR		A
		RR		H
		RR		L	
		OR		A
		RR		H
		RR		L							; Divide HL by 4. 4 entries per extent. 
		LD		(DIRECTORY_EXTENTS),HL		; Directory Max ( Max directory entries ). Will work out best way to do this later. 
		LD		A,H
		OR		A

		JR		Z,PREREAD_CONT				; Let's also limit the upper byte at this time... No more than 256 files extents. jump if nothing in H. 
		LD		HL,$003F					; Otherwise, let's reset it here, so that's 64 records max.
		LD		(DIRECTORY_EXTENTS),HL		; And tweak - Note that if L in HL is greater than $3F, this won't limit it. 
PREREAD_CONT:
		LD		BC,$00
		CALL	SETTRK_CALL					; Track 0
		LD		BC,$00
		LD		(START_SECTOR),BC		; Store the start sector. 
		CALL	SETSEC					; Sector 0. ( Actually record ). 
		LD		C,(IY+DIRBF)
		LD		B,(IY+DIRBF_H)
		LD		(START_DMA),BC			; Store where the DMA location is for the directory scratchpad. 
		CALL	SETDMA					; Let's read the first directory entry.
		CALL	READ
		RET

; Variables used and stored by the initial disk routines ( eg, Directory ). 

MATCH_EXTENT:	DW	$00				; Temp store for extent location. 
MATCH_FCB:		DW	$00				; Temp store for FCB location. 
DIR_CODE:		DB	$00				; Directory Code - Which entry is the match, or $FF if none. Store here so we can call it again later for find next.
DIRECTORY_FLAGS:DW	$00				; Directory Words when we initialise the drive... So we know how many iterations of search to do. DONT USE PRESENTLY
START_DMA:		DW	$00				; Start of DMA	
START_SECTOR:	DW	$00				; START SECTOR = We will need to know which sector we are reading in multiple reads for find next. 
DIRECTORY_EXTENTS: DW	$00				; How many extents are we going to read before we decide all directory entries have been read? (DRM+1)/4


MATCH_NEXT_FILENAME:						; See if we get another match. Don't forget to reset the FCB.
											; 
;		LD		(MATCH_FCB),DE		; Store the FCB we're matching against. This shouldn't change, but it is possible. OK, This seems to change?
		LD		A,(DIR_CODE)		; Get the current index, which is the last matched. Before we do anything, make sure we hadn't completed all matches. 
		CP		$FF					; Check if we previously determined there were no more extents to search.
		RET		Z					; Return if there are no more extents to search. We don't search again if the last search already said no more matches found.
		
		JP		MATCH_NOMATCH		; Pick up the search as though the last extent checked did not match ( even though it did ). 


;
;		INC		A					; Go to next extent in record.
;		CP		$04					; No more extents in record?
;		JR		NZ,MATCH_START		; If there's another extent to check, then check it...
;									
;									; otherwise...
;
;		CALL	MATCH_NEXT_EXTENT	; We may need to pick up another extent before we proceed.
;		CP		$FF					; But if we're out of extents, a=$FF
;		RET		Z					; Exit here if out of extents. 
;	
;		JR		MATCH_START			; Otherwise start matching more filenames. 
		
	
	

MATCH_FILENAME:						; Here's where we match the filename. DE should hold the FCB. Directory is in the DMA location.
									; Enter with DE at FCB location, and A with the number of the entry we're matching... 3=start with first. 
									; We can just decrement the directory code if necessary and continue to find the next match. 
									
;;		LD		A,$00				; Index first filename in extent. I really should set up everything in DE and A before calling this. 
									; Leave the labels as it for the moment - since we might want to change entry conditions. 
									; Note - I removed this as it gets set in "Match First Filename... " And we still need an entry point where we set MATCH_FCB.
									
									
		LD		(MATCH_FCB),DE		; Store the current FCB location we want to match against.
									; Do it here and loop via match start as we may exit the routine before coming back in...
									;     Note: Return as though the last match failed on subsequent loops to trigger loading a new
									;     record to review if necessary. 
									



MATCH_START:	
		LD		(DIR_CODE),A		; Store the current directory code. It's the index for where we start in the DMA area to read files.
		LD		HL,(START_DMA)		; Locate the current DMA area for THIS DISK. Need to make sure I get a different one for EACH DISK. 
		
		
;		RRCA						;    The DIR code tells us with 32 byte block is the one that holds the current directory entry.
;		RRCA						;    Dir Code can be from 0 to 3, since there are 4 directory entries per record in the directory area. 
;		RRCA						;    Also multiply by 32, but the other way.. Cheating. (5xRLCA would do OK too)
;		LD		C,A					; 	 C = A * 32 ( won't exceed 96 )
;		LD		B,$00				; BC = A * 32.
;		ADD		HL,BC				; Get the address of the start of the record in HL. 4 extents per record. 
		CALL	LOCATE_EXTENT		; does what the above lines of code do.

		LD		(MATCH_EXTENT),HL	; Store it, in case we need it again. We will, when we check the extent number in the extent. 

		LD		DE,(MATCH_FCB)		; DE = FCB = Filename specified -- Early match here for empty file detection. 

		
		LD		A,(HL)				; Get the first byte of the extent, will be $E5 if it's deleted or blank. 
		CP		$E5					; "Deleted" character.
		JR		NZ,MATCH_START_CONT	; if it's not E5, continue matching. 

		LD		A,(DE)
		CP		$E5					; If DE is E5 also, then we actually want to match, since we're looking for a blank spot, not a file. 
		JR		NZ,MATCH_NOMATCH	; Go straight to update search location in record. 4 directory entries per record. We don't match on deleted records since we wanted a match...

									; For when we're looking for a blank space in the allocations, this will do. 
	;;	LD		HL,(MATCH_EXTENT)		;	Currently, this extent is empty and waiting for use. HL was not changed since we last set it..Can leave this out....
		LD		A,(DIR_CODE)		; Return the DIR code....  Tells us success or failure. 

		ret		

MATCH_START_CONT:		
		LD		DE,(MATCH_FCB)		; DE = FCB = Filename specified 

		LD		A,(DE)				; First check if we want to match whatever is in the extent ( Extent Wildcard in first byte of FCB )
		CP		'?'					; See if the first character of the FCB is '?' instead of a drive...
		JR		Z,MATCH_PERFECT		; And if it is, just match every single entry.
		
									; ######################################################################################################################################
									; Here is where I should check the user ID if it's relevant to the match.
									; ######################################################################################################################################
		
		INC		DE					; Move DE to point to first character of filename in FCB. 
		INC		HL					; Move HL to point to first character of filename in the current extent.
									; Now we can proceed with filename matching. 

		EX		DE,HL				; Swap DE and HL , because of how we need to compare (DE) to (HL) with a CP (HL) 
											; DE is now the EXTENT POINTER in the directory entry. 
											; HL is now the FCB pointer. 
											; We want to see if the string in FCB matches the string in EXTENT POINTER, including wildcards. 
;		LD		B,12				; 11 characters to match. Twelvth character is the extent number. So now it's 12. 
		LD		B,11				; Nope, it's back to 11. We have some maths to do on the 12th match because of EXM. 
		
MATCH_NEXTCHARACTER:					; Do the filename matching here. FCB should be set up with the search mask. DIR = '???????????' (=match all)
		LD		A,(HL)					; Character (B) in FCB
		CP		'?'						; Wildcard test?
		JR		Z,	MATCH_1MATCH		; Consider it a match for ? Don't test against name in Extent then. Bypass on wildcard encounter. 
		
		LD		A,(DE)					; Get the character from the EXTENT ( Filename on disk )
		AND		%01111111				; Strip Bit7 - it carries permissions and other data such as passwords.
		CP		(HL)					; Compare both letter and case or other character ( perfect match )
		JP		Z,MATCH_1MATCH			; If it matched, we're done.
		
										; Imperfect Matches - Special Characters and Case Correction.
										; Maybe filename was lower case, and FCB was upper case?
		CP		$61						; 'a' Lower case 'A'
		JR		C,MATCH_CASEOK			; Jump if character was lower than 'a'.
		CP		$7B						; One character past 'z'
		JR		NC,MATCH_CASEOK			; Jump if character was higher than 'z'.
		AND		%01011111				; Mask case and strip bit 7 and bit 5 before compare. Convert case to Upper Case and check that. 
		CP		(HL)					; Test the modified case.
		JR		Z,MATCH_1MATCH			; An imperfect match - case was different. And proceed if it matched now.
		JR		MATCH_NOMATCH			; Otherwise fail out here, as CASE wasn't the reason for a lack of match earlier.
		
MATCH_CASEOK:		
		CP		$41						; Test for upper case A.
		JR		C,MATCH_NOMATCH			; No, as character was lower than "A" so not a case issue.
		CP		$5B						; Test for upper case Z.
		JR		NC,MATCH_NOMATCH		; No, as character was higher than "Z" so no a case issue.
		OR		%00100000				; Set bit 5 to change the case since we determined it was an Upper Case character at least, change to lower case. 
		CP		(HL)					; Now compare to the intended character.
		JR		Z,MATCH_1MATCH			; An imperfect match - case was different. And proceed if it matched now.
		JR		MATCH_NOMATCH			; Otherwise, it wasn't a CASE issue, so fail out of the filename match here. 
		

MATCH_1MATCH:							; We got a single character match... It's a start.
		INC		HL						; update pointers.
		INC		DE
		djnz	MATCH_NEXTCHARACTER		; Test for all 11 spaces
		
										; We got a match, now let's check the extent........
										; We can check the EX byte and the Directory Entry byte by OR'ing them both with the EXM first....
										; DE is now the EXTENT POINTER in the directory entry. 
										; HL is now the FCB pointer. 		
										; BOTH are now pointing to the relevant EX position in their respective tables. 
										
		LD		A,(HL)					; Check if we need to match the extent 
		CP		'?'						; If (HL) (EX in FCB) Happens to have a '?' character - match all extents. 
		JR		Z,MATCH_ALL12				; So bypass extent check. 
										
										; Here's where we match the logical extent to the physical extent. 
		LD		IX,(DISK_PARAMETER_BLOCK)	; Set up IX so we know it's pointing to the DPB, where we have the EXM ( Extent Mask )
		LD		A,(IX+EXM)				; Get the extent mask.
		XOR		$FF						; Invert the mask.
		LD		C,A						; Store the mask.
		LD		A,(DE)
		XOR		(HL)					; See which bits are different. If all same, will zero out bits.
		AND		C						; Inverted EXM mask so we can see if unmasked bits are different.
		JR		NZ,MATCH_NOMATCH		; If the extent is wrong, post mask, we have a different problem. 

MATCH_ALL12:				
		LD		IX,(MATCH_EXTENT)		; If we got this far, there's a match. 12 characters matched or were wildcard. 
;		LD		A,(IX+EX)				; Extent number needs to match FCB also, otherwise multiextent matches will show up (eg, files > 16K )
		LD		IY,(MATCH_FCB)
;		CP		(IY+EX)					; Compare to the extent in the FCB ( the extent we're looking for )
;		JR		NZ,MATCH_NOMATCH		; Even if the filename matches, we want the correct extent. ( Will need to make sure the EX is 0 for "dir" ) 

		; Now we just look through 12 characters of the FCB filename and recognize ? as wildcard for all extents. 	
		; We got a match! Get the start of the extent and print the filename 
MATCH_PERFECT:										; Copy found extent to FCB. 
		CALL	MATCH_WRITE_EXTENT		; Copy the extent to the record buffer. If we're doing an open or similar, we'll overwrite it on read. 

		LD		A,(DIR_CODE)			; Retrieve the DIR_CODE. 
		ret								; Return here, with the DIR_CODE intact because we have a match. 
										; Here we can break out of the process if we want... We can increment A and call it again later. 

									
			
			

		
MATCH_NOMATCH:							; Upon asking the directory to make a match, we come here if the currently indexed extent did not match the FCB. 
		LD		A,(DIR_CODE)			; DIR_CODE holds the index in the record of the extent. Counts 0,1,2,3 
		INC		A						;
		CP		$04

		JP		NZ,MATCH_START			; If not 4 iterations yet, and nothing found, repeat.
										; See if we can get another extent from a new sector... If so, load it up and look through it. 
										; We checked the record. Either get another record or we're done. 
										; Fall thrrough to get the next record if there is one to get. 
						
MATCH_NEXT_EXTENT:						; We want to read new areas of the disk to get more directory records. 
		LD		HL,(DIRECTORY_EXTENTS)	; Are there other extents to review?
		LD		A,H
		OR		L
		JR		NZ,MATCH_NEXT_SECTOR	; If there are more sectors to match... let's get them. 
		
		LD		A,$FF				; otherwise set the error. Nothing more to match. Should this be 04 seeking to an extent that doesn't exist?
;		LD		A,$04
		LD		(DIR_CODE),A		; Save this in case referenced later. 

		ret							; end exit. 

MATCH_NEXT_SECTOR:
		DEC		HL
		LD		(DIRECTORY_EXTENTS),HL	; Reduce the number of extent records we still have to review.
		
		LD		BC,(START_SECTOR)
		INC		BC
		LD		(START_SECTOR),BC
		CALL	SETSEC				; Set the new sector. (record)
		CALL	MATCH_SELECTED_DISK	; Pick up all of the necessary details of the aforementioned disk. 
		
		LD		C,(IY+DIRBF)
		LD		B,(IY+DIRBF_H)
		LD		(START_DMA),BC		; Store where the DMA location is for the directory scratchpad. 
		CALL	SETDMA				; Let's read the first directory entry.
		CALL	READ

		JP		MATCH_START			; We should be good to start matching again... It's a long jump to the top of the routine.
;		ret							; WHY? Why am I returning here? Something is SERIOUSLY wrong.  This should be JR MATCH_START should it not?
;
FCB_EXTENT:	DB	$00	; temporary store for the EX value in the FCB.

MATCH_WRITE_FCB:						; We can write the opened FCB from the current directory record (physical extent).  
		LD		HL,$0080				; Temp File Buffer
		CALL	LOCATE_EXTENT			; Need to call this immediately after Match_Start completes. It uses the variables set up there. 		; 

		LD		IY,(MATCH_FCB)		; Now we have the FCB in IY and the DPB in IX.
		LD		IX,(Disk_Parameter_Block)

		LD		D,(IY+EX)				; use IY so we can refer to IY+EX
		LD		B,31					; 32 characters, but we want 31 copied, since the first is the User ID. We ignore the other 4 bytes of the FCB. 
MATCH_WRITE_FCB_LP:
		INC		HL						; Yeah, this is LAZY AS, but I have both the right pointers here so I'll use IX and IY. 
		INC		IY
		LD		A,(HL)				; Get the byte from the extent.
		LD		(IY+0),A				; Store it in the FCB
		DJNZ	MATCH_WRITE_FCB_LP		; This will make the FCB match the extent that is found. 
		
		LD		IY,(MATCH_FCB)
		
		LD		A,D						; Get specified Extent.
		CP		(IY+EX)					; is it the same as the last extent specified in the FCB?
		ret		z						; if it is, then don't change the extent or RC.
		
		LD 		(IY+EX),D				; Restore FCB extent originally asked for. KLUDGE		
		LD		(IY+RC),$80		; But if it's not the last logical extent, mark it as $80... 
		
		ret

LOCATE_EXTENT:
		LD		A,(DIR_CODE)			; Retrieve the current directory code. It's the index for where we start in the DMA area to read files.
		RRCA
		RRCA
		RRCA						; Also multiply by 32, but the other way.. Cheating. (5xRLCA would do OK too)
		LD		C,A					; 	C = A * 32 ( won't exceed 96 )
		LD		B,$00				; BC = A * 32.
		ADD		HL,BC				; Get the address of the start of the record in HL. 4 extents per record. 
		ret
		
MATCH_WRITE_EXTENT:					; Like FCB, but copies ( right now ) to 0080+(DIRCODE)
		LD		HL,$0080			; Temp File Buffer
		CALL	LOCATE_EXTENT
		LD		B,31					; 32 characters, but we want 31 copied, since the first is the User ID. We ignore the other 4 bytes of the FCB. 
MATCH_WRITE_EXT_LP:
		INC		IX						; Yeah, this is LAZY AS, but I have both the right pointers here so I'll use IX and IY. 
;		INC		IY
		INC		HL
		LD		A,(IX+0)				; Get the byte from the extent.
;		LD		(IY+0),A				; Store it in the FCB
		LD		(HL),A
		DJNZ	MATCH_WRITE_EXT_LP			; This will make the FCB match the first found. 	
		ret

PRINT_EXTENT:							; A quick print out of the extent last matched. 
		LD		HL,(MATCH_EXTENT)		; Otherwise we print here. 
		INC		HL
		LD		B,11					

PRINTEXT_LOOP:								; Do something here. Printing via BIOS only affects A and C at the moment.								
		PUSH	BC						; Print the found filename. 
		LD		A,(HL)
		LD		C,A
		CP		' '						; Don't print spaces. (padding)
		CALL	NZ,CONOUT				; Only call for non-? characters.
		LD		A,B
		CP		04						; On the pass where B=4, print a dot.
		LD		C,'.'
		CALL	Z,CONOUT				; Only when B=4 though. 
		POP		BC						; Will need to protect DE and HL later. Or maybe adjust BIOS to protect what it touches. 
		INC		HL						; Update pointers. 
		DJNZ	PRINTEXT_LOOP
		CALL	CRLF					; Need to see what CRLF messes with. Actually, at this point, who cares. We reset the registers later anyway. 
		ret



;DR F1 F2 F3 F4 F5 F6 F7 F8 T1 T2 T3 EX S1 S2 RC  .FILENAMETYP...
;AL AL AL AL AL AL AL AL AL AL AL AL AL AL AL AL  ...............
;CR R0 R1 R2                                      ....		
CCP_FCB:			DB $0C																	; FCB for Command and Control Process. 
CCP_FILENAME:		DB	'ccp     bin'														; CCP Filename - and it's drive L:	
CCP_FILENAME_CNT:	DB $00,$00,$00,$00														; File variable tracking. 
CCP_FILENAME_ALL:	DB $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$00	; For the allocations.
CCP_FILENAME_HND:	DB $00,$00,$00,$00														; Four working bytes. 

CCP_PTR:			DW	$0000					; Where in the process are we? How much of the CCP have we written?


CCP_LOAD:								; Here's where we load the CCP.... I really want this at the end of the BDOS, and would be nice to have in the BIOS.
		LD		DE,CCPPRELOAD
;		LD		C,9						; BDOS Print String. 
		CALL	PRINTSTRING					; Message. 

		LD		DE,$0080				; Default DMA at 0080 -let's use that to save memory in the BDOS. 
;		LD		C,26					; Function 1A - Set the DMA address to the buffer at 0080.
		CALL	Set_DMA_Address					; Set DMA to 0080 - This is because the directory will show up here. Saves us copying the filenames too. 

		LD		DE,CCP_FCB				; Set up the file open for L:CCP.BIN
;		LD		C,15					; Function 15 = Open File.
		CALL	OPEN_FILE				; Get the first matching file. 
		AND		%11111100				; Mask the result bytes.
		JR		NZ,CCP_ERROR			; If the file didn't open, then exit.  AND will clear CF = error. 	



		
		LD		DE,$D000				; CCP destination starts at D000 - 8K allocated. 
		LD		(CCP_PTR),DE			; Store the pointer that contains where we will store the file. 
CCP_READ:
		LD		DE,CCP_FCB				; The CCP is located on L: and is usually the first file, but this is enough to find it on L:
;		LD		C,20					; BDOS Function 20 = read record
		CALL	READ_NEXT					; Get the first matching file. 
		OR		A						; Check for file status - And exit if we're at EOF ( we already exited earlier for other result other than read OK )
		JP		NZ,CCP_RUN				; Once we are fully loaded, try to execute the CCP. 				

		
		LD		HL,$0080				; Transfer the file to memory. 
		LD		DE,(CCP_PTR)
		LD		BC,$80
		LDIR
		LD		(CCP_PTR),DE			; Store the destination pointer for the next cycle. 

		JR		CCP_READ				; Loop until completely loaded - Note: CCP can ALSO include the BDOS and BIOS if both need to be overwritten. Cool eh?

CCP_ERROR:								; THIS SHOULD NEVER HAPPEN. ROM IS CORRUPT. WTF? Do I even need to do this?
		LD		DE,CCPERROR				; DE = Error string. 
;		LD		C,9						; Function 9 = Display String pointed to by DE
		CALL	PRINTSTRING
		HALT							; Nothing else to do... Hopefully they can reburn the boot eprom. Maybe they forgot the file?
		JP		$0000					; The halt should not go away. 

CCP_RUN:
;		LD		SP,$FFFF				; Just above the BIOS. Have around 160 memory locations for stack. Enough. 
;	
;		LD		HL,$0000				; Set up a return to exit via zero in case someone ret's too many times... Not sure if this even help?
;		PUSH	HL
	
		ret								; We're done. 
		

CCPPRELOAD:			DB	'Loading L:CCP.BIN',$0A,$0D,'$'		
CCPERROR:			DB	'L:CCP.BIN not found. Halting',$0A,$0D,'$'







;####################################################################################
;
;#  Constants, Notes, Tables, etc. 
;
;####################################################################################


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

;DPBLK2:			; Example. 
;		DW	$20		; 00,01 - eg, 32 sectors per track.
;		DB	3		; 02	- Like block mask, but a binary nuber. 1 less than the blocks. 
;		DB	7		; 03	- 8 records per Allocation. 0 is not an option ( or would mean 128 or 256 records/alloc )
;		DB	0		; 04	- Extent mask - Blocks are 1K, so should be zero. If 2K, 1, if 4K, 3 etc.  Our extents are fixed 16K
;		DW	242		; 05,06	- Number of allocation on disk. 242 = 243K. 
;		DW	63		; 07,08	- Directory Max (Max extents in directory allocations ). We could figure this out, but it's here. 
;		DB	%11000000 ;09	- High bits set first. One bit for each directory allocation (block). 
;		DB	%00000000 ;0A	- More directory bits in case 8 isn't enough. 
;		DW	16		; 0B,0C - Check size, DRM+1/4. or 0 for fixed. Not sure if I'll use this. 
;		DW 	42		; 0D,0E - Track Offset - Number of reserved tracks. The BIOS handles this. We can forget it. We just get Track0.
	


;CP/M File Control Block
;
;The File Control Block is a 36-byte data structure (33 bytes in CP/M 1). It is laid out as follows:
;
;DR F1 F2 F3 F4 F5 F6 F7 F8 T1 T2 T3 EX S1 S2 RC  .FILENAMETYP...
;AL AL AL AL AL AL AL AL AL AL AL AL AL AL AL AL  ...............
;CR R0 R1 R2                                      ....
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
;FCB+0Dh S1 - Reserved.   
;FCB+0Eh S2 - Reserved.
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
;How CP/M uses the bytes EX, S2, and CR. Some programs (such as the Digital Research linker, LINK.COM) manipulate these bytes to perform "seek" operations in files without using the random-access calls.
;
;CR = current record,   ie (file pointer % 16384)  / 128
;EX = current extent,   ie (file pointer % 524288) / 16384
;S2 = extent high byte, ie (file pointer / 524288). The CP/M Plus source
;    code refers to this use of the S2 byte as 'module number'.











	
	
.END

; CPM Calls.
;
;
;Function                           Entry Value to     Return Value from
; Number                            BDOS Passed in       BDOS Passed in 
;DEC  HEX     Function             (DE) or (E) regs   (HL) or (A) register
;-------------------------------------------------------------------------
; 0    00 | System Reset           |      ****        |      ****        |
; 1    01 | Console Input          |      ****        | (A)=character    |
; 2    02 | Console Output         | (E)=character    |      ****        |
; 3    03 | Reader Input           |      ****        | (A)=character    |
; 4    04 | Punch Output           | (E)=character    |      ****        |
; 5    05 | Printer Output         | (E)=character    |      ****        |
; 6    06 | Direct Console I/O     | (E)=0FFH is input| (A)=character    |
;         |                        | (E)=chr is output|      ****        |
; 7    07 | Get IOBYTE             |      ****        | (A)=IOBYTE       |
; 8    08 | Set IOBYTE             | (E)=IOBYTE       |      ****        |
; 9    09 | Display Console String | (DE)=string addr |      ****        |
;10    0A | Input Console String   | (DE)=string addr | (A)=# chr input  |
;11    0B | Get Console Status     |      ****        | (A)=000H idle    |
;         |                        |                  | (A)=0FFH ready   |
;12    0C | Get CP/M Version Number|      ****        | (HL)=Version #   |
;13    0D | Reset Disk Subsystem   |      ****        |      ****        |
;14    0E | Select Disk Drive      | (E)=disk number  |      ****        |
;15    0F | Open a File            | (DE)=FCB address | (A)=dir code     |
;16    10 | Close a File           | (DE)=FCB address | (A)=dir code     |
;17    11 | Search for File        | (DE)=FCB address | (A)=dir code     |
;18    12 | Search for Next        |      ****        | (A)=dir code     |
;19    13 | Delete File            | (DE)=FCB address | (A)=dir code     |
;20    14 | Read next Record       | (DE)=FCB address | (A)=error code   |
;21    15 | Write next Record      | (DE)=FCB address | (A)=error code   |
;22    16 | Create New File        | (DE)=FCB address | (A)=dir code     |
;23    17 | Rename File            | (DE)=FCB address | (A)=dir code     |
;24    18 | Get Login Vector       |      ****        | (HL)=login vector|
;25    19 | Get Logged Disk Number |      ****        | (A)=logged disk  |
;26    1A | Set R/W Data Buff Addr | (DE)=buffer addr |      ****        |
;27    1B | Get Allocation Vector  |      ****        | (HL)=alloc vector|
;         |                        |                  |      address     |
;28    1C | Write Protect Disk     | (E)=disk number  |      ****        |
;29    1D | Get Read Only Vector   |      ****        | (HL)=R/O vector  |
;30    1E | Set File Attributes    | (DE)=FCB address | (A)=dir code     |
;31    1F | Get Addr of Disk Parms |      ****        | (HL)=parm addr   |
;32    20 | Get/Set User Select    | (E)=0FFH get     | (A)=current user |
;33    21 | Read Random Record     | (DE)=long FCB adr| (A)=error code   |
;34    22 | Write Random Record    | (DE)=long FCB adr| (A)=error code   |
;35    23 | Get Size of File       | (DE)=long FCB adr| (r0-2=rec cnt)   |
;36    24 | Set Random Record Num  | (DE)=long FCB adr| (r0-2=rec numb)  |
;37    25 | Reset Drive            | (DE)=drive vector|      ****        |
;38    26 | Not used               |                  |                  |
;39    27 | Not used               |                  |                  |
;40    28 | Write Random with      | (DE)=long FCB adr| (A)=error code   |
;-------------------------------------------------------------------------

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


;.NOTES


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










;.NOTES
;Everything after this point can just be text. It's just general notes. The assembly should stop at end or notes. 
;5.6 System Function Summary BDOS related.
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
;6	6	Direct Console I/O 		E = 0FFH (input)		A = char
;								E = 0FEH (status)		A = status
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
;32	20	Set/Get User Code 		E = 0FFH for Get		A = User Number
;								E = 00 to 0FH for Set	none
;33	21	Read Random				DE = FCB address		A = Error
;34	22	Write Random			DE = FCB address		A = Error Code
;35	23	Compute File Size		DE = FCB address		r0, r1, r2
;36	24	Set Random Record		DE = FCB address		r0, r1, r2
;37	25	Reset Drive				DE = Drive Vector		A = 0
;38	26	Access Drive			not supported			not supported
;39	27	Free Drive				not supported			not supported
;40	28	Write Random w/Fill		DE = FCB				A = error code



 