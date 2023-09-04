; Loki Driver Installation Routine - Installs Driver code and hooks entry point to driver.
; Standard Drivers are at 0040 (Video) and 0048 (Network)
; Drivers have common pages in the first and last 4K page locations ( 0000-0FFF at Page01 and F000-FFFF at Page 10) with the ROOT-CPM process 0.
; Install driver uses random write to copy the driver then hooks the BIOS and exits. It can also unload a driver ( replace with ret ).
; Syntax:
; A>LINSTALL <DRIVER SOURCE> M:<DRIVER LOCATION>



.EQU	BDOS,	$0005

.equ	ADRIVE,	$00
.equ	LDRIVE,	$0B
.equ	MDRIVE,	$0C

.equ	TRACKPORT	,$06
.equ	SECTORPORT	,$07
.equ	MADPORT		,$0C

.equ	MMU,$0F				; The port to read/write MMU bytes to the MMU RAM.
.equ	PID,$0E				; The PID port - Which process is the MMU reflecting?

.equ 	DMA,$0080			; This DMA is suitable. 
.equ	NETWORK,$0048		; Network, Audio, Drives and Comms Hook. 

; Memory Map.
; Common Lower Page - common to all drivers. 4K in size.
; $0000 - Common memory, entry points in the Zero Page. Existing applications from $0100 to $0FFF.
; $1000 - Driver entry, Code and Bitmaps. Resident. 
; $3000 - Video Map - 8 bits - 2040 bytes * 2 ( 8 bits of attributes ). 5x ADM attributes.  
;

.ORG $100 		; Start as a COM file.

INITIALISE:					; Let's initialise the screen, Character map, etc. 
							; Constantly changing as I get it working under LokiOS and CP/M. 
							; Question: Should standard drivers exist already? Eg, Interrupts, Video, Network?

		LD		(SAVEHL),HL			; Save HL in case, if, for some reason, we actually want to save it. 
		LD		HL,$0000
		ADD		HL,SP				; save the SP
		LD		(SAVESTACK),HL
		LD		SP,TEMP_STACK		; New stack location for this routine in location 1. 


		CALL	MAKE_NETWORK_DRIVER	; Create the driver file on M: 
							; Need to put some error checking in here.
							; Once created, we need to set up the MMU to follow the file. 

		CALL	SET_MMU		; Set the MMU to match the FCB. 


		
		CALL 	TRANSFER		; Transfer the BIOS code to the Process. Later do this in CREATE_FILE (Transfer at same time and write it in)
								; It switches to the process, then just loads the driver at $1000 and executes with C=0.
	
		CALL	INTHOOK			; Install the interrupt hook. No going back now. 
								; This sets a hook at $0040 or other interrupt vectors the change the PID and then jump to $1000. 

		LD		A,$00		; Back to process 0.
		OUT		(PID),A

		
		CALL	RESET_MMU			; Drop back to the default MMU process 0. 
		

		LD		C,$00				; Reset NETWORK.
		CALL	NETWORK

		
		LD		DE,SUCCESS
		LD		C,PRINT_STRING
		CALL	BDOS
;		CALL	ERROR_MESSAGE

		LD		(SAVEHL),HL
		LD		HL,(SAVESTACK)
		LD		SP,HL				; Restore the SP. 
		LD		HL,(SAVEHL)


		RET							; Just exit here. 
	
SAVESTACK:	DW	$0000		; Stack location store.
SAVEHL:		DW	$0000		; Temporary location for HL. 




	
RESET_MMU:					; Return to Process 0.
		LD		A,$00		; Process ID for the driver we are installing. 
		OUT		(PID),A		; SHOULD be safe.. SHOULD have the same process information. Unless someone's changed it.
		LD		HL,DRVBDOS	; END of driver pages.
		LD		B,$0E		; Second last page... We'll transfer them backwards.
		LD		C,MMU		; C is the port address for the MMU. 
		ret

SET_MMU:

		LD DE,DRVFCB
		LD	C,OPEN_FILE
		CALL	BDOS				; Get current memory unit allocations from FCB after opening file. 

		LD		A,$08		; Process ID for the driver we are installing. 
		OUT		(PID),A		; SHOULD be safe.. SHOULD have the same process information. Unless someone's changed it.

		LD		HL,FCBCR	; END of driver pages. We'll deduct 1 each time, so will be the last allocation. 
		LD		B,$0F		; last page... We'll transfer them backwards.
		LD		C,MMU		; C is the port address for the MMU. 

SET_MMU_LOOP:	
		DEC		HL
		PUSH	BC
		LD		A,B
		RLC		A
		RLC		A
		RLC		A
		RLC		A
		LD		B,A
		LD		A,(HL)		; Pick up allocation from FCB.

		OUT		(C),A		; Set a block of memory... MMU is set for this page.
		POP		BC
		DJNZ	SET_MMU_LOOP	; Because we use DJNZ, we won't set page 0, which is reserved for the common zero page. 
		ret
	


		
		
INTHOOK:				; Code to hook the Interrupt, Install the device driver, rebuild the handler and write it all to M:
			LD	DE,NETWORK
			LD	HL,INT48
			LD	BC,$08
			LDIR			; Install RST40 hook.

			LD	DE,$0050
			LD	HL,RET50
			LD	BC,$05
			LDIR			; Install RET50 return hook. 
			
			LD	DE,$0055
			LD	HL,RET55
			LD	BC,$07
			LDIR
			
			ret
		
		
		
INT48:					; Standing to copy relative code to move to $0048 (NETWORK). 8 bytes. 
		PUSH	AF		; Protect the A register.
		LD		A,$09	; We're going to run NETWORK as Process 9 (reserved)
		OUT		(PID),A	; Switch to the NETWORK Process.
		JP		$1000	; Jump to the next segment at the 4K Boundary where the real handler lies. 
						; Map - $0000 to $0FFF - Handler, Install routines, Initialise routines. 
						;       $1000 to $1FFF - Routines.
						;       $2000 to $2FFF - More Routines. 
						;       $3000 to $3FFF - More Routines.
						; 		$4000 to $7FFF - Shared Memory and Buffers. 
						
						; RET50 is 5 bytes exactly. 
RET50:					; Return location for 50 where we install the "Return to previous call". 
		OUT		(PID),A	; Switch to the calling process. ( A contains the process - don't set it here. )
		POP		AF		; Recover A ( and maybe other registers )
		ret				; And return to where we were called from. 
						; Note - Return programs from 50 to 5B... 
		NOP				; So I can see a single 0. 
; DO NOT CHANGE THE ABOVE....... below follows on. 

RET55:					; RET55 is exactly 7 bytes. 
		OUT		(PID),A	; Switch to the calling process. 
		POP		AF
		RETI			;  And the RETI version if it's called by an interrupt. 
		NOP
		NOP



RETEND:
						; Marker for the end. 
	
LASTDMA:	DW	$0080	; Place to store the DMA. 

TRANSFER:
		; Later need to change process ID here to map in Process 08 - This memory segment will be stable at 0000 to 0FFF
		; reuse TYPE routine.
		LD		DE,$1000
		LD		(LASTDMA),DE			; Store the DMA. 
		LD		C,Set_DMA_Address		; Function 1A - Set the DMA address to the buffer at 0080.
		CALL	BDOS					; Set DMA to 0080 - This is because the directory will show up here. Saves us copying the filenames too. 

		LD		DE,VIDFCB				; The filename we want is located in the first FCB in lower memory from the Command Line
		LD		C,Open_File				; Open File.
		CALL	BDOS					; Get the first matching file. 

		AND		%11111100				; Mask the result bytes.
		JR		NZ,FILE_OPEN_ERROR

TRANSFER_READ:
		LD		DE,VIDFCB					; The filename we want remains located in the first FCB in lower memory from the Command Line
		LD		C,Read_Sequential					; read record
		CALL	BDOS					; Get the first matching file. 
		OR		A						; Check for file status - And exit if we're at EOF ( we already exited earlier for other result other than read OK )

		JR		NZ,TRANSFER_EXIT			; Nothing more to read. 		
;;;		CALL	PRINT_RECORD			; Print the record

		LD		HL,(LASTDMA)			; Step the DMA pointer along. 
		LD		DE,$0080
		ADD		HL,DE
		LD		(LASTDMA),HL			; DMA is not further along and saved. 
		EX		DE,HL					; Set DMA back in DE. 

		LD		C,Set_DMA_Address		; Function 1A - Set the DMA address to the buffer at 0080.
		CALL	BDOS					; Set DMA to 0080 - This is because the directory will show up here. Saves us copying the filenames too. 
		
		JR		TRANSFER_READ
		
TRANSFER_EXIT:		
		CALL	CRLF					; Start next on a new line. 
		ret

		
CRLF:	LD		E,$0D
		LD		C,Console_Output
		CALL	BDOS
		LD		E,$0A
		LD		C,Console_Output
		CALL	BDOS
		ret		

PRINT_RECORD:
		LD		B,128
		LD		HL,$0080				; DMA address.
PRINT_RECORD_LOOP:
		PUSH	HL
		PUSH	BC
		LD		E,(HL)
		LD		C,Console_Output
		CALL	BDOS
		POP		BC
		POP		HL
		
		LD		A,(HL)
		CP		$1A						; CTRL'Z'
		RET		Z						; Is end of file. Exit here now if that happens. 
		
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
;		ret

CANT_OPEN_PROCESS:
		LD		DE,PROCESS_ERROR
		JP		ERROR_MESSAGE

ERROR_MESSAGE:
		LD		C,Print_String
		CALL	BDOS
		RST		0						; restart - do not continue. 
		RET
	
COMMAND_BAD: db 'Command Unknown or Executable not located',$0D,$0A,'$'	
		ret
FILENAME_BAD:	db 'Filename had unexpected charactors or was malformed',$0D,$0A,'$'

COMMAND_HINT:	db 'Type HELP for a list of commands',$0D,$0A,'$'	

FILEOPEN_ERROR:	db	'File could not be opened',$0D,$0A,'$'

FILEREAD_ERROR:	db	'Error reading file',$0D,$0A,'$'

NODEST_ERROR:	db	'Bad Source or Destination',$0D,$0A,'$'	

FILE_COPY_ERROR:db	'Error copying file',$0D,$0A,'$'

SUCCESS:	db	'New network driver installed successfully.',$0D,$0A,'$'

PROCESS_ERROR:	DB	'Could not open process file on M:',$0D,$0A,'$'






MAKE_NETWORK_DRIVER:
		LD		DE,DRVFCB		; Location of new file FCB.
		LD		C,MAKE_FILE		; Open the file on M:
		CALL	BDOS
		AND		$FC
		JP		NZ,CANT_OPEN_PROCESS

		LD		C,CLOSE_FILE
		LD		DE,DRVFCB
		CALL	BDOS			; Now close the file, so it's written.
		
		LD		C,OPEN_FILE
		LD		DE,DRVFCB
		CALL	BDOS			; And make sure we can open it. 
		AND		$FC
		JP		NZ,CANT_OPEN_PROCESS
		
		LD		A,$01										; Make sure the first segment is mapped to the same as CP/M root, and the last to BDOS/BIOS, 
		LD		(DRVZERO),A		; Set the first page.
		LD		A,$10
		LD		(DRVBDOS),A		; Set the BDOS page. 
		LD		A,$0F
		LD		(DRVDRIBDOS),A	; And set it just in case the DRI BDOS is in place. Software can reallocate this anyway. 
								; Next we need to allocate 3 blocks of free memory. 
		
		LD		DE,DRVFCB
		LD		C,CLOSE_FILE
		CALL	BDOS

		
								; Did we get it working? Let's random write it. 

		LD		A,$3F			; Next 4K boundary.
		LD		(FCBR1),A
		CALL	SET_ALLOCATION

		LD		A,$5F			; Next 4K boundary.
		LD		(FCBR1),A
		CALL	SET_ALLOCATION
		
		LD		A,$7F			; Next 4K boundary.			Note - separate transfer and allocation setting to avoid short files failing to allocate enough.
		LD		(FCBR1),A									; Anything in the range of $60 to $7F is suitable, however the RC field will reflect the final random write.
		CALL	SET_ALLOCATION


								
								
		LD		DE,DRVFCB
		LD		C,CLOSE_FILE
		CALL	BDOS	
		
		ret

SET_ALLOCATION:
		LD		DE,DRVFCB
		LD		C,WRITE_RANDOM
		CALL	BDOS
		ret

						; Incoming Driver File.
VIDFCB:					; Source is VIDEO512BIN on L:
	DB	$0A				; Driver happens to be on L: but later can load from anywhere. Use this to change video modes too. 
	DB	'NET     BIN',$00,$00,$00,$00
	DB  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0
	DB  $00
	DB	$00
	DB	$00
	DB	$00

						; Driver File to build on M:
DRVFCB:					; Random FCB to write driver to MAD space at $1000... 
	DB	$0D				; Process will ALWAYS be M: drive. 
	DB	'NETWORK DRV',$00,$00,$00
DRVRC:		DB	$0						; Set RC Record Count to $20 - ie, all initial locations taken. We will subsequently assign three more allocations. 
DRVZERO:	DB  $0				; Establish the first file location. 
DRV0:		DB	0			; Default driver locations.
DRV1:		DB	0			; Default driver locations.
DRV2:		DB	0			; Default driver locations.
DRV_PAGES:	DB	0,0,0,0, 0,0,0,0, 0,0			; Reserved for paging in NETWORK for direct access. ( Eg, Scroll etc. ) but we don't need them yet. 
DRVDRIBDOS:	DB	$00								; Because the DRI BDOS is too fat, if it's installed. 
DRVBDOS:	DB	$00								; Standard LOKI FDOS.
FCBCR:		DB  $00
FCBR1:		DB	$00
FCBR2:		DB	$00
FCBR3:		DB	$00
	


.ORG	$0FF0
TEMP_STACK:
												; Put a temporary stack right at the end. 


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
