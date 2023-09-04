; Move CPM Alternative CCP file to replace existing LOKI CCP file.
;

.equ BDOS,$0005
.equ MYBDOS,$F000
.equ DRBDOS,$ED00

.org $0100


; Let's reuse the CCP load routine from the BDOS to load a replacement CCP.

;out ($13),A ; turn on debug
CALL	CCP_LOAD
RST 0





;DR F1 F2 F3 F4 F5 F6 F7 F8 T1 T2 T3 EX S1 S2 RC  .FILENAMETYP...
;AL AL AL AL AL AL AL AL AL AL AL AL AL AL AL AL  ...............
;CR R0 R1 R2                                      ....		
CCP_FCB:			DB $00																	; FCB for Command and Control Process. 
CCP_FILENAME:		DB	'altbdos bin'														; CCP Filename - and it's drive L:	
CCP_FILENAME_CNT:	DB $00,$00,$00,$00														; File variable tracking. 
CCP_FILENAME_ALL:	DB $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$00	; For the allocations.
CCP_FILENAME_HND:	DB $00,$00,$00,$00														; Four working bytes. 

CCP_PTR:			DW	$0000					; Where in the process are we? How much of the CCP have we written?


CCP_LOAD:								; Here's where we load the CCP.... I really want this at the end of the BDOS, and would be nice to have in the BIOS.
		LD		DE,CCPPRELOAD
		LD		C,9						; BDOS Print String. 
		CALL	BDOS					; Message. 

		LD		DE,$0080				; Default DMA at 0080 -let's use that to save memory in the BDOS. 
		LD		C,26					; Function 1A - Set the DMA address to the buffer at 0080.
		CALL	BDOS					; Set DMA to 0080 - This is because the directory will show up here. Saves us copying the filenames too. 

		LD		DE,CCP_FCB				; Set up the file open for L:CCP.BIN
		LD		C,15					; Function 15 = Open File.
		CALL	BDOS					; Get the first matching file. 
		AND		%11111100				; Mask the result bytes.
		JR		NZ,CCP_ERROR			; If the file didn't open, then exit.  AND will clear CF = error. 	



		
;		LD		DE,DRBDOS				; BDOS destination starts at DRBDOS -
		LD		DE,$8000				; temp store before transfer. 
		LD		(CCP_PTR),DE			; Store the pointer that contains where we will store the file. 
CCP_READ:
		LD		DE,CCP_FCB				; The CCP is located on L: and is usually the first file, but this is enough to find it on L:
		LD		C,20					; BDOS Function 20 = read record
		CALL	BDOS					; Get the first matching file. 
		OR		A						; Check for file status - And exit if we're at EOF ( we already exited earlier for other result other than read OK )
		JP		NZ,CCP_RUN				; Once we are fully loaded, try to execute the CCP. 				


;		LD		HL,$107F				; Reserved space before we transfer file.
;		LD		DE,(CCP_PTR)
;		ADD		HL,DE
;		JR		C,BDOSFAIL				; We will overwrite original BDOS at $F000 if we proceed - so error out. 
		
		LD		HL,$0080				; Transfer the file to memory. 
		LD		DE,(CCP_PTR)
		LD		BC,$80
		LDIR
		LD		(CCP_PTR),DE			; Store the destination pointer for the next cycle. 

		JR		CCP_READ				; Loop until completely loaded - Note: CCP can ALSO include the BDOS and BIOS if both need to be overwritten. Cool eh?

CCP_ERROR:								; THIS SHOULD NEVER HAPPEN. ROM IS CORRUPT. WTF? Do I even need to do this?
		LD		DE,CCPERROR				; DE = Error string. 
		LD		C,9						; Function 9 = Display String pointed to by DE
		CALL	BDOS
;		HALT							; Nothing else to do... Hopefully they can reburn the boot eprom. Maybe they forgot the file?
		JP		$0000					; The halt should not go away. 

CCP_RUN:
		LD		C,9
		LD		DE,BDOSSUCCESS
		CALL	BDOS

;		JP	$0000 ; exit. 



	LD		DE,M1
	LD		C,PRINT_STRING ; print string.
	CALL	BDOS

LD	HL,$8000
LD	DE,DRBDOS
LD	BC,$0E00 ; 2 pages short of a block. 
LDIR		; transfer it AS A BLOCK. 

LD		HL,DRBDOS+$06
LD		($0006),HL	; write new BDOS location into the BDOS Hook. 
LD	C,SELECT_DISK
LD	E,$0B	; disk  L I think.
CALL	BDOS

LD	C,RESET_DISK_SYSTEM
CALL BDOS
	
	LD		DE,M2
	LD		C,PRINT_STRING
	CALL	BDOS

; out ($10),A	; Display debub notice
; out ($13),A ; turn on debug

;	CALL	OTHERTESTS
	
; out (10),A	; turn off debug	
	
	ret
	

;;

M1: DB 'Preparing to migrate BDOS and install hook.',$0D,$0A,'$'

M2: DB 'BDOS Hook installed...',$0D,$0A,'$'



BDOSFAIL:
		LD		C,9	; Print
		LD		DE,BDOSOVER
		CALL	BDOS
		JP		$0000
		

CCPPRELOAD:			DB	'Loading ALTBDOS.BIN to E200 - Not forking BDOS hook',$0A,$0D,'$'		
CCPERROR:			DB	'ALTBDOS.BIN not found. Exiting',$0A,$0D,'$'

BDOSOVER:		DB	'Error: Attempted to overwrite existing BDOS - Exiting.',$0D,$0A,'$'
BDOSSUCCESS:	DB	'BDOS Successfully loaded at E200',$0D,$0A,'$'




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






; ########################################## Equates.		

		
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






	
	
.END

