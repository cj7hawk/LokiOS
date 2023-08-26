; Loki startup routines - COM - Can execute scripts or other activities - As a COM.
; THIS is the program that configured the system, loads up startup files, etc. 
; 
;
.EQU	BDOS,	$0005

.equ	ADRIVE,	$00
.equ	LDRIVE,	$0B
.equ	MDRIVE,	$0C

.equ	TRACKPORT	,$06
.equ	SECTORPORT	,$07
.equ	MADPORT		,$0C

.ORG $100 		; Start as a COM file.

		LD		DE,Startup_Message
		LD		C,Print_String
		CALL	BDOS

		LD		C,SELECT_DISK	; Select M: as the disk.
		LD		E,MDRIVE
		CALL	BDOS

		
		CALL	RST1
		CALL	RST2
		CALL	RST3
		CALL	RST4
		CALL	RST5
		CALL	RST6
		CALL	RST7
		
		CALL	BIOS1
		CALL	BIOS2
		CALL	BIOS3
		CALL	BIOS4
	

									; Multi Extent and Multi-Entry Files to mark. 
		CALL	NVMDRIVE			; Non Volatile Memory ( 256 bytes )
		CALL	NVMD2
		CALL	NVMD3
		CALL	NVMD4				; Set aside 256K for NVM. 

;		CALL	VIDEODRV			; Set up video driver blank. 
;		CALL	NETWORK				; Set up video driver blank. 

;		CALL	UPPERMEMORY					; Mark Upper Memory as unavailable.  - Can leave out - It's all marked now. 


		LD		C,Reset_Disk_System			; Force re-reading the disk, mark allocations as used etc. 
		CALL	BDOS
	
		LD		DE,TESTMESSAGE	
		LD		C,PRINT_STRING
		CALL	BDOS						; Print initialisation string. 
	
		ret

TESTMESSAGE:	DB	'2022 Sinclair LOKI project.$'
TESTEND:	DB '$' ; in case I forget. 




MARK_FCB: DW	$0000		; The FCB we are storing this form. 
AV_TABLE: DW	$0000		; Where the AV table is for the current disk ( Memory M: Disk )
BLOCK:	DB		$00			; Block in memory
ALLOCATION:	DB	$00			; Allocation in the extent(s)



RSTENTRIES:

		ret

VIDEODRV:
		LD		DE,VID
		LD		(MARK_FCB),DE
		LD		C,MAKE_FILE		;  	Create the file entry with BDOS. 
		CALL	BDOS		
		LD		D,$00		; First Allocation
		LD		E,$01		; Block 1 
		LD		B,$01		; 1 block presently assigned to each file.  	
		JP		VXLOOP	


NETWORK:
		LD		DE,NET
		LD		(MARK_FCB),DE
		LD		C,MAKE_FILE		;  	Create the file entry with BDOS. 
		CALL	BDOS		
		LD		D,$00		; First Allocation
		LD		E,$01		; Block 1 
		LD		B,$01		; 1 block presently assigned to each file.  	
		JP		VXLOOP		

RST1:						; Place stand in files for each interrupt handler.
		LD		DE,RST8
		LD		(MARK_FCB),DE
		LD		C,MAKE_FILE		;  	Create the file entry with BDOS. 
		CALL	BDOS		
		LD		D,$00		; First Allocation
		LD		E,$01		; Block 1 
		LD		B,$01		; 1 block presently assigned to each file.  	
		JP		VXLOOP		
RST2:						; Place stand in files for each interrupt handler.
		LD		DE,RST10
		LD		(MARK_FCB),DE
		LD		C,MAKE_FILE		;  	Create the file entry with BDOS. 
		CALL	BDOS		
		LD		D,$00		; First Allocation
		LD		E,$01		; Block 1
		LD		B,$01		; 1 block presently assigned to each file.  	
		JP		VXLOOP	
RST3:						; Place stand in files for each interrupt handler.
		LD		DE,RST18
		LD		(MARK_FCB),DE
		LD		C,MAKE_FILE		;  	Create the file entry with BDOS. 
		CALL	BDOS		
		LD		D,$00		; First Allocation
		LD		E,$01		; Block 1
		LD		B,$01		; 1 block presently assigned to each file.  	
		JP		VXLOOP	
RST4:						; Place stand in files for each interrupt handler.
		LD		DE,RST20
		LD		(MARK_FCB),DE
		LD		C,MAKE_FILE		;  	Create the file entry with BDOS. 
		CALL	BDOS		
		LD		D,$00		; First Allocation
		LD		E,$01		; Block 1 
		LD		B,$01		; 1 block presently assigned to each file.  	
		JP		VXLOOP	
RST5:						; Place stand in files for each interrupt handler.
		LD		DE,RST28
		LD		(MARK_FCB),DE
		LD		C,MAKE_FILE		;  	Create the file entry with BDOS. 
		CALL	BDOS		
		LD		D,$00		; First Allocation
		LD		E,$01		; Block 1
		LD		B,$01		; 1 block presently assigned to each file.  	
		JP		VXLOOP	
RST6:						; Place stand in files for each interrupt handler.
		LD		DE,RST30
		LD		(MARK_FCB),DE
		LD		C,MAKE_FILE		;  	Create the file entry with BDOS. 
		CALL	BDOS		
		LD		D,$00		; First Allocation
		LD		E,$01		; Block 1 
		LD		B,$01		; 1 block presently assigned to each file.  	
		JP		VXLOOP	
RST7:						; Place stand in files for each interrupt handler.
		LD		DE,RST38
		LD		(MARK_FCB),DE
		LD		C,MAKE_FILE		;  	Create the file entry with BDOS. 
		CALL	BDOS		
		LD		D,$00		; First Allocation
		LD		E,$01		; Block 1 
		LD		B,$01		; 1 block presently assigned to each file.  	
		JP		VXLOOP	


BIOS1:						; Place stand in files for each interrupt handler.
		LD		DE,BIOS1FCB
		LD		(MARK_FCB),DE
		LD		C,MAKE_FILE		;  	Create the file entry with BDOS. 
		CALL	BDOS		
		LD		D,$00		; First Allocation
		LD		E,$E0		; Block E0 
		LD		B,$04		; 4 block presently assigned to each extension bios file.  	
		JP		VLOOP	

BIOS2:						; Place stand in files for each interrupt handler.
		LD		DE,BIOS2FCB
		LD		(MARK_FCB),DE
		LD		C,MAKE_FILE		;  	Create the file entry with BDOS. 
		CALL	BDOS		
		LD		D,$00		; First Allocation
		LD		E,$E4		; Block E0 
		LD		B,$04		; 4 block presently assigned to each extension bios file.  	
		JP		VLOOP	
		
BIOS3:						; Place stand in files for each interrupt handler.
		LD		DE,BIOS3FCB
		LD		(MARK_FCB),DE
		LD		C,MAKE_FILE		;  	Create the file entry with BDOS. 
		CALL	BDOS		
		LD		D,$00		; First Allocation
		LD		E,$E8		; Block E0 
		LD		B,$04		; 4 block presently assigned to each extension bios file.  	
		JP		VLOOP	

BIOS4:						; Place stand in files for each interrupt handler.
		LD		DE,BIOS4FCB
		LD		(MARK_FCB),DE
		LD		C,MAKE_FILE		;  	Create the file entry with BDOS. 
		CALL	BDOS		
		LD		D,$00		; First Allocation
		LD		E,$EC		; Block E0 
		LD		B,$04		; 4 block presently assigned to each extension bios file.  	
		JP		VLOOP	

UPPERMEMORY:						; Place stand in files for each interrupt handler.
		LD		DE,UPPER
		LD		(MARK_FCB),DE
		LD		C,MAKE_FILE		;  	Create the file entry with BDOS. 
		CALL	BDOS		
		LD		D,$00		; First Allocation
		LD		E,$80		; Block E0 
		LD		B,$80		; 4 block presently assigned to each extension bios file.  	
		JP		VLOOP	

NVMDRIVE:				; Assume Video1 at least is used by the system. Video1 sits at 80000 to 9FFFF

		LD		DE,NVM1
		LD		(MARK_FCB),DE
		LD		C,MAKE_FILE		;  	Create the file entry with BDOS. 
		CALL	BDOS	
		
		LD		D,$00		; First Allocation
		LD		E,$80		; Block 80 = 512K boundary. 
		LD		B,$10		; 40 blocks for NVM = 256K 
		JP		VLOOP
		
NVMD2:
		LD		DE,NVM2
		LD		(MARK_FCB),DE
		LD		C,MAKE_FILE		;  	Create the file entry with BDOS. 
		CALL	BDOS	
		
		LD		D,$10		; First Allocation + 10.... 
		LD		E,$90		; Block 80 = 512K boundary. 
		LD		B,$10		; 40 blocks for NVM = 256K 
		JP		VLOOP

NVMD3:
		LD		DE,NVM3
		LD		(MARK_FCB),DE
		LD		C,MAKE_FILE		;  	Create the file entry with BDOS. 
		CALL	BDOS	
		
		LD		D,$20		; First Allocation + 10.... 
		LD		E,$A0		; Block 80 = 512K boundary. 
		LD		B,$10		; 40 blocks for NVM = 256K 
		JP		VLOOP

NVMD4:
		LD		DE,NVM4
		LD		(MARK_FCB),DE
		LD		C,MAKE_FILE		;  	Create the file entry with BDOS. 
		CALL	BDOS	
		
		LD		D,$30		; First Allocation + 10.... 
		LD		E,$B0		; Block 80 = 512K boundary. 
		LD		B,$10		; 40 blocks for NVM = 256K 
		JP		VLOOP		


VXLOOP:							; Also include the BDOS/BIOS blocks. 
		CALL	VLOOP
		LD		IX,(MARK_FCB)		; FCB location.
		LD		A,$0FH
		LD		(IX+ALLOC15),A
		INC		A
		LD		(IX+ALLOC16),A

		LD		C,CLOSE_FILE		; And close the file to save the BDOS / BIOS allocation. 
		LD		DE,(MARK_FCB)
		CALL	BDOS
		ret

VLOOP:
		PUSH	BC
		PUSH	DE
		LD		(BLOCK),DE	; Store Block and Allocation.
		CALL	Mark_Allocation
		POP 	DE
		POP		BC
		INC		D
		INC		E
		DJNZ	VLOOP	
		
		ret



Mark_Allocation:			; Marks an allocation as used by a specified block, and makes sure that block is marked in the Allocation Vector tables. 
									; D is FIRST ALLOCATION (Usually $00)
									; E is the Memory Block number (eg, typically 1 to FF )
									; B is the number of blocks to allocate.
		LD		A,D
		LD		(ALLOCATION),A		; Set Allocation as per D.
		
		LD		A,(ALLOCATION)
		RRA
		RRA							; Divide by four - Four allocations per extent. ( Because we use 32 records / allocation )

		AND		%00111111			; 64 possible extent numbers. 

		LD		IX,(MARK_FCB)		; FCB location. 
		LD		(IX+EX),A			; Store the Extent number. 
		

;LD DE,(MARK_FCB)					; Debug should show the FCB pre-change. 
;OUT (20),A	; debug - show FCB.  

		
		LD		A,(ALLOCATION)
		AND		$0F
		INC		A
		LD		B,A					; Set allocation into B. (1,2,3 or 4)

		LD		IX,(MARK_FCB)		; FCB
		LD		A,(IX+CR)			; Current Record, not Record Counter. 
		ADD		A,$20				; 32 records per allocation
		DEC		A
		AND		$7F					; Only 128 records/allocation. 
		INC		A
		LD		(IX+CR),A			; Add them to the FCB
	
		LD		A,(BLOCK)			; Which block should we set in the allocation?.		
		DEC		IX	
MARK_LP1:
		INC		IX
		DJNZ	MARK_LP1			; Shuffle IX so that ALLOC1 points to the correct allocation.
		LD		(IX+ALLOC1),A		; Write the block vector to the allocation.

;		DON'T USE BELOW CODE - It was to mark the AV table - But we can do that post-all-changes with a DISK RESET. 
; 		DISK RESET POPULATES THE AV TABLES for Drive M:
;									; Next we determine the bit to set in the AV tables.
;		LD		A,(BLOCK)
;		RRA
;		RRA
;		RRA
;		AND		%00011111			; Position in AV table for block.
;		LD		E,A
;		LD		D,$00				; DE is loaded with the offset for the vector byte.
;		LD		HL,(AV_TABLE)
;		ADD		HL,DE				; HL holds the absolute vector byte location in memory. 
;		
;		
;		LD		A,(BLOCK)
;		AND		%00000111			; Get the bit within the byte for the block.
;		INC		A
;		LD		B,A
;		XOR		A
;		SCF
;
;MARK_LP2:
;		RRA
;		DJNZ	MARK_LP2			; Move the bit along.
;									; A now holds the B
;									
;		OR		(HL)				; Or A with the vector byte.
;		LD		(HL),A				; And update the byte in memory.

		XOR		A					; These five lines are installed as without them, the "Close" operation fails to save the extent under DRI BDOS. 
		LD		IX,(MARK_FCB)		; FCB location. 
		LD		(IX+S1),A			; Note sure why CP/M 2.2 plays with these "reserved" values.
		LD		(IX+S2),A
		LD		A,(IX+CR)
		LD		(IX+RC),A			; Transfer the CR to RC. 

		LD		C,CLOSE_FILE		; And close the file to save the allocation. 
		LD		DE,(MARK_FCB)

		CALL	BDOS
		ret

Startup_Message:	db	$0D,$0A,'LokiOS running. Building MAD system',$0D,$0A,'$'


;LDRIVEFILE:		; FCB for the initial OS. 
;		DB		$0D,'L:_DRIVE','DSK',0,0,0,0			; Root System - Process 0.  
;		DB		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;		DB		0,0,0,0		


;VIDEO1:		; FCB for the initial OS. 
;		DB		$0D,'Screen_1','IMG',0,0,0,0			; Root System - Process 0.  
;		DB		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;		DB		0,0,0,0	

NVM1:		; FCB for the initial OS. 
		DB		$0D,'NVMemory','DSK',0,0,0,0			; NVRAM Disk 0.  
		DB		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		DB		0,0,0,0	


NVM2:		; FCB for the initial OS. 
		DB		$0D,'NVMemory','DSK',4,0,0,0			; NVRAM Disk 1. 
		DB		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		DB		0,0,0,0	


NVM3:		; FCB for the initial OS. 
		DB		$0D,'NVMemory','DSK',8,0,0,0			; NVRAM Disk 1. 
		DB		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		DB		0,0,0,0	
		

NVM4:		; FCB for the initial OS. 
		DB		$0D,'NVMemory','DSK',$0C,0,0,0			; NVRAM Disk 1. 
		DB		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		DB		0,0,0,0	

		
; RST FCBs.
RST8:		; FCB for the initial OS. 
		DB		$0D,'RST-#08H','INT',0,0,0,0			; Root System - Process 1.  
		DB		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		DB		0,0,0,0	
		
RST10:		; FCB for the initial OS. 
		DB		$0D,'RST-#10H','INT',0,0,0,0			; Root System - Process 2.  
		DB		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		DB		0,0,0,0

RST18:		; FCB for the initial OS. 
		DB		$0D,'RST-#18H','INT',0,0,0,0			; Root System - Process 3.  
		DB		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		DB		0,0,0,0

RST20:		; FCB for the initial OS. 
		DB		$0D,'RST-#20H','INT',0,0,0,0			; Root System - Process 4.  
		DB		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		DB		0,0,0,0

RST28:		; FCB for the initial OS. 
		DB		$0D,'RST-#28H','INT',0,0,0,0			; Root System - Process 5.  
		DB		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		DB		0,0,0,0

RST30:		; FCB for the initial OS. 
		DB		$0D,'RST-#30H','INT',0,0,0,0			; Root System - Process 6.  
		DB		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		DB		0,0,0,0

RST38:		; FCB for the initial OS. 
		DB		$0D,'RST-#38H','INT',0,0,0,0			; Root System - Process 7.  
		DB		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		DB		0,0,0,0
		
BIOS1FCB:		; FCB for the initial OS. 
		DB		$0D,'VIDEOROM','DSK',0,0,0,0			; Root System - Process 7.  
		DB		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	
		DB		0,0,0,0
		
BIOS2FCB:		; FCB for the initial OS. 
		DB		$0D,'DISK_ROM','DSK',0,0,0,0			; Root System - Process 7.  
		DB		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		DB		0,0,0,0
		
BIOS3FCB:		; FCB for the initial OS. 
		DB		$0D,'UNASSIG1','DSK',0,0,0,0			; Root System - Process 7.  
		DB		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		DB		0,0,0,0
		
BIOS4FCB:		; FCB for the initial OS. 
		DB		$0D,'UNASSIG2','DSK',0,0,0,0			; Root System - Process 7.  
		DB		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		DB		0,0,0,0
				
		
UPPER:		; FCB for the initial OS, upper memory. 
		DB		$0D,'UPPERMEM','BLK',0,0,0,0			; Upper Memory above $80000
		DB		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		DB		0,0,0,0

VID:	
		DB		$0D,'VIDEO   ','DRV',0,0,0,$80		; Video Driver Blank.
		DB		$01,0,0,0,0,0,0,0,0,0,0,0,0,0,$0F,$10
		DB		$80,0,0,0

NET:
		DB		$0D,'NETWORK ','DRV',0,0,0,0			; Network Driver Blank.
		DB		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		DB		0,0,0,0						
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
EQU		ALLOC15,$1E
EQU		ALLOC16,$1F
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
