; LOKI OS. Logical Output and Keyboard Interface. 
; LOKI OS is CP/M Compatible. At least it uses the same code calls, but is intended to "obfuscate" the hardware through a TTL Serial Bus @ 1Mbps.
; The "Megabus" is a serial shared networking protocol that allows calls for any function to be made over serial - eg, Disk over serial...  Console over serial.
; Protocol is basically SLIP with some new concepts. So it's talking IP. 
;
;
; DEFB - Byte or Bytes. 
; DEFW - Primarily for strings. 
; DEFC - Define Complex - Like DEFB but with more commands - including ^ for PC.  Only for one byte. 
; Test file.

;PORTS
.equ	CONSTATUS	,$00
.equ	CON			,$01
.equ	LPT			,$02
.equ	AUX			,$03
.equ	DRIVEACTIVE	,$04
.equ	DRIVE		,$05
.equ	TRACKPORT	,$06
.equ	SECTORPORT	,$07
.equ	DMALOW		,$08
.equ	DMAHIGH		,$09
.equ	MADPORT		,$0C	; MAD is port C - Memory As Disk ( Read/Write the memory location formed by Register B(7 bits) + sector(Bit8-11) + Track (Bit12-19(
.equ	SYSTEM 		,$0D	; System port, reset, startbit etc. Write 02H to port to cause a hardware reset. 

.equ	BDOS,$0005


.equ	STARTLOCATION			, $FC00
.equ	CCP_WARM_BOOT			, STARTLOCATION+$003D	; 3 bytes for vector to CCP Warm Boot. (ie, CCP + 3).
.equ	CCP_VECTOR				, STARTLOCATION+$003E	; Where the CCP vector is stored. D000 by default. 
.equ	DEFAULT_CCP				, $D003					; Warm Boot and Clear input buffer. 
.equ 	DISK_PARAMETER_HEADERS	, STARTLOCATION+$0040


.ORG STARTLOCATION

BIOSJUMPTABLE:
	JP	BOOT	;-3: Cold start routine - CAUSES A HARDWARE RESET TO OCCUR. 
	JP	WBOOT	; 0: Warm boot - reload command processor CCP via BDOS.
	JP	CONST	; 3: Console status - A=0 No character ready, A=FF character waiting to be read.
	JP	CONIN	; 6: Console input - Wait until character then A=character.
	JP	CONOUT	; 9: Console output - Write C register  to screen.
	JP	LIST	;12: Printer output - Wait until ready, then write C register to printer.
	JP	PUNCH	;15: Paper tape punch output - Wait until ready then write C to Punch Reader ( or AUX ). Will use for TAPE interfaces. 
	JP	READER	;18: Paper tape reader input - Wait until ready, then return A=Character. If not implemented, return 26 ( Ctrl Z ) Will use for TAPE interfaces.
	JP	HOME	;21: Move disc head to track 0
	JP	SELDSK	;24: Select disc drive	- C Register = disk 0...F. Enter with E=0 or E=FF. 
				; If bit 0 of E is 0, then the disc is logged in as if new; if the format has to be determined from the boot sector, for example, this will be done.
				;If bit 0 if E is 1, then the disc has been logged in before. The disc is not accessed; the DPH address (or zero) is returned immediately.
				;SELDSK returns the address of a Disc Parameter Header in HL. The exact format of a DPH varies between CP/M versions; note that under CP/M 3, 
				;	the DPH is in memory bank 0 and probably not visible to programs. If the disc could not be selected it returns HL=0.
	JP	SETTRK	;27: Set track number - Track in BC - Is a word - Starts at 0. 
	JP	SETSEC	;30: Set sector number - Sector in BC - Is a word. Starts at 1. I think. Will find out. 
	JP	SETDMA	;33: Set DMA address - Set DMA address. 
	JP	READ	;36: Read a sector - To DMA address. A=0 success. A=1 unrecoverable error. A=FF Media changed.
	JP	WRITE	;39: Write a sector - C contains blocking code.  0=deferred. 1=immediate. 2=Can be deferred. No preread necessary. A=0 success. 
				;	A=1 unrecoverable error. A=FF Media changed.
;In CP/M 2 and later, the following extra jumps appear:
	JP	LISTST	;42: Status of list device - A=0 Printer not ready. A=FF -printer ready. 
	JP	SECTRAN	;45: Sector translation for skewing DE=Tableaddress (IGNORE DE). BC=Incoming sector (logical). HL=Translated sector (physical) . Return BC. 
;In CP/M 3, a further set of jumps is present:
;	JMP	CONOST	;48: Status of console output
;	JMP	AUXIST	;51: Status of auxiliary input
;	JMP	AUXOST	;54: Status of auxiliary output
;	JMP	DEVTBL	;57: Address of devices table
;	JMP	DEVINI	;60: Initialise a device
;	JMP	DRVTBL	;63: Address of discs table
;	JMP	MULTIO	;66: Read/write multiple sectors
;	JMP	FLUSH	;69: Flush host buffers
;	JMP	MOVE	;72: Move a block of memory
;	JMP	TIME	;75: Real time clock
;	JMP	SELMEM	;78: Select memory bank
;	JMP	SETBNK	;81: Select bank for DMA operation
;	JMP	XMOVE	;84: Preload banks for MOVE
;	JMP	USERF	;87: System-depedent functions
;	JMP	RESERV1	;90: Reserved
;	JMP	RESERV2	;93: Reserved

;DPBMARKER:	DB 'DPB>'

.ORG	CCP_WARM_BOOT				; Should be around BIOS+$003D
		JP	DEFAULT_CCP			; Default CCP should be D000
;		JP	$D000
		
.ORG	DISK_PARAMETER_HEADERS	
	
DPBASE:				; DISK 0 (A) - Disk Parameter Header for Disk 0
;org ^
		DW	TRANS0		; Put in label for vector here if translation table. 00=no translate - Same as L: drive
		DW	00		; Current Track selected.
		DB	00		; Current Sector selected.
		DB  00		; PID for BIOS for disk. (0 means handled by the default BIOS)
 		DW	00		; B
		DW	DIRBF	; 128 byte scratchpad buffer for file directory. ( used by BDOS )
		DW	DPBLOKIA	; Disk Parameter Block. 
		DW	CHK00		; Check Vector. 
		DW	ALL00	; Allocation Vector. 


					; DISK 1 (B) - Disk Parameter Header for Disk 0
		DW	TRANS0		; Put in label for vector here if translation table. 00=no translate - Same as L: drive
		DW	00		; Current Track selected.
		DB	00		; Current Sector selected.
		DB  00		; PID for BIOS for disk. (0 means handled by the default BIOS)
 		DW	00		; B
		DW	DIRBF	; 128 byte scratchpad buffer for file directory. ( used by BDOS )
		DW	DPBLOKIB	; Disk Parameter Block. 
		DW	CHK01		; Check Vector. 
		DW	ALL00	; Allocation Vector. 

					; DISK 2 (C) - Disk Parameter Header for Disk 0
		DW	TRANS0		; Put in label for vector here if translation table. 00=no translate - Same as L: drive
		DW	00		; Current Track selected.
		DB	00		; Current Sector selected.
		DB  00		; PID for BIOS for disk. (0 means handled by the default BIOS)
 		DW	00		; B
		DW	DIRBF	; 128 byte scratchpad buffer for file directory. ( used by BDOS )
		DW	DPBLOKIA	; Disk Parameter Block. 
		DW	CHK02		; Check Vector. 
		DW	ALL00	; Allocation Vector. 
		
					; DISK 3 (D) - Disk Parameter Header for Disk 0
		DW	TRANS0		; Put in label for vector here if translation table. 00=no translate - Same as L: drive
		DW	00		; Current Track selected.
		DB	00		; Current Sector selected.
		DB  00		; PID for BIOS for disk. (0 means handled by the default BIOS)
 		DW	00		; B
		DW	DIRBF	; 128 byte scratchpad buffer for file directory. ( used by BDOS )
		DW	DPBLOKIA	; Disk Parameter Block. 
		DW	CHK03		; Check Vector. 
		DW	ALL00	; Allocation Vector. 

					; DISK 4 (E) - Disk Parameter Header for Disk 0
		DW	TRANS0		; Put in label for vector here if translation table. 00=no translate - Same as L: drive
		DW	00		; Current Track selected.
		DB	00		; Current Sector selected.
		DB  00		; PID for BIOS for disk. (0 means handled by the default BIOS)
 		DW	00		; B
		DW	DIRBF	; 128 byte scratchpad buffer for file directory. ( used by BDOS )
		DW	DPBLOKIA	; Disk Parameter Block. 
		DW	CHK04		; Check Vector. 
		DW	ALL00	; Allocation Vector. 

					; DISK 5 (F) - Disk Parameter Header for Disk 0
		DW	TRANS0		; Put in label for vector here if translation table. 00=no translate - Same as L: drive
		DW	00		; Current Track selected.
		DB	00		; Current Sector selected.
		DB  00		; PID for BIOS for disk. (0 means handled by the default BIOS)
 		DW	00		; B
		DW	DIRBF	; 128 byte scratchpad buffer for file directory. ( used by BDOS )
		DW	DPBLOKIA	; Disk Parameter Block. 
		DW	CHK05		; Check Vector. 
		DW	ALL00	; Allocation Vector. 

					; DISK 6 (G) - Disk Parameter Header for Disk 0
		DW	TRANS0		; Put in label for vector here if translation table. 00=no translate - Same as L: drive
		DW	00		; Current Track selected.
		DB	00		; Current Sector selected.
		DB  00		; PID for BIOS for disk. (0 means handled by the default BIOS)
 		DW	00		; B
		DW	DIRBF	; 128 byte scratchpad buffer for file directory. ( used by BDOS )
		DW	DPBLOKIA	; Disk Parameter Block. 
		DW	CHK06		; Check Vector. 
		DW	ALL00	; Allocation Vector. 

					; DISK 7 (H) - Disk Parameter Header for Disk 0
		DW	TRANS0		; Put in label for vector here if translation table. 00=no translate - Same as L: drive
		DW	00		; Current Track selected.
		DB	00		; Current Sector selected.
		DB  00		; PID for BIOS for disk. (0 means handled by the default BIOS)
 		DW	00		; B
		DW	DIRBF	; 128 byte scratchpad buffer for file directory. ( used by BDOS )
		DW	DPBLOKIA	; Disk Parameter Block. 
		DW	CHK07		; Check Vector. 
		DW	ALL00	; Allocation Vector. 

					; DISK 8 (I) - Disk Parameter Header for Disk 0
		DW	TRANS0	; Put in label for vector here if translation table.
		DW	00		; Scratchpad.
		DW	00		; Scratchpad.
		DW	00		; Scratchpad.
		DW	DIRBF	; 
		DW	dpblokin	;
		DW	CHK08		;
		DW	ALLDRIVEN	;Allocation Vector for Drive N. 

					; DISK 9 (J) - Disk Parameter Header for Disk 0 - VIDEO BIOS.
		DW	TRANS0	; Put in label for vector here if translation table.
		DW	00		; Scratchpad.
		DW	00		; Scratchpad.
		DW	00		; Scratchpad.
		DW	DIRBF	; 
		DW	DPBLOKIVBIOS	;
		DW	CHK09		;
		DW	NOALLOC16K	;Allocation Vector for Drive N. 

					; DISK 10 (K) - Disk Parameter Header for Disk 0
		DW	TRANS0	; Put in label for vector here if translation table.
		DW	00		; Scratchpad.
		DW	00		; Scratchpad.
		DW	00		; Scratchpad.
		DW	DIRBF	; 
		DW	DPBLOKINBIOS	;
		DW	CHK0A		;
		DW	NOALLOC16K	;Allocation Vector for Drive N. 

					; DISK 11 (L) - Disk Parameter Header for Disk 0 - LOCAL disk for LOKI
		DW	TRANS0	; Put in label for vector here if translation table. 00= NO TRANSLATION.
		DW	00		; Scratchpad.
		DW	00		; Scratchpad.
		DW	00		; Scratchpad.
		DW	DIRBF	; 
		DW	DPBLKL	; Loki File Format.  New MAD location. 
		DW	CHK0B	; Should be 00 instead of CHK00. L: is fixed.  
		DW	ALLDRIVEL	;Allocation Vector for Drive L. 

					; DISK 12 (M) - Disk Parameter Header for Disk 0 - DISK M is the FIXED DISK for RAM, starting at 00000, No reserved tracks, and up to FFFFF. 256 allocations.
		DW	TRANS0	; No translation, disk is exact. 
		DW	00		; Scratchpad.
		DW	00		; Scratchpad.
		DW	00		; Scratchpad.
		DW	DIRBF	; 
		DW	DPBMAD	; Memory as Disk.
		DW	CHK0C	;
		DW	ALLMAD	;

					; DISK 13 (N) - Disk Parameter Header for Disk 0 - Reserved for Non Volatile Memory - Either Battery Backed or NVM
		DW	TRANS0	; Put in label for vector here if translation table.
		DW	00		; Scratchpad.
		DW	00		; Scratchpad.
		DW	00		; Scratchpad.
		DW	DIRBF	; 
		DW	dpblokin	;
		DW	CHK0D		;
		DW	ALLDRIVEN	;Allocation Vector for Drive N. 

					; DISK 14 (O) - Disk Parameter Header for Disk 0 - Reserved for ONLINE (network) directory map. 
		DW	TRANS0	; Put in label for vector here if translation table.
		DW	00		; Scratchpad.
		DW	00		; Scratchpad.
		DW	00		; Scratchpad.
		DW	DIRBF	; 
		DW	DPBLOKIUBIOS1	;
		DW	CHK0E		;
		DW	NOALLOC16K	;Allocation Vector for Drive N. 

					; DISK 15 (P) - Disk Parameter Header for Disk 0
		DW	TRANS0	; Put in label for vector here if translation table.
		DW	00		; Scratchpad.
		DW	00		; Scratchpad.
		DW	00		; Scratchpad.
		DW	DIRBF	; 
		DW	DPBLOKIUBIOS2	;
		DW	CHK0F		;
		DW	NOALLOC16K	;Allocation Vector for Drive N. 

NOALLOC16K:
		DW	$FFFF	; No allocation for 16 Kb ROMs.

;ORG ^
TRANS0:
		DB	0,1,2,3
		DB	4,5,6,7
		DB 8,9,10,11
		DB 12,13,14,15
		DB 16,17,18,19
		DB 20,21,22,23
		DB 24,25,26,27
		DB 28,29,30,31	; This gives us a clean sector number starting with 0.

;TRANS1:
;		DB	1,2,3,4
;		DB	5,6,7,8
;		DB	9,10,11,12
;		DB	13,14,15,16
;		DB	17,18,19,20
;		DB	21,22,23,24
;		DB	25,26,27,28
;		DB	29,30,31,32 ; This gives us a clean sector number starting with 0.

;
;TRANS2:				; Translation table can be shared with Disk Parameter Base.
;		DB	1,7,13,19
;		DB	25,5,11,17
;		DB	23,3,9,1
;		DB	21,2,8,14
;		DB	20,26,6,12
;		DB	18,24,4,10
;		DB	16,22
		
;DPBLK:				; Standard to all disks.  ( can be different )
;		DW	$20		; Sectors per track. ie, 0 to 31. 
;		DB	3		; BSF Block Shift Factor - 3 bits over Bit6 ( Bit6 = 128 = allocation ). Bit7 = 256., Bit 8=512 and Bit9 is 1024 ( 0 to 1023 ). Number of bits past Bit6.
;		DB	7		; BLM Block Mask - Related directly to the above.  =2^BSF-1...  7 is a 3 bit mask, so BSF must be 3. 
;		DB	0		; EXM - Extent Mask - 
;		DW	242		; DSM - Disk size -1 - ie, 242= 243 BLOCKS. if the block size=1024k then  248,832 bytes on disk. Block=Allocation Unit. 
;		DW	63		; DRM - Directory Max - Directory Entries = 64 entries. If 1024K allocation holds 32 directory entries then this means 2 allocations for directory. 
;		DB	%11000000 ;AL0 - Alloc 0  = 192, but WATCH THE BITS. Why the heck they did this I do not know. One bit for each allocation.
;		DB	%00000000 ;AL1 - Alloc 1 - If all 16 bits are set, then it means 16 directory allocations... MAX... Why did they not just use a number here?
;		DW	16		; CKS - Check size = (DRM+1) /4 for removable media. = 64/4 = 16... In this case, If fixed then DRM=0 = Do not check directory records - Not sure why.
;		DW 	02		; OFF - Track Offset. Skipped tracks = Added to SETTRK when called. Can be used for partitioning a big disk into smaller disks. 2 = 2 system tracks. 

DPBLOKIVBIOS:		; Bios DPB... 4 Bios ( or more? ) each 16K with ONE directory allocation.  
		DW	$20		; 00,01			; BIOS drive is 32 sectors per track  - DISK 11!  128 byte "sectors" = records - 16k.
		DB	3		; 02			; 3 and 7(next ) = 1K Allocations
		DB	7		; 03			; See above. 
		DB	0		; 04			; Extent mask. Blocks are 1K, so should be 0. 
		DW	15		; 05,06			; This has 16 1K blocks. ( Allocation = 1 K ). Would be a 64k disk. ie, 27C512 ROMDISK. 
		DW	31		; 07,08			; 1 x 1k directory allocation = 32 file extents max. (32 total )
		DB	%10000000 ;09			; Only set 2 bits so we know 2 allocation is directory
		DB	%00000000 ;0A
		DW	0		; 0B,0C			; Have set this as non-removable. Might be necessary to make as 0 for fixed. 
		DW 	$E0		; 0D,0E			; $E0000 tracks offset, should put the directory at E0000

DPBLOKINBIOS:		; Bios DPB... 4 Bios ( or more? ) each 16K with ONE directory allocation.  
		DW	$20		; 00,01			; BIOS drive is 32 sectors per track  - DISK 11!  128 byte "sectors" = records - 16k.
		DB	3		; 02			; 3 and 7(next ) = 1K Allocations
		DB	7		; 03			; See above. 
		DB	0		; 04			; Extent mask. Blocks are 1K, so should be 0. 
		DW	15		; 05,06			; This has 16 1K blocks. ( Allocation = 1 K ). Would be a 64k disk. ie, 27C512 ROMDISK. 
		DW	31		; 07,08			; 1 x 1k directory allocation = 32 file extents max. (32 total )
		DB	%10000000 ;09			; Only set 2 bits so we know 2 allocation is directory
		DB	%00000000 ;0A
		DW	0		; 0B,0C			; Have set this as non-removable. Might be necessary to make as 0 for fixed. 
		DW 	$E4		; 0D,0E			; $E4000 tracks offset, should put the directory at E4000
									
DPBLOKIUBIOS1:		; Bios DPB... 4 Bios ( or more? ) each 16K with ONE directory allocation.  
		DW	$20		; 00,01			; BIOS drive is 32 sectors per track  - DISK 11!  128 byte "sectors" = records - 16k.
		DB	3		; 02			; 3 and 7(next ) = 1K Allocations
		DB	7		; 03			; See above. 
		DB	0		; 04			; Extent mask. Blocks are 1K, so should be 0. 
		DW	15		; 05,06			; This has 16 1K blocks. ( Allocation = 1 K ). Would be a 64k disk. ie, 27C512 ROMDISK. 
		DW	31		; 07,08			; 1 x 1k directory allocation = 32 file extents max. (32 total )
		DB	%10000000 ;09			; Only set 2 bits so we know 2 allocation is directory
		DB	%00000000 ;0A
		DW	0		; 0B,0C			; Have set this as non-removable. Might be necessary to make as 0 for fixed. 
		DW 	$E8		; 0D,0E			; $E8000 tracks offset, should put the directory at E8000 
									
DPBLOKIUBIOS2:		; Bios DPB... 4 Bios ( or more? ) each 16K with ONE directory allocation.  
		DW	$20		; 00,01			; BIOS drive is 32 sectors per track  - DISK 11!  128 byte "sectors" = records - 16k.
		DB	3		; 02			; 3 and 7(next ) = 1K Allocations
		DB	7		; 03			; See above. 
		DB	0		; 04			; Extent mask. Blocks are 1K, so should be 0. 
		DW	15		; 05,06			; This has 16 1K blocks. ( Allocation = 1 K ). Would be a 64k disk. ie, 27C512 ROMDISK. 
		DW	31		; 07,08			; 1 x 1k directory allocation = 32 file extents max. (32 total )
		DB	%10000000 ;09			; Only set 2 bits so we know 2 allocation is directory
		DB	%00000000 ;0A
		DW	0		; 0B,0C			; Have set this as non-removable. Might be necessary to make as 0 for fixed. 
		DW 	$EC		; 0D,0E			; $EC000 tracks offset, should put the directory at EC000 
									
									
DPBLK2:				; 
		DW	$20		; 00,01
		DB	3		; 02
		DB	7		; 03
		DB	0		; 04
		DW	242		; 05,06
		DW	63		; 07,08
		DB	%11000000 ;09
		DB	%00000000 ;0A
		DW	16		; 0B,0C
		DW 	42		; 0D,0E
	

;DPBLOKI:							; Single byte per allocation disk. 
;		DW	$20		; 00,01			; L drive is 8 sectors per track  - DISK 11! Need Nope is 128 byte "sectors" = records
;		DB	3		; 02			; 3 and 7(next ) = 512K sectors, 1024K allocations.
;		DB	7		; 03			; See above. 
;		DB	0		; 04			; Extent mask. Blocks are 1K, so should be 0. 
;		DW	63		; 05,06			; This has 64 1K blocks. ( Allocation = 1 K ). 
;		DW	127		; 07,08			; 4 directory allocation = 128 file extents max. (128 total )
;		DB	%11110000 ;09			; Only set 4 bits so we know 4 allocation is directory
;		DB	%00000000 ;0A
;		DW	8		; 0B,0C			; Have set this as removable. Might be necessary to make as 0 for fixed. 
;		DW 	1		; 0D,0E			; 1 track offset. This gives 2K for boot of BDOS and to load the CCP and other files. Enough. 
		
DPBLOKIA:							; Two bytes per allocation disk - This is for a 720K disk. For example, 8 sectors/track and each sector is 512k 
		DW	$20		; 00,01			; SPT - Sectors Per Track - L drive is 32 sectors per track  - DISK 0.  128 byte "sectors" = records
		DB	4		; 02			; BSF - Block Shift Factor - 4 = 4 bits = 16 sectors per block. = 16 x 128 byte sectors = 2048K allocations.
		DB	15		; 03			; BLM - Block Mask - (2^BSF) -1See above.  
		DB	0		; 04			; EXM - Extent mask. Blocks are 2K, but should be 0 as double allocation group means still 16k extent. One extent per entry.
		DW	315		; 05,06			; DSM - Disk Size - This has 316 2K blocks. ( Allocation = 1 K ). Would be a 720k disk. 
		DW	255		; 07,08			; DRM - Directory Max Entries - 4 x 2k directory allocation = 8k / 32 bytes per entry = 256 entries. minus 1.  
		DB	%11110000 ;09			; AL0 - Only set 4 bits so we know 4 allocation is directory
		DB	%00000000 ;0A			; AL1 - These bits get copied DIRECTLY to the allocation vector on initialisation since they are already taken.
		DW	64		; 0B,0C			; CKS - 64 bytes check vector ( since we have 256 entries
		DW 	1		; 0D,0E			; 1 track offset. This gives 2K for boot of BDOS and to load the CCP and other files. Enough. 
		
DPBLOKIB:							; Two bytes per allocation disk - This is for a 720K disk. 8s/t **********************ALTERNATE A view. 
		DW	$20		; 00,01			; L drive is 32 sectors per track  - DISK 11!  128 byte "sectors" = records
		DB	4		; 02			; 3 and 7(next ) = 512K sectors, 2048K allocations.
		DB	15		; 03			; See above. 
		DB	0		; 04			; Extent mask. Blocks are 2K, but should be 0 as double allocation group means still 16k extent. 
		DW	315		; 05,06			; This has 316 2K blocks. ( Allocation = 1 K ). Would be a 720k disk. 
		DW	127		; 07,08			; 4 x 2k directory allocation = 128 file extents max. (128 total ) I think my format might be a little off though. 
		DB	%11110000 ;09			; Only set 4 bits so we know 4 allocation is directory
		DB	%00000000 ;0A
		DW	8		; 0B,0C			; Have set this as removable. Might be necessary to make as 0 for fixed. 
		DW 	0		; 0D,0E			; 1 track offset. This gives 2K for boot of BDOS and to load the CCP and other files. Enough. 

DPBLKL:								; L Drive DPB
		DW	$20		; 00,01			; L drive is 32 sectors per track  - DISK 11!  128 byte "sectors" = records
		DB	3		; 02			; 3 and 7(next ) = 1K Allocations
		DB	7		; 03			; See above. 
		DB	0		; 04			; Extent mask. Blocks are 1K, so should be 0. 
		DW	58		; 05,06			; This has 58 1K blocks. ( Allocation = 1 K ). Would be a 64k disk. ie, 27C512 ROMDISK. 
		DW	31		; 07,08			; 1 x 1k directory allocation = 64 file extents max. (64 total )
		DB	%10000000 ;09			; Only set 2 bits so we know 2 allocation is directory
		DB	%00000000 ;0A
		DW	0		; 0B,0C			; Have set this as non-removable. Might be necessary to make as 0 for fixed. 
		DW 	$F1		; 0D,0E			; 241 tracks offset, should put the directory at F1000 - This gives 4K for boot of BDOS/BIOS and Bootstrap from F0000 to F1000 
									; Note data at F0000 is the BOOT track, contains the FDOS. 
								
;dpblokin:								; N Drive DPB. 
;		DW	$20		; 00,01			; N drive is 32 sectors per track  - DISK 13!  128 byte "sectors" = records
;		DB	4		; 02			; 3 and 7(next ) = 128B sectors, 2048K allocations.
;		DB	15		; 03			; See above. 
;		DB	1		; 04			; Extent mask. Blocks are 2K, so should be 1. 
;		DW	127		; 05,06			; This has 128 2K blocks. ( Allocation = 1 K ). Would be a 256k disk. ie, 824000 NVM
;		DW	63		; 07,08			; 1 x 1k directory allocation = 64 file extents max. (64 total )
;		DB	%10000000 ;09			; Only set 1 bit for directory ( 1 allocation )
;		DB	%00000000 ;0A
;		DW	0		; 0B,0C			; Have set this as nonremovable. Might be necessary to make as 0 for fixed. 
;		DW 	$80		; 0D,0E			; 128 tracks offset, should put the directory at 80000 - NVM disk for 256K

DPBLOKIN:							; N: Drive is 32 sectors/track, NonVolatile RAM disk. 256K - For non-boot system applications.
		DW	$20		; 00,01			; SPT - Sectors Per Track - N drive is 32 sectors per track  - DISK 0.  128 byte "sectors" = records
		DB	3		; 02			; BSF - Block Shift Factor - 3 = 3 bits = 8 sectors per block. = 8 x 128 byte sectors = 1024K allocations.
		DB	7		; 03			; BLM - Block Mask - (2^BSF) -1See above.  
		DB	0		; 04			; EXM - Extent mask. Blocks are 2K, but should be 0 as double allocation group means still 16k extent. One extent per directory entry.
		DW	255		; 05,06			; DSM - Disk Size - This has 256 x 1K blocks. ( Allocation = 1 K ). Would be a 256K disk (unformatted) 252Kb formatted.
		DW	127		; 07,08			; DRM - Directory Max Entries - 4 x 1k directory allocation = 4k / 32 bytes per entry = 127 entries. minus 1.  
		DB	%11110000 ;09			; AL0 - Only set 4 bits so we know 4 allocation is directory
		DB	%00000000 ;0A			; AL1 - These bits get copied DIRECTLY to the allocation vector on initialisation since they are already taken.
		DW	0		; 0B,0C			; CKS - 16 bytes check vector ( since we have 128 entries ) - But it's also a fixed disk. so don't need a check vector. 
		DW 	$80		; 0D,0E			; 128 track offset since it's a part of Drive M:  It's located at $8000 to $BFFF.  No reserved tracks. 


DPBMAD:								; Two bytes per allocation disk
		DW	$20		; 00,01			; M drive is 32 sectors per track  - DISK 12!  is 128 byte "sectors" = records
		DB	5		; 02			; 5 bits and 0-31 sectors per 4096k allocation.
		DB	31		; 03			; See above. 
		DB	3		; 04			; Extent mask. Blocks are 4K, so should be 3. 
		DW	255		; 05,06			; This has 256 4K blocks. ( Allocation = 4 K ). Would be a 1024k disk. Lower 1Mb of RAM can be accessed as DISK. 
		DW	127		; 07,08			; 1 x 4k directory allocation = 128 file extents max. (128 total ) I think my format might be a little off though. 
		DB	%10000000 ;09			; Only set 1 bits so we know 1 allocation is directory
		DB	%00000000 ;0A
		DW	0		; 0B,0C			; Have set this as removable. Might be necessary to make as 0 for fixed. 
		DW 	0		; 0D,0E			; 0 track offset. First 4K is Directory Allocation ONLY. Allocation 1 is first memory space. 
		
;DPBMAD:								; Two bytes per allocation disk - Half Size DPB - 512K - No upper memory.
;		DW	$20		; 00,01			; M drive is 32 sectors per track  - DISK 12!  is 128 byte "sectors" = records
;		DB	5		; 02			; 5 bits and 0-31 sectors per 4096k allocation.
;		DB	31		; 03			; See above. 
;		DB	3		; 04			; Extent mask. Blocks are 4K, so should be 3. 
;		DW	127		; 05,06			; This has 128 4K blocks. ( Allocation = 4 K ). Would be a 512k disk. Lower 1Mb of RAM can be accessed as DISK. 
;		DW	127		; 07,08			; 1 x 4k directory allocation = 128 file extents max. (128 total ) I think my format might be a little off though. 
;		DB	%10000000 ;09			; Only set 1 bits so we know 1 allocation is directory
;		DB	%00000000 ;0A
;		DW	0		; 0B,0C			; Have set this as fixed. Might be necessary to make as 0 for fixed. 
;		DW 	0		; 0D,0E			; 0 track offset. First 4K is Directory Allocation ONLY. Allocation 1 is first memory space. 

		
		; End of fixed tables. 
; DP BLOCK TABLE. BLS calculation (BLock Size)
; BSF=3, BLM=7	= Block of 1024K ( Block is allocation )	- Also means 32 directory entries per directory allocation.
; BSF=4, BLM=15	= Block of 2048K - Also means 64 directory entries per directory allocation.
; BSF=5, BLM=31	= Block of 4096K - Also means 128 directory entries per directory allocation.
; BSF=6, BLM=63 = Block of 8192K - Also means 256 directory entries per directory allocation. 
; BSF=7, BLM=127 = Block of 16384K - Also means 512 directory entries per directory allocation. 
		
; EXM values
; BLS	DSM<256	DSM>=256
; 1024	0		N/A
; 2048	1		0
; 4096	3		1
; 8192	7		3
;16384 	15		7
;
	
BOOT:							; -03 Can be separate. 	
	LD	A,$02					; Reset the system (cold boot)
	OUT	(SYSTEM),A				; This will HARD reset the system. Will force RESET to LOW then HIGH. 
								; No further instructions will execute after this as RESET is asserted low. CPU reset follows. 
	
	
								; Need to zero out some memory here and reload BDOS and CCP. 
WBOOT:							; 00 - System reset. 
WBOOTE:							; Seems to have other names too. 
	ld	SP,$FFFF				; Just above the BIOS. Have around 160 memory locations for stack. Enough. 
	
	LD		HL,STOP				; Set up a return to exit. 
	PUSH	HL
	
;	jp $D000 ; CCP				; start up CCP
	JP		CCP_WARM_BOOT		; Start CCP via this vector, since the CCP might change. 

STOP:	HALT
		JP	STOP
		

;CONST
;Sample the status of the currently assigned console
;device and return 0FFH in register A if a character is
;ready to read, and 00H in register A if no console
;characters are ready.		
CONST:
	LD		A,(CHARACTER)		; Is there already a character waiting? 0=np
	AND		A
	JR		NZ,CONST-WAITING	; Branch to indicate so if it's waiting. 
	IN		A,(CON)				; Test the console otherwise - is there a key pressed?
	AND		A
	RET		Z					; Ret with zero in the A register if nothing.
CONST-WAITING:
	LD		(CHARACTER),A		; (re)save the character into the buffer ( just in case we just read it )
	LD		A,$FF				; Indicate there's a character here.
	RET

CHARACTER:	DB	$00				; 1 character buffer since we lose when we check it. 00 means no character.
CHARSTORE:	DB	$00				; And we have a character buffer we can store it in for a single cycle. No need to clear it. 


;CONIN
;Read the next console character into register A, and
;set the parity oit (high order bit) to zero. If no
;console character is ready, wait until a character is
;typed before returning.
CONIN:
	CALL	CONST				; Check for a character, and store in the buffer if there's one.
	JR		Z,CONIN				; Loop until there's a character in the buffer.
	LD		A,(CHARACTER)		; Retrieve the character in A.
	LD		(CHARSTORE),A		; Temp store the character in A. 
	XOR		A
	LD		(CHARACTER),A		; Clear the short buffer. 
	LD		A,(CHARSTORE)		; Get it once more to return in A with the end of the call. 
	
;	RET							; Take the character back.
; Comment out the RET above to force translation. ( characters below $10 will be translated according to the table )

	CP		$80					; check for bit 7.
	RET		C					; Exit now if it's below 80.
	
								; Do I need a small XLATE function here?  Which would allow for other characters.
								; It could be reprogrammed for different functions - eg, Wordstar.
								; Or should I put that in the keyboard itself.  
XLATE:							; Console Input Translate Function... 
	CP		$90					; Just translate the first 16 numbers for now. 
	RET		NC					; We won't get a carry if the number if 16 ($10) or above.

	AND		$7F					; Mask out Bit8.
	
	LD		HL,XLTABLE			; Translate table.
	LD		D,$00
	LD		E,A					; Character.
	ADD		HL,DE				; We only care about small numbers.
	LD		A,(HL)				; Substitute the value in the table.
	RET
	
XLTABLE:		; 16 translation points. 
		DB	0	; NULL
		DB  1	; ^A
		DB 	2	; ^B
		DB	3	; ^C
		DB	4	; ^D
		DB	5	; ^E
		DB	6	; ^F
;		DB	7	; ^G - BELL
		DB	7	; ^H - BS 	- Backspace - Cursor Left
;		DB	8	; ^H - BS 	- Backspace - Cursor Left
		DB	$13	; ^H - BS 	- Backspace - Cursor Left
;		DB	9	; ^I
		DB	9	; ^I
;		DB	$A	; ^J - LF 	- Linefeed - Cursor Down
		DB	$18	; ^J - LF 	- Linefeed - Cursor Down
;		DB	$B	; ^K - VT 	- Vertical Tab - Cursor Up
		DB	5	; ^K - VT 	- Vertical Tab - Cursor Up
;		DB	$C	; ^L - FF 	- Form Feed - Cursor Right
		DB	4	; ^L - FF 	- Form Feed - Cursor Right
		DB	$D	; ^M
		DB	$E	; ^N
		DB	$F	; ^O
;		DB	$10	; ^P
;		DB	$11	; ^Q
;		DB	$12 ; ^R
;		DB	$13 ; ^S
;		DB	$14	; ^T
;		DB	$15	; ^U
;		DB	$16	; ^V
;		DB	$17	; ^W
;		DB	$18	; ^X
;		DB	$19	; ^Y
;		DB	$1A	; ^Z
	
	
;CONOUT
;Send the character from register C to the console
;output device. The character is in ASCII, with high
;order parity bit set to zero. You may want to include
;a time-out on a line feed or carriage return, if your
;console device requires some time interval at the end
;of the line (such as a TI Silent 700 terminal). You
;can, if you wish, filter out control characters which
;cause your console device to react in a strange way (a
;control-z causes the Lear Seigler terminal to clear
;the screen, for example)	
CONOUT:
	LD		A,($0040)			; Check if the jump has changed.
	CP		$C9					; Look for RET at $0040
	JR		NZ,	VIDEODRV
	LD		A,C					; 09 - Console Outs. 
	OUT		(CON),A
	ret
VIDEODRV:						; A video driver is installed. 	
	PUSH BC
	LD		A,C
								; Examine whether we want dual output (console and screen)
;	OUT (CON),A
;OUT ($0B),A				; send it to a character store file so we can see what codes are being sent for debugging. 
	LD		B,A					; Since we need to carry A all the way to the routine.... And the stack might be hidden. 
	LD		C,$01				; Function 1 = Print Characters to screen. 
	CALL	$0040				; "Video Hook" in Page 0....  Unless I hook it from here. But this seems most elegant. 
	POP	 BC
	ret
	
	
	
;LIST	
;Send the character from register C to the currently
;assigned listing device. The character is in ASCII
;with zero parity	
LIST:							; 0C - List device - use for default printer. 
	LD		A,C
	OUT		(LPT),A
	ret	
	
	
	
;PUNCH	
;Send the character from register C to the currently
;assigned punch device. The character is in ASCII with
;zero parity	
PUNCH:							; 0F - Punch card output... Might be able to use this for network files.
	LD		A,C
	OUT		(AUX),A
	ret
	
	
	
;READER	
;Read the next character from the currently assigned
;reader device into register A with zero parity (high
;order bit must be zero), an end of file condition is
;reported by returning an ASCII control-z ($1A)
READER:							; 12 - Additional serial input... Might be able to use this for network files.
	IN		A,(AUX)
	ret
	
	
	
;HOME	
;Return the disk head of the currently selected disk
;(initially disk A) to the track 00 position. If your
;controller allows access to the track 0 flag from the
;drive, step the head until the track 0 flag is
;detected. If your controller does not support this
;feature, you can translate the HOME call into a call
;on SETTRK with a parameter of 0
HOME:							; 15 - Move disc head to track 0
	LD		BC,$00
	JP		SETTRK				; Send track 0 to disk port.



;SELDSK
;Select the disk drive given by register C for further
;operations, where register C contains 0 for drive A, 1
;for drive 8, and so-forth UP to 15 for drive P (the
;standard CP/M distribution -version supports four
;drives). On each disk select, SELDSK must return in
;HL the base address of a l6-byte area, called the Disk
;Parameter Header, described in the Section 10. For
;standard floppy disk drlves, the contents of the
;header and associated tables does not change, and thus
;the program segment included in the sample CBIOS
;performs this operation automatically. If there is an
;attempt to select a non-existent drive, SELDSK returns
;HL=0000H as an error indicator. Although SELDSK must
;return the header address on each call, it is
;advisable to postpone the actual physical disk select
;operation until an I/O function (seek, read or write)
;is actually performed, since disk selects often occur
;without utimately performing any disk I/O, and many
;controllers will unload the head of the current disk	
;before selecting the new drive. This would cause an
;excessive amount of noise and disk wear
SELDSK:							; 18 - Select disc drive	- C Register = disk 0...F. Enter with E=0 or E=FF. 
								; If bit 0 of E is 0, then the disc is logged in as if new; if the format has to be determined from the boot sector, for example, this will be done.
								;If bit 0 if E is 1, then the disc has been logged in before. The disc is not accessed; the DPH address (or zero) is returned immediately.
								;SELDSK returns the address of a Disc Parameter Header in HL. The exact format of a DPH varies between CP/M versions; note that under CP/M 3, 
								;	the DPH is in memory bank 0 and probably not visible to programs. If the disc could not be selected it returns HL=0.
	LD		A,C
	AND		$0FH				; limit to 16 drives. Recursive selection after that. 
	LD		(DISKNO),A			; Store the drive selected. 
	OUT		(DRIVE),A
	LD		HL,DPBASE			; Set the Disk Parameter Header in HL. 
	RLC		A					; Disk Parameter Header is 16 bytes, so we need to select the right one from a table. 
	RLC		A
	RLC		A
	RLC		A					; Multiply A by 16. Bit 7 should have cycled bacl to Bit 0 each time do don't need to mask it. 
	ADD		A,L
	LD		L,A					; 
	LD		A,$00
	ADC		A,H
	LD		H,A					; HL now contains the correct DPBASE table. 
;;	LD		E,0					; I need to better understand this parameter. OK, This is not a parameter I set, but that I act on.
								; I will use a BIOS to address the disk format in each case... No need for multiple disk formats.
								; The specific DISK BIOS for that drive can handle it. 
	PUSH	HL
	POP		IY					; Get HL (DPBASE in IX ).
	LD		A,(IY+10)			; Get the Track Offset. 
	LD		E,A
	LD		A,(IY+11)
	LD		D,A					; Store the location of the DPC from the DP Header.
	PUSH	DE
	POP		IX					; IX now containts the Disk Parameter Block for the selected Disk Header. 
;	LD		A,(IX+13)			; Location of OFF (Track Offset) for the selected disk. 
;	LD		(TRACKOFF_L),A		; Track offset now in BDOS not BIOS. 
;	LD		A,(IX+14)
;	LD		(TRACKOFF_H),A
	ret
	
								; As we return from this routine, HL contains the DPH. DE is the DPB. 
								; Also IY is the DPH, IX is the DPB. 
	
	
TRACKOFF_L:	db $00			; Store the Track Offset for the currently selected disk here.
TRACKOFF_H:	db $00			; It's a 16 bit number.  NOTE, This doesn't get done in the BIOS in CP/M so I disabled it. 
	
	
	
	
;SETTRK	
;Register BC contains the track number for subsequent
;disk accesses on the currently selected arlve. You
;can choose to seek the selected track at this time, or
;delay the seek until the next read or write actually
;occurs. Register BC can take on values in the range
;0-76 corresponding to valid track numbers for standard
;floppy disk drives, and 0-65535 for non-standard disk
;subsystems. OK, that is a big number. 
SETTRK:							;27: Set track number - Track in BC - Is a word - Starts at 0. 
	LD		(TRACK),BC
;	LD		IX,TRACKOFF_L
;	LD		A,(IX+0)
;	ADD		A,C
;	LD		C,A


;	LD		A,C					; Don't mess with the track offset here - do it in the OS. This is all literal and direct.
;	OUT		(TRACKPORT),A		; Set for 256 tracks currently, but if I need more, I should clear A and ADC A,B Then out the upper byte. 
								; Only change this on READ or WRITE. Store here. 

;	LD		A,(IX+1)
;	ADC		A,B
;	LD		B,A
;	OUT		(TRACKPORT),A		; But this is the code for 16 bit track selection. After adding the OFF Track Offset. 

	ret
; I should add OFF from the DPB Disk Parameter Block to the track... So when the disk is selected, I should save that number in memory. 


;SETSEC
;Register BC contains the sector number (1 through 26)
;for subsequent disk accesses on the currently selected
;drive. You can choose to send this information to the
;controller at this point, or instead delay sector
;selection until a read or write operation occurs.
SETSEC:							;30: Set sector number - Sector in BC - Is a word. Starts at 1. I think. Will find out. 
	LD		(SECTOR),BC
;	LD		A,C					; Don't do this here - ONLY on READ or WRITE. 
;	OUT		(SECTORPORT),A
	ret



;SETDMA
;Register BC contains the DMA (disk memory access)
;address for subsequent read or write operations. For
;example, if B = 00H and C = 80H when SETDMA is called,
;then all subsequent read operations read their data
;into 80H through 0FFH, and all subsequent write
;operations get their data from 808 through 0FFH, until
;the next call to SE'rD!4A occurs. 'rhe initial DMA
;address is assumed to be 80H. Note that the
;controller need not actually support airect memory
;access. If, for example: all data is received and
;sent through I/O ports, the CBIOS which you construct
;will use the 120 byte area starting at the selected
;DMA address for the memory buffer during the following
;read or write operations
SETDMA:							;33: Set DMA address - Set DMA address. 
	LD		(DMAAD),BC
;	LD		A,C					; Don't do this here - only on READ or WRITE. 
;	OUT		(DMALOW),A
;	LD		A,B
;	OUT		(DMAHIGH),A
;	LD		(DMALOC),BC			; Store the DMA adddress
	ret
	
DMALOC:	DW	$0000				; Store the DMA address.
	
;READ	
;Assuming the drive has been selected, the track has
;been set, the sector has been set, and the DMA address
;has been specified, the READ subroutine attempts to
;read one sector based upon these parameters, and
;returns the following error codes in register A:
;
;0 no errors occurred
;1 non-recoverable error condition occurred
;
;Currently, CP/M responds only to a zero or non-zero
;value as the return code. That is, if the value in
;register A is 0 then CP/M assumes that the disk
;operation completed properly. If an error occurs,
;however, the CBIOS should attempt at least 10 retries
;to see if the error is recoverable. When an error is
;reported the BDOS will print the message "BDOS ERR ON
;x: BAD SECTOR". The operator then has the option of
;typing <cr> to ignore the error, or ctl-C to abort
READ:							;36: Read a sector - To DMA address. A=0 success. A=1 unrecoverable error. A=FF Media changed.
;								; Lower 8 disks are external, upper 8 disks are MAD (memory as disk)
	CALL	STD					; Sector Track DMA settings for disk operation. 

	LD		A,(DISKNO)			; which disk is selected? Internal or external?
	CP		$08					; Internal or External disk?
	JR		C,READEX			; If it's 0-7 then it's external. 

	LD		B,$7F				; Start at end of record block.
	LD		C,MADPORT			; And use MAD to transfer block.
	LD		HL,(DMAAD)			; Pick up the destination for DMA
	LD		DE,$007F			; Increment to end of DMA record buffer.
	ADD		HL,DE				;  by adding the 7F to the DMA location.
	INDR						; Transfer via BLOCK I/O transfer backwards ( so B is aligned with L )
	IND							; And transfer the final byte. 
	
	LD		A,0					; Success. 
	ret

READEX:							; External read.
	LD		A,$00
	OUT		(DRIVEACTIVE),A		; 00 says to read from DMA   - Old Disk Simulator via Hardware and DMA. 

;		out (10),A ; debug the registers. 

	LD		A,0					; Success. 
	ret

								
								; SET SECTOR/TRACK/DMA.... Hence STD. 
STD:							;GET SECTOR TRACK DMA for READ and WRITE. 
;	LD		BC,(DMAAD)			; Where the DMA address is stored... (Might do this later with disk selection first).
	LD		A,(DMAAD)				; Set the DMA address
	OUT		(DMALOW),A
	LD		A,(DMAAD+1)
	OUT		(DMAHIGH),A

;	LD		BC,(TRACK)
;	LD		A,C					; Don't mess with the track offset here - do it in the OS. This is all literal and direct.
	LD		A,(TRACK) ; Don't need more since 8 bit. 
	OUT		(TRACKPORT),A		; Set for 256 tracks currently, but if I need more, I should clear A and ADC A,B Then out the upper byte.  
;	LD		A,B						; 16 bit track selection?
;	OUT		(TRACKPORT_HIGH),A		; But this is the code for 16 bit track selection. After adding the OFF Track Offset. 	

;	LD		BC,(SECTOR)			; Now get the sector.
;	LD		A,C
	LD		A,(SECTOR)				; And set the sector...
	OUT		(SECTORPORT),A	
	ret

;WRITE
;Write the data from the currently selected DMA address
;to the currently selected drive, track, and sector.
;the data should be marked as "non deleted data" to
;maintain compatibility with other CP/M systems. The
;error codes given in the READ command are returned in
;register A, with error recovery attempts as described
;above. 
WRITE:							;39: Write a sector - C contains blocking code.  0=deferred. 1=immediate. 2=Can be deferred. No preread necessary. A=0 success. 
;								; Lower 8 disks are external, upper 8 disks are MAD (memory as disk)
	CALL	STD					; Sector Track DMA settings for current write. Since multiple DISK and I/O operations are now mixed. 

	LD		A,(DISKNO)			; which disk is selected? Internal or external?

	CP		$08					; Internal or External disk?
	JR		C,WRITEX			; If it's 0-7 then it's external. 								;	A=1 unrecoverable error. A=FF Media changed.

	LD		B,$7F				; Start at end of record block.
	LD		C,MADPORT			; And use MAD to transfer block.
;	LD		HL,(DMALOC)			; Recover the destination
	LD		HL,(DMAAD)			; Recover the destination
	LD		DE,$007F			; Increment to end of DMA record buffer.
	ADD		HL,DE				;  by adding the 7F to the DMA location.
	OTDR						; Transfer via BLOCK I/O transfer backwards ( so B is aligned with L )
	OUTD							; And transfer the final byte. 

;	OUT	(19),A					; DEBUG - show write. 
	
	LD		A,0					; Success. 
	ret

WRITEX:								;In CP/M 2 and later, the following extra jumps appear:
	LD		A,$01
	OUT		(DRIVEACTIVE),A		; 01 says to write from DMA. 
	LD		A,0
	ret
	
	
	
;LISTST	
;Return the ready status of the list device. Used by
;the DESPOOL program to improve console response dUJ:"Jng
;its operation. The value 00 is returned in A it the
;list device is not ready to accept a character, and
;0FFH if a character can be sent to the printer. Note
;that a 00 value always suffices
LISTST:							;42: Status of list device - A=0 Printer not ready. A=FF -printer ready. 
	LD		A,$00
	ret



;Sectran, Sectrn.
;Performs sector logical to physical sector translation
;in order to improve the overall response of CP/M.
;Standard CP/M systems are shipped wi th a "skew factor"
;of 6, where six physical sectors are skipped between
;each logical read operation. This skew factor allows
;enough time between sectors for most programs to load
;their buffers without missing the next sector. In
;particular computer systems which use fast processors,
;memory, and disk subsystems, the skew factor may be
;changed to improve overall response. Note, however,
;that you should maintain a single density IBM
;compatible version of CP/M for information transfer
;into and out of your computer system, using a skew
;factor of 6. In general, SECTRAN receives a logical
;sector number in BC, and a translate table address in
;DE. The sector number is used as an index into the
;translate table, with the resulting physical sector
;number in HL. For standard systems, the tables and
;indexing code is provided in the BIOS and need not be
;changed	- Table address of 0000 means NO TRANSLATION.
;and BC=0 is first logical sector. 
SECTRAN:						;45: Sector Translation.
	EX		DE,HL				; HL NOW HOLDS THE LOCATION OF THE TABLE
	LD		A,H
	OR		L					; Test DE for 0 = NO TRANSLATION.
	JR		Z,NOTRANSLATE
	PUSH	BC
	POP		DE					; DE now holds the offset in the table ( byte table ). Do I need to LD B,$00? 
	ADD		HL,DE				; Add sector offset in BC
	LD		L,(HL)				; GET translated sector.
	LD		H,B					; Transfer upper byte which is zero.
	RET

NOTRANSLATE:
	PUSH BC						; If no translate required, reflect the original value back. 
	POP	HL
	RET
	
	
;	.DM 	"-Start of Tables-"
	

; Set up further scratchpad at end of routines.



BEGDAT:						; Beginning of data and tables area 	


TRACK:		DW		0
SECTOR:		DW		0
DMAAD:		DW		0
DISKNO:		DB		0	
	
;DIRBF:		BLOCK	128		; 128 bytes for scratch directory. I'm really trying to work out if this needs to be in BIOS. Yes, it seems it does need to be in BIOS.
;							; This is a DMA area for directory buffers when we need to search filenames with data in the existing DMA location. 
EQU		DIRBF,$FB80			; Stick this into space between the BDOS and BIOS. 



CHK00:		BLOCK	64		; Check Vector 0	
							; Bitmapped byte for each drive.
							;	Bit 0 AV table current if set. 
							;	Bit 1 Internal/External drive
							;   Bit 2 Active drive
							;   Bit 3
							;	Bit 4
							; 	Bit 5
							; 	Bit 6
							;	Bit 7
CHK01:		BLOCK	1		; Check Vector for all other drives. 
CHK02:
CHK03:
CHK04:
CHK05:
CHK06:
CHK07:
CHK08:
CHK09:
CHK0A:
CHK0B:
CHK0C:
CHK0D:
CHK0E:
CHK0F:


			DB	'<A:>'
;ALL00:		BLOCK	40		; Allocation Vector for drive 0 (A:) 40 bytes. (316 allocations)
			DB	'<'
			DB  'L:>'
;ALLDRIVEL:	BLOCK	8		; only 64K of allocations at 1K each. 
			DB	'<'
			DB  'M:>'
;ALLMAD:		BLOCK	32		; MEMORY ALLOCATIONS - Memory As Disk. Use DISK to track memory use. 
			DB  '<'

EQU		ALLMAD,$FB00			; As per FULL MEMORY MAP. 4K sectors. 256 blocks. 
EQU		ALLDRIVEL,$FB20			; As per BOOT ROM. 1K sectors, 64 blocks. 8K.
EQU		ALLDRIVE0,$FB28			; Special case Online Drive for exchange. 
EQU		ALLDRIVEN,$FB30			; 2K sectors. 128 Blocks. 
EQU		ALL00,$FB40				; As per external drive. Current 320(decimal) blocks. up to 512 reserved. 

	ENDDAT:						; End of data area. 

; BIOS Jump table below. 17 active in this code. 
; Note - A Sector is 128 bytes. (Record)
;CP/M 1
; Let's go for 32 sectors/track ( 8 normal sectors/track ) and 


	.DM		"BIOS>"

