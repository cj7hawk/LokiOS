; Bootstrap.asm - 4K Boot Loader at F0000 on the Loki.
; Later we can use the BDOS to allocate memory - but it's a chicken and egg situation. Let's set up a normal 64K machine at the base of memory. From 01000 to 10FFF.

.ORG $0000					; Boots on hard reset.

EQU		MMU,$0F				; The port to read/write MMU bytes to the MMU RAM.
EQU		PID,$0E				; The PID port - Which process is the MMU reflecting?
EQU		STARTBIT,$0D		; The Start bit and other system bits. 
EQU		Console,$01			; The serial console port. 

EQU		TRACKPORT	,$06	; Where we write track data.
EQU		SECTORPORT	,$07	; Where we write sector data. 
EQU		MADPORT,$0C			; Where we write for MAD reads ( Memory As Disk )


EQU		BDOS,		$0005	; BDOS call location
EQU		MY-BDOS,$F000
;EQU		BIOS,$FC00		; Defined elsewhere as FC03

COLDBOOT:
.equ	TRACKPORT	,$06
.equ	SECTORPORT	,$07
.equ	MADPORT		,$0C

STEP1:	LD		A,'*'			; Step 1. Send a star to the console port to let us know it's alive. 
		OUT		(Console),A		; Just pseudo-startup debugging. Can be removed later if necessary. 
								; Though it's useful to let something(someone?) know the CPU started and is running 

STEP2:							; Next step - set up default processes for each of the non-prime PIDs so we can at least engage them from MMU PAGE 0. 
		LD		D,$1F			; 32 possible processes. Set them up now. First and last page. 
		LD		E,$20
SETUPALLPROCESSES:				; Now set up default processes for each process. This will reflect Process 0 block 0 and F.
		LD		A,D				; Get process ID.
		OUT		(PID),A			; 	Set process ID into Process Latch - Change which MMU PID we are on.
		LD		B,$00			; Page 0. (First 4Kb)
		LD		C,MMU			; Set up the MMU Address
		LD		A,$01			; Block 1 = 01000 to 01FFF
		OUT		(C),A			;	Write it.
		LD		B,$F0			; Page F. (Last 4Kb)
		LD		A,$10			; Block 10 = 10000 to 10FFF
		OUT		(C),A			;	Write it.
		DEC		D						; Count backwards through all processes. Last one will be 1. 
		JR		NZ,SETUPALLPROCESSES	; Loop until we have completed down to process 1. 
		
STEP3:							; Next step. Format the directory for MAD before we begin.
								; It doesn't really matter which process we are in presently. We're in Process 1 by default at this point. 
								; Move Block0 (Directory) into Page 1 (located at $1000) so we can format the MAD directory.
		LD		B,$10			; Set B for Page 1 (upper nibble only is page, since we're affecting A12,A13,A14,A15) 
		LD		C,MMU			; Set C for the MMU Port
		XOR		A				; Set A to point to Block 0 (value is the block, so a=0)
		OUT		(C),A			; And write to the MMU RAM. 
		
		LD		HL,$1000
		LD		BC,$1000
FORMAT:							; Format the MAD directory structure. 
		LD		(HL),$E5		; Format code.
		INC		HL
		DEC		BC
		LD		A,B
		OR		C
		JR		NZ,FORMAT
		
		LD		HL,DEFAULT1		; Now write the default directory elements so Drive M can be used without corrupting system memory.
		LD		DE,$1000
		LD		BC,128
		LDIR							
		


		
STEP4:							; Next step - set up Process 0 so we can use it. 		
								; Process 0 is a normal CP/M type memory set up. If we don't use processes then
								; CP/M will run in process 0, and the rest of memory is available as RAMDISK under
								; the MAD (MEMORY AS DISK) system.
								; And there is no need to process switch or use pseudo-segments. 
		XOR		A
		OUT		(PID),A			; Set the Process ID to Zero. 
								; Now set up Process 0. 
		LD		B,A				; Get the port for the MMU. Start at the first page.
		LD		C,MMU			; And load C with the MMU port. 
		LD		E,$01			; Allocation 1 - This can change later depending on how many allocations are in the memory directory.
		LD		D,$10			; We're going to do 16 cycles.
MMUINIT:		
		OUT		(C),E			; populate the MMU byte.
		LD		A,$10			; Increment the values in B upper nibble.
		ADD		A,B				; 
		LD		B,A
		INC		E				; Next allocation.
		DEC		D
		JR		NZ,MMUINIT		; And loop 16 times.
								; Now the basic memory structure for 64K should be set up - We still need to do other stuff, like write the directory and format memory.
								; But RAM should be available.

		

STEP5:							; We are still in Process 0 (inactive) presently, so switch the current bootstrap to Page 0 so we can turn the MMU on (active) 
		LD		B,$00			; Set the MMU to map the current boot block over the Page 0 memory so we can switch to it without losing our place in the code.
		LD		C,MMU			; MMU address without crashing everything. 
		LD		A,$F0			; Block F0 - Equates to F0000 - Where the boot rom is. 
		OUT		(C),A			; Set Page0 as the BOOTSTRAP ROM FIrst 4K direct. 
								; Process memory (MMU) is done - We need to switch out of startup to process mode. 
		XOR		A				; Now memory has been set up, and we can switch the MMU on, we need to clear the start bit flag. 
		OUT		(PID),A			; Select Process 0 - since it's the one we want to use. 
		OUT		(STARTBIT),A	; Clear startbit (Turn on MMU to active)
		
		LD		A,'#'			; Next symbol indicates we are running now in PID mode - and the PIDs have been set up and seem to be working. 
		OUT		(Console),A		; Until we have the other routines installed, just keep it to simple symbols. 
								; Just pseudo-startup debugging. Can be removed later if necessary. 


STEP6:							; Final Step - Copy the BIOS and BDOS (FDOS) to memory and RESET the computer. 
		LD		HL,$0100				; Start of stored BDOS in Page 0 ( equates to F0100 in memory )
		LD		DE,$F000				; 64K based memory location of BDOS destination - We want to move the BDOS to F000 in 64K memory. 
		LD		BC,2816					; Size of the BDOS in bytes ( noting that this is from F000 to FAFF )
		LDIR							; Copy the BDOS. 
		
		LD		HL,$0C00				; And copy the BIOS in the same way, from F0C00 in real memory to FC00 in 64K space. 
		LD		DE,$FC00
		LD		BC,$0400
		LDIR

STEP7:							; Set any last minute hooks and reset via the now-loaded FDOS ( BDOS+BIOS=FDOS ) which will load the CCP.


		LD	C,$00				; Set for BIOS reset. 
		JP $F000						; BDOS reset will load the CCP from the BDOS - Loads CCP.BIN from L: Since L: is now operational by default.  
										; BIOS reset also loads Bank 1 into Page 0 before resetting,  so switches
										; out the ROM Bootstrap.
										
										; We can now exit Bootstrap. 



; Default root image to populate the MAD directory with the zero process.
;DEFAULT1:
;DB	$0D,'root-cpmimg',$00,$00,$00,$00		;Drive M, filename ROOT-CPM.IMG
;DB	0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0
;DB  0,0,0,0

;.org $100
;DMA:	BLOCK	128			; Create the DMA block. 

DEFAULT1:	DB	$00,'ROOT_CPMIMG',$03,$00,$00,$80,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F,$10 ; Rem default image - mark this out.
			DB	$00,'VIDEOMEMIMG',$03,$00,$00,$80,$C0,$C1,$C2,$C3,$C4,$C5,$C6,$C7,$C8,$C9,$CA,$CB,$CC,$CD,$CE,$CF
			DB	$00,'VIDEOMEMIMG',$07,$00,$00,$80,$D0,$D1,$D2,$D3,$D4,$D5,$D6,$D7,$D8,$D9,$DA,$DB,$DC,$DD,$DE,$DF
			DB	$00,'BOOT_ROMDSK',$03,$00,$00,$80,$F0,$F1,$F2,$F3,$F4,$F5,$F6,$F7,$F8,$F9,$FA,$FB,$FC,$FD,$FE,$FF




; ################################################### Other Stuff ##################################################



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