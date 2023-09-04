                               ; Reformatted and converted for cross-assembly by Macro Assembler AS
                               ; Eric Smith <spacewar@gmail.com> 2018-01-24
                               ; from original source os3bdos.asm from
                               ;   http://www.cpm.z80.de/download/cpm2-plm.zip
                               ; includes Digital Research CP/M V2.2 Patch 01 (cpm22pat.01) from
                               ;   http://www.cpm.z80.de/download/cpm22pat.zip
                               ; Changes:
                               ;   multiple instructions per line split to separate lines
                               ;   dollar sign in labels replaced by underscore
                               ;   dollar sign (as digit separator) in binary constants removed
                               ;   no colons for labels for equates
                               ;   single quotes around strings replaced with double quotes
                               ;   true and false replaced with _true and _false
                               ;   eliminated equates for 8080 registers, added comments introduced with %
                               ;   replaced "not", "and" operators with "~", "&"
                               ;   removed empty comments
                               ;   added ifdef origin to allow origin to be specified from command line
                               ;   added commments about serial number
;.CPU          z80EQU        PATCH1,1TITLE
; #### Conversion Error for previous line. ####
;"BDOS INTERFACE, BDOS, VERSION 2.2 FEB, 1980"
; #### Conversion Error for previous line. ####
                               ;*****************************************************************
                               ;*****************************************************************
                               ;**                                                             **
                               ;**   B a s i c    D i s k   O p e r a t i n g   S y s t e m    **
                               ;**            I n t e r f a c e   M o d u l e                   **
                               ;**                                                             **
                               ;*****************************************************************
                               ;*****************************************************************
                               ;
                               ;	Copyright (c) 1978, 1979, 1980
                               ;	Digital Research
                               ;	Box 579, Pacific Grove
                               ;	California
                               ;
                               ;
                               ;      20 january 1980
                               ;
                               ;
ORG	$ED00	; BDOS at E200

							; Six zero's for serial.  Remember to change jumps later. 
                               ; serial number (not documented in original DRI source file)
DB                          0
                               ; OEM number, low byte
DB                          22
                               ; CP/M version, 16h = 2.2
DB                          0
                               ; OEM number, high byte
DB                          0,0,0
                               ; serial number, big-endian
                               ;	enter here from the user's program with function number in c,
                               ;	and information address in d,e


       JP      bdose           ;past parameter block
                               ;	************************************************
                               ;	*** relative locations 0009 - 000e           ***
                               ;	************************************************
							   
							   
							   
                               ;	bios value defined at end of module
EQU           SSIZE,24         ;24 level stack
                               ;	low memory locations
EQU           REBOOT,$0000     ;reboot system
EQU           IOLOC,$0003      ;i/o byte location
EQU           BDOSA,$0006      ;address field of jmp BDOS
                               ;	bios access constants



                               ;sector translate
                               ;	equates for non graphic characters
EQU           CTLC,$03         ;control c
EQU           CTLE,$05         ;physical eol
EQU           ascii_backspace,$08         ;backspace
EQU           CTLP,$10         ;prnt toggle
EQU           CTLR,$12         ;repeat line
EQU           CTLS,$13         ;stop/start screen
EQU           CTLU,$15         ;line delete
EQU           CTLX,$18         ;=ctl-u
EQU           CTLZ,$1A         ;end of file
EQU           RUBOUT,$7F       ;char delete
EQU           TAB,$09          ;tab char
EQU           CR,$0D           ;carriage return
EQU           LF,$0A           ;line feed
EQU           CTL,$5E          ;up arrow


pererr:
DW                          persub
                               ;permanent error subroutine

selerr:
DW                          selsub
                               ;select error subroutine

roderr:
DW                          rodsub
                               ;ro disk error subroutine

roferr:
DW                          rofsub
                               ;ro file error subroutine

bdose:
                               ;arrive here from user programs
       EX      DE,HL           ;info=DE, DE=info
       LD      (info),HL
       EX      DE,HL
       LD      A,E             ;linfo = low(info) - don't equ
       LD      (linfo),A
       LD      HL,0            ;return value defaults to 0000
       LD      (aret),HL       ;save user's stack pointer, set to local stack
       ADD     HL,SP           ;entsp = stackptr
       LD      (entsp),HL
       LD      SP,lstack       ;local stack setup
       XOR     A               ;fcbdsk,resel=false
       LD      (fcbdsk),A
       LD      (resel),A
       LD      HL,goback       ;return here after all functions
       PUSH    HL              ;jmp goback equivalent to ret
       LD      A,C             ;skip if invalid #
       CP      nfuncs
       RET     NC
       LD      C,E             ;possible output character to C
       LD      HL,functab      ;DE=func, HL=.ciotab
       LD      E,A
       LD      D,0
       ADD     HL,DE           ;DE=functab(func)
       ADD     HL,DE
       LD      E,(HL)
       INC     HL
       LD      D,(HL)
       LD      HL,(info)       ;info in DE for later xchg	
       EX      DE,HL           ;dispatched
       JP      (HL)            ;	dispatch table for functions

functab:
	dw      wbootf				; THIS is different from the factory image depending on where it goes. I should send it to the BIOS somehow to the warm reset routine. 
	dw		func1
	dw		func2
	dw		func3
	dw		punchf
	dw		listf
	dw		func6
	dw		func7
	dw		func8
	dw		func9
	dw		func10
	dw		func11
equ	diskf,12
	dw		func12
	dw		func13
	dw		func14
	dw		func15
	dw		func16
	dw		func17
	dw		func18
	dw		func19
	dw		func20
	dw		func21
	dw		func22
	dw		func23
	dw		func24
	dw		func25
	dw		func26
	dw		func27
	dw		func28
	dw		func29
	dw		func30
	dw		func31
	dw		func32
	dw		func33
	dw		func34
	dw		func35
	dw		func36
	dw		func37
	dw		func38
	dw		func39
	dw		func40
equ	nfuncs,41
                               ;	error subroutines

persub:
                               ;report permanent error
       LD      HL,permsg       ;to report the error
       CALL    errflg
       CP      ctlc            ;reboot if response is ctlc
       JP      Z,reboot
       RET                     ;and ignore the error

selsub:
                               ;report select error
       LD      HL,selmsg       ;wait console before boot
       JP      wait_err        ;

rodsub:
                               ;report write to read/only disk
       LD      HL,rodmsg       ;wait console
       JP      wait_err        ;

rofsub:
                               ;report read/only file
       LD      HL,rofmsg       ;drop through to wait for console
                               ;

wait_err:
                               ;wait for response before boot
       CALL    errflg
       JP      reboot          ;	error messages

dskmsg:
DB                          'Bdos Err On '

dskerr:
DB                          ' : $'         ;filled in by errflg

permsg:
DB                          'Bad Sector$'

selmsg:
DB                          'Select$'

rofmsg:
DB                          'File '

rodmsg:
DB                          'R/O$'

errflg:
                               ;report error to console, message address in HL
       PUSH    HL              ;stack mssg address, new line
       CALL    bcrlf
       LD      A,(curdsk)      ;current disk name
       ADD     A,'A'
       LD      (dskerr),A
       LD      BC,dskmsg       ;the error message
       CALL    bprint
       POP     BC              ;error mssage tail
       CALL    bprint           ;jmp	conin		;to get the input character			
                               ;(drop through to conin)
                               ;ret
                               ;	console handlers

conin:
                               ;read console character to A
       LD      HL,kbchar
       LD      A,(HL)
       LD      (HL),0
       OR      A
       RET     NZ              ;no previous keyboard character ready
       JP      coninf          ;get character externally
                               ;ret

conech:
                               ;read character with echo
       CALL    conin           ;echo character?
       CALL    echoc
       RET     C               ;character must be echoed before return
       PUSH    AF
       LD      C,A
       CALL    tabout
       POP     AF
       RET                     ;with character in A

echoc:
                               ;echo character if graphic
                               ;cr, lf, tab, or backspace
       CP      cr              ;carriage return?
       RET     Z
       CP      lf              ;line feed?
       RET     Z
       CP      tab             ;tab?
       RET     Z
       CP      ascii_backspace            ;backspace?
       RET     Z
       CP      ' '             ;carry set if not graphic
       RET
conbrk:
                               ;check for character ready
       LD      A,(kbchar)      ;skip if active kbchar
       OR      A
       JP      NZ,conb1        ;no active kbchar, check external break
       CALL    constf          ;return if no char ready
       AND     1
       RET     Z               ;character ready, read it
       CALL    coninf          ;to A
       CP      ctls            ;check stop screen function
       JP      NZ,conb0        ;found ctls, read next character
       CALL    coninf          ;to A
       CP      ctlc            ;ctlc implies re-boot
       JP      Z,reboot        ;not a reboot, act as if nothing has happened
       XOR     A               ;with zero in accumulator
       RET
conb0:
                               ;character in accum, save it
       LD      (kbchar),A
conb1:
                               ;return with true set in accumulator
       LD      A,1
       RET
conout:
                               ;compute character position/write console char from C
                               ;compcol = true if computing column position
       LD      A,(compcol)
       OR      A
       JP      NZ,compout      ;write the character, then compute the column
                               ;write console character from C
       PUSH    BC              ;check for screen stop function
       CALL    conbrk
       POP     BC              ;recall/save character
       PUSH    BC
       CALL    conoutf         ;externally, to console
       POP     BC              ;recall/save character
       PUSH    BC              ;may be copying to the list device
       LD      A,(listcp)      ;to printer, if so
       OR      A
       CALL    NZ,listf
       POP     BC              ;recall the character

compout:
       LD      A,C             ;recall the character
                               ;and compute column position
       LD      HL,column       ;A = char, HL = .column
       CP      rubout          ;no column change if nulls
       RET     Z
       INC     (HL)            ;column = column + 1
       CP      ' '             ;return if graphic
       RET     NC              ;not graphic, reset column position
       DEC     (HL)            ;column = column - 1
       LD      A,(HL)          ;return if at zero
       OR      A
       RET     Z               ;not at zero, may be backspace or end line
       LD      A,C             ;character back to A
       CP      ascii_backspace
       JP      NZ,notbacksp    ;backspace character
       DEC     (HL)            ;column = column - 1
       RET
notbacksp:
                               ;not a backspace character, eol?
       CP      lf              ;return if not
       RET     NZ              ;end of line, column = 0
       LD      (HL),0          ;column = 0
       RET
ctlout:
                               ;send C character with possible preceding up-arrow
       LD      A,C             ;cy if not graphic (or special case)
       CALL    echoc
       JP      NC,tabout       ;skip if graphic, tab, cr, lf, or ctlh
                               ;send preceding up arrow
       PUSH    AF              ;up arrow
       LD      C,ctl
       CALL    conout
       POP     AF              ;becomes graphic letter
       OR      $40
       LD      C,A             ;ready to print
                               ;(drop through to tabout)

tabout:
                               ;expand tabs to console
       LD      A,C             ;direct to conout if not
       CP      tab
       JP      NZ,conout       ;tab encountered, move to next tab position

tab0:
       LD      C,' '           ;another blank
       CALL    conout
       LD      A,(column)      ;column mod 8 = 0 ?
       AND     %111
       JP      NZ,tab0         ;back for another if not
       RET
backup:
                               ;back-up one screen position
       CALL    pctlh
       LD      C,' '
       CALL    conoutf         ;	(drop through to pctlh)

pctlh:
                               ;send ctlh to console without affecting column count
       LD      C,ascii_backspace
       JP      conoutf         ;ret

crlfp:
                               ;print #, cr, lf for ctlx, ctlu, ctlr functions
                               ;then move to strtcol (starting column)
       LD      C,'#'
       CALL    conout
       CALL    bcrlf            ;column = 0, move to position strtcol

crlfp0:
       LD      A,(column)
       LD      HL,strtcol
       CP      (HL)            ;stop when column reaches strtcol
       RET     NC
       LD      C,' '           ;print blank
       CALL    conout
       JP      crlfp0
bcrlf:
                               ;carriage return line feed sequence
       LD      C,cr
       CALL    conout
       LD      C,lf
       JP      conout          ;ret

bprint:
                               ;print message until M(BC) = '$'
       LD      A,(BC)          ;stop on $
       CP      '$'
       RET     Z               ;more to print
       INC     BC              ;char to C
       PUSH    BC
       LD      C,A
       CALL    tabout          ;another character printed
       POP     BC
       JP      bprint
read:
                               ;read to info address (max length, current length, buffer)
       LD      A,(column)      ;save start for ctl-x, ctl-h
       LD      (strtcol),A
       LD      HL,(info)
       LD      C,(HL)
       INC     HL
       PUSH    HL
       LD      B,0             ;B = current buffer length,
                               ;C = maximum buffer length,
                               ;HL= next to fill - 1

readnx:
                               ;read next character, BC, HL active
       PUSH    BC              ;blen, cmax, HL saved
       PUSH    HL
readn0:
       CALL    conin           ;next char in A
       AND     $7f             ;mask parity bit
       POP     HL              ;reactivate counters
       POP     BC
       CP      cr              ;end of line?
       JP      Z,readen
       CP      lf              ;also end of line
       JP      Z,readen
       CP      ascii_backspace            ;backspace?
       JP      NZ,noth         ;do we have any characters to back over?
       LD      A,B
       OR      A
       JP      Z,readnx        ;characters remain in buffer, backup one
       DEC     B               ;remove one character
       LD      A,(column)      ;col > 0
       LD      (compcol),A     ;compcol > 0 marks repeat as length compute
       JP      linelen         ;uses same code as repeat

noth:
                               ;not a backspace
       CP      rubout          ;rubout char?
       JP      NZ,notrub       ;rubout encountered, rubout if possible
       LD      A,B             ;skip if len=0
       OR      A
       JP      Z,readnx        ;buffer has characters, resend last char
       LD      A,(HL)          ;A = last char
       DEC     B
       DEC     HL              ;blen=blen-1, next to fill - 1 decremented
       JP      rdech1          ;act like this is an echo

notrub:
                               ;not a rubout character, check end line
       CP      ctle            ;physical end line?
       JP      NZ,note         ;yes, save active counters and force eol
       PUSH    BC
       PUSH    HL
       CALL    bcrlf
       XOR     A               ;start position = 00
       LD      (strtcol),A
       JP      readn0          ;for another character

note:
                               ;not end of line, list toggle?
       CP      ctlp            ;skip if not ctlp
       JP      NZ,notp         ;list toggle - change parity
       PUSH    HL              ;save next to fill - 1
       LD      HL,listcp       ;HL=.listcp flag
       LD      A,1             ;True-listcp
       SUB     (HL)
       LD      (HL),A          ;listcp = not listcp
       POP     HL              ;for another char
       JP      readnx
notp:
                               ;not a ctlp, line delete?
       CP      ctlx
       JP      NZ,notx
       POP     HL              ;discard start position
                               ;loop while column > strtcol

backx:
       LD      A,(strtcol)
       LD      HL,column
       CP      (HL)            ;start again
       JP      NC,read
       DEC     (HL)            ;column = column - 1
       CALL    backup          ;one position
       JP      backx
notx:
                               ;not a control x, control u?
                               ;not control-X, control-U?
       CP      ctlu            ;skip if not
       JP      NZ,notu         ;delete line (ctlu)
       CALL    crlfp           ;physical eol
       POP     HL              ;discard starting position
       JP      read            ;to start all over

notu:
                               ;not line delete, repeat line?
       CP      ctlr
       JP      NZ,notr
linelen:
                               ;repeat line, or compute line len (ctlh)
                               ;if compcol > 0
       PUSH    BC              ;save line length
       CALL    crlfp
       POP     BC
       POP     HL
       PUSH    HL
       PUSH    BC              ;bcur, cmax active, beginning buff at HL

rep0:
       LD      A,B             ;count len to 00
       OR      A
       JP      Z,rep1
       INC     HL              ;next to print
       LD      C,(HL)
       DEC     B               ;count length down
       PUSH    BC
       PUSH    HL
       CALL    ctlout          ;character echoed
       POP     HL              ;recall remaining count
       POP     BC
       JP      rep0            ;for the next character

rep1:
                               ;end of repeat, recall lengths
                               ;original BC still remains pushed
       PUSH    HL              ;save next to fill
       LD      A,(compcol)     ;>0 if computing length
       OR      A
       JP      Z,readn0        ;for another char if so
                               ;column position computed for ctlh
       LD      HL,column       ;diff > 0
       SUB     (HL)
       LD      (compcol),A     ;count down below
                               ;move back compcol-column spaces

backsp:
                               ;move back one more space
       CALL    backup          ;one space
       LD      HL,compcol
       DEC     (HL)
       JP      NZ,backsp
       JP      readn0          ;for next character

notr:
                               ;not a ctlr, place into buffer

rdecho:
       INC     HL              ;character filled to mem
       LD      (HL),A
       INC     B               ;blen = blen + 1

rdech1:
                               ;look for a random control character
       PUSH    BC              ;active values saved
       PUSH    HL
       LD      C,A             ;ready to print
       CALL    ctlout          ;may be up-arrow C
       POP     HL              ;recall char
       POP     BC
       LD      A,(HL)
       CP      ctlc            ;set flags for reboot test
       LD      A,B             ;move length to A
       JP      NZ,notc         ;skip if not a control c
       CP      1               ;control C, must be length 1
       JP      Z,reboot        ;reboot if blen = 1
                               ;length not one, so skip reboot

notc:
                               ;not reboot, are we at end of buffer?
       CP      C               ;go for another if not
       JP      C,readnx
readen:
                               ;end of read operation, store blen
       POP     HL              ;M(current len) = B
       LD      (HL),B
       LD      C,cr            ;return carriage
       JP      conout          ;ret

func1:
                               ;return console character with echo
       CALL    conech			; $CONEC - BAD TRANSLATION ERROR????
       JP      sta_ret         ;
EQU           FUNC2,TABOUT     ;write console character with tab expansion
                               ;

func3:
                               ;return reader character
       CALL    readerf
       JP      sta_ret         ;func4:	equated to punchf
                               ;write punch character
                               ;func5:	equated to listf
                               ;write list character
                               ;write to list device

func6:
                               ;direct console i/o - read if 0ffh
       LD      A,C             ;0ffh => 00h, means input mode
       INC     A
       JP      Z,dirinp
       INC     A               ;0feH in C for status
       JP      Z,constf        ;direct output function
       JP      conoutf
dirinp:
       CALL    constf          ;status check
       OR      A               ;skip, return 00 if not ready
       JP      Z,retmon        ;character is ready, get it
       CALL    coninf          ;to A
       JP      sta_ret
func7:
                               ;return io byte
       LD      A,(ioloc)
       JP      sta_ret
func8:
                               ;set i/o byte
       LD      HL,ioloc
       LD      (HL),C
       RET                     ;jmp goback

func9:
                               ;write line until $ encountered
       EX      DE,HL           ;was lhld info	
       LD      C,L             ;BC=string address
       LD      B,H
       JP      bprint           ;out to console	
EQU           FUNC10,READ      ;read a buffered console line

func11:
                               ;check console status
       CALL    conbrk          ;(drop through to sta_ret)

sta_ret:
                               ;store the A register to aret
       LD      (aret),A
func_ret:
                               ;
       RET                     ;jmp goback (pop stack for non cp/m functions)

setlret1:
                               ;set lret = 1
       LD      A,1
       JP      sta_ret         ;	data areas

compcol:
DB                          0
                               ;true if computing column position

strtcol:
DB                          0
                               ;starting column position after read

column:
DB                          0
                               ;column position

listcp:
DB                          0
                               ;listing toggle

kbchar:
DB                          0
                               ;initial key char = 00

entsp:
BLOCK         2                ;entry stack pointer
BLOCK         SSIZE          ;stack size
BLOCK         SSIZE          ;stack size ( double up )

lstack:
                               ;	end of Basic I/O System
                               ;*****************************************************************
                               ;*****************************************************************
                               ;	common values shared between bdosi and bdos

usrcode:
DB                          0
                               ;current user number

curdsk:
DB                          0
                               ;current disk number

info:
BLOCK         2                ;information address

aret:
BLOCK         2                ;address value to return
EQU           LRET,ARET        ;low(aret)
                               ;*****************************************************************
                               ;*****************************************************************
                               ;**                                                             **
                               ;**   B a s i c    D i s k   O p e r a t i n g   S y s t e m    **
                               ;**                                                             **
                               ;*****************************************************************
                               ;*****************************************************************
EQU           DVERS,$22        ;version 2.2
                               ;	module addresses
                               ;	literal constants
EQU           _TRUE,$0FF       ;constant true
EQU           _FALSE,$000      ;constant false
EQU           ENDDIR,$0FFFF    ;end of directory
EQU				BYTE,1
                               ;number of bytes for "byte" type
EQU				WORD,2
                               ;number of bytes for "word" type
                               ;	fixed addresses in low memory
EQU           TFCB,$005C       ;default fcb location
EQU           TBUFF,$0080      ;default buffer location
                               ;	fixed addresses referenced in bios module are
                               ;	pererr (0009), selerr (000c), roderr (000f)
                               ;	error message handlers
                               ;per_error:	 
                               ;report permanent error to user	
                               ;	lxi h,pererr  jmp goerr		
                               ;rod_error:
                               ;report read/only disk error
                               ;	lxi h,roderr  jmp goerr
                               ;rof_error:
                               ;report read/only file error
                               ;	lxi h,roferr	;jmp goerr	

sel_error:
                               ;report select error
       LD      HL,selerr
goerr:
                               ;HL = .errorhandler, call subroutine
       LD      E,(HL)          ;address of routine in DE
       INC     HL
       LD      D,(HL)
       EX      DE,HL           ;to subroutine
       JP      (HL)            ;	local subroutines for bios interface

move:
                               ;move data length of length C from source DE to
                               ;destination given by HL
       INC     C               ;in case it is zero

move0:
       DEC     C               ;more to move
       RET     Z
       LD      A,(DE)          ;one byte moved
       LD      (HL),A
       INC     DE              ;to next byte
       INC     HL
       JP      move0
selectdisk:
                               ;select the disk drive given by curdsk, and fill
                               ;the base addresses curtrka - alloca, then fill
                               ;the values of the disk parameter block
       LD      A,(curdsk)      ;current disk# to c
       LD      C,A             ;lsb of e = 0 if not yet logged - in
       CALL    seldskf         ;HL filled by call
                               ;HL = 0000 if error, otherwise disk headers
       LD      A,H             ;return with 0000 in HL and z flag
       OR      L
       RET     Z               ;disk header block address in hl
       LD      E,(HL)          ;DE=.tran
       INC     HL
       LD      D,(HL)
       INC     HL
       LD      (cdrmaxa),HL    ;.cdrmax
       INC     HL
       INC     HL
       LD      (curtrka),HL    ;HL=.currec
       INC     HL
       INC     HL
       LD      (curreca),HL    ;HL=.buffa
       INC     HL
       INC     HL              ;DE still contains .tran
       EX      DE,HL           ;.tran vector
       LD      (tranv),HL
       LD      HL,buffa        ;DE= source for move, HL=dest
       LD      C,addlist       ;addlist filled
       CALL    move            ;now fill the disk parameter block
       LD      HL,(dpbaddr)    ;DE is source
       EX      DE,HL
       LD      HL,sectpt       ;HL is destination
       LD      C,dpblist       ;data filled
       CALL    move            ;now set single/double map mode
       LD      HL,(maxall)     ;largest allocation number
       LD      A,H             ;00 indicates < 255
       LD      HL,single       ;assume a=00
       LD      (HL),_true
       OR      A
       JP      Z,retselect     ;high order of maxall not zero, use double dm
       LD      (HL),_false
retselect:
       LD      A,_true         ;select disk function ok
       OR      A
       RET
home:
                               ;move to home position, then offset to start of dir
       CALL    homef           ;move to track 00, sector 00 reference
                               ;lxi h,offset ;mov c,m ;inx h ;mov b,m ;call settrkf
                               ;first directory position selected
       XOR     A               ;constant zero to accumulator
       LD      HL,(curtrka)    ;curtrk=0000
       LD      (HL),A
       INC     HL
       LD      (HL),A
       LD      HL,(curreca)    ;currec=0000
       LD      (HL),A
       INC     HL
       LD      (HL),A          ;curtrk, currec both set to 0000
       RET
rdbuff:
                               ;read buffer and check condition
       CALL    readf           ;current drive, track, sector, dma
       JP      diocomp         ;check for i/o errors

wrbuff:
                               ;write buffer and check condition
                               ;write type (wrtype) is in register C
                               ;wrtype = 0 => normal write operation
                               ;wrtype = 1 => directory write operation
                               ;wrtype = 2 => start of new block
       CALL    writef          ;current drive, track, sector, dma

diocomp:
                               ;check for disk errors
       OR      A
       RET     Z
       LD      HL,pererr
       JP      goerr
seek_dir:
                               ;seek the record containing the current dir entry
       LD      HL,(dcnt)       ;directory counter to HL
       LD      C,dskshf        ;value to HL
       CALL    hlrotr
       LD      (arecord),HL    ;ready for seek
       LD      (drec),HL       ;  jmp seek
                               ;ret

seek:
                               ;seek the track given by arecord (actual record)
                               ;local equates for registers
                               ;arech	equ	b		;arecord = BC
                               ;arecl	equ	c
                               ;crech	equ	d		;currec  = DE
                               ;crecl	equ	e
                               ;ctrkh	equ	h		;curtrk  = HL
                               ;ctrkl	equ	l
                               ;tcrech	equ	h		;tcurrec = HL
                               ;tcrecl	equ	l
                               ;load the registers from memory
       LD      HL,arecord
       LD      C,(HL)          ; % c = arecl
       INC     HL
       LD      B,(HL)          ; % b = arech
       LD      HL,(curreca)
       LD      E,(HL)          ; % e = crecl
       INC     HL
       LD      D,(HL)          ; % d = crech
       LD      HL,(curtrka)
       LD      A,(HL)
       INC     HL
       LD      H,(HL)          ; % h = ctrkh
       LD      L,A             ; % l = ctrkl
                               ;loop while arecord < currec

seek0:
       LD      A,C             ; % c = arecl
       SUB     E               ; % e = crecl
       LD      A,B             ; % b = arech
       SBC     A,D             ; % d = crech
       JP      NC,seek1        ;skip if arecord >= currec
                               ;currec = currec - sectpt
       PUSH    HL              ; % h = ctrkh
       LD      HL,(sectpt)
       LD      A,E             ; % e = crecl
       SUB     L
       LD      E,A             ; % e = crecl
       LD      A,D             ; % d = crech
       SBC     A,H
       LD      D,A             ; % d = crech
       POP     HL              ; % h = ctrkh
                               ;curtrk = curtrk - 1
       DEC     HL              ; % h = ctrkh
       JP      seek0           ;for another try

seek1:
                               ;look while arecord >= (t:=currec + sectpt) ---- THIS IS WHERE IT LOCKS UP...
       PUSH    HL              ; % h = ctrkh
       LD      HL,(sectpt)     ;HL = currec+sectpt
       ADD     HL,DE           ; % d = crech
       JP      C,seek2         ;can be > FFFFH	
       LD      A,C             ; % c = arecl
       SUB     L               ; % l = tcrecl
       LD      A,B             ; % b = arech
       SBC     A,H             ; % h = tcrech
       JP      C,seek2         ;skip if t > arecord
                               ;currec = t
       EX      DE,HL           ;curtrk = curtrk + 1
       POP     HL              ; % h = ctrkh
       INC     HL              ; % h = ctrkh
       JP      seek1           ;for another try

seek2:
       POP     HL              ; % h = ctrkh
                               ;arrive here with updated values in each register
       PUSH    BC              ;to stack for later ; % b = arech
       PUSH    DE              ; % d = crech
       PUSH    HL              ; % h = ctrkh
                               ;stack contains (lowest) BC=arecord, DE=currec, HL=curtrk
       EX      DE,HL           ;HL = curtrk+offset
       LD      HL,(offset)
       ADD     HL,DE
       LD      B,H             ;track set up
       LD      C,L
       CALL    settrkf         ;note that BC - curtrk is difference to move in bios
       POP     DE              ;recall curtrk
       LD      HL,(curtrka)    ;curtrk updated
       LD      (HL),E
       INC     HL
       LD      (HL),D          ;now compute sector as arecord-currec
       POP     DE              ;recall currec ; % d = crech
       LD      HL,(curreca)
       LD      (HL),E          ; % e = crecl
       INC     HL
       LD      (HL),D          ; % d = crech
       POP     BC              ;BC=arecord, DE=currec ; % b = arech
       LD      A,C             ; % c = arecl
       SUB     E               ; % e = crecl
       LD      C,A             ; % c = arecl
       LD      A,B             ; % b = arech
       SBC     A,D             ; % d = crech
       LD      B,A             ; % b = arech
       LD      HL,(tranv)      ;BC=sector#, DE=.tran
       EX      DE,HL
       CALL    sectranf         ;HL = tran(sector)
       LD      C,L             ;BC = tran(sector)
       LD      B,H
       JP      setsecf         ;sector selected
                               ;ret
                               ;	file control block (fcb) constants
EQU           EMPTY,$0E5       ;empty directory entry
EQU           LSTREC,127       ;last record# in extent
EQU           RECSIZ,128       ;record size
EQU           FCBLEN,32        ;file control block size
EQU           DIRREC,4			;;;;;;RECSIZ/FCBLEN
                               ;directory elts / record
EQU           DSKSHF,2         ;log2(dirrec)
EQU           DSKMSK,DIRREC-1
EQU           FCBSHF,5
                               ;log2(fcblen)
EQU           EXTNUM,12        ;extent number field
EQU           MAXEXT,31        ;largest extent number
EQU           UBYTES,13        ;unfilled bytes field
EQU           MODNUM,14        ;data module number
EQU           MAXMOD,15        ;largest module number
EQU           FWFMSK,$80       ;file write flag is high order modnum
EQU		   NOTFWFMSK,$7F	   ; Complement of FWFMSK
EQU           NAMLEN,15        ;name length
EQU           RECCNT,15        ;record count field
EQU           DSKMAP,16        ;disk map field
EQU           LSTFCB,FCBLEN-1
EQU           NXTREC,FCBLEN
EQU           RANREC,NXTREC+1
                               ;random record field (2 bytes)
                               ;	reserved file indicators
EQU           ROFILE,9         ;high order of first type char
EQU           INVIS,10         ;invisible file in dir command
                               ;	equ	11	;reserved
                               ;	utility functions for file access

dm_position:
                               ;compute disk map position for vrecord to HL
       LD      HL,blkshf       ;shift count to C
       LD      C,(HL)
       LD      A,(vrecord)     ;current virtual record to A

dmpos0:
       OR      A
       RRA
       DEC     C
       JP      NZ,dmpos0       ;A = shr(vrecord,blkshf) = vrecord/2**(sect/block)
       LD      B,A             ;save it for later addition
       LD      A,8             ;8-blkshf to accumulator
       SUB     (HL)
       LD      C,A             ;extent shift count in register c
       LD      A,(extval)      ;extent value ani extmsk

dmpos1:
                               ;blkshf = 3,4,5,6,7, C=5,4,3,2,1
                               ;shift is 4,3,2,1,0
       DEC     C
       JP      Z,dmpos2
       OR      A
       RLA
       JP      dmpos1
dmpos2:
                               ;arrive here with A = shl(ext and extmsk,7-blkshf)
       ADD     A,B             ;add the previous shr(vrecord,blkshf) value
                               ;A is one of the following values, depending upon alloc
                               ;bks blkshf
                               ;1k   3     v/8 + extval * 16
                               ;2k   4     v/16+ extval * 8
                               ;4k   5     v/32+ extval * 4
                               ;8k   6     v/64+ extval * 2
                               ;16k  7     v/128+extval * 1
       RET                     ;with dm_position in A

getdm:
                               ;return disk map value from position given by BC
       LD      HL,(info)       ;base address of file control block
       LD      DE,dskmap       ;HL =.diskmap
       ADD     HL,DE
       ADD     HL,BC           ;index by a single byte value
       LD      A,(single)      ;single byte/map entry?
       OR      A               ;get disk map single byte
       JP      Z,getdmd
       LD      L,(HL)          ;with HL=00bb
       LD      H,0
       RET
getdmd:
       ADD     HL,BC           ;HL=.fcb(dm+i*2)
                               ;double precision value returned
       LD      E,(HL)
       INC     HL
       LD      D,(HL)
       EX      DE,HL
       RET
index:
                               ;compute disk block number from current fcb
       CALL    dm_position     ;0...15 in register A
       LD      C,A             ;value to HL
       LD      B,0
       CALL    getdm
       LD      (arecord),HL
       RET
allocated:
                               ;called following index to see if block allocated
       LD      HL,(arecord)
       LD      A,L
       OR      H
       RET
atran:
                               ;compute actual record address, assuming index called
       LD      A,(blkshf)      ;shift count to reg A
       LD      HL,(arecord)
atran0:
       ADD     HL,HL           ;shl(arecord,blkshf)
       DEC     A
       JP      NZ,atran0
       LD      (arecord1),HL   ;save shifted block #  
       LD      A,(blkmsk)      ;mask value to C
       LD      C,A
       LD      A,(vrecord)     ;masked value in A
       AND     C
       OR      L               ;to HL
       LD      L,A
       LD      (arecord),HL    ;arecord=HL or (vrecord and blkmsk)
       RET
getexta:
                               ;get current extent field address to A
       LD      HL,(info)       ;HL=.fcb(extnum)
       LD      DE,extnum
       ADD     HL,DE
       RET
getfcba:
                               ;compute reccnt and nxtrec addresses for get/setfcb
       LD      HL,(info)       ;DE=.fcb(reccnt)
       LD      DE,reccnt
       ADD     HL,DE
       EX      DE,HL			
								; was (nxtrec-reccnt)
       LD      HL,nxtrec-reccnt
                               ;HL=.fcb(nxtrec)
       ADD     HL,DE
       RET
getfcb:
                               ;set variables from currently addressed fcb
       CALL    getfcba         ;addresses in DE, HL
       LD      A,(HL)          ;vrecord=fcb(nxtrec)
       LD      (vrecord),A
       EX      DE,HL           ;rcount=fcb(reccnt)
       LD      A,(HL)
       LD      (rcount),A
       CALL    getexta         ;HL=.fcb(extnum)
       LD      A,(extmsk)      ;extent mask to a
       AND     (HL)            ;fcb(extnum) and extmsk
       LD      (extval),A
       RET
setfcb:
                               ;place values back into current fcb
       CALL    getfcba         ;addresses to DE, HL
       LD      A,(seqio)
       CP      02              ;check ranfill
       JP      NZ,setfcb1
       XOR     A
setfcb1:
       LD      C,A             ;=1 if sequential i/o
       LD      A,(vrecord)     ;fcb(nxtrec)=vrecord+seqio
       ADD     A,C
       LD      (HL),A
       EX      DE,HL           ;fcb(reccnt)=rcount
       LD      A,(rcount)
       LD      (HL),A
       RET
hlrotr:
                               ;hl rotate right by amount C
       INC     C               ;in case zero

hlrotr0:
       DEC     C               ;return when zero
       RET     Z
       LD      A,H             ;high byte
       OR      A
       RRA
       LD      H,A
       LD      A,L             ;low byte
       RRA
       LD      L,A
       JP      hlrotr0
compute_cs:
                               ;compute checksum for current directory buffer
       LD      C,recsiz        ;size of directory buffer
       LD      HL,(buffa)      ;current directory buffer
       XOR     A               ;clear checksum value

computecs0:
       ADD     A,(HL)          ;cs=cs+buff(recsiz-C)
       INC     HL
       DEC     C
       JP      NZ,computecs0
       RET                     ;with checksum in A

hlrotl:
                               ;rotate the mask in HL by amount in C
       INC     C               ;may be zero

hlrotl0:
       DEC     C               ;return if zero
       RET     Z
       ADD     HL,HL
       JP      hlrotl0
set_cdisk:
                               ;set a "1" value in curdsk position of BC
       PUSH    BC              ;save input parameter
       LD      A,(curdsk)      ;ready parameter for shift
       LD      C,A
       LD      HL,1            ;number to shift
       CALL    hlrotl          ;HL = mask to integrate
       POP     BC              ;original mask
       LD      A,C
       OR      L
       LD      L,A
       LD      A,B             ;HL = mask or rol(1,curdsk)
       OR      H
       LD      H,A
       RET
nowrite:
                               ;return true if dir checksum difference occurred
       LD      HL,(rodsk)
       LD      A,(curdsk)
       LD      C,A
       CALL    hlrotr
       LD      A,L             ;non zero if nowrite
       AND     %1
       RET
set_ro:
                               ;set current disk to read only
       LD      HL,rodsk
       LD      C,(HL)
       INC     HL
       LD      B,(HL)
       CALL    set_cdisk       ;sets bit to 1
       LD      (rodsk),HL      ;high water mark in directory goes to max
       LD      HL,(dirmax)     ;DE = directory max
       INC     HL
       EX      DE,HL
       LD      HL,(cdrmaxa)    ;HL = .cdrmax
       LD      (HL),E          ;cdrmax = dirmax
       INC     HL
       LD      (HL),D
       RET
check_rodir:
                               ;check current directory element for read/only status
       CALL    getdptra        ;address of element

check_rofile:
                               ;check current buff(dptr) or fcb(0) for r/o status
       LD      DE,rofile       ;offset to ro bit
       ADD     HL,DE
       LD      A,(HL)          ;return if not set
       RLA
       RET     NC
       LD      HL,roferr
       JP      goerr           ;	jmp rof_error ;exit to read only disk message

check_write:
                               ;check for write protected disk
       CALL    nowrite         ;ok to write if not rodsk
       RET     Z
       LD      HL,roderr
       JP      goerr           ;	jmp rod_error ;read only disk error

getdptra:
                               ;compute the address of a directory element at
                               ;positon dptr in the buffer
       LD      HL,(buffa)
       LD      A,(dptr)
addh:
                               ;HL = HL + A
       ADD     A,L
       LD      L,A
       RET     NC              ;overflow to H
       INC     H
       RET
getmodnum:
                               ;compute the address of the module number 
                               ;bring module number to accumulator
                               ;(high order bit is fwf (file write flag)
       LD      HL,(info)       ;HL=.fcb(modnum)
       LD      DE,modnum
       ADD     HL,DE
       LD      A,(HL)          ;A=fcb(modnum)
       RET
clrmodnum:
                               ;clear the module number field for user open/make
       CALL    getmodnum       ;fcb(modnum)=0
       LD      (HL),0
       RET
setfwf:
       CALL    getmodnum       ;HL=.fcb(modnum), A=fcb(modnum)
                               ;set fwf (file write flag) to "1"
       OR      fwfmsk          ;fcb(modnum)=fcb(modnum) or 80h
       LD      (HL),A          ;also returns non zero in accumulator
       RET
compcdr:
                               ;return cy if cdrmax > dcnt
       LD      HL,(dcnt)       ;DE = directory counter
       EX      DE,HL
       LD      HL,(cdrmaxa)    ;HL=.cdrmax
       LD      A,E             ;low(dcnt) - low(cdrmax)
       SUB     (HL)
       INC     HL              ;HL = .cdrmax+1
       LD      A,D             ;hig(dcnt) - hig(cdrmax)
       SBC     A,(HL)          ;condition dcnt - cdrmax  produces cy if cdrmax>dcnt
       RET
setcdr:
                               ;if not (cdrmax > dcnt) then cdrmax = dcnt+1
       CALL    compcdr
       RET     C               ;return if cdrmax > dcnt
                               ;otherwise, HL = .cdrmax+1, DE = dcnt
       INC     DE
       LD      (HL),D
       DEC     HL
       LD      (HL),E
       RET
subdh:
                               ;compute HL = DE - HL
       LD      A,E
       SUB     L
       LD      L,A
       LD      A,D
       SBC     A,H
       LD      H,A
       RET
newchecksum:
       LD      C,_true         ;drop through to compute new checksum

checksum:
                               ;compute current checksum record and update the
                               ;directory element if C=true, or check for = if not
                               ;drec < chksiz?
       LD      HL,(drec)       ;DE-HL
       EX      DE,HL
       LD      HL,(chksiz)
       CALL    subdh
       RET     NC              ;skip checksum if past checksum vector size
                               ;drec < chksiz, so continue
       PUSH    BC              ;save init flag
       CALL    compute_cs      ;check sum value to A
       LD      HL,(checka)     ;address of check sum vector
       EX      DE,HL
       LD      HL,(drec)       ;value of drec
       ADD     HL,DE           ;HL = .check(drec)
       POP     BC              ;recall true=0ffh or false=00 to C
       INC     C               ;0ffh produces zero flag
       JP      Z,initial_cs    ;not initializing, compare
       CP      (HL)            ;compute_cs=check(drec)?
       RET     Z               ;no message if ok
                               ;checksum error, are we beyond
                               ;the end of the disk?
       CALL    compcdr
       RET     NC              ;no message if so
       CALL    set_ro          ;read/only disk set
       RET
initial_cs:
                               ;initializing the checksum
       LD      (HL),A
       RET
wrdir:
                               ;write the current directory entry, set checksum
       CALL    newchecksum     ;initialize entry
       CALL    setdir          ;directory dma
       LD      C,1             ;indicates a write directory operation
       CALL    wrbuff          ;write the buffer
       JP      setdata         ;to data dma address
                               ;ret

rd_dir:
                               ;read a directory entry into the directory buffer
       CALL    setdir          ;directory dma
       CALL    rdbuff          ;directory record loaded
                               ; jmp setdata to data dma address    
                               ;ret

setdata:
                               ;set data dma address
       LD      HL,dmaad        ;to complete the call
       JP      setdma
setdir:
                               ;set directory dma address
       LD      HL,buffa        ;jmp setdma to complete call     

setdma:
                               ;HL=.dma address to set (i.e., buffa or dmaad)
       LD      C,(HL)          ;parameter ready
       INC     HL
       LD      B,(HL)
       JP      setdmaf
dir_to_user:
                               ;copy the directory entry to the user buffer
                               ;after call to search or searchn by user code
       LD      HL,(buffa)      ;source is directory buffer
       EX      DE,HL
       LD      HL,(dmaad)      ;destination is user dma address
       LD      C,recsiz        ;copy entire record
       JP      move            ;ret

end_of_dir:
                               ;return zero flag if at end of directory, non zero
                               ;if not at end (end of dir if dcnt = 0ffffh)
       LD      HL,dcnt         ;may be 0ffh
       LD      A,(HL)
       INC     HL              ;low(dcnt) = high(dcnt)?
       CP      (HL)
       RET     NZ              ;non zero returned if different
                               ;high and low the same, = 0ffh?
       INC     A               ;0ffh becomes 00 if so
       RET
set_end_dir:
                               ;set dcnt to the end of the directory
       LD      HL,enddir
       LD      (dcnt),HL
       RET
read_dir:
                               ;read next directory entry, with C=true if initializing
       LD      HL,(dirmax)     ;in preparation for subtract
       EX      DE,HL
       LD      HL,(dcnt)       ;dcnt=dcnt+1
       INC     HL
       LD      (dcnt),HL       ;continue while dirmax >= dcnt (dirmax-dcnt no cy)
       CALL    subdh           ;DE-HL
       JP      NC,read_dir0    ;yes, set dcnt to end of directory
       JP      set_end_dir     ;		ret

read_dir0:
                               ;not at end of directory, seek next element
                               ;initialization flag is in C
       LD      A,(dcnt)        ;low(dcnt) and dskmsk
       AND     dskmsk
       LD      B,fcbshf        ;to multiply by fcb size

read_dir1:
       ADD     A,A
       DEC     B
       JP      NZ,read_dir1    ;A = (low(dcnt) and dskmsk) shl fcbshf
       LD      (dptr),A        ;ready for next dir operation
       OR      A               ;return if not a new record
       RET     NZ
       PUSH    BC              ;save initialization flag C
       CALL    seek_dir        ;seek proper record
       CALL    rd_dir          ;read the directory record
       POP     BC              ;recall initialization flag
       JP      checksum        ;checksum the directory elt
                               ;ret

getallocbit:
                               ;given allocation vector position BC, return with byte
                               ;containing BC shifted so that the least significant
                               ;bit is in the low order accumulator position.  HL is
                               ;the address of the byte for possible replacement in
                               ;memory upon return, and D contains the number of shifts
                               ;required to place the returned value back into position
       LD      A,C
       AND     %111
       INC     A
       LD      E,A
       LD      D,A             ;d and e both contain the number of bit positions to shift
       LD      A,C             ;C shr 3 to C
       RRCA
       RRCA
       RRCA
       AND     %11111
       LD      C,A
       LD      A,B             ;B shl 5
       ADD     A,A
       ADD     A,A
       ADD     A,A
       ADD     A,A
       ADD     A,A
       OR      C               ;bbbccccc to C
       LD      C,A
       LD      A,B             ;BC shr 3 to BC
       RRCA
       RRCA
       RRCA
       AND     %11111
       LD      B,A
       LD      HL,(alloca)     ;base address of allocation vector
       ADD     HL,BC           ;byte to A, hl = .alloc(BC shr 3)
       LD      A,(HL)          ;now move the bit to the low order position of A

rotl:
       RLCA
       DEC     E
       JP      NZ,rotl
       RET
set_alloc_bit:
                               ;BC is the bit position of ALLOC to set or reset.  The
                               ;value of the bit is in register E.
       PUSH    DE              ;shifted val A, count in D
       CALL    getallocbit
       AND     %11111110       ;mask low bit to zero (may be set)
       POP     BC              ;low bit of C is masked into A
       OR      C               ;	jmp rotr ;to rotate back into proper position	
                               ;ret

rotr:
                               ;byte value from ALLOC is in register A, with shift count
                               ;in register C (to place bit back into position), and
                               ;target ALLOC position in registers HL, rotate and replace
       RRCA                    ;back into position
       DEC     D
       JP      NZ,rotr
       LD      (HL),A          ;back to ALLOC
       RET
scandm:
                               ;scan the disk map addressed by dptr for non-zero
                               ;entries, the allocation vector entry corresponding
                               ;to a non-zero entry is set to the value of C (0,1)
       CALL    getdptra        ;HL = buffa + dptr
                               ;HL addresses the beginning of the directory entry
       LD      DE,dskmap       ;hl now addresses the disk map
       ADD     HL,DE
       PUSH    BC              ;save the 0/1 bit to set
	
equ		LOC1,fcblen-dskmap+1
       LD      C,LOC1
                               ;size of single byte disk map + 1

scandm0:
                               ;loop once for each disk map entry
       POP     DE              ;recall bit parity
       DEC     C               ;all done scanning?
       RET     Z               ;no, get next entry for scan
       PUSH    DE              ;replace bit parity
       LD      A,(single)
       OR      A
       JP      Z,scandm1       ;single byte scan operation
       PUSH    BC              ;save counter
       PUSH    HL              ;save map address
       LD      C,(HL)          ;BC=block#
       LD      B,0
       JP      scandm2
scandm1:
                               ;double byte scan operation
       DEC     C               ;count for double byte
       PUSH    BC              ;save counter
       LD      C,(HL)          ;BC=block#
       INC     HL
       LD      B,(HL)
       PUSH    HL              ;save map address

scandm2:
                               ;arrive here with BC=block#, E=0/1
       LD      A,C             ;skip if = 0000
       OR      B
       JP      Z,scanm3
       LD      HL,(maxall)     ;check invalid index
       LD      A,L             ;maxall - block#
       SUB     C
       LD      A,H
       SBC     A,B
       CALL    NC,set_alloc_bit;bit set to 0/1

scanm3:
       POP     HL              ;to next bit position
       INC     HL
       POP     BC              ;recall counter
       JP      scandm0         ;for another item

initialize:
                               ;initialize the current disk
                               ;lret = false ;set to true if $ file exists
                               ;compute the length of the allocation vector - 2
       LD      HL,(maxall)     ;perform maxall/8
       LD      C,3             ;number of bytes in alloc vector is (maxall/8)+1
       CALL    hlrotr          ;HL = maxall/8+1
       INC     HL
       LD      B,H             ;count down BC til zero
       LD      C,L
       LD      HL,(alloca)     ;base of allocation vector
                               ;fill the allocation vector with zeros

initial0:
       LD      (HL),0          ;alloc(i)=0
       INC     HL
       DEC     BC              ;count length down
       LD      A,B
       OR      C
       JP      NZ,initial0     ;set the reserved space for the directory
       LD      HL,(dirblk)
       EX      DE,HL
       LD      HL,(alloca)     ;HL=.alloc()
       LD      (HL),E          ;sets reserved directory blks
       INC     HL
       LD      (HL),D          ;allocation vector initialized, home disk
       CALL    home            ;cdrmax = 3 (scans at least one directory record)
       LD      HL,(cdrmaxa)
       LD      (HL),3
       INC     HL
       LD      (HL),0          ;cdrmax = 0000
       CALL    set_end_dir     ;dcnt = enddir
                               ;read directory entries and check for allocated storage

initial2:
       LD      C,_true
       CALL    read_dir
       CALL    end_of_dir      ;return if end of directory
       RET     Z               ;not end of directory, valid entry?
       CALL    getdptra        ;HL = buffa + dptr
       LD      A,empty
       CP      (HL)
       JP      Z,initial2      ;go get another item
                               ;not empty, user code the same?
       LD      A,(usrcode)
       CP      (HL)
       JP      NZ,pdollar      ;same user code, check for '$' submit
       INC     HL              ;first character
       LD      A,(HL)
       SUB     '$'             ;dollar file?
       JP      NZ,pdollar      ;dollar file found, mark in lret
       DEC     A               ;lret = 255
       LD      (lret),A
pdollar:
                               ;now scan the disk map for allocated blocks
       LD      C,1             ;set to allocated
       CALL    scandm
       CALL    setcdr          ;set cdrmax to dcnt
       JP      initial2        ;for another entry

copy_dirloc:
                               ;copy directory location to lret following
                               ;delete, rename, ... ops
       LD      A,(dirloc)
       JP      sta_ret         ;	ret

compext:
                               ;compare extent# in A with that in C, return nonzero
                               ;if they do not match
       PUSH    BC              ;save C's original value
       PUSH    AF
       LD      A,(extmsk)
       CPL
       LD      B,A             ;B has negated form of extent mask
       LD      A,C             ;low bits removed from C
       AND     B
       LD      C,A
       POP     AF              ;low bits removed from A
       AND     B
       SUB     C               ;set flags
       AND     maxext
       POP     BC              ;restore original values
       RET
bsearch:
                               ;search for directory element of length C at info
       LD      A,$0ff          ;changed if actually found
       LD      (dirloc),A
       LD      HL,searchl      ;searchl = C
       LD      (HL),C
       LD      HL,(info)       ;searcha = info
       LD      (searcha),HL
       CALL    set_end_dir     ;dcnt = enddir
       CALL    home            ;to start at the beginning
                               ;(drop through to searchn)

searchn:
                               ;search for the next directory element, assuming
                               ;a previous call on search which sets searcha and
                               ;searchl
       LD      C,_false        ;read next dir element
       CALL    read_dir
       CALL    end_of_dir      ;skip to end if so
       JP      Z,search_fin    ;not end of directory, scan for match
       LD      HL,(searcha)    ;DE=beginning of user fcb
       EX      DE,HL
       LD      A,(DE)          ;first character
       CP      empty           ;keep scanning if empty
       JP      Z,searchnext    ;not empty, may be end of logical directory
       PUSH    DE              ;save search address
       CALL    compcdr         ;past logical end?
       POP     DE              ;recall address
       JP      NC,search_fin   ;artificial stop

searchnext:
       CALL    getdptra        ;HL = buffa+dptr
       LD      A,(searchl)     ;length of search to c
       LD      C,A
       LD      B,0             ;b counts up, c counts down

searchloop:
       LD      A,C
       OR      A
       JP      Z,endsearch		; Was $endsearc - BAD TRANSLATION ERROR???
       LD      A,(DE)          ;? matches all
       CP      '?'
       JP      Z,searchok      ;scan next character if not ubytes
       LD      A,B
       CP      ubytes
       JP      Z,searchok      ;not the ubytes field, extent field?
       CP      extnum          ;may be extent field
       LD      A,(DE)          ;fcb character
       JP      Z,searchext     ;skip to search extent
       SUB     (HL)            ;mask-out flags/extent modulus
       AND     $7f
       JP      NZ,searchn      ;skip if not matched
       JP      searchok        ;matched character

searchext:
                               ;A has fcb character
                               ;attempt an extent # match
       PUSH    BC              ;save counters
       LD      C,(HL)          ;directory character to c
       CALL    compext         ;compare user/dir char
       POP     BC              ;recall counters
       JP      NZ,searchn      ;skip if no match

searchok:
                               ;current character matches
       INC     DE
       INC     HL
       INC     B
       DEC     C
       JP      searchloop
endsearch:
                               ;entire name matches, return dir position
       LD      A,(dcnt)
       AND     dskmsk
       LD      (lret),A        ;lret = low(dcnt) and 11b
       LD      HL,dirloc       ;dirloc=0ffh?
       LD      A,(HL)
       RLA
       RET     NC              ;yes, change it to 0 to mark as found
       XOR     A               ;dirloc=0
       LD      (HL),A
       RET
search_fin:
                               ;end of directory, or empty name
       CALL    set_end_dir     ;may be artifical end
       LD      A,255
       JP      sta_ret
bdelete:
                               ;delete the currently addressed file
       CALL    check_write     ;write protected?
       LD      C,extnum        ;search through file type
       CALL    bsearch
delete0:
                               ;loop while directory matches
       CALL    end_of_dir      ;stop if end
       RET     Z               ;set each non zero disk map entry to 0
                               ;in the allocation vector
                               ;may be r/o file
       CALL    check_rodir     ;ro disk error if found
       CALL    getdptra        ;HL=.buff(dptr)
       LD      (HL),empty
       LD      C,0             ;alloc elts set to 0
       CALL    scandm
       CALL    wrdir           ;write the directory
       CALL    searchn         ;to next element
       JP      delete0         ;for another record

get_block:
                               ;given allocation vector position BC, find the zero bit
                               ;closest to this position by searching left and right.
                               ;if found, set the bit to one and return the bit position
                               ;in hl.  if not found (i.e., we pass 0 on the left, or
                               ;maxall on the right), return 0000 in hl
       LD      D,B             ;copy of starting position to de
       LD      E,C
lefttst:
       LD      A,C             ;skip if left=0000
       OR      B
       JP      Z,righttst      ;left not at position zero, bit zero?
       DEC     BC              ;left,right pushed
       PUSH    DE
       PUSH    BC
       CALL    getallocbit
       RRA                     ;return block number if zero
       JP      NC,retblock     ;bit is one, so try the right
       POP     BC              ;left, right restored
       POP     DE
righttst:
       LD      HL,(maxall)     ;value of maximum allocation#
       LD      A,E             ;right=maxall?
       SUB     L
       LD      A,D
       SBC     A,H
       JP      NC,retblock0    ;return block 0000 if so
       INC     DE              ;left, right pushed
       PUSH    BC
       PUSH    DE
       LD      B,D             ;ready right for call
       LD      C,E
       CALL    getallocbit
       RRA                     ;return block number if zero
       JP      NC,retblock
       POP     DE              ;restore left and right pointers
       POP     BC
       JP      lefttst         ;for another attempt

retblock:
       RLA                     ;bit back into position and set to 1
       INC     A               ;d contains the number of shifts required to reposition
       CALL    rotr            ;move bit back to position and store
       POP     HL              ;HL returned value, DE discarded
       POP     DE
       RET
retblock0:
                               ;cannot find an available bit, return 0000
       LD      A,C             ;
       OR      B               ;also at beginning
       JP      NZ,lefttst
       LD      HL,$0000
       RET
copy_fcb:
                               ;copy the entire file control block
       LD      C,0             ;start at 0, to fcblen-1
       LD      E,fcblen        ;	jmp copy_dir

copy_dir:
                               ;copy fcb information starting at C for E bytes
                               ;into the currently addressed directory entry
       PUSH    DE              ;save length for later
       LD      B,0             ;double index to BC
       LD      HL,(info)       ;HL = source for data
       ADD     HL,BC           ;DE=.fcb(C), source for copy
       EX      DE,HL
       CALL    getdptra        ;HL=.buff(dptr), destination
       POP     BC              ;DE=source, HL=dest, C=length
       CALL    move            ;data moved

seek_copy:
                               ;enter from close to seek and copy current element
       CALL    seek_dir        ;to the directory element
       JP      wrdir           ;write the directory element
                               ;ret

brename:
                               ;rename the file described by the first half of
                               ;the currently addressed file control block. the
                               ;new name is contained in the last half of the
                               ;currently addressed file conrol block.  the file
                               ;name and type are changed, but the reel number
                               ;is ignored.  the user number is identical
       CALL    check_write     ;may be write protected
                               ;search up to the extent field
       LD      C,extnum
       CALL    bsearch          ;copy position 0
       LD      HL,(info)       ;HL=.fcb(0), A=fcb(0)
       LD      A,(HL)
       LD      DE,dskmap       ;HL=.fcb(dskmap)
       ADD     HL,DE
       LD      (HL),A          ;fcb(dskmap)=fcb(0)
                               ;assume the same disk drive for new named file

rename0:
       CALL    end_of_dir      ;stop at end of dir
       RET     Z               ;not end of directory, rename next element
       CALL    check_rodir     ;may be read-only file
       LD      C,dskmap
       LD      E,extnum
       CALL    copy_dir        ;element renamed, move to next
       CALL    searchn
       JP      rename0
indicators:
                               ;set file indicators for current fcb
       LD      C,extnum        ;through file type
       CALL    bsearch
indic0:
       CALL    end_of_dir      ;stop at end of dir
       RET     Z               ;not end of directory, continue to change
       LD      C,0             ;copy name
       LD      E,extnum
       CALL    copy_dir
       CALL    searchn
       JP      indic0
bopen:
                               ;search for the directory entry, copy to fcb
       LD      C,namlen
       CALL    bsearch
       CALL    end_of_dir      ;return with lret=255 if end
       RET     Z               ;not end of directory, copy fcb information

open_copy:
                               ;(referenced below to copy fcb info)
       CALL    getexta         ;save extent#
       LD      A,(HL)
       PUSH    AF
       PUSH    HL
       CALL    getdptra        ;DE = .buff(dptr)
       EX      DE,HL
       LD      HL,(info)       ;HL=.fcb(0)
       LD      C,nxtrec        ;length of move operation
       PUSH    DE              ;save .buff(dptr)
       CALL    move            ;from .buff(dptr) to .fcb(0)
                               ;note that entire fcb is copied, including indicators
       CALL    setfwf          ;sets file write flag
       POP     DE              ;HL=.buff(dptr+extnum)
       LD      HL,extnum
       ADD     HL,DE
       LD      C,(HL)          ;C = directory extent number
       LD      HL,reccnt       ;HL=.buff(dptr+reccnt)
       ADD     HL,DE
       LD      B,(HL)          ;B holds directory record count
       POP     HL              ;restore extent number
       POP     AF
       LD      (HL),A          ;HL = .user extent#, B = dir rec cnt, C = dir extent#
                               ;if user ext < dir ext then user := 128 records
                               ;if user ext = dir ext then user := dir records
                               ;if user ext > dir ext then user := 0 records
       LD      A,C             ;ready dir reccnt
       CP      (HL)
       LD      A,B
       JP      Z,open_rcnt     ;if same, user gets dir reccnt
       LD      A,0             ;user is larger
       JP      C,open_rcnt
       LD      A,128           ;directory is larger

open_rcnt:
                               ;A has record count to fill
       LD      HL,(info)
       LD      DE,reccnt
       ADD     HL,DE
       LD      (HL),A
       RET
mergezero:
                               ;HL = .fcb1(i), DE = .fcb2(i),
                               ;if fcb1(i) = 0 then fcb1(i) := fcb2(i)
       LD      A,(HL)          ;return if = 0000
       INC     HL
       OR      (HL)
       DEC     HL
       RET     NZ
       LD      A,(DE)          ;low byte copied
       LD      (HL),A
       INC     DE
       INC     HL
       LD      A,(DE)          ;back to input form
       LD      (HL),A
       DEC     DE
       DEC     HL
       RET
bclose:
                               ;locate the directory element and re-write it
       XOR     A
       LD      (lret),A
       LD      (dcnt),A
       LD      (dcnt+1),A
       CALL    nowrite         ;skip close if r/o disk
       RET     NZ              ;check file write flag - 0 indicates written
       CALL    getmodnum       ;fcb(modnum) in A
       AND     fwfmsk          ;return if bit remains set
       RET     NZ
       LD      C,namlen        ;locate file
       CALL    bsearch
       CALL    end_of_dir      ;return if not found
       RET     Z               ;merge the disk map at info with that at buff(dptr)
       LD      BC,dskmap
       CALL    getdptra
       ADD     HL,BC           ;DE is .buff(dptr+16)
       EX      DE,HL
       LD      HL,(info)       ;DE=.buff(dptr+16), HL=.fcb(16)
       ADD     HL,BC
equ		LOC2,fcblen-dskmap
       LD      C,LOC2
                               ;length of single byte dm

merge0:
       LD      A,(single)      ;skip to double
       OR      A
       JP      Z,merged        ;this is a single byte map
                               ;if fcb(i) = 0 then fcb(i) = buff(i)
                               ;if buff(i) = 0 then buff(i) = fcb(i)
                               ;if fcb(i) <> buff(i) then error
       LD      A,(HL)
       OR      A
       LD      A,(DE)
       JP      NZ,fcbnzero     ;fcb(i) = 0
       LD      (HL),A          ;fcb(i) = buff(i)

fcbnzero:
       OR      A
       JP      NZ,buffnzero    ;buff(i) = 0
       LD      A,(HL)          ;buff(i)=fcb(i)
       LD      (DE),A
buffnzero:
       CP      (HL)            ;fcb(i) = buff(i)?
       JP      NZ,mergerr
       JP      dmset           ;if merge ok

merged:
                               ;this is a double byte merge operation
       CALL    mergezero       ;buff = fcb if buff 0000
       EX      DE,HL           ;fcb = buff if fcb 0000
       CALL    mergezero
       EX      DE,HL           ;they should be identical at this point
       LD      A,(DE)          ;low same?
       CP      (HL)
       JP      NZ,mergerr
       INC     DE              ;to high byte
       INC     HL
       LD      A,(DE)          ;high same?
       CP      (HL)
       JP      NZ,mergerr      ;merge operation ok for this pair
       DEC     C               ;extra count for double byte

dmset:
       INC     DE              ;to next byte position
       INC     HL
       DEC     C               ;for more
       JP      NZ,merge0       ;end of disk map merge, check record count
                               ;DE = .buff(dptr)+32, HL = .fcb(32)
							   ; 	lxi	b,-(fcblen-extnum)
equ		LOC3,fcblen-extnum
equ		LOC4,$0-LOC3				; bad translation error???? Check $0 and equation.	   
       LD      BC,LOC4
       ADD     HL,BC
       EX      DE,HL
       ADD     HL,BC           ;DE = .fcb(extnum), HL = .buff(dptr+extnum)
       LD      A,(DE)          ;current user extent number
                               ;if fcb(ext) >= buff(fcb) then
                               ;buff(ext) := fcb(ext), buff(rec) := fcb(rec)
       CP      (HL)
       JP      C,endmerge      ;fcb extent number >= dir extent number
       LD      (HL),A          ;buff(ext) = fcb(ext)
                               ;update directory record count field
							   ; Was (reccnt-extnum) - Shouldn't have brackets. 
       LD      BC,reccnt-extnum
       ADD     HL,BC
       EX      DE,HL
       ADD     HL,BC           ;DE=.buff(reccnt), HL=.fcb(reccnt)
       LD      A,(HL)          ;buff(reccnt)=fcb(reccnt)
       LD      (DE),A
endmerge:
       LD      A,_true         ;mark as copied
       LD      (fcb_copied),A
       JP      seek_copy       ;ok to "wrdir" here - 1.4 compat
                               ;		ret

mergerr:
                               ;elements did not merge correctly
       LD      HL,lret         ;=255 non zero flag set
       DEC     (HL)
       RET
make:
                               ;create a new file by creating a directory entry
                               ;then opening the file
       CALL    check_write     ;may be write protected
       LD      HL,(info)       ;save fcb address, look for e5
       PUSH    HL
       LD      HL,efcb         ;info = .empty
       LD      (info),HL
       LD      C,1             ;length 1 match on empty entry
       CALL    bsearch
       CALL    end_of_dir      ;zero flag set if no space
       POP     HL              ;recall info address
       LD      (info),HL       ;in case we return here
       RET     Z               ;return with error condition 255 if not found
       EX      DE,HL           ;DE = info address
                               ;clear the remainder of the fcb
       LD      HL,namlen       ;HL=.fcb(namlen)
       ADD     HL,DE
       LD      C,fcblen-namlen ;number of bytes to fill
       XOR     A               ;clear accumulator to 00 for fill

make0:
       LD      (HL),A
       INC     HL
       DEC     C
       JP      NZ,make0
       LD      HL,ubytes       ;HL = .fcb(ubytes)
       ADD     HL,DE
       LD      (HL),A          ;fcb(ubytes) = 0
       CALL    setcdr          ;may have extended the directory
                               ;now copy entry to the directory
       CALL    copy_fcb        ;and set the file write flag to "1"
       JP      setfwf          ;ret

open_reel:
                               ;close the current extent, and open the next one
                               ;if possible.  RMF is true if in read mode
       XOR     A               ;set true if actually copied
       LD      (fcb_copied),A
       CALL    bclose           ;close current extent
                               ;lret remains at enddir if we cannot open the next ext
       CALL    end_of_dir      ;return if end
       RET     Z               ;increment extent number
       LD      HL,(info)       ;HL=.fcb(extnum)
       LD      BC,extnum
       ADD     HL,BC
       LD      A,(HL)          ;fcb(extnum)=++1
       INC     A
       AND     maxext
       LD      (HL),A
       JP      Z,open_mod      ;move to next module if zero
                               ;may be in the same extent group
       LD      B,A
       LD      A,(extmsk)
       AND     B               ;if result is zero, then not in the same group
       LD      HL,fcb_copied   ;true if the fcb was copied to directory
       AND     (HL)            ;produces a 00 in accumulator if not written
       JP      Z,open_reel0    ;go to next physical extent
                               ;result is non zero, so we must be in same logical ext
       JP      open_reel1      ;to copy fcb information

open_mod:
                               ;extent number overflow, go to next module
							   ; was (modnum-extnum)
       LD      BC,modnum-extnum
                               ;HL=.fcb(modnum)
       ADD     HL,BC
       INC     (HL)            ;fcb(modnum)=++1
                               ;module number incremented, check for overflow
       LD      A,(HL)          ;mask high order bits
       AND     maxmod
       JP      Z,open_r_err    ;cannot overflow to zero
                               ;otherwise, ok to continue with new module

open_reel0:
       LD      C,namlen        ;next extent found?
       CALL    bsearch
       CALL    end_of_dir
       JP      NZ,open_reel1   ;end of file encountered
       LD      A,(rmf)         ;0ffh becomes 00 if read
       INC     A
       JP      Z,open_r_err    ;sets lret = 1
                               ;try to extend the current file
       CALL    make            ;cannot be end of directory
       CALL    end_of_dir
       JP      Z,open_r_err    ;with lret = 1
       JP      open_reel2
open_reel1:
                               ;not end of file, open
       CALL    open_copy
open_reel2:
       CALL    getfcb          ;set parameters
       XOR     A               ;lret = 0
       JP      sta_ret         ;	ret ;with lret = 0

open_r_err:
                               ;cannot move to next extent of this file
       CALL    setlret1        ;lret = 1
       JP      setfwf          ;ensure that it will not be closed
                               ;ret

seqdiskread:
                               ;sequential disk read operation
       LD      A,1
       LD      (seqio),A       ;drop through to diskread

diskread:
                               ;(may enter from seqdiskread)
       LD      A,_true         ;read mode flag = true (open_reel)
       LD      (rmf),A         ;read the next record from the current fcb
       CALL    getfcb          ;sets parameters for the read
       LD      A,(vrecord)     ;vrecord-rcount
       LD      HL,rcount
       CP      (HL)            ;skip if rcount > vrecord
       JP      C,recordok      ;not enough records in the extent
                               ;record count must be 128 to continue
       CP      128             ;vrecord = 128?
       JP      NZ,diskeof      ;skip if vrecord<>128
       CALL    open_reel       ;go to next extent if so
       XOR     A               ;vrecord=00
       LD      (vrecord),A     ;now check for open ok
       LD      A,(lret)        ;stop at eof
       OR      A
       JP      NZ,diskeof
recordok:
                               ;arrive with fcb addressing a record to read
       CALL    index           ;error 2 if reading unwritten data
                               ;(returns 1 to be compatible with 1.4)
       CALL    allocated       ;arecord=0000?
       JP      Z,diskeof       ;record has been allocated, read it
       CALL    atran           ;arecord now a disk address
       CALL    seek            ;to proper track,sector
       CALL    rdbuff          ;to dma address
       JP      setfcb          ;replace parameter	
                               ;		ret

diskeof:
       JP      setlret1        ;lret = 1
                               ;ret

seqdiskwrite:
                               ;sequential disk write
       LD      A,1
       LD      (seqio),A       ;drop through to diskwrite

diskwrite:
                               ;(may enter here from seqdiskwrite above)
       LD      A,_false        ;read mode flag
       LD      (rmf),A         ;write record to currently selected file
       CALL    check_write     ;in case write protected
       LD      HL,(info)       ;HL = .fcb(0)
       CALL    check_rofile    ;may be a read-only file
       CALL    getfcb          ;to set local parameters
       LD      A,(vrecord)     ;vrecord-128
       CP      lstrec+1        ;skip if vrecord > lstrec
                               ;vrecord = 128, cannot open next extent
       JP      NC,setlret1     ;lret=1

diskwr0:
                               ;can write the next record, so continue
       CALL    index
       CALL    allocated
       LD      C,0             ;marked as normal write operation for wrbuff
       JP      NZ,diskwr1      ;not allocated
                               ;the argument to getblock is the starting
                               ;position for the disk search, and should be
                               ;the last allocated block for this file, or
                               ;the value 0 if no space has been allocated
       CALL    dm_position
       LD      (dminx),A       ;save for later
       LD      BC,$0000        ;may use block zero
       OR      A               ;skip if no previous block
       JP      Z,nopblock      ;previous block exists at A
       LD      C,A             ;previous block # in BC
       DEC     BC
       CALL    getdm           ;previous block # to HL
       LD      B,H             ;BC=prev block#
       LD      C,L
nopblock:
                               ;BC = 0000, or previous block #
       CALL    get_block       ;block # to HL
                               ;arrive here with block# or zero
       LD      A,L
       OR      H
       JP      NZ,blockok      ;cannot find a block to allocate
       LD      A,2             ;lret=2
       JP      sta_ret
blockok:
                               ;allocated block number is in HL
       LD      (arecord),HL
       EX      DE,HL           ;block number to DE
       LD      HL,(info)       ;HL=.fcb(dskmap)
       LD      BC,dskmap
       ADD     HL,BC
       LD      A,(single)      ;set flags for single byte dm
       OR      A
       LD      A,(dminx)       ;recall dm index
       JP      Z,allocwd       ;skip if allocating word
                               ;allocating a byte value
       CALL    addh            ;single byte alloc BAD TRANSLATIONE ERROR - Was $add - I think it should be addh??? $add from $addh seems plausible. 
       LD      (HL),E
       JP      diskwru         ;to continue

allocwd:
                               ;allocate a word value
       LD      C,A             ;double(dminx)
       LD      B,0
       ADD     HL,BC           ;HL=.fcb(dminx*2)
       ADD     HL,BC
       LD      (HL),E          ;double wd
       INC     HL
       LD      (HL),D
diskwru:
                               ;disk write to previously unallocated block
       LD      C,2             ;marked as unallocated write

diskwr1:
                               ;continue the write operation of no allocation error
                               ;C = 0 if normal write, 2 if to prev unalloc block
       LD      A,(lret)        ;stop if non zero returned value
       OR      A
       RET     NZ
       PUSH    BC              ;save write flag
       CALL    atran           ;arecord set
       LD      A,(seqio)
       DEC     A
       DEC     A
       JP      NZ,diskwr11
       POP     BC
       PUSH    BC
       LD      A,C
       DEC     A
       DEC     A
       JP      NZ,diskwr11     ;old allocation  
       PUSH    HL              ;arecord in hl ret from atran
       LD      HL,(buffa)      ;zero buffa & fill
       LD      D,A
fill0:
       LD      (HL),A
       INC     HL
       INC     D
       JP      P,fill0
       CALL    setdir
       LD      HL,(arecord1)
       LD      C,2
fill1:
       LD      (arecord),HL
       PUSH    BC
       CALL    seek
       POP     BC
       CALL    wrbuff          ;write fill record
       LD      HL,(arecord)    ;restore last record     
       LD      C,0             ;change  allocate flag   
       LD      A,(blkmsk)
       LD      B,A
       AND     L
       CP      B
       INC     HL
       JP      NZ,fill1        ;cont until cluster is zeroed
       POP     HL
       LD      (arecord),HL
       CALL    setdata
diskwr11:
       CALL    seek            ;to proper file position
       POP     BC              ;restore/save write flag (C=2 if new block)
       PUSH    BC
       CALL    wrbuff          ;written to disk
       POP     BC              ;C = 2 if a new block was allocated, 0 if not
                               ;increment record count if rcount<=vrecord
       LD      A,(vrecord)     ;vrecord-rcount
       LD      HL,rcount
       CP      (HL)
       JP      C,diskwr2       ;rcount <= vrecord
       LD      (HL),A          ;rcount = vrecord+1
       INC     (HL)
       LD      C,2             ;mark as record count incremented

diskwr2:
; PATCH1
;                               ; CP/M V2.2 patch 1 for use of optional blocking/deblocking
;       NOP
;       NOP
;       LD      HL,0ELSE
;	; original code
	;A has vrecord, C=2 if new block or new record#
       DEC     C
       DEC     C
       JP      NZ,noupdate
; End of patch option. 




       PUSH    AF              ;save vrecord value
       CALL    getmodnum       ;HL=.fcb(modnum), A=fcb(modnum)
                               ;reset the file write flag to mark as written fcb
       AND     NOTFWFMSK	  	;bit reset ERROR? Not sure if this is correct - FULL BDOS comes up with zero here.... 
       LD      (HL),A          ;fcb(modnum) = fcb(modnum) and 7fh
       POP     AF              ;restore vrecord

noupdate:
                               ;check for end of extent, if found attempt to open
                               ;next extent in preparation for next write
       CP      lstrec          ;vrecord=lstrec?
       JP      NZ,diskwr3      ;skip if not
                               ;may be random access write, if so we are done
                               ;change next     
       LD      A,(seqio)       ;skip next extent open op
       CP      1
       JP      NZ,diskwr3      ;update current fcb before going to next extent
       CALL    setfcb
       CALL    open_reel       ;rmf=false
                               ;vrecord remains at lstrec causing eof if
                               ;no more directory space is available
       LD      HL,lret
       LD      A,(HL)
       OR      A
       JP      NZ,bnospace      ;space available, set vrecord=255
       DEC     A               ;goes to 00 next time
       LD      (vrecord),A
bnospace:
       LD      (HL),0          ;lret = 00 for returned value

diskwr3:
       JP      setfcb          ;replace parameters
                               ;ret

rseek:
                               ;random access seek operation, C=0ffh if read mode
                               ;fcb is assumed to address an active file control block
                               ;(modnum has been set to 1100_0000b if previous bad seek)
       XOR     A               ;marked as random access operation
       LD      (seqio),A
rseek1:
       PUSH    BC              ;save r/w flag
       LD      HL,(info)       ;DE will hold base of fcb
       EX      DE,HL
       LD      HL,ranrec       ;HL=.fcb(ranrec)
       ADD     HL,DE
       LD      A,(HL)          ;record number
       AND     $7f
       PUSH    AF
       LD      A,(HL)          ;cy=lsb of extent#
       RLA
       INC     HL              ;A=ext#
       LD      A,(HL)
       RLA
       AND     %11111
       LD      C,A             ;C holds extent number, record stacked
       LD      A,(HL)          ;mod#
       RRA
       RRA
       RRA
       RRA
       AND     %1111
       LD      B,A             ;B holds module#, C holds ext#
       POP     AF              ;recall sought record #
                               ;check to insure that high byte of ran rec = 00
       INC     HL              ;l=high byte (must be 00)
       LD      L,(HL)
       INC     L               ;zero flag, l=6
       DEC     L
       LD      L,6             ;produce error 6, seek past physical eod
       JP      NZ,seekerr      ;otherwise, high byte = 0, A = sought record
       LD      HL,nxtrec       ;HL = .fcb(nxtrec)
       ADD     HL,DE
       LD      (HL),A          ;sought rec# stored away
                               ;arrive here with B=mod#, C=ext#, DE=.fcb, rec stored
                               ;the r/w flag is still stacked.  compare fcb values
       LD      HL,extnum       ;A=seek ext#
       ADD     HL,DE
       LD      A,C
       SUB     (HL)            ;tests for = extents
       JP      NZ,ranclose     ;extents match, check mod#
       LD      HL,modnum       ;B=seek mod#
       ADD     HL,DE
       LD      A,B             ;could be overflow at eof, producing module#
                               ;of 90H or 10H, so compare all but fwf
       SUB     (HL)            ;same?
       AND     $7f
       JP      Z,seekok
ranclose:
       PUSH    BC              ;save seek mod#,ext#, .fcb
       PUSH    DE
       CALL    bclose           ;current extent closed
       POP     DE              ;recall parameters and fill
       POP     BC
       LD      L,3             ;cannot close error #3
       LD      A,(lret)
       INC     A
       JP      Z,badseek
       LD      HL,extnum       ;fcb(extnum)=ext#
       ADD     HL,DE
       LD      (HL),C
       LD      HL,modnum       ;fcb(modnum)=mod#
       ADD     HL,DE
       LD      (HL),B
       CALL    bopen            ;is the file present?
       LD      A,(lret)        ;open successful?
       INC     A
       JP      NZ,seekok       ;cannot open the file, read mode?
       POP     BC              ;r/w flag to c (=0ffh if read)
       PUSH    BC              ;everyone expects this item stacked
       LD      L,4             ;seek to unwritten extent #4
       INC     C               ;becomes 00 if read operation
       JP      Z,badseek       ;skip to error if read operation
                               ;write operation, make new extent
       CALL    make
       LD      L,5             ;cannot create new extent #5
       LD      A,(lret)        ;no dir space
       INC     A
       JP      Z,badseek       ;file make operation successful

seekok:
       POP     BC              ;discard r/w flag
       XOR     A               ;with zero set
       JP      sta_ret
badseek:
                               ;fcb no longer contains a valid fcb, mark
                               ;with 1100_000b in modnum field so that it
                               ;appears as overflow with file write flag set
       PUSH    HL              ;save error flag
       CALL    getmodnum       ;HL = .modnum
       LD      (HL),%11000000
       POP     HL              ;and drop through

seekerr:
       POP     BC              ;discard r/w flag
       LD      A,L             ;lret=#, nonzero
       LD      (lret),A        ;setfwf returns non-zero accumulator for err
       JP      setfwf          ;flag set, so subsequent close ok
                               ;ret

randiskread:
                               ;random disk read operation
       LD      C,_true         ;marked as read operation
       CALL    rseek
       CALL    Z,diskread      ;if seek successful
       RET
randiskwrite:
                               ;random disk write operation
       LD      C,_false        ;marked as write operation
       CALL    rseek
       CALL    Z,diskwrite     ;if seek successful
       RET
compute_rr:
                               ;compute random record position for getfilesize/setrandom
       EX      DE,HL
       ADD     HL,DE           ;DE=.buf(dptr) or .fcb(0), HL = .f(nxtrec/reccnt)
       LD      C,(HL)          ;BC = 0000 0000 ?rrr rrrr
       LD      B,0
       LD      HL,extnum       ;A=e000 0000
       ADD     HL,DE
       LD      A,(HL)
       RRCA
       AND     $80
       ADD     A,C
       LD      C,A
       LD      A,0
       ADC     A,B
       LD      B,A             ;BC = 0000 000? errrr rrrr
       LD      A,(HL)
       RRCA
       AND     $0f
       ADD     A,B
       LD      B,A             ;BC = 000? eeee errrr rrrr
       LD      HL,modnum       ;A=XXX? mmmm
       ADD     HL,DE
       LD      A,(HL)
       ADD     A,A             ;cy=? A=mmmm 0000
       ADD     A,A
       ADD     A,A
       ADD     A,A
       PUSH    AF
       ADD     A,B
       LD      B,A             ;cy=?, BC = mmmm eeee errr rrrr
       PUSH    AF              ;possible second carry
       POP     HL              ;cy = lsb of L
       LD      A,L             ;cy = lsb of A
       POP     HL              ;cy = lsb of L
       OR      L               ;cy/cy = lsb of A
       AND     1               ;A = 0000 000? possible carry-out
       RET
getfilesize:
                               ;compute logical file size for current fcb
       LD      C,extnum
       CALL    bsearch          ;zero the receiving ranrec field
       LD      HL,(info)       ;save position
       LD      DE,ranrec
       ADD     HL,DE
       PUSH    HL
       LD      (HL),D          ;=00 00 00
       INC     HL
       LD      (HL),D
       INC     HL
       LD      (HL),D
getsize:
       CALL    end_of_dir
       JP      Z,setsize       ;current fcb addressed by dptr
       CALL    getdptra        ;ready for compute size
       LD      DE,reccnt
       CALL    compute_rr      ;A=0000 000? BC = mmmm eeee errr rrrr
                               ;compare with memory, larger?
       POP     HL              ;recall, replace .fcb(ranrec)
       PUSH    HL
       LD      E,A             ;save cy
       LD      A,C             ;ls byte
       SUB     (HL)
       INC     HL
       LD      A,B             ;middle byte
       SBC     A,(HL)
       INC     HL
       LD      A,E             ;carry if .fcb(ranrec) > directory
       SBC     A,(HL)
       JP      C,getnextsize   ;for another try
                               ;fcb is less or equal, fill from directory
       LD      (HL),E
       DEC     HL
       LD      (HL),B
       DEC     HL
       LD      (HL),C
getnextsize:
       CALL    searchn
       JP      getsize
setsize:
       POP     HL              ;discard .fcb(ranrec)
       RET
setrandom:
                               ;set random record from the current file control block
       LD      HL,(info)       ;ready params for computesize
       LD      DE,nxtrec
       CALL    compute_rr      ;DE=info, A=cy, BC=mmmm eeee errr rrrr
       LD      HL,ranrec       ;HL = .fcb(ranrec)
       ADD     HL,DE
       LD      (HL),C          ;to ranrec
       INC     HL
       LD      (HL),B
       INC     HL
       LD      (HL),A
       RET
select:
                               ;select disk info for subsequent input or output ops
       LD      HL,(dlog)
       LD      A,(curdsk)
       LD      C,A
       CALL    hlrotr
       PUSH    HL              ;save it for test below, send to seldsk
       EX      DE,HL
       CALL    selectdisk      ;recall dlog vector
       POP     HL
       CALL    Z,sel_error     ;returns true if select ok
                               ;is the disk logged in?
       LD      A,L             ;return if bit is set
       RRA
       RET     C               ;disk not logged in, set bit and initialize
       LD      HL,(dlog)       ;call ready
       LD      C,L
       LD      B,H
       CALL    set_cdisk       ;dlog=set_cdisk(dlog)
       LD      (dlog),HL
       JP      initialize      ;ret

curselect:
       LD      A,(linfo)       ;skip if linfo=curdsk
       LD      HL,curdsk
       CP      (HL)
       RET     Z
       LD      (HL),A          ;curdsk=info
       JP      select          ;ret

reselect:
                               ;check current fcb to see if reselection necessary
       LD      A,_true         ;mark possible reselect
       LD      (resel),A
       LD      HL,(info)       ;drive select code
       LD      A,(HL)
       AND     %11111          ;non zero is auto drive select
       DEC     A               ;drive code normalized to 0..30, or 255
       LD      (linfo),A       ;save drive code
       CP      30
       JP      NC,noselect     ;auto select function, save curdsk
       LD      A,(curdsk)      ;olddsk=curdsk
       LD      (olddsk),A
       LD      A,(HL)          ;save drive code
       LD      (fcbdsk),A
       AND     %11100000       ;preserve hi bits
       LD      (HL),A
       CALL    curselect
noselect:
                               ;set user code
       LD      A,(usrcode)     ;0...31
       LD      HL,(info)
       OR      (HL)
       LD      (HL),A
       RET                     ;	individual function handlers

func12:
                               ;return version number
       LD      A,dvers         ;lret = dvers (high = 00)
       JP      sta_ret         ;	ret ;jmp goback

func13:
                               ;reset disk system - initialize to disk 0
       LD      HL,0
       LD      (rodsk),HL
       LD      (dlog),HL
       XOR     A               ;note that usrcode remains unchanged
       LD      (curdsk),A
       LD      HL,tbuff        ;dmaad = tbuff
       LD      (dmaad),HL
       CALL    setdata         ;to data dma address
       JP      select          ;ret ;jmp goback
EQU           FUNC14,CURSELECT ;select disk info
                               ;ret ;jmp goback

func15:
                               ;open file
       CALL    clrmodnum       ;clear the module number
       CALL    reselect
       JP      bopen            ;ret ;jmp goback

func16:
                               ;close file
       CALL    reselect
       JP      bclose           ;ret ;jmp goback

func17:
                               ;search for first occurrence of a file
       LD      C,0             ;length assuming '?' true
       EX      DE,HL           ;was lhld info		
       LD      A,(HL)          ;no reselect if ?
       CP      '?'
       JP      Z,qselect       ;skip reselect if so
                               ;normal search
       CALL    getexta
       LD      A,(HL)
       CP      '?'
       CALL    NZ,clrmodnum    ;module number zeroed
       CALL    reselect
       LD      C,namlen
qselect:
       CALL    bsearch
       JP      dir_to_user     ;copy directory entry to user
                               ;ret ;jmp goback

func18:
                               ;search for next occurrence of a file name
       LD      HL,(searcha)
       LD      (info),HL
       CALL    reselect
       CALL    searchn
       JP      dir_to_user     ;copy directory entry to user
                               ;ret ;jmp goback

func19:
                               ;delete a file
       CALL    reselect
       CALL    bdelete
       JP      copy_dirloc     ;ret ;jmp goback

func20:
                               ;read a file
       CALL    reselect
       JP      seqdiskread     ;
                               ;jmp goback

func21:
                               ;write a file
       CALL    reselect
       JP      seqdiskwrite    ;
                               ;jmp goback

func22:
                               ;make a file
       CALL    clrmodnum
       CALL    reselect
       JP      make            ;ret ;jmp goback

func23:
                               ;rename a file
       CALL    reselect
       CALL    brename
       JP      copy_dirloc     ;ret ;jmp goback

func24:
                               ;return the login vector
       LD      HL,(dlog)
       JP      sthl_ret        ;
                               ;	ret ;jmp goback

func25:
                               ;return selected disk number
       LD      A,(curdsk)
       JP      sta_ret         ;	ret ;jmp goback

func26:
                               ;set the subsequent dma address to info
       EX      DE,HL           ;was lhld info	
       LD      (dmaad),HL      ;dmaad = info
       JP      setdata         ;to data dma address
                               ;ret ;jmp goback

func27:
                               ;return the login vector address
       LD      HL,(alloca)
       JP      sthl_ret        ;	ret ;jmp goback
EQU           FUNC28,SET_RO    ;write protect current disk
                               ;ret ;jmp goback

func29:
                               ;return r/o bit vector
       LD      HL,(rodsk)
       JP      sthl_ret        ;	ret ;jmp goback

func30:
                               ;set file indicators
       CALL    reselect
       CALL    indicators
       JP      copy_dirloc     ;lret=dirloc
                               ;ret ;jmp goback

func31:
                               ;return address of disk parameter block
       LD      HL,(dpbaddr)
sthl_ret:
       LD      (aret),HL
       RET                     ;jmp goback

func32:
                               ;set user code
       LD      A,(linfo)
       CP      $0ff
       JP      NZ,setusrcode   ;interrogate user code instead
       LD      A,(usrcode)     ;lret=usrcode
       JP      sta_ret         ;		ret ;jmp goback

setusrcode:
       AND     $1f
       LD      (usrcode),A
       RET                     ;jmp goback
                               ;

func33:
                               ;random disk read operation
       CALL    reselect
       JP      randiskread     ;to perform the disk read
                               ;ret ;jmp goback
                               ;

func34:
                               ;random disk write operation
       CALL    reselect
       JP      randiskwrite    ;to perform the disk write
                               ;ret ;jmp goback
                               ;

func35:
                               ;return file size (0-65536)
       CALL    reselect
       JP      getfilesize     ;ret ;jmp goback
                               ;
EQU           FUNC36,SETRANDOM ;
                               ;set random record
                               ;ret ;jmp goback

func37:
                               ;
       LD      HL,(info)
       LD      A,L
       CPL
       LD      E,A
       LD      A,H
       CPL
       LD      HL,(dlog)
       AND     H
       LD      D,A
       LD      A,L
       AND     E
       LD      E,A
       LD      HL,(rodsk)
       EX      DE,HL
       LD      (dlog),HL
       LD      A,L
       AND     E
       LD      L,A
       LD      A,H
       AND     D
       LD      H,A
       LD      (rodsk),HL
       RET                     ;
                               ;

goback:
                               ;arrive here at end of processing to return to user
       LD      A,(resel)
       OR      A
       JP      Z,retmon        ;reselection may have taken place
       LD      HL,(info)       ;fcb(0)=0
       LD      (HL),0
       LD      A,(fcbdsk)
       OR      A
       JP      Z,retmon        ;restore disk number
       LD      (HL),A          ;fcb(0)=fcbdsk
       LD      A,(olddsk)
       LD      (linfo),A
       CALL    curselect       ;
                               ;	return from the disk monitor

retmon:
       LD      HL,(entsp)      ;user stack restored
       LD      SP,HL
       LD      HL,(aret)       ;BA = HL = aret
       LD      A,L
       LD      B,H
       RET
	   
	   
	   
	   EQU FUNC38,FUNC_RET
	   EQU FUNC39,FUNC_RET
func40:
                               ;random disk write with zero fill of unallocated block
       CALL    reselect
       LD      A,2
       LD      (seqio),A
       LD      C,_false
       CALL    rseek1
       CALL    Z,diskwrite     ;if seek successful
       RET                     ;	data areas
                               ;	initialized data

efcb:
DB                          empty
                               ;0e5=available dir entry

rodsk:
DW                          0  ;read only disk vector

dlog:
DW                          0  ;logged-in disks

dmaad:
DW                          tbuff
                               ;initial dma address
                               ;	curtrka - alloca are set upon disk select
                               ;	(data must be adjacent, do not insert variables)
                               ;	(address of translate vector, not used)

cdrmaxa:
BLOCK         WORD             ;pointer to cur dir max value

curtrka:
BLOCK         WORD             ;current track address

curreca:
BLOCK         WORD             ;current record address

buffa:
BLOCK         WORD             ;pointer to directory dma address

dpbaddr:
BLOCK         WORD             ;current disk parameter block address

checka:
BLOCK         WORD             ;current checksum vector address

alloca:
BLOCK         WORD             ;current allocation vector address
EQU           ADDLIST,$-BUFFA  ;address list size
                               ;	sectpt - offset obtained from disk parm block at dpbaddr
                               ;	(data must be adjacent, do not insert variables)

sectpt:
BLOCK         WORD             ;sectors per track

blkshf:
BLOCK         BYTE             ;block shift factor

blkmsk:
BLOCK         BYTE             ;block mask

extmsk:
BLOCK         BYTE             ;extent mask

maxall:
BLOCK         WORD             ;maximum allocation number

dirmax:
BLOCK         WORD             ;largest directory number

dirblk:
BLOCK         WORD             ;reserved allocation bits for directory

chksiz:
BLOCK         WORD             ;size of checksum vector

offset:
BLOCK         WORD             ;offset tracks at beginning
EQU           DPBLIST,$-SECTPT ;size of area
                               ;	local variables

tranv:
BLOCK         WORD             ;address of translate vector

fcb_copied:
BLOCK         BYTE             ;set true if copy_fcb called

rmf:
BLOCK         BYTE             ;read mode flag for open_reel

dirloc:
BLOCK         BYTE             ;directory flag in rename, etc.

seqio:
BLOCK         BYTE             ;1 if sequential i/o

linfo:
BLOCK         BYTE             ;low(info)

dminx:
BLOCK         BYTE             ;local for diskwrite

searchl:
BLOCK         BYTE             ;search length

searcha:
BLOCK         WORD             ;search address

tinfo:
BLOCK         WORD             ;temp for info in "make"

single:
BLOCK         BYTE             ;set true if single byte allocation map

resel:
BLOCK         BYTE             ;reselection flag

olddsk:
BLOCK         BYTE             ;disk on entry to bdos

fcbdsk:
BLOCK         BYTE             ;disk named in fcb

rcount:
BLOCK         BYTE             ;record count in current fcb

extval:
BLOCK         BYTE             ;extent number and extmsk

vrecord:
BLOCK         WORD             ;current virtual record

arecord:
BLOCK         WORD             ;current actual record

arecord1:
BLOCK         WORD             ;current actual block# * blkmsk
                               ;	local variables for directory access

dptr:
BLOCK         BYTE             ;directory pointer 0,1,2,3

dcnt:
BLOCK         WORD             ;directory counter 0,1,...,dirmax

drec:
BLOCK         WORD             ;directory record 0,1,...,dirmax/4
                               ;next module

;**************************************************************
;*
;*        B I O S   J U M P   T A B L E
;*
;**************************************************************
; I really wish I knew why this bios has a -3 jump... Would be good to figure that out. Might be due to CP/M 3???
; Jump table here - Define the BIOS entry points. 
.EQU		BIOS	,$FC03		; Location of BIOS. Though in reality, we have boot. Need to understand if BOOT=0 or BOOT=-3.
.EQU		BIOSF	,$FC03		; Location of BIOS. Though in reality, we have boot. Need to understand if BOOT=0 or BOOT=-3.
.EQU		BOOTF	,BIOS-3		;-3: Cold Boot - Set up system. 
.EQU		WBOOTF	,$D003		; Back to CCP WARM BOOT entry. 
.EQU		CONSTF	,BIOS+3		; 3: Console status - A=0 No character ready, A=FF character waiting to be read.
.EQU 		CONINF	,BIOS+6		; 6: Console input - Wait until character then A=character.
.EQU		CONOUTF	,BIOS+9		; 9: Console output - Write C register  to screen.
.EQU		LISTF	,BIOS+12	;12: Printer output - Wait until ready, then write C register to printer.
.EQU		PUNCHF	,BIOS+15	;15: Paper tape punch output - Wait until ready then write C to Punch Reader ( or AUX ). 
.EQU		READERF	,BIOS+18	;18: Paper tape reader input - Wait until ready, then return A=Character. If not implemented, return 26 ( Ctrl Z )
.EQU		HOMEF	,BIOS+21	;21: Move disc head to track 0
.EQU		SELDSKF	,BIOS+24	;24: Select disc drive	- C Register = disk 0...F. Enter with E=0 or E=FF. 
				; If bit 0 of E is 0, then the disc is logged in as if new; if the format has to be determined from the boot sector, for example, this will be done.
				;If bit 0 if E is 1, then the disc has been logged in before. The disc is not accessed; the DPH address (or zero) is returned immediately.
				;SELDSK returns the address of a Disc Parameter Header in HL. The exact format of a DPH varies between CP/M versions; note that under CP/M 3, 
				;	the DPH is in memory bank 0 and probably not visible to programs. If the disc could not be selected it returns HL=0.
.EQU		SETTRKF	,BIOS+27	;27: Set track number - Track in BC - Is a word - Starts at 0. 
.EQU		SETSECF	,BIOS+30	;30: Set sector number - Sector in BC - Is a word. Starts at 1. I think. Will find out. 
.EQU		SETDMAF	,BIOS+33	;33: Set DMA address - Set DMA address. 
.EQU		READF	,BIOS+36	;36: Read a sector - To DMA address. A=0 success. A=1 unrecoverable error. A=FF Media changed.
.EQU		WRITEF	,BIOS+39	;39: Write a sector - C contains blocking code.  0=deferred. 1=immediate. 2=Can be deferred. No preread necessary. A=0 success. 
				;	A=1 unrecoverable error. A=FF Media changed.
;In CP/M 2 and later, the following extra jumps appear:
.EQU		LISTSTF	,BIOS+42	;42: Status of list device - A=0 Printer not ready. A=FF -printer ready. 
.EQU		SECTRANF	,BIOS+45	;45: Sector translation for skewing DE=Tableaddress (IGNORE DE). BC=Incoming sector (logical). HL=Translated sector (physical) . Return BC. 
.EQU		SECTRNF	,BIOS+45	;45: Sector translation for skewing DE=Tableaddress (IGNORE DE). BC=Incoming sector (logical). HL=Translated sector (physical) . Return BC. 

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



 





.END