; IP Networking Module, LOKI - Simple IP stack using 20 byte headers. Unreliable comms. 
; Uses SLIP for transmission over serial port.
; Can form IP headers
; Supports simple protocols for network drive access and serial comms to another system. 
; 
; Conventions. 
; 1. MTU and MRU is 256 bytes AT THE IP LEVEL. 



IPHEADER:	; NOTE it's BIG ENDIAN. 
; 32 bits
DB	$45		; Version and IHL. ( packet header is 20 bytes )
DB	$00		; TOS is 0
DW	$0000	; Big endian packet length.

; 32 bits
DB	$00,$00	;	Identification. Update for each packet or frame - eg, frame counter.
DB	%010 00000, %00000000	; Zero, Don't Fragment, Last Fragment

; 32 bits
DB	$20		; 32 hops TTL.
DB	$11		; UDP?
DB	$00,$00	; Checksum.

; 32 bits
DB	192, 168, 0, 1		; Source Range default.

; 32 bits
DB	203, 13, 16, 30		; Destination Range default. "Loki Network".



EQU	VERSION	,	$00		; $45 Version 4, IHL 5 (IHL5 is 20 bytes for header )
EQU TOS		,	$01		; $00 Type of service (eg, IP Precedence) 
EQU LENGTH	,	$02		; Big Endian 2 bytes.
EQU ID		, 	$04		; Identification. Big endian, but doesn't really matter. 
EQU	FLAGS	,	$06		; Fragmentation. 
EQU TTL		,	$08
EQU	PROTOCOL,	$09
EQU CHECKSUM,	$0A
EQU	SOURCE	,	$0C
EQU	DEST	,	$10
;EQU	OPTIONS , $14			; No options, since we will use header length 5. 




