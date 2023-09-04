rem Shitty z80 emulator to test code.
rem Update 15/11/2022 - Fixed IX+DISP and added DD routines. 
rem BUG - Get displacement to accept Negative Numbers.  - Fixed 15/11/2022
rem Potential Bug - Adjusted to ONLY pick up code in the range 0 to 65535 and wrap around. 	
rem 20/11/2022	- Bug - HL flags not correctly set on 16 bit arithmetic. Manually adjusted for ADC and SBC since they didn't work. 
rem 20/11/2022 - Fixed DJNZ not changing B register due to bake/unbake missing. 
rem 26/11/2022 - MAJOR BUG - LD DE,(XX) and LD HL,(XX) did not work due to BakeBC instead of BakeHL and BakeDE
rem				- Added in decoding and disassembly for After ED commands. 
rem 27/11/2022	- Updated DISK image in Drive L: ($0B) to reflect an Amstrad DSK image 8 sect/track 32K disk. (simulated 27256)
rem 28/11/2022  - LD IX,(nn) and LD (nn),IX acted on HL instead of IX. And IY
rem 2/12/2022	- OMFG, How did I miss this? Several arithmetic shift routines were returning 0 instead of the result. Changed to Return Value
rem             - Corrected disassembly opcode ld l,x was showing as ld e,x
rem 3/12/2022	- Added feature FIXED and (HL) to monitor memory locations in debug.  Might make this a text string later, or just hex numbers. Or both. 
rem 4/12/2022 	- OK, this one caught me out. CCF...I'm spending too much time on MCS51, because I assumed clear not complement... Failed on real z80. Eventually figured it out. 
rem 5/12/2022	- Realized I was forgetting to BAKEAF after changing a FLAG BIT. 
rem 5/12/2022	- JP cc,XX was failing - Wasn't reading in jump location if flag not set, and so kept on executing the jump location as instructions. 
rem 5/12/2022	- BitX function - Reversed Power and Value by mistake as arguments. 
rem 5/12/2022 	- BitX function - Reversed Polarity of result... I really messed this one up. 
rem 6/12/2022###- Need to check the maths in the ALU routines for both HALF and PV against a real Z-80 and read the ZILOG manual ##### SEEMS OK NOW #####
rem								' z80 status indicator flags ' - Page: 65-69 and check that I have it right. Other flags seem OK. 
rem	10/12/2022	- SBC HL,DE faulty - Sign and wrong, Carry wrong. Rewrote as an ALU routine. Seems to work now. Fixed. . 
rem 11/12/2022	- Realized OR, XOR and AND set PV to Parity - Updated to reflect Parity. 
rem 19/12/2022  - Updated sector write - Forgot to ADD 1 for the difference in counting from 0 in arrays.
rem 19/12/2022  - CPIR/CPDR not terminating on zero flag. (compare matched) fixed
rem 19/12/2022  - PROBLEM with CP - Is parity overflow or is it parity? Need to check. Inverted parity on CPI, LDI etc. INI is real parity
rem 20/12/2022	- Added UnbakeHL to LD	(SP),HL 
rem 20/12/2022  - Fixed decimal values in CASE statements, back to HEX.
rem 20/12/2022	- ADC issues. Fixed - Carry=0 for ,B functions. ADC, SBC, AND, OR XOR... Fixed. 
rem 20/12/2022  - Fixed Xor X showing as ADC X in disassembly 
rem 20/12/2022	- Arithmetic for 8 bit A is wrong. Need to fix. Found issue - had to remove MOD from 2's compliment additor.  Still half carry issues. 
rem 20/12/2022	- Fixed half wrong for XOR.
rem 20/12/2022  - Fixed NEG - ran through the ALU. 
rem 20/12/2022  - Major rework of ALU functions to get carry and half correct. And had to add in carry to next carry calculations.  Seems OK now. 
rem 20/12/2022	- SCF should RESET N1. 
rem 21/12/2022	- DAA fixed... Seems the tables are completely wrong. Had to rewrite it from scratch. 
rem 22/12/2022  - CP 0 returns CARRY when it should return NC.  Running CP through ALUSUB now - Working. 
rem 22/12/2022  - Set missing bits in bit test. 
rem 23/12/2022	- Fixed wrong bit settings for B.bits 2 and 3 ( were 4 and 5 ) Affected RES SET and BIT
rem 23/12/2022  - Fixed reset and set bit functions (didn't work )
rem 23/12/2022	- LD H,C and LD L,C worked with B instead 
rem 23/12/2022	- Fixed IX IY Bit instructions. Still not quite working though. 
rem 23/12/2022	- Added ALU and INC/DEC operation for undocumented IXh,IYh,IXl,IYl
rem 23/12/2022  - Debugged ALU ops for IXh,IYh,IXl,IYl

rem 21/02/2023  - Writing now for LOKI - Adding hardware constraints, MMU, Disk Paging etc. 
rem 23/02/2023	- Fixed OUT (C),E instruction, which was performing OUT(C),C instead. Also updated z80F.bas

rem 27/03/2023  - Separated DEBUG from console - Open a file (loki.dbg) for output when selected. 
rem 30/03/2023  - Add limited ANSI/VT52 codes. 
rem               Codes now moved to screen, so not relevant to LOKI2. Only in LOKI1 and now ADM-3A. 
rem 31/05/2023  - Overflow from PC=FFFF to 0000 was 10000 then a sudden push would save a value in the stack > FFFF, which caused an error. Fixed. 

rem ------------------------------------------------
rem Stuff I have to fix yet. And bugs I found. 
rem ------------------------------------------------










rem DON'T FORGET TO MOVE CHANGES to LOKI.BAS - Or Merge both with screen options later. 

rem -------------------------------------------------
rem Lets get it started.
rem -------------------------------------------------

rem Let's set the screen emulation and pointers.
screenres 512, 384, 4					: rem 4 bits per colour BGRI 512 x 384 - Screen can map this as x 192 with double vertical pixels.
rem screenres 512, 1000, 4	
dim shared as ubyte ptr vram	
	rem For the moment, set these the same. Later target=screenPtr+(offset) and *target=4 pixels, so write 4 locations per pixel. Or 1 pixels if 256x192. 

rem z80 registers.
dim shared AF as integer		: rem for use when moving 16 bits.
dim shared HL as integer
dim shared BC as integer
dim shared DE as integer
dim shared A as integer
dim shared AF1 as integer	
dim shared HL1 as integer
dim shared BC1 as integer
dim shared DE1 as integer
DIM shared A1 as integer
dim shared PC as integer
dim shared TEMPPC as integer	: rem When we need PC to get the values to put into PC in two operations. 
dim shared STACK as integer
dim shared IO as integer		: rem IO address (16 bites) for any IO operations. 
dim shared F as integer
dim shared s as integer
dim shared z as integer
dim shared half as integer
dim shared pv as integer
dim shared prepv as integer : rem PV requires to know whether the operation resulted in an overflow ( eg, Positive to Negative, Over 255 etc ). 
dim shared n as integer
dim shared carry as integer
dim shared tempcarry as integer	: rem sometimes we need to track carry before and after. 
dim shared IX as integer
dim shared IY as integer
dim shared I as integer
dim shared R as integer
dim shared IFF1 as integer
dim shared IFF2 as integer
dim shared b as integer
dim shared c as integer
dim shared d as integer
dim shared e as integer
dim shared h as integer
dim shared l as integer
dim shared b1 as integer
dim shared c1 as integer
dim shared d1 as integer
dim shared e1 as integer
dim shared h1 as integer
dim shared l1 as integer
dim shared f5 as integer	: rem not fully supported.
dim shared f3 as integer	: rem not fully supported. 

dim shared t as integer	: rem Temp integer. s

dim shared temphalf as integer : rem REMOVE THIS 
dim shared vector as integer	: rem Because we need to read the vector anyway, whether we use it or not. 
dim shared count as integer		: rem Counter for looping. 
dim shared debug as integer		: rem If 1, then print debugging. 
dim shared lastcode as string	: rem For debugging, the last opcode executed as a string.

dim shared CODE(1048576) as integer	: rem Where we store code or instructions.
dim shared CODETEXT as string	: rem for converting CODE to STRING. 
dim shared MMUARRAY(2048) as integer	: rem MMU for each task.
dim shared PROCESS as integer	: rem Which process? This feeds the MMU value for memory.
dim shared LASTPROCESS as integer	: rem The last process ( that called the current process )
dim shared MEMSECTOR as integer	: rem When memory is accessed as disk.
dim shared MEMTRACK as integer 	: rem When memory is accessed as disk.
dim shared UPPERADDRESS as integer : rem What is the upper address for an OUT or IN operation. 

dim shared BYTES as string	: rem Bytes we will put into CODE. 

dim shared TRAP as integer	: rem When we need to trap specific events... When not 0, a trap has occured. 

dim shared currentop as string	: rem current operation bytes in string format as hex. 

rem Configuration Files
dim shared CLI as string : rem I will have CLI one day. 
dim shared MAXINSTRUCTIONS as integer : rem How many should we do before ending anyway?
dim shared showmemory as integer
dim shared showstart as integer
dim shared showend as integer
dim shared savememory as integer
dim shared savestart as integer
dim shared saveend as integer
dim shared savefile as string


dim shared CONNBUFFER as integer	: rem Console Buffer - If zero, then no character waiting. Set if we check console status. 
dim shared DRIVE as integer		
dim shared TRACK as integer
dim shared SECTOR as integer
dim shared DMALOW as integer
dim shared DMAHIGH as integer

rem Machine Specific Information. 
dim shared DISK as string	: rem Disk based on positional access. 
disk=string(8388608,&hE5) : rem Initialize Disk... 8Mb. 
rem disk2=string(8388608,&hE5) : rem Initialize Disk... 8Mb. 


dim shared LOADBIOS as string : rem  which file are we loading?
dim shared STARTBIOS as integer : rem Where to load the BIOS.
dim shared LOADBDOS as string : rem Let's load BDOS separately.
dim shared STARTBDOS as integer : rem Where to load the BDOS
dim shared LOADCCP as string : rem Let's keep the CCP out of the BDOS.
dim shared STARTCCP as integer : rem Where to load the CCP
dim shared LOADTPA as string : rem We can load in a program also - need to put in hooks to start executing though if it's not in the code. Maybe add to code. 
dim shared STARTTPA as integer : rem Where to load the TPA in case it's not &h100
dim shared START as integer : rem Where should we reset PC to ?

dim shared FIXED as integer : rem So we can watch a FIXED location when debugging... In case we get variable overrun, Stack failure etc. 
							: rem Just one should generally be enough, but I can change to a short sequence if necessary.
							
dim shared STARTBIT as integer : rem If not 0, then a reset has occured, and we want to lock out memory management and map in ROM and other bootstrap resources. 

rem ANSI variables
dim shared inescape as integer		: rem Are we in an escape sequence?
dim shared escsequence as integer	: rem which character are we at in escape?dim shared esclocation as integer	: rem Mode (eg, multicharacter operation, with other characters pending ).
dim shared escarg(10) as string			: rem Arguments ( up to 10 characters )



rem FUNCTIONS START HERE.


Function WRITEVRAM(Byref Address as Integer, Byref Value as Integer) as integer
dim result as integer
result=0	: rem should be null, but let's set it to 1 for a hit. 
dim REALADDRESS as integer
dim VRAMOFFSET as integer
dim VRAMOFFLOW as integer
dim VRAMOFFHIGH as integer



REALADDRESS=address : rem IT is a 0 based 20 bit number. 0 to 1Mbyte.


rem ONLY write video memory if the write is to video RAM within the C0000-CBFFF region.
if REALADDRESS >= &hC0000 and REALADDRESS < &hCBFFF then 

rem BELOW maths translates screen memory of 256 x 192 with TWO PIXELS PER BYTE (512x192 resolution) into address space for a 512x384 x 4bit pixel array
VRAMOFFSET=(REALADDRESS-&hC0000)*2
rem 	VRAM OFFSET is MULTIPLIED BY 2 because we're writing DOUBLE PIXELS at the moment for 192 vertical mode... 
rem - OK, that's only STARTING address... Now we need to shift every second line out = Bit 0-8 stay same. Bit 9-14 SHIFT UP ONE.
rem - Separate into two pixel groups and do the maths. 
VRAMOFFLOW=VRAMOFFSET MOD 512 : rem 256 bytes per line = 512 bytes if 4 "byte" pixels x 512. 
VRAMOFFHIGH=INT(VRAMOFFSET/512)	: rem Get higher value.
VRAMOFFSET=(1024*VRAMOFFHIGH)+VRAMOFFLOW	: rem FIX location. 

				screenLock 
			VRAM=screenPtr+VRAMOFFSET
			*VRAM=int(Value/16)	: rem Write upper nibble to video memory.
			VRAM=VRAM+512
			*VRAM=int(Value/16)	: rem Write upper nibble to video memory - double vertical pixels ( 192 pixels from 384 )
			VRAM=VRAM+1
			*VRAM=Value MOD 16 : rem write lower nibble ( next pixel )
			VRAM=VRAM-512
			*VRAM=Value MOD 16 : rem write lower nibble ( next pixel ) - Double vertical pixels. 
				screenunlock 

rem			locate 23,0,1
rem 			print "REALADDRESS:";hex(realaddress,5), "Offset:";hex(vramoffset,5),"Data:";hex(value,2)
if debug>0 then		print #9,"VRAM REALADDRESS:";hex(realaddress,5), "Offset:";hex(vramoffset,5),"Data:";hex(value,2)
rem waithere:
rem 		if inkey$="" then goto waithere

		endif

NOTVRAM:
return result
end function



Function MMU(Byref Address as Integer) as Integer
dim result as integer : rem The end address in 1Mb space.
dim page as integer	: rem Page we are addressing. 64K is divided up into 16 x 4K pages.
dim MMUAddress as integer : rem Where the page data is in the page RAM.
dim RealAddress as integer : rem The Real Address = Address -1... Remember to add one later. 

rem MMUARRAY(2048) is the MMU index for each task.
rem PROCESS is the offset into the MMU array * 16.

rem Generate a 1Mb address from a 64K address based on information stored in an array or in a flag.
rem 

result = ((Address) mod 4096) 	: rem Get the lower address
page=INT(Address / 4096) 		: rem Get the page.
MMUAddress=((Process AND &h1F)*16)+page	: rem Get the array address.

result=result+(MMUARRAY(MMUAddress+1)*4096)

if STARTBIT and page=0 then result=&HF0000+Address			: rem Alternative is if HARD RESET BIT is set, then FIX THE ADDRESS>



return result
End function


Function SETMMU(Byref portdata as integer, value as integer) as Integer
dim result as integer : rem The end address in 1Mb space.
dim page as integer	: rem Page we are addressing. 64K is divided up into 16 x 4K pages.
dim MMUAddress as integer : rem Where the page data is in the page RAM.

rem MMUARRAY(2048) is the MMU index for each task.
rem PROCESS is the offset into the MMU array * 16. <- This is set 
rem MEMSECTOR for When memory is accessed as disk.
rem MEMTRACK for When memory is accessed as disk.
rem UPPERADDRESS rem What is the upper address for an OUT or IN operation. Higher order bits of port.

rem Populate the MMU with the data in the out port.
rem Needs the PROCESS (PID) and PORT (upper 4 bits ) to define which page we're adjusting. 

UPPERADDRESS=INT (PORTDATA/4096)

page=(PORTDATA / 4096) mod 16		: rem Get the page. This is the lower 4 bits of the MMU address. 
MMUAddress=((Process AND &h1F)*16)+page	: rem Get the array address.

MMUARRAY(MMUAddress+1)=value			: rem Assign the value to the MMU - value is the upper 8 bits of a 20 bit address.

result=0
return result
End function


Function GETMMU(Byref portdata as integer) as Integer
dim result as integer : rem The end address in 1Mb space.
dim page as integer	: rem Page we are addressing. 64K is divided up into 16 x 4K pages.
dim MMUAddress as integer : rem Where the page data is in the page RAM.

rem MMUARRAY(2048) is the MMU index for each task.
rem PROCESS is the offset into the MMU array * 16. <- This is set 
rem MEMSECTOR for When memory is accessed as disk.
rem MEMTRACK for When memory is accessed as disk.
rem UPPERADDRESS rem What is the upper address for an OUT or IN operation. Higher order bits of port.

rem Populate the MMU with the data in the out port.
rem Needs the PROCESS (PID) and PORT (upper 4 bits ) to define which page we're adjusting. 

UPPERADDRESS=INT (PORTDATA/4096)

page=(PORTDATA / 4096) mod 16		: rem Get the page. This is the lower 4 bits of the MMU address. 
MMUAddress=((Process AND &h1F)*16)+page	: rem Get the array address.

result=MMUARRAY(MMUAddress+1)			: rem Return the MMU value as a result. ( we can read what page is set in the MMU ) 

return result
End function



Function LoadFile(ByRef filename As String) As String                                   : rem Load in a file from the hard disk. Used to load in DISK DSK files.

    Dim h As Integer
    Dim txt As String

    h = FreeFile

    If Open( filename For Binary Access Read As #h ) <> 0 Then Return ""

    If LOF(h) > 0 Then

        txt = String(LOF(h), 0)
        If Get( #h, ,txt ) <> 0 Then txt = ""

    endIf

    Close #h                                                                                   

    Return txt

End Function

Function BinaryLoadFile(ByRef filename As String, Byref FileLocation as integer) as integer                                  : rem Load in a file from the hard disk. Used to load in DISK DSK files.
rem Like Loadfile but loads a binary file into z80 simulated memory space.
rem Returns the file size maybe...  Don't really care.

    Dim h As Integer
    Dim txt As String
	Dim result as integer
	Dim count as integer

    h = FreeFile

    If Open( filename For Binary Access Read As #h ) <> 0 Then Return 0

    If LOF(h) > 0 Then

        txt = String(LOF(h), 0)
        If Get( #h, ,txt ) <> 0 Then txt = ""

    endIf

    Close #h     
	
	if len(txt)>0 then
		for count=1 to len(txt)
rem 			Print "Saving:";asc(mid(txt,count,1));" at:";hex(filelocation+count,4)
		
			CODE(filelocation+count)=asc(mid(txt,count,1))

		next count
		result=count : rem number of bytes written. 
		Print filename;" Loaded at ";FileLocation; " is";count;" bytes."
		else
		Print "Error: ";filename;" Not found or zero size."
	endif

    Return result

End Function


Function Casefix (Byref fname as string) as string
        dim cpos as integer
        dim result as string
        result=""
        for cpos = 1 to len(fname)
        if mid(fname,cpos,1) >= "a" and mid(fname,cpos,1) <="z" then
                result=result+chr((asc(mid(fname,cpos,1))-32) mod 128)
                else
                result=result+chr((asc(mid(fname,cpos,1))) mod 128)     : rem MOD 128 removes any hidden bits ( bit 7 )
                endif

        next cpos
        return result
End function

function parameter ( byref target as string, byref defaultpara as integer ) as integer
rem Works with what is in CLI - Examines for parameters without other factors in the form of AAA=NNNN with no spacing.
dim result as integer
dim localloop as integer
dim value as string     : rem where we collect the value.
dim hit as integer      : rem location in string.
dim CLEANCLI as string  : rem Cleaned casefix(ed) CLI

CLEANCLI=casefix(CLI)   : rem Clean up the case of the CLI.

        result=defaultpara  : rem Set the default value if we don't find it.
        value=""
        hit = instr(CLEANCLI,target+"=")

        if hit=0 then goto paraend2
        hit=hit+len(target)+1

paratop:
        if mid(CLEANCLI,hit,1) <"0" or mid(CLEANCLI,hit,1)>"9" then goto paraend
        value=value+mid(CLEANCLI,hit,1)
        hit=hit+1
        if hit <= len(CLEANCLI) then goto paratop

paraend:
        result=val(value)
paraend2:
        return result
end function

function parameterstring ( byref target as string, byref defaultpara as string ) as string
rem Works with what is in CLI - Examines for parameters without other factors in the form of AAA=NNNN with no spacing.
dim result as string
dim localloop as integer
dim value as string     : rem where we collect the value.
dim hit as integer      : rem location in string.
dim CLEANCLI as string  : rem Cleaned casefix(ed) CLI

CLEANCLI=casefix(CLI)   : rem Clean up the case of the CLI.

        result=defaultpara  : rem Set the default value if we don't find it.
        value=""
        hit = instr(CLEANCLI,target+"=")

        if hit=0 then goto paraend2
        hit=hit+len(target)+1

paratop:
        if mid(CLEANCLI,hit,1) <" " or mid(CLEANCLI,hit,1)>"z" then goto paraend
        value=value+mid(CLEANCLI,hit,1)
        hit=hit+1
        if hit <= len(CLEANCLI) then goto paratop

paraend:
        result=value
		print "Setting ";target;" to:";value
paraend2:
        return result
end function


Function Printhex (Byref image as string, Byref numhex as integer) as integer

        rem Image is the string you want to dump as HEX.
        rem numhex is the number of characters you want to display ( won't be less than 32 displayed... Might adjust that later. )

        dim a as integer
        dim b as integer
        dim c as integer

        print "         !           !           !           !           !           !           !           !           "

        for a=0 to int(((numhex-1)/32))

                print hex$(A*32,4);"  -  ";                             : rem Print relative location in Hex at start of line.

                for b=1 to 32
                        print hex$(asc(mid$(image,(a*32)+b,1)),2);" ";  : rem Print out the hex of the bytes.
                next b

                print "  -  ";

                for b=1 to 32                                                                   : rem Now print out the ASCII characters.
                        c=asc(mid$(image,(a*32)+b,1))
                        if c<32 then c=asc("+")
                        if c>127 then c=asc("+")
                        print chr$(c);
                next b

                print

        next a

        Return len (image)

End function

Function ConsolePrinthex (Byref image as string, Byref numhex as integer) as integer

        rem Image is the string you want to dump as HEX.
        rem numhex is the number of characters you want to display ( won't be less than 32 displayed... Might adjust that later. )

        dim a as integer
        dim b as integer
        dim c as integer

        print #9, "         !           !           !           !           !           !           !           !           "

        for a=0 to int(((numhex-1)/32))

                print #9, hex$(A*32,4);"  -  ";                             : rem Print relative location in Hex at start of line.

                for b=1 to 32
                        print #9, hex$(asc(mid$(image,(a*32)+b,1)),2);" ";  : rem Print out the hex of the bytes.
                next b

                print #9, "  -  ";

                for b=1 to 32                                                                   : rem Now print out the ASCII characters.
                        c=asc(mid$(image,(a*32)+b,1))
                        if c<32 then c=asc("+")
                        if c>127 then c=asc("+")
                        print #9, chr$(c);
                next b

                print #9, 

        next a

        Return len (image)

End function

Function DebugPrinthex (Byref memstart as integer, Byref numhex as integer) as integer

        rem Like Printhex above, but prints to DEBUG file. 256 bytes.
		rem memstart is the START of CPU memory (64k segment)
        rem numhex is the START of memory to debug. 

        dim a as integer
        dim b as integer
        dim c as integer

		print #9,
        print #9,"         !           !           !           !           !           !           !           !           "

        for a=int(memstart/32) to int(((memstart+numhex-1)/32))

                print #9,hex$(A*32,4);"  -  ";                             : rem Print relative location in Hex at start of line.

                for b=0 to 31
                        print #9,hex$(CODE(MMU((A*32)+B)+1),2);" ";  : rem Print out the hex of the bytes.
                next b

                print #9,"  -  ";

                for b=0 to 31                                                                   : rem Now print out the ASCII characters.
                        c=CODE(MMU((A*32)+B)+1)
                        if c<32 then c=asc("+")
                        if c>127 then c=asc("+")
                        print #9,chr$(c);
                next b

                print #9,

        next a

        Return 0

End function


Function savehex (Byref image as string, Byref numhex as integer) as integer

        rem Image is the string you want to dump as HEX.
        rem numhex is the number of characters you want to display ( won't be less than 32 displayed... Might adjust that later. )

        dim a as integer
        dim b as integer
        dim c as integer

if savefile="" then goto nosave

		Print "Saving File"


		open savefile for output as #1
		

        print #1,"         !           !           !           !           !           !           !           !           "

        for a=0 to int(((numhex-1)/32))

                print #1,hex$(A*32,4);"  -  ";                             : rem Print relative location in Hex at start of line.

                for b=1 to 32
                        print #1,hex$(asc(mid$(image,(a*32)+b,1)),2);" ";  : rem Print out the hex of the bytes.
                next b

                print #1,"  -  ";

                for b=1 to 32                                                                   : rem Now print out the ASCII characters.
                        c=asc(mid$(image,(a*32)+b,1))
                        if c<32 then c=asc("+")
                        if c>127 then c=asc("+")
                        print #1,chr$(c);
                next b

                print #1,

        next a
		
		close #1

nosave:
        Return len (image)
End function

Function LoadCode (Byref filename as String) As Integer
dim count as integer	: rem where in CODE are we?

	BYTES=Loadfile (Filename)
	
		Print "Length of file:";len(BYTES)
	
	for count=1 to len(BYTES)
		CODE(count)=ASC(MID(BYTES,count,1))
	next count
	

	return 0
End Function

Function LoadExec (Byref filename as String) As Integer
rem Load in a program to execute.
dim count as integer	: rem where in CODE are we?

	BYTES=Loadfile (Filename)
	
		Print "Length of file:";filename;"   is";len(BYTES)
	
	for count=257 to len(BYTES)+256
		CODE(count)=ASC(MID(BYTES,count,1))
	next count
	

	return 0
End Function


Function debugreg() as integer


	currentop=currentop+"                    "
	currentop=left(currentop,15)
	
	print #9,currentop;

	Print #9,tab(24); lastcode;
	
	Print #9,tab(40);
	Print #9,"EX:";hex(MMU(PC),5);" ";
	Print #9,"PC:";hex(PC,4);
	Print #9," : ";
	Print #9,"A:";hex(A,2);" ";
	Print #9,"BC:";hex(BC,4);" ";
	Print #9,"DE:";hex(DE,4);" ";
	Print #9,"HL:";hex(HL,4);" ";
	Print #9,"Stack:";hex(Stack,4);" ";
	
	Print #9,"Flags:";
	if s then print #9,"S"; else print #9,"-";
	if z then print #9,"Z"; else print #9,"-";
	print #9,"x";
	if half then print #9,"H"; else print #9,"-";
	print #9,"x";
	if PV then print #9,"P"; else print #9,"-";
	if n then print #9,"N"; else print #9,"-";
	if carry then print #9,"C"; else print #9,"-";
	print #9,"  ";

	Print #9,"IX:";hex(IX,4);" ";
	Print #9,"IY:";hex(IY,4);" ";	
	
print #9,
	
return 0
End Function


Function Higher (Byref Register as integer) as integer
rem Return the Higher Order byte.
dim result as integer
	result=int(Register/256)
	return result
End Function

Function Lower (Byref Register as integer) as integer
rem Return the Higher Order byte.
dim result as integer
	result=Register mod 256
	return result
End Function

Function SetHigher (Byref Register as integer, Byref Newregister as integer) as integer
rem Set the higher byte in a register with a new higher byte.
dim result as integer
	result=(register mod 256)*Newregister
	return result
End Function

Function SetLower (Byref Register as integer, Byref Newregister as integer) as integer
rem Set the lower Byte in a register with a new lower byte.
dim result as integer
	result=(int(Register/256)*256)+Newregister
	return result
End Function 



Function NextPC () as integer
rem Get the next instruction or data from PCs...
dim result as integer

		result=CODE(MMU(PC)+1)
		PC=PC+1
		PC=PC AND &HFFFF	: rem Mask in case we get an overflow. It can happen. 
		if debug>0 then currentop=currentop+hex(result,2)+" "

	return result
End function 


Function PeekPC () as integer
rem Peek at the next instruction or data from PCs... Don't update any counters. 
dim result as integer
		result=CODE(MMU(PC)+1)
	return result
End function 

Function FixDisp(Byref Offset as Integer) as integer
rem Fix the diplacement by making it negative when it's above 128 -
dim result as integer

If Offset > 127 then
	result=(256-Offset)*-1		: rem Make it a negative displacement. FF= 255 = -1.... FE = 254 = -2. etc.
	else
	result=Offset
	Endif

return result
End Function

Function fetch (Byref location as integer) as integer
rem Get the next instruction or data from PCs...
dim result as integer
dim maskedlocation as integer : rem Make sure it's int he range 0 to 65535

maskedlocation=location mod 65536

result=CODE(MMU(maskedlocation)+1)

return result
End function 

Function set (Byref location as integer, Byref value as integer) as integer
rem Get the next instruction or data from PCs...
dim result as integer

CODE(MMU(location)+1)=value
WRITEVRAM(MMU(location),value)	: rem Shadow Write the VRAM.

CODE(MMU(location)+1)=value
if location>65535 then 
	print
	print "Error: Writing 17+bit location address into 16 bit space. Emulator error..."
	print "Debugging information:"
	print "Current PC:";hex(PC,4)
	print "Value:";hex(value,4)
	print "Location:";hex(location,4)
	print
	trap=10		: rem HALT COMPUTER> 
	endif


return 0
End function 



Function Bit7 (Byref Acc as integer) as integer
rem Is Bit7 set? 
dim result as integer
	result=int (Acc/128) mod 2
return result
End function

Function Bittest (Byref Bitnum as integer, Byref Acc as integer) as integer
rem Is any bit set?
dim result as integer

	result=int (Acc/Bitnum) mod 2
	
return result
End function

Function BITX(Byref indice as integer,Byref value as integer) as integer
rem Returns outcome as a bit, 1 if set, 0 if not set. Sets H and N.
rem IF BIT IS ZERO, THEN ZERO FLAG IS SET... 
	dim result as integer
	dim power as integer
	power=2^indice
	
	result=(bittest(power, value)+1) mod 2
	half=1
	n=0
	
	rem Set Unknowns.
	if power=128 and result=0 then 
		S=1 
		else 
		S=0
		endif
	PV=result
	
return result
End Function


rem ##################################################################
rem   8 to 16 bit conversions ( we keep all registers in 8 and 16 )
rem ##################################################################



Function BakeAF () as integer
rem Store A and Flags in AF register 
rem Bit 	7 	6 	5 	4 	3 	2 	1 	0
rem Flag 	S 	Z 	F5 	H 	F3 	P/V N 	C

rem AF=(A*256)+(s*128)+(z*64)+(f5*32)+(half*16)+(f3*8)+(pv*4)+(n*2)+(carry)
AF=(A*256)+(s*128)+(z*64)+(half*16)+(pv*4)+(n*2)+(carry)

return 0
End Function

Function UnbakeAF () as integer

af=af AND 65535	: rem mask

A=higher(AF)
s=Bittest(128,AF)
z=bittest(64,AF)
						
half=bittest(16,AF)		: rem BIT 4 (weight 16)

pv=bittest(4,AF)		: rem BIT 2 (weight 4)
n=bittest(2,AF)			: rem BIT 1	(weight 2)
carry=bittest(1,AF)		: rem BIT 0 (weight 1)


return 0
End Function

Function BakeBC () as integer
rem Build BC register.
BC=(b*256)+c
return 0
End Function

Function UnbakeBC () as integer
rem Deconstruct BC register
b=higher(BC)
c=lower(BC)
return 0
End Function

Function BakeHL () as integer
rem Build HL register.
HL=(h*256)+l
return 0
End Function

Function UnbakeHL () as integer
rem Deconstruct BC register
h=higher(HL)
l=lower(HL)
return 0
End Function

Function BakeDE () as integer
rem Build DE register.
DE=(d*256)+e
return 0
End Function

Function UnbakeDE () as integer
rem Deconstruct DE register
d=higher(DE)
e=lower(DE)
return 0
End Function







rem ##################################################################
rem   Bitwise Functions
rem ##################################################################


Function Parity (Value as Integer) as Integer
rem Return a 1 if Even parity or 0 if Odd parity.
dim pbit as integer
dim count as integer
	pbit=1	: rem Until we get a bit, a value of 0 is Even parity.

	for count=1 to 8
		if value mod 2 = 1 then pbit=1-pbit	: rem Flip bit if we encounter a 1. 
		value=value SHR 1
	next count

	return pbit
End Function

Function RLC(Byref Value as Integer) as Integer
rem Rotate Left Circular with Carry.
		carry=bittest(128,value)
		value=((value shl 1) mod 256)+carry
		
		half=0
		n=0

		s=Bit7(value)
		if value=0 then z=1 else z=0
		
		pv=parity(Value)

		BakeAF()
		rem complete. 
return value
End Function

Function RRC(Byref Value as Integer) as Integer
		carry=bittest(1,value)
		value=value+(carry*256)
		value=((value shr 1) mod 256)
		
		half=0
		n=0

		s=Bit7(value)
		if value=0 then z=1 else z=0
		
		pv=parity(Value)

		BakeAF()		
return value
End Function

Function RL(Byref Value as Integer) as Integer
		Value=((value shl 1))+carry
		carry=bittest(256,value)
		value=value mod 256		: rem Clean up the overflow. 
		
		half=0
		n=0

		s=Bit7(value)
		if value=0 then z=1 else z=0
		
		pv=parity(Value)

		BakeAF()	
return value
End Function

Function RR(Byref value as Integer) as integer
		tempcarry=bittest(1,value)
		value=value+(carry*256)
		value=((value shr 1) mod 256)
		carry=tempcarry		: rem get the missing (lost) bit. 
		
		half=0
		n=0

		s=Bit7(value)
		if value=0 then z=1 else z=0
		
		pv=parity(Value)

		BakeAF()	
return value
End function 

Function SLA(Byref Value as Integer) as Integer
		Value=((value shl 1) AND &h1FF)
		carry=bittest(256,value)
		value=value AND &hFF		: rem Clean up the overflow. 
		
		half=0
		n=0

		s=Bit7(value)
		if value=0 then z=1 else z=0
		
		pv=parity(Value)

		BakeAF()	
return value
End Function

Function SRA(Byref value as Integer) as integer
		tempcarry=bittest(1,value)
		carry=bittest(128,value)				: rem Repeat Bit 7...  Ignore prior carry value. 
		value=value+(carry*256)
		value=((value shr 1) and &hFF)
		carry=tempcarry		: rem get the missing (lost) bit. 
		
		half=0
		n=0

		s=Bit7(value)
		if value=0 then z=1 else z=0
		
		pv=parity(Value)

		BakeAF()	
return value
End function 

Function SLL_OLD(Byref Value as Integer) as Integer
rem Seems same as SLA - and not listed in z80 manual.
		Value=((value shl 1))
		carry=bittest(256,value)
		value=value mod 256		: rem Clean up the overflow. 
		
		half=0
		n=0

		s=Bit7(value)
		if value=0 then z=1 else z=0
		
		pv=parity(Value)

		BakeAF()	
return value
End Function

Function SLL(Byref Value as Integer) as Integer
rem Seems same as SLA - and not listed in z80 manual.
		Value=((value shl 1))
		carry=bittest(256,value)
		value=value and &hFE		: rem Clean up the overflow and incoming bit.  
		value=value OR &h01			: rem seems we need to set Bit. 

		half=0
		n=0

		s=BITTEST(128,value)
		if value=0 then z=1 else z=0
		
		pv=parity(Value)

rem PRINT "SLL##############################" 

		BakeAF()	
return value
End Function

Function SRL(Byref value as Integer) as integer
		tempcarry=bittest(1,value)
		value=((value shr 1) mod 256)
		carry=tempcarry		: rem get the missing (lost) bit. 
		
		half=0
		n=0

		s=0
		if value=0 then z=1 else z=0
		
		pv=parity(Value)

		BakeAF()	
return value
End function 

Function SetBit(Byref bitid as integer, Byref Value as integer) as integer
rem Return Value with the bit set.
dim result as integer
dim power as integer
dim mask as integer

		power=2^bitid
		mask=power AND &hFF

		value=value OR mask
		value=value AND &hFF
	
		result=value


return result
end function

Function ResetBit(Byref Bitid as integer,Byref Value as integer) as integer
rem Return Value with the bit reset.
dim result as integer
dim power as integer
dim mask as integer

		power=2^bitid
		mask = power XOR &hFF

		value=value AND mask
		value=value AND &hFF
	
		result=value


return result
end function






Rem Decimal Function Here. 

Function DAA() as integer
rem Decimal Adjust A... Need to commonize this with RRD and RLD .
dim alta as integer		: rem Alternative A...
dim precarry as integer : rem Get the incoming and outgoing carry separate. 
dim prehalf as integer	: rem debug


precarry=carry
prehalf=half
Half=0
Carry=0				: rem preset the outgoing carry state, and record it's incoming state. 

rem ALTA calcs.
Alta=A
	if n=0 then
		if (AltA and &h0F) > 9 or Prehalf = 1 then AltA=AltA+&H06 : half=1 : rem Means we need to add 6 
		if AltA > 255 or AltA > &h99 or Precarry=1 then AltA=AltA+&h60 : Carry=1 : rem If we're already over 255 then it's like an upper-half-carry. We need to correct it.
	else
		if Prehalf=1 then AltA=AltA-&h06 : rem half=1 : rem lower
		if AltA < 0 or Precarry=1 then AltA=AltA-&h60 : carry=1 : rem upper
	endif
AltA=AltA and &hFF	: rem Mask the result to 8 bits. 			

rem if debug=3 then		print #9, "DAA  a:";hex(a,2),"High:";hex(uhalfa,2),"Low:";hex(lhalfa,2),DD,hex(adda,2),hex(a+adda,2),precarry,carry,half, hex(alta,2)

rem		half=0

		s=Bit7(AltA)

		if AltA=0 then z=1 else z=0
		
		pv=parity(alta)

		BakeAF()
		rem complete. 
return ALtA
End Function

rem STILL TO DO!!!

Function RRD() as integer
rem Rotate Right Decimal ( /10 ).
dim uhalfa as integer
dim lhalfa as integer
dim uhalfhl as integer
dim lhalfhl as integer
dim value as integer


		rem (hl) upper > (hl) Lower, (hl) lower to a lower. A lower to (HL) upper.
		
		uhalfa=A AND &hF0
		lhalfa=A AND &h0F
		value=fetch(hl)
		uhalfhl=value AND &hF0
		lhalfhl=value AND &h0F
		
		a=uhalfa+lhalfhl
		value=(lhalfa*16)+(uhalfhl/16)
		
		set(hl,value)		: rem and save the value. 
	
		half=0
		n=0

		s=Bit7(a)
		if a=0 then z=1 else z=0
		
		pv=parity(a)

		rem Carry is unaffected. 

		BakeAF()
		rem complete. 
return 0
End Function

Function RLD() as integer
rem RLD ( * 10 )
dim uhalfa as integer
dim lhalfa as integer
dim nibble as integer
dim value as integer

		rem (hl) upper > a, (hl) lower to a (HL) upper. A lower to HL lower. 
		
		uhalfa=int(a/16)*16
		lhalfa=a mod 16
		value=fetch(hl)
		nibble=int(value/16)
		value=(value*16) mod 256
		value=value+lhalfa
		a=uhalfa+nibble
		
		set(hl,value)		: rem and save the value. 
	
		half=0
		n=0

		s=Bit7(a)
		if a=0 then z=1 else z=0
		
		pv=parity(a)

		rem Carry is unaffected. 

		BakeAF()
		rem complete. 
return 0
End Function





rem ##################################################################
rem   ALU INTERACTIONS>
rem ##################################################################




Function ALUADD(Byref Register as integer, Byref Additor as integer) as integer
rem WAS ALUADD8
rem Set flags from ALU operation and perform an ADDITION of two registers, and return the result of the addition. 
dim result as integer	: rem 16 bit result.
dim Sregister as integer : rem Signed integer
dim SAdditor as integer : rem Signed integer
dim Sresult as integer : rem Signed result as integer.
dim sign1 as integer 
dim sign2 as integer
dim prehalf as integer

dim C4 as integer
dim C8 as integer

		rem Calculate the half and full carries as unsigned integers
		C4=(register and &hF)+(((Additor and &hF)+Carry) and &h1F)
		C8=(register and &hFF)+(((Additor and &hFF)+Carry) and &h1FF)		

		rem Convert to signed integers.
		SRegister=Register
		SAdditor=Additor
		If Sregister>127 then Sregister=-128+(Sregister-128)
		If SAdditor>127 then SAdditor=-128+(SAdditor-128)

		rem Perform the main calculation. 
		Result=SRegister+SAdditor+Carry
		

		rem Understand the sign before and after and make zero positive. 
		sign1=sgn(SRegister)+1 : if sign1=2 then sign1=1
		sign2=sgn(Result)+1 : if sign2=2 then sign2=1
		
 
		n=0	: rem N=1 is Addition. 
	
		if C8>255 then carry=1 else carry=0 : rem add version
		
		PV=0
		If result < -128 then PV=1
		If result > 127 then PV=1
		
rem Convert result back tp 16 bit unsigned integer 
		Result=Result AND &hFF			: rem mask for 16 bits positive binary integer. 
	

		if C4>15 then half=1	 else half=0 : rem add version. 
		
		s=bittest(128,Result)
		
		if Result=0 then 
			z=1
			else 
			z=0		: rem Set zero flag if necessary. 
			endif

		BAKEAF()			: rem Set flags into AF	
rem 		print "add",
	return result
End Function




Function ALUADD16(Byref Register as integer, Byref Additor as integer) as integer
rem Set flags from ALU operation and perform an ADDITION of two registers, and return the result of the addition. 
dim result as integer	: rem 16 bit result.
dim Sregister as integer : rem Signed integer
dim SAdditor as integer : rem Signed integer
dim Sresult as integer : rem Signed result as integer.
dim sign1 as integer 
dim sign2 as integer
dim prehalf as integer

dim c12 as integer
dim c16 as integer

		rem Calculate the half and full carries as unsigned integers
		c12=(register and &hFFF)+(((Additor and &hFFF)+Carry) and &h1FFF)
		c16=(register and &hFFFF)+(((Additor and &hFFFF)+Carry) and &h1FFFF)		

		rem Convert to signed integers.
		SRegister=Register
		SAdditor=Additor
		If Sregister>32767 then Sregister=-32768+(Sregister-32768)
		If SAdditor>32767 then SAdditor=-32768+(SAdditor-32768)

		rem Perform the main calculation. 
		Result=SRegister+SAdditor+Carry
		
rem			print "ADD:";hex(register,4),hex(Additor,4),hex(Result,4), hex(result AND 65535,4),hex(additor and &hFFF,4),hex((((Additor mod &hFFF)+Carry) and &h1FFF),4)
		
		rem Understand the sign before and after and make zero positive. 
		sign1=sgn(SRegister)+1 : if sign1=2 then sign1=1
		sign2=sgn(Result)+1 : if sign2=2 then sign2=1
		
 
		n=0	: rem N=1 is Addition. 
	

		if c16>65535 then carry=1 else carry=0 : rem add version.
rem		if c16>65535 then carry=0	 else carry=1 : rem subtract version. 

		
		PV=0
		If result < -32768 then PV=1
		If result > 32767 then PV=1
		
rem Convert result back tp 16 bit unsigned integer 
		Result=Result AND &hFFFF			: rem mask for 16 bits positive binary integer. 
	

		if c12>4095 then half=1	 else half=0 : rem add version. 
rem		if c12>4095 then half=0	 else half=1 : rem subtract version. 
		
		s=bittest(32768,Result)
		
		if Result=0 then 
			z=1
			else 
			z=0		: rem Set zero flag if necessary. 
			endif

		BAKEAF()			: rem Set flags into AF	
	
	return result
rem 		print "add16",
End Function



Function ALUINC(Byref Accumulator as integer) as integer
rem Set flags from ALU operation and perform an ADDITION of 1 to the specified register. Don't affect or consider the carry. 
dim result as integer	: rem 8 bit result.
dim result7 as integer	: rem 7 bit result.
dim result4 as integer	: rem 4 bit result.
	
	rem Perform calculations for all ADDs we need to know. Included carry ( Clear carry before if not needed - will be set out of this routine. )
	result=(Accumulator + 1)							: rem Perform the 8 bit addition. Make sure we zero carry for non-carry Add's prior to calculation.
	result4=(Accumulator mod 16)+1						: rem 4 bit operation.


	rem Sign Bit.
	s=Bit7(result)								: rem Set Sign bit ( reflects Bit7 of the result )
	
	rem Parity/Overflow.
	if accumulator=127 then pv=1 else pv=0
	
	rem Half Carry.
	if (accumulator mod 16) = 15 then half=1 else half=0

	rem Clean up any overflow here. 
	result=result mod 256	: rem Clean up any overflow. 

	rem N ( Add or Subtract )
	n=0						: rem Should be 1 for a subtract. 

	rem Zero
	if result=0 then z=1 else z=0				: rem do this with result, since for carry, Accumulator might be 256. 
	
	BAKEAF()		: rem Set flags into AF
	
	return result
End Function


Function ALUSUB(Byref Register as integer, Byref Subtractor as integer) as integer
rem was ALUSUB8
rem Set flags from ALU operation and perform an ADDITION of two registers, and return the result of the addition. 
dim result as integer	: rem 16 bit result.
dim Sregister as integer : rem Signed integer
dim SSubtractor as integer : rem Signed integer
dim Sresult as integer : rem Signed result as integer.
dim sign1 as integer 
dim sign2 as integer
dim prehalf as integer

dim R4 as integer
dim S4 as integer

		S4=Subtractor AND &hF
		R4=Register AND &hF

		SRegister=Register
		SSubtractor=Subtractor
		If Sregister>127 then Sregister=-128+(Sregister-128)
		If SSubtractor>127 then SSubtractor=-128+(SSubtractor-128)

		Result=SRegister-SSubtractor-Carry

rem		print "SUB:";hex(register,4),hex(subtractor,4),carry
		
		sign1=sgn(SRegister)+1 : if sign1=2 then sign1=1
		sign2=sgn(Result)+1 : if sign2=2 then sign2=1
		
rem N 
		n=1	: rem Subtract

rem HALF
		if S4+carry>R4 then Half=1 else Half=0
	
rem Carry
		if Subtractor+carry>Register then Carry=1 else Carry=0

rem PV(overflow)		
		PV=0
		If result < -128 then PV=1
		If result > 127 then PV=1
		
rem Convert result back to a non-signed 16 bit integer.
		Result=Result AND &hFF			: rem mask for 8 bits positive binary integer. 



rem Sign		
		s=bittest(128,Result)
		
rem Zero
		if Result=0 then 
			z=1
			else 
			z=0		: rem Set zero flag if necessary. 
			endif

		BAKEAF()			: rem Set flags into AF	
rem 	print "sub",
	return result
End Function



Function ALUSUB16(Byref Register as integer, Byref Subtractor as integer) as integer
rem Set flags from ALU operation and perform an ADDITION of two registers, and return the result of the addition. 
dim result as integer	: rem 16 bit result.
dim Sregister as integer : rem Signed integer
dim SSubtractor as integer : rem Signed integer
dim Sresult as integer : rem Signed result as integer.
dim sign1 as integer 
dim sign2 as integer
dim prehalf as integer

dim R12 as integer
dim S12 as integer

		S12=Subtractor AND &hFFF
		R12=Register AND &hFFF

		SRegister=Register
		SSubtractor=Subtractor
		If Sregister>32767 then Sregister=-32768+(Sregister-32768)
		If SSubtractor>32767 then SSubtractor=-32768+(SSubtractor-32768)

		Result=SRegister-SSubtractor-Carry

rem		print "SUB:";hex(register,4),hex(subtractor,4),carry
		
		sign1=sgn(SRegister)+1 : if sign1=2 then sign1=1
		sign2=sgn(Result)+1 : if sign2=2 then sign2=1
		
rem N 
		n=1	: rem Subtract

rem HALF
		if S12+carry>R12 then Half=1 else Half=0
	
rem Carry
		if Subtractor+carry>Register then Carry=1 else Carry=0

rem PV(overflow)		
		PV=0
		If result < -32768 then PV=1
		If result > 32767 then PV=1
		
rem Convert result back to a non-signed 16 bit integer.
		Result=Result AND &hFFFF			: rem mask for 16 bits positive binary integer. 



rem Sign		
		s=bittest(32768,Result)
		
rem Zero
		if Result=0 then 
			z=1
			else 
			z=0		: rem Set zero flag if necessary. 
			endif

		BAKEAF()			: rem Set flags into AF	
rem 		print "sub16",
	return result
End Function



Function ALUDEC(Byref Accumulator as integer) as integer
rem Set flags from ALU operation and perform an ADDITION of 1 to the specified register. Don't affect or consider the carry. 
dim result as integer	: rem 8 bit result.
dim result7 as integer	: rem 7 bit result.
	
	rem Perform calculations for all ADDs we need to know. Included carry ( Clear carry before if not needed - will be set out of this routine. )
	result=(Accumulator + 255)						: rem Perform the 8 bit addition. Make sure we zero carry for non-carry Add's prior to calculation.

	rem Sign Bit.
	s=Bit7(result)								: rem Set Sign bit ( reflects Bit7 of the result )
	
	rem Parity/Overflow.
	if accumulator=&h080 then pv=1 else pv=0
	
	rem Half Carry.
	if (accumulator mod 16) = 0 then half=1 else half=0

	rem Clean up any overflow here. 
	result=result mod 256	: rem Clean up any overflow. 

	rem N ( Add or Subtract )
	n=1						: rem Should be 1 for a subtract. DEC is subtract. 

	rem Zero
	if result=0 then z=1 else z=0				: rem do this with result, since for carry, Accumulator might be 256. 
	
	BAKEAF()			: rem Set flags into AF
	
	return result
End Function

Function ALUCP(Byref Accumulator as integer, Byref Subtractor as integer) as integer
rem Set flags from ALU operation and perform an ADDITION of two registers, and return the result of the addition. 
	
dim nullvalue as integer
rem Perform a carryless subtract and discard the result. 

		carry=0									: rem Clear the carry going in. 
		nullvalue=alusub(Accumulator,Subtractor)
	
	BAKEAF()			: rem Set flags into AF
	
	return Accumulator							: rem Leave the flags set, but return the original value. Not the calculated value. 
End Function

Function ALUAND(Byref Accumulator as integer, Byref Additor as integer) as integer
rem Set flags from ALU operation and perform an ADDITION of two registers, and return the result of the addition. 
dim result as integer	: rem 8 bit result.
dim result7 as integer	: rem 7 bit result.
dim result4 as integer	: rem 4 bit result.
dim temppv as integer 	: rem Parity/Overflow calculation 
	
	rem Perform calculations for all ADDs we need to know. Included carry ( Clear carry before if not needed - will be set out of this routine. )
	result=(Accumulator AND Additor )					: rem Perform the 8 bit addition. Make sure we zero carry for non-carry Add's prior to calculation.

	rem Sign Bit.
	s=Bit7(result)								: rem Set Sign bit ( reflects Bit7 of the result )
	
	pv=Parity(result)										: rem Parity of the result. 
	carry=0
	half=1
	n=0
	
	rem Zero
	if result=0 then z=1 else z=0				: rem do this with result, since for carry, Accumulator might be 256. 
	
	BAKEAF()			: rem Set flags into AF
	
	return result
End Function

Function ALUOR(Byref Accumulator as integer, Byref Additor as integer) as integer
rem Set flags from ALU operation and perform an ADDITION of two registers, and return the result of the addition. 
dim result as integer	: rem 8 bit result.
dim result7 as integer	: rem 7 bit result.
dim result4 as integer	: rem 4 bit result.
dim temppv as integer 	: rem Parity/Overflow calculation 
	
	rem Perform calculations for all ADDs we need to know. Included carry ( Clear carry before if not needed - will be set out of this routine. )
	result=(Accumulator OR Additor )					: rem Perform the 8 bit addition. Make sure we zero carry for non-carry Add's prior to calculation.

	rem Sign Bit.
	s=Bit7(result)								: rem Set Sign bit ( reflects Bit7 of the result )
	
	pv=Parity(result)							: rem Parity of the result
	carry=0
	half=0
	n=0
	
	rem Zero
	if result=0 then z=1 else z=0				: rem do this with result, since for carry, Accumulator might be 256. 
	
	BAKEAF()			: rem Set flags into AF
	
	return result
End Function

Function ALUXOR(Byref Accumulator as integer, Byref Additor as integer) as integer
rem Set flags from ALU operation and perform an ADDITION of two registers, and return the result of the addition. 
dim result as integer	: rem 8 bit result.
dim result7 as integer	: rem 7 bit result.
dim result4 as integer	: rem 4 bit result.
dim temppv as integer 	: rem Parity/Overflow calculation 
	
	rem Perform calculations for all ADDs we need to know. Included carry ( Clear carry before if not needed - will be set out of this routine. )
	result=(Accumulator XOR Additor )					: rem Perform the 8 bit addition. Make sure we zero carry for non-carry Add's prior to calculation.

	rem Sign Bit.
	s=Bit7(result)								: rem Set Sign bit ( reflects Bit7 of the result )
	
	pv=Parity(result)							: rem OK, we need to do parity of the RESULT here..... Not of the Acc. 
	carry=0
	half=0
	n=0
	
	rem Zero
	if result=0 then z=1 else z=0				: rem do this with result, since for carry, Accumulator might be 256. 
	
	BAKEAF()			: rem Set flags into AF
	
	return result
End Function







rem Big Register Functions Here. 

Function POP () as integer
rem Return the value at the stack pointer.  
dim result as integer
	
	result=0
	rem higher lower sethigher setlower
	rem set fetch
	
	result=fetch(Stack)
	STACK=(STACK+1) mod 65536
	result=result+(fetch(Stack)*256)
	STACK=(STACK+1) mod 65536

	return result
End Function

Function PUSH (Byref value as integer) as integer
rem Store the value at the stack pointer location.  
dim result as integer

	
	VALUE=VALUE AND &HFFFF : rem MASK just in case we get a variable overflow. (eg, PC from FFFF to 0000 )
	
	STACK=(STACK+65535) mod 65536	: rem Subtract 1 from stack.
	set (STACK,higher(value))
	STACK=(STACK+65535) mod 65536	: rem Subtract 1 from stack.
	set (STACK,lower(value))
	
	result=0	: rem we don't really care what the result is. We're storing a value.

	return result
End Function


Function JumpRelative(Byref vector as integer) as integer
rem Return a 16 bit value to be added to the PC when Jump relative. 
			if vector>127 then vector=65280+vector						: rem Backwards jump... Since we will get a negative. So FF=-1. FE=-2 etc/ 
			PC=(PC+vector) mod 65536							: rem add the vector to the PC and mask for range.		
return 0
End Function



rem MAD I/O here - LOKI emulation - MEMORY AS DISK registers. 

rem Install MAD read and write here
function MADWRITE (Byref Port as integer, Byref Value as integer) as integer
rem WRITE A MEMORY LOCATION AS AN I/O FUNCTION
dim result as integer
dim LOWER7 as integer		: rem Lower 7 bits of address formed from Bit 8 to Bit 14 of the port value
dim IOADDRESS as integer 	: rem Full I/O address in 20 bits, from Lower7, Sector and Track.

	LOWER7=INT(Port/256) MOD 128	: rem Strip out lower 8 bits and Bit 15.
	IOADDRESS = LOWER7 + ((sector MOD 32)*128) + ((TRACK MOD 256)*4096)

rem print "-I/O Write to memory address:";hex(IOADDRESS,5);"- Drive:";drive;"- TrackL:";track;"- Sector:";sector;" -DMA:";hex(dmahigh,2);hex(dmalow,2)

	rem FETCH and SET 
	rem SET (location,value )
	rem FETCH (location )

rem	SET (IOADDRESS,Value)	: rem Write the byte. 
CODE(IOADDRESS+1)=value		: rem set direct - we're in 19 bit space. 
WRITEVRAM(IOADDRESS,value)

return result
end function



function MADREAD (Byref Port as integer) as integer
rem READ A MEMORY LOCATION AS AN I/O FUNCTION.
dim result as integer
dim LOWER7 as integer		: rem Lower 7 bits of address formed from Bit 8 to Bit 14 of the port value
dim IOADDRESS as integer 	: rem Full I/O address in 20 bits, from Lower7, Sector and Track.

	LOWER7=INT(Port/256) MOD 128	: rem Strip out lower 8 bits and Bit 15.
	IOADDRESS = LOWER7 + ((sector MOD 32)*128) + ((TRACK MOD 256)*4096)

rem	print "-I/O READ to memory address:";hex(IOADDRESS,5);"-"

	rem FETCH and SET 
	rem SET (location,value )
	rem FETCH (location )

rem result=FETCH (IOADDRESS)	: rem Read the byte. 
	result=CODE(IOADDRESS+1)	: rem Read the byte direct - we're in 19 bit space. 

rem print "MAD READ - TRACK:";track;" - Sector:";sector;" - MADADDR:";IOADDRESS

return result
end function





rem IO Function Here. 

Function inport(Byref Port as integer) as integer
rem Return the value of input to a port.
dim bus as integer
bus=0	: rem set default input value... Might change this later.
dim keyboard as string
dim key as string
dim portval as integer
dim ext as string

portval=port mod 256

Select case portval
	Case 1


	key=inkey
	bus=asc(key)
 rem 	if bus=8 then bus=127 : rem Change backspace for rubout.
	
	if bus=255 then
		ext=right(key,1)	: rem It's a keyboard escape code. ( 255 key... ) - two characters.
		if ext="K" then bus=8 : rem Left
		if ext="M" then bus=&h8C : rem Right
		if ext="H" then bus=&h8B : rem Up
		if ext="P" then bus=&h8A : rem Down
		if ext="S" then bus=&h87 : rem Ctrl-G.  DELETE... Wordstar key. 
		endif

		rem if bus<>0 then print chr(bus)
		rem if bus<>0 then print chr(bus)
	
	Case &h0C
		rem I/O port for MEMORY AS DISK 
		bus=MADREAD(port)			: rem Write the byte to memory via I/O. 
		
	case &h0E
		rem The PID register. That works with the MMU. 
rem			Process=value
		bus=LastProcess AND &h1F	: rem we return the previous process ( what called the current process )
	Case &h0F
		rem MMU Port - A12 to A15 of BC defines the page value to the MMU CACHE.
		bus=GETMMU(port)

	Case else
	
End select

return bus
End Function 

Function inBC(Byref Port as integer) as integer
rem Return the value of input to a port.
dim bus as integer
bus=0	: rem set default input value... Might change this later.

bus=inport(Port)	: rem Reflect the IN function, but break out the register changes here.

rem Register changes. 
		half=0
		n=0

		s=Bit7(bus)
		if bus=0 then z=1 else z=0
		
		pv=parity(bus)

		BakeAF()

return bus
End Function 

rem .equ	CONSTATUS	,$00
rem .equ	CON			,$01
rem .equ	LPT			,$02
rem .equ	AUX			,$03
rem .equ	DRIVEACTIVE	,$04
rem .equ	DRIVE		,$05
rem .equ	TRACK		,$06
rem .equ	SECTOR		,$07
rem .equ	DMALOW		,$08
rem .equ	DMAHIGH		,$09
rem .equ	MADPORT		,$0C	; MEMORY AS DISK PORT. 




Function Insert(ByRef target as String, ByRef contents as String, Byref location as integer) As String                  : rem Insert a string into a string. For modifying a DSK file.

        dim li as Integer : rem Length of inserted string.
        dim lt as Integer : rem Length of targetted string.

        target=left$(target,location-1)+contents+right$(target,len(target)-len(contents)-location+1)

        Return target
End function


Function DiskLoc (Byref Track as integer, Byref Sector as integer ) as integer
rem Return 0-based (not 1-based) start of disk image for Track and Sector. 
rem Sector is 128bytes, while disk image is 8x512 Sectors/track. (4 blocks/allocations)
rem So that's 32 x 128 byte sectors/track, so the tracks line up. 
rem Remember there's a 256 byte header, and a 256 byte
dim result as integer

result=512		: rem Disk Information Block and first Track Information Block.
result=result+(track*(8*512))+(track*256)	: rem Track is 8 512K sectors, but there's a Track Information Block between tracks.
result=result+(Sector*128)		: rem Sector 0 is the start, so this provides an offset from the start of the track.

rem The Read and Write function BOTH add 1 in the 1 to 128 loop, so that addresses the fact the array starts at 1 and not 0. 

return result
End function


Function WRITEBLOCK() as integer
dim MEMLOC as integer
dim COUNT as integer
dim DMA as integer
dim BLOCK as string

	DMA=DMALOW+(DMAHIGH*256)
	rem MEMLOC=((SECTOR-1)+(TRACK*256))*128
	MEMLOC=DiskLoc(Track,Sector)
	BLOCK=""

	for count=1 to 128
		BLOCK=BLOCK+chr(CODE(MMU(DMA)+count))
	next count
	
	MEMLOC=MEMLOC+1		: rem Because the FIRST location isn't referenced by a 0, but by a 1. 

rem 	print "WRITING:";hex(memloc,8);" From DMA:";HEX(DMA,4);" ";BLOCK

rem print "DISK Write to memory address:";hex(MEMLOC,5);"- Drive:";drive;"- TrackL:";track;"- Sector:";sector;" -DMA:";hex(dmahigh,2);hex(dmalow,2)

	insert (DISK,BLOCK,MEMLOC)
	
rem	print "Saving:";block;" at ";(memloc-1); "   DMA:";DMA

return 0
End Function

Function READBLOCK() as integer
dim MEMLOC as integer
dim COUNT as integer
dim DMA as integer
dim diskdata as string

diskdata=""

	DMA=DMALOW+(DMAHIGH*256)
	rem MEMLOC=(SECTOR+(TRACK*256))*128
rem Print "Track:";track,"Sector:";sector
	MEMLOC=DiskLoc(Track,Sector)
	
	for count=1 to 128
		CODE(MMU(DMA)+count)=asc(mid(DISK,(MEMLOC+count),1))
		WRITEVRAM(MMU(DMA)+count-1,asc(mid(DISK,(MEMLOC+count),1))) : rem Shadow Write the VRAM> 
		diskdata=diskdata+mid(DISK,(MEMLOC+count),1)
	next count


rem if debug > 0 then print "Reading from Disk A:" 
rem if debug > 0 then printhex (diskdata,len(diskdata))

rem print #9,	: rem blank line. 
rem print #9,"Reading from Disk A: ";
rem print #9,"address:";hex(MEMLOC,5);"- Drive:";drive;"- TrackL:";track;"- Sector:";sector;" -DMA:";hex(dmahigh,2);hex(dmalow,2)
rem consoleprinthex (diskdata,len(diskdata))

return 0
End Function

Function ANSI(Byref char as string) as integer
dim response as integer
response=0
dim XPOS as integer
dim YPOS as integer
dim ESCCOMMAND as string

rem Set up current X and Y position.
YPOS=csrlin
XPOS=pos

escsequence=escsequence+1
escarg(escsequence)=char

esccommand=escarg(1)	: rem Set the argument - 


	if inescape then
	
		select case esccommand
			case "A"
				rem Cursor Up
				YPOS=YPOS-1 : if YPOS<=1 then YPOS=1
					locate YPOS,XPOS,1
					inescape=0	
					goto	ansiend

			case "B"
				rem Cursor Down
				YPOS=YPOS+1 : if YPOS>=24 then YPOS=24
					locate YPOS,XPOS,1
					inescape=0	
					inescape=0	
					goto	ansiend

			case "C"
				rem Cursor Right
				XPOS=XPOS+1 : if XPOS>=80 then XPOS=80
					locate YPOS,XPOS,1
					inescape=0	
					goto	ansiend

			case "D"
				rem Cursor Left
				XPOS=XPOS-1 : if XPOS<=180 then XPOS=1
					locate YPOS,XPOS,1
					inescape=0	
					goto	ansiend
					
			case "E"
				rem Clear Screen.
					cls
					goto ansiend

			case "F"
				rem Enter Graphics Character Mode - Find out what VT52 graphics are;
					inescape=0	
					goto	ansiend

			case "G"
				rem Exit Graphics Character Mode -
					inescape=0	
					goto	ansiend

			case "H"
				rem Home ( far left )
					inescape=0	
					goto	ansiend

			case "I"
				rem Reverse Line Feed - Same as Cursor Up, but with reverse scrolling at top.
					inescape=0	
					goto	ansiend

			case "J" 
				rem Erase to end of screen (do not change position) -
					inescape=0	
					goto	ansiend

			case "K"
				rem Erase all characters to the end of the current line (do not change position )
					inescape=0	
					goto	ansiend

			case "L" 
				rem Insert line.
					inescape=0	
					goto	ansiend

			case "M"
				rem Delete line. 
					inescape=0	
					goto	ansiend

			case "Y"
				rem Cursor to LINE COLUMN - Subtract 32 ( &h20 ) fromthe character in pos 2 and 3, then Y before X. 
				if escsequence=3 then
					ypos=asc(escarg(2))-31
					xpos=asc(escarg(3))-31
					locate ypos,xpos,1
					inescape=0	
					escsequence=0
					goto	ansiend
				else
					goto 	ansiend
				endif


			case "Z"
				rem Identify... WTF?
					inescape=0	
					goto	ansiend
	
			case "="
				rem Enter Alternate Keypad Mode. 
					inescape=0	
					goto	ansiend
					
			case ">"
				rem Exit Alternate Keypad Mode. 
					inescape=0	
					goto	ansiend
					
			case "3"
				rem Foreground.
					inescape=0
					goto ansiend
					
			case "4"
				rem Foreground.
					inescape=0
					goto ansiend
					
					
			case else
				rem Escape sequence we didn't expect -
				rem Drop out.
				print "~";esccommand					: rem Let's just print a dot so we can see it for the moment. 
					inescape=0	
					goto	ansiend

				
		end select
		endif


	if escsequence=5 then 
		inescape=0			: rem Something went wrong - switch back to normal mode. 
		escsequence=0
		goto ansiend
		endif

	if char=chr$(27) then 
		inescape=1
		escsequence=0
		goto ansiend
		endif
		
	if not inescape then 

			if char > chr(31) then 
				print char;
			else 
				print char;
			endif
			
			

		escsequence=0
		goto ansiend
		endif

ansiend:

return inescape

end function

Function outport(Byref Port as integer,Byref value as integer) as integer
rem Send the value to the port specified.
dim portval as integer	: rem This is the port being accessed, MOD 256. 

rem Do something here to set flags

portval=port mod 256

select case portval 
	case 1 
		rem Console Output... (Screen ). 
		if debug>1 then print #9, "###";
rem		print chr(value);	: rem debug output. 

		ANSI(chr(value))

		print #9,chr(value);
		
rem 		if value=&h1b then debug=1 : rem dodgy way to start debug on an event - eg, on receiving an escape code from the program. 
		
		if debug>1 then print #9,"###";
	case 2
rem		print "Value2:";value;" =";hex(value,2)
	case 3
rem			print "Value2:";value;" =";hex(value,2)
	case 4
	
rem				print
				
				
		if value=1 then 
			WRITEBLOCK
rem							print "Write to DMA=";hex(DMALOW+(DMAHIGH*256),4);" Action:";hex(value,2)

			endif
		if value=0 then 
			READBLOCK
	rem						print "Read  to DMA=";hex(DMALOW+(DMAHIGH*256),4);" Action:";hex(value,2)

			endif

	case 5
		DRIVE=VALUE
rem 	Print "Drive:";DRIVE;" Selected.";" =";hex(value,2);" (";chr(value+65);":)"
	case 6
		TRACK=VALUE
rem		Print "Track:";Track;" Selected";" =";hex(value,2)
	case 7
		SECTOR=VALUE
rem		Print "Sector:";Sector;" Selected";" =";hex(value,2)
	
rem print "Select Drv:";hex(drive,2);"  Track:";hex(track,4);"  Sector:";hex(sector,4); "   DMA:";HEX(DMAHIGH,2);HEX(DMALOW,2)
	
	
rem 	if track=248 then debug=2
	
	case 8
		DMALOW=value
rem			print "ValueLow:";value;" =";hex(value,2)
	case 9
		DMAHIGH=value
rem			print "ValueHigh:";value;" =";hex(value,2)

	case 10,16,17,18,19,20	: rem DEBUG PORT - can be used for anything including status or BIOS POST. 
rem 		PRINT "POST:";hex(value,2);"   Port:";hex(port,4)	: rem Just print the value of the port. Don't do anything else with it. 
	debugreg()
rem	print #9,"Debug Called: Value ";value
		if portval=16 then debug=0 : print #9, : print #9,"DEBUGGING DISABLED " : PRINT #9,
		if portval=17 then debug=1 : print #9, : print #9,"DEBUGGING ENABLED LEVEL 1. " : PRINT #9,
		if portval=18 then debug=2 : print #9, : print #9,"DEBUGGING ENABLED LEVEL 2. " : PRINT #9,

		if portval=19 then
			print #9,"Select Drv:";hex(drive,2);"  Track:";hex(track,4);"  Sector:";hex(sector,4); "   DMA:";HEX(DMAHIGH,2);HEX(DMALOW,2)
			endif
		if portval=20 then
				codetext=""
				for a=DE+1+4096 to DE+36+4096
				codetext=codetext+chr(code(a))
				next a
			consoleprinthex(codetext,36)
			endif

	case &h0B
				print #8,chr(value);	: rem Alternative character store. 

	case &h0C
		rem I/O port for MEMORY AS DISK 
			MADWRITE(port,value)			: rem Write the byte to memory via I/O. 

	case &h0D
		rem The Start Bit and other system flags. 
		rem BIT0 = startbit. Set to 1 if there's been a reset.
		rem BIT1 = RESET. Set to 1 to cause a software defined reset. 
			STARTBIT=VALUE
			if STARTBIT=2 then PC=0 : STARTBIT=1 : rem Force a reset if Bit1 is set to 1. 
			print "Setting start bit to value:";value

	case &h0E
		rem The PID register. That works with the MMU. 
			Lastprocess=Process
			Process=value AND &H1F
	case &h0F
		rem MMU Port - A12 to A15 of BC defines the page value to the MMU CACHE.

		SETMMU(port,value)


	case else
	Print "Unknown IO Trapped Port:";port;" Value:";value;" at PC:";hex(PC,4)

return 0
end select


End Function 

Function outBC(Byref Port as integer,Byref value as integer) as integer
rem Send the value to the port specified.

outport (Port,value)	: rem use the existing output.

rem Do something here to set flags. 

return 0
End Function 





Function LDI() as integer
rem Load and Increment.

	Set(DE,(Fetch(HL)))
	DE=(DE+1) mod 65536
	HL=(HL+1) mod 65536
	BC=(BC+65535) mod 65536
	if BC=0 then PV=0 else PV=1
	UnbakeBC()
	UnbakeDE()
	UnbakeHL()

return 0
End Function

Function LDD() as integer
rem Load and Decrement.


	Set(DE,(Fetch(HL)))
	DE=(DE+65535) mod 65536
	HL=(HL+65535) mod 65536
	BC=(BC+65535) mod 65536
	if BC=0 then PV=0 else PV=1
	UnbakeBC()
	UnbakeDE()
	UnbakeHL()

return 0
End Function

Function CPI() as integer
dim tempcarry as integer

	tempcarry=carry
	a=ALUCP(a,Fetch(HL))
	carry=tempcarry
	n=1
	HL=(HL+1) mod 65536
	BC=(BC+65535) mod 65536
	if BC=0 then PV=0 else PV=1
	UnbakeHL()
	UnbakeBC()

return 0
End Function

Function CPD() as integer
dim tempcarry as integer

	tempcarry=carry
	a=ALUCP(a,Fetch(HL))
	carry=tempcarry
	n=1
	HL=(HL+65535) mod 65536
	BC=(BC+65535) mod 65536
	if BC=0 then PV=0 else PV=1
	UnbakeHL()
	UnbakeBC()

return 0
End Function

Function INI() as integer
rem In and Increment

	SET(HL,InBC(BC))
	B=(B+255) mod 256
	HL=(HL+1) mod 65536
	n=1
	if B=0 then Z=1 else Z=0
	BakeBC()
	UnbakeHL()
return 0
End Function

Function IND() as integer
rem In and Decrement

	SET(HL,InBC(BC))
	B=(B+255) mod 256
	HL=(HL+65535) mod 65536
	n=1
	if B=0 then Z=1 else Z=0
	BakeBC()
	UnbakeHL()

return 0
End Function

Function OUTI() as integer
rem Out and Increment

	OutBC(BC,Fetch(HL))
	B=(B+255) mod 256
	HL=(HL+1) mod 65536
	n=1
	if B=0 then Z=1 else Z=0
	BakeBC()
	UnbakeHL()

return 0	
End Function

Function OUTD() as integer
rem Out and Decrement

	OutBC(BC,Fetch(HL))
	B=(B+255) mod 256
	HL=(HL+65535) mod 65536
	n=1
	if B=0 then Z=1 else Z=0
	BakeBC()
	UnbakeHL()

return 0
End Function



Function RESETz80() as integer
rem RESET FLAGS and anything else.
PC=START
half=1
carry=1
N=1
PV=1
S=1
Z=1

BAKEAF()	: rem store in the 16 bit AF> 
return 0
end function


rem Disassembly strings.

Function DisDD (Byref Nextcode as integer ) as string
dim result as string

Select Case As Const Nextcode
	Case &h00
		result="rlc(IX+disp),b"
	Case &h01
		result="rlc(IX+disp),c"
	Case &h02
		result="rlc(IX+disp),d"
	Case &h03
		result="rlc(IX+disp),e"
	Case &h04
		result="rlc(IX+disp),h"
	Case &h05
		result="rlc(IX+disp),l"
	Case &h06
		result="rlc (IX+disp)"
	Case &h07
		result="rlc(IX+disp),a"
	Case &h08
		result="rrc(IX+disp),b"
	Case &h09
		result="rrc(IX+disp),c"
	Case &h0A
		result="rrc(IX+disp),d"
	Case &h0B
		result="rrc(IX+disp),e"
	Case &h0C
		result="rrc(IX+disp),h"
	Case &h0D
		result="rrc(IX+disp),l"
	Case &h0E
		result="rrc (IX+disp)"
	Case &h0F
		result="rrc(IX+disp),a"
	Case &h10
		result="rl(IX+disp),b"
	Case &h11
		result="rl(IX+disp),c"
	Case &h12
		result="rl(IX+disp),d"
	Case &h13
		result="rl(IX+disp),e"
	Case &h14
		result="rl(IX+disp),h"
	Case &h15
		result="rl(IX+disp),l"
	Case &h16
		result="rl (IX+disp)"
	Case &h17
		result="rl(IX+disp),a"
	Case &h18
		result="rr(IX+disp),b"
	Case &h19
		result="rr(IX+disp),c"
	Case &h1A
		result="rr(IX+disp),d"
	Case &h1B
		result="rr(IX+disp),e"
	Case &h1C
		result="rr(IX+disp),h"
	Case &h1D
		result="rr(IX+disp),l"
	Case &h1E
		result="rr (IX+disp)"
	Case &h1F
		result="rr(IX+disp),a"
	Case &h20
		result="sla(IX+disp),b"
	Case &h21
		result="sla(IX+disp),c"
	Case &h22
		result="sla(IX+disp),d"
	Case &h23
		result="sla(IX+disp),e"
	Case &h24
		result="sla(IX+disp),h"
	Case &h25
		result="sla(IX+disp),l"
	Case &h26
		result="sla (IX+disp)"
	Case &h27
		result="sla(IX+disp),a"
	Case &h28
		result="sra(IX+disp),b"
	Case &h29
		result="sra(IX+disp),c"
	Case &h2A
		result="sra(IX+disp),d"
	Case &h2B
		result="sra(IX+disp),e"
	Case &h2C
		result="sra(IX+disp),h"
	Case &h2D
		result="sra(IX+disp),l"
	Case &h2E
		result="sra (IX+disp)"
	Case &h2F
		result="sra(IX+disp),a"
	Case &h30
		result="sll(IX+disp),b"
	Case &h31
		result="sll(IX+disp),c"
	Case &h32
		result="sll(IX+disp),d"
	Case &h33
		result="sll(IX+disp),e"
	Case &h34
		result="sll(IX+disp),h"
	Case &h35
		result="sll(IX+disp),l"
	Case &h36
		result="sll (IX+disp)"
	Case &h37
		result="sll(IX+disp),a"
	Case &h38
		result="srl(IX+disp),b"
	Case &h39
		result="srl(IX+disp),c"
	Case &h3A
		result="srl(IX+disp),d"
	Case &h3B
		result="srl(IX+disp),e"
	Case &h3C
		result="srl(IX+disp),h"
	Case &h3D
		result="srl(IX+disp),l"
	Case &h3E
		result="srl (IX+disp)"
	Case &h3F
		result="srl(IX+disp),a"
	Case &h40
		result="bit 0,(IX+disp)"
	Case &h41
		result="bit 0,(IX+disp)"
	Case &h42
		result="bit 0,(IX+disp)"
	Case &h43
		result="bit 0,(IX+disp)"
	Case &h44
		result="bit 0,(IX+disp)"
	Case &h45
		result="bit 0,(IX+disp)"
	Case &h46
		result="bit 0,(IX+disp)"
	Case &h47
		result="bit 0,(IX+disp)"
	Case &h48
		result="bit 1,(IX+disp)"
	Case &h49
		result="bit 1,(IX+disp)"
	Case &h4A
		result="bit 1,(IX+disp)"
	Case &h4B
		result="bit 1,(IX+disp)"
	Case &h4C
		result="bit 1,(IX+disp)"
	Case &h4D
		result="bit 1,(IX+disp)"
	Case &h4E
		result="bit 1,(IX+disp)"
	Case &h4F
		result="bit 1,(IX+disp)"
	Case &h50
		result="bit 4,(IX+disp)"
	Case &h51
		result="Bit 2,(IX+disp)"
	Case &h52
		result="Bit 2,(IX+disp)"
	Case &h53
		result="Bit 2,(IX+disp)"
	Case &h54
		result="Bit 2,(IX+disp)"
	Case &h55
		result="Bit 2,(IX+disp)"
	Case &h56
		result="Bit 2,(IX+disp)"
	Case &h57
		result="Bit 2,(IX+disp)"	
	Case &h58
		result="bit 5,(IX+disp)"
	Case &h59
		result="Bit 3,(IX+disp)"
	Case &h5A
		result="Bit 3,(IX+disp)"
	Case &h5B
		result="Bit 3,(IX+disp)"
	Case &h5C
		result="Bit 3,(IX+disp)"
	Case &h5D
		result="Bit 3,(IX+disp)"
	Case &h5E
		result="Bit 3,(IX+disp)"
	Case &h5F
		result="Bit 3,(IX+disp)"
	Case &h60
		result="bit 4,(IX+disp)"
	Case &h61
		result="bit 4,(IX+disp)"
	Case &h62
		result="bit 4,(IX+disp)"
	Case &h63
		result="bit 4,(IX+disp)"
	Case &h64
		result="bit 4,(IX+disp)"
	Case &h65
		result="bit 4,(IX+disp)"
	Case &h66
		result="bit 4,(IX+disp)"
	Case &h67
		result="bit 4,(IX+disp)"
	Case &h68
		result="bit 5,(IX+disp)"
	Case &h69
		result="bit 5,(IX+disp)"
	Case &h6A
		result="bit 5,(IX+disp)"
	Case &h6B
		result="bit 5,(IX+disp)"
	Case &h6C
		result="bit 5,(IX+disp)"
	Case &h6D
		result="bit 5,(IX+disp)"
	Case &h6E
		result="bit 5,(IX+disp)"
	Case &h6F
		result="bit 5,(IX+disp)"
	Case &h70
		result="bit 6,(IX+disp)"
	Case &h71
		result="bit 6,(IX+disp)"
	Case &h72
		result="bit 6,(IX+disp)"
	Case &h73
		result="bit 6,(IX+disp)"
	Case &h74
		result="bit 6,(IX+disp)"
	Case &h75
		result="bit 6,(IX+disp)"
	Case &h76
		result="bit 6,(IX+disp)"
	Case &h77
		result="bit 6,(IX+disp)"
	Case &h78
		result="bit 7,(IX+disp)"
	Case &h79
		result="bit 7,(IX+disp)"
	Case &h7A
		result="bit 7,(IX+disp)"
	Case &h7B
		result="bit 7,(IX+disp)"
	Case &h7C
		result="bit 7,(IX+disp)"
	Case &h7D
		result="bit 7,(IX+disp)"
	Case &h7E
		result="bit 7,(IX+disp)"
	Case &h7F
		result="bit 7,(IX+disp)"
	Case &h80
		result="res 0,(IX+disp)"
	Case &h81
		result="res 0,(IX+disp)"
	Case &h82
		result="res 0,(IX+disp)"
	Case &h83
		result="res 0,(IX+disp)"
	Case &h84
		result="res 0,(IX+disp)"
	Case &h85
		result="res 0,(IX+disp)"
	Case &h86
		result="res 0,(IX+disp)"
	Case &h87
		result="res 0,(IX+disp)"
	Case &h88
		result="res 1,(IX+disp)"
	Case &h89
		result="res 1,(IX+disp)"
	Case &h8A
		result="res 1,(IX+disp)"
	Case &h8B
		result="res 1,(IX+disp)"
	Case &h8C
		result="res 1,(IX+disp)"
	Case &h8D
		result="res 1,(IX+disp)"
	Case &h8E
		result="res 1,(IX+disp)"
	Case &h8F
		result="res 1,(IX+disp)"
	Case &h90
		result="res 4,(IX+disp)"
	Case &h91
		result="res 2,(IX+disp)"
	Case &h92
		result="res 2,(IX+disp)"
	Case &h93
		result="res 2,(IX+disp)"
	Case &h94
		result="res 2,(IX+disp)"
	Case &h95
		result="res 2,(IX+disp)"
	Case &h96
		result="res 2,(IX+disp)"
	Case &h97
		result="res 2,(IX+disp)"
	Case &h98
		result="res 5,(IX+disp)"
	Case &h99
		result="res 3,(IX+disp)"
	Case &h9A
		result="res 3,(IX+disp)"
	Case &h9B
		result="res 3,(IX+disp)"
	Case &h9C
		result="res 3,(IX+disp)"
	Case &h9D
		result="res 3,(IX+disp)"
	Case &h9E
		result="res 3,(IX+disp)"
	Case &h9F
		result="res 3,(IX+disp)"
	Case &hA0
		result="res 4,(IX+disp)"
	Case &hA1
		result="res 4,(IX+disp)"
	Case &hA2
		result="res 4,(IX+disp)"
	Case &hA3
		result="res 4,(IX+disp)"
	Case &hA4
		result="res 4,(IX+disp)"
	Case &hA5
		result="res 4,(IX+disp)"
	Case &hA6
		result="res 4,(IX+disp)"
	Case &hA7
		result="res 4,(IX+disp)"
	Case &hA8
		result="res 5,(IX+disp)"
	Case &hA9
		result="res 5,(IX+disp)"
	Case &hAA
		result="res 5,(IX+disp)"
	Case &hAB
		result="res 5,(IX+disp)"
	Case &hAC
		result="res 5,(IX+disp)"
	Case &hAD
		result="res 5,(IX+disp)"
	Case &hAE
		result="res 5,(IX+disp)"
	Case &hAF
		result="res 5,(IX+disp)"
	Case &hB0
		result="res 6,(IX+disp)"
	Case &hB1
		result="res 6,(IX+disp)"
	Case &hB2
		result="res 6,(IX+disp)"
	Case &hB3
		result="res 6,(IX+disp)"
	Case &hB4
		result="res 6,(IX+disp)"
	Case &hB5
		result="res 6,(IX+disp)"
	Case &hB6
		result="res 6,(IX+disp)"
	Case &hB7
		result="res 6,(IX+disp)"
	Case &hB8
		result="res 7,(IX+disp)"
	Case &hB9
		result="res 7,(IX+disp)"
	Case &hBA
		result="res 7,(IX+disp)"
	Case &hBB
		result="res 7,(IX+disp)"
	Case &hBC
		result="res 7,(IX+disp)"
	Case &hBD
		result="res 7,(IX+disp)"
	Case &hBE
		result="res 7,(IX+disp)"
	Case &hBF
		result="res 7,(IX+disp)"
	Case &hC0
		result="set 0,(IX+disp)"
	Case &hC1
		result="set 0,(IX+disp)"
	Case &hC2
		result="set 0,(IX+disp)"
	Case &hC3
		result="set 0,(IX+disp)"
	Case &hC4
		result="set 0,(IX+disp)"
	Case &hC5
		result="set 0,(IX+disp)"
	Case &hC6
		result="set 0,(IX+disp)"
	Case &hC7
		result="set 0,(IX+disp)"
	Case &hC8
		result="set 1,(IX+disp)"
	Case &hC9
		result="set 1,(IX+disp)"
	Case &hCA
		result="set 1,(IX+disp)"
	Case &hCB
		result="set 1,(IX+disp)"
	Case &hCC
		result="set 1,(IX+disp)"
	Case &hCD
		result="set 1,(IX+disp)"
	Case &hCE
		result="set 1,(IX+disp)"
	Case &hCF
		result="set 1,(IX+disp)"
	Case &hD0
		result="set 4,(IX+disp)"
	Case &hD1
		result="set 2,(IX+disp)"
	Case &hD2
		result="set 2,(IX+disp)"
	Case &hD3
		result="set 2,(IX+disp)"
	Case &hD4
		result="set 2,(IX+disp)"
	Case &hD5
		result="set 2,(IX+disp)"
	Case &hD6
		result="set 2,(IX+disp)"
	Case &hD7
		result="set 2,(IX+disp)"	
	Case &hD8
		result="set 5,(IX+disp)"
	Case &hD9
		result="set 3,(IX+disp)"
	Case &hDA
		result="set 3,(IX+disp)"
	Case &hDB
		result="set 3,(IX+disp)"
	Case &hDC
		result="set 3,(IX+disp)"
	Case &hDD
		result="set 3,(IX+disp)"
	Case &hDE
		result="set 3,(IX+disp)"
	Case &hDF
		result="set 3,(IX+disp)"
	Case &hE0
		result="set 4,(IX+disp)"
	Case &hE1
		result="set 4,(IX+disp)"
	Case &hE2
		result="set 4,(IX+disp)"
	Case &hE3
		result="set 4,(IX+disp)"
	Case &hE4
		result="set 4,(IX+disp)"
	Case &hE5
		result="set 4,(IX+disp)"
	Case &hE6
		result="set 4,(IX+disp)"
	Case &hE7
		result="set 4,(IX+disp)"
	Case &hE8
		result="set 5,(IX+disp)"
	Case &hE9
		result="set 5,(IX+disp)"
	Case &hEA
		result="set 5,(IX+disp)"
	Case &hEB
		result="set 5,(IX+disp)"
	Case &hEC
		result="set 5,(IX+disp)"
	Case &hED
		result="set 5,(IX+disp)"
	Case &hEE
		result="set 5,(IX+disp)"
	Case &hEF
		result="set 5,(IX+disp)"
	Case &hF0
		result="set 6,(IX+disp)"
	Case &hF1
		result="set 6,(IX+disp)"
	Case &hF2
		result="set 6,(IX+disp)"
	Case &hF3
		result="set 6,(IX+disp)"
	Case &hF4
		result="set 6,(IX+disp)"
	Case &hF5
		result="set 6,(IX+disp)"
	Case &hF6
		result="set 6,(IX+disp)"
	Case &hF7
		result="set 6,(IX+disp)"
	Case &hF8
		result="set 7,(IX+disp)"
	Case &hF9
		result="set 7,(IX+disp)"
	Case &hFA
		result="set 7,(IX+disp)"
	Case &hFB
		result="set 7,(IX+disp)"
	Case &hFC
		result="set 7,(IX+disp)"
	Case &hFD
		result="set 7,(IX+disp)"
	Case &hFE
		result="set 7,(IX+disp)"
	Case &hFF
		result="set 7,(IX+disp)"
	Case else 
		result="Undocumented"
	End Select
	
return result
End Function 

Function DisED (Byref Nextcode as integer ) as string
dim result as string

Select Case As Const Nextcode
	Case 0
		RESULT="Undocumented"

	Case &h40
		RESULT="in b,(c)"

	Case &h41
		RESULT="out (c),b"

	Case &h42
		RESULT="sbc hl,bc"

	Case &h43
		RESULT="ld (xx),bc"

	Case &h44
		RESULT="neg"

	Case &h45
		RESULT="retn"

	Case &h46
		RESULT="im0"

	Case &h47
		RESULT="ld i,a"

	Case &h48
		RESULT="in c,(c)"

	Case &h49
		RESULT="out (c),c"

	Case &h4A
		RESULT="adc hl,bc"
	
	Case &h4B
		RESULT="ld bc,(xx)"
	
	Case &h4C
		RESULT="neg"

	Case &h4D
		RESULT="reti"
		
	Case &h4E
		RESULT="Undocumented"
	
	Case &h4F
		RESULT="ld r,a"

	Case &h50
		RESULT="in d,(c)"

	Case &h51
		RESULT="out (c),d"
	
	Case &h52
		RESULT="sbc hl,de"

	Case &h53
		RESULT="ld (xx),de"
	
	Case &h54
		RESULT="neg"
		
	Case &h55
		RESULT="retn"
		
	Case &h56
		RESULT="im1"
	
	Case &h57
		RESULT="ld a,i"
		
	Case &h58
		RESULT="in e,(c)"
	
	Case &h59
		RESULT="out (c),e"
	
	Case &h5A
		RESULT="adc hl,de"
		
	Case &h5B
		RESULT="ld de,(xx)"
	
	Case &h5C
		RESULT="neg"

	Case &h5D
		RESULT="retn"
		
	Case &h5E
		RESULT="im 2"
	
	Case &h5F
		RESULT="ld a,r"

	Case &h60
		RESULT="in h,(c)"
		
	Case &h61
		RESULT="out (c),h"
		
	Case &h62
		RESULT="sbc hl,hl"
	
	Case &h63
		RESULT="ld (xx),hl"
		
	Case &h64
		RESULT="neg"
		
	Case &h65
		RESULT="retn"
		
	Case &h66
		RESULT="Undocumented"
		
	Case &h67
		RESULT="rrd"
		
	Case &h68
		RESULT="in l,(c)"
	
	Case &h69
		RESULT="out (c),l"
	
	Case &h6A
		RESULT="adc hl,hl"
		
	Case &h6B
		RESULT="ld hl,(xx)"
		
	Case &h6C
		RESULT="neg"

	Case &h6D
		RESULT="retn"
		
	Case &h6E
		RESULT="Undocumented"
	
	Case &h6F
		RESULT="rld"
		
	Case &h70
		RESULT="in f,(c)"
	
	Case &h71
		RESULT="out (c),0"
		
	Case &h72
		RESULT="sbc hl,sp"
	
	Case &h73
		RESULT="ld (xx),sp"
		
	Case &h74
		RESULT="neg"
		
	Case &h75
		RESULT="retn"
		
	Case &h76
		RESULT="Undocumented"
	
	Case &h77
		RESULT="Undocumented"	
		
	Case &h78
		RESULT="in a,(c)"
	
	Case &h79
		RESULT="out (c),a"
		
	Case &h7A
		RESULT="adc hl,sp"
		
	Case &h7B
		RESULT="ld sp,(xx)"
		
	Case &h7C
		RESULT="neg"

	Case &h7D
		RESULT="reti"
		
	Case &h7E
		RESULT="Undocumented"
		
	Case &h7F
		RESULT="rld"
		
	Case &hA0
		RESULT="ldi"
		
	Case &hA1
		RESULT="cpi"
		
	Case &hA2
		RESULT="ini"
		
	Case &hA3
		RESULT="outi"
		
	Case &hA8
		RESULT="ldd"
		
	Case &hA9
		RESULT="cpd"
		
	Case &hAA
		RESULT="ind"
	
	Case &hAB 
		RESULT="outd"
		
	Case &hB0
		RESULT="ldir"
				
	Case &hB1
		RESULT="cpir"
				
	Case &hB2
		RESULT="inir"
				
	Case &hB3
		RESULT="otir"
				
	Case &hB8
		RESULT="lddr"
			
	Case &hB9
		RESULT="cpir"
				
	Case &hBA
		RESULT="inir"
		
	Case &hBB
		RESULT="otdr"
		
	Case Else
		result="Undocumented"
End Select


return result
End Function


Function M1DIS (Byref opcode as integer) as string
rem: result=the binary codes for 
dim result as string

Select Case As Const opcode
	Case &h00
                result="NOP"
                
	Case &h01
                result="ld bc,xx"
                
	Case &h02
                result="ld (bc),a"
                
	Case &h03
                result="inc BC"
	Case &h04
                result="inc b"
	Case &h05
                result="dec b"
	Case &h06
                result="ld b,x"
	Case &h07
                result="rlca"
                
	Case &h08
                result="ex af,af'"
                
	Case &h09
                result="add hl,bc"
                
	Case &h0A
                result="ld a,(BC)"
                
	Case &h0B
                result="dec bc"
	Case &h0C
                result="inc c"
                
	Case &h0D
                result="dec c"
                
	Case &h0E
                result="ld  c,x"
                
	Case &h0F
                result="rrca"
                
	Case &h10
                result="djnz x"
    
	Case &h11
                result="ld DE,xx"
                
	Case &h12
                result="ld (DE),a"
                
	Case &h13
                result="inc DE"
	Case &h14
                result="inc d"
	Case &h15
                result="dec d"
	Case &h16
                result="ld d,x"
	Case &h17
                result="rla"
                
	Case &h18
                result="jr x"
                
	Case &h19
                result="add HL,DE"
                
	Case &h1A
                result="ld a,(DE)"
                
	Case &h1B
                result="dec DE"
	Case &h1C
                result="inc e"
                
	Case &h1D
                result="dec e"
                
	Case &h1E
                result="ld e,x"
                
	Case &h1F
                result="rra"
                
	Case &h20
                result="jr nz, x"
                
	Case &h21
                result="ld HL,xx"
                
	Case &h22
                result="ld (xx),HL"
                
	Case &h23
                result="inc HL"
	Case &h24
                result="inc h"
	Case &h25
                result="dec h"
	Case &h26
                result="ld h,x"
	Case &h27
                result="daa"
	Case &h28
                result="jr z,x"
                
	Case &h29
                result="add HL,HL"
                
	Case &h2A
                result="ld HL,(xx)"
                
	Case &h2B
                result="dec HL"
	Case &h2C
                result="inc l"
                
	Case &h2D
                result="dec l"
                
	Case &h2E
                result="ld l,x"
                
	Case &h2F
                result="cpl"
                
	Case &h30
                result="jr nc, x"
                
	Case &h31
                result="ld Stack,xx"
                
	Case &h32
                result="ld (xx),a"
                
	Case &h33
                result="inc Stack"
	Case &h34
                result="inc (hl)"
	Case &h35
                result="dec (hl)"
	Case &h36
                result="ld (hl),x"
	Case &h37
                result="scf"
                
	Case &h38
                result="jr c,x"
                
	Case &h39
                result="add HL,Stack"
                
	Case &h3A
                result="ld a,(xx)"
                
	Case &h3B
                result="dec Stack"
	Case &h3C
                result="inc a"
                
	Case &h3D
                result="dec a"
                
	Case &h3E
                result="ld  a,x"
                
	Case &h3F
                result="ccf"
                
	Case &h40
                result="ld b,b"
                
	Case &h41
                result="ld b,c"
                
	Case &h42
                result="ld b,d"
                
	Case &h43
                result="ld b,e"
                
	Case &h44
                result="ld b,h"
                
	Case &h45
                result="ld b,l"
                
	Case &h46
                result="ld b,(hl)"
                
	Case &h47
                result="ld b,a"
                
	Case &h48
                result="ld c,b"
                
	Case &h49
                result="ld c,c"
                
	Case &h4A
                result="ld c,d"
                
	Case &h4B
                result="ld c,e"
                
	Case &h4C
                result="ld c,h"
                
	Case &h4D
                result="ld c,l"
                
	Case &h4E
                result="ld c,(hl)"
                
	Case &h4F
                result="ld c,a"
                
	Case &h50
                result="ld d,b"
                
	Case &h51
                result="ld d,c"
                
	Case &h52
                result="ld d,d"
                
	Case &h53
                result="ld d,e"
                
	Case &h54
                result="ld d,h"
                
	Case &h55
                result="ld d,l"
                
	Case &h56
                result="ld d,(hl)"
                
	Case &h57
                result="ld d,a"
                
	Case &h58
                result="ld e,b"
                
	Case &h59
                result="ld e,c"
                
	Case &h5A
                result="ld e,d"
                
	Case &h5B
                result="ld e,e"
                
	Case &h5C
                result="ld e,h"
                
	Case &h5D
                result="ld e,l"
                
	Case &h5E
                result="ld e,(hl)"
                
	Case &h5F
                result="ld e,a"
                
	Case &h60
                result="ld h,b"
                
	Case &h61
                result="ld h,c"
                
	Case &h62
                result="ld h,d"
                
	Case &h63
                result="ld h,e"
                
	Case &h64
                result="ld h,h"
                
	Case &h65
                result="ld h,l"
                
	Case &h66
                result="ld h,(hl)"
                
	Case &h67
                result="ld h,a"
                
	Case &h68
                result="ld l,b"
                
	Case &h69
                result="ld l,c"
                
	Case &h6A
                result="ld l,d"
                
	Case &h6B
                result="ld l,e"
                
	Case &h6C
                result="ld l,h"
                
	Case &h6D
                result="ld l,l"
                
	Case &h6E
                result="ld l,(hl)"
                
	Case &h6F
                result="ld l,a"
                
	Case &h70
                result="ld (hl),b"
                
	Case &h71
                result="ld (hl),c"
                
	Case &h72
                result="ld (hl),d"
                
	Case &h73
                result="ld (hl),e"
                
	Case &h74
                result="ld (hl),h"
                
	Case &h75
                result="ld (hl),l"
                
	Case &h76
                result="halt"

	Case &h77
                result="ld (hl),a"
                
	Case &h78
                result="ld a,b"
                
	Case &h79
                result="ld a,c"
                
	Case &h7A
                result="ld a,d"
                
	Case &h7B
                result="ld a,e"
                
	Case &h7C
                result="ld a,h"
                
	Case &h7D
                result="ld a,l"
                
	Case &h7E
                result="ld a,(hl)"
                
	Case &h7F
                result="ld a,a"
                
	Case &h80
                result="add a,b"
                
	Case &h81
                result="add a,c"
                
	Case &h82
                result="add a,d"
                
	Case &h83
                result="add a,e"
                
	Case &h84
                result="add a,h"
                
	Case &h85
                result="add a,l"
                
	Case &h86
                result="add a,(hl)"
                
	Case &h87
                result="add a,a"
                
	Case &h88
                result="adc a,b"
                
	Case &h89
                result="adc a,c"
                
	Case &h8A
                result="adc a,d"
                
	Case &h8B
                result="adc a,e"
                
	Case &h8C
                result="adc a,h"
                
	Case &h8D
                result="adc a,l"
                
	Case &h8E
                result="adc a,(hl)"
                
	Case &h8F
                result="adc a,a"
                
	Case &h90
                result="sub a,b"
          
	Case &h91
                result="sub a,c"
                
	Case &h92
                result="sub a,d"
                
	Case &h93
                result="sub a,e"
                
	Case &h94
                result="sub a,h"
                
	Case &h95
                result="sub a,l"
                
	Case &h96
                result="sub a,(hl)"
                
	Case &h97
                result="sub a,a"
                
	Case &h98
                result="sbc a,b"
                
	Case &h99
                result="sbc a,c"
                
	Case &h9A
                result="sbc a,d"
                
	Case &h9B
                result="sbc a,e"
                
	Case &h9C
                result="sbc a,h"
                
	Case &h9D
                result="sbc a,l"
                
	Case &h9E
                result="sbc a,(hl)"
                
	Case &h9F
                result="sbc a,a"
                
	Case &hA0
                result="and b"
                
	Case &hA1
                result="and c"
                
	Case &hA2
                result="and d"
                
	Case &hA3
                result="and e"
                
	Case &hA4
                result="and h"
                
	Case &hA5
                result="and l"
                
	Case &hA6
                result="and (hl)"
                
	Case &hA7
                result="and a"
                
	Case &hA8
                result="xor b"
                
	Case &hA9
                result="xor c"
                
	Case &hAA
                result="xor d"
                
	Case &hAB
                result="xor e"
                
	Case &hAC
                result="xor h"
                
	Case &hAD
                result="xor l"
                
	Case &hAE
                result="xor (hl)"
                
	Case &hAF
                result="xor a"
                
	Case &hB0
                result="or b"
                
	Case &hB1
                result="or c"
                
	Case &hB2
                result="or d"
                
	Case &hB3
                result="or e"
                
	Case &hB4
                result="or h"
                
	Case &hB5
                result="or l"
                
	Case &hB6
                result="or (hl)"
                
	Case &hB7
                result="or a"
                
	Case &hB8
                result="cp b"
                
	Case &hB9
                result="cp c"
                
	Case &hBA
                result="cp d"
                
	Case &hBB
                result="cp e"
                
	Case &hBC
                result="cp h"
                
	Case &hBD
                result="cp l"
                
	Case &hBE
                result="cp (hl)"
                
	Case &hBF
                result="cp a"
                
	Case &hC0
                result="ret nz"
	Case &hC1
                result="pop bc"
	Case &hC2
                result="jp nz,xx"
	Case &hC3
                result="jp xx"
	Case &hC4
                result="call nz,xx"
   
	Case &hC5
                result="push bc"
	Case &hC6
                result="add a,x"
                
	Case &hC7
                result="RST 00H"
	Case &hC8
                result="ret z"
	Case &hC9
                result="ret"
	Case &hCA
                result="jp z,xx"

	Case &hCB
                result="xxBITxx"

	Case &hCC
                result="call z,xx"

	Case &hCD
                result="call xx"
	Case &hCE
                result="xor x"
                
	Case &hCF
                result="RST 08H"
	Case &hD0
                result="ret nc"
	Case &hD1
                result="pop de"
	Case &hD2
                result="jp nc,xx"

	Case &hD3
                result="out (x),a"

	Case &hD4
                result="call nc,xx"

	Case &hD5
                result="push de"
	Case &hD6
                result="sub x"
                
	Case &hD7
                result="RST 10H"
	Case &hD8
                result="ret c"
	Case &hD9
                result="exx"
	Case &hDA
                result="jp c,xx"

	Case &hDB
                result="in a,(x)"

	Case &hDC
                result="call c,xx"

	Case &hDD
                result="xxIXxx"

	Case &hDE
                result="sbc a,x"
                
	Case &hDF
                result="RST 18H"
	Case &hE0
                result="ret po"
	Case &hE1
                result="pop hl"
	Case &hE2
                result="jp po,xx"

	Case &hE3
                result="ex (sp),hl"
	Case &hE4
                result="call po,xx"

	Case &hE5
                result="push hl"
	Case &hE6
                result="and x"
                
	Case &hE7
                result="RST 20H"
	Case &hE8
                result="ret pe"
	Case &hE9
                result="jp (hl)"
	Case &hEA
                result="jp pe,xx"
	Case &hEB
                result="ex de,hl"
	Case &hEC
                result="call pe,xx"

	Case &hED
				rem After ED.
				rem Peek at the next opcode and disassemble that instead. 
                result=DisED(peekPC())

	Case &hEE
                result="adc a,x"
                
	Case &hEF
                result="RST 28H"
	Case &hF0
                result="ret p"
	Case &hF1
                result="pop af"
	Case &hF2
                result="jp p,xx"
	Case &hF3
                result="di"

	Case &hF4
                result="call p,xx"
	Case &hF5
                result="push af"
	Case &hF6
                result="or x"               
	Case &hF7
                result="RST 30H"
	Case &hF8
                result="ret m"
	Case &hF9
                result="ld  sp,hl"
	Case &hFA
                result="jp m,xx"
	Case &hFB
                result="ei"
	Case &hFC
                result="call m,xx"
	Case &hFD
                result="xxIYxx"
	Case &hFE
                result="cp x"               
	Case &hFF


	Case Else
		result="nocode"
End Select

result=result+"                "
result=left(result,16)

return result
End function



Function CB () as integer
rem: Return a binary result - 0 = no opcode NextPCuted or error.
rem After CB extended instructions. Called from M1 which processed the first byte.
rem Bitwise Instructions. 

dim opcode as integer
dim result as integer

opcode=NextPC()
result=1

Select Case As Const opcode
	Case &h00
		rem rlc b
		b=rlc(b)
		BakeBC()
		rem Complete
	
	Case &h01
		rem rlc c
		c=rlc(c)
		BakeBC()
		rem Complete

	Case &h02
		rem rlc d
		d=rlc(d)
		BakeDE()
		rem Complete
		
	Case &h03
		rem rlc e
		e=rlc(e)
		BakeDE()
		rem Complete
		
	Case &h04
		rem rlc h
		h=rlc(h)
		BakeHL()
		rem Complete
		
	Case &h05
		rem rlc l
		l=rlc(l)
		BakeHL()
		rem Complete
		
	Case &h06
		rem rlc (hl)
		set(hl,rlc(fetch(hl)))
		rem Complete		
		
	Case &h07
		rem rlc a
		a=rlc(a)
		rem Complete		
			
		Case &h08
		rem rrc b
		b=rrc(b)
		BakeBC()
		rem Complete
	
	Case &h09
		rem rrc c
		c=rrc(c)
		BakeBC()
		rem Complete

	Case &h0A
		rem rrc d
		d=rrc(d)
		BakeDE()
		rem Complete
		
	Case &h0B
		rem rrc e
		e=rrc(e)
		BakeDE()
		rem Complete
		
	Case &h0C
		rem rrc h
		h=rrc(h)
		BakeHL()
		rem Complete
		
	Case &h0D
		rem rrc l
		l=rrc(l)
		BakeHL()
		rem Complete
		
	Case &h0E
		rem rrc (hl)
		set(hl,rrc(fetch(hl)))
		rem Complete		
		
	Case &h0F
		rem rrc a
		a=rrc(a)
		rem Complete	

		Case &h10
		rem rl b
		b=rl(b)
		BakeBC()
		rem Complete
	
	Case &h11
		rem rl c
		c=rl(c)
		BakeBC()
		rem Complete

	Case &h12
		rem rl d
		d=rl(d)
		BakeDE()
		rem Complete
		
	Case &h13
		rem rl e
		e=rl(e)
		BakeDE()
		rem Complete
		
	Case &h14
		rem rl h
		h=rl(h)
		BakeHL()
		rem Complete
		
	Case &h15
		rem rl l
		l=rl(l)
		BakeHL()
		rem Complete
		
	Case &h16
		rem rl (hl)
		set(hl,rl(fetch(hl)))
		rem Complete		
		
	Case &h17
		rem rl a
		a=rl(a)
		rem Complete		
			
		Case &h18
		rem rr b
		b=rr(b)
		BakeBC()
		rem Complete
	
	Case &h19
		rem rr c
		c=rr(c)
		BakeBC()
		rem Complete

	Case &h1A
		rem rr d
		d=rr(d)
		BakeDE()
		rem Complete
		
	Case &h1B
		rem rr e
		e=rr(e)
		BakeDE()
		rem Complete
		
	Case &h1C
		rem rr h
		h=rr(h)
		BakeHL()
		rem Complete
		
	Case &h1D
		rem rr l
		l=rr(l)
		BakeHL()
		rem Complete
		
	Case &h1E
		rem rr (hl)
		set(hl,rr(fetch(hl)))
		rem Complete		
		
	Case &h1F
		rem rr a
		a=rr(a)
		rem Complete	
		
	Case &h20
		rem sla b
		b=sla(b)
		BakeBC()
		rem Complete
	
	Case &h21
		rem sla c
		c=sla(c)
		BakeBC()
		rem Complete

	Case &h22
		rem sla d
		d=sla(d)
		BakeDE()
		rem Complete
		
	Case &h23
		rem sla e
		e=sla(e)
		BakeDE()
		rem Complete
		
	Case &h24
		rem sla h
		h=sla(h)
		BakeHL()
		rem Complete
		
	Case &h25
		rem sla l
		l=sla(l)
		BakeHL()
		rem Complete
		
	Case &h26
		rem sla (hl)
		set(hl,sla(fetch(hl)))
		rem Complete		
		
	Case &h27
		rem sla a
		a=sla(a)
		rem Complete		
			
		Case &h28
		rem sra b
		b=sra(b)
		BakeBC()
		rem Complete
	
	Case &h29
		rem sra c
		c=sra(c)
		BakeBC()
		rem Complete

	Case &h2A
		rem sra d
		d=sra(d)
		BakeDE()
		rem Complete
		
	Case &h2B
		rem sra e
		e=sra(e)
		BakeDE()
		rem Complete
		
	Case &h2C
		rem sra h
		h=sra(h)
		BakeHL()
		rem Complete
		
	Case &h2D
		rem sra l
		l=sra(l)
		BakeHL()
		rem Complete
		
	Case &h2E
		rem sra (hl)
		set(hl,sra(fetch(hl)))
		rem Complete		
		
	Case &h2F
		rem sra a
		a=sra(a)
		rem Complete	

	Case &h30
		rem sll b
		b=sll(b)
		BakeBC()
		rem Complete
	
	Case &h31
		rem sll c
		c=sll(c)
		BakeBC()
		rem Complete

	Case &h32
		rem sll d
		d=sll(d)
		BakeDE()
		rem Complete
		
	Case &h33
		rem sll e
		e=sll(e)
		BakeDE()
		rem Complete
		
	Case &h34
		rem sll h
		h=sll(h)
		BakeHL()
		rem Complete
		
	Case &h35
		rem sll l
		l=sll(l)
		BakeHL()
		rem Complete
		
	Case &h36
		rem sll (hl)
		set(hl,sll(fetch(hl)))
		rem Complete		
		
	Case &h37
		rem sll a
		a=sll(a)
		rem Complete		
			
		Case &h38
		rem srl b
		b=srl(b)
		BakeBC()
		rem Complete
	
	Case &h39
		rem srl c
		c=srl(c)
		BakeBC()
		rem Complete

	Case &h3A
		rem srl d
		d=srl(d)
		BakeDE()
		rem Complete
		
	Case &h3B
		rem srl e
		e=srl(e)
		BakeDE()
		rem Complete
		
	Case &h3C
		rem srl h
		h=srl(h)
		BakeHL()
		rem Complete
		
	Case &h3D
		rem srl l
		l=srl(l)
		BakeHL()
		rem Complete
		
	Case &h3E
		rem srl (hl)
		set(hl,srl(fetch(hl)))
		rem Complete		
		
	Case &h3F
		rem srl a
		a=srl(a)
		rem Complete	

	Case &h40
		rem bit 0,b
		z=BitX(0,b)
		BakeBC()
		rem Complete
	
	Case &h41
		rem bit 0,c
		z=BitX(0,c)
		BakeBC()
		rem Complete

	Case &h42
		rem bit 0,d
		z=BitX(0,d)
		BakeDE()
		rem Complete
		
	Case &h43
		rem bit 0,e
		z=BitX(0,e)
		BakeDE()
		rem Complete
		
	Case &h44
		rem bit 0,h
		z=BitX(0,h)
		BakeHL()
		rem Complete
		
	Case &h45
		rem bit 0,l
		z=BitX(0,l)
		BakeHL()
		rem Complete
		
	Case &h46
		rem bit 0,(hl)
		z=BitX(0,fetch(hl))
		rem Complete		
		
	Case &h47
		rem bit 0,a
		z=BitX(0,a)
		rem Complete		
			
		Case &h48
		rem bit 1,b
		z=BitX(1,b)
		BakeBC()
		rem Complete
	
	Case &h49
		rem bit 1,c
		z=BitX(1,c)
		BakeBC()
		rem Complete

	Case &h4A
		rem bit 1,d
		z=BitX(1,d)
		BakeDE()
		rem Complete
		
	Case &h4B
		rem bit 1,e
		z=BitX(1,e)
		BakeDE()
		rem Complete
		
	Case &h4C
		rem bit 1,h
		z=BitX(1,h)
		BakeHL()
		rem Complete
		
	Case &h4D
		rem bit 1,l
		z=BitX(1,l)
		BakeHL()
		rem Complete
		
	Case &h4E
		rem bit 1,(hl)
		z=BitX(1,fetch(hl))
		rem Complete		
		
	Case &h4F
		rem bit 1,a
		z=BitX(1,a)
		rem Complete	
		
	Case &h50
		rem bit 2,b
		z=BitX(2,b)
		BakeBC()
		rem Complete
	
	Case &h51
		rem Bit 2,c
		z=BitX(2,c)
		BakeBC()
		rem Complete

	Case &h52
		rem Bit 2,d
		z=BitX(2,d)
		BakeDE()
		rem Complete
		
	Case &h53
		rem Bit 2,e
		z=BitX(2,e)
		BakeDE()
		rem Complete
		
	Case &h54
		rem Bit 2,h
		z=BitX(2,h)
		BakeHL()
		rem Complete
		
	Case &h55
		rem Bit 2,l
		z=BitX(2,l)
		BakeHL()
		rem Complete
		
	Case &h56
		rem Bit 2,(hl)
		z=BitX(2,fetch(hl))
		rem Complete		
		
	Case &h57
		rem Bit 2,a
		z=BitX(2,a)
		rem Complete		
			
		Case &h58
		rem bit 3,b
		z=BitX(3,b)
		BakeBC()
		rem Complete
	
	Case &h59
		rem Bit 3,c
		z=BitX(3,c)
		BakeBC()
		rem Complete

	Case &h5A
		rem Bit 3,d
		z=BitX(3,d)
		BakeDE()
		rem Complete
		
	Case &h5B
		rem Bit 3,e
		z=BitX(3,e)
		BakeDE()
		rem Complete
		
	Case &h5C
		rem Bit 3,h
		z=BitX(3,h)
		BakeHL()
		rem Complete
		
	Case &h5D
		rem Bit 3,l
		z=BitX(3,l)
		BakeHL()
		rem Complete
		
	Case &h5E
		rem Bit 3,(hl)
		z=BitX(3,fetch(hl))
		rem Complete		
		
	Case &h5F
		rem Bit 3,a
		z=BitX(3,a)
		rem Complete	
		
	Case &h60
		rem bit 4,b
		z=BitX(4,b)
		BakeBC()
		rem Complete
	
	Case &h61
		rem bit 4,c
		z=BitX(4,c)
		BakeBC()
		rem Complete

	Case &h62
		rem bit 4,d
		z=BitX(4,d)
		BakeDE()
		rem Complete
		
	Case &h63
		rem bit 4,e
		z=BitX(4,e)
		BakeDE()
		rem Complete
		
	Case &h64
		rem bit 4,h
		z=BitX(4,h)
		BakeHL()
		rem Complete
		
	Case &h65
		rem bit 4,l
		z=BitX(4,l)
		BakeHL()
		rem Complete
		
	Case &h66
		rem bit 4,(hl)
		z=BitX(4,fetch(hl))
		rem Complete		
		
	Case &h67
		rem bit 4,a
		z=BitX(4,a)
		rem Complete		
			
		Case &h68
		rem bit 5,b
		z=BitX(5,b)
		BakeBC()
		rem Complete
	
	Case &h69
		rem bit 5,c
		z=BitX(5,c)
		BakeBC()
		rem Complete

	Case &h6A
		rem bit 5,d
		z=BitX(5,d)
		BakeDE()
		rem Complete
		
	Case &h6B
		rem bit 5,e
		z=BitX(5,e)
		BakeDE()
		rem Complete
		
	Case &h6C
		rem bit 5,h
		z=BitX(5,h)
		BakeHL()
		rem Complete
		
	Case &h6D
		rem bit 5,l
		z=BitX(5,l)
		BakeHL()
		rem Complete
		
	Case &h6E
		rem bit 5,(hl)
		z=BitX(5,fetch(hl))
		rem Complete		
		
	Case &h6F
		rem bit 5,a
		z=BitX(5,a)
		rem Complete	
		
	Case &h70
		rem bit 6,b
		z=BitX(6,b)
		BakeBC()
		rem Complete
	
	Case &h71
		rem bit 6,c
		z=BitX(6,c)
		BakeBC()
		rem Complete

	Case &h72
		rem bit 6,d
		z=BitX(6,d)
		BakeDE()
		rem Complete
		
	Case &h73
		rem bit 6,e
		z=BitX(6,e)
		BakeDE()
		rem Complete
		
	Case &h74
		rem bit 6,h
		z=BitX(6,h)
		BakeHL()
		rem Complete
		
	Case &h75
		rem bit 6,l
		z=BitX(6,l)
		BakeHL()
		rem Complete
		
	Case &h76
		rem bit 6,(hl)
		z=BitX(6,fetch(hl))
		rem Complete		
		
	Case &h77
		rem bit 6,a
		z=BitX(6,a)
		rem Complete		
			
		Case &h78
		rem bit 7,b
		z=BitX(7,b)
		BakeBC()
		rem Complete
	
	Case &h79
		rem bit 7,c
		z=BitX(7,c)
		BakeBC()
		rem Complete

	Case &h7A
		rem bit 7,d
		z=BitX(7,d)
		BakeDE()
		rem Complete
		
	Case &h7B
		rem bit 7,e
		z=BitX(7,e)
		BakeDE()
		rem Complete
		
	Case &h7C
		rem bit 7,h
		z=BitX(7,h)
		BakeHL()
		rem Complete
		
	Case &h7D
		rem bit 7,l
		z=BitX(7,l)
		BakeHL()
		rem Complete
		
	Case &h7E
		rem bit 7,(hl)
		z=BitX(7,fetch(hl))
		rem Complete		
		
	Case &h7F
		rem bit 7,a
		z=BitX(7,a)
		rem Complete
		

	Case &h80
		rem res 0,b
		b=ResetBit(0,b)
		BakeBC()
		rem Complete
	
	Case &h81
		rem res 0,c
		c=ResetBit(0,c)
		BakeBC()
		rem Complete

	Case &h82
		rem res 0,d
		d=ResetBit(0,d)
		BakeDE()
		rem Complete
		
	Case &h83
		rem res 0,e
		e=ResetBit(0,e)
		BakeDE()
		rem Complete
		
	Case &h84
		rem res 0,h
		h=ResetBit(0,h)
		BakeHL()
		rem Complete
		
	Case &h85
		rem res 0,l
		l=ResetBit(0,l)
		BakeHL()
		rem Complete
		
	Case &h86
		rem res 0,(hl)
		set(hl,ResetBit(0,fetch(hl)))
		rem Complete		
		
	Case &h87
		rem res 0,a
		a=ResetBit(0,a)
		rem Complete		
			
		Case &h88
		rem res 1,b
		b=ResetBit(1,b)
		BakeBC()
		rem Complete
	
	Case &h89
		rem res 1,c
		c=ResetBit(1,c)
		BakeBC()
		rem Complete

	Case &h8A
		rem res 1,d
		d=ResetBit(1,d)
		BakeDE()
		rem Complete
		
	Case &h8B
		rem res 1,e
		e=ResetBit(1,e)
		BakeDE()
		rem Complete
		
	Case &h8C
		rem res 1,h
		h=ResetBit(1,h)
		BakeHL()
		rem Complete
		
	Case &h8D
		rem res 1,l
		l=ResetBit(1,l)
		BakeHL()
		rem Complete
		
	Case &h8E
		rem res 1,(hl)
		set(hl,ResetBit(1,fetch(hl)))
		rem Complete		
		
	Case &h8F
		rem res 1,a
		a=ResetBit(1,a)
		rem Complete	
		
	Case &h90
		rem res 2,b
		b=ResetBit(2,b)
		BakeBC()
		rem Complete
	
	Case &h91
		rem res 2,c
		c=ResetBit(2,c)
		BakeBC()
		rem Complete

	Case &h92
		rem res 2,d
		d=ResetBit(2,d)
		BakeDE()
		rem Complete
		
	Case &h93
		rem res 2,e
		e=ResetBit(2,e)
		BakeDE()
		rem Complete
		
	Case &h94
		rem res 2,h
		h=ResetBit(2,h)
		BakeHL()
		rem Complete
		
	Case &h95
		rem res 2,l
		l=ResetBit(2,l)
		BakeHL()
		rem Complete
		
	Case &h96
		rem res 2,(hl)
		set(hl,ResetBit(2,fetch(hl)))
		rem Complete		
		
	Case &h97
		rem res 2,a
		a=ResetBit(2,a)
		rem Complete		
			
		Case &h98
		rem res 3,b
		b=ResetBit(3,b)
		BakeBC()
		rem Complete
	
	Case &h99
		rem res 3,c
		c=ResetBit(3,c)
		BakeBC()
		rem Complete

	Case &h9A
		rem res 3,d
		d=ResetBit(3,d)
		BakeDE()
		rem Complete
		
	Case &h9B
		rem res 3,e
		e=ResetBit(3,e)
		BakeDE()
		rem Complete
		
	Case &h9C
		rem res 3,h
		h=ResetBit(3,h)
		BakeHL()
		rem Complete
		
	Case &h9D
		rem res 3,l
		l=ResetBit(3,l)
		BakeHL()
		rem Complete
		
	Case &h9E
		rem res 3,(hl)
		set(hl,ResetBit(3,fetch(hl)))
		rem Complete		
		
	Case &h9F
		rem res 3,a
		a=ResetBit(3,a)
		rem Complete	
		
	Case &hA0
		rem res 4,b
		b=ResetBit(4,b)
		BakeBC()
		rem Complete
	
	Case &hA1
		rem res 4,c
		c=ResetBit(4,c)
		BakeBC()
		rem Complete

	Case &hA2
		rem res 4,d
		d=ResetBit(4,d)
		BakeDE()
		rem Complete
		
	Case &hA3
		rem res 4,e
		e=ResetBit(4,e)
		BakeDE()
		rem Complete
		
	Case &hA4
		rem res 4,h
		h=ResetBit(4,h)
		BakeHL()
		rem Complete
		
	Case &hA5
		rem res 4,l
		l=ResetBit(4,l)
		BakeHL()
		rem Complete
		
	Case &hA6
		rem res 4,(hl)
		set(hl,ResetBit(4,fetch(hl)))
		rem Complete		
		
	Case &hA7
		rem res 4,a
		a=ResetBit(4,a)
		rem Complete		
			
		Case &hA8
		rem res 5,b
		b=ResetBit(5,b)
		BakeBC()
		rem Complete
	
	Case &hA9
		rem res 5,c
		c=ResetBit(5,c)
		BakeBC()
		rem Complete

	Case &hAA
		rem res 5,d
		d=ResetBit(5,d)
		BakeDE()
		rem Complete
		
	Case &hAB
		rem res 5,e
		e=ResetBit(5,e)
		BakeDE()
		rem Complete
		
	Case &hAC
		rem res 5,h
		h=ResetBit(5,h)
		BakeHL()
		rem Complete
		
	Case &hAD
		rem res 5,l
		l=ResetBit(5,l)
		BakeHL()
		rem Complete
		
	Case &hAE
		rem res 5,(hl)
		set(hl,ResetBit(5,fetch(hl)))
		rem Complete		
		
	Case &hAF
		rem res 5,a
		a=ResetBit(5,a)
		rem Complete	
		
	Case &hB0
		rem res 6,b
		b=ResetBit(6,b)
		BakeBC()
		rem Complete
	
	Case &hB1
		rem res 6,c
		c=ResetBit(6,c)
		BakeBC()
		rem Complete

	Case &hB2
		rem res 6,d
		d=ResetBit(6,d)
		BakeDE()
		rem Complete
		
	Case &hB3
		rem res 6,e
		e=ResetBit(6,e)
		BakeDE()
		rem Complete
		
	Case &hB4
		rem res 6,h
		h=ResetBit(6,h)
		BakeHL()
		rem Complete
		
	Case &hB5
		rem res 6,l
		l=ResetBit(6,l)
		BakeHL()
		rem Complete
		
	Case &hB6
		rem res 6,(hl)
		set(hl,ResetBit(6,fetch(hl)))
		rem Complete		
		
	Case &hB7
		rem res 6,a
		a=ResetBit(6,a)
		rem Complete		
			
		Case &hB8
		rem res 7,b
		b=ResetBit(7,b)
		BakeBC()
		rem Complete
	
	Case &hB9
		rem res 7,c
		c=ResetBit(7,c)
		BakeBC()
		rem Complete

	Case &hBA
		rem res 7,d
		d=ResetBit(7,d)
		BakeDE()
		rem Complete
		
	Case &hBB
		rem res 7,e
		e=ResetBit(7,e)
		BakeDE()
		rem Complete
		
	Case &hBC
		rem res 7,h
		h=ResetBit(7,h)
		BakeHL()
		rem Complete
		
	Case &hBD
		rem res 7,l
		l=ResetBit(7,l)
		BakeHL()
		rem Complete
		
	Case &hBE
		rem res 7,(hl)
		set(hl,ResetBit(7,fetch(hl)))
		rem Complete		
		
	Case &hBF
		rem res 7,a
		a=ResetBit(7,a)
		rem Complete
		

	Case &hC0
		rem set 0,b
		b=SetBit(0,b)
		BakeBC()
		rem Complete
	
	Case &hC1
		rem set 0,c
		c=SetBit(0,c)
		BakeBC()
		rem Complete

	Case &hC2
		rem set 0,d
		d=SetBit(0,d)
		BakeDE()
		rem Complete
		
	Case &hC3
		rem set 0,e
		e=SetBit(0,e)
		BakeDE()
		rem Complete
		
	Case &hC4
		rem set 0,h
		h=SetBit(0,h)
		BakeHL()
		rem Complete
		
	Case &hC5
		rem set 0,l
		l=SetBit(0,l)
		BakeHL()
		rem Complete
		
	Case &hC6
		rem set 0,(hl)
		set(hl,SetBit(0,fetch(hl)))
		rem Complete		
		
	Case &hC7
		rem set 0,a
		a=SetBit(0,a)
		rem Complete		
			
		Case &hC8
		rem set 1,b
		b=SetBit(1,b)
		BakeBC()
		rem Complete
	
	Case &hC9
		rem set 1,c
		c=SetBit(1,c)
		BakeBC()
		rem Complete

	Case &hCA
		rem set 1,d
		d=SetBit(1,d)
		BakeDE()
		rem Complete
		
	Case &hCB
		rem set 1,e
		e=SetBit(1,e)
		BakeDE()
		rem Complete
		
	Case &hCC
		rem set 1,h
		h=SetBit(1,h)
		BakeHL()
		rem Complete
		
	Case &hCD
		rem set 1,l
		l=SetBit(1,l)
		BakeHL()
		rem Complete
		
	Case &hCE
		rem set 1,(hl)
		set(hl,SetBit(1,fetch(hl)))
		rem Complete		
		
	Case &hCF
		rem set 1,a
		a=SetBit(1,a)
		rem Complete	
		
	Case &hD0
		rem set 2,b
		b=SetBit(2,b)
		BakeBC()
		rem Complete
	
	Case &hD1
		rem set 2,c
		c=SetBit(2,c)
		BakeBC()
		rem Complete

	Case &hD2
		rem set 2,d
		d=SetBit(2,d)
		BakeDE()
		rem Complete
		
	Case &hD3
		rem set 2,e
		e=SetBit(2,e)
		BakeDE()
		rem Complete
		
	Case &hD4
		rem set 2,h
		h=SetBit(2,h)
		BakeHL()
		rem Complete
		
	Case &hD5
		rem set 2,l
		l=SetBit(2,l)
		BakeHL()
		rem Complete
		
	Case &hD6
		rem set 2,(hl)
		set(hl,SetBit(2,fetch(hl)))
		rem Complete		
		
	Case &hD7
		rem set 2,a
		a=SetBit(2,a)
		rem Complete		
			
		Case &hD8
		rem set 3,b
		b=SetBit(3,b)
		BakeBC()
		rem Complete
	
	Case &hD9
		rem set 3,c
		c=SetBit(3,c)
		BakeBC()
		rem Complete

	Case &hDA
		rem set 3,d
		d=SetBit(3,d)
		BakeDE()
		rem Complete
		
	Case &hDB
		rem set 3,e
		e=SetBit(3,e)
		BakeDE()
		rem Complete
		
	Case &hDC
		rem set 3,h
		h=SetBit(3,h)
		BakeHL()
		rem Complete
		
	Case &hDD
		rem set 3,l
		l=SetBit(3,l)
		BakeHL()
		rem Complete
		
	Case &hDE
		rem set 3,(hl)
		set(hl,SetBit(3,fetch(hl)))
		rem Complete		
		
	Case &hDF
		rem set 3,a
		a=SetBit(3,a)
		rem Complete	
		
	Case &hE0
		rem set 4,b
		b=SetBit(4,b)
		BakeBC()
		rem Complete
	
	Case &hE1
		rem set 4,c
		c=SetBit(4,c)
		BakeBC()
		rem Complete

	Case &hE2
		rem set 4,d
		d=SetBit(4,d)
		BakeDE()
		rem Complete
		
	Case &hE3
		rem set 4,e
		e=SetBit(4,e)
		BakeDE()
		rem Complete
		
	Case &hE4
		rem set 4,h
		h=SetBit(4,h)
		BakeHL()
		rem Complete
		
	Case &hE5
		rem set 4,l
		l=SetBit(4,l)
		BakeHL()
		rem Complete
		
	Case &hE6
		rem set 4,(hl)
		set(hl,SetBit(4,fetch(hl)))
		rem Complete		
		
	Case &hE7
		rem set 4,a
		a=SetBit(4,a)
		rem Complete		
			
		Case &hE8
		rem set 5,b
		b=SetBit(5,b)
		BakeBC()
		rem Complete
	
	Case &hE9
		rem set 5,c
		c=SetBit(5,c)
		BakeBC()
		rem Complete

	Case &hEA
		rem set 5,d
		d=SetBit(5,d)
		BakeDE()
		rem Complete
		
	Case &hEB
		rem set 5,e
		e=SetBit(5,e)
		BakeDE()
		rem Complete
		
	Case &hEC
		rem set 5,h
		h=SetBit(5,h)
		BakeHL()
		rem Complete
		
	Case &hED
		rem set 5,l
		l=SetBit(5,l)
		BakeHL()
		rem Complete
		
	Case &hEE
		rem set 5,(hl)
		set(hl,SetBit(5,fetch(hl)))
		rem Complete		
		
	Case &hEF
		rem set 5,a
		a=SetBit(5,a)
		rem Complete	
		
	Case &hF0
		rem set 6,b
		b=SetBit(6,b)
		BakeBC()
		rem Complete
	
	Case &hF1
		rem set 6,c
		c=SetBit(6,c)
		BakeBC()
		rem Complete

	Case &hF2
		rem set 6,d
		d=SetBit(6,d)
		BakeDE()
		rem Complete
		
	Case &hF3
		rem set 6,e
		e=SetBit(6,e)
		BakeDE()
		rem Complete
		
	Case &hF4
		rem set 6,h
		h=SetBit(6,h)
		BakeHL()
		rem Complete
		
	Case &hF5
		rem set 6,l
		l=SetBit(6,l)
		BakeHL()
		rem Complete
		
	Case &hF6
		rem set 6,(hl)
		set(hl,SetBit(6,fetch(hl)))
		rem Complete		
		
	Case &hF7
		rem set 6,a
		a=SetBit(6,a)
		rem Complete		
			
	Case &hF8
		rem set 7,b
		b=SetBit(7,b)
		BakeBC()
		rem Complete
	
	Case &hF9
		rem set 7,c
		c=SetBit(7,c)
		BakeBC()
		rem Complete

	Case &hFA
		rem set 7,d
		d=SetBit(7,d)
		BakeDE()
		rem Complete
		
	Case &hFB
		rem set 7,e
		e=SetBit(7,e)
		BakeDE()
		rem Complete
		
	Case &hFC
		rem set 7,h
		h=SetBit(7,h)
		BakeHL()
		rem Complete
		
	Case &hFD
		rem set 7,l
		l=SetBit(7,l)
		BakeHL()
		rem Complete
		
	Case &hFE
		rem set 7,(hl)
		set(hl,SetBit(7,fetch(hl)))
		rem Complete		
		
	Case &hFF
		rem set 7,a
		a=SetBit(7,a)
		rem Complete		
		
	Case Else
		result=0
				rem PRINT "EMULATOR UNDOCUMENTED CODE."
End Select

return result
End function


Function DDCB () as integer
rem: Return a binary result - For IX instructions. Bitwise.
rem After CB extended instructions. Called from M1 which processed the first byte.
rem Bitwise Instructions. 

dim opcode as integer
dim result as integer
dim disp as integer	: rem Displacement.

disp=FixDisp(NextPC())		: rem Get displacement before Opcode.
opcode=NextPC()
result=1

Select Case As Const opcode

	Case &h00
		rem rlc(IX+disp),b
		b=rlc(fetch(IX+disp))
		BakeBC()
		rem complete
	
	Case &h01
		rem rlc(IX+disp),c
		c=rlc(fetch(IX+disp))
		BakeBC()
		rem complete

	Case &h02
		rem rlc(IX+disp),d
		d=rlc(fetch(IX+disp))
		BakeDE()
		rem complete
		
	Case &h03
		rem rlc(IX+disp),e
		e=rlc(fetch(IX+disp))
		BakeDE()
		rem complete
		
	Case &h04
		rem rlc(IX+disp),h
		h=rlc(fetch(IX+disp))
		BakeHL()
		rem complete
		
	Case &h05
		rem rlc(IX+disp),l
		l=rlc(fetch(IX+disp))
		BakeHL()
		rem complete
		
	Case &h06
		rem rlc (IX+disp)
		set(IX+disp,rlc(fetch(IX+disp)))
		rem complete		
		
	Case &h07
		rem rlc(IX+disp),a
		a=rlc(fetch(IX+disp))
		bakeaf()
		rem complete		
			
	Case &h08
		rem rrc(IX+disp),b
		b=rrc(fetch(IX+disp))
		BakeBC()
		rem complete
	
	Case &h09
		rem rrc(IX+disp),c
		c=rrc(fetch(IX+disp))
		BakeBC()
		rem complete

	Case &h0A
		rem rrc(IX+disp),d
		d=rrc(fetch(IX+disp))
		BakeDE()
		rem complete
		
	Case &h0B
		rem rrc(IX+disp),e
		e=rrc(fetch(IX+disp))
		BakeDE()
		rem complete
		
	Case &h0C
		rem rrc(IX+disp),h
		h=rrc(fetch(IX+disp))
		BakeHL()
		rem complete
		
	Case &h0D
		rem rrc(IX+disp),l
		l=rrc(fetch(IX+disp))
		BakeHL()
		rem complete
		
	Case &h0E
		rem rrc (IX+disp)
		set(IX+disp,rrc(fetch(IX+disp)))
		rem complete		
		
	Case &h0F
		rem rrc(IX+disp),a
		a=rrc(fetch(IX+disp))
		bakeaf()
		rem complete	

	Case &h10
		rem rl(IX+disp),b
		b=rl(fetch(IX+disp))
		BakeBC()
		rem complete
	
	Case &h11
		rem rl(IX+disp),c
		c=rl(fetch(IX+disp))
		BakeBC()
		rem complete

	Case &h12
		rem rl(IX+disp),d
		d=rl(fetch(IX+disp))
		BakeDE()
		rem complete
		
	Case &h13
		rem rl(IX+disp),e
		e=rl(fetch(IX+disp))
		BakeDE()
		rem complete
		
	Case &h14
		rem rl(IX+disp),h
		h=rl(fetch(IX+disp))
		BakeHL()
		rem complete
		
	Case &h15
		rem rl(IX+disp),l
		l=rl(fetch(IX+disp))
		BakeHL()
		rem complete
		
	Case &h16
		rem rl (IX+disp)
		set(IX+disp,rl(fetch(IX+disp)))
		rem complete		
		
	Case &h17
		rem rl(IX+disp),a
		a=rl(fetch(IX+disp))
		bakeaf()
		rem complete		
			
	Case &h18
		rem rr(IX+disp),b
		b=rr(fetch(IX+disp))
		BakeBC()
		rem complete
	
	Case &h19
		rem rr(IX+disp),c
		c=rr(fetch(IX+disp))
		BakeBC()
		rem complete

	Case &h1A
		rem rr(IX+disp),d
		d=rr(fetch(IX+disp))
		BakeDE()
		rem complete
		
	Case &h1B
		rem rr(IX+disp),e
		e=rr(fetch(IX+disp))
		BakeDE()
		rem complete
		
	Case &h1C
		rem rr(IX+disp),h
		h=rr(fetch(IX+disp))
		BakeHL()
		rem complete
		
	Case &h1D
		rem rr(IX+disp),l
		l=rr(fetch(IX+disp))
		BakeHL()
		rem complete
		
	Case &h1E
		rem rr (IX+disp)
		set(IX+disp,rr(fetch(IX+disp)))
		rem complete		
		
	Case &h1F
		rem rr(IX+disp),a
		a=rr(fetch(IX+disp))
		bakeaf()
		rem complete	
		
	Case &h20
		rem sla(IX+disp),b
		b=sla(fetch(IX+disp))
		BakeBC()
		rem complete
	
	Case &h21
		rem sla(IX+disp),c
		c=sla(fetch(IX+disp))
		BakeBC()
		rem complete

	Case &h22
		rem sla(IX+disp),d
		d=sla(fetch(IX+disp))
		BakeDE()
		rem complete
		
	Case &h23
		rem sla(IX+disp),e
		e=sla(fetch(IX+disp))
		BakeDE()
		rem complete
		
	Case &h24
		rem sla(IX+disp),h
		h=sla(fetch(IX+disp))
		BakeHL()
		rem complete
		
	Case &h25
		rem sla(IX+disp),l
		l=sla(fetch(IX+disp))
		BakeHL()
		rem complete
		
	Case &h26
		rem sla (IX+disp)
		set(IX+disp,sla(fetch(IX+disp)))
		rem complete		
		
	Case &h27
		rem sla(IX+disp),a
		a=sla(fetch(IX+disp))
		bakeaf()
		rem complete		
			
	Case &h28
		rem sra(IX+disp),b
		b=sra(fetch(IX+disp))
		BakeBC()
		rem complete
	
	Case &h29
		rem sra(IX+disp),c
		c=sra(fetch(IX+disp))
		BakeBC()
		rem complete

	Case &h2A
		rem sra(IX+disp),d
		d=sra(fetch(IX+disp))
		BakeDE()
		rem complete
		
	Case &h2B
		rem sra(IX+disp),e
		e=sra(fetch(IX+disp))
		BakeDE()
		rem complete
		
	Case &h2C
		rem sra(IX+disp),h
		h=sra(fetch(IX+disp))
		BakeHL()
		rem complete
		
	Case &h2D
		rem sra(IX+disp),l
		l=sra(fetch(IX+disp))
		BakeHL()
		rem complete
		
	Case &h2E
		rem sra (IX+disp)
		set(IX+disp,sra(fetch(IX+disp)))
		rem complete		
		
	Case &h2F
		rem sra(IX+disp),a
		a=sra(fetch(IX+disp))
		bakeaf()
		rem complete	

	Case &h30
		rem sll(IX+disp),b
		b=sll(fetch(IX+disp))
		BakeBC()
		rem complete
	
	Case &h31
		rem sll(IX+disp),c
		c=sll(fetch(IX+disp))
		BakeBC()
		rem complete

	Case &h32
		rem sll(IX+disp),d
		d=sll(fetch(IX+disp))
		BakeDE()
		rem complete
		
	Case &h33
		rem sll(IX+disp),e
		e=sll(fetch(IX+disp))
		BakeDE()
		rem complete
		
	Case &h34
		rem sll(IX+disp),h
		h=sll(fetch(IX+disp))
		BakeHL()
		rem complete
		
	Case &h35
		rem sll(IX+disp),l
		l=sll(fetch(IX+disp))
		BakeHL()
		rem complete
		
	Case &h36
		rem sll (IX+disp)
		set(IX+disp,sll(fetch(IX+disp)))
		rem complete		
		
	Case &h37
		rem sll(IX+disp),a
		a=sll(fetch(IX+disp))
		bakeaf()
		rem complete		
			
	Case &h38
		rem srl(IX+disp),b
		b=srl(fetch(IX+disp))
		BakeBC()
		rem complete
	
	Case &h39
		rem srl(IX+disp),c
		c=srl(fetch(IX+disp))
		BakeBC()
		rem complete

	Case &h3A
		rem srl(IX+disp),d
		d=srl(fetch(IX+disp))
		BakeDE()
		rem complete
		
	Case &h3B
		rem srl(IX+disp),e
		e=srl(fetch(IX+disp))
		BakeDE()
		rem complete
		
	Case &h3C
		rem srl(IX+disp),h
		h=srl(fetch(IX+disp))
		BakeHL()
		rem complete
		
	Case &h3D
		rem srl(IX+disp),l
		l=srl(fetch(IX+disp))
		BakeHL()
		rem complete
		
	Case &h3E
		rem srl (IX+disp)
		set(IX+disp,srl(fetch(IX+disp)))
		rem complete		
		
	Case &h3F
		rem srl(IX+disp),a
		a=srl(fetch(IX+disp))
		bakeaf()
		rem complete	
		
		
	Case &h40
		rem bit 0,(IX+disp)
		z=BitX(0,fetch(IX+disp))
		
		rem Complete
	
	Case &h41
		rem bit 0,(IX+disp)
		z=BitX(0,fetch(IX+disp))
		
		rem Complete

	Case &h42
		rem bit 0,(IX+disp)
		z=BitX(0,fetch(IX+disp))
		
		rem Complete
		
	Case &h43
		rem bit 0,(IX+disp)
		z=BitX(0,fetch(IX+disp))
		
		rem Complete
		
	Case &h44
		rem bit 0,(IX+disp)
		z=BitX(0,fetch(IX+disp))
		
		rem Complete
		
	Case &h45
		rem bit 0,(IX+disp)
		z=BitX(0,fetch(IX+disp))
		
		rem Complete
		
	Case &h46
		rem bit 0,(IX+disp)
		z=BitX(0,fetch(hl))
		rem Complete		
		
	Case &h47
		rem bit 0,(IX+disp)
		z=BitX(0,fetch(IX+disp))
		rem Complete		
			
	Case &h48
		rem bit 1,(IX+disp)
		z=BitX(1,fetch(IX+disp))
		
		rem Complete
	
	Case &h49
		rem bit 1,(IX+disp)
		z=BitX(1,fetch(IX+disp))
		
		rem Complete

	Case &h4A
		rem bit 1,(IX+disp)
		z=BitX(1,fetch(IX+disp))
		
		rem Complete
		
	Case &h4B
		rem bit 1,(IX+disp)
		z=BitX(1,fetch(IX+disp))
		
		rem Complete
		
	Case &h4C
		rem bit 1,(IX+disp)
		z=BitX(1,fetch(IX+disp))
		
		rem Complete
		
	Case &h4D
		rem bit 1,(IX+disp)
		z=BitX(1,fetch(IX+disp))
		
		rem Complete
		
	Case &h4E
		rem bit 1,(IX+disp)
		z=BitX(1,fetch(hl))
		rem Complete		
		
	Case &h4F
		rem bit 1,(IX+disp)
		z=BitX(1,fetch(IX+disp))
		rem Complete	
		
	Case &h50
		rem bit 2,(IX+disp)
		z=BitX(2,fetch(IX+disp))
		
		rem Complete
	
	Case &h51
		rem Bit 2,(IX+disp)
		z=BitX(2,fetch(IX+disp))
		
		rem Complete

	Case &h52
		rem Bit 2,(IX+disp)
		z=BitX(2,fetch(IX+disp))
		
		rem Complete
		
	Case &h53
		rem Bit 2,(IX+disp)
		z=BitX(2,fetch(IX+disp))
		
		rem Complete
		
	Case &h54
		rem Bit 2,(IX+disp)
		z=BitX(2,fetch(IX+disp))
		
		rem Complete
		
	Case &h55
		rem Bit 2,(IX+disp)
		z=BitX(2,fetch(IX+disp))
		
		rem Complete
		
	Case &h56
		rem Bit 2,(IX+disp)
		z=BitX(2,fetch(hl))
		rem Complete		
		
	Case &h57
		rem Bit 2,(IX+disp)
		z=BitX(2,fetch(IX+disp))
		rem Complete		
			
		Case &h58
		rem bit 3,(IX+disp)
		z=BitX(3,fetch(IX+disp))
		
		rem Complete
	
	Case &h59
		rem Bit 3,(IX+disp)
		z=BitX(3,fetch(IX+disp))
		
		rem Complete

	Case &h5A
		rem Bit 3,(IX+disp)
		z=BitX(3,fetch(IX+disp))
		
		rem Complete
		
	Case &h5B
		rem Bit 3,(IX+disp)
		z=BitX(3,fetch(IX+disp))
		
		rem Complete
		
	Case &h5C
		rem Bit 3,(IX+disp)
		z=BitX(3,fetch(IX+disp))
		
		rem Complete
		
	Case &h5D
		rem Bit 3,(IX+disp)
		z=BitX(3,fetch(IX+disp))
		
		rem Complete
		
	Case &h5E
		rem Bit 3,(IX+disp)
		z=BitX(3,fetch(hl))
		rem Complete		
		
	Case &h5F
		rem Bit 3,(IX+disp)
		z=BitX(3,fetch(IX+disp))
		rem Complete	
		
	Case &h60
		rem bit 4,(IX+disp)
		z=BitX(4,fetch(IX+disp))
		
		rem Complete
	
	Case &h61
		rem bit 4,(IX+disp)
		z=BitX(4,fetch(IX+disp))
		
		rem Complete

	Case &h62
		rem bit 4,(IX+disp)
		z=BitX(4,fetch(IX+disp))
		
		rem Complete
		
	Case &h63
		rem bit 4,(IX+disp)
		z=BitX(4,fetch(IX+disp))
		
		rem Complete
		
	Case &h64
		rem bit 4,(IX+disp)
		z=BitX(4,fetch(IX+disp))
		
		rem Complete
		
	Case &h65
		rem bit 4,(IX+disp)
		z=BitX(4,fetch(IX+disp))
		
		rem Complete
		
	Case &h66
		rem bit 4,(IX+disp)
		z=BitX(4,fetch(hl))
		rem Complete		
		
	Case &h67
		rem bit 4,(IX+disp)
		z=BitX(4,fetch(IX+disp))
		rem Complete		
			
		Case &h68
		rem bit 5,(IX+disp)
		z=BitX(5,fetch(IX+disp))
		
		rem Complete
	
	Case &h69
		rem bit 5,(IX+disp)
		z=BitX(5,fetch(IX+disp))
		
		rem Complete

	Case &h6A
		rem bit 5,(IX+disp)
		z=BitX(5,fetch(IX+disp))
		
		rem Complete
		
	Case &h6B
		rem bit 5,(IX+disp)
		z=BitX(5,fetch(IX+disp))
		
		rem Complete
		
	Case &h6C
		rem bit 5,(IX+disp)
		z=BitX(5,fetch(IX+disp))
		
		rem Complete
		
	Case &h6D
		rem bit 5,(IX+disp)
		z=BitX(5,fetch(IX+disp))
		
		rem Complete
		
	Case &h6E
		rem bit 5,(IX+disp)
		z=BitX(5,fetch(hl))
		rem Complete		
		
	Case &h6F
		rem bit 5,(IX+disp)
		z=BitX(5,fetch(IX+disp))
		rem Complete	
		
	Case &h70
		rem bit 6,(IX+disp)
		z=BitX(6,fetch(IX+disp))
		
		rem Complete
	
	Case &h71
		rem bit 6,(IX+disp)
		z=BitX(6,fetch(IX+disp))
		
		rem Complete

	Case &h72
		rem bit 6,(IX+disp)
		z=BitX(6,fetch(IX+disp))
		
		rem Complete
		
	Case &h73
		rem bit 6,(IX+disp)
		z=BitX(6,fetch(IX+disp))
		
		rem Complete
		
	Case &h74
		rem bit 6,(IX+disp)
		z=BitX(6,fetch(IX+disp))
		
		rem Complete
		
	Case &h75
		rem bit 6,(IX+disp)
		z=BitX(6,fetch(IX+disp))
		
		rem Complete
		
	Case &h76
		rem bit 6,(IX+disp)
		z=BitX(6,fetch(hl))
		rem Complete		
		
	Case &h77
		rem bit 6,(IX+disp)
		z=BitX(6,fetch(IX+disp))
		rem Complete		
			
		Case &h78
		rem bit 7,(IX+disp)
		z=BitX(7,fetch(IX+disp))
		
		rem Complete
	
	Case &h79
		rem bit 7,(IX+disp)
		z=BitX(7,fetch(IX+disp))
		
		rem Complete

	Case &h7A
		rem bit 7,(IX+disp)
		z=BitX(7,fetch(IX+disp))
		
		rem Complete
		
	Case &h7B
		rem bit 7,(IX+disp)
		z=BitX(7,fetch(IX+disp))
		
		rem Complete
		
	Case &h7C
		rem bit 7,(IX+disp)
		z=BitX(7,fetch(IX+disp))
		
		rem Complete
		
	Case &h7D
		rem bit 7,(IX+disp)
		z=BitX(7,fetch(IX+disp))
		
		rem Complete
		
	Case &h7E
		rem bit 7,(IX+disp)
		z=BitX(7,fetch(hl))
		rem Complete		
		
	Case &h7F
		rem bit 7,(IX+disp)
		z=BitX(7,fetch(IX+disp))
		rem Complete
		

	Case &h80
		rem res 0,(IX+disp)
		b=ResetBit(0,fetch(IX+disp))
		BakeBC()
		rem Complete
	
	Case &h81
		rem res 0,(IX+disp)
		c=ResetBit(0,fetch(IX+disp))
		BakeBC()
		rem Complete

	Case &h82
		rem res 0,(IX+disp)
		d=ResetBit(0,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &h83
		rem res 0,(IX+disp)
		e=ResetBit(0,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &h84
		rem res 0,(IX+disp)
		h=ResetBit(0,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &h85
		rem res 0,(IX+disp)
		l=ResetBit(0,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &h86
		rem res 0,(IX+disp)
		set((IX+disp),ResetBit(0,fetch(IX+disp)))
		rem Complete		
		
	Case &h87
		rem res 0,(IX+disp)
		a=ResetBit(0,fetch(IX+disp))
		BAKEAF()
		rem Complete		
			
		Case &h88
		rem res 1,(IX+disp)
		b=ResetBit(1,fetch(IX+disp))
		BakeBC()
		rem Complete
	
	Case &h89
		rem res 1,(IX+disp)
		c=ResetBit(1,fetch(IX+disp))
		BakeBC()
		rem Complete

	Case &h8A
		rem res 1,(IX+disp)
		d=ResetBit(1,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &h8B
		rem res 1,(IX+disp)
		e=ResetBit(1,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &h8C
		rem res 1,(IX+disp)
		h=ResetBit(1,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &h8D
		rem res 1,(IX+disp)
		l=ResetBit(1,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &h8E
		rem res 1,(IX+disp)
		set((IX+disp),ResetBit(1,fetch(IX+disp)))
		rem Complete		
		
	Case &h8F
		rem res 1,(IX+disp)
		a=ResetBit(1,fetch(IX+disp))
		BAKEAF()
		rem Complete	
		
	Case &h90
		rem res 2,(IX+disp)
		b=ResetBit(2,fetch(IX+disp))
		BakeBC()
		rem Complete
	
	Case &h91
		rem res 2,(IX+disp)
		c=ResetBit(2,fetch(IX+disp))
		BakeBC()
		rem Complete

	Case &h92
		rem res 2,(IX+disp)
		d=ResetBit(2,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &h93
		rem res 2,(IX+disp)
		e=ResetBit(2,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &h94
		rem res 2,(IX+disp)
		h=ResetBit(2,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &h95
		rem res 2,(IX+disp)
		l=ResetBit(2,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &h96
		rem res 2,(IX+disp)
		set((IX+disp),ResetBit(2,fetch(IX+disp)))
		rem Complete		
		
	Case &h97
		rem res 2,(IX+disp)
		a=ResetBit(2,fetch(IX+disp))
		BAKEAF()
		rem Complete		
			
		Case &h98
		rem res 3,(IX+disp)
		b=ResetBit(3,fetch(IX+disp))
		BakeBC()
		rem Complete
	
	Case &h99
		rem res 3,(IX+disp)
		c=ResetBit(3,fetch(IX+disp))
		BakeBC()
		rem Complete

	Case &h9A
		rem res 3,(IX+disp)
		d=ResetBit(3,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &h9B
		rem res 3,(IX+disp)
		e=ResetBit(3,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &h9C
		rem res 3,(IX+disp)
		h=ResetBit(3,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &h9D
		rem res 3,(IX+disp)
		l=ResetBit(3,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &h9E
		rem res 3,(IX+disp)
		set((IX+disp),ResetBit(3,fetch(IX+disp)))
		rem Complete		
		
	Case &h9F
		rem res 3,(IX+disp)
		a=ResetBit(3,fetch(IX+disp))
		BAKEAF()
		rem Complete	
		
	Case &hA0
		rem res 4,(IX+disp)
		b=ResetBit(4,fetch(IX+disp))
		BakeBC()
		rem Complete
	
	Case &hA1
		rem res 4,(IX+disp)
		c=ResetBit(4,fetch(IX+disp))
		BakeBC()
		rem Complete

	Case &hA2
		rem res 4,(IX+disp)
		d=ResetBit(4,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hA3
		rem res 4,(IX+disp)
		e=ResetBit(4,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hA4
		rem res 4,(IX+disp)
		h=ResetBit(4,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hA5
		rem res 4,(IX+disp)
		l=ResetBit(4,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hA6
		rem res 4,(IX+disp)
		set((IX+disp),ResetBit(4,fetch(IX+disp)))
		rem Complete		
		
	Case &hA7
		rem res 4,(IX+disp)
		a=ResetBit(4,fetch(IX+disp))
		BAKEAF()
		rem Complete		
			
		Case &hA8
		rem res 5,(IX+disp)
		b=ResetBit(5,fetch(IX+disp))
		BakeBC()
		rem Complete
	
	Case &hA9
		rem res 5,(IX+disp)
		c=ResetBit(5,fetch(IX+disp))
		BakeBC()
		rem Complete

	Case &hAA
		rem res 5,(IX+disp)
		d=ResetBit(5,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hAB
		rem res 5,(IX+disp)
		e=ResetBit(5,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hAC
		rem res 5,(IX+disp)
		h=ResetBit(5,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hAD
		rem res 5,(IX+disp)
		l=ResetBit(5,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hAE
		rem res 5,(IX+disp)
		set((IX+disp),ResetBit(5,fetch(IX+disp)))
		rem Complete		
		
	Case &hAF
		rem res 5,(IX+disp)
		a=ResetBit(5,fetch(IX+disp))
		BAKEAF()
		rem Complete	
		
	Case &hB0
		rem res 6,(IX+disp)
		b=ResetBit(6,fetch(IX+disp))
		BakeBC()
		rem Complete
	
	Case &hB1
		rem res 6,(IX+disp)
		c=ResetBit(6,fetch(IX+disp))
		BakeBC()
		rem Complete

	Case &hB2
		rem res 6,(IX+disp)
		d=ResetBit(6,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hB3
		rem res 6,(IX+disp)
		e=ResetBit(6,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hB4
		rem res 6,(IX+disp)
		h=ResetBit(6,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hB5
		rem res 6,(IX+disp)
		l=ResetBit(6,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hB6
		rem res 6,(IX+disp)
		set((IX+disp),ResetBit(6,fetch(IX+disp)))
		rem Complete		
		
	Case &hB7
		rem res 6,(IX+disp)
		a=ResetBit(6,fetch(IX+disp))
		BAKEAF()
		rem Complete		
			
		Case &hB8
		rem res 7,(IX+disp)
		b=ResetBit(7,fetch(IX+disp))
		BakeBC()
		rem Complete
	
	Case &hB9
		rem res 7,(IX+disp)
		c=ResetBit(7,fetch(IX+disp))
		BakeBC()
		rem Complete

	Case &hBA
		rem res 7,(IX+disp)
		d=ResetBit(7,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hBB
		rem res 7,(IX+disp)
		e=ResetBit(7,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hBC
		rem res 7,(IX+disp)
		h=ResetBit(7,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hBD
		rem res 7,(IX+disp)
		l=ResetBit(7,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hBE
		rem res 7,(IX+disp)
		set((IX+disp),ResetBit(7,fetch(IX+disp)))
		rem Complete		
		
	Case &hBF
		rem res 7,(IX+disp)
		a=ResetBit(7,fetch(IX+disp))
		BAKEAF()
		rem Complete
		

	Case &hC0
		rem set 0,(IX+disp)
		b=SetBit(0,fetch(IX+disp))
		BakeBC()
		rem Complete
	
	Case &hC1
		rem set 0,(IX+disp)
		c=SetBit(0,fetch(IX+disp))
		BakeBC()
		rem Complete

	Case &hC2
		rem set 0,(IX+disp)
		d=SetBit(0,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hC3
		rem set 0,(IX+disp)
		e=SetBit(0,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hC4
		rem set 0,(IX+disp)
		h=SetBit(0,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hC5
		rem set 0,(IX+disp)
		l=SetBit(0,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hC6
		rem set 0,(IX+disp)
		set((IX+disp),SetBit(0,fetch(IX+disp)))
		rem Complete		
		
	Case &hC7
		rem set 0,(IX+disp)
		a=SetBit(0,fetch(IX+disp))
		BAKEAF()
		rem Complete		
			
		Case &hC8
		rem set 1,(IX+disp)
		b=SetBit(1,fetch(IX+disp))
		BakeBC()
		rem Complete
	
	Case &hC9
		rem set 1,(IX+disp)
		c=SetBit(1,fetch(IX+disp))
		BakeBC()
		rem Complete

	Case &hCA
		rem set 1,(IX+disp)
		d=SetBit(1,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hCB
		rem set 1,(IX+disp)
		e=SetBit(1,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hCC
		rem set 1,(IX+disp)
		h=SetBit(1,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hCD
		rem set 1,(IX+disp)
		l=SetBit(1,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hCE
		rem set 1,(IX+disp)
		set((IX+disp),SetBit(1,fetch(IX+disp)))
		rem Complete		
		
	Case &hCF
		rem set 1,(IX+disp)
		a=SetBit(1,fetch(IX+disp))
		BAKEAF()
		rem Complete	
		
	Case &hD0
		rem set 2,(IX+disp)
		b=SetBit(2,fetch(IX+disp))
		BakeBC()
		rem Complete
	
	Case &hD1
		rem set 2,(IX+disp)
		c=SetBit(2,fetch(IX+disp))
		BakeBC()
		rem Complete

	Case &hD2
		rem set 2,(IX+disp)
		d=SetBit(2,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hD3
		rem set 2,(IX+disp)
		e=SetBit(2,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hD4
		rem set 2,(IX+disp)
		h=SetBit(2,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hD5
		rem set 2,(IX+disp)
		l=SetBit(2,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hD6
		rem set 2,(IX+disp)
		set((IX+disp),SetBit(2,fetch(IX+disp)))
		rem Complete		
		
	Case &hD7
		rem set 2,(IX+disp)
		a=SetBit(2,fetch(IX+disp))
		BAKEAF()
		rem Complete		
			
		Case &hD8
		rem set 3,(IX+disp)
		b=SetBit(3,fetch(IX+disp))
		BakeBC()
		rem Complete
	
	Case &hD9
		rem set 3,(IX+disp)
		c=SetBit(3,fetch(IX+disp))
		BakeBC()
		rem Complete

	Case &hDA
		rem set 3,(IX+disp)
		d=SetBit(3,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hDB
		rem set 3,(IX+disp)
		e=SetBit(3,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hDC
		rem set 3,(IX+disp)
		h=SetBit(3,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hDD
		rem set 3,(IX+disp)
		l=SetBit(3,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hDE
		rem set 3,(IX+disp)
		set((IX+disp),SetBit(3,fetch(IX+disp)))
		rem Complete		
		
	Case &hDF
		rem set 3,(IX+disp)
		a=SetBit(3,fetch(IX+disp))
		BAKEAF()
		rem Complete	
		
	Case &hE0
		rem set 4,(IX+disp)
		b=SetBit(4,fetch(IX+disp))
		BakeBC()
		rem Complete
	
	Case &hE1
		rem set 4,(IX+disp)
		c=SetBit(4,fetch(IX+disp))
		BakeBC()
		rem Complete

	Case &hE2
		rem set 4,(IX+disp)
		d=SetBit(4,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hE3
		rem set 4,(IX+disp)
		e=SetBit(4,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hE4
		rem set 4,(IX+disp)
		h=SetBit(4,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hE5
		rem set 4,(IX+disp)
		l=SetBit(4,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hE6
		rem set 4,(IX+disp)
		set((IX+disp),SetBit(4,fetch(IX+disp)))
		rem Complete		
		
	Case &hE7
		rem set 4,(IX+disp)
		a=SetBit(4,fetch(IX+disp))
		BAKEAF()
		rem Complete		
			
		Case &hE8
		rem set 5,(IX+disp)
		b=SetBit(5,fetch(IX+disp))
		BakeBC()
		rem Complete
	
	Case &hE9
		rem set 5,(IX+disp)
		c=SetBit(5,fetch(IX+disp))
		BakeBC()
		rem Complete

	Case &hEA
		rem set 5,(IX+disp)
		d=SetBit(5,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hEB
		rem set 5,(IX+disp)
		e=SetBit(5,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hEC
		rem set 5,(IX+disp)
		h=SetBit(5,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hED
		rem set 5,(IX+disp)
		l=SetBit(5,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hEE
		rem set 5,(IX+disp)
		set((IX+disp),SetBit(5,fetch(IX+disp)))
		rem Complete		
		
	Case &hEF
		rem set 5,(IX+disp)
		a=SetBit(5,fetch(IX+disp))
		BAKEAF()
		rem Complete	
		
	Case &hF0
		rem set 6,(IX+disp)
		b=SetBit(6,fetch(IX+disp))
		BakeBC()
		rem Complete
	
	Case &hF1
		rem set 6,(IX+disp)
		c=SetBit(6,fetch(IX+disp))
		BakeBC()
		rem Complete

	Case &hF2
		rem set 6,(IX+disp)
		d=SetBit(6,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hF3
		rem set 6,(IX+disp)
		e=SetBit(6,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hF4
		rem set 6,(IX+disp)
		h=SetBit(6,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hF5
		rem set 6,(IX+disp)
		l=SetBit(6,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hF6
		rem set 6,(IX+disp)
		set((IX+disp),SetBit(6,fetch(IX+disp)))
		rem Complete		
		
	Case &hF7
		rem set 6,(IX+disp)
		a=SetBit(6,fetch(IX+disp))
		BAKEAF()
		rem Complete		
			
		Case &hF8
		rem set 7,(IX+disp)
		b=SetBit(7,fetch(IX+disp))
		BakeBC()
		rem Complete
	
	Case &hF9
		rem set 7,(IX+disp)
		c=SetBit(7,fetch(IX+disp))
		BakeBC()
		rem Complete

	Case &hFA
		rem set 7,(IX+disp)
		d=SetBit(7,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hFB
		rem set 7,(IX+disp)
		e=SetBit(7,fetch(IX+disp))
		BakeDE()
		rem Complete
		
	Case &hFC
		rem set 7,(IX+disp)
		h=SetBit(7,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hFD
		rem set 7,(IX+disp)
		l=SetBit(7,fetch(IX+disp))
		BakeHL()
		rem Complete
		
	Case &hFE
		rem set 7,(IX+disp)
		set((IX+disp),SetBit(7,fetch(IX+disp)))
		rem Complete		
		
	Case &hFF
		rem set 7,(IX+disp)
		a=SetBit(7,fetch(IX+disp))
		BAKEAF()
		rem Complete		
		





	Case Else
		result=0
				rem PRINT "EMULATOR UNDOCUMENTED CODE."
End Select

return result
End function






Function FDCB () as integer
rem: Return a binary result - For IY instructions. Bitwise.
rem After CB extended instructions. Called from M1 which processed the first byte.
rem Bitwise Instructions. 

dim opcode as integer
dim result as integer
dim disp as integer	: rem Displacement.

disp=FixDisp(NextPC())		: rem Get displacement before Opcode.
opcode=NextPC()
result=1

Select Case As Const opcode

	Case &h00
		rem rlc(IY+disp),b
		b=rlc(fetch(IY+disp))
		BakeBC()
		rem complete
	
	Case &h01
		rem rlc(IY+disp),c
		c=rlc(fetch(IY+disp))
		BakeBC()
		rem complete

	Case &h02
		rem rlc(IY+disp),d
		d=rlc(fetch(IY+disp))
		BakeDE()
		rem complete
		
	Case &h03
		rem rlc(IY+disp),e
		e=rlc(fetch(IY+disp))
		BakeDE()
		rem complete
		
	Case &h04
		rem rlc(IY+disp),h
		h=rlc(fetch(IY+disp))
		BakeHL()
		rem complete
		
	Case &h05
		rem rlc(IY+disp),l
		l=rlc(fetch(IY+disp))
		BakeHL()
		rem complete
		
	Case &h06
		rem rlc (IY+disp)
		set(IY+disp,rlc(fetch(IY+disp)))
		rem complete		
		
	Case &h07
		rem rlc(IY+disp),a
		a=rlc(fetch(IY+disp))
		bakeaf()
		rem complete		
			
	Case &h08
		rem rrc(IY+disp),b
		b=rrc(fetch(IY+disp))
		BakeBC()
		rem complete
	
	Case &h09
		rem rrc(IY+disp),c
		c=rrc(fetch(IY+disp))
		BakeBC()
		rem complete

	Case &h0A
		rem rrc(IY+disp),d
		d=rrc(fetch(IY+disp))
		BakeDE()
		rem complete
		
	Case &h0B
		rem rrc(IY+disp),e
		e=rrc(fetch(IY+disp))
		BakeDE()
		rem complete
		
	Case &h0C
		rem rrc(IY+disp),h
		h=rrc(fetch(IY+disp))
		BakeHL()
		rem complete
		
	Case &h0D
		rem rrc(IY+disp),l
		l=rrc(fetch(IY+disp))
		BakeHL()
		rem complete
		
	Case &h0E
		rem rrc (IY+disp)
		set(IY+disp,rrc(fetch(IY+disp)))
		rem complete		
		
	Case &h0F
		rem rrc(IY+disp),a
		a=rrc(fetch(IY+disp))
		bakeaf()
		rem complete	

	Case &h10
		rem rl(IY+disp),b
		b=rl(fetch(IY+disp))
		BakeBC()
		rem complete
	
	Case &h11
		rem rl(IY+disp),c
		c=rl(fetch(IY+disp))
		BakeBC()
		rem complete

	Case &h12
		rem rl(IY+disp),d
		d=rl(fetch(IY+disp))
		BakeDE()
		rem complete
		
	Case &h13
		rem rl(IY+disp),e
		e=rl(fetch(IY+disp))
		BakeDE()
		rem complete
		
	Case &h14
		rem rl(IY+disp),h
		h=rl(fetch(IY+disp))
		BakeHL()
		rem complete
		
	Case &h15
		rem rl(IY+disp),l
		l=rl(fetch(IY+disp))
		BakeHL()
		rem complete
		
	Case &h16
		rem rl (IY+disp)
		set(IY+disp,rl(fetch(IY+disp)))
		rem complete		
		
	Case &h17
		rem rl(IY+disp),a
		a=rl(fetch(IY+disp))
		bakeaf()
		rem complete		
			
	Case &h18
		rem rr(IY+disp),b
		b=rr(fetch(IY+disp))
		BakeBC()
		rem complete
	
	Case &h19
		rem rr(IY+disp),c
		c=rr(fetch(IY+disp))
		BakeBC()
		rem complete

	Case &h1A
		rem rr(IY+disp),d
		d=rr(fetch(IY+disp))
		BakeDE()
		rem complete
		
	Case &h1B
		rem rr(IY+disp),e
		e=rr(fetch(IY+disp))
		BakeDE()
		rem complete
		
	Case &h1C
		rem rr(IY+disp),h
		h=rr(fetch(IY+disp))
		BakeHL()
		rem complete
		
	Case &h1D
		rem rr(IY+disp),l
		l=rr(fetch(IY+disp))
		BakeHL()
		rem complete
		
	Case &h1E
		rem rr (IY+disp)
		set(IY+disp,rr(fetch(IY+disp)))
		rem complete		
		
	Case &h1F
		rem rr(IY+disp),a
		a=rr(fetch(IY+disp))
		bakeaf()
		rem complete	
		
	Case &h20
		rem sla(IY+disp),b
		b=sla(fetch(IY+disp))
		BakeBC()
		rem complete
	
	Case &h21
		rem sla(IY+disp),c
		c=sla(fetch(IY+disp))
		BakeBC()
		rem complete

	Case &h22
		rem sla(IY+disp),d
		d=sla(fetch(IY+disp))
		BakeDE()
		rem complete
		
	Case &h23
		rem sla(IY+disp),e
		e=sla(fetch(IY+disp))
		BakeDE()
		rem complete
		
	Case &h24
		rem sla(IY+disp),h
		h=sla(fetch(IY+disp))
		BakeHL()
		rem complete
		
	Case &h25
		rem sla(IY+disp),l
		l=sla(fetch(IY+disp))
		BakeHL()
		rem complete
		
	Case &h26
		rem sla (IY+disp)
		set(IY+disp,sla(fetch(IY+disp)))
		rem complete		
		
	Case &h27
		rem sla(IY+disp),a
		a=sla(fetch(IY+disp))
		bakeaf()
		rem complete		
			
	Case &h28
		rem sra(IY+disp),b
		b=sra(fetch(IY+disp))
		BakeBC()
		rem complete
	
	Case &h29
		rem sra(IY+disp),c
		c=sra(fetch(IY+disp))
		BakeBC()
		rem complete

	Case &h2A
		rem sra(IY+disp),d
		d=sra(fetch(IY+disp))
		BakeDE()
		rem complete
		
	Case &h2B
		rem sra(IY+disp),e
		e=sra(fetch(IY+disp))
		BakeDE()
		rem complete
		
	Case &h2C
		rem sra(IY+disp),h
		h=sra(fetch(IY+disp))
		BakeHL()
		rem complete
		
	Case &h2D
		rem sra(IY+disp),l
		l=sra(fetch(IY+disp))
		BakeHL()
		rem complete
		
	Case &h2E
		rem sra (IY+disp)
		set(IY+disp,sra(fetch(IY+disp)))
		rem complete		
		
	Case &h2F
		rem sra(IY+disp),a
		a=sra(fetch(IY+disp))
		bakeaf()
		rem complete	

	Case &h30
		rem sll(IY+disp),b
		b=sll(fetch(IY+disp))
		BakeBC()
		rem complete
	
	Case &h31
		rem sll(IY+disp),c
		c=sll(fetch(IY+disp))
		BakeBC()
		rem complete

	Case &h32
		rem sll(IY+disp),d
		d=sll(fetch(IY+disp))
		BakeDE()
		rem complete
		
	Case &h33
		rem sll(IY+disp),e
		e=sll(fetch(IY+disp))
		BakeDE()
		rem complete
		
	Case &h34
		rem sll(IY+disp),h
		h=sll(fetch(IY+disp))
		BakeHL()
		rem complete
		
	Case &h35
		rem sll(IY+disp),l
		l=sll(fetch(IY+disp))
		BakeHL()
		rem complete
		
	Case &h36
		rem sll (IY+disp)
		set(IY+disp,sll(fetch(IY+disp)))
		rem complete		
		
	Case &h37
		rem sll(IY+disp),a
		a=sll(fetch(IY+disp))
		bakeaf()
		rem complete		
			
	Case &h38
		rem srl(IY+disp),b
		b=srl(fetch(IY+disp))
		BakeBC()
		rem complete
	
	Case &h39
		rem srl(IY+disp),c
		c=srl(fetch(IY+disp))
		BakeBC()
		rem complete

	Case &h3A
		rem srl(IY+disp),d
		d=srl(fetch(IY+disp))
		BakeDE()
		rem complete
		
	Case &h3B
		rem srl(IY+disp),e
		e=srl(fetch(IY+disp))
		BakeDE()
		rem complete
		
	Case &h3C
		rem srl(IY+disp),h
		h=srl(fetch(IY+disp))
		BakeHL()
		rem complete
		
	Case &h3D
		rem srl(IY+disp),l
		l=srl(fetch(IY+disp))
		BakeHL()
		rem complete
		
	Case &h3E
		rem srl (IY+disp)
		set(IY+disp,srl(fetch(IY+disp)))
		rem complete		
		
	Case &h3F
		rem srl(IY+disp),a
		a=srl(fetch(IY+disp))
		bakeaf()
		rem complete	
		
		
	Case &h40
		rem bit 0,(IY+disp)
		z=BitX(0,fetch(IY+disp))
		
		rem Complete
	
	Case &h41
		rem bit 0,(IY+disp)
		z=BitX(0,fetch(IY+disp))
		
		rem Complete

	Case &h42
		rem bit 0,(IY+disp)
		z=BitX(0,fetch(IY+disp))
		
		rem Complete
		
	Case &h43
		rem bit 0,(IY+disp)
		z=BitX(0,fetch(IY+disp))
		
		rem Complete
		
	Case &h44
		rem bit 0,(IY+disp)
		z=BitX(0,fetch(IY+disp))
		
		rem Complete
		
	Case &h45
		rem bit 0,(IY+disp)
		z=BitX(0,fetch(IY+disp))
		
		rem Complete
		
	Case &h46
		rem bit 0,(IY+disp)
		z=BitX(0,fetch(hl))
		rem Complete		
		
	Case &h47
		rem bit 0,(IY+disp)
		z=BitX(0,fetch(IY+disp))
		rem Complete		
			
	Case &h48
		rem bit 1,(IY+disp)
		z=BitX(1,fetch(IY+disp))
		
		rem Complete
	
	Case &h49
		rem bit 1,(IY+disp)
		z=BitX(1,fetch(IY+disp))
		
		rem Complete

	Case &h4A
		rem bit 1,(IY+disp)
		z=BitX(1,fetch(IY+disp))
		
		rem Complete
		
	Case &h4B
		rem bit 1,(IY+disp)
		z=BitX(1,fetch(IY+disp))
		
		rem Complete
		
	Case &h4C
		rem bit 1,(IY+disp)
		z=BitX(1,fetch(IY+disp))
		
		rem Complete
		
	Case &h4D
		rem bit 1,(IY+disp)
		z=BitX(1,fetch(IY+disp))
		
		rem Complete
		
	Case &h4E
		rem bit 1,(IY+disp)
		z=BitX(1,fetch(hl))
		rem Complete		
		
	Case &h4F
		rem bit 1,(IY+disp)
		z=BitX(1,fetch(IY+disp))
		rem Complete	
		
	Case &h50
		rem bit 2,(IY+disp)
		z=BitX(2,fetch(IY+disp))
		
		rem Complete
	
	Case &h51
		rem Bit 2,(IY+disp)
		z=BitX(2,fetch(IY+disp))
		
		rem Complete

	Case &h52
		rem Bit 2,(IY+disp)
		z=BitX(2,fetch(IY+disp))
		
		rem Complete
		
	Case &h53
		rem Bit 2,(IY+disp)
		z=BitX(2,fetch(IY+disp))
		
		rem Complete
		
	Case &h54
		rem Bit 2,(IY+disp)
		z=BitX(2,fetch(IY+disp))
		
		rem Complete
		
	Case &h55
		rem Bit 2,(IY+disp)
		z=BitX(2,fetch(IY+disp))
		
		rem Complete
		
	Case &h56
		rem Bit 2,(IY+disp)
		z=BitX(2,fetch(hl))
		rem Complete		
		
	Case &h57
		rem Bit 2,(IY+disp)
		z=BitX(2,fetch(IY+disp))
		rem Complete		
			
		Case &h58
		rem bit 3,(IY+disp)
		z=BitX(3,fetch(IY+disp))
		
		rem Complete
	
	Case &h59
		rem Bit 3,(IY+disp)
		z=BitX(3,fetch(IY+disp))
		
		rem Complete

	Case &h5A
		rem Bit 3,(IY+disp)
		z=BitX(3,fetch(IY+disp))
		
		rem Complete
		
	Case &h5B
		rem Bit 3,(IY+disp)
		z=BitX(3,fetch(IY+disp))
		
		rem Complete
		
	Case &h5C
		rem Bit 3,(IY+disp)
		z=BitX(3,fetch(IY+disp))
		
		rem Complete
		
	Case &h5D
		rem Bit 3,(IY+disp)
		z=BitX(3,fetch(IY+disp))
		
		rem Complete
		
	Case &h5E
		rem Bit 3,(IY+disp)
		z=BitX(3,fetch(hl))
		rem Complete		
		
	Case &h5F
		rem Bit 3,(IY+disp)
		z=BitX(3,fetch(IY+disp))
		rem Complete	
		
	Case &h60
		rem bit 4,(IY+disp)
		z=BitX(4,fetch(IY+disp))
		
		rem Complete
	
	Case &h61
		rem bit 4,(IY+disp)
		z=BitX(4,fetch(IY+disp))
		
		rem Complete

	Case &h62
		rem bit 4,(IY+disp)
		z=BitX(4,fetch(IY+disp))
		
		rem Complete
		
	Case &h63
		rem bit 4,(IY+disp)
		z=BitX(4,fetch(IY+disp))
		
		rem Complete
		
	Case &h64
		rem bit 4,(IY+disp)
		z=BitX(4,fetch(IY+disp))
		
		rem Complete
		
	Case &h65
		rem bit 4,(IY+disp)
		z=BitX(4,fetch(IY+disp))
		
		rem Complete
		
	Case &h66
		rem bit 4,(IY+disp)
		z=BitX(4,fetch(hl))
		rem Complete		
		
	Case &h67
		rem bit 4,(IY+disp)
		z=BitX(4,fetch(IY+disp))
		rem Complete		
			
		Case &h68
		rem bit 5,(IY+disp)
		z=BitX(5,fetch(IY+disp))
		
		rem Complete
	
	Case &h69
		rem bit 5,(IY+disp)
		z=BitX(5,fetch(IY+disp))
		
		rem Complete

	Case &h6A
		rem bit 5,(IY+disp)
		z=BitX(5,fetch(IY+disp))
		
		rem Complete
		
	Case &h6B
		rem bit 5,(IY+disp)
		z=BitX(5,fetch(IY+disp))
		
		rem Complete
		
	Case &h6C
		rem bit 5,(IY+disp)
		z=BitX(5,fetch(IY+disp))
		
		rem Complete
		
	Case &h6D
		rem bit 5,(IY+disp)
		z=BitX(5,fetch(IY+disp))
		
		rem Complete
		
	Case &h6E
		rem bit 5,(IY+disp)
		z=BitX(5,fetch(hl))
		rem Complete		
		
	Case &h6F
		rem bit 5,(IY+disp)
		z=BitX(5,fetch(IY+disp))
		rem Complete	
		
	Case &h70
		rem bit 6,(IY+disp)
		z=BitX(6,fetch(IY+disp))
		
		rem Complete
	
	Case &h71
		rem bit 6,(IY+disp)
		z=BitX(6,fetch(IY+disp))
		
		rem Complete

	Case &h72
		rem bit 6,(IY+disp)
		z=BitX(6,fetch(IY+disp))
		
		rem Complete
		
	Case &h73
		rem bit 6,(IY+disp)
		z=BitX(6,fetch(IY+disp))
		
		rem Complete
		
	Case &h74
		rem bit 6,(IY+disp)
		z=BitX(6,fetch(IY+disp))
		
		rem Complete
		
	Case &h75
		rem bit 6,(IY+disp)
		z=BitX(6,fetch(IY+disp))
		
		rem Complete
		
	Case &h76
		rem bit 6,(IY+disp)
		z=BitX(6,fetch(hl))
		rem Complete		
		
	Case &h77
		rem bit 6,(IY+disp)
		z=BitX(6,fetch(IY+disp))
		rem Complete		
			
		Case &h78
		rem bit 7,(IY+disp)
		z=BitX(7,fetch(IY+disp))
		
		rem Complete
	
	Case &h79
		rem bit 7,(IY+disp)
		z=BitX(7,fetch(IY+disp))
		
		rem Complete

	Case &h7A
		rem bit 7,(IY+disp)
		z=BitX(7,fetch(IY+disp))
		
		rem Complete
		
	Case &h7B
		rem bit 7,(IY+disp)
		z=BitX(7,fetch(IY+disp))
		
		rem Complete
		
	Case &h7C
		rem bit 7,(IY+disp)
		z=BitX(7,fetch(IY+disp))
		
		rem Complete
		
	Case &h7D
		rem bit 7,(IY+disp)
		z=BitX(7,fetch(IY+disp))
		
		rem Complete
		
	Case &h7E
		rem bit 7,(IY+disp)
		z=BitX(7,fetch(hl))
		rem Complete		
		
	Case &h7F
		rem bit 7,(IY+disp)
		z=BitX(7,fetch(IY+disp))
		rem Complete
		

	Case &h80
		rem res 0,(IY+disp)
		b=ResetBit(0,fetch(IY+disp))
		BakeBC()
		rem Complete
	
	Case &h81
		rem res 0,(IY+disp)
		c=ResetBit(0,fetch(IY+disp))
		BakeBC()
		rem Complete

	Case &h82
		rem res 0,(IY+disp)
		d=ResetBit(0,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &h83
		rem res 0,(IY+disp)
		e=ResetBit(0,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &h84
		rem res 0,(IY+disp)
		h=ResetBit(0,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &h85
		rem res 0,(IY+disp)
		l=ResetBit(0,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &h86
		rem res 0,(IY+disp)
		set((IY+disp),ResetBit(0,fetch(IY+disp)))
		rem Complete		
		
	Case &h87
		rem res 0,(IY+disp)
		a=ResetBit(0,fetch(IY+disp))
		BAKEAF()
		rem Complete		
			
		Case &h88
		rem res 1,(IY+disp)
		b=ResetBit(1,fetch(IY+disp))
		BakeBC()
		rem Complete
	
	Case &h89
		rem res 1,(IY+disp)
		c=ResetBit(1,fetch(IY+disp))
		BakeBC()
		rem Complete

	Case &h8A
		rem res 1,(IY+disp)
		d=ResetBit(1,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &h8B
		rem res 1,(IY+disp)
		e=ResetBit(1,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &h8C
		rem res 1,(IY+disp)
		h=ResetBit(1,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &h8D
		rem res 1,(IY+disp)
		l=ResetBit(1,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &h8E
		rem res 1,(IY+disp)
		set((IY+disp),ResetBit(1,fetch(IY+disp)))
		rem Complete		
		
	Case &h8F
		rem res 1,(IY+disp)
		a=ResetBit(1,fetch(IY+disp))
		BAKEAF()
		rem Complete	
		
	Case &h90
		rem res 2,(IY+disp)
		b=ResetBit(2,fetch(IY+disp))
		BakeBC()
		rem Complete
	
	Case &h91
		rem res 2,(IY+disp)
		c=ResetBit(2,fetch(IY+disp))
		BakeBC()
		rem Complete

	Case &h92
		rem res 2,(IY+disp)
		d=ResetBit(2,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &h93
		rem res 2,(IY+disp)
		e=ResetBit(2,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &h94
		rem res 2,(IY+disp)
		h=ResetBit(2,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &h95
		rem res 2,(IY+disp)
		l=ResetBit(2,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &h96
		rem res 2,(IY+disp)
		set((IY+disp),ResetBit(2,fetch(IY+disp)))
		rem Complete		
		
	Case &h97
		rem res 2,(IY+disp)
		a=ResetBit(2,fetch(IY+disp))
		BAKEAF()
		rem Complete		
			
		Case &h98
		rem res 3,(IY+disp)
		b=ResetBit(3,fetch(IY+disp))
		BakeBC()
		rem Complete
	
	Case &h99
		rem res 3,(IY+disp)
		c=ResetBit(3,fetch(IY+disp))
		BakeBC()
		rem Complete

	Case &h9A
		rem res 3,(IY+disp)
		d=ResetBit(3,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &h9B
		rem res 3,(IY+disp)
		e=ResetBit(3,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &h9C
		rem res 3,(IY+disp)
		h=ResetBit(3,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &h9D
		rem res 3,(IY+disp)
		l=ResetBit(3,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &h9E
		rem res 3,(IY+disp)
		set((IY+disp),ResetBit(3,fetch(IY+disp)))
		rem Complete		
		
	Case &h9F
		rem res 3,(IY+disp)
		a=ResetBit(3,fetch(IY+disp))
		BAKEAF()
		rem Complete	
		
	Case &hA0
		rem res 4,(IY+disp)
		b=ResetBit(4,fetch(IY+disp))
		BakeBC()
		rem Complete
	
	Case &hA1
		rem res 4,(IY+disp)
		c=ResetBit(4,fetch(IY+disp))
		BakeBC()
		rem Complete

	Case &hA2
		rem res 4,(IY+disp)
		d=ResetBit(4,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hA3
		rem res 4,(IY+disp)
		e=ResetBit(4,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hA4
		rem res 4,(IY+disp)
		h=ResetBit(4,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hA5
		rem res 4,(IY+disp)
		l=ResetBit(4,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hA6
		rem res 4,(IY+disp)
		set((IY+disp),ResetBit(4,fetch(IY+disp)))
		rem Complete		
		
	Case &hA7
		rem res 4,(IY+disp)
		a=ResetBit(4,fetch(IY+disp))
		BAKEAF()
		rem Complete		
			
		Case &hA8
		rem res 5,(IY+disp)
		b=ResetBit(5,fetch(IY+disp))
		BakeBC()
		rem Complete
	
	Case &hA9
		rem res 5,(IY+disp)
		c=ResetBit(5,fetch(IY+disp))
		BakeBC()
		rem Complete

	Case &hAA
		rem res 5,(IY+disp)
		d=ResetBit(5,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hAB
		rem res 5,(IY+disp)
		e=ResetBit(5,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hAC
		rem res 5,(IY+disp)
		h=ResetBit(5,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hAD
		rem res 5,(IY+disp)
		l=ResetBit(5,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hAE
		rem res 5,(IY+disp)
		set((IY+disp),ResetBit(5,fetch(IY+disp)))
		rem Complete		
		
	Case &hAF
		rem res 5,(IY+disp)
		a=ResetBit(5,fetch(IY+disp))
		BAKEAF()
		rem Complete	
		
	Case &hB0
		rem res 6,(IY+disp)
		b=ResetBit(6,fetch(IY+disp))
		BakeBC()
		rem Complete
	
	Case &hB1
		rem res 6,(IY+disp)
		c=ResetBit(6,fetch(IY+disp))
		BakeBC()
		rem Complete

	Case &hB2
		rem res 6,(IY+disp)
		d=ResetBit(6,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hB3
		rem res 6,(IY+disp)
		e=ResetBit(6,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hB4
		rem res 6,(IY+disp)
		h=ResetBit(6,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hB5
		rem res 6,(IY+disp)
		l=ResetBit(6,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hB6
		rem res 6,(IY+disp)
		set((IY+disp),ResetBit(6,fetch(IY+disp)))
		rem Complete		
		
	Case &hB7
		rem res 6,(IY+disp)
		a=ResetBit(6,fetch(IY+disp))
		BAKEAF()
		rem Complete		
			
		Case &hB8
		rem res 7,(IY+disp)
		b=ResetBit(7,fetch(IY+disp))
		BakeBC()
		rem Complete
	
	Case &hB9
		rem res 7,(IY+disp)
		c=ResetBit(7,fetch(IY+disp))
		BakeBC()
		rem Complete

	Case &hBA
		rem res 7,(IY+disp)
		d=ResetBit(7,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hBB
		rem res 7,(IY+disp)
		e=ResetBit(7,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hBC
		rem res 7,(IY+disp)
		h=ResetBit(7,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hBD
		rem res 7,(IY+disp)
		l=ResetBit(7,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hBE
		rem res 7,(IY+disp)
		set((IY+disp),ResetBit(7,fetch(IY+disp)))
		rem Complete		
		
	Case &hBF
		rem res 7,(IY+disp)
		a=ResetBit(7,fetch(IY+disp))
		BAKEAF()
		rem Complete
		

	Case &hC0
		rem set 0,(IY+disp)
		b=SetBit(0,fetch(IY+disp))
		BakeBC()
		rem Complete
	
	Case &hC1
		rem set 0,(IY+disp)
		c=SetBit(0,fetch(IY+disp))
		BakeBC()
		rem Complete

	Case &hC2
		rem set 0,(IY+disp)
		d=SetBit(0,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hC3
		rem set 0,(IY+disp)
		e=SetBit(0,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hC4
		rem set 0,(IY+disp)
		h=SetBit(0,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hC5
		rem set 0,(IY+disp)
		l=SetBit(0,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hC6
		rem set 0,(IY+disp)
		set((IY+disp),SetBit(0,fetch(IY+disp)))
		rem Complete		
		
	Case &hC7
		rem set 0,(IY+disp)
		a=SetBit(0,fetch(IY+disp))
		BAKEAF()
		rem Complete		
			
		Case &hC8
		rem set 1,(IY+disp)
		b=SetBit(1,fetch(IY+disp))
		BakeBC()
		rem Complete
	
	Case &hC9
		rem set 1,(IY+disp)
		c=SetBit(1,fetch(IY+disp))
		BakeBC()
		rem Complete

	Case &hCA
		rem set 1,(IY+disp)
		d=SetBit(1,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hCB
		rem set 1,(IY+disp)
		e=SetBit(1,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hCC
		rem set 1,(IY+disp)
		h=SetBit(1,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hCD
		rem set 1,(IY+disp)
		l=SetBit(1,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hCE
		rem set 1,(IY+disp)
		set((IY+disp),SetBit(1,fetch(IY+disp)))
		rem Complete		
		
	Case &hCF
		rem set 1,(IY+disp)
		a=SetBit(1,fetch(IY+disp))
		BAKEAF()
		rem Complete	
		
	Case &hD0
		rem set 2,(IY+disp)
		b=SetBit(2,fetch(IY+disp))
		BakeBC()
		rem Complete
	
	Case &hD1
		rem set 2,(IY+disp)
		c=SetBit(2,fetch(IY+disp))
		BakeBC()
		rem Complete

	Case &hD2
		rem set 2,(IY+disp)
		d=SetBit(2,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hD3
		rem set 2,(IY+disp)
		e=SetBit(2,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hD4
		rem set 2,(IY+disp)
		h=SetBit(2,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hD5
		rem set 2,(IY+disp)
		l=SetBit(2,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hD6
		rem set 2,(IY+disp)
		set((IY+disp),SetBit(2,fetch(IY+disp)))
		rem Complete		
		
	Case &hD7
		rem set 2,(IY+disp)
		a=SetBit(2,fetch(IY+disp))
		BAKEAF()
		rem Complete		
			
		Case &hD8
		rem set 3,(IY+disp)
		b=SetBit(3,fetch(IY+disp))
		BakeBC()
		rem Complete
	
	Case &hD9
		rem set 3,(IY+disp)
		c=SetBit(3,fetch(IY+disp))
		BakeBC()
		rem Complete

	Case &hDA
		rem set 3,(IY+disp)
		d=SetBit(3,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hDB
		rem set 3,(IY+disp)
		e=SetBit(3,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hDC
		rem set 3,(IY+disp)
		h=SetBit(3,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hDD
		rem set 3,(IY+disp)
		l=SetBit(3,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hDE
		rem set 3,(IY+disp)
		set((IY+disp),SetBit(3,fetch(IY+disp)))
		rem Complete		
		
	Case &hDF
		rem set 3,(IY+disp)
		a=SetBit(3,fetch(IY+disp))
		BAKEAF()
		rem Complete	
		
	Case &hE0
		rem set 4,(IY+disp)
		b=SetBit(4,fetch(IY+disp))
		BakeBC()
		rem Complete
	
	Case &hE1
		rem set 4,(IY+disp)
		c=SetBit(4,fetch(IY+disp))
		BakeBC()
		rem Complete

	Case &hE2
		rem set 4,(IY+disp)
		d=SetBit(4,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hE3
		rem set 4,(IY+disp)
		e=SetBit(4,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hE4
		rem set 4,(IY+disp)
		h=SetBit(4,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hE5
		rem set 4,(IY+disp)
		l=SetBit(4,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hE6
		rem set 4,(IY+disp)
		set((IY+disp),SetBit(4,fetch(IY+disp)))
		rem Complete		
		
	Case &hE7
		rem set 4,(IY+disp)
		a=SetBit(4,fetch(IY+disp))
		BAKEAF()
		rem Complete		
			
		Case &hE8
		rem set 5,(IY+disp)
		b=SetBit(5,fetch(IY+disp))
		BakeBC()
		rem Complete
	
	Case &hE9
		rem set 5,(IY+disp)
		c=SetBit(5,fetch(IY+disp))
		BakeBC()
		rem Complete

	Case &hEA
		rem set 5,(IY+disp)
		d=SetBit(5,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hEB
		rem set 5,(IY+disp)
		e=SetBit(5,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hEC
		rem set 5,(IY+disp)
		h=SetBit(5,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hED
		rem set 5,(IY+disp)
		l=SetBit(5,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hEE
		rem set 5,(IY+disp)
		set((IY+disp),SetBit(5,fetch(IY+disp)))
		rem Complete		
		
	Case &hEF
		rem set 5,(IY+disp)
		a=SetBit(5,fetch(IY+disp))
		BAKEAF()
		rem Complete	
		
	Case &hF0
		rem set 6,(IY+disp)
		b=SetBit(6,fetch(IY+disp))
		BakeBC()
		rem Complete
	
	Case &hF1
		rem set 6,(IY+disp)
		c=SetBit(6,fetch(IY+disp))
		BakeBC()
		rem Complete

	Case &hF2
		rem set 6,(IY+disp)
		d=SetBit(6,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hF3
		rem set 6,(IY+disp)
		e=SetBit(6,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hF4
		rem set 6,(IY+disp)
		h=SetBit(6,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hF5
		rem set 6,(IY+disp)
		l=SetBit(6,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hF6
		rem set 6,(IY+disp)
		set((IY+disp),SetBit(6,fetch(IY+disp)))
		rem Complete		
		
	Case &hF7
		rem set 6,(IY+disp)
		a=SetBit(6,fetch(IY+disp))
		BAKEAF()
		rem Complete		
			
		Case &hF8
		rem set 7,(IY+disp)
		b=SetBit(7,fetch(IY+disp))
		BakeBC()
		rem Complete
	
	Case &hF9
		rem set 7,(IY+disp)
		c=SetBit(7,fetch(IY+disp))
		BakeBC()
		rem Complete

	Case &hFA
		rem set 7,(IY+disp)
		d=SetBit(7,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hFB
		rem set 7,(IY+disp)
		e=SetBit(7,fetch(IY+disp))
		BakeDE()
		rem Complete
		
	Case &hFC
		rem set 7,(IY+disp)
		h=SetBit(7,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hFD
		rem set 7,(IY+disp)
		l=SetBit(7,fetch(IY+disp))
		BakeHL()
		rem Complete
		
	Case &hFE
		rem set 7,(IY+disp)
		set((IY+disp),SetBit(7,fetch(IY+disp)))
		rem Complete		
		
	Case &hFF
		rem set 7,(IY+disp)
		a=SetBit(7,fetch(IY+disp))
		BAKEAF()
		rem Complete		
		







	Case Else
		result=0
				rem PRINT "EMULATOR UNDOCUMENTED CODE."
End Select

return result
End function


Function DD () as integer
rem IX codes
dim opcode as integer
dim result as integer
dim disp as integer

opcode=NextPC()

Select Case As Const opcode

		
	Case &h09 
		rem add	IX,bc
		IX=IX+BC
		half=bittest(4096,IX)
		n=0
		carry=bittest(65536,IX)
		IX=IX mod 65536
		BAKEAF()
		rem Complete

	Case &h19
		rem add	IX,DE
		IX=IX+DE
		half=bittest(4096,IX)
		n=0
		carry=bittest(65536,IX)
		IX=IX mod 65536
		BAKEAF()
		rem complete
				

	Case &h21
		rem ld IX,xx
		IX=NextPC()+(NextPC()*256)
		rem complete

	Case &h22
		rem ld (xx),IX
		TempPC=NextPC()+(NextPC()*256)
		set(TempPC,lower(IX))
		TempPC=(TempPC+1) mod 65536
		set(TempPC,higher(IX))
		rem complete

	Case &h23
		rem inc IX
		IX=(IX+1) mod 65536
		Rem complete
		
	
	Case &h24
		rem inc IXh
		IX=(ALUINC(Higher(IX))*256)+LOWER(IX)
		rem complete
	
	Case &h25
		rem dec IXh
		IX=(ALUDEC(Higher(IX))*256)+Lower(IX)
		rem complete
		
	Case &h26
		rem	ld IXh,n
		IX=(NextPC()*256)+(IX AND &h00FF)
		rem complete. 
		
		
	Case &h29
		rem add	IX,IX
		IX=IX+IX
		half=bittest(4096,IX)
		n=0
		carry=bittest(65536,IX)
		IX=IX mod 65536
		BAKEAF()
		rem complete
		
	Case &h2A
		rem ld IX,(xx)
		TempPC=NextPC()+(NextPC()*256)
		IX=fetch(TempPC)				: rem Lower byte
		TempPC=(TempPC+1) mod 65536
		IX=IX+(fetch(TempPC)*256)		: rem Upper byte
		rem complete
		
	Case &h2B
		rem dec	IX
		IX=(IX+65535) mod 65536
		Rem complete
		
	Case &h2C
		rem inc IXl
		IX=(IX AND &hFF00)+ALUINC(Lower(IX))
		rem Complete

	Case &h2D
		rem dec IXl
		IX=(IX AND &hFF00)+ALUDEC(Lower(IX))
		rem Complete
		
	Case &h2E
		rem	ld IXl,n
		IX=(IX and &hFF00)+NextPC()
		rem complete.
		
	Case &h34
		rem inc (IX+d)
		disp=FixDisp(NextPC())

		Set(IX+disp,ALUINC(Fetch(IX+disp)))
		Rem complete

	Case &h35
		rem dec (IX+d)
		disp=FixDisp(NextPC())

		Set(IX+disp,ALUDEC(Fetch(IX+disp)))
		Rem complete

	Case &h36
		rem ld (IX+d),x
		disp=FixDisp(NextPC())
		Set(IX+disp,NextPC())
		Rem complete
		
	Case &h39
		rem add	IX,Stack
		IX=IX+Stack
		half=bittest(4096,IX)
		n=0
		carry=bittest(65536,IX)
		IX=IX mod 65536
		BAKEAF()
		rem complete


		
	Case &h46
		rem ld b,(IX+d)
		disp=FixDisp(NextPC())
		b=fetch(IX+disp)
		BakeBC()
		rem Complete. 
		
	Case &h4E
		rem ld c,(IX+d)
		disp=FixDisp(NextPC())
		c=fetch(IX+disp)
		BakeBC()
		rem Complete. 

	Case &h56
		rem ld d,(IX+d)
		disp=FixDisp(NextPC())
		d=fetch(IX+disp)
		BakeDE()
		rem Complete. 
		
	Case &h5E
		rem ld e,(IX+d)
		disp=FixDisp(NextPC())
		e=fetch(IX+disp)
		BakeDE()
		rem Complete. 


	Case &h66
		rem ld h,(IX+d)
		disp=FixDisp(NextPC())
		h=fetch(IX+disp)
		BakeHL()
		rem Complete. 
		
	Case &h6E
		rem ld l,(IX+d)
		disp=FixDisp(NextPC())
		l=fetch(IX+disp)
		BakeHL()
		rem Complete. 
		
	

	Case &h70
		rem ld (IX+d),b
		disp=FixDisp(NextPC())
		t=b
		Set(IX+disp,t)
		rem Complete. 
		
	Case &h71
		rem ld (IX+d),c
		disp=FixDisp(NextPC())
		t=c
		Set(IX+disp,t)
		rem Complete. 
		
	Case &h72
		rem ld (IX+d),d
		disp=FixDisp(NextPC())
		t=d
		Set(IX+disp,t)
		rem Complete. 
		
	Case &h73
		rem ld (IX+d),e
		disp=FixDisp(NextPC())
		t=e
		Set(IX+disp,t)
		rem Complete. 
		rem Complete. 

	Case &h74
		rem ld (IX+d),h
		disp=FixDisp(NextPC())
		t=h
		Set(IX+disp,t)
		rem Complete. 
		
	Case &h75
		rem ld (IX+d),l
		disp=FixDisp(NextPC())
		t=l
		Set(IX+disp,t)
		rem Complete. 
	
rem Case &h76 does not exist. 
	
	Case &h77
		rem ld (IX+d),a
		disp=FixDisp(NextPC())
		t=a
		Set(IX+disp,t)
		rem Complete. 		
		
	Case &h7E
		rem ld a,(IX+d)
		disp=FixDisp(NextPC())
		a=fetch(IX+disp)
		rem Complete. 
		



rem Arithmetic Operators
rem

	Case &h84
		rem add a,IXh
		carry=0
		a=ALUADD(a,Higher(IX))
		bakeaf()
		rem Complete. 
	
	Case &h85
		rem add a,IXl
		carry=0
		a=ALUADD(a,Lower(IX))
		bakeaf()
		rem Complete. 

	
	Case &h86
		rem add a,(IX+d)
		carry=0
		disp=FixDisp(NextPC())
		a=ALUADD(a,fetch(IX+disp))
		BAKEAF()
		rem Complete. 


	Case &h8C
		rem adc a,IXh
		a=ALUADD(a,Higher(IX))
		bakeaf()
		rem Complete. 
	
	Case &h8D
		rem adc a,IXl
		a=ALUADD(a,Lower(IX))
		bakeaf()
		rem Complete. 
		
	Case &h8E
		rem adc a,(IX+d)
		disp=FixDisp(NextPC())
		a=ALUADD(a,fetch(IX+disp))
		rem Complete. 




	Case &h94
		rem sub a,IXh
		carry=0
		a=ALUSUB(a,Higher(IX))
		bakeaf()
		rem Complete. 
	
	Case &h95
		rem sub a,IXl
		carry=0
		a=ALUSUB(a,Lower(IX))
		bakeaf()
		rem Complete. 

		
	Case &h96
		rem sub a,(IX+d)
		carry=0
		disp=FixDisp(NextPC())
		a=ALUSUB(a,fetch(IX+disp))
		BAKEAF()
		rem Complete. 

	Case &h9C
		rem sbc a,IXh
		a=ALUSUB(a,Higher(IX))
		bakeaf()
		rem Complete. 
	
	Case &h9D
		rem sbc a,IXl
		a=ALUSUB(a,Lower(IX))
		bakeaf()
		rem Complete. 
		
	Case &h9E
		rem sbc a,(IX+d)
		disp=FixDisp(NextPC())
		a=ALUSUB(a,fetch(IX+disp))
		rem Complete. 

	Case &hA4
		rem and IXh
		a=ALUAND(a,Higher(IX))
		bakeaf()
		rem Complete. 
	
	Case &hA5
		rem and IXl
		a=ALUAND(a,Lower(IX))
		bakeaf()
		rem Complete. 

	Case &hA6
		rem and (IX+d)
		disp=FixDisp(NextPC())
		a=ALUAND(a,fetch(IX+disp))
		rem Complete. 
	


	Case &hAC
		rem xor IXh
		a=ALUXOR(a,Higher(IX))
		bakeaf()
		rem Complete. 
	
	Case &hAD
		rem xor IXl
		a=ALUXOR(a,Lower(IX))
		bakeaf()
		rem Complete. 

	Case &hAE
		rem xor (IX+d)
		disp=FixDisp(NextPC())
		a=ALUXOR(a,fetch(IX+disp))
		rem Complete. 
	
	Case &hB4
		rem or IXh
		a=ALUOR(a,Higher(IX))
		bakeaf()
		rem Complete. 
	
	Case &hB5
		rem or IXl
		a=ALUOR(a,Lower(IX))
		bakeaf()
		rem Complete. 


	Case &hB6
		rem or (IX+d)
		disp=FixDisp(NextPC())
		a=ALUOR(a,fetch(IX+disp))
		rem Complete. 
	
	Case &hBC
		rem cp IXh
		a=ALUCP(a,Higher(IX))
		bakeaf()
		rem Complete. 
	
	Case &hBD
		rem cp IXl
		a=ALUCP(a,Lower(IX))
		bakeaf()
		rem Complete. 
		
	Case &hBE
		rem cp (IX+d)
		carry=0
		disp=FixDisp(NextPC())
		a=ALUCP(a,fetch(IX+disp))
		BAKEAF()
		rem Complete. 

	Case &hCB
		rem DDCB
		DDCB()		: rem Jump to DDCB. 	


	Case &hE1
		rem pop IX
		
		IX=POP()
		rem Unrem BakeIX()

	Case &hE3
		rem ex (sp),IX
		TEMPPC=POP()
		PUSH IX
		IX=TEMPPC

	Case &hE5
		rem  push IX
		PUSH(IX)
		
	Case &hE9
		rem jp (IX)	
		PC=IX

	Case &hF9
		rem ld	sp,IX
		STACK=IX


	


	Case Else
		result=0
				rem PRINT "EMULATOR UNDOCUMENTED CODE."
End Select

return result
End function



Function FD () as integer
rem IY codes
dim opcode as integer
dim result as integer
dim disp as integer

opcode=NextPC()

Select Case As Const opcode

	Case &h09 
		rem add	IY,bc
		IY=IY+BC
		half=bittest(4096,IY)
		n=0
		carry=bittest(65536,IY)
		IY=IY mod 65536
		BAKEAF()
		rem Complete

	Case &h19
		rem add	IY,DE
		IY=IY+DE
		half=bittest(4096,IY)
		n=0
		carry=bittest(65536,IY)
		IY=IY mod 65536
		BAKEAF()
		rem complete
				

	Case &h21
		rem ld IY,xx
		IY=NextPC()+(NextPC()*256)
		rem complete

	Case &h22
		rem ld (xx),IY
		TempPC=NextPC()+(NextPC()*256)
		set(TempPC,lower(IY))
		TempPC=(TempPC+1) mod 65536
		set(TempPC,higher(IY))
		rem complete

	Case &h23
		rem inc IY
		IY=(IY+1) mod 65536
		Rem complete
		
	
	Case &h24
		rem inc IYh
		IY=(ALUINC(Higher(IY))*256)+LOWER(IY)
		rem complete
	
	Case &h25
		rem dec IYh
		IY=(ALUDEC(Higher(IY))*256)+Lower(IY)
		rem complete
		
	Case &h26
		rem	ld IYh,n
		IY=(NextPC()*256)+(IY AND &h00FF)
		rem complete. 
		
		
	Case &h29
		rem add	IY,IY
		IY=IY+IY
		half=bittest(4096,IY)
		n=0
		carry=bittest(65536,IY)
		IY=IY mod 65536
		BAKEAF()
		rem complete
		
	Case &h2A
		rem ld IY,(xx)
		TempPC=NextPC()+(NextPC()*256)
		IY=fetch(TempPC)				: rem Lower byte
		TempPC=(TempPC+1) mod 65536
		IY=IY+(fetch(TempPC)*256)		: rem Upper byte
		rem complete
		
	Case &h2B
		rem dec	IY
		IY=(IY+65535) mod 65536
		Rem complete
		
	Case &h2C
		rem inc IYl
		IY=(IY AND &hFF00)+ALUINC(Lower(IY))
		rem Complete

	Case &h2D
		rem dec IYl
		IY=(IY AND &hFF00)+ALUDEC(Lower(IY))
		rem Complete
		
	Case &h2E
		rem	ld IYl,n
		IY=(IY and &hFF00)+NextPC()
		rem complete.
		
	Case &h34
		rem inc (IY+d)
		disp=FixDisp(NextPC())

		Set(IY+disp,ALUINC(Fetch(IY+disp)))
		Rem complete

	Case &h35
		rem dec (IY+d)
		disp=FixDisp(NextPC())

		Set(IY+disp,ALUDEC(Fetch(IY+disp)))
		Rem complete

	Case &h36
		rem ld (IY+d),x
		disp=FixDisp(NextPC())
		Set(IY+disp,NextPC())
		Rem complete
		
	Case &h39
		rem add	IY,Stack
		IY=IY+Stack
		half=bittest(4096,IY)
		n=0
		carry=bittest(65536,IY)
		IY=IY mod 65536
		BAKEAF()
		rem complete


		
	Case &h46
		rem ld b,(IY+d)
		disp=FixDisp(NextPC())
		b=fetch(IY+disp)
		BakeBC()
		rem Complete. 
		
	Case &h4E
		rem ld c,(IY+d)
		disp=FixDisp(NextPC())
		c=fetch(IY+disp)
		BakeBC()
		rem Complete. 

	Case &h56
		rem ld d,(IY+d)
		disp=FixDisp(NextPC())
		d=fetch(IY+disp)
		BakeDE()
		rem Complete. 
		
	Case &h5E
		rem ld e,(IY+d)
		disp=FixDisp(NextPC())
		e=fetch(IY+disp)
		BakeDE()
		rem Complete. 


	Case &h66
		rem ld h,(IY+d)
		disp=FixDisp(NextPC())
		h=fetch(IY+disp)
		BakeHL()
		rem Complete. 
		
	Case &h6E
		rem ld l,(IY+d)
		disp=FixDisp(NextPC())
		l=fetch(IY+disp)
		BakeHL()
		rem Complete. 
		
	

	Case &h70
		rem ld (IY+d),b
		disp=FixDisp(NextPC())
		t=b
		Set(IY+disp,t)
		rem Complete. 
		
	Case &h71
		rem ld (IY+d),c
		disp=FixDisp(NextPC())
		t=c
		Set(IY+disp,t)
		rem Complete. 
		
	Case &h72
		rem ld (IY+d),d
		disp=FixDisp(NextPC())
		t=d
		Set(IY+disp,t)
		rem Complete. 
		
	Case &h73
		rem ld (IY+d),e
		disp=FixDisp(NextPC())
		t=e
		Set(IY+disp,t)
		rem Complete. 

	Case &h74
		rem ld (IY+d),h
		disp=FixDisp(NextPC())
		t=h
		Set(IY+disp,t)
		rem Complete. 
		
	Case &h75
		rem ld (IY+d),l
		disp=FixDisp(NextPC())
		t=l
		Set(IY+disp,t)
		rem Complete. 
		
	Case &h77
		rem ld (IY+d),a
		disp=FixDisp(NextPC())
		t=a
		Set(IY+disp,t)
		rem Complete. 		
		
	Case &h7E
		rem ld a,(IY+d)
		disp=FixDisp(NextPC())
		a=fetch(IY+disp)
		rem Complete. 
		



rem Arithmetic Operators
rem

	Case &h84
		rem add a,IYh
		carry=0
		a=ALUADD(a,Higher(IY))
		bakeaf()
		rem Complete. 
	
	Case &h85
		rem add a,IYl
		carry=0
		a=ALUADD(a,Lower(IY))
		bakeaf()
		rem Complete. 

	
	Case &h86
		rem add a,(IY+d)
		carry=0
		disp=FixDisp(NextPC())
		a=ALUADD(a,fetch(IY+disp))
		BAKEAF()
		rem Complete. 


	Case &h8C
		rem adc a,IYh
		a=ALUADD(a,Higher(IY))
		bakeaf()
		rem Complete. 
	
	Case &h8D
		rem adc a,IYl
		a=ALUADD(a,Lower(IY))
		bakeaf()
		rem Complete. 
		
	Case &h8E
		rem adc a,(IY+d)
		disp=FixDisp(NextPC())
		a=ALUADD(a,fetch(IY+disp))
		rem Complete. 




	Case &h94
		rem sub a,IYh
		carry=0
		a=ALUSUB(a,Higher(IY))
		bakeaf()
		rem Complete. 
	
	Case &h95
		rem sub a,IYl
		carry=0
		a=ALUSUB(a,Lower(IY))
		bakeaf()
		rem Complete. 

		
	Case &h96
		rem sub a,(IY+d)
		carry=0
		disp=FixDisp(NextPC())
		a=ALUSUB(a,fetch(IY+disp))
		BAKEAF()
		rem Complete. 

	Case &h9C
		rem sbc a,IYh
		a=ALUSUB(a,Higher(IY))
		bakeaf()
		rem Complete. 
	
	Case &h9D
		rem sbc a,IYl
		a=ALUSUB(a,Lower(IY))
		bakeaf()
		rem Complete. 
		
	Case &h9E
		rem sbc a,(IY+d)
		disp=FixDisp(NextPC())
		a=ALUSUB(a,fetch(IY+disp))
		rem Complete. 

	Case &hA4
		rem and IYh
		a=ALUAND(a,Higher(IY))
		bakeaf()
		rem Complete. 
	
	Case &hA5
		rem and IYl
		a=ALUAND(a,Lower(IY))
		bakeaf()
		rem Complete. 

	Case &hA6
		rem and (IY+d)
		disp=FixDisp(NextPC())
		a=ALUAND(a,fetch(IY+disp))
		rem Complete. 
	


	Case &hAC
		rem xor IYh
		a=ALUXOR(a,Higher(IY))
		bakeaf()
		rem Complete. 
	
	Case &hAD
		rem xor IYl
		a=ALUXOR(a,Lower(IY))
		bakeaf()
		rem Complete. 

	Case &hAE
		rem xor (IY+d)
		disp=FixDisp(NextPC())
		a=ALUXOR(a,fetch(IY+disp))
		rem Complete. 
	
	Case &hB4
		rem or IYh
		a=ALUOR(a,Higher(IY))
		bakeaf()
		rem Complete. 
	
	Case &hB5
		rem or IYl
		a=ALUOR(a,Lower(IY))
		bakeaf()
		rem Complete. 


	Case &hB6
		rem or (IY+d)
		disp=FixDisp(NextPC())
		a=ALUOR(a,fetch(IY+disp))
		rem Complete. 
	
	Case &hBC
		rem cp IYh
		a=ALUCP(a,Higher(IY))
		bakeaf()
		rem Complete. 
	
	Case &hBD
		rem cp IYl
		a=ALUCP(a,Lower(IY))
		bakeaf()
		rem Complete. 
		
	Case &hBE
		rem cp (IY+d)
		carry=0
		disp=FixDisp(NextPC())
		a=ALUCP(a,fetch(IY+disp))
		BAKEAF()
		rem Complete. 

	Case &hCB
		rem DDCB
		DDCB()		: rem Jump to DDCB. 	


	Case &hE1
		rem pop IY
		
		IY=POP()
		rem Unrem BakeIY()

	Case &hE3
		rem ex (sp),IY
		TEMPPC=POP()
		PUSH IY
		IY=TEMPPC

	Case &hE5
		rem  push IY
		PUSH(IY)
		
	Case &hE9
		rem jp (IY)	
		PC=IY

	Case &hF9
		rem ld	sp,IY
		STACK=IY



	
	Case Else
		result=0
				rem PRINT "EMULATOR UNDOCUMENTED CODE."
End Select

return result
End function




Function ED () as integer
rem: Return a binary result - 0 = no opcode NextPCuted or error.
dim opcode as integer
dim result as integer

opcode=NextPC()

Select Case As Const opcode
	Case 0
		rem Not supposed to exist. Undocumented. 
		result=0
		rem Complete
		
	Case &h40
		rem in b,(c)
		b=inBC(BC)
		BakeBC()
		rem Complete
	
	Case &h41
		rem out (c),b
		OutBC(BC,b)
		rem Complete
		
	Case &h42
		rem sbc hl,bc
		hl=alusub16(hl,bc)
		UnbakeHL()
	
	Case &h43
		rem ld (xx),bc
		TempPC=NextPC()+(NextPC()*256)
		set(TempPC,c)
		TempPC=(TempPC+1) mod 65536
		set(TempPC,b)
		rem complete
		
	Case &h44
		rem neg
		a=alusub(0,a)
		rem Complete.
		
	Case &h45
		rem 	retn
		PC=POP()
		rem Incomplete - Add in interrupt control flags later.
		
	Case &h46
		rem im0
		rem Incomplete - Add in interrupt control modes later. 
		
	Case &h47
		rem ld	i,a
		i=a		: rem Interrupt control register.
		rem Incompleted - Doesn't do anything yet. 
		
		
	Case &h48
		rem in c,(c)
		c=inBC(BC)
		BakeBC()
		rem Complete
	
	Case &h49
		rem out (c),c
		OutBC(BC,c)
		rem Complete
		
	Case &h4A
		rem adc hl,bc
		HL=ALUADD16(HL,BC)
		BAKEAF()
		rem complete
		
	Case &h4B
		rem ld bc,(xx)
		TempPC=NextPC()+(NextPC()*256)
		c=fetch(TempPC)
		TempPC=(TempPC+1) mod 65536
		b=fetch(TempPC)
		BakeBC()
		rem complete
		
	Case &h4C
		rem neg
		a=alusub(0,a)
		rem Complete.

	Case &h4D
		rem reti
		PC=POP()
		rem Incomplete - Add in interrupt control flags later.
		
	Case &h4E
		rem not documented. 
		rem Incomplete - Add in interrupt control modes later. 
		
	Case &h4F
		rem ld	r,a
		r=a		: rem Interrupt control register.
		rem Complete. I think. 
		
	
	Case &h50
		rem in d,(c)
		d=inBC(BC)
		BakeDE()
		rem Complete
	
	Case &h51
		rem out (c),d
		OutBC(BC,d)
		rem Complete
		
	Case &h52
		rem sbc hl,de
		HL=ALUSUB16(HL,DE)		: rem Perform subtract and set flags. 	
		UnbakeHL()
	
	Case &h53
		rem ld (xx),de
		TempPC=NextPC()+(NextPC()*256)
		set(TempPC,e)
		TempPC=(TempPC+1) mod 65536
		set(TempPC,d)
		rem complete
		
	Case &h54
		rem neg
		a=alusub(0,a)
		rem Complete.
		
	Case &h55
		rem 	retn
		PC=POP()
		rem Incomplete - Add in interrupt control flags later.
		
	Case &h56
		rem im1
		rem Incomplete - Add in interrupt control modes later. 
		
	Case &h57
		rem ld	a,i
		a=I		: rem Interrupt control register.
		rem Complete. I think.  Interrupts not working yet. 
		
		
	Case &h58
		rem in e,(c)
		e=inBC(BC)
		BakeDE()
		rem Complete
	
	Case &h59
		rem out (c),e
		OutBC(BC,e)
		rem Complete
		
	Case &h5A
		rem adc	hl,de
		HL=ALUADD16(HL,DE)
		UnbakeHL()
		rem complete
		
	Case &h5B
		rem ld de,(xx)
		TempPC=NextPC()+(NextPC()*256)
		e=fetch(TempPC)
		TempPC=(TempPC+1) mod 65536
		d=fetch(TempPC)
		BakeDE()
		rem complete
		
	Case &h5C
		rem neg
		a=alusub(0,a)
		rem Complete.

	Case &h5D
		rem retn
		PC=POP()
		rem Incomplete - Add in interrupt control flags later.
		
	Case &h5E
		rem im 2
		rem Incomplete - Add in interrupt control modes later. 
		
	Case &h5F
		rem ld	a,r
		a=r		: rem refresh control register.
		rem Complete. I think. 
	
	Case &h60
		rem in h,(c)
		h=inBC(BC)
		BakeHL()
		rem Complete
	
	Case &h61
		rem out (c),h
		OutBC(BC,h)
		rem Complete
		
	Case &h62
		rem sbc hl,hl
		hl=alusub16(hl,hl)
		UnbakeHL()
	
	Case &h63
		rem ld (xx),hl
		TempPC=NextPC()+(NextPC()*256)
		set(TempPC,l)
		TempPC=(TempPC+1) mod 65536
		set(TempPC,h)
		rem complete
		
	Case &h64
		rem neg
		a=alusub(0,a)
		rem Complete.
		
	Case &h65
		rem 	retn
		PC=POP()
		rem Incomplete - Add in interrupt control flags later.
		
	Case &h66
		rem Undocumented
		rem Incomplete - Add in interrupt control modes later. 
		
	Case &h67
		rem rrd
		rrd()
		rem Complete. I think. 
		
		
	Case &h68
		rem in l,(c)
		l=inBC(BC)
		BakeHL()
		rem Complete
	
	Case &h69
		rem out (c),l
		OutBC(BC,l)
		rem Complete
		
	Case &h6A
		rem adc	hl,hl
		HL=ALUADD16(HL,HL)
		UnbakeHL()
		rem complete
		
	Case &h6B
		rem ld hl,(xx)
		TempPC=NextPC()+(NextPC()*256)
		l=fetch(TempPC)
		TempPC=(TempPC+1) mod 65536
		h=fetch(TempPC)
		BakeHL()
		rem complete
		
	Case &h6C
		rem neg
		a=alusub(0,a)
		rem Complete.

	Case &h6D
		rem retn
		PC=POP()
		rem Incomplete - Add in interrupt control flags later.
		
	Case &h6E
		rem Undocumented
		rem Incomplete - Add in interrupt control modes later. 
		
	Case &h6F
		rem rld
		RLD()
		rem Complete. I think. 
		
	Case &h70
		rem in f,(c)
		rem fucknows.
		rem Complete
	
	Case &h71
		rem out (c),0
		rem fucknows. 
		rem Complete
		
	Case &h72
		rem sbc hl,sp
		hl=alusub16(hl,stack)
		UnbakeHL()
	
	Case &h73
		rem ld (xx),sp
		TempPC=NextPC()+(NextPC()*256)
		set(TempPC,(Stack mod 256))
		TempPC=(TempPC+1) mod 65536
		set(TempPC,(int(Stack/256)))
		rem complete
		
	Case &h74
		rem neg
		a=alusub(0,a)
		rem Complete.
		
	Case &h75
		rem 	retn
		PC=POP()
		rem Incomplete - Add in interrupt control flags later.
		
	Case &h76
		rem Undocumented
		rem Incomplete - Add in interrupt control modes later. 
		
	Case &h77
		rem Undocumented
		rem Complete. I think. 
		
		
	Case &h78
		rem in a,(c)
		a=inBC(BC)
		rem Complete
	
	Case &h79
		rem out (c),a
		OutBC(BC,a)
		rem Complete
		
	Case &h7A
		rem adc hl,sp
		HL=ALUADD16(HL,Stack)
		UnbakeHL()
		rem complete
		
	Case &h7B
		rem ld sp,(xx)
		TempPC=NextPC()+(NextPC()*256)
		Stack=fetch(TempPC)
		TempPC=(TempPC+1) mod 65536
		Stack=Stack+(fetch(TempPC)*256)
		rem complete
		
	Case &h7C
		rem neg
		a=alusub(0,a)
		rem Complete.

	Case &h7D
		rem reti
		PC=POP()
		rem Incomplete - Add in interrupt control flags later.
		
	Case &h7E
		rem Undocumented
		rem Incomplete - Add in interrupt control modes later. 
		
	Case &h7F
		rem rld
		RLD()
		rem Complete. I think. 
		
	Case &hA0
		rem ldi
		LDI()
		rem Complete
		
	Case &hA1
		rem cpi
		CPI()
		rem Complete
		
	Case &hA2
		rem ini
		INI()
		rem Complete
		
	Case &hA3
		rem outi
		OUTI()
		rem Complete
		
	Case &hA8
		rem ldd
		LDD()
		rem Complete
		
	Case &hA9
		rem cpd
		CPD()
		rem Complete
		
	Case &hAA
		rem ind
		IND()
		rem Complete
	
	Case &hAB 
		rem outd
		OUTD()
		rem Complete
		
	Case &hB0
		rem ldir

		LDIR1:
				LDI()
				if BC <> 0 then goto LDIR1
				half=0
				
	Case &hB1
		rem cpir

		CPIR1:
				CPI()
				
				if BC <> 0 and Z <> 1 then goto CPIR1
				half=0
				
	Case &hB2
		rem inir

		INIR1:
				INI()
				if b <> 0 then goto INIR1
				half=0
				
	Case &hB3
		rem otir

		OTIR1:
				OUTI()
				if b <> 0 then goto OTIR1
				half=0		
				
	Case &hB8
		rem lddr

		LDDR1:
				LDD()
				if BC <> 0 then goto LDDR1
				half=0
				
	Case &hB9
		rem cpir

		CPDR1:
				CPD()
				if BC <> 0 and Z<>1 then goto CPDR1		
				half=0
				
	Case &hBA
		rem inir

		INDR1:
				IND()
				if b <> 0 then goto INDR1
				half=0
		
	Case &hBB
		rem otdr

		OTDR1:
				OUTD()
				if b <> 0 then goto OTDR1
				half=0	
		
	Case Else
		result=0
				rem PRINT "EMULATOR UNDOCUMENTED CODE."
End Select

return result
End function

Function M1 () as integer
rem: Return a binary result - 0 = no opcode NextPCuted or error.
dim opcode as integer
dim result as integer

opcode=NextPC()

if debug>0 then lastcode = m1dis(opcode)

result=1

Select Case As Const opcode
	Case &h00
		rem NOP
		

	Case &h01
		rem ld bc,xx
		BC=NextPC()+(NextPC()*256)
		UnbakeBC()
		rem Complete

	Case &h02
		rem ld (bc),a
		set(BC,A)
		rem Complete

	Case &h03
		rem inc BC
		BC=(BC+1) mod 65536
		UnbakeBC()
		Rem Complete
		
	Case &h04
		rem inc b
		b=ALUINC(b)
		BakeBC()
		Rem Complete

	Case &h05
		rem dec b
		b=ALUDEC(b)
		BakeBC()
		Rem Complete

	Case &h06
		rem ld b,x
		b=NextPC()
		BakeBC()
		Rem Complete
		
	Case &h07
		rem rlca
		carry=bittest(128,a)
		a=((a shl 1) mod 256)+carry
		half=0
		n=0
		BakeAF()
		rem complete. 
		
		
	Case &h08
		rem ex af,af'
		temppc=AF
		AF=AF1
		AF1=temppc
		unbakeAF()
		rem Complete
		
	Case &h09
		rem add hl,bc
		half=bittest(4096,((HL AND &hFFF)+(BC AND &hFFF)))
		HL=HL+BC
		n=0
		carry=bittest(65536,HL)
		hl=hl mod 65536
		UnbakeHL()
		BAKEAF()
		rem Complete
		
	Case &h0A
		rem ld a,(BC)
		a=Fetch(BC)
		rem Complete
		
	Case &h0B
		rem dec	bc
		BC=(BC+65535) mod 65536
		UnbakeBC()
		Rem Complete
		
	Case &h0C
		rem inc	c
		c=ALUINC(c)
		BakeBC()
		rem Complete
	
	Case &h0D
		rem dec c
		c=ALUDEC(c)
		BakeBC()
		rem Complete
		
	Case &h0E
		rem ld	c,x
		c=NextPC()
		BakeBC()
		rem Complete
		
	Case &h0F
		rem rrca
		carry=bittest(1,a)
		a=a+(carry*256)
		a=((a shr 1) mod 256)
		half=0
		n=0
		BakeAF()
		rem complete. 
		
	Case &h10
		rem djnz x
		UnbakeBC()
		vector=NextPC()
		b=(b+255) mod 256						: rem DEC B. No flags affected - Not even zero apparently?. 
		if b <> 0 then 		
			JumpRelative(vector)
		endif
		BakeBC()
		rem Complete	

	Case &h11
		rem ld DE,xx
		DE=NextPC()+(NextPC()*256)
		UnbakeDE()
		rem complete

	Case &h12
		rem ld (DE),a
		set(DE,A)
		rem complete

	Case &h13
		rem inc DE
		DE=(DE+1) mod 65536
		UnbakeDE()
		Rem complete
		
	Case &h14
		rem inc d
		d=ALUINC(d)
		BakeDE()
		Rem complete

	Case &h15
		rem dec d
		d=ALUDEC(d)
		BakeDE()
		Rem complete

	Case &h16
		rem ld d,x
		d=NextPC()
		BakeDE()
		Rem complete
		
	Case &h17
		rem rla 
		a=((a shl 1))+carry
		carry=bittest(256,a)
		a=a mod 256		: rem Clean up the overflow. 
		half=0
		n=0
		BakeAF()
		rem complete. 
		
		
	Case &h18
		rem jr x
			vector=NextPC()
			JumpRelative(vector)
		rem Complete
		
	Case &h19
		rem add HL,DE
		half=bittest(4096,((HL AND &hFFF)+(DE AND &hFFF)))
		HL=HL+DE
		n=0
		carry=bittest(65536,HL)
		hl=hl mod 65536
		UnbakeHL()
		BAKEAF()
		rem complete
		
	Case &h1A
		rem ld a,(DE)
		a=Fetch(DE)
		rem complete
		
	Case &h1B
		rem dec	DE
		DE=(DE+65535) mod 65536
		UnbakeDE()
		Rem complete
		
	Case &h1C
		rem inc e
		e=ALUINC(e)
		BakeDE()
		rem complete
	
	Case &h1D
		rem dec e
		e=ALUDEC(e)
		BakeDE()
		rem complete
		
	Case &h1E
		rem ld	e,x
		e=NextPC()
		BakeDE()
		rem complete
		
	Case &h1F
		rem rra
		tempcarry=bittest(1,a)
		a=a+(carry*256)
		a=((a shr 1) mod 256)
		carry=tempcarry		: rem get the missing (lost) bit. 
		half=0
		n=0
		BakeAF()
		rem complete. 

	Case &h20
		rem jr nz, x
		vector=NextPC()
		if z=0 then JumpRelative(Vector)
		rem Complete	

	Case &h21
		rem ld HL,xx
		HL=NextPC()+(NextPC()*256)
		UnbakeHL()
		rem complete

	Case &h22
		rem ld (xx),HL
		TempPC=NextPC()+(NextPC()*256)
		set(TempPC,l)
		TempPC=(TempPC+1) mod 65536
		set(TempPC,h)
		rem complete

	Case &h23
		rem inc HL
		HL=(HL+1) mod 65536
		UnbakeHL()
		Rem complete
		
	Case &h24
		rem inc h
		h=ALUINC(h)
		BakeHL()
		Rem complete

	Case &h25
		rem dec h
		h=ALUDEC(h)
		BakeHL()
		Rem complete

	Case &h26
		rem ld h,x
		h=NextPC()
		BakeHL()
		Rem complete
		
	Case &h27
		rem daa 
		rem Got to write this up... More to do.
		rem Incomplete. 
		a=DAA()
		
	Case &h28
		rem jr z,x
		Vector=NextPC()
		if z = 1 then JumpRelative(vector)
		rem Complete	
		
	Case &h29
		rem add HL,HL
		half=bittest(4096,((HL AND &hFFF)+(HL AND &hFFF)))
		HL=HL+HL
		n=0
		carry=bittest(65536,HL)
		hl=hl mod 65536
		UnbakeHL()
		BAKEAF()
		rem complete
		
	Case &h2A
		rem ld HL,(xx)
		TempPC=NextPC()+(NextPC()*256)
		l=fetch(TempPC)
		TempPC=(TempPC+1) mod 65536
		h=fetch(TempPC)
		BakeHL()
		rem complete
		
	Case &h2B
		rem dec	HL
		HL=(HL+65535) mod 65536
		UnbakeHL()
		Rem complete
		
	Case &h2C
		rem inc	l
		l=ALUINC(l)
		BakeHL()
		rem complete
	
	Case &h2D
		rem dec l
		l=ALUDEC(l)
		BakeHL()
		rem complete
		
	Case &h2E
		rem ld	l,x
		l=NextPC()
		BakeHL()
		rem complete
		
	Case &h2F
		rem cpl
		a=(a XOR &hFF)
		n=1
		h=1
		bakeaf()
		rem complete. 
		
	Case &h30
		rem jr nc, x
		Vector=NextPC()
		if carry=0 then JumpRelative(vector)
		rem Complete	

	Case &h31
		rem ld Stack,xx
		Stack=NextPC()+(NextPC()*256)
		rem complete

	Case &h32
		rem ld (xx),a
		TempPC=NextPC()+(NextPC()*256)
		set(TempPC,a)
		rem complete

	Case &h33
		rem inc Stack
		Stack=(Stack+1) mod 65536
		Rem complete
		
	Case &h34
		rem inc (hl)
		Set(HL,ALUINC(Fetch(HL)))
		Rem complete

	Case &h35
		rem dec (hl)
		Set(HL,ALUDEC(Fetch(HL)))
		Rem complete

	Case &h36
		rem ld (hl),x
		Set(HL,NextPC())
		Rem complete
		
	Case &h37
		rem scf 
		carry=1
		Half=0
		N=0
		BAKEAF()
		rem Complete. 
		
		
	Case &h38
		rem jr c,x
		Vector=NextPC()
		if carry=1 then JumpRelative(vector)
		rem Complete	
		
	Case &h39
		rem add HL,Stack
		half=bittest(4096,((HL AND &hFFF)+(Stack AND &hFFF)))
		HL=HL+Stack
		n=0
		carry=bittest(65536,HL)
		HL=HL mod 65536
		UnbakeHL()
		BAKEAF()
		rem complete
		
	Case &h3A
		rem ld a,(xx)
		TempPC=NextPC()+(NextPC()*256)
		a=fetch(TempPC)

		rem complete
		
	Case &h3B
		rem dec	Stack
		Stack=(Stack+65535) mod 65536
		Rem complete
		
	Case &h3C
		rem inc	a
		a=ALUINC(a)
		bakeaf()
		rem complete
	
	Case &h3D
		rem dec a
		a=ALUDEC(a)
		bakeaf()
		rem complete
		
	Case &h3E
		rem ld	a,x
		a=NextPC()
		rem complete
		
	Case &h3F
		rem ccf
		half=carry 					: Rem says previous carry???? 
		carry=(carry+1) mod 2		: rem Complement Carry Flag.. Not f'n clear... :(
		n=0

		BAKEAF()
		rem complete. 
		
		
		
rem 8 bit load group.
rem ld r,r

	Case &h40
		rem ld b,b
		b=b
		BakeBC()
		rem Complete. 
		
	Case &h41
		rem ld b,c
		b=c
		BakeBC()
		rem Complete. 
		
	Case &h42
		rem ld b,d
		b=d
		BakeBC()
		rem Complete. 
		
	Case &h43
		rem ld b,e
		b=e
		BakeBC()
		rem Complete. 

	Case &h44
		rem ld b,h
		b=h
		BakeBC()
		rem Complete. 
		
	Case &h45
		rem ld b,l
		b=l
		BakeBC()
		rem Complete. 
		
	Case &h46
		rem ld b,(hl)
		b=fetch(hl)
		BakeBC
		rem Complete. 
		
	Case &h47
		rem ld b,a
		b=a
		BakeBC()
		rem Complete. 		

	Case &h48
		rem ld c,b
		c=b
		BakeBC()
		rem Complete. 
		
	Case &h49
		rem ld c,c
		c=c
		BakeBC()
		rem Complete. 
		
	Case &h4A
		rem ld c,d
		c=d
		BakeBC
		rem Complete. 
		
	Case &h4B
		rem ld c,e
		c=e
		BakeBC()
		rem Complete. 

	Case &h4C
		rem ld c,h
		c=h
		BakeBC()
		rem Complete. 
		
	Case &h4D
		rem ld c,l
		c=l
		BakeBC()
		rem Complete. 
		
	Case &h4E
		rem ld c,(hl)
		c=fetch(hl)
		BakeBC()
		rem Complete. 
		
	Case &h4F
		rem ld c,a
		c=a
		BakeBC()
		rem Complete.

	Case &h50
		rem ld d,b
		d=b
		BakeDE()
		rem Complete. 
		
	Case &h51
		rem ld d,c
		d=c
		BakeDE()
		rem Complete. 
		
	Case &h52
		rem ld d,d
		d=d
		BakeDE()
		rem Complete. 
		
	Case &h53
		rem ld d,e
		d=e
		BakeDE()
		rem Complete. 

	Case &h54
		rem ld d,h
		d=h
		BakeDE()
		rem Complete. 
		
	Case &h55
		rem ld d,l
		d=l
		BakeDE()
		rem Complete. 
		
	Case &h56
		rem ld d,(hl)
		d=fetch(hl)
		BakeDE()
		rem Complete. 
		
	Case &h57
		rem ld d,a
		d=a
		BakeDE()
		rem Complete. 		

	Case &h58
		rem ld e,b
		e=b
		BakeDE()
		rem Complete. 
		
	Case &h59
		rem ld e,c
		e=c
		BakeDE()
		rem Complete. 
		
	Case &h5A
		rem ld e,d
		e=d
		BakeDE()
		rem Complete. 
		
	Case &h5B
		rem ld e,e
		e=e
		BakeDE()
		rem Complete. 

	Case &h5C
		rem ld e,h
		e=h
		BakeDE()
		rem Complete. 
		
	Case &h5D
		rem ld e,l
		e=l
		BakeDE()
		rem Complete. 
		
	Case &h5E
		rem ld e,(hl)
		e=fetch(hl)
		BakeDE()
		rem Complete. 
		
	Case &h5F
		rem ld e,a
		e=a
		BakeDE()
		rem Complete.
		
	Case &h60
		rem ld h,b
		h=b
		BakeHL()
		rem Complete. 
		
	Case &h61
		rem ld h,c
		h=c
		BakeHL()
		rem Complete. 
		
	Case &h62
		rem ld h,d
		h=d
		BakeHL()
		rem Complete. 
		
	Case &h63
		rem ld h,e
		h=e
		BakeHL()
		rem Complete. 

	Case &h64
		rem ld h,h
		h=h
		BakeHL()
		rem Complete. 
		
	Case &h65
		rem ld h,l
		h=l
		BakeHL()
		rem Complete. 
		
	Case &h66
		rem ld h,(hl)
		h=fetch(hl)
		BakeHL()
		rem Complete. 
		
	Case &h67
		rem ld h,a
		h=a
		BakeHL()
		rem Complete. 		

	Case &h68
		rem ld l,b
		l=b
		BakeHL()
		rem Complete. 
		
	Case &h69
		rem ld l,c
		l=c
		BakeHL()
		rem Complete. 
		
	Case &h6A
		rem ld l,d
		l=d
		BakeHL()
		rem Complete. 
		
	Case &h6B
		rem ld l,e
		l=e
		BakeHL()
		rem Complete. 

	Case &h6C
		rem ld l,h
		l=h
		BakeHL()
		rem Complete. 
		
	Case &h6D
		rem ld l,l
		l=l
		BakeHL()
		rem Complete. 
		
	Case &h6E
		rem ld l,(hl)
		l=fetch(hl)
		BakeHL()
		rem Complete. 
		
	Case &h6F
		rem ld l,a
		l=a
		BakeHL()
		rem Complete.

	Case &h70
		rem ld (hl),b
		t=b
		set(hl,t)
		rem Complete. 
		
	Case &h71
		rem ld (hl),c
		t=c
		set(hl,t)
		rem Complete. 
		
	Case &h72
		rem ld (hl),d
		t=d
		set(hl,t)
		rem Complete. 
		
	Case &h73
		rem ld (hl),e
		t=e
		set(hl,t)
		rem Complete. 

	Case &h74
		rem ld (hl),h
		t=h
		set(hl,t)
		rem Complete. 
		
	Case &h75
		rem ld (hl),l
		t=l
		set(hl,t)
		rem Complete. 
		
	Case &h76
		rem halt
		rem Not really sure how to handle a halt - Need to set a flag.... 
		rem incomplete. 
		trap=10
		rem TRAP=10 = HALT 
		
	Case &h77
		rem ld (hl),a
		t=a
		set(hl,t)
		rem Complete. 		

	Case &h78
		rem ld a,b
		a=b
		bakeaf()
		rem Complete. 
		
	Case &h79
		rem ld a,c
		a=c
		bakeaf()
		rem Complete. 
		
	Case &h7A
		rem ld a,d
		a=d
		bakeaf()
		rem Complete. 
		
	Case &h7B
		rem ld a,e
		a=e
		bakeaf()
		rem Complete. 

	Case &h7C
		rem ld a,h
		a=h
		bakeaf()
		rem Complete. 
		
	Case &h7D
		rem ld a,l
		a=l
		bakeaf()
		rem Complete. 
		
	Case &h7E
		rem ld a,(hl)
		a=fetch(hl)
		bakeaf()
		rem Complete. 
		
	Case &h7F
		rem ld a,a
		a=a
		bakeaf()
		rem Complete.

rem Arithmetic Operators
rem

	Case &h80
		rem add a,b
		carry=0
		a=ALUADD(a,b)	
		bakeaf()		
		rem Complete. 
	
	Case &h81
		rem add a,c
		carry=0
		a=ALUADD(a,c)
		bakeaf()
		rem Complete. 
	
	Case &h82
		rem add a,d
		carry=0
		a=ALUADD(a,d)
		bakeaf()
		rem Complete. 
	
	Case &h83
		rem add a,e
		carry=0		
		a=ALUADD(a,e)
		bakeaf()
		rem Complete. 
	
	Case &h84
		rem add a,h
		carry=0
		a=ALUADD(a,h)
		bakeaf()
		rem Complete. 
	
	Case &h85
		rem add a,l
		carry=0
		a=ALUADD(a,l)
		bakeaf()
		rem Complete. 
	
	Case &h86
		rem add a,(hl)
		carry=0
		a=ALUADD(a,fetch(hl))
		bakeaf()
		rem Complete. 
	
	Case &h87
		rem add a,a
		carry=0
		a=ALUADD(a,a)
		bakeaf()
		rem Complete. 
	

	Case &h88
		rem adc a,b
		a=ALUADD(a,b)	
		bakeaf()		
		rem Complete. 
	
	Case &h89
		rem adc a,c
		a=ALUADD(a,c)
		bakeaf()
		rem Complete. 
	
	Case &h8A
		rem adc a,d
		a=ALUADD(a,d)
		bakeaf()
		rem Complete. 
	
	Case &h8B
		rem adc a,e	
		a=ALUADD(a,e)
		bakeaf()
		rem Complete. 
	
	Case &h8C
		rem adc a,h
		a=ALUADD(a,h)
		bakeaf()
		rem Complete. 
	
	Case &h8D
		rem adc a,l
		a=ALUADD(a,l)
		bakeaf()
		rem Complete. 
	
	Case &h8E
		rem adc a,(hl)
		a=ALUADD(a,fetch(hl))
		bakeaf()
		rem Complete. 
	
	Case &h8F
		rem adc a,a
		a=ALUADD(a,a)
		bakeaf()
		rem Complete.


	Case &h90
		rem sub a,b
		carry=0
		a=ALUSUB(a,b)	
		bakeaf()		
		rem Complete. 
	
	Case &h91
		rem sub a,c
		carry=0
		a=ALUSUB(a,c)
		bakeaf()
		rem Complete. 
	
	Case &h92
		rem sub a,d
		carry=0
		a=ALUSUB(a,d)
		bakeaf()
		rem Complete. 
	
	Case &h93
		rem sub a,e
		carry=0		
		a=ALUSUB(a,e)
		bakeaf()
		rem Complete. 
	
	Case &h94
		rem sub a,h
		carry=0
		a=ALUSUB(a,h)
		bakeaf()
		rem Complete. 
	
	Case &h95
		rem sub a,l
		carry=0
		a=ALUSUB(a,l)
		bakeaf()
		rem Complete. 
	
	Case &h96
		rem sub a,(hl)
		carry=0
		a=ALUSUB(a,fetch(hl))
		bakeaf()
		rem Complete. 
	
	Case &h97
		rem sub a,a
		carry=0
		a=ALUSUB(a,a)
		bakeaf()
		rem Complete. 
	

	Case &h98
		rem sbc a,b
		a=ALUSUB(a,b)	
		bakeaf()		
		rem Complete. 
	
	Case &h99
		rem a,c
		a=ALUSUB(a,c)
		bakeaf()
		rem Complete. 
	
	Case &h9A
		rem sbc a,d
		a=ALUSUB(a,d)
		bakeaf()
		rem Complete. 
	
	Case &h9B
		rem sbc a,e	
		a=ALUSUB(a,e)
		bakeaf()
		rem Complete. 
	
	Case &h9C
		rem sbc a,h
		a=ALUSUB(a,h)
		bakeaf()
		rem Complete. 
	
	Case &h9D
		rem sbc a,l
		a=ALUSUB(a,l)
		bakeaf()
		rem Complete. 
	
	Case &h9E
		rem sbc a,(hl)
		a=ALUSUB(a,fetch(hl))
		bakeaf()
		rem Complete. 
	
	Case &h9F
		rem sbc a,a
		a=ALUSUB(a,a)
		bakeaf()
		rem Complete.

	Case &hA0
		rem and b
		a=ALUAND(a,b)	
		bakeaf()		
		rem Complete. 
	
	Case &hA1
		rem and c
		a=ALUAND(a,c)
		bakeaf()
		rem Complete. 
	
	Case &hA2
		rem and d
		a=ALUAND(a,d)
		bakeaf()
		rem Complete. 
	
	Case &hA3
		rem and e	
		a=ALUAND(a,e)
		bakeaf()
		rem Complete. 
	
	Case &hA4
		rem and h
		a=ALUAND(a,h)
		bakeaf()
		rem Complete. 
	
	Case &hA5
		rem and l
		a=ALUAND(a,l)
		bakeaf()
		rem Complete. 
	
	Case &hA6
		rem and (hl)
		a=ALUAND(a,fetch(hl))
		bakeaf()
		rem Complete. 
	
	Case &hA7
		rem and a
		a=ALUAND(a,a)
		bakeaf()
		rem Complete.

	Case &hA8
		rem xor b
		a=ALUXOR(a,b)	
		bakeaf()		
		rem Complete. 
	
	Case &hA9
		rem xor c
		a=ALUXOR(a,c)
		bakeaf()
		rem Complete. 
	
	Case &hAA
		rem xor d
		a=ALUXOR(a,d)
		bakeaf()
		rem Complete. 
	
	Case &hAB
		rem xor e	
		a=ALUXOR(a,e)
		bakeaf()
		rem Complete. 
	
	Case &hAC
		rem xor h
		a=ALUXOR(a,h)
		bakeaf()
		rem Complete. 
	
	Case &hAD
		rem xor l
		a=ALUXOR(a,l)
		bakeaf()
		rem Complete. 
	
	Case &hAE
		rem xor (hl)
		a=ALUXOR(a,fetch(hl))
		bakeaf()
		rem Complete. 
	
	Case &hAF
		rem xor a
		a=ALUXOR(a,a)
		bakeaf()
		rem Complete.

	Case &hB0
		rem or b
		a=ALUOR(a,b)	
		bakeaf()		
		rem Complete. 
	
	Case &hB1
		rem or c
		a=ALUOR(a,c)
		bakeaf()
		rem Complete. 
	
	Case &hB2
		rem or d
		a=ALUOR(a,d)
		bakeaf()
		rem Complete. 
	
	Case &hB3
		rem or e	
		a=ALUOR(a,e)
		bakeaf()
		rem Complete. 
	
	Case &hB4
		rem or h
		a=ALUOR(a,h)
		bakeaf()
		rem Complete. 
	
	Case &hB5
		rem or l
		a=ALUOR(a,l)
		bakeaf()
		rem Complete. 
	
	Case &hB6
		rem or (hl)
		a=ALUOR(a,fetch(hl))
		bakeaf()
		rem Complete. 
	
	Case &hB7
		rem or a
		a=ALUOR(a,a)
		bakeaf()
		rem Complete.

	Case &hB8
		rem cp b
		carry=0
		a=ALUCP(a,b)			
		rem Complete. 
	
	Case &hB9
		rem cp c
		carry=0
		a=ALUCP(a,c)
		rem Complete. 
	
	Case &hBA
		rem cp d
		carry=0
		a=ALUCP(a,d)
		rem Complete. 
	
	Case &hBB
		rem cp e	
		carry=0
		a=ALUCP(a,e)
		rem Complete. 
	
	Case &hBC
		rem cp h
		carry=0
		a=ALUCP(a,h)
		rem Complete. 
	
	Case &hBD
		rem cp l
		carry=0
		a=ALUCP(a,l)
		rem Complete. 
	
	Case &hBE
		rem cp (hl)
		carry=0
		a=ALUCP(a,fetch(hl))
		rem Complete. 
	
	Case &hBF
		rem cp a
		carry=0
		a=ALUCP(a,a)
		rem Complete.

	Case &hC0
		rem	ret nz
		
		if z=0 then PC=POP()
		
	Case &hC1
		rem pop bc
		
		BC=POP()
		UnbakeBC()
		
	Case &hC2
		rem jp nz,xx
		
		TEMPPC=NextPC()+(NextPC()*256)	: rem get next bytes either way.
		if z=0 then PC=TEMPPC
		
	
	Case &hC3
		rem jp xx
		TEMPPC=NextPC()+(NextPC()*256) : PC=TEMPPC

	Case &hC4
		rem call nz,xx
		TEMPPC=NextPC()+(NextPC()*256)	: rem get next bytes either way.
		if z=0 then PUSH(PC) : PC=TEMPPC
		
	Case &hC5
		rem  push bc
		PUSH(BC)
		
	Case &hC6
		rem add a,x
		carry=0
		a=ALUADD(a,NextPC())
		bakeaf()
		rem Complete. 
		
	Case &hC7
		rem RST 00H
		PUSH (PC)
		PC=&h0000
		

	Case &hC8
		rem	ret z
		
		if z=1 then PC=POP()
		
	Case &hC9
		rem ret (pop PC)
		
		PC=POP()
		
	Case &hCA
		rem jp z,xx
		
		TEMPPC=NextPC()+(NextPC()*256)	: rem get next bytes either way.
		if z=1 then PC=TEMPPC
		
	
	Case &hCB
		rem xxBITxx ( after CB )
		rem CASE fork here to a different function. PostM1 or something. Slightly iterative. 
		CB()


	Case &hCC
		rem call z,xx
		TEMPPC=NextPC()+(NextPC()*256)	: rem get next bytes either way.
		if z=1 then PUSH(PC) : PC=TEMPPC
		
	Case &hCD
		rem call xx
		TEMPPC=NextPC()+(NextPC()*256) 
		PUSH(PC)
		PC=TEMPPC
		
	Case &hCE
		rem adc a,x
		a=ALUADD(a,NextPC())
		bakeaf()
		rem Complete. 
		
	Case &hCF
		rem RST 08H
		PUSH (PC)
		PC=&h0008


	Case &hD0
		rem	ret nc
		
		if carry=0 then PC=POP()
		
	Case &hD1
		rem pop de
		
		DE=POP()
		UnbakeDE()
		
	Case &hD2
		rem jp nc,xx
		
		TEMPPC=NextPC()+(NextPC()*256)	: rem get next bytes either way.
		if carry=0 then PC=TEMPPC
		
	
	Case &hD3
		rem out (x),a
		Outport((a*256)+NextPC(),a) 			: rem A is both the output (data) and upper address lines. 
		rem PORT OUT HERE
		rem INCOMPLETE

	Case &hD4
		rem call nc,xx
		TEMPPC=NextPC()+(NextPC()*256)	: rem get next bytes either way.
		if carry=0 then PUSH(PC) : PC=TEMPPC
		
	Case &hD5
		rem  push de
		PUSH(DE)
		
	Case &hD6
		rem sub x
		carry=0
		a=ALUSUB(a,NextPC())
		bakeaf()
		rem Complete. 
		
	Case &hD7
		rem RST 10H
		PUSH (PC)
		PC=&h0010
		

	Case &hD8
		rem	ret c
		
		if carry=1 then PC=POP()
		
	Case &hD9
		rem exx
		TEMPPC=HL
		HL=HL1
		HL1=TEMPPC
		UnbakeHL()

		TEMPPC=DE
		DE=DE1
		DE1=TEMPPC
		UnbakeDE()

		TEMPPC=BC
		BC=BC1
		BC1=TEMPPC
		UnbakeBC()

rem		bakeAF()		: rem Assemble AF register. Because this is the only one we don't maintain sync for. 
rem		TEMPPC=AF
rem		AF=AF1
rem		AF1=TEMPPC
rem		UNbakeAF()	: rem Deconstruct AF register.	

	Case &hDA
		rem jp c,xx
		
		TEMPPC=NextPC()+(NextPC()*256)	: rem get next bytes either way.
		if carry=1 then PC=TEMPPC
		
	
	Case &hDB
		rem in a,(x)
		a=inport((a*256)+NextPC())		: rem read x anyway - Place existing value in A on upper address bus. 
		rem Need to build IO instructions here later. 

	Case &hDC
		rem call c,xx
		TEMPPC=NextPC()+(NextPC()*256)	: rem get next bytes either way.
		if carry=1 then PUSH(PC) : PC=TEMPPC
		
	Case &hDD
		rem xxIXxx ( After DD )
		rem CASE fork here to a different function. PostM1 or something. Slightly iterative. 
		rem Damn, I forgot to add this in for WEEKS... Let's see if it works. 
		DD ()	: rem Execute DD function. 
		
	Case &hDE
		rem sbc a,x
		a=ALUSUB(a,NextPC())
		bakeaf()
		rem Complete. 
		
	Case &hDF
		rem RST 18H
		PUSH (PC)
		PC=&h0018
		
		
	Case &hE0
		rem ret po		
		if pv=0 then PC=POP()
		
	Case &hE1
		rem pop hl
		
		HL=POP()
		UnbakeHL()

	Case &hE2
		rem jp po,xx	
		TEMPPC=NextPC()+(NextPC()*256)	: rem get next bytes either way.		
		if pv=0 then PC=TEMPPC
		
	
	Case &hE3
		rem ex (sp),hl
		TEMPPC=POP()
		PUSH HL
		HL=TEMPPC
		UnbakeHL()

	Case &hE4
		rem call po,xx
		TEMPPC=NextPC()+(NextPC()*256)	: rem get next bytes either way.
		if pv=0 then PUSH(PC) : PC=TEMPPC
		
	Case &hE5
		rem  push hl
		PUSH(HL)
		
	Case &hE6
		rem and x
		a=ALUAND(a,NextPC())
		bakeaf()
		rem Complete. 
		
	Case &hE7
		rem RST 20H
rem		PUSH (PC)
rem 	PC=&h0020
		debug=2

	Case &hE8
		rem ret pe
		if pv=1 then PC=POP()
		
	Case &hE9
		rem jp (hl)	
		PC=HL

	Case &hEA
		rem jp pe,xx
		TEMPPC=NextPC()+(NextPC()*256) :	Rem - Need to pick up the next two bytes anyway. 	
		if pv=1 then PC=TEMPPC
	
	Case &hEB
		rem ex de,hl
		temppc=de
		de=hl
		hl=temppc
		UnbakeDE()
		UnbakeHL()

	Case &hEC
		rem call pe,xx
		TEMPPC=NextPC()+(NextPC()*256)	: rem get next bytes either way.
		if pv=1 then PUSH(PC) : PC=TEMPPC
		
	Case &hED
		rem xx80xx After ED instruction set
		rem ED()
		
		ED()
		
	Case &hEE
		rem xor x
		a=ALUXOR(a,NextPC())
		bakeaf()
		rem Complete. 
		
	Case &hEF
		rem RST 28H
rem		PUSH (PC)
rem		PC=&h0028
	debug=0

	Case &hF0
		rem	ret p
		
		if s=0 then PC=POP()
		
	Case &hF1
		rem pop af
		
		AF=POP()
		unbakeAF()	: rem restore A and F registers.
		
	Case &hF2
		rem jp p,xx
		TEMPPC=NextPC()+(NextPC()*256) : rem Need to pick up next bytes anyway
		if s=0 then  PC=TEMPPC
		
	
	Case &hF3
		rem di
		rem Need to disable interrupts here.

	Case &hF4
		rem call p,xx
		TEMPPC=NextPC()+(NextPC()*256)	: rem get next bytes either way.
		if s=0 then PUSH(PC) : PC=TEMPPC
		
	Case &hF5
		rem  push af
		bakeAF()
		PUSH(AF)
		
	Case &hF6
		rem or x
		a=ALUOR(a,NextPC())
		bakeaf()
		rem Complete. 
		
	Case &hF7
		rem RST 30H
		PUSH (PC)
		PC=&h0030
		

	Case &hF8
		rem ret m
		
		if s=1 then PC=POP()
		
	Case &hF9
		rem ld	sp,hl
		STACK=HL

	Case &hFA
		rem jp m,xx
		TEMPPC=NextPC()+(NextPC()*256) :rem need to pick up next bytes anyway.
		if s=1 then PC=TEMPPC
		
	
	Case &hFB
		rem ei
		rem Enable Interrupts.

	Case &hFC
		rem call m,xx
		TEMPPC=NextPC()+(NextPC()*256)	: rem get next bytes either way.
		if s=1 then PUSH(PC) : PC=TEMPPC
		
	Case &hFD
		rem xxIYxx ( After FD )
		rem CASE fork here to a different function. PostM1 or something. Slightly iterative. 
		
		FD()
		
	Case &hFE
		rem cp x
		a=ALUCP(a,NextPC())
		rem Complete. 
		
	Case &hFF
		rem RST 38H
		PUSH (PC)
		PC=&h0038
		
		
		

	Case Else
		result=0
		rem PRINT "EMULATOR UNDOCUMENTED CODE."
End Select

return result
End function




Function Showreg() as integer

BakeAF()
if debug>0 then

	currentop=currentop+"                    "
	currentop=left(currentop,15)
	
	print #9,currentop;

	Print #9,tab(24); lastcode;
	
	Print #9,tab(40);
	Print #9,"EX:";hex(MMU(PC),5);" ";
	Print #9,"PC:";hex(PC,4);" : ";
	Print #9,"AF:";hex(AF,4);" ";
	Print #9,"BC:";hex(BC,4);" ";
	Print #9,"DE:";hex(DE,4);" ";
	Print #9,"HL:";hex(HL,4);" ";
	Print #9,"Stack:";hex(Stack,4);" ";
	
	Print #9,"Flags:";
	if s then print #9,"S"; else print #9,"-";
	if z then print #9,"Z"; else print #9,"-";
	print #9,"x";
	if half then print #9,"H"; else print #9,"-";
	print #9,"x";
	if PV then print #9,"P"; else print #9,"-";
	if n then print #9,"N"; else print #9,"-";
	if carry then print #9,"C"; else print #9,"-";
	print #9,"  ";
	
	endif

if debug > 1 then	
	Print #9,"AF':";hex(AF1,4);" ";
	Print #9,"BC':";hex(BC1,4);" ";
	Print #9,"DE':";hex(DE1,4);" ";
	Print #9,"HL':";hex(HL1,4);" ";
	Print #9,"IX:";hex(IX,4);" ";
	Print #9,"IY:";hex(IY,4);" ";
	PRINT #9,"(HL)";hex(fetch(HL),4);" ";
	PRINT #9,"(";hex(FIXED,4);")";hex(fetch(FIXED),2);",";hex(fetch(FIXED+1),2);hex(fetch(FIXED+2),2);hex(fetch(FIXED+3),2);
	endif
	
if debug>0 then print #9,
	
return 0
End Function

Function PCP() as integer
if debug>0 then print #9,"PC:";hex(PC,4);" ";
currentop=""
return 0
End Function




Function ShowRam() as integer
dim cx as integer
dim CODETEXT as string

Print "Showing RAM HERE";showstart,showend

CODETEXT=""

print "This is text:";CODETEXT

for cx=showstart+1 to showend+1
codetext=codetext+chr(code(MMU(cx)))
next cx

rem PRINTHEX (CODETEXT,(showend-showstart)+1)

CONSOLEPRINTHEX (CODETEXT,(showend-showstart)+1)

Print "End of Text"

return 0
End Function


Function SaveRam() as integer
dim count as integer

CODETEXT=""
for count=savestart+1 to saveend+1
CODETEXT=CODETEXT+chr(CODE(MMU(count)))
next count


SaveHEX (CODETEXT,(saveend-savestart)+1)


return 0
End Function


main:

debug=0
maxinstructions=65535
showmemory=0
showstart=0
showend=0
savememory=0
savestart=0
saveend=0
savefile=""
start=0 : rem default = start at 0.
LOADBIOS=""
LOADBDOS=""
LOADCCP=""
LOADTPA=""
FIXED=0		: rem Debugging also monitors a FIXED memory location. 

AF=&hFFFF
DE=&hFFFF
HL=&hFFFF
BC=&hFFFF
AF1=&hFFFF
DE1=&hFFFF
HL1=&hFFFF
BC1=&hFFFF
unbakeAF()
unbakeBC()
unbakeDE()
unbakeHL()

rem ANSI
inescape=0

rem External Registers.
startbit=1		: rem Startbit is set as resetted. 

rem open "loki.dbg" for output as #9
open "con:" for output as #9
rem Let's have a debug file now. 

open "textsave.txt" for output as #8


open "loki.cfg" for input as #1
READBATCH:
        INPUT #1,CLI
        CLI=CLI+" " : rem padding. s
        if left$(CLI,1)="#" then goto READBATCH



		debug=parameter("DEBUG",debug) 
		maxinstructions=parameter("MAXINSTRUCTIONS",maxinstructions)
		
		showmemory=parameter("SHOWMEMORY",showmemory)
		showstart=parameter("SHOWSTART",showstart)
		showend=parameter("SHOWEND",showend)
		
		savememory=parameter("SAVEMEMORY",savememory)
		savestart=parameter("SAVESTART",savestart)
		saveend=parameter("SAVEEND",saveend)
		savefile=parameterstring("SAVEFILE",savefile)
		
		LOADBIOS=parameterstring("LOADBIOS",LOADBIOS)
		LOADBDOS=parameterstring("LOADBDOS",LOADBDOS)
		LOADCCP=parameterstring("LOADCCP",LOADCCP)
		LOADTPA=parameterstring("LOADTPA",LOADTPA)

		STARTBIOS=parameter("STARTBIOS",startbios)
		STARTBDOS=parameter("STARTBDOS",startbdos)
		STARTCCP=parameter("STARTCCP",startccp)
		STARTTPA=parameter("STARTTPA",starttpa)
		FIXED=parameter("FIXED",fixed)

    if CLI<>" " then goto READBATCH
close #1

print "Preparing to execute";maxinstructions;" instuctions."

rem loadcode("boot.bin") : rem BOOT is the 0 sector information on startup. 256 bytes. 
rem Print "If length=0 check boot.bin is available."


rem	print "BOOTSTRAP=BOOTSTRAP.BIN at F0000 BY DEFAULT"
rem	BINARYLOADFILE("BOOTSTRAP.BIN",&h0F0000)			: rem The Bootstrap needs to occur at the END OF THE MEMORY.  Bootstrap is 4K of ROM. 

	print "BOOTSTRAP=LOKI.IMG at F0000 BY DEFAULT"
	BINARYLOADFILE("LOKI.IMG",&h0F0000)			: rem The Bootstrap needs to occur at the END OF THE MEMORY.  Bootstrap is 4K of ROM. 

	print "NVM=NVM.IMG at 80000 BY DEFAULT"
	BINARYLOADFILE("NVM.IMG",&h080000)			: rem Load up the NVM file - might be EEPROM or battery backed RAM. 


	print "VIDEOBIOS=VBIOS.IMG as E0000 BY DEFAULT"
	BINARYLOADFILE("VBIOS.IMG",&h0E0000)		: rem Video Bios, Network Bios, and two User Bios files. 
	
if LOADBIOS <> "" then									: rem leave these all in the usual location at 00000 to 0FFFF until BOOTSTRAP loads them. 
	print "Loadbios=";loadbios;" at";startbios
	BINARYLOADFILE(LOADBIOS,STARTBIOS)
	endif


if LOADBDOS <> "" then
	print "Loadbdos=";loadbdos;" at";startbdos
	BINARYLOADFILE(LOADBDOS,STARTBDOS)
	endif

	
if LOADCCP <> "" then
	print "Loadccp=";loadccp;" at";startccp
	BINARYLOADFILE(LOADCCP,STARTCCP)
	endif

	
if LOADTPA <> "" then
	print "Loadtpa=";loadtpa;" at";starttpa
	BINARYLOADFILE(LOADTPA,STARTTPA)
	endif
		

PC=start	: rem Set Program Counter.


rem loadexec("cpmbasic.bin") : rem Load in a program and run it. 


disk=loadfile("NEWLOKI.DSK")
rem disk=loadfile("LOKI2.DSK")


RESETz80()


trap=0 		
print TIME

count=0

rem ###############################
rem #                             #
rem # Let's get the party started #
rem #                             #
rem ###############################



TOP:
PCP():M1():Showreg()
count=count+1
if count < 0 then count=1
if count = maxinstructions then trap = 1
if trap = 0 then goto TOP




rem ###############################
rem #                             #
rem # The Party's Over... Go home #
rem #                             #
rem ###############################



print
Print TIME

Print "Executed";count;" instructions - Last instruction at ";hex(PC,4)



if debug > 0 then
	Print #9,"Config-"
	Print #9,"Debug:";debug
	Print #9,"Lines:";maxinstructions
	print #9,"Sbowstart:";showstart
	print #9,"Showend:";showend
	print #9,"Savememory:";savememory
	print #9,"Savestart:";savestart
	print #9,"Saveend:";saveend
	print #9,"Savefile:";savefile
	endif

print #9,

print #9,"Showing Zero Page for Process:"; hex(process,2)
showram()

print #9,"Ram Shown"
Print #9,"Current Process Map"
print #9,"PID:";
for a=0 to 15
print #9,"   ";hex(a,2);"   ";
next a
print #9,

for count=0 to 15
print #9,hex(count,2);
for process=0 to 15
	print #9," - ";hex(MMU(count*4096),5);
	next process
	print #9,
next count


if savememory then saveram()



rem Save disk as a different disk for testing. ( Main disk is read only )
open "OUTDISK.DSK" for output as #1
for count=1 to 1000000
print #1,mid(DISK,count,1);
next count
close #1


print #9,
print #9,"PAGE: 0"
rem Print out lower memory where the M: Directory is... 
codetext=""
for a=1 to 1024
codetext=codetext+chr(code(a))
next a
consoleprinthex(codetext,1024)


close #9
close #8

print "Press any key to exit emulation."

finalkey:
	if inkey$="" then goto finalkey

end
