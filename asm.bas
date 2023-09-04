rem Shitty z80 emulator to test code.
rem This is the Assembler... 
rem 16/11/2022 fixed bug with LDD DE,N ( or similar ) being confused for LDD. Check for other characters now. 
rem 17/11/2022 Wrote large test file to compile. Seems OK. Found a few missing instructions and typos. 
rem 18/11/2022 Fixed bugs in add IX/IY,RR commands due to failing to multiple the decoder by 16. 
rem 19/11/2022 Fixed issue with port simulator messing up the display when accessing the keyboard. 	
rem 20/11/2022 Fixed Relative Jump mask to detect Out Of Bounds relative jump targets. 
rem 23/11/2022 *BUG* EQU LABEL_LABEL,val works, but LABEL_LABEL EQU val only sees "LABEL_" as an instruction. ** NOT YET FIXED **
rem 3/12/2022  Fixed up label value detection failure due to Pass1 label message suppression. Identifies label value changing now. 
rem 10/05/2023 Fixed Casefix to ignore anything in quotes ( and to ensure that one or the other is recognized. )
rem 28/08/2023 Changed LitByte to SuperEVAL for DEFB..... Not sure if this is a good idea. I need to write a manual for this stuff. 
rem 28/08/2023 Changed Litbyte back. Can also use DEFC instead of DEFB.
rem 28/08/2023 Corrected 8 bit relative jump - was missing when the value was 80 - 


dim shared PC as integer		: rem Program Counter - We will use this to compile. Maybe I should use another? Will it clash? Also used in z80.bas
dim shared linePC as integer	: rem Since we change the PC as we progress through an instruction, sometimes we want to know where it started. 

dim shared vector as integer	: rem Because we need to read the vector anyway, for jump relative calculations. 
dim shared debug as integer		: rem If 1, then print debugging. 
dim shared debugonline as integer : rem If we just want to watch one line in the source... 
dim shared debugoffline as integer : Rem turn of debug here. 


dim shared count as integer		: rem General Counter. 
dim shared count2 as integer	: rem General Counter. 

dim shared CODE(65536) as integer	: rem Bytes of Code.
dim shared CODELINE(65535) as integer 	: rem Which line of assembly contributed to the code? For debugging and later help. 
dim shared CODETEXT as string	: rem for converting CODE to STRING. 
dim shared FIRSTCODE as integer : rem Start of code.
dim shared LASTCODE as integer  : rem End of code. 

dim shared BYTES as string	: rem Bytes we will put into CODE. 

dim shared TRAP as integer	: rem When we need to trap specific events... When not 0, a trap has occured. 

dim shared currentop as string	: rem current operation bytes in string format as hex. 

rem Configuration Files - Clean these up later. 
dim shared CLI as string : rem I will have CLI one day. 
dim shared MAXINSTRUCTIONS as integer : rem How many should we do before ending anyway?
dim shared showmemory as integer
dim shared showstart as integer
dim shared showend as integer
dim shared savememory as integer
dim shared savestart as integer
dim shared saveend as integer
dim shared savefile as string
dim shared showcode as integer : rem Do we want to show the code?


rem New Stuff for Assembler... 

dim shared Source as string		: rem Somewhere to load the file. 
dim shared SourceCount as integer : rem Source Counter. 
dim shared Operand as string	: rem when we pull out an operator 
dim shared Nominator as string	: rem Where we store what the operand works upon. 
dim shared Label as string		: rem Label for a vector 
dim shared Apos	as integer		: 
dim shared Literal as string	: rem Something in quotes. 

dim shared errorcode as integer		: rem Did we get an error?
dim shared errormessage as string : rem What was the error?

dim shared labletable(10000) as string	: rem Store of labels. Up to 32 bytes per label. 
	rem 32 bytes as label ( with dot padding ). 
	rem Byte 33/34 is memory location. 35/36 first vector... etc. Until End Of String. Then no more locations refer to Memory Location.
dim shared numlabels as integer	: rem number of labels. 	
dim shared relative as integer	: rem If 0, absolute. If 1, relative. Also is -7 for a number (ie, not a jump, but a referenced label)... 
dim shared linenumber as integer : rem Which line of the code are we on? For errors.
dim shared crlf as string : rem going to be a constant for EOL. 
dim shared oneline as string : rem one line of the assembly file at a time. It is a single pass assembler. 
dim shared originalline as string : rem What was the original line before we cut it up?
dim shared untouched as string : rem The original line and one line, but this time don't f'ing touch it. 
dim shared target as string : rem Target of operand - Let's store it when we analyse the line. It holds whatever is left 
dim shared pass as integer : rem "Which pass are we on?"
dim shared absolutelabel as integer : rem flag that shows we got an absolutel label from the current line.  DISABLE EQU. 
dim shared showlabels as integer : rem Flag to show the labels once we have assembled. 

dim shared comparerom as integer : rem If set, compare to ZX RoM. 
dim shared completed as integer	: rem if set, we have reached the logical end of the assembly file. 

dim shared asmfile as string : rem ASM filename
dim shared binfile as string : rem BIN file out. 
dim shared hexfile as string : rem HEX file out. 

dim shared firstword as string
dim shared secondword as string
dim shared thirdword as string 	: rem Break up a line logically. In case of EQUs and the like. 
dim shared tryagain as integer : rem Should we try to process the line again?


rem FUNCTIONS START HERE.


Function adderror(Byref description as string) as integer

if pass=1 then goto NOERROR

Errormessage=Errormessage + "Error line:" + str(Linenumber) +" "
Errormessage=Errormessage + description
rem Errormessage=Errormessage + "     MemLoc:"+hex(PC,4)+"  Instruction:"+Operand+Target
Errormessage=Errormessage + "     MemLoc:"+hex(PC,4)+"  Instruction:"+Untouched

Errormessage=Errormessage + CRLF	: rem New Line. 

NOERROR: 	rem we are just doing the label pass. Don't record errors here or we will double up.

return 0
End function

Function Adddebug (Byref mask as integer, Byref txt as string) as integer
rem Let's print the debug string if conditions are met.
rem Got to think about this.. 

return 0
End Function

Function Insert(ByRef target as String, ByRef contents as String, Byref location as integer) As String                  : rem Insert a string into a string. For modifying a DSK file.
rem Insert("ABCDEFG","XX",3) produces "ABXXDEFG"

        dim li as Integer : rem Length of inserted string.
        dim lt as Integer : rem Length of targetted string.

        target=left$(target,location-1)+contents+right$(target,len(target)-len(contents)-location+1)

        Return target
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


Function Casefix (Byref fname as string) as string
        dim cpos as integer
        dim result as string
		
		dim inquotes as integer	: rem if inquotes is 1, then don't FIX the case. We want the case to be maintained if it quotes. 
		dim singlechar as string : rem Single Character 
		
		inquotes=0
		
        result=""
        for cpos = 1 to len(fname)
		
		singlechar=mid(fname,cpos,1) 
		
rem		if singlechar="'" or singlechar=chr(34) then
		if singlechar="'" then
			inquotes=abs(inquotes-1)	: rem Toggle Inquotes 0>1 and 1>0
			endif
		
        if singlechar >= "a" and singlechar <="z" and inquotes=0 then
                result=result+chr((asc(singlechar)-32) mod 128)
                else
                result=result+chr(asc(singlechar) mod 128)     : rem MOD 128 removes any hidden bits ( bit 7 )
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
paraend2:
        return result
end function


Function Printhex (Byref image as string, Byref numhex as integer) as integer

        rem Image is the string you want to dump as HEX.
        rem numhex is the number of characters you want to display ( won't be less than 32 displayed... Might adjust that later. )

        dim a as integer
        dim b as integer
        dim c as integer

        print "         !0  1  2  3 !4  5  6  7 !8  9  A  B !C  D  E  F !0  1  2  3 !4  5  6  7 !8  9  A  B !C  D  E  F"

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


Function Showhex (Byref startofhex as integer, Byref Endofhex as integer) as integer

        rem Image is the string you want to dump as HEX.
        rem numhex is the number of characters you want to display ( won't be less than 32 displayed... Might adjust that later. )

        dim a as integer
        dim b as integer
        dim c as integer
		
		Print "Showing from:";hex(startofhex,4);" to";hex(endofhex,4)

        print "         !0  1  2  3 !4  5  6  7 !8  9  A  B !C  D  E  F !0  1  2  3 !4  5  6  7 !8  9  A  B !C  D  E  F"

        for a=int(startofhex / 32)*32 to (int(endofhex / 32) * 32 )  step 32
		

                print hex$(A,4);"  -  ";                             : rem Print relative location in Hex at start of line.

                for b=1 to 32
                        print hex(code(a+b),2);" ";  : rem Print out the hex of the bytes.
                next b

                print "  -  ";

                for b=1 to 32                                                                   : rem Now print out the ASCII characters.
                        c=code(a+b)
                        if c<32 then c=asc("+")
                        if c>127 then c=asc("+")
                        print chr$(c);
                next b

                print

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

		Print "Saving Hex File:";savefile


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


Function savebinary (Byref image as string, Byref numhex as integer) as integer

        rem Image is the string you want to dump as HEX.
        rem numhex is the number of characters you want to display ( won't be less than 32 displayed... Might adjust that later. )

        dim a as integer
        dim b as integer
        dim c as integer

if savefile="" then goto nosave

		Print "Saving Binary File:";savefile


		open savefile for output as #1
		

        print #1,image

		close #1

nosave:
        Return len (image)
End function

Function savecode () as integer
dim count as integer

if BINFILE="" or BINFILE=".BIN" then goto failsave
open BINFILE for output as #1

Print " Writing :";BINFILE;" start:";hex(firstcode,4);" end:";hex(lastcode,4)
	for	count=firstcode to lastcode
		print #1,chr(CODE(count+1));
	next count

close #1

	print "Written :";lastcode-firstcode-1;" bytes."
failsave:
return 0
End function

Function saveintelhex () as integer
dim count as integer
dim hnum as integer
dim csum as integer
dim upper as integer	: rem temp for upper address bytes.
dim lower as integer	: rem temp for lower address bytes.
 
rem Same as SaveCode but saves Intel HEX format. 
hnum=0	: rem First byte.

if HEXFILE="" or HEXFILE=".HEX" then goto failsave
open HEXFILE for output as #1


Print " Writing HEX :";HEXFILE;" start:";hex(firstcode,4);" end:";hex(lastcode,4)
	for	count=firstcode to lastcode
		if hnum=0 then
			print #1,":10"; : csum=csum+&h10
			upper=INT (count/256) 
			lower=count mod 256
			print #1,hex(upper,2); : csum=csum+upper
			print #1,hex(lower,2); : csum=csum+lower
			print #1,hex(0,2); : csum=csum+0	: rem Seems to be this extra 00 after the address? 
		endif
		
		print #1,hex(CODE(count+1),2); : csum=csum+CODE(count+1)

	hnum=hnum+1
	
	if hnum=16 then
			hnum=0
			csum=256-csum	: rem 2's complement. Kind of. 
			print #1,hex(csum,2)
			csum=0	: rem reset checksum. 
	endif
	
	next count

close #1

	print "Written :";lastcode-firstcode-1;" bytes."
failsave:
return 0
End function



Function LoadSource (Byref filename as String) As Integer
dim count as integer	: rem where in CODE are we?

	Source=Loadfile (Filename)
	
		Print "Length of file:";len(Source)
	

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


Function fetch (Byref location as integer) as integer
rem Get the next instruction or data from PCs...
dim result as integer

result=CODE(location+1)

return result
End function 

Function set (Byref location as integer, Byref value as integer) as integer
rem Get the next instruction or data from PCs...
dim result as integer

CODE(location+1)=value

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
	dim result as integer
	dim power as integer
	power=2^indice
	
	result=bittest(value,power)
	
return result
End Function



Function GetLabel(Byref label as string) as integer	
rem Return the array position of a Label in a table  - or add it to the table and return that array position.
rem Also adds the memlocation referring to the label to the 
dim response as integer
dim count as integer : rem general counter.
dim mylabel as string	: rem Label we are looking for.


	if label="" then goto Gotlabel	: rem Get rid of any null requests.

	label=CASEFIX(label)

	mylabel=left(label+"                                ",32)

	for count=1 to numlabels

		if mylabel = left(labletable(count),32) then response=count : goto Gotlabel

	next count
	
		numlabels=numlabels+1
		labletable(count)=mylabel+"----"			: rem reserve space for the setting of the label later. 
		response=count

Gotlabel:
	return response
End Function

Function SetLabel(Byref label as string, Byref memloc as integer) as integer	
rem Set the location in space where the label was found. Used for initial label - eg, LABEL: as opposed to a label in the middle of an operation. 
dim lnum as integer : rem general counter.
dim testlabel as integer : rem use as a test to determine if the label was used before... Still can be reset, but not an ideal way to do this. 

	
	if label="" then goto Gotlabel	: rem Get rid of any null requests.

	Label=CASEFIX(label) 	: rem All one case. 
	
	testlabel=numlabels

	lnum=getlabel(label)
	
	if debug =1 then print "Testlabel:";Testlabel;" lnum:";lnum
	
	if pass = 1 and lnum <= testlabel and mid(Labletable(lnum),33,4) <> "----" then Adderror ("Warning - Label used or assigned twice... Very likely to cause program failure. previously "+mid(Labletable(lnum),33,4)) : rem we didn't add a new entry... Rare... 

	
	if debug =1 then print "Label Currently:";Labletable(lnum)
	
	if mid(Labletable(lnum),33,4) <> "----" and mid(Labletable(lnum),33,4) <> hex(memloc,4) then
		rem We got a major error here. The label can be changed to itself, but should not change to another value. Reassigned? Repeated? EQU? IF?
		Adderror ("Warning - Label vaue changed during assembly on pass:"+hex(pass,1)+"Previous value:"+mid(Labletable(lnum),33,4)) : rem we didn't add a new entry... Rare... 
		endif
	
	labletable(lnum)=insert(labletable(lnum),hex(memloc,4),33)
	
	rem labletable(lnum)=left (labletable(lnum),32)+ hex(memloc,4) + right (labletable(lnum),len(labletable(lnum))-32)

Gotlabel:
	return lnum
End Function


Function AddLabel(Byref label as string, Byref memloc as integer) as integer	
rem Set the code location that references the label and add the code address to the table. Used for later labels, eg, LD A,LABEL or LD A,(LABEL) where it is a value. 
dim lnum as integer : rem general counter.

	Label=CASEFIX(label) 	: rem All one case. 

	if label="" then goto Gotlabel	: rem Get rid of any null requests. 
	lnum=getlabel(label)
	
	if pass=2 then labletable(lnum)=labletable(lnum) + chr(42+relative)+hex(memloc,4)


Gotlabel:
	return lnum
End Function

Function TestLabels() as integer
dim count as integer


for count=1 to numlabels

if count <> 1 and mid(Labletable(count),33,4) = "----" then 
	Linenumber=CodeLine(val("&h"+mid(Labletable(count),38,4))+1)	: REM +1 BECAUSE codeline is 1to65536 not 0 to 65535
	Adderror ("Post-Assemble Pass - Warning - Label never assigned:"+Labletable(count)) 
	rem Oops, label never got assigned a value... That's bad. 
	endif
	
next count

Return 0
End Function







Function ShowRam() as integer
dim count as integer

CODETEXT=""
for count=showstart+1 to showend+1
CODETEXT=CODETEXT+chr(CODE(count))
next count


PRINTHEX (CODETEXT,(showend-showstart)+1)


return 0
End Function

Function SaveRam() as integer
dim count as integer

CODETEXT=""
for count=savestart+1 to saveend+1
CODETEXT=CODETEXT+chr((CODE(count) mod 256))
next count


rem SaveHEX (CODETEXT,(saveend-savestart)+1)
SaveBinary (CODETEXT, (Saveend-savestart)+1)


return 0
End Function

Function Checkcount() as integer
rem Make sure we're not at End Of Source File.
dim response as integer
response=0

if SourceCount >= len(Source) then 
	errorcode=1
	response=1
	adderror("End Of File")
	endif



return response
end Function


function GotoEOL() as string
rem Move SourceCount to the start of the next line. 

rem Locate EOL.
EOLtop:
if SourceCount>=len(Source) then goto EOLERROR
if mid(source,sourcecount,1)=chr(&h0A) then goto EOLNEXT
if mid(source,sourcecount,1)=chr(&h0D) then goto EOLNEXT
print mid (source,sourcecount,1);"[";
sourcecount=sourcecount+1
goto EOLTOP

EOLNEXT: 		rem move to first non-EOL character ( next line ). 
if mid(source,sourcecount,1) <> chr(&h0A) and  mid(source,sourcecount,1) <> chr(&h0D) then
	sourcecount=sourcecount+1
	goto EOLNEXT
	endif


EOLERROR:

return ""
end function

function Getoneword () as string
dim position as integer : rem where are we in the string.
dim result as string

position=0
result=""

whitespace1:
position=position+1
if mid(originalline,position,1) <=" " then goto whitespace1

rem We now have a real character.
while mid(originalline,position,1) > " " 
	result=result+mid(originalline,position,1)
	position=position+1
	wend

	originalline=right(originalline,len(originalline)-position)
	rem Shorten original line, minus the first word. 

return result
end function

function GetOperand (Byref Oneline as string) as string
rem Returns the next operand from the source.
dim op as string
dim count as integer : rem Count down the max characters.
dim maxlen as integer : rem We need a maximum length for convenience. Let's make it 32 characters per operand or label.
dim pass as integer : rem how many passes do we need to make on this line?
dim t1 as integer	: rem Toggle for single quotes.
dim t2 as integer   : rem Toggle for double quotes. 

rem checkcount()

t1=0	: rem Note that we are removing whitespace, so keep what is in the quotes.
t2=0	: rem As per T1, but we look for double quotes. 
count=1
target=""
op=""
pass=1

rem Remove whitespace and any control codes before first text.. 
GetOp1:
if mid(oneline,count,1)<=" " and count <= len(Oneline) then
	count=count+1
	Goto GetOp1
	endif

target=""
op=""

maxlen=32	: rem Set maximum number of characters before we decide we have an error. Labels will not be longer. 

rem Initial scan of Opcode or Label. 
while maxlen > 0 and count <= len(oneline)
		if mid(oneline,count,1)<=" " then goto gettarget			: rem Whitespace or control codes are all EOL
		if mid(oneline,count,1)=";" goto getoperandend				: rem Comment mode is EOL. 
		if mid(oneline,count,1)=":" then goto GetOperandLabelfound
	op=op+mid(Oneline,count,1)

	count=count+1
	wend
	
	
	if count>len(oneline) then  goto getoperandend : rem If we're at the end of the line, either there's no CRLF or it was blank. 
	
	rem If we get here, the label is more than 32 characters. 
	adderror ("Warning: Label or Operator is longer than 32 characters. ")
	
	rem goto GetOperandEnd	: rem we can exit here, or get any target data. Might as well get target data. 
	
gettarget:	
rem Is there anything else on the line? Shove it into "Target" (Global Variable). 	Remove whitespace.

target=""
t1=0
t2=0

while count <= len(oneline)
	if mid(oneline,count,1)="'" and t2=0 then t1=(t1+1) mod 2 		: rem Check to disable other character checking within the quote. 
	if mid(oneline,count,1)=chr(34) and t1=0 then t2=(t2+1) mod 2 	: rem Check to disable other character checking within the quote. 
	
	if t1=0 and t2=0 then											: rem Ignore formatting conventions while in literal quotation mode. 
		if mid(oneline,count,1)=";" goto getoperandend	: rem Comment mode is EOL.
		endif
		
		if mid(Oneline,count,1) >" " then target=target+mid(Oneline,count,1)
		if mid(Oneline,count,1) =" " and (t1 or t2) then target=target+mid(Oneline,count,1)	: rem Add spaces when in literal mode. 

		
	count=count+1
	wend
	
	If t1=1 then adderror ("Missing closing quotation.(single) when scanning for operand target")
	If t2=1 then adderror ("Missing closing quotation.(single) when scanning for operand target")
	
	goto GetOperandEnd

GetOperandLabelfound:
	label=op
	if debug = 1 then Print "Label:";label;"  ";
	setlabel(label,PC)			: rem set the current label with the PC.
	count=count+1				: rem Get past the ":" character
	absolutelabel=1				: rem Note we already got a label here... Need to be careful about EQUs if we have a subsequent EQU. 
	goto Getop1	: rem Restart and continue to look for operand. 
	
rem GetOperandLiteral:

getoperandend:
	Oneline=right(oneline,Len(oneline)-count) : rem Let's remove what we already have... We don't need it now. 

	op=left(op+".....",5)

return op
end function

Function Eval(Byval location as string) as integer
dim result as integer
dim convert as string
dim count as integer
dim labeldetect as integer	: rem Did we detect a possible label? 
dim labelvalue as integer : rem what was the value of a label? ( If we can decode it ).  

if debug>1 then print "Eval called with [";location;"]"

	if len(location)=0 then adderror ("Warning, Nothing to evaluate. Null evaluation requested")

	if left(location,1)="%" then goto EvalBinary
	if left(location,1)="$" then goto EvalHex
	if left(location,1)="'" then goto EvalByte		 : rem Byte is a single binary number with possible "OR" or "AND" or "XOR" or "NOT"
	if left(location,1)=chr(34) then goto Evalstring : rem String is a series of binary numbers as ASCII

	rem Test for labels here. 000: can still be a label, but can't be jumped to.  Need to remove and make all labels have a non-numeric. 00A: would be OK. 
	for count=1 to len(location)		: rem It's not Binary or Hex ( or maybe Octal... What is the code for Octal)
		if mid(location,count,1) > "9" or mid(location,count,1)<"0" then goto EvalLabel : rem Detected what is most likely a Label. Cannot be Numeric
	next count

	result=val(location)

	goto Evalend

EvalBinary:

	convert="&b0"+right(location,len(location)-1)
	result=val(convert)

Goto EvalEnd



EvalHex:	

	convert="&h0"+right(location,len(location)-1)
	result=val(convert)
	
	rem Special case follows.
	if len(location)=1 then 
		result=LinePC	: rem We evaluate PC to be the same 
		if debug>0 then print "$ alone evaluated to PC."
		rem I think this is stupid... If I want PC, I can set a label, eg, PC: LD	A,PC - This gets around compiler issues. 
		rem I might need to be certain around which PC I mean since the instruction can have several bytes. 
		rem PC = Where we start writing the numbers... LinePC includes any original opcode PC location. 
		rem This is a reasonable workaround. 
		endif

Goto EvalEnd

EvalByte:
	result=asc(mid(location,2,1))	
	if mid(location,3,1) <> "'" then adderror ("Byte Allocation Error, missing closing single-quote")
	
Goto EvalEnd

EvalString:
	rem Only two bytes of a string recognized here. 16 bit value as string. Might only be one character though ( which would result in an 8 bit response for 16 bits, but is valid )
	result=asc(mid(location,2,1))	: rem Low Byte
	if mid(location,3,1) <> chr(34) then result=result+(256*(asc(mid(location,3,1))))  : rem High Byte. If it's a double quote, then don't do the high byte. 
	if right(location,1) <> chr(34) then adderror ("Double-Byte Allocation Error, missing closing double-quote or single character error")

Goto EvalEnd

EvalLabel:
	if debug = 1 then print "Referenced Label:";location
	Labelvalue=addlabel (location,PC)	: rem Add vector for label to label list.
	result=val("&h"+mid(Labletable(Labelvalue),33,4))
	if debug > 0 then print "Stored Label:";Labelvalue;" is:";mid(Labletable(Labelvalue),33,4)
	
	
EvalEnd:
if debug =1 then Print "Evaluate function returned:";hex(result,4)
return result
End function




Function SuperEval(Byref Nominator as String) as integer
rem OK, This is an anomoly that is kind of recursive, seeing if I can add maths to any assignment, so it starts as 16 bit version of LitByte and adds extra function, and returns the result.
rem Need to work out how to combine all three functions at some point, since they are basically the same. Not sure how many labels I can nest, or if that will affect things.
rem Also need a way to mark some calculations as NOT requiring to store their location...
rem Might need to make this a 2 pass assembler. 
dim segment as string
dim scan as integer	: rem where are we in the string?
dim Loperator as string : rem I just need a way to complete logical operations on subsequent passes. Might as well use a string for clarity
dim Toperator as string : rem THIS operator - Record in last until we get the next byte.
dim result as integer	: rem In the end, we convert all back to an integer. 
dim cont as integer : rem Flag for whether we continue. 
dim t1 as integer : rem Toggle for Single quotes. 
dim t2 as integer : rem Toggle for Double quotes. 

rem We may need to scan the byte for logical operators. 
rem Operators are * or + or ! or - or @(xor) or something else. Need to learn what to put in here.
rem Initially, at least do + and * and !... Maybe. 
rem Can be a list separated by commas also. eg, 'A', "'B'" and can mix formats, eg "'A', $01"
rem examples-
rem DB 01,$5A,%01010101,'G','A'+%1000000,3*1 : rem last example is 3 and 1. 
rem 

Loperator=""
cont=1
scan=0
result=0
segment=""
t1=0
t2=0

Litdecode:
while scan < len (nominator) and cont			
	rem - Make < len because we update scan location next. ( rather than <= )
	scan=scan+1
	
	if mid(nominator,scan,1)="'" and t2=0 then t1=(t1+1) mod 2 		: rem Check to disable other character checking within the quote. 
	if mid(nominator,scan,1)=chr(34) and t1=0 then t2=(t2+1) mod 2 	: rem Check to disable other character checking within the quote. 
	
	if t1=1 and t2=1 then adderror ("FAILURE - Quote confusion. Single and Double quotes both active- Code error in assembler. ")
	
	if t1=0 and t2=0 then
		rem T1 or T2 being 1 means blocking other character checks because it's a literal character. 
		rem if mid(nominator,scan,1)="," then Toperator="next" : cont=0 	: rem we remove muliple results from the stream. Just one. 
		if mid(nominator,scan,1)="*" then Toperator="mul" : cont=0
		if mid(nominator,scan,1)="@" then Toperator="and" : cont=0
		if mid(nominator,scan,1)="+" then Toperator="add" : cont=0
		if mid(nominator,scan,1)="-" then Toperator="sub" : cont=0
		if mid(nominator,scan,1)="#" then Toperator="or" : cont=0
		if mid(nominator,scan,1)="!" then Toperator="not" : cont=0
		if mid(nominator,scan,1)="&" then Toperator="xor" : cont=0
		if mid(nominator,scan,1)="<" then Toperator="rleft" : cont=0
		if mid(nominator,scan,1)=">" then Toperator="rright" : cont=0
		if mid(nominator,scan,1)="^" then segment="$"+hex(PC,4) : rem Current program counter. 
		endif
	if cont then segment=segment+mid(nominator,scan,1)
	wend


If Toperator <> "" then cont=1 : rem Mark to continue again. 

If Loperator="" and Toperator="next" then 
	Loperator="next"	: rem Finish this byte now. 
	result =eval(segment) : rem Establish the result.
	segment="" : 			rem Clear the segment
	cont=1		:			rem Highlight we want to continue.
	endif


		
Select Case Loperator	
rem Toperator is current operator - Just find a way to continue. 	
	Case "and"
		result=result AND eval(segment)
		segment=""
		Loperator=""
			
	Case "or"
		result=result OR eval(segment)
		segment=""	
		Loperator=""
		
	Case "not" : rem difficult to implement with logic. Test and do later. 
		result= NOT result
		Loperator=""
		
	Case "xor"
		result= result XOR eval(segment)
		segment=""
		Loperator=""
		
	Case "rleft"
		result = result SHL 1
		Loperator=""
		
	Case "rright"
		result = result SHR 1
		Loperator=""
		
	Case "mul"
		result=result * eval (segment)
		segment=""
		Loperator=""
	
	Case "add"
		result=result + eval (segment)
		segment=""
		Loperator=""
	
	Case "sub"
		result=result - eval (segment)
		segment=""
		Loperator=""
		
	Case "next"
		rem We don't really use this case here, but leave it in case I come up with a stupid idea later. 
rem		Writecode(lower(result))
rem		Writecode(higher(result)) : rem Write both bytes. 
		Toperator=""	: rem Make sure Loperator is cleared in case we get a single byte EOL. 
		result=0 
		Loperator=""
		
	Case Else
		rem No arithmetic operators at this stage. 
		result=eval(segment)			: rem Set the result. 
End Select


If Toperator="next" or scan>=len(nominator)then
rem		Writecode(Lower(result))
rem		Writecode(Higher(result))	: rem Write both bytes. 
		Toperator=""	: rem Make sure Loperator is cleared in case we get a single byte EOL. 
		segment=""
		endif

		if scan >= len(nominator) then 
			cont=0						: rem Indicate we are done. 
			endif

	Loperator=Toperator : rem remember the last operator for when we get another term. 
	
	Toperator=""
	segment=""
	
if cont then goto Litdecode

return result	: rem Unlike the others, we actively want to return a result. Note, some collisions with labels will occur. 
End Function






Function GetLeft(Byref txt as string) as string
dim result as string

result=left(txt,instr(txt,",")-1)

return result
End Function

Function GetRight(Byref txt as string) as string
dim result as string

result=right(txt,len(txt)-instr(txt,","))

return result
End Function

Function Reg8(Byref txt as string) as integer
rem Convert single register to register number. Or -1 if not convertable.
dim result as integer

Select Case txt
		Case "B"
			result=0
		Case "C"
			result=1
		Case "D"
			result=2
		Case "E"
			result=3
		Case "H"
			result=4
		Case "L"
			result=5
		Case "(HL)"
			result=6
		Case "A"
			result=7
	Case Else
		result=-1
End Select


return result
End function


Function Reg16(Byref txt as string) as integer
rem Convert single register to register number. Or -1 if not convertable.
dim result as integer

Select Case txt
		Case "BC"
			result=0
		Case "DE"
			result=1
		Case "HL"
			result=2
		Case "SP"
			result=3
	Case Else
		result=-1
End Select


return result
End function


Function Reg16AF(Byref txt as string) as integer
rem Convert single register to register number. Or -1 if not convertable.
dim result as integer

Select Case txt
		Case "BC"
			result=0
		Case "DE"
			result=1
		Case "HL"
			result=2
		Case "AF"
			result=3
	Case Else
		result=-1
End Select


return result
End function



Function WriteCode(Byref opcode as integer) as integer
rem
if firstcode>PC then firstcode=PC 
if lastcode<PC then lastcode=PC
Code(PC+1)=(opcode mod 256)
CodeLine(PC+1)=Linenumber
if debug>0 then
	Print "Writing memory location:";hex(PC,4);" with ";hex(Opcode,3)
	endif
PC=PC+1
if PC>65535 then
	adderror ("Caution - Program Counter Overflow. PC=0.")
	PC=0
	Endif
if opcode>255 then adderror ("Warning - Value exceeds 8 bits in byte ( Likely wrote 16 bit number to 8 bit location.) - Writing low order byte only.")
return 0
End Function


Function GetDisplacement(Byref txt as string) as string
rem Get displacement from IX and IY commands. Might be a number or a label.
dim result as string
	if right(TXT,1) <> ")" then adderror("Warning - Right Parenthesis Missing from IX/IY function. Got '"+result+"' -Wrong displacement specified?")
	result = right(txt,(len(TXT)-3))
	result = left(result,len(result)-1)
	
	if mid(txt,4,1)<>"+" and mid(txt,4,1)<>"-" then adderror ("Warning - Sign for displacement not correct. Should be + or - ")

	result="0"+result : rem To make the sign work... eg, +100 becomes 0+100. -100 becomes 0-100
	
return result
End Function

Function GetLocation(Byref txt as string) as string
rem Get Remove brackets from Location code - eg (100) becomes 100.
dim result as string

result=txt

	if left(result,1) <> "(" then adderror("Warning - Left Parenthesis Expected but not found, or other character present.")
	if right(result,1) <> ")" then adderror("Warning - Right Parenthesis Expected but not found, or other character present.")

While left(result,1)="(" 
	result=right(result,len(result)-1)
	wend
	
While right(result,1)=")" 
	result=left(result,len(result)-1)
	wend

	rem Strip out parenthesis and other functions.
return result
End Function

Function LD(Byref Nominator as string) as integer	
rem Complete LOAD functions.
dim NLEFT as string : rem left side of Nominator. ( all are left/right. )
dim NRIGHT as string : rem right side of Nominator. ( all are left/right. )
dim encode as integer	: rem Where we build the opcode. 
dim displacement as integer : rem for IX and IY commands. 

NLEFT=GetLeft(Nominator) : rem Get left of comma.
NRIGHT=GetRight(Nominator) : rem Get right of comma. 

if debug=2 then print "LD command found. "
if debug = 2 then Print "Left:";NLEFT;":";NRIGHT;":RIGHT"


rem LD r,r
if Reg8(NLEFT) >= 0 and Reg8(NRIGHT) >=0 then
	encode=&h40+(Reg8(NLEFT)*8)+Reg8(NRIGHT)
	WriteCode(encode)
	goto LDDONE
	endif

rem LD (BC),A
if NLEFT="(BC)" and NRIGHT="A" then
		encode=&h02
		WriteCode(encode)
		goto LDDONE
		endif
	
rem LD (DE),A	
If NLEFT="(DE)"  and NRIGHT="A" then
		encode=&h12
		WriteCode(encode)
		goto LDDONE
		endif
		
rem LD A,(BC)
if NLEFT="A" and NRIGHT="(BC)" then
		encode=&h0A
		WriteCode(encode)
		goto LDDONE
		endif
		
rem LD A,(DE)
If NLEFT="A" and NRIGHT="(DE)" then
		encode=&h1A
		WriteCode(encode)
		goto LDDONE
		endif

rem LD (IX+disp),r		
IF LEFT(NLEFT,3)="(IX" and Reg8(NRIGHT) >=0 then	
		displacement=supereval(GetDisplacement(NLEFT))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")
		Writecode(&hDD)
		Encode=&h70+(Reg8(NRight))
		WriteCode(encode)
		Writecode (displacement)
		goto LDDONE
		endif

rem LD (IY+disp),r
IF LEFT(NLEFT,3)="(IY" and Reg8(NRIGHT) >=0 then
		displacement=supereval(GetDisplacement(NLEFT))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")
		Writecode(&hFD)
		Encode=&h70+(Reg8(NRight))
		WriteCode(encode)
		Writecode (displacement)
		goto LDDONE
		endif

rem LD r,(IX+disp)
IF Reg8(NLEFT) >=0 and LEFT(NRIGHT,3)="(IX" then
		displacement=supereval(GetDisplacement(NRIGHT))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")
		
		Writecode(&hDD)
		Encode=&h46+(Reg8(NLeft)*8)
		WriteCode(encode)
		Writecode (displacement)
		goto LDDONE
		endif

rem LD r,(IY+disp)
IF Reg8(NLEFT) >=0 and LEFT(NRIGHT,3)="(IY" then
		displacement=supereval(GetDisplacement(NRIGHT))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")
		
		Writecode(&hFD)
		Encode=&h46+(Reg8(NLeft)*8)
		WriteCode(encode)
		Writecode (displacement)
		goto LDDONE
		endif

rem LD A,I
If Nominator="A,I" then
		Writecode(&hED)
		Writecode(&h57)
		goto LDDONE
		endif

rem LD A,R
If Nominator="A,R" then
		Writecode(&hED)
		Writecode(&h5F)
		goto LDDONE
		endif

rem LD I,A
If Nominator="I,A" then
		Writecode(&hED)
		Writecode(&h47)
		goto LDDONE
		endif

rem LD R,A
If Nominator="R,A" then
		Writecode(&hED)
		Writecode(&h4F)
		goto LDDONE
		endif


rem LD r,N
If Reg8(NLEFT) >=0 and Left(NRIGHT,1) <> "(" then		
		Rem - We've already checked for known responses in load, so it must be N. (immediate data ).
		encode=&h06+(Reg8(NLEFT)*8)
		Writecode(encode)
		Writecode(SuperEval(NRIGHT))		: rem Changed to Supereval. 
		goto LDDONE
		endif

rem LD (IX+disp),N
IF LEFT(NLEFT,3)="(IX" then
		displacement=supereval(GetDisplacement(NLEFT))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")
		
		Writecode(&hDD)
		WriteCode(&h36)
		Writecode (displacement)
		Writecode(SuperEval(NRIGHT))
		goto LDDONE
		endif
		
		
rem LD (IY+disp),N
IF LEFT(NLEFT,3)="(IY" then
		displacement=supereval(GetDisplacement(NLEFT))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")
		
		Writecode(&hFD)
		WriteCode(&h36)
		Writecode (displacement)
		Writecode(SuperEval(NRIGHT))
		goto LDDONE
		endif
		
rem LD A,(NN)
If left(Nominator,3)="A,(" then
		Writecode(&h3A)
		encode=SUPEREVAL(Getlocation(NRIGHT))
		if encode>65535 then adderror ("Warning: Evaluated expression outside of 16 bit vector.")
		Writecode(Lower(encode))
		Writecode(Higher(encode))
		goto LDDONE
		endif
		
rem LD (NN),A
If right(Nominator,3)="),A" then
		Writecode(&h32)
		encode=SuperEVAL(Getlocation(NLEFT))
		if encode>65535 then adderror ("Warning: Evaluated expression outside of 16 bit vector.")
		Writecode(Lower(encode))
		Writecode(Higher(encode))
		goto LDDONE
		endif

rem 16 bit load group here. 	





rem LD SP, HL 
if NLEFT="SP" and NRIGHT="HL" then
		Writecode (&hF9)
		goto LDDONE
		endif 

rem LD SP, IX 
if NLEFT="SP" and NRIGHT="IX" then
		Writecode (&hDD)
		Writecode (&hF9)
		goto LDDONE
		endif
 
rem LD SP, IY 
if NLEFT="SP" and NRIGHT="IY" then
		Writecode (&hFD)
		Writecode (&hF9)
		goto LDDONE
		endif



		

rem LD dd, nn 
If reg16(NLEFT)>=0 and left(NRIGHT,1) <>"(" then 
		encode=&h01+(reg16(NLEFT)*16)
		Writecode(encode)


rem 		encode=Eval(NRIGHT)
rem Test Supereval.
		encode=SuperEval(NRIGHT)		: rem See if we can make it super. 


		Writecode(Lower(encode))
		Writecode(Higher(encode))
		goto LDDONE
		endif
		
rem LD IX, nn 
If NLEFT="IX" and left(NRIGHT,1) <>"(" then 
		Writecode(&hDD)
		Writecode(&h21)
		encode=SuperEval(NRIGHT)
		Writecode(Lower(encode))
		Writecode(Higher(encode))
		goto LDDONE
		endif




rem LD IY, nn 
If NLEFT="IY" and left(NRIGHT,1) <>"(" then 
		Writecode(&hFD)
		Writecode(&h21)
		encode=SuperEval(NRIGHT)
		Writecode(Lower(encode))
		Writecode(Higher(encode))
		goto LDDONE
		endif





rem LD HL, (nn)
If left(Nominator,4)="HL,(" then
		Writecode(&h2A)
		encode=SuperEVAL(Getlocation(NRIGHT))
		if encode>65535 then adderror ("Warning: Evaluated expression outside of 16 bit vector.")
		Writecode(Lower(encode))
		Writecode(Higher(encode))
		goto LDDONE
		endif

rem LD (nn), HL
		If right(Nominator,4)="),HL" then
		Writecode(&h22)
		encode=SuperEVAL(Getlocation(NLEFT))
		if encode>65535 then adderror ("Warning: Evaluated expression outside of 16 bit vector.")
		Writecode(Lower(encode))
		Writecode(Higher(encode))
		goto LDDONE
		endif
		
		
		

rem LD dd, (nn) 
If reg16(NLEFT)>=0 and left(NRIGHT,1) ="(" then 
		Writecode(&hED)
		encode=&h4B+(reg16(NLEFT)*16)
		Writecode(encode)
		encode=SuperEVAL(Getlocation(NRIGHT))
		if encode>65535 then adderror ("Warning: Evaluated expression outside of 16 bit vector.")
		Writecode(Lower(encode))
		Writecode(Higher(encode))
		goto LDDONE
		endif

rem LD (nn), dd 
If reg16(NRIGHT)>=0 and left(NLEFT,1) ="(" then 
		Writecode(&hED)
		encode=&h43+(reg16(NRIGHT)*16)
		Writecode(encode)
		encode=SuperEVAL(Getlocation(NLEFT))
		if encode>65535 then adderror ("Warning: Evaluated expression outside of 16 bit vector.")
		Writecode(Lower(encode))
		Writecode(Higher(encode))
		goto LDDONE
		endif

rem LD IX, (nn)
If NLEFT="IX" and left(NRIGHT,1) ="(" then 
		Writecode(&hDD)
		Writecode(&h2A)
		encode=SuperEVAL(Getlocation(NRIGHT))
		if encode>65535 then adderror ("Warning: Evaluated expression outside of 16 bit vector.")
		Writecode(Lower(encode))
		Writecode(Higher(encode))
		goto LDDONE
		endif

rem LD (nn), IX 
If NRIGHT="IX" and left(NLEFT,1) ="(" then 
		Writecode(&hDD)
		Writecode(&h22)
		encode=supereval(Getlocation(NLEFT))
		if encode>65535 then adderror ("Warning: Evaluated expression outside of 16 bit vector.")
		Writecode(Lower(encode))
		Writecode(Higher(encode))
		goto LDDONE
		endif 


rem LD IY, (nn) 
If NLEFT="IY" and left(NRIGHT,1) ="(" then 
		Writecode(&hFD)
		Writecode(&h2A)
		encode=supereval(Getlocation(NRIGHT))
		if encode>65535 then adderror ("Warning: Evaluated expression outside of 16 bit vector.")
		Writecode(Lower(encode))
		Writecode(Higher(encode))
		goto LDDONE
		endif

rem LD (nn), IY  
If NRIGHT="IY" and left(NLEFT,1) ="(" then 
		Writecode(&hFD)
		Writecode(&h22)
		encode=supereval(Getlocation(NLEFT))
		if encode>65535 then adderror ("Warning: Evaluated expression outside of 16 bit vector.")
		Writecode(Lower(encode))
		Writecode(Higher(encode))
		goto LDDONE
		endif 
		
		
rem UNDOCUMENTED CODES HERE.
rem LD IXh, IXl, IYh, IYl,n.   

Select Case NLEFT

		Case "IXH"
			Writecode(&hDD)
			Writecode(&h26)
			Writecode(SuperEval(NRIGHT))		: rem Changed to Supereval. 	
			goto LDDONE			
		Case "IXL"
			Writecode(&hDD)
			Writecode(&h2E)		
			Writecode(SuperEval(NRIGHT))		: rem Changed to Supereval. 	
			goto LDDONE			
		Case "IYH"
			Writecode(&hFD)
			Writecode(&h26)	
			Writecode(SuperEval(NRIGHT))		: rem Changed to Supereval. 
			goto LDDONE			
		Case "IYL"
			Writecode(&hFD)
			Writecode(&h26)	
			Writecode(SuperEval(NRIGHT))		: rem Changed to Supereval. 
			goto LDDONE

End select

adderror ("Warning - LD Load Function not recognized. ")

LDDONE:
return 0	: rem Make this some kind of error handler later?
End Function


Function Push(Byref Nominator as string) as integer
dim encode as integer

rem PUSH qq 
If Reg16AF(Nominator)>=0 then
		encode=&hC5+(Reg16AF(Nominator)*16)			: rem Reg16AF because AF and SP are different by instruction. 
		Writecode(encode)
		goto Pushdone
		endif

rem PUSH IX 
If Nominator="IX" then
		Writecode(&hDD) 
		Writecode(&hE5)
		goto Pushdone
		endif

rem PUSH IY 
if	Nominator="IY" then
		Writecode(&hFD) 
		Writecode(&hE5)
		goto Pushdone
		endif


adderror ("Warning - PUSH Function not recognized. "+originalline)
Pushdone:
return 0
End Function

Function Pop(Byref Nominator as string) as integer
dim encode as integer

rem POP qq 
if Reg16AF(Nominator)>=0 then
		encode=&hC1+(Reg16AF(Nominator)*16)			: rem Reg16AF because AF and SP are different by instruction. 
		Writecode(encode)
		goto Popdone
		endif

rem POP IX 
if	Nominator="IX" then
		Writecode(&hDD) 
		Writecode(&hE1)
		goto Popdone
		endif

rem POP IY 
if	Nominator="IY" then
		Writecode(&hFD) 
		Writecode(&hE1)
		goto Popdone
		endif

adderror ("Warning - POP Function not recognized. "+Originalline)

Popdone:
return 0
End Function

Function Ex (Byref Nominator as string) as integer	
rem Complete Exchange and Block functions.
dim NLEFT as string : rem left side of Nominator. ( all are left/right. )
dim NRIGHT as string : rem right side of Nominator. ( all are left/right. )
dim encode as integer	: rem Where we build the opcode. 
dim displacement as integer : rem for IX and IY commands. 

NLEFT=GetLeft(Nominator) : rem Get left of comma.
NRIGHT=GetRight(Nominator) : rem Get right of comma. 

if debug=2 then print "LD command found. "
if debug = 2 then Print "Left:";NLEFT;":";NRIGHT;":RIGHT"

rem EXX
If Nominator="X" then 
		Writecode(&hD9)	: rem EXX.... 
		goto	ExDone
		endif

rem EX DE,HL
If NLEFT="DE" and NRIGHT="HL" then 
		Writecode(&hEB)	 
		goto	ExDone
		endif

rem EX AF,AF'
If NLEFT="AF" and left(NRIGHT,2)="AF" then 
		Writecode(&h08)	
		goto	ExDone
		endif	

rem EX (SP),HL
If NLEFT="(SP)" and NRIGHT="HL" then 
		Writecode(&hE3)	
		goto	ExDone
		endif		

rem EX (SP),IX
If NLEFT="(SP)" and NRIGHT="IX" then 
		Writecode(&hDD)	
		Writecode(&hE3)
		goto	ExDone
		endif

rem EX (SP),IY	
If NLEFT="(SP)" and NRIGHT="IY" then 
		Writecode(&hFD)	
		Writecode(&hE3)
		goto	ExDone
		endif
 
adderror ("Warning - Exchange pair invalid")
	
ExDone:

return 0
End Function


Function LDIR (Byref Nominator as string, Byref extra as string) as integer	
rem Complete Exchange and Block functions.
dim NLEFT as string : rem left side of Nominator. ( all are left/right. )
dim NRIGHT as string : rem right side of Nominator. ( all are left/right. )
dim encode as integer	: rem Where we build the opcode. 
dim displacement as integer : rem for IX and IY commands. 

NLEFT=GetLeft(Nominator) : rem Get left of comma.
NRIGHT=GetRight(Nominator) : rem Get right of comma. 

if debug=2 then print "LD command found. "
if debug = 2 then Print "Left:";NLEFT;":";NRIGHT;":RIGHT"

if extra<>"" then adderror ("Warning - Additional data after block command.")

rem LDI
If Nominator="LDI.." then 
		Writecode(&hED)	
		Writecode(&hA0)
		goto LDIDONE
		endif

rem LDIR
If Nominator="LDIR." then 
		Writecode(&hED)	
		Writecode(&hB0)
		goto LDIDONE
		endif
		
rem LDD
If Nominator="LDD.." then 
		Writecode(&hED)	
		Writecode(&hA8)
		goto LDIDONE
		endif
		
rem LDDR
If Nominator="LDDR." then 
		Writecode(&hED)	
		Writecode(&hB8)
		goto LDIDONE
		endif

rem CPI
If Nominator="CPI.." then 
		Writecode(&hED)	
		Writecode(&hA1)
		goto LDIDONE
		endif
		
rem CPIR
If Nominator="CPIR." then 
		Writecode(&hED)	
		Writecode(&hB1)
		goto LDIDONE
		endif

rem CPD
If Nominator="CPD.." then 
		Writecode(&hED)	
		Writecode(&hA9)
		goto LDIDONE
		endif
		
rem CPDR
If Nominator="CPDR." then 
		Writecode(&hED)	
		Writecode(&hB9)
		goto LDIDONE
		endif

adderror ("ASSEMBLER FAILURE - Unknown Block Move:"+Nominator)

LDIDONE:
return 0
End Function


Function ADD(Byref Nominator as string) as integer
dim encode as integer
dim NLEFT as string
dim NRIGHT as string
dim displacement as integer

NLEFT=GetLeft(Nominator) : rem Get left of comma.
NRIGHT=GetRight(Nominator) : rem Get right of comma. 

rem Add	A,r
if 	NLEFT="A" and Reg8(NRIGHT) >=0 then
		Encode=&h80+Reg8(NRIGHT)
		Writecode(Encode)
		Goto AddDone
		Endif

rem Add A,(HL)
if 	NLEFT="A" and NRIGHT="(HL)" then
		Writecode(&h86)
		Goto AddDone
		Endif
		
rem Add A,(IX+d)
If Left(Nominator,5)="A,(IX" then
		displacement=supereval(GetDisplacement(NRIGHT))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")	
		Writecode(&hDD)
		WriteCode(&h86)
		Writecode (displacement)
		goto AddDone
		endif

rem Add A,(IY+d)
If Left(Nominator,5)="A,(IY" then
		displacement=supereval(GetDisplacement(NRIGHT))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")	
		Writecode(&hFD)
		WriteCode(&h86)
		Writecode (displacement)
		goto AddDone
		endif

rem Add A,s
if NLEFT="A" then
		Writecode(&hC6)
		Writecode(supereval(NRIGHT))
		Goto AddDone
		Endif
		
rem Add HL,ss
if NLEFT="HL" and Reg16(NRIGHT) >=0 then
		encode=&h09+(Reg16(NRIGHT)*16)
		Writecode(encode)
		Goto AddDone
		Endif

rem Add IX,PP
if NLEFT="IX" then
		if NRIGHT="IX" then NRIGHT="HL" 	: rem substitution.
		if Reg16(NRIGHT)<0 then AddError("Warning - Index ADD target incorrect.")
		encode=&h09+(Reg16(NRIGHT)*16)
		Writecode(&hDD)
		Writecode(encode)
		Goto AddDone
		Endif
		

rem Add IY,PP
if NLEFT="IY" then
		if NRIGHT="IY" then NRIGHT="HL" 	: rem substitution.
		if Reg16(NRIGHT)<0 then AddError("Warning - Index ADD target incorrect.")
		encode=&h09+(Reg16(NRIGHT)*16)
		Writecode(&hFD)
		Writecode(encode)
		Goto AddDone
		Endif
		
		
Adderror ("Warning: ADD function failed. Missing suitable target.")

AddDone:
return 0
End Function

Function ADC(Byref Nominator as string) as integer
dim encode as integer
dim NLEFT as string
dim NRIGHT as string
dim displacement as integer

NLEFT=GetLeft(Nominator) : rem Get left of comma.
NRIGHT=GetRight(Nominator) : rem Get right of comma. 

rem Adc	A,r
if 	NLEFT="A" and Reg8(NRIGHT) >=0 then
		Encode=&h88+Reg8(NRIGHT)
		Writecode(Encode)
		Goto AdcDone
		Endif

rem Adc A,(HL)
if 	NLEFT="A" and NRIGHT="(HL)" then
		Writecode(&h8E)
		Goto AdcDone
		Endif
		
rem Adc A,(IX+d)
If Left(Nominator,5)="A,(IX" then
		displacement=supereval(GetDisplacement(NRIGHT))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")	
		Writecode(&hDD)
		WriteCode(&h8E)
		Writecode (displacement)
		Goto AdcDone
		endif

rem Adc A,(IY+d)
If Left(Nominator,5)="A,(IY" then
		displacement=supereval(GetDisplacement(NRIGHT))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")	
		Writecode(&hFD)
		WriteCode(&h8E)
		Writecode (displacement)
		Goto AdcDone
		endif

rem Adc A,s
if NLEFT="A" then
		Writecode(&hCE)
		Writecode(supereval(NRIGHT))
		Goto AdcDone
		Endif
		
		

rem Adc HL,ss
if NLEFT="HL" and Reg16(NRIGHT) >=0 then 
		Writecode(&hED)
		encode=&h4A+(Reg16(NRIGHT)*16)
		Writecode(encode)
		Goto AdcDone
		Endif


Adderror ("Warning: ADC function failed. Missing suitable target.")

AdcDone:
return 0
End Function

Function Subtract(Byref Nominator as string) as integer
dim encode as integer
dim displacement as integer

rem Sub r
if Reg8(Nominator) >=0 then
		encode=&h90+(Reg8(Nominator))
		Writecode(encode)
		Goto SubDone
		Endif

rem Sub (HL)
if 	Nominator="(HL)" then
		Writecode(&h96)
		Goto SubDone
		Endif
		
rem Sub (IX+d)
If Left(Nominator,3)="(IX" then
		displacement=supereval(GetDisplacement(Nominator))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")	
		Writecode(&hDD)
		WriteCode(&h96)
		Writecode (displacement)
		Goto SubDone
		endif

rem Sub (IY+d)
If Left(Nominator,3)="(IY" then
		displacement=supereval(GetDisplacement(Nominator))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")	
		Writecode(&hFD)
		WriteCode(&h96)
		Writecode (displacement)
		Goto SubDone
		endif

rem Sub n
rem No test here, Just assume it's data at this point.

		encode=supereval(Nominator)
		if encode <0 or encode > 256 then Adderror ("Warning, Bad SUB, assuming default operation.")

		Writecode(&hD6)
		Writecode(encode)
		Goto SubDone
rem No error below even though it's there. 

Adderror ("Warning: Sub function failed. Missing suitable target.")

SubDone:
return 0
End Function

Function Sbc(Byref Nominator as string) as integer
dim encode as integer
dim NLEFT as string
dim NRIGHT as string
dim displacement as integer

NLEFT=GetLeft(Nominator) : rem Get left of comma.
NRIGHT=GetRight(Nominator) : rem Get right of comma. 

rem SBC	A,r
if 	NLEFT="A" and Reg8(NRIGHT) >=0 then
		Encode=&h98+Reg8(NRIGHT)
		Writecode(Encode)
		Goto SBCDone
		Endif

rem SBC A,(HL)
if 	NLEFT="A" and NRIGHT="(HL)" then
		Writecode(&h9E)
		Goto SBCDone
		Endif
		
rem SBC A,(IX+d)
If Left(Nominator,5)="A,(IX" then
		displacement=supereval(GetDisplacement(NRIGHT))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")	
		Writecode(&hDD)
		WriteCode(&h9E)
		Writecode (displacement)
		Goto SBCDone
		endif

rem SBC A,(IY+d)
If Left(Nominator,5)="A,(IY" then
		displacement=supereval(GetDisplacement(NRIGHT))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")	
		Writecode(&hFD)
		WriteCode(&h9E)
		Writecode (displacement)
		Goto SBCDone
		endif

rem SBC A,s
if NLEFT="A" then
		Writecode (&hDE)
		Writecode(supereval(NRIGHT))
		Goto SBCDone
		Endif

rem JUST THIS FAR IN EDITING. 

		
rem SBC HL,ss
if NLEFT="HL" and Reg16(NRIGHT) >=0 then
		Writecode(&hED)
		encode=&h42+(Reg16(NRIGHT)*16)
		Writecode(encode)
		Goto SBCDone
		Endif



Adderror ("Warning: SBC function failed. Missing suitable target.")

SbcDone:
return 0
End Function




Function AND8(Byref Nominator as string) as integer
dim encode as integer
dim displacement as integer

rem AND r
if Reg8(Nominator) >=0 then
		encode=&hA0+(Reg8(Nominator))
		Writecode(encode)
		Goto AndDone
		Endif

rem AND (HL)
if 	Nominator="(HL)" then
		Writecode(&hA6)
		Goto AndDone
		Endif
		
rem AND (IX+d)
If Left(Nominator,3)="(IX" then
		displacement=supereval(GetDisplacement(Nominator))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")	
		Writecode(&hDD)
		WriteCode(&hA6)
		Writecode (displacement)
		Goto AndDone
		endif

rem AND (IY+d)
If Left(Nominator,3)="(IY" then
		displacement=supereval(GetDisplacement(Nominator))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")	
		Writecode(&hFD)
		WriteCode(&hA6)
		Writecode (displacement)
		Goto AndDone
		endif

rem AND n
rem No test here, Just assume it's data at this point.

		encode=supereval(Nominator)
		if encode <0 or encode > 256 then Adderror ("Warning, Bad SUB, assuming default operation.")

		Writecode(&hE6)
		Writecode(encode)
		Goto AndDone
rem No error below even though it's there. 

Adderror ("Warning: AND function failed. Missing suitable target.")


AndDone:
return 0
End Function


Function Or8(Byref Nominator as string) as integer
dim encode as integer
dim displacement as integer

rem OR r
if Reg8(Nominator) >=0 then
		encode=&hB0+(Reg8(Nominator))
		Writecode(encode)
		Goto OrDone
		Endif

rem OR (HL)
if 	Nominator="(HL)" then
		Writecode(&hB6)
		Goto OrDone
		Endif
		
rem OR (IX+d)
If Left(Nominator,3)="(IX" then
		displacement=supereval(GetDisplacement(Nominator))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")	
		Writecode(&hDD)
		WriteCode(&hB6)
		Writecode (displacement)
		Goto OrDone
		endif

rem OR (IY+d)
If Left(Nominator,3)="(IY" then
		displacement=supereval(GetDisplacement(Nominator))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")	
		Writecode(&hFD)
		WriteCode(&hB6)
		Writecode (displacement)
		Goto OrDone
		endif

rem OR n
rem No test here, Just assume it's data at this point.

		encode=supereval(Nominator)
		if encode <0 or encode > 256 then Adderror ("Warning, Bad SUB, assuming default operation.")

		Writecode(&hF6)
		Writecode(encode)
		Goto OrDone
rem No error below even though it's there. 

Adderror ("Warning: OR function failed. Missing suitable target.")


OrDone:
return 0
End Function


Function Xor8(Byref Nominator as string) as integer
dim encode as integer
dim displacement as integer

rem XOR r
if Reg8(Nominator) >=0 then
		encode=&hA8+(Reg8(Nominator))
		Writecode(encode)
		Goto XorDone
		Endif

rem XOR (HL)
if 	Nominator="(HL)" then
		Writecode(&hAE)
		Goto XorDone
		Endif
		
rem XOR (IX+d)
If Left(Nominator,3)="(IX" then
		displacement=supereval(GetDisplacement(Nominator))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")	
		Writecode(&hDD)
		WriteCode(&hAE)
		Writecode (displacement)
		Goto XorDone
		endif

rem XOR (IY+d)
If Left(Nominator,3)="(IY" then
		displacement=supereval(GetDisplacement(Nominator))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")	
		Writecode(&hFD)
		WriteCode(&hAE)
		Writecode (displacement)
		Goto XorDone
		endif

rem XOR n
rem No test here, Just assume it's data at this point.

		encode=supereval(Nominator)
		if encode <0 or encode > 256 then Adderror ("Warning, Bad SUB, assuming default operation.")

		Writecode(&hEE)
		Writecode(encode)
		Goto XorDone
rem No error below even though it's there. 

Adderror ("Warning: OR function failed. Missing suitable target.")


XorDone:
return 0
End Function



Function CP8(Byref Nominator as string) as integer
dim encode as integer
dim displacement as integer

rem CP r
if Reg8(Nominator) >=0 then
		encode=&hB8+(Reg8(Nominator))
		Writecode(encode)
		Goto XorDone
		Endif

rem CP (HL)
if 	Nominator="(HL)" then
		Writecode(&hBE)
		Goto XorDone
		Endif
		
rem CP (IX+d)
If Left(Nominator,3)="(IX" then
		displacement=supereval(GetDisplacement(Nominator))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")	
		Writecode(&hDD)
		WriteCode(&hBE)
		Writecode (displacement)
		Goto XorDone
		endif

rem CP (IY+d)
If Left(Nominator,3)="(IY" then
		displacement=supereval(GetDisplacement(Nominator))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")	
		Writecode(&hFD)
		WriteCode(&hBE)
		Writecode (displacement)
		Goto XorDone
		endif

rem CP n
rem No test here, Just assume it's data at this point.

		Encode=supereval(Nominator)
		if Encode <0 or Encode > 256 then Adderror ("Warning, Bad SUB, assuming default operation.")

		Writecode(&hFE)
		Writecode(Encode)
		Goto XorDone
rem No error below even though it's there. 

Adderror ("Warning: OR function failed. Missing suitable target.")


XorDone:
return 0
End Function



Function Inc(Byref Nominator as string) as integer
dim encode as integer
dim displacement as integer

rem INC r
if Reg8(Nominator) >=0 then
		encode=&h04+(Reg8(Nominator)*8)
		Writecode(encode)
		Goto IncDone
		Endif

rem Inc (HL)
if 	Nominator="(HL)" then
		Writecode(&h34)
		Goto IncDone
		Endif
		
rem Inc (IX+d)
If Left(Nominator,3)="(IX" then
		displacement=supereval(GetDisplacement(Nominator))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")	
		Writecode(&hDD)
		WriteCode(&h34)
		Writecode (displacement)
		Goto IncDone
		endif

rem Inc (IY+d)
If Left(Nominator,3)="(IY" then
		displacement=supereval(GetDisplacement(Nominator))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")	
		Writecode(&hFD)
		WriteCode(&h34)
		Writecode (displacement)
		Goto IncDone
		endif

rem Inc ss
If Reg16(Nominator) >=0 then
		encode=&h03+(Reg16(Nominator)*16)
		Writecode(encode)
		Goto IncDone
		endif

rem Inc IX
If Nominator="IX" then
		Writecode(&hDD)
		Writecode(&h23)
		Goto IncDone
		endif	

rem Inc IY
If Nominator="IY" then
		Writecode(&hFD)
		Writecode(&h23)
		Goto IncDone
		endif		


IncDone:
return 0
End Function



Function Dec(Byref Nominator as string) as integer
dim encode as integer
dim displacement as integer

rem Dec r
if Reg8(Nominator) >=0 then
		encode=&h05+(Reg8(Nominator)*8)
		Writecode(encode)
		Goto DecDone
		Endif

rem Inc (HL)
if 	Nominator="(HL)" then
		Writecode(&h35)
		Goto DecDone
		Endif
		
rem Inc (IX+d)
If Left(Nominator,3)="(IX" then
		displacement=supereval(GetDisplacement(Nominator))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")	
		Writecode(&hDD)
		WriteCode(&h35)
		Writecode (displacement)
		Goto DecDone
		endif

rem Inc (IY+d)
If Left(Nominator,3)="(IY" then
		displacement=supereval(GetDisplacement(Nominator))
		if displacement>255 then adderror ("Warning: Evaluated expression for displacement exceeds 8 bits for 8 bit operation ")	
		Writecode(&hFD)
		WriteCode(&h35)
		Writecode (displacement)
		Goto DecDone
		endif

rem Dec ss
If Reg16(Nominator) >=0 then
		encode=&h0B+(Reg16(Nominator)*16)
		Writecode(encode)
		Goto DecDone
		endif

rem Dec IX
If Nominator="IX" then
		Writecode(&hDD)
		Writecode(&h2B)
		Goto DecDone
		endif	

rem Dec IY
If Nominator="IY" then
		Writecode(&hFD)
		Writecode(&h2B)
		Goto DecDone
		endif	

DecDone:
return 0
End Function


Function INPORT(Byref Nominator as string) as integer
dim encode as integer
dim displacement as integer
dim thisport as string

dim NLEFT as string
dim NRIGHT as string

NLEFT=GetLeft(Nominator) : rem Get left of comma.
NRIGHT=GetRight(Nominator) : rem Get right of comma. 


rem In r(C) or should that be IN r,(C)	: rem Going to go with r,(C)


rem In A,(n) first.
		Thisport=Getlocation(NRIGHT)	: rem Should be an 8 bit value. Might be Binary or Hex or a label, so we still need to evaluate it. 
If NLEFT="A" and NRIGHT<> "(C)" then
		Writecode(&hDB)
		Writecode(Eval(Thisport))
		Goto Indone
		Endif

rem We allready checked "A" in the above, but it can be done here too.  With BC. It's a bit more complex. 

if Reg8(NLEFT) >=0 then					
		rem Should be In r,(C) - I don't really have to check the "C"
		encode=&h040+(Reg8(NLEFT)*8)
		Writecode (&hED)
		Writecode(encode)
		Goto InDone
		Endif

	Adderror ("Warning Unknown error in IN Opcode")

INDone:
return 0
End Function



Function OUTPORT(Byref Nominator as string) as integer
dim encode as integer
dim displacement as integer
dim thisport as string

dim NLEFT as string
dim NRIGHT as string

NLEFT=GetLeft(Nominator) : rem Get left of comma.
NRIGHT=GetRight(Nominator) : rem Get right of comma. 


rem Out n,(C) or should that be IN r,(C)	: rem Going to go with r,(C)

rem Out (n),A first.
		Thisport=Getlocation(NLEFT)	: rem Should be an 8 bit value. Might be Binary or Hex or a label, so we still need to evaluate it. 
If NRIGHT="A" and NLEFT<> "(C)" then
		Writecode(&hD3)
		Writecode(Eval(Thisport))
		Goto Outdone
		Endif

rem We allready checked "A" in the above, but it can be done here too.  With BC. It's a bit more complex. 

rem Out(C),r
if Reg8(NRIGHT) >=0 then					
		rem Should be In r,(C) - I don't really have to check the "C"
		encode=&h041+(Reg8(NRIGHT)*8)
		Writecode (&hED)
		Writecode(encode)
		Goto OutDone
		Endif

	Adderror ("Warning Unknown error in OUT Opcode")

OutDone:
return 0
End Function

Function IM(Byref Nominator as string) as integer

Select Case Nominator	
	
	Case "0"
			Writecode (&hED)
			Writecode (&h46)

	Case "1"
			Writecode (&hED)
			Writecode (&h56)	
	
	Case "2"
			Writecode (&hED)
			Writecode (&h5E)
			
	Case Else
		AddError("FAILURE: Seems you want an interrupt mode that doesn't exist. ")
End Select

return 0
End function

Function RotateM(Byref Nominator as string, Byref Mask as integer) as integer
Rem Mask = Instruction OpCode Mask.
dim encode as integer	: rem coding.
dim displacement as integer : rem displacement.

rem XX r
if Reg8(Nominator) >=0 then
	Writecode(&hCB)
	Encode=Mask+(Reg8(Nominator))
	Writecode(encode)
	Goto Rotend
	Endif
	
if left(Nominator,3)="(IX" then
	displacement=supereval(GetDisplacement(Nominator))
	if displacement>255 then adderror ("Warning, Bad Displacement ")
	Writecode(&hDD)
	Writecode(&hCB)
	Writecode (displacement)
	Writecode(MASK+6)	: rem 6 = HL position. 
	Goto Rotend
	Endif
	
		
if left(Nominator,3)="(IY" then
	displacement=supereval(GetDisplacement(Nominator))
	if displacement>255 then adderror ("Warning, Bad Displacement ")
	Writecode(&hFD)
	Writecode(&hCB)
	Writecode (displacement)
	Writecode(MASK+6)	: rem 6 = HL position. 
	Goto Rotend
	Endif
	
Adderror ("Warning - Rotate and Shift Function Bad Target")

Rotend:
Return 0
End function

Function GetBit(Byref Nominator as string) as integer
dim Bitnum as integer

Bitnum=val(right(Nominator,1)) 
if Bitnum=0 and right (Nominator,1) <> "0" then adderror ("Warning - Incorrect Bit Number in bit instruction.")

return Bitnum
End Function

Function BIT8(Byref Nominator as string, Byref Mask as integer) as integer
dim encode as integer	: rem coding.
dim displacement as integer : rem displacement.
dim NLEFT as string
dim NRIGHT as string

NLEFT=GetLeft(Nominator) : rem Get left of comma.
NRIGHT=GetRight(Nominator) : rem Get right of comma. 

rem XX r
if Reg8(NRIGHT) >=0 then
	Writecode(&hCB)
	Encode=Mask+(Reg8(NRIGHT))+(Getbit(NLEFT)*8)
	Writecode(encode)
	Goto Bitend
	Endif
	
if left(NRIGHT,3)="(IX" then
	displacement=supereval(GetDisplacement(NRIGHT))
	if displacement>255 then adderror ("Warning, Bad Displacement ")
	Writecode(&hDD)
	Writecode(&hCB)
	Writecode (displacement)
	Writecode(MASK+6+(Getbit(NLeft)*8))	: rem 6 = HL position. 
	Goto Bitend
	Endif
	
		
if left(NRIGHT,3)="(IY" then
	displacement=supereval(GetDisplacement(NRIGHT))
	if displacement>255 then adderror ("Warning, Bad Displacement ")
	Writecode(&hFD)
	Writecode(&hCB)
	Writecode (displacement)
	Writecode(MASK+6+(Getbit(NLeft)*8))	: rem 6 = HL position.  
	Goto Bitend
	Endif
	
Adderror ("Warning - BIT, Set or Reset Function Bad Target")

Bitend:
return 0
End Function

Function BITCODE (Byref Nominator as string) as integer
dim result as integer

Select Case Nominator
	Case "NZ"
		result=0
	Case "Z"
		result=1
	Case "NC"
		result=2
	Case "C"
		result=3
	Case "PO"
		result=4
	Case "PE"
		result=5
	Case "P"
		result=6
	Case "M"
		result=7
	Case else
	result=-1
End Select

return result
End function

Function JUMP(Byref Nominator as string) as integer
dim encode as integer	: rem coding.
dim disp as integer : rem displacement.
dim NLEFT as string
dim NRIGHT as string
dim displacement as integer
dim MEMLOC as integer

	NLEFT=GetLeft(Nominator) : rem Get left of comma.
	NRIGHT=GetRight(Nominator) : rem Get right of comma. 

	relative=0
	
rem JP condition, Absolute
IF Bitcode(NLEFT)>=0 then

		Encode=&hC2+(Bitcode(NLEFT)*8)
		Writecode(encode)
		MEMLOC=Eval(NRIGHT)
		If debug = 1 then print "Jump Target:";hex(MEMLOC,4)
		Writecode(Lower(MEMLOC))
		Writecode(Higher(MEMLOC))
		Goto Jumpend
		Endif

rem JP (HL)
If Nominator="(HL)" then
		Writecode (&hE9)
		Goto Jumpend
		Endif

rem JP (IX)
If Nominator="(IX)" then
		Writecode (&hDD)
		Writecode (&hE9)
		Goto Jumpend
		Endif

rem JP (IY)
If Nominator="(IY)" then
		Writecode (&hFD)
		Writecode (&hE9)
		Goto Jumpend
		Endif


rem JP absolute. is likely. 
		Writecode(&hC3)
		MEMLOC=Eval(NRIGHT)
		If debug = 1 then print "Jump Target:";hex(MEMLOC,4)
		Writecode(Lower(MEMLOC))
		Writecode(Higher(MEMLOC))

Jumpend:
return 0
End Function


Function CallJump(Byref Nominator as string) as integer
dim encode as integer	: rem coding.
dim disp as integer : rem displacement.
dim NLEFT as string
dim NRIGHT as string
dim displacement as integer
dim MEMLOC as integer

	NLEFT=GetLeft(Nominator) : rem Get left of comma.
	NRIGHT=GetRight(Nominator) : rem Get right of comma. 

	relative=0
	
rem Call condition, Absolute
IF Bitcode(NLEFT)>=0 then

		Encode=&hC4+(Bitcode(NLEFT)*8)
		Writecode(encode)
		MEMLOC=Eval(NRIGHT)
		If debug = 1 then print "Jump Target:";hex(MEMLOC,4)
		Writecode(Lower(MEMLOC))
		Writecode(Higher(MEMLOC))
		Goto Callend
		Endif

rem Call absolute. is likely. 
		Writecode(&hCD)
		MEMLOC=Eval(NRIGHT)
		If debug = 1 then print "Jump Target:";hex(MEMLOC,4)
		Writecode(Lower(MEMLOC))
		Writecode(Higher(MEMLOC))

Callend:
return 0
End Function


Function RstJump(Byref Nominator as string) as integer
dim encode as integer	: rem coding.

	relative=0
	
rem Question: Do I ever want to allow labels here to move stuff around? eg, RST PRINT where EQU PRINT,8? Future upgrades. 
	
rem Probably easiest for one per case,

rem RST - eg RST $10 or RST 10H - Need to accept both. Maybe should even consider decimal.
Select Case Nominator
		Case "0","00H","$00"
			Writecode (&hC7)
		Case "8","08H","$08"
			Writecode (&hCF)
		Case "10","10H","$10"
			Writecode (&hD7)
		Case "18","18H","$18"
			Writecode (&hDF)
		Case "20","20H","$20"
			Writecode (&hE7)
		Case "28","28H","$28"
			Writecode (&hEF)
		Case "30","30H","$30"
			Writecode (&hF7)
		Case "38","38H","$30"
			Writecode (&hFF)
		Case Else
		Adderror ("Unrecognized RST routine. ")
End Select
	
RSTend:
return 0
End Function



Function ReturnJump(Byref Nominator as string) as integer

dim encode as integer	: rem coding.

	relative=0
	
rem Ret cc condition, Absolute
IF Bitcode(Nominator)>=0 then

		Encode=&hC0+(Bitcode(Nominator)*8)
		Writecode(encode)
		Goto Retend
		Endif


rem RET
	if Nominator <> "" then Adderror ("Warning - Return Condition Not Recognized")
	Writecode(&hC9)

RetEnd:
return 0
End Function 


Function JUMPREL(Byref Nominator as string) as integer
dim encode as integer	: rem coding.
dim disp as integer : rem displacement.
dim NLEFT as string
dim NRIGHT as string
dim displacement as integer
dim MEMLOC as integer
dim TESTVAL as integer	: rem for out of bounds test. 

NLEFT=GetLeft(Nominator) : rem Get left of comma.
NRIGHT=GetRight(Nominator) : rem Get right of comma. 



relative=1

IF Bitcode(NLEFT)>=0 then
		Encode=&h20+(Bitcode(NLEFT)*8)
		if Bitcode(NLEFT)>3 then adderror ("Warning - Illegal Jump Relative Condition")
				
			Writecode(encode)
			rem Conditioned JR - eg, JR Z,<DEST>
		
		else
			
			Writecode(&h18)
			rem Conditionless JR - eg, JR <DEST>

		endif
		
RELBYTE:
		MEMLOC=Eval(NRIGHT)
		DISPLACEMENT=MEMLOC		: Rem I'm not using this variable, so let's hold the destination for a bit. 
		If debug = 1 then print "Jump Target:";hex(MEMLOC,4)

		MEMLOC=MEMLOC-PC-1	: rem Make Relative. The minus-1 is because we progressed the PC when we picked up the opcode for the target of the jump. 

		TESTVAL=MEMLOC
		
 		MEMLOC=MEMLOC AND &hFF		: rem Just keep 8 bits of it. 
		IF TESTVAL > &h7F and TESTVAL < &hFF80 and pass <> 1 then 		
				rem We do it this way, because otherwise it doesn't know that the value can be -80 (negative larger than positive in signed integers )
				AddError ("Warning - Relative jump from "+hex(PC+1,4)+" to "+hex(Displacement,4)+" is out of bounds. ")  
				ENDIF

		

		rem Test for Displacement 0, since if we're trying to absolute jump to 0000, it's probably because we haven't found the label location yet. 
		rem The Displacement 0 is to avoid errors... But since we have passes, let's use that in case we get a real jump rel to zero. 

		Writecode(Lower(MEMLOC))				
		
		if debug=1 then print "Special Debugging: DISP=";hex(memloc,4);" and absolute from:";hex(PC-1,4);" to:";hex(displacement,4);"  at line:";linenumber
		
Jumpend:

rem Change Function DJNZ if above changes. They use the same code, but not the same routine.

return 0
End Function
	
				
Function JUMPDJNZ(Byref Nominator as string) as integer
dim MEMLOC as integer
dim TESTVAL as integer	: rem for out of bounds test. 
dim DISPLACEMENT as integer  : rem hold for debugging. 

rem Change Function DJNZ if above changes. They use the same code, but not the same routine.

relative=1

		Writecode(&h10)

		MEMLOC=Eval(Nominator)
		DISPLACEMENT=MEMLOC		: Rem I'm not using this variable, so let's hold the destination for a bit. 
		If debug = 1 then print "Jump Target:";hex(MEMLOC,4)

		MEMLOC=MEMLOC-PC-1	: rem Make Relative. The minus-1 is because we progressed the PC when we picked up the opcode for the target of the jump. 

		TESTVAL=MEMLOC
 		MEMLOC=MEMLOC and &hFF		: rem MASK instead of MOD since -80 is a valid number 
		IF TESTVAL > &h7F and TESTVAL < &hFF80 and pass <> 1 then 	
			AddError ("Warning - Relative jump from "+hex(PC+1,4)+" to "+hex(Displacement,4)+" is out of bounds. ")  
			endif
		rem Test for Displacement 0, since if we're trying to absolute jump to 0000, it's probably because we haven't found the label location yet. 
		rem The Displacement 0 is to avoid errors... But since we have passes, let's use that in case we get a real jump rel to zero. 

		Writecode(Lower(MEMLOC))				
		
		if debug=1 then print "Special Debugging: DISP=";hex(memloc,4);" and absolute from:";hex(PC-1,4);" to:";hex(displacement,4);"  at line:";linenumber
	
Jumpend:
return 0
End Function			

Function AssignLabel(Byref Nominator as string) as integer
rem Call to assign label outside of usual process. Special values. 
dim encode as integer	: rem coding.
dim disp as integer : rem displacement.
dim NLEFT as string
dim NRIGHT as string
dim displacement as integer
dim MEMLOC as integer

NLEFT=GetLeft(Nominator) : rem Get left of comma.
NRIGHT=GetRight(Nominator) : rem Get right of comma. 

relative=-7	: rem Assign as a value... 8 or 16 bit doesn't matter. We assume it's always the right size.

Setlabel(NLEFT,supereval(NRIGHT))

Jumpend:
return 0
End Function			

Function Litstring(Byref Nominator as string) as integer
dim posit as integer	: rem counter for string.
dim stripped as string	: rem Work on string.

stripped=nominator

if left(stripped,1) <> chr(34) or right(stripped,1) <> chr(34) then
	adderror ("Warning - String defined with missing double quotes. Internal quotes are ignored ")
	endif
stripped=left(stripped,len(stripped)-1)
stripped=right(stripped,len(stripped)-1)

for posit=1 to len(stripped)
	Writecode(asc(mid(stripped,posit,1)))
	next posit

return 0
End function

Function Blockwrite(Byref Nominator as String) as integer
dim count as integer

count=eval(Nominator)	: rem Should be a single number. Should not change too much... I can use supereval if necessary. Need to change order of routines first.

while count > 0 
	Writecode(0)	: rem Just fill in 00's in the block. I don't think the fill pattern matters. 
	count=count-1
wend

return 0
End function

Function LitByte(Byref Nominator as String) as integer
rem Program in literal data, and process formulas.
dim segment as string
dim scan as integer	: rem where are we in the string?
dim Loperator as string : rem I just need a way to complete logical operations on subsequent passes. Might as well use a string for clarity
dim Toperator as string : rem THIS operator - Record in last until we get the next byte.
dim result as integer	: rem In the end, we convert all back to an integer. 
dim cont as integer : rem Flag for whether we continue. 
dim t1 as integer : rem Toggle for Single quotes. 
dim t2 as integer : rem Toggle for Double quotes. 
dim exception as integer	: rem counter for exception for string bytes eg, '1234567890'

rem We may need to scan the byte for logical operators. 
rem Operators are * or + or ! or - or @(xor) or something else. Need to learn what to put in here.
rem Initially, at least do + and * and !... Maybe. 
rem Can be a list separated by commas also. eg, 'A', "'B'" and can mix formats, eg "'A', $01"
rem examples-
rem DB 01,$5A,%01010101,'G','A'+%1000000,3*1 : rem last example is 3 and 1. 
rem 

rem Keep Separate as we want to test for 8 bits or make an error.

Loperator=""
cont=1
scan=0
result=0
segment=""
t1=0
t2=0

Litdecode:
while scan < len (nominator) and cont			
	rem - Make < len because we update scan location next. ( rather than <= )
	scan=scan+1
	
	if mid(nominator,scan,1)="'" and t2=0 then t1=(t1+1) mod 2 		: rem Check to disable other character checking within the quote. 
	if mid(nominator,scan,1)=chr(34) and t1=0 then t2=(t2+1) mod 2 	: rem Check to disable other character checking within the quote. 
	
	if t1=1 and t2=1 then adderror ("FAILURE - Quote confusion. Single and Double quotes both active- Code error in assembler. ")
	
	if t1=0 and t2=0 then
		rem T1 or T2 being 1 means blocking other character checks because it's a literal character. 
		if mid(nominator,scan,1)="," then Toperator="next" : cont=0
		if mid(nominator,scan,1)="*" then Toperator="and" : cont=0
		if mid(nominator,scan,1)="+" then Toperator="or" : cont=0
		if mid(nominator,scan,1)="!" then Toperator="not" : cont=0
		if mid(nominator,scan,1)="&" then Toperator="xor" : cont=0
		if mid(nominator,scan,1)="<" then Toperator="rleft" : cont=0
		if mid(nominator,scan,1)=">" then Toperator="rright" : cont=0
		endif
	if cont then segment=segment+mid(nominator,scan,1)
	wend

If left(segment,1)="'" and right(segment,1)="'" and len(segment)>3 then 
	rem String Handling Exception - A series of Bytes, and we blind write all except the last, which we process as a single byte 
	for exception=2 to len(segment)-2
		Writecode asc(mid(segment,exception,1))
		next exception
		segment="'"+mid(segment,exception,1)+"'"
	Endif

If Toperator <> "" then cont=1 : rem Mark to continue again. 

If Loperator="" and Toperator="next" then 
	Loperator="next"	: rem Finish this byte now. 
	result =eval(segment) : rem Establish the result.
	segment="" : 			rem Clear the segment
	cont=1		:			rem Highlight we want to continue.
	endif


		
Select Case Loperator	
rem Toperator is current operator - Just find a way to continue. 	
	Case "and"
		result=result AND eval(segment)
		segment=""
		Loperator=""
			
	Case "or"
		result=result OR eval(segment)
		segment=""	
		Loperator=""
		
	Case "not" : rem difficult to implement with logic. Test and do later. 
		result= NOT result
		Loperator=""
		
	Case "xor"
		result= result XOR eval(segment)
		segment=""
		Loperator=""
		
	Case "rleft"
		result = result SHL 1
		Loperator=""
		
	Case "rright"
		result = result SHR 1
		Loperator=""
		
	Case "next"
		Writecode(result)
		Toperator=""	: rem Make sure Loperator is cleared in case we get a single byte EOL. 
		result=0 
		Loperator=""
		
	Case Else
		rem No arithmetic operators at this stage. 
		result=eval(segment)			: rem Set the result. 
		
		if t1=1 then adderror ("Warning - No closing quotation (single). ")
		if t2=1 then adderror ("Warning - No closing quotation (double). ")
		
End Select


If Toperator="next" or scan>=len(nominator)then
		Writecode(result)
		Toperator=""	: rem Make sure Loperator is cleared in case we get a single byte EOL. 
		result=0 
		segment=""
		endif

		if scan >= len(nominator) then 
			cont=0						: rem Indicate we are done. 
			endif

	Loperator=Toperator : rem remember the last operator for when we get another term. 
	
	Toperator=""
	segment=""
	
if cont then goto Litdecode

return 0	: rem We just write the bytes from here. 
End Function



Function LitWord(Byref Nominator as String) as integer
rem Program in literal data, and process formulas. Copied from LitByte but writes two bytes, lower order first. 
dim segment as string
dim scan as integer	: rem where are we in the string?
dim Loperator as string : rem I just need a way to complete logical operations on subsequent passes. Might as well use a string for clarity
dim Toperator as string : rem THIS operator - Record in last until we get the next byte.
dim result as integer	: rem In the end, we convert all back to an integer. 
dim cont as integer : rem Flag for whether we continue. 
dim t1 as integer : rem Toggle for Single quotes. 
dim t2 as integer : rem Toggle for Double quotes. 

rem We may need to scan the byte for logical operators. 
rem Operators are * or + or ! or - or @(xor) or something else. Need to learn what to put in here.
rem Initially, at least do + and * and !... Maybe. 
rem Can be a list separated by commas also. eg, 'A', "'B'" and can mix formats, eg "'A', $01"
rem examples-
rem DB 01,$5A,%01010101,'G','A'+%1000000,3*1 : rem last example is 3 and 1. 
rem 

Loperator=""
cont=1
scan=0
result=0
segment=""
t1=0
t2=0

Litdecode:
while scan < len (nominator) and cont			
	rem - Make < len because we update scan location next. ( rather than <= )
	scan=scan+1
	
	if mid(nominator,scan,1)="'" and t2=0 then t1=(t1+1) mod 2 		: rem Check to disable other character checking within the quote. 
	if mid(nominator,scan,1)=chr(34) and t1=0 then t2=(t2+1) mod 2 	: rem Check to disable other character checking within the quote. 
	
	if t1=1 and t2=1 then adderror ("FAILURE - Quote confusion. Single and Double quotes both active- Code error in assembler. ")
	
	if t1=0 and t2=0 then
		rem T1 or T2 being 1 means blocking other character checks because it's a literal character. 
		if mid(nominator,scan,1)="," then Toperator="next" : cont=0
		if mid(nominator,scan,1)="*" then Toperator="and" : cont=0
		if mid(nominator,scan,1)="+" then Toperator="or" : cont=0
		if mid(nominator,scan,1)="!" then Toperator="not" : cont=0
		if mid(nominator,scan,1)="&" then Toperator="xor" : cont=0
		if mid(nominator,scan,1)="<" then Toperator="rleft" : cont=0
		if mid(nominator,scan,1)=">" then Toperator="rright" : cont=0
		endif
	if cont then segment=segment+mid(nominator,scan,1)
	wend


If Toperator <> "" then cont=1 : rem Mark to continue again. 

If Loperator="" and Toperator="next" then 
	Loperator="next"	: rem Finish this byte now. 
	result =eval(segment) : rem Establish the result.
	segment="" : 			rem Clear the segment
	cont=1		:			rem Highlight we want to continue.
	endif


		
Select Case Loperator	
rem Toperator is current operator - Just find a way to continue. 	
	Case "and"
		result=result AND eval(segment)
		segment=""
		Loperator=""
			
	Case "or"
		result=result OR eval(segment)
		segment=""	
		Loperator=""
		
	Case "not" : rem difficult to implement with logic. Test and do later. 
		result= NOT result
		Loperator=""
		
	Case "xor"
		result= result XOR eval(segment)
		segment=""
		Loperator=""
		
	Case "rleft"
		result = result SHL 1
		Loperator=""
		
	Case "rright"
		result = result SHR 1
		Loperator=""
		
	Case "next"
		Writecode(lower(result))
		Writecode(higher(result)) : rem Write both bytes. 
		Toperator=""	: rem Make sure Loperator is cleared in case we get a single byte EOL. 
		result=0 
		Loperator=""
		
	Case Else
		rem No arithmetic operators at this stage. 
		result=eval(segment)			: rem Set the result. 
End Select


If Toperator="next" or scan>=len(nominator)then
		Writecode(Lower(result))
		Writecode(Higher(result))	: rem Write both bytes. 
		Toperator=""	: rem Make sure Loperator is cleared in case we get a single byte EOL. 
		result=0 
		segment=""
		endif

		if scan >= len(nominator) then 
			cont=0						: rem Indicate we are done. 
			endif

	Loperator=Toperator : rem remember the last operator for when we get another term. 
	
	Toperator=""
	segment=""
	
if cont then goto Litdecode

return 0	: rem We just write the bytes from here. 
End Function





Function Assemble () as string
rem: result=the binary codes for 
dim result as string

Operand=Casefix(Operand)

If debug>0 then Print "Operand:";operand;"    Target:";target;"   PC:";hex(PC,4)

Nominator=Casefix(Target)

Select Case Operand

rem Assembler Directives. 
		Case "ORG..", ".ORG."
			PC=SUPEREVAL(target)
			Print "ORG Statement found:";target; " on line ";linenumber;" Equates to:";hex(PC,4)
			
		Case "DEFB.", "DB...", ".DEFB", ".DB.."
rem			SuperEVAL(target)	
			Litbyte(target)

		Case "DEFC."
			Writecode(Supereval(Target))

		Case "DEFM.", "DM...", ".DEFM", ".DM..", ".TEXT", "TEXT."
			Litstring(target)

		Case "DEFW.", "DW...", ".DEFW", ".DW.."
			LitWord(target)	: rem let's use LitByte here, since it handles 8 and 16 bit bytes... And then we can use the logic operators also... Actually, it really goes to any length. Depends on the instruction.

		Case ".EQU.", "EQU..","=....","EQ..."
			AssignLabel(Target)
			if absolutelabel then adderror("Warning - Label with colon assigned PC instead of EQUvalue.")
		rem eg, EQU	COUNTER,$55 - Assigns the value 55 when it sees the label COUNTER. 

		Case "#DEFI"
			rem Do nothing. We can ignore this - it's for other assemblers. Just make the above take all examples. 
			
		Case "BLOCK",".BLK.","DS...",".DS.."
		rem DS for Define Space seems to be legit. 
		rem Write a block of 00's target characters long.  At the current PC.
			Blockwrite(target)
		


			
rem 8-bit load group. Shared with 16-bit load group.

rem Handle Push and Pop within the LD group ( 16 bit load elements )	
		Case "LD..."
                rem"ld..."
				LD (Nominator)			: rem And let's complete the function.
	
		Case "PUSH."
                rem"PUSH."
				Push (Nominator)			: rem And let's complete the function.
				
		Case "POP.."
                rem"POP.."
				Pop (Nominator)			: rem And let's complete the function.

Rem Block and Exchange and Transfer and Search functions.
		Case "EX..."
				rem EX
				EX (Nominator)
				
		Case "EXX.."
				rem EXX
				EX ("X")					: rem Special case. Exchange all.
		
		Case "LDI..","LDIR.","LDD..","LDDR.","CPI..","CPIR.","CPD..","CPDR."
				rem Transfer function. 
				LDIR(Casefix(Operand),Nominator)
	


Rem Arithmetic ( 8 and 16 bit are mixed. )


		Case "ADD.."
			Add (Nominator)
			
		Case "ADC.."
			ADC (Nominator)

		Case "SUB.."
			Subtract (Nominator)

		Case "SBC.."
			SBC(Nominator)
	
rem Logical and Comparison. 

		Case "AND.."
			AND8(Nominator)
			
		Case "OR..."
			OR8(Nominator)
			
		Case "XOR.."
			XOR8(Nominator)
			
		Case "CP..."
			CP8(Nominator)
			
		Case "INC.."
			INC(Nominator)
			
		Case "DEC.."
			DEC(Nominator)

Rem Single stuff. CPU Controls. 

		Case "DAA.."
			Writecode(&h27)
			
		Case "CPL.."
			Writecode(&h2F)
			
		Case "NEG.."
			Writecode(&hED)
			Writecode(&h44)
			
		Case "CCF.."
			Writecode(&h3f)
		
		Case "SCF.."
			Writecode(&h37)
			
		Case "NOP.."
			Writecode (&h00)
			
		Case "HALT."
			Writecode (&h76)
			
		Case "DI..."
			Writecode (&hF3)
			
		Case "EI..."
			Writecode (&hFB)
			
		Case "IM0.."
			Writecode (&hED)
			Writecode (&h46)
		
		Case "IM1.."
			Writecode (&hED)
			Writecode (&h56)
		
		Case "IM2.."
			Writecode (&hED)
			Writecode (&h5E)
		
		Case "IM..."
			rem As above, but OOpps! A space.
			IM(Nominator)

rem Bit and Shift		
		Case "RLCA."
				Writecode(&h07)
		Case "RLA.."
				Writecode(&h17)
		Case "RRCA."
				Writecode(&h0F)
		Case "RRA.."
				Writecode(&h1F)
		Case "RLC.."
				RotateM(Nominator,&h00)
		Case "RRC.."
				RotateM(Nominator,&h08)
		Case "RL..."
				RotateM(Nominator,&h10)
		Case "RR..."
				RotateM(Nominator,&h18)
		Case "SLA.."
				RotateM(Nominator,&h20)
		Case "SRA.."
				RotateM(Nominator,&h28)
		Case "SLL.."
				RotateM(Nominator,&h30)
		Case "SRL.."
				RotateM(Nominator,&h38)
		Case "RLD.."
				Writecode(&hED)
				Writecode(&h6F)
		Case "RRD.."
				Writecode(&hED)
				Writecode(&h67)

		Case "BIT.."
				Bit8(Nominator,&h40)
		Case "RES.."
				Bit8(Nominator,&h80)
		Case "SET.."
				Bit8(Nominator,&hC0)


rem Jump group.

		Case "JP..."
				rem Jump Direct.
				JUMP(Nominator)

		Case "JR..."
				rem Jump Relative.
				JUMPREL(Nominator)

		Case "DJNZ."
				rem DJNZ...
				JUMPDJNZ(Nominator)
			
		Case "DJNZ,"
				rem Could be with a comma - eg, DJNZ,
				JUMPDJNZ(Nominator)
    

				
rem The Call girls.

		Case "CALL."
				rem Call nn or Call cc,nn
				CallJump(Nominator)
		Case "RET.."
				ReturnJump(Nominator)
		
		Case "RETI."
				Writecode(&hED)
				Writecode(&h4D)
				
		Case "RETN."
				Writecode(&hED)
				Writecode(&h45)
				
		Case "RST.."		
				RstJump(Nominator)
				
rem Input and Output Group.

rem		In A,(n) and In r(C)
		Case "IN..."		
				INPORT(Nominator)
		Case "OUT.."
				OUTPORT(Nominator)
		Case "INI.."
				Writecode(&hED)
				Writecode(&hA2)
		Case "INIR."
				Writecode(&hED)
				Writecode(&hB2)
		Case "IND.."
				Writecode(&hED)
				Writecode(&hAA)
		Case "INDR."
				Writecode(&hED)
				Writecode(&hBA)

		Case "OUTI."
				Writecode(&hED)
				Writecode(&hA3)
		Case "OTIR."
				Writecode(&hED)
				Writecode(&hB3)
		Case "OUTD."
				Writecode(&hED)
				Writecode(&hAB)
		Case "OTDR."
				Writecode(&hED)
				Writecode(&hBB)				


		Case ".END.", "#END."
				Print ".END found at PC:";hex(PC,4);" - Last valid location:";hex(pc-1,4)
			
				
				
                 
	Case Else
	
		rem 		result="nocode"
		rem Check for exceptions. 

		rem - Here I can process exceptions such as EQU after the label. 
		
			originalline=originalline+"   ;    NULL "
		
			firstword=getoneword()
			secondword=getoneword()
			thirdword=originalline
			rem Break down the line for further checks. 
	
		if secondword="equ" or secondword=".EQU" or secondword=".eq" or secondword="=" or secondword="eq" then

			
			if debug>0 then print "EQU found out of order, Trying:";secondword;" ";firstword;",";thirdword
			
			oneline=secondword+" "+firstword+","+thirdword
			tryagain=1
			operand=getoperand(oneline)
			
		else
			
			if operand <> "....." then AddError ("Warning - Instruction "+operand+" not found or understood.")
					
			endif
End Select

result=result+"                "
result=left(result,16)

return result
End function



Function Labelpass() as integer

linenumber=1

ASMFILE=COMMAND+".ASM"
BINFILE=COMMAND+".BIN"
HEXFILE=COMMAND+".HEX"

open ASMFILE for input as #9

Nextline:

line input #9,oneline
originalline=oneline		: rem store for later. 
untouched=oneline			: rem Store for errors. 

linePC=PC					: rem Record where we started ( PC before we started writing the instruction. ) 

Operand=GetOperand(oneline)
if Operand <> "....." then 
	Assemble()
	Endif
if tryagain then
	Assemble()
	tryagain=0
	Endif

absolutelabel=0	: rem clean any absolute label.

linenumber=linenumber+1

if EOF (9) then goto Gotlabels
if linenumber < 60000 then goto Nextline


Gotlabels:

Print "Label Pass Complete - ";numlabels;" labels found."
Close #9
return 0
End function


Function Compare() as integer
rem ########################################################################################################
dim binlen as integer
dim infile as string
dim mybyte as integer

infile=Loadfile ("48k.rom")

Print "Tested ";binlen;" bytes."

for binlen=1 to len(infile)
Mybyte=((code(binlen)+65536) mod 256)

if Mybyte <> asc(mid(infile,binlen,1)) then 
	print "Error: Binary Mismatch at location:";hex(binlen-1,4); "   Source line:";CODELINE(binlen); 
	print "  Expected:";hex(asc(mid(infile,binlen,1)),2);
	print "  I got:";hex(Mybyte,3)
	endif
next binlen


Print "Tested ";binlen-1;" bytes."


return 0
End Function


main:

Labletable(1)="12345678901234567890123456789012----"
errormessage=""
crlf=chr(10)+chr(13)
numlabels=1
pass=1

debug=0
maxinstructions=65535
showmemory=0
showstart=0
showend=0
savememory=0
savestart=0
saveend=0
savefile=""
debugonline=0	: rem when one line is debugged.
debugoffline=0
comparerom=0
showlabels=0	: rem Should we show the labletable?

open "asm.cfg" for input as #1
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
		
		showcode=parameter("SHOWCODE",showcode)

		debugonline=parameter("DEBUGONLINE",debugonline)
		debugoffline=parameter("DEBUGOFFLINE",debugoffline)
		comparerom=parameter("COMPAREROM",comparerom)
		
		showlabels=parameter("SHOWLABELS",showlabels)

    if CLI<>" " then goto READBATCH
close #1

Print "Debug from:";debugonline;" to "; debugoffline

rem LoadSource("48k.asm")

rem for sourcecount=1 to 10000
rem print mid(source,sourcecount,1);
rem next sourcecount


firstcode=&hFFFF : rem the only way is down.

PC=&hFFFF	: rem Set Program Counter. Set to end of memory so we have to have an org. 


trap=0 		
print TIME

Print "Getting Labels."
Labelpass()	: rem Do the following first to populate all the labels. 

PC=&hFFFF
linenumber=1
pass=2

open ASMFILE for input as #9

Nextline:

if linenumber=debugonline then debug=1
if linenumber=debugoffline then debug=0

line input #9,oneline
originalline=oneline		: rem store for later. 
untouched=oneline			: rem Store for errors. 


if debug > 0 then print "Line:";linenumber;"  ";oneline



linePC=PC					: rem Record where we started ( PC before we started writing the instruction. ) 



Operand=GetOperand(oneline)
if debug > 0 then print "Operand at";linenumber;":";Operand
if Operand <> "....." then 
	Assemble()
	Endif
if tryagain and operand<> "....." then
	Assemble()
	tryagain=0
	Endif

absolutelabel=0	: rem clean any absolute label.

linenumber=linenumber+1

if EOF (9) then goto completed
if linenumber < 60000 then goto Nextline

Completed:
close #9

print

Print TIME

Print "Compiled";Linenumber;" lines"


if debug > 0 then
	Print "Config-"
	Print "Debug:";debug
	Print "Lines:";maxinstructions
	print "Showmemory";showmemory
	print "Sbowstart:";showstart
	print "Showend:";showend
	print "Savememory:";savememory
	print "Savestart:";savestart
	print "Saveend:";saveend
	print "Savefile:";savefile
	endif

print
if showmemory then showram()

if showcode then showhex(firstcode,lastcode)


if savememory then saveram()

testlabels()	: rem Check for unassigned labels. 

if showlabels then
	print "Label Table:"
	for count=1 to numlabels
	print count,labletable(count)
	next count
	endif

if errormessage <> "" then
		print
		print "########## WARNING - Assembly did not complete. ##########"
		print errormessage
		endif
		
print

Print "Final Code Start:";FIRSTCODE
Print "Final Code End  :";LASTCODE
print
if errormessage="" then
	print "No errors. Building binary file."
	savecode()
	print "Writing Intel Hex File also."
	saveintelhex()
	endif

if comparerom then compare()

end
