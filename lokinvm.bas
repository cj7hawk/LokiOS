rem LOKIDISK - Creates a DISK for LOKI L: from a series of files, offset from F1000 - As L:
rem
rem 17-3-23 Fixed bug in record writing that didn't reduce number of records to write for very large files in multiple extents. 

dim shared DISK as string		: rem 27512 ROM = 64K. That's the size of this disk. Actual disk includes BOOTSTRAP.BIN from 0000 to 0FFF also, then directory, then files.


dim shared DIRECTORY as string	: rem As we build the directory.

dim shared FILESPACE as string 	: rem Temporary space to store the files.... 
dim shared nullfile as string		: rem Construction of a NULL FILE - eg, Blank File followed by FF for Eprom Use. So we can add files later and reburn. 


dim shared BOOTFILE as string
dim shared BDOSFILE as string
dim shared BIOSFILE as string

dim shared FILEIN as string		: rem DOS.
dim shared fileout as string	: rem 8+3 format.
dim shared filecontent as string	: rem the actual file itself. 
dim shared thisfile as string	: rem Contents of the single opened file. 
dim shared ALLOCATIONS as integer
dim shared filesize as integer	: rem
dim shared records as integer	: rem How many records have we written to the file?
dim shared erecords as integer 	: rem How many records in just this extent?
dim shared blocks as integer	: rem How many blocks in the file?
dim shared extents as integer	: rem Which extent are we writing?
dim shared nextalloc as integer	: rem Current allocation ( base )

dim shared eloop as integer		: rem Extent LOOP. 
dim shared aloop as integer		: rem Allocation Loop.
dim shared anum as integer		: rem Allocation Number for the loop. 
dim shared allopoint as integer	: rem allocation pointer. 

dim shared alloc(16) as integer	: rem Array of allocations we want to write into the current extent.  


dim shared missing as integer	: rem missing padding on end of file. 

BOOTFILE="bootstrap.bin"
BDOSFILE="My-Bdos.bin"
BIOSFILE="bios.bin"
FILEIN="ccp.bin"
FILEOUT="ccp     bin"

rem File Name Structure 
rem UU F1 F2 F3 F4 F5 F6 F7 F8 T1 T2 T3 EX S1 S2 RC   .FILENAMETYP....
rem AL AL AL AL AL AL AL AL AL AL AL AL AL AL AL AL   ................

rem Initialise variables. 


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
	
end function	


Function PadFile (Byref filename as string, Byref Thislong as integer ) as string
rem Call Loadfile, then pad with spaces until it's "thislong".
dim result as string

	result=LoadFile (filename)
	if len(result) > Thislong then print "##### ERROR - Input file is bigger than allocated space. Truncation will occur #####"
	print "Padding:";filename;len(result);" bytes - Segment is";
	result=result+string(Thislong,0)
	result=left(result,Thislong)
	print len(result);" bytes"
return result
end function

function COPYBOOT (Byref DISK as string) as integer

dim result as integer
dim block as string
result=0
block=padfile(BOOTFILE,256)
print "Loading:";BOOTFILE;" Length:";LEN(BLOCK)

close #2

return result
end function

function PAD (Byref text as string, Byref Length as integer ) as string
rem Pad out any space to achieve LENGTH size.
dim result as string

	result=text+string(Length,0)
	result=left(result,Length)

return result
end function

function PADE5 (Byref text as string, Byref Length as integer ) as string
rem Pad out any space to achieve LENGTH size.
dim result as string

	result=text+string(Length,&h0E5)
	result=left(result,Length)

return result
end function

function SECTPAD (Byref text as string) as string
rem Pad a file to the allocation size ( allocation = 1024bytes)
dim result as string
dim size as integer
dim resize as integer

rem Pad the sector to allocation size blocks, and records records, allocations and extents. 

size=len(text)
resize=(int((size-1)/1024)+1)*1024
records=(int((size-1)/128)+1)
result=text+string(1023,0)
result=left(result,resize)
ALLOCATIONS=(resize/1024)
extents=(int((allocations-1)/16)+1)
Print "File size:";size;"  Resizing to:";resize ;" = ";resize/1024;" allocations contained in"; records;" records to write in";extents;" Extents - SCHECK:";len(RESULT)


return result
end function


function transfer (Byref DISKFILE as string, Byref Fileout as string) as integer
dim result as integer
rem ALL GLOBAL VARIABLES.

thisfile=sectpad(loadfile(DISKFILE))

print "Adding file:";fileout
filecontent=filecontent+thisfile	: rem File is now added as allocations to the filecontent ( written to disk last ). 

for eloop=0 to extents-1		: rem  As many allocation filenames as we need to add. 

directory=directory+chr(0)+fileout+chr(eloop)+chr(0)+chr(0)

if allocations>16 then
	allocations=allocations-16
	records=records-128
	anum=16
	directory=directory+chr(&h80)
	else
	anum=allocations
	directory=directory+chr(records)	
	endif

	for aloop = 1 to 16		: rem number of allocations.
		if	aloop <= anum then
			directory=directory+chr(nextalloc)
			nextalloc=nextalloc+1		: rem Increment allocation number
rem 			print "Writing allocation:";aloop;"with block:";nextalloc
			else
			directory=directory+chr(0)
rem 			print "Writing allocation:";aloop;"with block:";0
		endif
	next aloop
next eloop


return result
end function

MAIN:

rem disk=disk+padfile(bootfile,256) : rem NOT A SYSTEM DISK, so no boot structure here. 
rem disk=disk+padfile(bdosfile,2816)
rem disk=disk+padfile(biosfile,1024)

print "Disk Boot Image Created - LOKI.DSK ";len(disk);" bytes"



nextalloc=4						: rem Start with allocation 4 - Allocations 0,1,2,3 are used for the directory. Allocations are 1K. Similar to L: drive. 


rem FILE TRANSFER LIST - THIS IS FIXED SINCE IT GENERATES AN EPROM OUTPUT. 
transfer ("longtext.txt","TEST    TXT")
transfer ("zork2.com","ZORK2   COM")
transfer ("zork2.dat","ZORK2   DAT")
transfer ("zork3.com","ZORK3   COM")
transfer ("zork3.dat","ZORK3   DAT")
transfer ("mbasic.com","MBASIC  COM")
transfer ("memdump.bin","MEMDUMP COM")
transfer ("testx.bin","TESTX   COM")
transfer ("fsck.com","FSCK    COM")

rem print "Directory:";directory


print "Base Directory Size:";len(directory)
directory=pade5(directory,4096)	: rem size of 2 directory allocations, but for NVM, Allocation is 2K....
directory=left(directory,4096)
print "Final Directory Size:";len(directory)

printhex (Directory,512)


rem File Name Structure 
rem UU F1 F2 F3 F4 F5 F6 F7 F8 T1 T2 T3 EX S1 S2 RC   .FILENAMETYP....
rem AL AL AL AL AL AL AL AL AL AL AL AL AL AL AL AL   ................
rem thisfile
rem thissize
rem thisalloc
rem thisextent
rem dim shared FILEIN as string		: rem DOS.
rem dim shared fileout as string	: rem 8+3 format.
rem dim shared thisfile as string	: rem Contents of the single opened file. 
rem dim shared filecontent as string	: rem the actual file itself. 
rem dim shared ALLOCATIONS as integer
rem dim shared filesize as integer	: rem
rem dim shared records as integer	: rem How many records have we written to the file?
rem dim shared blocks as integer	: rem How many blocks in the file?
rem dim shared extents as integer	: rem Which extent are we writing?

DISK=DISK+Directory+filecontent


Print "Final disk image size:";len(DISK)


rem printhex (filecontent,len(filecontent))

open "nvm.img" for output as #1
print #1,DISK;
close #1


end