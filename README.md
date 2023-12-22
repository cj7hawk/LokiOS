# LokiOS

HOW TO BUILD: Download FREEBASIC - https://www.freebasic.net/
Put Freebasic in a directory. Copy the contents of this repository to the same directory.
Run MAKE.BAT and it will build all the files. 
You can run the LOKI2.EXE to then run the emulator, and it should build the environment and work.

LokiOS is a Modern version of CP/M written from the CP/M 2.2 API upwards in z80 Assembly - Created in the same way MS-DOS 1.0 was.

LokiOS is a simple CP/M-compatible OS that is small, hardware agnostic, and builds upon CP/M concepts in a new way to maintain the original objectives of CP/M 2.2
It provides a base for anyone needing a z80 based OS that works with CP/M software and calls while being updated and intended to be built upon for modern architectures.

The BIOS and BDOS together sit within a 4K memory space making it ideal to port to low-memory systems and it brings none of the previous disk baggage
  from other versions of CP/M that require physical floppy hardware which is often omitted from modern CP/M designs.

LokiOS has also been optimized for modern storage and memory requirements, without limits and without requiring paging through the ability to support large memory spaces
  in hardware. The code itself however is somewhat skeletal, allowing room for significant customisation. It will connect straight up to Flash and other drives 
  though if you meet the hardware requirements.
  
It is written entirely in z80 assembler, and uses the extended capabilities of the processor, so will not work on 8080/8085 systems. 

It is intended to run any software that would run on CP/M 2.2 and can be ported to most z80 based systems. All source is documented, and the assembler, while BASIC
  and Command Line, can be compiled on modern systems, so no more Windows 11 compatability issues with older assemblers !

The primary differences between LokiOS and CP/M are;
  * LokiOS is designed to run on a minimum z80 system and does not support 8080/8085 based systems.
  * It is Case Insensitive ( CP/M is just CAPS only )
  * The addition of new commands into the CCP -
    # Copy - Copies files in a similar way to DOS copy.
    # Monitor - Built in monitor
  * Makes all Restarts available for the user, and can separate RSTs from Mode 0 Interrupts ( or can merge them )
  * Uses reserved memory from 0040H to 005BH inclusive for Video and Network hooks
  * Inbuilt support for TCP and UDP
  * Supports remote network disk sharing and mounting under TCP and UDP port 280 (z80) or Decimal 640 for Loki Network Protocol. ( Under Construction )
  * Extends network controls to CP/M ( Under Construction )
  * Designed for 1Mb of memory space (Version 1) including mapping boot BIOSes and disk auto boot executable capability.
  * Simple bootstrap if the full OS is built.

But
  * Can be used with JUST the BDOS and BIOS and CCP or can swap between Loki and DRI BDOS or CCP. 

Future iterations intentions to support;
  * Hardware architectures to support large memory systems without requiring paging by accessing all memory space under existing BDOS calls.
  * Hardware architectures to support large-scale Symmetrical Multiprocessor environments.
  * Memory architectures beginning at 256Mb and extending out to any size.
  
Loki-OS in it's basic format is a suitable replacement for CP/M for modern z80 computer systems with backwards compatability to CP/M 2.2, and although it is designed for the Loki and Loki MAD architectures, it is less hardware fixed than you might imagine, and would be easy to rebuild for other architectures.

This repository is designed for PCs or x86 machines, and uses FREEBASIC as the initial language. Download FREEBASIC, copy this respository to the Freebasic directory ( where the FBC.EXE or FBC64.EXE files exist ) and run the MAKE batch file for a clean set of binaries you should be able to trust. You may need to rename FBC_64.EXE or whatever the latest is to FBC.EXE - but it first builds an assembler then it builds the emulation environment, then the support files, then it assembles all of the z80 binaries with the new assembler.  I'll include instructions on that elsewhere. 
