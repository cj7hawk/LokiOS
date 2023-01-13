# LokiOS
Modern version of CP/M written from the CP/M 2.2 API upwards in z80 Assembly - Created in the same way MS-DOS 1.0 was.

LokiOS is a simple CP/M-compatible OS that is small, hardware agnostic, and builds upon CP/M concepts in a new way to maintain the original objectives of CP/M 2.2
It provides a base for anyone needing a z80 based OS that works with CP/M software and calls while being updated and intended to be built upon for modern architectures.

The BIOS and BDOS together sit within a 4K memory space making it ideal to port to low-memory systems and it brings none of the previous disk baggage
  from other versions of CP/M that require physical floppy hardware which is often omitted from modern CP/M designs.

LokiOS has also been optimized for modern storage and memory requirements, without limits and without requiring paging through the ability to support large memory spaces
  in hardware. The code itself however is somewhat skeletal, allowing room for significant customisation. It will connect straight up to Flash and other drives 
  though if you meet the hardware requirements.
  
It is written entirely in z80 assembler, and uses the extended capabilities of the process, so will not work on 8080/8085 systems. 

It is intended to run any software that would run on CP/M 2.2 and can be ported to most z80 based systems. All source is documented, and the assembler, while BASIC
  and Command Line, can be compiled on modern systems, so no more Windows 11 compatability issues with older assemblers !

The primary differences between LokiOS and CP/M are;
  * LokiOS is designed to run on a minimum z80 system and does not support 8080/8085 based systems.
  * It is Case Insensitive ( CP/M is just CAPS only )
  * The addition of new commands into the CCP -
    # Copy - Copies files in a similar way to DOS copy.
    # Monitor - Built in monitor

Future iterations intentions to support;
  * Hardware architectures to support large memory systems without requiring paging by accessing all memory space under existing BDOS calls.
  * Hardware architectures to support large-scale Symmetrical Multiprocessor environments.
  * Memory architectures beginning at 256Mb and extending out to any size.
  
Loki-OS in it's basic format is a suitable replacement for CP/M for modern computer systems with backwards compatability to CP/M 2.2 
