;TITLE ppc eForth

;===============================================================
; 
;		PowerPC eForth 1.02 by C. H. Ting, March, 1997
;		Subroutine threaded code.  Colon lists conatin branch instructions.
;		Top of both the data and return stacks are cached in rTOS and rTOR.
;		To be called from a C caller routine.
;		As many kernel words are coded in assembly for performance.
;
;	0.	Start porting 386 eForth to Power PC.
;	1.	Add rBase point to COLDD, all addresses are relative to it.
;		Addresses on the return stack are absolute and not relocatable.
;		Complications is do$, doVOV and COMPILE.  Abs addresses must be
;		converted to relative addresses to be operated by @ and !.
;	2.	UM/MOD and M/MOD discard the upper half of divident for now.
;	3.	Code pointer, link and name fields are compiled with code.
;		They will be separated out after the system is fully debugged
;	4.	Stacks are squeezed in between 0X80 and 0XFF for debugging.
;	5.	TIBB put at 0X4000 for recursive file loading.
;	6.	Tops of both stacks are cached in rTOS and rTOR.
;	7.	KEY and EMIT temporarily disabled for debugging.
;	8.	Add keyTest1 and keyTest2 to debug KEY and EMIT.
; 		Lift from MPW:Examples:PPCExamples:AExamples:AsmSample:asm.s
;	9.  Add 0 at DEBUG to break into Macsbug.
;	10.	The interpreter is working, 4/17/97.
;	11.	Force compiling names of names containing #, $, and ','.
;	12.	AGAIN replaces call, .  AGAIN and THEN resolve all structures.
;	13.	The compiler is working, 4/20/97.
;	14.	Add file words and INCLUDE.  4/25/97
;		Lower the case for system words.
;		
;       486 eForth 4.03 by C. H. Ting, 1996, July 30, 1996
;       Replace Video ROM at C0000-C7FFF.
;       486 eForth 4.02 by C. H. Ting, 1996, June 28
;       OK.ASM ported from Chuck Moore's HELLO.COM
;       486 EfORTH 4.01 by C. H. Ting, 1996
;       32-bit eForth, protected mode under Chuck Moore's OK
;       4.01 is running from 110000H as logical 0.
;
;       386 eForth 3.01 by C. H. Ting, 1996
;       16-bit protected mode to be used with Chuck Moore's OK.
;
;       8086 eForth 2.02 by C. H. Ting, 1996
;       Fully optimized by recoding most common functions in code.
;               Add doUSE, doVAR, and doCON in code.
;
;       8086 eForth 2.01 by C. H. Ting, 1996
;       To be committed to Boot ROM on a standard PC motherboard.
;       !IO,?RX and TX! access COM1 port directly, 9600-8-1-n.
;       16KB of ROM from FC000 to FFFFF.  CS is set to FC00 upon booting.
;       Stacks and user area just below 5000, physically below 01000.
;       Code dictionary from 5000 up, name dictionary from FFF0 down.
;       Working under DOS, 27may96cht
;               8K ROM starting FE000.
;               Add VFORTH for ROMmability. 30may96cht
;
;       386 eForth 1.1 by C. H. Ting, 1992 with Al Williams' DOS
;               extender.
;
;       Much of the code is derived from the following sources: ;
;       8086 eForth 1.0 by Bill Muench and C. H. Ting, 1990
;       8086 figForth by Thomas Newman, 1981 and Joe smith, 1983
;       aFORTH by John Rible, 1988
;       bFORTH by Bill Muench, 1989
;
;
;===============================================================

#include <stdio.h>
#include <stdlib.h>

;===============================================================
; eforth code body
;===============================================================

	export	.eforth
	export	.keyTest1
	export	.keyTest2
	export	.debug

	import	.fread
	import	.fwrite
	import	_iob
	
	STRING 	ASIS							;straight ASCII strings
	ALIGN	2								;4 byte words
	
	csect	ppcforth

;===============================================================
; register usage	
;===============================================================
					
rTOS:		equ		r31						; Top of data stack
rNOS:		equ		r30						; Next of data stack
rDP:		equ		r29						; Data stack pointer
rLR:		equ		r28						; Shadow of link register
rTOR:		equ		r27						; Top of return stack
rRP:		equ		r26						; Return stack pointer 
rBase:		equ		r25						; Base address for relocation
rUP:		equ		r24						; User area pointer

t1:			equ		r23						; scratch pad registers
t2:			equ		r22						; scratch pad registers
		
		
;; Version control

VER             EQU     1              		;major release version
EXT             EQU     2                	;minor extension

;; Constants

COMPO           EQU     0X40	        	;lexicon compile only bit
IMEDD           EQU     0X80		       	;lexicon immediate bit
MASKK           EQU     0X1F7F7F7F       	;lexicon bit mask

CELLL           EQU     4               	;size of a cell
BASEE           EQU     10               	;default radix
VOCSS           EQU     8                	;depth of vocabulary stack

BKSPP           EQU     8                 	;backspace
LF              EQU     10              	;line feed
CRR             EQU     13               	;carriage return
ERR             EQU     27               	;error escape
TIC             EQU     39               	;tick

;; Memory allocation    0//code>--//--<name//em//up>--<sp//rp//tib

EM              EQU     0X08000        		;top of memory
COLDD           EQU     0X00000          	;cold start vector

US              EQU     64*CELLL         	;user area size in cells
RTS             EQU     64*CELLL          	;return stack/TIB size

TIBB            EQU     0X04000         	;terminal input buffer (TIB)
RPP             EQU     0X00100          	;start of return stack (RP0)
SPP             EQU     0X00170          	;start of data stack (SP0)
UPP             EQU     0X03D00          	;start of user area (UP0)
NAMEE           EQU     UPP-8*CELLL     	;name dictionary
CODEE           EQU     0X00180         	;code dictionary

;; Initialize assembly variables

_LINK   		set	 0                  	;force a null link
_NAME   		set  NAMEE              	;initialize name pointer
_CODE   		set	 CODEE               	;initialize code pointer
_USER  			set  4*CELLL              	;first user variable offset

;; Define assembly macros

	macro
	li32 &rA, &val
		lis		&rA, (((&val) & $FFFF0000) >> 16)
		ori		&rA, &rA, ((&val) & $0000FFFF)
	endm

	macro
	put &stack, &x, &reg
		stw		&reg, (4*(&x))(&stack)
	endm
	
	macro
	get &stack, &x, &reg
		lwz		&reg, (4*(&x))(&stack)
	endm
	
	macro
	push &stack, &reg 
		stwu	&reg, -4(&stack) 
	endm
	
	macro
	pop &stack, &reg
		lwz		&reg, 0(&stack)
		addi	&stack, &stack, 4
	endm
	
	macro
	pushi &stack, &immed 
		li32	TOS, &immed
		stwu	TOS, -4(&stack) 
	endm

	macro
	prologue 
		mflr	r0						; Get link register
		stw		r0, 0x0008(SP)			; Store the link resgister on the stack
		stw		rTOS,-4(SP)
		stw		rNOS,-8(SP)
		stw		rDP,-12(SP)
		stw		rLR,-16(SP)
		stw		rTOR,-20(SP)
		stw		rRP,-24(SP)
		stw		rBase,-28(SP)
		stw		rUP,-32(SP)
		stwu	SP, -spaceToSave(SP)	; skip over the stack frame	
	endm

	macro
	epilogue 
		addic	SP,SP,spaceToSave		; Reset the stack pointer
		lwz		r0,0x8(SP)				; Get the saved link register
		lwz		rTOS,-4(SP)
		lwz		rNOS,-8(SP)
		lwz		rDP,-12(SP)
		lwz		rLR,-16(SP)
		lwz		rTOR,-20(SP)
		lwz		rRP,-24(SP)
		lwz		rBase,-28(SP)
		lwz		rUP,-32(SP)
		mtlr	r0						; Reset the link register
	endm

;       Adjust an address to the next cell boundary.

		MACRO
		.ALIGN
        ALIGN   2                       	;for 32 bit systems
        ENDM

;       Compile a code definition header.

		MACRO   
		HEADER	&LEX,&NAME,&LABEL
_LEN    SET	 	(&LEX & 0X1F)/CELLL      	;string cell count, round down
_CODE   SET	 	$+((_LEN+3)*CELLL)       	;save code pointer
        DC.L    _CODE,_LINK             	;token pointer and link$$$$$$
_LINK   SET		$                        	;link points to a name string
        ENDM

		MACRO   
		.CODE   &LEX,&NAME,&LABEL
		HEADER	&LEX,&NAME,&LABEL
		DC.B	&LEX,&NAME
		ALIGN	2
&LABEL:                                  	;assembly label
        ENDM

;       Compile a colon definition header.

		MACRO   
		.COLON  &LEX,&NAME,&LABEL
		HEADER	&LEX,&NAME,&LABEL
        DC.B    &LEX,&NAME              	;name string
		ALIGN	2
&LABEL:                                   	;assembly label
		push	rRP,rTOR					;push cached TOR to return stack
		mflr	rTOR						;save IP from lr to rTOR
        ENDM

;       Compile a user variable header.

		MACRO   
		.USER   &LEX,&NAME,&LABEL
		HEADER	&LEX,&NAME,&LABEL
        DC.B    &LEX,&NAME              	;name string
		ALIGN	2
&LABEL:                                   	;assembly label
        mflr	rLR
		bl		doUser                    	;followed by doUSER and offset
		dc.l	_USER
_USER   set		_USER+CELLL             	;update user area offset
        ENDM

;       Compile an inline string.

		MACRO   
		.DS      &FUNCT,&STRNG
        bl      &FUNCT                   	;function
_LEN    set		$                        	;save address of count byte
        DC.B    0,&STRNG                 	;count byte and string
_CODE   set		$                       	;save code pointer
		ORG     _LEN                     	;point to count byte
        DC.B    _CODE-_LEN-1           		;set count
		ORG     _CODE                    	;restore code pointer
        .ALIGN  
        ENDM

;       Assemble inline direct threaded code ending.

;		MACRO	
;		.next 
;        blr                           		;return
;        ENDM
		
		
;; Main entry points and COLD start data

		ORG     COLDD                   	;beginning of cold boot

        .ALIGN                        		;align to cell boundary

UZERO:          dcb.l	  4,0               ;reserved
                dc.l      SPP             	;SP0
                dc.l      RPP             	;RP0
                dc.l      QRX             	;'?KEY
                dc.l      TXSTO          	;'EMIT
                dc.l      ACCEP           	;'EXPECT
                dc.l      KTAP             	;'TAP
                dc.l      DROP            	;'ECHO
                dc.l      DOTOK           	;'PROMPT
                dc.l      BASEE           	;BASE
                dc.l      0              	;tmp
                dc.l      0              	;SPAN
                dc.l      0               	;>IN
                dc.l      0               	;#TIB
                dc.l      TIBB           	;TIB
                dc.l      0               	;CSP
                dc.l      INTER           	;'EVAL
                dc.l      NUMBQ         	;'NUMBER
                dc.l      0               	;HLD
                dc.l      0               	;HANDLER
                dc.l      FORTH1        	;CONTEXT pointer
                dcb.l     8,0			 	;vocabulary stack
                dc.l      FORTH1        	;CURRENT pointer
                dc.l      0              	;vocabulary link pointer
                dc.l      CTOP	         	;code dictionary
                dc.l      TIBB-4           	;name dictionary
                dc.l      LASTN          	;LAST
ULAST:

;Stacks are placed here temporarily for ease of debugging
;RP0=0XBC, 16 cells
;SP0=0XFC, 16 cells

		ORG     CODEE                    	;start code dictionary

.debug:
buff:			dc.l	0  					;illegal instruction to trap to Macsbug
.eforth:        prologue
				bl		efor1
efor1:			mflr	rBase
				addi	rBase,rBase,UZERO-efor1
				li		rUP,UZERO
				addi	rDP,rBase,SPP
				addi	rRP,rBase,RPP
				li		rTOS,-1
				li		rNOS,-1
				li		rTOR,-1
				b		COLD
				
.keyTest2:		prologue	
				bl		efor2
efor2:			mflr	rBase
				addi	rBase,rBase,UZERO-efor2
				li		rUP,UZERO
				addi	rDP,rBase,SPP
				addi	rRP,rBase,RPP
				li		rTOS,-1
				li		rNOS,-1
				li		rTOR,-1

efor3:			bl		QRX
				bl		QBRN
				bl		efor3
				bl		DUPP
				bl		TXSTO
				bl		DLIT
				dc.l	0X72
				bl		XORR
				bl		QBRN
				bl		BYE
				bl		efor3

; COLD start moves the following to USER variables.
; MUST BE IN SAME ORDER AS USER VARIABLES.

;; Device dependent I/O

;   BYE         ( -- )
;               Exit eForth.

                .CODE   3,'BYE',BYE
				epilogue	
				blr						; return via the link register

;   ?RX         ( -- c T | F )
;               Return input character and true, or a false if no input.

                .CODE   3,'?rx',QRX
				
				prologue
				li		r3,0
				li		r5,1
				la		r4,buff(rBase)
				bl		.read
				nop
				epilogue
		
				cmpwi	cr0,r3,0
				push	rDP,rTOS
				li		rTOS,0
				blelr
		
				lbz		rTOS,buff(rBase)
				push	rDP,rTOS
				li		rTOS,-1
				blr						; return via the link register

;   TX!         ( c -- )
;               Send character c to the output device.

                .CODE   3,'tx!',TXSTO
				stb		rTOS,buff(rBase)
				pop		rDP,rTOS
		
				prologue
				li		r3,1
				li		r5,1
				la		r4,buff(rBase)
				bl		.write
				nop								; this will be fixed by the linker
				epilogue
		
				blr							; return via the link register

;   !IO         ( -- )
;               Initialize the serial I/O devices.

                .CODE   3,'!io',STOIO
                blr


;; The kernel

;   doLIST      ( a -- )
;               Not needed.  Included in the doColon macro.
;		push	rRP,rTOR					;push cached TOR to return stack
;		mflr	rTOR						;save IP from lr to rTOR
;	Top of return stack is cached in rTOR.  Rest is in memory pointed to by rRP.


;   DOLIT       ( -- w )
;               Push an inline literal.

                .CODE   COMPO+5,'dolit',DLIT
                push	rDP,rTOS
				mflr	rLR
				addi	t1,rLR,4
				mtctr	t1
				lwz		rTOS,0(rLR)
				bctr

;   EXIT        ( -- )
;               Terminate a colon definition.

                .CODE   4,'EXIT',EXIT
                mtlr	rTOR
				pop		rRP,rTOR
                blr

;   EXECUTE     ( ca -- )
;               Execute the word at ca.

                .CODE   7,'EXECUTE',EXEC
				add		rTOS,rTOS,rBase		;convert relative address to absolute
				mtctr	rTOS				;LR still must be preserved
				pop		rDP,rTOS			;EXECUTE is not a GOTO! 4/16/97 cht
				bctr

;   next        ( -- )
;               Run time code for the single index loop.
;               : next ( -- ) \ hilevel model
;                 r> r> dup if 1 - >r @ >r exit then drop cell+ >r ;
;				After next is a bl instruction, not an address.

                .CODE   COMPO+4,'next',DNXT
                addic.	rTOR,rTOR,-1
				mflr	rLR
				addi	t1,rLR,4
				mtctr	t1
				bgelr						;continue looping
				pop		rRP,rTOR
				bctr						;exit loop

;	branch		branch is simply a bl instruction.  Branch is not needed.
;				Colon list is a list of bl instructions.

;   ?branch     ( f -- )
;               Branch if flag is zero.
;				After ?branch is a bl instruction, not an address.

                .CODE   COMPO+7,'?branch',QBRN
				mflr	rLR
				cmpi	CR0,rTOS,0
				addi	t1,rLR,4
				mtctr	t1
				pop		rDP,rTOS
				beqlr
				bctr

;   branch      ( -- )						; Macro according to Ron Hochsprung.
;               Branch to an inline address.

;               MACRO	
;				BRAN	&arg
;                bl		&arg
;                ENDM

;   !           ( w a -- )
;               Pop the data stack to memory.

                .CODE   1,'!',STOR
				pop		rDP, rNOS
				stwx	rNOS,rTOS,rBase
				pop		rDP, rTOS
                blr

;   @           ( a -- w )
;               Push memory location to the data stack.

                .CODE   1,'@',AT
				lwzx	rTOS,rTOS,rBase
                blr

;   C!          ( c b -- )
;               Pop the data stack to byte memory.

                .CODE   2,'C!',CSTR
				pop		rDP, rNOS
				stbx	rNOS,rTOS,rBase
				pop		rDP, rTOS
                blr

;   C@          ( b -- c )
;               Push byte memory location to the data stack.

                .CODE   2,'C@',CAT
				lbzx	rTOS,rTOS,rBase
                blr

;   RP@         ( -- a )
;               Push the current RP to the data stack.

                .CODE   3,'rp@',RPAT
                push	rDP,rTOS
				subf	rTOS,rBase,rRP
                blr

;   RP!         ( a -- )
;               Set the return stack pointer.

                .CODE   COMPO+3,'rp!',RPST
                add		rRP,rTOS,rBase
				pop		rDP,rTOS
				li		rTOR, 0
                blr

;   R>          ( -- w )
;               Pop the return stack to the data stack.

                .CODE   2,'R>',RFRM
				push	rDP, rTOS
				mr		rTOS, rTOR
				pop		rRP, rTOR
                blr

;   R@          ( -- w )
;               Copy top of return stack to the data stack.

                .CODE   2,'R@',RAT
				push	rDP, rTOS
				mr		rTOS, rTOR
                blr

;   >R          ( w -- )
;               Push the data stack to the return stack.

                .CODE   COMPO+2,'>R',TOR
				push	rRP, rTOR
				mr		rTOR, rTOS
				pop		rDP, rTOS
                blr

;   SP@         ( -- a )
;               Push the current data stack pointer.

                .CODE   3,'sp@',SPAT
				push	rDP, rTOS
				subf	rTOS,rBase,rDP
                blr

;   SP!         ( a -- )
;               Set the data stack pointer.

                .CODE   3,'sp!',SPST
				add		rDP,rTOS,rBase
				li		rTOS, 0
                blr

;   DROP        ( w -- )
;               Discard top stack item.

                .CODE   4,'DROP',DROP
				pop		rDP, rTOS
                blr

;   DUP         ( w -- w w )
;               Duplicate the top stack item.

                .CODE   3,'DUP',DUPP
				push	rDP, rTOS
                blr

;   SWAP        ( w1 w2 -- w2 w1 )
;               Exchange top two stack items.

                .CODE   4,'SWAP',SWAP
				get		rDP, 0, rNOS
				put		rDP, 0, rTOS
				mr		rTOS, rNOS
                blr

;   OVER        ( w1 w2 -- w1 w2 w1 )
;               Copy second stack item to top.

                .CODE   4,'OVER',OVER
				push	rDP, rTOS
				get		rDP, 1, rTOS
                blr

;   0<          ( n -- t )
;               Return true if n is negative.

                .CODE   2,'0<',ZLSS
				cmpwi	cr0, rTOS, 0
				li		rTOS,0
				bgelr
				li		rTOS,-1
                blr

;   AND         ( w w -- w )
;               Bitwise AND.

                .CODE   3,'AND',ANDD
				pop		rDP, rNOS
				and		rTOS, rTOS, rNOS
                blr

;   OR          ( w w -- w )
;               Bitwise inclusive OR.

                .CODE   2,'OR',ORR
				pop		rDP, rNOS
				or		rTOS, rTOS, rNOS
                blr

;   XOR         ( w w -- w )
;               Bitwise exclusive OR.

                .CODE   3,'XOR',XORR
				pop		rDP, rNOS
				xor		rTOS, rTOS, rNOS
                blr

;   UM+         ( w w -- w cy )
;               Add two numbers, return the sum and carry flag.

                .CODE   3,'um+',UPLUS
				pop		rDP, rNOS
				addc	rNOS, rNOS, rTOS
				adde	rTOS,0,0
                blr

;; System and user variables

;   doVAR       ( -- a )
;               Run time routine for VARIABLE and CREATE.
doVar:			mtctr	rLR
				push	rDP,rTOS
				mflr	rTOS
				subf	rTOS,rBase,rTOS
				bctr

;   doCON       ( -- n )
;               Run time routine for CONSTANT and VALUE.
doCon:			mtctr	rLR
				mflr	t1
				push	rDP,rTOS
				lwz		rTOS,0(t1)
				bctr

;   UP          ( -- a )
;               Pointer to the user area.

                .CODE  2,'UP',UP
                push	rDP,rTOS
				mr		rTOS,rUP
				blr

;   doUSER      ( -- a )
;               Run time routine for user variables.

doUser:			mtctr	rLR
				mflr	t1
				lwz		t1,0(t1)
				push	rDP, rTOS
				add		rTOS,t1,rUP
				bctr

;   SP0         ( -- a )
;               Pointer to bottom of the data stack.

                .USER   3,'sp0',SZER

;   RP0         ( -- a )
;               Pointer to bottom of the return stack.

                .USER   3,'rp0',RZER

;   '?KEY       ( -- a )
;               Execution vector of ?KEY.

                .USER   5,'''?key',TQKY

;   'EMIT       ( -- a )
;               Execution vector of EMIT.

                .USER   5,'''emit',TEMIT

;   'EXPECT     ( -- a )
;               Execution vector of EXPECT.

                .USER   7,'''expect',TEXPE

;   'TAP        ( -- a )
;               Execution vector of TAP.

                .USER   4,'''tap',TTAP

;   'ECHO       ( -- a )
;               Execution vector of ECHO.

                .USER   5,'''echo',TECH

;   'PROMPT     ( -- a )
;               Execution vector of PROMPT.

                .USER   7,'''prompt',TPRM

;   BASE        ( -- a )
;               Storage of the radix base for numeric I/O.

                .USER   4,'BASE',BASE

;   tmp         ( -- a )
;               A temporary storage location used in parse and find.

                .USER   COMPO+3,'tmp',TEMP

;   SPAN        ( -- a )
;               Hold character count received by EXPECT.

                .USER   4,'span',SPAN

;   >IN         ( -- a )
;               Hold the character pointer while parsing input stream.

                .USER   3,'>in',INN

;   #TIB        ( -- a )
;               Hold the current count and address of the terminal input buffer.

                HEADER  4,,NTIB
				DC.B	4,$23,$54,$49,$42
NTIB:			mflr	rLR
				bl		doUser                    	;followed by doUSER and offset
				dc.l	_USER
_USER   		set		_USER+CELLL             	;update user area offset

_USER 			set  _USER+CELLL

;   CSP         ( -- a )
;               Hold the stack pointer for error checking.

                .USER   3,'csp',CSP

;   'EVAL       ( -- a )
;               Execution vector of EVAL.

                .USER   5,'''eval',TEVAL

;   'NUMBER     ( -- a )
;               Execution vector of NUMBER?.

                .USER   7,'''number',TNUMB

;   HLD         ( -- a )
;               Hold a pointer in building a numeric output string.

                .USER   3,'hld',HLD

;   HANDLER     ( -- a )
;               Hold the return stack pointer for error handling.

                .USER   7,'handler',HNDL

;   CONTEXT     ( -- a )
;               A area to specify vocabulary search order.

                .USER   7,'context',CTXT
_USER 			set		 _USER+VOCSS*CELLL	;vocabulary stack

;   CURRENT     ( -- a )
;               Point to the vocabulary to be extended.

                .USER   7,'current',CRNT
_USER 			set	 _USER+CELLL          	;vocabulary link pointer

;   CP          ( -- a )
;               Point to the top of the code dictionary.

                .USER   2,'cp',CP

;   NP          ( -- a )
;               Point to the bottom of the name dictionary.

                .USER   2,'np',NP

;   LAST        ( -- a )
;               Point to the last name in the name dictionary.

                .USER   4,'last',LAST

;; Common functions

;   doVOC       ( -- )
;               Run time action of VOCABULARY's.

                .COLON  COMPO+5,'dovoc',DVOC
                bl      RFRM
				subf	rTOS,rBase,rTOS		;convert absolute addr to relative
				bl		CTXT
				bl		STOR
				bl		EXIT

;   FORTH       ( -- )
;               Make FORTH the context vocabulary.

                .COLON  5,'FORTH',FRTH
                bl      DVOC
FORTH1:         dc.l    LASTN           	;vocabulary head pointer
                dc.l      0               	;vocabulary link pointer

;   ?DUP        ( w -- w w | 0 )
;               Dup tos if its is not zero.

                .CODE   4,'?DUP',QDUP
				cmpi	cr0, rTOS, 0
				beqlr
				push	rDP, rTOS
                blr

;   ROT         ( w1 w2 w3 -- w2 w3 w1 )
;               Rot 3rd item to top.

                .CODE   3,'ROT',ROT
				get		rDP, 0, rNOS
				put		rDP, 0, rTOS
				get		rDP, 1, rTOS
				put		rDP, 1, rNOS
                blr

;   2DROP       ( w w -- )
;               Discard two items on stack.

                .CODE   5,'2DROP',DDRP
				pop		rDP, rTOS
				pop		rDP, rTOS
                blr

;   2DUP        ( w1 w2 -- w1 w2 w1 w2 )
;               Duplicate top two items.

                .CODE   4,'2DUP',DDUP
				get		rDP, 0, rNOS
				push	rDP, rTOS
				push	rDP, rNOS
                blr

;   +           ( w w -- sum )
;               Add top two items.

                .CODE   1,'+',PLUS
				pop		rDP, rNOS
				add		rTOS, rTOS, rNOS
                blr

;   D+          ( d d -- d )
;               Double addition.
;
                .CODE   2,'D+',DPLS
				pop		rDP,rNOS
				get		rDP,1,t2
				pop		rDP,t1
				addc	rNOS,rNOS,t2
				adde	rTOS,rTOS,t1
				put		rDP,0,rNOS
				blr

;   NOT         ( w -- w )
;               One's complement of tos.

                .CODE   3,'NOT',INVR
				li		rNOS,-1
				xor		rTOS,rTOS,rNOS
                blr

;   NEGATE      ( n -- -n )
;               Two's complement of tos.

                .CODE   6,'NEGATE',NEGAT
				neg		rTOS,rTOS
                blr

;   DNEGATE     ( d -- -d )
;               Two's complement of top double.

                .CODE   7,'DNEGATE',DNEGA
				pop		rDP,rNOS
				li		t1,-1
				xor		rTOS,rTOS,t1
				xor		rNOS,rNOS,t1
				addic	rNOS,rNOS,1
				adde	rTOS,rTOS,0
				push	rDP,rNOS
                blr

;   -           ( n1 n2 -- n1-n2 )
;               Subtraction.

                .CODE   1,'-',SUBB
				pop		rDP, rNOS
				subf	rTOS, rTOS, rNOS
                blr

;   ABS         ( n -- n )
;               Return the absolute value of n.

                .CODE   3,'ABS',ABSS
				cmplwi	cr0,rTOS, 0
				bgelr
				neg		rTOS, rTOS
                blr

Odd2			equ		0x3d

;   =           ( w w -- t )
;               Return true if top two are equal.

                .CODE   1,Odd2,EQUL
 				pop		rDP, rNOS
 				cmpw	cr0, rNOS, rTOS
				li		rTOS,-1
 				beqlr
				li		rTOS, 0
                blr

;   U<          ( u u -- t )
;               Unsigned compare of top two items.

                .CODE   2,'U<',ULSS
				pop		rDP, rNOS
				cmplw	cr0, rNOS, rTOS
				li		rTOS,-1
				bltlr
				li	rTOS, 0
                blr

;   <           ( n1 n2 -- t )
;               Signed compare of top two items.

                .CODE   1,'<',LESS
				pop		rDP, rNOS
				cmpw	cr0, rNOS, rTOS
				li		rTOS,-1
				bltlr
				li		rTOS, 0
                blr

;   MAX         ( n n -- n )
;               Return the greater of two top stack items.

                .CODE   3,'MAX',MAX
				pop		rDP, rNOS
				cmp		cr0, rTOS, rNOS
				nop
				bgtlr
				mr		rTOS, rNOS
                blr

;   MIN         ( n n -- n )
;               Return the smaller of top two stack items.

                .CODE   3,'MIN',MIN
				pop		rDP, rNOS
				cmp		cr0, rTOS, rNOS
				nop
				bltlr
				mr		rTOS, rNOS
                blr

;   WITHIN      ( u ul uh -- t )
;               Return true if u is within the range of ul and uh.

                .CODE   6,'WITHIN',WITHI
				pop		rDP,rNOS
				pop		rDP,t1
				cmpl	cr0,t1,rTOS
				li		rTOS,0
				bgtlr
				cmpl	cr0,t1,rNOS
				nop
				bltlr
				li		rTOS,-1
				blr

;; Divide

;   UM/MOD      ( udl udh u -- ur uq ) ;discard udh for the moment
;               Unsigned divide of a double by a single. Return mod and quotient.

                .CODE   6,'UM/MOD',UMMD
				pop		rDP,rNOS
				get		rDP, 0, rNOS
				divwu	t1, rNOS, rTOS
				mullw	t2, t1, rTOS
				subf	rNOS, t2, rNOS
				put		rDP, 0, rNOS
				mr		rTOS, t1
                blr

;   M/MOD       ( d n -- r q )				;discard ud for the moment
;               Signed floored divide of double by single. Return mod and quotient.
                .CODE  5,'M/MOD',MSMD
				pop		rDP,rNOS
				get		rDP, 0, rNOS
				divw	t1, rNOS, rTOS
				mullw	t2, t1, rTOS
				subf	rNOS, t2, rNOS
				put		rDP, 0, rNOS
				mr		rTOS, t1
				blr

;   /MOD        ( n n -- r q )
;               Signed divide. Return mod and quotient.

                .CODE   4,'/MOD',SLMOD
				get		rDP, 0, rNOS
				divw	t1, rNOS, rTOS
				mullw	t2, t1, rTOS
				subf	rNOS, t2, rNOS
				put		rDP, 0, rNOS
				mr		rTOS, t1
                blr

;   MOD         ( n n -- r )
;               Signed divide. Return mod only.

                .CODE  3,'MOD',MODD
				pop		rDP, rNOS
				divw	t1, rNOS, rTOS
				mullw	t2, t1, rTOS
				subf	rTOS, t2, rNOS
				blr

;   /           ( n n -- q )
;               Signed divide. Return quotient only.

                .CODE  1,'/',SLASH
				pop		rDP, rNOS
				divw	rTOS, rNOS, rTOS
				blr

;; Multiply

;   UM*         ( u u -- ud )
;               Unsigned multiply. Return double product.

                .CODE   3,'UM*',UMSTA
				pop		rDP,rNOS
				mullw	t1,rNOS,rTOS
				push	rDP,t1
				mulhwu	rTOS,rTOS,rNOS
                blr

;   *           ( n n -- n )
;               Signed multiply. Return single product.

                .CODE   1,'*',STAR
				pop		rDP, rNOS
				mullw	rTOS, rTOS, rNOS
                blr

;   M*          ( n n -- d )
;               Signed multiply. Return double product.

                .CODE   2,'M*',MSTR
				pop		rDP,rNOS
				mullw	t1,rNOS,rTOS
				push	rDP,t1
				mulhw	rTOS,rTOS,rNOS
                blr

;   */MOD       ( n1 n2 n3 -- r q )
;               Multiply n1 and n2, then divide by n3. Return mod and quotient.

                .COLON  5,'*/MOD',SSMOD
                bl      TOR
				bl		MSTR
				bl		RFRM
				bl		MSMD
				bl		EXIT

;   */          ( n1 n2 n3 -- q )
;               Multiply n1 by n2, then divide by n3. Return quotient only.

                .CODE  2,'*/',STASL
				pop		rDP, rNOS
				pop		rDP, t1
				mullw	rNOS, rNOS, t1
				divw	rTOS, rNOS, rTOS
				blr

;; Miscellaneous

;   CELL+       ( a -- a )
;               Add cell size in byte to address.

                .CODE   5,'CELL+',CLLP
				addi	rTOS,rTOS,4
                blr

;   CELL-       ( a -- a )
;               Subtract cell size in byte from address.

                .CODE   5,'CELL-',CLLM
				addi	rTOS,rTOS,-4
                blr

;   CELLS       ( n -- n )
;               Multiply tos by cell size in bytes.

                .CODE   5,'CELLS',CLLS
				slwi	rTOS,rTOS,2
                blr

;   ALIGNED     ( b -- a )
;               Align address to the cell boundary.

                .CODE   7,'ALIGNED',ALGD
				addi	rTOS, rTOS, 3
				li		t1, -4
				and		rTOS, rTOS, t1
                blr

;   BL          ( -- 32 )
;               Return 32, the blank character.

                .COLON  2,'BL',BLNK
                bl      DLIT
				dc.l	32
				bl		EXIT

;   >CHAR       ( c -- c )
;               Filter non-printing characters.

                .COLON  5,'>char',TCHR
                bl      DLIT
				dc.l	0X7F
				bl		ANDD
				bl		DUPP    			;mask msb
                bl      BLNK
				bl		DLIT
				dc.l	127
				bl		WITHI   			;check for printable
                bl      INVR
				bl		QBRN
				bl		TCHA1
                bl      DROP
				bl		DLIT
				dc.l	0X2E				;'.'		;replace non-printables
TCHA1:          bl      EXIT

;   DEPTH       ( -- n )
;               Return the depth of the data stack.

                .COLON  5,'DEPTH',DPTH
                bl      SPAT
				bl		SZER
				bl		AT
				bl		SWAP
				bl		SUBB
				bl		CLLM				;forgot TOS is cached 4/17/97
                bl      DLIT
				dc.l	CELLL
				bl		SLASH
				bl		EXIT

;   PICK        ( ... +n -- ... w )
;               Copy the nth stack item to tos.

                .CODE   4,'PICK',PICK
				slwi	rTOS,rTOS,2
				lwzx	rTOS,rDP,rTOS
                blr

;; Memory access

;   +!          ( n a -- )
;               Add n to the contents at address a.

                .CODE   2,'+!',PSTR
				pop		rDP, rNOS
				lwzx	t1,rTOS,rBase
				add		t1, t1, rNOS
				stwx	t1,rTOS,rBase
				pop		rDP, rTOS
                blr

;   2!          ( d a -- )
;               Store the double integer to address a.

                .CODE   2,'2!',DSTR
				pop		rDP,rNOS
				pop		rDP,t1
				stwx	rNOS,rTOS,rBase
				addi	rTOS,rTOS,4
				stwx	t1,rTOS,rBase
				pop		rDP,rTOS
                blr

;   2@          ( a -- d )
;               Fetch double integer from address a.

                .CODE   2,'2@',DAT
				addi	t1,rTOS,4
				lwzx	rNOS,t1,rBase
				push	rDP,rNOS
				lwzx	rTOS,rTOS,rBase
                blr

;   COUNT       ( b -- b +n )
;               Return count byte of a string and add 1 to byte address.

                .CODE   5,'COUNT',COUN
				addi	rNOS, rTOS, 1
				push	rDP, rNOS
				lbzx	rTOS,rTOS,rBase
                blr

;   HERE        ( -- a )
;               Return the top of the code dictionary.

                .COLON  4,'HERE',HERE
                bl      CP
				bl		AT
				bl		EXIT

;   PAD         ( -- a )
;               Return the address of a temporary buffer.

                .COLON  3,'PAD',PAD
                bl      HERE
				bl		DLIT
				dc.l	80
				bl		PLUS
				bl		EXIT

;   TIB         ( -- a )
;               Return the address of the terminal input buffer.

                .COLON  3,'TIB',TIB
                bl      NTIB
				bl		CLLP
				bl		AT
				bl		EXIT

;   @EXECUTE    ( a -- )
;               Execute vector stored in address a.

                .CODE   8,'@execute',ATEX
				lwzx	rTOS,rTOS,rBase
				cmpi	cr0,rTOS,0
				add		rTOS,rTOS,rBase		; need abs address to branch to
                mtctr	rTOS
				pop		rDP,rTOS
				beqlr
				bctr

;   CMOVE       ( b1 b2 u -- )
;               Copy u bytes from b1 to b2.

                .CODE   5,'CMOVE',CMOV
				cmpwi	cr0,rTOS,0
				mtctr	rTOS
				pop		rDP, rTOS			; dest-addr
				pop		rDP, rNOS			; src-addr
				beq		move1
				add		rTOS,rBase,rTOS		; rel to abs address
				add		rNOS,rBase,rNOS
				subi	rTOS, rTOS, 1
				subi	rNOS, rNOS, 1
move0:			lbzu	t1, 1(rNOS)
				stbu	t1, 1(rTOS)
				bdnz	move0
move1:			pop		rDP, rTOS
                blr

;   FILL        ( b u c -- )
;               Fill u bytes of character c to area beginning at b.

                .CODE   4,'FILL',FILL
				pop		rDP, rNOS			; len
				mtctr	rNOS
				cmpwi	cr0,rNOS,0
				pop		rDP, rNOS			; addr
				add		rNOS,rBase,rNOS		; rel to abs address
				subi	rNOS, rNOS, 1
				beq		fill1
fill0:			stbu	rTOS, 1(rNOS)
				bdnz	fill0
fill1:			pop		rDP,rTOS
                blr

;   -TRAILING   ( b u -- b u )
;               Adjust the count to eliminate trailing white space.

                .COLON  9,'-TRAILING',DTRA
                bl      TOR
                bl		DTRA2
DTRA1:          bl      BLNK
				bl		OVER
				bl		RAT
				bl		PLUS
				bl		CAT
				bl		LESS
                bl      QBRN
				bl		DTRA2
                bl      RFRM
				bl		DLIT
				dc.l	1
				bl		PLUS
				bl		EXIT ;adjusted count
DTRA2:          bl      DNXT
				bl		DTRA1
                bl      DLIT
				dc.l	0
				bl		EXIT            	;count=0

;   PACK$       ( b u a -- a )
;               Build a counted string with u characters from b. Null fill.

                .COLON  5,'pack$',PCKS
                bl      ALGD
				bl		DUPP
				bl		TOR          		;strings only on cell boundary
                bl      OVER
				bl		DUPP
				bl		DLIT
				dc.l	0
                bl      DLIT
				dc.l	CELLL
				bl		UMMD
				bl		DROP  				;count mod cell
                bl      SUBB
				bl		OVER
				bl		PLUS
                bl      DLIT
				dc.l	0
				bl		SWAP
				bl		STOR      			;null fill cell
                bl      DDUP
				bl		CSTR
				bl		DLIT
				dc.l		1
				bl		PLUS 				;save count
                bl      SWAP
				bl		CMOV
				bl		RFRM
				bl		EXIT   				;move string

;; Numeric output, single precision

;   DIGIT       ( u -- c )
;               Convert digit u to a character.

                .COLON  5,'digit',DIGT
                bl      DLIT
				dc.l	9
				bl		OVER
				bl		LESS
                bl      DLIT
				dc.l	7
				bl		ANDD
				bl		PLUS
                bl      DLIT
				dc.l	0X30				;'0'
				bl		PLUS
				bl		EXIT

;   EXTRACT     ( n base -- n c )
;               Extract the least significant digit from n.

                .COLON  7,'extract',EXTR
                bl      DLIT
				dc.l	0
				bl		SWAP
				bl		UMMD
                bl      SWAP
				bl		DIGT
				bl		EXIT

;   <#          ( -- )
;               Initiate the numeric output process.

                HEADER  2,,BDGS
				DC.B	2,$3c,$23
BDGS:			push	rRP,rTOR					;push cached TOR to return stack
				mflr	rTOR						;save IP from lr to rTOR

                bl      PAD
				bl		HLD
				bl		STOR
				bl		EXIT

;   HOLD        ( c -- )
;               Insert a character into the numeric output string.

                .COLON  4,'HOLD',HOLD
                bl      HLD
				bl		AT
				bl		DLIT
				DC.L	1
				bl		SUBB
                bl      DUPP
				bl		HLD
				bl		STOR
				bl		CSTR
				bl		EXIT
				
Odd4			equ		$23

;   #           ( u -- u )
;               Extract one digit from u and append the digit to output string.

                .COLON  1,Odd4,DIG
                bl      BASE
				bl		AT
				bl		EXTR
				bl		HOLD
				bl		EXIT

;   #S          ( u -- 0 )
;               Convert u until all digits are added to the output string.

                HEADER  2,,DIGS
				DC.B	2,$23,$53
DIGS:			push	rRP,rTOR					;push cached TOR to return stack
				mflr	rTOR						;save IP from lr to rTOR

DIGS1:          bl      DIG
				bl		DUPP
                bl      QBRN
				bl		DIGS2
                bl	DIGS1
DIGS2:          bl      EXIT

;   SIGN        ( n -- )
;               Add a minus sign to the numeric output string.

                .COLON  4,'SIGN',SIGN
                bl      ZLSS
                bl      QBRN
				bl		SIGN1
                bl      DLIT
				dc.l	0X2D				;'-'
				bl		HOLD
SIGN1:          bl      EXIT

;   #>          ( w -- b u )
;               Prepare the output string to be TYPE'd.

                HEADER  2,,EDGS
				DC.B	2,$23,$3e
EDGS:			push	rRP,rTOR					;push cached TOR to return stack
				mflr	rTOR						;save IP from lr to rTOR

                bl      DROP
				bl		HLD
				bl		AT
                bl      PAD
				bl		OVER
				bl		SUBB
				bl		EXIT

;   str         ( n -- b u )
;               Convert a signed integer to a numeric string.

                .COLON  3,'str',STRR
                bl      DUPP
				bl		TOR
				bl		ABSS
                bl      BDGS
				bl		DIGS
				bl		RFRM
                bl      SIGN
				bl		EDGS
				bl		EXIT

;   HEX         ( -- )
;               Use radix 16 as base for numeric conversions.

                .COLON  3,'HEX',HEX
                bl      DLIT
				DC.L	16
				bl		BASE
				bl		STOR
				bl		EXIT

;   DECIMAL     ( -- )
;               Use radix 10 as base for numeric conversions.

                .COLON  7,'DECIMAL',DECIM
                bl      DLIT
				DC.L	10
				bl		BASE
				bl		STOR
				bl		EXIT

;; Numeric input, single precision

;   DIGIT?      ( c base -- u t )
;               Convert a character to its numeric value. A flag indicates success.

                .COLON  6,'digit?',DGTQ
                bl      TOR
				bl		DLIT
				dc.l	0X30				;'0'
				bl		SUBB
                bl      DLIT
				dc.l	9
				bl		OVER
				bl		LESS
                bl      QBRN
				bl		DGTQ1
                bl      DLIT
				dc.l	7
				bl		SUBB
                bl      DUPP
				bl		DLIT
				dc.l	10
				bl		LESS
				bl		ORR
DGTQ1:          bl      DUPP
				bl		RFRM
				bl		ULSS
				bl		EXIT

;   NUMBER?     ( a -- n T | a F )
;               Convert a number string to integer. Push a flag on tos.

                .COLON  7,'number?',NUMBQ
                bl      BASE
				bl		AT
				bl		TOR
				bl		DLIT
				dc.l	0
				bl		OVER
				bl		COUN
                bl      OVER
				bl		CAT
				bl		DLIT
				dc.l	0X24				;'$'
				bl		EQUL
                bl      QBRN
				bl		NUMQ1
                bl      HEX
				bl		SWAP
				bl		DLIT
				dc.l	1
				bl		PLUS
                bl      SWAP
				bl		DLIT
				dc.l		1
				bl		SUBB
NUMQ1:          bl      OVER
				bl		CAT
				bl		DLIT
				dc.l	0X2D				;'-'
				bl		EQUL
				bl		TOR
                bl      SWAP
				bl		RAT
				bl		SUBB
				bl		SWAP
				bl		RAT
				bl		PLUS
				bl		QDUP
                bl      QBRN
				bl		NUMQ6
                bl      DLIT
				dc.l		1
				bl		SUBB
				bl		TOR
NUMQ2:          bl      DUPP
				bl		TOR
				bl		CAT
				bl		BASE
				bl		AT
				bl		DGTQ
                bl      QBRN
				bl		NUMQ4
                bl      SWAP
				bl		BASE
				bl		AT
				bl		STAR
				bl		PLUS
				bl		RFRM
                bl      DLIT
				dc.l		1
				bl		PLUS
                bl      DNXT
				bl		NUMQ2
                bl      RAT
				bl		SWAP
				bl		DROP
                bl      QBRN
				bl		NUMQ3
                bl      NEGAT
NUMQ3:          bl      SWAP
                bl	NUMQ5
NUMQ4:          bl      RFRM
				bl		RFRM
				bl		DDRP
				bl		DDRP
				bl		DLIT
				dc.l		0
NUMQ5:          bl      DUPP
NUMQ6:          bl      RFRM
				bl		DDRP
                bl      RFRM
				bl		BASE
				bl		STOR
				bl		EXIT

;; Basic I/O

;   ?KEY        ( -- c T | F )
;               Return input character and true, or a false if no input.

                .COLON  4,'?KEY',QKEY
                bl      TQKY
				bl		ATEX
				bl		EXIT

;   KEY         ( -- c )
;               Wait for and return an input character.

                .COLON  3,'KEY',KEY
KEY1:           bl      QKEY
                bl      QBRN
				bl		KEY1
                bl      EXIT

;   EMIT        ( c -- )
;               Send a character to the output device.

                .COLON  4,'EMIT',EMIT
                bl      TEMIT
				bl		ATEX
				bl		EXIT

;   PACE        ( -- )
;               Send a pace character for the file downloading process.

                .COLON  4,'pace',PACE
                bl      DLIT
				dc.l		11
				bl		EMIT
				bl		EXIT

;   SPAC       ( -- )
;               Send the blank character to the output device.

                .COLON  5,'SPACE',SPAC
                bl      BLNK
				bl		EMIT
				bl		EXIT

;   SPACES      ( +n -- )
;               Send n spaces to the output device.

                .COLON  6,'SPACES',SPCS
                bl      DLIT
				dc.l		0
				bl		MAX
				bl		TOR
                bl	CHAR2
CHAR1:          bl      SPAC
CHAR2:          bl      DNXT
				bl		CHAR1
                bl      EXIT

;   TYPE        ( b u -- )
;               Output u characters from b.

                .COLON  4,'TYPE',TYP
                bl      TOR
                bl	TYPE2
TYPE1:          bl      DUPP
				bl		CAT
				bl		EMIT
                bl      DLIT
				dc.l		1
				bl		PLUS
TYPE2:          bl      DNXT
				bl		TYPE1
                bl      DROP
				bl		EXIT

;   CR          ( -- )
;               Output a carriage return and a line feed.

                .COLON  2,'CR',CR
                bl      DLIT
				dc.l		CRR
				bl		EMIT
                bl      DLIT
				dc.l		LF
				bl		EMIT
				bl		EXIT

;   do$         ( -- a )
;               Return the address of a compiled string.

                .COLON  COMPO+3,'do$',DOSTR
                bl      RFRM
				bl		RAT
				subf	rTOS,rBase,rTOS		;convert absolute addr to relative
				bl		DUPP				;one copy must be returned as a
				bl		COUN
				bl		RFRM
				bl		PLUS
				addi	rTOS,rTOS,1			;go to the end of string
                bl      ALGD				;absolute addr after string
				bl		TOR
				bl		DROP				;discard counted address
				bl		SWAP
				bl		TOR
				bl		EXIT

;   $"|         ( -- a )
;               Run time routine compiled by $". Return address of a compiled string.

                .COLON  COMPO+3,'$"|',STQP
                bl      DOSTR
				bl		EXIT              	;force a call to do$

;   ."|         ( -- )
;               Run time routine of ." . Output a compiled string.

                .COLON  COMPO+3,'."|',DTQP
                bl      DOSTR
				bl		COUN
				bl		TYP
				bl		EXIT

;   .R          ( n +n -- )
;               Display an integer in a field of n columns, right justified.

                .COLON  2,'.R',DOTR
                bl      TOR
				bl		STRR
				bl		RFRM
				bl		OVER
				bl		SUBB
                bl      SPCS
				bl		TYP
				bl		EXIT

;   U.R         ( u +n -- )
;               Display an unsigned integer in n column, right justified.

                .COLON  3,'U.R',UDOTR
                bl      TOR
				bl		BDGS
				bl		DIGS
				bl		EDGS
                bl      RFRM
				bl		OVER
				bl		SUBB
                bl      SPCS
				bl		TYP
				bl		EXIT

;   U.          ( u -- )
;               Display an unsigned integer in free format.

                .COLON  2,'U.',UDOT
                bl      BDGS
				bl		DIGS
				bl		EDGS
                bl      SPAC
				bl		TYP
				bl		EXIT

;   .           ( w -- )
;               Display an integer in free format, preceeded by a space.

                .COLON  1,'.',DOT
                bl      BASE
				bl		AT
				bl		DLIT
				dc.l		10
				bl		XORR   				;?decimal
                bl      QBRN
				bl		DOT1
                bl      UDOT
				bl		EXIT               	;no, display unsigned
DOT1:           bl      STRR
				bl		SPAC
				bl		TYP
				bl		EXIT    			;yes, display signed

;   ?           ( a -- )
;               Display the contents in a memory cell.

                .COLON  1,'?',QUEST
                bl      AT
				bl		DOT
				bl		EXIT

;; Parsing

;   parse       ( b u c -- b u delta ; <string> )
;               Scan string delimited by c. Return found string and its offset.

                .COLON  5,'parse',PRS
                bl      TEMP
				bl		STOR
				bl		OVER
				bl		TOR
				bl		DUPP
                bl      QBRN
				bl		PARS8
                bl      DLIT
				dc.l		1
				bl		SUBB
				bl		TEMP
				bl		AT
				bl		BLNK
				bl		EQUL
                bl      QBRN
				bl		PARS3
                bl      TOR
PARS1:          bl      BLNK
				bl		OVER
				bl		CAT          		;skip leading blanks ONLY
                bl      SUBB
				bl		ZLSS
				bl		INVR
                bl      QBRN
				bl		PARS2
                bl      DLIT
				dc.l		1
				bl		PLUS
                bl      DNXT
				bl		PARS1
                bl      RFRM
				bl		DROP
				bl		DLIT
				dc.l		0
				bl		DUPP
				bl		EXIT
PARS2:          bl      RFRM
PARS3:          bl      OVER
				bl		SWAP
                bl      TOR
PARS4:          bl      TEMP
				bl		AT
				bl		OVER
				bl		CAT
				bl		SUBB   				;scan for delimiter
                bl      TEMP
				bl		AT
				bl		BLNK
				bl		EQUL
                bl      QBRN
				bl		PARS5
                bl      ZLSS
PARS5:          bl      QBRN
				bl		PARS6
                bl      DLIT
				dc.l		1
				bl		PLUS
                bl      DNXT
				bl		PARS4
                bl      DUPP
				bl		TOR
                bl	PARS7
PARS6:          bl      RFRM
				bl		DROP
				bl		DUPP
                bl      DLIT
				dc.l		1
				bl		PLUS
				bl		TOR
PARS7:          bl      OVER
				bl		SUBB
                bl      RFRM
				bl		RFRM
				bl		SUBB
				bl		EXIT
PARS8:          bl      OVER
				bl		RFRM
				bl		SUBB
				bl		EXIT

;   PARSE       ( c -- b u ; <string> )
;               Scan input stream and return counted string delimited by c.

                .COLON  5,'PARSE',PARS
                bl      TOR
				bl		TIB
				bl		INN
				bl		AT
				bl		PLUS     			;current input buffer pointer
                bl      NTIB
				bl		AT
				bl		INN
				bl		AT
				bl		SUBB     			;remaining count
                bl      RFRM
				bl		PRS
				bl		INN
				bl		PSTR
				bl		EXIT

;   .(          ( -- )
;               Output following string up to next ) .

                .COLON  IMEDD+2,'.(',DOTPR
                bl      DLIT
				dc.l	0X29				;')'
				bl		PARS
				bl		TYP
				bl		EXIT

;   (           ( -- )
;               Ignore following string up to next ) . A comment.

                .COLON  IMEDD+1,'(',PAREN
                bl      DLIT
				dc.l	0X29				;')'
				bl		PARS
				bl		DDRP
				bl		EXIT

Odd7:			equ		0x5c

;   \           ( -- )
;               Ignore following text till the end of line.

                .COLON  IMEDD+1,Odd7,BKSLA
                bl      NTIB
				bl		AT
				bl		INN
				bl		STOR
				bl		EXIT

;   CHAR        ( -- c )
;               Parse next word and return its first character.

                .COLON  4,'CHAR',CHAR
                bl      BLNK
				bl		PARS
				bl		DROP
				bl		CAT
				bl		EXIT

;   TOKEN       ( -- a ; <string> )
;               Parse a word from input stream and copy it to name dictionary.

                .COLON  5,'token',TOKN
                bl      BLNK
				bl		PARS
				bl		DLIT
				dc.l	31
				bl		MIN
                bl      NP
				bl		AT
				bl		OVER
				bl		SUBB
				bl		CLLM
                bl      PCKS
				bl		EXIT

;   WORD        ( c -- a ; <string> )
;               Parse a word from input stream and copy it to code dictionary.

                .COLON  4,'WORD',WRD
                bl      PARS
				bl		HERE
				bl		PCKS
				bl		EXIT

;; Dictionary search

;   NAME>       ( na -- ca )
;               Return a code address given a name address.

                .COLON  5,'name>',NAMT
                bl      CLLM             	;32 bit cells, 16 bit values.$$$
                bl      CLLM
				bl		AT
				bl		EXIT

;   SAME?       ( a a u -- a a f \ -0+ )
;               Compare u cells in two strings. Return 0 if identical.

                .COLON  5,'same?',SAMQ
                bl      TOR
                bl	SAME2
SAME1:          bl      OVER
				bl		RAT
				bl		CLLS
				bl		PLUS
				bl		AT   				;32/16 mix-up
                bl      OVER
				bl		RAT
				bl		CLLS
				bl		PLUS
				bl		AT   				;32/16 mix-up
                bl      SUBB
				bl		QDUP
                bl      QBRN
				bl		SAME2
                bl      RFRM
				bl		DROP
				bl		EXIT         		;strings not equal
SAME2:          bl      DNXT
				bl		SAME1
                bl      DLIT
				dc.l	0
				bl		EXIT            	;strings equal

;   find        ( a va -- ca na | a F )
;               Search a vocabulary for a string. Return ca and na if succeeded.

                .COLON  4,'find',FIND
                bl      SWAP
				bl		DUPP
				bl		CAT
                bl      DLIT
				dc.l	CELLL
				bl		SLASH
				bl		TEMP
				bl		STOR        		;32/16 bit mix-up
                bl      DUPP
				bl		AT
				bl		TOR
				bl		CLLP
				bl		SWAP
FIND1:          bl      AT
				bl		DUPP
                bl      QBRN
				bl		FIND6
                bl      DUPP
				bl		AT
				bl		DLIT
				dc.l	MASKK
				bl		ANDD
				bl		RAT
				bl		XORR
                bl      QBRN
				bl		FIND2
                bl      CLLM              	;backup to link field
                bl	FIND1             		;try next word
FIND2:          bl      CLLP
				bl		TEMP
				bl		AT
				bl		SAMQ             	;32/16 mix-up
FIND3:          bl	FIND4
FIND6:          bl      RFRM
				bl		DROP
                bl      SWAP
				bl		CLLM
				bl		SWAP
				bl		EXIT            	;not in this vocab
FIND4:          bl      QBRN
				bl		FIND5
                bl      CLLM
				bl		CLLM                
                bl	FIND1
FIND5:          bl      RFRM
				bl		DROP
				bl		SWAP
				bl		DROP
                bl      CLLM            	
                bl      DUPP
				bl		NAMT
				bl		SWAP
				bl		EXIT            	;ca

;   NAME?       ( a -- ca na | a F )
;               Search all context vocabularies for a string.

                .COLON  5,'name?',NAMQ
                bl      CTXT
				bl		DUPP
				bl		DAT
				bl		XORR     			;?context=also
                bl      QBRN
				bl		NAMQ1
                bl      CLLM             	;no, start with context
NAMQ1:          bl      TOR
NAMQ2:          bl      RFRM
				bl		CLLP
				bl		DUPP
				bl		TOR    				;next in search order
                bl      AT
				bl		QDUP
                bl      QBRN
				bl		NAMQ3
                bl      FIND
				bl		QDUP          		;search vocabulary
                bl      QBRN
				bl		NAMQ2
                bl      RFRM
				bl		DROP
				bl		EXIT         		;found name
NAMQ3:          bl      RFRM
				bl		DROP              	;name not found
                bl      DLIT
				dc.l	0
				bl		EXIT            	;false flag

;; Terminal response

;   ^H          ( bot eot cur -- bot eot cur )
;               Backup the cursor by one character.

                .COLON  2,'^h',BKSP
                bl      TOR
				bl		OVER
				bl		RFRM
				bl		SWAP
				bl		OVER
				bl		XORR
                bl      QBRN
				bl		BACK1
                bl      DLIT
				dc.l	BKSPP
				bl		TECH
				bl		ATEX
				bl		DLIT
				dc.l	1
				bl		SUBB
                bl      BLNK
				bl		TECH
				bl		ATEX
                bl      DLIT
				dc.l	BKSPP
				bl		TECH
				bl		ATEX
BACK1:          bl      EXIT

;   TAP         ( bot eot cur c -- bot eot cur )
;               Accept and echo the key stroke and bump the cursor.

                .COLON  3,'tap',TAP
                bl      DUPP
				bl		TECH
				bl		ATEX
                bl      OVER
				bl		CSTR
				bl		DLIT
				dc.l	1
				bl		PLUS
				bl		EXIT

;   kTAP        ( bot eot cur c -- bot eot cur )
;               Process a key stroke, CR or backspace.

                .COLON  4,'ktap',KTAP
                bl      DUPP
				bl		DLIT
				dc.l	CRR
				bl		XORR
                bl      QBRN
				bl		KTAP2
                bl      DLIT
				dc.l	BKSPP
				bl		XORR
                bl      QBRN
				bl		KTAP1
                bl      BLNK
				bl		TAP
				bl		EXIT
KTAP1:          bl      BKSP
				bl		EXIT
KTAP2:          bl      DROP
				bl		SWAP
				bl		DROP
				bl		DUPP
				bl		EXIT

;   accept      ( b u -- b u )
;               Accept characters to input buffer. Return with actual count.

                .COLON  6,'accept',ACCEP
                bl      OVER
				bl		PLUS
				bl		OVER
ACCP1:          bl      DDUP
				bl		XORR
                bl      QBRN
				bl		ACCP4
                bl      KEY
				bl		DUPP
                bl      BLNK
				bl		DLIT
				dc.l	127
				bl		WITHI
                bl      QBRN
				bl		ACCP2
                bl      TAP
                bl	ACCP3
ACCP2:          bl      TTAP
				bl		ATEX
ACCP3:          bl	ACCP1
ACCP4:          bl      DROP
				bl		OVER
				bl		SUBB
				bl		EXIT

;   EXPECT      ( b u -- )
;               Accept input stream and store count in SPAN.

                .COLON  6,'expect',EXPEC
                bl      TEXPE
				bl		ATEX
				bl		SPAN
				bl		STOR
				bl		DROP
				bl		EXIT

;   QUERY       ( -- )
;               Accept input stream to terminal input buffer.

                .COLON  5,'QUERY',QURY
                bl      TIB
				bl		DLIT
				dc.l	80
				bl		TEXPE
				bl		ATEX
				bl		NTIB
				bl		STOR
                bl      DROP
				bl		DLIT
				dc.l	0
				bl		INN
				bl		STOR
				bl		EXIT

;; Error handling

;   CATCH       ( ca -- 0 | err# )
;               Execute word at ca and set up an error frame for it.

                .COLON  5,'catch',CTCH
                bl      SPAT
				bl		TOR
				bl		HNDL
				bl		AT
				bl		TOR   				;save error frame
                bl      RPAT
				bl		HNDL
				bl		STOR
				bl		EXEC  				;execute
                bl      RFRM
				bl		HNDL
				bl		STOR				;restore error frame
                bl      RFRM
				bl		DROP
				bl		DLIT
				dc.l	0
				bl		EXIT ;no error

;   THROW       ( err# -- err# )
;               Reset system to current local error frame an update error flag.

                .COLON  5,'throw',THRW
                bl      HNDL
				bl		AT
				bl		RPST				;restore return stack
                bl      RFRM
				bl		HNDL
				bl		STOR				;restore handler frame
                bl      RFRM
				bl		SWAP
				bl		TOR
				bl		SPST				;restore data stack
                bl      DROP
				bl		RFRM
				bl		EXIT

;   NULL$       ( -- a )
;               Return address of a null string with zero count.

                .CODE  5,'null$',NLLS
				mflr	rLR
				bl		doVar
                dc.l      0
                dc.b      99,111,121,111,116,101
                .ALIGN

;   ABORT       ( -- )
;               Reset data stack and jump to QUIT.

                .COLON  5,'ABORT',ABORT
                bl      NLLS
				bl		THRW

;   abort"      ( f -- )
;               Run time routine of ABORT" . Abort with a message.

                .COLON  COMPO+6,'abort"',ABRQ
                bl      QBRN
				bl		ABOR1           	;text flag
                bl      DOSTR
				bl		THRW             	;pass error string
ABOR1:          bl      DOSTR
				bl		DROP
				bl		EXIT         		;drop error

;; The text interpreter

;   $INTERPRET  ( a -- )
;               Interpret a word. If failed, try to convert it to an integer.

                .COLON  10,'$interpret',INTER
                bl      NAMQ
				bl		QDUP              	;?defined
                bl      QBRN
				bl		INTE1
                bl      AT
				bl		DLIT
				dc.l	COMPO<<24
				bl		ANDD     			;?compile only lexicon bits
                .DS      ABRQ,' compile only'
                bl      EXEC
				bl		EXIT              	;execute defined word
INTE1:          bl      TNUMB
				bl		ATEX             	;convert a number
                bl      QBRN
				bl		INTE2
                bl      EXIT
INTE2:          bl      THRW             	;error

;   [           ( -- )
;               Start the text interpreter.

                .COLON  IMEDD+1,'[',LBRC
                bl      DLIT
				dc.l	INTER
				bl		TEVAL
				bl		STOR
				bl		EXIT

;   .OK         ( -- )
;               Display 'ok' only while interpreting.

                .COLON  3,'.ok',DOTOK
                bl      DLIT
				dc.l	INTER
				bl		TEVAL
				bl		AT
				bl		EQUL
                bl      QBRN
				bl		DOTO1
                .DS      DTQP,' ok'
DOTO1:          bl      CR
				bl		EXIT

;   ?STACK      ( -- )
;               Abort if the data stack underflows.

                .COLON  6,'?stack',QSTC
                bl      DPTH
				bl		ZLSS             	;check only for underflow
                .DS      ABRQ,' underflow'
                bl      EXIT

;   EVAL        ( -- )
;               Interpret the input stream.

                .COLON  4,'eval',EVAL
EVAL1:          bl      TOKN
				bl		DUPP
				bl		CAT          		;?input stream empty
                bl      QBRN
				bl		EVAL2
                bl      TEVAL
				bl		ATEX
				bl		QSTC       			;evaluate input, check stack
                bl	EVAL1
EVAL2:          bl      DROP
				bl		TPRM
				bl		ATEX
				bl		EXIT   				;prompt

;; Shell

;   PRESET      ( -- )
;               Reset data stack pointer and the terminal input buffer.

                .COLON  6,'preset',PRES
                bl      SZER
				bl		AT
				bl		SPST
                bl      DLIT
				dc.l	TIBB
				bl		NTIB
				bl		CLLP
				bl		STOR
				bl		EXIT

;   xio         ( a a a -- )
;               Reset the I/O vectors 'EXPECT, 'TAP, 'ECHO and 'PROMPT.

                .COLON  COMPO+3,'xio',XIO
                bl      DLIT
				dc.l	ACCEP
				bl		TEXPE
				bl		DSTR
                bl      TECH
				bl		DSTR
				bl		EXIT

;   FILE        ( -- )
;               Select I/O vectors for file download.

                .COLON  4,'FILE',FILEE
                bl      DLIT
				dc.l	PACE
				bl		DLIT
				dc.l	DROP
                bl      DLIT
				dc.l	KTAP
				bl		XIO
				bl		EXIT

;   HAND        ( -- )
;               Select I/O vectors for terminal interface.

                .COLON  4,'HAND',HAND
                bl      DLIT
				dc.l	DOTOK
				bl		DLIT
				dc.l	DROP				;don't repeat characters.  Mac OS
											;dit it already. 4/20/97 cht
                bl      DLIT
				dc.l	KTAP
				bl		XIO
				bl		EXIT

;   I/O         ( -- a )
;               Array to store default I/O vectors.

                .CODE  3,'i/o',ISLO
				mflr	rLR
				bl		doVar
                dc.l    QRX
				dc.l	TXSTO             	;default I/O vectors
                                         	;for 32 bit system 3/16/92

;   CONSOLE     ( -- )
;               Initiate terminal interface.

                .COLON  7,'CONSOLE',CONS
                bl      ISLO
				bl		DAT
				bl		TQKY
				bl		DSTR    			;restore default I/O device
                bl      HAND
				bl		EXIT             	;keyboard input

;   QUIT        ( -- )
;               Reset return stack pointer and start text interpreter.

                .COLON  4,'QUIT',QUIT
                bl      RZER
				bl		AT
				bl		RPST          		;reset return stack pointer
QUIT1:          bl      LBRC             	;start interpretation
QUIT2:          bl      QURY             	;get input
                bl      DLIT
				dc.l	EVAL
				bl		CTCH
				bl		QDUP   				;evaluate input
                bl      QBRN
				bl		QUIT2             	;continue till error
                bl      TPRM
				bl		AT
				bl		SWAP           		;save input device
                bl      CONS
				bl		NLLS
				bl		OVER
				bl		XORR   				;?display error message
                bl      QBRN
				bl		QUIT3
                bl      SPAC
				bl		COUN
				bl		TYP       			;error message
                .DS      DTQP,' ? '   		;error prompt
                bl      CR
QUIT3:          bl      DLIT
				dc.l	DOTOK
				bl		XORR        		;?file input
                bl      QBRN
				bl		QUIT4
                bl      DLIT
				dc.l	ERR
				bl		EMIT          		;file error, tell host
QUIT4:          bl      PRES           		;some cleanup
                bl	QUIT1

;; The compiler

;   '           ( -- ca )
;               Search context vocabularies for the next word in input stream.

                .COLON  1,'''',TICK
                bl      TOKN
				bl		NAMQ             	;?defined
                bl      QBRN
				bl		TICK1
                bl      EXIT            	;yes, push code address
TICK1:          bl      THRW           		;no, error

;   ALLOT       ( n -- )
;               Allocate n bytes to the code dictionary.

                .COLON  5,'ALLOT',ALLOT
                bl      CP
				bl		PSTR
				bl		EXIT           		;adjust code pointer

Odd8:			set		$2c

;   ,           ( w -- )
;               Compile an integer into the code dictionary.

                .COLON  1,Odd8,CMMA
                bl      HERE
				bl		DUPP
				bl		CLLP         		;cell boundary
                bl      CP
				bl		STOR
				bl		STOR
				bl		EXIT     			;adjust code pointer, compile

;   [COMPILE]   ( -- ; <string> )
;               Compile the next immediate word into code dictionary.

                .COLON  IMEDD+9,'[COMPILE]',BCOMP
                bl      TICK
				bl		AGAN
				bl		EXIT				;compile a branch-link instruction
				
;   COMPILE     ( -- )
;               Compile the next address in colon list to code dictionary.

                .COLON  COMPO+7,'COMPILE',COMPI
                bl      RFRM				; a --
				bl		DUPP
				lwz		rTOS,0(rTOS)		; a bl --
				bl		OVER				;convert abs addr to rel addr
				subf	rTOS,rBase,rTOS
				bl		PLUS
				bl		DLIT
				dc.l	0X03FFFFFC
				bl		ANDD				; a ca --
				bl		AGAN     			;compile bl instruction
                bl      CLLP
				bl		TOR
				bl		EXIT          		;adjust return address

;   LITERAL     ( w -- )
;               Compile tos to code dictionary as an integer literal.

                .COLON  IMEDD+7,'LITERAL',LITER
                bl      COMPI
				bl		DLIT
				bl		CMMA
				bl		EXIT

;   $,"         ( -- )
;               Compile a literal string up to next " .

                HEADER  3,,STCQ
				DC.B	3,$24,$2c,$22
STCQ:			push	rRP,rTOR					;push cached TOR to return stack
				mflr	rTOR						;save IP from lr to rTOR

                bl      DLIT
				dc.l	0X22				;'"'
				bl		WRD         		;move string to code dictionary
                bl      COUN
				bl		PLUS
				bl		ALGD        		;calculate aligned end of string
                bl      CP
				bl		STOR
				bl		EXIT           		;adjust the code pointer

;   RECURSE     ( -- )
;               Make the current word available for compilation.

                .COLON  IMEDD+7,'RECURSE',RECUR
                bl      LAST
				bl		AT
				bl		NAMT
				bl		AGAN
				bl		EXIT				;compile branch instruction

;; Structures

;	THEN and AGAIN are the primitive operations to resolve branch and loop
;	structures.  Code are modified to suit the subroutine threading lists
;	in this powerPC implementation.  CHT, 3/28/97
;	Use CMMA to compile a literal.  Use AGAIN to compile an execution address as a
;	branch instruction.   ppc specific, 3/31/97 cht


;   THEN        ( a -- )
;               Terminate a forward branch structure.
;				The frward reference deposit a branch instruction.  The address
;				field is filled by A.

                .COLON  IMEDD+4,'THEN',THNN
                bl      HERE
				bl		OVER
				bl		SUBB
				bl		DLIT
				dc.l	0X03FFFFFC			;fit into the address field
				bl		ANDD
                bl      DLIT
				dc.l	0X48000001
				bl		ORR					;make the 'bl addr' instruction
				bl		SWAP
				bl		STOR
				bl		EXIT

;   AGAIN       ( a -- )
;               Resolve a backward jump and terminate a loop structure.
;				Compile a branch instruction, with 'a' in the address field.

                .COLON  IMEDD+5,'AGAIN',AGAN
				bl		HERE				;offset from HERE
				bl		SUBB
				bl		DLIT
				dc.l	0X03FFFFFC			;fit into the address field
				bl		ANDD
                bl      DLIT
				dc.l	0X48000001			;branch-link is more useful than branch.
				bl		ORR
				bl		CMMA
				bl		EXIT

;   FOR         ( -- a )
;               Start a FOR-NEXT loop structure in a colon definition.

                .COLON  IMEDD+3,'FOR',FOR
                bl      COMPI
				bl		TOR
				bl		HERE
				bl		EXIT

;   BEGIN       ( -- a )
;               Start an infinite or indefinite loop structure.

                .COLON  IMEDD+5,'BEGIN',BEGN
                bl      HERE
				bl		EXIT

;   NEXT        ( a -- )
;               Terminate a FOR-NEXT loop structure.

                .COLON  IMEDD+4,'NEXT',NEXT
                bl      COMPI
				bl		DNXT
				bl		AGAN
				bl		EXIT

;   UNTIL       ( a -- )
;               Terminate a BEGIN-UNTIL indefinite loop structure.

                .COLON  IMEDD+5,'UNTIL',UNTIL
                bl      COMPI
				bl		QBRN
				bl		AGAN
				bl		EXIT

;   IF          ( -- A )
;               Begin a conditional branch structure.

                .COLON  IMEDD+2,'IF',IFF
                bl      COMPI
				bl		QBRN
				bl		HERE
                bl      DLIT
				dc.l	0X48000001
				bl		CMMA
				bl		EXIT

;   AHEAD       ( -- A )
;               Compile a forward branch instruction.

                .COLON  IMEDD+5,'AHEAD',AHEA
				bl		HERE
				bl		DLIT
				dc.l	0X48000001
				bl		CMMA
				bl		EXIT

;   REPEAT      ( A a -- )
;               Terminate a BEGIN-WHILE-REPEAT indefinite loop.

                .COLON  IMEDD+6,'REPEAT',REPEA
                bl      AGAN
				bl		THNN
				bl		EXIT

;   AFT         ( a -- a A )
;               Jump to THEN in a FOR-AFT-THEN-NEXT loop the first time through.

                .COLON  IMEDD+3,'AFT',AFT
                bl      DROP
				bl		AHEA
				bl		BEGN
				bl		SWAP
				bl		EXIT

;   ELSE        ( A -- A )
;               Start the false clause in an IF-ELSE-THEN structure.

                .COLON  IMEDD+4,'ELSE',ELSEE
                bl      AHEA
				bl		SWAP
				bl		THNN
				bl		EXIT

;   WHILE       ( a -- A a )
;               Conditional branch out of a BEGIN-WHILE-REPEAT loop.

                .COLON  IMEDD+5,'WHILE',WHILE
                bl      IFF
				bl		SWAP
				bl		EXIT

;   ABORT"      ( -- ; <string> )
;               Conditional abort with an error message.

                .COLON  IMEDD+6,'ABORT"',ABRTQ
                bl      COMPI
				bl		ABRQ
				bl		STCQ
				bl		EXIT

;   $"          ( -- ; <string> )
;               Compile an inline string literal.

                .COLON  IMEDD+2,'$"',STRQ
                bl      COMPI
				bl		STQP
				bl		STCQ
				bl		EXIT

;   ."          ( -- ; <string> )
;               Compile an inline string literal to be typed out at run time.

                .COLON  IMEDD+2,'."',DOTQ
                bl      COMPI
				bl		DTQP
				bl		STCQ
				bl		EXIT

;; Name compiler

;   ?UNIQUE     ( a -- a )
;               Display a warning message if the word already exists.

                .COLON  7,'?unique',UNIQ
                bl      DUPP
				bl		NAMQ              	;?name exists
                bl      QBRN
				bl		UNIQ1             	;redefinitions are OK
                .DS      DTQP,' reDef '   	;but warn the user
                bl      OVER
				bl		COUN
				bl		TYP        			;just in case its not planned
UNIQ1:          bl      DROP
				bl		EXIT

;   $,n         ( na -- )
;               Build a new dictionary name using the string at na.

                HEADER  3,,SNAM
				DC.B	3,$23,$2c,$6e
SNAM:			push	rRP,rTOR					;push cached TOR to return stack
				mflr	rTOR						;save IP from lr to rTOR
				
                bl      DUPP
				bl		CAT                	;?null input
                bl      QBRN
				bl		PNAM1
                bl      UNIQ            	;?redefinition
                bl      DUPP
				bl		LAST
				bl		STOR         		;save na for vocabulary link
                bl      HERE
				bl		ALGD
				bl		SWAP         		;align code address
                bl      CLLM              	;link address
                bl      CRNT
				bl		AT
				bl		AT
				bl		OVER
				bl		STOR
                bl      CLLM
				bl		DUPP
				bl		NP
				bl		STOR     			;adjust name pointer
                bl      STOR
				bl		EXIT              	;save code pointer
PNAM1:          .DS      STQP,' name'   	;null input
                bl      THRW

;; FORTH compiler

;   $COMPILE    ( a -- )
;               Compile next word to code dictionary as a token or literal.

                .COLON  8,'$compile',SCMP
                bl      NAMQ
				bl		QDUP              	;?defined
                bl      QBRN
				bl		SCOM2
                bl      AT
				bl		DLIT
				dc.l	IMEDD<<24
				bl		ANDD     			;?immediate
                bl      QBRN
				bl		SCOM1
                bl      EXEC
				bl		EXIT              	;its immediate, execute
SCOM1:          
				bl      AGAN				;compile a bl instruction
				bl		EXIT              	;its not immediate, compile
											;Replace CMMA by AGAIN, 3/31/97 cht
SCOM2:          bl      TNUMB	
				bl		ATEX             	;try to convert to number
                bl      QBRN
				bl		SCOM3
                bl      LITER
				bl		EXIT              	;compile number as integer
SCOM3:          bl      THRW             	;error

;   OVERT       ( -- )
;               Link a new word into the current vocabulary.

                .COLON  5,'overt',OVRT
                bl      LAST
				bl		AT
				bl		CRNT
				bl		AT
				bl		STOR
				bl		EXIT

Odd11:			equ		$3b

;   ;           ( -- )
;               Terminate a colon definition.

                .COLON  IMEDD+COMPO+1,Odd11,SEMIS
                bl      COMPI
				bl		EXIT
				bl		LBRC
				bl		OVRT
				bl		EXIT

;   ]           ( -- )
;               Start compiling the words in the input stream.

                .COLON  1,']',RBRAC
                bl      DLIT
				dc.l	SCMP
				bl		TEVAL
				bl		STOR
				bl		EXIT

Odd12:			equ		$3a

;   :           ( -- ; <string> )
;               Start a new colon definition using next word as its name.

                .COLON  1,Odd12,COLON
                bl      TOKN
				bl		SNAM
				bl		DLIT				;compile 2 instruction nest
				push	rRP,rTOR
				bl		CMMA
				bl		DLIT
				mflr	rTOR
                bl      CMMA
				bl		RBRAC
				bl		EXIT

;   IMMEDIATE   ( -- )
;               Make the last compiled word an immediate word.

                .COLON  9,'IMMEDIATE',IMMD
                bl      DLIT
				dc.l	IMEDD<<24
				bl		LAST
				bl		AT
				bl		AT
				bl		ORR
                bl      LAST
				bl		AT
				bl		STOR
				bl		EXIT

;; Defining words

;   USER        ( u -- ; <string> )
;               Compile a new user variable.

                .COLON  4,'USER',USER
                bl      TOKN
				bl		SNAM
				bl		OVRT
                bl      DLIT
				mflr	rLR
				bl		CMMA
				bl		DLIT
				dc.l	doUser
				bl		AGAN
				bl		CMMA
				bl		EXIT

;   CREATE      ( -- ; <string> )
;               Compile a new array entry without allocating code space.

                .COLON  6,'CREATE',CREA
                bl      TOKN
				bl		SNAM
				bl		OVRT
                bl      DLIT
				mflr	rLR
				bl		CMMA
				bl		DLIT
				dc.l	doVar
				bl		AGAN
				bl		EXIT

;   VARIABLE    ( -- ; <string> )
;               Compile a new variable initialized to 0.

                .COLON  8,'VARIABLE',VARI
                bl      CREA
				bl		DLIT
				dc.l	0
				bl		CMMA
				bl		EXIT

;; Tools

;   _TYPE       ( b u -- )
;               Display a string. Filter non-printing characters.

                .COLON  5,'_type',UTYP
                bl      TOR               	;start count down loop
                bl	UTYP2              		;skip first pass
UTYP1:          bl      DUPP
				bl		CAT
				bl		TCHR
				bl		EMIT     			;display only printable
                bl      DLIT
				dc.l	1
				bl		PLUS            	;increment address
UTYP2:          bl      DNXT
				bl		UTYP1             	;loop till done
                bl      DROP
				bl		EXIT

;   dm+         ( a u -- a )
;               Dump u bytes from , leaving a+u on the stack.

                .COLON  3,'dm+',DMP
                bl      OVER
				bl		DLIT
				dc.l	4
				bl		UDOTR      			;display address
                bl      SPAC
				bl		TOR               	;start count down loop
                bl	PDUM2              		;skip first pass
PDUM1:          bl      DUPP
				bl		CAT
				bl		DLIT
				dc.l	3
				bl		UDOTR  ;display numeric data
                bl      DLIT
				dc.l	1
				bl		PLUS            	;increment address
PDUM2:          bl      DNXT
				bl		PDUM1             	;loop till done
                bl      EXIT

;   DUMP        ( a u -- )
;               Dump u bytes from a, in a formatted manner.

                .COLON  4,'DUMP',DUMP
                bl      BASE
				bl		AT
				bl		TOR
				bl		HEX         		;save radix, set hex
                bl      DLIT
				dc.l	16
				bl		SLASH          		;change count to lines
                bl      TOR
				bl	DUMP4          			;start count down loop
DUMP1:          bl      CR
				bl		DLIT
				dc.l	16
				bl		DDUP
				bl		DMP    				;display numeric
                bl      ROT
				bl		ROT
                bl      SPAC
				bl		SPAC
				bl		UTYP       			;display printable characters
DUMP4:          bl      DNXT
				bl		DUMP1             	;loop till done
DUMP3:          bl      DROP
				bl		RFRM
				bl		BASE
				bl		STOR   				;restore radix
                bl      EXIT

;   .S          ( ... -- ... )
;               Display the contents of the data stack.

                .COLON  2,'.S',DOTS
                bl      SPAC
				bl		DPTH             	;stack depth
                bl      TOR            		;start count down loop
                bl	DOTS2              		;skip first pass
DOTS1:          bl      RAT
				bl		PICK
				bl		DOT            		;index stack, display contents
DOTS2:          bl      DNXT
				bl		DOTS1             	;loop till done
                bl      SPAC
                bl      EXIT

;   !CSP        ( -- )
;               Save stack pointer in CSP for error checking.

                .COLON  4,'!csp',STCSP
                bl      SPAT
				bl		CSP
				bl		STOR
				bl		EXIT     			;save pointer

;   ?CSP        ( -- )
;               Abort if stack pointer differs from that saved in CSP.

                .COLON  4,'?csp',QCSP
                bl      SPAT
				bl		CSP
				bl		AT
				bl		XORR        		;compare pointers
                .DS     ABRQ,'stacks'		;abort if different
                bl      EXIT

;   >NAME       ( ca -- na | F )
;               Convert code address to a name address.

                .COLON  5,'>name',TNAME
                bl      CRNT        		;vocabulary link
TNAM1:          bl      CLLP
				bl		AT
				bl		QDUP        		;check all vocabularies
                bl      QBRN
				bl		TNAM4
                bl      DDUP
TNAM2:          bl      AT
				bl		DUPP        		;?last word in a vocabulary
                bl      QBRN
				bl		TNAM3
                bl      DDUP
				bl		NAMT
				bl		XORR         		;compare
                bl      QBRN
				bl		TNAM3
                bl      CLLM 				;continue with next word***$$$$
                bl	TNAM2
TNAM3:          bl      SWAP
				bl		DROP
				bl		QDUP
                bl      QBRN
				bl		TNAM1
                bl      SWAP
				bl		DROP
				bl		SWAP
				bl		DROP
				bl		EXIT
TNAM4:          bl      DROP
				bl		DLIT
				dc.l	0
				bl		EXIT       			;false flag

;   .ID         ( na -- )
;               Display the name at address.

                .COLON  3,'.id',DTID
                bl      QDUP         		;if zero no name
                bl      QBRN
				bl		DOTI1
                bl      COUN
				bl		DLIT
				dc.l	0X1F
				bl		ANDD   				;mask lexicon bits
                bl      UTYP
				bl		EXIT      			;display name string
DOTI1:          .DS      DTQP,' {noName}'
                bl      EXIT

;   @A         ( a -- n | ca )
;               Fetch contents.  If it is a branch instruction, convert to ca.
;				DUP @ 48000000 2DUP AND =
;				IF DUP 2000000 AND  IF   FC000000 OR FFFFFFFC AND
;									ELSE 03FFFFFC AND THEN  +
;				ELSE SWAP DROP THEN ;

                .COLON  2,'@a',ATA
				bl		DUPP				; a a --
				bl		AT					; a n --
				bl		DLIT
				dc.l	0X48000000			;
				bl		DDUP				; a n 48000000 n 48000000 --
				bl		ANDD
				bl		EQUL				; a n ? --
				bl		QBRN				;is it a bl instruction?
				bl		ATA3
				
				bl		DUPP				; a n n --
				bl		DLIT
				dc.l	0X02000000			;negative offset?
				bl		ANDD
				bl		QBRN
				bl		ATA1
				bl		DLIT
				dc.l	0XFC000000			;negative offset.  sign extend.
				bl		ORR
				bl		DLIT
				dc.l	0XFFFFFFFC			;get rid of the KL bits
				bl		ANDD				; a offset --
				bl		ATA2
ATA1:			bl		DLIT				;positive offset.  mask off
				dc.l	0X03FFFFFC			;    misc bits.
				bl		ANDD				; a offset --
ATA2:			bl		PLUS				;convert to code address
				bl		EXIT				; ca --
				
ATA3:			bl		SWAP				;discard address
				bl		DROP				; n --
				bl		EXIT
				

;   SEE         ( -- ; <string> )
;               A simple decompiler.

                .COLON  3,'SEE',SEE
                bl      TICK    			;starting address
				bl		CLLP        		;****************
				bl		DLIT
				dc.l	19
				bl		TOR
SEE1:           bl      CLLP
				bl		DUPP
				bl		ATA					;ppc subroutine threading
				bl		DUPP      			;?does it contain a zero**$$$
                bl      QBRN
				bl		SEE2
                bl      TNAME         		;?is it a name
SEE2:           bl      QDUP        		;name address or zero
                bl      QBRN
				bl		SEE3
                bl      SPAC
				bl		DTID     			;display name
                bl	SEE4
SEE3:           bl      DUPP
				bl		AT
				bl		UDOT           		;display number
SEE4:           bl      DNXT        		;user control
				bl		SEE1
                bl      DROP
				bl		EXIT

;   WORDS       ( -- )
;               Display the names in the context vocabulary.

                .COLON  5,'WORDS',WRDS
				bl		CTXT
				bl		AT          		;only in context
WORS0:			bl		DLIT
				dc.l	10
				bl		TEMP
				bl		STOR
                bl      CR
WORS1:          bl      AT
				bl		QDUP        		;?at end of list
                bl      QBRN
				bl		WORS2
                bl      DUPP
				bl		SPAC
				bl		DTID        		;display a name
                bl      CLLM
				bl		TEMP
				bl		AT
				bl		DLIT
				dc.l	1
				bl		SUBB
				bl		QDUP
				bl		QBRN
				bl		WORS0
				bl		TEMP
				bl		STOR
				bl		WORS1
WORS2:          bl      EXIT

;; Hardware reset

;   VER         ( -- n )
;               Return the version number of this implementation.

                .CODE  3,'ver',VRSN
                mflr	rLR
				bl		doCon
				dc.l	VER*256+EXT

;   hi          ( -- )
;               Display the sign-on message of eForth.

                .COLON  2,'hi',HI
                bl      STOIO
				bl		CR					;initialize I/O
                .DS      DTQP,'ppc eForth v'    ;model
                bl      BASE
				bl		AT
				bl		HEX 				;save radix
                bl      VRSN
				bl		BDGS
				bl		DIG
				bl		DIG
                bl      DLIT
				dc.l	0X2E				;'.'
				bl		HOLD
                bl      DIGS
				bl		EDGS
				bl		TYP        			;format version number
                bl      BASE
				bl		STOR
				bl		CR
				bl		EXIT      			;restore radix

;   'BOOT       ( -- a )
;               The application startup vector.

                .CODE  5,'''boot',TBOT
				mflr	rLR
				bl		doVar
                dc.l    HI           		;application to boot

;   COLD        ( -- )
;               The hilevel cold start sequence.

                .COLON  4,'COLD',COLD
COLD1:          bl      PRES        		;initialize stack and TIB
                bl      TBOT
				bl		ATEX        		;application boot
                bl      FRTH
				bl		CTXT
				bl		AT
				bl		DUPP     			;initialize search order
                bl      CRNT
				bl		DSTR
				bl		OVRT
                bl      QUIT        		;start interpretation
                bl	COLD1            		;just in case


;===============================================================
;
;		File Interface
;
;===============================================================

;   source-id	( -- n )
;               Return the file-id of the currently opened file.

                .CODE  9,'SOURCE-ID',SOUR
                mflr	rLR
				bl		doCon
source:			dc.l	0

;	open		( str-addr mode -- fileid )
;
				.CODE	4,'open',OPEN
				pop		rDP,r3
				add		r3,r3,rBase
				ori		r4,rTOS,0
				
				prologue
				bl		.open
				nop
				epilogue
				
				ori		rTOS,r3,0
				stw		r3,source(rBase)
				blr
				
;	creat		( str-addr -- fileid )
;
				.CODE	5,'creat',CRET
				add		r3,rTOS,rBase
				
				prologue
				bl		.creat
				nop
				epilogue
				
				ori		rTOS,r3,0
				stw		r3,source(rBase)
				blr
				
;	read		( addr count fileid -- count2 )
;
				.CODE	4,'read',READ
				ori		r3,rTOS,0
				pop		rDP,r5
				pop		rDP,r4
				add		r4,r4,rBase
				
				prologue
				bl		.read
				nop
				epilogue
				
				ori		rTOS,r3,0
				blr
				
;	write		( addr count fileid -- count2 )
;
				.CODE	5,'write',WRIT
				ori		r3,rTOS,0
				pop		rDP,r5
				pop		rDP,r4
				add		r4,r4,rBase
				
				prologue
				bl		.write
				nop
				epilogue
				
				ori		rTOS,r3,0
				blr
				
;	seek		( count whence fileid -- count2 )
;
				.CODE	4,'seek',SEEK
				ori		r3,rTOS,0
				pop		rDP,r5
				pop		rDP,r4
				
				prologue
				bl		.lseek
				nop
				epilogue
				
				ori		rTOS,r3,0
				blr
				
;	delete		( fileid -- ior )
;
				.CODE	6,'delete',DELE
				ori		r3,rTOS,0
				
				prologue
				bl		.unlink
				nop
				epilogue
				
				ori		rTOS,r3,0
				blr
				
;	close		( fileid -- ior )
;
				.CODE	5,'close',CLOS
				ori		r3,rTOS,0
				
				prologue
				bl		.close
				nop
				epilogue
				
				ori		rTOS,r3,0
				blr
				
;	OPEN		( <filename> ; -- ior )
;
				.COLON	4,'OPEN',OPNN
				bl		TOKN
				bl		DLIT
				dc.l	1
				bl		PLUS
				bl		DLIT
				dc.l	0
				bl		OPEN
				bl		EXIT
				
;	INCLUDE		( <filename> ; -- )
;
				.COLON	7,'INCLUDE',INCL
				bl		TIB
				bl		NTIB
				bl		AT					;
				bl		PLUS				;new buffer address
				bl		DLIT
				dc.l	0X2000				;read entire file
											;free-biffer -1 --
				bl		OPNN				;high level OPEN new file
				
				bl		DUPP				;buffAddr count id id --
				bl		ZLSS				;file open ok?
				.DS		ABRQ,'Cannot open file.'
				bl		READ				; go read file
											;length --
				bl		SOUR
				bl		CLOS				;close new file
				bl		DROP
				
				bl		INN
				bl		DAT
				bl		TOR					;save >IN and #TIB
				bl		TOR
				bl		NTIB				;move >IN to end of old buffer
				bl		AT					;to interpret new file
				bl		INN
				bl		STOR
				bl		NTIB
				bl		PSTR				;end of new buffer
				bl		EVAL				;interpret new file contents
				
				bl		RFRM				;continue interpreting old file
				bl		RFRM
				bl		INN
				bl		DSTR				;restore >IN and #TIB
				bl		EXIT
				
				
				
;===============================================================

LASTN           equ     _LINK       		;last name address

CTOP            equ     $             		;next available memory in code dictionary

				ORG		TIBB-4
NTOP            equ     $             		;next available memory in name dictionary

				ORG		TIBB				;grow to inifinite for file loading
				dcb.l	0X8000,0
				
;				END     


;===============================================================
;
; System interface to support key and emit
; Lift from MPW:Examples:PPCExamples:AExamples:
; 4/14/97 cht
;
;===============================================================

	MACRO
	MakeFunction &fnName
		EXPORT &fnName[DS]
 		EXPORT .&fnName[PR]
		
		TC &fnName[TC], &fnName[DS]
			
		CSECT &fnName[DS]
			DC.L .&fnName[PR]
 			DC.L TOC[tc0]
		
		CSECT .&fnName[PR]
		FUNCTION .&fnName[PR]	
		
	ENDM
	
linkageArea:		set 24	; constant comes from the PowerPC Runtime Architecture Document
CalleesParams:		set	32	; always leave space for GPR's 3-10
CalleesLocalVars:	set 0	; ClickHandler doesn't have any
numGPRs:			set 10	; num volitile GPR's (GPR's 13-31) used by ClickHandler
numFPRs:			set 0	; num volitile FPR's (FPR's 14-31) used by ClickHandler

spaceToSave:	set linkageArea + CalleesParams + CalleesLocalVars + 4*numGPRs + 8*numFPRs  

	; declare the C function DisplayAlert as external
	import .read,.write,.open,.close,.creat,.lseek,.unlink
	
	import gHelloString		; global variable from C program

	toc
		tc gHelloString[TC], gHelloString
	

; Call the MakeFunction macro, defined in MakeFunction.s to begin the function
	MakeFunction	CharHandler		
	
; PROLOGUE - called routine's responsibilities
.keyTest1:
		mflr	r0					; Get link register
		stw		r0, 0x0008(SP)		; Store the link resgister on the stack
		stwu	SP, -spaceToSave(SP); skip over the stack space where the caller		
									; might have saved stuff
		
; FUNCTION BODY

repeat:
		li		r3,0
		li		r5,1
		lwz		r4,gHelloString[TC](RTOC)
		bl		.read
		nop
		
		cmpwi	cr0,r3,0
		ble		repeat
		
		lwz		r4,gHelloString[TC](RTOC)
		lbz		r4,0(r4)
		cmpwi	cr0,r4,0X70
		beq		bailout

		li		r3,1
		li		r5,1
		lwz		r4,gHelloString[TC](RTOC)
		bl		.write
		nop						; this may be fixed up by the linker
		b		repeat

; EPILOGUE - return sequence

bailout:
		lwz		r0,0x8+spaceToSave(SP)	; Get the saved link register
		addic	SP,SP,spaceToSave		; Reset the stack pointer
		mtlr	r0						; Reset the link register
		blr								; return via the link register
	
	
	csect .CharHandler[pr]
		dc.b 'test'

		end
