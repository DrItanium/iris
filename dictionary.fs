0 constant zero
1 constant ir \ instruction register
              \ contains the address of the next instruction in the threaded
              \ list of the current secondary
2 constant wa \ word address register
              \ contains the word address of the current keyword or the address
              \ of the first code body location of the current keyword
3 constant ca \ code address register
4 constant rs \ return stack register
5 constant sp \ stack pointer
6 constant pc \ processor program counter register
7 constant t0 \ temporary 0
8 constant t1 \ temporary 1
9 constant t2 \ temporary 2
64 constant w0
65 constant w1
66 constant bName
67 constant bChar0
68 constant bChar1
69 constant bChar2
70 constant wMask
71 constant flag

\ generic computer instructions from threaded interpretive languages book
: @-> ( a b -- ) 
  \ the contents of the memory location word whose address is in register A
  \ are loaded into register B ( a 16-bit indirect fetch from A to B )
  !ld ;

: =+n ( n a -- ) 
  \ The contents of register A are incremented by constant n
  dup  ( n a a ) 
  !addi ;

: pop-> ( s a -- ) 
  \ the S pushdown stack top entry is loaded to register A and the stack pointer
  \ is adjusted
  !pop ;

: psh-> ( a s -- )
  \ the A register contents are loaded to the S pushdown stack and the stack 
  \ pointer is adjusted
  !push ;

: -> ( a b -- ) 
  !move ;
: ->pc ( a -- ) 
  \ the contents of the A register are loaded into the PC. The processor will
  \ fetch its next instruction from this location
  pc -> ;

: jmp ( addr -- ) 
  \ unconditional jump to the address encoded into the instruction
  !b ;

\ inner-interpreter words
variable code-location
0 code-location !
: .label ( -- n ) code-location @ constant ; 
: .org ( value -- ) code-location ! ;
0x0100 .org 
.label fnSEMI
    code-location @ 1+ 0x0000FFFF and code<<
    rs ir pop->
.label fnNEXT
    ir wa @->
    2 ir =+n 
.label fnRUN
    wa ca @->
    2 wa =+n 
    ca ->pc
0x0050 .org
    \ embed execute
    0x4507 code<< \ dictionary header
    0x4558 code<< \ dictionary header
    0x0000 code<< \ link address
.label fnEXECUTE
    code-location @ 1+ 0x0000FFFF and code<< \ code address for execute
    sp wa pop->
    fnRUN jmp
0x0140 .org
.label fnCOLON
    ir rs psh->
    wa ir ->
    fnNEXT jmp



\ ------------------------------------------
: group-mask ( mask group -- n ) 
  0x1F and ( mask group:5 )
  swap ( group:5 mask ) 
  8 u<< ( group:5 mask<<8 )
  or ;

0xFF 8 group-mask constant r64-r71
1 constant stdout
1 constant stdin
2 constant term-control

: write-io ( value addr -- ) !stio ;
: read-io ( addr dest -- ) !ldio ;
: print-character ( register -- ) 
  stdout t0 !set
  t0 write-io ;

: print-ichar ( char -- )
  t1 dup ( char t1 t1 ) 
  -rot ( t1 char t1 )
  !set
  print-character ;
: print-space ( -- ) 0x20 print-ichar ;
: print-newline ( -- ) 0xA print-ichar ;

.label LoadAddresses
    r64-r71 sp !pushg
    arg0 w0 !ld \ first word
    arg0 arg0 !1+
    arg0 w1 !ld \ second word
    w0 bChar0 bName !unpackh \ name and first character
    w1 bChar2 bChar1 !unpackh \ second and third character
    0x80 wMask !set \ get the flag bit
    bName wMask flag !and \ load the flag in r71
    1 wMask wMask !subi \ subtract one
    bName wMask bName !and \ get the length
    r64-r71 sp !popg
    !ret
