\ this iris program stores everything in the code section, this includes the
\ dictionary
\ the data section now acts as "core"
\ the stack section is the stack section...
\ the io section is still the io section
\ having a harvard architecture is absolutely insane, to get around this we 
\ have to become creative. Having 64kb for code and dictionary is plenty
" iris.fs" open-input-file
" forth.iris" {bin
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
10 constant top \ top of the stack
11 constant lower \ lower element of the stack
12 constant third \ third element of the stack
13 constant io \ io device number or address
14 constant ci \ core index number
15 constant ci1 \ second core index number
16 constant cond \ condition variable
( io devices )
{enum
enum: /dev/null 
enum: /dev/console0
enum: /dev/console1
enum: /dev/core-dump
enum: /dev/core-load
enum}

: !lw ( src dest -- ) 
  \ since this is inside the code section we have to get creative
  \ put the upper half into 
  zero swap !ldc ;
: !sw ( value addr -- )
  zero -rot !stc ;
\ generic computer instructions from threaded interpretive languages book
: @-> ( a b -- ) 
  \ the contents of the memory location word whose address is in register A
  \ are loaded into register B ( a 16-bit indirect fetch from A to B )
  !lw ;

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

: ->io ( reg -- ) io -> ;

: $-> ( value reg -- ) !set ;
: $->pc ( value -- ) pc $-> ;
: $->io ( value -- ) io $-> ;
: io-write ( src -- ) io !stio ;
: io-read  ( dest -- ) io swap !ldio ;
: printc ( char -- ) \ assume that console is already set
  t0 !set t0 io-write ;
: *printc ( addr -- ) 
  t0 !set
  t0 t1 !lw
  t1 io-write ;

: get-string-length ( src dest -- )
  swap ( dest src )
  t0 !lowerb ( dest )
  0x7F t1 !set
  t0 t1 rot !and ;
: *get-string-length ( addr dest -- )
  swap ( dest addr )
  t2 !lw
  t2 swap ( t2 dest )
  get-string-length ;


0x0000 .org
    0x8000 sp !set
    0xFFFF rs !set
    zero t0 !move
    0xFFFF t1 !set
.label PopulateLoop
    t0 t0 !st
    1 t0 t0 !addi
    t1 t0 cond !neq
    PopulateLoop addr16 cond !bc
    /dev/core-dump $->io 
    ci io-write
\ inner-interpreter words
    zero top ->
.label fnTERMINATE
    top !terminateExecution
0x0100 .org 
.label fnSEMI
    next-addr .data16 
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
    0x45584507 dictionary-header \ dictionary header
    0 link-address
.label fnEXECUTE
    next-addr execution-address \ code address for execute
    sp wa pop->
    fnRUN jmp
0x0140 .org
.label fnCOLON
    ir rs psh->
    wa ir ->
    fnNEXT jmp
.label fnBYE
    sp top pop->
    fnTERMINATE jmp
.label fnOK
  /dev/console0 $->io
  0x4F printc
  0x4B printc 
  0xA printc 

0x2000 .org
    \ dup dictionary entry
.label dictionaryDUP
    0x50554403 dictionary-header
    fnEXECUTE link-address
    next-addr execution-address
    sp ca pop->
    ca sp psh->
    ca sp psh->
    fnNEXT jmp
\ 0x2100 .org
    \ secondary defining keyword constant
\    0x4E4F4308 dictionary-header 
\    dictionaryDUP link-address
\    fnCOLON execution-address

bin}
;s
