\ this iris program stores everything in the code section, this includes the
\ dictionary

\ the data section now acts as "core"
\ the stack section is the stack section...
\ the io section is still the io section
\ having a harvard architecture is absolutely insane, to get around this we 
\ have to become creative. Having 64kb for code and dictionary is plenty
variable location
: .org ( value -- ) location ! ;
0 .org
: current-location ( -- n ) location @ ;
: .label ( -- n ) current-location constant ; 
: next-location ( -- n ) current-location 1+ ;
: addr16 ( n -- n ) 0xFFFF and ;
: addr32 ( n -- n ) 0xFFFFFFFF and ;
: increment-location ( -- ) next-location location ! ;
: next-addr ( -- n ) next-location addr16 ;
: code<< ( value -- ) 
  drop \ right now just drop the top of the stack
  increment-location ;
: .data16 ( n -- ) addr16 code<< ;
: .data32 ( n -- ) addr32 code<< ;
: dictionary-header ( n -- ) .data32 ;
: link-address ( addr -- ) .data16 ;
: execution-address ( addr -- ) .data16 ;
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

: !ld ( src dest -- ) 
  drop \ do this for now
  code<< ;
: !addi ( n src dest -- )
  2drop
  code<< ;

: !pop ( src dest -- )
  drop
  code<< ;
: !push ( src dest -- )
  drop
  code<< ;
: !move ( src dest -- )
  drop
  code<< ;
: !b ( dest -- )
  code<< ;

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
0x0100 .org 
.label fnSEMI
    next-addr code<<
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
