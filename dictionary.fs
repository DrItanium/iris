\ this iris program stores everything in the code section, this includes the
\ dictionary
" misc/forth_interpreter/basics.fs" open-input-file
\ the data section now acts as "core"
\ the stack section is the stack section...
\ the io section is still the io section
\ having a harvard architecture is absolutely insane, to get around this we 
\ have to become creative. Having 64kb for code and dictionary is plenty
( assembler words )
: addr16 ( n -- n ) 0xFFFF and ;
: addr32 ( n -- n ) 0xFFFFFFFF and ;
: addr8 ( n -- n ) 0xFF and ;
: position-byte ( reg shift -- reg<<shift ) 
  swap addr8 
  swap ( reg shift -- masked-reg shift )
  u<< ( reg shift -- reg<<shift ) ;
: destination-register ( reg -- shifted-reg ) 8 position-byte ;
: source-register ( reg -- shifted-reg ) 16 position-byte ;
: source2-register ( reg -- shifted-reg ) 24 position-byte ;
: imm8 ( imm8 -- shifted-imm8 ) source2-register ;
: imm16 ( imm16 -- shifted-imm16 ) addr16 16 u<< ;
: NoArguments ( -- 0 ) 0 ;
: OneRegister ( dest -- value ) destination-register ;

: TwoRegister ( src dest -- value ) 
  OneRegister
  swap
  source-register
  or ;


: ThreeRegister ( src2 src dest -- value )
  TwoRegister ( src2 src dest -- src2 value )
  swap
  source2-register
  or ;

: Immediate16 ( imm16 -- value ) imm16 ;

: OneRegisterWithImmediate ( imm16 dest -- value ) 
  OneRegister
  swap
  Immediate16
  or ;

: TwoRegisterWithImmediate ( imm8 src dest -- value ) 
  TwoRegister ( imm8 src dest -- imm8 value )
  swap
  imm8 
  or ;

{enum
enum: AsmNop
enum: AsmAdd
enum: AsmSub
enum: AsmMul
enum: AsmDiv
enum: AsmRem
enum: AsmShiftLeft
enum: AsmShiftRight
enum: AsmAnd
enum: AsmOr
enum: AsmNot
enum: AsmXor
enum: AsmNand
enum: AsmNor
enum: AsmAddImmediate
enum: AsmSubImmediate
enum: AsmMulImmediate
enum: AsmDivImmediate
enum: AsmRemImmediate
enum: AsmShiftLeftImmediate
enum: AsmShiftRightImmediate
enum: AsmMin
enum: AsmMax
enum: AsmLogicalXor
enum: AsmLogicalNot
enum: AsmLogicalAnd
enum: AsmLogicalOr
enum: AsmLogicalNand
enum: AsmLogicalNor
enum: AsmEq
enum: AsmEqImmediate
enum: AsmNeq
enum: AsmNeqImmediate
enum: AsmLessThan
enum: AsmLessThanImmediate
enum: AsmGreaterThan
enum: AsmGreaterThanImmediate
enum: AsmLessThanOrEqualTo
enum: AsmLessThanOrEqualToImmediate
enum: AsmGreaterThanOrEqualTo
enum: AsmGreaterThanOrEqualToImmediate
enum: AsmMove
enum: AsmSet
enum: AsmSwap
enum: AsmLoad
enum: AsmLoadImmediate
enum: AsmStore
enum: AsmStoreImmediate
enum: AsmPush
enum: AsmPushImmediate
enum: AsmPop
enum: AsmLoadCode
enum: AsmStoreCode
enum: AsmBranch
enum: AsmBranchAndLink
enum: AsmBranchIndirect
enum: AsmBranchIndirectLink
enum: AsmBranchConditional
enum: AsmBranchConditionalIndirect
enum: AsmBranchConditionalIndirectLink
enum: AsmTerminateExecution
enum: AsmLoadIO
enum: AsmStoreIO
enum: AsmSaveGroupOfRegisters
enum: AsmRestoreGroupOfRegisters
enum: AsmGetUpperByte
enum: AsmGetLowerByte
enum: AsmUnpackHalves
enum}


: section-entry ( value address section -- ) bin<<q bin<<q bin<<h ;
1 constant code-section-id
variable location
: .org ( value -- ) location ! ;
0 .org
: current-location ( -- n ) location @ ;
: .label ( -- n ) current-location constant ; 
: next-location ( -- n ) current-location 1+ ;
: increment-location ( -- ) next-location location ! ;
: next-addr ( -- n ) next-location addr16 ;
: code<< ( value address -- ) 
  code-section-id bin<<q
  bin<<q
  bin<<h 
  increment-location ;
: asm<< ( a b -- ) or current-location code<< ;
: !nop ( args* -- n ) NoArguments AsmNop asm<< ;
: !add ( args* -- n ) ThreeRegister AsmAdd asm<< ;
: !sub ( args* -- n ) ThreeRegister AsmSub asm<< ;
: !mul ( args* -- n ) ThreeRegister AsmMul asm<< ;
: !div ( args* -- n ) ThreeRegister AsmDiv asm<< ;
: !rem ( args* -- n ) ThreeRegister AsmRem asm<< ;
: !shl ( args* -- n ) ThreeRegister AsmShiftLeft asm<< ;
: !shr ( args* -- n ) ThreeRegister AsmShiftRight asm<< ;
: !and ( args* -- n ) ThreeRegister AsmAnd asm<< ;
: !or ( args* -- n ) ThreeRegister AsmOr asm<< ;
: !not ( args* -- n ) TwoRegister AsmNot asm<< ;
: !xor ( args* -- n ) ThreeRegister AsmXor asm<< ;
: !nand ( args* -- n ) ThreeRegister AsmNand asm<< ;
: !nor ( args* -- n ) ThreeRegister AsmNor asm<< ;
: !addi ( args* -- n ) TwoRegisterWithImmediate AsmAddImmediate asm<< ;
: !subi ( args* -- n ) TwoRegisterWithImmediate AsmSubImmediate asm<< ;
: !muli ( args* -- n ) TwoRegisterWithImmediate AsmMulImmediate asm<< ;
: !divi ( args* -- n ) TwoRegisterWithImmediate AsmDivImmediate asm<< ;
: !remi ( args* -- n ) TwoRegisterWithImmediate AsmRemImmediate asm<< ;
: !shli ( args* -- n ) TwoRegisterWithImmediate AsmShiftLeftImmediate asm<< ;
: !shri ( args* -- n ) TwoRegisterWithImmediate AsmShiftRightImmediate asm<< ;
: !min ( args* -- n ) ThreeRegister AsmMin asm<< ;
: !max ( args* -- n ) ThreeRegister AsmMax asm<< ;
: !lxor ( args* -- n ) ThreeRegister AsmLogicalXor asm<< ;
: !lnot ( args* -- n ) TwoRegister AsmLogicalNot asm<< ;
: !land ( args* -- n ) ThreeRegister AsmLogicalAnd asm<< ;
: !lor ( args* -- n ) ThreeRegister AsmLogicalOr asm<< ;
: !lnand ( args* -- n ) ThreeRegister AsmLogicalNand asm<< ;
: !lnor ( args* -- n ) ThreeRegister AsmLogicalNor asm<< ;
: !eq ( args* -- n ) ThreeRegister AsmEq asm<< ;
: !eqi ( args* -- n ) TwoRegisterWithImmediate AsmEqImmediate asm<< ;
: !neq ( args* -- n ) ThreeRegister AsmNeq asm<< ;
: !neqi ( args* -- n ) TwoRegisterWithImmediate AsmNeqImmediate asm<< ;
: !lt ( args* -- n ) ThreeRegister AsmLessThan asm<< ;
: !lti ( args* -- n ) TwoRegisterWithImmediate AsmLessThanImmediate asm<< ;
: !gt ( args* -- n ) ThreeRegister AsmGreaterThan asm<< ;
: !gti ( args* -- n ) TwoRegisterWithImmediate AsmGreaterThanImmediate asm<< ;
: !le ( args* -- n ) ThreeRegister AsmLessThanOrEqualTo asm<< ;
: !lei ( args* -- n ) TwoRegisterWithImmediate AsmLessThanOrEqualToImmediate asm<< ;
: !ge ( args* -- n ) ThreeRegister AsmGreaterThanOrEqualTo asm<< ;
: !gei ( args* -- n ) TwoRegisterWithImmediate AsmGreaterThanOrEqualToImmediate asm<< ;
: !move ( args* -- n ) TwoRegister AsmMove asm<< ;
: !set ( args* -- n ) OneRegisterWithImmediate AsmSet asm<< ;
: !swap ( args* -- n ) TwoRegister AsmSwap asm<< ;
: !ld ( args* -- n ) TwoRegister AsmLoad asm<< ;
: !ldi ( args* -- n ) OneRegisterWithImmediate AsmLoadImmediate asm<< ;
: !st ( args* -- n ) TwoRegister AsmStore asm<< ;
: !sti ( args* -- n ) OneRegisterWithImmediate AsmStoreImmediate asm<< ;
: !push ( args* -- n ) TwoRegister AsmPush asm<< ;
: !pushi ( args* -- n ) OneRegisterWithImmediate AsmPushImmediate asm<< ;
: !pop ( args* -- n ) TwoRegister AsmPop asm<< ;
: !ldc ( args* -- n ) ThreeRegister AsmLoadCode asm<< ;
: !stc ( args* -- n ) ThreeRegister AsmStoreCode asm<< ;
: !b ( args* -- n ) Immediate16 AsmBranch asm<< ;
: !bl ( args* -- n ) OneRegisterWithImmediate AsmBranchAndLink asm<< ;
: !br ( args* -- n ) OneRegister AsmBranchIndirect asm<< ;
: !brl ( args* -- n ) TwoRegister AsmBranchIndirectLink asm<< ;
: !bc ( args* -- n ) OneRegisterWithImmediate AsmBranchConditional asm<< ;
: !bcr ( args* -- n ) TwoRegister AsmBranchConditionalIndirect asm<< ;
: !bcrl ( args* -- n ) ThreeRegister AsmBranchConditionalIndirectLink asm<< ;
: !terminateExecution ( args* -- n ) OneRegister AsmTerminateExecution asm<< ;
: !ldio ( args* -- n ) TwoRegister AsmLoadIO asm<< ;
: !stio ( args* -- n ) TwoRegister AsmStoreIO asm<< ;
: !pushg ( args* -- n ) OneRegisterWithImmediate AsmSaveGroupOfRegisters asm<< ;
: !popg ( args* -- n ) OneRegisterWithImmediate AsmRestoreGroupOfRegisters asm<< ;
: !upperb ( args* -- n ) TwoRegister AsmGetUpperByte asm<< ;
: !lowerb ( args* -- n ) TwoRegister AsmGetLowerByte asm<< ;
: !unpackh ( args* -- n ) ThreeRegister AsmUnpackHalves asm<< ;

: .data16 ( n -- ) addr16 current-location code<< ;
: .data32 ( n -- ) addr32 current-location code<< ;
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
10 constant top \ top of the stack
11 constant lower \ lower element of the stack
12 constant third \ third element of the stack
13 constant io \ io device number or address
14 constant ci \ core index number
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
    zero ci !move
    1 zero t0 !addi
    sp zero !st
    rs t0 !st
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
