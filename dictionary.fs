\ this iris program stores everything in the code section, this includes the
\ dictionary

\ the data section now acts as "core"
\ the stack section is the stack section...
\ the io section is still the io section
\ having a harvard architecture is absolutely insane, to get around this we 
\ have to become creative. Having 64kb for code and dictionary is plenty
( assembler words )
: mask-immediate16 ( value -- imm16 ) 0xFFFF bitwise-andu ;
: mask-imm8 ( reg -- masked-reg ) 0xFF bitwise-andu ;
: position-byte ( reg shift -- reg<<shift ) 
  swap mask-imm8 swap ( reg shift -- masked-reg shift )
  <<u ( reg shift -- reg<<shift ) ;
: destination-register ( reg -- shifted-reg ) 8 position-byte ;
: source-register ( reg -- shifted-reg ) 16 position-byte ;
: source2-register ( reg -- shifted-reg ) 24 position-byte ;
: imm8 ( imm8 -- shifted-imm8 ) source2-register ;
: imm16 ( imm16 -- shifted-imm16 ) mask-immediate16 16 <<u ;
: NoArguments ( -- 0 ) 0 ;
: OneRegister ( dest -- value ) destination-register ;

: TwoRegister ( src dest -- value ) 
  OneRegister
  swap
  source-register
  bitwise-oru ;


: ThreeRegister ( src2 src dest -- value )
  TwoRegister ( src2 src dest -- src2 value )
  swap
  source2-register
  bitwise-oru  ;
: Immediate16 ( imm16 -- value ) imm16 ;

: OneRegisterWithImmediate ( imm16 dest -- value ) 
  OneRegister
  swap
  Immediate16
  bitwise-oru ;

: TwoRegisterWithImmediate ( imm8 src dest -- value ) 
  TwoRegister ( imm8 src dest -- imm8 value )
  swap
  imm8 
  bitwise-oru ;

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



: !nop ( args* -- n ) NoArguments AsmNop or ;
: !add ( args* -- n ) ThreeRegister AsmAdd or ;
: !sub ( args* -- n ) ThreeRegister AsmSub or ;
: !mul ( args* -- n ) ThreeRegister AsmMul or ;
: !div ( args* -- n ) ThreeRegister AsmDiv or ;
: !rem ( args* -- n ) ThreeRegister AsmRem or ;
: !shl ( args* -- n ) ThreeRegister AsmShiftLeft or ;
: !shr ( args* -- n ) ThreeRegister AsmShiftRight or ;
: !and ( args* -- n ) ThreeRegister AsmAnd or ;
: !or ( args* -- n ) ThreeRegister AsmOr or ;
: !not ( args* -- n ) TwoRegister AsmNot or ;
: !xor ( args* -- n ) ThreeRegister AsmXor or ;
: !nand ( args* -- n ) ThreeRegister AsmNand or ;
: !nor ( args* -- n ) ThreeRegister AsmNor or ;
: !addi ( args* -- n ) TwoRegisterWithImmediate AsmAddImmediate or ;
: !subi ( args* -- n ) TwoRegisterWithImmediate AsmSubImmediate or ;
: !muli ( args* -- n ) TwoRegisterWithImmediate AsmMulImmediate or ;
: !divi ( args* -- n ) TwoRegisterWithImmediate AsmDivImmediate or ;
: !remi ( args* -- n ) TwoRegisterWithImmediate AsmRemImmediate or ;
: !shli ( args* -- n ) TwoRegisterWithImmediate AsmShiftLeftImmediate or ;
: !shri ( args* -- n ) TwoRegisterWithImmediate AsmShiftRightImmediate or ;
: !min ( args* -- n ) ThreeRegister AsmMin or ;
: !max ( args* -- n ) ThreeRegister AsmMax or ;
: !lxor ( args* -- n ) ThreeRegister AsmLogicalXor or ;
: !lnot ( args* -- n ) TwoRegister AsmLogicalNot or ;
: !land ( args* -- n ) ThreeRegister AsmLogicalAnd or ;
: !lor ( args* -- n ) ThreeRegister AsmLogicalOr or ;
: !lnand ( args* -- n ) ThreeRegister AsmLogicalNand or ;
: !lnor ( args* -- n ) ThreeRegister AsmLogicalNor or ;
: !eq ( args* -- n ) ThreeRegister AsmEq or ;
: !eqi ( args* -- n ) TwoRegisterWithImmediate AsmEqImmediate or ;
: !neq ( args* -- n ) ThreeRegister AsmNeq or ;
: !neqi ( args* -- n ) TwoRegisterWithImmediate AsmNeqImmediate or ;
: !lt ( args* -- n ) ThreeRegister AsmLessThan or ;
: !lti ( args* -- n ) TwoRegisterWithImmediate AsmLessThanImmediate or ;
: !gt ( args* -- n ) ThreeRegister AsmGreaterThan or ;
: !gti ( args* -- n ) TwoRegisterWithImmediate AsmGreaterThanImmediate or ;
: !le ( args* -- n ) ThreeRegister AsmLessThanOrEqualTo or ;
: !lei ( args* -- n ) TwoRegisterWithImmediate AsmLessThanOrEqualToImmediate or ;
: !ge ( args* -- n ) ThreeRegister AsmGreaterThanOrEqualTo or ;
: !gei ( args* -- n ) TwoRegisterWithImmediate AsmGreaterThanOrEqualToImmediate or ;
: !move ( args* -- n ) TwoRegister AsmMove or ;
: !set ( args* -- n ) OneRegisterWithImmediate AsmSet or ;
: !swap ( args* -- n ) TwoRegister AsmSwap or ;
: !ld ( args* -- n ) TwoRegister AsmLoad or ;
: !ldi ( args* -- n ) OneRegisterWithImmediate AsmLoadImmediate or ;
: !st ( args* -- n ) TwoRegister AsmStore or ;
: !sti ( args* -- n ) OneRegisterWithImmediate AsmStoreImmediate or ;
: !push ( args* -- n ) TwoRegister AsmPush or ;
: !pushi ( args* -- n ) OneRegisterWithImmediate AsmPushImmediate or ;
: !pop ( args* -- n ) TwoRegister AsmPop or ;
: !ldc ( args* -- n ) ThreeRegister AsmLoadCode or ;
: !stc ( args* -- n ) ThreeRegister AsmStoreCode or ;
: !b ( args* -- n ) Immediate16 AsmBranch or ;
: !bl ( args* -- n ) OneRegisterWithImmediate AsmBranchAndLink or ;
: !br ( args* -- n ) OneRegister AsmBranchIndirect or ;
: !brl ( args* -- n ) TwoRegister AsmBranchIndirectLink or ;
: !bc ( args* -- n ) OneRegisterWithImmediate AsmBranchConditional or ;
: !bcr ( args* -- n ) TwoRegister AsmBranchConditionalIndirect or ;
: !bcrl ( args* -- n ) ThreeRegister AsmBranchConditionalIndirectLink or ;
: !terminateExecution ( args* -- n ) OneRegister AsmTerminateExecution or ;
: !ldio ( args* -- n ) TwoRegister AsmLoadIO or ;
: !stio ( args* -- n ) TwoRegister AsmStoreIO or ;
: !pushg ( args* -- n ) OneRegisterWithImmediate AsmSaveGroupOfRegisters or ;
: !popg ( args* -- n ) OneRegisterWithImmediate AsmRestoreGroupOfRegisters or ;
: !upperb ( args* -- n ) TwoRegister AsmGetUpperByte or ;
: !lowerb ( args* -- n ) TwoRegister AsmGetLowerByte or ;
: !unpackh ( args* -- n ) ThreeRegister AsmUnpackHalves or ;

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
