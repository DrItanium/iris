" misc/forth_interpreter/basics.fs" open-input-file
( assembler words )
: addr16 ( n -- n ) 0xFFFF and ;
: addr8 ( n -- n ) 0xFF and ;
: addr6 ( n -- n ) 0x1F and ;
: position-byte ( reg shift -- reg<<shift ) 
  swap addr8 
  swap ( reg shift -- masked-reg shift )
  u<< ( reg shift -- reg<<shift ) ;
: position-reg ( reg shift -- reg<<shift ) swap addr6 swap u<< ;
: destination-register ( reg -- shifted-reg ) 8 position-reg ;
: source-register ( reg -- shifted-reg ) 14 position-reg ;
: source2-register ( reg -- shifted-reg ) 20 position-reg ;
: source3-register ( reg -- shifted-reg ) 26 position-reg ;
: imm6 ( imm6 -- shifted-imm6 ) source3-register ;
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

: section-entry ( value address section -- ) bin<<q bin<<q bin<<h ;
0 constant register-section-id
1 constant code-section-id
2 constant core-section-id
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
  bin<<q 
  increment-location ;
: d16<< ( v -- ) 
  addr16 current-location code<< ;
: asm<< ( a b -- ) 
  or
  dup ( n n )
  \ output the lower half
  d16<< 
  \ output the upper half
  16 >>u d16<< ;

{enum
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
enum: AsmLoad
enum: AsmLoadImmediate
enum: AsmStore
enum: AsmStoreImmediate
enum: AsmPush
enum: AsmPushImmediate
enum: AsmPop
enum: AsmLoadCore
enum: AsmStoreCore
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
enum: AsmGetUpperByte
enum: AsmGetLowerByte
enum: AsmUnpackHalves
enum: AsmUnsignedEq
enum: AsmUnsignedEqImmediate
enum: AsmUnsignedNeq
enum: AsmUnsignedNeqImmediate
enum: AsmUnsignedLessThan
enum: AsmUnsignedLessThanImmediate
enum: AsmUnsignedGreaterThan
enum: AsmUnsignedGreaterThanImmediate
enum: AsmUnsignedLessThanOrEqualTo
enum: AsmUnsignedLessThanOrEqualToImmediate
enum: AsmUnsignedGreaterThanOrEqualTo
enum: AsmUnsignedGreaterThanOrEqualToImmediate
enum: AsmUnsignedAnd
enum: AsmUnsignedOr
enum: AsmUnsignedNot
enum: AsmUnsignedXor
enum: AsmUnsignedNand
enum: AsmUnsignedNor
enum: AsmUnsignedMin
enum: AsmUnsignedMax
enum: AsmReadToken
enum: AsmWriteCodeRangeToIO
enum: AsmNumberRoutine
enum: AsmReadRangeFromIOIntoCode
enum}




: !add ( args* -- ) ThreeRegister AsmAdd asm<< ;
: !sub ( args* -- ) ThreeRegister AsmSub asm<< ;
: !mul ( args* -- ) ThreeRegister AsmMul asm<< ;
: !div ( args* -- ) ThreeRegister AsmDiv asm<< ;
: !rem ( args* -- ) ThreeRegister AsmRem asm<< ;
: !shl ( args* -- ) ThreeRegister AsmShiftLeft asm<< ;
: !shr ( args* -- ) ThreeRegister AsmShiftRight asm<< ;
: !and ( args* -- ) ThreeRegister AsmAnd asm<< ;
: !or ( args* -- ) ThreeRegister AsmOr asm<< ;
: !not ( args* -- ) TwoRegister AsmNot asm<< ;
: !xor ( args* -- ) ThreeRegister AsmXor asm<< ;
: !nand ( args* -- ) ThreeRegister AsmNand asm<< ;
: !nor ( args* -- ) ThreeRegister AsmNor asm<< ;
: !addi ( args* -- ) TwoRegisterWithImmediate AsmAddImmediate asm<< ;
: !subi ( args* -- ) TwoRegisterWithImmediate AsmSubImmediate asm<< ;
: !muli ( args* -- ) TwoRegisterWithImmediate AsmMulImmediate asm<< ;
: !divi ( args* -- ) TwoRegisterWithImmediate AsmDivImmediate asm<< ;
: !remi ( args* -- ) TwoRegisterWithImmediate AsmRemImmediate asm<< ;
: !shli ( args* -- ) TwoRegisterWithImmediate AsmShiftLeftImmediate asm<< ;
: !shri ( args* -- ) TwoRegisterWithImmediate AsmShiftRightImmediate asm<< ;
: !min ( args* -- ) ThreeRegister AsmMin asm<< ;
: !max ( args* -- ) ThreeRegister AsmMax asm<< ;
: !lxor ( args* -- ) ThreeRegister AsmLogicalXor asm<< ;
: !lnot ( args* -- ) TwoRegister AsmLogicalNot asm<< ;
: !land ( args* -- ) ThreeRegister AsmLogicalAnd asm<< ;
: !lor ( args* -- ) ThreeRegister AsmLogicalOr asm<< ;
: !lnand ( args* -- ) ThreeRegister AsmLogicalNand asm<< ;
: !lnor ( args* -- ) ThreeRegister AsmLogicalNor asm<< ;
: !eq ( args* -- ) ThreeRegister AsmEq asm<< ;
: !eqi ( args* -- ) TwoRegisterWithImmediate AsmEqImmediate asm<< ;
: !neq ( args* -- ) ThreeRegister AsmNeq asm<< ;
: !neqi ( args* -- ) TwoRegisterWithImmediate AsmNeqImmediate asm<< ;
: !lt ( args* -- ) ThreeRegister AsmLessThan asm<< ;
: !lti ( args* -- ) TwoRegisterWithImmediate AsmLessThanImmediate asm<< ;
: !gt ( args* -- ) ThreeRegister AsmGreaterThan asm<< ;
: !gti ( args* -- ) TwoRegisterWithImmediate AsmGreaterThanImmediate asm<< ;
: !le ( args* -- ) ThreeRegister AsmLessThanOrEqualTo asm<< ;
: !lei ( args* -- ) TwoRegisterWithImmediate AsmLessThanOrEqualToImmediate asm<< ;
: !ge ( args* -- ) ThreeRegister AsmGreaterThanOrEqualTo asm<< ;
: !gei ( args* -- ) TwoRegisterWithImmediate AsmGreaterThanOrEqualToImmediate asm<< ;
: !move ( args* -- ) TwoRegister AsmMove asm<< ;
: !set ( args* -- ) OneRegisterWithImmediate AsmSet asm<< ;
: !ld ( args* -- ) TwoRegister AsmLoad asm<< ;
: !ldi ( args* -- ) OneRegisterWithImmediate AsmLoadImmediate asm<< ;
: !st ( args* -- ) TwoRegister AsmStore asm<< ;
: !sti ( args* -- ) OneRegisterWithImmediate AsmStoreImmediate asm<< ;
: !push ( args* -- ) TwoRegister AsmPush asm<< ;
: !pushi ( args* -- ) OneRegisterWithImmediate AsmPushImmediate asm<< ;
: !pop ( args* -- ) TwoRegister AsmPop asm<< ;
: !ld.c ( args* -- ) TwoRegister AsmLoadCore asm<< ;
: !st.c ( args* -- ) TwoRegister AsmStoreCore asm<< ;
: !b ( args* -- ) Immediate16 AsmBranch asm<< ;
: !bl ( args* -- ) OneRegisterWithImmediate AsmBranchAndLink asm<< ;
: !br ( args* -- ) OneRegister AsmBranchIndirect asm<< ;
: !brl ( args* -- ) TwoRegister AsmBranchIndirectLink asm<< ;
: !bc ( args* -- ) OneRegisterWithImmediate AsmBranchConditional asm<< ;
: !bcr ( args* -- ) TwoRegister AsmBranchConditionalIndirect asm<< ;
: !bcrl ( args* -- ) ThreeRegister AsmBranchConditionalIndirectLink asm<< ;
: !terminateExecution ( args* -- ) OneRegister AsmTerminateExecution asm<< ;
: !ldio ( args* -- ) TwoRegister AsmLoadIO asm<< ;
: !stio ( args* -- ) TwoRegister AsmStoreIO asm<< ;
: !upperb ( args* -- ) TwoRegister AsmGetUpperByte asm<< ;
: !lowerb ( args* -- ) TwoRegister AsmGetLowerByte asm<< ;
: !unpackh ( args* -- ) ThreeRegister AsmUnpackHalves asm<< ;
: !equ ( args* -- ) ThreeRegister AsmUnsignedEq asm<< ;
: !equi ( args* -- ) TwoRegisterWithImmediate AsmUnsignedEqImmediate asm<< ;
: !nequ ( args* -- ) ThreeRegister AsmUnsignedNeq asm<< ;
: !nequi ( args* -- ) TwoRegisterWithImmediate AsmUnsignedNeqImmediate asm<< ;
: !ltu ( args* -- ) ThreeRegister AsmUnsignedLessThan asm<< ;
: !ltui ( args* -- ) TwoRegisterWithImmediate AsmUnsignedLessThanImmediate asm<< ;
: !gtu ( args* -- ) ThreeRegister AsmUnsignedGreaterThan asm<< ;
: !gtui ( args* -- ) TwoRegisterWithImmediate AsmUnsignedGreaterThanImmediate asm<< ;
: !leu ( args* -- ) ThreeRegister AsmUnsignedLessThanOrEqualTo asm<< ;
: !leui ( args* -- ) TwoRegisterWithImmediate AsmUnsignedLessThanOrEqualToImmediate asm<< ;
: !geu ( args* -- ) ThreeRegister AsmUnsignedGreaterThanOrEqualTo asm<< ;
: !geui ( args* -- ) TwoRegisterWithImmediate AsmUnsignedGreaterThanOrEqualToImmediate asm<< ;
: !andu ( args* -- ) ThreeRegister AsmUnsignedAnd asm<< ;
: !oru ( args* -- ) ThreeRegister AsmUnsignedOr asm<< ;
: !notu ( args* -- ) TwoRegister AsmUnsignedNot asm<< ;
: !xoru ( args* -- ) ThreeRegister AsmUnsignedXor asm<< ;
: !nandu ( args* -- ) ThreeRegister AsmUnsignedNand asm<< ;
: !noru ( args* -- ) ThreeRegister AsmUnsignedNor asm<< ;
: !minu ( src2 src dest -- ) ThreeRegister AsmUnsignedMin asm<< ;
: !maxu ( src2 src dest -- ) ThreeRegister AsmUnsignedMax asm<< ;
: !readtok ( src2 src dest -- ) ThreeRegister AsmReadToken asm<< ;
: !write-code-range-to-io ( rlen rstart rioaddr -- ) ThreeRegister AsmWriteCodeRangeToIO asm<< ;
: !number-routine ( address result flag -- ) ThreeRegister AsmNumberRoutine asm<< ;
: !read-io-to-code-range ( count terminator dest -- ) ThreeRegister AsmReadRangeFromIOIntoCode asm<< ;


: .data16 ( n -- ) addr16 current-location code<< ;
{enum
\ registers that are used by the core internally
enum: zero
enum: error-code
enum: terminator \ terminator character
enum: nbase \ numeric base
enum: sp0 \ stack pointer 0
enum: sp1 \ stack pointer 1
\ custom registers start
enum: cv \ condition variable
enum: lr \ link register
enum: at0 \ assembler temporary 0
enum: at1 \ assembler temporary 1
enum: at2 \ assembler temporary 2
enum: at3 \ assembler temporary 3
enum: ret0
enum: ret1
enum: arg0
enum: arg1
enum: arg2
enum: arg3
enum: io
enum: ci \ core index number
enum: fixed-registers-stop
enum}

: !nop ( -- ) zero zero zero !add ;
: !1+ ( reg -- ) 1 swap dup !addi ;
: !1- ( reg -- ) 1 swap dup !subi ;
: !zero ( reg -- ) zero swap !move ;
: !exit ( code -- ) 
  at0 ( code at0 ) 
  tuck ( at0 code at0 )
  !set 
  !terminateExecution ;

: !memswap ( addr0 addr1 -- )
  \ swap the contents of two memory cells in core
  2over ( addr0 addr1 addr0 addr1 )
  at0 swap at1 ( addr0 addr1 addr0 at0 addr1 at1 )
  !ld ( addr0 addr1 addr0 at0 )
  !ld \ we've loaded memory as needed at this point
      \ however, we now need to store at1 into addr0 and at0 into addr1
  ( addr0 addr1 )
  at0 !st ( addr0 )
  at1 !st ;



\ basic registers
\ io devices
{enum
enum: /dev/null 
enum: /dev/console0
enum: /dev/console1
enum: /dev/core-dump
enum: /dev/core-load
enum: /dev/dump-vm
enum}
\ concepts and other macro routines for doing crazy things
: $-> ( value reg -- ) !set ;
: $->io ( value -- ) io $-> ;
: !lw ( src dest -- ) !ld ;
: !sw ( value addr -- ) !st ;
: !swi ( imm addr -- ) !sti ;

: !ret ( register -- )
  !br ;

: io-write ( src -- ) io !stio ;
: io-read  ( dest -- ) io swap !ldio ;

: dump-core-id-in-register ( n -- )
  /dev/core-dump $->io
  io-write ;
: load-core-id-in-register ( n -- )
  /dev/core-load $->io
  io-write ;
: dump-core ( -- ) ci dump-core-id-in-register ;
: load-core ( -- ) ci load-core-id-in-register ;

: !call ( dest -- ) lr !bl ;

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

: jmp ( addr -- ) 
  \ unconditional jump to the address encoded into the instruction
  !b ;

: ->io ( reg -- ) io -> ;

: !push.sp0 ( reg -- ) sp0 !push ;
: !push.sp1 ( reg -- ) sp1 !push ;
: !pop.sp0 ( reg -- ) sp0 swap !pop ;
: !pop.sp1 ( reg -- ) sp1 swap !pop ;

: !eqz ( reg dest -- ) zero swap !eq ;
: !equz ( reg dest -- ) zero swap !equ ;
: !neqz ( reg dest -- ) zero swap !neq ;
: !nequz ( reg dest -- ) zero swap !nequ ;
: !gtz ( src dest -- ) zero -rot !gt ;
: !gtuz ( src dest -- ) zero -rot !gtu ;
: !ltz ( src dest -- ) zero -rot !lt ;
: !ltuz ( src dest -- ) zero -rot !ltu ;
: !gez ( src dest -- ) zero -rot !ge ;
: !geuz ( src dest -- ) zero -rot !geu ;
: !lez ( src dest -- ) zero -rot !le ;
: !leuz ( src dest -- ) zero -rot !leu ;

: !eqiz ( imm6 dest -- ) zero swap !eqi ;
: !equiz ( imm6 dest -- ) zero swap !equi ;
: !neqz ( imm6 dest -- ) zero swap !neqi ;
: !nequiz ( imm6 dest -- ) zero swap !nequi ;
: !gtiz ( imm6 dest -- ) zero -rot !gti ;
: !gtuiz ( imm6 dest -- ) zero -rot !gtui ;
: !ltiz ( imm6 dest -- ) zero -rot !lti ;
: !ltuiz ( imm6 dest -- ) zero -rot !ltui ;
: !geiz ( imm6 dest -- ) zero -rot !gei ;
: !geuiz ( imm6 dest -- ) zero -rot !geui ;
: !leiz ( imm6 dest -- ) zero -rot !lei ;
: !leuiz ( imm6 dest -- ) zero -rot !leui ;
: !bccv ( imm -- ) cv !bc ;
: !swap ( r0 r1 -- ) 
  dup ( r0 r1 r1 )
  at0 -> ( r0 r1 )
  over swap ( r0 r0 r1 )
  -> \ move r1 <- r0
  at0 swap ( at0 r0 )
  -> ;

;s
