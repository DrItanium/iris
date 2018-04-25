" misc/forth_interpreter/basics.fs" open-input-file
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
: !swap ( args* -- ) TwoRegister AsmSwap asm<< ;
: !ld ( args* -- ) TwoRegister AsmLoad asm<< ;
: !ldi ( args* -- ) OneRegisterWithImmediate AsmLoadImmediate asm<< ;
: !st ( args* -- ) TwoRegister AsmStore asm<< ;
: !sti ( args* -- ) OneRegisterWithImmediate AsmStoreImmediate asm<< ;
: !push ( args* -- ) TwoRegister AsmPush asm<< ;
: !pushi ( args* -- ) OneRegisterWithImmediate AsmPushImmediate asm<< ;
: !pop ( args* -- ) TwoRegister AsmPop asm<< ;
: !ldc ( args* -- ) ThreeRegister AsmLoadCode asm<< ;
: !stc ( args* -- ) ThreeRegister AsmStoreCode asm<< ;
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
: !pushg ( args* -- ) OneRegisterWithImmediate AsmSaveGroupOfRegisters asm<< ;
: !popg ( args* -- ) OneRegisterWithImmediate AsmRestoreGroupOfRegisters asm<< ;
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

: .data16 ( n -- ) addr16 current-location code<< ;
: .data32 ( n -- ) addr32 current-location code<< ;
: dictionary-header ( n -- ) .data32 ;
: link-address ( addr -- ) .data16 ;
: execution-address ( addr -- ) .data16 ;
{enum
enum: zero
enum: cv \ condition variable
enum: lr \ link register
enum: ctr \ count register
enum: at0 \ assembler temporary 0
enum: at1 \ assembler temporary 1
enum: fixed-registers-stop
enum}

: !nop ( -- ) zero zero zero !add ;
: !1+ ( reg -- ) 1 swap dup !add ;
: !zero ( reg -- ) zero swap !move ;
: !bccv ( dest -- )
  \ use the cv register
  cv !bc ;
: !eqz ( reg -- )
  zero cv !eq ;
: !beqz ( dest reg -- )
  !eqz
  !bccv ;
: !neqz ( reg -- ) 
  \ this will emit ?reg zero cond !neq
  zero cv !neq ;
: !bneqz ( dest reg -- )
  \ first emit the neqz call
  !neqz ( dest )
  !bccv ;
: !if ( on-false on-true f -- ) 
  \ a two way branch
  !bcr  ( of )
  \ otherwise, always goto the false condition
  !br ;
: !ifl ( link on-false on-true f )
  >r \ push the flag onto the return stack
  rot ( of ot l )
  dup ( of ot l l )
  rot ( of l l ot )
  r> ( of l l ot f )
  !bcrl ( of l )
  swap ( l of )
  !brl ;
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
enum}

;s
