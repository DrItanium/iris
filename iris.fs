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
: !equ ( args* -- n ) ThreeRegister AsmUnsignedEq asm<< ;
: !equi ( args* -- n ) TwoRegisterWithImmediate AsmUnsignedEqImmediate asm<< ;
: !nequ ( args* -- n ) ThreeRegister AsmUnsignedNeq asm<< ;
: !nequi ( args* -- n ) TwoRegisterWithImmediate AsmUnsignedNeqImmediate asm<< ;
: !ltu ( args* -- n ) ThreeRegister AsmUnsignedLessThan asm<< ;
: !ltui ( args* -- n ) TwoRegisterWithImmediate AsmUnsignedLessThanImmediate asm<< ;
: !gtu ( args* -- n ) ThreeRegister AsmUnsignedGreaterThan asm<< ;
: !gtui ( args* -- n ) TwoRegisterWithImmediate AsmUnsignedGreaterThanImmediate asm<< ;
: !leu ( args* -- n ) ThreeRegister AsmUnsignedLessThanOrEqualTo asm<< ;
: !leui ( args* -- n ) TwoRegisterWithImmediate AsmUnsignedLessThanOrEqualToImmediate asm<< ;
: !geu ( args* -- n ) ThreeRegister AsmUnsignedGreaterThanOrEqualTo asm<< ;
: !geui ( args* -- n ) TwoRegisterWithImmediate AsmUnsignedGreaterThanOrEqualToImmediate asm<< ;
: !andu ( args* -- n ) ThreeRegister AsmUnsignedAnd asm<< ;
: !oru ( args* -- n ) ThreeRegister AsmUnsignedOr asm<< ;
: !notu ( args* -- n ) TwoRegister AsmUnsignedNot asm<< ;
: !xoru ( args* -- n ) ThreeRegister AsmUnsignedXor asm<< ;
: !nandu ( args* -- n ) ThreeRegister AsmUnsignedNand asm<< ;
: !noru ( args* -- n ) ThreeRegister AsmUnsignedNor asm<< ;

: .data16 ( n -- ) addr16 current-location code<< ;
: .data32 ( n -- ) addr32 current-location code<< ;
: dictionary-header ( n -- ) .data32 ;
: link-address ( addr -- ) .data16 ;
: execution-address ( addr -- ) .data16 ;

;s
