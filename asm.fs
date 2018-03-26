( assembler words )

: mask-immediate16 ( value -- imm16 ) FFFF# bitwise-andu ;
: mask-imm8 ( reg -- masked-reg ) FF# bitwise-andu ;
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

: TwoRegister ( reg1 reg2 -- value ) 
  source-register ( dest src -- dest src-shifted )
  swap ( dest src-shifted -- src-shifted dest )
  destination-register ( src-shifted dest -- src-shifted dest-shifted )
  bitwise-oru ( src-shifted dest-shifted -- args ) ;

: ThreeRegister ( dest src src2 -- value )
  source2-register ( dest src src2 -- dest src src2-shift )
  swap ( dest src src2-shifted -- dest src2-shifted src )
  source-register ( dest src2-shifted src -- dest src2-shifted src-shifted )
  bitwise-oru ( dest src2-s src-s -- dest combined )
  swap ( dest combined -- combined dest )
  destination-register ( combined dest -- combined dest-s )
  bitwise-oru ( combined dest-s -- value ) ;
: Immediate16 ( imm16 -- value ) imm16 ;
: OneRegisterWithImmediate ( dest imm16 -- value ) 
  imm16
  swap
  destination-register
  bitwise-oru ;

: TwoRegisterWithImmediate ( dest src imm8 -- value ) 
  imm8 
  swap
  source-register
  bitwise-oru
  swap
  destination-register
  bitwise-oru ;

enum-start
: AsmNop ( -- n ) literal ;
enum-next : AsmAdd ( -- n ) literal ;
enum-next : AsmSub ( -- n ) literal ;
enum-next : AsmMul ( -- n ) literal ;
enum-next : AsmDiv ( -- n ) literal ;
enum-next : AsmRem ( -- n ) literal ;
enum-next : AsmShiftLeft ( -- n ) literal ;
enum-next : AsmShiftRight ( -- n ) literal ;
enum-next : AsmAnd ( -- n ) literal ;
enum-next : AsmOr ( -- n ) literal ;
enum-next : AsmNot ( -- n ) literal ;
enum-next : AsmXor ( -- n ) literal ;
enum-next : AsmNand ( -- n ) literal ;
enum-next : AsmNor ( -- n ) literal ;
enum-next : AsmAddImmediate ( -- n ) literal ;
enum-next : AsmSubImmediate ( -- n ) literal ;
enum-next : AsmMulImmediate ( -- n ) literal ;
enum-next : AsmDivImmediate ( -- n ) literal ;
enum-next : AsmRemImmediate ( -- n ) literal ;
enum-next : AsmShiftLeftImmediate ( -- n ) literal ;
enum-next : AsmShiftRightImmediate ( -- n ) literal ;
enum-next : AsmMin ( -- n ) literal ;
enum-next : AsmMax ( -- n ) literal ;
enum-next : AsmLogicalXor ( -- n ) literal ;
enum-next : AsmLogicalNot ( -- n ) literal ;
enum-next : AsmLogicalAnd ( -- n ) literal ;
enum-next : AsmLogicalOr ( -- n ) literal ;
enum-next : AsmLogicalNand ( -- n ) literal ;
enum-next : AsmLogicalNor ( -- n ) literal ;
enum-next : AsmEq ( -- n ) literal ;
enum-next : AsmEqImmediate ( -- n ) literal ;
enum-next : AsmNeq ( -- n ) literal ;
enum-next : AsmNeqImmediate ( -- n ) literal ;
enum-next : AsmLessThan ( -- n ) literal ;
enum-next : AsmLessThanImmediate ( -- n ) literal ;
enum-next : AsmGreaterThan ( -- n ) literal ;
enum-next : AsmGreaterThanImmediate ( -- n ) literal ;
enum-next : AsmLessThanOrEqualTo ( -- n ) literal ;
enum-next : AsmLessThanOrEqualToImmediate ( -- n ) literal ;
enum-next : AsmGreaterThanOrEqualTo ( -- n ) literal ;
enum-next : AsmGreaterThanOrEqualToImmediate ( -- n ) literal ;
enum-next : AsmMove ( -- n ) literal ;
enum-next : AsmSet ( -- n ) literal ;
enum-next : AsmSwap ( -- n ) literal ;
enum-next : AsmLoad ( -- n ) literal ;
enum-next : AsmLoadImmediate ( -- n ) literal ;
enum-next : AsmLoadWithOffset ( -- n ) literal ;
enum-next : AsmStore ( -- n ) literal ;
enum-next : AsmStoreImmediate ( -- n ) literal ;
enum-next : AsmStoreWithOffset ( -- n ) literal ;
enum-next : AsmPush ( -- n ) literal ;
enum-next : AsmPushImmediate ( -- n ) literal ;
enum-next : AsmPop ( -- n ) literal ;
enum-next : AsmLoadCode ( -- n ) literal ;
enum-next : AsmStoreCode ( -- n ) literal ;
enum-next : AsmBranch ( -- n ) literal ;
enum-next : AsmBranchAndLink ( -- n ) literal ;
enum-next : AsmBranchIndirect ( -- n ) literal ;
enum-next : AsmBranchIndirectLink ( -- n ) literal ;
enum-next : AsmBranchConditional ( -- n ) literal ;
enum-next : AsmBranchConditionalIndirect ( -- n ) literal ;
enum-next : AsmBranchConditionalIndirectLink ( -- n ) literal ;
enum-next : AsmTerminateExecution ( -- n ) literal ;
enum-done
: !nop ( args* -- n ) NoArguments AsmNop bitwise-oru ;
: !add ( args* -- n ) ThreeRegister AsmAdd bitwise-oru ;
: !sub ( args* -- n ) ThreeRegister AsmSub bitwise-oru ;
: !mul ( args* -- n ) ThreeRegister AsmMul bitwise-oru ;
: !div ( args* -- n ) ThreeRegister AsmDiv bitwise-oru ;
: !rem ( args* -- n ) ThreeRegister AsmRem bitwise-oru ;
: !shl ( args* -- n ) ThreeRegister AsmShiftLeft bitwise-oru ;
: !shr ( args* -- n ) ThreeRegister AsmShiftRight bitwise-oru ;
: !and ( args* -- n ) ThreeRegister AsmAnd bitwise-oru ;
: !or ( args* -- n ) ThreeRegister AsmOr bitwise-oru ;
: !not ( args* -- n ) TwoRegister AsmNot bitwise-oru ;
: !xor ( args* -- n ) ThreeRegister AsmXor bitwise-oru ;
: !nand ( args* -- n ) ThreeRegister AsmNand bitwise-oru ;
: !nor ( args* -- n ) ThreeRegister AsmNor bitwise-oru ;
: !addi ( args* -- n ) TwoRegisterWithImmediate AsmAddImmediate bitwise-oru ;
: !subi ( args* -- n ) TwoRegisterWithImmediate AsmSubImmediate bitwise-oru ;
: !muli ( args* -- n ) TwoRegisterWithImmediate AsmMulImmediate bitwise-oru ;
: !divi ( args* -- n ) TwoRegisterWithImmediate AsmDivImmediate bitwise-oru ;
: !remi ( args* -- n ) TwoRegisterWithImmediate AsmRemImmediate bitwise-oru ;
: !shli ( args* -- n ) TwoRegisterWithImmediate AsmShiftLeftImmediate bitwise-oru ;
: !shri ( args* -- n ) TwoRegisterWithImmediate AsmShiftRightImmediate bitwise-oru ;
: !min ( args* -- n ) ThreeRegister AsmMin bitwise-oru ;
: !max ( args* -- n ) ThreeRegister AsmMax bitwise-oru ;
: !lxor ( args* -- n ) ThreeRegister AsmLogicalXor bitwise-oru ;
: !lnot ( args* -- n ) TwoRegister AsmLogicalNot bitwise-oru ;
: !land ( args* -- n ) ThreeRegister AsmLogicalAnd bitwise-oru ;
: !lor ( args* -- n ) ThreeRegister AsmLogicalOr bitwise-oru ;
: !lnand ( args* -- n ) ThreeRegister AsmLogicalNand bitwise-oru ;
: !lnor ( args* -- n ) ThreeRegister AsmLogicalNor bitwise-oru ;
: !eq ( args* -- n ) ThreeRegister AsmEq bitwise-oru ;
: !eqi ( args* -- n ) TwoRegisterWithImmediate AsmEqImmediate bitwise-oru ;
: !neq ( args* -- n ) ThreeRegister AsmNeq bitwise-oru ;
: !neqi ( args* -- n ) TwoRegisterWithImmediate AsmNeqImmediate bitwise-oru ;
: !lt ( args* -- n ) ThreeRegister AsmLessThan bitwise-oru ;
: !lti ( args* -- n ) TwoRegisterWithImmediate AsmLessThanImmediate bitwise-oru ;
: !gt ( args* -- n ) ThreeRegister AsmGreaterThan bitwise-oru ;
: !gti ( args* -- n ) TwoRegisterWithImmediate AsmGreaterThanImmediate bitwise-oru ;
: !le ( args* -- n ) ThreeRegister AsmLessThanOrEqualTo bitwise-oru ;
: !lei ( args* -- n ) TwoRegisterWithImmediate AsmLessThanOrEqualToImmediate bitwise-oru ;
: !ge ( args* -- n ) ThreeRegister AsmGreaterThanOrEqualTo bitwise-oru ;
: !gei ( args* -- n ) TwoRegisterWithImmediate AsmGreaterThanOrEqualToImmediate bitwise-oru ;
: !move ( args* -- n ) TwoRegister AsmMove bitwise-oru ;
: !set ( args* -- n ) OneRegisterWithImmediate AsmSet bitwise-oru ;
: !swap ( args* -- n ) TwoRegister AsmSwap bitwise-oru ;
: !ld ( args* -- n ) TwoRegister AsmLoad bitwise-oru ;
: !ldi ( args* -- n ) OneRegisterWithImmediate AsmLoadImmediate bitwise-oru ;
: !ldwo ( args* -- n ) TwoRegisterWithImmediate AsmLoadWithOffset bitwise-oru ;
: !st ( args* -- n ) TwoRegister AsmStore bitwise-oru ;
: !sti ( args* -- n ) OneRegisterWithImmediate AsmStoreImmediate bitwise-oru ;
: !stwo ( args* -- n ) TwoRegisterWithImmediate AsmStoreWithOffset bitwise-oru ;
: !push ( args* -- n ) TwoRegister AsmPush bitwise-oru ;
: !pushi ( args* -- n ) OneRegisterWithImmediate AsmPushImmediate bitwise-oru ;
: !pop ( args* -- n ) TwoRegister AsmPop bitwise-oru ;
: !ldc ( args* -- n ) ThreeRegister AsmLoadCode bitwise-oru ;
: !stc ( args* -- n ) ThreeRegister AsmStoreCode bitwise-oru ;
: !b ( args* -- n ) Immediate16 AsmBranch bitwise-oru ;
: !bl ( args* -- n ) OneRegisterWithImmediate AsmBranchAndLink bitwise-oru ;
: !br ( args* -- n ) OneRegister AsmBranchIndirect bitwise-oru ;
: !brl ( args* -- n ) TwoRegister AsmBranchIndirectLink bitwise-oru ;
: !bc ( args* -- n ) OneRegisterWithImmediate AsmBranchConditional bitwise-oru ;
: !bcr ( args* -- n ) TwoRegister AsmBranchConditionalIndirect bitwise-oru ;
: !bcrl ( args* -- n ) ThreeRegister AsmBranchConditionalIndirectLink bitwise-oru ;
: !terminateExecution ( args* -- n ) OneRegister AsmTerminateExecution bitwise-oru ;

close-input-file
