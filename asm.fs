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
: {asm ( a -- ) {bin ;
: asm} ( -- ) bin} ;
variable dataLoc 
variable codeLoc 
variable stackLoc 
variable currentsection
variable sectionid

{enum
 : section:register ( -- n ) literal ; enum,
 : section:code ( -- n ) literal ; enum,
 : section:data ( -- n ) literal ; enum,
 : section:stack ( -- n ) literal ;
enum}

: deflabel ( -- ) variable ;
: get-current-address ( -- value ) currentsection @ @ ;
: label-here ( variable -- ) get-current-address swap ! ;
: current-section-var ( -- value ) currentsection @ ; 
: set-current-address ( value -- ) mask-immediate16 current-section-var ! ;
: .code ( -- ) codeLoc currentsection ! ;
: .data ( -- ) dataLoc currentsection ! ;
: .stack ( -- ) stackLoc currentsection ! ;
: .org ( addr -- ) set-current-address ;
: next-word ( -- ) get-current-address 1+ set-current-address ;
: section-entry ( section address value -- ) -rot swap bin<< bin<<q bin<<h ;
: .data16 ( value -- ) mask-immediate16 ;

{enum
: AsmNop ( -- n ) literal ; enum, 
: AsmAdd ( -- n ) literal ; enum, 
: AsmSub ( -- n ) literal ; enum, 
: AsmMul ( -- n ) literal ; enum, 
: AsmDiv ( -- n ) literal ; enum, 
: AsmRem ( -- n ) literal ; enum, 
: AsmShiftLeft ( -- n ) literal ; enum, 
: AsmShiftRight ( -- n ) literal ; enum, 
: AsmAnd ( -- n ) literal ; enum, 
: AsmOr ( -- n ) literal ; enum, 
: AsmNot ( -- n ) literal ; enum, 
: AsmXor ( -- n ) literal ; enum, 
: AsmNand ( -- n ) literal ; enum, 
: AsmNor ( -- n ) literal ; enum, 
: AsmAddImmediate ( -- n ) literal ; enum, 
: AsmSubImmediate ( -- n ) literal ; enum, 
: AsmMulImmediate ( -- n ) literal ; enum, 
: AsmDivImmediate ( -- n ) literal ; enum, 
: AsmRemImmediate ( -- n ) literal ; enum, 
: AsmShiftLeftImmediate ( -- n ) literal ; enum, 
: AsmShiftRightImmediate ( -- n ) literal ; enum, 
: AsmMin ( -- n ) literal ; enum, 
: AsmMax ( -- n ) literal ; enum, 
: AsmLogicalXor ( -- n ) literal ; enum, 
: AsmLogicalNot ( -- n ) literal ; enum, 
: AsmLogicalAnd ( -- n ) literal ; enum, 
: AsmLogicalOr ( -- n ) literal ; enum, 
: AsmLogicalNand ( -- n ) literal ; enum, 
: AsmLogicalNor ( -- n ) literal ; enum, 
: AsmEq ( -- n ) literal ; enum, 
: AsmEqImmediate ( -- n ) literal ; enum, 
: AsmNeq ( -- n ) literal ; enum, 
: AsmNeqImmediate ( -- n ) literal ; enum, 
: AsmLessThan ( -- n ) literal ; enum, 
: AsmLessThanImmediate ( -- n ) literal ; enum, 
: AsmGreaterThan ( -- n ) literal ; enum, 
: AsmGreaterThanImmediate ( -- n ) literal ; enum, 
: AsmLessThanOrEqualTo ( -- n ) literal ; enum, 
: AsmLessThanOrEqualToImmediate ( -- n ) literal ; enum, 
: AsmGreaterThanOrEqualTo ( -- n ) literal ; enum, 
: AsmGreaterThanOrEqualToImmediate ( -- n ) literal ; enum, 
: AsmMove ( -- n ) literal ; enum, 
: AsmSet ( -- n ) literal ; enum, 
: AsmSwap ( -- n ) literal ; enum, 
: AsmLoad ( -- n ) literal ; enum, 
: AsmLoadImmediate ( -- n ) literal ; enum, 
: AsmLoadWithOffset ( -- n ) literal ; enum, 
: AsmStore ( -- n ) literal ; enum, 
: AsmStoreImmediate ( -- n ) literal ; enum, 
: AsmStoreWithOffset ( -- n ) literal ; enum, 
: AsmPush ( -- n ) literal ; enum, 
: AsmPushImmediate ( -- n ) literal ; enum, 
: AsmPop ( -- n ) literal ; enum, 
: AsmLoadCode ( -- n ) literal ; enum, 
: AsmStoreCode ( -- n ) literal ; enum, 
: AsmBranch ( -- n ) literal ; enum, 
: AsmBranchAndLink ( -- n ) literal ; enum, 
: AsmBranchIndirect ( -- n ) literal ; enum, 
: AsmBranchIndirectLink ( -- n ) literal ; enum, 
: AsmBranchConditional ( -- n ) literal ; enum, 
: AsmBranchConditionalIndirect ( -- n ) literal ; enum, 
: AsmBranchConditionalIndirectLink ( -- n ) literal ; enum, 
: AsmTerminateExecution ( -- n ) literal ;
enum}

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


.stack 0 .org 
.data 0 .org
.code 0 .org

close-input-file
