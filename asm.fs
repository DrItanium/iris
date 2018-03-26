( assembler words )
enum-start : Nop ( -- n ) literal ;
enum-next : Add ( -- n ) literal ;
enum-next : Sub ( -- n ) literal ;
enum-next : Mul ( -- n ) literal ;
enum-next : Div ( -- n ) literal ;
enum-next : Rem ( -- n ) literal ;
enum-next : ShiftLeft ( -- n ) literal ;
enum-next : ShiftRight ( -- n ) literal ;
enum-next : And ( -- n ) literal ;
enum-next : Or ( -- n ) literal ;
enum-next : Not ( -- n ) literal ;
enum-next : Xor ( -- n ) literal ;
enum-next : Nand ( -- n ) literal ;
enum-next : Nor ( -- n ) literal ;
enum-next : AddImmediate ( -- n ) literal ;
enum-next : SubImmediate ( -- n ) literal ;
enum-next : MulImmediate ( -- n ) literal ;
enum-next : DivImmediate ( -- n ) literal ;
enum-next : RemImmediate ( -- n ) literal ;
enum-next : ShiftLeftImmediate ( -- n ) literal ;
enum-next : ShiftRightImmediate ( -- n ) literal ;
enum-next : Min ( -- n ) literal ;
enum-next : Max ( -- n ) literal ;
enum-next : LogicalXor ( -- n ) literal ;
enum-next : LogicalNot ( -- n ) literal ;
enum-next : LogicalAnd ( -- n ) literal ;
enum-next : LogicalOr ( -- n ) literal ;
enum-next : LogicalNand ( -- n ) literal ;
enum-next : LogicalNor ( -- n ) literal ;
enum-next : Eq ( -- n ) literal ;
enum-next : EqImmediate ( -- n ) literal ;
enum-next : Neq ( -- n ) literal ;
enum-next : NeqImmediate ( -- n ) literal ;
enum-next : LessThan ( -- n ) literal ;
enum-next : LessThanImmediate ( -- n ) literal ;
enum-next : GreaterThan ( -- n ) literal ;
enum-next : GreaterThanImmediate ( -- n ) literal ;
enum-next : LessThanOrEqualTo ( -- n ) literal ;
enum-next : LessThanOrEqualToImmediate ( -- n ) literal ;
enum-next : GreaterThanOrEqualTo ( -- n ) literal ;
enum-next : GreaterThanOrEqualToImmediate ( -- n ) literal ;
enum-next : Move ( -- n ) literal ;
enum-next : Set ( -- n ) literal ;
enum-next : Swap ( -- n ) literal ;
enum-next : Load ( -- n ) literal ;
enum-next : LoadImmediate ( -- n ) literal ;
enum-next : LoadWithOffset ( -- n ) literal ;
enum-next : Store ( -- n ) literal ;
enum-next : StoreImmediate ( -- n ) literal ;
enum-next : StoreWithOffset ( -- n ) literal ;
enum-next : Push ( -- n ) literal ;
enum-next : PushImmediate ( -- n ) literal ;
enum-next : Pop ( -- n ) literal ;
enum-next : LoadCode ( -- n ) literal ;
enum-next : StoreCode ( -- n ) literal ;
enum-next : Branch ( -- n ) literal ;
enum-next : BranchAndLink ( -- n ) literal ;
enum-next : BranchIndirect ( -- n ) literal ;
enum-next : BranchIndirectLink ( -- n ) literal ;
enum-next : BranchConditional ( -- n ) literal ;
enum-next : BranchConditionalIndirect ( -- n ) literal ;
enum-next : BranchConditionalIndirectLink ( -- n ) literal ;
enum-next : TerminateExecution ( -- n ) literal ;
enum-done

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

: nop ( args* -- n ) NoArguments Nop bitwise-oru ;
: add ( args* -- n ) ThreeRegister Add bitwise-oru ;
: sub ( args* -- n ) ThreeRegister Sub bitwise-oru ;
: mul ( args* -- n ) ThreeRegister Mul bitwise-oru ;
: div ( args* -- n ) ThreeRegister Div bitwise-oru ;
: rem ( args* -- n ) ThreeRegister Rem bitwise-oru ;
: shl ( args* -- n ) ThreeRegister ShiftLeft bitwise-oru ;
: shr ( args* -- n ) ThreeRegister ShiftRight bitwise-oru ;
: and ( args* -- n ) ThreeRegister And bitwise-oru ;
: or ( args* -- n ) ThreeRegister Or bitwise-oru ;
: not ( args* -- n ) TwoRegister Not bitwise-oru ;
: xor ( args* -- n ) ThreeRegister Xor bitwise-oru ;
: nand ( args* -- n ) ThreeRegister Nand bitwise-oru ;
: nor ( args* -- n ) ThreeRegister Nor bitwise-oru ;
: addi ( args* -- n ) TwoRegisterWithImmediate AddImmediate bitwise-oru ;
: subi ( args* -- n ) TwoRegisterWithImmediate SubImmediate bitwise-oru ;
: muli ( args* -- n ) TwoRegisterWithImmediate MulImmediate bitwise-oru ;
: divi ( args* -- n ) TwoRegisterWithImmediate DivImmediate bitwise-oru ;
: remi ( args* -- n ) TwoRegisterWithImmediate RemImmediate bitwise-oru ;
: shli ( args* -- n ) TwoRegisterWithImmediate ShiftLeftImmediate bitwise-oru ;
: shri ( args* -- n ) TwoRegisterWithImmediate ShiftRightImmediate bitwise-oru ;
: min ( args* -- n ) ThreeRegister Min bitwise-oru ;
: max ( args* -- n ) ThreeRegister Max bitwise-oru ;
: lxor ( args* -- n ) ThreeRegister LogicalXor bitwise-oru ;
: lnot ( args* -- n ) TwoRegister LogicalNot bitwise-oru ;
: land ( args* -- n ) ThreeRegister LogicalAnd bitwise-oru ;
: lor ( args* -- n ) ThreeRegister LogicalOr bitwise-oru ;
: lnand ( args* -- n ) ThreeRegister LogicalNand bitwise-oru ;
: lnor ( args* -- n ) ThreeRegister LogicalNor bitwise-oru ;
: eq ( args* -- n ) ThreeRegister Eq bitwise-oru ;
: eqi ( args* -- n ) TwoRegisterWithImmediate EqImmediate bitwise-oru ;
: neq ( args* -- n ) ThreeRegister Neq bitwise-oru ;
: neqi ( args* -- n ) TwoRegisterWithImmediate NeqImmediate bitwise-oru ;
: lt ( args* -- n ) ThreeRegister LessThan bitwise-oru ;
: lti ( args* -- n ) TwoRegisterWithImmediate LessThanImmediate bitwise-oru ;
: gt ( args* -- n ) ThreeRegister GreaterThan bitwise-oru ;
: gti ( args* -- n ) TwoRegisterWithImmediate GreaterThanImmediate bitwise-oru ;
: le ( args* -- n ) ThreeRegister LessThanOrEqualTo bitwise-oru ;
: lei ( args* -- n ) TwoRegisterWithImmediate LessThanOrEqualToImmediate bitwise-oru ;
: ge ( args* -- n ) ThreeRegister GreaterThanOrEqualTo bitwise-oru ;
: gei ( args* -- n ) TwoRegisterWithImmediate GreaterThanOrEqualToImmediate bitwise-oru ;
: move ( args* -- n ) TwoRegister Move bitwise-oru ;
: set ( args* -- n ) OneRegisterWithImmediate Set bitwise-oru ;
: swap ( args* -- n ) TwoRegister Swap bitwise-oru ;
: ld ( args* -- n ) TwoRegister Load bitwise-oru ;
: ldi ( args* -- n ) OneRegisterWithImmediate LoadImmediate bitwise-oru ;
: ldwo ( args* -- n ) TwoRegisterWithImmediate LoadWithOffset bitwise-oru ;
: st ( args* -- n ) TwoRegister Store bitwise-oru ;
: sti ( args* -- n ) OneRegisterWithImmediate StoreImmediate bitwise-oru ;
: stwo ( args* -- n ) TwoRegisterWithImmediate StoreWithOffset bitwise-oru ;
: push ( args* -- n ) TwoRegister Push bitwise-oru ;
: pushi ( args* -- n ) OneRegisterWithImmediate PushImmediate bitwise-oru ;
: pop ( args* -- n ) TwoRegister Pop bitwise-oru ;
: ldc ( args* -- n ) ThreeRegister LoadCode bitwise-oru ;
: stc ( args* -- n ) ThreeRegister StoreCode bitwise-oru ;
: b ( args* -- n ) Immediate16 Branch bitwise-oru ;
: bl ( args* -- n ) OneRegisterWithImmediate BranchAndLink bitwise-oru ;
: br ( args* -- n ) OneRegister BranchIndirect bitwise-oru ;
: brl ( args* -- n ) TwoRegister BranchIndirectLink bitwise-oru ;
: bc ( args* -- n ) OneRegisterWithImmediate BranchConditional bitwise-oru ;
: bcr ( args* -- n ) TwoRegister BranchConditionalIndirect bitwise-oru ;
: bcrl ( args* -- n ) ThreeRegister BranchConditionalIndirectLink bitwise-oru ;
: terminateExecution ( args* -- n ) OneRegister TerminateExecution bitwise-oru ;

close-input-file
