" misc/forth_interpreter/basics.fs" open-input-file
( assembler words )
: addr16 ( n -- n ) 0xFFFF and ;
: addr12 ( n -- n ) 0xFFF and ;
: addr6 ( n -- n ) 0x1F and ;
: position-reg ( reg shift -- reg<<shift ) swap addr6 swap u<< ;
: destination-register ( reg -- shifted-reg ) 8 position-reg ;
: source-register ( reg -- shifted-reg ) 14 position-reg ;
: source2-register ( reg -- shifted-reg ) 20 position-reg ;
: source3-register ( reg -- shifted-reg ) 26 position-reg ;
: imm6 ( imm6 -- shifted-imm6 ) source3-register ;
: imm12 ( imm12 -- shifted-imm12 ) addr12 20 u<< ;
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

: FourRegister ( src3 src2 src dest -- value )
  ThreeRegister ( src3 src2 src dest -- src3 value )
  swap
  source3-register
  or ;

: Immediate16 ( imm16 -- value ) imm16 ;

: OneRegisterWithImmediate ( imm16 dest -- value ) 
  OneRegister
  swap
  Immediate16
  or ;

: TwoRegisterWithImmediate ( imm12 src dest -- value ) 
  TwoRegister ( imm12 value )
  swap
  imm12
  or ;
: ThreeRegisterWithImmediate ( imm6 src2 src dest -- value )
  ThreeRegister ( imm6 value )
  swap 
  imm6
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
  16 u>> d16<< ;

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
enum: AsmMin
enum: AsmMax
enum: AsmLogicalXor
enum: AsmLogicalNot
enum: AsmLogicalAnd
enum: AsmLogicalOr
enum: AsmLogicalNand
enum: AsmLogicalNor
enum: AsmEq
enum: AsmNeq
enum: AsmLessThan
enum: AsmGreaterThan
enum: AsmLessThanOrEqualTo
enum: AsmGreaterThanOrEqualTo
enum: AsmSet
enum: AsmLoad
enum: AsmStore
enum: AsmPush
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
enum: AsmUnsignedEq
enum: AsmUnsignedNeq
enum: AsmUnsignedLessThan
enum: AsmUnsignedGreaterThan
enum: AsmUnsignedLessThanOrEqualTo
enum: AsmUnsignedGreaterThanOrEqualTo
enum: AsmUnsignedAnd
enum: AsmUnsignedOr
enum: AsmUnsignedNot
enum: AsmUnsignedXor
enum: AsmUnsignedNand
enum: AsmUnsignedNor
enum: AsmUnsignedMin
enum: AsmUnsignedMax
enum: AsmUnsignedAdd
enum: AsmUnsignedSub
enum: AsmUnsignedMul
enum: AsmUnsignedDiv
enum: AsmUnsignedRem
enum: AsmUnsignedShiftLeft
enum: AsmUnsignedShiftRight
enum: AsmReadToken
enum: AsmNumberRoutine
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
: !min ( args* -- ) ThreeRegister AsmMin asm<< ;
: !max ( args* -- ) ThreeRegister AsmMax asm<< ;
: !lxor ( args* -- ) ThreeRegister AsmLogicalXor asm<< ;
: !lnot ( args* -- ) TwoRegister AsmLogicalNot asm<< ;
: !land ( args* -- ) ThreeRegister AsmLogicalAnd asm<< ;
: !lor ( args* -- ) ThreeRegister AsmLogicalOr asm<< ;
: !lnand ( args* -- ) ThreeRegister AsmLogicalNand asm<< ;
: !lnor ( args* -- ) ThreeRegister AsmLogicalNor asm<< ;
: !eq ( args* -- ) ThreeRegister AsmEq asm<< ;
: !neq ( args* -- ) ThreeRegister AsmNeq asm<< ;
: !lt ( args* -- ) ThreeRegister AsmLessThan asm<< ;
: !gt ( args* -- ) ThreeRegister AsmGreaterThan asm<< ;
: !le ( args* -- ) ThreeRegister AsmLessThanOrEqualTo asm<< ;
: !ge ( args* -- ) ThreeRegister AsmGreaterThanOrEqualTo asm<< ;
: !set ( args* -- ) OneRegisterWithImmediate AsmSet asm<< ;
: !ld ( args* -- ) TwoRegister AsmLoad asm<< ;
: !st ( args* -- ) TwoRegister AsmStore asm<< ;
: !push ( args* -- ) TwoRegister AsmPush asm<< ;
: !pop ( args* -- ) TwoRegister AsmPop asm<< ;
: !ldc ( args* -- ) TwoRegister AsmLoadCore asm<< ;
: !stc ( args* -- ) TwoRegister AsmStoreCore asm<< ;
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
: !equ ( args* -- ) ThreeRegister AsmUnsignedEq asm<< ;
: !nequ ( args* -- ) ThreeRegister AsmUnsignedNeq asm<< ;
: !ltu ( args* -- ) ThreeRegister AsmUnsignedLessThan asm<< ;
: !gtu ( args* -- ) ThreeRegister AsmUnsignedGreaterThan asm<< ;
: !leu ( args* -- ) ThreeRegister AsmUnsignedLessThanOrEqualTo asm<< ;
: !geu ( args* -- ) ThreeRegister AsmUnsignedGreaterThanOrEqualTo asm<< ;
: !andu ( args* -- ) ThreeRegister AsmUnsignedAnd asm<< ;
: !oru ( args* -- ) ThreeRegister AsmUnsignedOr asm<< ;
: !notu ( args* -- ) TwoRegister AsmUnsignedNot asm<< ;
: !xoru ( args* -- ) ThreeRegister AsmUnsignedXor asm<< ;
: !nandu ( args* -- ) ThreeRegister AsmUnsignedNand asm<< ;
: !noru ( args* -- ) ThreeRegister AsmUnsignedNor asm<< ;
: !minu ( src2 src dest -- ) ThreeRegister AsmUnsignedMin asm<< ;
: !maxu ( src2 src dest -- ) ThreeRegister AsmUnsignedMax asm<< ;
: !readtok ( src2 src dest -- ) TwoRegister AsmReadToken asm<< ;
: !number-routine ( address result flag -- ) ThreeRegister AsmNumberRoutine asm<< ;
: !addu ( args* -- ) ThreeRegister AsmUnsignedAdd asm<< ;
: !subu ( args* -- ) ThreeRegister AsmUnsignedSub asm<< ;
: !mulu ( args* -- ) ThreeRegister AsmUnsignedMul asm<< ;
: !divu ( args* -- ) ThreeRegister AsmUnsignedDiv asm<< ;
: !remu ( args* -- ) ThreeRegister AsmUnsignedRem asm<< ;
: !shlu ( args* -- ) ThreeRegister AsmUnsignedShiftLeft asm<< ;
: !shru ( args* -- ) ThreeRegister AsmUnsignedShiftRight asm<< ;


: .data16 ( n -- ) addr16 current-location code<< ;
{enum
\ registers that are used by the core internally
enum: zero
enum: error-code
enum: terminator \ terminator character
enum: nbase \ numeric base
enum: sp0 \ stack pointer 0
enum: sp1 \ stack pointer 1
enum: vmsp \ stack pointer 2 ( generally the vm stack )

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

: !move ( a b -- ) zero swap !oru ;


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

: $->at0 ( imm -- ) at0 $-> ;
\ perform checks at this point to see if we shouldn't emit an instruction
: $->at0,at0 ( imm -- at0 ) dup 0= if zero else $->at0 at0 then ;

: $->at0,at0-arg3 ( imm a b -- at0 a b )
  rot ( a b imm )
  $->at0,at0 -rot ( at0 a b ) ;
: $->at0,at0-arg2 ( imm a -- at0 a )
  swap $->at0,at0 swap ;

: !addi ( imm src dest -- ) $->at0,at0-arg3 !add ;
: !subi ( imm src dest -- ) $->at0,at0-arg3 !sub ;
: !muli ( imm src dest -- ) $->at0,at0-arg3 !mul ;
: !divi ( imm src dest -- ) $->at0,at0-arg3 !div ;
: !remi ( imm src dest -- ) $->at0,at0-arg3 !rem ;
: !shli ( imm src dest -- ) $->at0,at0-arg3 !shl ;
: !shri ( imm src dest -- ) $->at0,at0-arg3 !shr ;
: !ldi ( imm dest -- ) $->at0,at0-arg2 !ld ;
: !sti ( imm dest -- ) $->at0,at0-arg2 !st ;
: !pushi ( imm sp -- ) $->at0,at0-arg2 !push ;

: !andi ( imm a b -- ) $->at0,at0-arg3 !and ;
: !ori ( imm a b -- ) $->at0,at0-arg3 !or ;
: !xori ( imm a b -- ) $->at0,at0-arg3 !xor ;
: !nori ( imm a b -- ) $->at0,at0-arg3 !nor ;
: !nandi ( imm a b -- ) $->at0,at0-arg3 !nand ;
: !andui ( imm a b -- ) $->at0,at0-arg3 !andu ;
: !orui ( imm a b -- ) $->at0,at0-arg3 !oru ;
: !xorui ( imm a b -- ) $->at0,at0-arg3 !xoru ;
: !norui ( imm a b -- ) $->at0,at0-arg3 !noru ;
: !nandui ( imm a b -- ) $->at0,at0-arg3 !nandu ;

: =+n ( n a -- ) 
  \ if immediate is zero then do nothing!
  over 0= if 2drop else
  \ The contents of register A are incremented by constant n
  dup  ( n a a ) 
  !addi then ;
: =-n ( n a -- )
  dup
  !subi ;
    

: !1+ ( reg -- ) 1 swap =+n ;
: !1- ( reg -- ) 1 swap =-n ;

: !eqi ( imm a b -- ) 
  $->at0,at0-arg3 ( at0 a b ) 
  !eq ;

: !neqi ( imm a b -- )
  $->at0,at0-arg3 ( at0 a b )
  !neq ;

: !gti ( imm a b -- )
  $->at0,at0-arg3 ( at0 a b )
  !gt ;

: !lti ( imm a b -- )
  $->at0,at0-arg3 ( at0 a b )
  !lt ;

: !gei ( imm a b -- )
  $->at0,at0-arg3 ( at0 a b )
  !ge ;
: !lei ( imm a b -- )
  $->at0,at0-arg3 ( at0 a b )
  !le ;

: !equi ( imm a b -- ) 
  $->at0,at0-arg3 ( at0 a b ) 
  !equ ;

: !nequi ( imm a b -- )
  $->at0,at0-arg3 ( at0 a b )
  !nequ ;

: !gtui ( imm a b -- )
  $->at0,at0-arg3 ( at0 a b )
  !gtu ;

: !ltui ( imm a b -- )
  $->at0,at0-arg3 ( at0 a b )
  !ltu ;

: !geui ( imm a b -- )
  $->at0,at0-arg3 ( at0 a b )
  !geu ;
: !leui ( imm a b -- )
  $->at0,at0-arg3 ( at0 a b )
  !leu ;

: zero-arg2 ( a -- zero a ) zero swap ;
: zero-arg3 ( a b -- zero a b ) zero -rot ;
: !eqz ( reg dest -- ) zero-arg2 !eq ;
: !equz ( reg dest -- ) zero-arg2 !equ ;
: !neqz ( reg dest -- ) zero-arg2 !neq ;
: !nequz ( reg dest -- ) zero-arg2 !nequ ;
: !gtz ( src dest -- ) zero-arg3 !gt ;
: !gtuz ( src dest -- ) zero-arg3 !gtu ;
: !ltz ( src dest -- ) zero-arg3 !lt ;
: !ltuz ( src dest -- ) zero-arg3 !ltu ;
: !gez ( src dest -- ) zero-arg3 !ge ;
: !geuz ( src dest -- ) zero-arg3 !geu ;
: !lez ( src dest -- ) zero-arg3 !le ;
: !leuz ( src dest -- ) zero-arg3 !leu ;

: !eqiz ( imm dest -- ) zero-arg2 !eqi ;
: !equiz ( imm dest -- ) zero-arg2 !equi ;
: !neqz ( imm dest -- ) zero-arg2 !neqi ;
: !nequiz ( imm dest -- ) zero-arg2 !nequi ;
: !gtiz ( imm dest -- ) zero-arg3 !gti ;
: !gtuiz ( imm dest -- ) zero-arg3 !gtui ;
: !ltiz ( imm dest -- ) zero-arg3 !lti ;
: !ltuiz ( imm dest -- ) zero-arg3 !ltui ;
: !geiz ( imm dest -- ) zero-arg3 !gei ;
: !geuiz ( imm dest -- ) zero-arg3 !geui ;
: !leiz ( imm dest -- ) zero-arg3 !lei ;
: !leuiz ( imm dest -- ) zero-arg3 !leui ;
: !bccv ( imm -- ) cv !bc ;
: !swap.r ( r0 r1 -- ) 
  2over = if
  dup ( r0 r1 r1 )
  at0 -> ( r0 r1 )
  over swap ( r0 r0 r1 )
  -> \ move r1 <- r0
  at0 swap ( at0 r0 )
  -> 
  else
  2drop 
  then ;


: !addui ( imm src dest -- ) $->at0,at0-arg3 !addu ;
: !subui ( imm src dest -- ) $->at0,at0-arg3 !subu ;
: !mului ( imm src dest -- ) $->at0,at0-arg3 !mulu ;
: !divui ( imm src dest -- ) $->at0,at0-arg3 !divu ;
: !remui ( imm src dest -- ) $->at0,at0-arg3 !remu ;
: !shlui ( imm src dest -- ) $->at0,at0-arg3 !shlu ;
: !shrui ( imm src dest -- ) $->at0,at0-arg3 !shru ;

: !nop ( -- ) zero zero zero !add ;
: !zero ( reg -- ) zero-arg2 !move ;
: !exit ( code -- ) $->at0,at0 !terminateExecution ;

: !swi ( imm addr -- ) !sti ;
: !drop ( sp -- ) zero !pop ;
: !dup ( sp -- ) 
   at0 2over  ( sp at0 sp at0 )
   pop->
   swap 2over ( at0 sp at0 sp )
   psh->
   psh-> ;
: !2drop ( sp -- ) dup !drop !drop ;
: !swap.st ( sp -- ) 
  2dup ( sp sp sp )
  at1 over ( sp sp sp at1 sp )
  at0 ( sp sp sp at1 sp at0 )
  pop-> ( sp sp sp at1 ) \ top - at0
  pop-> ( sp sp ) \ lower - at1
  at0 swap ( sp at0 sp ) psh->
  at1 swap ( at1 sp ) psh-> ;

: !save-vmsp ( reg -- ) vmsp psh-> ;
: !restore-vmsp ( reg -- ) vmsp swap pop-> ;
  
;s
