( 
    A test of the assembler system
    close-input-file must be the last word in the file, anything after it will not
    even be parsed so it is like a giant comment block there
)
: .data16 ( value -- ) dup variable? if @ then mask-immediate16 asm<< ;
deflabel JumpTableStart
deflabel JumpTableLocation
: increment-variable ( var -- ) 
  dup ( var var )
  @ ( var value ) 
  1+ ( var value+1 )
  swap ( value+1 var )
  ! ;
: jump-table-entry ( n -- ) 
  .data 
  JumpTableLocation .org@ 
  .data16 
  JumpTableLocation increment-variable ;

\ always save and restore the link register
: func: ( l -- ) 
  dup 
  label-here 
  jump-table-entry 
  !save-lr ;
: func; ( -- ) 
  !restore-lr 
  !return ;
: !pop2 ( dlower dtop -- ) 
  \ pop the top two items from the stack
  !top ( dlower )
  !top ;
{asm 
.data 
0x0000 .org
JumpTableStart label-here
JumpTableLocation label-here
0x7FFF sp register!
0x7FFF sp-bottom register!
0 sp2-bottom register!
0 zero register!
0 sp2 register!
1 one register!
2 two register!
.code
jump-table-end arg0 !1-
jump-table-start t0 !ld
lr t0 !brl
0x0100 .org
deflabel TableCall
TableCall func: 
    jump-table-start arg0 t1 !add \ first we need to combine the jump-table-start with arg0
                                  \ load the actual stored address
    t1 t0 indirect-register !br      \ perform the indirect jump
    func;
deflabel OperationDuplicate
OperationDuplicate func: 
    arg0 !top
    arg0 !save-param
    arg0 !save-param
    func;
deflabel OperationOver
OperationOver func:
    ( a b -- a b a )
    arg1 arg0 !pop2 
    arg1 !save-param
    arg0 !save-param
    arg1 !save-param
    func;
deflabel OperationRotate
OperationRotate func:
   ( a b c -- b c a )
   arg1 arg0 !pop2
   arg2 !top \ a
   arg1 !save-param \ b
   arg0 !save-param \ c
   arg2 !save-param \ a
   func;
deflabel OperationSwap
OperationSwap func: 
   ( a b -- b a )
   arg1 arg0 !pop2
   arg0 !save-param
   arg1 !save-param 
   func;
deflabel ClearParameterStack
ClearParameterStack func: 
   sp-bottom sp !move 
func;
deflabel ClearSubroutineStack
ClearSubroutineStack func:
    sp2-bottom sp2 !move
func;

deflabel ClearStacks
ClearStacks func:
    ClearParameterStack @ !call 
    ClearSubroutineStack @ !call 
func;

deflabel OperationReverseRotate
OperationReverseRotate func:
    OperationRotate @ !call
    OperationRotate @ !call
    func;

deflabel OperationLoad
OperationLoad func:
    ( addr - value )
    arg0 !top
    arg0 ret0 !ld
    !save-ret0
    func;

deflabel OperationStore
OperationStore func:
    ( src dest -- )
    arg0 !top \ dest
    arg1 !top \ src
    arg1 arg0 !st \ perform the store
    func;

deflabel OperationLoadCode
OperationLoadCode func:
    ( address -- upper lower )
    arg0 !top \ address
    arg0 ret1 ret0 !ldc
    ret1 !save-param
    ret0 !save-param
    func;

deflabel OperationStoreCode
OperationStoreCode func:
    ( upper lower dest -- )
    arg0 !top \ dest
    arg1 !top \ lower
    arg2 !top \ upper
    arg2 arg1 arg0 !stc 
    func;
deflabel BitwiseAnd 
BitwiseAnd func:
    ( a b -- c )
    arg0 !top \ b
    arg1 !top \ a
    arg0 arg1 ret0 !and 
    !save-ret0
    func;
deflabel BitwiseOr
BitwiseOr func:
    arg0 !top \ b
    arg1 !top \ a
    arg1 arg0 ret0 !or
    !save-ret0
    func;
deflabel EmitCharacter
EmitCharacter func:
    arg0 !top
    arg0 !putc
    func;
deflabel NewlineChar
NewlineChar func:
    0xA !save-parami
    EmitCharacter @ !call
    func;
deflabel SpaceChar
SpaceChar func:
    0x20 !save-parami
    EmitCharacter @ !call
    func;
deflabel OperationLoop
OperationLoop func:
    terminate-loop !save-subr
    loop-body !save-subr
    arg0 loop-body !move
    terminate-loop !clr-reg
    deflabel OperationLoopStart
    OperationLoopStart label-here
    lr loop-body !brl
    OperationLoopStart @ terminate-loop !bneqz
    loop-body !restore-subr
    terminate-loop !restore-subr
    func;
deflabel DecodeBits 
DecodeBits func:
    \ result = (value & mask) >> shift;
    \ arg0 - value
    \ arg1 - mask
    \ arg2 - shift
    \ ret0 - result
    arg0 arg1 ret0 !and
    arg2 ret0 ret0 !shr
    func;
deflabel EncodeBits
EncodeBits func:
    \ result = (value & ~mask) | (mask & (data << shift))
    \ ret0 - result
    \ arg0 - value
    \ arg1 - data
    \ arg2 - mask
    \ arg3 - shift
    arg3 arg1 arg1 !shl
    arg1 arg2 arg1 !and
    arg2 arg2 !not
    arg2 arg0 arg0 !and
    arg1 arg0 ret0 !or
    func;

deflabel GetUpperHalf
GetUpperHalf func:
    \ arg0 - value
    0xFF00 arg1 !set
    8 arg2 !set
    DecodeBits @ !call
    func;
deflabel GetLowerHalf
GetLowerHalf func:
    \ arg0 -value
    0x00FF arg1 !set
    arg2 !clr-reg
    DecodeBits @ !call
    func;
deflabel GetUpperLowerHalves
    \ arg0 - value
    GetUpperHalf @ !call 
    ret0 ret1 !move
    GetLowerHalf @ !call
    func;
deflabel OperationWriteOutCode
OperationWriteOutCode func:
    \ save lower and upper word to memory at code-address
    upper-word lower-word code-address !stc 
    func;
deflabel ShutdownProcessor
ShutdownProcessor func: 
    args1 !terminateExecution 
    func;
.code 0x0000 .org

JumpTableStart @ jump-table-start register!
JumpTableLocation @ jump-table-end register!
asm}
close-input-file



