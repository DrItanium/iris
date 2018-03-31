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

: func: ( l -- ) 
  dup 
  label-here 
  jump-table-entry ;
: func; ( -- ) !return ;
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
.code
1 jump-table-end arg0 !subi
jump-table-start t0 !ld
lr t0 !brl
0x0100 .org
deflabel TableCall
TableCall func: 
    jump-table-start arg0 t4 !add \ first we need to combine the jump-table-start with arg0
                                  \ load the actual stored address
    t4 t0 indirect-register !br      \ perform the indirect jump
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
    0xA arg0 !set
    arg0 !putc 
    func;
deflabel SpaceChar
SpaceChar func:
    0x20 arg0 !set
    arg0 !putc
    func;
deflabel OperationLoop
OperationLoop func:
    !save-lr
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
    !restore-lr
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



