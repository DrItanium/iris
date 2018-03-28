( 
    A test of the assembler system
    close-input-file must be the last word in the file, anything after it will not
    even be parsed so it is like a giant comment block there
)
: .data16 ( value -- ) dup variable? if @ then mask-immediate16 asm<< ;
deflabel ShutdownProcessor
deflabel OperationAdd
deflabel OperationSubtract
deflabel JumpTableStart
deflabel JumpTableLocation
deflabel OperationMultiply
deflabel TableCall
deflabel OperationPopParameter
deflabel OperationPushParameter
deflabel OperationDrop
deflabel OperationDuplicate
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

: {func ( l -- ) dup label-here jump-table-entry ;
: func} ( -- ) !return ;
{asm 
.data 
0000# .org
JumpTableStart label-here
JumpTableLocation label-here
7FFF# sp register!
0 zero register!
0 sp2 register!
.code
1 jump-table-end arg0 !subi
jump-table-start t0 !ld
lr t0 !brl
0100# .org
TableCall {func 
    lr sp2 !push \ save the link register to the subroutine stack
    jump-table-start arg0 t0 !add \ first we need to combine the jump-table-start with arg0
    t0 t1 !ld \ load the actual stored address
    lr t1 !brl \ perform the indirect call
    sp2 lr !pop \ restore before returning
    func}
OperationPopParameter {func 
    sp ret0 !pop \ get the top of the stack
    func}
OperationPushParameter {func
    arg0 sp !push \ push onto the top of the stack
    func}
OperationDrop {func
    lr sp2 !push \ save the link register to the subroutine stack
    OperationPopParameter @ lr !bl \ call pop parameter
    zero ret0 !move \ delete the contents
    sp2 lr !pop 
    func}
OperationDuplicate {func 
    lr sp2 !push \ save link register
    OperationPopParameter @ lr !bl 
    ret0 arg0 !move
    OperationPushParameter @ lr !bl
    OperationPushParameter @ lr !bl
    sp2 lr !pop
    func}
OperationAdd {func 
    args3 !add
    func}
OperationSubtract {func
   args3 !sub
   func}
OperationMultiply {func
   args3 !mul
   func}
ShutdownProcessor {func 
    args1 !terminateExecution 
    func}
.code 0000# .org

JumpTableStart @ jump-table-start register!
JumpTableLocation @ jump-table-end register!
asm}
close-input-file



