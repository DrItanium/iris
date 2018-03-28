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
ShutdownProcessor {func 
    args1 !terminateExecution 
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
JumpTableStart @ jump-table-start register!
JumpTableEnd @ jump-table-end register!
asm}
close-input-file



