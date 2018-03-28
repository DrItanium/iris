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
deflabel OperationMultiply
{asm 
.code
    0 zero !set
    zero sp2 !move
    7FFF# sp !set
ShutdownProcessor label-here
    args1 !terminateExecution
OperationAdd {func 
   args3 !add 
   func}
OperationSubtract {func
   args3 !sub
   func}
OperationMultiply {func
   args3 !mul
   func}
.data 
FF00# .org
JumpTableStart label-here
    ShutdownProcessor .data16
    OperationAdd .data16
    OperationSubtract .data16
    OperationMultiply .data16
    0 .data16
asm}
close-input-file



