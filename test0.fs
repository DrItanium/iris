( 
    A test of the assembler system
    close-input-file must be the last word in the file, anything after it will not
    even be parsed so it is like a giant comment block there
)
{asm 
    .code
    !nop
    0 zero !set
    zero sp2 !move
    7FFF# sp !set
    r0 !terminateExecution
asm}
close-input-file



