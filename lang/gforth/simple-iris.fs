include iris.fs
deflabel jumpTarget
deflabel jumpTarget2
s" foo.test" {asm
jumpTarget .label
0xfded #, r43 r44 addi, 
r43 r44 swap, 
jumpTarget2 .label
jumpTarget !jmp

asm}
bye
