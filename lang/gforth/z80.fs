\ z80 assembler words
\ taken from the Threaded Interpretive Languages book
: cconstant create c, does> c@ ;
: <builds 0 constant ;
get-current 
vocabulary z80 also z80 definitions
hex
: 1byte <builds c, does> c@ c, ;
3f 1byte ccf, \ complement carry flag
af 1byte cla, \ clear accumulator { xor A }

set-current
previous
