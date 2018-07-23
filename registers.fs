\ generic register definitions
: {registers ( -- 0 ) 0 ;
: registers} ( n -- ) drop ;
: register: ( n -- n+1 ) dup constant 1+ ;
{registers
register: x0
register: x1
register: x2
register: x3
register: x4
register: x5
register: x6
register: x7
register: x8
register: x9
register: x10
register: x11
register: x12
register: x13
register: x14
register: x15
registers}
