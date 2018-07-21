\ generic register definitions
: {registers ( -- 0 ) 0 ;
: registers} ( n -- ) drop ;
: register: ( n -- n+1 ) constant 
{registers
register: r0
register: r1
register: r2
register: r3
register: r4
register: r5
register: r6
register: r7
register: r8
register: r9
register: r10
register: r11
register: r12
register: r13
register: r14
register: r15
registers}
