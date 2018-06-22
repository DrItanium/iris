\ enumeration concept in gforth
: ={enum ( n -- n n ) dup ;
: {enum ( -- 0 0 ) 0 ={enum ;
: enum} ( n -- ) drop ;

: enum: ( n -- k k ) constant 1+ dup ;
