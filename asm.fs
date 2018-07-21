\ iris assembler for gforth
get-current
also assembler definitions

: encode-2reg ( src dest -- v 1 ) 
  reg>idx addr4 swap 
  reg>idx 4 lshift or 
  addr8 1 ;
: encode-1reg ( dest -- v 1 ) _r0 swap encode-2reg ;
: encode-0reg ( -- 0 1 ) 0 1 ;
: encode-3reg ( src2 src dest -- v2 v1 2 ) 
  2>r encode-1reg drop 2r> encode-2reg 1+ ; 
: encode-4reg ( src3 src2 src dest -- v2 v1 2 )
  2>r encode-2reg drop 2r> encode-2reg 1+ ; 
: encode-imm16 ( imm16 -- u l 2 ) 
  dup >r 
  8 rshift  addr8 
  r> addr8 2 ;
: encode-1reg-imm16 ( imm16 dest -- v3 v2 v1 3 )
  encode-1reg drop ( imm16 v1 ) >r ( imm16 )
  encode-imm16 drop ( v3 v2 ) r> ( v3 v2 v1 ) 3 ;
: encode-2reg-imm16 ( imm16 src dest -- v3 v2 v1 3 ) 
  encode-2reg drop ( imm16 v1 ) >r ( imm16 )
  encode-imm16 drop ( v3 v2 ) r> ( v3 v2 v1 ) 3 ;
