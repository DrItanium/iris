( machine related words )
: *register-count* ( -- 256 ) 256 ;
: *space-size* ( -- 0x10000 ) 10000# ;
: *max-address* ( -- 0xFFFF ) *space-size* 1- ;
: *word-size* ( -- 2 ) 2 ;
: *double-word-size* ( -- n ) *word-size* *word-size* + ;

: *register-byte-count* ( -- n ) *register-count* *word-size* * ;
: number-of-bytes-for-space ( s -- c ) *max-address* * ;
: *data16-space-byte-count* ( -- n ) *word-size* number-of-bytes-for-space ;
: *data32-space-byte-count* ( -- n ) *double-word-size* number-of-bytes-for-space ;

: get-lower8 ( i -- n ) FF# bitwise-andu ;
: get-lower16 ( i -- n ) FFFF# bitwise-andu ;
: decode-byte ( i pos -- reg ) 8 * >>u get-lower8 ;
: decode-word ( i pos -- immediate ) 16 * >>u get-lower16 ;

: decode-opcode ( i -- opcode ) get-lower8 ;
: decode-destination ( i -- n ) 1 decode-byte ;
: decode-source ( i -- n ) 2 decode-byte ;
: decode-source2 ( i -- n ) 3 decode-byte ;
: decode-imm8 ( i -- n ) decode-source2 ;
: decode-imm16 ( i -- n ) 1 decode-word ;

: decode-immediate-only ( i -- imm16 ) decode-imm16 ;

: decode-one-register ( i -- dest ) 
  decode-destination ;
: decode-two-register ( i -- src dest ) 
  dup  ( i i )
  decode-source ( i src )
  swap ( src i )
  decode-one-register ( src dest ) ;

: decode-three-register ( i -- src2 src dest )
  dup ( i i )
  decode-source2 ( i src2 )
  swap ( src2 i )
  decode-two-register ( src2 src dest ) ;

: decode-two-register-with-immediate ( i -- imm8 src dest )
  decode-three-register ;

: decode-one-register-with-immediate ( i -- imm16 dest ) 
  dup ( i i )
  decode-immediate-only ( i imm16 )
  swap ( imm16 i )
  decode-destination ( imm16 dest ) ;

: decode-no-argument ( i -- ) drop ;

: form-noarg ( -- n ) 0 ;
: form-onereg ( -- n ) [ form-noarg 1+ ] literal ;
: form-tworeg ( -- n ) [ form-onereg 1+ ] literal ;
: form-threereg ( -- n ) [ form-tworeg 1+ ] literal ;
: form-imm16 ( -- n ) [ form-threereg 1+ ] literal ;
: form-onereg-imm ( -- n ) [ form-imm16 1+ ] literal ;
: form-tworeg-imm ( -- n ) [ form-onereg-imm 1+ ] literal ;


variable current-instruction
: decode-operation ( i -- *args* opcode ) 
  dup current-instruction ! ( make a copy of the current instruction )
  
  current-instruction @ decode-opcode ( opcode ) ;
  

close-input-file
