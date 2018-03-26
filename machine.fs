( machine related words )
: *register-count* ( -- 256) 256 ;
: *space-size* ( -- 0x10000) 10000# ;
: *max-address* ( -- 0xFFFF) *space-size* 1 - ;
: *word-size* ( -- 2 ) 2 ;
: *double-word-size* ( -- n ) *word-size* *word-size* + ;

: *register-byte-count* ( -- n ) *register-count* *word-size* * ;
: number-of-bytes-for-space ( s -- c ) *max-address* * ;
: *data16-space-byte-count* ( -- n ) *word-size* number-of-bytes-for-space ;
: *data32-space-byte-count* ( -- n ) *double-word-size* number-of-bytes-for-space ;

close-input-file
