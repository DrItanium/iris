( linker words )
: *linker-section:register* ( -- 0 ) 0 ;
: *linker-section:code* ( -- 1 ) 1 ;
: *linker-section:data* ( -- 2 ) 2 ;
: *linker-section:stack* ( -- 3 ) 3 ;

: core-memory-capacity *memory-size* ;

*data32-space-byte-count* *register-byte-count* + *data16-space-byte-count* 2* + resize-memory
