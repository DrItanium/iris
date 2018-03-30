( linker words )
{enum
: *linker-section:register* ( -- 0 ) literal ; enum,
: *linker-section:code* ( -- 1 ) literal ; enum,
: *linker-section:data* ( -- 2 ) literal ; enum,
: *linker-section:stack* ( -- 3 ) literal ; 
enum}

: core-memory-capacity *memory-size* ;

*data32-space-byte-count* *register-byte-count* + *data16-space-byte-count* 2* + resize-memory

close-input-file
