" iris.fs" open-input-file
" registers.fs" open-input-file
" boot.rom" {bin

\ generic computer instructions from threaded interpretive languages book
: @-> ( a b -- ) 
  \ the contents of the memory location word whose address is in register A
  \ are loaded into register B ( a 16-bit indirect fetch from A to B )
  !lw ;

: =+n ( n a -- ) 
  \ The contents of register A are incremented by constant n
  dup  ( n a a ) 
  !addi ;

: pop-> ( s a -- ) 
  \ the S pushdown stack top entry is loaded to register A and the stack pointer
  \ is adjusted
  !pop ;

: psh-> ( a s -- )
  \ the A register contents are loaded to the S pushdown stack and the stack 
  \ pointer is adjusted
  !push ;

: -> ( a b -- ) 
  !move ;
: ->pc ( a -- ) 
  \ the contents of the A register are loaded into the PC. The processor will
  \ fetch its next instruction from this location
  pc -> ;

: jmp ( addr -- ) 
  \ unconditional jump to the address encoded into the instruction
  !b ;

: ->io ( reg -- ) io -> ;

: $-> ( value reg -- ) !set ;
: $->pc ( value -- ) pc $-> ;
: $->io ( value -- ) io $-> ;
: io-write ( src -- ) io !stio ;
: io-read  ( dest -- ) io swap !ldio ;

: dump-core-id-in-register ( n -- )
  /dev/core-dump $->io
  io-write ;
: load-core-id-in-register ( n -- )
  /dev/core-load $->io
  io-write ;
: dump-core ( -- ) ci dump-core-id-in-register ;
: load-core ( -- ) ci load-core-id-in-register ;



0xF100 constant input-buffer-start
0xF300 constant inpub-buffer-end
0x0000 .org
    0x8000 sp $-> 
    0xFFFF rs $->
    \ need a simple machine code monitor to build up things
    ( zero zero !st
    1 zero t0 !addi
.label PopulateLoop
    t0 t0 !st
    t0 !1+
    PopulateLoop t0 !bneqz 
    dump-core
    ci !1+ \ goto the next one
    load-core \ it will zero out memory since it doesn't exist
    dump-core \ immediately dump it to disk and thus create it
    ci !1+ \ goto the next one
    zero load-core-id-in-register
    dump-core
\ inner-interpreter words
    3 ci $->
    load-core
    /dev/console0 $->io )
    0xA t1 $->
    input-buffer-start ibcurr $-> 
    ibcurr ibend ->
.label readline
    t0 io-read
    t0 io-write
    t0 ibend !sw
    ibend !1+
    t1 t0 cv !neq
    readline_test cv !bc
    dump-core
    \ always last thing to do
    top !zero
.label fnTERMINATE
    top !terminateExecution
0x0100 .org 
.label fnSEMI
    next-addr .data16 
    rs ir pop->
.label fnNEXT
    ir wa @->
    1 ir =+n 
.label fnRUN
    wa ca @->
    1 wa =+n 
    ca ->pc
0x0050 .org
    \ embed execute
    0x45584507 dictionary-header \ dictionary header
    0 link-address
.label fnEXECUTE
    next-addr execution-address \ code address for execute
    sp wa pop->
    fnRUN jmp
0x0140 .org
.label fnCOLON
    ir rs psh->
    wa ir ->
    fnNEXT jmp
.label fnBYE
    sp top pop->
    fnTERMINATE jmp

0x2000 .org
    \ dup dictionary entry
.label dictionaryDUP
    0x50554403 dictionary-header
    fnEXECUTE link-address
    next-addr execution-address
    sp ca pop->
    ca sp psh->
    ca sp psh->
    fnNEXT jmp
\ 0x2100 .org
    \ secondary defining keyword constant
\    0x4E4F4308 dictionary-header 
\    dictionaryDUP link-address
\    fnCOLON execution-address

bin}
;s
