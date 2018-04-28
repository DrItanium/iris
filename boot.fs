" iris.fs" open-input-file
fixed-registers-stop ={enum
enum: sp \ stack pointer
enum: csp \ call stack pointer
enum: t0 \ temporary 0
enum: t1 \ temporary 1
enum: t2 \ temporary 2
enum: t3 \ temporary 3
enum: io \ io device number or address
enum: ci \ core index number
enum: ibcurr \ input buffer current position
enum: ibend \ input buffer end
enum: iblen
enum: token-start \ where to start on a given token
enum: token-stop \ where the token of magic is meant to stop (this includes the space)
enum: keep-executing \ variable for determine whether to keep executing or not
enum: &terminate-execution \ location of terminate-execution
enum: &InputRoutine \ location for input-routine
enum: &Restart \ location for Restart
enum: arg0 \ first argument
enum: arg1 \ second argument
enum: arg2 \ third argument
enum: arg3 \ fourth argument
enum: ret0
enum: ret1
enum}

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

: jmp ( addr -- ) 
  \ unconditional jump to the address encoded into the instruction
  !b ;

: ->io ( reg -- ) io -> ;

: $-> ( value reg -- ) !set ;
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

: !call ( dest -- ) lr !bl ;

0xF000 constant input-buffer-start
0xF100 constant input-buffer-end
0x0100 constant boot-rom-start
0xC000 constant routines-start

\ code start

routines-start .org
     .label terminate-execution
        zero !terminateExecution
     .label return
        \ all functions go through here to make sure that we do the right thing
        csp lr pop-> 
        lr !ret 
: (defun ( -- ) .label lr csp psh-> ;
: defun)  ( -- ) return jmp ;
: return-on-true ( -- ) return cv !bc ;
       
      (defun fix-case
            \ lower case becomes upper case
            97 t1 cv !lti
            return-on-true
            122 t1 cv !gti
            return-on-true
            \ we are looking at a value greater than z
            32 t1 t1 !subi \ subtract 32 to get the upper case version
            defun)
     (defun readline
        /dev/console0 $->io
        0xA t0 $->
        .label readline-loop
        t1 io-read \ load a character from input
        fix-case !call
        t1 ibend !sw \ save to memory
        ibend !1+
        t1 t0 cv !neq
        readline-loop cv !bc
        ibcurr ibend iblen !sub
        defun)
      (defun print-characters
       \ start at arg0 and go for arg1 number of elements
       arg1 zero cv !eq
       return-on-true \ leave early if length is zero
       /dev/console0 $->io
       arg0 arg1 t0 !add \ compute the last address to print
       .label printcharacters-loop 
       arg0 t1 !lw
       t1 io-write \ print it out
       arg0 !1+
       arg0 t0 cv !neq
       printcharacters-loop cv !bc
       defun)
     (defun check-for-quit
            \ arg0 contains starting point for checking 
            \ arg1 contains the length
            arg1 zero cv !eq
            return-on-true \ leave early if length is zero
            5 arg1 cv !neqi \ is the length incorrect?
            return-on-true
            /dev/console0 $->io
            arg0 t0 !lw
            81 t0 cv !neqi \ Q
            return-on-true
            arg0 !1+
            arg0 t0 !lw
            85 t0 cv !neqi \ U
            return-on-true
            arg0 !1+
            arg0 t0 !lw
            73 t0 cv !neqi \ I
            return-on-true
            arg0 !1+
            arg0 t0 !lw
            84 t0 cv !neqi \ T
            return-on-true
            arg0 !1+
            arg0 t0 !lw
            0xA t0 cv !neqi \ newline
            return-on-true
            zero keep-executing !move
            defun)



boot-rom-start .org
.label Restart
    0x7FFF sp $->
    0xFFFF csp $->
    0xFFFF keep-executing $->
.label InputRoutine
    \ this code will read a line and save it to 0xF100
    0xA t1 $->
    input-buffer-start ibcurr $-> 
    ibcurr ibend ->
    readline !call
    ibcurr arg0 !move
    iblen arg1 !move
    print-characters !call
    ibcurr arg0 !move
    iblen arg1 !move
    check-for-quit !call
    zero keep-executing cv !eq
    cv &terminate-execution !bcr
    &InputRoutine !br
0x0000 .org 
\ setup variables that will not be known until now
InputRoutine &InputRoutine !set
terminate-execution &terminate-execution !set
Restart &Restart !set
boot-rom-start jmp 
bin}
;s
