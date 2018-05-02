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
enum: tokstart \ token start
enum: tokend \ token end
enum: keep-executing \ variable for determine whether to keep executing or not
enum: &terminate-execution \ location of terminate-execution
enum: &InputRoutine \ location for input-routine
enum: &Restart \ location for Restart
enum: error-code 
enum: arg0 \ first argument
enum: arg1 \ second argument
enum: arg2 \ third argument
enum: arg3 \ fourth argument
enum: ret0
enum: ret1
enum: digit-current
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
: defun: ( -- ) .label lr csp psh-> ;
: defun;  ( -- ) return jmp ;
: return-on-true ( -- ) return cv !bc ;
: terminate-if-not-char ( index -- )
       arg0 t0 !lw
       t0 cv !neqi 
       return-on-true
       arg0 !1+ ;

defun: fix-case
      \ lower case becomes upper case
      97 t1 cv !lti return-on-true
      122 t1 cv !gti return-on-true \ we are looking at a value in between a and z in the ascii table
      32 t1 t1 !subi \ subtract 32 to get the upper case version
      defun;
defun: skip-whitespace-in-input
       \ TODO implement logic for whitespace skipping
       \ TODO migrate this to microcode?
       defun;
defun: readline
       ibcurr ibend cv !neq return-on-true
       /dev/console0 $->io
       0xA t0 $->
       .label readline-loop
       t1 io-read \ load a character from input
       fix-case !call
       t1 ibend !sw \ save to memory
       ibend !1+
       t1 t0 cv !neq
       readline-loop cv !bc
       ibend t2 !move
       1 t2 t2 !subi \ walk back a character as well
       0x20 t1 !set \ make sure that we put a space in instead
       t1 t2 !sw
       skip-whitespace-in-input !call
       ibcurr ibend iblen !sub 
       defun;
defun: print-characters
       \ start at arg0 and go for arg1 number of elements
       arg1 !eqz return-on-true \ leave early if length is zero
       /dev/console0 $->io
       arg0 arg1 t0 !add \ compute the last address to print
       .label printcharacters-loop 
       arg0 t1 !lw
       t1 io-write \ print it out
       arg0 !1+
       arg0 t0 cv !neq
       printcharacters-loop cv !bc
       defun;

defun: check-for-quit
       \ arg0 contains starting point for checking 
       \ arg1 contains the length
       arg1 !eqz return-on-true \ leave early if length is zero
       5 arg1 cv !neqi return-on-true \ is the length incorrect?
       81 terminate-if-not-char \ Q
       85 terminate-if-not-char \ U
       73 terminate-if-not-char \ I
       84 terminate-if-not-char \ T
       0x20 terminate-if-not-char \ space
       zero keep-executing !move
       defun;

.label read-hex-digit-done
       zero error-code !move
       at1 ret0 !move
       defun;
: emit-value-if-matches ( input output -- ) 
    at1 !set
    at0 !set
    arg0 at0 cv !eq
    read-hex-digit-done cv !bc ;
defun: read-hex-digit
       \ arg0 contains the current position
       48 0x0 emit-value-if-matches
       49 0x1 emit-value-if-matches
       50 0x2 emit-value-if-matches
       51 0x3 emit-value-if-matches
       52 0x4 emit-value-if-matches
       53 0x5 emit-value-if-matches
       54 0x6 emit-value-if-matches
       55 0x7 emit-value-if-matches
       56 0x8 emit-value-if-matches
       57 0x9 emit-value-if-matches
       65 0xA emit-value-if-matches
       66 0xB emit-value-if-matches
       67 0xC emit-value-if-matches
       68 0xD emit-value-if-matches
       69 0xE emit-value-if-matches
       70 0xF emit-value-if-matches
       0xFFFF error-code !move
       defun;
: load-shifted-hex-digit ( -- ) .label 
    read-hex-digit !call
    arg0 !1+
    ret0 digit-current digit-current !add
    4 digit-current digit-current !shli
    0xFFFF at0 !set
    at0 error-code cv !eq
    return-on-true ;
load-shifted-hex-digit rhd4
load-shifted-hex-digit rhd3
load-shifted-hex-digit rhd2
.label rhd1
    read-hex-digit !call
    arg0 !1+
    ret0 digit-current digit-current !add
    0xFFFF at0 !set
    at0 error-code cv !eq
    return-on-true
    \ probably want to push this onto the stack at some point
    defun;

defun: read-hex-number
       \ arg0 contains starting point for checking
       \ arg1 contains the length 
       0xFFFF error-code !set
       arg1 !eqz return-on-true \ if we have no characters then no way bro either
       5 arg1 cv !gti return-on-true \ if we have more than five characters then no way bro!
       5 arg1 cv !eqi
       rhd4 cv !bc
       4 arg1 cv !eqi
       rhd3 cv !bc
       3 arg1 cv !eqi
       rhd2 cv !bc
       2 arg1 cv !eqi
       rhd1 cv !bc
       defun;




boot-rom-start .org
.label Restart
    zero error-code !move
    0x7FFF sp $->
    0xFFFF csp $->
    0xFFFF keep-executing $->
.label InputRoutine
    \ this code will read a line and save it to 0xF100
    input-buffer-start ibcurr $-> 
    ibcurr ibend ->
    readline !call
    ibcurr arg0 !move
    iblen arg1 !move
    print-characters !call
    ibcurr tokend tokstart !readtok
    1 tokend ibcurr !addi
    tokstart arg0 !move
    tokstart tokend arg1 !sub
    check-for-quit !call
    keep-executing !eqz
    cv &terminate-execution !bcr
    error-code !eqz
    cv &InputRoutine !bcr
    \ printout the error message and then restart execution!
    \ token-start arg0 !move
    \ token-stop arg1 !move
    \ perform the call to printout the unknown token

0x0000 .org 
\ setup variables that will not be known until now
InputRoutine &InputRoutine !set
terminate-execution &terminate-execution !set
Restart &Restart !set
boot-rom-start jmp 
bin}
;s
