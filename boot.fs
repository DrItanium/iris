" iris.fs" open-input-file
sp0 constant sp \ stack pointer
sp1 constant csp \ call stack pointer

fixed-registers-stop ={enum
enum: dp \ dictionary pointer
enum: t0 \ temporary 0
enum: t1 \ temporary 1
enum: t2 \ temporary 2
enum: t3 \ temporary 3
enum: t4 \ temporary 4
enum: t5 \ temporary 5
enum: ibcurr \ input buffer current position
enum: ibend \ input buffer end
enum: tokstart \ token start
enum: tokend \ token end
enum: keep-executing \ variable for determine whether to keep executing or not
enum: &terminate-execution \ location of terminate-execution
enum: &InputRoutine \ location for input-routine
enum: &Restart \ location for Restart
enum}
" routines.rom" open-input-file
" boot.rom" {bin

\ generic computer instructions from threaded interpretive languages book


0xF000 constant input-buffer-start
0xF100 constant input-buffer-end
0x0100 constant boot-rom-start
0xC000 constant routines-start
0xD000 constant dictionary-start
0xE800 constant data-stack-start
0xF000 constant call-stack-start

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
: return-on-true ( -- ) return !bccv ;

defun: fix-case
      \ lower case becomes upper case
      97 t1 cv !lti return-on-true
      122 t1 cv !gti return-on-true \ we are looking at a value in between a and z in the ascii table
      32 t1 t1 !subi \ subtract 32 to get the upper case version
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
       t2 !1- \ walk back a character as well
       0x20 t1 $-> \ make sure that we put a space in instead
       t1 t2 !sw
       \ skip-whitespace-in-input !call
       ibcurr ibend t3 !sub 
       1 ibcurr t4 !subi
       t3 t4 !sw
       t4 ibcurr !move \ now make ibcurr the start with the length as well
       defun;
defun: print-characters
       /dev/console0 $->io
       arg1 arg0 io !write-code-range-to-io
       defun;

.label unknown-word
       \ use the token start and end to print it out
       tokstart arg0 ->
       tokstart tokend arg1 !sub
       2 arg1 =+n
       tokend t0 ->
       63 t0 !swi 
       t0 !1+
       0xA t0 !swi \ save newline here
       print-characters !call
       boot-rom-start jmp
: mk-mtbase-fun ( base-num -- )
    defun: 
    nbase $->
    defun; ;
16 mk-mtbase-fun 16base

dictionary-start .org
\ all builtins should go here
.label core-dictionary-start
boot-rom-start .org
.label Restart
    core-dictionary-start dp $->
    data-stack-start sp $-> 
    call-stack-start csp $-> 
    16base !call
    zero error-code ->
    0xFFFF keep-executing $->
.label InputRoutine
    \ this code will read a line and save it to 0xF100
    input-buffer-start ibcurr $-> 
    ibcurr !1+
    ibcurr ibend ->
    readline !call
.label read-token-routine
    ibcurr dp !readtok
    keep-executing cv !eqz
    cv &terminate-execution !bcr
    error-code cv !neqz
    unknown-word cv !bc \ if we hit an error code then restart the loop
    ret0 sp psh->
    ibcurr ibend cv !neq 
    read-token-routine cv !bc \ keep reading if we got this far
    &InputRoutine !br
0x0000 .org 
\ setup variables that will not be known until now
InputRoutine &InputRoutine !set
terminate-execution &terminate-execution !set
Restart &Restart !set
boot-rom-start jmp 
bin}
;s
