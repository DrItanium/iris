\ basic routines that everyone wants to use
" registers.fs" open-input-file
0xC000 constant routines-start
0x0100 constant boot-rom-start
" routines.bin" {bin
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
	   \ arg0 - start (ibcurr)
	   \ arg1 - end  (ibend)
	   \ ret0 - updated arg0
	   \ ret1 - updated arg1
	   arg0 arg1 cv !neq return-on-true
	   /dev/console0 $->io
	   0xA t0 $->
       .label readline-loop
       t1 io-read \ load a character from input
       fix-case !call
       t1 arg1 !sw \ save to memory
       arg1 !1+
       t1 t0 cv !neq
       readline-loop cv !bc
       arg1 t2 -> 
       t2 !1- \ walk back a character as well
       0x20 t1 $-> \ make sure that we put a space in instead
       t1 t2 !sw
       \ skip-whitespace-in-input !call
       arg0 arg1 t3 !sub 
       1 arg0 t4 !subi
       t3 t4 !sw
       t4 ret0 -> \ now make ibcurr the start with the length as well
	   t2 ret1 ->
       defun;
defun: print-characters
       /dev/console0 $->io
       arg1 arg0 io !write-code-range-to-io
       defun;
defun: func-2drop
	   sp !2drop
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
10 mk-mtbase-fun 10base
8 mk-mtbase-fun 8base
2 mk-mtbase-fun 2base
bin}
;s
