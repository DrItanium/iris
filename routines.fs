\ basic routines that everyone wants to use
" registers.fs" open-input-file
0xC000 constant routines-start
0x0100 constant boot-rom-start
" routines.bin" {bin
routines-start .org
.label terminate-execution
       arg0 !exit
.label return
        \ all functions go through here to make sure that we do the right thing
        csp lr pop-> 
        lr !ret 
: defun: ( -- ) .label lr csp psh-> ;
: defun;  ( -- ) return jmp ;
: return-on-true ( -- ) return !bccv ;
: return-on-zero-len ( reg -- ) cv !eqz return-on-true ;
.label restore-t2
	   t2 !restore-vmsp
.label restore-t1
	   t1 !restore-vmsp
.label restore-t0
	   t0 !restore-vmsp
	   defun;
defun: write-range-to-io-address
	   \ arg0 - starting point in memory
	   \ arg1 - length
	   \ whatever is stored in io is used
	   arg1 return-on-zero-len
	   t0 !save-vmsp
	   t1 !save-vmsp
	   t0 !zero
	   \ setup the loop to perform io writes
.label write-range-to-io-address-loop
	   arg0 t0 t1 !addu
	   t1 t1 !lw 
	   t1 io-write
	   t0 !1+
	   t0 arg1 cv !neq
	   write-range-to-io-address-loop !bccv
	   restore-t1 jmp
defun: read-range-from-io-address
	   \ arg0 is the destination address to write into within memory
	   \ arg1 is the count to read, it will keep reading until we get to this
	   \      point. This can become an issue when reading from standard input
	   \ io contains the io address to read from
	   arg1 return-on-zero-len
	   t0 !save-vmsp
	   t1 !save-vmsp
	   t2 !save-vmsp
	   t0 !zero
.label read-range-from-io-address-loop
	   arg0 t0 t1 !add
	   t2 !io-read
	   t2 t1 !sw
	   t0 !1+
	   t0 arg1 cv !neq 
	   read-range-from-io-address-loop !bccv
	   restore-t2 jmp

.label done:read-range-from-io-address-with-terminator
	   t0 ret0 -> \ return the number of characters read
	   restore-t2 jmp
defun: read-range-from-io-address-with-terminator
	   \ arg0 is the destination address to write into within memory
	   \ arg1 is the count to read, it will keep reading until we get to this
	   \      point. This can become an issue when reading from standard input
	   \ io contains the io address to read from
	   arg1 return-on-zero-len
	   t0 !save-vmsp
	   t1 !save-vmsp
	   t2 !save-vmsp
	   t0 !zero
.label read-range-from-io-address-with-terminator-loop
	   arg0 t0 t1 !add
	   t2 !io-read
	   t2 t1 !sw
	   t0 !1+
	   \ make sure that we actually set things up correctly
	   t2 terminator cv !eq 
	   done:read-range-from-io-address-with-terminator !bccv
	   t0 arg1 cv !neq 
	   read-range-from-io-address-with-terminator-loop !bccv
	   done:read-range-from-io-address-with-terminator jmp

defun: fix-case
      \ lower case becomes upper case
      97 t1 cv !lti return-on-true
      122 t1 cv !gti return-on-true \ we are looking at a value in between a and z in the ascii table
      32 t1 t1 !subi \ subtract 32 to get the upper case version
      defun;
defun: readline
	   \ arg0 - start
	   \ arg1 - length 
	   \ ret0 - start address
	   \ ret1 - end address
	   /dev/console0 $->io
	   0xA terminator $->
	   read-range-from-io-address-with-terminator !call
	   ret0 ret1 ->
	   arg0 ret0 ->
       defun;

defun: print-characters
       /dev/console0 $->io
	   write-range-to-io-address !call
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
defun: println
	   \ arg0 - start
	   \ arg1 - length
	   print-characters !call
	   /dev/console0 $->io
	   0xA $->at0
	   at0 io-write
	   defun;
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
