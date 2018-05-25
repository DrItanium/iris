include iris.fs
\ contains all of the registers and pieces used for the monitor itself
\ the monitor is now also the forth system itself
s" figforth.o" {asm
\ the core memory is a disk buffer of a kind so it will become the disk buffer 
\ of legend that is being discussed in the forth book.
0xFFFF constant ram-end
0x0000 constant ram-start
ram-start constant interpreter-start
\ 0x0100 constant bootstrap-end
0xFF00 constant &LIMIT
&LIMIT 0x420 - constant &FIRST \ address of the first byte of the disk buffers
&FIRST 0xE0 - constant system-variables-end
system-variables-end 0x100 - constant system-variables-start
system-variables-start constant input-buffer-end
input-buffer-end 0x100 - constant input-buffer-start
input-buffer-start constant return-stack-start \ 512 entries
return-stack-start 0x200 - constant return-stack-end
return-stack-end constant data-stack-start

\ bootstrap-end constant dictionary-start

\ register reservations
: too-many-vars-defined ( addr -- ) 0x40 >= ABORT" To many registers used!" ;
: 1+cconstant ( n "name" -- ) dup cconstant 1+ ;
unused-start 
\ constants
\ user variables
1+cconstant s0 \ initial value of the data stack pointer
1+cconstant r0 \ initial value of the return stack pointer
1+cconstant &tib \ address of the terminal input buffer
1+cconstant warning \ error message control number. If 1, disk is present, 
                    \ and screen 4 of drive 0 is the base location of error messages
                    \ if 0, no disk is present and error messages will be presented
                    \ by number. If -1, execute (ABORT) on error
1+cconstant fence \ address below which FORGETting is trapped.
                  \ To forget below this point, the user must alter the contents
                  \ of FENCE
1+cconstant dp \ The dictionary pointer which contains the next free memory
               \ above the dictionary. The value may be read by HERE and altered
               \ by ALLOT.
1+cconstant voc-link \ address of a field in the definition of the most recently created
                     \ created vocabulary. All vocabulary names are linked by
                     \ these fields to allow control for FORGETting through multiple
                     \ vocabularies
1+cconstant blk \ current block number under interpretatio. If 0, input is
                \ being taken from the terminal input buffer
1+cconstant xin \ Byte offset within the current input text buffer (terminal or
               \ disk) from which the next text will be accepted. WORD uses and
               \ move the value of IN
1+cconstant xout \ Offset in the text output buffer. Its value is incremented by EMIT
                 \ The user may yalter and examine OUT to control output display formatting.

1+cconstant xscr \ Screen number most recently referenced by LIST
1+cconstant xoffset \ Block offset disk drives. Contents of OFFSET is added to the stack number by BLOCK
1+cconstant xcurrent \ Pointer to the vocabulary in which new definitions are to be added
1+cconstant xstate \ If 0, the system is in interpretive or executing state. If non-zero, the system is in compiling state. The value itself is implementation
                   \ dependent. So in this case it would be 0 is interpretive and 0xFFFF is compiling
1+cconstant xdpl \ number of digits to the right of the decimal point on double integer input. It may also be used to hold output column
                 \ location of a decimal point in user generated formatting. The default value on single number input is -1
1+cconstant xfld  \ field width for formatted number output
1+cconstant xcsp \ temporarily stored data stack pointer for compilation error checking
1+cconstant xr# \ location of editor cursor in a text screen
1+cconstant xhld \ address of the latest character of text during numeric output conversion
1+cconstant xsp \ data stack pointer
1+cconstant xrp \ return stack pointer
1+cconstant xip \ interpretive pointer
1+cconstant xw \ current word pointer

too-many-vars-defined
num-base cconstant xbase

deflabel RoutineCOLD
deflabel RoutineABORT
deflabel RoutineQUIT
deflabel RoutineINTERPRET
deflabel RoutineNEXT

ram-start .org 
\ set the constants
&LIMIT constant xlimit
&FIRST constant xfirst
0x8 constant b/scr
0x80 constant b/buf 
0x40 constant c/l 
0x20 constant bl 
\ start user variables
zero s0 ->


RoutineCOLD .label 
\ when the machine starts from cold
\ COLD calls ABORT
\ ABORT calls QUIT
\ where the text interpreter is embedded
\ : COLD ( -- ) 
\   EMPTY-BUFFERS \ Clear all disk buffers by writing zero's from FIRST to LIMIT
\   0 DENSITY !  \ Specify single density diskette drives
\   FIRST USE ! \ Store the first buffer address in USE and PREV, preparing for disk accessing
\   FIRST PREV !
\   DR0  \ select drive 0 by setting offset to 0
\   0 EPRINT ! \ turn off the printer
\   ORIG       \ Starting address of Forth codes, where initial user variables are kept
\   12H +      
\   UP @ 6 + \ User area
\   10H CMOVE  \ Move 16 bytes of initial values over to the user area. Initialize the terminal
\   ORIG 0CH + @ \ Fetch the name field address of the last word defined in the trunk forth vocabulary, and
\   FORTH 6 + !   \ store it in the FORTH vocabulary link. Dictionary searches will start at the top of FORTH vocabulary.
\                \ New words will be added to FORTH vocabulary unless another vocabulary is named.
\   ABORT        \ call ABORT to do a warm start procedure
\   ;
RoutineNEXT .label
    xip xw -> \ move the contents of xip (which points to the next word to be executed, into xw .
    xip 1+, \ Increment xip, pointing to the second word in execution sequence.
    xw at0 ldtincr, \ load the contents of xw into at0 and then increment xw
                    \ this will make xw point to the parameter field of the word
    at0 jmp \ jump to the address found at that point


\ use the upper stack elements as 
\ : check-overflow ( -- ) loc@ monitor-memory-start < ABORT" routines are too large!" ;
\ deflabel monitor-loop-start
\ deflabel FixCaseRoutine 
\ deflabel PrintCharactersRoutine
\ deflabel WriteRangeToIOAddressRoutine
\ deflabel $ECHO 
\ deflabel $NEWLINERoutine
\ deflabel $->HEX
\ deflabel printline
\ deflabel readline 
\ deflabel IOWrite
\ deflabel PRINT-NUMBER 
\ deflabel DontTerminateExecution
\ deflabel $HEX->KEY 
\ deflabel ReadToken
\ : $KEY ( -- ) /dev/console0 #->io out0 io-read ;
\ : $TERMINATE ( -- ) zero in0 -> /dev/terminate-vm #->io IOWrite !jmp ;
\ : #ECHO ( a -- ) #, in0 set, $ECHO !, call, ;
\ : $SPACE ( -- ) 0x20 #ECHO ;
\ : $PROMPT ( -- ) 0x2D #ECHO $SPACE ;
\ : $NEWLINE ( -- ) $NEWLINERoutine !, call, ;

\ deflabel RoutineDUP
\ dictionary-start .org 
\ 
\ RoutineDUP !, .data16
\ RoutineDup .label
\ 
\ monitor-program-start .org
\     0xA #, terminator set,
\     monitor-stack-start #, rsp set,
\     0x10 #, num-base set,
\ monitor-loop-start .label
\ 	\ print out the set of registers before accepting input
\ 	deflabel DISPLAY_REGISTERS_LOOP
\ 	deflabel DISPLAY-REGISTER8-LOOP
\ 	$NEWLINE
\ 	$NEWLINE
\ 	zero loc1 ->
\ 	DISPLAY_REGISTERS_LOOP .label
\ 	0x8 #, loc1 loc2 addi,
\ 	DISPLAY-REGISTER8-LOOP .label
\ 	/dev/register #->io
\ 	loc1 io-write
\ 	$SPACE
\ 	loc1 1+, 
\ 	DISPLAY-REGISTER8-LOOP !, loc2 loc1 cv bclt, 
\ 	$NEWLINE
\ 	DISPLAY_REGISTERS_LOOP !, 0x40 #, loc1 cv bclti,
\ 	$NEWLINE
\ 	\ print the prompt out
\ 	$PROMPT
\     readline !, call,
\ 	\ if we fail the check then see if the front of 
\ 	\ input is M for terMinate
\ 	\ precompute the offset
\     monitor-input-start 1+ #, loc0 set,
\     loc0 loc0 ld,
\ 	DontTerminateExecution !, 0x4D #, loc0 cv bcneqi,
\ 	\ terminate execution if we get in here
\ 	$TERMINATE
\ 	DontTerminateExecution .label
\ 	\ end TERMINATE
\     out0 loc0 ->
\     monitor-loop-start !, 5 #, loc0 cv bclti, 
\     monitor-input-start 1+ #, loc0 set,
\ \ $HEX
\     loc0 in0 ldtincr, \ first character
\     $->HEX !, call,
\     out0 loc1 ->
\     loc0 in0 ldtincr, \ second character
\     $->HEX !, call,
\     4 #, loc1 loc1 lshifti,
\     out0 loc1 loc1 add,
\     loc0 in0 ldtincr, \ third character
\     $->HEX !, call,
\     4 #, loc1 loc1 lshifti,
\     out0 loc1 loc1 add,
\     loc0 in0 ldtincr, \ fourth character
\     $->HEX !, call,
\     4 #, loc1 loc1 lshifti,
\     out0 loc1 in0 add,
\     PRINT-NUMBER !, call,
\ 	$NEWLINE
\     monitor-input-start #, in0 set,
\ 	in0 in1 ldtincr,
\     printline !, call,
\     monitor-loop-start !, b,
\ \ this must always be first!
\ IOWrite (leafn in0 io-write leafn)
\ WriteRangeToIOAddressRoutine (leafn
\ 	\ in0 - starting point in memory
\ 	\ in1 - length
\ 	deflabel WriteRangeToIOAddress_Done
\ 	deflabel WriteRangeToIOAddress_Loop
\     2 save-locals
\ 	zero loc0 ->
\ 	WriteRangeToIOAddress_Done !, in1 cv bceqz,
\ 	WriteRangeToIOAddress_Loop .label
\ 	in0 loc0 loc1 uadd, \ uadd bro!?
\ 	loc1 loc1 ld,
\ 	loc1 io-write
\ 	loc0 1+,
\ 	WriteRangeToIOAddress_Loop !, loc0 in1 cv bcgt,
\ 	WriteRangeToIOAddress_Done .label
\     2 restore-locals
\     leafn)
\ $->HEX (fn
\     deflabel $->HEX_Done
\     deflabel $->HEX_IsDigit
\     \ in0 - value to hexify
\     0x30 #, in0 in0 subi,
\     $->HEX_Done !, in0 cv bcltz,
\     $->HEX_IsDigit !, 0x0A #, in0 cv bclti, 
\     $->HEX_Done !, 0x0A #, in0 cv bclti, 
\     0x7 #, in0 in0 subi,
\     $->HEX_IsDigit .label
\     $->HEX_Done .label
\     fn)
\ 
\     \ reads the next four characters in as hexadecimal characters and converts them to hexidecimal numbers
\         
\ $NEWLINERoutine .label
\       0xA #, in0 set,
\ $ECHO (leafn
\       \ in0, the character to write
\       /dev/console0 #->io
\       in0 io-write
\       leafn)
\ 
\ $HEX->KEY (leafn
\     deflabel $HEX->KEY_DONE
\     \ go backwards from what we originally got in
\     \ in0 - contains lowest 4 bits to convert
\     0x000F #, in0 in0 andi,
\     0xA #, in0 cv lti,
\     0x30 #, in0 in0 addi, \ always want to do this
\     $HEX->KEY_DONE !, cv bc,
\     0x7 #, in0 in0 addi,
\     $HEX->KEY_DONE .label
\     leafn)
\ : print-number-$hex->key ( index -- ) 
\   #, loc0 in0 rshifti, 
\   $HEX->KEY !, call,
\   $ECHO !, call, ;
\ 
\ PRINT-NUMBER (fn
\     deflabel print-number-done
\     \ in0 - number to print
\     1 save-locals
\     in0 loc0 ->
\ 	$NEWLINE
\     0xC print-number-$hex->key
\     8 print-number-$hex->key
\     4 print-number-$hex->key
\     loc0 in0 ->
\     $HEX->KEY !, call,
\     $ECHO !, call,
\     print-number-done .label
\     1 restore-locals
\     fn)
\ readline (fn
\ deflabel readline_loop
\ deflabel readline_done
\ deflabel readline_consume_rest_of_line 
\ deflabel FixCaseRoutineDone
\     1 save-locals 
\     monitor-input-start 1+ #, loc0 set,
\ readline_loop .label 
\ 	$KEY \ get the key
\     FixCaseRoutineDone !, 0x61 #, out0 cv bclti,
\     FixCaseRoutineDone !, 0x7a #, out0 cv bcgti, 
\     0x20 #, out0 out0 subi, 
\ FixCaseRoutineDone .label
\     readline_done !, out0 terminator cv bceq,
\     out0 loc0 sttincr,
\     readline_loop !, monitor-input-end #, loc0 cv bclti,
\     readline_done !, out0 terminator cv bceq,
\ readline_consume_rest_of_line .label
\ 	$KEY \ get the key
\     readline_consume_rest_of_line !, out0 terminator cv bcneq,
\ readline_done .label 
\     \ save the length in memory
\     monitor-input-start #, at0 set,
\     at0 loc0 out0 usub,
\     out0 at0 st,
\     \ terminator is used to terminate early
\     1 restore-locals
\     fn)
\ 
\ printline (fn
\     \ in0 - start address
\     \ in1 - length
\     /dev/console0 #->io
\     WriteRangeToIOAddressRoutine !, call,
\ 	$NEWLINE
\ fn)
\ 
\ 
\ check-overflow
asm}
bye
