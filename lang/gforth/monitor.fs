include iris.fs
\ contains all of the registers and pieces used for the monitor itself
s" monitor.o" {asm
0xFFFF constant monitor-memory-end
0xFA00 constant monitor-routines-start
0xF400 constant monitor-input-end
0xF300 constant monitor-input-start
0xF200 constant monitor-stack-start 
0xF100 constant monitor-stack-end \ 512 elements
0xF000 constant monitor-memory-start
monitor-memory-start constant monitor-loop

0x0000 .org monitor-loop #, jmp

deflabel TerminateExecutionRoutine
deflabel FixCaseRoutine 
deflabel PrintCharactersRoutine
deflabel WriteRangeToIOAddressRoutine
deflabel $KEY 
deflabel $ECHO 
deflabel $HEX
deflabel $->HEX
deflabel $NEWLINE
deflabel $SPACE
deflabel $PROMPT
deflabel printline
deflabel printbuf
deflabel printinput
monitor-routines-start .org
TerminateExecutionRoutine .label
    /dev/terminate-vm #->io
    arg0 io-write \ this will not return

FixCaseRoutine (leafn 
deflabel FixCaseRoutineDone
    \ arg0 - character to fix case of
    0x61 #, arg0 cv lti,
    FixCaseRoutineDone !, cv bc,
    0x7a #, arg0 cv gti, 
    FixCaseRoutineDone !, cv bc,
    0x20 #, arg0 arg0 subi, 
    FixCaseRoutineDone .label
    arg0 out0 ->
    leafn)
 FixCaseRoutine
WriteRangeToIOAddressRoutine (leafn
	\ arg0 - starting point in memory
	\ arg1 - length
	deflabel WriteRangeToIOAddress_Done
	deflabel WriteRangeToIOAddress_Loop
    2 save-locals
	zero loc0 ->
	arg1 cv eqz,
	WriteRangeToIOAddress_Done !, cv bc,
	WriteRangeToIOAddress_Loop .label
	arg0 loc0 loc1 uadd, \ uadd bro!?
	loc1 loc1 ld,
	loc1 io-write
	loc0 1+,
	loc0 arg1 cv gt,  
	WriteRangeToIOAddress_Loop !, cv bc,
	WriteRangeToIOAddress_Done .label
    2 restore-locals
    leafn)
$KEY (fn 
    \ resets the keyboard and then awaits the next keyboard input
    \ The next input is return in a known register (in0)
    /dev/console0 #->io
    arg0 io-read
    FixCaseRoutine !, call,
    fn)
$->HEX (fn
    deflabel $->HEX_Done
    \ arg0 - value to hexify
    1 save-locals 
    0x30 #, arg0 loc0 subi,
    loc0 cv ltz,
    $->HEX_Done !, cv bc,
    0x09 #, loc0 cv lei, 
    deflabel $->HEX_IsDigit
    $->HEX_IsDigit !, cv bc,
    0x0A #, loc0 cv lti, 
    $->HEX_Done !, cv bc,
    0x7 #, loc0 loc0 subi,
    $->HEX_IsDigit .label
    $->HEX_Done .label
    loc0 out0 ->
    1 restore-locals
    fn)

$HEX (fn
    \ arg0 - starting location
    deflabel $HEX_DONE
    5 save-locals
    arg0 loc0 ld, \ first character
    arg0 1+,
    arg0 loc1 ld, \ second character
    arg0 1+,
    arg0 loc2 ld, \ third character
    arg0 1+,
    arg0 loc3 ld, \ fourth character
    loc0 arg0 ->
    $->HEX !, call,
    out0 loc4 ->
    loc1 arg0 ->
    $->HEX !, call,
    4 #, loc4 loc4 lshifti,
    out0 loc4 loc4 add,
    loc2 arg0 ->
    $->HEX !, call,
    4 #, loc4 loc4 lshifti,
    out0 loc4 loc4 add,
    loc3 arg0 ->
    $->HEX !, call,
    4 #, loc4 loc4 lshifti,
    out0 loc4 loc4 add,
    loc4 out0 ->
    5 restore-locals
    fn)
    \ reads the next four characters in as hexidecimal characters and converts them to hexidecimal numbers
        
$PROMPT (fn
      0x2D #, arg0 set,
      $ECHO !, call,
      $SPACE !, call,
      fn)
$SPACE .label
      0x20 #, arg0 set,
      $ECHO !, jmp
$NEWLINE .label
      0xA #, arg0 set,
$ECHO (leafn
      \ arg0, the character to write
      /dev/console0 #->io
      arg0 io-write
      leafn)

deflabel $HEX->KEY 
$HEX->KEY (leafn
    deflabel $HEX->KEY_DONE
    \ go backwards from what we originally got in
    \ arg0 - contains lowest 4 bits to convert
    0x000F #, arg0 arg0 andi,
    0xA #, arg0 cv lti,
    0x30 #, arg0 arg0 addi, \ always want to do this
    $HEX->KEY_DONE !, cv bc,
    0x7 #, arg0 arg0 addi,
    $HEX->KEY_DONE .label
    arg0 out0 ->
    leafn)
deflabel PRINT-NUMBER 
PRINT-NUMBER (fn
    deflabel print-number-done
    \ arg0 - number to print
    1 save-locals
    arg0 loc0 ->
    $NEWLINE !, call,
    0xC #, loc0 arg0 rshifti,
    $HEX->KEY !, call,
    out0 arg0 ->
    $ECHO !, call,
    8 #, loc0 arg0 rshifti,
    $HEX->KEY !, call,
    out0 arg0 ->
    $ECHO !, call,
    4 #, loc0 arg0 rshifti,
    $HEX->KEY !, call,
    out0 arg0 ->
    $ECHO !, call,
    loc0 arg0 ->
    $HEX->KEY !, call,
    out0 arg0 ->
    $ECHO !, call,
    print-number-done .label
    1 restore-locals
    fn)
deflabel readline 
readline (fn
deflabel readline_loop
deflabel readline_done
deflabel readline_consume_rest_of_line 
    2 save-locals 
    monitor-input-start #, loc0 set,
    loc0 1+,
    zero loc1 -> \ current
readline_loop .label 
    $KEY !, call,  \ get the key
    out0 loc1 ->
    loc1 terminator cv eq,
    readline_done !, cv bc,
    loc1 loc0 st,
    loc0 1+,
    monitor-input-end #, loc0 cv lti,
    readline_loop !, cv bc,
    loc1 terminator cv eq, 
    readline_done !, cv bc,
readline_consume_rest_of_line .label
    $KEY !, call,  \ get the key
    out0 terminator cv neq,
    readline_consume_rest_of_line !, cv bc,
readline_done .label 
    \ save the length in memory
    monitor-input-start #, at0 set,
    at0 loc0 at1 usub,
    at1 at0 st,
    at1 out0 ->
    \ terminator is used to terminate early
    2 restore-locals
    fn)

printinput .label
    monitor-input-start #, arg0 set,
printbuf .label
    arg0 arg1 ld, 
    arg0 1+,
printline (fn
    \ arg0 - start address
    \ arg1 - length
    /dev/console0 #->io
    WriteRangeToIOAddressRoutine !, call,
    $NEWLINE !, call,
fn)
deflabel DISPLAY_REGISTER
DISPLAY_REGISTER (leafn
    \ arg0 - register index
    /dev/register #->io
    arg0 io-write
    leafn)
: call-display-register ( reg -- ) 
    #, arg0 set, 
    DISPLAY_REGISTER !, call, 
    $SPACE !, call, ;
deflabel DISPLAY_REGISTER_L0
DISPLAY_REGISTER_L0 (fn
    r0 call-display-register 
    r1 call-display-register 
    r2 call-display-register 
    r3 call-display-register 
    r4 call-display-register 
    r5 call-display-register 
    r6 call-display-register 
    r7 call-display-register 
    $NEWLINE !, call,
fn)

deflabel DISPLAY_REGISTER_L1
DISPLAY_REGISTER_L1 (fn
    r8 call-display-register 
    r9 call-display-register 
    r10 call-display-register 
    r11 call-display-register 
    r12 call-display-register 
    r13 call-display-register 
    r14 call-display-register 
    r15 call-display-register 
    $NEWLINE !, call,
fn)

deflabel DISPLAY_REGISTER_L2
DISPLAY_REGISTER_L2 (fn
    r16 call-display-register 
    r17 call-display-register 
    r18 call-display-register 
    r19 call-display-register 
    r20 call-display-register 
    r21 call-display-register 
    r22 call-display-register 
    r23 call-display-register 
    $NEWLINE !, call,
fn)

deflabel DISPLAY_REGISTER_L3
DISPLAY_REGISTER_L3 (fn
    r24 call-display-register 
    r25 call-display-register 
    r26 call-display-register 
    r27 call-display-register 
    r28 call-display-register 
    r29 call-display-register 
    r30 call-display-register 
    r31 call-display-register 
    $NEWLINE !, call,
fn)
deflabel DISPLAY_REGISTER_L4
DISPLAY_REGISTER_L4 (fn
    r32 call-display-register 
    r33 call-display-register 
    r34 call-display-register 
    r35 call-display-register 
    r36 call-display-register 
    r37 call-display-register 
    r38 call-display-register 
    r39 call-display-register 
    $NEWLINE !, call,
fn)

deflabel DISPLAY_REGISTER_L5
DISPLAY_REGISTER_L5 (fn
    r40 call-display-register 
    r41 call-display-register 
    r42 call-display-register 
    r43 call-display-register 
    r44 call-display-register 
    r45 call-display-register 
    r46 call-display-register 
    r47 call-display-register 
    $NEWLINE !, call,
fn)
deflabel DISPLAY_REGISTER_L6
DISPLAY_REGISTER_L6 (fn
    r48 call-display-register 
    r49 call-display-register 
    r50 call-display-register 
    r51 call-display-register 
    r52 call-display-register 
    r53 call-display-register 
    r54 call-display-register 
    r55 call-display-register 
    $NEWLINE !, call,
fn)
deflabel DISPLAY_REGISTER_L7
DISPLAY_REGISTER_L7 (fn
    r56 call-display-register 
    r57 call-display-register 
    r58 call-display-register 
    r59 call-display-register 
    r60 call-display-register 
    r61 call-display-register 
    r62 call-display-register 
    r63 call-display-register 
    $NEWLINE !, call,
fn)
deflabel DISPLAY_REGISTERS
DISPLAY_REGISTERS (fn
    $NEWLINE !, call,
    $NEWLINE !, call,
    DISPLAY_REGISTER_L0 !, call,
    DISPLAY_REGISTER_L1 !, call,
    DISPLAY_REGISTER_L2 !, call,
    DISPLAY_REGISTER_L3 !, call,
    DISPLAY_REGISTER_L4 !, call,
    DISPLAY_REGISTER_L5 !, call,
    DISPLAY_REGISTER_L6 !, call,
    DISPLAY_REGISTER_L7 !, call,
    $NEWLINE !, call,
    fn)
monitor-loop .org
deflabel monitor-loop-start
deflabel monitor-call-shutdown
    0xA #, terminator set,
    monitor-stack-start #, vmsp set,
    0x10 #, num-base set,
monitor-loop-start .label
    DISPLAY_REGISTERS !, call,
    $PROMPT !, call,
    readline !, call,
    5 #, out0 cv lti, 
    monitor-loop-start !, cv bc,
    monitor-input-start #, arg0 set,
    arg0 1+,
    $HEX !, call,
    out0 arg0 -> 
    PRINT-NUMBER !, call,
    $NEWLINE !, call,
    printinput !, call,
    monitor-loop-start !, b,
    zero arg0 ->
monitor-call-shutdown .label
    TerminateExecutionRoutine !, jmp

asm}
bye
