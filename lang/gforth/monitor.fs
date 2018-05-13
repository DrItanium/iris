include iris.fs
s" monitor.o" {asm
0xF000 constant input-buffer-start
0xF100 constant input-buffer-end
0xD000 constant dictionary-start
0xE800 constant data-stack-start
0xF000 constant call-stack-start
0xA000 constant routines-start
deflabel InputRoutine
deflabel TerminateExecutionRoutine
deflabel Restart
deflabel CoreDictionaryStart
r63 constant &InputRoutine
r62 constant &TerminateExecutionRoutine
r61 constant &Restart
r60 constant dp
r59 constant arg0
r58 constant arg1
r57 constant ret0
r56 constant ?sysinit
routines-start .org
TerminateExecutionRoutine .label
	/dev/terminate-vm #->io
	arg0 io-write
	ret, 
	
dictionary-start .org
CoreDictionaryStart .label
0x0000 .org
Restart !, &Restart $->
TerminateExecutionRoutine !, &TerminateExecutionRoutine $->
InputRoutine !, &InputRoutine $->
0xFFFF #, ?sysinit $->
\ initialization code goes here
Restart .label
	data-stack-start #, dsp $->
	call-stack-start #, csp $->
	CoreDictionaryStart !, dp $->
	0x20 #, separator $->
	16 #, num-base $->
	zero error-code ->
	zero ci ->
InputRoutine .label
	&TerminateExecutionRoutine lr brl,
asm}
bye
