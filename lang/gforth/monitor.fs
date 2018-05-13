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
deflabel Start
deflabel Restart
deflabel CoreDictionaryStart
deflabel FixCaseRoutine
r63 constant &InputRoutine
r62 constant &TerminateExecutionRoutine
r61 constant &Restart
r60 constant dp
r59 constant arg0
r58 constant arg1
r57 constant ret0
r56 constant ?sysinit
r55 constant &Start
routines-start .org
TerminateExecutionRoutine .label
	/dev/terminate-vm #->io
	arg0 io-write
	ret, 
FixCaseRoutine .label
	deflabel DoneFixCaseRoutine
	\ look in a given range
	97 #, arg0 cv lti,
	DoneFixCaseRoutine !, cv bc,
	122 #, arg0 cv gti,
	DoneFixCaseRoutine !, cv bc,
	32 #, arg0 arg0 subi,
	DoneFixCaseRoutine .label
	arg0 ret0 ->
	ret,
	
dictionary-start .org
CoreDictionaryStart .label
0x0000 .org
	zero ?sysinit ->
\ initialization code goes here
Start .label
	zero ?sysinit cv neq,
	cv &Restart bcr, \ skip over the 
	Start !, &Start $->
	Restart !, &Restart $->
	TerminateExecutionRoutine !, &TerminateExecutionRoutine $->
	InputRoutine !, &InputRoutine $->
	0xFFFF #, ?sysinit $->
Restart .label
	data-stack-start #, dsp $->
	call-stack-start #, csp $->
	CoreDictionaryStart !, dp $->
	0x20 #, separator $->
	16 #, num-base $->
	zero error-code ->
	zero ci ->
InputRoutine .label
	&TerminateExecutionRoutine br,
asm}
bye
