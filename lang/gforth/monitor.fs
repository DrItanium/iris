include iris.fs
s" monitor.o" {asm
0xF000 constant input-buffer-start
0xF100 constant input-buffer-end
0xD000 constant dictionary-start
0xE800 constant data-stack-start \ 2048 entries
0xE000 constant data-stack-full 
0xF000 constant call-stack-start \ 2048 entries
0xE800 constant call-stack-full
0xA000 constant routines-start
0xE000 constant vmstack-start \ 2048 entries
0xD800 constant vmstack-full 
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
r54 constant t0
r53 constant t1
r52 constant t2
r51 constant ret1
: save-register ( reg -- ) vmsp psh-> ;
: restore-register ( reg -- ) vmsp swap pop-> ;
: save-lr ( -- ) lr save-register ;
: restore-lr ( -- ) lr restore-register ;
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
deflabel ReadLine
ReadLine .label
	deflabel ReadLineLoop
	deflabel ReadLineLoopDone
	save-lr
	t0 save-register
	t1 save-register
	t2 save-register
	zero t0 ->
	arg0 t2 ->
	\ arg0 - start location
	\ arg1 - length
	/dev/console0 #->io
	0xA #, terminator $->
	\ if length is zero then do nothing
	t2 cv eqz,
	ReadLineLoopDone !, cv bc,
	ReadLineLoop .label
	t2 t0 t1 add, 
	arg0 io-read
	FixCaseRoutine !, call,
	ret0 t1 sw,
	t0 1+,
	ret0 terminator cv eq,
	ReadLineLoopDone !, cv bc,
	t0 arg1 cv neq,
	ReadLineLoop !, cv bc,
	ReadLineLoopDone .label
	t0 ret1 ->
	t2 ret0 ->
	t2 restore-register
	t1 restore-register
	t0 restore-register
	restore-lr
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
	vmstack-start #, vmsp $->
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
