" iris.fs" open-input-file
" routines.rom" open-input-file
" boot.rom" {bin

\ generic computer instructions from threaded interpretive languages book


0xF000 constant input-buffer-start
0xF100 constant input-buffer-end
0xD000 constant dictionary-start
0xE800 constant data-stack-start
0xF000 constant call-stack-start

\ code start


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
