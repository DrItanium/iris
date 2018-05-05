\ register conventions
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
;s
