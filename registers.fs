" iris.fs" open-input-file
fixed-registers-stop ={enum
enum: ir \ instruction register
              \ contains the address of the next instruction in the threaded
              \ list of the current secondary
enum: wa \ word address register
              \ contains the word address of the current keyword or the address
              \ of the first code body location of the current keyword
enum: ca \ code address register
enum: rs \ return stack register
enum: sp \ stack pointer
enum: pc \ processor program counter register
enum: t0 \ temporary 0
enum: t1 \ temporary 1
enum: t2 \ temporary 2
enum: top \ top of the stack
enum: lower \ lower element of the stack
enum: third \ third element of the stack
enum: io \ io device number or address
enum: ci \ core index number
enum: ibcurr \ input buffer current position
enum: ibend \ input buffer end
enum}

;s
