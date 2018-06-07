include iris.fs
\ contains all of the registers and pieces used for the monitor itself
\ the monitor is now also the forth system itself
s" figforth.o" {asm
\ the core memory is a disk buffer of a kind so it will become the disk buffer 
\ of legend that is being discussed in the forth book.
0xFFFF constant ram-end
0xFF00 constant io-start
0xF000 constant system-start
0x0000 constant ram-start
ram-start constant interpreter-start
\ 0x0100 constant bootstrap-end
0xF000 constant &LIMIT
0xE000 constant &FIRST \ address of the first byte of the disk buffers
\ 0xF000 - 0xF100 \ system variables
\ 0xF100 - 0xF400 \ data stack
\ 0xF400 - 0xF600 \ return stack
\ 0xF600 - 0xF700 \ input buffer
\ 0xF800 - 0xF900 \ output buffer
0xF100 constant system-variables-end
system-variables-end 0x100 - constant system-variables-start
system-variables-end constant data-stack-end
data-stack-end 0x300 + constant data-stack-start
data-stack-start constant return-stack-end
return-stack-end 0x200 + constant return-stack-start
return-stack-start constant input-buffer-start
input-buffer-start 0x100 + constant input-buffer-end
input-buffer-end constant output-buffer-start
output-buffer-start 0x100 + constant output-buffer-end


\ bootstrap-end constant dictionary-start

\ register reservations
: too-many-vars-defined ( addr -- ) 0x40 >= ABORT" To many registers used!" ;
deflabel forth_vocabulary_start
deflabel base-dict-done
deflabel &S0 \ initial value of the data stack pointer
deflabel &R0 \ initial value of the return stack pointer
deflabel &TIB \ address of the terminal input buffer
deflabel &WARNING \ error message control number. If 1, disk is present, 
                  \ and screen 4 of drive 0 is the base location of error messages
                  \ if 0, no disk is present and error messages will be presented
                  \ by number. If -1, execute (ABORT) on error
deflabel &FENCE \ address below which FORGETting is trapped.
                \ To forget below this point, the user must alter the contents
                \ of FENCE
deflabel &DP \ The dictionary pointer which contains the next free memory
             \ above the dictionary. The value may be read by HERE and altered
             \ by ALLOT.
deflabel &VOC-LINK \ address of a field in the definition of the most recently created
                   \ created vocabulary. All vocabulary names are linked by
                   \ these fields to allow control for FORGETting through multiple
                   \ vocabularies
deflabel &BLK \ current block number under interpretatio. If 0, input is
              \ being taken from the terminal input buffer
deflabel &IN  \ Byte offset within the current input text buffer (terminal or
              \ disk) from which the next text will be accepted. WORD uses and
              \ move the value of IN
deflabel &OUT \ Offset in the text output buffer. Its value is incremented by EMIT
              \ The user may yalter and examine OUT to control output display formatting.
deflabel &SCR      \ Screen number most recently referenced by LIST
deflabel &OFFSET   \ Block offset disk drives. Contents of OFFSET is added to the stack number by BLOCK
deflabel &CONTEXT  \ pointer to the vocabulary within which dictionary search
                   \ will first begin
deflabel &CURRENT  \ Pointer to the vocabulary in which new definitions are to be added
deflabel &STATE    \ If 0, the system is in interpretive or executing state. If non-zero, the system is in compiling state. The value itself is implementation
                   \ dependent. So in this case it would be 0 is interpretive and 0xFFFF is compiling
deflabel &DPL      \ number of digits to the right of the decimal point on double integer input. It may also be used to hold output column
                   \ location of a decimal point in user generated formatting. The default value on single number input is -1
deflabel &FLD      \ field width for formatted number output
deflabel &CSP      \ temporarily stored data stack pointer for compilation error checking
deflabel &R#       \ location of editor cursor in a text screen
deflabel &HLD      \ address of the latest character of text during numeric output conversion
deflabel &SEPARATOR \ word separator contents
deflabel &TERMINATOR \ terminator index
deflabel &BASE       \ numeric base
unused-start 
\ constants
\ user variables
1+cconstant xsp \ data stack pointer
1+cconstant xrp \ return stack pointer
1+cconstant xip \ interpretive pointer
1+cconstant xw \ current word pointer
1+cconstant xo \ accumulator register ( according to figforth )
1+cconstant at2 \ another temporary
1+cconstant xtop \  contents of the top of stack when a pop is called
1+cconstant xlower \ contents of the second stack item when a pop is called
1+cconstant xthird \ contents of the third stack item
1+cconstant xdp    \ temporary storage container for the dictionary pointer
1+cconstant xtaddr \ temporary storage for an address
1+cconstant xstate \ temporary storage for the state variable
1+cconstant xfourth \ contents of the fourth stack item
too-many-vars-defined


\ set the constants
&LIMIT constant xlimit
&FIRST constant xfirst
0x8 constant b/scr
0x80 constant b/buf 
variable last-word
0 last-word !
\ program start
0x1000 .org \ dictionary starts at 0x1000
\ jump to COLD most likely here
deflabel-here _next
    xip xw -> \ move the contents of xip (which points to the next word to be executed, into xw .
    xip 1+, \ Increment xip, pointing to the second word in execution sequence.
    xw at0 ldtincr, \ load the contents of xw into at0 and then increment xw
                    \ this will make xw point to the parameter field of the word
    at0 br,         \ jump to the address found at that point
: next, ( -- ) _next !, b, ;
: machine-code-execute ( -- ) loc@ 1+ #, .data16 ;
: machineword ( n -- ) .label machine-code-execute ;
: machine-code-jump ( imm id -- ) 
  at0 set,
  at0 at0 ld,
  at0 br, ;
: embed-name ( str length -- ) 
  dup #, .data16 \ embed length into its own storage
  swap @ swap \ make sure we load the front string address
  case 
  	1 of 0xFF and #, .data16 
	     0 #, .data16 endof
	2 of 0xFFFF and #, .data16
	     0 #, .data16 endof
	3 of dup 0xFFFF and #, .data16
	     0xFF0000 and 16 rshift #, .data16 endof
	dup 0xFFFF and #, .data16
	0xFFFF0000 and 16 rshift #, .data16 
   endcase ;

0 constant word/none
1 constant word/smudge
2 constant word/imm
word/imm word/smudge or constant word/all
: defmachineword-base ( str length control-bits "name" -- ) 
  deflabel-here 
  #, .data16 \ stash the control bits here
  embed-name
  last-word @ !, .data16
  execute-latest dup 
  last-word !
  machine-code-execute ;
: defmachineword ( str length "name" -- ) word/none defmachineword-base ;
: embed-string-length ( len -- ) #, .data16 ;
deflabel-here _execute
    \ execute the definition whose code field address cfa is on the data stack
    xsp xw pop, \ pop the code field address into xw, the word pointer
    xw at0 ldtincr, \ Jump indirectly to the code routine. Increment xw to point to the parameter field
    at0 br, 
deflabel-here _docolon 
	\ runtime routine for all colon definitions
    xip xrp push,  \ push the address of the next word to the return stack and enter a lower nesting level
    xw xip -> \ move the parameter field address into IP, pointing to the first word in this definition
    \ duplicate NEXT for now
    next,
: embed-docolon ( -- ) _DOCOLON !, .data16 ;
: colondef ( n -- ) .label embed-docolon ;
deflabel-here _;S
\ _;S .label \ perform unnesting
\ return execution to the calling definition. Unnest one level.
    xrp xip pop, \ pop the return stack into xip, pointing now to the next word to be executed in the calling definition
    next,
: ;s, ( -- ) _;S !, b, ;
deflabel-here _push
s" push" embed-name 
0 #, .data16
_push machine-code-execute
_push last-word !
    xo xsp push, 
    next,
s" pop" defmachineword _pop
    xsp zero pop,
    next,
s" put" defmachineword _put
    \ replace the top of data stack with the contents of the accumulator
    xo xsp st, 
    next,
s" lit" defmachineword _lit
    \ push the next word to the data stack as a literal. Increment IP and skip this literal.
    \ NEXT Return
    \ LIT is used to compile numbers into the dictionary. At run-time, LIT pushes the 
    \ inline literal to the data stack to be used in computations
    xip xsp push,
    xip 1+,
    next,
: push-literal ( n -- )
  \ compile the literal into the dictionary by putting the _LIT command followed by
  \ the number itself
  _LIT !, .data16
  #, .data16 ;

: 1pop ( -- )
  xsp xtop pop, ;
: 2pop ( -- )
  1pop 
  xsp xlower pop, ;
: 3pop ( -- )
  2pop
  xsp xthird pop, ;
: defbinaryop ( str length "name" "op" -- )
  defmachineword 
  2pop
  xtop xlower xtop ' execute 
  xtop xsp push,
  next, ;
s" +" defbinaryop _+ add,
s" -" defbinaryop _- sub, 
s" *" defbinaryop _* mul,
s" /" defbinaryop _/ div, 
s" mod" defbinaryop _mod rem,
s" min" defbinaryop _min min,
s" max" defbinaryop _max max,
s" and" defbinaryop _and and,
s" or"  defbinaryop _or or,
s" xor" defbinaryop _xor xor,
s" <"  defbinaryop _< lt,
s" >" defbinaryop _> gt,
s" =" defbinaryop _= eq,
s" !=" defbinaryop _!= neq,
s" >=" defbinaryop _>= ge,
s" <=" defbinaryop _<= le,
s" nand" defbinaryop _nand nand,
s" nor" defbinaryop _nor nor,
s" lshift" defbinaryop _lshift lshift,
s" rshift" defbinaryop _rshift rshift,
s" rot" defmachineword _rot ( a b c -- b c a )
    3pop \ a (third) b (lower)  c (top)
    xlower xsp push,
    xtop xsp push,
    xthird xsp push,
    next,
s" -dup" defmachineword _-dup \ duplicate if non zero
    deflabel _-dup_done
    1pop 
    xtop cv eqz, 
    _-dup_done !, cv bc,
        xtop xsp push,
    _-dup_done .label
    xtop xsp push,
    next,
s" >r" defmachineword _>r \ move top item to return stack
    1pop 
    xtop xrp push,
    next,
s" r>" defmachineword _r> \ retrieve item from top of return stack
    xrp xtop pop,
    xtop xsp push,
    next,
s" r" defmachineword _r \ copy top of return stack onto stack
   xrp xtop ld,
   xtop xsp push,
   next,
s" abs" defmachineword _abs
    deflabel _absDone
    1pop
    xtop cv gez,
    _absDone !, cv bc,
    0xFFFF #, xtop xtop muli, \ multiply by negative one
    _absDone .label
    xtop xsp push,
    next,

s" minus" defmachineword _minus
    deflabel _minusDone
    1pop
    0xFFFF #, xtop xtop muli, \ multiply by negative one
    xtop xsp push,
    next,

s" 0<" defmachineword _0<
    1pop
    xtop xtop ltz,
    xtop xsp push,
    next,
s" 0=" defmachineword _0=
    1pop
    xtop xtop eqz,
    xtop xsp push,
    next,

s" over" defmachineword _over 
    xsp xtop incr,
    xtop xlower ld,
    xlower xsp push,
    next,
s" dup" defmachineword _dup 
    xsp xtop ld,
    xtop xsp push,
    next,
s" drop" defmachineword _drop 
    xsp zero pop, 
    next,

s" swap" defmachineword _swap
	2pop \ top - a 
		 \ lower - b
    xtop xsp push, 
    xlower xsp push,
    next,
s" !" defmachineword _! ( v a -- )
   2pop \ top - addr
   		\ lower - value
   xlower xtop st, \ perform the store
   next,

s" c," defmachineword _c,
	1pop
    0xFF #, xtop xtop andi, 
	&DP xtaddr set,
	xtaddr xdp ld, 
    xtop xdp st, \ save it to the current dict pointer front
    xdp 1+, \ move ahead by one
	xdp xtaddr st,
    next,
s" c!" defmachineword _c!  ( value addr -- )
	2pop \ top - addr
		 \ lower - value
    0xFF #, xlower xlower andi,
    xlower xtop st, \ save it to memory with the upper 8 bits masked
    next,
s" c@" defmachineword _c@
    1pop \ top - addr
    xtop xtop ld,
    0xFF #, xtop xtop andi,
    xtop xsp push,
    next,
s" @" defmachineword _@
    1pop
    xtop xtop ld,
    xtop xsp push,
    next,
s" ." defmachineword _.
   /dev/console2 #, xlower set,
   1pop
   xtop xlower st,
   next,
s" cr" defmachineword _cr
    /dev/console0 #, xlower set,
    0xA #, xtop set,
    xtop xlower st,
    next,
s" space" defmachineword _space
    /dev/console0 #, xlower set,
    0x20 #, xtop set,
    xtop xlower st,
    next,
s" key" defmachineword _key
    /dev/console0 #, xlower set,
    xlower xtop ld,
    xtop xsp push,
    next,
s" emit" defmachineword _emit
    /dev/console0 #, xlower set,
    1pop
    xtop xlower st,
    next,
s" sp@" defmachineword _sp@
    xsp xsp push,
    next,

s" ?" defmachineword _?
    1pop 
    xtop xtop ld,
    /dev/console0 #, xlower set,
    xtop xlower st,
    next,
    
s" +!" defmachineword _+!
    2pop \ top - addr 
         \ lower - n
    xtop at0 ld,
    xlower at0 xlower add, 
    xlower xtop st,
    next,
s" cmove" defmachineword _cmove ( from to u -- ) \ move u bytes in memory 
    3pop \ top - u
         \ lower - to
         \ third - from
    deflabel _cmove_loop 
    deflabel _cmove_done
    _cmove_loop .label
    xtop cv ltz,
    _cmove_done !, cv bc,
    xlower at0 ldtincr,
    at0 xthird sttincr,
    xtop 1-,
    xlower 1+,
    xthird 1+,
    _cmove_loop !, b,
    _cmove_done .label
    next,
s" fill" defmachineword _fill ( addr n b -- ) 
	\ fill u bytes in memory with b beginning at address
    3pop \ top - b
         \ lower - u
         \ third - addr
    deflabel _fill_loop
    deflabel _fill_done
    _fill_loop .label
    xlower cv eqz,
    _fill_done !, cv bc,
    xtop xthird sttincr,
    xlower 1-,
    _fill_loop !, b,
    _fill_done .label
    next,
s" bl" defmachineword _bl
    bl #, xsp pushi,
    next,

s" here" defmachineword _here
	&DP xtaddr set,
	xtaddr xdp ld, 
    xdp xsp push,
    next,
s" allot" defmachineword _allot ( n -- )
	1pop
	&DP xtaddr set,
	xtaddr xdp ld, 
    xtop xdp xdp add,
	xdp xtaddr st,
    next,
s" pad" defmachineword _pad ( -- n )
	&DP xtaddr set,
	xtaddr xdp ld, 
    0x44 #, xdp xtop addi,
    xtop xsp push,
    next,
s" ," defmachineword _, ( n -- )
    \ store n into the next available cell above dictionary and advance DP by 2 thus
    \ compiling into the dictionary
	1pop 
	&DP xtaddr set,
	xtaddr xdp ld, 
    xtop xdp st, \ save it to the current dict pointer front
    xdp 1+, \ move ahead by one
	xdp xtaddr st,
    next,
s" 0" defmachineword _zero
    zero xsp push,
    next,
s" [" word/imm defmachineword-base _leftbracket
	&state !, xtaddr set,
	zero xtaddr st,
    next,
s" ]" defmachineword _rightbracket
	&state !, xtaddr set,
    0xFFFF #, xstate set,
	xstate xtaddr st,
    next,

s" dodoes" defmachineword _dodoes_prime
    xip xrp push,
    xw xip move,
    xw 1+,
    xw xsp push,
    next,
s" branch" defmachineword _branch
	xip xtop ld,
	xtop xip xip add,
	next,
s" 0branch" defmachineword _0branch
	deflabel _zbra1
	1pop \ flag
	zero xtop cv neq,
	_zbra1 !, cv bc,
	xip xlower ld,
	xip xlower xip add,
	next,
	_zbra1 .label
	0x2 #, xip xip addi,
	next,
s" ?comp" defmachineword _?comp
	&state !, xtaddr set,
	xtaddr xstate ld,
	zero xstate cv neq,
	cv xsp push,
	next,
s" lfa" defmachineword _lfa \ convert the parameter field address to link field address
	1pop
	4 #, xtop xtop subi, 
	xtop xsp push,
	next,
s" cfa" defmachineword _cfa \ convert the parameter field address to code field address
	1pop
	2 #, xtop xtop subi,
	xtop xsp push,
	next,
s" latest" defmachineword _latest \ leave the name field address of the last word defined in the current vocabulary
	&current !, xtaddr set,
    xtaddr xtop ld,
	xtop xtop ld,
	xtop xsp push,
	next,
s" hex" defmachineword _hex \ set the base to 16
	&base !, xtaddr set,
	0x10 #, at0 set,
	at0 xtaddr st,
	next,
s" decimal" defmachineword _decimal \ set the base to 10
	&base !, xtaddr set,
	0xA #, at0 set,
	at0 xtaddr st,
	next,
s" octal" defmachineword _octal \ set the base to 8
	&base !, xtaddr set,
	0x8 #, at0 set,
	at0 xtaddr st,
	next,
s" c/l" defmachineword _c/l 
	0x40 #, xtop set,
	xtop xsp push,
	next,
s" b/buf" defmachineword _b/buf
	b/buf #, xtop set,
	xtop xsp push,
	next,
s" b/scr" defmachineword _b/scr
	b/scr #, xtop set,
	xtop xsp push,
	next,
s" immediate" defmachineword _immediate
	\ mark the current dictionary entry as immediate
	&dp !, xtaddr set,
	xtaddr xtop ld,
	xtop xlower ld,
	word/imm #, at0 set,
	xlower at0 xlower or,
	xlower xtop st,
	next,
s" (do)" defmachineword _(do)
	2pop 
	xlower xrp push,
	xtop xrp push,
	next,
s" I" defmachineword _I
	xrp xtop ld, \ load the top loop element
	xtop xsp push, \ put it onto the data stack
	next,
s" leave" defmachineword _leave
	\ make the loop limit equal to the loop count and force the loop to
	\ terminate at loop or +loop
	\ copy loop count to loop limit on return stack
	xrp xtop pop,
	xrp zero pop,
	xtop xrp push,
	xtop xrp push,
	next,
s" (loop)" defmachineword _(loop)
	deflabel loop_1
	\ runtime routine of loop
	xrp xtaddr move,
	xtaddr xtop ld, 
	xtop 1+,
	xtop xtaddr st,
	2 #, xtaddr xtaddr addi,
	xtaddr xlower ld,
	xtop xlower cv ge, 
	loop_1 !, cv bc,
	xip at0 ld, \ add backwards branch offset to IP and branch back to the DO-LOOP
	at0 xip xip add, 
	next,
	loop_1 .label
	\ discard the loop parameters off the return stack
	xrp zero pop,
	xrp zero pop,
	2 #, xip xip addi, \ advance ip over the inline offset number and continue
		               \ executing the next word after loop
	next,

s" (+loop)" defmachineword _(+loop)
	deflabel loop_2
	deflabel loop_3
	\ runtime routine at the end of a DO --+loop loop
	1pop
	xrp xlower ld,
	xtop xlower xlower add,
	xlower xrp st,
	zero xtop cv lt, \ if n is less than zero, jump to loop3 for special processing
	loop_3 !, cv bc, \ jump to loop3 for special processing
	xrp xtaddr move,
	xtaddr at0 ld, \ loop count
	xtaddr 1+,
	xtaddr at1 ld, \ loop limit
	at0 at1 cv le,
	loop_2 !, cv bc,
	xip at0 ld,
	at0 xip xip add,
	next,
	loop_2 .label
		xrp zero pop,
		xrp zero pop,
		2 #, xip xip addi,
		next,
	loop_3 .label
	xtaddr at0 ld, \ loop count
	xtaddr 1+,
	xtaddr at1 ld, \ loop limit
	at1 at0 cv le, \ negative increment n, reverse comparison
	loop_2 !, cv bc,
	xip at0 ld,
	at0 xip xip add, \ not yet done with the loop. Return to the word after DO
	next,
s" 2drop" defmachineword _2drop ( a b -- ) 2pop next,
s" 2dup" defmachineword _2dup ( a b -- a b a b ) 
	xsp xtaddr move,
	xtaddr xtop ld,
	xtaddr 1+,
	xtaddr xlower ld,
	xlower xsp push,
	xtop xsp push,
	next,
s" 2swap" defmachineword _2swap ( a b c d -- c d a b ) 
	3pop \ d - top
		 \ c - lower
		 \ b - third
	xsp xfourth pop, \ a - fourth
	xlower xsp push,
    xtop xsp push,
	xfourth xsp push,
	xthird xsp push,
	next,
: assign-variable, ( value type address type -- )
  xtaddr set,
  xtop set,
  xtop xtaddr st, ;
s" forth" defmachineword _forth
	\ set the context to the forth base vocabulary
	forth_vocabulary_start !, &context !, assign-variable,
	next,
.label forth_vocabulary_start
s" terminate" defmachineword _terminate
	/dev/terminate-vm #, xtaddr set,
	zero xtaddr st,
	next,
base-dict-done .label \ always is the front address
: zero-variable, ( address type -- ) 2>r 0 #, 2r> assign-variable, ;
ram-start .org
	\ setup the data stack pointer
	data-stack-start #, &S0 !, assign-variable,
	xtop xsp move,
	\ setup the return stack pointer
	return-stack-start #, &R0 !, assign-variable,
	xtop xrp move,
	input-buffer-start #, &tib !, assign-variable, \ setup the terminal input buffer
	0x10 #, &base !, assign-variable,              \ setup the numeric base
	base-dict-done !, &fence !, assign-variable,   \ setup the fence
	base-dict-done !, &dp !, assign-variable,      \ setup the dictionary pointer
	&state !, zero-variable, 
	&warning !, zero-variable,
	forth_vocabulary_start !, &context !, assign-variable,      \ setup the context variable

: defvariableword ( label str-addr len "name" -- )
	defmachineword
	!, xtop set,
	xtop xsp push,
	next, ;
	
&state s" state" defvariableword _state
&base s" base" defvariableword _base
&current s" current" defvariableword _current
&tib s" tib" defvariableword _tib
&s0 s" s0" defvariableword _s0
&r0 s" r0" defvariableword _r0
&warning s" warning" defvariableword _warning
&dp s" dp" defvariableword _dp
&fence s" fence" defvariableword _fence
&voc-link s" voc-link" defvariableword _voc-link
&blk s" blk" defvariableword _blk
&in s" in" defvariableword _in
&out s" out" defvariableword _out
&dpl s" dpl" defvariableword _dpl
&fld s" fld" defvariableword _fld
&csp s" csp" defvariableword _csp
&r#  s" r#" defvariableword _r#
&hld s" hld" defvariableword _hld
&separator s" separator" defvariableword _separator
&terminator s" terminator" defvariableword _terminator
system-start .org \ system variables
&state .label 0 #, .data16
&base .label 0x10 #, .data16
&tib  .label 0 #, .data16
&s0   .label 0 #, .data16
&r0   .label 0 #, .data16
&warning   .label 0 #, .data16
&fence .label 0 #, .data16
&dp .label 0 #, .data16
&voc-link .label 0 #, .data16
&blk .label 0 #, .data16
&in .label 0 #, .data16
&out .label 0 #, .data16
&current .label 0 #, .data16
&dpl  .label 0 #, .data16
&fld  .label 0 #, .data16
&csp  .label 0 #, .data16
&r#   .label 0 #, .data16
&hld  .label 0 #, .data16
&separator .label 0 #, .data16
&terminator .label 0 #, .data16
&context .label 0 #, .data16
asm}

bye
