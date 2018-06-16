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

\ ascii characters used
0x20 constant aspace \ ascii space
0x0d constant acr \ carriage return
0x2e constant adot \ period
0x7 constant abell \ bell
0x5f constant absin  \ input delete char
0x08 constant absout  \ output backspace 
0x10 constant adle \ (^p)
0x0a constant alf \ line feed
0x0c constant aff \ form feed

\ memory allocation
ram-end 1+ constant EM \ end of memory


\ bootstrap-end constant dictionary-start

\ register reservations
: too-many-vars-defined ( addr -- ) 0x40 >= ABORT" To many registers used!" ;
deflabel _origin
deflabel forth_vocabulary_start
deflabel _cold
deflabel _abort
deflabel _quit
deflabel base-dict-done
deflabel _handle-error
deflabel _interpret
deflabel &up
deflabel &porigin
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
deflabel &BLK \ current block number under interpretation. If 0, input is
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
deflabel &width 	\ width of some kind
unused-start 
\ constants
\ user variables
1+cconstant xsp \ data stack pointer
1+cconstant xrp \ return stack pointer
1+cconstant xip \ interpretive pointer
1+cconstant xw \ current word pointer
1+cconstant xtop \  contents of the top of stack when a pop is called
1+cconstant xlower \ contents of the second stack item when a pop is called
1+cconstant xthird \ contents of the third stack item
1+cconstant xfourth \ contents of the fourth stack item
1+cconstant xtaddr \ temporary storage for an address
1+cconstant xerror \ error code
1+cconstant xcoreid \ current core section id
too-many-vars-defined

: lit, ( n t -- ) xsp pushi, ;
: #lit, ( n -- ) #, lit, ;
: ??lit, ( n -- ) ??, lit, ;

\ set the constants
&LIMIT constant xlimit
&FIRST constant xfirst
0x8 constant b/scr
0x80 constant b/buf 
variable last-word
0 last-word !
\ program start
0x1000 .org \ dictionary starts at 0x1000
deflabel-here _2push xlower xsp push,
deflabel-here _1push xtop xsp push,
deflabel-here _next
    xip xw -> \ move the contents of xip (which points to the next word to be executed, into xw .
    xip 1+, \ Increment xip, pointing to the second word in execution sequence.
    xw at0 ldtincr, \ load the contents of xw into at0 and then increment xw
                    \ this will make xw point to the parameter field of the word
    at0 br,         \ jump to the address found at that point
: .skip ( -- ) 0 #, .cell ; 
: next, ( -- ) _next ??, b, ;
: 1push, ( -- ) _1push ??, b, ;
: x.scr ( -- ) hex .s decimal cr ;
: machine-code-execute ( -- ) loc@ 1+ #, .cell ;
: machineword ( n -- ) .label machine-code-execute ;
: machine-code-jump ( imm id -- ) 
  at0 set,
  at0 at0 ld,
  at0 br, ;
: embed-name ( str length -- ) 
  dup #, .cell \ embed length into its own storage
  swap @ swap \ make sure we load the front string address
  case 
  	1 of dup dup dup 
         0xFF and #, .cell 
         .skip .skip .skip endof
    2 of dup dup dup 
         0xFF and #, .cell 
         0xFF00 and 8 rshift #, .cell
         .skip
         .skip endof
	3 of dup dup dup
         0xFF and #, .cell 
         0xFF00 and 8 rshift #, .cell
         0xFF0000 and 16 rshift #, .cell 
         .skip endof
	swap \ the length must be consumed by endcase :(
	dup dup dup
    0xFF and #, .cell
    0xFF00 and 8 rshift #, .cell
    0xFF0000 and 16 rshift #, .cell
    0xFF000000 and 24 rshift #, .cell
   endcase ;


0 constant word/none
1 constant word/smudge
2 constant word/imm
word/imm word/smudge or constant word/all
\ format:
\ [0-5]: name field
\ 6: link field
\ 7: code field
\ 6-n: parameter field
\ address 5 in this case is where the label is pointing to
\ 0: control bits 
\ 1: length 
\ 2: char0
\ 3: char1
\ 4: char2
\ 5: char3
\ 6: next 
\ 7: address (interpreter routine) 
\ 8-n: body 
: defword-header ( str length control-bits "name" -- )
  loc@ >r \ stash a copy of the current location here!
  \ a shim to make next and docol unaware of encoding layout
  \ it does slow things down but it can't be helped at this point
  \ another revision will fix this but now I don't care
  #, .cell \ stash the control bits here
  embed-name \ stash three more bytes
  last-word @ #, .cell \ stash the previous word here
  r> last-word ! \ stash the top of this dictionary entry to last word
  ;
deflabel _(;code)
: defword-base ( str length control-bits "name" -- ) defword-header deflabel-here ( then define the label to point at here ) ;
: defword-base-predef ( label str length control-bits -- ) defword-header .label ;
: defmachineword-base ( str length control-bits "name" -- ) defword-base machine-code-execute ;
: defmachineword-base-predef ( label str length control-bits -- ) defword-base-predef machine-code-execute ;
: defmachineword ( str length "name" -- ) word/none defmachineword-base ;
: defmachineword-predef ( label str length -- ) word/none defmachineword-base-predef ;
: embed-string-length ( len -- ) #, .cell ;
deflabel _docolon
deflabel _?exec
deflabel _?csp
deflabel _!csp
deflabel _&current
deflabel _&context
deflabel _create
deflabel _leftbracket
deflabel _rightbracket
deflabel _compile
deflabel _smudge
deflabel _doconstant
deflabel _douser
deflabel _dovariable
deflabel _,
: embed-docolon ( -- ) _docolon ??, .cell ;
: defcolonword-base ( str length control-bits "name" -- ) defword-base embed-docolon ;
: defcolonword-base-predef ( label str length control-bits -- ) defword-base-predef embed-docolon ;
: defcolonword ( str length "name"  -- ) word/none defcolonword-base ;
: defcolonword-predef ( label str length -- ) word/none defcolonword-base-predef ;
: embed-doconstant ( -- ) _doconstant ??, .cell ;
: defconstantword-base ( str length control-bits "name" -- ) defword-base embed-doconstant ;
: defconstantword ( n -- ) word/none defconstantword-base ;
: embed-douser ( -- ) _douser ??, .cell ;
: defuserword-base ( str length control-bits "name" -- ) defword-base embed-douser ;
: defuserword ( n -- ) word/none defuserword-base ;
: embed-dovariable ( -- ) _dovariable ??, .cell ;
: defvariableword-base ( str length control-bits "name" -- ) defword-base embed-dovariable ;
: defvariableword ( n -- ) word/none defvariableword-base ;
: word, ( v -- ) ??, .cell ;

: 1pop ( -- )
  xsp xtop pop, ;
: 2pop ( -- )
  1pop 
  xsp xlower pop, ;
: 3pop ( -- )
  2pop
  xsp xthird pop, ;
: 4pop ( -- )
  3pop
  xsp xfourth pop, ;
: I, ( -- )
	xrp xtop ld, \ load the top loop element
	xtop xsp push, \ put it onto the data stack
	;
: enclose,, ( -- ) xsp enclose, ;
: key, ( -- )
	/dev/console0 #, xlower set,
	xlower xtop ld,
	xtop xsp push, ;
: emit, ( -- )
	/dev/console0 #, xlower set,
	1pop
	xtop xlower st, ;
: leave, ( -- ) 
		xrp xtop pop,
		xrp zero pop,
		xtop xrp push,
		xtop xrp push, ;
: r, ( -- ) 
	xrp xtop ld,
	xtop xsp push, 
;
: over, ( -- ) 
	2pop 
	xlower xsp push,
	xtop xsp push,
	xlower xsp push, ;
: drop, ( -- ) xsp zero pop, ;
: swap, ( -- )
	2pop \ top -- b
		 \ lower -- a
	xtop xsp push,
	xlower xsp push, ;
: dup, ( -- ) 
	xsp xtop ld,
	xtop xsp push, ;
: toggle, ( -- )
	2pop  \ top - addr
		  \ lower - pattern
	xtop xthird ld,
	xthird xlower xthird xor,
	xtop xthird st, ;
: !,, ( -- )
   2pop \ top - addr
   		\ lower - value
   xlower xtop st, \ perform the store
	;
: c!, ( -- )
	2pop \ top - addr
		 \ lower - value
    0xFF #, xlower xlower andi,
    xlower xtop st, \ save it to memory with the upper 8 bits masked
		;
: c@, ( -- )
	1pop 
	xtop xtop ld,
	0xFF #, xtop xtop andi,
	xtop xsp push, ;
: @, ( -- )
    1pop
    xtop xtop ld,
    xtop xsp push, ;
: here, ( -- ) 
	&DP ??, xtaddr set,
	xtaddr xtop ld,
	xtop xsp push, ;
: allot, ( -- )
	1pop
	&DP ??, xtaddr set,
	xtaddr xlower ld, 
    xlower xtop xlower add,
	xlower xtaddr st, ;
: ,, ( -- )
	1pop 
	&DP ??, xtaddr set,
	xtaddr xlower ld, 
    xtop xlower st, \ save it to the current dict pointer front
    xlower 1+, \ move ahead by one
	xlower xtaddr st, ;
s" lit" defmachineword _lit
    \ push the next word to the data stack as a literal. Increment IP and skip this literal.
    \ NEXT Return
    \ LIT is used to compile numbers into the dictionary. At run-time, LIT pushes the 
    \ inline literal to the data stack to be used in computations
	xip xtop ld,
    xip 1+,
	1push,
: two-cell-op ( n id op -- ) word, .cell ;
: push-literal ( n id -- )
  \ compile the literal into the dictionary by putting the _LIT command followed by
  \ the number itself
  _lit two-cell-op ;
s" execute" defmachineword _execute
	\ execute the definition whose code field address cfa is on the data stack
    xsp xw pop, \ pop the code field address into xw, the word pointer
    xw at0 ldtincr, \ Jump indirectly to the code routine. Increment xw to point to the parameter field
    at0 br, 
s" branch" defmachineword _branch
	xip xtop ld,
	xtop xip xip add,
	next,
: branch, ( location id -- ) _branch two-cell-op ;
s" 0branch" defmachineword _0branch
deflabel _zbra1
	1pop \ flag
	zero xtop cv neq,
	_zbra1 ??, cv bc,
	xip xlower ld,
	xip xlower xip add,
	next,
_zbra1 .label
	0x2 #, xip xip addi,
	next,
: zbranch, ( location id -- ) _0branch two-cell-op ;
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
	loop_1 ??, cv bc,
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
	loop_3 ??, cv bc, \ jump to loop3 for special processing
	xrp xtaddr move,
	xtaddr at0 ld, \ loop count
	xtaddr 1+,
	xtaddr at1 ld, \ loop limit
	at0 at1 cv le,
	loop_2 ??, cv bc,
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
	loop_2 ??, cv bc,
	xip at0 ld,
	at0 xip xip add, \ not yet done with the loop. Return to the word after DO
	next,
s" (do)" defmachineword _(do)
	2pop 
	xlower xrp push,
	xtop xrp push,
	next,

s" I" defmachineword _I
	xrp xtop ld, \ load the top loop element
	1push,
s" digit" defmachineword _digit
	xsp digit, 
	next,
s" (find)" defmachineword _(find)
    xsp pfind,
	next,
s" enclose" defmachineword _enclose
	enclose,,
	next,

s" emit" defmachineword _emit 
	emit, 
	next,
s" key" defmachineword _key 
	/dev/console0 #, xlower set,
	xlower xtop ld,
	1push,
\ TODO ?TERMINAL goes here
s" cr" defmachineword _cr
    /dev/console0 #, xlower set,
    0xA #, xtop set,
    xtop xlower st,
    next,
s" cmove" defmachineword _cmove ( from to u -- ) \ move u bytes in memory 
    3pop \ top - u
         \ lower - to
         \ third - from
    deflabel _cmove_loop 
    deflabel _cmove_done
_cmove_loop .label
    xtop cv ltz,
    _cmove_done ??, cv bc,
    xlower at0 ldtincr,
    at0 xthird sttincr,
    xtop 1-,
    xlower 1+,
    xthird 1+,
    _cmove_loop ??, b,
_cmove_done .label
    next,
: defbinaryop ( str length "name" "op" -- )
  defmachineword 
  2pop
  xtop xlower xtop ' execute 
  1push, ;
s" u*" defbinaryop _u* umul,
s" u/" defbinaryop _u/ udiv,
s" and" defbinaryop _and and,
s" or"  defbinaryop _or or,
s" xor" defbinaryop _xor xor,
s" sp@" defmachineword _sp@
    xsp xsp push,
    next,
s" sp!" defmachineword _sp!
    \ initialize the stack pointer from S0
    &S0 ??, xtaddr set,
    xtaddr xsp ld,
    next,
s" rp@" defmachineword _rp@
    \ push xrp onto xsp
    xrp xsp push,
    next,
s" rp!" defmachineword _rp!
    \ initialize the stack pointer from R0
    &R0 ??, xtaddr set,
    xtaddr xrp ld,
    next,
s" ;s" defmachineword _;s
	\ return execution to the calling definition. Unnest one level.
    xrp xip pop, \ pop the return stack into xip, pointing now to the next word to be executed in the calling definition
    next,
: ;;s ( -- )
  \ embed the semicolons routine
_;s word, ;
s" leave" defmachineword _leave
	\ make the loop limit equal to the loop count and force the loop to
	\ terminate at loop or +loop
	\ copy loop count to loop limit on return stack
	leave,
	next,
s" >r" defmachineword _>r \ move top item to return stack
    1pop 
    xtop xrp push,
    next,
: >r; ( -- ) _>r word, ;
s" r>" defmachineword _r> \ retrieve item from top of return stack
    xrp xtop pop,
	1push,
: r>; ( -- ) _r> word, ;
s" r" defmachineword _r \ copy top of return stack onto stack
	xrp xtop ld,
	1push,
: r; ( -- ) _r word, ;
: 0=, ( -- ) 
  1pop
  xtop xtop eqz,
  xtop xsp push, ;
s" 0=" defmachineword _0=
    1pop
    xtop xtop eqz,
	1push,
: 0=; ( -- ) _0= word, ;
s" 0<" defmachineword _0<
    1pop
    xtop xtop ltz,
	1push,
: 0<; ( -- ) _0< word, ;
s" +" defbinaryop _+ add,
: +; ( -- ) _+ word, ;
s" d+" defmachineword _dplus
    ( xl xh yl yh -- )
    \ iris takes in a front register which is the lower half
    \ because of this we have to pop registers differently
    \ not in the order found on the stack
    \ top - yl
    \ lower - yh
    \ third - xl
    \ fourth - xh
    \ actual input from the stack
    ( xl xh yl yh -- sl sh )
    xsp xtop popw, \ xtop then lower
    xsp xthird popw, \ third then fourth
    xtop xthird xtop addw, \ result will be in xtop,xlower
    xtop xsp pushw, \ push xtop then xlower
    next,
: d+; ( -- ) _dplus word, ;
s" minus" defmachineword _minus
    deflabel _minusDone
    1pop
    0xFFFF #, xtop xtop muli, \ multiply by negative one
	1push,
s" d-" defmachineword _dminus
    ( xl xh yl yh -- )
    \ iris takes in a front register which is the lower half
    \ because of this we have to pop registers differently
    \ not in the order found on the stack
    \ top - yl
    \ lower - yh
    \ third - xl
    \ fourth - xh
    \ actual input from the stack
    ( xl xh yl yh -- sl sh )
    xsp xtop popw, \ xtop then lower
    xsp xthird popw, \ third then fourth
    xtop xthird xtop subw, \ result will be in xtop,xlower
    xtop xsp pushw, \ push xtop then xlower
    next,
: d-; ( -- ) _dminus word, ;
s" over" defmachineword _over 
	over,
	next,
: over; ( -- ) _over word, ;
s" drop" defmachineword _drop 
	drop,
    next,
: drop; ( -- ) _drop word, ;
s" swap" defmachineword _swap
	swap,
    next,
: swap; ( -- ) _swap word, ;
s" dup" defmachineword _dup 
	xsp xtop ld,
	1push,
: dup; ( -- ) _dup word, ;
s" 2dup" defmachineword _2dup ( a b -- a b a b ) 
	xsp xtaddr move,
	xtaddr xtop ld,
	xtaddr 1+,
	xtaddr xlower ld,
	xlower xsp push,
	xtop xsp push,
	next,
: +!, ( -- )
    2pop \ top - addr 
         \ lower - n
    xtop at0 ld,
    xlower at0 xlower add, 
    xlower xtop st, ;
s" +!" defmachineword _+!
	+!,
    next,
: +!; ( -- ) _+! word, ;
s" toggle" defmachineword _toggle ( p addr -- )
	toggle,
	next,
: toggle; ( -- ) _toggle word, ;
s" @" defmachineword _@
    1pop
    xtop xtop ld,
	1push,
: @; ( -- ) _@ word, ;
: word-then@ ( word -- ) word, @; ;

s" c@" defmachineword _c@
	1pop 
	xtop xtop ld,
	0xFF #, xtop xtop andi,
	1push,
: c@; ( -- ) _c@ word, ;
s" !" defmachineword _! ( v a -- ) !,, next,
: !; ( -- ) _! word, ;
s" c!" defmachineword _c!  ( value addr -- ) c!, next,
: c!; ( -- ) _c! word, ;
s" :" word/imm defmachineword-base _colon
    _?exec word,
    _!csp word,
    _&current word-then@
    _&context word,
    !;
    _create word,
    _rightbracket word,
    _(;code) word,
_docolon .label
    xw 1+,
	\ runtime routine for all colon definitions
    xip xrp push,  \ push the address of the next word to the return stack and enter a lower nesting level
    xw xip -> \ move the parameter field address into IP, pointing to the first word in this definition
    next,
s" ;" defcolonword _semi
    _?csp word,
    _compile word,
    ;;s
    _smudge word,
    _leftbracket word,
    ;;s


s" noop" defmachineword _noop next,
s" constant" defcolonword _constant
    _create word,
    _smudge word,
    _, word,
    _(;code) word,
_doconstant .label
    xw 1+,
    xw xtop move,
    xtop xtop ld, \ get data
    1push,
s" variable" defcolonword _variable
    _constant word,
    _(;code) word,
    _dovariable .label
        xw 1+, 
        xw xsp push, 
        next,
s" user" defcolonword _user
    _constant word,
    _(;code) word,
_douser .label
    xw 1+,
    xw xtop move,
    xtop xtop ld,
    0xFF #, xtop xtop andi,
    &up ??, xlower set, \ user variable addr
    xlower xtop xtop add, \ address of variable
    1push,
    

\ TODO implement user
s" 0" defconstantword _0 0x0 #, .cell 
: 0; ( -- ) _0 word, ;
s" 1" defconstantword _1 0x1 #, .cell
: 1; ( -- ) _1 word, ;
s" 2" defconstantword _2 0x2 #, .cell
: 2; ( -- ) _2 word, ;
s" 3" defconstantword _3 0x3 #, .cell
: 3; ( -- ) _3 word, ;
s" bl" defconstantword _bl bl #, .cell
s" c/l" defconstantword _c/l 0x40 #, .cell
s" first" defmachineword _first
	&FIRST #, xsp pushi,
	next,
s" b/buf" defmachineword _b/buf
	b/buf #, xsp pushi,
	next,
s" b/scr" defmachineword _b/scr
	b/scr #, xsp pushi,
	next,
s" +origin" defmachineword _+origin
	_origin ??, xtaddr set,
	1pop
	xtop xtaddr xtop add,
	1push,
\ s" s0" defuserword _s0 0x6 #, .cell
\ s" r0" defuserword _r0 0x8 #, .cell
\ s" tib" defuserword _tib 0
\ TODO WIDTH
\ TODO WARNING
\ TODO more user variables
: 1+,, ( -- ) 
	1pop
	xtop 1+,
	xtop xsp push, ;
s" 1+" defmachineword _1+
	1pop
	xtop 1+,
	1push,
: 1+; ( -- ) _1+ word, ;
s" 2+" defmachineword _2+
	1pop
	2 #, xtop xtop addi,
	1push,

s" here" defmachineword _here
	&DP ??, xtaddr set,
	xtaddr xtop ld,
	1push,
: here; ( -- ) _here word, ;
s" allot" defmachineword _allot ( n -- )
	allot,
    next,

_, s" ," defmachineword-predef ( n -- )
    \ store n into the next available cell above dictionary and advance DP by 2 thus
    \ compiling into the dictionary
	,,
    next,
s" c," defmachineword _c,
	1pop
    0xFF #, xtop xtop andi, 
	&DP ??, xtaddr set,
	xtaddr xlower ld, 
    xtop xlower st, \ save it to the current dict pointer front
    xlower 1+, \ move ahead by one
	xlower xtaddr st,
    next,

s" -" defbinaryop _- sub, 
: -; ( -- ) _- word, ;
s" =" defbinaryop _= eq,
s" u<" defbinaryop _u< ult,
s" >" defbinaryop _> gt,
: rot, ( -- ) 
	3pop ( a b c -- b c a )
		 \ top - c
		 \ lower - b
		 \ third - a 
	xlower xsp push,
	xtop xsp push,
	xthird xsp push, ;
s" rot" defmachineword _rot ( a b c -- b c a )
	rot, 
    next,
: rot; ( -- ) _rot word, ;
s" space" defmachineword _space
    /dev/console0 #, xlower set,
    0x20 #, xtop set,
    xtop xlower st,
    next,
s" -dup" defmachineword _-dup \ duplicate if non zero
    deflabel _-dup_done
    1pop 
    xtop cv eqz, 
    _-dup_done ??, cv bc,
        xtop xsp push,
_-dup_done .label
	1push,
: push-zero, ( sp -- ) zero swap push, ;
: begin, ( -- ) 
	\ check and see if we are compiling
	&state ??, xtaddr set,
	xtaddr xtaddr ld, \ load the state
	zero xtaddr cv eq, \ check and see if it is not equal to zero
	_handle-error ??, cv bc,
	&dp ??, xtaddr set,
	xtaddr xtaddr ld, 
    xtaddr xsp push,
	0x1 #, xsp pushi, ;
: =, ( -- )
	2pop 
	xlower xtop cv eq,
	cv xsp push, ;
: >r, ( -- )
	1pop
	xtop xrp push, ;
: store, ( -- )
	2pop \ top - address
		 \ lower - thing to store
	xlower xtop st,
	;
: +, ( -- ) 2pop xlower xtop xtop add, xtop xsp push, ;
: -, ( -- ) 2pop xlower xtop xtop sub, xtop xsp push, ;
: 0,, ( -- ) 0 #lit, ;
: r>, ( -- )
	xrp xtop pop,
	xtop xsp push, ;
: if,, ( jump type -- )
	1pop
	xtop cv eqz,
	cv bc, ;
: negate, ( -- )
	1pop
	xtop xtop not,
	xtop xsp push, ;
: base@, ( -- )
	&base ??, xtaddr set,
	xtaddr xtop ld,
	xtop xsp push,
	;
: dpl@, ( -- )
	&dpl ??, xtaddr set,
	xtaddr xtop ld,
	xtop xsp push, 
	;
: u*, ( -- )
	2pop
	xlower xtop xtop umul,
	xtop xsp push, ;
: <, ( -- ) 
	2pop
	xtop xlower xtop lt,
	xtop xsp push, ;
: latest,, ( -- )
	&current ??, xtaddr set,
	xtaddr xtop ld,
	xtop xtop ld,
	xtop xsp push, ;
s" latest" defmachineword _latest \ leave the name field address of the last word defined in the current vocabulary
	&current ??, xtaddr set,
	xtaddr xtop ld,
	xtop xtop ld,
	1push, 

s" lfa" defmachineword _lfa \ convert the parameter field address to link field address
	1pop
	2 #, xtop xtop subi, 
	1push,
s" cfa" defmachineword _cfa \ convert the parameter field address to code field address
	1pop
	1 #, xtop xtop subi,
	1push,
s" nfa" defmachineword _nfa \ convert the parameter field address to name field address
    1pop
    7 #, xtop xtop subi,
    1push,
s" pfa" defmachineword _pfa \ convert the name field address to its corresponding pfa
    1pop
    8 #, xtop xtop addi,
    1push,
_!csp s" !csp" defmachineword-predef
    xsp xtop move,
    &csp ??, xlower set,
    xtop xlower st,
    next,
: ?err, ( "name0" "name1" -- )
    deflabel execute-latest  \ else
    deflabel execute-latest \ endif
    swap over over 
    swap,
    ??, if,,
    _handle-error ??, b,
    ??, b, \ dead code
    .label
    drop,
    .label ;

s" ?error" defmachineword _?error
    ?err, _?error_else _?error_endif
    next,
s" ?comp" defmachineword _?comp 
    &state ??lit,
    @,
    0=,
    0x11 #lit,
    ?err, _?comp_?err_else _?comp_?err_endif
    next,
_?exec s" ?exec" defmachineword-predef
    &state ??lit,
    @,
    0x12 #lit,
    ?err, _?exec_?err_else _?exec_?err_endif
    next,
s" ?pairs" defmachineword _?pairs
    -,
    0x13 #lit,
    ?err, _?pairs_?err_else _?pairs_?err_endif
    next,
_?csp s" ?csp" defmachineword-predef 
    xsp xsp push,
    &csp ??, lit,
    @, 
    -,
    0x14 #lit,
    ?err, _?csp_?err_else _?csp_?err_endif
    next,
s" ?loading" defmachineword _?loading
    &blk ??, lit,
    @, 
    0=,
    0x16 #lit,
    ?err, _?loading_?err_else _?loading_?err_endif
    next,
_compile s" compile" defcolonword-predef
    _?comp word,
    r>;
    dup; 
    _2+ word,
    >r;
    @;
    _, word,
    ;;s
_smudge s" smudge" defmachineword-predef
	\ mark the current dictionary entry as smudge
	&dp ??, xtaddr set,
	xtaddr xtop ld,
	xtop xlower ld,
	word/smudge #, at0 set,
	xlower at0 xlower or,
	xlower xtop st,
	next,

_(;code) s" (;code)" defcolonword-predef
    _r> word,
    _latest word,
    _pfa word,
    _cfa word,
    @;
    ;;s
s" ;code" defcolonword _;code
    _?csp word,
    _compile word,
    _(;code) word,
    _leftbracket word,
    _noop word,
    ;;s
s" <builds" defcolonword _<builds
    0;
    _constant word,
    ;;s
s" does>" defcolonword _does>
    deflabel _dodoes
    _r> word,
    _latest word,
    _pfa word,
    @;
    _(;code) word,
_dodoes .label 
    xip xrp push,
    xw 1+,      \ pfa
    xw xtop move, 
    xtop xip ld, \ new cfa
    xw 1+, 
    xw xsp push, \ pfa
    next,
s" count" defcolonword _count dup; 1+; swap; c@; ;;s
s" type" defcolonword _type
    \ TODO type body
    ;;s
s" trailing" defcolonword _trailing
    \ TODO trailing body
    ;;s
s" print-ok" defmachineword _printok prok, next,
s\" (.\")" 
defcolonword _pdotq
    _r word,
    _count word,
    dup;
    1+; 
    r>;
    +;
    >r;
    \ _types word,
    ;;s

s\" .\"" 
defcolonword _dotq
\ TODO dotq body
    ;;s

s" *" defbinaryop _* mul,
s" /" defbinaryop _/ div, 
s" mod" defbinaryop _mod rem,
s" min" defbinaryop _min min,
: min,, ( -- )
	2pop
	xtop xlower xtop min,
	xtop xsp push, ;

s" max" defbinaryop _max max,
s" <"  defbinaryop _< lt,
s" !=" defbinaryop _!= neq,
s" >=" defbinaryop _>= ge,
s" <=" defbinaryop _<= le,
s" nand" defbinaryop _nand nand,
s" nor" defbinaryop _nor nor,
s" lshift" defbinaryop _lshift lshift,
s" rshift" defbinaryop _rshift rshift,
: 1-,, ( -- )
	1pop
	xtop 1-,
	xtop xsp push, ;

s" 1-" defmachineword _1-
	1pop
	xtop 1-,
	1push,
s" abs" defmachineword _abs
    deflabel _absDone
    1pop
    xtop cv gez,
    _absDone ??, cv bc,
    0xFFFF #, xtop xtop muli, \ multiply by negative one
_absDone .label
	1push,




s" ." defmachineword _.
   1pop
   /dev/console2 #, xlower set,
   xtop xlower st,
   next,

s" ?" defmachineword _?
    1pop 
    xtop xtop ld,
    /dev/console0 #, xlower set,
    xtop xlower st,
    next,

_leftbracket s" [" word/imm defmachineword-base-predef
	&state ??, xtaddr set,
	zero xtaddr st,
    next,
_rightbracket s" ]" defmachineword-predef
	&state ??, xtaddr set,
    0xFFFF #, xtop set,
	xtop xtaddr st,
    next,

: ?comp, ( -- )
	&state ??, xtaddr set,
	xtaddr xtop ld,
	zero xtop xtop neq,
	xtop xsp push, ;
s" hex" defmachineword _hex \ set the base to 16
	&base ??, xtaddr set,
	0x10 #, at0 set,
	at0 xtaddr st,
	next,
s" decimal" defmachineword _decimal \ set the base to 10
	&base ??, xtaddr set,
	0xA #, at0 set,
	at0 xtaddr st,
	next,
s" octal" defmachineword _octal \ set the base to 8
	&base ??, xtaddr set,
	0x8 #, at0 set,
	at0 xtaddr st,
	next,
s" immediate" defmachineword _immediate
	\ mark the current dictionary entry as immediate
	&dp ??, xtaddr set,
	xtaddr xtop ld,
	xtop xlower ld,
	word/imm #, at0 set,
	xlower at0 xlower or,
	xlower xtop st,
	next,
	
s" 2drop" defmachineword _2drop ( a b -- ) 2pop next,
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
: load-variable, ( var type reg -- )
	>r
	xtaddr set,
	r> 
	xtaddr swap ld, ;
: zero-variable, ( address type -- ) 2>r 0 #, 2r> assign-variable, ;
: ?compiling, ( label -- ) 
	&state ??, xtaddr set,
	xtaddr xtop ld, \ load the state
	zero xtop cv neq, \ check and see if it is not equal to zero
	??, cv bc,
	;
: ?interpreting, ( label -- )
	&state ??, xtaddr set,
	xtaddr xtop ld, \ load the state
	zero xtop cv eq, \ check and see if it is equal to zero
	??, cv bc, ;
s" forth" defmachineword _forth
	\ set the context to the forth base vocabulary
	forth_vocabulary_start ??, &context ??, assign-variable,
	next,
s" block" defmachineword _block 
	deflabel _block_done
	( bid -- addr )
	1pop \ xtop - block number to select
	xcoreid xtop cv eq, \ if the ids are the same then do nothing
	_block_done ??, cv bc,
	\ if they are not then perform the sync automatically followed by
	\ loading the new id
	\ will need to expand on this later on by encoding the core contents
	/dev/core-dump #, io set,
	xcoreid io st,
	/dev/core-load #, io set,
	xtop io st,
	xtop xcoreid move, 
_block_done .label
	&FIRST #, xsp pushi,
	next,

s" query" defcolonword _query
    _tib word-then@
    0x50 #, push-literal
    _expect word,
    0;
    _inn word,
    !;
    ;;s
s" " defcolonword _null
deflabel _null1
deflabel _null2
deflabel _null3
    _&blk word-then@
    _null1 ??, zbranch, \ if
    1;
    &blk word,
    +!;
    0;
    _inn word,
    !;
    _&blk word-then@
    &bscr word,
    1;
    -;
    _and word,
    0=; 
    _null2 ??, zbranch, \ if
    _?exec word,
    r>;
    drop; \ endif
_null2 .label   \ else
    _null3 ??, branch,
    r>;
_null1 .label
    drop; \ endif
_null3 .label
    ;;s

s" fill" defmachineword _fill ( addr n b -- ) 
	\ fill u bytes in memory with b beginning at address
    3pop \ top - b
         \ lower - u
         \ third - addr
    deflabel _fill_loop
    deflabel _fill_done
_fill_loop .label
    xlower cv eqz,
    _fill_done ??, cv bc,
    xtop xthird sttincr,
    xlower 1-,
    _fill_loop ??, b,
_fill_done .label
    next,
: fill; ( -- ) _fill word, ;
s" erase" defcolonword _erase
    0;
    fill;
    ;;s
s" blanks" defcolonword _blanks
    _bls word,
    fill;
    ;;s
s" hold" defcolonword _hold
    -1 #, push-literal
    _HLD word,
    +!;
    _HLD word-then@
    c@;
    ;;s

s" pad" defcolonword _pad ( -- n )
    here;
    0x44 #, push-literal
    +;
    ;;s
s" word" defcolonword _word
deflabel word1
deflabel word2
    _blk word-then@
    _word1 ??, zbranch,
    _blk word-then@
    _block word,
    _word2 ??, branch,
_word1 .label
    _tib word-then@ \ endif
_word2 .label
    _inn word-then@ 
    +;
    _swap word,
    _enclose word,
    here;
    0x22 #, push-literal
    _blank word,
    _inn word,
    +!;
    over; -;
    >r; r; here;
    _C! word,
    _1+ word,
    r>;
    _cmove word,
    ;;s
s" (number)" defcolonword _pnum
deflabel-here _pnum1
deflabel _pnum2
deflabel _pnum3
    1+; \ begin
    dup;
    >r;
    c@;
    _base word-then@
    _digit word,
    _pnum2 ??, zbranch, \ while
    swap;
    _base word-then@
    _u* word,
    d+; 
    _dpl word-then@
    1+;
    _pnum3 ??, zbranch, \ if
    _1 word,
    _dpl word,
    +!; \ endif
_pnum3 .label
    r>;
    _pnum1 ??, branch, \ repeat
_pnum2 .label
    r>;
    ;;s
s" number" defcolonword _number
    0; 0; rot;
    dup; 1+;
    c@;
    0x2D #, push-literal
    _= word,
    dup; >r;






\ s" compile" defmachineword _compile
\ 	?comp, \ error if not compiling
\ 	r>,    \ top of return stack is pointing to the next word following compile
\ 	dup, 2 #lit, +, >r,
\ 	@, ,, 
\ 	next,
\ s" count" defmachineword _count dup, 1+,, swap, next,
s" definitions" defmachineword _definitions
  \ used in the form: cccc definitions 
  \ make cccc vocabulary the current vocabulary.
  \ new definitions will be added to the cccc vocabulary
	&context ??lit, @, 
	&current ??lit, !,,
	next,
s" (" defcolonword _paren
    0x29 #, push-literal
    _word word,
    ;;s
s" quit" defcolonword-predef _quit
deflabel _quit1
deflabel _quit2
    0;
    &blk word,
    !;
    _leftbracket word,
_quit1 .label
    _rp! word,
    _cr word,
    _query word,
    _inter word,
    &state word,
    @;
    0=;
    _quit2 ??, zbranch, \ if
    _printok word,
    \ endif 
_quit2 .label
    _quit1 ??, branch, \ again
s" abort" defcolonword-predef _abort
    _sp! word,
    _decimal word,
    \ qstac
    _cr word,
    \ TODO print information out here at some point
    \ _dotcpu word,
    \ _pdotq word,
    \ 0xD #, cell,
    \ s" fig-forth" string,
    \ version information embedded here too
    _forth word,
    _defin word,
    _quit word,
deflabel _wrm
deflabel _wrm1
deflabel _warm
\ warm start vector comes here
_wrm .label
    _wrm1 ??, xip set,
    next,
_wrm1 .label 
    _warm word,
s" warm" defcolonword-predef _warm
    _mtbuf word,
    _abort word,
\ cold start vector comes here
deflabel _cld
deflabel _cld1
_cld .label
    _cld1 ??, xip set,
    \ setup the data and return stacks
    data-stack-start #, xsp set,
    return-stack-start #, xrp set,
	zero xcoreid move, \ set to the zeroth core by default
	/dev/core-load #, xtop set,
	xcoreid xtop st, \ setup the zeroth core
	\ base-dict-done ??, &fence ??, assign-variable,   				\ setup the fence
	\ base-dict-done ??, &dp ??, assign-variable,      				\ setup the dictionary pointer
	\ 0x10 #, &base ??, assign-variable,              \ setup the numeric base
    \ setup the core load routines
    next,
_cld1 .label
    _cold word,
s" cold" defcolonword-predef _cold
    _mtbuf word,
    \ TODO set density
    \ 0;
    \ _density word,
    \ !;
    \ TODO set _use
    \ TODO set _prev
    \ _first word,
    \ _prev word,
    \ !;
    _drzer word,
    0 #, push-literal
    _eprint ??, push-literal
    !;
    \ orig + 12H 
    _origin ??, push-literal
    0x12 #, push-literal
    +;
    _up ??, push-literal
    @;
    _forth ??, push-literal
    0x6 #, push-literal
    !;
    _abort word, \ last
forth_vocabulary_start .label 
s" terminate" defmachineword _terminate
	/dev/terminate-vm #, xtaddr set,
	zero xtaddr st,
	next,
base-dict-done .label \ always is the front address
system-start .org \ system variables
&state .label 0 #, .cell
&base .label 0x10 #, .cell
&tib  .label 0 #, .cell
&s0   .label 0 #, .cell
&r0   .label 0 #, .cell
&warning   .label 0 #, .cell
&fence .label 0 #, .cell
&dp .label 0 #, .cell
&voc-link .label 0 #, .cell
&blk .label 0 #, .cell
&in .label 0 #, .cell
&out .label 0 #, .cell
&current .label 0 #, .cell
&dpl  .label 0 #, .cell
&fld  .label 0 #, .cell
&csp  .label 0 #, .cell
&r#   .label 0 #, .cell
&hld  .label 0 #, .cell
&separator .label 0 #, .cell
&terminator .label 0 #, .cell
&context .label 0 #, .cell
&width .label 0 #, .cell
&porigin .label 0 #, .cell
ram-start .org
_origin .label
\ _cold .label
\ _abort .label
\ 	forth_vocabulary_start ??, &context ??, assign-variable,      \ setup the context variable
\ 	\ setup the data stack pointer
\ 	data-stack-start #, &S0 ??, assign-variable,
\ 	xtop xsp move,
\ 	\ setup the return stack pointer
\ 	return-stack-start #, &R0 ??, assign-variable,
\ 	xtop xrp move,
\ 	0xFFFF #, &warning ??, assign-variable, \ always skip error messages for now
\ _quit .label
\ 	input-buffer-start #, &tib ??, assign-variable, \ setup the terminal input buffer
\ 	&state ??, zero-variable, 
\ 	deflabel-here _quit_loop_start
\ 	return-stack-start #, xrp set, \ clear return stack
\ 	input-buffer-start #, at0 set, \ set where to write to
\ 	input-buffer-end input-buffer-start - #, at1 set, \ set the maximum length
\ 	at1 at0 rltm, \ input a line of text
\ 	\ perform interpretation
\ 	\ at the end check and see if we are looking at 
\ 	zero xerror cv neq, 
\ 	_handle-error ??, cv bc,
\ 	_quit_loop_start ?compiling,
\ 	prok, \ type OK on terminal
\ 	_quit_loop_start ??, b,
_handle-error .label
	deflabel _handle-error0
	&warning ??, xtaddr set,
	0xFFFF #, at1 set,
	xtaddr at0 ld,
	at1 at0 cv neq, \ equal negative one?
	_handle-error0 ??, cv bc,
	_abort ??, b,
_handle-error0 .label
	\ print text string under interpretation
	\ perform checks to see what kind of warning message we should print
	\ TODO add support for printing more information
	data-stack-start #, xsp set,
	\ push IN and BLK on Data stack
	&in ??, xtaddr set,
	xtaddr xsp push,
	&blk ??, xtaddr set,
	xtaddr xsp push,
	_quit ??, b,
asm}

bye
