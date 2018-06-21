include iris.fs
\ contains all of the registers and pieces used for the monitor itself
\ the monitor is now also the forth system itself
s" figforth.o" {asm
\ the core memory is a disk buffer of a kind so it will become the disk buffer 
\ of legend that is being discussed in the forth book.
: word, ( v -- ) ??, .cell ;
: constant, ( id -- ) #, .cell ;
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

\ unlike the 8086 forth impl, iris does things a tad differently
\ there is only one drive which holds up to 64 megawords of data
\ Hence drive 0 will always be storage
\ We divide these 64 megawords into 1 kiloword sections called tracks
\ There are 65536 total tracks available from the 'drive'
\ The sector is where this design gets even stranger, by default, 8086 forth 
\ is defined to access 8 inch magnetic disks with 128 bytes per sector and 
\ 26 or 52 sectors per track depending on the density. In our case, a track
\ is divided up into 8 sectors of 128 words each.
0x400 constant words/block
0x80 constant words/sector
0x400 constant words/track
0xFFFF 1+ constant tracks/disk
words/track words/sector / constant sectors/track
sectors/track tracks/disk * constant sectors/disk
sectors/track tracks/disk 2* * constant sectors/double-disk \ double density, double the number of tracks per disk
0x80 constant words/disk-buffer 
1 constant max-drive-count \ this could change in the future
words/block words/sector constant sectors/block
1 constant num-buffers
words/sector constant keyboard-buffer
keyboard-buffer 4 + constant co

\ bootstrap-end constant dictionary-start

\ register reservations
: too-many-vars-defined ( addr -- ) 0x40 >= ABORT" To many registers used!" ;
deflabel _origin
deflabel _error
: error; ( -- ) _error word, ;
deflabel forth_vocabulary_start
deflabel base-dict-done
deflabel _cold
: cold; ( -- ) _cold word, ;
deflabel _abort
: abort; ( -- ) _abort word, ;
deflabel _quit
: quit; ( -- ) _quit word, ;
deflabel _interpret
: interpret; ( -- ) _interpret word, ;
deflabel _message \ message routine
: message; ( -- ) _message word, ;
deflabel _warn 
: warn; ( -- ) _warn word, ;
deflabel &up
deflabel &porigin
deflabel &S0 \ initial value of the data stack pointer
: s0; ( -- ) &s0 word, ;
deflabel &R0 \ initial value of the return stack pointer
: r0; ( -- ) &r0 word, ;
deflabel &TIB \ address of the terminal input buffer
: tib; ( -- ) &tib word, ;
deflabel &WARNING \ error message control number. If 1, disk is present, 
                  \ and screen 4 of drive 0 is the base location of error messages
                  \ if 0, no disk is present and error messages will be presented
                  \ by number. If -1, execute (ABORT) on error
: warn; ( -- ) &WARNING word, ;
deflabel &FENCE \ address below which FORGETting is trapped.
                \ To forget below this point, the user must alter the contents
                \ of FENCE
: fence; ( -- ) &fence word, ;
deflabel &DP \ The dictionary pointer which contains the next free memory
             \ above the dictionary. The value may be read by HERE and altered
             \ by ALLOT.
: dp; ( -- ) &DP word, ;
deflabel &VOC-LINK \ address of a field in the definition of the most recently created
                   \ created vocabulary. All vocabulary names are linked by
                   \ these fields to allow control for FORGETting through multiple
                   \ vocabularies
: voc-link; ( -- ) &voc-link word, ;
deflabel &BLK \ current block number under interpretation. If 0, input is
              \ being taken from the terminal input buffer
: blk; ( -- ) &blk word, ;
deflabel &IN  \ Byte offset within the current input text buffer (terminal or
              \ disk) from which the next text will be accepted. WORD uses and
              \ move the value of IN
: inn; ( -- ) &IN word, ;
deflabel &OUT \ Offset in the text output buffer. Its value is incremented by EMIT
              \ The user may yalter and examine OUT to control output display formatting.
: out; ( -- ) &OUT word, ;
deflabel &SCR      \ Screen number most recently referenced by LIST
: scr; ( -- ) &scr word, ;
deflabel &OFFSET   \ Block offset disk drives. Contents of OFFSET is added to the stack number by BLOCK
: offset; ( -- ) &offset word, ;
deflabel &CONTEXT  \ pointer to the vocabulary within which dictionary search
                   \ will first begin
: context; ( -- ) &context word, ;
deflabel &CURRENT  \ Pointer to the vocabulary in which new definitions are to be added
: current; ( -- ) &current word, ;
deflabel &STATE    \ If 0, the system is in interpretive or executing state. If non-zero, the system is in compiling state. The value itself is implementation
                   \ dependent. So in this case it would be 0 is interpretive and 0xFFFF is compiling
: state; ( -- ) &state word, ;
deflabel &DPL      \ number of digits to the right of the decimal point on double integer input. It may also be used to hold output column
                   \ location of a decimal point in user generated formatting. The default value on single number input is -1
: dpl; ( -- ) &dpl word, ;
deflabel &FLD      \ field width for formatted number output
: fld; ( -- ) &fld word, ;
deflabel &CSP      \ temporarily stored data stack pointer for compilation error checking
: csp; ( -- ) &csp word, ;
deflabel &R#       \ location of editor cursor in a text screen
: r#; ( -- ) &r# word, ;
deflabel &HLD      \ address of the latest character of text during numeric output conversion
: hld; ( -- ) &hld word, ;
deflabel &SEPARATOR \ word separator contents
: separator; ( -- ) &separator word, ;
deflabel &TERMINATOR \ terminator index
: terminator; ( -- ) &terminator word, ;
deflabel &BASE       \ numeric base
: base; ( -- ) &base word, ;
deflabel &width 	\ width of some kind
: width; ( -- ) &width word, ;
deflabel _eprint
deflabel _up \ user pointer
deflabel _rpp \ return stack pointer
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
: 2push, ( -- ) _2push ??, b, ;
: x.scr ( -- ) hex .s decimal cr ;
: machine-code-execute ( -- ) loc@ 1+ constant, ;
: machineword ( n -- ) .label machine-code-execute ;
: machine-code-jump ( imm id -- ) 
  at0 set,
  at0 at0 ld,
  at0 br, ;
: embed-name ( str length -- ) 
  dup constant, \ embed length into its own storage
  swap @ swap \ make sure we load the front string address
  case 
  	1 of dup dup dup 
         0xFF and constant,
         .skip .skip .skip endof
    2 of dup dup dup 
         0xFF and constant,
         0xFF00 and 8 rshift constant,
         .skip
         .skip endof
	3 of dup dup dup
         0xFF and constant,
         0xFF00 and 8 rshift constant,
         0xFF0000 and 16 rshift constant,
         .skip endof
	swap \ the length must be consumed by endcase :(
	dup dup dup
    0xFF and constant,
    0xFF00 and 8 rshift constant, 
    0xFF0000 and 16 rshift constant,
    0xFF000000 and 24 rshift constant,
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
  constant, \ stash the control bits here
  embed-name \ stash three more bytes
  last-word @ constant, \ stash the previous word here
  r> last-word ! \ stash the top of this dictionary entry to last word
  ;
deflabel _(;code)
: (;code); ( -- ) _(;code) word, ;
: defword-base ( str length control-bits "name" -- ) defword-header deflabel-here ( then define the label to point at here ) ;
: defword-base-predef ( label str length control-bits -- ) defword-header .label ;
: machineword-base ( str length control-bits "name" -- ) defword-base machine-code-execute ;
: machineword-base-predef ( label str length control-bits -- ) defword-base-predef machine-code-execute ;
: machineword ( str length "name" -- ) word/none machineword-base ;
: machineword-predef ( label str length -- ) word/none machineword-base-predef ;
: embed-string-length ( len -- ) constant, ;
deflabel _docolon
deflabel _?exec
: ?exec; ( -- ) _?exec word, ;
deflabel _?csp
: ?csp; ( -- ) _?csp word, ;
deflabel _!csp
: !csp; ( -- ) _!csp word, ;
deflabel _&current
deflabel _&context
deflabel _create
: create; ( -- ) _create word, ;
deflabel _leftbracket
: leftbracket; ( -- ) _leftbracket word, ;
deflabel _rightbracket
: rightbracket; ( -- ) _rightbracket word, ;
deflabel _compile
: compile; ( -- ) _compile word, ;
deflabel _smudge
: smudge; ( -- ) _smudge word, ;
deflabel _doconstant
deflabel _douser
deflabel _dovariable
deflabel _,
: smudge; ( -- ) _smudge word, ;
: dovariable; ( -- ) _dovariable word, ;
: ,; ( -- ) _, word, ;
: embed-docolon ( -- ) _docolon ??, .cell ;
: colonword-base ( str length control-bits "name" -- ) defword-base embed-docolon ;
: colonword-base-predef ( label str length control-bits -- ) defword-base-predef embed-docolon ;
: colonword ( str length "name"  -- ) word/none colonword-base ;
: colonword-predef ( label str length -- ) word/none colonword-base-predef ;
: embed-doconstant ( -- ) _doconstant ??, .cell ;
: defconstantword-base ( str length control-bits "name" -- ) defword-base embed-doconstant ;
: defconstantword ( n -- ) word/none defconstantword-base ;
: embed-douser ( -- ) _douser ??, .cell ;
: userword-base ( str length control-bits "name" -- ) defword-base embed-douser ;
: userword-base-predef ( label str length control-bits -- ) defword-base-predef embed-douser ;
: userword ( n -- ) word/none userword-base ;
: userword-predef ( label n len -- ) word/none userword-base-predef ;
: embed-dovariable ( -- ) _dovariable ??, .cell ;
: defvariableword-base ( str length control-bits "name" -- ) defword-base embed-dovariable ;
: defvariableword-base-predef ( label str length control-bits -- ) defword-base-predef embed-dovariable ;
: defvariableword ( n -- ) word/none defvariableword-base ;
: defvariableword-predef ( label n len -- ) word/none defvariableword-base-predef ;

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
s" lit" machineword _lit
    \ push the next word to the data stack as a literal. Increment IP and skip this literal.
    \ NEXT Return
    \ LIT is used to compile numbers into the dictionary. At run-time, LIT pushes the 
    \ inline literal to the data stack to be used in computations
	xip xtop ld,
    xip 1+,
	1push,
: two-cell-op ( n id op -- ) word, .cell ;
: plit; ( n id -- )
  \ compile the literal into the dictionary by putting the _LIT command followed by
  \ the number itself
  _lit two-cell-op ;
: ??plit; ( n -- ) ??, plit; ;
: #plit; ( n -- ) #, plit; ;
: lit; ( -- ) _lit word, ;
s" execute" machineword _execute
: execute; ( -- ) _execute word, ;
	\ execute the definition whose code field address cfa is on the data stack
    xsp xw pop, \ pop the code field address into xw, the word pointer
    xw at0 ldtincr, \ Jump indirectly to the code routine. Increment xw to point to the parameter field
    at0 br, 
s" branch" machineword _branch
	xip xtop ld,
	xtop xip xip add,
	next,
: branch; ( location id -- ) _branch two-cell-op ;
: ??branch; ( loc -- ) ??, branch; ;
s" 0branch" machineword _0branch
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
: zbranch; ( location id -- ) _0branch two-cell-op ;
: ??zbranch; ( location -- ) ??, zbranch; ;
s" (loop)" machineword _(loop)
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
: (loop); ( location id -- ) _(loop) two-cell-op ;
: ??(loop); ( location -- ) ??, (loop); ;
s" (+loop)" machineword _(+loop)
: (+loop); ( loc id -- ) _(+loop) two-cell-op ;
: ??(+loop); ( loc -- ) ??, (+loop); ;
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
s" (do)" machineword _(do)
: (do); ( -- ) _(do) word, ;
	2pop 
	xlower xrp push,
	xtop xrp push,
	next,

s" i" machineword _i
: i; ( -- ) _i word, ;
	xrp xtop ld, \ load the top loop element
	1push,
s" digit" machineword _digit
: digit; ( -- ) _digit word, ;
	xsp digit, 
	next,
s" (find)" machineword _(find)
    xsp pfind,
	next,
: (find); ( -- ) _(find) word, ;
s" enclose" machineword _enclose
    xsp enclose,
	next,
: enclose; ( -- ) _enclose word, ;
s" emit" machineword _emit 
	/dev/console0 #, xlower set,
	1pop
	xtop xlower st,
	next,
: emit; ( -- ) _emit word, ;
s" key" machineword _key 
	/dev/console0 #, xlower set,
	xlower xtop ld,
	1push,
\ TODO ?TERMINAL goes here
s" cr" machineword _cr
: cr; ( -- ) _cr word, ;
    /dev/console0 #, xlower set,
    0xA #, xtop set,
    xtop xlower st,
    next,
s" cmove" machineword _cmove ( from to u -- ) \ move u bytes in memory 
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
: cmove; ( -- ) _cmove word, ;
: defbinaryop ( str length "name" "op" -- )
  machineword 
  2pop
  xtop xlower xtop ' execute 
  1push, ;
s" u*" defbinaryop _u* umul,
: u*; ( -- ) _u* word, ;
s" u/" defbinaryop _u/ udiv,
: u/; ( -- ) _u/ word, ;
s" and" defbinaryop _and and,
: and; ( -- ) _and word, ;
s" or"  defbinaryop _or or,
: or; ( -- ) _or word, ;
s" xor" defbinaryop _xor xor,
: xor; ( -- ) _xor word, ;

s" sp@" machineword _sp@
: sp@; ( -- ) _sp@ word, ;
    xsp xsp push,
    next,
s" sp!" machineword _sp!
: sp!; ( -- ) _sp! word, ;
    \ initialize the stack pointer from S0
    &S0 ??, xtaddr set,
    xtaddr xsp ld,
    next,
s" rp@" machineword _rp@
    \ push xrp onto xsp
    xrp xsp push,
    next,
s" rp!" machineword _rp!
: rp!; ( -- ) _rp! word, ;
    \ initialize the stack pointer from R0
    &R0 ??, xtaddr set,
    xtaddr xrp ld,
    next,
s" ;s" machineword _;s
	\ return execution to the calling definition. Unnest one level.
    xrp xip pop, \ pop the return stack into xip, pointing now to the next word to be executed in the calling definition
    next,
: ;;s ( -- )
  \ embed the semicolons routine
_;s word, ;
s" leave" machineword _leave
: leave; ( -- ) _leave word, ;
	\ make the loop limit equal to the loop count and force the loop to
	\ terminate at loop or +loop
	\ copy loop count to loop limit on return stack
	xrp xtop pop,
	xrp zero pop,
	xtop xrp push,
	xtop xrp push, 
	next,
s" >r" machineword _>r \ move top item to return stack
    1pop 
    xtop xrp push,
    next,
: >r; ( -- ) _>r word, ;
s" r>" machineword _r> \ retrieve item from top of return stack
    xrp xtop pop,
	1push,
: r>; ( -- ) _r> word, ;
s" r" machineword _r \ copy top of return stack onto stack
	xrp xtop ld,
	1push,
: r; ( -- ) _r word, ;
: 0=, ( -- ) 
  1pop
  xtop xtop eqz,
  xtop xsp push, ;
s" 0=" machineword _0=
    1pop
    xtop xtop eqz,
	1push,
: 0=; ( -- ) _0= word, ;
s" 0<" machineword _0<
    1pop
    xtop xtop ltz,
	1push,
: 0<; ( -- ) _0< word, ;
s" +" defbinaryop _+ add,
: +; ( -- ) _+ word, ;
s" d+" machineword _dplus
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
s" minus" machineword _minus
    deflabel _minusDone
    1pop
    0xFFFF #, xtop xtop muli, \ multiply by negative one
	1push,
s" d-" machineword _dminus
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
s" over" machineword _over 
	2pop 
	xlower xsp push,
	xtop xsp push,
	xlower xsp push, 
	next,
: over; ( -- ) _over word, ;
s" drop" machineword _drop 
    xsp zero pop,
    next,
: drop; ( -- ) _drop word, ;
s" swap" machineword _swap
	2pop \ top -- b
		 \ lower -- a
	xtop xsp push,
	xlower xsp push,
    next,
: swap; ( -- ) _swap word, ;
s" dup" machineword _dup 
	xsp xtop ld,
	1push,
: dup; ( -- ) _dup word, ;
s" 2dup" machineword _2dup ( a b -- a b a b ) 
	xsp xtaddr move,
	xtaddr xtop ld,
	xtaddr 1+,
	xtaddr xlower ld,
	xlower xsp push,
	xtop xsp push,
	next,
: 2dup; ( -- ) _2dup word, ;
: +!, ( -- )
    2pop \ top - addr 
         \ lower - n
    xtop at0 ld,
    xlower at0 xlower add, 
    xlower xtop st, ;
s" +!" machineword _+!
	+!,
    next,
: +!; ( -- ) _+! word, ;
s" toggle" machineword _toggle ( p addr -- )
	2pop  \ top - addr
		  \ lower - pattern
	xtop xthird ld,
	xthird xlower xthird xor,
	xtop xthird st, 
	next,
: toggle; ( -- ) _toggle word, ;
s" @" machineword _@
    1pop
    xtop xtop ld,
	1push,
: @; ( -- ) _@ word, ;

s" c@" machineword _c@
: c@; ( -- ) _c@ word, ;
	1pop 
	xtop xtop ld,
	0xFF #, xtop xtop andi,
	1push,
s" !" machineword _! ( v a -- ) 
   2pop \ top - addr
   		\ lower - value
   xlower xtop st, \ perform the store
next,
: !; ( -- ) _! word, ;
s" c!" machineword _c!  ( value addr -- ) 
	2pop \ top - addr
		 \ lower - value
    0xFF #, xlower xlower andi,
    xlower xtop st, \ save it to memory with the upper 8 bits masked
    next,
: c!; ( -- ) _c! word, ;
s" :" word/imm machineword-base _colon
    ?exec;
    !csp;
    current; @;
    context; !;
    create;
    rightbracket;
    (;code);
_docolon .label
    xw 1+,
	\ runtime routine for all colon definitions
    xip xrp push,  \ push the address of the next word to the return stack and enter a lower nesting level
    xw xip -> \ move the parameter field address into IP, pointing to the first word in this definition
    next,
s" ;" colonword _semi
    ?csp;
    compile;
    ;;s
    smudge;
    leftbracket;
    ;;s


s" noop" machineword _noop next,
: noop; ( -- ) _noop word, ;
s" constant" colonword _constant
: constant; ( -- ) _constant word, ;
    create;
    smudge;
    ,;
    (;code);
_doconstant .label
    xw 1+,
    xw xtop move,
    xtop xtop ld, \ get data
    1push,
s" variable" colonword _variable
: variable; ( -- ) _variable word, ;
    constant;
    (;code);
    _dovariable .label
        xw 1+, 
        xw xsp push, 
        next,
s" user" colonword _user
: user; ( -- ) _user word, ;
    constant;
    (;code);
_douser .label
    xw 1+,
    xw xtop move,
    xtop xtop ld,
    0xFF #, xtop xtop andi,
    &up ??, xlower set, \ user variable addr
    xlower xtop xtop add, \ address of variable
    1push,
s" 0" defconstantword _0 0x0 constant, 
: 0; ( -- ) _0 word, ;
s" 1" defconstantword _1 0x1 constant,
: 1; ( -- ) _1 word, ;
s" 2" defconstantword _2 0x2 constant,
: 2; ( -- ) _2 word, ;
s" 3" defconstantword _3 0x3 constant,
: 3; ( -- ) _3 word, ;
s" bl" defconstantword _bl bl constant,
: bl; ( -- ) _bl word, ;
s" c/l" defconstantword _c/l 0x40 constant,
: c/l; ( -- ) _c/l word, ;
s" first" defconstantword _first &FIRST constant,
: first; ( -- ) _first word, ;
s" limit" defconstantword _limit &LIMIT constant,
: limit; ( -- ) _limit word, ;
s" b/buf" defconstantword _b/buf b/buf constant,
: b/buf; ( -- ) _b/buf word, ;
s" b/scr" defconstantword _b/scr b/scr constant,
: b/scr; ( -- ) _b/scr word, ;
s" +origin" colonword _+origin
    lit;
    _origin ??plit;
    +;
    ;;s
&S0 s" s0" userword-predef 0x6 constant,
&R0 s" r0" userword-predef 0x7 constant,
&tib s" tib" userword-predef 0x8 constant,
&width s" width" userword-predef 0x9 constant,
&warning s" warning" userword-predef 0xa constant,
&fence s" fence" userword-predef 0xb constant,
&dp s" dp" userword-predef 0xc constant,
&voc-link s" voc-link" userword-predef 0xd constant,
&blk s" blk" userword-predef 0xe constant,
&in s" in" userword-predef 0xf constant,
&out s" out" userword-predef 0x10 constant,
&scr s" scr" userword-predef 0x11 constant,
&offset s" offset" userword-predef 0x12 constant,
&context s" context" userword-predef 0x13 constant,
&current s" current" userword-predef 0x14 constant,
&state s" state" userword-predef 0x15 constant,
&base s" base" userword-predef 0x16 constant,
&dpl s" dpl" userword-predef 0x17 constant,
&fld s" fld" userword-predef 0x18 constant,
&csp s" csp" userword-predef 0x19 constant,
&r# s" r#" userword-predef 0x1a constant,
&hld s" hld" userword-predef 0x1b constant,
\ TODO more user variables
s" 1+" machineword _1+
: 1+; ( -- ) _1+ word, ;
	1pop
	xtop 1+,
	1push,
s" 2+" machineword _2+
: 2+; ( -- ) _2+ word, ;
	1pop
	2 #, xtop xtop addi,
	1push,
s" here" machineword _here
	&DP ??, xtaddr set,
	xtaddr xtop ld,
	1push,
: here; ( -- ) _here word, ;
s" allot" machineword _allot ( n -- )
	1pop
	&DP ??, xtaddr set,
	xtaddr xlower ld, 
    xlower xtop xlower add,
	xlower xtaddr st, 
    next,
: allot; ( -- ) _allot word, ;
_, s" ," machineword-predef ( n -- )
    \ store n into the next available cell above dictionary and advance DP by 2 thus
    \ compiling into the dictionary
	1pop 
	&DP ??, xtaddr set,
	xtaddr xlower ld, 
    xtop xlower st, \ save it to the current dict pointer front
    xlower 1+, \ move ahead by one
	xlower xtaddr st,
    next,
s" c," machineword _c,
	1pop
    0xFF #, xtop xtop andi, 
	&DP ??, xtaddr set,
	xtaddr xlower ld, 
    xtop xlower st, \ save it to the current dict pointer front
    xlower 1+, \ move ahead by one
	xlower xtaddr st,
    next,
: c,; ( -- ) _c, word, ;
s" -" defbinaryop _- sub, 
: -; ( -- ) _- word, ;
s" =" defbinaryop _= eq,
: =; ( -- ) _= word, ;
s" u<" defbinaryop _u< ult,
: u<; ( -- ) _u< word, ;
s" >" defbinaryop _> gt,
: rot, ( -- ) 
	3pop ( a b c -- b c a )
		 \ top - c
		 \ lower - b
		 \ third - a 
	xlower xsp push,
	xtop xsp push,
	xthird xsp push, ;
s" rot" machineword _rot ( a b c -- b c a )
	rot, 
    next,
: rot; ( -- ) _rot word, ;
s" space" machineword _space
: space; ( -- ) _space word, ;
    /dev/console0 #, xlower set,
    0x20 #, xtop set,
    xtop xlower st,
    next,
s" -dup" machineword _-dup \ duplicate if non zero
: -dup; ( -- ) _-dup word, ;
    deflabel _-dup_done
    1pop 
    xtop cv eqz, 
    _-dup_done ??, cv bc,
        xtop xsp push,
_-dup_done .label
	1push,
s" latest" machineword _latest \ leave the name field address of the last word defined in the current vocabulary
	&current ??, xtaddr set,
	xtaddr xtop ld,
	xtop xtop ld,
	1push, 
: latest; ( -- ) _latest word, ;

s" lfa" machineword _lfa \ convert the parameter field address to link field address
	1pop
	2 #, xtop xtop subi, 
	1push,
: lfa; ( -- ) _lfa word, ;
s" cfa" machineword _cfa \ convert the parameter field address to code field address
	1pop
	1 #, xtop xtop subi,
	1push,
: cfa; ( -- ) _cfa word, ;
s" nfa" machineword _nfa \ convert the parameter field address to name field address
    1pop
    7 #, xtop xtop subi,
    1push,
: nfa; ( -- ) _nfa word, ;
s" pfa" machineword _pfa \ convert the name field address to its corresponding pfa
    1pop
    8 #, xtop xtop addi,
    1push,
: pfa; ( -- ) _pfa word, ;
_!csp s" !csp" machineword-predef
    xsp xtop move,
    &csp ??, xlower set,
    xtop xlower st,
    next,
: !csp; ( -- ) _!csp word, ;

s" ?error" colonword _?error
: ?error; ( -- ) _?error word, ;
deflabel _?err1
deflabel _?err2
    swap;
    _?err1 ??zbranch; \ if 
    error;
    _?err2 ??branch; \ else
_?err1 .label
    drop;
_?err2 .label
    ;;s



s" ?comp" colonword _?comp 
: ?comp; ( -- ) _?comp word, ;
    state; @;
    0=;
    0x11 #plit;
    ?error;
    ;;s
_?exec s" ?exec" colonword-predef
    state; @;
    0x12 #plit;
    ?error;
    ;;s
s" ?pairs" colonword _?pairs
    -;
    0x13 #plit;
    ?error;
    ;;s
_?csp s" ?csp" colonword-predef 
    sp@; @;
    -;
    0x14 #plit;
    ?error;
    ;;s
s" ?loading" colonword _?loading
: ?loading; ( -- ) _?loading word, ;
    blk; @;
    0=;
    0x16 #plit;
    ?error;
    ;;s
_compile s" compile" colonword-predef
    ?comp;
    r>;
    dup; 
    2+;
    >r; @;
    ,;
    ;;s
_smudge s" smudge" machineword-predef
	\ mark the current dictionary entry as smudge
	&dp ??, xtaddr set,
	xtaddr xtop ld,
	xtop xlower ld,
	word/smudge #, at0 set,
	xlower at0 xlower or,
	xlower xtop st,
	next,

_(;code) s" (;code)" colonword-predef
    r>;
    latest;
    pfa;
    cfa; @;
    ;;s
s" ;code" colonword _;code
    ?csp;
    compile;
    (;code); 
    leftbracket;
    noop;
    ;;s
s" <builds" colonword _<builds 0; constant; ;;s
: <builds; ( -- ) _<builds word, ;
s" does>" colonword _does>
: does>; ( -- ) _does> word, ;
    deflabel _dodoes
    r>;
    latest;
    pfa; @;
    (;code);
_dodoes .label 
    xip xrp push,
    xw 1+,      \ pfa
    xw xtop move, 
    xtop xip ld, \ new cfa
    xw 1+, 
    xw xsp push, \ pfa
    next,
s" count" colonword _count dup; 1+; swap; c@; ;;s
: count; ( -- ) _count word, ;
s" type" colonword _type
: type; ( -- ) _type word, ;
deflabel _type1
deflabel _type2
deflabel _type3
    2dup;
    _type1 ??zbranch;
    over;
    +;
    swap;
    (do); \ do
_type2 .label
    i;
    c@;
    emit;
    _type2 ??(loop); \ loop
    _type3 ??branch; \ else
    drop; \ endif
    ;;s
s" -trailing" colonword _dtrailing
    dup;
    0;
    (do); \ do
deflabel-here _dtrailing1
deflabel _dtrailing2
deflabel _dtrailing3
    over; over;
    +;
    1;
    -;
    c@;
    bl;
    -;
    _dtrailing2 ??zbranch; \ if
    leave;
    _dtrailing3 ??branch; \ else
_dtrailing2 .label 
    1;
    -; \ endif
_dtrailing3 .label 
    _dtrailing1 ??(loop);
    ;;s
: -trailing; ( -- ) _dtrailing word, ;
s" print-ok" machineword _printok prok, next,
: print-ok; ( -- ) _printok word, ;
s\" (.\")" 
colonword _pdotq
    r; 
    count;
    dup;
    1+; 
    r>;
    +;
    >r;
    type;
    ;;s
: pdotq; ( -- ) _pdotq word, ;
s\" .\"" 
colonword _dotq
\ TODO dotq body
    ;;s
: dotq; ( -- ) _dotq word, ;
s" *" defbinaryop _* mul,
: *; ( -- ) _* word, ;
s" /" defbinaryop _/ div, 
: /; ( -- ) _/ word, ;
s" mod" defbinaryop _mod rem,
: mod; ( -- ) _mod word, ;
s" min" defbinaryop _min min,
: min; ( -- ) _min word, ;
s" max" defbinaryop _max max,
: max; ( -- ) _max word, ;
s" <"  defbinaryop _< lt,
: <; ( -- ) _< word, ;
s" !=" defbinaryop _!= neq,
: !=; ( -- ) _!= word, ;
s" >=" defbinaryop _>= ge,
: >=; ( -- ) _>= word, ;
s" <=" defbinaryop _<= le,
: <=; ( -- ) _<= word, ;
s" nand" defbinaryop _nand nand,
: nand; ( -- ) _nand word, ;
s" nor" defbinaryop _nor nor,
: nor; ( -- ) _nor word, ;
s" lshift" defbinaryop _lshift lshift,
: lshift; ( -- ) _lshift word, ;
s" rshift" defbinaryop _rshift rshift,
: rshift; ( -- ) _rshift word, ;

s" 1-" machineword _1-
: 1-; ( -- ) _1- word, ;
	1pop
	xtop 1-,
	1push,
s" abs" machineword _abs
: abs; ( -- ) _abs word, ;
    deflabel _absDone
    1pop
    xtop cv gez,
    _absDone ??, cv bc,
    0xFFFF #, xtop xtop muli, \ multiply by negative one
_absDone .label
	1push,

s" ." machineword _.
: .; ( -- ) _. word, ;
   1pop
   /dev/console2 #, xlower set,
   xtop xlower st,
   next,

s" ?" machineword _?
: ?; ( -- ) _? word, ;
    1pop 
    xtop xtop ld,
    /dev/console0 #, xlower set,
    xtop xlower st,
    next,

_leftbracket s" [" word/imm machineword-base-predef
	&state ??, xtaddr set,
	zero xtaddr st,
    next,
_rightbracket s" ]" machineword-predef
	&state ??, xtaddr set,
    0xFFFF #, xtop set,
	xtop xtaddr st,
    next,

s" hex" machineword _hex \ set the base to 16
: hex; ( -- ) _hex word, ;
	&base ??, xtaddr set,
	0x10 #, at0 set,
	at0 xtaddr st,
	next,
s" decimal" machineword _decimal \ set the base to 10
: decimal; ( -- ) _decimal word, ;
	&base ??, xtaddr set,
	0xA #, at0 set,
	at0 xtaddr st,
	next,
s" octal" machineword _octal \ set the base to 8
: octal; ( -- ) _octal word, ;
	&base ??, xtaddr set,
	0x8 #, at0 set,
	at0 xtaddr st,
	next,
s" immediate" machineword _immediate
: immediate; ( -- ) _immediate word, ;
	\ mark the current dictionary entry as immediate
	&dp ??, xtaddr set,
	xtaddr xtop ld,
	xtop xlower ld,
	word/imm #, at0 set,
	xlower at0 xlower or,
	xlower xtop st,
	next,
	
s" 2drop" machineword _2drop ( a b -- ) 2pop next,
: 2drop; ( -- ) _2drop word, ;
s" 2swap" machineword _2swap ( a b c d -- c d a b ) 
: 2swap; ( -- ) _2swap word, ;
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
s" block" machineword _block 
: block; ( -- ) _block word, ;
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

s" expect" colonword _expect
: expect; ( -- ) _expect word, ;
    \ todo implement
    ;;s
s" query" colonword _query
: query; ( -- ) _query word, ;
    tib; @;
    0x50 #plit;
    expect;
    0;
    inn; !;
    ;;s
s" " colonword _null
: null; ( -- ) _null word, ;
deflabel _null1
deflabel _null2
deflabel _null3
    blk; @;
    _null1 ??zbranch; \ if
    1;
    blk;
    +!;
    0;
    inn; !;
    blk; @;
    b/scr;
    1;
    -;
    and;
    0=; 
    _null2 ??zbranch; \ if
    ?exec;
    r>;
    drop; \ endif
_null2 .label   \ else
    _null3 ??branch;
    r>;
_null1 .label
    drop; \ endif
_null3 .label
    ;;s

s" fill" machineword _fill ( addr n b -- ) 
: fill; ( -- ) _fill word, ;
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
s" erase" colonword _erase
: erase; ( -- ) _erase word, ;
    0;
    fill;
    ;;s
s" blanks" colonword _blanks
: blanks; ( -- ) _blanks word, ;
    bl;
    fill;
    ;;s
s" hold" colonword _hold
: hold; ( -- ) _hold word, ;
    -1 #plit;
    hld;
    +!;
    hld; @;
    c@;
    ;;s

s" pad" colonword _pad ( -- n )
: pad; ( -- ) _pad word, ;
    here;
    0x44 #plit;
    +;
    ;;s
s" word" colonword _word
: word; ( -- ) _word word, ;
deflabel _word1
deflabel _word2
    blk; @;
    _word1 ??zbranch;
    blk; @;
    block;
    _word2 ??branch;
_word1 .label
    tib; @; \ endif
_word2 .label
    inn; @;
    +;
    swap;
    enclose;
    here;
    0x22 #plit;
    blanks;
    inn;
    +!;
    over; -;
    >r; r; here;
    c!;
    1+;
    r>;
    cmove;
    ;;s
s" (number)" colonword _pnum
: (number); ( -- ) _pnum word, ;
deflabel-here _pnum1
deflabel _pnum2
deflabel _pnum3
    1+; \ begin
    dup;
    >r;
    c@;
    base; @;
    digit;
    _pnum2 ??zbranch; \ while
    swap;
    base; @;
    u*;
    d+; 
    dpl; @;
    1+;
    _pnum3 ??zbranch; \ if
    1;
    dpl;
    +!; \ endif
_pnum3 .label
    r>;
    _pnum1 ??branch; \ repeat
_pnum2 .label
    r>;
    ;;s
s" number" colonword _number
deflabel _number1
deflabel _number2
deflabel _number3
    0; 0; rot;
    dup; 1+;
    c@;
    0x2D #plit;
    =;
    dup; >r;
    +;
    -1 #plit;
_number1 .label 
    &dpl ??plit; \ begin 
    !;
    (number);
    dup;
    c@;
    _bl ??, plit; 
    -;
    _number2 ??zbranch; \ while
    dup;
    c@;
    0x2E #plit;
    -;
    0;
    ?error;
    0;
    _number1 ??branch; \ repeat
_number2 .label
    drop;
    r>;
    _number3 ??zbranch; \ if
    d-; \ endif
_number3 .label
    ;;s
: number; ( -- ) _number word, ;
s" -find" colonword _dfind
deflabel _dfind1
    bl;
    word;
    here;
    (find);
    dup;
    0=;
    _dfind1 ??zbranch; \ if
    drop;
    here;
    latest;
    (find);
_dfind1 .label \ endif
    ;;s
: -find; ( -- ) _dfind word, ;
s" (abort)" colonword _pabort
    abort;
    ;;s
: (abort); ( -- ) _pabort word, ;

_error s" error" colonword-predef
deflabel _error1
deflabel _error2
    warn; @;
    0<;
    _error1 ??zbranch; \ if
    (abort); \ endif
_error1 .label
    here;
    count;
    type;
    pdotq;
    2;
    \ _questionMarkString word, \ "? "
    message; 
    sp!;
    \ change from the fig model
    \ inn @ blk @
    blk; @;
    2dup;
    _error2 ??zbranch; \ if
    inn; @;
    swap; \ endif
    _error2 .label
    quit;

s" id." colonword _iddot
    pad;
    0x20 #plit;
    0x5f #plit;
    fill;
    dup;
    pfa;
    lfa;
    over;
    -;
    pad;
    swap;
    cmove;
    pad;
    count;
    0x1f #plit;
    and;
    type;
    space;
    ;;s
: id.; ( -- ) _iddot word, ;
_create s" create" colonword-predef 
deflabel _create1
    -find;
    _create1 ??zbranch; \ if
    drop;
    nfa;
    id.;
    4 #plit;
    message;
    space; \ endif
    _create1 .label
    here;
    dup;
    c@;
    width; @;
    min;
    1+;
    allot;
    dup;
    0x0a0 #plit;
    toggle;
    here;
    1;
    -;
    0x80 #plit;
    toggle;
    latest;
    ,;
    current; @; !;
    here;
    2+;
    ,;
    ;;s
s" [compile]" colonword _bcompilep
: [compile]; ( -- ) _bcompilep word, ; 
    -find;
    0=;
    0;
    ?error;
    drop;
    cfa;
    ,;
    ;;s
s" literal" colonword _literal
: literal; ( -- ) _literal word, ;
deflabel _literal1
    state; @;
    _literal1 ??zbranch; \ if
    compile;
    lit;
    ,; \ endif
    _literal1 .label
    ;;s
s" dliteral" colonword _dliteral
: dliteral; ( -- ) _dliteral word, ;
\ TODO implement
    ;;s
s" ?stack" colonword _?stack
: ?stack; ( -- ) _?stack word, ;
    sp@;
    s0; @;
    swap;
    u<;
    1;
    ?error;
    sp@;
    here;
    0x80 #plit;
    +;
    u<;
    7 #plit;
    ?error;
    ;;s
_interpret s" interpret" colonword-predef
deflabel _interpret2
deflabel _interpret3
deflabel _interpret4
deflabel _interpret5
deflabel _interpret6
deflabel _interpret7
deflabel-here _interpret1
    -find; \ begin
    _interpret2 ??zbranch; \ if 
    state; @;
    <;
    _interpret3 ??zbranch; \ if
    cfa;
    ,;
    _interpret4 ??branch; \ else
_interpret3 .label 
    cfa;
    execute;  \ endif
_interpret4 .label
    ?stack;
    _interpret5 ??branch; \ else
_interpret2 .label
    here;
    number;
    dpl; @;
    1+;
    _interpret6 ??zbranch; \ if
    dliteral;
    _interpret7 ??branch; \ else
_interpret6 .label
    drop;
    literal; \ endif
_interpret7 .label
    ?stack; \ endif
_interpret5 .label
    _interpret1 ??branch; \ again
s" vocabulary" colonword _vocabulary
deflabel _dovocab
: vocabulary; ( -- ) _vocabulary word, ;
    <builds;
    literal;
    0xA081 #plit; \ ????
    ,;
    current; @; 
    cfa; ,;
    here;
    voc-link; @; ,;
    voc-link; !;
    does>;
_dovocab .label
    2+;
    context; !;
    ;;s
\ this is a special word that uses dodoes as its interpreter o_O
s" forth" colonword _forth
: forth; ( -- ) _forth word, ;
	\ set the context to the forth base vocabulary
    _dodoes word,
    _dovocab word,
    0xA081 #plit;
    \ TASK - 7 \ cold start value only
    .skip \ end of vocabulary list
s" definitions" colonword _definitions
: definitions; ( -- ) _definitions word, ;
  \ used in the form: cccc definitions 
  \ make cccc vocabulary the current vocabulary.
  \ new definitions will be added to the cccc vocabulary
    context; @;
    current; !;
    ;;s
s" (" colonword _paren
    0x29 #plit;
    word;
    ;;s
_quit s" quit" colonword-predef
deflabel _quit1
deflabel _quit2
    0; blk; !;
    leftbracket;
_quit1 .label
    rp!;
    cr;
    query;
    interpret;
    state; @;
    0=;
    _quit2 ??zbranch; \ if
    print-ok;
    \ endif 
_quit2 .label
    _quit1 ??branch; \ again
_abort s" abort" colonword-predef
    sp!;
    decimal;
    ?stack;
    cr;
    \ TODO print information out here at some point
    \ _dotcpu word,
    pdotq;
    0xd #plit;
    \ s" fig-forth" string,
    \ version information embedded here too
    forth;
    definitions;
    quit;
deflabel _empty-bufs
: empty-buffers; ( -- ) _empty-bufs word, ;
deflabel _wrm
deflabel _wrm1
deflabel _warm
\ warm start vector comes here
_wrm .label
    _wrm1 ??, xip set,
    next,
_wrm1 .label 
    _warm ??, .cell
_warm s" warm" colonword-predef 
    empty-buffers;
    abort;
\ cold start vector comes here
deflabel _density
: density; ( -- ) _density word, ;
deflabel _next_use
: use; ( -- ) _next_use word, ;
deflabel _prev_use
: prev; ( -- ) _prev_use word, ;
deflabel _dr0
: dr0; ( -- ) _dr0 word, ;
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
    cold;
_cold s" cold" colonword-predef 
    empty-buffers;
    0; density; !;
    first; use; !;
    first; prev; !;
    dr0;
    0 #plit;
    _eprint ??plit;
    !;
    \ orig + 12H 
    _origin ??plit;
    0x12 #plit;
    +;
    _up ??plit;
    @;
    _forth ??plit;
    0x6 #plit;
    !;
    abort; \ last
s" s->d" machineword _s->d 
: s->d; ( -- ) _s->d word, ;
deflabel _s->d1
    2pop
    \ TODO implement
_s->d1 .label 
    2push,
s" +-" colonword _pm
: +-; ( -- ) _pm word, ;
deflabel _pm1
    0<;
    _pm1 ??, zbranch; \ if
    -; \ endif
    _pm1 .label
    ;;s
s" d+-" colonword _dpm
    \ TODO implement
    ;;s
s" dabs" colonword _dabs
: dabs; ( -- ) _dabs word, ;
\ TODO implement 
;;s
s" m*" colonword _mstar
: m*; ( -- ) _mstar word, ;
\ TODO implement 
;;s
s" m/" colonword _mslas
: m/; ( -- ) _mslas word, ;
\ todo implement m/
;;s
s" /mod" colonword _slmod
: /mod; ( -- ) _slmod word, ;
\ todo implement /mod
;;s
s" */mod" colonword _ssmod
: */mod; ( -- ) _ssmod word, ;
    >r;
    m*;
    r>;
    m/;
    ;;s

s" */" colonword _ssla
: */; ( -- ) _ssla word, ;
\ todo implement */
;;s
s" m/mod" colonword _msmod
: m/mod; ( -- ) _msmod word, ;
\ todo implement m/mod
;;s
s" (line)" colonword _(line)
: (line); ( -- ) _(line) word, ;
    >r;
    0x40 #plit;
    b/buf;
    */mod;
    r>;
    b/scr;
    *;
    +;
    block;
    +;
    0x40 #plit;
    ;;s
s" .line" colonword _dline
: .line; ( -- ) _dline word, ;
    (line);
    -trailing;
    type;
    ;;s
_message s" message" colonword-predef
    deflabel mess1
    deflabel mess2
    deflabel mess3
    warn; @;
    mess1 ??, zbranch; \ if
    2dup;
    mess2 ??, zbranch; \ if
    4 #plit;
    offset; @;
    b/scr;
    /;
    -;
    .line;
    space; \ endif
    mess3 ??, branch;

    mess2 .label 
        mess3 ??, branch;
    mess1 .label
        pdotq;
        \ encode string and length "msg # "
        .;
    mess3 .label ;;s
s" pc@" machineword _pcload
    \ todo implement
    next,
s" pc!" machineword _pcstore
    \ todo implement
    next,
s" p@" machineword _ptat
    \ todo implement
    next,
s" p!" machineword _pstore
    \ todo implement
    next,
s" drive" defvariableword _drive
: drive; ( -- ) _drive word, ;
    .skip
s" sec" defvariableword _sector_num
: sec; ( -- ) _sector_num word, ;
    .skip 
s" track" defvariableword _track#
: track; ( -- ) _track# word, ;
    .skip
_next_use s" use" defvariableword-predef 
    &FIRST constant,
_prev_use s" prev" defvariableword
    &FIRST constant,
s" sec/blk" defconstantword _sectors/block
: sec/blk; ( -- ) _sectors/block word, ;
    sectors-per-block constant,
s" #buff" defconstantword _numbuff
: #buff; ( -- ) _numbuff word, ;
    num-buffers constant, 
_density s" density" defvariableword-predef
    .skip
s" disk-error" defvariableword _disk_error
: disk-error; ( -- ) _disk_error word, ;
    .skip
s" +buf" colonword _pbuf
deflabel pbuf1
: +buf; ( -- ) _pbuf word, ;
    co #plit;
    +; dup;
    limit; =;
    pbuf1 ??zbranch;
    drop; first;
    pbuf1 .label 
    dup; pref;
    @; -;
    ;;s
s" update" colonword _update
: update; ( -- ) _update word, ;
    @; @;
    0x8000 #plit;
    or;
    prev; @;
    !;
    ;;s
_empty-bufs s" empty-buffers" colonword-predef
    first;
    limit; 
    over;
    -;
    erase;
    ;;s
_dr0 s" dr0" colonword-predef
    0; offset; !;
    ;;s
s" dr1" colonword _dr1
: dr1; ( -- ) _dr1 word, ;
deflabel dr11
deflabel dr12
    density; @; 
    dr11 ??zbranch;
    sectors/double-disk #plit;
    dr12 ??branch;
    dr11 .label
    sectors/disk #plit;
    dr12 .label
    offset; @;
    ;;s
\ note: won't work if only using a single buffer
deflabel _rslw
: r/w; ( -- ) _rslw word, ;
s" buffer" colonword _buffer
: buffer; ( -- ) _buffer word, ;
deflabel buff1
deflabel buff2
    use; @; dup;
    >r; 
    buff1 .label
    +buf;
    buff1 ??zbranch;
    use; !;
    r; @;
    0<; 
    buff2 ??zbranch;
    r; 2+;
    r; @;
    0x7FFF #plit;
    and; 0;
    r/w;
    buff2 .label
    r; !;
    r; prev;
    !; r>;
    2+; ;;s
s" block" colonword _block
deflabel blk1
deflabel blk3
    offset;
    @; +;
    >r; prev;
    @; dup;
    @; r;
    -;
    dup; +;
    blk1 ??, zbranch;
deflabel-here blk2
    +buf; 0=;
    blk3 ??, zbranch;
    drop; r;
    buffer; dup;
    r; 1;
    r/w;
    2; -;
blk3 .label
    dup; @;
    r; -;
    dup; +;
    0=;
    blk2 ??, zbranch;
    dup; prev;
    !;
blk1 .label
    r>; drop;
    2+; ;;s
s" set-io" machineword _setio
: setio; ( -- ) _setio word, ;
\ todo implement
    next,
s" set-drive" machineword _setdrv
: setdrive; ( -- ) _setdrv word, ;
\ todo implement
    next,
s" t&scalc" machineword _t&scalc
: t&scalc; ( -- ) _t&scalc word, ;
    \ todo implement
    ;;s
s" sec-read" machineword _sector_read
: sec-read; ( -- ) _sector_read word, ;
    \ todo implement
    next,
s" sec-write" machineword _sector_write
: sec-write; ( -- ) _sector_write word, ;
    \ todo implement
    next,
_rslw s" r/w" colonword-predef 
    use; @;
    >r;
    swap; sec/blk;
    *; rot;
    use; !;
    sec/blk; 0;
    (do);
deflabel rslw2
deflabel rslw3
deflabel-here rslw1
    over; over;
    t&scalc; setio;
    rslw2 ??zbranch;
    sec-read;
    rslw3 ??branch;
rslw2 .label 
    sec-write;
rslw3 .label
    1+;
    0x80 #plit;
    use; +!;
    rslw1 ??(loop);
    drop; drop;
    r>; use;
    !; ;;s
s" flush" colonword _flush
: flush; ( -- ) _flush word, ;
    #buff; 1+;
    0; (do);
    deflabel-here flush1
    0; buffer;
    drop;
    flush1 ??(loop);
    ;;s
s" load" colonword _load
: load; ( -- ) _load word, ;
   blk; @; >r;
   inn; @; 
   >r; 0;
   inn; !;
   b/scr;  *;
   blk; !; \ blk <- scr * b/scr
   interpret; \ interpret from other screen
   r>; inn; !;
   r>; blk; !;
   ;;s
s" -->" colonword _arrow
: -->; ( -- ) _arrow word, ;
    ?load;  0;
    inn; !; 
    b/scr; blk; @; 
    over; mod; -; 
    blk; +!; ;;s
\ todo support eprint
_eprint .label .skip
s" '" colonword _tick
: '; ( -- ) _tick word, ;
    -find;
    0=;
    0;
    ?error;
    drop;
    literal;
    ;;s
s" forget" colonword _forget
: forget; ( -- ) _forget word, ;
    current; @;
    context; @; -;
    0x18 #plit;
    ?error;
    '; dup;
    fence; @;
    <; 0x15 #plit;
    ?error; dup;
    nfa; dp; !;
    lfa; @;
    context; @;
    !; ;;s
s" back" colonword _back 
: back; ( -- ) _back word, ;
    here; 
    -; 
    ,; 
    ;;s

s" begin" colonword _begin 
: begin; ( -- ) _begin word, ;
    ?comp;
    here; 1;
    ;;s

s" endif" colonword _endiff
: endif; ( -- ) _endiff word, ;
    ?comp; 2; ?pairs;
    here; over; -;
    swap; !;
    ;;s
s" then" colonword _then
: then; ( -- ) _then word, ;
    endif; ;;s

s" do" colonword _do
: do; ( -- ) _do word, ;
    compile;
    (do);
    here;
    3;
    ;;s
s" loop" colonword _loop
: loop; ( -- ) _loop word, ;
    3; ?pairs;
    compile;
    _back ??(loop);
    ;;s
s" +loop" colonword _ploop
: +loop; ( -- ) _ploop word, ;
    3;
    ?pairs;
    compile;
    _back ??(+loop);
    ;;s
s" until" colonword _until
: until; ( -- ) _until word, ;
    1;
    ?pairs;
    compile;
    _back ??zbranch;
    ;;s
s" end" colonword _end
: end; ( -- ) _end word, ;
    until; ;;s
s" again" colonword _again
: again; ( -- ) _again word, ;
    1; ?pairs; 
    compile; 
    _back ??branch; 
    ;;s
s" repeat" colonword _repeat
: repeat; ( -- ) _repeat word, ;
    >r; >r;
    again;
    r>; r>;
    2; -;
    endif;
    ;;s
s" if" colonword _if 
: if; ( -- ) _if word, ;
    compile;
    _here ??zbranch;
    0; ,; 2; ;;s
s" else" colonword _else
: else; ( -- ) _else word, ;
    2; ?pairs; 
    compile; 
    _here ??branch; 
    0; ,; swap; 
    2; endif; 2; ;;s
s" while" colonword _while
: while; ( -- ) _while word, ;
    if; 2+; 
    ;;s
s" spaces" colonword _spaces
: spaces; ( -- ) _spaces word, ;
deflabel spax1
deflabel spax2
    0; max; 
    2dup; 
    spax1 ??zbranch;
    0; (do);
spax2 .label
    space;
    spax2 ??(loop);
spax1 .label
    ;;s
s" <#" colonword bdigs
: <#; ( -- ) bdigs word, ;
    pad;
    hld;
    !;
    ;;s
s" #>" colonword edigs
: #>; ( -- ) edigs word, ;
    drop; drop;
    hld; @;
    pad;
    over;
    -;
    ;;s
s" sign" colonword _sign
: sign; ( -- ) _sign word, ;
deflabel sign1
    rot;
    0<;
    sign1 ??zbranch;
    0x2d #plit;
    hold;
sign1 .label
    ;;s
s" #" colonword dig
: #; ( -- ) dig word, ;
deflabel dig1
    base; @;
    */mod; 
    rot; 
    0x9 #plit;
    over;
    <;
    dig1 ??zbranch;
    0x7 #plit;
    +;
dig1 .label
    0x30 #plit; +;
    hold; ;;s
s" #s" colonword digs
: #s; ( -- ) digs word, ;
deflabel-here digs1
    #;
    over; over; or;
    0=; digs1 ??zbranch;
    ;;s
s" d.r" colonword d.r
: d.r; ( -- ) d.r word, ;
    >r;
    swap;
    ovber;
    -abs;
    <#; #; sign; #>;
    r>; over; -; spaces;
    type; ;;s
s" .r" colonword .r
: .r; ( -- ) .r word, ;
    >r; s->d; r>; d.r; ;;s

s" d." colonword d.
: d.; ( -- ) d. word, ;
0; d.r; space; ;;s

s" u." colonword u.
: u.; ( -- ) u. word, ;
    0; d.; ;;s
s" vlist" colonword vlist 
: vlist; ( -- ) vlist word, ;
    \ todo implement
    ;;s
s" bye" machineword _bye
: bye; ( -- ) _bye word, ;
    _exit ??, jmp
s" list" colonword _list 
: list; ( -- ) _list word, ;
\ todo implement
;;s
s" index" colonword _index 
: index; ( -- ) _index word, ;
\ todo implement
;;s
s" triad" colonword _triad 
: triad; ( -- ) _triad word, ;
\ todo implement
;;s
s" .cpu" colonword _.cpu 
: .cpu; ( -- ) _.cpu word, ;
\ todo implement
;;s

s" match" machineword _match
: match; ( -- ) _match word, ;
    \ todo implement
    ;;s
s" task" colonword _task
: task; ( -- ) _task word, ;
    \ todo implement
    ;;s
ram-start .org
_origin .label
nop,
_cold ??, b,
nop,
_warm ??, b,
\ todo put data to install about version data
\ todo put cold word variables here
_up .label  \ where the user pointer data is located
system-variables-start constant, 
_rpp .label
system-variables-start constant,

asm}

bye
