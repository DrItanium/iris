\ this design is ported from the eforth ppc implementation found online
\ written by C.H. Ting. The comments you see in the words are not mine but the
\ porting effort is mine. Iris is a 16-bit riscy chip with 64 16-bit registers,
\ no FPU, and a 64 kiloword memory space. Instructions are two words wide and the
\ machine exclusively operates on 16-bit words. 
\ the words I define are meant for adaption purposes as well

include iris.fs
\ true iris-debug !
: x.scr ( -- ) 
	\ display the stack and then print a newline
hex .s decimal cr ;
: too-many-vars-defined ( addr -- ) 0x40 >= ABORT" To many registers used!" ;
\ contains all of the registers and pieces used for the monitor itself
\ the monitor is now also the forth system itself
s" figforth.o" {asm
\ setup the registers first
unused-start 1+cconstant xsp \ data stack pointer
1+cconstant xrp \ return stack pointer
1+cconstant xip \ interpretive pointer
1+cconstant xw \ current word pointer
1+cconstant xtop \  contents of the top of stack when a pop is called
1+cconstant xlower \ contents of the second stack item when a pop is called
1+cconstant xthird \ contents of the third stack item
1+cconstant xfourth \ contents of the fourth stack item
1+cconstant xfifth \ contents of the fifth stack item or result of a double add
1+cconstant xsixth \ contents of the sixth stack item or result of a double add
1+cconstant xtaddr \ temporary storage for an address
1+cconstant xerror \ error code
1+cconstant xcoreid \ current core section id
1+cconstant xtmp \ temporary used only by _next
1+cconstant xup \ user area pointer
too-many-vars-defined
xtop cconstant wxtop \ masquerade for double wide operations
xthird cconstant wxlower \ masquerade for double wide operations
xfifth cconstant wxthird 
\ the core memory is a disk buffer of a kind so it will become the disk buffer 
\ of legend that is being discussed in the forth book.
: word, ( v -- ) ??, xrp bl, ;
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
1 constant words-per-cell

\ ascii characters used
0x8 constant cbksp \ backspace
0x0a constant clf \ line feed
0x0d constant ccr \ carriage return

\ memory allocation
ram-end 1+ constant EM \ end of memory

0x400 constant words/block
0x80 constant words/sector
0x400 constant words/track
0xFFFF 1+ constant tracks/disk
words/track words/sector / constant sectors/track
sectors/track tracks/disk * constant sectors/disk
sectors/track tracks/disk 2* * constant sectors/double-disk \ double density, double the number of tracks per disk
0x80 constant words/disk-buffer 
1 constant max-drive-count \ this could change in the future
words/block words/sector * constant sectors/block
1 constant num-buffers
words/sector constant keyboard-buffer
keyboard-buffer 4 + constant co

\ bootstrap-end constant dictionary-start

\ register reservations
deflabel forth1
deflabel _origin
\ deflabel _error
deflabel forth_vocabulary_start
deflabel _cold
deflabel _abort
deflabel _quit
deflabel _interpret
deflabel _message \ message routine
deflabel &S0 \ initial value of the data stack pointer
deflabel &R0 \ initial value of the return stack pointer
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
deflabel _eprint
deflabel _up \ user pointer
deflabel _&current
deflabel _leftbracket
deflabel _compile
deflabel _douser
deflabel _,
: voc-link; ( -- ) &voc-link word, ;
: blk; ( -- ) &blk word, ;
: inn; ( -- ) &IN word, ;
: out; ( -- ) &OUT word, ;
: scr; ( -- ) &scr word, ;
: offset; ( -- ) &offset word, ;
: context; ( -- ) &context word, ;
: current; ( -- ) _&current word, ;
: cold; ( -- ) _cold word, ;
: abort; ( -- ) _abort word, ;
: quit; ( -- ) _quit word, ;
: interpret; ( -- ) _interpret word, ;
: message; ( -- ) _message word, ;
: sp0; ( -- ) &s0 word, ;
: r0; ( -- ) &r0 word, ;
: leftbracket; ( -- ) _leftbracket word, ;
: compile; ( -- ) _compile word, ;
: ,; ( -- ) _, word, ;
: lit, ( n t -- ) xsp pushi, ;
: #lit, ( n -- ) #, lit, ;
: ??lit, ( n -- ) ??, lit, ;
: 0lit, ( -- ) zero xsp push, ;
: 1lit, ( -- ) 0x1 #lit, ;
: w/slit, ( -- ) words-per-cell #lit, ;
: #-> ( constant dest -- ) #, swap set, ;
: ??-> ( label dest -- ) ??, swap set, ;

\ set the constants
&LIMIT constant xlimit
&FIRST constant xfirst
0x8 constant b/scr
0x80 constant b/buf 
1 constant cell-size
variable last-word
variable user-offset
0 last-word !
0 user-offset !
: user-offset@ ( -- n ) user-offset @ ;
: user-offset! ( n -- ) user-offset ! ;
: user-offset1+ ( -- ) user-offset@ 1+ user-offset! ;
\ program start
: next, ( -- ) xrp ret, ;
: 1push, ( -- ) xtop xsp push, next, ;
: 2push, ( -- ) xlower xsp push, 1push, ;
: .string, ( addr len -- ) 
    dup constant, \ embed the length as well
    0 \ go from 0 to length
    ?do
      dup i + c@ constant,
    loop
    drop ;
: embed-name ( str length -- ) .string, ;

0x0 constant word/none
0x1 constant word/compile \ lexicon compile only bit
0x2 constant word/immediate \ lexicon immediate bit
word/compile word/immediate or  constant word/all
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
: print-current-word ( str len -- str len )
  2dup ." current-word: " type cr ;
: defword-header ( str length control-bits "name" -- )
  loc@ >r \ stash a copy of the current location here!
  \ a shim to make next and docol unaware of encoding layout
  \ it does slow things down but it can't be helped at this point
  \ another revision will fix this but now I don't care
  constant, \ stash the control bits here
  \ print-current-word
  embed-name \ stash three more bytes
  last-word @ constant, \ stash the previous word here
  r> last-word ! \ stash the top of this dictionary entry to last word
  \ ." end of entry: " loc@ hex . decimal cr
  ;
: defword-base ( str length control-bits "name" -- ) defword-header deflabel-here ( then define the label to point at here ) ;
: defword-base-predef ( label str length control-bits -- ) defword-header .label ;
: machineword-base ( str length control-bits "name" -- ) defword-base ;
: machineword-base-predef ( label str length control-bits -- ) defword-base-predef ;
: immediate-machineword ( str length "name" -- ) word/immediate machineword-base ;
: immediate-machineword-predef ( label str length -- ) word/immediate machineword-base-predef ;
: machineword ( str length "name" -- ) word/none machineword-base ;
: machineword-predef ( label str length -- ) word/none machineword-base-predef ;
: embed-douser ( -- ) 
    _douser ??, xrp bl,
    user-offset@ constant,
    user-offset1+ ;
: userword-base ( str length control-bits "name" -- ) defword-base embed-douser ;
: userword-base-predef ( label str length control-bits -- ) defword-base-predef embed-douser ;
: userword ( n -- ) word/none userword-base ;
: userword-predef ( label n len -- ) word/none userword-base-predef ;

: 1pop, ( -- ) xsp xtop pop, ;
: 2pop, ( -- ) 1pop, xsp xlower pop, ;
: 3pop, ( -- ) 2pop, xsp xthird pop, ;
: 4pop ( -- ) 3pop, xsp xfourth pop, ;
: defbinaryop ( str length "name" "op" -- )
  machineword 
  2pop,
  xtop xlower xtop ' execute 
  1push, ;
deflabel _lit
deflabel _execute
deflabel _0branch
: two-cell-op ( n id op -- ) word, .cell ;
: plit; ( n id -- )
  \ compile the literal into the dictionary by putting the _LIT command followed by
  \ the number itself
  _lit two-cell-op ;
: ??plit; ( n -- ) ??, plit; ;
: #plit; ( n -- ) #, plit; ;
: lit; ( -- ) _lit word, ;
: execute; ( -- ) _execute word, ;
: branch; ( location id -- ) xrp bl, ;
: ?branch; ( -- ) _0branch word, ;
: ??branch; ( label -- ) ?branch; word, ;
\ code start
0x0000 .org
deflabel .eforth
	.eforth ??, b,
deflabel-here _coldv
deflabel _qrx
deflabel _txsto
deflabel _accept
deflabel _ktap 
deflabel _drop
deflabel _dotok
deflabel _numberq
deflabel _ctop 
deflabel _lastn
deflabel-here _uzero
	0x400 constant, \ reserved
	data-stack-start constant, \ SP0
	return-stack-start constant, \ RP0
	_qrx ??, .cell \ '?key
	_txsto ??, .cell \ 'emit
	_accept ??, .cell \ 'expect
	_ktap ??, .cell \ 'tap
	_drop ??, .cell \ 'echo
	_dotok ??, .cell \ 'prompt
	decimal 10 constant, \ base
	0 constant, \ tmp
	0 constant, \ span
	0 constant, \ >in
	0 constant, \ #tib
	input-buffer-start constant, \ tib
	0 constant, \ csp
	_interpret ??, .cell \ 'eval
	_numberq ??, .cell \ 'number 0x13
	0 constant, \ hld
	0 constant, \ handler
	forth1 ??, .cell \ context pointer
	0x800 constant, \ vocabulary stack
	forth1 ??, .cell \ current pointer
	0 constant, \ vocabulary link pointer
	_ctop ??, .cell \ code dictionary
	input-buffer-start 4 - constant, \ name dictionary
	_lastn ??, .cell \ last
deflabel-here _ulast
deflabel _eforth1
0x0180 .org
.eforth .label
	_eforth1 word,
_eforth1 .label
    _uzero xup ??->
	0x1 #, xup xtop addi,
	xtop xsp ld,
	xtop 1+,
	xtop xrp ld,
	_cold ??, b,
s" bye" machineword _bye ( -- )
	\ exit simulator 
	/dev/terminate-vm io #->
	zero io st,
_qrx s" ?rx" machineword-predef ( -- c T | F )
deflabel qrx1
	\ return input character and true, or a false if no input
	/dev/console0 io #-> 
	io xlower ld, 
	xlower cv eqz,
	qrx1 ??, cv bc, \ if equal zero then no input
    0xFFFF xtop #->
	2push,
qrx1 .label
    0lit,
	next,
_txsto s" tx!" machineword-predef ( c -- )
	\ send character c to the output device.
	1pop,
    /dev/console0 io #->
	xtop io st,
	next,
s" !io" machineword _storeio ( -- )
	\ initialize the serial I/O devices
	next,
_lit s" lit" word/compile machineword-base-predef
    xrp xlower ld, \ address of next which is a literal
    xlower xtop ld, \ load the value
    xlower 1+,     \ skip over the cell
    xlower xrp st, \ overwrite the cell
    1push,
s" exit" machineword _exit
    \ terminate a colon definition
    xrp ret,
: exit; ( -- ) _exit word, ;
_execute s" execute" machineword-predef
	\ execute the definition whose code field address cfa is on the data stack
    1pop, \ top - cfa
    \ do not use normal call procedure since we don't want to come back here
    \ and muck up the return stack
    xtop br, \ go there, the return stack has not been touched
s" next" word/compile machineword-base _donext ( -- )
deflabel donext0
    \ runtime code for the single index loop.
    \ : next ( -- ) \ hilevel model
    \   r> r> dup if 1 - >r @ >r exit then drop cell+ >r ;
    \ After next is a bl instruction, not an address.
    xrp xtop pop, xtop xsp push, \ r>
    xrp xtop pop, xtop xsp push, \ r>
                                 \ implied dup
    xtop xtop eqz,               \ if
    donext0 ??, xtop bc,
    xsp xtop pop, xtop 1-,       \ 1 - 
    xtop xrp push,               \ >r
    xsp xtop pop, xtop xtop ld,  \ @
    xtop xrp push,               \ >r
    xrp ret,
donext0 .label
    xsp zero pop,                \ drop
    xsp xtop pop, xtop 1+,       \ cell+
    xtop xrp push,               \ >r
    xrp ret,                     \ ;
: donext; ( -- ) _donext word, ;
_0branch s" 0branch" word/compile machineword-base-predef
deflabel _zbra1
	1pop, \ flag
	zero xtop cv neq,
	_zbra1 ??, cv bc, \ 0<>?
    \ do nothing at this point since it is already setup correctly
	next,
_zbra1 .label
    xrp xlower ld, \ load the return address which is equals 0 case
    0x2 #, xlower xlower addi, \ compute <>0 case
    xlower xrp st, 
	next,
s" !" machineword _store ( v a -- ) 
   2pop, \ top - addr
         \ lower - value
   xlower xtop st, \ perform the store
   next,
s" @" machineword _at
    1pop,
    xtop xtop ld,
	1push,
: @; ( -- ) _at word, ;
s" c!" machineword _cstore  ( value addr -- ) 
	2pop, \ top - addr
		 \ lower - value
    0xFF #, xlower xlower andi,
    xlower xtop st, \ save it to memory with the upper 8 bits masked
    next,
: c!; ( -- ) _cstore word, ;
s" c@" machineword _cat
	1pop, 
	xtop xtop ld,
	0xFF #, xtop xtop andi,
	1push,
: c@; ( -- ) _cat word, ;
s" rp@" machineword _rpat
    \ push xrp onto xsp
    xrp xsp push,
    next,
: rp@; ( -- ) _rpat word, ;
s" rp!" word/compile machineword-base _rpstore
    \ set the return stack pointer.
    ( a -- )
    1pop, \ top - new stack pointer
    xtop xrp move, 
    next,
s" r>" machineword _rfrm \ retrieve item from top of return stack
    xrp xtop pop,
	1push,
s" r@" machineword _rat \ copy top of return stack onto stack
	xrp xtop ld,
	1push,
s" >r" word/compile machineword-base _>r 
    \ push the data stack to the return stack
    ( w -- )
    1pop, 
    xtop xrp push,
    next,
s" sp@" machineword _spat
    xsp xsp push,
    next,
s" sp!" machineword _spstore ( a -- )
    \ set the data stack pointer
    1pop,
    xtop xsp move,
    next,
_drop s" drop" machineword-predef 
    xsp zero pop,
    next,
s" dup" machineword _dup 
	xsp xtop ld,
	1push,
s" swap" machineword _swap
	2pop, \ top -- b
		 \ lower -- a
	xtop xsp push,
	xlower xsp push,
    next,
s" over" machineword _over 
	2pop, 
	xlower xsp push,
	xtop xsp push,
	xlower xsp push, 
	next,
s" 0<" machineword _0<
    1pop,
    xtop xtop ltz,
	1push,
s" and" defbinaryop _and and,
s" or"  defbinaryop _or or,
s" xor" defbinaryop _xor xor,
s" um+" machineword _uplus ( a b -- c f )
    \ add two numbers, return the sum and carry flag
    xsp xtop pop, \ do a wide add at this point and just zero out the upper portion
    zero xlower move, 
    xsp xthird pop, 
    zero xfourth move,
    xlower xtop xfifth addw, \ we take the result and
    xfifth xlower move,
    xtop xtop neqz, \ check and see if the upper portion has been set, this becomes the carry flag
    2push,
deflabel-here dovariable ( -- a )
    \ runtime routine for VARIABLE and CREATE 
    xrp xtop pop, \ this routine will be called from variables, we use the return address as the beginning of values
    1push, \ push the address of the next storage to the data stack and then return to what called the routine two levels up
: dovariable; ( -- ) dovariable word, ;
deflabel-here doconstant ( -- n )
    \ runtime routine for CONSTANT and VALUE
    xrp xtop pop, \ get the address of the CONSTANT or VALUE
    xtop xtop ld, \ load the value at this location
    1push, \ push the value onto the stack
_up s" up" machineword-predef  ( -- a )
    \ pointer to the user area
    xup xsp push,
    next,
_douser .label ( -- a )
    \ runtime routine for user variables
    xrp xtop pop, \ get the address of where we were
    xtop xtop ld, \ get the offset from memory
    xup xtop xtop add,
    1push,
&S0 s" sp0" userword-predef 
&R0 s" rp0" userword-predef
s" '?key" userword _tqky
: 'qky; ( -- ) _tqky word, ;
s" 'emit" userword _temit
: 'emit; ( -- ) _temit word, ;
s" 'expect" userword _texpect
: 'expect; ( -- ) _texpect word, ;
s" 'tap" userword _ttap
s" 'echo" userword _techo
: 'echo; ( -- ) _techo word, ;
s" 'prompt" userword _tprompt
: 'prompt; ( -- ) _tprompt word, ;
s" base" userword _base
: base; ( -- ) _base word, ;
: base@; ( -- ) base; @; ;
s" tmp" word/compile userword-base _tmp
: temp; ( -- ) _tmp word, ;
s" span" userword _span
&in s" >in" userword-predef
s" #tib" userword _ntib
user-offset1+ \ since it is doublewide advance by one again
: #tib; ( -- ) _ntib word, ;
s" csp" userword _csp
s" 'eval" userword _teval
: 'eval; ( -- ) _teval word, ;
s" 'number" userword _tnumber
: 'number; ( -- ) _tnumber word, ;
s" hld" userword _hld
: hld; ( -- ) _hld word, ;
s" handler" userword _handler
: handler; ( -- ) _handler word, ;
&context s" context" userword-predef \ already advanced one at this point
\ consumes eight cells
user-offset@ 8 + user-offset!
_&current s" current" userword-predef \ advance four cells
user-offset@ 4 + user-offset!
s" cp" userword _cp
: cp; ( -- ) _cp word, ;
s" np" userword _np
: np; ( -- ) _np word, ;
s" last" userword _last
: last; ( -- ) _last word, ;
: !; ( -- ) _store word, ;
: um+; ( -- ) _uplus word, ;
: and; ( -- ) _and word, ;
: or; ( -- ) _or word, ;
: xor; ( -- ) _xor word, ;
: 0<; ( -- ) _0< word, ;
: rp!; ( -- ) _rpstore word, ;
: sp@; ( -- ) _spat word, ;
: sp!; ( -- ) _spstore word, ;
: drop; ( -- ) _drop word, ;
: dup; ( -- ) _dup word, ;
: over; ( -- ) _over word, ;
: swap; ( -- ) _swap word, ;
: >r; ( -- ) _>r word, ;
: r>; ( -- ) _rfrm word, ;
: r@; ( -- ) _rat word, ;
s" dovoc" word/compile machineword-base _dovocab  ( -- )
    \ runtime action of vocabularies
   r>; 
   context; 
   !; 
   exit; 
s" forth" machineword _forth ( -- )
	_dovocab word,
forth1 .label
    last-word @ constant, \ vocabulary head pointer
    0 constant, \ vocabulary link pointer
: forth; ( -- ) _forth word, ;
    \ make forth the context vocabulary
    _dovocab word,
s" ?dup" machineword _qdup
    xsp xtop ld,
    xtop xrp reteqz,
    xtop xsp push,
    next,
: ?dup; ( -- ) _qdup word, ;
s" rot" machineword _rot ( a b c -- b c a )
	3pop, ( a b c -- b c a )
		 \ top - c
		 \ lower - b
		 \ third - a 
	xlower xsp push,
	xtop xsp push,
	xthird xsp push, 
    next,
s" 2drop" machineword _2drop ( a b -- ) 
    2pop, 
    next,
s" 2dup" machineword _2dup ( a b -- a b a b ) 
    2pop, \ top - b
          \ lower - a
    xlower xsp push,
    xtop xsp push,
    2push,
s" +" defbinaryop _+ add,
s" d+" machineword _dplus
    ( xl xh yl yh -- zl zh )
    \ iris takes in a front register which is the lower half
    \ because of this we have to pop registers differently
    \ not in the order found on the stack
    \ top - yl
    \ lower - yh
    \ third - xl
    \ fourth - xh
    \ actual input from the stack
    ( xl xh yl yh -- sl sh )
    xsp wxtop popw, \ wxtop then lower
    xsp wxlower popw, \ third then fourth
    wxtop wxlower wxtop addw, \ result will be in xtop,xlower
    wxtop xsp pushw, \ push xtop then xlower
    next,
s" not" machineword _not
    1pop, 
    0xFFFF xlower #->
    xlower xtop xtop xor,
    1push,
s" negate" machineword _negate
    1pop, 
    xtop xtop negate,
    1push,
: negate; ( -- ) _negate word, ;
s" dnegate" machineword _dnegate
    xsp wxtop popw, 
    wxtop wxtop negatew,
    wxtop xsp pushw, 
    next,
s" -" defbinaryop _- sub, 
s" abs" machineword _abs
    xsp xtop ld,
    xtop xrp retgez,
    xtop xtop negate,
    xtop xsp st,
    next,
s" =" defbinaryop _= eq,
s" u<" defbinaryop _u< ult,
s" u>" defbinaryop _u> ugt,
s" >" defbinaryop _> gt,
s" <" defbinaryop _less lt,
s" min" defbinaryop _min min,
s" max" defbinaryop _max max,
s" umin" defbinaryop _umin umin,
s" umax" defbinaryop _umax umax,
s" within" machineword _within ( u ul uh -- t )
deflabel within0
    \ return true if u is within the range of ul and uh
    3pop, \ top - uh
          \ lower - ul
          \ third - u

    within0 xtaddr ??->
    xtop xthird cv gt, \ u > top
    xtaddr cv bcr,
    xlower xthird cv lt, \ u < lower
    xtaddr cv bcr,
    0xFFFF #lit,
    next,
within0 .label
    0lit,
    next,
: within; ( -- ) _within word, ;
: min; ( -- ) _min word, ;
: max; ( -- ) _max word, ;
: u<; ( -- ) _u< word, ;
: u>; ( -- ) _u> word, ;
: >; ( -- ) _> word, ;
: <; ( -- ) _less word, ;
: =; ( -- ) _= word, ;
: abs; ( -- ) _abs word, ;
: -; ( -- ) _- word, ;
: not; ( -- ) _not word, ;
: negate: ( -- ) _negate word, ;
: d+; ( -- ) _dplus word, ;
: +; ( -- ) _+ word, ;
: 2dup; ( -- ) _2dup word, ;
: rot; ( -- ) _rot word, ;
: 2drop; ( -- ) _2drop word, ;
\ division operations
s" um/mod" machineword _ummd ( udl udh u -- ur uq ) \ discard udh for the moment
    \ unsigned divide of a double by a single. Return mod and quotient
    1pop,
    xsp wxlower popw,
    xtop wxlower wxtop um/mod, \ top -> quotient
                               \ lower -> remainder
    2push,
: um/mod; ( -- ) _ummd word, ;
s" m/mod" machineword _msmd ( d n -- r q ) 
    \ signed floored divide of double 
    xsp wxtop popw,
    xsp xthird pop,
    xthird wxtop wxtop m/mod, \ top -> quotient
                              \ lower -> remainder
    2push,
: m/mod; ( -- ) _msmd word, ;
s" /mod" machineword _slmod ( n n -- r q )
\ signed divide. Return mod and quotient
    xsp xthird pop,
    xsp xfourth pop,
    xthird xfourth xtop div,
    xthird xfourth xlower rem,
    2push,
s" mod" defbinaryop _mod rem,
s" /" defbinaryop _/ div, 
s" u*" defbinaryop _u* umul,
s" um*" machineword _umstar ( a b -- n )
    \ unsigned multiply, return double product
    xsp xthird pop, \ b
    xsp xfourth pop, \ a
    xthird xfourth wxtop um*,
    wxtop xsp pushw,
    next,
s" *" defbinaryop _* mul,
s" m*" machineword _mstar ( a b -- n )
    \ signed multiply, return double product
    xsp xthird pop, \ b
    xsp xfourth pop, \ a
    xthird xfourth wxtop m*,
    wxtop xsp pushw,
    next,
: m*; ( -- ) _mstar word, ;
: *; ( -- ) _* word, ;
: u*; ( -- ) _u* word, ;
: mod; ( -- ) _mod word, ;
: /; ( -- ) _/ word, ;
s" */mod" machineword _ssmod ( n1 n2 n3 -- r q )
    \ multiply n1 and n2, then divide by n3. Return mod and quotient.
    >r;
    m*;
    r>;
    m/mod;
    exit;
: */mod; ( -- ) _ssmod word, ;
s" */" machineword _stasl ( n1 n2 n3 -- q )
    \ multiply n1 by n2, then divide by n3. Return quotient only.
    3pop, \ top - n3
          \ lower - n2
          \ third - n1
    xlower xthird xlower mul,
    xtop xlower xtop div,
    1push,
s" cell+" machineword _cell+ ( a -- b )
    \ add cell size in words to address [ originally this was bytes ]
    1pop,
    xtop 1+,
    1push,
: cell+; ( -- ) _cell+ word, ;
s" cell-" machineword _cell- ( a -- b )
    \ subtract cell size in words from address
    1pop,
    xtop 1-,
    1push,
: cell-; ( -- ) _cell- word, ;
s" cells" machineword _cells ( n -- n )
    \ multiply tos by cell size in words
    \ this is a nop since this is in words
    next,
: cells; ( -- ) _cells word, ;
s" aligned" machineword _aligned ( n -- n )
    \ align address to the cell boundary
    \ nop since automatic alignment :D
    next,
: aligned; ( -- ) _aligned word, ;
s" bl" machineword _blank ( -- 32 )
    \ return 32, the blank character
    0x20 #lit,
    next,
: bl; ( -- ) _blank word, ;
s" >char" machineword _tchr ( c -- c )
deflabel tcha1
    \ filter non-printing characters
    0x7f #lit, and; dup; \ mask msb
    bl; 0x7f #lit, within; \ check for printable
    not;
    tcha1 ??branch; 
    drop; 0x2e #lit, \ replace non printables
tcha1 .label
    exit;
s" depth" machineword _depth ( -- n )
\ return the depth of the data stack
    sp@;
    sp0; @;
    swap; -;
    cell-;
    w/slit, /;
    exit;
: depth; ( -- ) _depth word, ;
s" pick" machineword _pick ( ... +n --  ... w )
    \ copy the nth stack item to tos
    1pop, \ top - index
    xsp xtop xlower add, \ make the address to load from
    xlower xtop ld, \ load the address
    1push,
s" +!" machineword _pstore ( n a -- )
    \ add n to the contents at address a
    2pop,
    xtop xthird ld,
    xlower xthird xlower add,
    xlower xtop st,
    next,
: +!; ( -- ) _pstore word, ;
s" 2!" machineword _dstore ( d a -- )
    \ store the double integer to address a
    1pop, \ top - 
    xsp wxlower popw,
    wxlower xtop stw,
    next,
: 2!; ( -- ) _dstore word, ;
s" 2@" machineword _dat ( a -- d )
    \ fetch double integer from address a
    xsp xthird pop,
    xthird wxtop ldw,
    2push,
: 2@; ( -- ) _dat word, ;
s" count" machineword _count ( b -- b +n )
    \ return count byte of a string and add 1 to byte address.
    1pop,
    1 #, xtop xlower addi,
    xtop xtop ld,
    2push,
: count; ( -- ) _count word, ;
s" here" machineword _here ( -- a )
    \ return the top of the code dictionary.
    cp; @;
    exit;
: here; ( -- ) _here word, ;
s" pad" machineword _pad ( -- a )
    \ return the address of a temporary buffer.
    here; decimal 80 #lit, +;
    exit;
: pad; ( -- ) _pad word, ;
s" tib" machineword _tib ( -- a )
    \ return the address of the terminal input buffer
    #tib; cell+; @;
    exit;
: tib; ( -- ) _tib word, ;
s" @execute" machineword _atexec ( a -- )
    \ execute vector stored in address a 
    1pop,
    xtop xtop ld,
    xtop cv eqz,
    cv xsp cret,
    xtop br,
: @execute; ( -- ) _atexec word, ;
s" cmove" machineword _cmove ( b1 b2 u -- )
deflabel cmove0
deflabel cmove1
    \ copy u words from b1 to b2
    3pop, \ top - u - count
          \ lower - b2 dest-addr
          \ third - b1 src-addr
    xtop cv eqz,
    cmove1 ??, cv bc,
cmove0 .label
    xthird xfourth ld, \ load from src
    xfourth xlower st, \ store to dest
    xthird 1+,
    xlower 1+,
    xtop 1-,
    xtop cv neqz,
    cmove0 ??, cv bc,
cmove1 .label
    next,
: cmove; ( -- ) _cmove word, ;
s" fill" machineword _fill ( b u c -- )
deflabel fill1
    \ fill u words of character c to area beginning at b
    3pop, \ top - character
          \ lower - count
          \ third - address
    xlower cv eqz,
    fill1 ??, cv bc,
deflabel-here fill0 
    xtop xthird st,
    xthird 1+,
    xlower 1-,
    xlower cv neqz,
    fill0 ??, cv bc,
fill1 .label
    next,
: 1+,, ( -- ) 
  \ push a 1 onto the stack and then do the plus operation 
  1lit,
  +; ;
s" -trailing" machineword _dtrailing ( b u -- b u )
deflabel dtrail2
    \ adjust the count ot eliminate trailing white space
    >r;
    dtrail2 word,
deflabel-here dtrail1
    bl; over; r@; +; c@;
    <; dtrail2 ??branch; 
    r>;
    1+,,
    exit; \ adjusted count
dtrail2 .label
    donext; dtrail1 word,
    0lit,
    exit;
: -trailing; ( -- ) _dtrailing word, ;
s" pack$" machineword _pack$ ( b u a -- a )
    \ build a counted string with u characters from b. Null fill
    aligned; dup;
    >r; \ strings only on cell boundary
    over; dup;
    0lit, \ push zero
    w/slit, \ push cell size
    um/mod; drop; \ count mod cell
    -; over; +; 0lit, swap; !;              \ null fill cell
    dup; c!; 1+,, \ save count
    swap; cmove; r>; 
    exit; \ move string
: pack$; ( -- ) _pack$ word, ;
\ numeric output single precision
s" digit" machineword _digit ( u -- c )
    \ convert digit u to a character
    0x9 #lit, over; <;
    0x7 #lit, and; +;
    0x30 #lit, +;
    exit;
: digit; ( -- ) _digit word, ;
s" extract" machineword _extract
    0lit, swap; um/mod;
    swap; digit;
    exit;
: extract; ( -- ) _extract word, ;
s" <#" machineword _bdgs ( -- )
    \ initiate the numeric output process.
    \ following actions are not necessary because there is no cached top of stack
    \ push the cached TOR to return stack
    \ save IP from lr to TOR
    pad; hld; !;
    exit;
: <#; ( -- ) _bdgs word, ;
: 1-,, ( -- ) 
  1lit,
  -; ; 
s" hold" machineword _hold ( c -- )
    \ insert a character into the numeric output string
    hld; @; 1-,,
    dup; hld; !; c!;
    exit;
: hold; ( -- ) _hold word, ;
s" #" machineword _extractDigit ( u -- u )
    \ extract one digit from u and append the digit to output string.
    base@; extract; hold;
    exit; 
: #; ( -- ) _extractDigit word, ;

s" #s" machineword _exdigs ( u -- 0 )
deflabel digs2
    \ convert u until all digits are added to the output string
deflabel-here digs1
    digit; dup;
    digs2 ??branch;
    digs1 word,
digs2 .label
    exit;
: #s; ( -- ) _exdigs word, ;
s" sign" machineword _sign ( n -- )
    \ add a minus sign to the numeric output string
deflabel sign1
    0<;
    sign1 ??branch;
    0x2d #lit, hold;
sign1 .label
    exit;
: sign; ( -- ) _sign word, ;
s" #>" machineword _edgs ( w --  b u )
    \ prepare the output string to be typed
    drop; hld; @; pad; over; -;
    exit;
: #>; ( -- ) _edgs word, ;
s" str" machineword _str ( n -- b u )
    \ convert a signed integer to a numeric string
    dup; >r; abs; 
    <#; #s; r>; sign; #>;
    exit;
: str; ( -- ) _str word, ;
s" hex" machineword _hex ( -- )
    \ use radix 16 as base for numeric conversions
    0x10 #lit, base; !;
    exit;
: hex; ( -- ) _hex word, ;
s" decimal" machineword _decimal ( -- )
    \ use radix 10 as base for numeric conversions
    0xa #lit, base; !; 
    exit; 
: decimal; ( -- ) _decimal word, ;
\ numeric input single precision
s" digit?" machineword _digit? ( c base -- u t )
deflabel dgtq1
    \ convert a character to its numeric value. A flag indicates success.
    >r; 0x30 #lit, \ '0'
    -;
    0x9 #lit, over; <;
    dgtq1 ??branch;
    7 #lit, -; dup; 0xa #lit, <; or;
dgtq1 .label
    dup; r>; u<; 
    exit;
: digit?; ( -- ) _digit? word, ;
_numberq s" number?" machineword-predef ( a -- n T | a F )
deflabel numq1
deflabel numq2
deflabel numq3
deflabel numq4
deflabel numq5
deflabel numq6
    \ convert a number string to integer. Push a flag on tos.
    base@; >r;
    0lit,
    over; count; 
    over; c@;
    0x24 #lit, \ '$'
    =;
    numq1 ??branch;
    hex; swap;
    1+,,
    swap;
    1-,,
numq1 .label
    over; c@;
    0x2d #lit, =;
    >r; swap; r@; -;
    swap; r@; +;
    ?dup;
    numq6 ??branch;
    1-,,
    >r;
numq2 .label
    dup;
    >r; c@;
    base@;
    digit?;
    numq4 ??branch;
    swap;
    base@;
    *;
    +;
    r>;
    1+,,
    donext; numq2 word, 
    r@; swap; drop;
    numq3 ??branch;
    negate;
numq3 .label
    swap;
    numq5 word,
numq4 .label
    r>; r>;
    2drop; 2drop;
    0lit,
numq5 .label
    dup;
numq6 .label
    r>;
    2drop;
    r>;
    base; !;
    exit;

\ basic io
s" ?key" machineword _qkey ( -- c T | F )
    \ return input character and true, or a false if no input
    'qky; 
    @execute;
    exit;
s" key" machineword _key ( -- c )
    \ wait for and return an input character
deflabel-here key1
    _qkey word,
    key1 ??branch;
    exit;
: key; ( -- ) _key word, ;
s" emit" machineword _emit ( c -- )
    \ send a character to the output device
    'emit;
    @execute;
    exit;
: emit; ( -- ) _emit word, ;
: emit#; ( value -- ) #lit, emit; ;
s" pace" machineword _pace ( -- )
    \ send a pace character for the file downloading process.
    decimal 11 #lit,
    emit;
    exit;
s" space" machineword _space ( -- )
    \ send the blank character to the output device
    bl;
    emit;
    exit;
: space; ( -- ) _space word, ;
s" spaces" machineword _spaces ( +n -- ) 
deflabel char2
    \ send n spaces to the output device.
    0lit,
    max;
    >r;
    char2 word,
deflabel-here char1
    space;
char2 .label
    donext;
    char1 word, 
    exit;
: spaces; ( -- ) _spaces word, ;
s" type" machineword _type ( b u -- )
deflabel type2
    \ output u characters from b.
    >r;
    type2 word,
deflabel-here type1
    dup; c@;
    emit;
    1+,,
type2 .label
    donext;
    type1 word,
    drop;
    exit;
: type; ( -- ) _type word, ;
s" cr" machineword _cr ( -- )
    \ output a carriage return and a line feed.
    ccr emit#;
    clf emit#;
    exit;
: cr; ( -- ) _cr word, ;
s" do$" word/compile machineword-base _dostr ( -- a )
    \ return the address of a compiled string
    r>; r@; 
    dup; \ one copy must be returned
    count; 
    r>;
    +;
    1+,, \ go to the end of string
    aligned; \ absolute addr after string
    >r;
    drop;    \ discard counted address
    swap;
    >r;
    exit;
: dostr; ( -- ) _dostr word, ;
s\" $\"|" word/compile machineword-base _stqp ( -- a )
    \ runtime routine compiled by $". Return address of a compiled string.
    dostr;
    exit;
: stqp; ( -- ) _stqp word, ;
s\" .\"|" word/compile machineword-base _dtqp ( -- )
    \ runtime routine of ." . output a compile string.
    dostr;
    count;
    type;
    exit;
: dtqp; ( -- ) _dtqp word, ;
s" .r" machineword _dotr ( n +n -- )
	\ display an integer in a field of n columsn, right justified
	>r;
	str;
	r>;
	over;
	-;
	spaces;
	type;
	exit;
s" u.r" machineword _udotr ( u +n -- )
	\ display an unsigned integer in n column, right justified
	>r;
	<#;
	#s;
	#>;
	r>;
	over;
	-;
	spaces;
	type;
	exit;
s" u." machineword _udot ( u -- ) 
	\ display an unsigned integer in free format
	<#;
	#s;
	#>;
	spaces;
	type;
	exit;
: u.; ( -- ) _udot word, ;
s" ." machineword _dot ( w -- )
deflabel dot1
	\ display an integer in free format, preceeded by a space
	base@;
    0xa #lit,
	xor;			 \ ?decimal
	dot1 ??branch;
	u.;
	exit;
dot1 .label
	str;
	spaces;
	type;
	exit;
: .; ( -- ) _dot word, ;
s" ?" machineword _quest ( a -- )
	\ display the contents in a memory cell
	@;
	.;
	exit;
\ parsing words
s" parse" machineword _parse0 ( b u c -- b u delta ; <string> )
deflabel parse2
deflabel parse3
deflabel parse5
deflabel parse6
deflabel parse7
deflabel parse8
	\ scan string delimited by c. Return found string and its offset
	temp; !;
	over;
	>r;
	dup;
	parse8 ??branch; 
	xsp xtop pop,
	xtop 1-,
	xtop xsp push,
	temp; @; bl; =;
	parse3 ??branch; 
	>r;
deflabel-here parse1
	bl; over; c@; 	\ skip leading blanks only
	-; 0<; not;
	parse2 ??branch; 
    1+,,
	donext; parse1 word,
	r>;
	drop;
    0lit, 0lit,
    \ originally a dup
	exit;
parse2 .label
	r>;
parse3 .label
	over;
	swap;
	>r;
deflabel-here parse4 
	temp;
	@;
	bl;
	=;
	parse5 ??branch; 
	0<;
parse5 .label
	parse6 ??branch; 
	xsp xtop pop,
	xtop 1+,
	xtop xsp push,
	donext; parse4 word,
	dup; >r;
	parse7 word,
parse6 .label
	r>; drop; dup; 1+,, >r;
parse7 .label
	over; -; r>; r>; -;
    exit;
parse8 .label
	over; r>; -;
	exit;
s" parse" machineword _parse ( c -- b u ; <string> )
	\ scan input stream and return counted string delimited by c.
	>r; tib; inn; @; +; \ current input buffer pointer
	#tib; @; inn; @; -; 	\ remaining count
	r>; _parse0 word, inn; +!;
    exit;
: parse; ( -- ) _parse word, ;
s" .(" immediate-machineword _dotpr ( -- )
	\ output following string up to next ) .
	0x29 #lit, \ )
	parse; type;
	exit;
s" (" immediate-machineword _paren ( -- )
	\ ignore following string up to next ). A comment
	0x29 #lit, \ )
	parse; 2drop;
	exit;
s" \" immediate-machineword _backslash ( -- )
	\ ignore following text till the end of line
	#tib; @; inn; !;
	exit;
s" char" machineword _char ( -- c )
	\ parse next word and return its first character
	bl; parse; drop; c@;
	exit;
s" token" machineword _token
	bl; parse;
	decimal 31 #lit, min; np; @;
	over; -; cell-; pack$;
	exit;
: token; ( -- ) _token word, ;
s" word" machineword _word ( c -- a ; <string> )
	\ parse a word from input stream and copy it to code dictionary.
	parse; here; pack$;
	exit;
: word; ( -- ) _word word, ;
\ dictionary search
s" name>" machineword _namet ( na -- ca )
	\ return a code address given a name address
	cell-; cell-; \ 16-bit cells, 16 bit values.$$$ ???
	@;
	exit;
: name>; ( -- ) _namet word, ;
s" same?" machineword _sameq ( a a u -- a a f \ -0+ )
deflabel same2
	\ compare u cells in two strings. Return 0 if identical
	>r; same2 word,
deflabel-here same1
	over; r@; cells; +; @; \ 32/16 mix-up
	over; r@; cells; +; @; \ 32/16 mix-up
	-; ?dup;
	same2 ??branch; 
	r>; drop;
	exit;	\ strings not equal
same2 .label
	donext; same1 word,
    0lit,
	exit;	\ strings equal
: same?; ( -- ) _sameq word, ;
s" find" machineword _find ( a va -- ca na | a F )
deflabel find2
deflabel find3
deflabel find4
deflabel find5
deflabel find6
	\ search a vocabulary for a string. Return ca and na if succeeded
	swap; dup; c@; w/slit, /; temp; !;	\ 32/16 bit mix-up
	dup; @; >r; cell+; swap;
deflabel-here find1
	@; dup; 
    find6 ??branch; 
	dup; @; 0x1f7f #lit, and; r@; xor;
	find2 ??branch; 
	cell-; \ backup to link field
	find1 word,	\ try the next word
find2 .label
	cell+; temp; @; same?;
find3 .label
	find4 word,
find6 .label
	r>; drop; swap; cell-; swap;
	exit;
find4 .label
	find5 ??branch; 
	cell-; cell-; find1 word,
find5 .label
	r>; drop; swap; drop; cell-; dup; name>; swap;
	exit; \ ca

s" name?" machineword _nameq ( a -- ca na | a F ) 
deflabel nameq1
deflabel nameq2
deflabel nameq3
	\ search all context vocabularies for a string.
	context; dup; 2@; xor;	\ ?context = also
	nameq1 ??branch; 
	cell-; \ no, start with context
nameq1 .label
	>r;
nameq2 .label
	r>; cell+; dup; >r; 	\ next in search order
	@; 
    ?dup; nameq3 ??branch; 
	_find word,
	?dup; nameq2 ??branch; 
	r>; drop;
	exit;	\ found name
nameq3 .label
	r>; drop; \ name not found
    0lit,
	exit; \ false flag
: name?; ( -- ) _nameq word, ;
: 'echo->@exec; ( -- ) 'echo; @execute; ;
s" ^h" machineword _bksp ( bot eot cur -- bot eot cur )
	\ backup the cursor by one character.
deflabel back1
	>r; over; r>;
	swap; over; xor;
	back1 ??branch; 
    cbksp #lit,
	'echo->@exec;
    1-,, bl;
	'echo->@exec;
    cbksp #lit,
	'echo->@exec;
back1 .label
	exit;
s" tap" machineword _tap ( bot eot cur c -- bot eot cur )
	\ accept and echo the key stroke and bump the cursor.
	dup; 
    'echo->@exec;
	over; c!; 1+,,
	exit;
_ktap s" ktap" machineword-predef ( bot eot cur c -- bot eot cur )
deflabel ktap1
deflabel ktap2
	\ Process a key stroke, cr, or backspace
	dup;
    ccr #lit, xor;
	ktap2 ??branch; 
    cbksp #lit, xor;
	ktap1 ??branch; 
	bl; _tap word,
	exit;
ktap1 .label
	_bksp word,
	exit;
ktap2 .label
	drop; swap; drop; dup;
	exit;
_accept s" accept" machineword-predef ( b u -- b u )
deflabel accept2
deflabel accept3
deflabel accept4
	\ accept characters to input buffer. Return with actual count
	over; +; over; 
deflabel-here accept1
	2dup; xor;
	accept4 ??branch; 
	key; dup; bl; decimal 127 #lit, within;
	accept2 ??branch; 
	_tap word,
	accept3 word,
accept2 .label
	_ttap word, @execute;
accept3 .label
	accept1 word,
accept4 .label
	drop; over; -;
	exit;
s" except" machineword _except ( b u -- )
	\ accept input stream and store count in SPAN.
	_texpect word, @execute;
	_span word, !;
	drop;
	exit;
s" query" machineword _query ( -- )
	\ accept input stream to terminal input buffer.
	tib; decimal 80 #lit,
	_texpect word, @execute; #tib; !;
	drop;
    0lit, inn; !;
	exit;
\ error handling
s" catch" machineword _catch ( ca -- 0 | err# )
	\ execute word at ca and setup and error frame for it
	sp@; >r;
	handler; @; >r; \ save error frame
	rp@; handler; !; execute; \ execute
	r>; _handler word; !; \ restore error frame
	r>; drop; 0lit, exit; \ no error
s" throw" machineword _throw ( err# -- err# ) 
	handler; @;
	rp!;  \ restore return stack
	r>; handler; !; \ restore handler frame
	r>; swap; >r; sp!; \ restore data stack
	drop; r>; exit;
: throw; ( -- ) _throw word, ;
s" null$" machineword _nulld ( -- a )
	\ return address of a null string with zero count
	dovariable; 
	0 constant,
	99 constant, \ c
	111 constant, \ o
	121 constant, \ y
	111 constant, \ o
	116 constant, \ t 
	101 constant, \ e
_abort s" abort" machineword-predef ( -- )
	\ reset data stack and jump to quit
	_nulld word,
	throw;

s\" abort\"" word/compile machineword-base _abortq ( f -- ) 
	\ runtime routine of abort" . Abort with a message.
deflabel abortq1
	abortq1 ??branch; \ text flag
	dostr; 
	throw; \ pass error string
abortq1 .label
	dostr; drop;
	exit;	\ drop error
: abortq; ( -- ) _abortq word, ;
\ the text interpreter
_interpret s" $interpret" machineword-predef ( a -- )
deflabel _interpret1
deflabel interpret2
	\ interpret a word. If failed, try to convert it to an integer.
	name?; ?dup; 	\ ?defined
	_interpret1 ??branch; 
	@; word/compile #lit, and; \ ?compile only lexicon bits
	abortq; s" compile only" .string,
	execute;
	exit; \ execute defined word
_interpret1 .label
	'number; \ convert a number
	@execute;
	interpret2 ??branch; 
	exit;
interpret2 .label \ error
	throw;
_leftbracket s" [" immediate-machineword-predef ( -- )
	\ start the text interpreter
    _interpret ??lit,
	'eval; !;
	exit;
_dotok s" .ok" machineword-predef ( -- ) 
deflabel dotok1
	\ display ok only while interpreting
	_interpret ??lit,
	'eval; @; =;
	dotok1 ??branch; 
	dtqp; s" ok" .string,
dotok1 .label
	cr;
	exit;
		
s" ?stack" machineword _qstack ( -- )
	\ abort if the data stack underflows
	depth; 0<;
	abortq; s" underflow" .string, 
	exit;

s" eval" machineword _eval ( -- )
	\ interpret the input stream
deflabel-here eval1
deflabel eval2
	token; dup; c@; \ input stream empty
	eval2 ??branch; 
	'eval; @execute; _qstack word, \ evaluate input, check stack
	eval1 word,
eval2 .label
	drop; 'prompt; @execute;
	exit;	\ prompt
\ shell
s" preset" machineword _preset ( -- ) 
	\ reset data stack pointer and the terminal input buffer
	sp0; @; sp!;
	input-buffer-start #lit,
    #tib; cell+; !;
	exit;
s" xio" word/compile machineword-base _xio ( a a a -- )
	\ reset the i/o vectors 'expect, 'tap, 'echo, and 'prompt
	\ this seems questionable to me :/
	_accept ??lit, 
	'expect; 2!;
	'echo; 2!; 
	exit;
s" file" machineword _file ( -- )
	\ select io vectors for file download
	_pace ??lit,
	_drop ??lit,
	_ktap ??lit,
	_xio word,
	exit;
s" hand" machineword _hand ( -- )
	\ select io vectors for terminal interface
	_dotok ??lit, 
	\ _drop ??lit, \ don't repeat characters, macos did it already 4/20/97 - cht
				   \ doesn't seem to apply to me since I'm not on mac os - jws 
	_ktap ??lit,
	_xio word,
	exit;
s" i/o" machineword _i/o ( -- a )
	\ array to store default io vectors
	dovariable;
	_qrx ??, .cell
	_txsto ??, .cell \ default io vectors for 32 bit systems 3/16/92
s" console" machineword _console ( -- )
	\ initiate terminal interface.
	_i/o word, 2@;
	'qky; 2!;
	_hand word,
	exit;
_quit s" quit" machineword-predef ( -- )
deflabel quit3
deflabel quit4
	\ reset stack pointer and start text interpreter
	r0; @; rp!; \ reset return stack pointer
deflabel-here quit1
	leftbracket; \ start interpretation
deflabel-here quit2
	_query word, 	   \ get input
    _eval ??lit, _catch word, ?dup;			   \ evaluate input
	quit2 ??branch; \ continue till error
	'prompt; @; swap;    \ save input device
	_console word, _nulld word, over; xor; \ display error message?
	quit3 ??branch; spaces; count; type; \ error message
	dtqp; s"  ? " .string, \ error prompt
	cr;
quit3 .label
    _dotok ??lit,
	xor; \ file input?
	quit4 ??branch; 
    decimal 27 #lit, \ push the error code
	emit; \ file error, tell host
quit4 .label
	_preset word, \ some cleanup
	quit1 ??, xrp bl,
\ compiler routines
s" '" machineword _tick ( -- ca )
deflabel tick1
	\ search context vocabularies for the next word in input stream
	_token word, _nameq word, \ ?defined
	tick1 ??branch; 
	exit;	\ yes, push code address
tick1 .label
	throw;	\ no, error
s" allot" machineword _allot ( n -- )
	\ allocate n words to the code dictionary
	_cp word, +!;
	exit;	\ adjust code pointer
_, s" ," machineword-predef ( w -- )
	\ compile an integer into the code dictionary
	here; dup; cell+; _cp word, !; !;
	exit; \ adjust code pointer, compile
deflabel _again
: again; ( -- ) _again word, ;
s" [compile]" immediate-machineword _bcompile ( -- ; <string> )
	\ compile the next immediate word into code dictionary
	_tick word,
	again;
	exit;

_compile s" compile" word/compile machineword-base-predef ( -- )
	\ compile the next address in colon list to code dictionary
	r>; ( a )
	dup; ( a a )
	1pop, \ xtop - a
	xtop xtop ld, 
	xtop xsp push, ( a bl )
	over;  \ convert abs addr to rel addr ( not needed for my design )
	+;
	\ 0x03FFFFFFC #lit,
	\ and; ; a ca -- 
	again; \ compile bl instruction
	cell+; >r;
	exit;

s" literal" immediate-machineword _literal ( w -- )
	\ compile top of stack to code dictionary as an integer literal
	compile; lit; \ _lit will be compiled into the dictionary
	,;
	exit;

s\" $,\"" machineword _stcq ( -- )
	\ compile a literal string up to next " .
	\ Is this supposed to be compile time?
	0x22 #lit, \ '"'
	word; \ move string to code dictionary
	count; +; _aligned word, \ calculate aligned end of string
	cp; !;
	exit; \ adjust the code pointer
: stcq; ( -- ) _stcq word, ;
s" recurse" machineword _recurse ( -- )
	\ make the current word available for compilation
	last; @; name>; again;
	exit; \ compile branch instruction

\ structures

s" then" immediate-machineword _then ( a -- )
	\ terminate a forward branch structure.
	\ the forward reference deposits a branch instruction. The address
	\ is filled by a.
	here; over; -; \ construct a base address to store to
	\ need to construct a branch instruction
	#call #lit, over; !; \ stash a zero #call 
    1+,, \ go to next address
	!; \ store the address into this cell
	exit;
: then; ( -- ) _then word, ;
_again s" again" immediate-machineword-predef ( a -- )
	\ resolve a backwards jump and terminate a loop structure
	\ compile a branch instruction, with 'a' in the address field.
	here; -; \ offset from here
	#call #lit, ,; \ stash the call instruction portion
	,; \ stash the address
	exit;
s" for" immediate-machineword _for ( -- a )
	\ start a for-next loop structure in a colon definition
	compile; >r; here;
	exit;
s" begin" immediate-machineword _begin ( -- a )
	\ start an infinite or indefinite loop structure
	here;
	exit;
s" next" immediate-machineword _next ( a -- )
	\ terminate a for-next loop structure
	compile; donext; again;
	exit;
s" until" immediate-machineword _until ( a -- )
	\ terminate a begin-until indefinite loop structure
	compile; ?branch; again;
	exit;
s" if" immediate-machineword _if ( -- A )
    \ begin a conditional branch
    compile; ?branch; here; #call #lit, \ call with register zero thus it is a branch
    ,;
    0lit, ,;
    exit;
s" ahead" immediate-machineword _ahead ( -- A )
    \ compile a forward branch instruction
    here; #call #lit, \ call with register zero thus it is a branch
    ,;
    0lit, ,;
    exit;
: ahead; ( -- ) _ahead word, ;
s" repeat" immediate-machineword _repeat ( A a -- ) 
    \ terminate a begin while-repeat indefinite loop
    again; then;
    exit;

s" aft" immediate-machineword _aft ( a -- a A )
    \ Jump to THEN in a FOR-AFT-THEN-NEXT loop the first time through
    drop; ahead; _begin word, swap;
    exit;

s" else" immediate-machineword _else ( A -- A )
    \ start the false clause in an if-else-then structure
    ahead; swap; then;
    exit;
s" while" immediate-machineword _while ( a -- A a )
    \ Conditional branch out of a begin-while-repeat loop
    _if word, swap;
    exit;
s\" abort\"" immediate-machineword _rabortq ( -- ; <string> )
    \ conditional abort with an error message.
    compile; abortq; stcq;
    exit;
s\" $\"" immediate-machineword _strq ( -- ; <string> )
    \ compile an inline string literal.
    compile; stqp; stcq;
    exit;
s\" .\"" immediate-machineword _dotq ( -- ; <string> )
    \ compile an inline string literal to be typed out at run time.
    compile; dtqp; stcq;
    exit;
\ name compiler
s" ?unique" machineword _unique ( a -- a )
deflabel unique1
    \ display a warning message if the word already exists.
    dup; name?; \ ?name exists
    unique1 ??branch; \ redefinitions are OK
    dtqp; s" redefined " .string, \ but warn the user
    over; count; type;   \ just in case it is not planned
unique1 .label
    drop;
    exit;
s" $,n" machineword _snam ( na -- )
deflabel pnam1
    \ build a new dictionary name using the string at na.
    dup; c@;     \ null input?
    pnam1 ??branch; 
    _unique word, \ redef?
    dup; last; !; \ save na for vocabulary link
    here; aligned; swap;  \ align code address
    cell-; \ link addresso
    current; @; @; over; !; cell-; dup; np; !; \ adjust name pointer
    !; 
    exit;
pnam1 .label
    stqp; s"  name" .string,  \ null input
    throw;
\ forth compiler
\ s" $compile" machineword _$compile ( a -- )
\ deflabel scom1
\ deflabel scom2
\ deflabel scom3
\     \ compile next word to code dictionary as a token or literal.
\     name?;
\     ?dup;   \ ?defined
\     scom2 ??branch;
\     @;
\     word/immediate 
_cold s" cold" machineword-predef ( -- ) 
    \ the high level cold start sequence
deflabel-here cold1
    \ todo finish
    _forth word, context; @;
    dup;
    \ todo finish
    _quit word,
    cold1 ??, b,
\ always should be last
last-word @ .org
_ctop .label \ a hack to stash the correct address in the user variables
_lastn .label \ hack for now
asm}

bye
