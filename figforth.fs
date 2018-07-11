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
\ contains all of the registers and pieces used for the monitor itself
\ the monitor is now also the forth system itself
s" figforth.o" {asm
2 constant bytes-per-word
1 constant words-per-cell
0 constant version-major
1 constant version-minor
\ setup the registers first
unused-start 
1+cconstant xsp \ data stack pointer
1+cconstant xrp \ return stack pointer
1+cconstant xtop \  contents of the top of stack when a pop is called
1+cconstant xlower \ contents of the second stack item when a pop is called
1+cconstant xthird \ contents of the third stack item
1+cconstant xfourth \ contents of the fourth stack item
1+cconstant xfifth \ contents of the fifth stack item or result of a double add
1+cconstant xsixth \ contents of the sixth stack item or result of a double add
1+cconstant xup \ user area pointer
1+cconstant xlink \ used when we need to do some crazy manipulations of the stack top
too-many-registers-defined
xtop cconstant wxtop \ masquerade for double wide operations
xthird cconstant wxlower \ masquerade for double wide operations
xfifth cconstant wxthird 
\ the core memory is a disk buffer of a kind so it will become the disk buffer 
\ of legend that is being discussed in the forth book.
: word, ( v -- ) ??, xrp call, ;
: defalias ( l "name" -- ) create , does> @ word, ;
0xF000 constant system-start
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
0x8 constant cbksp \ backspace
0x0a constant clf \ line feed
0x0d constant ccr \ carriage return
: dropxrp, ( -- ) xrp spdrop, ;
: dropxsp, ( -- ) xsp spdrop, ;
: retxrp, ( -- ) xrp ret, ;

\ register reservations
deflabel forth1 
deflabel _cold 
def2label _interpret _message \ message routine
def2label _eprint _up \ user pointer
def3label _&current _leftbracket _compile
deflabel _douser

: lit, ( n t -- ) xsp pushi, ;
: #lit, ( n -- ) #, lit, ;
: ??lit, ( n -- ) ??, lit, ;
: 0lit, ( -- ) zero xsp push, ;
: 1lit, ( -- ) 0x1 #lit, ;
: w/slit, ( -- ) words-per-cell #lit, ;

\ set the constants
variable last-word
variable user-offset
0 last-word !
2 user-offset ! \ skip reserved section
: user-offset@ ( -- n ) user-offset @ ;
: user-offset! ( n -- ) user-offset ! ;
: user-offset1+ ( -- ) user-offset@ 2+ user-offset! ;
\ program start
: next, ( -- ) retxrp, ;
: 1push, ( -- ) xtop xsp push, next, ;
: 2push, ( -- ) xlower xsp push, 1push, ;
: .string, ( addr len -- ) 
    dup constant, \ embed the length as well
    0 \ go from 0 to length
    ?do
      dup i + c@ .data8
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
: defword-header ( str length control-bits "name" -- )
  loc@ >r \ stash a copy of the current location here!
  \ a shim to make next and docol unaware of encoding layout
  \ it does slow things down but it can't be helped at this point
  \ another revision will fix this but now I don't care
  constant, \ stash the control bits here
  embed-name \ stash three more bytes
  last-word @ constant, \ stash the previous word here
  r> last-word ! \ stash the top of this dictionary entry to last word
  \ ." end of entry: " loc@ hex . decimal cr
  ;
: defword-base ( str length control-bits "name" -- ) 
  defword-header 
  deflabel-here 
  ( then define the label to point at here ) 
  execute-latest defalias ;
: defword-base-predef ( label str length control-bits -- ) defword-header dup .label defalias ;
: machineword-base ( str length control-bits "name" -- ) defword-base ;
: machineword-base-predef ( label str length control-bits -- ) defword-base-predef ;
: immediate-machineword ( str length "name" -- ) word/immediate machineword-base ;
: immediate-machineword-predef ( label str length -- ) word/immediate machineword-base-predef ;
: machineword ( str length "name" -- ) word/none machineword-base ;
: machineword-predef ( label str length -- ) word/none machineword-base-predef ;

: embed-douser ( -- ) 
    _douser ??, xrp call, 
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
: machineword-1arg ( s l "n" -- ) machineword 1pop, ;
: machineword-2arg ( s l "n" -- ) machineword 2pop, ;
: machineword-predef-1arg ( s l "n" -- ) machineword-predef 1pop, ;
: machineword-predef-2arg ( s l "n" -- ) machineword-predef 2pop, ;
: defbinaryop ( str length "name" "op" -- )
  machineword-2arg
  xtop xlower xtop ' execute 
  1push, ;
: branch; ( location id -- ) xrp call, ;
\ code start
0x0000 .org
deflabel .eforth
\ setup the stacks as well
	.eforth ??, b,
deflabel-here _coldv
def2label _qrx _txsto
def2label _accept _ktap
def2label _drop _dotok
def3label _numberq _ctop _lastn
deflabel-here _uzero
	0x400 constant, \ reserved
	data-stack-start constant, \ SP0
	return-stack-start constant, \ RP0
	_qrx ??cell, \ '?key
	_txsto ??cell, \ 'emit
	_accept ??cell, \ 'expect
	_ktap ??cell, \ 'tap
	_drop ??cell, \ 'echo
	_dotok ??cell, \ 'prompt
	decimal 10 constant, \ base
	0 constant, \ tmp
	0 constant, \ span
	0 constant, \ >in
	0 constant, \ #tib
	input-buffer-start constant, \ tib
	0 constant, \ csp
	_interpret ??cell, \ 'eval
	_numberq ??cell, \ 'number 0x13
	0 constant, \ hld
	0 constant, \ handler
	forth1 ??cell, \ context pointer
	0x800 constant, \ vocabulary stack
	forth1 ??cell, \ current pointer
	0 constant, \ vocabulary link pointer
	_ctop ??cell, \ code dictionary
	input-buffer-start 4 - constant, \ name dictionary
	_lastn ??cell, \ last
deflabel-here _ulast
deflabel _eforth1
0x0180 .org
.eforth .label
    activate-debug-if-logical,
	_eforth1 ??, b, 
_eforth1 .label
    _uzero ??, xup set,
	0x2 #, xup xtop addi,
	xtop xsp ldtincr,
	xtop xrp ldtincr,
	_cold ??, b,
s" bye" machineword _bye bye; ( -- )
	\ exit simulator 
    terminate,
_qrx s" ?rx" machineword-predef ?rx; ( -- c T | F )
deflabel qrx1
	\ return input character and true, or a false if no input
    xlower getc, 
    qrx1 ??, xlower beqz, \ if equal zero then no input
    0xFFFF xtop #set,
	2push,
qrx1 .label
    0lit,
	next,
_txsto s" tx!" machineword-predef-1arg tx!; ( c -- )
	\ send character c to the output device.
    xtop putc, 
	next,
s" !io" machineword _storeio !io; ( -- )
	\ initialize the serial I/O devices
	next,
s" lit" word/compile machineword-base _lit lit; 
    xrp xlower ld, \ address of next which is a literal
    xlower xtop ldtincr, \ load the value then skip over the cell
    xlower xrp st, \ overwrite the cell
    1push,
s" lit2" word/compile machineword-base _lit2 lit2; ( -- h l )
    \ push the next two words onto the stack
    xrp xthird ld, \ address of next which is a literal
    xthird xtop ldtincr, \ load the lower half then skip over the cell
    xthird xlower ldtincr, \ load the upper half then skip over the cell
    xlower xrp st, \ overwrite the return address
    2push,

s" exit" machineword _exit exit;
    \ terminate a colon definition
    xrp spdrop, \ pop the top element off ( where we were )
    retxrp,
s" execute" machineword-1arg _execute execute;
	\ execute the definition whose code field address cfa is on the data stack
    \ top - cfa
    \ do not use normal call procedure since we don't want to come back here
    \ and muck up the return stack
    xtop br, \ go there, the return stack has not been touched

s" -" defbinaryop _- -; sub,
s" r>" machineword _rfrm r>; \ retrieve item from top of return stack
    xrp xlink pop, \ get the return address separately
    xrp xtop pop, \ now get the value we actually want
    xtop xsp push,
    xlink br, \ get out of here
s" >r" word/compile machineword-base _>r >r;
    \ push the data stack to the return stack
    ( w -- )
    xrp xlink ld, \ load the top element
    1pop, 
    xtop xrp st, \ replace the top with this value
    xlink br,
s" dup" machineword _dup  dup;
	xsp xtop ld,
	1push,
_drop s" drop" machineword-predef drop;
    xsp zero pop,
    next,
s" cell+" machineword-1arg _cell+ cell+; ( a -- b )
    \ add cell size in words to address [ originally this was bytes ]
    words-per-cell #, xtop xtop addi,
    1push,
s" @" machineword-1arg _at @;
    xtop xtop ld,
	1push,
s" next" word/compile machineword-base _donext donext; ( -- )
deflabel donext0
    \ runtime code for the single index loop.
    \ : next ( -- ) \ hilevel model
    \   r> r> dup if 1 - >r @ >r exit then drop cell+ >r ;
    \ After next is a bl instruction, not an address.
    r>; r>; dup;
    1pop,
    donext0 ??, xtop beqz,
    0x1 #lit, -;
    >r; @; >r; 
    next,
donext0 .label
    drop;
    cell+; cell+; 
    >r;
    next,
s" 0branch" word/compile machineword-base _0branch ?branch;
deflabel _zbra1
	1pop, \ flag
    _zbra1 ??, xtop bneqz, \ 0<>?
    \ do nothing at this point since it is already setup correctly
	next,
_zbra1 .label
    xrp xlower ld, \ load the return address which is equals 0 case
    0x2 #, xlower xlower addi, \ compute <>0 case
    xlower xrp st, 
	next,
s" !" machineword-2arg _store !; ( v a -- ) 
   \ top - addr
   \ lower - value
   xlower xtop st, \ perform the store
   next,
s" c!" machineword-2arg _cstore c!; ( value addr -- ) 
	\ top - addr
	\ lower - value
    xlower xtop stb, \ save it to memory with the upper 8 bits masked
    next,
s" c@" machineword-1arg _cat c@;
	xtop xtop ldb,
	1push,
s" rp@" machineword _rpat rp@;
    \ push xrp onto xsp
    xrp xsp push,
    next,

s" rp!" word/compile machineword-base _rpstore rp!;
    \ set the return stack pointer.
    ( a -- )
    1pop, \ top - new stack pointer
    xtop xrp move, 
    next,
s" r@" machineword _rat r@; \ copy top of return stack onto stack
	xrp xtop ld,
	1push,
s" sp@" machineword _spat sp@;
    xsp xsp push,
    next,
s" sp!" machineword-1arg _spstore sp!; ( a -- )
    \ set the data stack pointer
    xtop xsp move,
    next,
s" swap" machineword-2arg _swap swap;
	\ top -- b
	\ lower -- a
	xtop xsp push,
	xlower xsp push,
    next,
s" over" machineword-2arg _over over;
	xlower xsp push,
	xtop xsp push,
	xlower xsp push, 
	next,
s" 0<" machineword-1arg _0< 0<;
    xtop xtop ltz,
	1push,
s" and" defbinaryop _and and; and, 
s" or"  defbinaryop _or or; or, 
s" xor" defbinaryop _xor xor; xor, 
s" um+" machineword _uplus um+; ( a b -- c f )
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
dovariable defalias dovariable;
deflabel-here doconstant ( -- n )
    \ runtime routine for CONSTANT and VALUE
    xrp xtop pop, \ get the address of the CONSTANT or VALUE
    xtop xtop ld, \ load the value at this location
    1push, \ push the value onto the stack
doconstant defalias doconstant;
_up s" up" machineword-predef up; ( -- a )
    \ pointer to the user area
    xup xsp push,
    next,
_douser .label ( -- a )
    \ runtime routine for user variables
    xrp xtop pop, \ get the address of where we were
    xtop xtop ld, \ get the offset from memory
    xup xtop xtop add,
    1push,
s" sp0" userword &s0 sp0;
s" rp0" userword &r0 rp0;
s" '?key" userword _tqky '?key;
s" 'emit" userword _temit 'emit;
s" 'expect" userword _texpect 'expect;
s" 'tap" userword _ttap 'tap;
s" 'echo" userword _techo 'echo;
s" 'prompt" userword _tprompt 'prompt;
s" base" userword _base base;
: base@; ( -- ) base; @; ;
s" tmp" word/compile userword-base _tmp temp;
s" span" userword _span span;
s" >in" userword &in inn;
s" #tib" userword _ntib #tib;
user-offset1+ \ since it is doublewide advance by one again
s" csp" userword _csp csp;
s" 'eval" userword _teval 'eval;
s" 'number" userword _tnumber 'number;
s" hld" userword _hld hld;
s" handler" userword _handler handler;
s" context" userword &context context; \ already advanced one at this point
\ consumes eight cells
user-offset@ decimal 16 + user-offset!

_&current s" current" userword-predef current; \ advance four cells
user-offset@ decimal 8 + user-offset!

s" cp" userword _cp cp;
s" np" userword _np np;
s" last" userword _last last;
s" dovoc" word/compile machineword-base _dovocab  dovocab; ( -- )
    \ runtime action of vocabularies
   r>; 
   context; 
   !; 
   exit; 
s" forth" machineword _forth forth; ( -- )
    dovocab;
forth1 .label
    last-word @ constant, \ vocabulary head pointer
    0 constant, \ vocabulary link pointer
    \ make forth the context vocabulary
    dovocab;
s" ?dup" machineword _qdup ?dup;
    xsp xtop ld,
    xtop xrp reteqz,
    xtop xsp push,
    next,
s" rot" machineword _rot rot; ( a b c -- b c a )
	3pop, ( a b c -- b c a )
		 \ top - c
		 \ lower - b
		 \ third - a 
	xlower xsp push,
	xtop xsp push,
	xthird xsp push, 
    next,
s" 2drop" machineword _2drop 2drop; ( a b -- ) 
    2pop, 
    next,
s" 2dup" machineword _2dup 2dup; ( a b -- a b a b ) 
    2pop, \ top - b
          \ lower - a
    xlower xsp push,
    xtop xsp push,
    2push,
s" +" defbinaryop _+ +; add, 
s" d+" machineword _dplus d+;
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
s" not" machineword _not not;
    1pop, 
    0xFFFF xlower #set,
    xlower xtop xtop xor,
    1push,
s" negate" machineword _negate negate;
    1pop, 
    xtop xtop negate,
    1push,
s" dnegate" machineword _dnegate dnegate;
    xsp wxtop popw, 
    wxtop wxtop negatew,
    wxtop xsp pushw, 
    next,
s" abs" machineword _abs abs;
    xsp xtop ld,
    xtop xrp retgez,
    xtop xtop negate,
    xtop xsp st,
    next,
s" =" defbinaryop _= =; eq, 
s" u<" defbinaryop _u< u<; ult, 
s" u>" defbinaryop _u> u>; ugt, 
s" >" defbinaryop _> >; gt, 
s" <" defbinaryop _less <; lt, 
s" min" defbinaryop _min min; min, 
s" max" defbinaryop _max max; max, 
s" umin" defbinaryop _umin umin; umin,
s" umax" defbinaryop _umax umax; umax,
s" within" machineword _within within; ( u ul uh -- t )
deflabel within0
    \ return true if u is within the range of ul and uh
    3pop, \ top - uh
          \ lower - ul
          \ third - u
    within0 ??, xtop xthird bgt,   \ u > top
    within0 ??, xlower xthird blt, \ u < lower
    0xFFFF #lit,
    next,
within0 .label
    0lit,
    next,
\ division operations
s" um/mod" machineword _ummd um/mod; ( udl udh u -- ur uq ) \ discard udh for the moment
    \ unsigned divide of a double by a single. Return mod and quotient
    1pop,
    xsp wxlower popw,
    xtop wxlower wxtop um/mod, \ top -> quotient
                               \ lower -> remainder
    2push,
s" m/mod" machineword _msmd m/mod; ( d n -- r q ) 
    \ signed floored divide of double 
    xsp wxtop popw,
    xsp xthird pop,
    xthird wxtop wxtop m/mod, \ top -> quotient
                              \ lower -> remainder
    2push,
s" /mod" machineword _slmod /mod; ( n n -- r q )
\ signed divide. Return mod and quotient
    xsp xthird pop,
    xsp xfourth pop,
    xthird xfourth xtop div,
    xthird xfourth xlower rem,
    2push,
s" mod" defbinaryop _mod mod; rem, 
s" /" defbinaryop _/ /; div,  
s" u*" defbinaryop _u* u*; umul, 
s" um*" machineword _umstar um*; ( a b -- n )
    \ unsigned multiply, return double product
    xsp xthird pop, \ b
    xsp xfourth pop, \ a
    xthird xfourth wxtop um*,
    wxtop xsp pushw,
    next,
s" *" defbinaryop _* *; mul, 
s" m*" machineword _mstar m*; ( a b -- n )
    \ signed multiply, return double product
    xsp xthird pop, \ b
    xsp xfourth pop, \ a
    xthird xfourth wxtop m*,
    wxtop xsp pushw,
    next,
s" */mod" machineword _ssmod */mod; ( n1 n2 n3 -- r q )
    \ multiply n1 and n2, then divide by n3. Return mod and quotient.
    >r; m*; r>;
    m/mod;
    exit;
s" */" machineword _stasl */; ( n1 n2 n3 -- q )
    \ multiply n1 by n2, then divide by n3. Return quotient only.
    3pop, \ top - n3
          \ lower - n2
          \ third - n1
    xlower xthird xlower mul,
    xtop xlower xtop div,
    1push,
s" cell-" machineword _cell- cell-; ( a -- b )
    \ subtract cell size in words from address
    1pop,
    words-per-cell #, xtop xtop subi,
    1push,
s" cells" machineword _cells cells; ( n -- n )
    \ multiply tos by cell size in words
    1pop,
    words-per-cell #, xtop xtop muli,
    1push,
s" aligned" machineword _aligned aligned; ( n -- n )
    1pop,
    0xFFFE #, xtop xtop andi,
    1push,
s" bl" machineword _blank bl; ( -- 32 )
    \ return 32, the blank character
    0x20 #lit,
    next,
: ??branch; ( label -- ) ?branch; word, ;
s" >char" machineword _tchr >char; ( c -- c )
deflabel tcha1
    \ filter non-printing characters
    0x7f #lit, and; dup; \ mask msb
    bl; 0x7f #lit, within; \ check for printable
    not; tcha1 ??branch; 
    drop; 0x2e #lit, \ replace non printables
tcha1 .label
    exit;
s" depth" machineword _depth depth; ( -- n )
\ return the depth of the data stack
    sp@; sp0; @;
    swap; -;
    cell-;
    w/slit, /;
    exit;
s" pick" machineword _pick pick; ( ... +n --  ... w )
    \ copy the nth stack item to tos
    1pop, \ top - index
    xsp xtop xlower add, \ make the address to load from
    xlower xtop ld, \ load the address
    1push,
s" +!" machineword _pstore +!; ( n a -- )
    \ add n to the contents at address a
    2pop,
    xtop xthird ld,
    xlower xthird xlower add,
    xlower xtop st,
    next,
s" 2!" machineword _dstore 2!; ( d a -- )
    \ store the double integer to address a
    1pop, \ top - 
    xsp wxlower popw,
    wxlower xtop stw,
    next,
s" 2@" machineword _dat 2@; ( a -- d )
    \ fetch double integer from address a
    xsp xthird pop,
    xthird wxtop ldw,
    2push,
s" count" machineword _count count; ( b -- b +n )
    \ return count byte of a string and add 1 to byte address.
    1pop,
    1 #, xtop xlower addi,
    xtop xtop ld,
    2push,
s" here" machineword _here here; ( -- a )
    \ return the top of the code dictionary.
    cp; @;
    exit;
s" pad" machineword _pad pad; ( -- a )
    \ return the address of a temporary buffer.
    here; decimal 80 #lit, +;
    exit;
s" tib" machineword _tib tib; ( -- a )
    \ return the address of the terminal input buffer
    #tib; cell+; @;
    exit;
s" @execute" machineword _atexec @execute; ( a -- )
    \ execute vector stored in address a 
    1pop,
    xtop xtop ld,
    xtop xsp reteqz,
    xtop br,
s" cmove" machineword _cmove cmove; ( b1 b2 u -- )
def2label cmove0 cmove1
    \ copy u words from b1 to b2
    3pop, \ top - u - count
          \ lower - b2 dest-addr
          \ third - b1 src-addr
    cmove1 ??, xtop beqz,
cmove0 .label
    xthird xfourth ld, \ load from src
    xfourth xlower st, \ store to dest
    xthird 1+,
    xlower 1+,
    xtop 1-,
    cmove0 ??, xtop bneqz, 
cmove1 .label
    next,

s" fill" machineword _fill fill; ( b u c -- )
deflabel fill1
    \ fill u words of character c to area beginning at b
    3pop, \ top - character
          \ lower - count
          \ third - address
    fill1 ??, xlower beqz, 
deflabel-here fill0 
    xtop xthird st,
    xthird 1+,
    xlower 1-,
    fill0 ??, xlower bneqz,
fill1 .label
    next,
: 1+,, ( -- ) 
  \ push a 1 onto the stack and then do the plus operation 
  1lit,
  +; ;
s" -trailing" machineword _dtrailing -trailing; ( b u -- b u )
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
s" pack$" machineword _pack$ pack$; ( b u a -- a )
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
\ numeric output single precision
s" digit" machineword _digit digit; ( u -- c )
    \ convert digit u to a character
    0x9 #lit, over; <;
    0x7 #lit, and; +;
    0x30 #lit, +;
    exit;
s" extract" machineword _extract extract;
    0lit, swap; um/mod;
    swap; digit;
    exit;
s" <#" machineword _bdgs <#; ( -- )
    \ initiate the numeric output process.
    \ following actions are not necessary because there is no cached top of stack
    \ push the cached TOR to return stack
    \ save IP from lr to TOR
    pad; hld; !;
    exit;
: 1-,, ( -- ) 1lit, -; ; 
s" hold" machineword _hold hold; ( c -- )
    \ insert a character into the numeric output string
    hld; @; 1-,,
    dup; hld; !; c!;
    exit;
s" #" machineword _extractDigit #; ( u -- u )
    \ extract one digit from u and append the digit to output string.
    base@; extract; hold;
    exit; 

s" #s" machineword _exdigs #s; ( u -- 0 )
deflabel digs2
    \ convert u until all digits are added to the output string
deflabel-here digs1
    digit; dup;
    digs2 ??branch;
    digs1 word,
digs2 .label
    exit;
s" sign" machineword _sign sign; ( n -- )
    \ add a minus sign to the numeric output string
deflabel sign1
    0<; sign1 ??branch;
    0x2d #lit, hold;
sign1 .label
    exit;
s" #>" machineword _edgs #>; ( w --  b u )
    \ prepare the output string to be typed
    drop; hld; @; pad; over; -;
    exit;
s" str" machineword _str str; ( n -- b u )
    \ convert a signed integer to a numeric string
    dup; >r; abs; 
    <#; #s; r>; sign; #>;
    exit;
s" hex" machineword _hex hex; ( -- )
    \ use radix 16 as base for numeric conversions
    0x10 #lit, base; !;
    exit;
s" decimal" machineword _decimal decimal; ( -- )
    \ use radix 10 as base for numeric conversions
    0xa #lit, base; !; 
    exit; 
\ numeric input single precision
s" digit?" machineword _digit? digit?; ( c base -- u t )
deflabel dgtq1
    \ convert a character to its numeric value. A flag indicates success.
    >r; 0x30 #lit, \ '0'
    -;
    0x9 #lit, over; <; dgtq1 ??branch;
    7 #lit, -; dup; 0xa #lit, <; or;
dgtq1 .label
    dup; r>; u<; 
    exit;
_numberq s" number?" machineword-predef number?; ( a -- n T | a F )
def3label numq1 numq2 numq3
def3label numq4 numq5 numq6
    \ convert a number string to integer. Push a flag on tos.
    base@; >r;
    0lit,
    over; count; 
    over; c@;
    0x24 #lit, \ '$'
    =; numq1 ??branch;
    hex; swap;
    1+,,
    swap;
    1-,,
numq1 .label
    over; c@;
    0x2d #lit, =;
    >r; swap; r@; -;
    swap; r@; +;
    ?dup; numq6 ??branch;
    1-,,
    >r;
numq2 .label
    dup;
    >r; c@;
    base@;
    digit?; numq4 ??branch;
    swap;
    base@;
    *; +; r>;
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
s" ?key" machineword _qkey ?key; ( -- c T | F )
    \ return input character and true, or a false if no input
    '?key; 
    @execute;
    exit;
s" key" machineword _key key; ( -- c )
    \ wait for and return an input character
deflabel-here key1
    ?key; key1 ??branch;
    exit;
s" emit" machineword _emit emit; ( c -- )
    \ send a character to the output device
    'emit;
    @execute;
    exit;
: emit#; ( value -- ) #lit, emit; ;
s" pace" machineword _pace pace; ( -- )
    \ send a pace character for the file downloading process.
    decimal 11 #lit,
    emit;
    exit;
s" space" machineword _space space; ( -- )
    \ send the blank character to the output device
    bl;
    emit;
    exit;
s" spaces" machineword _spaces spaces; ( +n -- ) 
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
s" type" machineword _type type; ( b u -- )
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
s" cr" machineword _cr cr; ( -- )
    \ output a carriage return and a line feed.
    ccr emit#;
    clf emit#;
    exit;
s" do$" word/compile machineword-base _dostr do$; ( -- a )
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
s\" $\"|" word/compile machineword-base _stqp stqp; ( -- a )
    \ runtime routine compiled by $". Return address of a compiled string.
    do$; 
    exit;
s\" .\"|" word/compile machineword-base _dtqp dtqp; ( -- )
    \ runtime routine of ." . output a compile string.
    do$;
    count;
    type;
    exit;
s" .r" machineword _dotr .r; ( n +n -- )
	\ display an integer in a field of n columsn, right justified
	>r; str; r>;
	over;
	-;
	spaces;
	type;
	exit;
s" u.r" machineword _udotr u.r; ( u +n -- )
	\ display an unsigned integer in n column, right justified
	>r; <#; #s; #>; r>;
	over; -;
	spaces;
	type;
	exit;
s" u." machineword _udot u.; ( u -- ) 
	\ display an unsigned integer in free format
	<#; #s; #>;
	spaces;
	type;
	exit;
s" ." machineword _dot .; ( w -- )
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
s" ?" machineword _quest ?; ( a -- )
	\ display the contents in a memory cell
	@;
	.;
	exit;
\ parsing words
s" parse" machineword _parse0 parse0; ( b u c -- b u delta ; <string> )
def3label parse2 parse3 parse5 
def3label parse6 parse7 parse8
	\ scan string delimited by c. Return found string and its offset
	temp; !;
	over;
	>r;
	dup;
	parse8 ??branch; 
	xsp xtop pop,
	xtop 1-,
	xtop xsp push,
	temp; @; bl; 
    =; parse3 ??branch; 
	>r;
deflabel-here parse1
	bl; over; c@; 	\ skip leading blanks only
	-; 
    0<; not; parse2 ??branch; 
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
	temp; @; bl; 
    =; parse5 ??branch; 
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
s" parse" machineword _parse parse; ( c -- b u ; <string> )
	\ scan input stream and return counted string delimited by c.
	>r; tib; inn; @; +; \ current input buffer pointer
	#tib; @; inn; @; -; 	\ remaining count
	r>; parse0; inn; +!;
    exit;
s" .(" immediate-machineword _dotpr .(; ( -- )
	\ output following string up to next ) .
	0x29 #lit, \ )
	parse; type;
	exit;
s" (" immediate-machineword _paren (; ( -- )
	\ ignore following string up to next ). A comment
	0x29 #lit, \ )
	parse; 2drop;
	exit;
s" \" immediate-machineword _backslash \; ( -- )
	\ ignore following text till the end of line
	#tib; @; inn; !;
	exit;
s" char" machineword _char char; ( -- c )
	\ parse next word and return its first character
	bl; parse; drop; c@;
	exit;
s" token" machineword _token token;
	bl; parse;
	decimal 31 #lit, min; np; @;
	over; -; cell-; pack$;
	exit;
s" word" machineword _word word; ( c -- a ; <string> )
	\ parse a word from input stream and copy it to code dictionary.
	parse; here; pack$;
	exit;
\ dictionary search
s" name>" machineword _namet name>; ( na -- ca )
	\ return a code address given a name address
	cell-; cell-; \ 16-bit cells, 16 bit values.$$$ ???
	@;
	exit;
s" same?" machineword _sameq same?; ( a a u -- a a f \ -0+ )
deflabel same2
	\ compare u cells in two strings. Return 0 if identical
	>r; same2 word,
deflabel-here same1
	over; r@; cells; +; @; \ 32/16 mix-up
	over; r@; cells; +; @; \ 32/16 mix-up
	-; 
    ?dup; same2 ??branch; 
	r>; drop;
	exit;	\ strings not equal
same2 .label
	donext; same1 word,
    0lit,
	exit;	\ strings equal
s" find" machineword _find find; ( a va -- ca na | a F )
def3label find2 find3 find4
def2label find5 find6
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

s" name?" machineword _nameq name?; ( a -- ca na | a F ) 
def3label nameq1 nameq2 nameq3
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
    find;
	?dup; nameq2 ??branch; 
	r>; drop;
	exit;	\ found name
nameq3 .label
	r>; drop; \ name not found
    0lit,
	exit; \ false flag
: 'echo->@exec; ( -- ) 'echo; @execute; ;
s" ^h" machineword _bksp bksp; ( bot eot cur -- bot eot cur )
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
s" tap" machineword _tap tap; ( bot eot cur c -- bot eot cur )
	\ accept and echo the key stroke and bump the cursor.
	dup; 
    'echo->@exec;
	over; c!; 1+,,
	exit;
_ktap s" ktap" machineword-predef ktap; ( bot eot cur c -- bot eot cur )
def2label ktap1 ktap2
	\ Process a key stroke, cr, or backspace
	dup;
    ccr #lit, xor;
	ktap2 ??branch; 
    cbksp #lit, xor;
	ktap1 ??branch; 
	bl; tap;
	exit;
ktap1 .label
    bksp;
	exit;
ktap2 .label
	drop; swap; drop; dup;
	exit;
_accept s" accept" machineword-predef accept; ( b u -- b u )
def3label accept2 accept3 accept4
	\ accept characters to input buffer. Return with actual count
	over; +; over; 
deflabel-here accept1
	2dup; xor;
	accept4 ??branch; 
	key; dup; bl; decimal 127 #lit, 
    within; accept2 ??branch; 
    tap;
	accept3 word,
accept2 .label
    'tap; @execute;
accept3 .label
	accept1 word,
accept4 .label
	drop; over; -;
	exit;
s" except" machineword _except except; ( b u -- )
	\ accept input stream and store count in SPAN.
    'expect; @execute;
    span; !;
	drop;
	exit;
s" query" machineword _query query; ( -- )
	\ accept input stream to terminal input buffer.
	tib; decimal 80 #lit,
    'expect; @execute; #tib; !;
	drop;
    0lit, inn; !;
	exit;
\ error handling
s" catch" machineword _catch catch; ( ca -- 0 | err# )
	\ execute word at ca and setup and error frame for it
	sp@; >r;
	handler; @; >r; \ save error frame
	rp@; handler; !; execute; \ execute
	r>; _handler word; !; \ restore error frame
	r>; drop; 0lit, exit; \ no error
s" throw" machineword _throw throw; ( err# -- err# ) 
	handler; @;
	rp!;  \ restore return stack
	r>; handler; !; \ restore handler frame
	r>; swap; >r; sp!; \ restore data stack
	drop; r>; exit;
s" null$" machineword _nulld null$; ( -- a )
	\ return address of a null string with zero count
	dovariable; 
	0 constant,
	99 constant, \ c
	111 constant, \ o
	121 constant, \ y
	111 constant, \ o
	116 constant, \ t 
	101 constant, \ e
s" abort" machineword _abort abort; ( -- )
	\ reset data stack and jump to quit
    null$;
	throw;

s\" abort\"" word/compile machineword-base _abortq abortq; ( f -- ) 
	\ runtime routine of abort" . Abort with a message.
deflabel abortq1
	abortq1 ??branch; \ text flag
    do$;	
	throw; \ pass error string
abortq1 .label
	do$; drop;
	exit;	\ drop error
\ the text interpreter
_interpret s" $interpret" machineword-predef $interpret; ( a -- )
def2label _interpret1 interpret2
	\ interpret a word. If failed, try to convert it to an integer.
	name?; 
    ?dup; 	\ ?defined
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
_leftbracket s" [" immediate-machineword-predef leftbracket; ( -- )
	\ start the text interpreter
    _interpret ??lit,
	'eval; !;
	exit;
_dotok s" .ok" machineword-predef .ok; ( -- ) 
deflabel dotok1
	\ display ok only while interpreting
	_interpret ??lit,
	'eval; @; 
    =; dotok1 ??branch; 
	dtqp; s" ok" .string,
dotok1 .label
	cr;
	exit;
		
s" ?stack" machineword _qstack ?stack; ( -- )
	\ abort if the data stack underflows
	depth; 0<;
	abortq; s" underflow" .string, 
	exit;

s" eval" machineword _eval eval; ( -- )
	\ interpret the input stream
deflabel-here eval1
deflabel eval2
	token; dup; c@; \ input stream empty
	eval2 ??branch; 
	'eval; @execute; ?stack; \ evaluate input, check stack
	eval1 word,
eval2 .label
	drop; 'prompt; @execute;
	exit;	\ prompt
\ shell
s" preset" machineword _preset preset; ( -- ) 
	\ reset data stack pointer and the terminal input buffer
	sp0; @; sp!;
	input-buffer-start #lit,
    #tib; cell+; !;
	exit;
s" xio" word/compile machineword-base _xio xio; ( a a a -- )
	\ reset the i/o vectors 'expect, 'tap, 'echo, and 'prompt
	\ this seems questionable to me :/
	_accept ??lit, 
	'expect; 2!;
	'echo; 2!; 
	exit;
s" file" machineword _file file; ( -- )
	\ select io vectors for file download
	_pace ??lit,
	_drop ??lit,
	_ktap ??lit,
    xio;
	exit;
s" hand" machineword _hand hand; ( -- )
	\ select io vectors for terminal interface
	_dotok ??lit, 
	\ _drop ??lit, \ don't repeat characters, macos did it already 4/20/97 - cht
				   \ doesn't seem to apply to me since I'm not on mac os - jws 
	_ktap ??lit,
    xio;
	exit;
s" i/o" machineword _i/o i/o; ( -- a )
	\ array to store default io vectors
	dovariable;
	_qrx ??cell,
	_txsto ??cell, \ default io vectors for 32 bit systems 3/16/92
s" console" machineword _console console; ( -- )
	\ initiate terminal interface.
    i/o; 2@;
	'?key; 2!;
    hand;
	exit;
s" quit" machineword _quit quit; ( -- )
def2label quit3 quit4
	\ reset stack pointer and start text interpreter
	rp0; @; rp!; \ reset return stack pointer
deflabel-here quit1
	leftbracket; \ start interpretation
deflabel-here quit2
    query; \ get input
    _eval ??lit, catch; ?dup;			   \ evaluate input
	quit2 ??branch; \ continue till error
	'prompt; @; swap;    \ save input device
    console; null$; over; xor; \ display error message?
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
    preset; \ some cleanup
	quit1 ??, xrp call,
\ compiler routines
s" '" machineword _tick '; ( -- ca )
deflabel tick1
	\ search context vocabularies for the next word in input stream
    token; name?; \ ?defined
	tick1 ??branch; 
	exit;	\ yes, push code address
tick1 .label
	throw;	\ no, error
s" allot" machineword _allot allot; ( n -- )
	\ allocate n words to the code dictionary
    cp; +!;
	exit;	\ adjust code pointer
s" ," machineword _, ,; ( w -- )
	\ compile an integer into the code dictionary
	here; dup; cell+; _cp word, !; !;
	exit; \ adjust code pointer, compile
deflabel _again
_again defalias again; \ this will cause a redef warning from gforth, its fine
s" [compile]" immediate-machineword _bcompile [compile]; ( -- ; <string> )
	\ compile the next immediate word into code dictionary
    '; again;
	exit;

_compile s" compile" word/compile machineword-base-predef compile; ( -- )
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

s" literal" immediate-machineword _literal literal; ( w -- )
	\ compile top of stack to code dictionary as an integer literal
	compile; lit; \ _lit will be compiled into the dictionary
	,;
	exit;
s\" $,\"" machineword _stcq stcq; ( -- )
	\ compile a literal string up to next " .
	\ Is this supposed to be compile time?
	0x22 #lit, \ '"'
	word; \ move string to code dictionary
	count; +; aligned; \ calculate aligned end of string
	cp; !;
	exit; \ adjust the code pointer
s" recurse" machineword _recurse recurse; ( -- )
	\ make the current word available for compilation
	last; @; name>; again;
	exit; \ compile branch instruction

\ structures

s" then" immediate-machineword _then then; ( a -- )
	\ terminate a forward branch structure.
	\ the forward reference deposits a branch instruction. The address
	\ is filled by a.
	here; over; -; \ construct a base address to store to
	\ need to construct a branch instruction
	#call #lit, over; !; \ stash a zero #call 
    1+,, \ go to next address
	!; \ store the address into this cell
	exit;
_again s" again" immediate-machineword-predef again; ( a -- )
	\ resolve a backwards jump and terminate a loop structure
	\ compile a branch instruction, with 'a' in the address field.
	here; -; \ offset from here
	#call #lit, ,; \ stash the call instruction portion
	,; \ stash the address
	exit;
s" for" immediate-machineword _for for; ( -- a )
	\ start a for-next loop structure in a colon definition
	compile; >r; here;
	exit;
s" begin" immediate-machineword _begin begin; ( -- a )
	\ start an infinite or indefinite loop structure
	here;
	exit;
s" next" immediate-machineword _next next; ( a -- )
	\ terminate a for-next loop structure
	compile; donext; again;
	exit;
s" until" immediate-machineword _until until; ( a -- )
	\ terminate a begin-until indefinite loop structure
	compile; ?branch; again;
	exit;
s" if" immediate-machineword _if if; ( -- A )
    \ begin a conditional branch
    compile; ?branch; here; 
    #call #lit, \ call with register zero thus it is a branch
    ,;
    0lit, ,;
    exit;
s" ahead" immediate-machineword _ahead ahead; ( -- A )
    \ compile a forward branch instruction
    here; #call #lit, \ call with register zero thus it is a branch
    ,;
    0lit, ,;
    exit;
s" repeat" immediate-machineword _repeat repeat; ( A a -- ) 
    \ terminate a begin while-repeat indefinite loop
    again; then;
    exit;

s" aft" immediate-machineword _aft aft; ( a -- a A )
    \ Jump to THEN in a FOR-AFT-THEN-NEXT loop the first time through
    drop; ahead; begin; swap;
    exit;

s" else" immediate-machineword _else else; ( A -- A )
    \ start the false clause in an if-else-then structure
    ahead; swap; then;
    exit;
s" while" immediate-machineword _while while; ( a -- A a )
    \ Conditional branch out of a begin-while-repeat loop
    if; swap;
    exit;
s\" abort\"" immediate-machineword _rabortq rabortq; ( -- ; <string> )
    \ conditional abort with an error message.
    compile; abortq; stcq;
    exit;
s\" $\"" immediate-machineword _strq strq; ( -- ; <string> )
    \ compile an inline string literal.
    compile; stqp; stcq;
    exit;
s\" .\"" immediate-machineword _dotq .q; ( -- ; <string> )
    \ compile an inline string literal to be typed out at run time.
    compile; dtqp; stcq;
    exit;
\ name compiler
s" ?unique" machineword _unique ?unique; ( a -- a )
deflabel unique1
    \ display a warning message if the word already exists.
    dup; name?; \ ?name exists
    unique1 ??branch; \ redefinitions are OK
    dtqp; s" redefined " .string, \ but warn the user
    over; count; type;   \ just in case it is not planned
unique1 .label
    drop;
    exit;
s" $,n" machineword _snam sname; ( na -- )
deflabel pnam1
    \ build a new dictionary name using the string at na.
    dup; c@;     \ null input?
    pnam1 ??branch; 
    ?unique; \ redef?
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
 s" $compile" machineword _$compile $compile; ( a -- )
def3label scom1 scom2 scom3
    \ compile next word to code dictionary as a token or literal.
    name?; ?dup; \ ?defined
    scom2 ??branch; 
    @;
    word/immediate #lit, and; \ ?immediate
    scom1 ??branch; 
    execute;
    exit; \ it's immediate, execute
scom1 .label
    again;  \ compile a bl instruction
    exit;   \ its not immediate, compile
scom2 .label
    'number; @execute;  \ try to convert to number
    scom3 ??branch;  \ 
    literal;         
    exit;            \ compile number as integer
scom3 .label
    throw;           \ error

s" overt" machineword _overt overt; ( -- )
    \ link a new word into the current vocabulary
    last; @; current; @; !;
    exit;
s" ;" word/all machineword-base _semis ;; 
    compile; exit;
    leftbracket; overt; 
    exit;

s" ]" machineword _rightbracket rightbracket; ( -- ) 
    \ start compiling the words in the input stream
    _$compile ??lit,
    'eval; !; 
    exit;
s" :" machineword _colon colon; ( -- ; <string> ) 
    \ start a new colon definition using next word as its name
    token;
    sname;
    rightbracket;
    exit;

s" immediate" machineword _immediate immediate; ( -- )
    \ make the last compiled word an immediate word.
    word/immediate #lit,
    last; @; @; or; last; @; !; 
    exit;

\ defining words
s" user" machineword _user user; ( u -- ; <string> )
    \ compile a new user variable.
    token; sname; 
    overt;
    _douser ??lit,
    again;
    ,;
    exit;
s" create" machineword _create create; ( -- ; <string> )
    \ compile a new array entry without allocating code space.
    token; sname; 
    overt;
    dovariable ??lit,
    again;
    exit;
s" variable" machineword _variable variable; ( -- ; <string> )
    create; 0lit, ,; exit;
\ tools 
s" _type" machineword _utype _type; ( b u -- )
    \ display a string. Filter non-printing characters
def2label utype1 utype2
    >r;  \ start count down loop
    utype2 word,    \ skip first pass
utype1 .label
    dup; c@; >char; 
    emit; \ display only printable
    1 #lit,
    +;
utype2 .label
    donext; 
    utype1 word, \ loop till done
    drop;
    exit;
s" dump" machineword _dump dump; ( a u -- )
    \ dump u bytes from a, in a formatted manner
    \ TODO this
    exit;
s" .s" machineword _dots .s; ( ... -- ... )
    \ display the contents of the data stack
    \ todo this
    exit;
s" !csp" machineword stcsp !csp; ( -- )
    \ save stack pointer in CSP for erro rchecking.
    sp@; csp; !;
    exit;
s" ?csp" machineword qcsp ?csp; ( -- )
    sp@;
    csp; @;
    xor; \ compare pointers
    abortq; s" stacks" .string, \ abort if different
    exit;
s" >name" machineword _tname >name; ( ca -- na | F )
    \ convert code address to a name address.
    \ todo finish
    exit;
s" .id" machineword _dotid .id; ( na -- ) 
    \ display the name at address
    \ todo this
    exit;
s" @a" machineword _ata @a; ( a -- n | ca )
    \ fetch contents. If it is a branch instruction, convert to ca
    \ todo this
    exit;
s" see" machineword _see see; ( -- ; <string> )
def3label see2 see3 see4
    ';
    cell+;
    decimal 19 #lit,
    >r;
deflabel-here see1
    cell+;
    dup;
    @a; \ ppc subroutine threading
    dup; \ does it contain a zero**$$$ ?
    see2 ??branch;
    >name;  \ is it a name?
see2 .label
    ?dup; \ name address or zero
    see3 ??branch;
    spaces;
    .id;    \ display name
    see4 word,
see3 .label
    dup; @; u.; \ display number
see4 .label
    donext; \ user control
    see1 word,
    drop;
    exit;

s" words" machineword _words words; ( -- )
deflabel words2
    \ display the names in the context vocabulary.
    context; @; 
deflabel-here words0
    decimal 10 #lit, temp; !;
    cr; 
deflabel-here words1
    @;
    ?dup; \ ? at end of list
    words2 ??branch;
    dup;
    spaces;
    .id;
    cell-;
    temp; @;
    0x1 #lit, -;
    ?dup;
    words0 ??branch;
    temp; !;
    words1 word,
words2 .label
    exit;

    
\ hardware reset
s" ver" machineword _ver version; ( -- n )
    doconstant;
    version-major decimal 256 * version-minor + constant, 

s" hi" machineword _hi hi; ( -- )
    \ display the sign-on message of eForth
    !io;
    cr;
    dtqp; s" ppc eForth v" .string, \ model
    base@;
    hex;
    version;
    <#; digit; digit;
    0x2e #lit, \ '.'
    hold; #s; #>; 
    type; \ format version number
    base; !;
    cr;
    exit;

s" 'boot" machineword _tboot 'boot; 
    dovariable;
    _hi ??cell, 
    
_cold s" cold" machineword-predef cold; ( -- ) 
    \ the high level cold start sequence
deflabel-here cold1
    preset; 'boot; @execute;
    forth; context; @;
    dup; \ initialize search order
    current; 2!;
    overt; quit;
    cold1 ??b,
\ did not include the file operations as we don't normally have access to them
\ always should be last
last-word @ .org
_ctop .label \ a hack to stash the correct address in the user variables
_lastn .label \ hack for now
asm}

bye
