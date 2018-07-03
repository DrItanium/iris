include iris.fs
: x.scr ( -- ) hex .s decimal cr ;
\ contains all of the registers and pieces used for the monitor itself
\ the monitor is now also the forth system itself
s" figforth.o" {asm
: too-many-vars-defined ( addr -- ) 0x40 >= ABORT" To many registers used!" ;
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


\ ascii characters used
0x8 constant cbksp \ backspace
0x0a constant clf \ line feed
0x0d constant ccr \ carriage return

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
words/block words/sector * constant sectors/block
1 constant num-buffers
words/sector constant keyboard-buffer
keyboard-buffer 4 + constant co

\ bootstrap-end constant dictionary-start

\ register reservations
deflabel forth1
deflabel _origin
deflabel _error
deflabel forth_vocabulary_start
deflabel _cold
deflabel _abort
deflabel _quit
deflabel _interpret
deflabel _message \ message routine
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
deflabel _eprint
deflabel _up \ user pointer
deflabel _rpp \ return stack pointer
deflabel _(;code)
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
: dp; ( -- ) &DP word, ;
: voc-link; ( -- ) &voc-link word, ;
: blk; ( -- ) &blk word, ;
: inn; ( -- ) &IN word, ;
: out; ( -- ) &OUT word, ;
: scr; ( -- ) &scr word, ;
: offset; ( -- ) &offset word, ;
: context; ( -- ) &context word, ;
: current; ( -- ) &current word, ;
: error; ( -- ) _error word, ;
: cold; ( -- ) _cold word, ;
: abort; ( -- ) _abort word, ;
: quit; ( -- ) _quit word, ;
: interpret; ( -- ) _interpret word, ;
: message; ( -- ) _message word, ;
: sp0; ( -- ) &s0 word, ;
: r0; ( -- ) &r0 word, ;
: tib; ( -- ) &tib word, ;
: warn; ( -- ) &WARNING word, ;
: fence; ( -- ) &fence word, ;
: state; ( -- ) &state word, ;
: dpl; ( -- ) &dpl word, ;
: fld; ( -- ) &fld word, ;
: csp; ( -- ) &csp word, ;
: r#; ( -- ) &r# word, ;
: hld; ( -- ) &hld word, ;
: separator; ( -- ) &separator word, ;
: terminator; ( -- ) &terminator word, ;
: base; ( -- ) &base word, ;
: width; ( -- ) &width word, ;
: (;code); ( -- ) _(;code) word, ;
: ?exec; ( -- ) _?exec word, ;
: ?csp; ( -- ) _?csp word, ;
: !csp; ( -- ) _!csp word, ;
: create; ( -- ) _create word, ;
: leftbracket; ( -- ) _leftbracket word, ;
: rightbracket; ( -- ) _rightbracket word, ;
: compile; ( -- ) _compile word, ;
: smudge; ( -- ) _smudge word, ;
: dovariable; ( -- ) _dovariable word, ;
: ,; ( -- ) _, word, ;

: lit, ( n t -- ) xsp pushi, ;
: #lit, ( n -- ) #, lit, ;
: ??lit, ( n -- ) ??, lit, ;
: 0lit, ( -- ) zero xsp push, ;
: 1lit, ( -- ) 0x1 #lit, ;

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
0x1000 .org \ dictionary starts at 0x1000
deflabel-here _2push 
    xlower xsp push,
deflabel-here _1push 
    xtop xsp push,
deflabel-here _next
    xrp ret,
: .skip ( -- ) 0 #, .cell ; 
: next, ( -- ) _next ??, b, ;
: 1push, ( -- ) _1push ??, b, ;
: 2push, ( -- ) _2push ??, b, ;
\ : machine-code-execute ( -- ) loc@ 1+ constant, ;
: machine-code-execute ( -- ) ( do nothing ) ;
: .string, ( addr len -- ) 
    dup constant, \ embed the length as well
    0 \ go from 0 to length
    ?do
      dup i + c@ constant,
    loop
    drop ;
: variable-embed-name ( str length -- ) .string, ;
: fixed-embed-name ( str length -- ) 
  \ fixed length string embedding
  dup constant, \ embed length into its own storage
  swap @ swap \ make sure we load the front string address
  case 
  	1 of 0xFF and constant, .skip .skip .skip endof
    2 of dup 
         0xFF and constant,
         0xFF00 and 8 rshift constant,
         .skip .skip endof
	3 of dup dup 
         0xFF and constant,
         0xFF00 and 8 rshift constant,
         0xFF0000 and 16 rshift constant,
         .skip 
         endof
	swap \ the length must be consumed by endcase :(
	dup dup dup 
    0xFF and constant,
    0xFF00 and 8 rshift constant, 
    0xFF0000 and 16 rshift constant,
    0xFF000000 and 24 rshift constant,
   endcase 
   ;
: embed-name ( str length -- ) 
  \ abstraction because I'm indecisive
  variable-embed-name ;

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
: defword-base ( str length control-bits "name" -- ) 
defword-header 
deflabel-here 
( then define the label to point at here ) 
;
: defword-base-predef ( label str length control-bits -- ) defword-header .label ;
: machineword-base ( str length control-bits "name" -- ) 
  defword-base 
  machine-code-execute 
  ;
: machineword-base-predef ( label str length control-bits -- ) defword-base-predef machine-code-execute ;
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

: 1pop, ( -- )
  xsp xtop pop, ;
: 2pop, ( -- )
  1pop, 
  xsp xlower pop, ;
: 3pop, ( -- )
  2pop,
  xsp xthird pop, ;
: 4pop ( -- )
  3pop,
  xsp xfourth pop, ;
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
: ??branch; ( loc -- ) ??, branch; ;
: zbranch; ( location id -- ) _0branch two-cell-op ;
: ??zbranch; ( location -- ) ??, zbranch; ;
: ?branch; ( -- ) _0branch word, ;
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
: base@; ( -- ) base; @; ;
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
s" drop" machineword _drop 
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
: tqky; ( -- ) _tqky word, ;
s" 'emit" userword _temit
: temit; ( -- ) _temit word, ;
s" 'expect" userword _texpect
s" 'tap" userword _ttap
s" 'echo" userword _techo
s" 'prompt" userword _tprompt
s" base" userword _base
s" tmp" word/compile userword-base _tmp
s" span" userword _span
&in s" >in" userword-predef
s" #tib" userword _ntib
user-offset1+ \ since it is doublewide advance by one again
: #tib; ( -- ) _ntib word, ;
s" csp" userword _csp
s" 'eval" userword _teval
s" 'number" userword _tnumber
s" hld" userword _hld
s" handler" userword _handler
&context s" context" userword-predef \ already advanced one at this point
\ consumes eight cells
user-offset@ 8 + user-offset!
&current s" current" userword-predef \ advance four cells
user-offset@ 4 + user-offset!
s" cp" userword _cp
: cp; ( -- ) _cp word, ;
s" np" userword _np
s" last" userword _last
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
: forth; ( -- ) _forth word, ;
    \ make forth the context vocabulary
    _dovocab word,
forth1 .label
    last-word @ constant, \ vocabulary head pointer
    0 constant, \ vocabulary link pointer
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
	xsp xtaddr move,
	xtaddr xtop ld,
	xtaddr 1+,
	xtaddr xlower ld,
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
    0xFFFF #, xlower set,
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
    within0 ??, xtaddr set,
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
\ TODO continue here
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
    \ filter non-printing characters
deflabel tcha1
    0x7f #lit,
    and;
    dup; \ mask msb
    bl;
    0x7f #lit,
    within; \ check for printable
    not;
    ?branch; 
    tcha1 word,
    drop;
    0x2e #lit, \ replace non printables
tcha1 .label
    exit;
s" depth" machineword _depth ( -- n )
\ return the depth of the data stack
    sp@;
    sp0;
    @;
    swap;
    -;
    cell-;
    1lit,
    /;
    exit;
s" pick" machineword _pick ( ... +n --  ... w )
    \ copy the nth stack item to tos
    1pop, \ top - index
    xsp xtop xlower add, \ make the address to load from
    xlower xtop ld, \ load the address
    1push,
\ todo continue with memory access line 1210
s" +!" machineword _pstore ( n a -- )
    \ add n to the contents at address a
    2pop,
    xtop xthird ld,
    xlower xthird xlower add,
    xlower xtop st,
    next,
s" 2!" machineword _dstore ( d a -- )
    \ store the double integer to address a
    1pop, \ top - 
    xsp wxlower popw,
    wxlower xtop stw,
    next,
s" 2@" machineword _dat ( a -- d )
    \ fetch double integer from address a
    xsp xthird pop,
    xthird wxtop ldw,
    2push,
s" count" machineword _count ( b -- b +n )
    \ return count byte of a string and add 1 to byte address.
    1pop,
    1 #, xtop xlower addi,
    xtop xtop ld,
    2push,
: count; ( -- ) _count word, ;
s" here" machineword _here ( -- a )
    \ return the top of the code dictionary.
    cp;
    @;
    exit;
: here; ( -- ) _here word, ;
s" pad" machineword _pad ( -- a )
    \ return the address of a temporary buffer.
    here;
    decimal 80 #lit,
    +;
    exit;
: pad; ( -- ) _pad word, ;
s" tib" machineword _tib ( -- a )
    \ return the address of the terminal input buffer
    #tib;
    cell+;
    @;
    exit;
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
deflabel fill0
deflabel fill1
    \ fill u words of character c to area beginning at b
    3pop, \ top - character
          \ lower - count
          \ third - address
    xlower cv eqz,
    fill1 ??, cv bc,
fill0 .label
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
    bl;
    over;
    r@;
    +;
    c@;
    <;
    ?branch; 
    dtrail2 word,
    r>;
    1+,,
    exit; \ adjusted count
dtrail2 .label
    _donext word,
    dtrail1 word,
    0lit,
    exit;
: -trailing; ( -- ) _dtrailing word, ;
s" pack$" machineword _pack$ ( b u a -- a )
    \ build a counted string with u characters from b. Null fill
    aligned;
    dup;
    >r; \ strings only on cell boundary
    over;
    dup;
    0lit, \ push zero
    1lit, \ push cell size
    um/mod; 
    drop; \ count mod cell
    -;
    over;
    +;
    0lit, 
    swap;
    !;              \ null fill cell
    dup;
    c!; 
    1+,, \ save count
    swap;
    cmove;
    r>; 
    exit; \ move string
\ numeric output single precision
s" digit" machineword _digit ( u -- c )
    \ convert digit u to a character
    0x9 #lit,
    over;
    <;
    0x7 #lit,
    and;
    +;
    0x30 #lit,
    +;
    exit;
: digit; ( -- ) _digit word, ;
s" extract" machineword _extract
    0lit,
    swap;
    um/mod;
    swap;
    digit;
    exit;
: extract; ( -- ) _extract word, ;
s" <#" machineword _bdgs ( -- )
    \ initiate the numeric output process.
    \ following actions are not necessary because there is no cached top of stack
    \ push the cached TOR to return stack
    \ save IP from lr to TOR
    pad;
    hld; 
    !;
    exit;
: <#; ( -- ) _bdgs word, ;
: 1-,, ( -- ) 
  1lit,
  -; ; 
s" hold" machineword _hold ( c -- )
    \ insert a character into the numeric output string
    hld;
    @;
    1-,,
    dup;
    hld;
    !;
    c!;
    exit;
: hold; ( -- ) _hold word, ;
s" #" machineword _extractDigit ( u -- u )
    \ extract one digit from u and append the digit to output string.
    base;
    @;
    extract;
    hold;
    exit; 
: #; ( -- ) _extractDigit word, ;

s" #s" machineword _exdigs ( u -- 0 )
deflabel digs2
    \ convert u until all digits are added to the output string
deflabel-here digs1
    digit;
    dup;
    ?branch;
    digs2 word,
    digs1 word,
digs2 .label
    exit;
: #s; ( -- ) _exdigs word, ;
s" sign" machineword _sign ( n -- )
    \ add a minus sign to the numeric output string
deflabel sign1
    0<;
    ?branch;
    sign1 word,
    0x2d #lit,
    hold;
sign1 .label
    exit;
: sign; ( -- ) _sign word, ;
s" #>" machineword _edgs ( w --  b u )
    \ prepare the output string to be typed
    drop;
    hld;
    @;
    pad;
    over;
    -;
    exit;
: #>; ( -- ) _edgs word, ;
s" str" machineword _str ( n -- b u )
    \ convert a signed integer to a numeric string
    dup; >r;
    abs; 
    <#; 
    #s;
    r>;
    sign;
    #>;
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
    >r;
    0x30 #lit, \ '0'
    -;
    0x9 #lit, 
    over;
    <;
    ?branch;
    dgtq1 word,
    7 #lit,
    -;
    dup;
    0xa #lit,
    <;
    or;
dgtq1 .label
    dup;
    r>;
    u<; 
    exit;
: digit?; ( -- ) _digit? word, ;
s" number?" machineword _number? ( a -- n T | a F )
deflabel numq1
deflabel numq2
deflabel numq3
deflabel numq4
deflabel numq5
deflabel numq6
    \ convert a number string to integer. Push a flag on tos.
    base@; >r;
    0lit,
    over;
    count;
    over;
    c@;
    0x24 #lit, \ '$'
    =;
    ?branch;
    numq1 word,
    hex;
    swap;
    1+,,
    swap;
    1-,,
numq1 .label
    over; c@;
    0x2d #lit, =;
    >r; swap; r@; -;
    swap; r@; +;
    ?dup;
    ?branch;
    numq6 word,
    1-,,
    >r;
numq2 .label
    dup;
    >r; c@;
    base@;
    digit?;
    ?branch;
    numq4 word,
    swap;
    base@;
    *;
    +;
    r>;
    1+,,
    donext;
    numq2 word, 
    r@;
    swap;
    drop;
    ?branch;
    numq3 word,
    negate;
numq3 .label
    swap;
    numq5 word,
numq4 .label
    r>;
    r>;
    2drop;
    2drop;
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
    tqky; 
    @execute;
    exit;
s" key" machineword _key ( -- c )
    \ wait for and return an input character
deflabel-here key1
    _qkey word,
    ?branch;
    key1 word,
    exit;
s" emit" machineword _emit ( c -- )
    \ send a character to the output device
    temit;
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
    dup;
    c@;
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
s\" .\"|" word/compile machineword-base _dtqp ( -- )
    \ runtime routine of ." . output a compile string.
    dostr;
    count;
    type;
    exit;
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
	digits;
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
	digits;
	#>;
	spaces;
	type;
	exit;
: u.; ( -- ) _udot word, ;
s" ." machineword _dot ( w -- )
deflabel dot1
	\ display an integer in free format, preceeded by a space
	base@;
	0xa #, xsp pushi,
	xor;			 \ ?decimal
	?branch;
	dot1 word,
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
	\ scan string delimited by c. Return found string and its offset
	temp; !;
	over;
	>r;
	dup;
	?branch; parse8 word,
	xsp xtop pop,
	xtop 1-,
	xtop xsp push,
	temp; @; blank; =;
	?branch; parse3 word,
	>r;
deflabel-here parse1
	blank;
	over;
	c@; 	\ skip leading blanks only
	-; 
	0<;
	not;
	?branch; parse2 word,
	xsp xtop pop, 
	xtop 1+,
	xtop xsp push, 
	donext; parse1 word,
	r>;
	drop;
	zero xsp push,
	zero xsp push, \ originally a dup
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
	blank;
	=;
	?branch; parse5 word,
	0<;
parse5 .label
	?branch; parse6 word,
	xsp xtop pop,
	xtop 1+,
	xtop xsp push,
	donext; parse4 word,
	dup; >r;
	parse7 word,
parse6 .label
	r>;
	drop;
	dup;
	xsp xtop pop,
	xtop 1+,
	xtop xsp push,
	>r;
parse7 .label
	over;
	-;
	r>;
	r>;
	-;
	exit;
parse8 .label
	over;
	r>;
	-;
	exit;
s" parse" machineword _parse ( c -- b u ; <string> )
	\ scan input stream and return counted string delimited by c.
	>r;
	tib;
	in; @;
	+; \ current input buffer pointer
	#tib; @;
	in; @;
	-; 	\ remaining count
	r>;
	_parse0 word,
	in; 
	+!;
	exit;
: parse; ( -- ) _parse word, ;
s" .(" word/immediate machineword-base _dotpr ( -- )
	\ output following string up to next ) .
	0x29 #, xsp pushi, \ )
	parse;
	type;
	exit;
s" (" word/immediate machineword-base _paren ( -- )
	\ ignore following string up to next ). A comment
	0x29 #, xsp pushi,	\ )
	parse;
	2drop;
	exit;
s" \" word/immediate machineword-base _backslash ( -- )
	\ ignore following text till the end of line
	#tib; @;
	in; !;
	exit;
s" char" machineword _char ( -- c )
	\ parse next word and return its first character
	blank;
	parse;
	drop;
	c@;
	exit;
s" token" machineword _token
	blank;
	parse;
	decimal 31 #, xsp pushi,
	min;
	np; @;
	over;
	-;
	cell-;
	pack$;
	exit;
s" word" machineword _word ( c -- a ; <string> )
	\ parse a word from input stream and copy it to code dictionary.
	parse;
	here;
	pack$;
	exit;
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
	>r;
	same2 word,
deflabel-here same1
	over;
	r@;
	cells; +; @; \ 32/16 mix-up
	over; r@;
	cells; +; @; \ 32/16 mix-up
	-; ?dup;
	?branch; same2 word, 
	r>;
	drop;
	exit;	\ strings not equal
same2 .label
	donext; same1 word,
	zero xsp push,
	exit;	\ strings equal
: same?; ( -- ) _sameq word, ;
s" find" machineword _find ( a va -- ca na | a F )
deflabel find2
deflabel find3
deflabel find4
deflabel find5
deflabel find6
	\ search a vocabulary for a string. Return ca and na if succeeded
	swap; dup; c@;
	0x1 #, xsp pushi,
	/;
	temp; !;	\ 32/16 bit mix-up
	dup; @; >r;
	cell+;
	swap;
deflabel-here find1
	@; 
	dup; 
	?branch; find6 word,
	dup; @;
	0x1f7f #, xsp pushi,
	and;
	r@;
	xor;
	?branch; find2 word,
	cell- \ backup to link field
	find1 word,	\ try the next word
find2 .label
	cell+;
	temp; @;
	same?;
find3 .label
	find4 word,
find6 .label
	r>;
	drop;
	swap;
	cell-;
	swap;
	exit;
find4 .label
	?branch; find5 word,
	cell-; cell-;
	find1 word,
find5 .label
	r>;
	drop;
	swap;
	drop;
	cell-;
	dup;
	name>;
	swap;
	exit; \ ca

s" name?" machineword _nameq ( a -- ca na | a F ) 
deflabel nameq1
deflabel nameq2
deflabel nameq3
	\ search all context vocabularies for a string.
	context;
	dup;
	d@;
	xor;	\ ?context = also
	?branch; nameq1 word,
	cell-; \ no, start with context
nameq1 .label
	>r;
nameq2 .label
	r>;
	cell+;
	dup;
	>r; 	\ next in search order
	@;
	?dup;
	?branch; nameq3 word,
	_find word,
	?dup;
	?branch; nameq2 word,
	r>;
	drop;
	exit;	\ found name
nameq3 .label
	r>; 
	drop; \ name not found
	zero xsp push,
	exit; \ false flag




asm}

bye
