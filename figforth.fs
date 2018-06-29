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
\ : word, ( v -- ) ??, .cell ;
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
\ : embed-docolon ( -- ) _docolon ??, .cell ;
: embed-docolon ( -- ) ( do nothing ) ;
: colonword-base ( str length control-bits "name" -- ) defword-base embed-docolon ;
: colonword-base-predef ( label str length control-bits -- ) defword-base-predef embed-docolon ;
: colonword ( str length "name"  -- ) word/none colonword-base ;
: colonword-predef ( label str length -- ) word/none colonword-base-predef ;
: embed-doconstant ( -- ) _doconstant ??, xrp bl, ;
: defconstantword-base ( str length control-bits "name" -- ) defword-base embed-doconstant ;
: defconstantword ( n -- ) word/none defconstantword-base ;
: embed-douser ( -- ) 
    _douser ??, xrp bl,
    user-offset@ constant,
    user-offset1+ ;
: userword-base ( str length control-bits "name" -- ) defword-base embed-douser ;
: userword-base-predef ( label str length control-bits -- ) defword-base-predef embed-douser ;
: userword ( n -- ) word/none userword-base ;
: userword-predef ( label n len -- ) word/none userword-base-predef ;
: embed-dovariable ( -- ) _dovariable ??, .cell ;
: defvariableword-base ( str length control-bits "name" -- ) defword-base embed-dovariable ;
: defvariableword-base-predef ( label str length control-bits -- ) defword-base-predef embed-dovariable ;
: defvariableword ( n -- ) word/none defvariableword-base ;
: defvariableword-predef ( label n len -- ) word/none defvariableword-base-predef ;

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
_lit s" lit" machineword-predef
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
s" next" machinword _donext ( -- )
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
    \ todo continue
    next,
_0branch s" 0branch" machineword-predef
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
s" c@" machineword _cat
	1pop, 
	xtop xtop ld,
	0xFF #, xtop xtop andi,
	1push,
s" rp@" machineword _rpat
    \ push xrp onto xsp
    xrp xsp push,
    next,
s" rp!" machineword _rpstore
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
s" >r" machineword _>r 
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
s" 'emit" userword _temit
s" 'expect" userword _texpect
s" 'tap" userword _ttap
s" 'echo" userword _techo
s" 'prompt" userword _tprompt
s" base" userword _base
s" tmp" userword _tmp
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
s" dovoc" machineword _dovocab  ( -- )
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
s" <" defbinaryop _< lt,
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
    0xFFFF #, xsp pushi,
    next,
within0 .label
    zero xsp push,
    next,
: within; ( -- ) _within word, ;
: min; ( -- ) _min word, ;
: max; ( -- ) _max word, ;
: u<; ( -- ) _u< word, ;
: u>; ( -- ) _u> word, ;
: >; ( -- ) _> word, ;
: <; ( -- ) _< word, ;
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
s" bl" machineword _blank ( -- 32 )
    \ return 32, the blank character
    0x20 #, xsp pushi,
    next,
: bl; ( -- ) _blank word, ;
s" >char" machineword _tchr ( c -- c )
    \ filter non-printing characters
deflabel tcha1
    0x7f #, xsp pushi,
    and;
    dup; \ mask msb
    bl;
    0x7f #, xsp pushi,
    within; \ check for printable
    not;
    ?branch; 
    tcha1 word,
    drop;
    0x2e #, xsp pushi,  \ replace non printables
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
    1 #, xsp pushi,
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
s" here" machineword _here ( -- a )
    \ return the top of the code dictionary.
    cp;
    @;
    exit;
: here; ( -- ) _here word, ;
s" pad" machineword _pad ( -- a )
    \ return the address of a temporary buffer.
    here;
    decimal 80 #, xsp pushi,
    +;
    exit;
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
s" -trailing" machineword _dtrailing ( b u -- b u )
deflabel dtrail2
    \ adjust the count ot eliminate trailing white space
    >r;
    dtrail2 word,
deflabel-here dtrail1
dtrail2 .label
    

    
\ s" (loop)" machineword _(loop)
\ 	deflabel loop_1
\ 	\ runtime routine of loop
\ 	xrp xtaddr move,
\ 	xtaddr xtop ld, 
\ 	xtop 1+,
\ 	xtop xtaddr st,
\ 	2 #, xtaddr xtaddr addi,
\ 	xtaddr xlower ld,
\ 	xtop xlower cv ge, 
\ 	loop_1 ??, cv bc,
\ 	xip at0 ld, \ add backwards branch offset to IP and branch back to the DO-LOOP
\ 	at0 xip xip add, 
\ 	next,
\ loop_1 .label
\ 	\ discard the loop parameters off the return stack
\ 	xrp zero pop,
\ 	xrp zero pop,
\ 	2 #, xip xip addi, \ advance ip over the inline offset number and continue
\ 		               \ executing the next word after loop
\ 	next,
\ : (loop); ( location id -- ) _(loop) two-cell-op ;
\ : ??(loop); ( location -- ) ??, (loop); ;
\ s" (+loop)" machineword _(+loop)
\ : (+loop); ( loc id -- ) _(+loop) two-cell-op ;
\ : ??(+loop); ( loc -- ) ??, (+loop); ;
\ deflabel loop_2
\ 	deflabel loop_3
\ 	\ runtime routine at the end of a DO --+loop loop
\ 	1pop,
\ 	xrp xlower ld,
\ 	xtop xlower xlower add,
\ 	xlower xrp st,
\ 	zero xtop cv lt, \ if n is less than zero, jump to loop3 for special processing
\ 	loop_3 ??, cv bc, \ jump to loop3 for special processing
\ 	xrp xtaddr move,
\ 	xtaddr at0 ld, \ loop count
\ 	xtaddr 1+,
\ 	xtaddr at1 ld, \ loop limit
\ 	at0 at1 cv le,
\ 	loop_2 ??, cv bc,
\ 	xip at0 ld,
\ 	at0 xip xip add,
\ 	next,
\ loop_2 .label
\ 	xrp zero pop,
\ 	xrp zero pop,
\ 	2 #, xip xip addi,
\ 	next,
\ loop_3 .label
\ 	xtaddr at0 ld, \ loop count
\ 	xtaddr 1+,
\ 	xtaddr at1 ld, \ loop limit
\ 	at1 at0 cv le, \ negative increment n, reverse comparison
\ 	loop_2 ??, cv bc,
\ 	xip at0 ld,
\ 	at0 xip xip add, \ not yet done with the loop. Return to the word after DO
\ 	next,
\ s" (do)" machineword _(do)
\ : (do); ( -- ) _(do) word, ;
\ 	2pop, 
\ 	xlower xrp push,
\ 	xtop xrp push,
\ 	next,
\ 
\ s" i" machineword _i
\ : i; ( -- ) _i word, ;
\ 	xrp xtop ld, \ load the top loop element
\ 	1push,
\ s" digit" machineword _digit
\ : digit; ( -- ) _digit word, ;
\ deflabel digit1
\ deflabel digit2
\ deflabel digit3
\ deflabel digit4
\     2pop, \ top - numeric base 
\           \ lower - number itself
\     0xFF #, xlower xlower andi, \ get the lower half of the number as all words are 16-bit
\     0x30 #, xlower cv gei,
\     digit1 ??, cv bc,
\     zero xsp push,
\     next,
\ digit1 .label
\     0x30 #, xlower cv neqi, \ at this point it is just greater if this returns true
\     digit2 ??, cv bc, \ goto that place 
\     \ since they are equal just push zero onto the stack
\     zero xsp push,
\     0xFFFF #, xsp pushi,
\     next,
\ digit2 .label
\     0x30 #, xlower xlower subi, \ lower - '0'
\     0x9 #, xlower cv lei, 
\     digit3 ??, cv bc,
\     \ lower > 0x9
\     0x7 #, xlower xlower subi,
\     0xa #, xlower cv gei,
\     digit3 ??, cv bc,
\     \ lower < 0xa
\     zero xsp push,
\     next,
\ digit3 .label
\     xlower xtop cv lt,
\     digit4 ??, cv bc,
\     zero xsp push,
\     next,
\ digit4 .label
\     xlower xsp push,
\     0xFFFF #, xsp pushi,
\ 	next,
\ s" (find)" machineword _(find)
\     \ if we support variable length names then this will be updated at that point
\     xsp pfind,
\ 	next,
\ : (find); ( -- ) _(find) word, ;
\ s" enclose" machineword _enclose
\ 	\ A primitive word to scan the text. From the byte address and the
\ 	\ delimiter c, it determines the byte offset to the first non-delimiter
\ 	\ character, the offset to the first delimiter after the text string, and
\ 	\ the offset to the next character after the delimiter. If the string is
\ 	\ delimited by a nul, the last offset is equal  to the previous offset
\ 	\ dest - stack pointer to grab arguments from
\ 	\ ( addr c -- addr nl n2 n3 )
\ deflabel enclose2
\ deflabel enclose3
\ deflabel enclose4
\     2pop, \ top - terminator 
\           \ lower - address
\           \ third - count
\           \ fourth - temporary
\     xlower xsp push, \ addr back to stack
\     0x00FF #, xtop xtop andi,  \ zero uppper half of the value
\     0xFFFF #, xthird set, \ char offset counter
\     xlower 1-, \ decrement address
\     \ scan to first non terminator character
\ deflabel-here enclose1
\     xlower 1+,  \ increment address
\     xthird 1+,  \ increment count
\     xlower xfourth ld,
\     xtop xfourth cv eq, \ does it equal the terminator?
\     enclose1 ??, cv bc, \ if so keep looking
\     xthird xsp push,
\     zero xfourth cv neq, \ is it a null char?
\     enclose2 ??, cv bc, \ no? then branch
\     \ found null before first non-terminator char
\     xthird xtop move, \ copy the counter
\     1 #, xthird xlower addi, \ increment the count by 1
\     2push, \ get out of here
\ enclose2 .label
\     \ found first text char, count the characters
\     xlower 1+,
\     xthird 1+, 
\     xlower xfourth ld,
\     xfourth xtop cv eq,
\     enclose4 ??, cv bc,
\     zero xfourth cv neq,
\     enclose2 ??, cv bc,
\ enclose3 .label
\     xthird xtop move,
\     xthird xlower move,
\     2push,
\ enclose4 .label
\     xthird xtop move,
\     xtop 1+,
\     xthird xlower move,
\     2push,
\ : enclose; ( -- ) _enclose word, ;
\ s" emit" machineword _emit 
\ 	/dev/console0 #, xlower set,
\ 	1pop,
\ 	xtop xlower st,
\ 	next,
\ : emit; ( -- ) _emit word, ;
\ s" key" machineword _key 
\ 	/dev/console0 #, xlower set,
\ 	xlower xtop ld,
\ 	1push,
\ s" ?terminal" machineword _?terminal
\ : ?terminal; ( -- ) _?terminal word, ;
\ \ todo implement
\ next,
\ s" cr" machineword _cr
\ : cr; ( -- ) _cr word, ;
\     /dev/console0 #, xlower set,
\     0xA #, xtop set,
\     xtop xlower st,
\     next,
\ s" cmove" machineword _cmove ( from to u -- ) \ move u bytes in memory 
\     3pop, \ top - u
\          \ lower - to
\          \ third - from
\     deflabel _cmove_loop 
\     deflabel _cmove_done
\ _cmove_loop .label
\     xtop cv ltz,
\     _cmove_done ??, cv bc,
\     xlower at0 ldtincr,
\     at0 xthird sttincr,
\     xtop 1-,
\     xlower 1+,
\     xthird 1+,
\     _cmove_loop ??, b,
\ _cmove_done .label
\     next,
\ : cmove; ( -- ) _cmove word, ;
\ s" u/" defbinaryop _u/ udiv,
\ : u/; ( -- ) _u/ word, ;
\ 
\ s" ;s" machineword _;s
\ 	\ return execution to the calling definition. Unnest one level.
\     next,
\ : ;;s ( -- )
\   \ embed the semicolons routine
\ _;s word, ;
\ s" leave" machineword _leave
\ : leave; ( -- ) _leave word, ;
\ 	\ make the loop limit equal to the loop count and force the loop to
\ 	\ terminate at loop or +loop
\ 	\ copy loop count to loop limit on return stack
\ 	xrp xtop pop,
\ 	xrp zero pop,
\ 	xtop xrp push,
\ 	xtop xrp push, 
\ 	next,
\ : 0=, ( -- ) 
\   1pop,
\   xtop xtop eqz,
\   xtop xsp push, ;
\ s" 0=" machineword _0=
\     1pop,
\     xtop xtop eqz,
\ 	1push,
\ : 0=; ( -- ) _0= word, ;
\ s" minus" machineword _minus
\     deflabel _minusDone
\     1pop,
\     0xFFFF #, xtop xtop muli, \ multiply by negative one
\ 	1push,
\ s" d-" machineword _dminus
\     ( xl xh yl yh -- )
\     \ iris takes in a front register which is the lower half
\     \ because of this we have to pop registers differently
\     \ not in the order found on the stack
\     \ top - yl
\     \ lower - yh
\     \ third - xl
\     \ fourth - xh
\     \ actual input from the stack
\     ( xl xh yl yh -- sl sh )
\     xsp xtop popw, \ xtop then lower
\     xsp xthird popw, \ third then fourth
\     xtop xthird xtop subw, \ result will be in xtop,xlower
\     xtop xsp pushw, \ push xtop then xlower
\     next,
\ : d-; ( -- ) _dminus word, ;
\ : +!, ( -- )
\     2pop, \ top - addr 
\          \ lower - n
\     xtop at0 ld,
\     xlower at0 xlower add, 
\     xlower xtop st, ;
\ s" +!" machineword _+!
\ 	+!,
\     next,
\ : +!; ( -- ) _+! word, ;
\ s" toggle" machineword _toggle ( p addr -- )
\ 	2pop,  \ top - addr
\ 		  \ lower - pattern
\ 	xtop xthird ld,
\ 	xthird xlower xthird xor,
\ 	xtop xthird st, 
\ 	next,
\ : toggle; ( -- ) _toggle word, ;
\ : c@; ( -- ) _cat word, ;
\ s" 2@" machineword _2@ 
\    1pop,
\    xtop xlower ld, \ lower word
\    xtop 1+,
\    xtop xtop ld, \ upper word
\    2push,
\ : 2@; ( -- ) _2@ word, ;
\ : c!; ( -- ) _cstore word, ;
\ s" 2!" machineword _2! 
\     3pop, \ top addr
\           \ lower data high
\           \ third data low
\     xthird xtop st, \ low
\     xtop 1+,
\     xlower xtop st, \ high
\     next,
\ : 2!; ( -- ) _2! word, ;
\ s" :" word/imm machineword-base _colon
\     ?exec;
\     !csp;
\     current; @;
\     context; !;
\     create;
\     rightbracket;
\     (;code);
\ _docolon .label
\     xw 1+,
\ 	\ runtime routine for all colon definitions
\     xip xrp psh-> \ push the address of the next word to the return stack and enter a lower nesting level
\     xw xip -> \ move the parameter field address into IP, pointing to the first word in this definition
\     next,
\ s" ;" colonword _semi
\     ?csp;
\     compile;
\     ;;s
\     smudge;
\     leftbracket;
\     ;;s
\ 
\ 
\ s" noop" machineword _noop next,
\ : noop; ( -- ) _noop word, ;
\ s" constant" colonword _constant
\ : constant; ( -- ) _constant word, ;
\     create;
\     smudge;
\     ,;
\     (;code);
\ _doconstant .label
\     xrp xlower ld,
\     xlower 1+,
\     xlower xtop ld,
\     xlower 1+,
\     xlower xrp st,
\     1push,
\ s" variable" colonword _variable
\ : variable; ( -- ) _variable word, ;
\     constant;
\     (;code);
\ _dovariable .label
\     xrp xlower ld,
\     0x1 #, xlower xtop addi,
\     0x2 #, xlower xlower addi,
\     xlower xrp st,
\     1push,
\ s" 0" defconstantword _0 0x0 constant, 
\ : 0; ( -- ) _0 word, ;
\ s" 1" defconstantword _1 0x1 constant,
\ : 1; ( -- ) _1 word, ;
\ s" 2" defconstantword _2 0x2 constant,
\ : 2; ( -- ) _2 word, ;
\ s" 3" defconstantword _3 0x3 constant,
\ : 3; ( -- ) _3 word, ;
\ s" c/l" defconstantword _c/l 0x40 constant,
\ : c/l; ( -- ) _c/l word, ;
\ s" first" defconstantword _first &FIRST constant,
\ : first; ( -- ) _first word, ;
\ s" limit" defconstantword _limit &LIMIT constant,
\ : limit; ( -- ) _limit word, ;
\ s" b/buf" defconstantword _b/buf b/buf constant,
\ : b/buf; ( -- ) _b/buf word, ;
\ s" b/scr" defconstantword _b/scr b/scr constant,
\ : b/scr; ( -- ) _b/scr word, ;
\ s" +origin" colonword _+origin
\     lit;
\     _origin ??plit;
\     +;
\     ;;s
\ &tib s" tib" userword-predef 0x8 constant,
\ &width s" width" userword-predef 0x9 constant,
\ &warning s" warning" userword-predef 0xa constant,
\ &fence s" fence" userword-predef 0xb constant,
\ &dp s" dp" userword-predef 0xc constant,
\ &voc-link s" voc-link" userword-predef 0xd constant,
\ &blk s" blk" userword-predef 0xe constant,
\ &in s" in" userword-predef 0xf constant,
\ &out s" out" userword-predef 0x10 constant,
\ &scr s" scr" userword-predef 0x11 constant,
\ &offset s" offset" userword-predef 0x12 constant,
\ &context s" context" userword-predef 0x13 constant,
\ &current s" current" userword-predef 0x14 constant,
\ &state s" state" userword-predef 0x15 constant,
\ &base s" base" userword-predef 0x16 constant,
\ &dpl s" dpl" userword-predef 0x17 constant,
\ &fld s" fld" userword-predef 0x18 constant,
\ &csp s" csp" userword-predef 0x19 constant,
\ &r# s" r#" userword-predef 0x1a constant,
\ &hld s" hld" userword-predef 0x1b constant,
\ \ TODO more user variables
\ s" 1+" machineword _1+
\ : 1+; ( -- ) _1+ word, ;
\ 	1pop,
\ 	xtop 1+,
\ 	1push,
\ s" 2+" machineword _2+
\ : 2+; ( -- ) _2+ word, ;
\ 	1pop,
\ 	2 #, xtop xtop addi,
\ 	1push,
\ s" here" machineword _here
\ 	&DP ??, xtaddr set,
\ 	xtaddr xtop ld,
\ 	1push,
\ : here; ( -- ) _here word, ;
\ s" allot" machineword _allot ( n -- )
\ 	1pop,
\ 	&DP ??, xtaddr set,
\ 	xtaddr xlower ld, 
\     xlower xtop xlower add,
\ 	xlower xtaddr st, 
\     next,
\ : allot; ( -- ) _allot word, ;
\ _, s" ," machineword-predef ( n -- )
\     \ store n into the next available cell above dictionary and advance DP by 2 thus
\     \ compiling into the dictionary
\ 	1pop, 
\ 	&DP ??, xtaddr set,
\ 	xtaddr xlower ld, 
\     xtop xlower st, \ save it to the current dict pointer front
\     xlower 1+, \ move ahead by one
\ 	xlower xtaddr st,
\     next,
\ s" c," machineword _c,
\ 	1pop,
\     0xFF #, xtop xtop andi, 
\ 	&DP ??, xtaddr set,
\ 	xtaddr xlower ld, 
\     xtop xlower st, \ save it to the current dict pointer front
\     xlower 1+, \ move ahead by one
\ 	xlower xtaddr st,
\     next,
\ : c,; ( -- ) _c, word, ;
\ s" space" machineword _space
\ : space; ( -- ) _space word, ;
\     /dev/console0 #, xlower set,
\     0x20 #, xtop set,
\     xtop xlower st,
\     next,
\ s" -dup" machineword _-dup \ duplicate if non zero
\ : -dup; ( -- ) _-dup word, ;
\     deflabel _-dup_done
\     1pop, 
\     xtop cv eqz, 
\     _-dup_done ??, cv bc,
\         xtop xsp push,
\ _-dup_done .label
\ 	1push,
\ s" latest" machineword _latest \ leave the name field address of the last word defined in the current vocabulary
\ 	&current ??, xtaddr set,
\ 	xtaddr xtop ld,
\ 	xtop xtop ld,
\ 	1push, 
\ : latest; ( -- ) _latest word, ;
\ 
\ s" lfa" machineword _lfa \ convert the parameter field address to link field address
\ 	1pop,
\ 	2 #, xtop xtop subi, 
\ 	1push,
\ : lfa; ( -- ) _lfa word, ;
\ s" cfa" machineword _cfa \ convert the parameter field address to code field address
\ 	1pop,
\ 	1 #, xtop xtop subi,
\ 	1push,
\ : cfa; ( -- ) _cfa word, ;
\ s" nfa" machineword _nfa \ convert the parameter field address to name field address
\     1pop,
\     7 #, xtop xtop subi,
\     1push,
\ : nfa; ( -- ) _nfa word, ;
\ s" pfa" machineword _pfa \ convert the name field address to its corresponding pfa
\     1pop,
\     8 #, xtop xtop addi,
\     1push,
\ : pfa; ( -- ) _pfa word, ;
\ _!csp s" !csp" machineword-predef
\     xsp xtop move,
\     &csp ??, xlower set,
\     xtop xlower st,
\     next,
\ 
\ s" ?error" colonword _?error
\ : ?error; ( -- ) _?error word, ;
\ deflabel _?err1
\ deflabel _?err2
\     swap;
\     _?err1 ??zbranch; \ if 
\     error;
\     _?err2 ??branch; \ else
\ _?err1 .label
\     drop;
\ _?err2 .label
\     ;;s
\ 
\ 
\ 
\ s" ?comp" colonword _?comp 
\ : ?comp; ( -- ) _?comp word, ;
\     state; @;
\     0=;
\     0x11 #plit;
\     ?error;
\     ;;s
\ _?exec s" ?exec" colonword-predef
\     state; @;
\     0x12 #plit;
\     ?error;
\     ;;s
\ s" ?pairs" colonword _?pairs
\ : ?pairs; ( -- ) _?pairs word, ;
\     -;
\     0x13 #plit;
\     ?error;
\     ;;s
\ _?csp s" ?csp" colonword-predef 
\     sp@; @;
\     -;
\     0x14 #plit;
\     ?error;
\     ;;s
\ s" ?loading" colonword _?loading
\     blk; @;
\     0=;
\     0x16 #plit;
\     ?error;
\     ;;s
\ : ?loading; ( -- ) _?loading word, ;
\ _compile s" compile" colonword-predef
\     ?comp;
\     r>;
\     dup; 
\     2+;
\     >r; @;
\     ,;
\     ;;s
\ _smudge s" smudge" machineword-predef
\ 	\ mark the current dictionary entry as smudge
\ 	&dp ??, xtaddr set,
\ 	xtaddr xtop ld,
\ 	xtop xlower ld,
\ 	word/smudge #, at0 set,
\ 	xlower at0 xlower or,
\ 	xlower xtop st,
\ 	next,
\ 
\ _(;code) s" (;code)" colonword-predef
\     r>;
\     latest;
\     pfa;
\     cfa; @;
\     ;;s
\ s" ;code" colonword _;code
\     ?csp;
\     compile;
\     (;code); 
\     leftbracket;
\     noop;
\     ;;s
\ s" <builds" colonword _<builds 0; constant; ;;s
\ : <builds; ( -- ) _<builds word, ;
\ s" does>" colonword _does>
\ : does>; ( -- ) _does> word, ;
\     deflabel _dodoes
\     r>;
\     latest;
\     pfa; @;
\     (;code);
\ _dodoes .label 
\     xip xrp push,
\     xw 1+,      \ pfa
\     xw xtop move, 
\     xtop xip ld, \ new cfa
\     xw 1+, 
\     xw xsp push, \ pfa
\     next,
\ s" count" colonword _count dup; 1+; swap; c@; ;;s
\ : count; ( -- ) _count word, ;
\ s" type" colonword _type
\ : type; ( -- ) _type word, ;
\ deflabel _type1
\ deflabel _type2
\ deflabel _type3
\     2dup;
\     _type1 ??zbranch;
\     over;
\     +;
\     swap;
\     (do); \ do
\ _type2 .label
\     i;
\     c@;
\     emit;
\     _type2 ??(loop); \ loop
\  
\     _type3 ??branch; \ else
\ _type1 .label
\     drop; \ endif
\ _type3 .label
\     ;;s
\ s\" (.\")" 
\ colonword _pdotq
\     r@; 
\     count;
\     dup;
\     1+; 
\     r>;
\     +;
\     >r;
\     type;
\     ;;s
\ : pdotq; ( -- ) _pdotq word, ;
\ s\" .\"" 
\ colonword _dotq
\ \ todo dotq body
\     ;;s
\ : dotq; ( -- ) _dotq word, ;
\ s" !=" defbinaryop _!= neq,
\ : !=; ( -- ) _!= word, ;
\ s" >=" defbinaryop _>= ge,
\ : >=; ( -- ) _>= word, ;
\ s" <=" defbinaryop _<= le,
\ : <=; ( -- ) _<= word, ;
\ \ s" nand" defbinaryop _nand nand,
\ \ : nand; ( -- ) _nand word, ;
\ \ s" nor" defbinaryop _nor nor,
\ \ : nor; ( -- ) _nor word, ;
\ s" lshift" defbinaryop _lshift lshift,
\ : lshift; ( -- ) _lshift word, ;
\ s" rshift" defbinaryop _rshift rshift,
\ : rshift; ( -- ) _rshift word, ;
\ 
\ s" 1-" machineword _1-
\ : 1-; ( -- ) _1- word, ;
\ 	1pop,
\ 	xtop 1-,
\ 	1push,
\ 
\ s" ." machineword _.
\ : .; ( -- ) _. word, ;
\    1pop,
\    /dev/console2 #, xlower set,
\    xtop xlower st,
\    next,
\ 
\ s" ?" machineword _?
\ : ?; ( -- ) _? word, ;
\     1pop, 
\     xtop xtop ld,
\     /dev/console0 #, xlower set,
\     xtop xlower st,
\     next,
\ 
\ _leftbracket s" [" word/imm machineword-base-predef
\ 	&state ??, xtaddr set,
\ 	zero xtaddr st,
\     next,
\ _rightbracket s" ]" machineword-predef
\ 	&state ??, xtaddr set,
\     0xFFFF #, xtop set,
\ 	xtop xtaddr st,
\     next,
\ 
\ s" hex" machineword _hex \ set the base to 16
\ : hex; ( -- ) _hex word, ;
\ 	&base ??, xtaddr set,
\ 	0x10 #, at0 set,
\ 	at0 xtaddr st,
\ 	next,
\ s" decimal" machineword _decimal \ set the base to 10
\ : decimal; ( -- ) _decimal word, ;
\ 	&base ??, xtaddr set,
\ 	0xA #, at0 set,
\ 	at0 xtaddr st,
\ 	next,
\ s" octal" machineword _octal \ set the base to 8
\ : octal; ( -- ) _octal word, ;
\ 	&base ??, xtaddr set,
\ 	0x8 #, at0 set,
\ 	at0 xtaddr st,
\ 	next,
\ s" immediate" machineword _immediate
\ : immediate; ( -- ) _immediate word, ;
\ 	\ mark the current dictionary entry as immediate
\ 	&dp ??, xtaddr set,
\ 	xtaddr xtop ld,
\ 	xtop xlower ld,
\ 	word/imm #, at0 set,
\ 	xlower at0 xlower or,
\ 	xlower xtop st,
\ 	next,
\ 	
\ s" 2swap" machineword _2swap ( a b c d -- c d a b ) 
\ : 2swap; ( -- ) _2swap word, ;
\ 	3pop, \ d - top
\ 		 \ c - lower
\ 		 \ b - third
\ 	xsp xfourth pop, \ a - fourth
\ 	xlower xsp push,
\     xtop xsp push,
\ 	xfourth xsp push,
\ 	xthird xsp push,
\ 	next,
\ : assign-variable, ( value type address type -- )
\   xtaddr set,
\   xtop set,
\   xtop xtaddr st, ;
\ : load-variable, ( var type reg -- )
\ 	>r
\ 	xtaddr set,
\ 	r> 
\ 	xtaddr swap ld, ;
\ : zero-variable, ( address type -- ) 2>r 0 #, 2r> assign-variable, ;
\ 
\ s" expect" colonword _expect
\ : expect; ( -- ) _expect word, ;
\     \ todo implement
\     ;;s
\ s" query" colonword _query
\ : query; ( -- ) _query word, ;
\     tib; @;
\     0x50 #plit;
\     expect;
\     0;
\     inn; !;
\     ;;s
\ s" " colonword _null
\ : null; ( -- ) _null word, ;
\ deflabel _null1
\ deflabel _null2
\ deflabel _null3
\     blk; @;
\     _null1 ??zbranch; \ if
\     1;
\     blk;
\     +!;
\     0;
\     inn; !;
\     blk; @;
\     b/scr;
\     1;
\     -;
\     and;
\     0=; 
\     _null2 ??zbranch; \ if
\     ?exec;
\     r>;
\     drop; \ endif
\ _null2 .label   \ else
\     _null3 ??branch;
\     r>;
\ _null1 .label
\     drop; \ endif
\ _null3 .label
\     ;;s
\ 
\ s" fill" machineword _fill ( addr n b -- ) 
\ : fill; ( -- ) _fill word, ;
\ 	\ fill u bytes in memory with b beginning at address
\     3pop, \ top - b
\           \ lower - u
\           \ third - addr
\     deflabel _fill_loop
\     deflabel _fill_done
\ _fill_loop .label
\     xlower cv eqz,
\     _fill_done ??, cv bc,
\     xtop xthird sttincr,
\     xlower 1-,
\     _fill_loop ??, b,
\ _fill_done .label
\     next,
\ s" erase" colonword _erase
\ : erase; ( -- ) _erase word, ;
\     0;
\     fill;
\     ;;s
\ s" blanks" colonword _blanks
\ : blanks; ( -- ) _blanks word, ;
\     bl;
\     fill;
\     ;;s
\ s" hold" colonword _hold
\ : hold; ( -- ) _hold word, ;
\     -1 #plit;
\     hld;
\     +!;
\     hld; @;
\     c@;
\     ;;s
\ 
\ s" pad" colonword _pad ( -- n )
\ : pad; ( -- ) _pad word, ;
\     here;
\     0x44 #plit;
\     +;
\     ;;s
\ deflabel _block
\ : block; ( -- ) _block word, ;
\ s" word" colonword _word
\ : word; ( -- ) _word word, ;
\ deflabel _word1
\ deflabel _word2
\     blk; @;
\     _word1 ??zbranch;
\     blk; @;
\     block;
\     _word2 ??branch;
\ _word1 .label
\     tib; @; \ endif
\ _word2 .label
\     inn; @;
\     +;
\     swap;
\     enclose;
\     here;
\     0x22 #plit;
\     blanks;
\     inn;
\     +!;
\     over; -;
\     >r; r@; here;
\     c!;
\     1+;
\     r>;
\     cmove;
\     ;;s
\ s" (number)" colonword _pnum
\ : (number); ( -- ) _pnum word, ;
\ deflabel-here _pnum1
\ deflabel _pnum2
\ deflabel _pnum3
\     1+; \ begin
\     dup;
\     >r;
\     c@;
\     base; @;
\     digit;
\     _pnum2 ??zbranch; \ while
\     swap;
\     base; @;
\     u*;
\     d+; 
\     dpl; @;
\     1+;
\     _pnum3 ??zbranch; \ if
\     1;
\     dpl;
\     +!; \ endif
\ _pnum3 .label
\     r>;
\     _pnum1 ??branch; \ repeat
\ _pnum2 .label
\     r>;
\     ;;s
\ s" number" colonword _number
\ deflabel _number1
\ deflabel _number2
\ deflabel _number3
\     0; 0; rot;
\     dup; 1+;
\     c@;
\     0x2D #plit;
\     =;
\     dup; >r;
\     +;
\     -1 #plit;
\ _number1 .label 
\     &dpl ??plit; \ begin 
\     !;
\     (number);
\     dup;
\     c@;
\     0x20 #, xsp pushi, 
\     -;
\     _number2 ??zbranch; \ while
\     dup;
\     c@;
\     0x2E #plit;
\     -;
\     0;
\     ?error;
\     0;
\     _number1 ??branch; \ repeat
\ _number2 .label
\     drop;
\     r>;
\     _number3 ??zbranch; \ if
\     d-; \ endif
\ _number3 .label
\     ;;s
\ : number; ( -- ) _number word, ;
\ s" -find" colonword _dfind
\ deflabel _dfind1
\     bl;
\     word;
\     here;
\     (find);
\     dup;
\     0=;
\     _dfind1 ??zbranch; \ if
\     drop;
\     here;
\     latest;
\     (find);
\ _dfind1 .label \ endif
\     ;;s
\ : -find; ( -- ) _dfind word, ;
\ s" (abort)" colonword _pabort
\     abort;
\     ;;s
\ : (abort); ( -- ) _pabort word, ;
\ 
\ _error s" error" colonword-predef
\ deflabel _error1
\ deflabel _error2
\     warn; @;
\     0<;
\     _error1 ??zbranch; \ if
\     (abort); \ endif
\ _error1 .label
\     here;
\     count;
\     type;
\     pdotq;
\     2;
\     \ _questionMarkString word, \ "? "
\     message; 
\     sp!;
\     \ change from the fig model
\     \ inn @ blk @
\     blk; @;
\     2dup;
\     _error2 ??zbranch; \ if
\     inn; @;
\     swap; \ endif
\     _error2 .label
\     quit;
\ 
\ s" id." colonword _iddot
\     pad;
\     0x20 #plit;
\     0x5f #plit;
\     fill;
\     dup;
\     pfa;
\     lfa;
\     over;
\     -;
\     pad;
\     swap;
\     cmove;
\     pad;
\     count;
\     0x1f #plit;
\     and;
\     type;
\     space;
\     ;;s
\ : id.; ( -- ) _iddot word, ;
\ _create s" create" colonword-predef 
\ deflabel _create1
\     -find;
\     _create1 ??zbranch; \ if
\     drop;
\     nfa;
\     id.;
\     4 #plit;
\     message;
\     space; \ endif
\     _create1 .label
\     here;
\     dup;
\     c@;
\     width; @;
\     min;
\     1+;
\     allot;
\     dup;
\     0x0a0 #plit;
\     toggle;
\     here;
\     1;
\     -;
\     0x80 #plit;
\     toggle;
\     latest;
\     ,;
\     current; @; !;
\     here;
\     2+;
\     ,;
\     ;;s
\ s" [compile]" colonword _bcompilep
\ : [compile]; ( -- ) _bcompilep word, ; 
\     -find;
\     0=;
\     0;
\     ?error;
\     drop;
\     cfa;
\     ,;
\     ;;s
\ s" literal" colonword _literal
\ : literal; ( -- ) _literal word, ;
\ deflabel _literal1
\     state; @;
\     _literal1 ??zbranch; \ if
\     compile;
\     lit;
\     ,; \ endif
\     _literal1 .label
\     ;;s
\ s" dliteral" colonword _dliteral
\ : dliteral; ( -- ) _dliteral word, ;
\ \ TODO implement
\     ;;s
\ s" ?stack" colonword _?stack
\ : ?stack; ( -- ) _?stack word, ;
\     sp@;
\     s0; @;
\     swap;
\     u<;
\     1;
\     ?error;
\     sp@;
\     here;
\     0x80 #plit;
\     +;
\     u<;
\     7 #plit;
\     ?error;
\     ;;s
\ _interpret s" interpret" colonword-predef
\ deflabel _interpret2
\ deflabel _interpret3
\ deflabel _interpret4
\ deflabel _interpret5
\ deflabel _interpret6
\ deflabel _interpret7
\ deflabel-here _interpret1
\     -find; \ begin
\     _interpret2 ??zbranch; \ if 
\     state; @;
\     <;
\     _interpret3 ??zbranch; \ if
\     cfa;
\     ,;
\     _interpret4 ??branch; \ else
\ _interpret3 .label 
\     cfa;
\     execute;  \ endif
\ _interpret4 .label
\     ?stack;
\     _interpret5 ??branch; \ else
\ _interpret2 .label
\     here;
\     number;
\     dpl; @;
\     1+;
\     _interpret6 ??zbranch; \ if
\     dliteral;
\     _interpret7 ??branch; \ else
\ _interpret6 .label
\     drop;
\     literal; \ endif
\ _interpret7 .label
\     ?stack; \ endif
\ _interpret5 .label
\     _interpret1 ??branch; \ again
\ \ s" vocabulary" colonword _vocabulary
\ \ deflabel _dovocab
\ \ : vocabulary; ( -- ) _vocabulary word, ;
\ \     <builds;
\ \     literal;
\ \     0xA081 #plit; \ ????
\ \     ,;
\ \     current; @; 
\ \     cfa; ,;
\ \     here;
\ \     voc-link; @; ,;
\ \     voc-link; !;
\ \     does>;
\ \ _dovocab .label
\ \     2+;
\ \     context; !;
\ \     ;;s
\ \ this is a special word that uses dodoes as its interpreter o_O
\ \ s" forth" colonword _forth
\ \ 	\ set the context to the forth base vocabulary
\ \     _dodoes word,
\ \     _dovocab word,
\ \     0xA081 #plit;
\ \     \ TASK - 7 \ cold start value only
\ \     .skip \ end of vocabulary list
\ s" definitions" colonword _definitions
\ : definitions; ( -- ) _definitions word, ;
\   \ used in the form: cccc definitions 
\   \ make cccc vocabulary the current vocabulary.
\   \ new definitions will be added to the cccc vocabulary
\     context; @;
\     current; !;
\     ;;s
\ s" (" colonword _paren
\     0x29 #plit;
\     word;
\     ;;s
\ _quit s" quit" colonword-predef
\ deflabel _quit1
\ deflabel _quit2
\     0; blk; !;
\     leftbracket;
\ _quit1 .label
\     rp!;
\     cr;
\     query;
\     interpret;
\     state; @;
\     0=;
\     _quit2 ??zbranch; \ if
\     pdotq;
\     s" ok" .string,
\     \ endif 
\ _quit2 .label
\     _quit1 ??branch; \ again
\ _abort s" abort" colonword-predef
\     sp!;
\     decimal;
\     ?stack;
\     cr;
\     \ TODO print information out here at some point
\     \ _dotcpu word,
\     pdotq;
\     0xd #plit;
\     \ s" fig-forth" .string,
\     \ version information embedded here too
\     forth;
\     definitions;
\     quit;
\ deflabel _empty-bufs
\ : empty-buffers; ( -- ) _empty-bufs word, ;
\ deflabel _wrm
\ deflabel _wrm1
\ deflabel _warm
\ \ warm start vector comes here
\ _wrm .label
\     _wrm1 ??, xip set,
\     next,
\ _wrm1 .label 
\     _warm ??, .cell
\ _warm s" warm" colonword-predef 
\     empty-buffers;
\     abort;
\ \ cold start vector comes here
\ deflabel _density
\ : density; ( -- ) _density word, ;
\ deflabel _next_use
\ : use; ( -- ) _next_use word, ;
\ deflabel _prev_use
\ : prev; ( -- ) _prev_use word, ;
\ deflabel _dr0
\ : dr0; ( -- ) _dr0 word, ;
\ deflabel _cld
\ deflabel _cld1
\ _cld .label
\     _cld1 ??, xip set,
\     \ setup the data and return stacks
\     data-stack-start #, xsp set,
\     return-stack-start #, xrp set,
\     next,
\ _cld1 .label
\     cold;
\ _cold s" cold" colonword-predef 
\     empty-buffers;
\     0; density; !;
\     first; use; !;
\     first; prev; !;
\     dr0;
\     0 #plit;
\     _eprint ??plit;
\     !;
\     \ orig + 12H 
\     _origin ??plit;
\     0x12 #plit;
\     +;
\     _up ??plit;
\     @;
\     _forth ??plit;
\     0x6 #plit;
\     !;
\     abort; \ last
\ s" s->d" machineword _s->d 
\ : s->d; ( -- ) _s->d word, ;
\ deflabel s2d0
\     1pop,
\     xtop xlower move,
\     zero xtop move,
\     xlower cv gtz,
\     s2d0 ??, cv bc,
\     xtop 1-,
\ s2d0 .label
\     2push,
\ s" +-" colonword _pm
\ : +-; ( -- ) _pm word, ;
\ deflabel _pm1
\     0<;
\     _pm1 ??, zbranch; \ if
\     -; \ endif
\     _pm1 .label
\     ;;s
\ s" d+-" colonword _dpm
\     \ TODO implement
\     ;;s
\ s" dabs" colonword _dabs
\ : dabs; ( -- ) _dabs word, ;
\ \ TODO implement 
\ ;;s
\ s" m/" colonword _mslas
\ : m/; ( -- ) _mslas word, ;
\ \ todo implement m/
\ ;;s
\ 
\ s" (line)" colonword _(line)
\ : (line); ( -- ) _(line) word, ;
\     >r;
\     0x40 #plit;
\     b/buf;
\     */mod;
\     r>;
\     b/scr;
\     *;
\     +;
\     block;
\     +;
\     0x40 #plit;
\     ;;s
\ s" .line" colonword _dline
\ : .line; ( -- ) _dline word, ;
\     (line);
\     -trailing;
\     type;
\     ;;s
\ _message s" message" colonword-predef
\ deflabel mess1
\ deflabel mess2
\ deflabel mess3
\     warn; @;
\     mess1 ??, zbranch; \ if
\     2dup;
\     mess2 ??, zbranch; \ if
\     4 #plit;
\     offset; @;
\     b/scr;
\     /;
\     -;
\     .line;
\     space; \ endif
\ mess2 .label 
\     mess3 ??, branch;
\ mess1 .label
\     pdotq;
\     s" msg # " .string,
\     .;
\ mess3 .label 
\     ;;s
\ s" pc@" machineword _pcload
\     \ todo implement
\     next,
\ s" pc!" machineword _pcstore
\     \ todo implement
\     next,
\ s" p@" machineword _ptat
\     \ todo implement
\     next,
\ s" p!" machineword _pstore
\     \ todo implement
\     next,
\ s" drive" defvariableword _drive
\ : drive; ( -- ) _drive word, ;
\     .skip
\ s" sec" defvariableword _sector_num
\ : sec; ( -- ) _sector_num word, ;
\     .skip 
\ s" track" defvariableword _track#
\ : track; ( -- ) _track# word, ;
\     .skip
\ _next_use s" use" defvariableword-predef 
\     &FIRST constant,
\ _prev_use s" prev" defvariableword-predef
\     &FIRST constant,
\ s" sec/blk" defconstantword _sectors/block
\ : sec/blk; ( -- ) _sectors/block word, ;
\     sectors/block constant,
\ s" #buff" defconstantword _numbuff
\ : #buff; ( -- ) _numbuff word, ;
\     num-buffers constant, 
\ _density s" density" defvariableword-predef
\     .skip
\ s" disk-error" defvariableword _disk_error
\ : disk-error; ( -- ) _disk_error word, ;
\     .skip
\ s" +buf" colonword _pbuf
\ deflabel pbuf1
\ : +buf; ( -- ) _pbuf word, ;
\     co #plit;
\     +; dup;
\     limit; =;
\     pbuf1 ??zbranch;
\     drop; first;
\     pbuf1 .label 
\     dup; prev;
\     @; -;
\     ;;s
\ s" update" colonword _update
\ : update; ( -- ) _update word, ;
\     @; @;
\     0x8000 #plit;
\     or;
\     prev; @;
\     !;
\     ;;s
\ _empty-bufs s" empty-buffers" colonword-predef
\     first;
\     limit; 
\     over;
\     -;
\     erase;
\     ;;s
\ _dr0 s" dr0" colonword-predef
\     0; offset; !;
\     ;;s
\ s" dr1" colonword _dr1
\ : dr1; ( -- ) _dr1 word, ;
\ deflabel dr11
\ deflabel dr12
\     density; @; 
\     dr11 ??zbranch;
\     sectors/double-disk #plit;
\     dr12 ??branch;
\     dr11 .label
\     sectors/disk #plit;
\     dr12 .label
\     offset; @;
\     ;;s
\ \ note: won't work if only using a single buffer
\ deflabel _rslw
\ : r/w; ( -- ) _rslw word, ;
\ s" buffer" colonword _buffer
\ : buffer; ( -- ) _buffer word, ;
\ deflabel buff1
\ deflabel buff2
\     use; @; dup;
\     >r; 
\     buff1 .label
\     +buf;
\     buff1 ??zbranch;
\     use; !;
\     r@; @;
\     0<; 
\     buff2 ??zbranch;
\     r@; 2+;
\     r@; @;
\     0x7FFF #plit;
\     and; 0;
\     r/w;
\     buff2 .label
\     r@; !;
\     r@; prev;
\     !; r>;
\     2+; ;;s
\ _block s" block" colonword-predef
\ deflabel blk1
\ deflabel blk3
\     offset;
\     @; +;
\     >r; prev;
\     @; dup;
\     @; r@;
\     -;
\     dup; +;
\     blk1 ??, zbranch;
\ deflabel-here blk2
\     +buf; 0=;
\     blk3 ??, zbranch;
\     drop; r@;
\     buffer; dup;
\     r@; 1;
\     r/w;
\     2; -;
\ blk3 .label
\     dup; @;
\     r@; -;
\     dup; +;
\     0=;
\     blk2 ??, zbranch;
\     dup; prev;
\     !;
\ blk1 .label
\     r>; drop;
\     2+; ;;s
\ s" set-io" machineword _setio
\ : setio; ( -- ) _setio word, ;
\ \ todo implement
\     next,
\ s" set-drive" machineword _setdrv
\ : setdrive; ( -- ) _setdrv word, ;
\ \ todo implement
\     next,
\ s" t&scalc" machineword _t&scalc
\ : t&scalc; ( -- ) _t&scalc word, ;
\     \ todo implement
\     ;;s
\ s" sec-read" machineword _sector_read
\ : sec-read; ( -- ) _sector_read word, ;
\     \ todo implement
\     next,
\ s" sec-write" machineword _sector_write
\ : sec-write; ( -- ) _sector_write word, ;
\     \ todo implement
\     next,
\ _rslw s" r/w" colonword-predef 
\     use; @;
\     >r;
\     swap; sec/blk;
\     *; rot;
\     use; !;
\     sec/blk; 0;
\     (do);
\ deflabel rslw2
\ deflabel rslw3
\ deflabel-here rslw1
\     over; over;
\     t&scalc; setio;
\     rslw2 ??zbranch;
\     sec-read;
\     rslw3 ??branch;
\ rslw2 .label 
\     sec-write;
\ rslw3 .label
\     1+;
\     0x80 #plit;
\     use; +!;
\     rslw1 ??(loop);
\     drop; drop;
\     r>; use;
\     !; ;;s
\ s" flush" colonword _flush
\ : flush; ( -- ) _flush word, ;
\     #buff; 1+;
\     0; (do);
\     deflabel-here flush1
\     0; buffer;
\     drop;
\     flush1 ??(loop);
\     ;;s
\ s" load" colonword _load
\ : load; ( -- ) _load word, ;
\    blk; @; >r;
\    inn; @; 
\    >r; 0;
\    inn; !;
\    b/scr;  *;
\    blk; !; \ blk <- scr * b/scr
\    interpret; \ interpret from other screen
\    r>; inn; !;
\    r>; blk; !;
\    ;;s
\ s" -->" colonword _arrow
\ : -->; ( -- ) _arrow word, ;
\     ?loading;  0;
\     inn; !; 
\     b/scr; blk; @; 
\     over; mod; -; 
\     blk; +!; ;;s
\ \ todo support eprint
\ _eprint .label .skip
\ s" '" colonword _tick
\ : '; ( -- ) _tick word, ;
\     -find;
\     0=;
\     0;
\     ?error;
\     drop;
\     literal;
\     ;;s
\ s" forget" colonword _forget
\ : forget; ( -- ) _forget word, ;
\     current; @;
\     context; @; -;
\     0x18 #plit;
\     ?error;
\     '; dup;
\     fence; @;
\     <; 0x15 #plit;
\     ?error; dup;
\     nfa; dp; !;
\     lfa; @;
\     context; @;
\     !; ;;s
\ s" back" colonword _back 
\ : back; ( -- ) _back word, ;
\     here; 
\     -; 
\     ,; 
\     ;;s
\ 
\ s" begin" colonword _begin 
\ : begin; ( -- ) _begin word, ;
\     ?comp;
\     here; 1;
\     ;;s
\ 
\ s" endif" colonword _endiff
\ : endif; ( -- ) _endiff word, ;
\     ?comp; 2; ?pairs;
\     here; over; -;
\     swap; !;
\     ;;s
\ s" then" colonword _then
\ : then; ( -- ) _then word, ;
\     endif; ;;s
\ 
\ s" do" colonword _do
\ : do; ( -- ) _do word, ;
\     compile;
\     (do);
\     here;
\     3;
\     ;;s
\ s" loop" colonword _loop
\ : loop; ( -- ) _loop word, ;
\     3; ?pairs;
\     compile;
\     _back ??(loop);
\     ;;s
\ s" +loop" colonword _ploop
\ : +loop; ( -- ) _ploop word, ;
\     3;
\     ?pairs;
\     compile;
\     _back ??(+loop);
\     ;;s
\ s" until" colonword _until
\ : until; ( -- ) _until word, ;
\     1;
\     ?pairs;
\     compile;
\     _back ??zbranch;
\     ;;s
\ s" end" colonword _end
\ : end; ( -- ) _end word, ;
\     until; ;;s
\ s" again" colonword _again
\ : again; ( -- ) _again word, ;
\     1; ?pairs; 
\     compile; 
\     _back ??branch; 
\     ;;s
\ s" repeat" colonword _repeat
\ : repeat; ( -- ) _repeat word, ;
\     >r; >r;
\     again;
\     r>; r>;
\     2; -;
\     endif;
\     ;;s
\ s" if" colonword _if 
\ : if; ( -- ) _if word, ;
\     compile;
\     _here ??zbranch;
\     0; ,; 2; ;;s
\ s" else" colonword _else
\ : else; ( -- ) _else word, ;
\     2; ?pairs; 
\     compile; 
\     _here ??branch; 
\     0; ,; swap; 
\     2; endif; 2; ;;s
\ s" while" colonword _while
\ : while; ( -- ) _while word, ;
\     if; 2+; 
\     ;;s
\ s" spaces" colonword _spaces
\ : spaces; ( -- ) _spaces word, ;
\ deflabel spax1
\ deflabel spax2
\     0; max; 
\     2dup; 
\     spax1 ??zbranch;
\     0; (do);
\ spax2 .label
\     space;
\     spax2 ??(loop);
\ spax1 .label
\     ;;s
\ s" <#" colonword bdigs
\ : <#; ( -- ) bdigs word, ;
\     pad;
\     hld;
\     !;
\     ;;s
\ s" #>" colonword edigs
\ : #>; ( -- ) edigs word, ;
\     drop; drop;
\     hld; @;
\     pad;
\     over;
\     -;
\     ;;s
\ s" sign" colonword _sign
\ : sign; ( -- ) _sign word, ;
\ deflabel sign1
\     rot;
\     0<;
\     sign1 ??zbranch;
\     0x2d #plit;
\     hold;
\ sign1 .label
\     ;;s
\ s" #" colonword dig
\ : #; ( -- ) dig word, ;
\ deflabel dig1
\     base; @;
\     */mod; 
\     rot; 
\     0x9 #plit;
\     over;
\     <;
\     dig1 ??zbranch;
\     0x7 #plit;
\     +;
\ dig1 .label
\     0x30 #plit; +;
\     hold; ;;s
\ s" #s" colonword digs
\ : #s; ( -- ) digs word, ;
\ deflabel-here digs1
\     #;
\     over; over; or;
\     0=; digs1 ??zbranch;
\     ;;s
\ s" d.r" colonword _d.r
\ : d.r; ( -- ) _d.r word, ;
\     >r;
\     swap;
\     over;
\     dabs;
\     <#; #; sign; #>;
\     r>; over; -; spaces;
\     type; ;;s
\ s" .r" colonword _.r
\ : .r; ( -- ) _.r word, ;
\     >r; s->d; r>; d.r; ;;s
\ 
\ s" d." colonword _d.
\ : d.; ( -- ) _d. word, ;
\ 0; d.r; space; ;;s
\ 
\ s" u." colonword _u.
\ : u.; ( -- ) _u. word, ;
\     0; d.; ;;s
\ s" vlist" colonword _vlist 
\ : vlist; ( -- ) _vlist word, ;
\     0x80 #plit; out; !;
\     context;
\     @; @;
\ deflabel-here vlist1
\ deflabel vlist2
\     out; \ begin
\     @;
\     c/l;
\     >;
\     vlist2 ??zbranch;
\     cr;
\     0; out; !;
\ vlist2 .label
\     dup;
\     id.;
\     space;
\     space;
\     pfa;
\     lfa; @;
\     dup;
\     0=;
\     ?terminal;
\     or;
\     vlist1 ??zbranch;
\     drop;
\     ;;s
\ s" bye" machineword _bye
\ : bye; ( -- ) _bye word, ;
\   /dev/terminate-vm #, xtop set,
\   zero xtop st,
\   next, \ dead code but here for consistency
\ s" list" colonword _list 
\ deflabel list1
\ deflabel list2
\ : list; ( -- ) _list word, ;
\     decimal;
\     cr;
\     dup;
\     scr; !;
\     pdotq;
\     s" scr # " .string,
\     .;
\     0x10 #plit;
\     0; (do);
\ list1 .label 
\     cr; i;
\     0x3 #plit;
\     .r; space;
\     i; scr;
\     @; .line;
\     ?terminal;
\     list2 ??zbranch; \ if
\     leave;
\ list2 .label
\     list1 ??(loop);
\     cr; 
\     ;;s
\ ;;s
\ s" index" colonword _index 
\ : index; ( -- ) _index word, ;
\ \ todo implement
\ ;;s
\ s" triad" colonword _triad 
\ : triad; ( -- ) _triad word, ;
\ \ todo implement
\ ;;s
\ s" .cpu" colonword _.cpu 
\ : .cpu; ( -- ) _.cpu word, ;
\ ;;s
\ 
\ s" match" machineword _match
\ : match; ( -- ) _match word, ;
\     \ todo implement
\     ;;s
\ s" task" colonword _task
\ : task; ( -- ) _task word, ;
\     \ todo implement
\     ;;s
\ ram-start .org
\ _origin .label
\ nop,
\ _cld ??, b,
\ nop,
\ _wrm ??, b,
\ \ todo put data to install about version data
\ \ todo put cold word variables here
\ \ _up .label  \ where the user pointer data is located
\ \ system-variables-start constant, 
\ _rpp .label
\ system-variables-start constant,
asm}

bye
