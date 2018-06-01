include iris.fs
\ contains all of the registers and pieces used for the monitor itself
\ the monitor is now also the forth system itself
s" figforth.o" {asm
\ the core memory is a disk buffer of a kind so it will become the disk buffer 
\ of legend that is being discussed in the forth book.
0xFFFF constant ram-end
0x0000 constant ram-start
ram-start constant interpreter-start
\ 0x0100 constant bootstrap-end
0xFF00 constant &LIMIT
&LIMIT 0x420 - constant &FIRST \ address of the first byte of the disk buffers
&FIRST 0xE0 - constant system-variables-end
system-variables-end 0x100 - constant system-variables-start
system-variables-start constant input-buffer-end
input-buffer-end 0x100 - constant input-buffer-start
input-buffer-start constant return-stack-start \ 512 entries
return-stack-start 0x200 - constant return-stack-end
return-stack-end constant data-stack-start

\ bootstrap-end constant dictionary-start

\ register reservations
: too-many-vars-defined ( addr -- ) 0x40 >= ABORT" To many registers used!" ;
: .word ( n -- ) !, .data16 ;
: defshorthand ( n "name" -- ) create , does> @ .word ;
unused-start 
\ constants
\ user variables
1+cconstant xs0 \ initial value of the data stack pointer
1+cconstant xr0 \ initial value of the return stack pointer
1+cconstant x&tib \ address of the terminal input buffer
1+cconstant xwarning \ error message control number. If 1, disk is present, 
                    \ and screen 4 of drive 0 is the base location of error messages
                    \ if 0, no disk is present and error messages will be presented
                    \ by number. If -1, execute (ABORT) on error
1+cconstant xfence \ address below which FORGETting is trapped.
                  \ To forget below this point, the user must alter the contents
                  \ of FENCE
1+cconstant xdp \ The dictionary pointer which contains the next free memory
               \ above the dictionary. The value may be read by HERE and altered
               \ by ALLOT.
1+cconstant xvoc-link \ address of a field in the definition of the most recently created
                     \ created vocabulary. All vocabulary names are linked by
                     \ these fields to allow control for FORGETting through multiple
                     \ vocabularies
1+cconstant xblk \ current block number under interpretatio. If 0, input is
                \ being taken from the terminal input buffer
1+cconstant xin \ Byte offset within the current input text buffer (terminal or
               \ disk) from which the next text will be accepted. WORD uses and
               \ move the value of IN
1+cconstant xout \ Offset in the text output buffer. Its value is incremented by EMIT
                 \ The user may yalter and examine OUT to control output display formatting.

1+cconstant xscr \ Screen number most recently referenced by LIST
1+cconstant xoffset \ Block offset disk drives. Contents of OFFSET is added to the stack number by BLOCK
1+cconstant xcurrent \ Pointer to the vocabulary in which new definitions are to be added
1+cconstant xstate \ If 0, the system is in interpretive or executing state. If non-zero, the system is in compiling state. The value itself is implementation
                   \ dependent. So in this case it would be 0 is interpretive and 0xFFFF is compiling
1+cconstant xdpl \ number of digits to the right of the decimal point on double integer input. It may also be used to hold output column
                 \ location of a decimal point in user generated formatting. The default value on single number input is -1
1+cconstant xfld  \ field width for formatted number output
1+cconstant xcsp \ temporarily stored data stack pointer for compilation error checking
1+cconstant xr# \ location of editor cursor in a text screen
1+cconstant xhld \ address of the latest character of text during numeric output conversion
1+cconstant xsp \ data stack pointer
1+cconstant xrp \ return stack pointer
1+cconstant xip \ interpretive pointer
1+cconstant xw \ current word pointer
1+cconstant xo \ accumulator register ( according to figforth )
1+cconstant at2 \ another temporary
1+cconstant xtop \  contents of the top of stack when a pop is called
1+cconstant xlower \ contents of the second stack item when a pop is called
1+cconstant xthird \ contents of the third stack item

too-many-vars-defined
num-base cconstant xbase

deflabel _COLD
deflabel _ABORT
_abort defshorthand _abort.
deflabel _QUIT
deflabel _INTERPRET
deflabel _NEXT
deflabel _EXECUTE
deflabel _DOCOLON
deflabel _;S

ram-start .org 
\ set the constants
&LIMIT constant xlimit
&FIRST constant xfirst
0x8 constant b/scr
0x80 constant b/buf 
\ start user variables
zero xs0 ->

: next, ( -- )
    xip xw -> \ move the contents of xip (which points to the next word to be executed, into xw .
    xip 1+, \ Increment xip, pointing to the second word in execution sequence.
    xw at0 ldtincr, \ load the contents of xw into at0 and then increment xw
                    \ this will make xw point to the parameter field of the word
    at0 br,         \ jump to the address found at that point
    ;

: ;s, ( -- ) _;S !, b, ;
: machine-code-execute ( -- ) loc@ 1+ #, .data16 ;
: embed-docolon ( -- ) _DOCOLON !, .data16 ;
: machineword ( n -- ) .label machine-code-execute ;
: colondef ( n -- ) .label embed-docolon ;
: machine-code-jump ( imm id -- ) 
  at0 set,
  at0 at0 ld,
  at0 br, ;
_Execute .label
    \ execute the definition whose code field address cfa is on the data stack
    xsp xw pop, \ pop the code field address into xw, the word pointer
    xw at0 ldtincr, \ Jump indirectly to the code routine. Increment xw to point to the parameter field
    at0 br, 
_DOCOLON .label \ runtime routine for all colon definitions
    xip xrp push,  \ push the address of the next word to the return stack and enter a lower nesting level
    xw xip -> \ move the parameter field address into IP, pointing to the first word in this definition
    \ duplicate NEXT for now
    next,
_;S .label \ perform unnesting
\ return execution to the calling definition. Unnest one level.
    xrp xip pop, \ pop the return stack into xip, pointing now to the next word to be executed in the calling definition
    next,
deflabel _PUSH
_PUSH machineword
    xo xsp push, 
    next,
deflabel _POP
_POP machineword
    xsp zero pop,
    next,
deflabel _PUT
_PUT machineword
    \ replace the top of data stack with the contents of the accumulator
    xo xsp st, 
    next,
deflabel _LIT
_LIT machineword

    \ push the next word to the data stack as a literal. Increment IP and skip this literal.
    \ NEXT Return
    \ LIT is used to compile numbers into the dictionary. At run-time, LIT pushes the 
    \ inline literal to the data stack to be used in computations
    xip xsp push,
    xip 1+,
    next,
: compile-literal ( n -- )
  \ compile the literal into the dictionary by putting the _LIT command followed by
  \ the number itself
  _LIT !, .data16
  #, .data16 ;

deflabel _;IMMEDIATE
deflabel _LeftBracket
_LeftBracket machineword
    \ suspend compilation and execute the words following [ up to ]. 
    \ this allows calculation or compilation exceptions before resuming compilation
    \ with ]. 
    zero xstate ->
    _;IMMEDIATE !, b,
deflabel _RightBracket
_RightBracket machineword
    \ resume compilation till the end of a colon definition
    0xc0 #, xstate set, \ the text interpreter compares the value stored in xstate
                        \ with the value in the length byte of the definition found
                        \ in the dictionary. If the definition is an immediate word,
                        \ its length byte is greater than 0xC0 because of the precedence and the
                        \ sign bits are both set.
                        \ 
                        \ Setting xstate to 0xc0 will force non-immediate words
                        \ to be compiled and immediate words to be executed, 
                        \ thus entering into the 'compiling state'
    next,
deflabel _WORD
deflabel _HERE
deflabel _DP
deflabel _!
_! defshorthand _!.
deflabel _ALLOT
deflabel _SWAP
deflabel _DROP
deflabel _DUP
deflabel _-DUP
deflabel _OVER
deflabel _+
deflabel _-
deflabel _*
deflabel _rot
: 1pop ( -- )
  xsp xtop pop, ;
: 2pop ( -- )
  1pop 
  xsp xlower pop, ;
: 3pop ( -- )
  2pop
  xsp xthird pop, ;
_+ machineword
    2pop \ top -> b | lower -> a
    xtop xlower xtop add, 
    xtop xsp push,
    next,
_- machineword
    2pop \ a b
    xtop xlower xtop sub, 
    xtop xsp push,
    next,
_rot machineword ( a b c -- b c a )
    3pop \ a (third) b (lower)  c (top)
    xlower xsp push,
    xtop xsp push,
    xthird xsp push,
    next,
_-dup machineword \ duplicate if non zero
    deflabel _-dup_done
    1pop 
    xtop cv eqz, 
    _-dup_done !, cv bc,
        xtop xsp push,
    _-dup_done .label
    xtop xsp push,
    next,
deflabel _>r
_>r machineword \ move top item to return stack
    1pop 
    xtop xrp push,
    next,
deflabel _r>
_r> machineword \ retrieve item from top of return stack
    xrp xtop pop,
    xtop xsp push,
    next,
deflabel _r
deflabel _x
_r machineword \ copy top of return stack onto stack
   xrp xtop ld,
   xtop xsp push,
   next,
_* machineword 
    2pop 
    xtop xlower xtop mul,
    xtop xsp push,
    next,
deflabel _/
_/ machineword
    2pop
    xtop xlower xtop div, 
    xtop xsp push,
    next,

deflabel _mod
_mod machineword
    2pop
    xtop xlower xtop rem, 
    xtop xsp push,
    next,
deflabel _min
_min machineword
    2pop
    xtop xlower xtop min,
    xtop xsp push,
    next,
deflabel _max
_max machineword
    2pop
    xtop xlower xtop max,
    xtop xsp push,
    next,

deflabel _abs
_abs machineword
    deflabel _absDone
    1pop
    xtop cv gez,
    _absDone !, cv bc,
    0xFFFF #, xtop xtop muli, \ multiply by negative one
    _absDone .label
    xtop xsp push,
    next,

deflabel _minus
_minus machineword
    deflabel _minusDone
    1pop
    0xFFFF #, xtop xtop muli, \ multiply by negative one
    xtop xsp push,
    next,
deflabel _and
_and machineword
    2pop
    xtop xlower xtop and,
    xtop xsp push,
    next,
deflabel _or
_or machineword
    2pop
    xtop xlower xtop or,
    xtop xsp push,
    next,
deflabel _xor
_xor machineword
    2pop
    xtop xlower xtop xor,
    xtop xsp push,
    next,
deflabel _<
_< machineword
    2pop
    xtop xlower xtop lt,
    xtop xsp push,
    next,
deflabel _>
_> machineword
    2pop
    xtop xlower xtop gt,
    xtop xsp push,
    next,
deflabel _=
_= machineword
    2pop
    xtop xlower xtop eq,
    xtop xsp push,
    next,

deflabel _0<
_0< machineword
    1pop
    xtop xtop ltz,
    xtop xsp push,
    next,
deflabel _0=
_0= machineword
    1pop
    xtop xtop eqz,
    xtop xsp push,
    next,

_over machineword
    xsp xtop incr,
    xtop xlower ld,
    xlower xsp push,
    next,
_dup machineword
    xsp xtop ld,
    xtop xsp push,
    next,
_drop machineword
    xsp zero pop, 
    next,

_swap machineword
    xsp xtop pop, \ get a
    xsp xlower pop, \ get b
    xtop xsp push, 
    xlower xsp push,
    next,
_dp machineword ( -- n ) 
    dp xsp push, 
    next,
_! machineword ( v a -- )
   xsp xtop pop, \ addr
   xsp xlower pop, \ value
   xlower xtop st, \ perform the store
   next,

deflabel _c,
_c, machineword
    xsp xtop pop, \ get n
    0xFF #, xtop xtop andi, 
    xtop xdp st, \ save it to the current dict pointer front
    dp 1+, \ move ahead by one
    next,
deflabel _c!
_c! machineword ( value addr -- )
    xsp xtop pop, \ addr
    xsp xlower pop, \ value
    0xFF #, xlower xlower andi,
    xlower xtop st, \ save it to memory with the upper 8 bits masked
    next,
deflabel _c@
_c@ machineword
    1pop \ top - addr
    xtop xtop ld,
    0xFF #, xtop xtop andi,
    xtop xsp push,
    next,
deflabel _@
_@ machineword
    1pop
    xtop xtop ld,
    xtop xsp push,
    next,
deflabel _.
_. machineword
   /dev/console2 #, xlower set,
   1pop
   xtop xlower stio,
   next,
deflabel _CR
_CR machineword
    /dev/console0 #, xlower set,
    0xA #, xtop set,
    xtop xlower stio,
    next,
deflabel _space
_space machineword
    /dev/console0 #, xlower set,
    0x20 #, xtop set,
    xtop xlower stio,
    next,
deflabel _key
_key machineword
    /dev/console0 #, xlower set,
    xlower xtop ldio,
    xtop xsp push,
    next,
deflabel _emit
_emit machineword
    /dev/console0 #, xlower set,
    1pop
    xtop xlower stio,
    next,
deflabel _sp@
_sp@ machineword
    xsp xsp push,
    next,

deflabel _?
_? machineword
    1pop 
    xtop xtop ld,
    /dev/console0 #, xlower set,
    xtop xlower stio,
    next,
    
deflabel _+!
_+! machineword
    2pop \ top - addr 
         \ lower - n
    xtop at0 ld,
    xlower at0 xlower add, 
    xlower xtop st,
    next,
deflabel _cmove
_cmove machineword ( from to u -- ) \ move u bytes in memory 
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
deflabel _fill
_fill machineword ( addr n b -- ) \ fill u bytes in memory with b beginning at address
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
deflabel _bl
_bl machineword 
    bl #, xsp pushi,
    next,

_here machineword 
    xdp xsp push,
    next,
_allot machineword ( n -- )
    xsp xtop pop,
    xtop dp dp add,
    next,
deflabel _pad
_pad machineword ( n -- )
    0x44 #, xdp xtop addi,
    xtop xsp push,
    next,
deflabel _,
_, machineword ( n -- )
    xsp xtop pop, \ get n
    xtop xdp st, \ save it to the current dict pointer front
    dp 1+, \ move ahead by one
    next,
\ deflabel _'
deflabel _zero
_zero machineword
    zero xsp push,
    next,
_zero defshorthand _zero.
deflabel _erase
_erase colondef ( addr u -- ) \ fill u bytes in memory with zeros
    _zero.
    _fill .word
    _;S .word
deflabel _blanks
_blanks colondef ( addr u -- ) \ fill u bytes in memory with zeros
    bl compile-literal
    _fill .word
    _;S .word
deflabel _empty-buffers
deflabel _density
_COLD colondef
    _empty-buffers .word
    _zero.
    _density .word
    _!.
    _first .word
    _use .word
    _!.
    _dr0 .word
    _zero.
    _eprint .word
    _!.
    _orig .word
    0x12 compile-literal
    _+ .word
    _up .word
    _@ .word
    6 compile-literal
    _+ .word
    0x10 compile-literal
    _cmove .word
    _orig .word
    0x0C compile-literal
    _+ .word
    _@ .word
    _forth .word
    6 compile-literal
    _+ .word
    _!. 
    _abort .word
    _;s .word
\ when the machine starts from cold
\ COLD calls ABORT
\ ABORT calls QUIT
deflabel _leftbracket
_leftbracket colondef
    _zero.
    _state .word
    _!.
    _;IMMEDIATE .word
deflabel _rightbracket
_rightbracket colondef
    0xc0 compile-literal
    _state .word
    _!.
    _;s .word
asm}

bye
