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


ram-start .org 
\ set the constants
&LIMIT constant xlimit
&FIRST constant xfirst
0x8 constant b/scr
0x80 constant b/buf 
\ start user variables
zero xs0 ->
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
: defmachineword ( "name" -- ) 
  deflabel-here 
  execute-latest 
  machine-code-execute ;
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
defmachineword _push
    xo xsp push, 
    next,
defmachineword _pop
    xsp zero pop,
    next,
defmachineword _put
    \ replace the top of data stack with the contents of the accumulator
    xo xsp st, 
    next,
defmachineword _lit
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

: 1pop ( -- )
  xsp xtop pop, ;
: 2pop ( -- )
  1pop 
  xsp xlower pop, ;
: 3pop ( -- )
  2pop
  xsp xthird pop, ;
defmachineword _+
    2pop \ top -> b | lower -> a
    xtop xlower xtop add, 
    xtop xsp push,
    next,
defmachineword _-
    2pop \ a b
    xtop xlower xtop sub, 
    xtop xsp push,
    next,
defmachineword _rot ( a b c -- b c a )
    3pop \ a (third) b (lower)  c (top)
    xlower xsp push,
    xtop xsp push,
    xthird xsp push,
    next,
defmachineword _-dup \ duplicate if non zero
    deflabel _-dup_done
    1pop 
    xtop cv eqz, 
    _-dup_done !, cv bc,
        xtop xsp push,
    _-dup_done .label
    xtop xsp push,
    next,
defmachineword _>r \ move top item to return stack
    1pop 
    xtop xrp push,
    next,
defmachineword _r> \ retrieve item from top of return stack
    xrp xtop pop,
    xtop xsp push,
    next,
defmachineword _r \ copy top of return stack onto stack
   xrp xtop ld,
   xtop xsp push,
   next,
defmachineword _*
    2pop 
    xtop xlower xtop mul,
    xtop xsp push,
    next,
defmachineword _/
    2pop
    xtop xlower xtop div, 
    xtop xsp push,
    next,

defmachineword _mod
    2pop
    xtop xlower xtop rem, 
    xtop xsp push,
    next,
defmachineword _min
    2pop
    xtop xlower xtop min,
    xtop xsp push,
    next,
defmachineword _max
    2pop
    xtop xlower xtop max,
    xtop xsp push,
    next,

defmachineword _abs
    deflabel _absDone
    1pop
    xtop cv gez,
    _absDone !, cv bc,
    0xFFFF #, xtop xtop muli, \ multiply by negative one
    _absDone .label
    xtop xsp push,
    next,

defmachineword _minus
    deflabel _minusDone
    1pop
    0xFFFF #, xtop xtop muli, \ multiply by negative one
    xtop xsp push,
    next,
defmachineword _and
    2pop
    xtop xlower xtop and,
    xtop xsp push,
    next,
defmachineword _or
    2pop
    xtop xlower xtop or,
    xtop xsp push,
    next,
defmachineword _xor
    2pop
    xtop xlower xtop xor,
    xtop xsp push,
    next,
defmachineword _<
    2pop
    xtop xlower xtop lt,
    xtop xsp push,
    next,
defmachineword _>
    2pop
    xtop xlower xtop gt,
    xtop xsp push,
    next,
defmachineword _=
    2pop
    xtop xlower xtop eq,
    xtop xsp push,
    next,

defmachineword _0<
    1pop
    xtop xtop ltz,
    xtop xsp push,
    next,
defmachineword _0=
    1pop
    xtop xtop eqz,
    xtop xsp push,
    next,

defmachineword _over 
    xsp xtop incr,
    xtop xlower ld,
    xlower xsp push,
    next,
defmachineword _dup 
    xsp xtop ld,
    xtop xsp push,
    next,
defmachineword _drop 
    xsp zero pop, 
    next,

defmachineword _swap
	2pop \ top - a 
		 \ lower - b
    xtop xsp push, 
    xlower xsp push,
    next,
defmachineword _dp ( -- n ) 
    dp xsp push, 
    next,
defmachineword _! ( v a -- )
   2pop \ top - addr
   		\ lower - value
   xlower xtop st, \ perform the store
   next,

defmachineword _c,
	1pop
    0xFF #, xtop xtop andi, 
    xtop xdp st, \ save it to the current dict pointer front
    dp 1+, \ move ahead by one
    next,
defmachineword _c!  ( value addr -- )
	2pop \ top - addr
		 \ lower - value
    0xFF #, xlower xlower andi,
    xlower xtop st, \ save it to memory with the upper 8 bits masked
    next,
defmachineword _c@
    1pop \ top - addr
    xtop xtop ld,
    0xFF #, xtop xtop andi,
    xtop xsp push,
    next,
defmachineword _@
    1pop
    xtop xtop ld,
    xtop xsp push,
    next,
defmachineword _.
   /dev/console2 #, xlower set,
   1pop
   xtop xlower stio,
   next,
defmachineword _cr
    /dev/console0 #, xlower set,
    0xA #, xtop set,
    xtop xlower stio,
    next,
defmachineword _space
    /dev/console0 #, xlower set,
    0x20 #, xtop set,
    xtop xlower stio,
    next,
defmachineword _key
    /dev/console0 #, xlower set,
    xlower xtop ldio,
    xtop xsp push,
    next,
defmachineword _emit
    /dev/console0 #, xlower set,
    1pop
    xtop xlower stio,
    next,
defmachineword _sp@
    xsp xsp push,
    next,

defmachineword _?
    1pop 
    xtop xtop ld,
    /dev/console0 #, xlower set,
    xtop xlower stio,
    next,
    
defmachineword _+!
    2pop \ top - addr 
         \ lower - n
    xtop at0 ld,
    xlower at0 xlower add, 
    xlower xtop st,
    next,
defmachineword _cmove ( from to u -- ) \ move u bytes in memory 
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
defmachineword _fill ( addr n b -- ) 
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
defmachineword _bl
    bl #, xsp pushi,
    next,

defmachineword _here
    xdp xsp push,
    next,
defmachineword _allot ( n -- )
	1pop
    xtop dp dp add,
    next,
defmachineword _pad ( -- n )
    0x44 #, xdp xtop addi,
    xtop xsp push,
    next,
defmachineword _, ( n -- )
    \ store n into the next available cell above dictionary and advance DP by 2 thus
    \ compiling into the dictionary
	1pop 
    xtop xdp st, \ save it to the current dict pointer front
    dp 1+, \ move ahead by one
    next,
defmachineword _zero
    zero xsp push,
    next,
defmachineword _leftbracket
    zero xstate move,
    \ ;IMMEDIATE to mark it as an immediate word
    next,
defmachineword _rightbracket
    0xFFFF #, xstate set,
    next,

defmachineword _dodoe_prime
    xip xrp push,
    xw xip move,
    xw 1+,
    xw xsp push,
    next,
defmachineword _branch
	xip xtop ld,
	xtop xip xip add,
	next,
defmachineword _0branch
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
asm}

bye
