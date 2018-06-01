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
1+cconstant s0 \ initial value of the data stack pointer
1+cconstant r0 \ initial value of the return stack pointer
1+cconstant &tib \ address of the terminal input buffer
1+cconstant warning \ error message control number. If 1, disk is present, 
                    \ and screen 4 of drive 0 is the base location of error messages
                    \ if 0, no disk is present and error messages will be presented
                    \ by number. If -1, execute (ABORT) on error
1+cconstant fence \ address below which FORGETting is trapped.
                  \ To forget below this point, the user must alter the contents
                  \ of FENCE
1+cconstant xdp \ The dictionary pointer which contains the next free memory
               \ above the dictionary. The value may be read by HERE and altered
               \ by ALLOT.
1+cconstant voc-link \ address of a field in the definition of the most recently created
                     \ created vocabulary. All vocabulary names are linked by
                     \ these fields to allow control for FORGETting through multiple
                     \ vocabularies
1+cconstant blk \ current block number under interpretatio. If 0, input is
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
0x40 constant c/l 
0x20 constant bl 
\ start user variables
zero s0 ->

_COLD .label 
\ when the machine starts from cold
\ COLD calls ABORT
\ ABORT calls QUIT
\ where the text interpreter is embedded
\ : COLD ( -- ) 
\   EMPTY-BUFFERS \ Clear all disk buffers by writing zero's from FIRST to LIMIT
\   0 DENSITY !  \ Specify single density diskette drives
\   FIRST USE ! \ Store the first buffer address in USE and PREV, preparing for disk accessing
\   FIRST PREV !
\   DR0  \ select drive 0 by setting offset to 0
\   0 EPRINT ! \ turn off the printer
\   ORIG       \ Starting address of Forth codes, where initial user variables are kept
\   12H +      
\   UP @ 6 + \ User area
\   10H CMOVE  \ Move 16 bytes of initial values over to the user area. Initialize the terminal
\   ORIG 0CH + @ \ Fetch the name field address of the last word defined in the trunk forth vocabulary, and
\   FORTH 6 + !   \ store it in the FORTH vocabulary link. Dictionary searches will start at the top of FORTH vocabulary.
\                \ New words will be added to FORTH vocabulary unless another vocabulary is named.
\   ABORT        \ call ABORT to do a warm start procedure
\   ;
: next, ( -- )
    xip xw -> \ move the contents of xip (which points to the next word to be executed, into xw .
    xip 1+, \ Increment xip, pointing to the second word in execution sequence.
    xw at0 ldtincr, \ load the contents of xw into at0 and then increment xw
                    \ this will make xw point to the parameter field of the word
    at0 br,         \ jump to the address found at that point
    ;

: s;, ( -- ) _;S !, b, ;
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

deflabel _C@
deflabel _C,
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
    dp 1+, \ move ahead by one
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
deflabel _erase
_erase machineword ( addr u -- ) \ fill u bytes in memory with zeros
    zero xsp push,
    _fill !, machine-code-jump
deflabel _bl
_bl machineword 
    0x20 #, xsp pushi,
    next,
deflabel _blanks
_blanks machineword ( addr u -- ) \ fill u bytes in memory with zeros
    0x20 #, at0 set,
    at0 xsp push,
    _fill !, machine-code-jump

_here machineword 
    xdp xsp push,
    next,
_allot machineword ( n -- )
    xsp xtop pop,
    xtop dp dp add,
    next,
deflabel _,
_, machineword ( n -- )
    xsp xtop pop, \ get n
    xtop xdp st, \ save it to the current dict pointer front
    dp 1+, \ move ahead by one
    next,
\ deflabel _'



\ deflabel _CREATE
\ deflabel _WIDTH
\ deflabel _MIN
\ deflabel _1+
\ deflabel _TOGGLE
\ deflabel _LATEST,
\ deflabel _BL
\ deflabel _0A0H
\ deflabel _1-
\ deflabel _80H
\ deflabel _2+
\ deflabel _Semicolon
\ _BL .label bl xsp push, ;s, 
\ _0A0H .label 0xA0 xsp push, ;s
\ _80H .label 0x80 xsp push, ;s
\ _CREATE .label
\     \ create a dictionary header for a new definition with name cccc. 
\     \ The new word is linked to the current vocabulary. The code field points
\     \ to the parameter field, ready to compile a code definition. 
\     bl #, at0 set,
\     at0 xsp push, \ push bl onto the stack
\ 
\     _WORD !.data16 \ bring the next string delimited by blanks to the top of dictionary
\     _HERE !.data16 \ save the dictionary pointer as name field address to be linked
\     _DUP !.data16 _C@ !.data16 \ get length byte of string
\     _WIDTH !.data16 _@ !.data16 \ WIDTH has the maximum number of characters allowed in the name field
\     _WIDTH !.data16 _MIN !.data16 \ use the smaller of the two, and
\     _1+ !.data16 _ALLOT !.data16 \ allocate space for name field, and advance DP to the link field
\     _DUP !.data16 _0A0H !.data16 _TOGGLE !.data16 \ Toggle the 8th (start) and sixth (smudge) bits in the length byte of the name field.
\                                                                     \ make a 'smudged' head so that the dictionary search will not find this name
\     _HERE !.data16 _1- !.data16 _80H !.data16 _TOGGLE !.data16 \ Toggle the 8th bit in the last character of the name as a delimiter to
\                                                                                        \ the name field.
\     _Latest, !.data16 \ compile the name field address of the last word in the link field, extending the linking chain
\     _CURRENT !.data16 _@ !.data16 _! !.data16 \ update contents of latest in the current vocabulary
\     _HERE !.data16 _2+ !.data16 _, !.data16 \ Compile the parameter field address into code field, for teh convenience of a new code
\                                                               \ definition. For other types of definitions, proper code routine address will be compiled here.
\     _Semicolon !.data16
\ 
\ deflabel _CODE
\ deflabel _COMPILE
\ deflabel _Assembler
\ _CODE .label
\     _DOCOLON !.data16
\     _CREATE !.data16  \ create the header, nothing more to be done on the header
\     _COMPILE !.data16 \ ????
\     _Assembler !.data16 \ Select ASSEMBLER vocabulary as the CONTEXT vocabulary,
\                               \ which has all the assembly mnemonics and words pertaining to assembly processes.
\     _Semicolon !.data16
\ 
\ deflabel _?ERROR
\ deflabel _IF
\ deflabel _ERROR
\ deflabel _ELSE
\ deflabel _ENDIF
\ deflabel _Warning@
\ deflabel _Count
\ deflabel _Type
\ _if defshorthand _if.
\ _else defshorthand _else.
\ _endif defshorthand _endif.
\ _do defshorthand _do.
\ _loop defshorthand _loop.
\ _over defshorthand _over.
\ _swap defshorthand _swap.
\ _here defshorthand _here.
\ _type defshorthand _type.
\ _?ERROR .label
\     _SWAP .word
\     _if .word
\     _error .word
\     _else .word
\     _drop .word
\     _endif .word
\     _Semicolon .word
\ deflabel _0< 
\ deflabel _(ABORT) 
\ deflabel _PrintQuestionMark 
\ deflabel _MESSAGE 
\ deflabel _SP! 
\ deflabel _IN@ 
\ deflabel _BLK@
\ _ERROR .label
\     _Warning@ .word
\     _0< .word \ see if warning is less than -1
\     _if. 
\     _(ABORT) .word 
\     _Endif .word
\     _here.  _Count .word _type. \ print name of the offending word on top of the dictionary
\     _PrintQuestionMark .word
\     _MESSAGE .word \ type the error message stored on disk
\     _SP! .word \ clean the data stack
\     _IN@ .word
\     _BLK@ .word \ fetch in and blk on stack for the operator to look at if he wishes
\     _QUIT .word \ restart the forth loop
\     _Semicolon .word
\ _(ABORT) .label
\     _ABORT .word \ execute ABORT after an error when WARNING is -1. IT may be changed to a user defined procedure
\     _Semicolon .word
\ \ terminal routines
\ deflabel _EXPECT ( n addr -- ) 
\ \ transfer n characters from the terminal to memory starting at addr. 
\ \ The text may be terminated by a carriage return
\ \ An ASCII NUL is appended to the end of text
\ deflabel _-
\ deflabel _Push8
\ deflabel _Push2
\ deflabel _DO
\ deflabel _KEY
\ deflabel _DUP
\ deflabel _+ORIGIN
\ deflabel _Equal
\ deflabel _R>
\ deflabel _>R
\ deflabel _0EH 
\ deflabel _I=
\ _EXPECT .label
\     _OVER .word _+ .word \ address, the end of text
\     _Over .word \ start of text
\     _Do .word  \ repeat the following for n times
\     _Key .word \ get one character from terminal
\     _Dup .word \ make a copy
\     _0EH .word _+ORIGIN .word \ Get the ascii code of input backspace
\     _Equal .word
\     _if.  \ if the input is a backspace
\         _DROP .word \ discard the backspace still on stack
\         _Push8 .word \ replace it with back-space for the output device
\         _OVER .word \ copy addr
\         _I= .word \ see if the current character is the first character of text
\         _dup .word \ copy it, to be used as a flag
\         _r> .word _push2 .word _- .word _+ .word \ get the loop index. Decrement it by 1 if it is the starting character, or decrement it by
\                                                                          \ 2 if it is in the middle of the text.
\         _>r .word \ put the corrected loop back on the return stack
\         _- \ if the backspace is the first character, ring the bell.
\                  \ otherwise, output backspace and decrement character count
\     _else .word \ not a backspace
\         _dup .word _push0d .word _= .word \ is it a carriage return?
\         _if .word \ yes
\             _leave .word \ prepare to exit the loop. CR is the end of text line
\             _drop .word _bl .word \ drop cr on the stack and replace with a blank.
\             _push0 .word \ put a null on the stack
\         _else .word 
\             _dup .word \ input is a regular ascii character. make a copy
\         _endif .word
\         _i .word _c! .word \ store the ascii character into the input buffer area
\         _push0 .word _i .word _1+ .word _! .word \ guard the text with an ascii null
\     _endif .word \ end of the input loop
\     _emit .word \ echo the input character to terminal
\     _loop .word \ loop back if not the end of text
\     _drop .word \ discard the addr remaining on the stack
\     _semicolon .word
\ deflabel _QUERY
\ _QUERY .label
\     \ input 80 characters (or until a carriage-return) from the terminal and 
\     \ place the text in the terminal input buffer.
\     _TIB .word \ contains the starting address of the input terminal buffer
\     _Push50h .word _EXPECT .word \ get 80 characters
\     _0 .word _In .word _! .word \ set the input character counter IN to 0. Text parsing shall begin at TIB
\     _Semicolon .word
\ 
\ _WORD .label ( c -- )
\     ( read text from the input stream until a delimiter c is encountered. Store
\       the text string at the top of dictionary starting HERE. The first byte is
\       the character count, then the text string, and two or more blanks. If
\       BLK is zero, input it from the terminal; otherwise, input from the disc
\       block referred to by BLK. )
\     \ write this in native eventually
\     _BLK@ .word 
\     _IF. \ BLK = 0 ?, if BLK is not zero, go look at the disk.
\         _BLK@ .word \ The block number
\         _BLOCK .word \ Grab a block of data from disc and put it in a disc buffer
\                            \ Leave the buffer address on the stack. BLOCK is the
\                            \ word to access disc virtual memory
\     _ELSE. \ blk = 0, input is from terminal
\         _TIB@ .word \ text should be put in the terminal input buffer
\     _Endif. 
\     _IN@. \ in contains the character offset into the current input text buffer
\     _+. \ add offset to the starting address of buffer, pointing to the next character to be read in.
\     _swap. \ get delimiter c over the string addr
\     _ENCLOSE .word \ a primitive word to scan the text. ( addr c -- addr nl n2 n3 )
\     _here. _022H .word  _BLANKS .word \ write 34 blanks to the top of dictionary
\     _IN. _+!. \ increment in by the character count, pointing to the next text string to be parsed.
\     _OVER. _-. _>R. \ save n2-nl onto the return stack
\     _R. _over. _C!. \ store character count as the length byte at HERE.
\     _+. \ buffer address + nl, starting point of the text string in the text buffer
\     _HERE. _1+. \ address after the length byte on dictionary
\     _R>. \ get the character count back from the return stack
\     _semicolon.
\ _TYPE .label ( addr n -- )
\     \ transmit n charactesr from a text string stored at addr to the terminal
\     _-dup .word \ copy n if it is not zero
\     _if. \ n is non-zero
\         _over. _+ .word \ addr + n, the end of the text
\         _swap. \ addr, the start of text
\         _do. \ loop to type n characters
\         _I .word _c@ .word \ fetch character from text
\         _emit .word \ typeout
\         _loop. 
\     _else.
\         _drop. \ discard addr
\     _endif.
\     _semicolon.
\ 
\ _count .label ( addr1 -- addr2 n )
\     \ push the address and byte count of n of a text string at addr1 to the data stack.
\     \ The first byte of the text string is a byte count. COUNT is usually followed by TYPE
\     _dup. _1+. \ addr2 = addr1 + 1
\     _swap. \ swap addr1 over addr2 and fetch teh byte count to the stack
\     _@. \ load the address to get the length
\     _semicolon.
\ 
\ \ deflabel _-trailing
\ \ _-trailing .label
\ \     deflabel _-trailing_loop
\ \     deflabel _-trailing_done
\ \     deflabel _-trailing_cond0
\ \     xsp at0 ld,
\ \     at0 xsp push,  
\ \     zero xsp push, 
\ \     _-trailing_loop .label
\ \     \ over over, copy addr and n1
\ \     1 #, xsp at0 addi,
\ \     at0 at1 ld,  \ load lower but leave stack alone
\ \     xsp at2 ld,  \ load top  but leave stack alone
\ \     at1 at2 at0 add, \ combine them
\ \     at0 1-, \ decrement it, addr+n1 - 1, the address of the last character in the string
\ \     at0 at1 ld, 
\ \     bl #, at2 set,
\ \     at2 at1 at0 sub, \ see if it is blank
\ \     \ TODO continue here
asm}
bye
