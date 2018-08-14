\ 
\  iris
\  Copyright (c) 2013-2018, Joshua Scoggins and Contributors
\  All rights reserved.
\ 
\  Redistribution and use in source and binary forms, with or without
\  modification, are permitted provided that the following conditions are met:
\      * Redistributions of source code must retain the above copyright
\        notice, this list of conditions and the following disclaimer.
\      * Redistributions in binary form must reproduce the above copyright
\        notice, this list of conditions and the following disclaimer in the
\        documentation and/or other materials provided with the distribution.
\ 
\  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
\  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
\  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
\  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
\  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
\  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
\  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
\  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
\  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
\  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
\  

require ./iris.fs
: 2+ ( a -- b ) 2 + ;
assembler
\ macros and higher level operations
x0 constant xsp
x1 constant xrp
x2 constant xtop
x3 constant xlower
x4 constant xthird
x5 constant xfourth
x6 constant xinput
x7 constant xlength
x8 constant xrow
x9 constant xcolumn

0xFE00 constant data-stack-start
0xFD00 constant data-stack-end
data-stack-end constant return-stack-start
return-stack-start 0x200 - constant return-stack-end
0x0000 constant input-buffer-start
input-buffer-start 0x100 + constant input-buffer-end
: 1pop, ( -- ) xsp xtop pop, ;
: 2pop, ( -- )
    1pop,
    xsp xlower pop, ;
: literal, ( imm -- ) xsp pushi, ;
: bl, ( imm -- ) xrp call, ;
0x0100 constant variables-start
: &bootkind ( -- value ) variables-start ;
: &delimiter ( -- value ) &bootkind 2+ ;
: &base ( -- value ) &delimiter 2+ ;
: &dp ( -- value ) &base 2+ ; 
: &maxrow ( -- value ) &dp 2+ ;
: &maxcolumn ( -- value ) &maxrow 2+ ;
: if, ( -- addr ) 
  xsp xtop pop, ( get the top of the stack )
  xtop xtop invert,  ( invert the condition since we want to jump to the else )
  0 xtop ?branch, ( branch if the code was zero )
  .mloc 2 - ( we want the address where to store the jump target )
  ;
: else, ( addr -- addr2 )
	0 branch, \ the skip over point for all of this 
	.mloc swap over ( loc addr loc ) \ need addr for later
	2 - ( loc addr loc-2 )
	>r ( loc addr )
	memory_base @ +
	2dup swap addr8 swap c!
	1+ swap 8 rshift addr8 swap c! 
	r> ( loc-2 ) ;
: then, ( addr -- )
  \ take the address on the top of the stack and stash the current location
  \ into it
  .mloc ( addr loc )
  swap ( loc addr ) memory_base @ + 
  2dup swap addr8 swap c!
  1+ swap 8 rshift addr8 swap c! ;
: begin, ( -- addr ) .mloc ;
: while, ( -- addr ) 
  1pop,
  xtop xtop invert,
  0 xtop ?branch,
  .mloc 2 - ;
: repeat, ( at al -- )
  swap ( al at )
  branch, ( al )
  memory_base @ + .mloc swap 
  2dup c! ( v al )
  1+ ( v al+1 ) swap 8 rshift swap c! ;
: until, ( addr -- )
  xsp xtop pop,
  0 xlower set,
  xlower xtop xtop eq,
  xtop ?branch, ;
: again, ( addr -- ) branch, ; 
0x0200 constant dictionary-start
variables-start .org
    0xFFFF word,        \ we are doing cold or warm boot?
    0x0020 word,        \ delimiter character
    0x000a word,        \ numeric base
dictionary-start .org
0x0001 constant flag/compile 
0x0002 constant flag/immediate
flag/compile flag/immediate or constant flag/comp,imm 
variable previousWord
0 previousWord !
3 constant column-start

: compute-hash ( str u -- nhu nhm nhl ) 
  addr8 over c@ 8 lshift 0xff00 and or addr16 ( str nhl ) 
  swap ( nhl str ) 
  1+ dup dup 2+ >r ( nhl str+1 str+1 )
  c@ ( nhl str+1 c2 ) addr8 
  swap 1+ ( nhl c2 str+2 ) 
  c@ addr8 ( nhl c2 c3 )
  8 lshift 0xff00 and or addr16 ( nhl nhu )
  swap ( nhm nhl ) 
  r> ( nhm nhl addr+3 )
  dup 1+ ( nhm nhl addr+3 addr+4 )
  c@ addr8 ( nhm nhl addr+3 c5 )
  8 lshift 0xff00 and swap ( nhm nhl c5<<8 addr+3 )
  c@ addr8 or addr16 ( nhm nhl nhu ) 
  -rot ( nhu nhm nhl )
  ; 

: defword_custom: ( previous flags str len "id" -- ) 
  compute-hash 
  x10 set16, \ lowest name-hash
  x11 set16, \ middle name-hash
  x12 set16, \ upper name-hash
  x13 set16, \ flags 
  x14 set16, \ previous
  label: ;
: update-previous-word ( -- ) latest name>int execute previousWord ! ;
: defword_custom_flags: ( flags str len "id" -- )
  2>r previousWord @ swap 2r> defword_custom:
  update-previous-word ;
: defword: ( str len "id" -- ) 
  2>r previousWord @ 0x0000 2r> defword_custom: 
  update-previous-word ;

label: 2push_
    xlower xsp push,
label: 1push_
    xtop xsp push,
label: next_
    xrp xtmp pop,
    xtmp rbranch,
: next, ( -- ) next_ branch, ;
: 1push, ( -- ) 1push_ branch, ;
: 2push, ( -- ) 2push_ branch, ;
    
   
s" dup" defword: dup_
    xsp xtop ld,
    1push,
s" over" defword: over_ ( a b -- a b a )
    2pop, \ xtop - b
          \ xlower - a 
    xlower xsp push,
    xtop xsp push,
    xlower xsp push,
    next,
: over; ( -- ) over_ bl, ;
: dup; ( -- ) dup_ bl, ;
s" @" defword: @_ 
    xsp xtop pop, \ get the address
    xtop xtop ld, \ load the value
    xtop xsp push,
    next,
s" !" defword: !_ 
    2pop, \ top -> address
          \ lower -> value
    xlower xtop st,
    next,
s" c@" defword: c@_
    xsp xtop pop, \ addr
    xtop xtop ld, 
    0x00FF xtop xtop andi, 
    xtop xsp push,
    next,
s" c!" defword: c!_ ( value addr -- )
    2pop, \ top -> addr
          \ lower -> value
    xtop xthird ld, \ load the full 16-bit value
    0x00FF xlower xlower andi, \ construct the lower 8 bits
    0xFF00 xthird xthird andi, \ clear the lower half out
    xlower xthird xlower or, \ combine the numbers together
    xlower xtop st, \ store the new value
    next,
s" =" defword: =_ ( a b -- f )
   2pop,
   xlower xtop xtop eq,
   1push,
: =, ( -- ) =_ bl, ;
s" or" defword: or_ ( a b -- c ) 
  2pop,
  xtop xlower xtop or,
  1push,
: or; ( -- ) or_ bl, ;
s" swap" defword: swap_ 
   2pop, 
   xtop xsp push,
   xlower xsp push, 
   next,
: swap; ( -- ) swap_ bl, ;
s" update-terminal" defword: update-terminal_ ( -- ) 
   xcolumn xrow setcur,
   next,
s" column@" defword: column@_ ( -- v )
   xcolumn xsp push,
   next,
: column@; ( -- ) column@_ bl, ;
s" row@" defword: row@_ ( -- v )
   xrow xsp push,
   next,
s" column!" defword: column!_ ( -- v )
   1pop,
   xtop xcolumn move,
   next,
: column!; ( -- ) column!_ bl, ;
s" row!" defword: row!_ ( -- v )
   1pop,
   xtop xrow move,
   next,
s" column1+" defword: column1+_ ( -- )
   xcolumn 1+,
   next,
s" row1+" defword: row1+_ ( -- ) 
   xrow 1+,
   next,
s" termpos!" defword: termpos!_ ( r c -- ) 
   column!;
   row!_ bl,
   update-terminal_ bl,
   next,
s" linestart!" defword: linestart!_ ( -- ) 
   0 literal, column!;
   update-terminal_ bl,
   next,
s" emit" defword: emit_ ( c -- ) 
    1pop,
    xtop emit,
    column1+_ bl,
    next,
: emiti; ( v -- ) literal, emit_ bl, ; 
s" key" defword: key_ ( -- v ) 
   xtop key,
   1push,
s" ?backspace" defword: ?backspace_ ( c -- f ) 
  0x7f literal, =,
  next,
: ?backspace; ( -- ) ?backspace_ bl, ;
s" ?line-feed" defword: ?line-feed_ ( c -- f )
  0xa literal, 
  =, 
  next,
s" ?carriage-return" defword: ?carriage-return_ ( c -- f )
  0xd literal, 
  =, 
  next,

s" ?newline" defword: ?newline_ ( c -- f )
  dup; ( c c )
  ?line-feed_ bl,
  swap;
  ?carriage-return_ bl,
  or;
  next,
: ?newline; ( -- ) ?newline_ bl, ;

s" next-input-line" defword: next-input-line_ ( -- ) 
   row1+_ bl,
   linestart!_ bl,
   next,
s" space" defword: space_ ( -- )
  0x20 emiti; 
  next,
s" cr" defword: cr_ ( -- )
  0xa emiti;
  next,
: space; ( -- ) space_ bl, ;
s" indent-input-line" defword: indent-input-line_ ( -- )
  0 literal, column!;
  space; space; space; space; 
  next,
s" <=" defword: <=_ ( a b -- f )
  2pop, \ xtop - b 
        \ xlower -a 
  xtop xlower xtop le,
  1push,
s" >=" defword: >=_ ( a b -- f )
  2pop, \ xtop - b 
        \ xlower -a 
  xtop xlower xtop ge,
  1push,
s" -" defword: -_ ( a b -- c )
  2pop,
  xtop xlower xtop sub,
  1push,
: -; ( -- ) -_ bl, ;
s" drop" defword: drop_ ( a -- )
   1pop,
   next,
: drop; ( -- ) drop_ bl, ;
s" +" defword: add_ ( a b -- v )
    2pop, 
    xtop xlower xtop add,
    1push,
: +; ( -- ) add_ bl, ;
s" 1+" defword: incr_ ( a -- v )
     1pop,
     xtop 1+,
     1push,
: 1+; ( -- ) incr_ bl, ;
s" input-stream@" defword: input-stream@_ ( -- a )
    xinput xsp push,
    next,
s" input-stream!" defword: input-stream!_ ( a -- )
    1pop, \ xtop -> value
          \ prevent us from jumping out of the input area

    0x00FF xtop xinput andi, \ we can only go so far
    next,
s" input-length@" defword: input-length@_ ( -- l )
    xlength xtop push,
    next,
s" input-length!" defword: input-length!_ ( v -- )
    1pop, \ xtop -> length
    0x00FF xtop xlength andi,
    next,

s" reset-input-stream" defword: reset-input-stream_
    input-buffer-start literal,
    input-stream!_ bl,
    0 literal,
    input-length!_ bl,
    next,
s" 0=" defword: 0=_ ( a -- f )
   0 literal,
   =, 
   next,
: 0=, ( -- ) 0=_ bl, ;
s" and" defword: and_ ( a b -- v )
  2pop,
  xtop xlower xtop and,
  1push,
: and; ( -- ) and_ bl, ;

s" backspace" defword: backspace_ ( -- )
  column@; 
  3 literal,
  <=_ bl, 
  if,
    \ fix the output if it is all cocked up
    indent-input-line_ bl,
  else,
    column@;
    4 literal, 
    =_ bl,
    \ otherwise we should just go back two spaces
    \ Because we are looking at the space _following_ the 
    \ last output character :D we have to clear out the previous character as 
    \ part of the delete process
    column@; 1 literal, -; column!; \ jump back two positions
    update-terminal_ bl, \ update the terminal position
    column@; \ stash a copy of where we currently are too :)
    space; \ emit a space to clear the position we are concerned about
    column@; 4 literal, =,
    if,
        drop;
    else,
        column!; \ move back now that the character is cleared
        update-terminal_ bl,
    then,
  then,
  next,
s" row-full?" defword: row-full?_ ( -- f )
    column@;
    &maxcolumn xtop set, 
    xtop xtop ld,
    xtop xsp push,
    =,
    next,
s" invert" defword: invert_ ( a -- b )
   1pop,
   xtop xtop invert,
   1push,
s" unstash-character" defword: unstash-char_ ( -- )
   xlength xsp push, \ go back a character with the length
   0=_ bl, \ check first if it is equal to zero
   invert_ bl, \ neq zero
   if, 
        \ it is not equal to zero so decrement it
        1 xlength xlength subi, \ go back a position
   then,
   next,

s" stash-character" defword: stash-char_ ( c -- )
   xinput xlength xtop add,
   xtop xsp push,
   c!_ bl,
   1 xlength xlength addi,
   next,
   
s" stash-character-to-buffer" defword: stash-char-to-buf_ ( c -- )
    row-full?_ bl,
    if,
        backspace_ bl,
        1 xlength xlength subi,
    then,
    dup_ bl,
    stash-char_ bl,
    emit_ bl,
    next,
s" 0=" defword: 0=_ ( a -- f )
   0 literal, 
   =, 
   next,
s" >" defword: >_ ( a b -- f )
   2pop, 
   xtop xlower xtop gt,
   1push,
s" 0>" defword: 0>_ ( a -- f )
   0 literal, 
   >_ bl, 
   next,
: 0>; ( -- ) 0>_ bl, ;
s" <>" defword: <>_ ( a b -- f )
   2pop,
   xtop xlower xtop neq,
   1push,
: <>; ( -- ) <>_ bl, ;
s" print-input" defword: print-input_ ( -- ) 
  xinput xlength xtop add,
  xtop xsp push, ( stop )
  xinput xsp push, ( stop pos )
  begin,
    over; over; <>;
  while,
    dup; 
    c@_ bl, 
    dup;
    0 literal,
    <>;
    if,
        emit_ bl,
    else,
        drop;
    then,
    1+;
  repeat,
  next,
    
s" invoke-input-newline" defword: invoke-input-newline_ ( -- )
  next-input-line_ bl,
  print-input_ bl,
  next-input-line_ bl,
  indent-input-line_ bl,
  reset-input-stream_ bl,
  next,

s" interpreter" defword: interpreter_ ( -- )
    indent-input-line_ bl,
    begin,
        key_ bl, \ read a character in
        dup;
        ?backspace; 
        if,
            drop; 
            backspace_ bl,
            unstash-char_ bl,
        else,
            dup; 
            ?newline;
            if,
                drop;
                invoke-input-newline_ bl,
            else,
                stash-char-to-buf_ bl, 
                \ stash a copy to the input buffer
            then,
        then,
    again,
s" cold" defword: cold_
    \ reset/set the stacks
    data-stack-start xsp set,
    return-stack-start xrp set,
    &bootkind literal, @_ bl, \ coldboot?
	if, 
    	0 xtop set,
    	&bootkind xlower set,
    	xtop xlower st,
        &dp xlower set,
        cold_ xtop set,
        xtop xlower st, \ setup the initial dictionary pointer
        xrow xcolumn tdims, \ get the dimensions
        &maxrow xtop set, 
        xrow xtop st,
        &maxcolumn xtop set,
        xcolumn xtop st, 
        clrscr,
        0 xrow set,
        0 xcolumn set,
	then,
    input-buffer-start xinput set,
    0 xlength set,
    interpreter_ branch,
0x0000 .org
\ we should not put anything in this area as it could be useful for other things :D
cold_ branch,

: run ( -- ) memory_base &bootkind + @ addr16 0= if cold_ pc! execute-core else execute-core endif ;

