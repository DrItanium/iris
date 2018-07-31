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

label: next_
    xrp xtmp pop,
    xtmp rbranch,
: next, ( -- ) next_ branch, ;
s" swap" defword: swap_ 
   2pop, 
   xtop xsp push,
   xlower xsp push, 
   next,
s" over" defword: over_ 
   2 xsp xtop addi,
   xtop xtop ld,
   xtop xsp push, 
   next, 
s" drop" defword: drop_
    xsp xtop pop,
    next,
: drop, ( -- ) drop_ bl, ;
s" 2drop" defword: 2drop_
    2pop,
    next,
s" dup" defword: dup_
    xsp xtop ld,
    xtop xsp push,
    next,
: dup, ( -- ) dup_ bl, ;
s" rot" defword: rot_
    xsp xtop pop,  \ c
    xsp xlower pop, \ b 
    xsp xthird pop, \ a
    xlower xsp push, 
    xtop xsp push,
    xthird xsp push,
    next,
s" -" defword: -_
   2pop,
   xtop xlower xtop sub,
   xtop xsp push,
   next,
s" +" defword: +_
   2pop,
   xtop xlower xtop add,
   xtop xsp push,
   next,
s" *" defword: *_
	2pop, 
	xtop xlower xtop mul,
	xtop xsp push,
	next,
s" 1+" defword: 1+_
   1pop, 
   xtop xtop incr, 
   xtop xsp push,
   next,
s" 2+" defword: 2+_
    1pop,
    2 xtop xtop addi,
    xtop xsp push,
    next,
s" invert" defword: invert_ ( n -- n )
   1pop,
   xtop xtop invert,
   xtop xsp push,
   next,
s" =" defword: =_ ( a b -- f )
   2pop,
   xlower xtop xtop eq,
   xtop xsp push,
   next,
: =, ( -- ) =_ bl, ;
s" 0=" defword: 0=_ ( v -- f )
   0 literal, 
   =,
   next,
: 0=, ( -- ) 0=_ bl, ;
s" rshift" defword: rshift_ ( n d -- v )
   2pop,
   xtop xlower xtop rshift,
   xtop xsp push,
   next,
: rshift,, ( -- ) rshift_ bl, ;
s" lshift" defword: lshift_ ( n d -- v )
	2pop, 
	xtop xlower xtop lshift,
	xtop xsp push,
	next,
: lshift,, ( -- ) lshift_ bl, ;
s" /" defword: div_ ( n d -- v ) 
   dup, ( n d d ) 
   0=, ( n d f )
   if, 
	2pop,
	0 xsp pushi, 
   else,
	2pop,
	xtop xlower xtop div,
	xtop xsp push,
   then,
   next,
s" 2*" defword: 2*_ ( a -- b )
  1pop, 
  1 xtop xtop lshift,
  xtop xsp push,
  next,
s" 2/" defword: 2/_ ( a -- b )
  1pop,
  1 xtop xtop rshift, 
  xtop xsp push,
  next,

s" mod" defword: mod_ ( n d -- v )
  dup_ bl, ( n d d )
  0=_ bl, ( n d f )
  if,
  	2pop,
	0 xsp pushi,
  else,
	2pop,
	xtop xlower xtop rem,
	xtop xsp push,
  then,
  next, 
s" bye" defword: bye_
    0 stopi,
s" @" defword: @_ 
    xsp xtop pop, \ get the address
    xtop xtop ld, \ load the value
    xtop xsp push,
    next,
s" dp" defword: dp_ ( -- v )
   &dp xtop set,
   xtop xsp push,
   next,
s" here" defword: here_ ( -- v ) dp_ bl, @_ bl, next,
s" create" defword: create_ 
    
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

s" >r" defword: >r_ \ transfer to the return stack from data stack
    \ todo verify that the parameter stack is not empty
    xsp xlower pop, \ pull the top off of the stack
    xrp xtop pop, \ pull the top of the return stack off as well
    xlower xrp push,  \ stash the value we want _under_ the top so we don't do bad things to the stack
    xtop rbranch, \ go back to where we were

s" r>" defword: r>_ 
    xrp xtop pop, \ the return from this function
    xrp xlower pop, \ the value we want
    xlower xsp push,
    xtop rbranch,
s" 2dup" defword: 2dup_  ( a b -- a b a b )
    xsp xtop pop, \ b
    xsp xlower pop, \ a
    xlower xsp push,
    xtop xsp push,
    xlower xsp push,
    xtop xsp push,
    next,
: 2dup, ( -- ) 2dup_ bl, ;
s" -rot" defword: -rot_
    rot_ bl,
    rot_ bl,
    next,
s" ?exec" defword: ?exec_
    2pop, \ top - flag
          \ lower - addr to jump to
    xrp xtop xlower ?rbranch-link,
    next,
s" cr" defword: newline_ ( -- )
    0xa emiti, 
    0xb emiti,
    next,
s" space" defword: space_ ( -- ) 
    0x20 emiti, 
    next, 
s" spaces" defword: spaces_ ( n -- ) 
    1pop, \ xtop count
    0 xthird set,
label: spaces_loop_
    xtop xsp push,
    space_ bl,
    xsp xtop pop,
    xthird xtop xlower gt,
    xtop xtop decr,
    spaces_loop_ xlower ?branch,
    next,
s" ?even" defword: ?even_ ( v -- f )
	1pop, 
	0x1 xlower set,
	0x0 xthird set,
	xlower xtop xtop and,
	xthird xtop xtop neq, 
	xtop xsp push,
	next,
s" ?odd" defword: ?odd_ ( v -- f )
	?even_ bl,
	invert_ bl,
	next,

s" &hashlower" defword: &hashlower_ 
    \ extract the lower hash 
    1pop, 
    18 xtop xtop subi,
    xtop xsp push,
    next,
s" &hashmiddle" defword: &hashmiddle_
    1pop,
    14 xtop xtop subi,
    xtop xsp push,
    next,
s" &hashupper" defword: &hashupper_
    \ extract the flags field 
    ( addr -- &hashupper )
    1pop, 
    10 xtop xtop subi,
    xtop xsp push,
    next,
s" extract-name-hash" defword: extract_name_hash_
    ( addr -- hu hm hl )
    dup_ bl, 
    &hashupper_ bl, 
    @_ bl,
    swap_ bl, 
    dup_ bl,
    &hashmiddle_ bl, 
    @_ bl, 
    swap_ bl,
    &hashlower_ bl,
    @_ bl, 
    next,

s" &flags" defword: flags_
    \ extract the flags field 
    ( addr -- &flags )
    1pop, 
    6 xtop xtop subi,
    xtop xsp push,
    next,
s" &previous" defword: getprevious_
    \ exract the previous field
    ( addr -- &prev )
    1pop, 
    2 xtop xtop subi,
    xtop xsp push,
    next,

s" dook" defword: ok_ ( -- ) 
    0x6f emiti,
    0x6b emiti, 
    newline_ bl,
    next,
s" dobootmessage" defword: bootmessage_ ( -- ) 
    0x69 emiti, 0x72 emiti, 0x69 emiti, 0x73 emiti, 
    space_ bl,
    0x66 emiti, 0x6f emiti, 0x72 emiti, 0x74 emiti, 0x68 emiti,
    newline_ bl,
    next, 
label: reset_delimiter_ 
    variables-start 2+ 2+ xtop set, 
    0x20 xlower set,
    xlower xtop st,
    next,
label: reset_base_
    &base xtop set, 
    0x0a xlower set,
    xlower xtop st,
    next,
s" ?key" defword: ?key_ ( -- f )
   xtop ?key,
   xtop xsp push,
   next,
s" key" defword: key_ ( -- v ) 
   xtop key,
   xtop xsp push,
   next,
s" emit" defword: emit_
   1pop,
   xtop emit,
   next,
s" <=" defword <=_ ( a b -- f )
  2pop,  \ xtop - b 
         \ xlower - a
  xtop xlower xtop le,
  xtop xsp push,
  next,
s" read-key" defword: read-key_ ( -- )
  key_ bl,
  dup,
  0x7f literal,
  =_ bl, 
  if,
    
  else, 
    emit_ bl, \ print out the character to the display
    dup, 
    input-buffer-end literal,
    <=_ bl,
    if,
      1pop,     \ stash the output to the input buffer
      xtop xinput stb,
      xinput 1+,
    else,
    then,
  then,
  next,
    


s" interpreter" defword: interpreter_
	begin,
        ?key_ bl,
        if,
            key_ bl,
            dup,
            emit_ bl,
            1pop, 
            xtop xinput stb,
            xinput 1+,
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
    	newline_ bl, newline_ bl,
    	bootmessage_ bl,
        &dp xlower set,
        cold_ xtop set,
        xtop xlower st, \ setup the initial dictionary pointer
	then,
    input-buffer-start xinput set,
    reset_delimiter_ bl,
	reset_base_ bl,
    interpreter_ branch,
0x0000 .org
\ we should not put anything in this area as it could be useful for other things :D
cold_ branch,

: run ( -- ) memory_base &bootkind + @ addr16 0= if cold_ pc! execute-core else execute-core endif ;

