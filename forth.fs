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

x2 constant wtop
x4 constant wlower

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
0x0200 constant dictionary-start
dictionary-start .org
label: next_
    xrp xtmp pop,
    xtmp rbranch,
: next, ( -- ) next_ branch, ;
label: -_
   2pop,
   xtop xlower xtop sub,
   xtop xsp push,
   next,
label: +_
   2pop,
   xtop xlower xtop add,
   xtop xsp push,
   next,
label: 1+_
   1pop, 
   xtop xtop incr, 
   xtop xsp push,
   next,
label: 2+_
    1pop,
    2 xtop xtop addi,
    xtop xsp push,
    next,
label: swap_ 
   2pop, 
   xtop xsp push,
   xlower xsp push, 
   next,
label: over_ 
   2 xsp xtop addi,
   xtop xtop ld,
   xtop xsp push, 
   next, 
label: drop_
    xsp xtop pop,
    next,
label: 2drop_
    2pop,
    next,
label: dup_
    xsp xtop ld,
    xtop xsp push,
    next,
label: rot_
    xsp xtop pop,  \ c
    xsp xlower pop, \ b 
    xsp xthird pop, \ a
    xlower xsp push, 
    xtop xsp push,
    xthird xsp push,
    next,
label: bye_
    0 stopi,
label: @_ 
    xsp xtop pop, \ get the address
    xtop xtop ld, \ load the value
    xtop xsp push,
    next,
label: !_ 
    2pop, \ top -> address
          \ lower -> value
    xlower xtop st,
    next,
label: c@_
    xsp xtop pop, \ addr
    xtop xtop ld, 
    0x00FF xtop xtop andi, 
    xtop xsp push,
    next,
label: c!_ ( value addr -- )
    2pop, \ top -> addr
          \ lower -> value
    xtop xthird ld, \ load the full 16-bit value
    0x00FF xlower xlower andi, \ construct the lower 8 bits
    0xFF00 xthird xthird andi, \ clear the lower half out
    xlower xthird xlower or, \ combine the numbers together
    xlower xtop st, \ store the new value
    next,

label: >r_ \ transfer to the return stack from data stack
    \ todo verify that the parameter stack is not empty
    xsp xlower pop, \ pull the top off of the stack
    xrp xtop pop, \ pull the top of the return stack off as well
    xlower xrp push,  \ stash the value we want _under_ the top so we don't do bad things to the stack
    xtop rbranch, \ go back to where we were

label: r>_ 
    xrp xtop pop, \ the return from this function
    xrp xlower pop, \ the value we want
    xlower xsp push,
    xtop rbranch,
label: 2dup_  ( a b -- a b a b )
    xsp xtop pop, \ b
    xsp xlower pop, \ a
    xlower xsp push,
    xtop xsp push,
    xlower xsp push,
    xtop xsp push,
    next,
label: -rot_
    rot_ bl,
    rot_ bl,
    next,
label: 1=_
label: ?exec_
    2pop, \ top - flag
          \ lower - addr to jump to
    xrp xtop xlower ?rbranch-link,
    next,
label: newline_ ( -- )
    0xa emiti, 
    0xb emiti,
    next,
label: space_ ( -- ) 
    0x20 emiti, 
    next, 
label: spaces_ ( n -- ) 
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
label: ok_ ( -- ) 
    0x6f emiti,
    0x6b emiti, 
    newline_ bl,
    next,
label: bootmessage_ ( -- ) 
    0x69 emiti, 0x72 emiti, 0x69 emiti, 0x73 emiti, 
    space_ bl,
    0x66 emiti, 0x6f emiti, 0x72 emiti, 0x74 emiti, 0x68 emiti,
    newline_ bl,
    next, 

label: interpreter_
    newline_ bl, newline_ bl,
    bootmessage_ bl,
    ok_ bl,
    bye_ branch,
label: initcold_
    0 xtop set,
    variables-start 2+ xlower set,
    xtop xlower st,
    next, 
label: cold_
    \ reset/set the stacks
    data-stack-start xsp set,
    return-stack-start xrp set,
    initcold_ literal,
    variables-start 2+ literal, @_ bl, \ coldboot?
    ?exec_ bl, \ see if we should do an initial cold boot or not
    interpreter_ branch,
variables-start .org
    interpreter_ word,  \ first variable, interpreter location, this is fixed in memory
    0xFFFF word,             \ we are doing cold or warm boot?
0x0000 .org
\ we should not put anything in this area as it could be useful for other things :D
cold_ branch,


