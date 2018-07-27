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

: dup, ( -- ) 
  xsp xtop ld,
  xtop xsp push, ;
: over, ( -- )
  2 xsp xtop addi,
  xtop xtop ld,
  xtop xsp pushi, ;
: swap, ( -- )
  xsp xtop pop,
  xsp xlower pop,
  xtop xsp push,
  xlower xsp push, ;

: rot, ( -- ) 
  \ a b c -- b c a 
  xsp xtop pop,  \ c
  xsp xlower pop, \ b 
  xsp xthird pop, \ a
  xlower xsp push, 
  xtop xsp push,
  xthird xsp push, ;
: 2pop, ( -- )
    xsp xtop pop,
    xsp xlower pop, ;
: literal, ( imm -- ) xsp pushi, ;
: drop, ( -- ) xsp xtop pop, ; 
: 2drop, ( -- ) drop, drop, ;
: next, ( -- ) xrp return, ;
: bl, ( imm -- ) xrp call, ;
0x0100 constant variables-start
0x0200 constant dictionary-start
dictionary-start .org
label: +_
   2pop,
   xtop xlower xtop add,
   xtop xsp push,
   next,
label: swap_ 
   swap, 
   next,
label: over_ 
   over, 
   next, 
label: drop_
    drop,
    next,
label: dup_
    dup,
    next,
label: rot_
    rot,
    next,
label: bye_ 
    0 stopi,
label: load_
    xsp xtop pop, \ get the address
    xtop xtop ld, \ load the value
    xtop xsp push,
    next,
label: >r_ \ transfer to the return stack from data stack
    \ todo verify that we are not empty
    xsp xlower pop, \ pull the top off of the stack
    xrp xtop pop, \ pull the top of the return stack off as well
    xlower xrp push,  \ stash the value we want _under_ the top so we don't do bad things to the stack
    xtop xrp push, \ push the return address
    next,
label: 2dup_ 
    over_ bl,
    over_ bl,
    next,
label: -rot_
    rot_ bl,
    rot_ bl,
    next,
label: interpreter_
variables-start .org
    interpreter_ word,  \ first variable
0x0000 .org
\ setup the stacks
data-stack-start xsp set,
return-stack-start xrp set,
variables-start literal,
load_ bl,
xsp xtop pop,
xtop xrp push,
next, \ jump out of the bootstrap area
variables-start xtop ldi, \ get the interpreter start location


