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
: defstackbinaryop ( "name" "op" -- )
  create ' , 
  does>
  >r
  2pop,
  xtop xlower xtop r> @ execute 
  xtop xsp push, ;
defstackbinaryop +, add,
defstackbinaryop -, sub, 
defstackbinaryop *, mul, 
: -rot, ( -- ) rot, rot, ;
: 2dup, ( -- ) over, over, ;

