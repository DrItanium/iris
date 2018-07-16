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


: {opcode ( -- 0 ) 0 ;
: opcode: dup addr8 constant 1+ ;
: opcode} ( n -- ) drop ;
{opcode
opcode: #illegal,
opcode: #add 
opcode: #sub 
opcode: #mul 
opcode: #div 
opcode: #rem 
opcode: #lshift
opcode: #rshift
opcode: #and 
opcode: #or 
opcode: #negate
opcode: #xor 
opcode: #min
opcode: #max
opcode: #eq
opcode: #neq
opcode: #lt
opcode: #gt
opcode: #le
opcode: #ge
opcode: #set
opcode: #ld
opcode: #st
opcode: #push
opcode: #pop
opcode: #br
opcode: #brl
opcode: #bcr
opcode: #bcrl
opcode: #ueq
opcode: #uneq
opcode: #ult
opcode: #ugt
opcode: #ule
opcode: #uge
opcode: #uand
opcode: #uor
opcode: #unegate
opcode: #uxor
opcode: #umin
opcode: #umax
opcode: #uadd
opcode: #usub
opcode: #umul
opcode: #udiv
opcode: #urem
opcode: #ulshift
opcode: #urshift
opcode: #incr
opcode: #decr
opcode: #uincr
opcode: #udecr
opcode: #call
opcode: #condb
opcode: #addi
opcode: #subi
opcode: #rshifti
opcode: #lshifti
opcode: #ldtincr
opcode: #lti
opcode: #move
opcode: #sttincr
opcode: #addw
opcode: #subw
opcode: #pushw
opcode: #popw
opcode: #return
opcode: #creturn
opcode: #negatew
opcode: #umsmod
opcode: #msmod
opcode: #umstar
opcode: #mstar
opcode: #stw
opcode: #ldw
opcode: #ldbu
opcode: #stbu
opcode: #ldbl
opcode: #stbl
opcode: #setb
opcode: #bi
opcode: #eqz
opcode: #neqz
opcode: #ltz
opcode: #gtz
opcode: #lez
opcode: #gez
opcode: #andi
opcode: #uandi
opcode: #muli
opcode: #divi
opcode: #pushi
opcode: #memincr
opcode: #memdecr
opcode}

