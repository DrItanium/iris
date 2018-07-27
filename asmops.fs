\ iris
\ Copyright (c) 2013-2018, Joshua Scoggins and Contributors
\ All rights reserved.
\ 
\ Redistribution and use in source and binary forms, with or without
\ modification, are permitted provided that the following conditions are met:
\     * Redistributions of source code must retain the above copyright
\       notice, this list of conditions and the following disclaimer.
\     * Redistributions in binary form must reproduce the above copyright
\       notice, this list of conditions and the following disclaimer in the
\       documentation and/or other materials provided with the distribution.
\ 
\ THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
\ ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
\ WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
\ DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
\ ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
\ (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
\ LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
\ ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
\ (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
\ SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#illegal  asm0: illegal,
#add      asm3: add,
#sub      asm3: sub,
#mul      asm3: mul,
#div      asm3: div,
#rem      asm3: rem,
#lshift   asm3: lshift,             
#rshift   asm3: rshift,             
#and      asm3: and,                
#or       asm3: or,                 
#invert   asm2: invert,             
#xor      asm3: xor,                
#min      asm3: min,                
#max      asm3: max,                
#eq       asm3: eq,                 
#neq      asm3: neq,                
#lt       asm3: lt,                 
#gt       asm3: gt,                 
#le       asm3: le,                 
#ge       asm3: ge,                
#incr     asm2: incr,               
#decr     asm2: decr,               
#set      asm1i16: set,                
#ld       asm2: ld,                 
#st       asm2: st,                
#push     asm2: push,               
#pop      asm2: pop,                
#br       asm1: rbranch,            
#brl      asm2: rbranch-link,       
#bcr      asm2: ?rbranch,           
#bcrl     asm3: ?rbranch-link,      
#ueq      asm3: ueq,                
#uneq     asm3: uneq,               
#ult      asm3: ult,                
#ugt      asm3: ugt,                
#ule      asm3: ule,                
#uge      asm3: uge,                
#umin     asm3: umin,               
#umax     asm3: umax,               
#call     asm1i16: call,
#condb    asm1i16: ?branch,
#addi     asm2i16: addi,
#subi     asm2i16: subi,
#rshifti  asm2i16: rshifti,
#lshifti  asm2i16: lshifti,
#ldtincr  asm2: ldtincr,           
#lti      asm2i16: lti,            
#move     asm2: move,              
#sttincr  asm2: sttincr,           
#addw     asm3: addw,              
#subw     asm3: subw,              
#pushw    asm2: pushw,             
#popw     asm2: popw,              
#invertw  asm2: invertw,           
#bi       asmi16: branch,          
#eqz      asm2: eqz,
#neqz     asm2: neqz,
#ltz      asm2: ltz,
#gtz      asm2: gtz,
#lez      asm2: lez,
#gez      asm2: gez,
#andi     asm2i16: andi,
#muli     asm2i16: muli,
#divi     asm2i16: divi,
#pushi    asm1i16: pushi,
#memincr  asm1: memincr,           
#memdecr  asm1: memdecr,           
#stop     asm1: stop,              
#set4     asm1i4: set4,
#set8     asm1i8: set8,
#pushi4   asm1i4: pushi4,
#pushi8   asm1i8: pushi8,

