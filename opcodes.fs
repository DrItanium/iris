{opcode 
' illegal-instruction ' illegal-instruction opcode: #illegal  
' add;           opcode3: #add
' sub;           opcode3: #sub      
' mul;           opcode3: #mul
' div;           opcode3: #div
' rem;           opcode3: #rem
' lshift;        opcode3: #lshift   
' rshift;        opcode3: #rshift   
' and;           opcode3: #and      
' or;            opcode3: #or       
' invert;        opcode2: #invert   
' xor;           opcode3: #xor      
' min;           opcode3: #min      
' max;           opcode3: #max      
' eq;            opcode3: #eq       
' neq;           opcode3: #neq      
' lt;            opcode3: #lt       
' gt;            opcode3: #gt       
' le;            opcode3: #le       
' ge;            opcode3: #ge       
' set;           opcode1i16: #set      
' ld;            opcode2: #ld
' st;            opcode2: #st
' push;          opcode2: #push     
' pop;           opcode2: #pop      
' rbranch;       opcode1: #br       
' rbranch-link;  opcode2: #brl      
' ?rbranch;      opcode2: #bcr      
' ?rbranch-link; opcode3: #bcrl     
' ueq;           opcode3: #ueq      
' uneq;          opcode3: #uneq     
' ult;           opcode3: #ult      
' ugt;           opcode3: #ugt      
' ule;           opcode3: #ule      
' uge;           opcode3: #uge      
' and;           opcode3: #uand     
' or;            opcode3: #uor      
' invert;        opcode2: #uinvert  
' xor;           opcode3: #uxor     
' umin;          opcode3: #umin     
' umax;          opcode3: #umax     
' add;           opcode3: #uadd     
' sub;           opcode3: #usub     
' mul;           opcode3: #umul     
' div;           opcode3: #udiv     
' rem;           opcode3: #urem     
' lshift;        opcode3: #ulshift  
' rshift;        opcode3: #urshift  
' 1+;            opcode2: #incr     
' 1-;            opcode2: #decr     
' 1+;            opcode2: #uincr    
' 1-;            opcode2: #udecr    
' call;          opcode1i16: #call     
' ?branch;       opcode1i16: #condb    
' addi;          opcode2i16: #addi     
' subi;          opcode2i16: #subi     
' rshifti;       opcode2i16: #rshifti  
' lshifti;       opcode2i16: #lshifti  
' ldtincr;       opcode2: #ldtincr  
' lti;           opcode2i16: #lti      
' move;          opcode2: #move     
' sttincr;       opcode2: #sttincr  
' addw;          opcode3w: #addw     
' subw;          opcode3w: #subw     
' pushw;         opcode2w: #pushw    
' popw;          opcode2w: #popw     
' return;        opcode1: #return   
' ?return;       opcode2: #creturn  
' invertw;       opcode2w: #invertw  
' branch;        opcodei16: #bi       
' eqz;           opcode2: #eqz      
' neqz;          opcode2: #neqz     
' ltz;           opcode2: #ltz      
' gtz;           opcode2: #gtz      
' lez;           opcode2: #lez      
' gez;           opcode2: #gez      
' andi;          opcode2i16: #andi     
' andi;          opcode2i16: #uandi    
' muli;          opcode2i16: #muli     
' divi;          opcode2i16: #divi     
' pushi;         opcode1i16: #pushi    
' memincr;       opcode1: #memincr  
' memdecr;       opcode1: #memdecr  
' stop;          opcode1: #stop     \ stop execution
opcode}
