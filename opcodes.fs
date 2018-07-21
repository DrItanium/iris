{opcode 
(       name      operation           decoder            )
opcode: #illegal  illegal-instruction illegal-instruction
opcode: #add      add;                decode-3reg
opcode: #sub      sub;                decode-3reg
opcode: #mul      mul;                decode-3reg
opcode: #div      div;                decode-3reg
opcode: #rem      rem;                decode-3reg
opcode: #lshift   lshift;             decode-3reg
opcode: #rshift   rshift;             decode-3reg
opcode: #and      and;                decode-3reg
opcode: #or       or;                 decode-3reg
opcode: #invert   invert;             decode-2reg
opcode: #xor      xor;                decode-3reg
opcode: #min      min;                decode-3reg
opcode: #max      max;                decode-3reg
opcode: #eq       eq;                 decode-3reg
opcode: #neq      neq;                decode-3reg
opcode: #lt       lt;                 decode-3reg
opcode: #gt       gt;                 decode-3reg
opcode: #le       le;                 decode-3reg
opcode: #ge       ge;                 decode-3reg
opcode: #set      set;                decode-1reg-imm16
opcode: #ld       ld;                 decode-2reg
opcode: #st       st;                 decode-2reg
opcode: #push     push;               decode-2reg
opcode: #pop      pop;                decode-2reg
opcode: #br       rbranch;            decode-1reg
opcode: #brl      rbranch-link;       decode-2reg
opcode: #bcr      ?rbranch;           decode-2reg
opcode: #bcrl     ?rbranch-link;      decode-3reg
opcode: #ueq      ueq;                decode-3reg
opcode: #uneq     uneq;               decode-3reg
opcode: #ult      ult;                decode-3reg
opcode: #ugt      ugt;                decode-3reg
opcode: #ule      ule;                decode-3reg
opcode: #uge      uge;                decode-3reg
opcode: #uand     and;                decode-3reg
opcode: #uor      or;                 decode-3reg
opcode: #uinvert  invert;             decode-2reg
opcode: #uxor     xor;                decode-3reg
opcode: #umin     umin;               decode-3reg
opcode: #umax     umax;               decode-3reg
opcode: #uadd     add;                decode-3reg
opcode: #usub     sub;                decode-3reg
opcode: #umul     mul;                decode-3reg
opcode: #udiv     div;                decode-3reg
opcode: #urem     rem;                decode-3reg
opcode: #ulshift  lshift;             decode-3reg
opcode: #urshift  rshift;             decode-3reg
opcode: #incr     1+;                 decode-2reg
opcode: #decr     1-;                 decode-2reg
opcode: #uincr    1+;                 decode-2reg
opcode: #udecr    1-;                 decode-2reg
opcode: #call     call;               decode-1reg-imm16
opcode: #condb    ?branch;            decode-1reg-imm16
opcode: #addi     addi;               decode-2reg-imm16
opcode: #subi     subi;               decode-2reg-imm16
opcode: #rshifti  rshifti;            decode-2reg-imm16
opcode: #lshifti  lshifti;            decode-2reg-imm16
opcode: #ldtincr  ldtincr;            decode-2reg
opcode: #lti      lti;                decode-2reg-imm16
opcode: #move     move;               decode-2reg
opcode: #sttincr  sttincr;            decode-2reg
opcode: #addw     addw;               decode-wide-3reg
opcode: #subw     subw;               decode-wide-3reg
opcode: #pushw    pushw;              decode-wide-2reg
opcode: #popw     popw;               decode-wide-2reg
opcode: #return   return;             decode-1reg
opcode: #creturn  ?return;            decode-2reg
opcode: #invertw  invertw;            decode-wide-2reg
opcode: #bi       branch;             decode-imm16
opcode: #eqz      eqz;                decode-2reg
opcode: #neqz     neqz;               decode-2reg
opcode: #ltz      ltz;                decode-2reg
opcode: #gtz      gtz;                decode-2reg
opcode: #lez      lez;                decode-2reg
opcode: #gez      gez;                decode-2reg
opcode: #andi     andi;               decode-2reg-imm16
opcode: #uandi    andi;               decode-2reg-imm16
opcode: #muli     muli;               decode-2reg-imm16
opcode: #divi     divi;               decode-2reg-imm16
opcode: #pushi    pushi;              decode-1reg-imm16
opcode: #memincr  memincr;            decode-1reg
opcode: #memdecr  memdecr;            decode-1reg
opcode: #stop     stop;               decode-1reg \ stop execution
opcode}
