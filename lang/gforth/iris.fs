include enum.fs
get-current ( wid )
vocabulary iris also iris definitions
: addr-mask ( mask "name" -- )
  CREATE , 
  does> @ and ;
0x3f addr-mask addr6
0xFFFF addr-mask addr16
0x0FFF addr-mask addr12
: reg-pos ( shift "name" -- ) 
  create , 
  does> swap addr6 swap @ lshift ;
8 reg-pos dest-reg
14 reg-pos src-reg
20 reg-pos src2-reg
26 reg-pos src3-reg
: 1reg ( dest -- value ) dest-reg ;
: 2reg ( src dest -- value ) 1reg swap src-reg or ;
: 3reg ( src2 src dest -- value ) 2reg swap src2-reg or ;
: 4reg ( src3 src2 src dest -- value ) 3reg swap src3-reg or ;
: imm16 ( imm16 -- value ) addr16 16 lshift ;
: imm12 ( imm12 -- value ) addr12 20 lshift ;
: imm6 ( imm6 -- value ) addr6 26 lshift ;

: 1reg-with-imm ( imm16 dest -- value )
  1reg swap imm16 or ;
: 2reg-with-imm ( imm12 src dest -- value )
  2reg swap imm12 or ;
: 3reg-with-imm ( imm6 src2 src dest -- value )
  3reg swap imm6 or ;
: cconstant ( byte "name" -- ) create c, does> c@ ;
: inst-0reg ( opcode-index "name" -- ) 
  create c, \ embed opcode
  does> c@ 0 and ;
: >xop& ( n -- k ) r> c@ and ;
: inst-1reg ( opcode-index "name" -- )
  create c, \ embed opcode
  does> >r 
        1reg
        >xop& ;
: inst-2reg ( opcode-index "name" -- )
  create c, \ embed opcode
  does> >r
        2reg
        >xop& ;
: inst-3reg ( opcode-index "name" -- )
  create c, \ embed opcode
  does> >r
        3reg
        >xop& ;
: inst-4reg ( opcode-index "name" -- )
  create c, \ embed opcode
  does> >r
        4reg
        >xop& ;
: inst-1reg-with-imm ( opcode-index "name" -- )
  create c, \ embed opcode
  does> >r
        1reg-with-imm
        >xop& ;
: inst-imm16 ( opcode-index "name" -- )
  create c, \ embed opcode
  does> >r
        imm16
        >xop& ;
: next-op ( n -- n+1 n ) dup 1+ swap ;
: :inst-0reg ( n -- k ) next-op inst-0reg ;
: :inst-1reg ( n -- k ) next-op inst-1reg ;
: :inst-2reg ( n -- k ) next-op inst-2reg ;
: :inst-3reg ( n -- k ) next-op inst-3reg ;
: :inst-4reg ( n -- k ) next-op inst-4reg ;
: :inst-1reg-with-imm ( n -- k ) next-op inst-1reg-with-imm ;
: :inst-imm16 ( n -- k ) next-op inst-imm16 ;


\ registers
0 cconstant reg-zero
1 cconstant reg-error-code
2 cconstant reg-terminator
3 cconstant reg-num-base
4 cconstant reg-sp0
5 cconstant reg-sp1
64 cconstant reg-count
set-current \ go back

0 :inst-3reg add, 
:inst-3reg sub, 
:inst-3reg mul, 
:inst-3reg div, 
:inst-3reg rem, 
:inst-3reg shl, 
:inst-3reg shr, 
:inst-3reg and, 
:inst-3reg or, 
:inst-2reg not, 
:inst-3reg xor, 
:inst-3reg nand, 
:inst-3reg nor,
:inst-3reg min,
:inst-3reg max,
:inst-3reg lxor,
:inst-2reg lnot,
:inst-3reg land,
:inst-3reg lor,
:inst-3reg lnand,
:inst-3reg lnor,
:inst-3reg eq,
:inst-3reg neq,
:inst-3reg lt,
:inst-3reg gt,
:inst-3reg le,
:inst-3reg ge,
:inst-1reg-with-imm set,
:inst-2reg ld,
:inst-2reg st,
:inst-2reg push,
:inst-2reg pop,
:inst-2reg ld.core,
:inst-2reg st.core,
:inst-imm16 b,
:inst-1reg br,
:inst-2reg brl,
:inst-1reg-with-imm bc,
:inst-2reg bcr,
:inst-3reg bcrl,
:inst-3reg if,
:inst-4reg ifl,
:inst-1reg terminateExecution,
:inst-2reg ldio,
:inst-2reg stio,
:inst-3reg ueq,
:inst-3reg uneq,
:inst-3reg ult,
:inst-3reg ugt,
:inst-3reg ule,
:inst-3reg uge,
:inst-3reg uand,
:inst-3reg uor,
:inst-2reg unot,
:inst-3reg uxor,
:inst-3reg unand,
:inst-3reg unor,
:inst-3reg umin,
:inst-3reg umax,
:inst-3reg uadd,
:inst-3reg usub,
:inst-3reg umul,
:inst-3reg udiv,
:inst-3reg urem,
:inst-3reg ushl,
:inst-3reg ushr,
:inst-4reg choose,
:inst-4reg schoose,
:inst-2reg readtok,
:inst-3reg number,
drop

previous

