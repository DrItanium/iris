\ simple iris disassembler for gforth
\ according to the documentation this should be done first before other actions
get-current 
vocabulary disassembler
also disassembler definitions

\ instruction fields
: disasm-op ( w -- u )
  \ lowest eight bits
  0xFF and ;
: disasm-register ( w shift -- u ) rshift 0x0F and ;
: disasm-rdest ( w -- u )  8 disasm-register ;
: disasm-rsrc ( w -- u )  12 disasm-register ;
: disasm-rsrc2 ( w -- u ) 16 disasm-register ;
: disasm-rsrc3 ( w -- u ) 20 disasm-register ;
: disasm-imm16 ( w -- u ) 16 rshift 0xFFFF and ;
: disasm-imm16-noreg ( w -- u ) 12 rshift 0xFFFF and ;
