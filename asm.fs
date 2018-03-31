( assembler words )
: mask-immediate16 ( value -- imm16 ) 0xFFFF bitwise-andu ;
: mask-imm8 ( reg -- masked-reg ) 0xFF bitwise-andu ;
: position-byte ( reg shift -- reg<<shift ) 
  swap mask-imm8 swap ( reg shift -- masked-reg shift )
  <<u ( reg shift -- reg<<shift ) ;
: destination-register ( reg -- shifted-reg ) 8 position-byte ;
: source-register ( reg -- shifted-reg ) 16 position-byte ;
: source2-register ( reg -- shifted-reg ) 24 position-byte ;
: imm8 ( imm8 -- shifted-imm8 ) source2-register ;
: imm16 ( imm16 -- shifted-imm16 ) mask-immediate16 16 <<u ;
: NoArguments ( -- 0 ) 0 ;
: OneRegister ( dest -- value ) destination-register ;

: TwoRegister ( src dest -- value ) 
  OneRegister
  swap
  source-register
  bitwise-oru ;


: ThreeRegister ( src2 src dest -- value )
  TwoRegister ( src2 src dest -- src2 value )
  swap
  source2-register
  bitwise-oru  ;
: Immediate16 ( imm16 -- value ) imm16 ;

: OneRegisterWithImmediate ( imm16 dest -- value ) 
  OneRegister
  swap
  Immediate16
  bitwise-oru ;

: TwoRegisterWithImmediate ( imm8 src dest -- value ) 
  TwoRegister ( imm8 src dest -- imm8 value )
  swap
  imm8 
  bitwise-oru ;
: {asm ( a -- ) {bin ;
: asm} ( -- ) bin} ;
variable dataLoc 
variable codeLoc 
variable stackLoc 
variable currentsection
variable sectionid

{enum
 : section:register ( -- n ) literal ; enum,
 : section:code ( -- n ) literal ; enum,
 : section:data ( -- n ) literal ; enum,
 : section:stack ( -- n ) literal ;
enum}
: section:register? ( a -- flag ) section:register = ;
: section:code? ( a -- flag ) section:code = ;
: section:data? ( a -- flag ) section:data = ;
: section:stack? ( a -- flag ) section:stack = ;
: deflabel ( -- ) variable ;
: get-current-address ( -- value ) currentsection @ @ ;
: label-here ( variable -- ) get-current-address swap ! ;
: current-section-var ( -- value ) currentsection @ ; 
: set-current-address ( value -- ) mask-immediate16 current-section-var ! ;
: set-current-section ( id -- ) sectionid ! ;
: .register ( -- ) section:register set-current-section ;
: .code ( -- ) codeLoc currentsection ! section:code set-current-section ;
: .data ( -- ) dataLoc currentsection ! section:data set-current-section ;
: .stack ( -- ) stackLoc currentsection ! section:stack set-current-section ;
: .org ( addr -- ) set-current-address ;
: .org@ ( var -- ) @ .org ;
: next-word ( -- ) get-current-address 1+ .org ;
: then,     ( -- ) next-word ;
: section-entry ( value address section -- ) bin<<q bin<<q bin<<h ;
: known-section-entry ( value address -- ) sectionid @ section-entry ;
: register! ( value address -- ) .register known-section-entry ;
: stack! ( value address -- ) .stack known-section-entry ;
: data! ( value address -- ) .data known-section-entry ;
: code! ( value address -- ) .code known-section-entry ;
: in-section:register? ( -- flag ) sectionid @ section:register? ;
: in-section:code? ( -- flag ) sectionid @ section:code? ;
: in-section:data? ( -- flag ) sectionid @ section:data? ;
: in-section:stack? ( -- flag ) sectionid @ section:stack? ;

: asm<< ( value -- ) 
  in-section:register? if drop " can't use asm<< to install into registers section" . CR else
  get-current-address
  in-section:code?     if code! then, else
  in-section:data?     if data! then, else
  in-section:stack?    if stack! then, else 
  2drop " can't install into unknown section" .  CR then then then then ;

: assemble ( in out -- ) swap open-input-file ;

{enum
: AsmNop ( -- n ) literal ; enum, 
: AsmAdd ( -- n ) literal ; enum, 
: AsmSub ( -- n ) literal ; enum, 
: AsmMul ( -- n ) literal ; enum, 
: AsmDiv ( -- n ) literal ; enum, 
: AsmRem ( -- n ) literal ; enum, 
: AsmShiftLeft ( -- n ) literal ; enum, 
: AsmShiftRight ( -- n ) literal ; enum, 
: AsmAnd ( -- n ) literal ; enum, 
: AsmOr ( -- n ) literal ; enum, 
: AsmNot ( -- n ) literal ; enum, 
: AsmXor ( -- n ) literal ; enum, 
: AsmNand ( -- n ) literal ; enum, 
: AsmNor ( -- n ) literal ; enum, 
: AsmAddImmediate ( -- n ) literal ; enum, 
: AsmSubImmediate ( -- n ) literal ; enum, 
: AsmMulImmediate ( -- n ) literal ; enum, 
: AsmDivImmediate ( -- n ) literal ; enum, 
: AsmRemImmediate ( -- n ) literal ; enum, 
: AsmShiftLeftImmediate ( -- n ) literal ; enum, 
: AsmShiftRightImmediate ( -- n ) literal ; enum, 
: AsmMin ( -- n ) literal ; enum, 
: AsmMax ( -- n ) literal ; enum, 
: AsmLogicalXor ( -- n ) literal ; enum, 
: AsmLogicalNot ( -- n ) literal ; enum, 
: AsmLogicalAnd ( -- n ) literal ; enum, 
: AsmLogicalOr ( -- n ) literal ; enum, 
: AsmLogicalNand ( -- n ) literal ; enum, 
: AsmLogicalNor ( -- n ) literal ; enum, 
: AsmEq ( -- n ) literal ; enum, 
: AsmEqImmediate ( -- n ) literal ; enum, 
: AsmNeq ( -- n ) literal ; enum, 
: AsmNeqImmediate ( -- n ) literal ; enum, 
: AsmLessThan ( -- n ) literal ; enum, 
: AsmLessThanImmediate ( -- n ) literal ; enum, 
: AsmGreaterThan ( -- n ) literal ; enum, 
: AsmGreaterThanImmediate ( -- n ) literal ; enum, 
: AsmLessThanOrEqualTo ( -- n ) literal ; enum, 
: AsmLessThanOrEqualToImmediate ( -- n ) literal ; enum, 
: AsmGreaterThanOrEqualTo ( -- n ) literal ; enum, 
: AsmGreaterThanOrEqualToImmediate ( -- n ) literal ; enum, 
: AsmMove ( -- n ) literal ; enum, 
: AsmSet ( -- n ) literal ; enum, 
: AsmSwap ( -- n ) literal ; enum, 
: AsmLoad ( -- n ) literal ; enum, 
: AsmLoadImmediate ( -- n ) literal ; enum, 
: AsmStore ( -- n ) literal ; enum, 
: AsmStoreImmediate ( -- n ) literal ; enum, 
: AsmPush ( -- n ) literal ; enum, 
: AsmPushImmediate ( -- n ) literal ; enum, 
: AsmPop ( -- n ) literal ; enum, 
: AsmLoadCode ( -- n ) literal ; enum, 
: AsmStoreCode ( -- n ) literal ; enum, 
: AsmBranch ( -- n ) literal ; enum, 
: AsmBranchAndLink ( -- n ) literal ; enum, 
: AsmBranchIndirect ( -- n ) literal ; enum, 
: AsmBranchIndirectLink ( -- n ) literal ; enum, 
: AsmBranchConditional ( -- n ) literal ; enum, 
: AsmBranchConditionalIndirect ( -- n ) literal ; enum, 
: AsmBranchConditionalIndirectLink ( -- n ) literal ; enum, 
: AsmTerminateExecution ( -- n ) literal ; enum,
: AsmLoadIO ( -- n ) literal ; enum,
: AsmStoreIO ( -- n ) literal ; 
enum}

: code<< ( a b -- ) .code asm<< ;
: finish-op ( a b -- c ) bitwise-oru ; 
: !nop ( args* -- n ) NoArguments AsmNop finish-op code<< ;
: !add ( args* -- n ) ThreeRegister AsmAdd finish-op code<< ;
: !sub ( args* -- n ) ThreeRegister AsmSub finish-op code<< ;
: !mul ( args* -- n ) ThreeRegister AsmMul finish-op code<< ;
: !div ( args* -- n ) ThreeRegister AsmDiv finish-op code<< ;
: !rem ( args* -- n ) ThreeRegister AsmRem finish-op code<< ;
: !shl ( args* -- n ) ThreeRegister AsmShiftLeft finish-op code<< ;
: !shr ( args* -- n ) ThreeRegister AsmShiftRight finish-op code<< ;
: !and ( args* -- n ) ThreeRegister AsmAnd finish-op code<< ;
: !or ( args* -- n ) ThreeRegister AsmOr finish-op code<< ;
: !not ( args* -- n ) TwoRegister AsmNot finish-op code<< ;
: !xor ( args* -- n ) ThreeRegister AsmXor finish-op code<< ;
: !nand ( args* -- n ) ThreeRegister AsmNand finish-op code<< ;
: !nor ( args* -- n ) ThreeRegister AsmNor finish-op code<< ;
: !addi ( args* -- n ) TwoRegisterWithImmediate AsmAddImmediate finish-op code<< ;
: !subi ( args* -- n ) TwoRegisterWithImmediate AsmSubImmediate finish-op code<< ;
: !muli ( args* -- n ) TwoRegisterWithImmediate AsmMulImmediate finish-op code<< ;
: !divi ( args* -- n ) TwoRegisterWithImmediate AsmDivImmediate finish-op code<< ;
: !remi ( args* -- n ) TwoRegisterWithImmediate AsmRemImmediate finish-op code<< ;
: !shli ( args* -- n ) TwoRegisterWithImmediate AsmShiftLeftImmediate finish-op code<< ;
: !shri ( args* -- n ) TwoRegisterWithImmediate AsmShiftRightImmediate finish-op code<< ;
: !min ( args* -- n ) ThreeRegister AsmMin finish-op code<< ;
: !max ( args* -- n ) ThreeRegister AsmMax finish-op code<< ;
: !lxor ( args* -- n ) ThreeRegister AsmLogicalXor finish-op code<< ;
: !lnot ( args* -- n ) TwoRegister AsmLogicalNot finish-op code<< ;
: !land ( args* -- n ) ThreeRegister AsmLogicalAnd finish-op code<< ;
: !lor ( args* -- n ) ThreeRegister AsmLogicalOr finish-op code<< ;
: !lnand ( args* -- n ) ThreeRegister AsmLogicalNand finish-op code<< ;
: !lnor ( args* -- n ) ThreeRegister AsmLogicalNor finish-op code<< ;
: !eq ( args* -- n ) ThreeRegister AsmEq finish-op code<< ;
: !eqi ( args* -- n ) TwoRegisterWithImmediate AsmEqImmediate finish-op code<< ;
: !neq ( args* -- n ) ThreeRegister AsmNeq finish-op code<< ;
: !neqi ( args* -- n ) TwoRegisterWithImmediate AsmNeqImmediate finish-op code<< ;
: !lt ( args* -- n ) ThreeRegister AsmLessThan finish-op code<< ;
: !lti ( args* -- n ) TwoRegisterWithImmediate AsmLessThanImmediate finish-op code<< ;
: !gt ( args* -- n ) ThreeRegister AsmGreaterThan finish-op code<< ;
: !gti ( args* -- n ) TwoRegisterWithImmediate AsmGreaterThanImmediate finish-op code<< ;
: !le ( args* -- n ) ThreeRegister AsmLessThanOrEqualTo finish-op code<< ;
: !lei ( args* -- n ) TwoRegisterWithImmediate AsmLessThanOrEqualToImmediate finish-op code<< ;
: !ge ( args* -- n ) ThreeRegister AsmGreaterThanOrEqualTo finish-op code<< ;
: !gei ( args* -- n ) TwoRegisterWithImmediate AsmGreaterThanOrEqualToImmediate finish-op code<< ;
: !move ( args* -- n ) TwoRegister AsmMove finish-op code<< ;
: !set ( args* -- n ) OneRegisterWithImmediate AsmSet finish-op code<< ;
: !swap ( args* -- n ) TwoRegister AsmSwap finish-op code<< ;
: !ld ( args* -- n ) TwoRegister AsmLoad finish-op code<< ;
: !ldi ( args* -- n ) OneRegisterWithImmediate AsmLoadImmediate finish-op code<< ;
: !st ( args* -- n ) TwoRegister AsmStore finish-op code<< ;
: !sti ( args* -- n ) OneRegisterWithImmediate AsmStoreImmediate finish-op code<< ;
: !push ( args* -- n ) TwoRegister AsmPush finish-op code<< ;
: !pushi ( args* -- n ) OneRegisterWithImmediate AsmPushImmediate finish-op code<< ;
: !pop ( args* -- n ) TwoRegister AsmPop finish-op code<< ;
: !ldc ( args* -- n ) ThreeRegister AsmLoadCode finish-op code<< ;
: !stc ( args* -- n ) ThreeRegister AsmStoreCode finish-op code<< ;
: !b ( args* -- n ) Immediate16 AsmBranch finish-op code<< ;
: !bl ( args* -- n ) OneRegisterWithImmediate AsmBranchAndLink finish-op code<< ;
: !br ( args* -- n ) OneRegister AsmBranchIndirect finish-op code<< ;
: !brl ( args* -- n ) TwoRegister AsmBranchIndirectLink finish-op code<< ;
: !bc ( args* -- n ) OneRegisterWithImmediate AsmBranchConditional finish-op code<< ;
: !bcr ( args* -- n ) TwoRegister AsmBranchConditionalIndirect finish-op code<< ;
: !bcrl ( args* -- n ) ThreeRegister AsmBranchConditionalIndirectLink finish-op code<< ;
: !terminateExecution ( args* -- n ) OneRegister AsmTerminateExecution finish-op code<< ;
: !ldio ( args* -- n ) TwoRegister AsmLoadIO finish-op code<< ;
: !stio ( args* -- n ) TwoRegister AsmStoreIO finish-op code<< ;

: r0 ( -- 0 ) 0 ; : r1 ( -- 1 ) 1 ; : r2 ( -- 2 ) 2 ; : r3 ( -- 3 ) 3 ;
: r4 ( -- 4 ) 4 ; : r5 ( -- 5 ) 5 ; : r6 ( -- 6 ) 6 ; : r7 ( -- 7 ) 7 ;
: r8 ( -- 8 ) 8 ; : r9 ( -- 9 ) 9 ; : r10 ( -- 10 ) 10 ; : r11 ( -- 11 ) 11 ;
: r12 ( -- 12 ) 12 ; : r13 ( -- 13 ) 13 ; : r14 ( -- 14 ) 14 ; : r15 ( -- 15 ) 15 ;
: r16 ( -- 16 ) 16 ; : r17 ( -- 17 ) 17 ; : r18 ( -- 18 ) 18 ; : r19 ( -- 19 ) 19 ;
: r20 ( -- 20 ) 20 ; : r21 ( -- 21 ) 21 ; : r22 ( -- 22 ) 22 ; : r23 ( -- 23 ) 23 ;
: r24 ( -- 24 ) 24 ; : r25 ( -- 25 ) 25 ; : r26 ( -- 26 ) 26 ; : r27 ( -- 27 ) 27 ;
: r28 ( -- 28 ) 28 ; : r29 ( -- 29 ) 29 ; : r30 ( -- 30 ) 30 ; : r31 ( -- 31 ) 31 ;
: r32 ( -- 32 ) 32 ; : r33 ( -- 33 ) 33 ; : r34 ( -- 34 ) 34 ; : r35 ( -- 35 ) 35 ;
: r36 ( -- 36 ) 36 ; : r37 ( -- 37 ) 37 ; : r38 ( -- 38 ) 38 ; : r39 ( -- 39 ) 39 ;
: r40 ( -- 40 ) 40 ; : r41 ( -- 41 ) 41 ; : r42 ( -- 42 ) 42 ; : r43 ( -- 43 ) 43 ;
: r44 ( -- 44 ) 44 ; : r45 ( -- 45 ) 45 ; : r46 ( -- 46 ) 46 ; : r47 ( -- 47 ) 47 ;
: r48 ( -- 48 ) 48 ; : r49 ( -- 49 ) 49 ; : r50 ( -- 50 ) 50 ; : r51 ( -- 51 ) 51 ;
: r52 ( -- 52 ) 52 ; : r53 ( -- 53 ) 53 ; : r54 ( -- 54 ) 54 ; : r55 ( -- 55 ) 55 ;
: r56 ( -- 56 ) 56 ; : r57 ( -- 57 ) 57 ; : r58 ( -- 58 ) 58 ; : r59 ( -- 59 ) 59 ;
: r60 ( -- 60 ) 60 ; : r61 ( -- 61 ) 61 ; : r62 ( -- 62 ) 62 ; : r63 ( -- 63 ) 63 ;
: r64 ( -- 64 ) 64 ; : r65 ( -- 65 ) 65 ; : r66 ( -- 66 ) 66 ; : r67 ( -- 67 ) 67 ;
: r68 ( -- 68 ) 68 ; : r69 ( -- 69 ) 69 ; : r70 ( -- 70 ) 70 ; : r71 ( -- 71 ) 71 ;
: r72 ( -- 72 ) 72 ; : r73 ( -- 73 ) 73 ; : r74 ( -- 74 ) 74 ; : r75 ( -- 75 ) 75 ;
: r76 ( -- 76 ) 76 ; : r77 ( -- 77 ) 77 ; : r78 ( -- 78 ) 78 ; : r79 ( -- 79 ) 79 ;
: r80 ( -- 80 ) 80 ; : r81 ( -- 81 ) 81 ; : r82 ( -- 82 ) 82 ; : r83 ( -- 83 ) 83 ;
: r84 ( -- 84 ) 84 ; : r85 ( -- 85 ) 85 ; : r86 ( -- 86 ) 86 ; : r87 ( -- 87 ) 87 ;
: r88 ( -- 88 ) 88 ; : r89 ( -- 89 ) 89 ; : r90 ( -- 90 ) 90 ; : r91 ( -- 91 ) 91 ;
: r92 ( -- 92 ) 92 ; : r93 ( -- 93 ) 93 ; : r94 ( -- 94 ) 94 ; : r95 ( -- 95 ) 95 ;
: r96 ( -- 96 ) 96 ; : r97 ( -- 97 ) 97 ; : r98 ( -- 98 ) 98 ; : r99 ( -- 99 ) 99 ;
: r100 ( -- 100 ) 100 ; : r101 ( -- 101 ) 101 ; : r102 ( -- 102 ) 102 ; : r103 ( -- 103 ) 103 ;
: r104 ( -- 104 ) 104 ; : r105 ( -- 105 ) 105 ; : r106 ( -- 106 ) 106 ; : r107 ( -- 107 ) 107 ;
: r108 ( -- 108 ) 108 ; : r109 ( -- 109 ) 109 ; : r110 ( -- 110 ) 110 ; : r111 ( -- 111 ) 111 ;
: r112 ( -- 112 ) 112 ; : r113 ( -- 113 ) 113 ; : r114 ( -- 114 ) 114 ; : r115 ( -- 115 ) 115 ;
: r116 ( -- 116 ) 116 ; : r117 ( -- 117 ) 117 ; : r118 ( -- 118 ) 118 ; : r119 ( -- 119 ) 119 ;
: r120 ( -- 120 ) 120 ; : r121 ( -- 121 ) 121 ; : r122 ( -- 122 ) 122 ; : r123 ( -- 123 ) 123 ;
: r124 ( -- 124 ) 124 ; : r125 ( -- 125 ) 125 ; : r126 ( -- 126 ) 126 ; : r127 ( -- 127 ) 127 ;
: r128 ( -- 128 ) 128 ; : r129 ( -- 129 ) 129 ; : r130 ( -- 130 ) 130 ; : r131 ( -- 131 ) 131 ;
: r132 ( -- 132 ) 132 ; : r133 ( -- 133 ) 133 ; : r134 ( -- 134 ) 134 ; : r135 ( -- 135 ) 135 ;
: r136 ( -- 136 ) 136 ; : r137 ( -- 137 ) 137 ; : r138 ( -- 138 ) 138 ; : r139 ( -- 139 ) 139 ;
: r140 ( -- 140 ) 140 ; : r141 ( -- 141 ) 141 ; : r142 ( -- 142 ) 142 ; : r143 ( -- 143 ) 143 ;
: r144 ( -- 144 ) 144 ; : r145 ( -- 145 ) 145 ; : r146 ( -- 146 ) 146 ; : r147 ( -- 147 ) 147 ;
: r148 ( -- 148 ) 148 ; : r149 ( -- 149 ) 149 ; : r150 ( -- 150 ) 150 ; : r151 ( -- 151 ) 151 ;
: r152 ( -- 152 ) 152 ; : r153 ( -- 153 ) 153 ; : r154 ( -- 154 ) 154 ; : r155 ( -- 155 ) 155 ;
: r156 ( -- 156 ) 156 ; : r157 ( -- 157 ) 157 ; : r158 ( -- 158 ) 158 ; : r159 ( -- 159 ) 159 ;
: r160 ( -- 160 ) 160 ; : r161 ( -- 161 ) 161 ; : r162 ( -- 162 ) 162 ; : r163 ( -- 163 ) 163 ;
: r164 ( -- 164 ) 164 ; : r165 ( -- 165 ) 165 ; : r166 ( -- 166 ) 166 ; : r167 ( -- 167 ) 167 ;
: r168 ( -- 168 ) 168 ; : r169 ( -- 169 ) 169 ; : r170 ( -- 170 ) 170 ; : r171 ( -- 171 ) 171 ;
: r172 ( -- 172 ) 172 ; : r173 ( -- 173 ) 173 ; : r174 ( -- 174 ) 174 ; : r175 ( -- 175 ) 175 ;
: r176 ( -- 176 ) 176 ; : r177 ( -- 177 ) 177 ; : r178 ( -- 178 ) 178 ; : r179 ( -- 179 ) 179 ;
: r180 ( -- 180 ) 180 ; : r181 ( -- 181 ) 181 ; : r182 ( -- 182 ) 182 ; : r183 ( -- 183 ) 183 ;
: r184 ( -- 184 ) 184 ; : r185 ( -- 185 ) 185 ; : r186 ( -- 186 ) 186 ; : r187 ( -- 187 ) 187 ;
: r188 ( -- 188 ) 188 ; : r189 ( -- 189 ) 189 ; : r190 ( -- 190 ) 190 ; : r191 ( -- 191 ) 191 ;
: r192 ( -- 192 ) 192 ; : r193 ( -- 193 ) 193 ; : r194 ( -- 194 ) 194 ; : r195 ( -- 195 ) 195 ;
: r196 ( -- 196 ) 196 ; : r197 ( -- 197 ) 197 ; : r198 ( -- 198 ) 198 ; : r199 ( -- 199 ) 199 ;
: r200 ( -- 200 ) 200 ; : r201 ( -- 201 ) 201 ; : r202 ( -- 202 ) 202 ; : r203 ( -- 203 ) 203 ;
: r204 ( -- 204 ) 204 ; : r205 ( -- 205 ) 205 ; : r206 ( -- 206 ) 206 ; : r207 ( -- 207 ) 207 ;
: r208 ( -- 208 ) 208 ; : r209 ( -- 209 ) 209 ; : r210 ( -- 210 ) 210 ; : r211 ( -- 211 ) 211 ;
: r212 ( -- 212 ) 212 ; : r213 ( -- 213 ) 213 ; : r214 ( -- 214 ) 214 ; : r215 ( -- 215 ) 215 ;
: r216 ( -- 216 ) 216 ; : r217 ( -- 217 ) 217 ; : r218 ( -- 218 ) 218 ; : r219 ( -- 219 ) 219 ;
: r220 ( -- 220 ) 220 ; : r221 ( -- 221 ) 221 ; : r222 ( -- 222 ) 222 ; : r223 ( -- 223 ) 223 ;
: r224 ( -- 224 ) 224 ; : r225 ( -- 225 ) 225 ; : r226 ( -- 226 ) 226 ; : r227 ( -- 227 ) 227 ;
: r228 ( -- 228 ) 228 ; : r229 ( -- 229 ) 229 ; : r230 ( -- 230 ) 230 ; : r231 ( -- 231 ) 231 ;
: r232 ( -- 232 ) 232 ; : r233 ( -- 233 ) 233 ; : r234 ( -- 234 ) 234 ; : r235 ( -- 235 ) 235 ;
: r236 ( -- 236 ) 236 ; : r237 ( -- 237 ) 237 ; : r238 ( -- 238 ) 238 ; : r239 ( -- 239 ) 239 ;
: r240 ( -- 240 ) 240 ; : r241 ( -- 241 ) 241 ; : r242 ( -- 242 ) 242 ; : r243 ( -- 243 ) 243 ;
: r244 ( -- 244 ) 244 ; : r245 ( -- 245 ) 245 ; : r246 ( -- 246 ) 246 ; : r247 ( -- 247 ) 247 ;
: r248 ( -- 248 ) 248 ; : r249 ( -- 249 ) 249 ; : r250 ( -- 250 ) 250 ; : r251 ( -- 251 ) 251 ;
: r252 ( -- 252 ) 252 ; : r253 ( -- 253 ) 253 ; : r254 ( -- 254 ) 254 ; : r255 ( -- 255 ) 255 ;
( aliases )
{enum 
: zero ( -- n ) literal ; enum,
: sp ( -- n ) literal ; enum,
: lr ( -- n ) literal ; enum,
: t0 ( -- n ) literal ; enum,
: t1 ( -- n ) literal ; enum,
: t2 ( -- n ) literal ; enum,
: t3 ( -- n ) literal ; enum,
: t4 ( -- n ) literal ; enum,
: sp2 ( -- n ) literal ; enum,
: arg0 ( -- n ) literal ; enum,
: arg1 ( -- n ) literal ; enum,
: arg2 ( -- n ) literal ; enum,
: arg3 ( -- n ) literal ; enum,
: ret0 ( -- n ) literal ; enum,
: ret1 ( -- n ) literal ; enum,
: jump-table-start ( -- n ) literal ; enum,
: jump-table-end ( -- n ) literal ; enum,
: sp-bottom ( -- n ) literal ; enum,
: sp2-bottom ( -- n ) literal ; enum,
: cond ( -- n ) literal ; enum,
: loop-body ( -- n ) literal ; enum,
: terminate-loop ( -- n ) literal ; enum,
: io-device ( -- n ) literal ; enum,
: lower-word ( -- n ) literal ; enum,
: upper-word ( -- n ) literal ; enum,
: code-address ( -- n ) literal ; 
enum}
: !set-iodev ( imm -- reg ) io-device !set io-device ;
: !set-iodev-and-store ( src devid -- ) !set-iodev !stio ;
: !set-iodev-and-load ( dest devid -- ) !set-iodev swap !ldio ;
( io devices )
{enum
: /dev/null ( -- n ) literal ; enum,
: /dev/console0 ( -- n ) literal ; enum,
: /dev/console1 ( -- n ) literal ; enum,
: /dev/rng ( -- n ) literal ; 
enum}

: !putc ( src -- ) /dev/console0 !set-iodev-and-store ;
: !getc ( dest -- ) /dev/console0 !set-iodev-and-load ;
: !flush-cout ( -- ) zero /dev/console1 !set-iodev-and-store ;

: !seed-rng ( src -- ) /dev/rng !set-iodev-and-store ;
: !get-rng-value ( dest -- ) /dev/rng !set-iodev-and-load ;
: !skip-rng-value ( -- ) zero !get-rng-value ;

: !return ( -- ) lr !br ;
: !imm16 ( imm16 src dest -- t0 src dest ) 
  rot ( src dest imm16 )
  t0  ( src dest imm16 t0 )
  !set ( src dest )
  t0
  -rot ( t0 src dest ) ;
: indirect-register ( register dest -- dest ) 
  \ equivalent of doing [register] in the operands
  dup -rot !ld \ load from memory before performing the operation
  ;

: !addi16 ( imm16 src dest -- ) !imm16 !add ;
: !subi16 ( imm16 src dest -- ) !imm16 !sub ;
: !muli16 ( imm16 src dest -- ) !imm16 !mul ;
: !divi16 ( imm16 src dest -- ) !imm16 !div ;
: !remi16 ( imm16 src dest -- ) !imm16 !rem ;
: !*ld    ( src dest -- )
  \ indirect load
  swap t0 indirect-register swap !ld \ put t0 back as destination 
  ;
: !*st ( src dest -- ) \ indirect store
  t0 indirect-register !st \ then store at the loaded address 
  ;

: !top ( dest -- ) \ load the top of the stack into the target register
  sp swap !pop ;
: !drop ( -- ) \ remove the top of the stack
  zero !top \ writes to zero are disabled so this will drop the top parameter
  ; 
: !save-param ( reg -- ) \ save to the parameter stack
  sp !push ;
: !save-subr ( reg -- ) \ save to the subroutine stack
  sp2 !push ;
: !restore-subr ( reg -- ) \ restore from the subroutine stack
  sp2 swap !pop ;
: !restore-param ( reg -- ) !top ;
: !1+ ( reg dest -- ) 1 -rot !addi ;
: !1- ( reg dest -- ) 1 -rot !subi ;
: rot-!eq ( dest src2 src -- ) rot !eq ;
: !empty-param? ( dest -- ) sp sp-bottom rot-!eq ;
: !empty-subr?  ( dest -- ) sp2 sp2-bottom rot-!eq ;
: !save-lr ( -- ) lr !save-subr ;
: !restore-lr ( -- ) lr !restore-subr ;
: !call ( imm16 -- ) lr !bl ;
: !callr ( dest -- ) lr !brl ;

: args1 ( -- r0 ) ret0 ;
: args2 ( -- a0 r0 ) arg0 args1 ;
: args3 ( -- a1 a0 r0 ) arg1 args2 ;
: zero-src2 ( src dest -- zero src dest ) zero -rot ;
: zero-src ( dest -- zero dest ) zero swap ;
: !clr-reg ( dest -- ) zero-src !move ;
: !save-ret0 ( -- ) ret0 !save-param ;
: !bcl ( link imm16 cond -- n ) 
  \ make a new instruction to store the immediate value
  swap t0 !set \ stash our immediate to t0
  t0 swap !bcrl \ put out the !bcrl command
  ;
: !neqz ( src dest -- ) zero-src2 !neq ;
: !neqc ( a b -- cond ) cond !neq cond ;
: !neqzc ( a -- cond ) cond !neqz cond ;
: !bneq ( dest a b -- ) !neqc !bc ;
: !bneql ( link dest a b -- ) !neqc !bcl ;
: !bneqz ( dest a -- ) !neqzc !bc ;
: !bneqzl ( link dest a -- ) !neqzc !bcl ;
: !bneqr ( dest a b -- ) !neqc !bcr ;
: !bneqrl ( link dest a b -- ) !neqc !bcrl ;
: !bneqrz ( dest a -- ) !neqzc !bcr ;
: !bneqrzl ( link dest a b -- ) !neqzc !bcrl ;

: !eqz ( src dest -- ) zero-src2 !eq ;
: !eqc ( a b -- cond ) cond !eq cond ;
: !eqzc ( a -- cond ) cond !eqz cond ;
: !beq ( dest a b -- ) !eqc !bc ;
: !beql ( link dest a b -- ) !eqc !bcl ;
: !beqz ( dest a -- ) !eqzc !bc ;
: !beqzl ( link dest a -- ) !eqzc !bcl ;
: !beqr ( dest a b -- ) !eqc !bcr ;
: !beqrl ( link dest a b -- ) !eqc !bcrl ;
: !beqrz ( dest a -- ) !eqzc !bcr ;
: !beqrzl ( link dest a b -- ) !eqzc !bcrl ;

: !gtz ( src dest -- ) zero-src2 !gt ;
: !gtc ( a b -- cond ) cond !gt cond ;
: !gtzc ( a -- cond ) cond !gtz cond ;
: !bgt ( dest a b -- ) !gtc !bc ;
: !bgtl ( link dest a b -- ) !gtc !bcl ;
: !bgtz ( dest a -- ) !gtzc !bc ;
: !bgtzl ( link dest a -- ) !gtzc !bcl ;
: !bgtr ( dest a b -- ) !gtc !bcr ;
: !bgtrl ( link dest a b -- ) !gtc !bcrl ;
: !bgtrz ( dest a -- ) !gtzc !bcr ;
: !bgtrzl ( link dest a b -- ) !gtzc !bcrl ;

: !ltz ( src dest -- ) zero-src2 !lt ;
: !ltc ( a b -- cond ) cond !lt cond ;
: !ltzc ( a -- cond ) cond !ltz cond ;
: !blt ( dest a b -- ) !ltc !bc ;
: !bltl ( link dest a b -- ) !ltc !bcl ;
: !bltz ( dest a -- ) !ltzc !bc ;
: !bltzl ( link dest a -- ) !ltzc !bcl ;
: !bltr ( dest a b -- ) !ltc !bcr ;
: !bltrl ( link dest a b -- ) !ltc !bcrl ;
: !bltrz ( dest a -- ) !ltzc !bcr ;
: !bltrzl ( link dest a b -- ) !ltzc !bcrl ;

: !gez ( src dest -- ) zero-src2 !ge ;
: !gec ( a b -- cond ) cond !ge cond ;
: !gezc ( a -- cond ) cond !gez cond ;
: !bge ( dest a b -- ) !gec !bc ;
: !bgel ( link dest a b -- ) !gec !bcl ;
: !bgez ( dest a -- ) !gezc !bc ;
: !bgezl ( link dest a -- ) !gezc !bcl ;
: !bger ( dest a b -- ) !gec !bcr ;
: !bgerl ( link dest a b -- ) !gec !bcrl ;
: !bgerz ( dest a -- ) !gezc !bcr ;
: !bgerzl ( link dest a b -- ) !gezc !bcrl ;

: !lez ( src dest -- ) zero-src2 !le ;
: !lec ( a b -- cond ) cond !le cond ;
: !lezc ( a -- cond ) cond !lez cond ;
: !ble ( dest a b -- ) !lec !bc ;
: !blel ( link dest a b -- ) !lec !bcl ;
: !blez ( dest a -- ) !lezc !bc ;
: !blezl ( link dest a -- ) !lezc !bcl ;
: !bler ( dest a b -- ) !lec !bcr ;
: !blerl ( link dest a b -- ) !lec !bcrl ;
: !blerz ( dest a -- ) !lezc !bcr ;
: !blerzl ( link dest a b -- ) !lezc !bcrl ;

: !if ( onFalse onTrue a -- ) 
  !bc ( onFalse onTrue a -- onFalse) 
  !b ( onFalse -- ) ;
: !ifc ( onFalse onTrue -- ) cond !if ;
: !ifeq ( onFalse onTrue a b -- ) !eqc !if ;
: !ifeqz ( onFalse onTrue a b -- ) !eqzc !if ;
: !ifneq ( onFalse onTrue a b -- ) !neqc !if ;
: !ifneqz ( onFalse onTrue a b -- ) !neqzc !if ;
: !iflt ( onFalse onTrue a b -- ) !ltc !if ;
: !ifltz ( onFalse onTrue a b -- ) !ltzc !if ;
: !ifgt ( onFalse onTrue a b -- ) !gtc !if ;
: !ifgtz ( onFalse onTrue a b -- ) !gtzc !if ;
: !ifle ( onFalse onTrue a b -- ) !lec !if ;
: !iflez ( onFalse onTrue a b -- ) !lezc !if ;
: !ifge ( onFalse onTrue a b -- ) !gec !if ;
: !ifgez ( onFalse onTrue a b -- ) !gezc !if ;

.stack 0 .org 
.data 0 .org
.code 0 .org

close-input-file
