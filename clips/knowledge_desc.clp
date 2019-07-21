; iris
; Copyright (c) 2013-2019, Joshua Scoggins and Contributors
; All rights reserved.
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;     * Redistributions in binary form must reproduce the above copyright
;       notice, this list of conditions and the following disclaimer in the
;       documentation and/or other materials provided with the distribution.
; 
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(defrule MAIN::make-deftranslation
         ?f <- (decl $?contents)
         =>
         (retract ?f)
         (assert (deftranslation (contents $?contents))))
(deffacts MAIN::descriptions 
          (instruction-class (kind noarg)
                             (args)
                             (members BranchUnconditionalToTheLinkRegister
                                      BranchUnconditionalToTheLinkRegisterAndLink
                                      ReturnFromError
                                      ))
          (instruction-class (kind 3gpr)
                             (args destination-gpr
                                   source-gpr
                                   source2-gpr)
                             (members Add 
                                      Subtract 
                                      Multiply Divide
                                      Remainder 
                                      ShiftLeft 
                                      ShiftRight 
                                      BinaryAnd
                                      BinaryOr 
                                      BinaryExclusiveOr 
                                      BinaryNand 
                                      BinaryNor
                                      Min 
                                      Max
                                      LoadFromCode
                                      StoreToCode
                                      ))
          (instruction-class (kind 2gpr+imm8)
                             (args destination-gpr
                                   source-gpr
                                   imm8)
                             (members AddImmediate 
                                      SubtractImmediate 
                                      MultiplyImmediate 
                                      DivideImmediate
                                      RemainderImmediate 
                                      ShiftLeftImmediate 
                                      ShiftRightImmediate 
                                      LoadFromDataWithOffset
                                      StoreToDataWithOffset 
                                      LoadFromIOWithOffset 
                                      StoreToIOWithOffset
                                      ))
          (instruction-class (kind 2gpr)
                             (args destination-gpr
                                   source-gpr)
                             (members UnaryNot 
                                      MoveRegisterContents 
                                      SwapRegisterContents
                                      LoadFromIO 
                                      StoreToIO 
                                      LoadFromData 
                                      StoreToData 
                                      PushDataOntoStack
                                      PopDataFromStack
                                      ))
          (instruction-class (kind 1gpr+imm16)
                             (args destination-gpr
                                   imm16)
                             (members StoreToDataWithImmediateAddress 
                                      LoadFromDataWithImmediateAddress
                                      LoadImmediate 
                                      PushImmediateOntoStack 
                                      SaveConditionRegisters
                                      RestoreConditionRegisters
                                      ))
          (instruction-class (kind 1gpr)
                             (args destination-gpr)
                             (members MoveFromIP 
                                      MoveToIP 
                                      MoveFromLinkRegister 
                                      MoveToLinkRegister 
                                      SaveAllRegisters 
                                      RestoreAllRegisters
                                      BranchUnconditionalRegister 
                                      BranchUnconditionalRegisterAndLink
                                      ))
          (instruction-class (kind imm16only)
                             (args imm16)
                             (members BranchUnconditionalImmediate
                                      BranchUnconditionalImmediateAndLink
                                      ))
          (instruction-class (kind conditional-gpr)
                             (args destination-predicate 
                                   source-gpr)
                             (members BranchConditionalRegister
                                      BranchConditionalRegisterAndLink))
          (instruction-class (kind conditional-imm)
                             (args destination-predicate
                                   imm16)
                             (members BranchConditionalImmediateAndLink
                                      BranchConditionalImmediate))
          (instruction-class (kind conditional-noargs)
                             (args destination-predicate)
                             (members BranchConditionalToTheLinkRegisterAndLink
                                      BranchConditionalToTheLinkRegister))
          (instruction-class (kind compare-gpr)
                             (args destination-predicate
                                   destination-inverse-predicate
                                   source-gpr
                                   source2-gpr)
                             (members Equals 
                                      NotEqual 
                                      LessThan 
                                      LessThanOrEqualTo
                                      GreaterThan 
                                      GreaterThanOrEqualTo))
          (instruction-class (kind compare-imm8)
                             (args destination-predicate
                                   destination-inverse-predicate
                                   source-gpr
                                   imm8)
                             (members EqualsImmediate
                                      NotEqualImmediate
                                      LessThanImmediate
                                      LessThanOrEqualToImmediate
                                      GreaterThanOrEqualToImmediate
                                      GreaterThanImmediate))
          (instruction-class (kind 2predicates)
                             (args destination-predicate 
                                   destination-inverse-predicate)
                             (members ConditionRegisterSwap
                                      ConditionRegisterMove))
          (instruction-class (kind 3predicates)
                             (args destination-predicate
                                   destination-inverse-predicate
                                   source-predicate)
                             (members ConditionRegisterNot))
          (instruction-class (kind 4predicates)
                             (args destination-predicate
                                   destination-inverse-predicate
                                   source-predicate
                                   source1-predicate)
                             (members ConditionRegisterExclusiveOr
                                      ConditionRegisterAnd
                                      ConditionRegisterOr
                                      ConditionRegisterNand
                                      ConditionRegisterNor))


          (operation-group (kind arithmetic)
                           (operations Add Subtract Multiply Divide 
                                       Remainder ShiftLeft ShiftRight BinaryAnd
                                       BinaryOr UnaryNot BinaryExclusiveOr BinaryNand
                                       BinaryNor AddImmediate SubtractImmediate MultiplyImmediate
                                       DivideImmediate RemainderImmediate ShiftLeftImmediate ShiftRightImmediate
                                       Min Max))
          (defaliases (contents Add -> { add combine }
                                AddImmediate -> { addi combinei add.imm combine.imm }
                                Subtract -> { sub subtract }
                                SubtractImmediate -> { subi sub.imm subtracti subtract.imm }
                                Multiply -> { mul multiply }
                                MultiplyImmediate -> { muli mul.imm multiply.imm multiplyi }
                                Divide -> { divide }
                                DivideImmediate -> { divide.imm }
                                Remainder -> { rem remainder } 
                                RemainderImmediate -> { remi rem.imm remainderi remainder.imm }
                                ShiftLeft -> { shl shift-left left-shift lshift shift.left left.shift }
                                ShiftLeftImmediate -> { shli shift-lefti shift-left-imm left-shift-imm left-shifti lshifti lshift-imm shift.lefti shift.left.imm left.shift.imm left.shifti lshift.imm }
                                ShiftRight -> { shr shift-right right-shift rshift shift.right right.shift }
                                ShiftRightImmediate -> { shri shift-righti shift-right-imm right-shift-imm right-shifti rshifti rshift-imm shift.righti shift.right.imm right.shift.imm right.shifti rshift.imm }
                                BinaryAnd -> { and! binary-and binary.and }
                                BinaryOr -> { or! binary-or binary.or }
                                UnaryNot -> { not! unary-not unary.not }
                                BinaryExclusiveOr -> { xor binary-xor binary.xor }
                                Min -> { min! minimum }
                                Max -> { max! maximum }
                                ))
          (operation-group (kind jump)
                           (operations BranchUnconditionalImmediate 
                                       BranchUnconditionalImmediateAndLink 
                                       BranchUnconditionalRegister 
                                       BranchUnconditionalRegisterAndLink
                                       BranchConditionalImmediate 
                                       BranchConditionalImmediateAndLink
                                       BranchConditionalRegister 
                                       BranchConditionalRegisterAndLink
                                       BranchUnconditionalToTheLinkRegister 
                                       BranchUnconditionalToTheLinkRegisterAndLink
                                       BranchConditionalToTheLinkRegister 
                                       BranchConditionalToTheLinkRegisterAndLink
                                       ReturnFromError))
          (defaliases (contents BranchUnconditionalImmediate -> { bi bui branch.imm }
                                BranchUnconditionalImmediateAndLink -> { bil buil branch.imm.link }
                                BranchUnconditionalRegister -> { b br bu bur branch.register branch.reg }
                                BranchUnconditionalRegisterAndLink -> { bl brl bul burl branch.reg.link }
                                BranchConditionalImmediate -> { bci branch.conditional.imm branch.cond.imm }
                                BranchConditionalImmediateAndLink -> { bcil branch.conditional.imm.link branch.cond.imm.link }
                                BranchConditionalRegister -> { bcr branch.cond.reg }
                                BranchConditionalRegisterAndLink -> { bcrl branch.cond.reg.link }
                                BranchUnconditionalToTheLinkRegister -> { blr bulr branch.lr }
                                BranchUnconditionalToTheLinkRegisterAndLink -> { blrl bulrl branch.lr.link }
                                BranchConditionalToTheLinkRegister -> { bclr branch.cond.lr }
                                BranchConditionalToTheLinkRegisterAndLink -> { bclrl branch.cond.lr.link }
                                ReturnFromError -> { rfe error-ret error.ret ret.error }
                                ))
          (operation-group (kind move)
                           (operations MoveRegisterContents LoadImmediate SwapRegisterContents LoadFromData
                                       LoadFromDataWithImmediateAddress LoadFromDataWithOffset StoreToData
                                       StoreToDataWithImmediateAddress StoreToDataWithOffset PushDataOntoStack
                                       PushImmediateOntoStack PopDataFromStack LoadFromCode StoreToCode
                                       LoadFromIO StoreToIO LoadFromIOWithOffset StoreToIOWithOffset
                                       MoveFromIP MoveToIP MoveFromLinkRegister MoveToLinkRegister
                                       SaveAllRegisters RestoreAllRegisters))
          (defaliases (contents MoveRegisterContents -> { move mov transfer reg.move move.reg copy cpy copy.reg cpy.reg reg->reg }
                                SwapRegisterContents -> { swp swap transpose reg.swap swap.reg }
                                LoadImmediate -> { load.const ldconst set assign imm->reg }
                                LoadFromData -> { ld ld.data load.data data->gpr data.load }
                                LoadFromDataWithImmediateAddress -> { ldi loadi ldi.data loadi.data data.loadi }
                                LoadFromDataWithOffset -> { ldwo loadwo ldwo.data loadwo.data data.loadwo }
                                StoreToData -> { st st.data store.data gpr->data data.store }
                                StoreToDataWithImmediateAddress -> { sti storei sti.data storei.data data.storei data.store.imm }
                                StoreToDataWithOffset -> { stwo storewo stwo.data storewo.data data.storewo }
                                PushDataOntoStack -> { push push.stack stack.push }
                                PushImmediateOntoStack -> { pushi pushi.stack stack.pushi stack.push.imm }
                                PopDataFromStack -> { pop pop.stack stack.pop }
                                LoadFromCode -> { ldc loadc ld.code load.code code.load code->gpr }
                                StoreToCode -> { stc storec st.code store.code code.store gpr->code }
                                LoadFromIO -> { ldio loadio ld.io load.io io.load io->gpr }
                                StoreToIO -> { stio storeio st.io store.io io.store gpr->io }
                                LoadFromIOWithOffset -> { ldiowo loadiowo ldwo.io loadwo.io }
                                StoreToIOWithOffset -> { stiowo storeiowo stwo.io storewo.io }
                                MoveFromIP -> { mfip ip->gpr }
                                MoveToIP -> { mtip gpr->ip  }
                                MoveFromLinkRegister -> { mflr lr->gpr move-from-lr mov-from-lr move.from.lr mov.from.lr }
                                MoveToLinkRegister -> { mtlr gpr->lr move-to-lr mov-to-lr move.to.lr mov.to.lr }
                                SaveAllRegisters -> { sregs save.regs regs.save }
                                RestoreAllRegisters -> { rregs restore.regs regs.restore }
                                ))
          (operation-group (kind compare)
                           (operations Equals EqualsImmediate NotEqual NotEqualImmediate
                                       LessThan LessThanImmediate GreaterThan GreaterThanImmediate
                                       LessThanOrEqualTo LessThanOrEqualToImmediate GreaterThanOrEqualTo
                                       GreaterThanOrEqualToImmediate))
          (defaliases (contents Equals -> { equals }
                                EqualsImmediate -> { equals.imm }
                                NotEqual -> { not.equals }
                                NotEqualImmediate -> { not.equals.imm }
                                LessThan -> { less.than }
                                LessThanImmediate -> { less.than.imm }
                                GreaterThan -> { greater.than }
                                GreaterThanImmediate -> { greater.than.imm }
                                LessThanOrEqualTo -> { less.than.or.equal }
                                LessThanOrEqualToImmediate -> { less.than.or.equal.imm }
                                GreaterThanOrEqualTo -> { greater.than.or.equal }
                                GreaterThanOrEqualToImmediate -> { greater.than.or.equal.imm }))
          (operation-group (kind condition-register-op)
                           (operations SaveConditionRegisters RestoreConditionRegisters
                                       ConditionRegisterExclusiveOr ConditionRegisterNot
                                       ConditionRegisterAnd ConditionRegisterOr
                                       ConditionRegisterNand ConditionRegisterNor
                                       ConditionRegisterSwap ConditionRegisterMove))
          (defaliases (contents SaveConditionRegisters -> { save.crs sav.crs crs.save crs.sav crssav crssave savecrs savcrs crs->gpr }
                                RestoreConditionRegisters -> { restore.crs restorecrs gpr->crs crs.restore crsrestore gpr->crs }
                                ConditionRegisterExclusiveOr -> { cr.xor xor.cr crxor xorcr }
                                ConditionRegisterNot -> { cr.not not.cr crnot notcr }
                                ConditionRegisterAnd -> { cr.and and.cr crand andcr }
                                ConditionRegisterOr -> { cr.or or.cr cror orcr }
                                ConditionRegisterNand -> { cr.nand nand.cr crnand nandcr }
                                ConditionRegisterNor -> { cr.nor nor.cr crnor norcr }
                                ConditionRegisterSwap -> { cr.swap swap.cr crswap swapcr }
                                ConditionRegisterMove -> { cr.move move.cr crmove movecr mov.cr cr.mov movcr crmov cr.copy copy.cr cr.transfer transfer.cr }))
          )

(definstances MAIN::registers
              (r0 of gpr) (r1 of gpr) (r2 of gpr)
              (r3 of gpr) (r4 of gpr) (r5 of gpr)
              (r6 of gpr) (r7 of gpr) (r8 of gpr)
              (r9 of gpr) (r10 of gpr) (r11 of gpr)
              (r12 of gpr) (r13 of gpr) (r14 of gpr)
              (r15 of gpr) (r16 of gpr) (r17 of gpr)
              (r18 of gpr) (r19 of gpr) (r20 of gpr)
              (r21 of gpr) (r22 of gpr) (r23 of gpr)
              (r24 of gpr) (r25 of gpr) (r26 of gpr)
              (r27 of gpr) (r28 of gpr) (r29 of gpr)
              (r30 of gpr) (r31 of gpr) (r32 of gpr)
              (r33 of gpr) (r34 of gpr) (r35 of gpr)
              (r36 of gpr) (r37 of gpr) (r38 of gpr)
              (r39 of gpr) (r40 of gpr) (r41 of gpr)
              (r42 of gpr) (r43 of gpr) (r44 of gpr)
              (r45 of gpr) (r46 of gpr) (r47 of gpr)
              (r48 of gpr) (r49 of gpr) (r50 of gpr)
              (r51 of gpr) (r52 of gpr) (r53 of gpr)
              (r54 of gpr) (r55 of gpr) (r56 of gpr)
              (r57 of gpr) (r58 of gpr) (r59 of gpr)
              (r60 of gpr) (r61 of gpr) (r62 of gpr)
              (r63 of gpr) (r64 of gpr) (r65 of gpr)
              (r66 of gpr) (r67 of gpr) (r68 of gpr)
              (r69 of gpr) (r70 of gpr) (r71 of gpr)
              (r72 of gpr) (r73 of gpr) (r74 of gpr)
              (r75 of gpr) (r76 of gpr) (r77 of gpr)
              (r78 of gpr) (r79 of gpr) (r80 of gpr)
              (r81 of gpr) (r82 of gpr) (r83 of gpr)
              (r84 of gpr) (r85 of gpr) (r86 of gpr)
              (r87 of gpr) (r88 of gpr) (r89 of gpr)
              (r90 of gpr) (r91 of gpr) (r92 of gpr)
              (r93 of gpr) (r94 of gpr) (r95 of gpr)
              (r96 of gpr) (r97 of gpr) (r98 of gpr)
              (r99 of gpr) (r100 of gpr) (r101 of gpr)
              (r102 of gpr) (r103 of gpr) (r104 of gpr)
              (r105 of gpr) (r106 of gpr) (r107 of gpr)
              (r108 of gpr) (r109 of gpr) (r110 of gpr)
              (r111 of gpr) (r112 of gpr) (r113 of gpr)
              (r114 of gpr) (r115 of gpr) (r116 of gpr)
              (r117 of gpr) (r118 of gpr) (r119 of gpr)
              (r120 of gpr) (r121 of gpr) (r122 of gpr)
              (r123 of gpr) (r124 of gpr) (r125 of gpr)
              (r126 of gpr) (r127 of gpr) (r128 of gpr)
              (r129 of gpr) (r130 of gpr) (r131 of gpr)
              (r132 of gpr) (r133 of gpr) (r134 of gpr)
              (r135 of gpr) (r136 of gpr) (r137 of gpr)
              (r138 of gpr) (r139 of gpr) (r140 of gpr)
              (r141 of gpr) (r142 of gpr) (r143 of gpr)
              (r144 of gpr) (r145 of gpr) (r146 of gpr)
              (r147 of gpr) (r148 of gpr) (r149 of gpr)
              (r150 of gpr) (r151 of gpr) (r152 of gpr)
              (r153 of gpr) (r154 of gpr) (r155 of gpr)
              (r156 of gpr) (r157 of gpr) (r158 of gpr)
              (r159 of gpr) (r160 of gpr) (r161 of gpr)
              (r162 of gpr) (r163 of gpr) (r164 of gpr)
              (r165 of gpr) (r166 of gpr) (r167 of gpr)
              (r168 of gpr) (r169 of gpr) (r170 of gpr)
              (r171 of gpr) (r172 of gpr) (r173 of gpr)
              (r174 of gpr) (r175 of gpr) (r176 of gpr)
              (r177 of gpr) (r178 of gpr) (r179 of gpr)
              (r180 of gpr) (r181 of gpr) (r182 of gpr)
              (r183 of gpr) (r184 of gpr) (r185 of gpr)
              (r186 of gpr) (r187 of gpr) (r188 of gpr)
              (r189 of gpr) (r190 of gpr) (r191 of gpr)
              (r192 of gpr) (r193 of gpr) (r194 of gpr)
              (r195 of gpr) (r196 of gpr) (r197 of gpr)
              (r198 of gpr) (r199 of gpr) (r200 of gpr)
              (r201 of gpr) (r202 of gpr) (r203 of gpr)
              (r204 of gpr) (r205 of gpr) (r206 of gpr)
              (r207 of gpr) (r208 of gpr) (r209 of gpr)
              (r210 of gpr) (r211 of gpr) (r212 of gpr)
              (r213 of gpr) (r214 of gpr) (r215 of gpr)
              (r216 of gpr) (r217 of gpr) (r218 of gpr)
              (r219 of gpr) (r220 of gpr) (r221 of gpr)
              (r222 of gpr) (r223 of gpr) (r224 of gpr)
              (r225 of gpr) (r226 of gpr) (r227 of gpr)
              (r228 of gpr) (r229 of gpr) (r230 of gpr)
              (r231 of gpr) (r232 of gpr) (r233 of gpr)
              (r234 of gpr) (r235 of gpr) (r236 of gpr)
              (r237 of gpr) (r238 of gpr) (r239 of gpr)
              (r240 of gpr) (r241 of gpr) (r242 of gpr)
              (r243 of gpr) (r244 of gpr) (r245 of gpr)
              (r246 of gpr) (r247 of gpr) (r248 of gpr)
              (r249 of gpr) (r250 of gpr) (r251 of gpr)
              (r252 of gpr) (r253 of gpr) (r254 of gpr)
              (r255 of gpr) 
              (p0 of predicate-register) (p1 of predicate-register) (p2 of predicate-register) 
              (p3 of predicate-register) (p4 of predicate-register) (p5 of predicate-register) 
              (p6 of predicate-register) (p7 of predicate-register) (p8 of predicate-register) 
              (p9 of predicate-register) (p10 of predicate-register) (p11 of predicate-register)
              (p12 of predicate-register) (p13 of predicate-register) (p14 of predicate-register) 
              (p15 of predicate-register)
              )



(deffacts MAIN::register-translations
          (decl const.zero -> const0 -> zero -> r0)
          (decl t0 -> tmp0 -> temp0 -> temporary0 -> r1)
          (decl t1 -> tmp1 -> temp1 -> temporary1 -> r2)
          (decl t2 -> tmp2 -> temp2 -> temporary2 -> r3)
          (decl t3 -> tmp3 -> temp3 -> temporary3 -> r4)
          (decl t4 -> tmp4 -> temp4 -> temporary4 -> r5)
          (decl t5 -> tmp5 -> temp5 -> temporary5 -> r6)
          (decl t6 -> tmp6 -> temp6 -> temporary6 -> r7)
          (decl t7 -> tmp7 -> temp7 -> temporary7 -> r8)
          (decl stack-pointer -> sp -> r9)
          (decl gcond -> pred-bits -> all-preds -> r10)
          (decl counter -> ctr -> count -> r11)
          (decl loc0 -> local0 -> r12)
          (decl loc1 -> local1 -> r13)
          (decl loc2 -> local2 -> r14)
          (decl loc3 -> local3 -> r15)
          (decl loc4 -> local4 -> r16)
          (decl loc5 -> local5 -> r17)
          (decl loc6 -> local6 -> r18)
          (decl loc7 -> local7 -> r19)
          (decl link-backup -> lrbak -> r20)
          (decl return-value -> ret.val -> r21)
          (decl ip-backup -> r22)
          (decl const.one -> const1 -> one -> r23)
          (decl const.two -> const2 -> two -> r24)
          (decl const.four -> const4 -> four -> r25)
          (decl sys0 -> system0 -> r248) 
          (decl sys1 -> system1 -> r249) 
          (decl sys2 -> system2 -> r250) 
          (decl sys3 -> system3 -> r251) 
          (decl sys4 -> system4 -> r252) 
          (decl sys5 -> system5 -> r253)
          (decl sys6 -> system6 -> r254)
          (decl sys7 -> system7 -> r255)
          (decl pfalse -> p1)
          (decl ptrue -> p0))
