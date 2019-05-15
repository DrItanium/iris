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

(deffacts descriptions 
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
          (defaliases Add -> { add combine }
                      AddImmediate -> { addi combinei add.imm combine.imm }
                      Subtract -> { sub subtract }
                      SubtractImmediate -> { subi sub.imm subtracti subtract.imm }
                      Multiply -> { mul multiply }
                      MultiplyImmediate -> { muli mul.imm multiply.imm multiplyi }
                      Divide -> { div divide }
                      DivideImmediate -> { divi div.imm divide.imm dividei }
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
                      )
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
          (defaliases BranchUnconditionalImmediate -> { bi bui branch.imm }
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
                      )
          (operation-group (kind move)
                           (operations MoveRegisterContents LoadImmediate SwapRegisterContents LoadFromData
                                       LoadFromDataWithImmediateAddress LoadFromDataWithOffset StoreToData
                                       StoreToDataWithImmediateAddress StoreToDataWithOffset PushDataOntoStack
                                       PushImmediateOntoStack PopDataFromStack LoadFromCode StoreToCode
                                       LoadFromIO StoreToIO LoadFromIOWithOffset StoreToIOWithOffset
                                       MoveFromIP MoveToIP MoveFromLinkRegister MoveToLinkRegister
                                       SaveAllRegisters RestoreAllRegisters))
          (defaliases MoveRegisterContents -> { move mov transfer reg.move move.reg copy cpy copy.reg cpy.reg reg->reg }
                      SwapRegisterContents -> { swp swap transpose reg.swap swap.reg }
                      LoadImmediate -> { load.const ldconst set assign imm->reg }
                      LoadFromData -> { ld load ld.data load.data data->gpr data.load }
                      LoadFromDataWithImmediateAddress -> { ldi loadi ldi.data loadi.data data.loadi }
                      LoadFromDataWithOffset -> { ldwo loadwo ldwo.data loadwo.data data.loadwo }
                      StoreToData -> { st store st.data store.data gpr->data data.store }
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
                      )
          (operation-group (kind compare)
                           (operations Equals EqualsImmediate NotEqual NotEqualImmediate
                                       LessThan LessThanImmediate GreaterThan GreaterThanImmediate
                                       LessThanOrEqualTo LessThanOrEqualToImmediate GreaterThanOrEqualTo
                                       GreaterThanOrEqualToImmediate))
          (defaliases Equals -> { eq }
                      EqualsImmediate -> { eqi }
                      NotEqual -> { ne neq }
                      NotEqualImmediate -> { nei neqi }
                      LessThan -> { lt less.than }
                      LessThanImmediate -> { lti }
                      GreaterThan -> { gt }
                      GreaterThanImmediate -> { gti }
                      LessThanOrEqualTo -> { le }
                      LessThanOrEqualToImmediate -> { lei }
                      GreaterThanOrEqualTo -> { ge }
                      GreaterThanOrEqualToImmediate -> { gei })
          (operation-group (kind condition-register-op)
                           (operations SaveConditionRegisters RestoreConditionRegisters
                                       ConditionRegisterExclusiveOr ConditionRegisterNot
                                       ConditionRegisterAnd ConditionRegisterOr
                                       ConditionRegisterNand ConditionRegisterNor
                                       ConditionRegisterSwap ConditionRegisterMove))
          (defaliases SaveConditionRegisters -> { save.crs sav.crs crs.save crs.sav crssav crssave savecrs savcrs crs->gpr }
                      RestoreConditionRegisters -> { restore.crs restorecrs gpr->crs crs.restore crsrestore gpr->crs }
                      ConditionRegisterExclusiveOr -> { cr.xor xor.cr crxor xorcr }
                      ConditionRegisterNot -> { cr.not not.cr crnot notcr }
                      ConditionRegisterAnd -> { cr.and and.cr crand andcr }
                      ConditionRegisterOr -> { cr.or or.cr cror orcr }
                      ConditionRegisterNand -> { cr.nand nand.cr crnand nandcr }
                      ConditionRegisterNor -> { cr.nor nor.cr crnor norcr }
                      ConditionRegisterSwap -> { cr.swap swap.cr crswap swapcr }
                      ConditionRegisterMove -> { cr.move move.cr crmove movecr mov.cr cr.mov movcr crmov cr.copy copy.cr cr.transfer transfer.cr })
          )


