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
          (defaliases Add -> { add }
                      AddImmediate -> { addi }
                      Subtract -> { sub }
                      SubtractImmediate -> { subi }
                      Multiply -> { mul }
                      MultiplyImmediate -> { muli }
                      Divide -> { div }
                      DivideImmediate -> { divi }
                      Remainder -> { rem } 
                      RemainderImmediate -> { remi }
                      ShiftLeft -> { shl }
                      ShiftLeftImmediate -> { shli }
                      ShiftRight -> { shr }
                      ShiftRightImmediate -> { shri }
                      BinaryAnd -> { and! binary-and }
                      BinaryOr -> { or! binary-or }
                      UnaryNot -> { not! unary-not }
                      BinaryExclusiveOr -> { xor binary-xor }
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
          (defaliases BranchUnconditionalImmediate -> { bi bui }
                      BranchUnconditionalImmediateAndLink -> { bil buil }
                      BranchUnconditionalRegister -> { b br bu bur }
                      BranchUnconditionalRegisterAndLink -> { bl brl bul burl }
                      BranchConditionalImmediate -> { bci }
                      BranchConditionalImmediateAndLink -> { bcil }
                      BranchConditionalRegister -> { bcr }
                      BranchConditionalRegisterAndLink -> { bcrl }
                      BranchUnconditionalToTheLinkRegister -> { blr bulr }
                      BranchUnconditionalToTheLinkRegisterAndLink -> { blrl bulrl }
                      BranchConditionalToTheLinkRegister -> { bclr }
                      BranchConditionalToTheLinkRegisterAndLink -> { bclrl }
                      ReturnFromError -> { rfe error-ret }
                      )
          (operation-group (kind move)
                           (operations MoveRegisterContents LoadImmediate SwapRegisterContents LoadFromData
                                       LoadFromDataWithImmediateAddress LoadFromDataWithOffset StoreToData
                                       StoreToDataWithImmediateAddress StoreToDataWithOffset PushDataOntoStack
                                       PushImmediateOntoStack PopDataFromStack LoadFromCode StoreToCode
                                       LoadFromIO StoreToIO LoadFromIOWithOffset StoreToIOWithOffset
                                       MoveFromIP MoveToIP MoveFromLinkRegister MoveToLinkRegister
                                       SaveAllRegisters RestoreAllRegisters))
          (defaliases MoveRegisterContents -> { move mov transfer }
                      SwapRegisterContents -> { swp swap transpose }
                      LoadImmediate -> { load.const ldconst set assign }
                      LoadFromData -> { ld load ld.data load.data <-data }
                      LoadFromDataWithImmediateAddress -> { ldi loadi ldi.data loadi.data }
                      LoadFromDataWithOffset -> { ldwo loadwo ldwo.data loadwo.data }
                      StoreToData -> { st store st.data store.data ->data }
                      StoreToDataWithImmediateAddress -> { sti storei sti.data storei.data }
                      StoreToDataWithOffset -> { stwo storewo stwo.data storewo.data }
                      PushDataOntoStack -> { push push.stack }
                      PushImmediateOntoStack -> { pushi pushi.stack }
                      PopDataFromStack -> { pop pop.stack }
                      LoadFromCode -> { ldc loadc ld.code load.code }
                      StoreToCode -> { stc storec st.code store.code }
                      LoadFromIO -> { ldio loadio ld.io load.io }
                      StoreToIO -> { stio storeio st.io store.io }
                      LoadFromIOWithOffset -> { ldiowo loadiowo ldwo.io loadwo.io }
                      StoreToIOWithOffset -> { stiowo storeiowo stwo.io storewo.io }
                      MoveFromIP -> { mfip gpr<-ip }
                      MoveToIP -> { mtip gpr->ip  }
                      MoveFromLinkRegister -> { mflr gpr<-lr }
                      MoveToLinkRegister -> { mtlr gpr->lr }
                      SaveAllRegisters -> { sregs save.regs }
                      RestoreAllRegisters -> { rregs restore.regs }
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
                      LessThan -> { lt }
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
          (defaliases SaveConditionRegisters -> { save.crs sav.crs crs.save crs.sav crssav crssave savecrs savcrs gpr<-crs crs->gpr }
                      RestoreConditionRegisters -> { restore.crs restorecrs gpr->crs crs.restore crsrestore crs<-gpr }
                      ConditionRegisterExclusiveOr -> { cr.xor xor.cr crxor xorcr }
                      ConditionRegisterNot -> { cr.not not.cr crnot notcr }
                      ConditionRegisterAnd -> { cr.and and.cr crand andcr }
                      ConditionRegisterOr -> { cr.or or.cr cror orcr }
                      ConditionRegisterNand -> { cr.nand nand.cr crnand nandcr }
                      ConditionRegisterNor -> { cr.nor nor.cr crnor norcr }
                      ConditionRegisterSwap -> { cr.swap swap.cr crswap swapcr }
                      ConditionRegisterMove -> { cr.move move.cr crmove movecr mov.cr cr.mov movcr crmov })
          )


