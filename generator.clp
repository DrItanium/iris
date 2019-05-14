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

(defmessage-handler INTEGER to-string primary
                    ()
                    (str-cat ?self))
(defmessage-handler LEXEME to-string primary
                    ()
                    ?self)
(defclass component
  (is-a USER)
  (role abstract)
  (pattern-match non-reactive)
  (slot title
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default-dynamic FALSE))
  (message-handler to-string primary)
  (message-handler init after))

(defmessage-handler component init after
                    ()
                    (if (not (dynamic-get title)) then
                      (dynamic-put title
                                   (instance-name-to-symbol (instance-name ?self)))))

(defmessage-handler component to-string primary
                    ()
                    (send (dynamic-get title)
                          to-string))

(defclass register
  (is-a component)
  (role concrete)
  (pattern-match reactive)
  (message-handler to-string primary))

(defmessage-handler register to-string primary
                    ()
                    (format nil
                            "[%s]"
                            (send (dynamic-get title)
                                  to-string)))
(defclass tagged-component
  "Tags a given with extra information, this is used during pattern matching"
  (is-a component)
  (role concrete)
  (pattern-match reactive)
  (slot target
        (type INSTANCE)
        (allowed-classes component)
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot tags
             (type SYMBOL)
             (storage local)
             (visibility public)
             (default ?NONE))
  (message-handler to-string primary))
(defmessage-handler tagged-component to-string primary
                    ()
                    ; do an indirect dispatch
                    (send (dynamic-get target)
                          to-string))

(defclass opcode
  (is-a component)
  (role concrete)
  (pattern-match reactive))



(defclass aliased-constant 
  "Top level constant concept to differentiate against normal components"
  (is-a component))



(defclass label
  (is-a aliased-constant)
  (role concrete)
  (pattern-match reactive))

(defclass numerical-constant
  (is-a aliased-constant)
  (role concrete)
  (pattern-match reactive)
  (slot value
        (type INTEGER
              INSTANCE)
        (allowed-classes aliased-constant)
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler to-string primary))

(defmessage-handler numerical-constant to-string primary
                    ()
                    (send (dynamic-get value)
                          to-string))


(defclass instruction 
  (is-a USER)
  (slot op 
        (type INSTANCE)
        (allowed-classes opcode)
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot arguments
             (type INSTANCE)
             (allowed-classes tagged-component)))

(deftemplate instruction-class
             "Maps a given kind to a given set of tagged components, used during code emission"
             (slot kind
                   (type SYMBOL)
                   (default ?NONE))
             (multislot args
                        (type SYMBOL))
             (multislot members
                        (type SYMBOL)
                        (default ?NONE)))
(deftemplate operation-group
             (slot kind
                   (type SYMBOL)
                   (default ?NONE))
             (multislot operations
                        (type SYMBOL)
                        (default ?NONE)))

(deftemplate instruction-description
             (slot kind
                   (type SYMBOL)
                   (default ?NONE))
             (slot class
                   (type SYMBOL)
                   (default ?NONE))
             (slot group
                   (type SYMBOL)
                   (default FALSE)))

(deftemplate alias-decl
             (slot real-name
                   (type SYMBOL)
                   (default ?NONE))
             (slot alias
                   (type SYMBOL)
                   (default ?NONE)))

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


          (operation-group (kind arithmetic)
                           (operations Add Subtract Multiply Divide 
                                       Remainder ShiftLeft ShiftRight BinaryAnd
                                       BinaryOr UnaryNot BinaryExclusiveOr BinaryNand
                                       BinaryNor AddImmediate SubtractImmediate MultiplyImmediate
                                       DivideImmediate RemainderImmediate ShiftLeftImmediate ShiftRightImmediate
                                       Min Max))
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
          (operation-group (kind move)
                           (operations MoveRegisterContents LoadImmediate SwapRegisterContents LoadFromData
                                       LoadFromDataWithImmediateAddress LoadFromDataWithOffset StoreToData
                                       StoreToDataWithImmediateAddress StoreToDataWithOffset PushDataOntoStack
                                       PushImmediateOntoStack PopDataFromStack LoadFromCode StoreToCode
                                       LoadFromIO StoreToIO LoadFromIOWithOffset StoreToIOWithOffset
                                       MoveFromIP MoveToIP MoveFromLinkRegister MoveToLinkRegister
                                       SaveAllRegisters RestoreAllRegisters))
          (operation-group (kind compare)
                           (operations Equals EqualsImmediate NotEqual NotEqualImmediate
                                       LessThan LessThanImmediate GreaterThan GreaterThanImmediate
                                       LessThanOrEqualTo LessOrEqualToImmediate GreaterThanOrEqualTo
                                       GreaterThanOrEqualToImmediate))
          (operation-group (kind condition-register-op)
                           (operations SaveConditionRegisters RestoreConditionRegisters
                                       ConditionRegisterExclusiveOr ConditionRegisterNot
                                       ConditionRegisterAnd ConditionRegisterOr
                                       ConditionRegisterNand ConditionRegisterNor
                                       ConditionRegisterSwap ConditionRegisterMove))
          )
