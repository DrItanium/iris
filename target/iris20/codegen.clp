;-----------------------------------------------------------------------------
; Generate the basic boot code for an iris20 system
;-----------------------------------------------------------------------------
(defclass MAIN::program
  (is-a USER)
  (multislot contents))
(deffunction MAIN::enum->int
             (?symbol ?collection)
             (symbol->zero-index ?symbol
                                 ?collection))
(deffunction MAIN::section-descriptor->int
             (?symbol)
             (enum->int ?symbol
                        ?*iris20-enumSectionType*))
(deffunction MAIN::operation->int
             (?id)
             (enum->int ?id
                        ?*iris20-enumOperation*))
(deffunction MAIN::system-call->int
             (?id)
             (enum->int ?id
                        ?*iris20-enumSystemCalls*))

(defmethod MAIN::iris20-encode-Operation
  ((?value INTEGER)
   (?field SYMBOL))
  (iris20-encode-Operation ?value
                           (operation->int ?field)))
(defgeneric MAIN::encode)
(defgeneric MAIN::decode)


(deffunction MAIN::generic-encode-decode-operation
             (?action ?section $?args)
             (funcall (sym-cat (target-architecture)
                               -
                               ?action
                               -
                               ?section)
                      (expand$ ?args)))

(defmethod MAIN::encode
  ((?section SYMBOL)
   (?value INTEGER)
   (?field INTEGER
           SYMBOL))
  (generic-encode-decode-operation encode
                                   ?section
                                   ?value
                                   ?field))

(defmethod MAIN::decode
  ((?section SYMBOL)
   (?value INTEGER))
  (generic-encode-decode-operation decode
                                   ?section
                                   ?value))

(defgeneric MAIN::encode-register)
(defgeneric MAIN::decode-register)

(defmethod MAIN::encode-register
  ((?index INTEGER)
   (?section INTEGER))
  (encode SectionIndex
          (encode SectionDescriptor
                  0
                  ?section)
          ?index))



(defmethod MAIN::encode-register
  ((?index INTEGER)
   (?section SYMBOL))
  (encode-register ?index
                   (section-descriptor->int ?section)))

(defmethod MAIN::decode-register
  ((?register INTEGER))
  (create$ (decode SectionDescriptor
                   ?register)
           (decode SectionIndex
                   ?register)))
(deffunction MAIN::build-register-encoder
             (?title)
             (buildf "(deffunction MAIN::%s (?i) (encode-register ?i %s))"
                     (lowcase ?title)
                     ?title))
(map build-register-encoder
     (expand$ ?*iris20-enumSectionType*))

(build-stubbed-message-handler INTEGER
                               encode)
(defglobal MAIN
           ?*addr-max* = (hex->int 0x03FFFFFF)
           ?*space-size* = (hex->int 0x00FFFFFF)
           ?*half-space* = (div ?*space-size* 2)
           ?*stack-bottom* = ?*addr-max*
           ?*stack-top* = (- ?*stack-bottom*
                             ?*half-space*)
           ?*call-stack-bottom* = (- ?*stack-top* 1)
           ?*call-stack-top* = (- ?*call-stack-bottom*
                                  ?*half-space*)
           ?*memory1-end* = (- ?*stack-top* 1)
           ?*memory1-start* = (- ?*memory1-end*
                                 ?*space-size*)
           ?*memory0-end* = (- ?*memory1-start* 1)
           ?*memory0-start* = (- ?*memory0-end*
                                 ?*space-size*)
           ?*addr-table-end* = (- ?*memory0-start* 1)
           ?*addr-table-begin* = (- ?*addr-table-end*
                                    (hex->int 0xFFFF))
           ?*code-end* = (- ?*addr-table-begin* 1)
           ?*code-start* = 0
           ?*register-count* = 64
           ?*register-ip* = (- ?*register-count* 1)
           ?*register-lr* = (- ?*register-count* 2)
           ?*register-sp* = (- ?*register-count* 3)
           ?*register-stack-pointer-top* = (- ?*register-count* 4)
           ?*register-stack-pointer-bottom* = (- ?*register-count* 5)
           ?*register-memory-space0-start* = (- ?*register-count* 6)
           ?*register-memory-space0-end* = (- ?*register-count* 7)
           ?*register-memory-space1-start* = (- ?*register-count* 8)
           ?*register-memory-space1-end* = (- ?*register-count* 9)
           ?*register-call-stack-bottom* = (- ?*register-count* 10)
           ?*register-call-stack-top* = (- ?*register-count* 11)
           ?*register-code-end* = (- ?*register-count* 12)
           ?*register-code-start* = (- ?*register-count* 13)
           ?*register-call-stack-pointer* = (- ?*register-count* 14)
           ?*register-address-table-base* = (- ?*register-count* 15)
           ?*register-address-table-pointer* = (- ?*register-count* 16)
           ?*register-temp0* = (- ?*register-count* 17)
           ?*register-temp1* = (- ?*register-count* 18)
           ?*register-temp2* = (- ?*register-count* 19)
           ?*register-temp3* = (- ?*register-count* 20)
           ?*register-temp4* = (- ?*register-count* 21)
           ?*register-temp5* = (- ?*register-count* 22)
           ?*register-temp6* = (- ?*register-count* 23)
           ?*register-ret0* = (- ?*register-count* 24)
           ?*register-ret1* = (- ?*register-count* 25)
           ?*register-arg0* = (- ?*register-count* 26)
           ?*register-arg1* = (- ?*register-count* 27)
           ?*register-arg2* = (- ?*register-count* 28))

(deffunction MAIN::link-register () ?*register-lr*)
(deffunction MAIN::instruction-pointer () ?*register-ip*)
(deffunction MAIN::stack-pointer () ?*register-sp*)

(deffunction MAIN::has-register-prefix
             (?title)
             (str-index "register-"
                        ?title))
(deffunction MAIN::build-register-operation
             (?title)
             (buildf "(deffunction MAIN::register:%s () ?*%s*)"
                     (sub-string (+ (str-length "register-")
                                    1)
                                 (str-length ?title)
                                 ?title)
                     ?title))

(map build-register-operation
     (expand$ (filter has-register-prefix
                      (expand$ (get-defglobal-list MAIN)))))
(loop-for-count (?i 0 63) do
                (buildf "(deffunction MAIN::register:%s () %d)"
                        (sym-cat r
                                 ?i)
                        ?i))

(deffunction MAIN::register:
             (?name)
             (funcall (sym-cat register: ?name)))


(defgeneric MAIN::output-bytes-to-router)
(defmethod MAIN::output-bytes-to-router
  ((?bytes MULTIFIELD)
   (?router SYMBOL))
  (progn$ (?byte ?bytes)
          (put-char ?router
                    ?byte)))
(defmethod MAIN::output-bytes-to-router
  ((?bytes MULTIFIELD))
  (output-bytes-to-router ?bytes
                          t))


(defgeneric MAIN::return-from-register)
(defgeneric MAIN::make:atom)
(defgeneric MAIN::make:molecule)
(defgeneric MAIN::make:word-container)
(defgeneric MAIN::return-from-stack)



(defclass MAIN::instruction
  (is-a USER)
  (role abstract)
  (slot operation
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot dest
        (type INTEGER)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot src0
        (type INTEGER)
        (visibility public)
        (storage local))
  (slot src1
        (type INTEGER)
        (visibility public)
        (storage local))
  (slot immediate
        (type INTEGER
              INSTANCE)
        (allowed-classes label)
        (storage local)
        (visibility public)
        (default-dynamic 0))
  (slot operation-suffix
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default Operation))
  (slot dest-suffix
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot src0-suffix
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot src1-suffix
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default ?NONE))
  (message-handler MAIN::encode primary))
(defclass MAIN::atom
  (is-a instruction)
  (role concrete)
  (pattern-match reactive)
  (slot dest-suffix
        (source composite)
        (default Destination))
  (slot src0-suffix
        (source composite)
        (default Source0))
  (slot src1-suffix
        (source composite)
        (default Source1))
  (message-handler MAIN::encode primary))
(defclass MAIN::molecule
  (is-a instruction)
  (role concrete)
  (pattern-match reactive)
  (slot dest-suffix
        (source composite)
        (default MoleculeDestination))
  (slot src0-suffix
        (source composite)
        (default MoleculeSource0))
  (slot src1-suffix
        (source composite)
        (default MoleculeSource1))
  (message-handler MAIN::encode primary))

(defmethod MAIN::make:molecule
  ((?operation SYMBOL)
   (?dest INTEGER)
   (?src0 INTEGER)
   (?src1 INTEGER))
  (make-instance of molecule
                 (operation ?operation)
                 (dest ?dest)
                 (src0 ?src0)
                 (src1 ?src1)))
(defmethod MAIN::make:molecule
  ((?operation SYMBOL)
   (?dest INTEGER))
  (make:molecule ?operation
                 ?dest
                 0
                 0))
(defmethod MAIN::make:molecule
  ((?operation SYMBOL))
  (make:molecule ?operation
                 0))

(defmethod MAIN::make:molecule
  ((?operation SYMBOL)
   (?dest INTEGER)
   (?src0 INTEGER))
  (make:molecule ?operation
                 ?dest
                 ?src0
                 0))

(defmethod MAIN::make:atom
  ((?operation SYMBOL))
  (make:atom ?operation
             0))

(defmethod MAIN::make:atom
  ((?operation SYMBOL)
   (?destination INTEGER))
  (make:atom ?operation
             ?destination
             0))

(defmethod MAIN::make:atom
  ((?operation SYMBOL)
   (?dest INTEGER)
   (?src0 INTEGER))
  (make:atom ?operation
             ?dest
             ?src0
             0))

(defmethod MAIN::make:atom
  ((?operation SYMBOL)
   (?dest INTEGER)
   (?src0 INTEGER)
   (?src1 INTEGER))
  (make-instance of atom
                 (operation ?operation)
                 (dest ?dest)
                 (src0 ?src0)
                 (src1 ?src1)))


(defclass MAIN::word-container
  (is-a USER)
  (multislot contents
             (storage local)
             (visibility public)
             (default ?NONE))
  (message-handler MAIN::encode primary))
(defclass MAIN::label
  (is-a USER)
  (slot title
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot address
        (type SYMBOL
              INTEGER)
        (allowed-symbols FALSE)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default-dynamic FALSE)))

(defmessage-handler MAIN::label encode primary () ?self:address)

(defgeneric MAIN::.label)
(defmethod MAIN::.label
  ((?title SYMBOL
           (not (instance-existp (symbol-to-instance-name ?current-argument)))))
  (make-instance ?title of label
                 (title ?title)))
(defmethod MAIN::.label
  ((?title SYMBOL
           (instance-existp (symbol-to-instance-name ?current-argument))))
  (symbol-to-instance-name ?title))

(defmethod MAIN::make:word-container
  ((?first INTEGER
           atom)
   (?second INTEGER
            atom))
  (make-instance of word-container
                 (contents ?first
                           ?second)))
(defmethod MAIN::make:word-container
  ((?wide-instruction INTEGER
                      molecule))
  (make-instance of word-container
                 (contents ?wide-instruction)))
(defmethod MAIN::encode
  ((?thing molecule
           atom))
  (send ?thing
        encode))
(defmethod MAIN::encode
  ((?first atom)
   (?second atom))
  (encode SecondAtom
          (encode FirstAtom
                  0
                  (encode ?first))
          (encode ?second)))
(defmessage-handler MAIN::word-container encode primary
                    ()
                    (encode MoleculeContainsOneInstruction
                            (encode (expand$ ?self:contents))
                            (switch (length$ ?self:contents)
                                    (case 1 then 1)
                                    (case 2 then 0)
                                    (default (format werror
                                                     "ERROR: word-container contains %d instructions!%n"
                                                     (length$ ?self:contents))
                                             (create$)))))

(defmethod MAIN::return-instruction
  ((?register INTEGER))
  (make:atom BranchUnconditionalRegister
             ?register))
(defmethod MAIN::return-from-stack
  ((?sp INTEGER))
  (return-instruction (stack ?sp)))
(defmethod MAIN::return-from-stack
  ()
  (return-from-stack (stack-pointer)))
(defmethod MAIN::return-from-register
  ((?reg INTEGER))
  (return-instruction (register ?reg)))
(deffunction MAIN::return-from-memory
             (?r)
             (return-instruction (memory ?r)))
(deffunction MAIN::return-to-register
             (?r)
             (return-instruction (register ?r)))
(deffunction MAIN::return-to-link-register
             ()
             (return-to-register (link-register)))
(defgeneric MAIN::number-of-args)
(defgeneric MAIN::operation-to-call)
(deffunction MAIN::make:special-defmethod
             (?title ?symbol ?value)
             (buildf "(defmethod MAIN::%s
                        ((?a SYMBOL
                             (not (neq ?current-argument
                                       %s
                                       %s))))
                        %s)"
             ?title
             ?symbol
             (lowcase ?symbol)
             (str-cat ?value)))
(deffunction MAIN::immediatep
             (?symbol)
             (has-suffix ?symbol
                         Immediate))
(deffunction MAIN::make:operation-to-call
             (?symbol ?func)
             (make:special-defmethod MAIN::operation-to-call
                                     ?symbol
                                     ?func))

(deffunction MAIN::make:arg-count-function
             (?op ?count)
             (make:special-defmethod MAIN::number-of-args
                                     ?op
                                     ?count))

(deffunction MAIN::setoperationp
             (?symbol)
             (has-prefix ?symbol
                         Set))
(deffunction MAIN::build-generic
             (?title)
             (buildf "(defgeneric MAIN::%s)"
                     ?title))
(deffunction MAIN::make-operation-title
             (?title)
             (sym-cat op:
                      (lowcase ?title)))

(deffunction MAIN::make-stack-operation-title
             (?title)
             (sym-cat (make-operation-title ?title)
                      :stack))

(deffunction MAIN::make-default-data-stack-op
             (?title)
             (buildf "(defmethod MAIN::%s () (%s (stack-pointer)))"
                     ?title
                     ?title))

(deffunction MAIN::build-specific-operation-three-arg
             (?operation)
             (build-generic (bind ?title
                                  (make-operation-title ?operation)))
             (build-generic (bind ?stack-title
                                  (make-stack-operation-title ?operation)))
             (make:arg-count-function ?operation
                                      3)
             (make:operation-to-call ?operation
                                     ?title)
             (buildf "(defmethod MAIN::%s ((?dest INTEGER) (?src0 INTEGER) (?src1 INTEGER)) (make:atom %s ?dest ?src0 ?src1))"
                     ?title
                     ?operation)
             (buildf "(defmethod MAIN::%s ((?dest INTEGER) (?src0 INTEGER) (?src1 INTEGER)) (%s (stack ?dest) (stack ?src0) (stack ?src1)))"
                     ?stack-title
                     ?title)
             (buildf "(defmethod MAIN::%s ((?stack INTEGER)) (%s ?stack ?stack ?stack))"
                     ?stack-title
                     ?stack-title)
             (make-default-data-stack-op ?stack-title))

(deffunction MAIN::build-specific-operation-two-arg
             (?operation)
             (build-generic (bind ?title
                                  (make-operation-title ?operation)))
             (build-generic (bind ?stack-title
                                  (make-stack-operation-title ?operation)))
             (make:arg-count-function ?operation
                                      2)
             (make:operation-to-call ?operation
                                     ?title)
             (buildf "(defmethod MAIN::%s ((?dest INTEGER) (?src0 INTEGER)) (make:atom %s ?dest ?src0))"
                     ?title
                     ?operation)
             (buildf "(defmethod MAIN::%s ((?dest INTEGER) (?src0 INTEGER)) (%s (stack ?dest) (stack ?src0)))"
                     ?stack-title
                     ?title)
             (buildf "(defmethod MAIN::%s ((?stack INTEGER)) (%s ?stack ?stack))"
                     ?stack-title
                     ?stack-title)
             (make-default-data-stack-op ?stack-title))

(deffunction MAIN::build-specific-operation-one-arg
             (?operation)
             (build-generic (bind ?title
                                  (make-operation-title ?operation)))
             (build-generic (bind ?stack-title
                                  (make-stack-operation-title ?operation)))
             (make:arg-count-function ?operation
                                      1)
             (make:operation-to-call ?operation
                                     ?title)
             (buildf "(defmethod MAIN::%s ((?dest INTEGER)) (make:atom %s ?dest))"
                     ?title
                     ?operation)
             (buildf "(defmethod MAIN::%s ((?dest INTEGER)) (%s (stack ?dest)))"
                     ?stack-title
                     ?title)
             (make-default-data-stack-op ?stack-title))
(defclass MAIN::branch-immediate-atom
  (is-a atom)
  (slot immediate
        (source composite)
        (default ?NONE))
  (message-handler MAIN::encode primary))
(deffunction MAIN::encode-branch-operation
             (?operation-suffix ?operation ?imm-suffix ?immediate ?dest-suffix ?dest)
             (encode ?operation-suffix
                     (encode ?imm-suffix
                             (if (= (number-of-args ?operation)
                                    2) then
                               (encode ?dest-suffix
                                       0
                                       ?dest)
                               else
                               0)
                             ?immediate)
                     ?operation))
(defmessage-handler MAIN::branch-immediate-atom encode primary
                    ()
                    (encode-branch-operation ?self:operation-suffix
                                             ?self:operation
                                             Immediate
                                             ?self:immediate
                                             Destination
                                             ?self:dest))


(defclass MAIN::branch-immediate-molecule
  (is-a molecule)
  (slot width
        (type INTEGER)
        (allowed-integers 32
                          48)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot immediate
        (source composite)
        (default ?NONE))
  (message-handler MAIN::encode primary))
(defmessage-handler MAIN::branch-immediate-molecule encode primary
                    ()
                    (encode-branch-operation ?self:operation-suffix
                                             ?self:operation
                                             (sym-cat Immediate
                                                      ?self:width)
                                             ?self:immediate
                                             MoleculeDestination
                                             ?self:dest))

(deffunction MAIN::make:link-version
             (?op ?link)
             (if ?link then (sym-cat ?op Link) else ?op))
(deffunction MAIN::branch-unconditional-immediate
             (?address ?link)
             (make-instance of branch-immediate-atom
                            (operation (make:link-version BranchUnconditionalImmediate
                                                          ?link))
                            (dest 0)
                            (immediate ?address)))
(deffunction MAIN::branch-conditional-immediate
             (?dest ?address ?check-false ?link)
             (bind ?base-op
                   (if ?check-false then
                     BranchConditionalFalseImmediate
                     else
                     BranchConditionalTrueImmediate))
             (make-instance of branch-immediate-atom
                            (dest ?dest)
                            (operation (make:link-version ?base-op
                                                          ?link))
                            (immediate ?address)))
(deffunction MAIN::branch-unconditional-immediate32
             (?address ?link)
             (make-instance of branch-immediate-molecule
                            (width 32)
                            (operation (make:link-version BranchUnconditionalImmediate32
                                                          ?link))
                            (dest 0)
                            (immediate ?address)))

(deffunction MAIN::branch-unconditional-immediate48
             (?address ?link)
             (make-instance of branch-immediate-molecule
                            (width 48)
                            (operation (make:link-version BranchUnconditionalImmediate48
                                                          ?link))
                            (dest 0)
                            (immediate ?address)))

(deffunction MAIN::branch-conditional-immediate-molecule
             (?dest ?address ?check-false ?link ?width)
             (bind ?base-op
                   (make:link-version (sym-cat BranchConditional
                                               (if ?check-false then
                                                 False
                                                 else
                                                 True)
                                               Immediate
                                               ?width)
                                      ?link))
             (make-instance of branch-immediate-molecule
                            (operation ?base-op)
                            (width ?width)
                            (destination ?dest)
                            (immediate ?address)))

(deffunction MAIN::branch-conditional-immediate32
             (?dest ?address ?check-false ?link)
             (branch-conditional-immediate-molecule ?dest
                                                    ?address
                                                    ?check-false
                                                    ?link
                                                    32))

(deffunction MAIN::branch-conditional-immediate48
             (?dest ?address ?check-false ?link)
             (branch-conditional-immediate-molecule ?dest
                                                    ?address
                                                    ?check-false
                                                    ?link
                                                    48))

(deffunction MAIN::encode-set-operation
             (?op-suffix ?op ?dest-suffix ?dest ?imm-suffix ?imm)
             (encode ?op-suffix
                     (encode ?dest-suffix
                             (encode ?imm-suffix
                                     0
                                     (send ?imm
                                           encode))
                             ?dest)
                     ?op))
(defclass MAIN::set16-instruction
  (is-a atom)
  (slot operation
        (source composite)
        (default-dynamic Set16))
  (slot immediate
        (source composite)
        (default ?NONE))
  (message-handler MAIN::encode primary))
(defmessage-handler MAIN::set16-instruction encode primary
                    ()
                    (encode-set-operation ?self:operation-suffix
                                          ?self:operation
                                          ?self:dest-suffix
                                          ?self:dest
                                          Immediate
                                          ?self:immediate))

(defclass MAIN::wide-set-instruction
  (is-a molecule)
  (slot immediate
        (source composite)
        (default ?NONE))
  (message-handler MAIN::encode primary))

(defmethod MAIN::encode
  ((?operation SYMBOL
               (eq ?current-argument
                   Set32))
   (?value INTEGER)
   (?field INTEGER
           SYMBOL))
  (encode Immediate32
          ?value
          ?field))
(defmethod MAIN::encode
  ((?operation SYMBOL
               (eq ?current-argument
                   Set48))
   (?value INTEGER)
   (?field INTEGER
           SYMBOL))
  (encode Immediate48
          ?value
          ?field))

(defmessage-handler MAIN::wide-set-instruction encode primary
                    ()
                    (encode-set-operation ?self:operation-suffix
                                          ?self:operation
                                          MoleculeDestination
                                          ?self:dest
                                          ?self:operation
                                          ?self:immediate))

(deffunction MAIN::make:wide-set-instruction
             (?dest ?i ?op)
             (make:word-container
               (make-instance of wide-set-instruction
                              (operation ?op)
                              (immediate ?i)
                              (dest ?dest))))


(deffunction MAIN::set16
             (?dest ?i)
             (make-instance of set16-instruction
                            (dest ?dest)
                            (immediate ?i)))
(deffunction MAIN::set32
             (?dest ?i)
             (make:wide-set-instruction ?dest
                                        ?i
                                        Set32))

(deffunction MAIN::set48
             (?dest ?i)
             (make:wide-set-instruction ?dest
                                        ?i
                                        Set48))
(deffunction MAIN::make:two-argument-set-count
             (?t)
             (make:arg-count-function ?t
                                      2))
(deffunction MAIN::make:operation-to-call:set
             (?t)
             (make:operation-to-call ?t
                                     (lowcase ?t)))
(deffunction MAIN::make:set-operation:to-call-and-arg-count
             (?t)
             (make:operation-to-call:set ?t)
             (make:two-argument-set-count ?t))


(map make:set-operation:to-call-and-arg-count
     Set16
     Set32
     Set48)

(map build-specific-operation-one-arg
     BranchUnconditionalRegister
     BranchUnconditionalRegisterLink)
(map build-specific-operation-two-arg
     Move
     Swap
     BinaryNot
     BranchConditionalTrueRegister
     BranchConditionalFalseRegister
     BranchConditionalTrueRegisterLink
     BranchConditionalFalseRegisterLink)
(map build-specific-operation-three-arg
     SystemCall
     Add
     Sub
     Mul
     Div
     Rem
     ShiftLeft
     ShiftRight
     BinaryAnd
     BinaryOr
     BinaryXor
     AddImmediate
     SubImmediate
     MulImmediate
     DivImmediate
     RemImmediate
     ShiftLeftImmediate
     ShiftRightImmediate
     Eq
     EqImmediate
     Neq
     NeqImmediate
     LessThan
     LessThanImmediate
     GreaterThan
     GreaterThanImmediate
     LessThanOrEqualTo
     LessThanOrEqualToImmediate
     GreaterThanOrEqualTo
     GreaterThanOrEqualToImmediate
     BinaryAndImmediate
     BinaryOrImmediate
     BinaryXorImmediate
     BinaryNand
     BinaryNandImmediate
     BranchIfThenElseLinkPredTrue
     BranchIfThenElseLinkPredFalse
     BranchIfThenElseNormalPredTrue
     BranchIfThenElseNormalPredFalse)


(deffunction MAIN::push
             (?sp ?value)
             (op:move (stack ?sp)
                      ?value))
(deffunction MAIN::pop
             (?sp ?dest)
             (op:move ?dest
                      (stack ?sp)))

(deffunction MAIN::op:load
             (?dest ?src)
             (op:move ?dest
                      (memory ?src)))
(deffunction MAIN::op:store
             (?dest ?src)
             (op:move (memory ?dest)
                      ?src))
(deffunction MAIN::nop
             ()
             (op:swap (register:r0)
                      (register:r0)))

(defgeneric MAIN::push16)
(defmethod MAIN::push16
  ((?immediate INTEGER
               label)
   (?sp INTEGER))
  (set16 (stack ?sp)
         ?immediate))
(defmethod MAIN::push16
  ((?immediate INTEGER))
  (push16 ?immediate
          (stack-pointer)))


(deffunction MAIN::store16
             (?address ?imm)
             (set16 (memory ?address)
                    ?imm))

(defgeneric MAIN::op:increment)
(defgeneric MAIN::op:decrement)
(defgeneric MAIN::op:double)
(defgeneric MAIN::op:halve)

(defmethod MAIN::op:increment
  ((?dest INTEGER)
   (?src INTEGER))
  (op:addimmediate ?dest
                   ?src
                   1))

(defmethod MAIN::op:decrement
  ((?dest INTEGER)
   (?src INTEGER))
  (op:subimmediate ?dest
                   ?src
                   1))

(defmethod MAIN::op:double
  ((?dest INTEGER)
   (?src INTEGER))
  (op:mulimmediate ?dest
                   ?src
                   2))

(defmethod MAIN::op:halve
  ((?dest INTEGER)
   (?src INTEGER))
  (op:divimmediate ?dest
                   ?src
                   2))

(deffunction MAIN::stack-store
             (?sp)
             (bind ?rtemp0
                   (register (register:temp0)))
             (make:word-container (pop ?sp
                                       ?rtemp0)
                                  (op:store ?rtemp0
                                            (stack ?sp))))

(deffunction MAIN::stack-load
             (?sp ?dest)
             (make:word-container (pop ?sp
                                       (register ?dest))
                                  (op:load (register ?dest)
                                           ?dest)))
(defclass MAIN::decoder
  (is-a USER)
  (slot reference
        (type INSTANCE)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot mask
        (type INTEGER)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot shift-count
        (type INTEGER)
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler encode primary))

(defmessage-handler MAIN::decoder encode primary
                    ()
                    (decode-bits (send ?self:reference
                                       encode)
                                 ?self:mask
                                 ?self:shift-count))
(defgeneric MAIN::make:decoder)
(defmethod MAIN::make:decoder
  ((?reference INTEGER)
   (?mask INTEGER)
   (?shift-count INTEGER))
  (decode-bits ?reference
               ?mask
               ?shift-count))
(defmethod MAIN::make:decoder
  ((?reference label)
   (?mask INTEGER)
   (?shift-count INTEGER))
  (make-instance of decoder
                 (reference ?reference)
                 (mask ?mask)
                 (shift-count ?shift-count)))


(defgeneric MAIN::set64)
(defmethod MAIN::set64
  ((?dest INTEGER)
   (?value INTEGER
           label)
   (?left-over INTEGER
               atom))
  (bind ?rtemp0
        (register (register:temp0)))
  (bind ?rtemp1
        (register (register:temp1)))
  (create$ (make:word-container (set16 ?rtemp0
                                       (make:decoder ?value
                                                     (hex->int 0xFFFF000000000000)
                                                     48))
                                (op:shiftleftimmediate ?rtemp0
                                                       ?rtemp0
                                                       48))
           (set48 ?rtemp1
                  (make:decoder ?value
                                (hex->int 0x0000FFFFFFFFFFFF)
                                0))
           (make:word-container (op:add ?dest
                                        ?rtemp1
                                        ?rtemp0)
                                ?left-over)))

(defmethod MAIN::set64
  ((?dest INTEGER)
   (?value INTEGER))
  (set64 ?dest
         ?value
         (nop)))

(defmessage-handler MAIN::instruction encode primary
                    ()
                    (bind ?result
                          (encode (dynamic-get operation-suffix)
                                  0
                                  ?self:operation))
                    (bind ?arg-count
                          (number-of-args ?self:operation))
                    (if (>= ?arg-count 1) then
                      (bind ?result
                            (encode (dynamic-get dest-suffix)
                                    ?result
                                    ?self:dest)))
                    (if (>= ?arg-count 2) then
                      (bind ?result
                            (encode (dynamic-get src0-suffix)
                                    ?result
                                    ?self:src0)))
                    (if (>= ?arg-count 3) then
                      (bind ?result
                            (encode (dynamic-get src1-suffix)
                                    ?result
                                    ?self:src1)))
                    ?result)
(defmethod MAIN::op:systemcall
  ((?destination SYMBOL)
   (?source0 INTEGER)
   (?source1 INTEGER))
  (op:systemcall (system-call->int ?destination)
                 ?source0
                 ?source1))

(deffunction MAIN::sys-terminate
             ()
             (op:systemcall Terminate
                            (register:r0)
                            (register:r0)))
(deffunction MAIN::sys-getc
             (?destination)
             (op:systemcall GetC
                            ?destination
                            (register:r0)))
(deffunction MAIN::sys-putc
             (?source)
             (op:systemcall PutC
                            ?source
                            (register:r0)))
;--------------------------------------------------------------------------------

(defgeneric MAIN::func)
(defmethod MAIN::func
  ((?title SYMBOL)
   (?single-atom INTEGER
                 INSTANCE))
  (create$ (.label ?title)
           (make:word-container ?single-atom
                                (return-from-register (link-register)))))

(defgeneric MAIN::stack-func)
(defmethod MAIN::stack-func
  ((?title SYMBOL)
   (?operation SYMBOL)
   (?sp INTEGER))
  (func (sym-cat stack:
                 ?title)
        (funcall (sym-cat op:
                          ?operation
                          :stack)
                 ?sp)))
(defmethod MAIN::stack-func
  ((?title SYMBOL)
   (?operation SYMBOL))
  (stack-func ?title
              ?operation
              (stack-pointer)))
(defmethod MAIN::stack-func
  ((?title SYMBOL))
  (stack-func ?title
              ?title))


(deffunction MAIN::is-of-type
             (?v ?t)
             (and (instancep ?v)
                  (eq (class ?v)
                      ?t)))

(deffunction MAIN::labelp
             (?l)
             (is-of-type ?l
                         label))
(deffunction MAIN::word-containerp
             (?m)
             (is-of-type ?m
                         word-container))

(deffunction MAIN::transmute-item
             (?input)
             (if (labelp ?input) then
               label
               else
               (if (word-containerp ?input) then
                 word-container
                 else
                 unknown)))
(deffunction MAIN::transmute-list
             (?items)
             (map transmute-item
                  (expand$ ?items)))

(deffunction MAIN::strip-label
             (?input)
             (if (labelp ?input) then (create$) else ?input))

(deffunction MAIN::strip-labels
             (?input)
             (map strip-label
                  (expand$ ?input)))
(deffunction MAIN::compute-address
             (?input)
             (length$ (strip-labels ?input)))

(deffunction MAIN::encode-thing
             (?thing)
             (send ?thing
                   encode))
(deffunction MAIN::encode-list
             (?input)
             (map encode-thing
                  (expand$ ?input)))
(deffunction MAIN::make:program
             (?name $?contents)
             (make-instance ?name of program
                            (contents ?contents)))
(defmessage-handler MAIN::program encode primary
                    ()
                    (encode-list ?self:contents))
; constructs
(deffunction MAIN::!if-true
             (?atom ?condition ?true ?false)
             (make:word-container ?atom
                                  (op:branchifthenelsenormalpredtrue ?condition
                                                                     ?true
                                                                     ?false)))

(deffunction MAIN::!if-false
             (?atom ?condition ?true ?false)
             (make:word-container ?atom
                                  (op:branchifthenelsenormalpredfalse ?condition
                                                                      ?true
                                                                      ?false)))
(deffunction MAIN::!loop
             (?title $?contents)
             (create$ (.label ?title)
                      $?contents
                      (set64 (register (register:temp0))
                             (.label ?title)
                             (op:branchunconditionalregister (register (register:temp0))))))
(defgeneric MAIN::!push-immediate)

(defmethod MAIN::!push-immediate
  ((?value INTEGER
           label)
   (?nop-cell INTEGER
              atom))

  (set64 (stack (stack-pointer))
         ?value
         ?nop-cell))
(defmethod MAIN::!push-immediate
  ((?value INTEGER
           label))
  (!push-immediate ?value
                   (nop)))

(deffunction MAIN::!load-immediate
             (?dest ?value)
             (create$ (!push-immediate ?value)
                      (make:word-container (pop (stack-pointer)
                                                (bind ?t0
                                                      (register (register:temp0))))
                                           (op:load ?dest
                                                    ?t0))))
(deffunction MAIN::!store-immediate
             (?addr-register ?value)
             ; stash the store immediately after the set64 call proper
             (!push-immediate ?value
                              ; load it off of the stack since we've just pushed the value
                              (op:store ?addr-register
                                        (stack (stack-pointer)))))
(defgeneric MAIN::!drop)
(defmethod MAIN::!drop
  "Pop the top element using the given pointer register"
  ((?pointer INTEGER)
   (?slot INTEGER
          atom))
  (make:word-container (op:decrement (register ?pointer)
                                     (register ?pointer))
                       ?slot))

(defmethod MAIN::!drop
  ((?pointer INTEGER))
  (!drop ?pointer
         (nop)))
(defmethod MAIN::!drop
  ()
  (!drop (stack-pointer)))

(defgeneric MAIN::!peek)
(defmethod MAIN::!peek
  ((?destination INTEGER)
   (?pointer INTEGER)
   (?slot INTEGER
          atom))
  (make:word-container (op:load ?destination
                                ?pointer)
                       ?slot))
(defmethod MAIN::!peek
  ((?destination INTEGER)
   (?pointer INTEGER))
  (!peek ?destination
         ?pointer
         (nop)))
;-----------------------------------------------------------------------------
; Rules
;-----------------------------------------------------------------------------
(defrule MAIN::resolve-label-address
         (object (is-a program)
                 (contents $?address ?label $?)
                 (name ?program))
         (object (is-a label)
                 (name ?label)
                 (address FALSE))
         =>
         (assert (output encoded-operation ?program))
         (modify-instance ?label
                          (address (compute-address ?address))))
(defrule MAIN::output-program-contents
         (declare (salience -1))
         ?f <- (output encoded-operation ?program)
         (object (is-a program)
                 (name ?program)
                 (contents $?contents))
         =>
         (retract ?f)
         (output-bytes-to-router (map break-apart-number
                                      (expand$ (encode-list (strip-labels ?contents))))))