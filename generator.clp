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

(deftemplate stage
             (slot current
                   (type SYMBOL)
                   (default ?NONE))
             (multislot rest
                        (type SYMBOL)))
(defrule next-stage
         (declare (salience -10000))
         ?f <- (stage (rest ?next $?rest))
         =>
         (modify ?f
                 (current ?next)
                 (rest $?rest)))
(defrule done-with-stage-fact
         (declare (salience -10000))
         ?f <- (stage (rest))
         =>
         (retract ?f))
(deffacts stages
          ; builder stages
          (stage (current parse-knowledge-graph)
                 (rest build-instruction-description
                       build-instruction-functions)))
;------------------------------------------------------------------------------
(defmessage-handler INTEGER to-string primary
                    ()
                    (str-cat ?self))
(defmessage-handler LEXEME to-string primary
                    ()
                    ?self)
(defclass has-parent
  (is-a USER)
  (slot parent
        (type INSTANCE
              SYMBOL)
        (allowed-symbols FALSE)
        (storage local)
        (visibility public)
        (default-dynamic FALSE)))
(defclass has-title
  (is-a USER)
  (slot title
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default-dynamic FALSE))
  (message-handler to-string primary)
  (message-handler init after))


(defmessage-handler has-title init after
                    ()
                    (if (not (dynamic-get title)) then
                      (dynamic-put title
                                   (instance-name-to-symbol (instance-name ?self)))))

(defmessage-handler has-title to-string primary
                    ()
                    (send (dynamic-get title)
                          to-string))

(defclass component
  (is-a has-title)
  (role abstract)
  (pattern-match non-reactive))

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
(defclass predicate-register
  (is-a register))
(defclass gpr
  (is-a register))
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
        (access initialize-only)
        (default ?NONE))
  (multislot tags
             (type SYMBOL)
             (storage local)
             (visibility public)
             (default ?NONE))
  (message-handler init after)
  (message-handler to-string primary))
(defmessage-handler tagged-component init after
                    ()
                    ; see if the target is a tagged-component
                    (if (eq (class (dynamic-get target))
                            tagged-component) then
                      (dynamic-put tags
                                   (dynamic-get tags)
                                   (send (dynamic-get target)
                                         get-tags))))

(defmessage-handler tagged-component to-string primary
                    ()
                    ; do an indirect dispatch
                    (send (dynamic-get target)
                          to-string))




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
  (is-a has-parent)
  (slot opcode
        (type INSTANCE)
        (allowed-classes opcode)
        (storage local)
        (visibility public)
        (access initialize-only)
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
                   (default ?NONE))
             (multislot class-match 
                        (type SYMBOL)
                        (default ?NONE))
             (multislot aliases
                        (type SYMBOL)))

(deftemplate alias-decl
             (slot real-name
                   (type SYMBOL)
                   (default ?NONE))
             (slot alias
                   (type SYMBOL)
                   (default ?NONE)))
(defclass opcode
  (is-a component)
  (role concrete)
  (pattern-match reactive)
  (slot class
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot group
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot class-match
             (type SYMBOL)
             (storage local)
             (visibility public)
             (default ?NONE))
  (multislot aliases
             (type SYMBOL)
             (storage local)
             (visibility public)
             (default ?NONE))
  (message-handler construct-method primary))

(defclass memory-space-entry
  (is-a has-parent)
  (slot parent
        (source composite)
        (allowed-classes memory-space)
        (default ?NONE))
  (slot address
        (type INTEGER)
        (range 0 ?VARIABLE)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot size
        (type INTEGER)
        (range 0 ?VARIABLE)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot contents
        (type INSTANCE)
        (allowed-classes component)
        (storage local)
        (visibility public)
        (default ?NONE)))

(defclass memory-space
  (is-a has-title)
  (slot address
        (type INTEGER)
        (range 0 65535)
        (storage local)
        (visibility public))
  (multislot contents
             (type INSTANCE)
             (allowed-classes memory-space-entry)))

(defclass iris-program
  (is-a USER)
  (slot data-space
        (type INSTANCE)
        (allowed-classes memory-space)
        (storage local)
        (visibility public)
        (default-dynamic (make-instance of memory-space)))
  (slot code-space
        (type INSTANCE)
        (allowed-classes memory-space)
        (storage local)
        (visibility public)
        (default-dynamic (make-instance of memory-space)))
  (slot stack-space
        (type INSTANCE)
        (allowed-classes memory-space)
        (storage local)
        (visibility public)
        (default-dynamic (make-instance of memory-space)))
  (slot io-space
        (type INSTANCE)
        (allowed-classes memory-space)
        (storage local)
        (visibility public)
        (default-dynamic (make-instance of memory-space))))

(defclass buildable-method-argument
  (is-a has-title
        has-parent)
  (slot title
        (source composite)
        (default ?NONE))
  (slot is-multislot
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE)
        (storage local)
        (visibility public))
  (multislot types
             (storage local)
             (visibility public)
             (type SYMBOL
                   INSTANCE))
  (message-handler to-string primary))
(defmessage-handler buildable-method-argument to-string primary
                    ()
                    (bind ?var
                          (str-cat "?" 
                                   (dynamic-get title)))
                    (if (dynamic-get is-multislot) then
                      (str-cat "$" 
                               ?var)
                      else
                      (if (> (length$ (dynamic-get types)) 0) then
                        (bind ?result
                              (create$))
                        (progn$ (?type (dynamic-get types))
                                (bind ?result
                                      ?result
                                      (send ?type
                                            to-string)
                                      " "))

                        (format nil 
                                "(%s %s)"
                                ?var
                                (str-cat (expand$ ?result)))
                        else
                        ?var)))

(defclass buildable-method
  (is-a has-title)
  (multislot args
             (type INSTANCE)
             (allowed-classes buildable-method-argument)
             (storage local)
             (visibility public))
  (multislot body
             (storage local)
             (visibility public))
  (message-handler body-to-string primary)
  (message-handler args-to-string primary)
  (message-handler to-string primary))
(defmessage-handler buildable-method to-string primary
                    ()
                    (format nil
                            "(defmethod %s
                               %s
                               %s)"
                            (dynamic-get title)
                            (send ?self 
                                  args-to-string)
                            (send ?self
                                  body-to-string)))
(defmessage-handler buildable-method args-to-string primary
                    ()
                    (bind ?content
                          (create$))
                    (progn$ (?arg (dynamic-get args))
                            (bind ?content
                                  ?content
                                  (send ?arg
                                        to-string)
                                  " "))
                    (format nil
                            "(%s)"
                            (if (> (length$ ?content) 1) then
                              (str-cat (expand$ ?content))
                              else
                              "")))
(defmessage-handler buildable-method body-to-string primary
                    ()
                    (bind ?content
                          (create$))
                    (progn$ (?arg (dynamic-get body))
                            (bind ?content
                                  ?content
                                  (send ?arg
                                        to-string)
                                  " "))
                    (if (> (length$ ?content) 1) then
                      (str-cat (expand$ ?content))
                      else
                      ""))
(defclass instruction-sequence
  "Container for a set of instructions"
  (is-a has-parent)
  (multislot children
             (visibility public)
             (storage local))
  (message-handler to-string primary))

(defrule add-alias-decl-to-instruction-description
         (stage (current parse-knowledge-graph))
         ?f <- (alias-decl (real-name ?kind)
                           (alias ?alias))
         ?g <- (instruction-description (kind ?kind)
                                        (aliases $?a))
         =>
         (retract ?f)
         (modify ?g 
                 (aliases $?a 
                          ?alias)))
(defrule make-alias-from-group
         (stage (current parse-knowledge-graph))
         ?f <- (defaliases ?name -> { $?contents&:(not (member$ } ?contents)) } $?rest)
         =>
         (retract ?f)
         (if (<> (length$ ?rest) 0) then
           (assert (defaliases $?rest)))
         (progn$ (?a $?contents)
                 (assert (alias-decl (real-name ?name)
                                     (alias ?a)))))

(defrule make-instruction-description
         (stage (current parse-knowledge-graph))
         (operation-group (kind ?group)
                          (operations $? ?operation $?))
         (instruction-class (kind ?class)
                            (members $? ?operation $?)
                            (args $?match))
         =>
         (assert (instruction-description (kind ?operation)
                                          (class ?class)
                                          (group ?group)
                                          (class-match ?match))))
(defrule error:one-operation-mapped-to-multiple-groups
         (stage (current parse-knowledge-graph))
         (instruction-description (kind ?operation)
                                  (group ?group))
         (instruction-description (kind ?operation)
                                  (group ?group2&~?group))
         =>
         (printout stderr 
                   "ERORR: Found that operation " ?operation " maps to groups " ?group " and " ?group2 crlf)
         (halt))

(defrule error:one-operation-mapped-to-multiple-classes
         (stage (current parse-knowledge-graph))
         (instruction-description (kind ?operation)
                                  (class ?group))
         (instruction-description (kind ?operation)
                                  (class ?group2&~?group))
         =>
         (printout stderr 
                   "ERORR: Found that operation " ?operation " maps to classes " ?group " and " ?group2 crlf)
         (halt))
(defrule error:no-mapped-alias
         (declare (salience -1))
         (stage (current parse-knowledge-graph))
         (alias-decl (real-name ?operation)
                     (alias ?alias))
         =>
         (printout stderr
                   "ERROR: Found that operation " ?operation " was never described as an instruction yet alias " ?alias " exists for it!" crlf)
         (halt))

(defrule make-opcode-from-instruction-description
         (stage (current build-instruction-description))
         ?f <- (instruction-description (kind ?operation)
                                        (class ?class)
                                        (group ?group)
                                        (class-match $?match)
                                        (aliases $?aliases))
         =>
         (retract ?f)
         (make-instance ?operation of opcode
                        (class ?class)
                        (group ?group)
                        (aliases ?aliases)
                        (class-match ?match)))

(deffunction construct-args-string-from-symbols
             ($?symbols)
             (if (= (length$ ?symbols) 0) then
               ""
               else
               (bind ?output
                     (create$))
               (progn$ (?sym $?symbols) 
                       (bind ?output
                             ?output
                             (format nil
                                     "?%s"
                                     ?sym)
                             " "))
               (str-cat (expand$ ?output))))

(defrule build-instruction-deffunction
         (stage (current build-instruction-functions))
         (object (is-a opcode)
                 (title ?title)
                 (class-match $?match)
                 (name ?name))
         =>
         (assert (built deffunction ?title using ?name))
         (bind ?constructed-args
               (create$))
         (progn$ (?sym ?match)
                 (bind ?current-input
                       (str-cat "?" ?sym " "))
                 (bind ?constructed-args
                       ?constructed-args
                       (format nil
                               "(make-instance of tagged-component
                                               (target %s)
                                               (tags %s))%n"
                               ?current-input
                               ?sym)))
         (build (format nil
                        "(deffunction %s
                                      (%s)
                                      (make-instance of instruction
                                                     (opcode [%s])
                                                     (arguments %s)))%n"
                        ?title
                        (construct-args-string-from-symbols ?match)
                        (instance-name-to-symbol ?name)
                        (if (> (length$ ?constructed-args) 0) then
                          (str-cat (expand$ ?constructed-args))
                          else
                          ""))))

(defrule build-deffunction-using-aliases
         (stage (current build-instruction-functions))
         (built deffunction ?title using ?name)
         (object (is-a opcode)
                 (name ?name)
                 (title ?title)
                 (class-match $?match)
                 (aliases $? ?alias $?))
         (not (built deffunction ?alias using ?name))
         =>
         (assert (built deffunction ?alias using ?name))
         (build (format nil
                        "(deffunction %s
                                      (%s)
                                      (%s %s))"
                        ?alias
                        (bind ?args
                              (construct-args-string-from-symbols ?match))
                        ?title
                        ?args)))
(defrule perform-phase2
         ?f <- (phase2)
         =>
         (retract ?f)
         (assert (stage (current translate-fields)
                        (rest))))
(deffunction process-instances
             ()
             (assert (phase2))
             (run))

(deffunction process-file
             (?path)
             (batch* ?path)
             (process-instances))
(defrule replace-symbols-with-components
         (stage (current translate-fields))
         ?tc <- (object (is-a tagged-component)
                        (target ?value))
         (object (is-a component)
                 (title ?title)
                 (name ?gpr))
         (test (eq ?value 
                   ?title))

         =>
         (modify-instance ?tc
                          (target ?gpr)))
