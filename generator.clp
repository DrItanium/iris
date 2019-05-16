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

(deffacts code-generator-stages
          ; builder stages
          (stage (current parse-knowledge-graph)
                 (rest build-instruction-description
                       build-instruction-functions)))
;------------------------------------------------------------------------------



(deftemplate alias-decl
             (slot real-name
                   (type SYMBOL)
                   (default ?NONE))
             (slot alias
                   (type SYMBOL)
                   (default ?NONE)))

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
