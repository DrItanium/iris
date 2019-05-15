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
             (default ?NONE)))


(defrule add-alias-decl-to-instruction-description
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
         ?f <- (defaliases ?name -> { $?contents&:(not (member$ } ?contents)) } $?rest)
         =>
         (retract ?f)
         (if (<> (length$ ?rest) 0) then
           (assert (defaliases $?rest)))
         (progn$ (?a $?contents)
                 (assert (alias-decl (real-name ?name)
                                     (alias ?a)))))

(defrule make-instruction-description
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
         (instruction-description (kind ?operation)
                                  (group ?group))
         (instruction-description (kind ?operation)
                                  (group ?group2&~?group))
         =>
         (printout stderr 
                   "ERORR: Found that operation " ?operation " maps to groups " ?group " and " ?group2 crlf)
         (halt))

(defrule error:one-operation-mapped-to-multiple-classes
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
         (alias-decl (real-name ?operation)
                     (alias ?alias))
         =>
         (printout stderr
                   "ERROR: Found that operation " ?operation " was never described as an instruction yet alias " ?alias " exists for it!" crlf)
         (halt))

(defrule make-opcode-from-instruction-description
         (declare (salience -2))
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
                        (

