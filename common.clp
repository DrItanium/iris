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

(defclass instruction-sequence
  "Container for a set of instructions"
  (is-a has-parent)
  (multislot children
             (visibility public)
             (storage local))
  (message-handler to-string primary))
