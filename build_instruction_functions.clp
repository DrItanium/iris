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
(defmodule build-instruction-functions
           (import MAIN ?ALL))
(deffunction build-instruction-functions::construct-args-string-from-symbols
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

(defrule build-instruction-functions::build-root-deffunction 
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
         (build (format stdout
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

(defrule build-instruction-functions::build-alias-deffunction
         (built deffunction ?title using ?name)
         (object (is-a opcode)
                 (name ?name)
                 (title ?title)
                 (class-match $?match)
                 (aliases $? ?alias $?))
         (not (built deffunction ?alias using ?name))
         =>
         (assert (built deffunction ?alias using ?name))
         (build (format stdout
                        "(deffunction %s
                                      (%s)
                                      (%s %s))%n"
                        ?alias
                        (bind ?args
                              (construct-args-string-from-symbols ?match))
                        ?title
                        ?args)))

