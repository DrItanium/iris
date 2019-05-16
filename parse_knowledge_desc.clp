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

(defmodule parse-knowledge-graph
           "Phase to parse the knowledge description and make corresponding objects out of them"
           (import MAIN ?ALL))

(deftemplate parse-knowledge-graph::alias-decl
             (slot real-name
                   (type SYMBOL)
                   (default ?NONE))
             (slot alias
                   (type SYMBOL)
                   (default ?NONE)))

(defrule parse-knowledge-graph::add-alias-decl-to-instruction-description
         ?f <- (alias-decl (real-name ?kind)
                           (alias ?alias))
         ?g <- (instruction-description (kind ?kind)
                                        (aliases $?a))
         =>
         (retract ?f)
         (modify ?g 
                 (aliases $?a 
                          ?alias)))
(defrule parse-knowledge-graph::error:one-operation-mapped-to-multiple-groups
         (instruction-description (kind ?operation)
                                  (group ?group))
         (instruction-description (kind ?operation)
                                  (group ?group2&~?group))
         =>
         (printout stderr 
                   "ERORR: Found that operation " ?operation " maps to groups " ?group " and " ?group2 crlf)
         (halt))

(defrule parse-knowledge-graph::error:one-operation-mapped-to-multiple-classes
         (instruction-description (kind ?operation)
                                  (class ?group))
         (instruction-description (kind ?operation)
                                  (class ?group2&~?group))
         =>
         (printout stderr 
                   "ERORR: Found that operation " ?operation " maps to classes " ?group " and " ?group2 crlf)
         (halt))
(defrule parse-knowledge-graph::error:no-mapped-alias
         (declare (salience -1))
         (alias-decl (real-name ?operation)
                     (alias ?alias))
         =>
         (printout stderr
                   "ERROR: Found that operation " ?operation " was never described as an instruction yet alias " ?alias " exists for it!" crlf)
         (halt))

(defrule parse-knowledge-graph::make-instruction-description
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

(defrule parse-knowledge-graph::unpack-defaliases 
         ?f <- (defaliases (contents ?name -> { $?contents&:(not (member$ } ?contents)) } $?rest))
         =>
         (if (empty$ ?rest) then
           (retract ?f)
           else
           (modify ?f 
                   (contents $?rest)))
         (progn$ (?a $?contents)
                 (assert (alias-decl (real-name ?name)
                                     (alias ?a)))))

(defrule parse-knowledge-graph::process-register-translation
         ?f <- (deftranslation (contents $?before ?a -> ?b))
         =>
         (if (empty$ $?before) then
           (retract ?f)
           else
           (modify ?f 
                   (contents $?before ?a)))
         (make-instance ?a of tagged-component
                        (target ?b)
                        (tags)))
