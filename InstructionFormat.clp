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

; this is an expert system meant to generate extended metadata about each 
; instruction

(defclass instruction-property 
  (is-a USER)
  (slot title 
        (type SYMBOL)
        (default-dynamic ?NONE))
  (slot parent
        (type INSTANCE)
        (default ?NONE))
  (slot value
        (type SYMBOL)
        (default-dynamic FALSE)))
(deftemplate instruction-facet
             (slot title
                   (type SYMBOL)
                   (default ?NONE)))

(deftemplate facet->c++
             (slot title
                   (type SYMBOL)
                   (default ?NONE))
             (slot convert-to
                   (type SYMBOL)
                   (default ?NONE)))
(deffacts facets
              (definstruction-facet (title manipulates-stack))
              (definstruction-facet (title manipulates-code))
              (definstruction-facet (title manipulates-io))
              (definstruction-facet (title manipulates-ip))
              (definstruction-facet (title manipulates-data))
              (definstruction-facet (title disallows-divide-by-zero))
              (definstruction-facet (title is-ordinal-operation))
              (definstruction-facet (title is-integer-operation))
              (definstruction-facet (title is-memory-operation))
              (definstruction-facet (title is-compare-operation))
              (definstruction-facet (title is-arithmetic-operation))
              (definstruction-facet (title is-branch-operation))
              (definstruction-facet (title uses-relative-offset))
              (definstruction-facet (title uses-absolute-position))
              (definstruction-facet (title is-divide-operation))
              (definstruction-facet (title is-remainder-operation))
              (definstruction-facet (title bitwise-not-after-core-operation))
              (definstruction-facet (title is-bitwise-and-operation))
              (definstruction-facet (title is-bitwise-or-operation))
              (definstruction-facet (title is-bitwise-xor-operation))
(defclass instruction-declaration
  (is-a USER))
(deftemplate instruction-fact
             (slot target
                   (type INSTANCE)
                   (default ?NONE))
             (slot property
                   (type SYMBOL)
                   (default ?NONE))
             (slot value
                   (type SYMBOL)
                   (default TRUE)))
(deftemplate instruction-implication
             (slot property-to-check
                   (type SYMBOL)
                   (default ?NONE))
             (slot value-to-check
                   (default TRUE))
             (slot property-to-assign
                   (type SYMBOL)
                   (default ?NONE))
             (slot value-to-assign
                   (default TRUE)))
(defrule generate-instruction-property
         (definstruction-facet (title ?title))
         (object (is-a instruction-declaration)
                 (name ?id))
         (not (object (is-a instruction-property)
                      (title ?title)
                      (parent ?id)))
         =>
         (make-instance of instruction-property
                        (title ?title)
                        (parent ?id)))
(defrule mark-instruction-declaration-from-fact
         ?f <- (instruction-fact (target ?target)
                                 (property ?prop)
                                 (value ?value))
         ?z <- (object (is-a instruction-property)
                       (title ?prop)
                       (parent ?target))
         =>
         (retract ?f)
         (send ?z 
               put-value 
               ?value))
(defrule mark-instruction-implication
         (instruction-implication (property-to-check ?prop)
                                  (value-to-check ?check)
                                  (property-to-assign ?then)
                                  (value-to-assign ?assign))
         (object (is-a instruction-property)
                 (title ?prop)
                 (parent ?t)
                 (value ?check))
         =>
         (assert (instruction-fact (target ?t)
                                   (property ?then)
                                   (value ?assign))))


