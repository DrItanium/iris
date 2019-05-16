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

