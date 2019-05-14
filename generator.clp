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

(defclass register
  (is-a USER)
  (slot title
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default-dynamic FALSE))
  (message-handler init after))

(defmessage-handler register init after
                    ()
                    (if (not (dynamic-get title)) then
                      (dynamic-put title
                                   (instance-name-to-symbol (instance-name ?self)))))

(defclass aliased-constant 
  (is-a USER)
  (role abstract)
  (pattern-match non-reactive)
  (slot title
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default-dynamic FALSE))
  (message-handler init after)
  (message-handler to-string primary))

(defmessage-handler aliased-constant to-string primary
                    ()
                    (dynamic-get title))

(defmessage-handler aliased-constant init after
                    ()
                    (if (not (dynamic-get title)) then
                      (dynamic-put title
                                   (instance-name-to-symbol (instance-name ?self)))))

(defclass label
  (is-a aliased-constant)
  (role concrete)
  (pattern-match reactive))

  

(defclass instruction 
  (is-a USER)
  (slot op 
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot destination
        (type INSTANCE 
              SYMBOL)
        (allowed-classes register)
        (storage local)
        (visibility public)
        (allowed-symbols FALSE)
        (default-dynamic FALSE))
  (slot source1
        (type INSTANCE 
              SYMBOL)
        (allowed-classes register)
        (storage local)
        (visibility public)
        (allowed-symbols FALSE)
        (default-dynamic FALSE))
  (slot source2
        (type INSTANCE
              SYMBOL
              INTEGER)
        (allowed-classes register
                         aliased-constant)
        (storage local)
        (visibility public)
        (default-dynamic FALSE))
  (message-handler to-string primary))

(defmessage-handler
