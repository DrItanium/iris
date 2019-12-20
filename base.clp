; @file
; Generate the clp library file from opcode data
; @copyright 
; iris
; Copyright (c) 2013-2020, Joshua Scoggins and Contributors
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
; 
; base code routines
; 

(defmethod MAIN::bitwise-or
  ((?first INTEGER)
   (?second INTEGER)
   $?rest)
  (bitwise-or 
    (bitwise-or ?first
                ?second)
    (expand$ ?rest)))

(defmethod MAIN::bitwise-or 
  ((?first INTEGER) 
   (?second INTEGER) 
   (?third INTEGER) 
   (?fourth INTEGER) ) 
  (bitwise-or (bitwise-or ?first 
                          ?second) 
              (bitwise-or ?third 
                          ?fourth))) 
(defmethod MAIN::bitwise-or 
  ((?first INTEGER) 
   (?second INTEGER) 
   (?third INTEGER)) 
  (bitwise-or (bitwise-or ?first 
                          ?second) 
              ?third)) 

(defmethod MAIN::bitwise-and
  ((?first INTEGER)
   (?second INTEGER)
   $?rest)
  (bitwise-and (bitwise-and ?first
                            ?second)
               (expand$ ?rest)))
(defmethod MAIN::bitwise-and 
  ((?first INTEGER) 
   (?second INTEGER) 
   (?third INTEGER) 
   (?fourth INTEGER) ) 
  (bitwise-and (bitwise-and ?first 
                            ?second) 
               (bitwise-and ?third 
                            ?fourth))) 
(defmethod MAIN::bitwise-and 
  ((?first INTEGER) 
   (?second INTEGER) 
   (?third INTEGER)) 
  (bitwise-and (bitwise-and ?first 
                            ?second)
               ?third))
(defmethod MAIN::arg0-position
  ((?value INTEGER))
  (left-shift (bitwise-and ?value
                           255)
              16))
(defmethod MAIN::arg1-position
  ((?value INTEGER))
  (left-shift (bitwise-and ?value
                           255)
              8))
(defmethod MAIN::arg0-position
  ((?value INTEGER))
  (bitwise-and ?value
               255))

(defmethod MAIN::make-32bit
  ((?value INTEGER))
  (bitwise-and ?value 
               4294967295)) 

(defmethod MAIN::encode-instruction
 ((?opcode INTEGER)
  (?arg0 INTEGER)
  (?arg1 INTEGER)
  (?arg2 INTEGER))
 (make-32bit (bitwise-or ?opcode
                         (arg0-position ?arg0)
                         (arg1-position ?arg1)
                         (arg2-position ?arg2))))
(defmethod MAIN::encode-instruction
 ((?opcode INTEGER)
  (?arg0 INTEGER)
  (?imm16 INTEGER))
 (make-32bit (bitwise-or ?opcode
                         (arg0-position ?arg0)
                         (bitwise-and 65535
                                      ?imm16))))
