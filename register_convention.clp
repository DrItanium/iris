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

(definstances register-convention
              (zero of tagged-component (target r0) (tags))
              (t0 of tagged-component (target r1) (tags))
              (t1 of tagged-component (target r2) (tags))
              (t2 of tagged-component (target r3) (tags))
              (t3 of tagged-component (target r4) (tags))
              (t4 of tagged-component (target r5) (tags))
              (t5 of tagged-component (target r6) (tags))
              (t6 of tagged-component (target r7) (tags))
              (t7 of tagged-component (target r8) (tags))
              (sp of tagged-component (target r9) (tags))
              (stack-pointer of tagged-component (target sp) (tags))
              (all-preds of tagged-component (target r10) (tags))
              (pred-bits of tagged-component (target all-preds) (tags))
              (count of tagged-component (target r11) (tags))
              (ctr of tagged-component (target count) (tags))
              ; predicates
              (pfalse of tagged-component (target p1) (tags))
              (ptrue of tagged-component (target p0) (tags))
              )
