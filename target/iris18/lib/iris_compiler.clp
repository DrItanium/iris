; basic runtime for the iris base environment
; requires implementation details from a separate target file.
; However the iris environment uses 16-bit numbers as words
; There are also other constants for the environment as well.
; 1) Labels start with @label
; 2) The org directive starts with @org
; 3) 32bit values are @dword
; 4) 16bit values are @word
(deffunction to-char-list
             (?uid)
             (bind ?output
                   (create$))
             (while (<> (bind ?char
                              (get-char ?uid))
                        -1) do
                    (bind ?output
                          ?output
                          ?char))
             ?output)
(deffunction string-to-char-list
             "convert the given string to a char list"
             (?str)
             (bind ?path
                   (str-cat "/tmp/" 
                            (gensym*)))
             (bind ?uid
                   (gensym*))
             (bind ?outcome
                   (create$))
             (if (open ?path 
                       ?uid
                       "w") then
              (printout ?uid ?str)
               (close ?uid)
               (open ?path
                     ?uid
                     "r")
               (bind ?outcome
                     (to-char-list ?uid))
               (close ?uid)
               (remove ?path))
             ?outcome)

(defgeneric compile
            "Compile the primary environment")
(defgeneric code
            "Install code into the environment")

(defgeneric output
            "Output the input body to the target IO router")
(defgeneric deflabel
            "Declare a label which points to a given address")
(defgeneric memory-location
            "Wrapper over the org directive")
(defgeneric dword
            "Define a 32bit constant")
(defgeneric word
            "Define a 16bit constant")
(defgeneric comment
            "declare a comment")
(defgeneric note
            "Declare a note comment")
(defgeneric todo
            "Declare a todo comment")
(defgeneric body
            "Declare a body of code")

(defgeneric scope
            "Declares a label targeted body of code")
(defgeneric defunc
            "defines a 'function' that is registered into the current iris environment")
(defgeneric jump-table
            "Define a series of addresses prefixed with a base label address")
(defgeneric at-memory-location
            "At the given memory address, output the following things")
(defgeneric describe-arg
            "Describe a given argument")
(defgeneric ret
            "Instruction(s) for returning from a procedure")
(defgeneric zero
            "Set the given register to zero")
(defgeneric assign32
            "Assign the given 32bit constant to the target register")
(defgeneric assign16
            "Assign the given 16bit constant to the target register")
(defgeneric assign24)
(defgeneric assign8)
(defgeneric assign
            "Assing the give constant to the target register")
(defgeneric use-register
            "Save the given register to the stack, perform the provided actions, and then restore the original value")
(defgeneric save-register
            "Save the given register to the stack")
(defgeneric restore-register
            "Restore the given register from the stack")
(defgeneric set-address)
(defgeneric defjump-table)

(defgeneric make-string)

(defmethod make-string
 ((?title SYMBOL)
  (?string LEXEME))
 (scope ?title
        (map word
            (expand$ (string-to-char-list ?string)))
        (word 0x00)))
             

(defmethod dword
  ((?value LEXEME
           INTEGER))
  (format nil
          "    @dword %s"
          (str-cat ?value)))

(defmethod word
  ((?value LEXEME
           INTEGER))
  (format nil
          "    @word %s"
          (str-cat ?value)))



(defmethod comment
  ((?op LEXEME)
   (?comment STRING))
  (format nil
          "%s ; %s"
          ?op
          ?comment))
(defmethod comment
  ((?comment STRING))
  (format nil
          "    ; %s"
          ?comment))

(defmethod todo
  ((?message STRING))
  (comment (format nil
                   "TODO: %s"
                   ?message)))
(deffunction generate-note-text
             (?msg)
             (format nil
                     "NOTE: %s"
                     ?msg))
(defmethod note
  ((?op LEXEME)
   (?message STRING))
  (comment ?op
           (generate-note-text ?message)))
(defmethod note
  ((?message STRING))
  (comment (generate-note-text ?message)))

(defmethod describe-arg
  ((?argument LEXEME)
   (?description STRING))
  (comment (format nil
                   "%s - %s"
                   ?argument
                   ?description)))


(defmethod output
  ((?router SYMBOL)
   (?lines MULTIFIELD))
  (progn$ (?line ?lines)
          (printout ?router
                    ?line crlf)))
(defmethod output
  ((?router SYMBOL)
   $?lines)
  (output ?router
          ?lines))

(defmethod use-register 
  ((?registers MULTIFIELD
               (<> (length$ ?current-argument)
                   0))
   (?body MULTIFIELD))
  (use-register (nth$ 1 
                      ?registers)
                (use-register (rest$ ?registers)
                              ?body)))
(defmethod use-register
 ((?registers MULTIFIELD
   (= (length$ ?current-argument)
    0))
  (?body MULTIFIELD))
 ?body)

(defmethod use-register
  ((?register SYMBOL)
   (?body MULTIFIELD))
  (create$ (save-register ?register)
           ?body
           (restore-register ?register)))
(defmethod use-register
  ((?register SYMBOL
              MULTIFIELD)
   $?body)
  (use-register ?register
                ?body))


(defmethod label-text
  ((?title LEXEME)
   (?router SYMBOL))
  (format ?router
          "@label %s%n"
          ?title))
(defmethod label-text
  ((?title LEXEME))
  (label-text ?title
              nil))
(defclass label
  (is-a USER)
  (slot title
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler compile primary))

(defmessage-handler label compile primary
                    (?router)
                    (label-text ?self:title
                                ?router))
(defclass container
  (is-a USER)
  (multislot contents
             (visibility public)
             (storage local))
  (message-handler compile primary))

(defmessage-handler container compile primary
                    (?router)
                    (progn$ (?a ?self:contents)
                            (if (instancep ?a) then
                              (send ?a
                                    compile
                                    ?router)
                              else
                              (output ?router
                                      ?a))))

(defclass environment
  (is-a USER)
  (multislot children
             (visibility public)
             (storage local))
  (message-handler add-entries primary)
  (message-handler compile primary))

(defmessage-handler environment add-entries primary
                    ($?entry)
                    (bind ?self:children
                          ?self:children
                          ?entry))
(defmessage-handler environment compile primary
                    (?router)
                    (progn$ (?child ?self:children)
                            (if (instance-namep ?child) then
                              (send ?child
                                    compile
                                    ?router)
                              else
                              (output ?router
                                      ?child))))




(definstances iris-env
              (main-env of environment))

(defglobal MAIN
           ?*primary-env* = [main-env])

(defmethod defunc
  ((?name SYMBOL)
   (?entries MULTIFIELD))
  (scope ?name
         ?entries
         (ret)))
(defmethod defunc
  ((?name SYMBOL)
   $?entries)
  (defunc ?name
          ?entries))

(defmethod deflabel
  ((?title LEXEME))
  (make-instance of label
                 (title ?title)))

(defmethod scope
  ((?name LEXEME)
   (?body MULTIFIELD))
  (body (deflabel ?name)
        ?body))
(defmethod body
  ((?body MULTIFIELD))
  (make-instance of container
                 (contents ?body)))
(defmethod body
  ($?body)
  (body ?body))
(defmethod scope
  ((?name LEXEME)
   $?body)
  (scope ?name
         ?body))

(defmethod jump-table
  ((?title LEXEME)
   (?locations MULTIFIELD))
  (scope ?title
         (map dword
              (expand$ ?locations))))

(defmethod jump-table
  ((?title LEXEME)
   $?locations)
  (jump-table ?title
              ?locations))

(defmethod memory-location
  ((?value LEXEME
           NUMBER))
  (format nil
          "@org %s"
          (str-cat ?value)))

(defmethod at-memory-location
  ((?value LEXEME
           NUMBER)
   (?body MULTIFIELD))
  (body (memory-location ?value)
        ?body))
(defmethod at-memory-location
  ((?value LEXEME
           NUMBER)
   $?body)
  (at-memory-location ?value
                      ?body))

(defmethod compile
  ((?router SYMBOL))
  (send ?*primary-env*
        compile
        ?router))
(defmethod compile
  ()
  (compile t))

(defmethod compile-to-file
  ((?path LEXEME)
   (?mode LEXEME))
  (if (open ?path
            file
            ?mode) then
    (compile file)
    (close file)
    else
    (printout werror
              "ERROR: Couldn't open " ?path " with permissions " ?mode crlf)
    FALSE))
(defmethod compile-to-file
  ((?path LEXEME))
  (compile-to-file ?path
                   "w"))

(defmethod code
  ((?contents MULTIFIELD))
  (send ?*primary-env*
        add-entries
        ?contents))

(defmethod code
  ($?contents)
  (code ?contents))
