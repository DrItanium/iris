: compile ( -- )
	?comp  \ error if not compiling
	r>     \ top of return stack is pointing to the next word following COMPILE
	dup 2 + >r \ increment this pointer by 2 to point to the second word
			   \ following COMPILE, which will be the next word to be executed. 
			   \ The word immediately following COMPILE should be compiled, not
			   \ executed.
	@ ,        \ do the compilation at runtime
	;

: (abort) ( -- ) ABORT ;
: error ( n -- in blk )
  warning @ 0<
  if (abort) endif 
  here count type 
  ." ?"
  message
  SP!
  in @
  blk @
  quit ;
: ?ERROR ( f n -- ) swap if error else drop endif ;

: word ( c ------ )
\ work horse of the interpreter
blk @ if blk @ block else tib @ endif 
in @ + swap enclose here 0x22 blanks in +! over - >r R here C! + 
here !+
R> 
;  \ move the string from input buffer to top of dictionary


: create ( -- ) 
  bl word
  here
  dup c@
  width @
  min
  1+ allot
  dup 0x0a0 toggle
  here 1- 0x80 toggle
  latest,
  current @ !
  here 2+ , ;


: expect ( addr n -- )
over + over 
do 
key 
dup 0xE +origin = 
if 
drop 0x8 over I= dup r> 0x2 - + r> - 
else 
dup 0x0D = 
if 
  leave drop bl 0 
else 
  dup 
endif 
I C! 0 I 1+ ! 
endif 
emit 
loop 
drop ; 

: query TIB 0x50 expect 0 in ! ;


: type  ( addr n -- )
  -dup 
  if 
    over + 
    swap 
    do 
        I C@
        emit
    loop
  else 
    drop
  endif ;

: count ( addr n -- ) dup 1+ swap ;

: -trailing ( addr n1 -- addr n2 )
  dup 0
  do
  over over
  + 1 - 
  c@ bl - 
  if leave else 1- endif loop ;
      

: (.") \ runtime procedure compiled by ." to type inline text
  R \ copy IP from the return stack, which points to the beginning of the inline text string
  count \ get the length byte of the string, preparing for TYPE
  dup 1+ \ length + 1
  r> + >r \ increment ip on the return stack by length +1, thus skipping the 
          \ text string and pointing to the next word after ", which is the 
          \ next word to be executed
  type    \ now type out the text string 
  ;

: ." 0x22 state @ if compile 
(.")
  word here C@ 1+ allot else word here endif ; immediate
: id. ( nfa -- ) 
  \ print an entry's name from its name field address on stack.
  pad
  0x20 \ ascii blank
  0x5F fill \ fill pad with 95 blanks
  dup pfa lfa \ find the link field address
  over - 
  pad swap cmove \ move the entire name with the length to PAD
  pad count \ prepare string for output
  0x1f and \ no more than 31 characters
  type \ type out the name
  space \ append a space
  ;
: (line) ( line scr -- addr count )
  >r c/l b/buf */mod \ calculate the character offset and the screen offset
                     \ members from the line number, characters/line, and bytes/buffer
  r> b/scr * +
  block
  + 
  c/l
  ;
: .line ( line scr -- )
  \ print on the terminal a line of text from disc by its line number and 
  \ screen number scr given on stack
  \ trailing blanks are also supported
  (line) \ runtime procedure to convert the line number
  -trailing type ;

: list ( n -- )
  \ display the ascii text of screen n on the terminal
  decimal cr
  dup scr !
  ." SCR #" . \ print th escreen number first
  0x10 0 do 
    cr I 3 .R space
    I SCR @ .line
    loop cr ;

: (number) ( d1 addr1 -- d2 addr2 )
  \ runtime routine of number conversion. Convert an ascii text beginning at addr1 + 1
  \ according to base. The result is accumulated with d1 to become d2. Addr2 is
  \ the address of the first unconvertable digit
  begin
    1+ dup >r 
    c@ 
    base @
    digit
    while
    swap 
    base @ U* 
    drop 
    rot 
    base @ U*
    D+
    dpl @ 1+
    if 
      1 dpl +!
      endif 
      r>
      repeat
      r>
      ;

: number ( addr -- d )
  0 0 rot \ push the two zero's on stack as the initial value of d
  dup 1+ c@ \ get the first digit
  0x2D = \ is it a -sign ?
  dup >R \ save the flag on return stack
  + \ if the first digit is -, the flag is 1, and addr + 1 points to the second digit
    \ addr + 0 remains the same, pointing to the first digit
  -1 \ The initial value of dpl
  begin 
    dpl ! \ store the decimal point counter
    (number) \ convert one digit after another until an invalid char occurs.
             \ result is accumulated into d.
    dup c@   \ fetch the invalid digit
    bl - \ is it a blank?
  while  \ not a blank, see if it is a decimal point
    dup c@  \ get the digit sign
    0x2e - \ is it a decimal point?
    0 ?error \ not a decimal point, it is an illegal character for a number.
             \ issue an error message and quit
    0 \ a decimal point was found. set DPL to 0 the next time
    repeat \ exit here if a blank was detected. Otherwise repeat the conversion
           \ process
    drop \ discard addr on stack
    r>   \ pop the flag of - sign back
    if dminus \ negate d if the first digit is a - sign
    endif 
;

: <# ( -- )
  pad \ pad is the scratch pad address for text output, 68 bytes above the dictionary head HERE
  HLD ! \ hld is a user variable holding the address of last character in the output text string
  ;
: erase ( addr u -- ) 0 fill ;
: blanks ( addr u -- ) bl fill ;
\ when the machine starts from cold
\ COLD calls ABORT
\ ABORT calls QUIT
: cold 
empty-buffers
0 density !
first use !
dr0 0 eprint !
orig 0x12 + 
up @ 6 + 
0x10 cmove 
orig 0x0c + @ forth 
6 + ! abort ;

: -find ( -- [ pfa b tf ] or [ f ] )
  \ accept the next word delimited by blanks in the input stream to here, and
  \ search the context and then the current vocabularies for a matching name.
  \ If found, the entry's parameter field address, a length byte, and a flag
  \ are left on stack. Otherwise only boolean false flag is left
  bl word \ move text string delimited by blanks from input string to the top of dictionary here
  here \ the address of text to be matched
  context @ @ \ fetch the name field address of the last word defined in the context
              \ vocabulary and begin the dictionary search
  (find) \ a primitive. search the dictionary starting at the address on stack for 
         \ a name matching the text at the address second on stack. Return
         \ the parameter field address of the matching name, its length byte,
         \ and a boolean true flag on stack for a match. If no match is possible
         \ then only a boolean false flag is left on stack
  dup 0= \ look at flag on stack
  if  \ no match in context vocabulary
    drop \ discard the false flag
    here \ get the address of text again
    latest \ the name field address of the last word defined in the current
           \ vocabulary
    (find) \ search again through the current vocabulary
  endif
;

: vocabulary ( -- )
  <builds 0xa081 , \ a dummy header at vocabulary intersection
  current @ \ fetch the parameter field address pointing to the last word defined
            \ in the current vocabulary
  cfa ,     \ store its code field address in the second cell in the parameter field
  here \ address of vocabulary link
  voc-link @ , \ fetch the user variable voc-link and insert it in the dictionary
  voc-link ! \ update boc-link with the link in this vocabulary
  does> \ this is the end in defining cccc vocabulary
  2 + context ! \  when cccc is invoked, the second cell in its parameter field will
  \ be stored into the variable context. the next dictionary search will begin with the cccc vocabulary
  ;

: definitions ( -- )
  \ used in the form: cccc definitions 
  \ make cccc vocabulary the current vocabulary.
  \ new definitions will be added to the cccc vocabulary
  context @ current ! ;
\ dictionary entry stuff
: traverse ( a1 n -- a2 )
  \ move across the name field of a variable length name field.
  \ a1 is the address of either the length byte or the last character
  \ if n == 1, the motion is towards high memory; 
  \ if n == -1, the motion is towards low memory
  \ a2 is the address of the other end of the name field
  swap \ get a1 to the top of the stack
  begin
    over + \ copy n and add to addr, pointing to the next character
    0x7F \ test number for the eighth bit of a character
    over c@ \ fetch the character
    < \ if it is greater than 127, the end is reached
  until \ loop back if not the end
  swap drop \ discard n
  ;
: nfa ( pfa -- nfa )
    \ convert the parameter field address to name field address
    5 - 
    -1 traverse ;
: vlist ( -- )
  \ list the names of all entries in the context vocabulary. The 'break' key on
  \ terminal will terminate the listing
  0x80 out ! \ init the output char count to print 128 chars
  context @ @ \ fetch the name field address of the last word in the context vocab
  begin
    out @ \ get the out char count
    c/l >  \ if it is larger than the chars per line of the output device
    if 
        cr 0 out ! \ output a cr/lf and reset out
    endif
    dup id. \ type out the name and 
    space space \ add two spaces
    pfa lfa @ \ get the link pointing to previous word
    dup 0=    \ see if it is zero, the end of the link,
    ?terminal or \ or if the break key on terminal was pressed.
  until \ exit at the end of link or after break key was pressed; 
        \ otherwise continue the listing of names
  drop \ discard the parameter field address on stack and return
  ;
: (;code) ( -- )
  r> \ pop the address of the next instruction off the return stack,
     \ which is the starting address of the runtime code routine
  latest \ get the name field address of the word under construction
  pfa cfa ! \ find the code field address and store in it the address of the
            \ code routine to be exec at runtime
  ;
: [compile] ( -- )
  \ force the compilation of the following immediate word
  -find \ accept
  0= 0 ?error 
  drop
  cfa , 
  ; immediate

: ' ( -- pfa )
  \ locate a word in the dictionary. Form is ' cccc
  -find \ get cccc and search the dictionary, first the context and then the 
        \ current vocabularies
  0= 0 ?error \ not found, issue error message
  drop 
  [compile] \ compile the next immediate word literal to compile the parameter field address at run-time
  literal ; immediate
: code ( -- ) create [compile] assembler ;
: forget ( -- )
  \ special form, forget cccc
  \ delete definitions defined after and including the word cccc. The current and
  \ context vocabulary must be the same

  current @ context @ - 0x12 ?error \ compare current with context, if not the same, issue an error
  [compile] ' \ locate cccc in the dictionary
  dup \ copy the parameter field address
  fence @ \ compare with the context in the user variable fence
  < 0xF ?error \ if cccc is less than fence, do not forget. Fence guards the trunk forth vocabulary 
               \ from being accidentally forgotten.
  dup nfa \ fetch teh name field address of cccc and,
  dp !    \ store in the dictionary pointer DP. Now the top of the dictionary is redefined to be the first byte of cccc
          \ in effect deleting all definitions above cccc.
  lfa @ \ get the link field address of cccc pointing to the word just below it.
  current @ ! \ store it in the current vocabulary, adjusting the current vocabulary to the fact that
              \ all definitions above (including) cccc no longer exist
  ;
: ;code ( -- ) 
  \ stop compilation and terminate a new defining word ccc by compiling the run-time routine
  \ (;code). Assemble the assembly mnemonics following. Used in the form:
  \ 
  \ : cccc -- ;CODE assembly mnemonics
  ?CSP \ check the stack pointer. Issue an error message if not equal to what was saved in csp by :
  compile \ when ;code is executed at runtime, the address of the next word will be compiled into dictionary
  (;code) \ runtime procedure which completes the definition of a new defining word
  [compile] \ compile the next immediate word instead of executing it
  [ \ return to executing state to assemble the following assembly mnemonics
    smudge \ toggle the smudge bit in the length byte, and complete the new definition
    ; immediate
                

: constant ( n -- )
  create 
  smudge
  ,
  ;code 
  docon:
    xw xsp push,
    next,

: <builds ( -- )
  \ create a new header for a definition with the name taken from the next text
  \ in the input stream
  0 constant \ create a new entry in the dictionary with a zero in its parameter
             \ field. It will be replaced by the address of the code field routine
             \ after DOES> when DOES> is executed
             ;
: <does ( -- )
  \ define runtime routine action within a high-level defining word.
  \ alters the code field and the first cell in the parameter field in the 
  \ defining word, so that when a new word create by this defining word is called,
  \ the sequence of words compiled after DOES> will be executed
  r> \ get the addr of the first word after does>
  latest \ get the name field address of the new definition under construction
  pfa ! \ store the address of the runtime routine as the first param
  ;code \ when does> is executed, it will first do the following code routine
        \ because ;code puts the next address into the code field of code>
  dodoe: 
  xip xrp push,
  xw xip move,
  xw 1+,
  xw xsp push,
  next,


: variable ( n -- )
  constant
  ;code
  dovar:
    xw xsp push,
    next,

: user ( n -- )
  constant
  ;code
  douse:
    xw xsp push,
    xsp xup xup add,
    next,

: if ( f -- ) \ at runtime
     ( -- addr n ) \ at compile time
	 compile 0branch
	 here \ push dictionary address on stack to be used by else or endif to
	      \ calculate branching effect.
	 0 , \ compile a dummy zero here, later it is to be replaced by an
	     \ offset value used by 0BRANCH to compute the next word address
	 2   \ error checking number.
	 ; immediate
: endif ( addr n -- ) \ compile time
  ?comp          \ issue an error message if we are not compiling
  2 ?pairs endif \ must be paired with IF or else.
  				 \ if n is not 2, the structure was disturbed or improperly
				 \ nested.
  here   \ push the current dictionary address to stack
  over - \ here-addr is the forward branching offset
  swap ! \ store the offset in addr, thus completing the if-endif or
         \ if-else-endif construct
  ; immediate

: else ( a1 n1 -- a2 n2 ) 
       2 ?pairs \ error checking for proper nesting
	   compile branch \ compile branch at runtime when else is executed.
	   here           \ push here on stack as a2
	   0 ,            \ dummy zero reserving a cell for branching to endif
	   swap           \ move a1 to top of stack
	   [compile] endif \ call endif to work on the offset for forward branching
	   2 
	   ; immediate

: begin ( -- addr n )
	?comp \ error if not compiling
	here  \ push dictionary pointer onto stack to be used to compute backwards
	      \ branching offset
	1     \ error checking number
	; immediate
: until ( addr n -- )
\ compile 0branch and an in-line offset from here to addr. Test the error
\ checking code n.
\ if not equal to 1, there is an error in the nesting structure
1 ?pairs \ if n is not 1, issue an error message
compile 0branch \ compile 0branch at runtime
back 			\ compute backward branching offset and compile the offset
; immediate

: again ( addr n -- ) 
  \ similar to until but compile braing instead of 0branch in the dictionary
  \ to construct an infinite loop. 
  \ 
  \ Execution cannot leave this loop unless the words R> drop are executed in a
  \ word inside this loop
  1 ?pairs \ error checking
  compile branch \ compile branch and an offset to begin
  back ; immediate
: while ( a1 n1 -- a1 n1 a2 n2 )
  [compile] if \ call if to compile 0branch and the offset
  2 + \ leave 4 as n2 to be checked by repeat
  ; immediate

: repeat ( a1 n1 a2 n2 -- )
  \ compile branch to jump back to begin. Resolve also the branching offset
  \ required by while
  >r >r \ get a2 and n2 out of the way
  [compile] again \ let again do the dirty work of compiling unconditional
  \ branch back to begin
  r> r> \ restore addr2 and n2 
  [compile] endif \ use endif to resolve the forward branching needed by WHILE
  ; immediate
: do ( n1 n2 -- ) \ runtime
     ( -- addr n ) \ compile time
	 compile (do) \ compile the runtime routine address of (do) into the dictionary
	 here \ address addr for bacward branching from loop or loop.
	 3 \ number for error checking
	 ; immediate
: loop ( addr n -- )
  \ terminate a do loop structure in a colon definition
  3 ?pairs \ check the number left by do, it should be a 3
  compile (loop) \ compile loop at runtime when loop is executed
  back \ compute and compile the backward branch offset
  ; immediate
: +loop ( n1 -- ) \ runtime
		( addr n1 -- ) \ compile time
	\ increment the loop index by n1 on the stack and test for loop completion
	\ branch back to do if not yet done
	3 ?pairs \ check n. If it is not 3 as left by DO, issue an error message
	compile (+loop) \ compile the address of (+loop) at runtime when the colon
	                \ definition is being built
	back \ compile back branch offset
	; immediate
