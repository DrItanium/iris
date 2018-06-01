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

: code ( -- ) create [compile] assembler ;
