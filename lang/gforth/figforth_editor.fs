\ editor stuff
: text ( c -- )
  \ move a text string delimited by character c from the dict buf into PAD,
  \ blank-filling the remainder of PAD to 64 chars
  here \ top of dict to be used as a word buffer
  c/l 1+ blanks \ fill word buffer with 65 blanks
  word 			\ move the text, delimited by character c, from the input
  				\ stream to the word buf
  pad 			\ address of the text buffer
  c/l 1+ cmove  \ move the text 64 bytes of text and 1 length byte to pad
  ;
: line ( n -- addr )
  \ leave address of the beginning of line n in the screen buffer.
  \ the screen number is in SCR. Read the disk block from disk if it is not
  \ already in the disk buffers
  
  dup 0xFFF0 and \ make sure n is between 0 and 15
  17 ?error \ if not, issue an error message
  scr @ \ get the screen number from SCR
  (line)  \ read the screen into screen buffer which is composed of the disk
  		  \ buffers. Compute the address of the n'th line in the screen buffer
		  \ and push it on the stack
  drop    \ discard the char count left on stack by (line)
		  \ only the line address is left on stack now
  ;

: -move ( addr n -- )
\ copy a line of text from addr to nth line in the current screen buffer
line \ get the line address in screen buffer
c/l cmove \ move 64 characters from addr to line n in screen buffer
update    \ notify the disk handler this buffer has been modified
		  \ it will be written back to disk to update the disk storage
;

: H ( n -- )
  \ copy nth line to PAD. Hold the text there ready to be typed out
  line \ get the line address
  pad 1+ \ starting address of text in PAD.
  c/l dup pad c! \ put 64 in the length byte of PAD
  cmove \ move one line 
  ;
: S ( n -- )
  \ spread nth line with blanks. Down shift the original nth and subsequent
  \ lines by one line. The last line of the screen is lost.
  dup 1- \ lower limit of lines to be moved
  0x0E \ 14, the last line to be shifted down
  DO
  	I line \ get the line address
	I 1+ \ next line
	-move \ downshift one line
1 +loop \ decrement loop count and repeat till done
E \ erase the nth line
;

: D ( n -- )

