\ TODO make this part of the editor vocabulary
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
\ delete the nth line. Move subsequent lines up one line.
\ The delete line is held in PAD in case it is still needed.
dup H \ copy the nth line to PAD
0x0F \ the last line
dup rot \ get n to top of stack
do
	I 1+ line \ next line to be moved
	I -move \ upshift by one line
loop 
E \ erase the last line
;

: E ( n -- )
  \ erase the nth line in the screen buffer by filling with 64 blanks
  line \ line address
  c/l blanks \ fill with blanks
  update ;
: R ( n -- )
  \ replace the nth line with text stored in PAD
  PAD 1+ \ starting address of the text in PAD
  swap -move \ move text from PAD to nth line
  ;

: P ( n -- )
  \ put the following text on line n. Write over its contents
  1 text \ accept the following text of c/l characters or until CR to PAD
  R \ put the text into line n
  ;
: I ( n -- )
  \ insert text from PAD to nth line. Shift the original nth and subsequent
  \ lines down by one line
  \ 
  \ The last line in the screen is lot
  dup S \ spread line n and pad with blanks
  R     \ move PAD into line n
  ;

: clear ( n -- ) 
  \ clear the nth screen by padding with blanks
  SCR ! \ store the screen number into scr
  0x10 0 do  \ erase 16 lines
  forth I \ get the loop count from return stack.
  		  \ I was redefined by the editor to insert line into a screen. 
		  \ to call the I which gets the loop count, forth must be called to
		  \ make the trunk FORTH vocabulary the context vocabulary
		  \ which is searched first to get the correct I. This demonstrates
		  \ the use of vocabularies
  editor E \ set the context vocabulary back to editor vocabulary
           \ to continue editing texts. E will erase the i'th line
  LOOP
  ;
	
: copy ( n1 n2 -- )
	\ copy screen n1 in drive 0 to screen n2 in drive 1. 
	\ This is accomplished by reading blocks in screen n1 to disk buffers and
	\ changing block numbers to those associated with screen n2.
	\ The disk buffers are then flushed back to disk.
	b/scr * \ first block in screen n2
	offset @ + \ add block offset for drive 1
	swap b/scr * \ first block in screen n1
	b/scr over + \ last block number + 1
	swap do \ go through all blocks in screen n1
		dup \ copy block number in screen n2
		forth I \ current block number in screen n1 as the loop count
		block \ read the block from screen n1 to disk buffer
		2 - ! \ store the block number in screen n2 into the first cell of the
			  \ disk buffer, which contains the disk block number.
			  \ This tricks the system to think the block is in the screen n2
		1 + T
		update \ set update bit in disk buffer to be flushed back to disk
		loop
		drop \ discard the block number on stack
		flush \ write all disk buffers containing data from screen n1 back to 
			  \ screen n2, because the block numbers were switched
	;
\ string routines
: top ( -- ) 
  \ move the cursor to home, top left of the screen
0 R# ! \ store 0 in R#, the cursor pointer
;

: #locate ( -- n1 n2 )
  \ from the cursor pointer R# compute the line number n2 and the character
  \ offset n1 in line number n2
  R# @ \ get the cursor location
  c/l /mod \ divide cursor location by c/L. 
  		   \ Line number is the quotient and the offset is the remainder
	;
