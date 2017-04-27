
.include "atari.inc"
.include "atari.mac"
.include "graph.inc"

.export _start ;, incPoint


.zeropage
pnt1: .res 2
pnt2: .res 2
tmp1: .res 1
tmp2: .res 1
cnt:  .res 1
posy: .res 1
posz: .res 1
pos:  .res 1
pix:  .res 1
msgP: .res 1
shrinkFactor_fractionnal: .res 1
shrinkFactor_integer: .res 1
shrink_integer: .res 1
shrink_fractionnal: .res 1
sdd: .res 2
crawlineptr: .res 2
crawlineptr2: .res 2
acc: .res 1
last_intruction_ptr: .res 2
last_intruction: .res 2
yoff:.res 1
first_pixel_hi:.res 1
last_pixel_hi:.res 1
first_pixel_lo:.res 1
last_pixel_lo:.res 1
half_line_lenght:.res 1
mask:.res 1
oimask:.res 1
flag:.res 1


.macro makeident lname, count
    .ident(.concat(lname,.sprintf("%d", count))):
.endmacro

.macro makeident2 lname, count
    .word .ident(.concat(lname,.sprintf("%d", count)))
.endmacro

.macro lomakeident2 lname, count
    .lobytes .ident(.concat(lname,.sprintf("%d", count)))
.endmacro

.macro himakeident2 lname, count
    .hibytes .ident(.concat(lname,.sprintf("%d", count)))
.endmacro


.code

 screen = $9000

 half_screen_x = 64+64

 first_line=35

pente=220
;
;
;
;   +----+----------------------+----+
;   |    |                      |    |
;   |    | F                    |    |
;   | O  | i                    | O  |
;   | f  | r                    | f  |
;   | f  | t                    | f  |
;   |    | +-----------------+| |    |
;   | s  |                    L | s  |
;   | c  |                    a | c  |
;   | r  |                    s | r  |
;   | e  |                    t | e  |
;   | e  |                      | e  |
;   | n  |                      | n  |
;   |    |                      |    |
;   0   64                   128+64 256   
;
;
_start:

 lda #$ff
 sta screen

 lda #$1e
 sta 708


 lda #$0
 sta 712

 jsr LOADER

 jsr initScreen

 lda #<crawline
 sta crawlineptr
 lda #>crawline
 sta crawlineptr+1

 lda #<crawline
 sta crawlineptr2
 lda #>crawline
 sta crawlineptr2+1

  lda #first_line
  and #$7
  tay
  lda mask_,y
  sta oimask
 
 
loupe:

 lda #<screen
 sta sdd
 lda #>screen
 sta sdd+1


  lda oimask
  sta mask

  ; first pixel=half_screen_x-curline
  sec
  lda #half_screen_x
  sbc #first_line 
  sta first_pixel_hi
 
 ; last pixel=half_screen_x+curline
  clc
  lda #half_screen_x
  adc #first_line
  sta last_pixel_hi
 
  lda #0
  sta first_pixel_lo
  sta last_pixel_lo
  
  sec
  lda last_pixel_hi
  sbc first_pixel_hi
  lsr
  sta half_line_lenght

loop58:

 ;lsr flag_full_line ; clear flag
 
 lsr flag ; clear flag

  ldx half_line_lenght
   cpx #64
   bcs full_line  ; x>=64
	
  lda first_pixel_hi
  tay
  lda jmp12_lo,y
  sta jmp13+1

  lda jmp12_hi,y
  sta jmp13+2

  
  lda last_pixel_hi
  tay     ; y=last-pixel
  and #$7
  beq :+  ; last_pixel and last-pixel-1 are not in the same byte

  tya     ; A=last-pixel
  sec
  sbc #64
  lsr
  lsr
  lsr
  sta yoff

  lda #$80 ; set flag
  sta flag
 
: 
  lda jmp12_lo,y
  sta last_intruction_ptr

  lda jmp12_hi,y
  sta last_intruction_ptr+1
 
  ldy #0

  lda (last_intruction_ptr),Y
  sta last_intruction;

  lda #$60 ; RTS   
  sta (last_intruction_ptr),Y
  

  lda half_line_lenght
  and #$3f 
  asl ; *2
  tay
  lda fixed,y 
  sta shrinkFactor_integer
  lda fixed+1,y 
  sta shrinkFactor_fractionnal
 
  lda #0
  sta shrink_integer
  sta shrink_fractionnal

  ldx #0
  beq jmp13

full_line:

  lda jmp12_lo_
  sta jmp13+1

  lda jmp12_hi_
  sta jmp13+2
  
  sec
  lda half_line_lenght 
  sbc #64
  and #$3f 
  asl ; *2
  tay

  lda offset,y 
  sta shrink_integer
  lda offset+1,y 
  sta shrink_fractionnal

  lda fixed_2,y 
  sta shrinkFactor_integer
  lda fixed_2+1,y 
  sta shrinkFactor_fractionnal
  
  ldx #0
  
jmp13:
   jsr loop58

   bit flag
  bpl :+
 
  ldy yoff
  txa
 sta (sdd),Y

:	clc		
	lda sdd
	adc #16
	sta sdd
        bcc :+             
        inc sdd+1
:



  ; Restore instruction
  ldy #0
  lda last_intruction;
  sta (last_intruction_ptr),Y

  	sec				; set carry for borrow purpose
	lda first_pixel_lo
	sbc #pente			; perform subtraction on the LSBs
	sta first_pixel_lo
        bcs :+
	dec first_pixel_hi			; do the same for the MSBs, with carry
	lda first_pixel_hi
	cmp #30       
	beq finisk
	;bmi finisk

:   	clc		
	lda last_pixel_lo
	adc #pente
	sta last_pixel_lo
        bcc :+             
        inc last_pixel_hi
:

        sec
	lda last_pixel_hi
	sbc first_pixel_hi
	lsr
	sta half_line_lenght
      
   	asl mask
	bcc :+
	
	lda #1
	sta mask
	
	clc		
	lda crawlineptr
	adc #128
	sta crawlineptr
    	bcc :+

	inc crawlineptr+1
:
 

   jmp loop58

  
finisk:
  
    asl oimask
	bcc :+
	
	lda #1
	sta oimask

  	clc		
	lda crawlineptr2
	adc #128
	sta crawlineptr2
    bcc :+

	inc crawlineptr2+1
:

   lda crawlineptr2
   sta crawlineptr
   lda crawlineptr2+1
   sta crawlineptr+1
   
   
   jmp loupe  
   
  
;
; Draw a line
;

.repeat 16, J        ; 128 pixels=16 bytes


.repeat 8, I         ; For each bits of the byte
	
makeident "foo", J*8+I

	ldy shrink_integer 
	lda (crawlineptr),y  ; A=8 pixels of the source
        bit mask 
	beq :+

	txa
	ora #(1<<(7-I))		; set the pixel 
	tax
:
	clc				
	lda shrink_fractionnal
	adc shrinkFactor_fractionnal
	sta shrink_fractionnal			
	lda shrink_integer
	adc shrinkFactor_integer			
	sta shrink_integer	
	
.endrepeat

	ldy #J
	txa
	sta (sdd),Y			; Put on screen 
        ldx #0
    
.endrepeat

	rts

LOADER: LDA $d301   ;Disable BASIC bit in PORTB for MMU
       ORA #$02
       STA $d301
       LDA #$01    ;Set BASICF for OS, so BASIC remains OFF after RESET
       STA $3f8

       lda #$c0    ;Check if RAMTOP is already OK
       CMP $6a     ;This prevent flickering if BASIC is already off
       BEQ RAMOK
       STA $6a     ;Set RAMTOP to end of BASIC

       LDA $e401   ;Open "E:" to ensure screen is not at $9C00
       PHA         ;This prevents garbage when loading up to $bc000
       LDA $e400
       PHA
RAMOK:  RTS

jmp12_lo:

.repeat 64
.byte 0
.endrepeat

jmp12_lo_:
.repeat 128,I
	lomakeident2 "foo",I
.endrepeat

jmp12_hi:

.repeat 64
.byte 0
.endrepeat

jmp12_hi_:
.repeat 128,I
	himakeident2 "foo",I
.endrepeat

mask_:
.byte 1,2,4,8,16,32,64,128

offset:
.repeat 64,I
   _x .set I
   _halsf_lenght .set 64+ _x
   _xx .set 256*_x*64/_halsf_lenght
  .hibytes _xx
  .lobytes _xx
  
.endrepeat

; 64/1 , 64/2 , 64/3 ......... 

fixed:
.repeat 63,I
  _65 .set 256*64/(I+1)
  .hibytes _65
  .lobytes _65
.endrepeat

fixed_2:

; 64/64
.byte 1,0

; 64/65 , 64/66, 64/67,.....
.repeat 63,I
   _66 .set 256*64/(64+I+1)
  .hibytes _66
  .lobytes _66
.endrepeat 

crawline:
.incbin "crawl.dat"


.bss

