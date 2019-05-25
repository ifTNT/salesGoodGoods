%ifndef GRAPHIC_ASM
%define GRAPHIC_ASM

segment doubleBuf align=16
    resb FBWidth*FBHeight
segment data align=16
    ptrBufSeg dw 0 ;Storage of destination buffer

segment lib align=16

;=====================================================
;Action: Flush second buffer to frame buffer
;=====================================================
flushBuffer:
    ;Backup registers going to use
    pushf
    push di
    push si
    push cx
    push es
    push ds

    mov cx, data
    mov ds, cx
    mov cx, word [ds:ptrBufSeg]
    cmp cx, FrameBuffer
    je .ignore ;Fool-proofing of direct mode
    mov ds, cx ;Set source segment
    mov cx, FrameBuffer
    mov es, cx ;Set destination to frame buffer

    mov di, 0
    mov si, 0
    mov cx, FBHeight*FBWidth
    rep movsb ;Copy whole buffer

.ignore:
    pop ds
    pop es
    pop cx
    pop si
    pop di
    popf
    retf

;=========================================
;Action: Set all pixel to specific color
;Parameters: al: Color to set
;Exit: None
;=========================================
fillColor:
    ;Backup registers going to use
    pushf
    push di
    push cx
    push es
    
    mov cx, data
    mov es, cx
    mov cx, word [es:ptrBufSeg]
    mov es, cx ;Set destinaton segment
    mov di, 0
    mov cx, FBHeight*FBWidth
    rep stosb ;Copy whole screen

    pop es
    pop cx
    pop di
    popf
    retf

;====================================================================
;Action: Print a 32*32 color block which each color takes 2*2 space
;Parameters: di: Start position
;Exit: none
;====================================================================
printColorBlock:
    ;Backup registers going to use
    pushf
    push ax
    push di
    push cx
    push es
    
    mov cx, data
    mov es, cx
    mov cx, word [es:ptrBufSeg]
    mov es, cx ;Set destinaton segment
    mov al, 0 ;Reset color
    mov cx, 16
;--------
.LH:
    push cx ;Backup vertical counter
    mov cx, 16
;----
.LW:
    mov byte [es:di], al
    mov byte [es:di+1], al
    mov byte [es:di+FBWidth], al
    mov byte [es:di+FBWidth+1], al ;Draw 4 pixel
    inc di
    inc di ;Move horizontally by 2 pixel
    inc al ;Next color
    loop .LW
;----
    nextRow di, 16
    nextRow di, 16 ;Move to next 2 row 
    pop cx ;Restore vertical counter
    loop .LH
;--------    
    
    ;Restore used registers
    pop es
    pop cx
    pop di
    pop ax
    popf
    retf
    
;=========================================
;Action: Fill all screen with 256 color
;Parameters: none
;Exit: none
;=========================================
fillRainbow:
    ;Backup registers going to use
    pushf
    push ax
    push di
    push cx
    push es

    mov cx, data
    mov es, cx
    mov cx, word [es:ptrBufSeg]
    mov es, cx ;Set destinaton segment
    mov di, 0
    mov cx, FBHeight
;--------
.LH:
    push cx
    mov al, 32 ;Reset color
    mov cx, FBWidth ;Fullfill the screen
;----
.LW:    
    mov byte [es:di], al
    inc di ;Next position
    inc al ;Change to next color
    cmp al, 256-8
    je .RESET ;If reach top of color space
    jmp .NORESET
.RESET:
    mov al, 32 ;Reset color
.NORESET:
    loop .LW
;----
    pop cx
    loop .LH
;--------
    ;Restore used registers
    pop es
    pop cx
    pop di
    pop ax
    popf
    retf

;==========================================================    
;Action: Print a bitmap with transparent(255=transparent)
;        First two word of bitmap indicate [width, hight]
;Parameters:
;   di: Start position
;   ds: Segment to bitmap
;   si: Offset of head of bitmap
;Exit:
;   di: End position
;   si: End of bitmap
;==========================================================
printBitmap:
    ;Backup registers going to use
    pushf
    push ax
    push bx
    push cx
    push dx
    push es

    mov cx, data
    mov es, cx
    mov cx, word [es:ptrBufSeg]
    mov es, cx ;Set destinaton segment
    mov ax, word [ds:si+0] ;width
    mov bx, word [ds:si+2] ;height
    add si, 4 ;Offset of image data segment
    cld ;Set direction flag to 0

;--------
    mov cx, bx ;Repeat height times
.LH:
    push cx ;Backup vertical counter
    mov cx, ax ;Repeat width times
;----
.LW:
    mov dl, byte [ds:si]
    cmp dl, 0ffh  ;Define 255=transparent
    je .nextPixel ;If transparent, skip the pixel
    mov byte [es:di], dl ;Copy a byte from bitmap to frame buffer
.nextPixel:
    inc si ;Move bitmap index to next position
    inc di ;Move frame buffer index to next position
    loop .LW
;----    
    nextRow di, ax
    pop cx ;Restore vertical counter
    loop .LH
;--------

    ;Restore used register
    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    popf
    retf

;==========================================================    
;Action: Print a bitmap with transparent(255=transparent)
;        First two word of bitmap indicate [width, hight]
;        And can print circular panning offset
;Parameters:
;   di: Start position
;   ds: Segment to bitmap
;   si: Offset of head of bitmap
;   dx: x-axis offset(0~width-1, 0=no offset)
;Exit:
;   di: End position
;   si: End of bitmap
;==========================================================
printCircularBitmap:
    ;Backup registers going to use
    pushf
    push ax
    push bx
    push cx
    push dx
    push es

    mov cx, data
    mov es, cx
    mov cx, word [es:ptrBufSeg]
    mov es, cx ;Set destinaton segment
    mov ax, word [ds:si+0] ;width
    mov bx, word [ds:si+2] ;height
    add si, 4 ;Offset of image data segment
    cld ;Set direction flag to 0

;--------
    mov cx, bx ;Repeat height times
.LH:
    push cx ;Backup vertical counter
    mov cx, ax ;Repeat width times
    add si, dx ;Add offset
;----
.LW:
    push dx
    mov dl, byte [ds:si]
    cmp dl, 0ffh  ;Define 255=transparent
    je .nextPixel ;If transparent, skip the pixel
    mov byte [es:di], dl ;Copy a byte from bitmap to frame buffer
.nextPixel:
    pop dx
    inc si ;Move bitmap index to next position
    inc di ;Move frame buffer index to next position
    push dx
    inc dx
    cmp cx, dx ;If si==ax-(dx+1) (the end index of the row)
    pop dx
    jne .loop
    sub si, ax ;Move si to start of this row
.loop:
    loop .LW
;----    
    ;Net effect: Move si to the end of this row
    push dx
    neg dx
    add dx, ax
    add si, dx ;si += (ax-dx)
    pop dx

    nextRow di, ax
    pop cx ;Restore vertical counter
    loop .LH
;--------

    ;Restore used register
    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    popf
    retf

%endif