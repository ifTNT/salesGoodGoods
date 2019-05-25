bits 16

segment code
    jmp start
    %include "mode13h.asm"

start:
    ;Set stack
    mov ax, stack
    mov ss, ax
    mov sp, stack_top

    enterVideoMode
    setDoubleBufMode
    clearScreen

draw_bg:
    mov cx, 180
row:
    push cx
    mov cx, 300
col:
    pop bx
    push bx
    setPos di, cx, bl

    mov ax, images
    mov ds, ax  ;Segment of bitmap
    mov si, spacer ;Head offset of bitmap
    call printBitmap

    sub cx, 20
    jnc col

    pop cx
    sub cl, 20
    jnc row
    
    setPos di, 20, 0
    mov ax, images
    mov ds, ax  ;Segment of bitmap
    mov si, box_empty ;Head offset of bitmap
    call printBitmap

    call flushBuffer

    mov ah, 00h;
    int 16h

    enterTextMode

    ;Return to dos
    mov ah, 4ch
    int 21h

;==============================
;Action: Draw the sepcific map
;Parameters: si: target level
;Return: None
;==============================
draw_map:
    mov ax, maps
    mov es, ax
    add si, 16*10 ;Move to the end of map
    mov bx, (200-20)

row:
    mov cx, (320-20)




segment maps align=16
l1:
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 1, 1, 1, 3, 4, 1, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 1, 1, 2, 1, 1, 1, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

segment images align=16
%macro tile 1
%1:
    dw 20, 20 ;Width, Height
    %defstr fileName %1
    %strcat path "media/img/tile/", fileName, ".bin"
    incbin path
    %undef path
    %undef fileName

%endmacro
    tile box_empty
    tile spacer

segment stack stack align=16
    resb 256
    stack_top: