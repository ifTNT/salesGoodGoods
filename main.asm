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

;draw_bg:
;    mov cx, 180
;.row:
;    push cx
;    mov cx, 300
;.col:
;    pop bx
;    push bx
;    setPos di, cx, bl
;
;    mov ax, images
;    mov ds, ax  ;Segment of bitmap
;    mov si, spacer ;Head offset of bitmap
;    call printBitmap
;
;    sub cx, 20
;    jnc .col
;
;    pop cx
;    sub cl, 20
;    jnc .row

    mov di, level1
    call draw_map
    
    setPos di, 120, 80
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
;Parameters: di: target level
;Return: None
;==============================
draw_map:
    push ax
    push bx
    push cx
    push ds
    push es
    push di
    push si

    mov ax, maps
    mov es, ax
    
    mov ax, images
    mov ds, ax  ;Segment of bitmap
    
    add di, 16*10-1 ;Move to the end of map
    mov bx, (200-20) ;End of graph rows

.row:
    mov cx, (320-20) ;End of graph cols
.col:
    mov ah, 00h
    mov al, byte [es:di] ;Get tile id of current position
    
    ;Indirect searching offset of head of bitmap
    push di
    mov di, tile_table
    sal ax, 1
    add di, ax
    mov si, word [ds:di]
    pop di

    push di    
    setPos di, cx, bl
    call printBitmap
    pop di

    dec di
    sub cx, 20
    jnc .col

    sub bx, 20
    jnc .row
    
    pop si
    pop di
    pop es
    pop ds
    pop cx
    pop bx
    pop ax
    ret

segment maps align=16
level1:
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 2, 1, 1, 1, 1, 2, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 1, 1, 2, 1, 1, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 1, 2, 1, 1, 1, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 2, 1, 1, 1, 1, 2, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

%macro tile 1
%1:
    dw 20, 20 ;Width, Height
    %defstr fileName %1
    %strcat path "media/img/tile/", fileName, ".bin"
    incbin path
    %undef path
    %undef fileName

%endmacro
segment images align=16
tile_table:
    dw void, spacer, barrier
;-----------------------------
    tile box_empty
    tile spacer
    tile void
    tile barrier

segment stack stack align=16
    resb 256
    stack_top: