bits 16

%include "mod13h_mac.asm"

segment code
    jmp start
start:
    ;Set stack
    mov ax, stack
    mov ss, ax
    mov sp, stack_top

    enterVideoMode
    setDoubleBufMode
    clearScreen

    call lib:midiInit
    call lib:midiPlayBgm

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
;    call lib:printBitmap
;
;    sub cx, 20
;    jnc .col
;
;    pop cx
;    sub cl, 20
;    jnc .row

jmp .move_left

.move_right:
    mov cx, 80
.L1
    mov di, level1
    call drawMap
    
    setPos di, cx, 80
    mov ax, images
    mov ds, ax  ;Segment of bitmap
    mov si, box_empty ;Head offset of bitmap
    call lib:printBitmap

    call lib:flushBuffer

    add cx, 2
    cmp cx, 140
    jbe .L1
.move_left:
    mov cx, 140
.L2
    mov di, level1
    push cx
    mov cx, 71
    call drawSingleMap
    pop cx

    setPos di, cx, 80
    mov ax, images
    mov ds, ax  ;Segment of bitmap
    mov si, box_empty ;Head offset of bitmap
    call lib:printBitmap

    call lib:flushBuffer

    sub cx, 2
    cmp cx, 80
    jae .L2
    jmp .move_right

    mov ah, 00h;
    int 16h

    call lib:midiStop
    call lib:midiHalt

    enterTextMode

    ;Return to dos
    mov ah, 4ch
    int 21h

;==============================
;Action: Draw the sepcific map
;Parameters: di: target level
;Return: None
;==============================
drawMap:
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
    mov si, tile_table
    sal ax, 1
    add si, ax
    mov si, word [ds:si] ;si = ds:tile_table+ax*2

    push di    
    setPos di, cx, bl
    call lib:printBitmap
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

;=========================================
;Action: Draw single pice of map
;Parameters:
;   di: target level
;   cx: offset of tile (map coordinate)
;=========================================
drawSingleMap:
    push ax
    push cx
    push dx
    push di
    push si
    push ds
    push es

    mov ax, maps
    mov es, ax
    mov ax, images
    mov ds, ax
    add di, cx    
    mov ah, 00h
    mov al, byte [es:di] ;Get tile id of current position
    
    ;Indirect searching offset of head of bitmap
    mov si, tile_table
    sal ax, 1
    add si, ax
    mov si, word [ds:si] ;si = ds:tile_table+ax*2

    mov dx, cx
    and dx, 000FH;
    mov dh, dl
    shl dl, 2
    add dl, dh
    mov dh, 0
    shl dx, 2 ;dx = cx%16*20 (x-coordinate)

    mov di, cx
    shr di, 4
    imul di, 6400 ;di = (cx/16)*20*320 (y-coordinate)

    add di, dx ;combine x and y coordinate

    call lib:printBitmap

    pop es
    pop ds
    pop si
    pop di
    pop dx
    pop cx
    pop ax
    ret


%include "mode13h.asm"
%include "midi.asm"


segment maps align=16
level1:
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 2, 1, 1, 1, 1, 2, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 1, 3, 2, 1, 1, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 1, 2, 4, 1, 1, 1, 0, 0, 0, 0
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
    dw void, spacer, barrier, banana, watermelon
;-----------------------------
    tile box_empty
    tile spacer
    tile void
    tile barrier
    tile banana
    tile watermelon

segment stack stack align=16
    resb 256
    stack_top: