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

startup:
    %include "startup.asm"

    cmp ax, 0FFFFh
    je end

    mov di, level1
    call drawMap

    mov ax, maps
    mov es, ax
    mov ax, images
    mov ds, ax
    
moveRight:
    mov cx, 80 ;Starting point of left
    mov bx, 1 ;Speed when moving right
.L1:
    push cx
    mov di, level1
    mov cx, 71
    call drawSingleTile
    mov cx, 70
    call drawSingleTile
    mov cx, 69
    call drawSingleTile
    mov cx, 68
    call drawSingleTile
    pop cx
    
    setPos di, cx, 80
    mov ax, images
    mov ds, ax  ;Segment of bitmap
    test bx, bx
    js .bear_2
    mov si, bear_1 ;Head offset of bitmap
    jmp .next
.bear_2:
    mov si, bear_2
.next:
    call lib:printBitmap
    call lib:flushBuffer

    add cx, bx ;Update position with velocity
    test bx, bx
    js .isMoveLeft ;If direction is negitive
    cmp cx, 140 ;Right most boundary
    jbe .L1 ;If not touch boundary, loop
    mov cx, 140 ;Start point of right
    mov bx, -1 ;Speed when moving left
.isMoveLeft:
    cmp cx, 80 ;Left most boundary
    jae .L1 ;If not touch boundary, loop
    jmp moveRight ;Start again

    mov ah, 00h;
    int 16h

end:

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
    push cx
    push es
    push ds

    mov ax, maps
    mov es, ax
    mov ax, images
    mov ds, ax
    
    mov cx, 16*10-1
.L1:
    call drawSingleTile
    sub cx, 1
    jnc .L1

    pop ds
    pop es
    pop cx
    ret

;=========================================
;Action: Draw single pice of map
;Parameters:
;   es: segment of map
;   ds: segment of images
;   di: target level
;   cx: offset of tile (map coordinate)
;=========================================
drawSingleTile:
    push ax
    push cx
    push dx
    push di
    push si

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
    db 0, 0, 0, 0, 1, 1, 1, 3, 2, 5, 1, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 1, 2, 4, 6, 1, 1, 0, 0, 0, 0
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
    dw banana, watermelon
    dw banana_box, watermelon_box
;-----------------------------
    tile box_empty
    tile spacer
    tile void
    tile barrier
    tile banana
    tile watermelon
    tile banana_box
    tile watermelon_box
    tile bear_1
    tile bear_2

segment stack stack align=16
    resb 256
    stack_top: