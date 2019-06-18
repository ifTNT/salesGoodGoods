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
    ;%include "startup.asm"

    cmp ax, 0FFFFh
    je end

startGame:
    
    ;---Begin level initialize---
    ;Draw initial map
    mov di, level1
    call drawMap

    ;Read map info
    mov ax, maps
    mov ds, ax
    mov si, level1_info
    mov ax, InGameData
    mov es, ax
    ;Copy inital main character position
    mov ax, word [ds:si]
    mov word [es:mainCharPos], ax
    ;Copy box counter
    add si, 2 ;Offset of box counter
    mov ax, word [ds:si]
    mov word [es:cntBox], ax
    ;Copy inital box position
    add si, 2 ;Offset of box pos
    mov cx, ax
    mov di, boxPos
    rep movsw 

    ;---End level initalize---

    call drawMainChar
    call drawBoxes
    call lib:flushBuffer

    jmp end_pause

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

end_pause:
    mov ah, 00h;
    int 16h

end:

    call lib:midiStop
    call lib:midiHalt

    enterTextMode

    ;Return to dos
    mov ah, 4ch
    int 21h

;=======================================================
;Action: Draw main character accroding to in game date
;=======================================================
drawMainChar:
    ;Save register will use
    push ax
    push bx
    push cx
    push di
    push si
    push ds
    push es
    
    mov ax, InGameData
    mov ds, ax
    mov bx, [ds:mainCharPos]
    movzx cx, bl ;X-pos

    ;Draw images
    mov ax, images
    mov ds, ax
    mov si, bear_1
    setPos di, cx, bh
    call lib:printBitmap
    
    ;Restore registers
    pop es
    pop ds
    pop si
    pop di
    pop cx
    pop bx
    pop ax
    ret

;==============================================
;Action: Draw boxed accroding to in game data
;==============================================
drawBoxes:
    
    mov ax, InGameData
    mov ds, ax
    mov cx, [ds:cntBox]
.drawBox:
    ;Read individual box position
    mov si, cx
    dec si
    shl si, 1
    add si, boxPos
    mov bx, [ds:si]
    movzx dx, bl ;X-pos
    
    ;Draw box
    setPos di, dx, bh
    mov ax, images
    push ds
    mov ds, ax
    mov si, box_empty
    call lib:printBitmap
    pop ds

    loop .drawBox

    ret

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

;=======================
;Action: Delay a while
;Parameter: cx
;=======================
delay:
.l1:
    push cx
    mov cx, 0
.l2:
    loop .l2
    pop cx
    loop .l1
    ret

%include "mode13h.asm"
%include "midi.asm"

segment InGameData align=16
mainCharPos:
    resb 2
cntBox:
    resw 1
boxPos:
    resb 320

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

level1_info:
    db 160, 100  ;Initial main char pos(screen coordinate)
    dw 2         ;Count of following box
    db 140, 80   ;Position of box0(screen coordinate)
    db 120, 90  ;Position of box1(screen coordinate)

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