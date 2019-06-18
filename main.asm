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
    ;Read map info
    mov ax, InGameData
    mov es, ax
    mov word [es:currentLevelPtr], level1
    mov ax, maps
    mov ds, ax
    mov si, word [es:currentLevelPtr]
    add si, 16*10 ;Offset of map info
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

    ;Initial placed box
    mov word[es:cntPlaced], 0
    ;mov word[es:placedBox], 0505h
    ;mov word[es:placedBox+2], 5
    ;mov word[es:placedBox+4], 0605h
    ;mov word[es:placedBox+6], 6

    ;---End level initalize---

.gameLoop:
    call drawMap
    call drawMainChar
    call drawBoxes
    call drawPlaced
    call lib:flushBuffer

    mov ah, 01h
    int 16h
    ;jz .gameLoop

    ;Key judge
    mov ah, 00h
    int 16h

    cmp al, 'w'
    je .w
    cmp al, 'a'
    je .a
    cmp al, 's'
    je .s
    cmp al, 'd'
    je .d
    cmp al, 'r'
    je .reset
    jmp .endKeyJudge

.w:
    mov bh, -1
    mov bl, 0
    jmp .charPosJudge
.a:
    mov bh, 0
    mov bl, -1
    jmp .charPosJudge
.s:
    mov bh, 1
    mov bl, 0
    jmp .charPosJudge
.d:
    mov bh, 0
    mov bl, 1
    jmp .charPosJudge
.reset:
    jmp startGame
.charPosJudge:
    mov ax, InGameData
    mov es, ax
    mov ax, word [es:mainCharPos]
    mov word [es:lastCharPos], ax ;Backup old position
    add al, bl ;Update position
    add ah, bh
    mov word [es:mainCharPos], ax
    push ax
    call isLeagle
    cmp ax, 0
    je .endKeyJudge

    ;Restore last position
    push word [es:lastCharPos]
    pop word [es:mainCharPos]
    cmp ax, 2
    jne .endKeyJudge

    ;Move box
    pop ax
    mov di, cx
    dec di
    shl di, 1
    add di, boxPos
    mov cx, 2;
    push ax
.infinity_box:
    pop ax
    add al, bl
    add ah, bh
    push ax
    
    call isLeagle
    cmp ax, 2
    je .endInfinityBox
    cmp ax, 1
    je .boxHitBlock
    pop ax
    push ax
    
    mov word [es:di], ax
    call drawMap
    call drawMainChar
    call drawBoxes
    call drawPlaced
    call lib:flushBuffer

    jmp .infinity_box

.boxHitBlock:
    pop ax
    push ax
    call blockAt
    cmp al, 3
    je .banana
    cmp al, 4
    je .watermelon
    jmp .endInfinityBox

.banana:
    ;Place 5 into placed
    mov ax, InGameData
    mov ds, ax
    pop ax
    mov bx, word [ds:cntPlaced] ;Get last index
    mov word [es:di], ax ;Put box into block
    push bx
    shl bx, 2
    mov word [ds:bx+placedBox], ax  ;Position of new placed box
    mov word [ds:bx+placedBox+2], 5 ;ID of new placed box
    pop bx
    inc bx ;Move to next position
    mov word [ds:cntPlaced], bx ;Put next position
    jmp .endKeyJudge 
.watermelon:
    ;Place 6 into placed 
    mov ax, InGameData
    mov ds, ax
    pop ax
    mov bx, word [ds:cntPlaced] ;Get last index
    push bx
    shl bx, 2
    mov word [es:di], ax ;Put box into block
    mov word [ds:bx+placedBox], ax  ;Position of new placed box
    mov word [ds:bx+placedBox+2], 6 ;ID of new placed box
    pop bx
    inc bx ;Move to next position
    mov word [ds:cntPlaced], bx ;Put next position
    jmp .endKeyJudge

.endInfinityBox:
    pop ax

.endKeyJudge:
    
    jmp .gameLoop

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

;====================================================
;Action: Draw placed box accroding to in game data
;====================================================
drawPlaced:
    ;Save register will use
    push ax
    push bx
    push cx
    push dx
    push di
    push si
    push ds
    push es

    mov ax, InGameData
    mov ds, ax
    mov cx, [ds:cntPlaced]
    cmp cx, 0
    je .return
.drawBox:
    push cx
    ;Read individual box position
    mov si, cx
    dec si
    shl si, 2
    add si, placedBox
    mov bx, [ds:si]
    movzx dx, bl ;X-pos
    mov bl, bh
    mov bh, 0

    imul dx, 20
    imul bx, 20
    
    ;Draw box
    setPos di, dx, bl
    add si, 2
    mov dx, word [ds:si] ;Box ID

    mov ax, images
    push ds
    mov ds, ax
    
    ;Indirect searching offset of head of bitmap
    mov si, tile_table
    sal dx, 1
    add si, dx
    mov si, word [ds:si] ;si = ds:tile_table+ax*2
    
    call lib:printBitmap
    pop ds

    pop cx
    loop .drawBox

.return:
    ;Restore registers
    pop es
    pop ds
    pop si
    pop di
    pop dx
    pop cx
    pop bx
    pop ax

    ret

;========================================================
;Action: Get block at current level at spcific position
;Parameter: ax: Position{Ypos, Xpos}
;Return: al: Block ID
;========================================================
blockAt:
    push bx
    push si
    push ds
    push es

    push ax
    mov ax, InGameData
    mov es, ax
    mov ax, maps
    mov ds, ax
    pop ax
    
    mov si, word [es:currentLevelPtr]
    movzx bx, ah
    mov ah, 0
    shl bx, 4
    add ax, bx ;ax = Ypos*16+Xpos
    add si, ax
    mov al, byte [ds:si]
    
    pop es
    pop ds
    pop si
    pop bx
    ret


;======================================
;Action: Judge whether is leagle move
;Parameter: ax: {Ypos, Xpos}
;Return: ax==0: leagle move
;        ax==1: Touch bokc
;        ax==2: Touch empty box
;        cx = box id
;======================================
isLeagle:
    ;Save registers
    push bx
    push si
    push es
    push ds

    push ax
    mov ax, InGameData
    mov es, ax
    mov ax, maps
    mov ds, ax
    pop ax
    
    ;Judge empty box
    mov cx, word [es:cntBox]
.l1:
    mov si, cx
    dec si
    shl si, 1
    add si, boxPos
    mov bx, [es:si]
    cmp bx, ax
    je .touchBox
    loop .l1

    ;Judge block
    call blockAt
    cmp al, 1
    jne .illeagle

    ;Leagle move
    mov ax, 0
    jmp .return

.illeagle:
    mov ax, 1
    jmp .return
.touchBox
    mov ax, 2
.return:
    ;Restore registers
    pop ds
    pop es
    pop si
    pop bx
    ret


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
    mov bl, bh
    mov bh, 0

    imul cx, 20
    imul bx, 20

    ;Draw images
    mov ax, images
    mov ds, ax
    mov si, bear_1
    setPos di, cx, bl
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
;Action: Draw boxes accroding to in game data
;==============================================
drawBoxes:
    ;Save register will use
    push ax
    push bx
    push cx
    push dx
    push di
    push si
    push ds
    push es

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
    mov bl, bh
    mov bh, 0

    imul dx, 20
    imul bx, 20
    
    ;Draw box
    setPos di, dx, bl
    mov ax, images
    push ds
    mov ds, ax
    mov si, box_empty
    call lib:printBitmap
    pop ds

    loop .drawBox
    
    ;Restore registers
    pop es
    pop ds
    pop si
    pop di
    pop dx
    pop cx
    pop bx
    pop ax

    ret
;============================================================
;Action: Copy 4 adjacency neiborhood tile from current level
;============================================================
clearAdj:
    ret

;================================================
;Action: Draw the map accroding to in game data
;Return: None
;================================================
drawMap:
    push cx
    push di
    push es
    push ds

    mov ax, InGameData
    mov ds, ax
    mov di, word [ds:currentLevelPtr]
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
    pop di
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
currentLevelPtr:
    resw 1 ;Pointer of current level(in map segment)
lastCharPos:
    resb 2
mainCharPos:
    resb 2
cntBox:
    resw 1
boxPos:
    resb 320
cntPlaced:
    resw 1
placedBox:
    resb 320

segment maps align=16
level1:
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 3, 4, 1, 1, 1, 1, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 2, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 1, 3, 2, 1, 1, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 1, 2, 4, 1, 1, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 2, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.info:
    db 6, 5  ;Initial main char pos(map coordinate)
    dw 2     ;Count of following box
    db 5, 4  ;Position of box0(map coordinate)
    db 6, 4  ;Position of box1(map coordinate)

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