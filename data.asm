segment InGameData align=16
currentLevel:
    resw 1 ;Number of current level
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
levels:
    dw level0, level1, level2, level3, level4
level0:
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

level1:
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 1, 1, 2, 1, 2, 1, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 1, 1, 1, 3, 1, 1, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 1, 1, 4, 1, 1, 1, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.info:
    db 8, 5  ;Initial main char pos(map coordinate)
    dw 2     ;Count of following box
    db 9, 5  ;Position of box0(map coordinate)
    db 9, 6  ;Position of box1(map coordinate)

level2:
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0
    db 0, 0, 0, 1, 1, 2, 1, 1, 1, 2, 2, 1, 1, 0, 0, 0
    db 0, 0, 0, 1, 1, 1, 1, 2, 1, 1, 3, 1, 1, 0, 0, 0
    db 0, 0, 0, 1, 1, 1, 1, 4, 1, 1, 1, 1, 1, 0, 0, 0
    db 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 0, 0, 0
    db 0, 0, 0, 0, 0, 1, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.info:
    db 5, 8  ;Initial main char pos(map coordinate)
    dw 2     ;Count of following box
    db 7, 3  ;Position of box0(map coordinate)
    db 4, 5  ;Position of box1(map coordinate)

level3:
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 2, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 2, 1, 1, 1, 1, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 1, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 2, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 3, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 1, 2, 1, 1, 2, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 2, 2, 1, 1, 4, 3, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.info:
    db 8, 4   ;Initial main char pos(map coordinate)
    dw 3      ;Count of following box
    db 9, 2   ;Position of box0(map coordinate)
    db 10, 3  ;Position of box1(map coordinate)
    db 6, 6   ;Position of box2(map coordinate)

level4:
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 2, 1, 1, 1, 1, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 3, 1, 1, 1, 2, 1, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 1, 4, 1, 1, 1, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 3, 1, 1, 1, 1, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 1, 1, 2, 1, 1, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.info:
    db 8, 4  ;Initial main char pos(map coordinate)
    dw 3     ;Count of following box
    db 8, 2  ;Position of box0(map coordinate)
    db 6, 4  ;Position of box1(map coordinate)
    db 9, 6  ;Position of box2(map coordinate)

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

segment ending_img align=16
ed:
    dw 320, 200
    incbin "media/img/ending.bin"

segment stack stack align=16
    resb 256
    stack_top: