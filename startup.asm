segment startup_img align=16
bg_exit:
    dw 320, 200
    incbin "media/img/startup/startup_exit.bin"

arrow:
    dw 6, 9
    incbin "media/img/startup/arrow.bin"

;cloud_s:
;    dw 28, 18
;    incbin "media/img/startup/cloud_s.bin"

;cloud_l:
;    dw 39, 25
;    incbin "media/img/startup/cloud_l.bin"

segment start_img align=16
bg_start:
    dw 320, 200
    incbin "media/img/startup/bg_start.bin"

segment manual_img align=16
op_manual:
    dw 320, 200
    incbin "media/img/startup/op_manual.bin"


%define ARROW_X 45
%define ARROW_Y_START 28
%define ARROW_Y_EXIT 61

segment code align=16

    mov bx, 0
.loop:
    
    setPos di, 0, 0
    mov ax, startup_img
    mov ds, ax
    mov si, bg_exit
    call lib:printBitmap

    cmp bx, 0FFFFh
    je .exit_arrow

.start_arrow:
    setPos di, ARROW_X, ARROW_Y_START
    mov si, arrow
    call lib:printBitmap

    jmp .flush
    
.exit_arrow:
    setPos di, ARROW_X, ARROW_Y_EXIT
    mov si, arrow
    call lib:printBitmap

.flush:
    call lib:flushBuffer

    ;Judge key
    mov ah, 01h
    int 16h
    jz .loop

    ;Key pressed
    mov ah, 00h ;Clear buffer
    int 16h

    cmp al, 13 ;If pressed enter
    je .exit_loop

    not bx ;Toggle state
    jmp .loop

.exit_loop:
    cmp bx, 0
    jne .return

    setPos di, 0, 0
    mov ax, start_img
    mov ds, ax
    mov si, bg_start
    call lib:printBitmap
    call lib:flushBuffer

    mov cx, 40 ;Delay a while
    ;call delay
    
    setPos di, 0, 0
    mov ax, manual_img
    mov ds, ax
    mov si, op_manual
    call lib:printBitmap
    call lib:flushBuffer
    
    mov cx, 160 ;Delay a while
    ;call delay

.return:
    mov ax, bx
    
