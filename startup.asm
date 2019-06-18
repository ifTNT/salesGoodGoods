segment startup_img align=16
bg_exit:
    dw 320, 200
    incbin "media/img/startup/startup_exit.bin"

arrow:
    dw 6, 9
    incbin "media/img/startup/arrow.bin"

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
    je .draw_exit

.draw_start:
    setPos di, ARROW_X, ARROW_Y_START
    mov si, arrow
    call lib:printBitmap

    jmp .flush
    
.draw_exit:
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
    mov ax, bx
    
