%ifndef GRAPHIC_M
%define GRAPHIC_M

%define FrameBuffer 0a000h
%define FBWidth 320
%define FBHeight 200

;======================================================
;Action: Switch to VGA video mode(320x200,256 colors)
;======================================================
%macro enterVideoMode 0
    push ax
    mov ax, 13h
    int 10h
    pop ax
%endmacro

;====================================
;Action: Switch to text mode(80x25)
;====================================
%macro enterTextMode 0
    push ax
    mov ax, 3
    int 10h
    pop ax
%endmacro

;===================================
;Action: Set to direct output mode
;===================================
%macro setDirectMode 0
    push ax
    push ds
    mov ax, data
    mov ds, ax
    mov ax, FrameBuffer
    mov word [ds:ptrBufSeg], ax
    pop ds
    pop ax
%endmacro

;==========================================================
;Action: Set to double buffering mode
;==========================================================
%macro setDoubleBufMode 0
    push ax
    push ds
    mov ax, data
    mov ds, ax
    mov ax, doubleBuf
    mov word [ds:ptrBufSeg], ax
    pop ds
    pop ax
%endmacro

;=============================================================
;Action: Calculate position of 2D coordinate in frame buffer
;Parameters: index register,X,Y
;Exit: index register=flatten position
;=============================================================
%macro setPos 3
    push ax
    mov ah, 0
    mov al, %3 ;Y-pos
    shl ax, 6
    add ah, %3 ;y*320 = (y<<8)+(y<<6)
    add ax, %2 ;X-pos
    mov %1, ax
    pop ax
%endmacro

;================================================
;Action: move index of frame buffer to next row
;Parameters: index register, width
;Exit: index register=new position
;================================================
%macro nextRow 2
    add %1, FBWidth
    sub %1, %2
%endmacro

;================================
;Action: Set all pixel to black
;================================
%macro clearScreen 0
    push ax
    mov al, 0
    call lib:fillColor
    pop ax
%endmacro

%endif