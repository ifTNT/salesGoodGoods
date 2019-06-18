segment lib align=16
midiInit:
	; Get Environment Settings
	mov bx, 2
	mov dx, enviroment
	mov ax, blaster
	call driver:ct_midi
	
	; Initialize Driver
	mov bx, 3
	call driver:ct_midi

	retf

midiStop:
    ; Stop playing
	mov bx, 10
	call driver:ct_midi
	retf
	
midiPlayBgm:
	; Prepare MIDI Start
	mov bx, 8
	mov dx, midi
	mov ax, bgm
	call driver:ct_midi
	
	;Set system clock counter to 0
	mov ah, 1
	mov cx, 0
	mov dx, 0
	int 1ah
	
	; Play MIDI Music
	mov bx, 9
	call driver:ct_midi
	
	;===TODO===
	;Hook system ISR
	;Get system clock counter (result stored at cx:dx)
	;mov ah, 0
	;int 1ah
    ;
	;If system clock exceed duration, replay music
	;mov ax, midi
	;mov ds, ax
	;mov ax, word [ds:bgm_duration]
	;cmp dx, ax
	;ja play
	;===END TODO===
	
	retf

midiHalt:
	; Terminate Driver
	mov bx, 4
	call driver:ct_midi
	retf

segment enviroment align=16
	blaster db "A220 I7 D1 H5 T6"

segment driver align=16
	ct_midi:
		incbin "ctmidi.drv"
		
segment midi align=16
    duration: dw 0
    current_time: dw 0
	bgm:
        incbin "media/music/Rag_time_on_the_rag.mid"
    bgm_duration EQU 2575



