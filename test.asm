.model tiny
.code
org 100h

;------------------------------------------------
;Exits to DOS
;------------------------------------------------
;Entry: None
;Exit: (not defined)
;Expects: None
;Destroys: AX
;------------------------------------------------
 EXIT		macro
		mov ax, 4c00h
		int 21h
		endm
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

Start:
    mov ax, 0aaaah
    mov bx, 0bbbbh
    mov cx, 0cccch
    mov dx, 0ddddh
    delay:
        mov ax, 0aaaah
        jmp delay

    EXIT

end Start