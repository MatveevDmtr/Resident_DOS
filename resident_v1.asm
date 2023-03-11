.286

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
    xor bx, bx
    mov es, bx

    cli                                 ; prohibit ints

    mov bx, 36d
    mov ax, es:[bx]
    mov Old09ofs, ax
    mov ax, es:[bx+2]
    mov Old09seg, ax                    ; save Old keyboard handler

    mov es:[bx], offset New09
    mov ax, cs
    mov es:[bx+2], ax                   ; Insert my kbd handler

    mov bx, 32d
    mov ax, es:[bx]
    mov Old08ofs, ax
    mov ax, es:[bx+2]
    mov Old08seg, ax                    ; Save Old timer handler

    mov es:[bx], offset New08
    mov ax, cs
    mov es:[bx+2], ax                   ; Insert my timer handler

    sti                                 ; allow ints

    mov ax, 3100h                       ;terminate and stay resident
    mov dx, offset EOP
    shr dx, 4                           ; count blocks for prog
    inc dx
    int 21h

	EXIT


;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;New09: New handler of keyboard events
;++++++++++++++++++++++++++++++++++++++++++++++++
;Entry: None
;Exit: None
;Expects: None
;Destroys: AX, CX
;++++++++++++++++++++++++++++++++++++++++++++++++
New09	proc
        push AX BX CX DX SI DI BP SP DS ES SS

		mov bx, 0b800h										; save di
		mov es, bx											; STOSW+
		mov ah, 34h										    ; color
		mov bx, 160d*5+80d									; fill the color

        mov ax, cs  
        mov ds, ax


Next_newres:
        in al, 60h

        cmp al, 18h                                         ; 'o'
        jne not_Rhotkey                                            

        cmp UPDATE_REGS, 01h
        je not_hotkey

        call CPY_VMEM_SAVE                                  ; save mem on frame place

        call SHOW_FRAME
        call SHOW_REGS
        call SHOW_REG_VALUES                                ; show everything

        call CPY_VMEM_BUF                                   ; save frame to draw buf
        mov UPDATE_REGS, 1h

        jmp not_hotkey

        not_Rhotkey:

        cmp al, 2eh                                         ; 'c'
        jne not_hotkey

        call CPY_SAVE_VMEM                                  ; Recover VMem from Save_buf
        mov UPDATE_REGS, 0h

        not_hotkey:
        in al, 61h
        or al, 80h                                          ;10 000 000
        out 61h, al

        and al, not 80h                                     ;01 111 111
        out 61h, al

        mov al, 20h
        out 20h, al                                         ; talk to PPI
        
    pop ss es ds sp bp di si dx cx bx ax

    db 0eah
Old09ofs dw 0
Old09seg dw 0                                               ; jump to old kbd handler

	iret
endp
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;New08: New handler of timer
;++++++++++++++++++++++++++++++++++++++++++++++++
;Entry: None
;Exit: None
;Expects: None
;Destroys: AX, CX
;++++++++++++++++++++++++++++++++++++++++++++++++
New08	proc
        push AX BX CX DX SI DI BP SP DS ES SS

        mov ax, cs
        mov ds, ax                                          ; DS = code segment

		mov bx, 0b800h										; ES = VideoMem
		mov es, bx											
		mov ah, COLOR										; color							                                         

        cmp UPDATE_REGS, 0h                                 ; if not update => skip
        je dont_upd_regs

        call CMP_VMEM_BUF                                   ; save changes from Vmem

        cmp FOUND_DIF, 01h                                  ;
        jne dont_upd_frame

        call SHOW_FRAME                                     ;upd frame
        call SHOW_REGS

        dont_upd_frame:
        call SHOW_REG_VALUES                                ; upd values
        call CPY_VMEM_BUF

        dont_upd_regs:
        
    pop ss es ds sp bp di si dx cx bx ax

    db 0eah
Old08ofs dw 0
Old08seg dw 0                                               ; jump to old timer handler

	iret
endp
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;Puts hex number to VideoMem
;++++++++++++++++++++++++++++++++++++++++++++++++
;Entry: DI = shift, BX = number
;Exit: None
;Expects: ES = start VideoMem
;Destroys: AX, BX, CX, DX
;++++++++++++++++++++++++++++++++++++++++++++++++
SHOW_HEX	proc
		std							; STOSW+
        xor cx, cx
        mov cx, 4d					; 4 digits in a hex num
        add di, cx
        add di, cx
		mov dh, 10
        loop_hex:
                mov ax, bx
				and ax, 1111b       ; first digit
				mov dh, al
				cmp dh, 10
				jb add_to_mem
				add ax, 7           ; if a letter
                add_to_mem:
				add ax, 48          ; number => ASCII
				mov ah, COLOR
		        stosw
				shr bx, 4			; look next 4 bits
				loop loop_hex		; end of cycle
	ret
endp
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;Puts registers
;++++++++++++++++++++++++++++++++++++++++++++++++
;Entry: Dl = color, DI = shift, BX = number
;Exit: None
;Expects: ES = start VideoMem
;Destroys: AX, BX, CX
;++++++++++++++++++++++++++++++++++++++++++++++++
SHOW_REGS	proc

        push bp
        mov bp, sp                  ;prologue

        mov bx, 0b800h
        mov es, bx

        mov ax, cs
        mov ds, ax

        mov di, FRAME_DEST_ADR
        add di, WSCR_LENGTH
        add di, 4d                  ; DI = adr of start reg signs
        mov ah, COLOR
        mov si, offset REG_SIGN
        mov cl, 12d                 ; CL = num of regs
        print_all_registers:
            mov START_OF_LINE, di
            lodsb
            stosw
            lodsb
            stosw                   ; draw 2 syms of sign (fe 'AX')
            push si
            mov si, offset EQUALITY
            mov ch, 3d
            print_equality:
                lodsb
                stosw               ; draw ' = '
                dec ch
                cmp ch, 00h
                jne print_equality

            dec cl
            pop si
            
            mov di, START_OF_LINE
            add di, WSCR_LENGTH     ; next line
            cmp cl, 00h
            jne print_all_registers

        pop bp
	ret
endp
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;Puts reg values
;++++++++++++++++++++++++++++++++++++++++++++++++
;Entry: Dl = color, DI = shift, BX = number
;Exit: None
;Expects: ES = start VideoMem
;Destroys: AX, BX, CX, ES
;++++++++++++++++++++++++++++++++++++++++++++++++
SHOW_REG_VALUES	proc

        push bp
        mov bp, sp                  ;prologue

        add bp, 22d                 ;start from the end of regs in stack

        mov bx, 0b800h
        mov es, bx                  ;ES:=VideoMem

        mov ax, cs
        mov ds, ax                  ;DS:= code segment

        mov di, REGS_DEST_ADR       ;DI:= where to paste regs
        mov ah, 02h
        mov cl, 12d                 ;CL = num of regs
        print_regvalues:
            mov START_OF_LINE, di   ;save DI

            push cx                 ; save CX
            mov bx, [bp+2]          ;print reg from stack
            call SHOW_HEX
            sub bp, 2               ;next reg in stack
            pop cx

            dec cl                  ;dec counter
            
            mov di, START_OF_LINE   ;recover DI
            add di, WSCR_LENGTH     ;next line
            cmp cl, 00h
            jne print_regvalues

        pop bp                      ;recover BP (epilogue)
    ret
endp
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;Puts a frame by coords
;++++++++++++++++++++++++++++++++++++++++++++++++
;Entry: None
;Exit: None
;Expects: ES = start VideoMem
;Destroys: AX, BX,  CX, DL
;++++++++++++++++++++++++++++++++++++++++++++++++
SHOW_FRAME	proc

        mov bx, 0b800h                              ; ES = Videomem
        mov es, bx

        mov ax, cs
        mov ds, ax                                  ; DS:= code segment

		mov si, offset FRAME_STYLE				    ; src = frame style
		mov di, FRAME_DEST_ADR
		call SHOW_FLINE								; show head line
		xor bx, bx
		while_lines:
			add di, WSCR_LENGTH						; next line (enter)
			call SHOW_FLINE							; show body line
			sub si, 3								; symbols of body don't change
			inc bh									; inc counter
			cmp bh, FRAME_HEIGHT		
			jne while_lines							; if counter lines less than height of frame => repeat
		add di, WSCR_LENGTH							; next line (enter)
		add si, 3									; change symbols for last line
		call SHOW_FLINE								; show last line
	ret
endp
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;Puts a line of frame
;++++++++++++++++++++++++++++++++++++++++++++++++
;Entry: DI = shift, SI = adr or 3style
;Exit: None
;Expects: ES = start VideoMem
;Destroys: AX, CX
;++++++++++++++++++++++++++++++++++++++++++++++++
SHOW_FLINE	proc
		push di										; save di
		cld											; STOSW+
		lodsb										; get sym
		mov ah, COLOR									; fill the color
		stosw										; put sym to videomem
							; finish head of line
		lodsb										
		xor cx, cx
		mov cl, FRAME_LENGTH
		rep stosw									; put bodysym CL-times
							; finish body of line
		lodsb
		stosw
		;mov cl, ah									; CL = color
		pop di										; get original di
	ret
endp
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;Recovers Memory from Save_buf to Vmem
;++++++++++++++++++++++++++++++++++++++++++++++++
;Entry: DI = adr in Videomem, SI = adr in buf
;Exit: None
;Expects: ES = start VideoMem
;Destroys: AX, BX, CX
;++++++++++++++++++++++++++++++++++++++++++++++++
CPY_SAVE_VMEM	proc
        mov bx, 0b800h
        mov es, bx                                  ; ES = VideoMem

        mov ax, cs  
        mov ds, ax

        mov di, FRAME_DEST_ADR
        mov si, offset Save_buf
        xor bx, bx
        while_lines_cpy_bufvmem:
			call RECOVER_VMEMLINE					; show body line
            add di, WSCR_LENGTH						; next line (enter)
			inc bh								    ; inc counter
			cmp bh, FULL_FRAME_HEIGHT		
			jne while_lines_cpy_bufvmem				; if counter lines less than height of frame => repeat
	ret
endp
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;Copies from VideoMem to draw buffer
;++++++++++++++++++++++++++++++++++++++++++++++++
;Entry: DI = adr in Videomem, SI = adr in buf
;Exit: None
;Expects: ES = start VideoMem
;Destroys: AX, CX
;++++++++++++++++++++++++++++++++++++++++++++++++
CPY_VMEM_BUF	proc
        mov bx, 0b800h
        mov es, bx

        mov ax, cs  
        mov ds, ax

        mov di, FRAME_DEST_ADR
        mov si, offset Videomem_buf
        xor bx, bx
        while_lines_cpy_vmembuf:
			call CPY_FLINE							; copy current line
            add di, WSCR_LENGTH						; next line (enter)
			inc bh									; inc counter
			cmp bh, FULL_FRAME_HEIGHT		
			jne while_lines_cpy_vmembuf				; if counter lines less than height of frame => repeat
	ret
endp
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;Compares memory in bufs
;++++++++++++++++++++++++++++++++++++++++++++++++
;Entry: DI = shift, SI = adr or 3style
;Exit: None
;Expects: ES = start VideoMem
;Destroys: AX, BH,CX
;++++++++++++++++++++++++++++++++++++++++++++++++
CMP_VMEM_BUF	proc
        mov FOUND_DIF, 00h                          ; flag of difference down
        mov bx, 0b800h
        mov es, bx

        mov ax, cs  
        mov ds, ax                                  ; DS:= code segment

        mov di, FRAME_DEST_ADR
        mov si, offset Videomem_buf
        xor cx, cx
        while_lines_cmp_vmembuf:
			call CMP_FLINE							; show body line
            add di, WSCR_LENGTH						; next line (enter)
			inc ch									; inc counter
			cmp ch, FULL_FRAME_HEIGHT		
			jne while_lines_cmp_vmembuf				; if counter lines less than height of frame => repeat
	ret
endp
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;Compares one line of memory
;++++++++++++++++++++++++++++++++++++++++++++++++
;Entry: DI = adr in videomem, SI = adr in buf
;Exit: None
;Expects: ES = start VideoMem
;Destroys: AL, CL, DI, SI
;++++++++++++++++++++++++++++++++++++++++++++++++
RECOVER_VMEMLINE	proc
    mov cl, FRAME_LENGTH
    add cl, FRAME_LENGTH
    add cl, 4                                       ; CL = full length of frame
    push di
    for_syminline_recover_vmemline:
        mov al, ds:[si]
        mov byte ptr es:[di], al                    ; sym from buf to VMem

        inc di
        inc si                                      ; next sym
        dec cl
        cmp cl, 00h
        jne for_syminline_recover_vmemline
    pop di
	ret
endp
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;Compares one line of memory, saves in case of difference
;++++++++++++++++++++++++++++++++++++++++++++++++
;Entry: DI = adr in videomem, SI = adr in buf
;Exit: None
;Expects: ES = start VideoMem
;Destroys: AX, CX
;++++++++++++++++++++++++++++++++++++++++++++++++
CMP_FLINE	proc
    mov cl, FRAME_LENGTH
    add cl, 2
    push di
    for_syminline_cmp_fline:
        mov ax, ds:[si]
        mov bx, es:[di]                         ; Words to compare

        cmp ax, bx
        je dont_copy

        mov ds:[si+016ch], bx                   ; copy sym from VMem to Save_buf
        mov FOUND_DIF, 01h                      ; raise difference flag

        dont_copy:
        add di, 2                               ; next word
        add si, 2
        dec cl
        cmp cl, 00h
        jne for_syminline_cmp_fline
    pop di
	ret
endp
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;Copies one line of memory
;++++++++++++++++++++++++++++++++++++++++++++++++
;Entry: DI = adr in videomem, SI = adr in buf
;Exit: None
;Expects: ES = start VideoMem
;Destroys: AL, CL, DI, SI
;++++++++++++++++++++++++++++++++++++++++++++++++
CPY_FLINE	proc
    mov cl, FRAME_LENGTH
    add cl, FRAME_LENGTH
    add cl, 4
    push di
    for_syminline_cpy_fline:
        mov al, es:[di]
        mov byte ptr ds:[si], al

        inc di
        inc si
        dec cl
        cmp cl, 00h
        jne for_syminline_cpy_fline
    pop di
	ret
endp
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;Copy memory from VideoMem to Save buffer
;++++++++++++++++++++++++++++++++++++++++++++++++
;Entry: DI = adr in Videomem, SI = adr in buf
;Exit: None
;Expects: ES = start VideoMem
;Destroys: AX, BX, ES, SI, DI
;++++++++++++++++++++++++++++++++++++++++++++++++
CPY_VMEM_SAVE	proc
        ;push AX BX CX DX SI DI BP SP DS ES SS

        mov bx, 0b800h
        mov es, bx

        mov di, FRAME_DEST_ADR
        mov si, offset Save_buf
        xor bx, bx
        while_lines_cpy_vmemsave:
			call CPY_FLINE							; copy line
            add di, WSCR_LENGTH						; next line (enter)
			inc bh									; inc counter
			cmp bh, FULL_FRAME_HEIGHT		
			jne while_lines_cpy_vmemsave			; if counter lines less than height of frame => repeat

        ;pop ss es ds sp bp di si dx cx bx ax
	ret
endp
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

;.data
;Variables:
REG_SIGN    db 'AXBXCXDXSIDIBPSPDSESSSCS'
EQUALITY    db ' = 0000'

COLOR            db 34h
UPDATE_REGS      db 0h
FOUND_DIF        db 0h

WSCR_LENGTH   	 dw 160d
WSCR_HEIGHT 	 dw 50d
BSCR_LENGTH   	 db 160d
BSCR_HEIGHT 	 db 50d

FRAME_STYLE 	 db 213d, 205d, 184d, 179d, ' ', 179d, 212d, 205d, 190d

FRAME_DEST_ADR  dw 294d
REGS_DEST_ADR   dw 466d
FRAME_LENGTH    db 11d
FRAME_HEIGHT    db 12d
FULL_FRAME_HEIGHT    db 14d
SIZEOFBUF       dw 016ch

START_OF_LINE	 dw ?
PROG_DS          dw  ?

Videomem_buf    db 364d dup('v')
Save_buf        db 364d dup('s')

;-------------------------------------------------
;Variables:
;-------------------------------------------------


EOP:

end         Start