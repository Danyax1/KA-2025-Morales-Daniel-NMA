.model small
org 100h
.data
    msg db '|||9||||||||||||||||||||'
    msg2 db '|0111111'
.code
main:
    mov ax, @data
    mov ds, ax
    mov cx, 2
    mov dx, 3
    mov si, offset msg2 ; Address of string 1
    mov di, offset msg ; Address of string 2
    call StrDelete



exit_program:
    ; Exit the program
    mov ax, 4C00h
    int 21h

MoveLeft proc 

; MoveLeft Move byte-block left (down) in memory

; Input:
; Si = address of source string (s1)
; di = address of destination string (s2)
; bx = index s1 (i1)
; dx = index s2 (i2)
; cx = number of bytes to move (count)

; Output:
; count bytes from s1[i1] moved to the location
; starting at s2[i2]
; Registers: none
    jcxz finish ; Exit if count = 0

    push cx ; Save modified registers
    push si
    push di

    add si, bx ; Index into source string
    add di, dx ; Index into destination string
    cld        ; Ensure auto-increment for si and di
    rep movsb  ; Move while cx <> 0
    rep movsb ; Move while cx <> @

    pop di ; Restore registers
    pop cx

    finish:
    ret ; Return to caller
    ret ; Return to caller
MoveLeft endp
MoveRight proc 
; MoveRight - Move byte-block right (up) in memory
; Input: (same as MoveLeft)  
; Output: (same as MoveLeft)  
; Registers: none  
    jcxz exit_MoveRight ; Exit if count = 0  
    push cx            ; Save modified registers  
    push di  
    push si  

    add si, bx         ; Index into source string  
    add di, dx         ; Index into destination string  
    add si, cx         ; Adjust to last source byte  
    dec si  
    add di, cx         ; Adjust to last destination byte  
    dec di  
    std                ; Auto-decrement si and di  
    rep movsb          ; Move while cx <> 0  

    pop si             ; Restore registers  
    pop di  
    pop cx  

exit_MoveRight:  
    ret                ; Return to caller  
MoveRight endp
StrLength proc  
    ; Count non-null characters in a string  
    ; Input:  
    ;   di = address of string (s)  
    ; Output:  
    ;   cx = number of non-null characters in s  
    ; Registers:  
    ;   cx  

    push ax          ; Save modified registers  
    push di  

    xor al, al       ; al <- search char (null)  
    mov cx, 0
countdown:
    cmp [di], 0h  
    jne increment
    jmp convert
increment:
    inc di
    inc cx
    jmp countdown
convert:

    pop di           ; Restore registers  
    pop ax  

    ret              ; Return to caller  

StrLength endp  

StrInsert proc  

    ; Insert characters from string s1 into string s2 at index i  
    ; Input:  
    ;   si = address of string 1 (s1)  
    ;   di = address of string 2 (s2)  
    ;   dx = insertion index for s2 (i)  
    ; Output:  
    ;   chars from string s1 inserted at s2[i]  
    ;   s1 not changed  
    ; Registers:  
    ;   none  

    push ax          ; Save modified registers  
    push bx  
    push cx  

    ; ax = LenInsertion  
    ; cx = CharsToMove  
    xchg si, di      ; Exchange si and di  
    call StrLength   ; Find length of s1  
    xchg si, di      ; Restore si and di  
    mov ax, cx       ; Save length(s1) in ax  

    call StrLength   ; Find length of s2  
    sub cx, dx       ; cx = (CharsToMove)  
    inc cx           ; Adjust cx for insertion  

    ; bx = si index  
    push dx          ; Save index (dx) and si  
    push si  

    mov si, di       ; Make si and di address s2  
    mov bx, dx       ; Set s1 index to dx (insertion index)  
    add dx, ax       ; Set s2 index to LenInsertion  

    call MoveRight   ; Open a hole for the insertion  

    pop si           ; Restore index (dx) and si  
    pop dx  

    xor bx, bx       ; Clear bx  
    mov cx, ax       ; Set cx to LenInsertion  

    call MoveLeft    ; Insert s1 into hole in s2  

    pop cx           ; Restore registers  
    pop bx  
    pop ax  

    ret              ; Return to caller  
StrInsert endp  
StrDelete proc  
    ; Delete characters anywhere in a string  
    ; Input:  
    ;   di = address of string (s)  
    ;   dx = index (i) of first character to delete  
    ;   cx = number of characters to delete (n)  
    ; Output:  
    ;   n characters deleted from string at s[i]  
    ; Note:  
    ;   Prevents deleting past the end of the string  
    ; Registers:  
    ;   none  

    push bx          ; Save modified registers  
    push cx  
    push di  
    push si  

    mov bx, dx      ; Assign string index to bx  
    add dx, cx      ; Source index <- index + count  
    call StrLength  ; cx <- length(s)  
    cmp cx, bx      ; Is length > index?  
    ja delete_chars     ; If yes, jump to delete chars  

    ; If index is beyond string length, truncate string  
    add di, dx      ; Calculate index to string end  
    mov [byte ptr di], 0  ; Insert null character  
    jmp exit_delete ; Jump to exit  

delete_chars:  
    mov si, di      ; Set source = destination  
    sub cx, bx      ; CharsToMove <- Len - SourceIndex  
    inc cx          ; Plus one for null at end of string  
    call MoveLeft   ; Move chars over deleted portion  

exit_delete:  
    pop si          ; Restore registers  
    pop di  
    pop cx  
    pop bx  
    ret             ; Return to caller  

StrDelete endp  
end main