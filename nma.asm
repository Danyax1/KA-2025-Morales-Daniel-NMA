.model small
org 100h
.data
    filename    db "input.nma", 0   ; file to load
    file_handle dw 0                ; store file handle
    bytes_read  dw 0                ; Number of bytes read
    descr_len   dw 0                ; len for description
    expr_len    dw 0                ; len for expression
    rule_count  dw 0                ; count for rules
    rule_index  dw 1h                ; what rule is being executed(default 1)
    rule_buffer db 128 dup(0)       ; buffer for extracted left
    rule_buffer_r db 128 dup(0)     ; buffer for extracted Rright
    buffer      db 32000 dup(0)     ; buffer for symbols in file
    expr_buffer db 32768 dup(0)     ; buffer for input string

.code
main proc
    mov ax, @data
    mov ds, ax
    ; read the file into buffer
    call read_file  

    ; the following steps are for preparing data for execution
    ; i need to store the len of expr to check if it is not too big
    ; i will place string the last in order to not overwrite any other data

    ; 1) skip description
    xor ax, ax
    xor dx, dx
    
    call skip_description
    ; 2) read input expression length
    xor ax, ax
    xor dx, dx

    call read_len_expr
    ; 3): сopy input string without last 2 bytes
    xor ax, ax
    xor dx, dx

    call copy_expr
    ; 4) count the amount of rules
    xor ax, ax
    xor dx, dx

    call count_rules
    
    ;Everything is ready for execution
    ;Execution process consists of the following steps
    ;1) read the left part of 1st rule
    ;2) go and compare strings
    ;   2.1 if the match was not found by the and of the string, then go to 2nd rule and so on
    ;       2.1.1 if rule's number == rule count => finish aprogram
    ;3) if match was found, we do the following
    ;   3.1 calculate the difference between left and right parts of the rule
    ;   3.2 update the expr_len value
    ;       3.2.1 check for overflow
    ;       3.2.2 if OF then don't do any substitutions and end program
    ;   3.3 go to the 1st rule


    ; rule_buffer - left part (to be replaced)
    ; rule_buffer - new right part (to be inserted)
rule_caller:
    mov cx, [rule_index]
    cmp cx, 0FFFFh      ; only appear if last rule was final (l => r.)
    je exit_prog
    cmp cx, [rule_count]
    ja exit_prog        ; if we ran out of rules
    call getRule


is_substring: 
    mov si, offset rule_buffer
    mov di, offset expr_buffer
    ;   si = address of substring to find  
    ;   di = address of target string to scan 
    call StrPos

    jnz next_rule           ; if there is no substring => next rule

    mov di, offset rule_index
    mov ax, [di]
    inc ax
    mov [di], ax        ; if not a substring => go to the next rule
    jmp exit_prog


next_rule:
    mov di, offset rule_buffer
    call StrNull
    mov di, offset rule_buffer_r
    call StrNull
    mov di, offset rule_index
    mov dx, [di]
    inc dx
    mov [di], dx
    jmp rule_caller
    
exit_prog:
    mov ax, 4c00h
    int 21h

main endp
; the following procs are for preparing execution

read_file proc
    ; open file
    mov ah, 3dh
    mov dx, offset filename
    mov al, 0          ; read-only mode
    int 21h
    mov file_handle, ax  ; save file handle

    ; read file
    mov ah, 3fh
    mov bx, file_handle
    mov dx, offset buffer ; buffer to store data
    mov cx, 32000        ; up to 32 000 bytes
    int 21h
    mov bytes_read, ax      ; store number of bytes

    ; close file
    mov ah, 3eh
    mov bx, file_handle
    int 21h
    ret
read_file endp

skip_description proc
    mov ax, word ptr buffer     
    mov [descr_len], ax ; store description length (little-endian)

    add ax, 4                      ; adjust for the 4-byte length field
    add ax, offset buffer          ; compute start of input expr
    mov si, ax                     ; si points to input expr

    ret

skip_description endp

read_len_expr proc
    mov ax, [si]          ; read length
    mov [expr_len], ax

    add si, 4                      ; skip the 4 bytes that are describing the lenght
    mov di, offset expr_buffer     ; prepare to write to the expr_buffer

    ret
read_len_expr endp

copy_expr proc
    mov cx, [expr_len]    ; cx = len of expr
    sub cx, 2                      ; without last 2 bytes (0D0A)

copy_loop:
    mov al, [si]                   ; load byte from input buffer
    mov [di], al                   ; store into expr_buffer
    inc si
    inc di
    dec cx
    jnz copy_loop

    ret
copy_expr endp

count_rules proc
    ; find the start of the rule section
    mov ax, [descr_len]
    add ax, [expr_len]      
    add ax, 12              ; skip expr/descr/rule length field
    add ax, offset buffer   ; Compute start of rules section
    mov si, ax              ; si points to rules

    xor cx, cx              ; clear cx

count_loop:
    mov al, [si]            ; read a byte
    cmp al, 0dh             ; check for 0d
    jne skip_check
    cmp byte ptr [si+1], 0ah ; check 0a
    jne skip_check

    inc cx                  ; Found a rule separator, increase count
    
    inc si
    inc si
    cmp [si], 00h
    je end_count
    dec si

skip_check:
    inc si                  ; move to next byte
    jmp count_loop         

end_count:
    mov [rule_count], cx    ; store rule count
    ret
count_rules endp

;the following procs are for executing
getRule proc
    rule_fetching:

    xor ax, ax
    mov ax, [descr_len]
    add ax, [expr_len]      
    add ax, 12              ; skip expr/descr/rule length field
    add ax, offset buffer   ; Compute start of rules section
    mov si, ax              ; si points to rules
    find_rule:
        dec cx              ;  start to count the rules, until needed
        cmp cx, 0
        je precopy_to_buffer ; if found the rule
        ; go to the next rule
        check_0a:
            inc si          
            mov al, [si]        
            cmp al, 0ah     ;if 0a found - next rule
            jne check_0a
        jmp find_rule

    precopy_to_buffer:
        mov al, [si]
        cmp al, 0ah ; if not 1st rule we'll stand at tab
        jne copy_to_buffer
        inc si

    copy_to_buffer:
        mov di, offset rule_buffer
        mov cx, 2           ; find second tab
        start_process:
            mov al, [si]
            cmp al, 09          ; tab found
            jne write_to_buffer
            dec cx
            cmp cx, 0           ; if 2nd tab found -finish
            je finish_rule

            inc si                       ; skip tab
            mov di, offset rule_buffer_r ; now copiing second part of the rule to the other buffer

            write_to_buffer:
                mov al, [si]                   ; load byte from input buffer
                mov [di], al                   ; store into expr_buffer
                inc si
                inc di
                jmp start_process
        finish_rule:
            ret
getRule endp

;the following procs are for executing (copied from the book)

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
    add di, dx ; Index into destination string 81: cld ; Auto-increment si and di
    rep movsb ; Move while cx <> @

    pop di ; Restore registers
    pop si
    pop cx
    finish:
    ret ; Return to caller
MoveLeft endp 

MoveRight proc 
; MoveRight - Move byte-block right (up) in memory
; Input: (same as MoveLeft)  
; Output: (same as MoveLeft)  
; Registers: none  
    jcxz @@exit        ; Exit if count = 0  
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

@@exit:  
    ret                ; Return to caller  

MoveRight endp 

StrNull proc  
    ; Erase all characters in a string  
    ; Input:  
    ;   di = address of string (s)  
    ; Output:  
    ;   s[0] <- null character (ASCII 0)  
    ; Registers:  
    ;   dx, cx 
    push dx
    push cx

    mov cx, 64      ;max len
erase:
    dec cx
    cmp cx, 0       ; check step
    je pop_register

    mov [di], 0  ; Insert null character at s[0]
    add di, 2       ; go to the next word
    mov dx, [di]
    cmp dx, 0
    jne erase

pop_register:
    pop cx
    pop dx
    ret                   ; Return to caller  

StrNull endp  

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

StrCompare proc  
    ; Compare two strings  
    ; Input:  
    ;   si = address of string 1 (s1) - substring
    ;   di = address of string 2 (s2) - main string
    ; Output:  
    ;   Flags set for conditional jumps: jb, jbe, je, ja, jae  
    ; Registers:  
    ;   None  

    push ax          ; Save modified registers  
    push di  
    push si    

compare:  
    mov al, [si]           ; Load byte from s1 into al, advance si  
    cmp al, [di]            ; compare with di, advance di
    jne done      ; Exit if non-equal chars found  
    or al, al       ; Check if al = 0 (end of s1)  
    jne incrementation   ; If not null, continue comparing  
    jmp done

incrementation:
    inc si
    inc di
    jmp compare

done:  
    pop si          ; Restore registers  
    pop di  
    pop ax  

    ret             ; Return flags to caller  

StrCompare endp  

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
    ja @@delete     ; If yes, jump to delete chars  

    ; If index is beyond string length, truncate string  
    add di, dx      ; Calculate index to string end  
    mov [byte ptr di], 0  ; Insert null character  
    jmp short exit ; Jump to exit  

@@delete:  
    mov si, di      ; Set source = destination  
    sub cx, bx      ; CharsToMove <- Len - SourceIndex  
    inc cx          ; Plus one for null at end of string  
    call MoveLeft   ; Move chars over deleted portion  

exit:  
    pop si          ; Restore registers  
    pop di  
    pop cx  
    pop bx  
    ret             ; Return to caller  

StrDelete endp  

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

StrConcat proc  
    ; Concatenate string s1 to the end of string s2  
    ; Input:  
    ;   si = address of source string (s1)  
    ;   di = address of destination string (s2)  
    ; Output:  
    ;   chars from s1 added to the end of s2  
    ; Registers:  
    ;   none  

    push bx          ; Save modified registers  
    push cx  
    push dx  

    ; Find the length of s2  
    call StrLength   ; Length of s2 is in cx  
    mov dx, cx       ; Save the length of s2 in dx  
    xchg si, di      ; Swap si and di  
    call StrLength   ; Find the length of s1 (cx)  
    inc cx           ; Increment cx to account for the null terminator  
    xchg si, di      ; Restore si and di  

    xor bx, bx       ; Clear bx for the move operation  
    call MoveLeft    ; Move s2 to make space for s1  

    pop dx           ; Restore registers  
    pop cx  
    pop bx  

    ret              ; Return to caller  

StrConcat endp

StrCopy proc  
    ; Copy one string to another  
    ; Input:  
    ;   si = address of source string (s1)  
    ;   di = address of destination string (s2)  
    ; Output:  
    ;   Chars in si copied to s2  
    ; Note:  
    ;   s2 must be at least as large as s1  
    ; Registers:  
    ;   none  

    push bx          ; Save modified registers  
    push cx  
    push dx  

    xchg si, di      ; Exchange si and di, so we can use di as the source and si as the destination  
    call StrLength   ; Call StrLength to determine the length of the source string (s1)  
    inc cx           ; Increment cx to include space for the null terminator  
    xchg si, di      ; Restore original values of si and di  

    xor bx, bx       ; Clear bx (used for movement)  
    xor dx, dx       ; Clear dx (used for addressing)  
    call MoveLeft    ; Move the characters from si to di  

    pop dx           ; Restore registers  
    pop cx  
    pop bx  

    ret              ; Return to caller  

StrCopy endp  

StrPos proc  
    ; Search for the position of a substring in a string  
    ; Input:  
    ;   si = address of substring to find  
    ;   di = address of target string to scan  
    ; Output:  
    ;   if zf = 1, then dx = index of substring  
    ;   if zf = 0, then substring was not found  
    ;   Note: dx is meaningless if zf = 0  
    ; Registers:  
    ;   dx  

    push ax          ; Save modified registers  
    push bx  
    push cx  
    push di  

    call StrLength   ; Find length of target string (s2)  
    mov ax, cx       ; Save length of target string in ax  

    xchg si, di      ; Swap si and di (now si = target string, di = substring)  
    call StrLength   ; Find length of substring (s1)  
    mov bx, cx       ; Save length of substring in bx  

    xchg si, di      ; Restore si and di (si = substring, di = target string)  
    sub ax, bx       ; ax = last possible starting index in the target string  
    jb not_found     ; Exit if target string is shorter than substring  

    mov dx, 0FFFFh   ; Initialize dx to -1 (default: substring not found)  

search_loop:  
    inc dx             ; Increment index (dx)  
    mov cl, [di + bx]  ; Save character at s[bx] in cl  
    mov byte ptr [byte ptr di + bx], 0   ; Temporarily replace character with null terminator  
    call StrCompare    ; Compare substring (si) with the modified target string (di)  
    mov [byte ptr di + bx], cl  ; Restore replaced character  

    je found          ; If match found, jump to found (zf=1, dx=index)  

    inc di           ; Otherwise, move to the next character in the target string  
    cmp dx, ax       ; Have we reached the last possible position?  
    jne search_loop  ; If not, continue search  

not_found:  
    xor cx, cx       ; Reset zf to 0 (substring not found)  
    inc cx           ; Indicate failure by setting cx to 1  

found:  
    pop di           ; Restore registers  
    pop cx  
    pop bx  
    pop ax  

    ret              ; Return to caller  

StrPos endp  

end main
