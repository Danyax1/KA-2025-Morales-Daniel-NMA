.model tiny

.code
org 100h
main proc
    call read_filename  ; read the filename from command line
    ; read the file into buffer
    call read_file  

    call locate_expr  ; find the location of the expression in the buffer

    ; 1) skip description
    
    call skip_description
    ; 2) read input expression length

    call read_len_expr
    ; 2.1) store the static length of the expression
    mov [expr_len_static], ax
    ; 3): сopy input string without last 2 bytes

    call copy_expr
    ; 4) count the amount of rules

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
    cmp cx, 0affh      ; only appear if last rule was final (l => r.)
    jae printing_expr                                        ;                               !!!(print expr)
    cmp cx, [rule_count]
    ja printing_expr        ; if we ran out of rules                                        !!!(print expr)
    call getRule


is_substring: 
    mov si, offset rule_buffer
    mov di, [location_expr]
    ;   si = address of substring to find  
    ;   di = address of target string to scan 
    call StrPos

    jnz next_rule           ; if there is no substring => next rule

    jmp calcLenExpr           


next_rule:
    call delPreviousRule
    jmp rule_caller

calcLenExpr:
    call calcLenExpression

substitution:

    ; substitute the left part of the rule with the right part of the rule
    mov di, offset rule_buffer
    call StrLength          ; get length of left part of the rule (di - pointer; cx - length)
    push cx
    mov di, [location_expr]
go_to_symbol:
    add di, dx          ; go to the symbol where we need to del symbols
    cmp cx, 0
    je create_hole_for_inserting
    delete_symbols:
        mov byte ptr [di], 1           ; load byte from input buffer
        inc di
        dec cx
        cmp cx, 0
        jne delete_symbols
        jmp create_hole_for_inserting

create_hole_for_inserting:
    pop cx
    call Create_hole                                                       

inserting:
    ; insert the right part of the rule into the hole
    mov di, offset rule_buffer_r
    mov si, [location_expr]
    ; go to the place [si] to insert the right part of the rule
moving_to_hole:
    mov al, byte ptr [si]           ; load byte from expr
    inc si
    cmp al, 1                       ; check for 1 (start of the hole)
    jne moving_to_hole
    dec si
swapping_inserting: 
    mov al, byte ptr [di]         ; load byte from rule_buffer_r
    mov ah, byte ptr [si]         ; load next byte from input buffer
    cmp ah, 1
    jne set_up_rule
    xchg al, ah
    mov byte ptr [di], al         ; perform swap
    mov byte ptr [si], ah         ; perform swap
    inc si
    inc di
    jmp swapping_inserting

set_up_rule:
    mov di, offset rule_index
    cmp [di], 0affh             ; check if it is the last rule
    jae printing_expr
    mov [di], 0
    jmp next_rule
printing_expr:
    mov di, [location_expr]
    printing_cycle:
        mov al, byte ptr [di]         ; load byte from expr
        cmp al, 0
        je add_symbols
        inc di
        jmp printing_cycle
add_symbols:
    mov byte ptr [di], '$'         ; add '$' to the end of the string
    mov dx, [location_expr]
    mov ah, 09h
    int 21h
exit_prog:
    mov ax, 4c00h
    int 21h

main endp
; the following procs are for preparing execution
read_filename proc
    mov si, 81h          ; Command-line arguments start at 81h
    mov di, offset filename
    mov cx, 20           ; Max filename length (20 bytes)

read_loop:
    lodsb                ; Load a byte from the command-line arguments into AL
    cmp al, 0Dh          ; Check for end of arguments (carriage return)
    je done_reading
    cmp al, 20h          ; check for space
    je read_loop
    stosb                ; Store the byte in the filename buffer
    loop read_loop       ; Continue reading until CX reaches 0

done_reading:
    mov byte ptr [di], 0 ; Null-terminate the filename
    ret
read_filename endp

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
    mov cx, 3200        ; up to 32 000 bytes
    int 21h
    mov bytes_read, ax      ; store number of bytes

    ; close file
    mov ah, 3eh
    mov bx, file_handle
    int 21h
    ret
read_file endp

locate_expr proc
    ; find the end of prog buffer 
    mov ax, offset buffer
    add ax, [bytes_read]
    inc ax
    inc ax
    mov di, offset location_expr
    mov [di], ax         ; store the location of the future expression
    ret
locate_expr endp

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
    mov di, [location_expr]     ; prepare to write to the expr_buffer

    ret
read_len_expr endp

copy_expr proc
    mov cx, [expr_len]    ; cx = len of expr

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
    mov [rule_index], 1h    ; set rule index to 1
    ret
count_rules endp

;the following procs are for executing
getRule proc
    rule_fetching:

    xor ax, ax
    mov ax, offset buffer
    add ax, 12d             ; skip expr/descr/rule length field + 0D0A
    add ax, [descr_len]   ; Compute start of rules section
    add ax, [expr_len_static]      ; skip expr length field

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
            jmp start_process
            write_to_buffer:
                mov al, [si]                   ; load byte from input buffer
                cmp al, 2Eh                    ; check for 2E (dot)
                je last_rule
                mov [di], al                   ; store into expr_buffer
                inc si
                inc di
                jmp start_process
        finish_rule:
            ret
        last_rule:
            cmp cx, 1
            jne force_write_dot
            inc si
            cmp byte ptr [si], 09h
            je overwrite_rule_count
            dec si
            dec si
            cmp byte ptr [si], 09h
            jne restore_pos_1
            inc si
            inc si
        overwrite_rule_count:
            add [rule_index], 0affh    ; set rule index to large_number (last rule)
            jmp start_process
        force_write_dot:
            mov al, 2Eh
            mov [di], al                   ; store into expr_buffer
            inc si
            inc di
            jmp start_process
        restore_pos_1:
            inc si
            jmp force_write_dot
getRule endp

delPreviousRule proc
    mov di, offset rule_buffer
    call StrNull                ; del left part

    mov di, offset rule_buffer_r
    call StrNull                ; del right part
    starting_lol:
        mov di, offset rule_index
        cmp [di], 0affh             ; check if it is the last rule
        jae sub_processing_rule
        mov dx, [di]                ; get the current rule
        inc dx                      ; go to next rule (+1)
        mov [di], dx                ; store the next rule
        jmp resigh_from
    resigh_from:
        ret
    sub_processing_rule:
        sub [di], 0affh             ; set rule index to latest rule
        jmp starting_lol
delPreviousRule endp

calcLenExpression proc
    ; calculate the difference between left and right parts of the rule
    ; update the expr_len value
    ; check for overflow
    ; if OF then don't do any substitutions and end program


    mov di, offset rule_buffer
    call StrLength          ; get length of left part of the rule (di - pointer; cx - length)
    mov bx, cx              ; store it in bx

    mov di, offset rule_buffer_r
    xor cx, cx               ; clear cx
    call StrLength          ; get length of right part of the rule (di - pointer; cx - length)
    sub cx, bx              ; calculate difference between left and right parts of the rule

    add expr_len, cx      ; update expr_len value

    cmp expr_len, 32768d  ; check for overflow
    ;ja exit_prog            ; if overflow => exit program                                       !!!(print expr)

    ret
calcLenExpression endp

Create_hole proc
    mov bx, cx  ; store it in bx
    mov di, offset rule_buffer_r
    call StrLength          ; get length of right part of the rule (di - pointer; cx - length)
    sub cx, bx              ; calculate difference between left and right parts of the rule
    
    jz finish_ret                                                                                    ;!!!(insert right part of the rule)
    js move_left         ; if l>r => move left
    move_right:         ; if l<r => move right
        push cx
        mov di, [location_expr]
        move_to_the_end_of_expr:
            
            mov al, [di]         ; load byte from input buffer
            cmp al, 0           ; check for 0
            je start_r_swap  
            inc di                                                                          
            jmp move_to_the_end_of_expr 
        start_r_swap:
            inc byte ptr [di]           ; change byte 00 to 01
            pop cx
            swapping:
                jcxz almost_finish_ret                                                      
                dec cx
                one_cycle:
                    mov al, byte ptr [di]         ; load byte from input buffer
                    mov ah, byte ptr [di-1]         ; load previous byte from input buffer
                    cmp ah, 01
                    jbe preperation
                    xchg al, ah
                    mov byte ptr [di], al         ; peform swap
                    mov byte ptr [di-1], ah         ; perform swap
                    dec di
                    jmp one_cycle
                preperation:
                    push cx
                    movement_right:
                        inc di
                        mov al, byte ptr [di]         ; load byte from input buffer
                        cmp al, 0
                        je movement_right
                        jmp move_to_the_end_of_expr
            jmp finish_ret                                       ;!!!(insert right part of the rule)
        almost_finish_ret:
            dec byte ptr [di]         ; change last byte 01 to 00
            jmp finish_ret
    move_left:
        neg cx
        inc cx
        push cx
        mov di, [location_expr]
        move_to_last_clear_symbol:
            mov al, byte ptr [di]         ; load byte from input buffer
            cmp al, 1
            je find_end
            inc di
            jmp move_to_last_clear_symbol  
        find_end:
            inc di
            mov al, byte ptr [di]         ; load byte from input buffer
            cmp al, 1
            jne swapping_left
            jmp find_end
        swapping_left:
            pop cx
            dec cx
            one_cycle_left:
                jcxz finish_ret                                                      ;!!!(insert left part of the rule)
                
                mov al, byte ptr [di-1]         ; load byte from input buffer
                mov ah, byte ptr [di]         ; load next byte from input buffer
                cmp ah, 0
                je preperation_left
                xchg al, ah
                mov byte ptr [di-1], al         ; peform swap
                mov byte ptr [di], ah         ; perform swap
                inc di
                jmp one_cycle_left
            preperation_left:
                push cx
                movement_left:
                    dec di
                    mov byte ptr [di], 0         ;change byte 01 to 00
                    mov di, [location_expr]
                    jmp move_to_last_clear_symbol 
    finish_ret:
        ret
Create_hole endp
;the following procs are for executing (copied from the book)

StrNull proc  
    push dx
    push cx

    mov cx, 128      ;max len
erase:
    dec cx
    cmp cx, 0       ; check step
    je pop_register

    mov byte ptr [di], 0    ; Insert null character at s[0]
    inc di                  ; go to the next bytr
    mov dx, [di]
    cmp dx, 0
    jne erase

pop_register:
    pop cx
    pop dx
    ret                   ; Return to caller  

StrNull endp  

StrLength proc  

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

StrPos proc  

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

filename    db 20 dup(0)   ; file to load
file_handle dw 0                ; store file handle
bytes_read  dw 0                ; Number of bytes read

location_expr dw 0                ; location of expression in program

descr_len   dw 0                ; len for description
expr_len    dw 0                ; len for expression
expr_len_static    dw 0         ; starting len for expression
rule_count  dw 0                ; count for rules
rule_index  dw 1h                ; what rule is being executed(default 1)

rule_buffer db 128 dup(0)       ; buffer for extracted left
rule_buffer_r db 128 dup(0)     ; buffer for extracted Rright

buffer      db 3200 dup(0)     ; buffer for symbols in file

end main
