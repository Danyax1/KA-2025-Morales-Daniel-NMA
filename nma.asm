.model small
.stack 100h
.data
    filename    db "input.nma", 0   ; file to load
    file_handle dw 0                ; store file handle
    bytes_read  dw 0                ; Number of bytes read
    descr_len   dw 0                ; len for description
    expr_len    dw 0                ; len for expression
    rule_count  dw 0                ; count for rules
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


exit_prog:
    mov ax, 4c00h
    int 21h

main endp

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

skip_check:
    inc si                  ; move to next byte

    check_overflow:
        cmp si, 32000                               ; Prevent overflow
        jae end_count                               ; Stop if at end of buffer
    
    jmp count_loop         

end_count:
    mov [rule_count], cx    ; store rule count
    ret
count_rules endp

end main
