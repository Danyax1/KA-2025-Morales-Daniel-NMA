.model small
.stack 100h
.data
    filename    db "input.nma", 0   ; file to load
    file_handle dw 0                ; store file handle
    bytes_read  dw 0                ; Number of bytes read
    descr_len   dd 0                ; len for description
    expr_len    dd 0                ; len for expression
    rule_len    dd 0                ; len for rules
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
    call skip_description
    ; 2) read input expression length
    call read_len_expr


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
    mov dx, word ptr buffer+2 
    mov word ptr [descr_len], dx   ; store description length (little-endian)
    mov word ptr [descr_len+2], ax 

    add ax, 4                      ; adjust for the 4-byte length field
    add ax, offset buffer          ; compute start of input expr
    mov si, ax                     ; si points to input expr

    ret

skip_description endp

read_len_expr proc
    mov ax, word ptr [si]          ; read length
    mov dx, word ptr [si+2]        ;
    mov word ptr [expr_len], dx
    mov word ptr [expr_len+2], ax

    add si, 4                      ; skip the 4 bytes that are describing the lenght
    mov di, offset expr_buffer     ; prepare to write to the expr_buffer

    ret
read_len_expr endp

end main
