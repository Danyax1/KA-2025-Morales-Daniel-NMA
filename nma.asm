.model small
.stack 100h
.data
    filename    db "input.nma", 0   ; file to load
    file_handle dw 0                ; store file handle
    buffer      db 32000 dup(0)     ; buffer for symbols in file
    bytes_read  dw 0                ; stored symboles (later without comments)

.code
main proc
    mov ax, @data
    mov ds, ax

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
    mov bytes_read, ax  ; store bytes actually read

    ; close file
    mov ah, 3eh
    mov bx, file_handle
    int 21h

exit_prog:
    mov ax, 4c00h
    int 21h

main endp
end main

