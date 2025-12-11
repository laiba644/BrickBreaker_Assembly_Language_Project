[org 0x0100]
jmp start
start_game: db 0
oldisr: dd 0
colume: dw 0
row: dw 0
incC: db 0
incR: db 0
previous: dw 0
tickcount: db 0
left_edge: dw 3524
right_edge: dw 3652
right_: dw 0
left_: dw 0
pre_stack_pos: dw 3580
second: dw 0
clock: db 0
bonus: dw 0
bricks_start_location: dw 810 , 828 , 846 , 864 , 882 , 900 , 918 , 936 , 1290 , 1308 , 1326 , 1344 , 1362, 1380, 1398 , 1416 , 1770 , 1788 , 1806 , 1824 , 1842 , 1860 , 1878 , 1896 ,  2250 , 2268 , 2286 , 2304 , 2322 , 2340 , 2358 , 2376
bricks_end_location:   dw 822 , 840 , 858 , 876 , 894 , 912 , 930 , 948 , 1302 , 1320 , 1338 , 1356 , 1374 , 1392 , 1414 , 1428 , 1782 , 1800  , 1818 , 1836 ,1854 , 1872 , 1890 , 1908 , 2260 , 2278 , 2296 , 2314 , 2332 , 2350 , 2368 , 2386
score: dw 0
total_bricks: dw 32
calculated_location:  dw 0
left_limit dw 0
right_limit dw 0
mid dw 0
highscore_filename db 'HISCORE.DAT',0
highscore_value   dw 0       ; file se read / file me write
left_or_right: db 0
preBall:dw 0
live: db 3
end_of_game: dw 0
StayOnStacker: db 0
counter: dw 0
solid: db 0
solid1: db 0
current_attr: db 0xf0
Win_str:db 'YOU WIN!',0
Lose_str: db 'YOU_LOSE',0
Score_str: db 'SCORE',0
Lives_str: db 'LIVES',0
;last menu 
welcome_str: db 'BRICK BREAKER By Laiba Kashif and Noor Ul Ain',0
play_str: db 'PRESS enter TO PLAY GAME',0
total_score_str: db 'YOUR TOTAL SCORES :',0
exit_str: db 'PRESS esc TO EXIT',0
quit_str: db 'PRESS esc TO QUIT GAME',0
restart_str: db 'PRESS R TO RESTART YOUR GAME',0
; NEW MENU STRINGS
welcome_len: dw 14
subtitle_str: db 'BrickBreaker Game By Laiba and Noor Ul Ain',0
subtitle_len: dw 19
menu_title_str: db 'MAIN MENU ',0
menu_title_len: dw 9
option1_str: db '1) START GAME',0
option2_str: db '2) HOW TO PLAY',0
option3_str: db '3) HIGH SCORES',0
option4_str: db '4) GAME INFO',0
exit_menu_str: db 'E) EXIT',0
prompt_str: db 'Select an option and press key...',0
current_screen: db 0
; EXTRA TEXT SCREENS
how_title:        db 'HOW TO PLAY',0
how1:             db 'Use LEFT and RIGHT arrow keys to move and enter to start.',0
how2:             db 'Press SPACE to release the ball.',0
how3:             db 'Break all bricks to win.',0
how_back:         db 'Press B to go back',0
score_info1: db 'RED bricks give 5 points....',0
score_info2: db 'ORANGE bricks give 3 points....',0
score_info3: db 'PINK bricks give 2 points.........',0
score_info4: db 'GREEN bricks give 1 point....',0

info_title:       db 'GAME INFO',0
info1:            db 'BRICK BREAKER made by Laiba & Noor',0
info2:            db '8086 Assembly - TEXT MODE',0
info3:            db 'Developed with Hard Work',0
info_back:        db 'Press B to go back',0
high_title:       db 'HIGH SCORES',0
high1:            db 'Your current score is:',0
high_back:        db 'Press B to go back',0
temp_score:       dw 0
; CENTERED STRING PRINTER
;  bp+4 = row
;  bp+6 = string address
print_center:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push es
    mov ax, 0xb800
    mov es, ax
    mov ax, [bp+4]       ; row
    mov bx, 80
    mul bx               ; ax = row * 80
    shl ax, 1            ; *2 for bytes
    mov di, ax           ; base offset for this row
    mov si, [bp+6]       ; string pointer
    ; length in CX
    mov cx, 0
.pc_len_loop:
    mov bx, si
add bx, cx
mov al, [bx]
    cmp al, 0
    je .pc_len_done
    inc cx
    jmp .pc_len_loop
.pc_len_done:
    ; col = (80 - len)/2
    mov ax, 80
    sub ax, cx
    shr ax, 1           ; divide by 2
    mov dx, ax
    shl dx, 1           ; *2 bytes
    add di, dx
    mov ah, [current_attr]
.pc_draw:
    mov al, [si]
    cmp al, 0
    je .pc_done
    mov [es:di], ax
    inc si
    add di, 2
    jmp .pc_draw
.pc_done:
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 4
; MAIN MENU SCREEN
main_menu:
    call clrscr
    push ax
    push es
    push di
    mov ax,0B800h
    mov es,ax
    mov di,0
    mov ax,0720h
    mov cx,80
    rep stosw
    pop di
    pop es
    pop ax
    call border
    ; Subtitle
mov byte [current_attr], 0x0B
    push subtitle_str
    push 5
    call print_center
    ; Menu Heading
mov byte [current_attr], 0x0F
    push menu_title_str
    push 8
    call print_center
    ; Options
mov byte [current_attr], 0x0A
    push option1_str
    push 11
    call print_center
mov byte [current_attr], 0x0B
    push option2_str
    push 13
    call print_center
mov byte [current_attr], 0x0D
    push option3_str
    push 15
    call print_center
mov byte [current_attr], 0x0C
    push option4_str
    push 17
    call print_center
 mov byte [current_attr], 0x0E
    push exit_menu_str
    push 19
    call print_center
     ; Prompt
    mov byte [current_attr], 0x07
    push prompt_str
    push 22
    call print_center
; Score info lines (row 23/24 ke upar)
    mov byte [current_attr], 0x04     ; dark red
    push score_info1
    push 20
    call print_center
    mov byte [current_attr], 0x06     ; brown/orange
    push score_info2
    push 21
    call print_center
    mov byte [current_attr], 0x0D     ; pink/magenta
    push score_info3
    push 22
    call print_center
    mov byte [current_attr], 0x0A     ; green
    push score_info4
    push 23
    call print_center
    mov byte [current_screen], 0
    ret
; HOW TO PLAY SCREEN
how_screen:
    call clrscr
 mov byte [current_attr], 0x0E
    push how_title
    push 4
    call print_center
 mov byte [current_attr], 0x0B
    push how1
    push 8
    call print_center
 mov byte [current_attr], 0x0A
    push how2
    push 10
    call print_center
 mov byte [current_attr], 0x0D
    push how3
    push 12
    call print_center
 mov byte [current_attr], 0x07
    push how_back
    push 22
    call print_center
.how_waitkey:
    in al, 0x60
    cmp al, 0x30       ; 'B'
    jne .how_waitkey
    ret
; INFO SCREEN
info_screen:
    call clrscr
mov byte [current_attr], 0x0E
    push info_title
    push 4
    call print_center
 mov byte [current_attr], 0x0B
    push info1
    push 9
    call print_center
 mov byte [current_attr], 0x0A
    push info2
    push 11
    call print_center
 mov byte [current_attr], 0x0D
    push info3
    push 13
    call print_center
mov byte [current_attr], 0x07
    push info_back
    push 22
    call print_center
.info_waitkey:
    in al, 0x60
    cmp al, 0x30
    jne .info_waitkey
    ret
high_screen:
    call clrscr
    ; title
    mov byte [current_attr], 0x0E
    push high_title
    push 4
    call print_center
    ; text "Your current score is:"
    mov byte [current_attr], 0x0B
    push high1
    push 9
    call print_center
    ; row = 11, col = 35 (thoda center)
    push ax
    push bx
    push es
    push di
    mov ax,0B800h
    mov es,ax
    mov ax,11          ; row
    mov bx,80
    mul bx             ; AX = 11*80
    add ax,35          ; col
    shl ax,1           ; *2 bytes
    mov di,ax          ; di = offset
    mov ax,[highscore_value]
    push di            ; position
    push ax            ; number
    call printnum
   pop di
    pop es
    pop bx
    pop ax
    mov byte [current_attr], 0x07
    push high_back
    push 22
    call print_center
.high_waitkey:
    in al, 0x60
    cmp al, 0x30
    jne .high_waitkey
    ret

; AX = number, BP+4=row
print_score_center:
    push bp
    mov bp, sp
    push ax
    push di
    push es

    mov ax,0B800h
    mov es,ax

    ; row * 80 + col (maan lo col=35)
    mov ax, [bp+4]
    mov bx,80
    mul bx
    add ax,35
    shl ax,1
    mov di,ax

    pop ax              ; number
    push di
    push ax
    call printnum

    pop es
    pop di
    pop bp
    ret 2

; NUMBER PRINTER
printnum:
    push bp
    mov bp, sp
    push es
    push ax
    push bx
    push cx
    push dx
    push di
    mov ax, 0xb800
    mov es, ax 
    mov ax, [bp+4]      ; number
    mov bx, 10 
    mov cx, 0 
.nextdigit:
    mov dx, 0 
    div bx 
    add dl, 0x30 
    push dx 
    inc cx 
    cmp ax, 0 
    jnz .nextdigit 
    mov di, [bp+6]      ; position
.nextpos:
    pop dx 
    mov dh, 0x07 
    mov [es:di], dx 
    add di, 2 
    loop .nextpos 
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    pop bp
    ret 4
callmee:
    mov ax , 1990
    push ax
    mov ax , Lose_str
    push ax
    mov ax , 8
    push ax
    call printstr
    jmp callmee
    pop ax
    ret
printstr:
    push bp
    mov bp, sp
    push es
    push ax
    push cx
    push si
    push di
    mov ax, 0xb800
    mov es, ax 
    mov di, [bp+8] 
    mov si, [bp+6] 
    mov cx, [bp+4] 
    mov ah, [current_attr]
.nextchar: 
    mov al, [si] 
    mov [es:di], ax 
    add di, 2 
    add si, 1 
    loop .nextchar 
    pop di
    pop si
    pop cx
    pop ax
    pop es
    pop bp
    ret 6
time_str: db 'TIME '
printStrings:
    push ax
    mov ax , 280
    push ax
    mov ax , Lives_str
    push ax
    mov ax , 5
    push ax
    call printstr   
    mov ax , 162
    push ax
    mov ax , Score_str
    push ax
    mov ax , 5
    push ax
    call printstr    
    mov ax , 220
    push ax
    mov ax , time_str
    push ax
    mov ax , 5
    push ax
    call printstr  
    pop ax
    ret
clrscr: 
    push es
    push ax
    push cx
    push di
    mov ax, 0xb800
    mov es, ax
    xor di, di 
    mov ax, 0x0720 
    mov cx, 2000 
    cld 
    rep stosw 
    pop di 
    pop cx
    pop ax
    pop es
    ret 
border:
    push ax
    push es
    push di    
    mov ax, 0xb800
    mov es, ax
    mov ah, 0x37      
    mov al, '='
    mov di, 0
.top_border:
    mov [es:di], ax
    add di, 2
    cmp di, 160
    jb .top_border
    mov di, 3840
.bottom_border:
    mov [es:di], ax
    add di, 2
    cmp di, 4000
    jb .bottom_border
    mov di, 0
    mov cx, 25         ; rows
.side_loop:
    mov [es:di], ax
    mov bx, di
    add bx, 158
    mov [es:bx], ax
    add di, 160
    loop .side_loop
    pop di
    pop es
    pop ax
    ret
brick_remove:
    push es
    push ax
    push dx
    push cx
    push si
    push bx    
    mov ax,0xb800
    mov es,ax   
    mov cx , 32
    mov si , 0  
    mov dx , [cs:calculated_location] 
check:
    mov ax , word[cs:bricks_start_location + si]
    mov bx , word[cs:bricks_end_location + si]
    add si , 2
    cmp dx , ax
    jae checknext
    loop check
    jmp end_ofFunc
checknext:
    cmp dx , bx
    jbe remove
    loop check
    jmp end_ofFunc
remove:
    sub si , 2
    mov di , word [cs:bricks_start_location + si]
    mov cx, 1500        ; brick break beep
    call beep
    push es
    mov ax,0B800h
    mov es,ax
    mov bx,di
    mov ax,[es:bx]          ; AL = char, AH = attribute
    mov bl,ah               ; BL = color attribute
    pop es
    cmp bl,040h
    je  add_red
    cmp bl,060h
    je  add_orange
    cmp bl,050h
    je  add_pink
   cmp bl,020h
    je  add_green
    jmp score_done
add_red:
    add word [cs:score],5
    jmp score_done
add_orange:
    add word [cs:score],3
    jmp score_done
add_pink:
    add word [cs:score],2
    jmp score_done
add_green:
    add word [cs:score],1   
    jmp score_done
score_done:
    mov cx , 6
    mov ax , 0720h
    rep stosw
    dec word [cs:total_bricks]
    mov ax , 174
    push ax
    push word [cs:score]
    call printnum
    jmp end_ofFunc
end_ofFunc:
    pop bx
    pop si
    pop cx
    pop dx
    pop ax
    pop es
    ret
bricks:
    push es
    push cx
    push bx
    push si
    push di
    mov ax, 0B800h
    mov es, ax
    mov di, 810
    cld
brickline1:
    cmp di, 936
    ja  brickline2
    mov ah, 040h       
    mov al, 020h
    mov cx, 6
    rep stosw           
    mov cx, 3
    mov ax, 0720h
    rep stosw
    jmp brickline1
brickline2:
    mov di, 1290
brickline2_print:
    cmp di, 1416
    ja  brickline3
    mov ah, 060h       
    mov al, 020h
    mov cx, 6
    rep stosw
    mov cx, 3
    mov ax, 0720h
    rep stosw
    jmp brickline2_print
brickline3:
    mov di, 1770
brickline3_print:
    cmp di, 1896
    ja  brickline4
    mov ah, 050h        
    mov al, 020h
    mov cx, 6
    rep stosw 
    mov cx,3
    mov ax, 0720h
    rep stosw
    jmp brickline3_print
brickline4:
    mov di, 2250        
brickline4_print:
    cmp di, 2376        
    ja  endn
    mov ah, 020h        
    mov al, 020h
    mov cx, 6
    rep stosw
    mov cx, 3
    mov ax, 0720h
    rep stosw
    jmp brickline4_print
endn:
    pop di
    pop si
    pop bx
    pop cx
    pop es
    ret
clearStacker:
    push bp
    mov bp , sp
    push es
    push ax
    push di
    push cx 
    mov ax , 0xb800
    mov es , ax    
    mov ax , 0x0720
    mov cx , 13
    mov di , [bp+4]   
    rep stosw
    mov di,[cs:preBall]
    mov word[es:di],ax   
    pop cx
    pop di
    pop ax
    pop es
    pop bp
    ret 2
printStacker:
    push bp
    mov bp , sp
    push es
    push ax
    push di
    push cx   
    mov ax , 0xb800
    mov es , ax  
    mov al , 0x20
    mov ah , 0x10
    mov cx , 13
    mov di , [bp+4]  
    mov word[cs:left_limit] , di
    rep stosw
    sub di , 2
    mov word[cs:right_limit] , di   
    mov ax , word[cs:right_limit]
    sub ax,12
    mov word[cs:mid] , ax    
    sub ax,160
    mov di,ax
    shr ax,1
    sub ax,1680
    mov cx,ax   
    cmp byte[cs:StayOnStacker],1
    jne endi
    mov al,'O'
    mov ah,0x07
    mov word[es:di],ax
    mov [cs:preBall],di
    mov word[cs:row],21
    mov word[cs:colume],cx
    mov word[cs:previous],di        
endi:
    pop cx
    pop di
    pop ax
    pop es
    pop bp
    ret 2
stacker:
    push ax
    push di   
    cmp word[cs:right_] , 1
    je movRight
    cmp word[cs:left_] , 1
    je movLeft   
movRight:
    mov ax, word[cs:pre_stack_pos]
    add ax , 8
    cmp ax , word[cs:right_edge]
    ja exit1
    mov di, word[cs:pre_stack_pos]
    push di
    call clearStacker
    push ax
    call printStacker
    mov word[cs:pre_stack_pos] , ax
    jmp exit1    
movLeft:
    mov ax, word[cs:pre_stack_pos]
    sub ax , 8
    cmp ax , word[cs:left_edge]
    jb exit1
    mov di, word[cs:pre_stack_pos]
    push di
    call clearStacker
    push ax
    call printStacker
    mov word[cs:pre_stack_pos] , ax    
exit1:
    pop di
    pop ax
    ret
calculate_position:
    push bp
    mov bp , sp
    push ax   
    mov al , 80
    mul byte[bp+4]
    add ax , [bp+6]
    shl ax ,1   
    mov word[cs:calculated_location] , ax   
    pop ax
    pop bp
    ret 4
nextposition:
    push ax
    push bx
    push cx   
    mov al,[cs:incC]
    mov ah,[cs:incR]
    mov bx,[cs:colume]
    mov cx,[cs:row]
    cmp word[cs:colume],3
    jne nextcond4
    mov al,1
    jmp rowCheck3
nextcond4:
    cmp word[cs:colume],77
    jne rowCheck3
    mov al,0           
rowCheck3:
    cmp word[cs:row],4
    jne nextcond5
    mov ah,1
    jmp printingLocation1
nextcond5:
    cmp word[cs:row],22
    jne printingLocation1
    mov ah,0    
printingLocation1:
    cmp al,1
    jne nextcond6
    add bx,1
    jmp rowCheck4
nextcond6:
    sub bx,1            
rowCheck4:
    cmp ah,1
    jne nextcond7
    add cx,1
    jmp calculatelocation1
nextcond7:
    sub cx,1
calculatelocation1:
    push bx 
    push cx 
    call calculate_position        
    pop cx
    pop bx
    pop ax
    ret
left_right:
    push ax    
    mov ax , word[cs:calculated_location]
    cmp ax , [cs:mid]
    ja check_right   
    cmp ax , [cs:left_limit]
    jb endit
    mov byte[cs:left_or_right] , 0
    jmp endit   
check_right:
    cmp ax , [cs:right_limit]
    ja endit
    mov byte[cs:left_or_right] , 1   
endit:
    pop ax
    ret
ball:
    push es
    push ax
    push bx
    push cx
    push di   
    mov ax,0xb800
    mov es,ax    
    mov di,[cs:previous]
    mov word[es:di],0x0720
    call nextposition
    mov di,[cs:calculated_location]
    mov ax,word[es:di]
    cmp ah,0x07
    je R
    cmp ah,0x10
    je n
    call brick_remove
    jmp n1
n:
    mov cx,2000 ;paddle beep
    call beep
    call left_right
    cmp byte[cs:left_or_right],1
    jne n3
    mov byte[cs:incC],1
    jmp n1
n3:
    mov byte[cs:incC],0
n1:
    cmp byte[cs:incR],1
    jne r1
    mov byte[cs:incR],0
    jmp R
r1:
    cmp byte[cs:incR],0
    jne R
    mov byte[cs:incR],1
R:
    cmp word[cs:colume],3
    jne nextcond
    mov byte[cs:incC],1
    jmp rowCheck
nextcond:
    cmp word[cs:colume],77
    jne rowCheck
    mov byte[cs:incC],0           
rowCheck:
    cmp word[cs:row],4
    jne nextcond1
    mov byte[cs:incR],1
    jmp printingLocation
nextcond1:
    cmp byte[cs:solid],0
    jne solid12
    cmp word[cs:row],22
    jne printingLocation
    mov byte[cs:StayOnStacker],1 
    mov ax,word[cs:mid]
    sub ax,160
    mov di,ax
    shr ax,1
    sub ax,1680
    mov cx,ax
    mov al,'O'
    mov ah,0x07
    mov word[es:di],ax
    mov [cs:preBall],di
    mov word[cs:row],21
    mov word[cs:colume],cx
    mov word[cs:previous],di
    sub byte[cs:live],1
    mov cx, 4000        
    call beep
    call print_lives 
    cmp byte[cs:live],0
    jne endii
    jmp endii
solid12:
    cmp word[cs:row],23
    jne printingLocation
    mov byte[incR],0
printingLocation:
    cmp byte[cs:incC],1
    jne nextcond2
    add word[cs:colume],1
    jmp rowCheck1
nextcond2:
    sub word[cs:colume],1           
rowCheck1:
    cmp byte[cs:incR],1
    jne nextcond3
    add word[cs:row],1
    jmp calculatelocation
nextcond3:
    sub word[cs:row],1
calculatelocation:
    mov ax,word[cs:row]
    mov bx,80
    mul bx
    add ax,word[cs:colume]
    shl ax,1
    mov di,ax
    mov word[cs:previous],ax        
    mov ah,0x07
    mov al,'O'
    mov word[es:di],ax   
endii:
    pop di
    pop cx
    pop bx
    pop ax
    pop es
    ret 
call_instruction_menu: db 0
   kbisr:
    push ax
    push es
    mov word [cs:right_], 0
    mov word [cs:left_], 0
    mov ax, 0B800h
    mov es, ax
    in  al, 60h
    cmp al, 01h          
    je  set_quit
    cmp al, 13h          
    je  set_restart
    cmp al, 01h          
    je  set_end
    cmp byte [start_game], 0
    jne main_game
 cmp al, 01h          
    je  set_end
    cmp al, 1Ch          
    je  menu_start
    cmp al, 03h          
    je  menu_how
    cmp al, 04h          
    je  menu_high
    cmp al, 05h         
    je  menu_info
    jmp isr_exit
set_quit:
    mov byte [cs:quit], 1
    jmp isr_exit
set_restart:
    mov byte [cs:restart], 1
    jmp isr_exit
set_end:
    mov byte [cs:end_game], 1
    jmp isr_exit
menu_start:
    mov byte [start_game], 1
    jmp isr_exit
menu_how:
    call how_screen
    call main_menu
    jmp isr_exit
menu_high:
    call high_screen
    call main_menu
    jmp isr_exit
menu_info:
    call info_screen
    call main_menu
    jmp isr_exit
main_game:
    cmp al, 4Bh
    je  key_left
    cmp al, 4Dh
    je  key_right
    cmp al, 39h
    je  key_space
    jmp isr_exit
key_left:
    mov word [cs:left_], 1
    call stacker
    jmp isr_exit
key_right:
    mov word [cs:right_], 1
    call stacker
    jmp isr_exit
key_space:
    mov byte [cs:StayOnStacker], 0
    jmp isr_exit
nomatch:
    pop es
    pop ax
    jmp far [cs:oldisr]
isr_exit:
    mov al, 20h
    out 20h, al
    pop es
    pop ax
    iret
timer: 
    cmp byte[cs:start_game],1
    jne pp
    inc byte[cs:clock]
    cmp byte[cs:clock],18
    jne ppp
    add word[cs:second],1
    mov byte[cs:clock],0
ppp:
    cmp byte[cs:solid],1
    jne po
    inc byte[cs:solid1]
    cmp byte[cs:solid1],180
    jne po
    mov byte[cs:solid],0
po:
    inc word[cs:bonus]
    cmp word[cs:bonus],2160
    jnbe pk
    cmp word[cs:total_bricks],0
    jne pk
    add word[cs:score],50
pk:
    push ax
    mov ax , 230
    push ax
    mov ax , word[cs:second]
    push ax
    call printnum
    pop ax
pp:
    cmp byte[cs:StayOnStacker],0
    jne endof
    cmp byte[cs:start_game] , 1
    jne endof
    inc byte[cs:tickcount]
    cmp byte[cs:tickcount], 2
    jne endof
    call ball
    mov byte[cs:tickcount],0
endof:
    mov al, 0x20
    out 0x20, al 
    iret
beep:
    push ax
    push bx
    push cx
    push dx
    mov al, 0B6h
    out 43h, al
    mov ax, 700         
    out 42h, al         
    mov al, ah
    out 42h, al         
    in  al, 61h
    or  al, 03h
    out 61h, al         
.delay_loop:
    loop .delay_loop
    in  al, 61h
    and al, 0FCh
    out 61h, al        
    pop dx
    pop cx
    pop bx
    pop ax
    ret
print_lives:
    push ax
    push es   
    mov ax , 0xb800
    mov es , ax
    mov cx , 3
    mov ax , 0x0720
    mov di , 292
    rep stosw   
    mov cl , byte[cs:live]
    mov ch , 0
    mov ah , 0x07
    mov al , '*'
    mov di , 292
    rep stosw   
    pop es
    pop ax
    ret
oldtmr: dd 0
end_game: db 0
load_highscore:
    push ax
    push bx
    push cx
    push dx
    mov ah,3Dh              ; open existing HISCORE.DAT read-only
    mov al,0
    mov dx, highscore_filename
    int 21h
    jc  .no_file           
    mov bx,ax               ; handle
    mov ah,3Fh              ; read
    mov cx,2                ; 2 bytes
    mov dx, highscore_value
    int 21h                 ; highscore_value = file contents 
    mov ah,3Eh              ; close
    int 21h
    jmp .done
.no_file:
    mov word [highscore_value],0
.done:
    pop dx
    pop cx
    pop bx
    pop ax
    ret
start:
    xor ax, ax
    mov es, ax    
    mov ax, [es:9*4]
    mov [oldisr], ax 
    mov ax, [es:9*4+2]
    mov [oldisr+2], ax    
    mov ax, [es:8*4]
    mov [oldtmr], ax 
    mov ax, [es:8*4+2]
    mov [oldtmr+2], ax 
    cli
    mov word [es:9*4], kbisr 
    mov [es:9*4+2], cs 
    mov word [es:8*4],timer
    mov [es:8*4+2],cs
    sti 
call load_highscore
call clrscr
    call main_menu
menu_loop:
    cmp byte[start_game] , 0
    je menu_loop
    cmp byte[end_game] , 1
    je endgame
start_game_here:
    mov byte[restart] , 0
    mov byte[quit] , 0
    call clrscr
    call border
    call printStrings
    call print_lives
    mov ax , 174
    push ax
    push word[score]
    call printnum
    call bricks
    mov byte[StayOnStacker],1
    call stacker
game_inner_loop:
    cmp word[total_bricks] , 0
    je endgame
    cmp byte[end_game] , 1
    je endgame
    cmp byte[live] , 0
    je endgame
    jmp game_inner_loop
instruction:
again_ins:
    cmp byte[start_game] , 1
    je start_game_here
    jne again_ins   
endgame:
    mov byte[start_game] , 0
    call last_menu
    call clrscr
    cmp byte[restart] , 1
    je start_game_here
    mov ax , [oldisr]
    mov bx , [oldisr+2]
    mov cx , [oldtmr]
    mov dx , [oldtmr+2]
    cli 
    mov [es:9*4] , ax
    mov [es:9*4+2], bx
    mov [es:8*4] , cx
    mov [es:8*4+2], dx
    sti
    mov ax , 0x4c00
    int 0x21
restart: db 0
quit: db 0
variable: dw 0
last_menu:
    push ax
    call clrscr
 call update_highscore
    cmp byte[live] , 0
    jne check_win
 mov byte [current_attr], 0x0C
    mov ax , 1990
    push ax
    mov ax , Lose_str
    push ax
    mov ax , 8
    push ax
    call printstr
check_win:
    cmp word[total_bricks] , 0
    jne no_results
 mov byte [current_attr], 0x0E
    mov ax , 1990
    push ax
    mov ax , Win_str
    push ax
    mov ax , 8
    push ax
    call printstr   
no_results:
    call last_menu_display   
what_nxt:
    cmp byte[cs:restart] , 1
    je do_restart
    cmp byte[cs:quit] , 1
    je go_quit
    jmp what_nxt
go_quit:
    pop ax
    ret       
do_restart:
    pop ax
    mov word[second] , 0
    mov byte[clock] , 0
    mov byte[start_game] , 1
    mov word[total_bricks] , 24
    mov byte[live] , 3
    mov word[score] , 0
    mov byte[end_game] , 0
    mov word[bonus] , 0
    ret       
last_menu_display:
    push ax   
 mov byte [current_attr], 0x0E
    mov ax , 1010          
 push ax
    mov ax , total_score_str
    push ax
    mov ax , 17
    push ax
    call printstr   
    mov ax , 1052
    push ax
    push word[score]
    call printnum  
  mov byte [current_attr], 0x0A  
    mov ax , 1330
    push ax
    mov ax , restart_str
    push ax
    mov ax , 34
    push ax
    call printstr  
 mov byte [current_attr], 0x0B  
    mov ax , 1650
    push ax
    mov ax , quit_str
    push ax
    mov ax , 26
    push ax
    call printstr   
    pop ax
    ret
update_highscore:
    push ax
    push bx
    push cx
    push dx
    mov ax, [score]
    cmp ax, [highscore_value]
    jbe .done               
    mov [highscore_value], ax
    mov ah,3Ch              ; create/overwrite HISCORE.DAT
    mov cx,0
    mov dx, highscore_filename
    int 21h
    jc  .done
    mov bx,ax               ; handle
    mov ah,40h              ; write 2 bytes
    mov cx,2
    mov dx, highscore_value
    int 21h
    mov ah,3Eh              ; close
    int 21h
.done:
    pop dx
    pop cx
    pop bx
    pop ax
    ret
