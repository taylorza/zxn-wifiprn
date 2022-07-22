        SLDOPT COMMENT WPMEM, LOGPOINT, ASSERTION
        DEVICE ZXSPECTRUMNEXT
        CSPECTMAP "wifiprn.map"

BASE_ADDRESS equ $8000

        org  BASE_ADDRESS

        ; CSpect macro used for debugging
        macro DBG_BREAK
            defb $dd, $01
            nop
        endm

;------------------------------------------------------------------------------
; Driver header
        db "NDRV"               ; .DRV Signature
        db "P" | $80            ; Driver ID - "P" standard printer device
        db relocate_count       ; Number of relocations
        db 0                    ; Number of 8K DivMMC RAM banks needed
        db 0                    ; Number of 8K Spectrum RAM banks needed
        
;------------------------------------------------------------------------------
; Driver code
        disp $0000              ; Driver entry point starts at $0000
        RELOCATE_START HIGH
entry: 
        jp api ; 3 byte jump to place IM1 routine is at address $0003
im1_entry:
        ld a, (printState)          ; Get current state
        or a                        ; If STATE_IDLE    
        ret z                       ;   exit early

        di                                                                               
        ld a, (activityCounter)     ; Get the current activity counter
        cp STATE_TRANSITION_DELAY   ; Is it time to transition to next state
        jr nc, .tearDownFSM         ;   if so, tick the state machine  
                                    ; Else
        inc a                       ;   increment the counter
        ld (activityCounter), a     
        jr .exit                    

.tearDownFSM
        ld hl, .stateTbl            ; HL = Base of state table
        ld a, (printState)          ; A = current state
        add a                       ; A x 2 => index into state table
        add hl, a                   ; This is for NextOS so assume Z80N :)
        jp (hl)                     ; Jump to dispatcher in stateTbl
        
;-- Teardown state handlers --                  
.endJob
        ld hl, JOB_END
        call uartSendZ
        ld a, STATE_ENDPRINT         
        jr .nextState

.endPrint
        ld hl, CMD_ENDPASSTHROUGH
        call uartSendZ
        ld a, STATE_DISCONNECT
        jr .nextState

.disconnect
        ld hl, CMD_TCPDISCONNECT
        call sentATCIP
;         |
;         | Fall through to .goIdle
;         V
.goIdle
        xor a                   ; ld a, STATE_IDLE
;         |
;         | Fall through to .nextState
;         V

.nextState
        ld (printState), a
        xor a
        ld (activityCounter), a

.exit
        ret

; Print job termination state transition dispatch table
.stateTbl    
        jr .exit                    ; STATE_IDLE
        jr .endJob                  ; STATE_PRINTING
        jr .endPrint                ; STATE_ENDPRINT
        jr .disconnect              ; STATE_DISCONNECT
        jr .goIdle                  ; STATE_ERROR

api:
        ld a, b
        
        cp $fb                  ; Output character
        jr z, output_char       

        cp $fa                  ; Close channel
        jr z, close_channel     

        cp $f9                  ; Open channel
        jr z, open_channel

        cp $f7                  ; Return output status
        jr z, get_output_status  

api_error
        xor a                   ; A=0 unsupported call id
        scf                     ; CY=1 indicates and error
        ret

;-----------------------------------------------------------------------------
; Call ID - $fb
;       E - char to print
output_char:
        ld a, (printState)
        cp STATE_ERROR
        jr z, returnError       ; If in error state we return

        or a                    ; 0 - STATE_IDLE
        call z, connectPrinter  ; If STATE_IDLE we need to connect to the printer
        jr c, returnError       ; If the connection failed we return an error
        
        xor a
        ld (activityCounter), a ; Reset activity counter
        
        ld a, e                 ; Get char to send in A
.sendChar
        call uartSend           ; Send char
        cp 13                   ; Check for carriage return
        jr z, .sendLF           ; If so, setup to send LF
        xor a                   ; else clear CY
        ret                     ; and return

.sendLF
        ld a, 10                ; Put LF in A
        jr .sendChar            ; Go back and wait for the printer to be ready

;------------------------------------------------------------------------------
; Call ID - $f7
get_output_status:  
        ld bc, $ffff            ; Assume not busy
        ld a, (printState)
        or a
        ret z                   
        inc bc                  ; Printer is busy
        ret

;------------------------------------------------------------------------------
; Call ID - $f9
open_channel:
        ; Parse IP address
        ld a,e                  ; check if E is 1..15 (ignores D completely, IP string should be short)
        dec a                   ; 0..14 is valid range
        add a,-15               ; CF = 1 for 15..255, CF = 0 for valid range
        jr c, returnError
        ld b,0
        ld c,e
        ld de, printerIP
        ldir
        ex de,hl
        dec hl
        set 7,(hl)
        ret

;------------------------------------------------------------------------------
; Call ID - $fa
close_channel:
        ld hl, printerIP
        ld (hl), $80            ; Invalidate the IP address
        xor a                   ; A=0, CY=0 for no error
        ret

;------------------------------------------------------------------------------
; returnError - Jump to this code will set the error code and return with CY set
returnError:
        ld a, STATE_ERROR
        ld (printState), a
        ld a, $fe
        scf
        ret
connectPrinter:
        ; Select ESP UART
        ld bc, $153b            ; UART Select
        or a                    ; Select the ESP UART
        out (c), a

        ; Drain Rx Fifo
        dec b
        dec b                   ; BC = $133b Tx/Status port
.drain
        in a, (c)
        rrca
        jr nc, .connect
        inc b                   ; Rx Port ($143b)
        in a, (c)
        dec b                   ; Status port
        jr .drain

.connect
        ld hl, ATE0
        call sendCommand

        ld hl, CMD_TCPDISCONNECT
        call sentATCIP
         
        push bc    
        ld hl, PRE_ATCIP
        call uartSendZ
        ld hl, PRE_TCPCONNECT
        call uartSendZ
        ld hl, printerIP
        call uartSendZ
        ld hl, POST_TCPCONNECT
        call sendCommand
        pop bc
        ret c

        ld hl, CMD_PASSTHROUGH
        call sentATCIP
        ret c

        ld hl, CMD_SEND
        call sentATCIP
        ret c

        ld hl, JOB_START
        call uartSendZ

        ld a, STATE_PRINTING
        ld (printState), a
        ret

sentATCIP:
        push hl
        ld hl, PRE_ATCIP
        call uartSendZ
        pop hl
sendCommand:
        call uartSendZ        
        ld hl, CRLF
        call uartSendZ

;------------------------------------------------------------------------------
; Response Parser - reads the response from ESP
; Result:
;   CY - set on error
        
        ; Compare the character in reg-A
        ;   if it is a match jump to 'target'
        macro CurrentIs ch, target
            cp ch
            jr z, target
        endm

        ; Read from the UART and compare with the character in 'ch'
        ;   if it is not a match jump to 'target'
        macro NextIsNot ch, target
            call .readAndCompare
            db ch
            jr nz, target
        endm
parseResponse:
        call .readToLF                  ; Read until the first LF all status response start with CRLF

        call uartRead                   ; Check lead character
        CurrentIs 'O', .parseOK         ; Possibly 'OK'
        CurrentIs 'E', .parseERROR      ; Possibly 'ERROR'
        ;CurrentIs 'F', .parseFAIL       ; Possibly 'FAIL' (excluded to save space see note below)
        jr parseResponse

.parseOK    ; Follow set for 'OK'
        NextIsNot 'K', parseResponse    
        NextIsNot CR, parseResponse
        call .readToLF
        xor a
        ret

.parseERROR ; Follow set for 'ERROR'
        NextIsNot 'R', parseResponse
        NextIsNot 'R', parseResponse
        NextIsNot 'O', parseResponse
        NextIsNot 'R', parseResponse
        NextIsNot CR, parseResponse
;        jr .error
;
;.parseFAIL  ; Follow set for 'FAIL'    ; Non of the commands used result in FAIL,
;        NextIsNot 'A', parseResponse   ; so I can save some space by not checking this
;        NextIsNot 'I', parseResponse
;        NextIsNot 'L', parseResponse
;        NextIsNot CR, parseResponse
;            |
;            | Fallthrough to .error
;            V
.error
        call .readToLF          ; Read the rest of the response
        scf
        ret

.readToLF
        call uartRead
        cp LF
        jr nz, .readToLF
        ret

;------------------------------------------------------------------------------
; .readAndCompare - Read a character from the UART and compare with the byte
;                 - immediately following the call to this function
.readAndCompare:
        call uartRead
        pop hl          ; Return address contains byte to compare
        ld b, (hl)
        cp b            
        inc hl          ; Move past the byte
        push hl         ; And push as new return address
        ret
        
uartSendZ:
        ld a, (hl)
        rlca                    ; CY = high bit
        srl a                   ; Restore A with 0 in high bit
        call uartSend           ; Saves flags, so we still have CY
        inc hl          
        jr nc, uartSendZ        ; CY not set, so send next character
        or a                    ; Clear CY
        ret 

uartSend:
        push af                 ; Save the character to send
        ld bc, $133b            
.wait
        in a, (c)               ; Read the UART status
        and %00000010           ; Test the TX Busy flag
        jr nz, .wait            ; If busy, wait

        pop af                  ; Restore the character to send
        out (c), a              ; Write the character to the UART
        ret

uartRead:
        ld bc, $133b
.wait
        in a, (c)               ; Read the status
        rrca                    ; Rotate the data ready status bit into CY
        jr nc, .wait            ; Wait if there is no data

        inc b                   ; Rx port ($143b)
        in a, (c)               ; Read from UART
        ret

CR                      equ $0d
LF                      equ $0a

CRLF                    db CR, LF | $80
ATE0                    db 'ATE', '0' | $80
PRE_ATCIP               db 'AT+CI', 'P' | $80
PRE_TCPCONNECT          db 'START="TCP",', '"' | $80
POST_TCPCONNECT         db '",910', '0' | $80
CMD_TCPDISCONNECT       db 'CLOS', 'E' | $80
CMD_PASSTHROUGH         db 'MODE=', '1' | $80
CMD_SEND                db 'SEN', 'D' | $80
CMD_ENDPASSTHROUGH      db '++', '+' | $80

JOB_END                 db $0c              ; JOB_END = $0c,$1b, 'E' | $80 => use JOB_START for suffix
JOB_START               db $1b,'E' | $80

activityCounter         db 0
printState              db STATE_IDLE

printerIP               ds 15, $fe
        
        RELOCATE_END

;------------------------------------------------------------------------------
; Print Job teardown state constants
STATE_TRANSITION_DELAY  equ 100 ; Delay between state transitions (50 per second)

STATE_IDLE              equ 0
STATE_PRINTING          equ 1
STATE_ENDPRINT          equ 2
STATE_DISCONNECT        equ 3
STATE_ERROR             equ 4

; Pad driver to 512
        ASSERT $ <= 512, Driver image exceeds 512 bytes
        ds 512-$, 0

;------------------------------------------------------------------------------
; Relication table generated after the 512 byte code
relocator_table:
        RELOCATE_TABLE 
                
;------------------------------------------------------------------------------
; Output configuration
image_size  equ     $$$-BASE_ADDRESS ; Size includes the header, code and relocation table

            SAVEBIN "wifiprn.drv", BASE_ADDRESS, image_size 