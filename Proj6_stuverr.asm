TITLE Project6     (Proj6_stuverr.asm)

; Author: Richie Stuver
; Last Modified: 03-14-21
; OSU email address: stuverr@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number: 6                Due Date: 03-14-21
; Description: Write macros and low level procedures to read string input from user, 
;               convert to numeric data, and perform operations on that data.

INCLUDE Irvine32.inc

; (insert macro definitions here)

; ---------------------------------------------------------------------------------
; Name: mGetString
;
; Description: Calls Irvine proc to read a string from keyboard and stores at a 
;   given memory location.
;
; Preconditions:
;
; Receives:     prompt_addr = address for user prompt string
;               write_addr = address to write the user string
;               buffer_val = length of string buffer for user string
;               bytes_read_addr = address to write the number of bytes of user string
; 
; Returns:      user string contents written to memory at write_addr
;               length of user string written to bytes_read_addr
;
; ---------------------------------------------------------------------------------
mGetString MACRO prompt_addr:REQ, write_addr:REQ, buffer_val:REQ, bytes_read_addr:REQ
    
    PUSH    EAX
    PUSH    EBX
    PUSH    ECX
    PUSH    EDX

    MOV     EDX, prompt_addr
    CALL    WriteString

    MOV     EDX, write_addr
    MOV     ECX, buffer_val
    CALL    ReadString ; address of user string now in EDX, lenght in EAX
    
    MOV     EBX, bytes_read_addr
    MOV     [EBX], EAX

    POP     EDX
    POP     ECX
    POP     EBX
    POP     EAX

ENDM

; ---------------------------------------------------------------------------------
; Name: mDisplayString
;
; Description: Calls Irvine proc to write a string from memory
;
; Preconditions:
;
; Receives:     string_addr = address for string to write
; 
; Returns:      string contained at string_addr written to console
;
; ---------------------------------------------------------------------------------
mDisplayString MACRO string_addr

    PUSH    EDX

    MOV     EDX, offset string_addr
    CALL    WriteString

    POP     EDX

ENDM

; (insert constant definitions here)

.data

; (insert variable definitions here)
    intro               BYTE    "Project 6: The final project - by Richie Stuver", 13,10,13,10,
                                "Enter 10 signed decimal integers small enough to fit into a 32bit register.",
                                "The program will display the list as well as the sum and rounded average.",13,10,13,10,0
    
    prompt              BYTE    "Please enter a signed number: ",0

    error               BYTE    "ERROR: Invalid input. Too large or not a signed integer.",0

    userInts            DWORD   10 DUP(?)

    userString          BYTE    33 DUP(0) ; large enough for user to enter up to 32 chars
    lenUserString       DWORD   ? ; 32 bit signed integer is 10 digits long


.code
main PROC

; (insert executable instructions here)

    ;------------------------------
    ; Introduction and instructions to the user
    ;------------------------------
    mDisplayString offset intro

    ;------------------------------
    ; Collect the user's inputs and validate.
    ;------------------------------
    
    PUSH    offset userInts         ; [EBP + 24]
    PUSH    offset prompt           ; [EBP + 20]
    push    offset userString       ; [EBP + 16]
    push    SIZEOF userString       ; [EBP + 12]
    push    offset lenUserString    ; [EBP + 8]
    CALL    ReadVal

    ;mDisplayString offset userString

    Invoke ExitProcess,0	; exit to operating system
main ENDP

; (insert additional procedures here)

; ---------------------------------------------------------------------------------
; Name: ReadVal
;
; Description: Gets user input as a string of ascii digits. Converts digits to 
;               numeric SDWORD. Validates the input. Writes the SDWORD to memory.
;
; Preconditions: pass in 5x 4byte data types == 20 bytes
;
; Postconditions: all registers saved and restored. 
;
; Recieves: 1) addr to store user DWORD         [EBP + 24]
;           2) addr of prompt string            [EBP + 20]
;           3) addr to store user string        [EBP + 16]
;           4) size of user string              [EBP + 12]
;           5) addr to store bytes read         [EBP + 8]
;
; Returns: user's input saved as dword in memory
;
; ---------------------------------------------------------------------------------
ReadVal PROC

    ;------------------------------
    ; Setup the call stack and preserve any 
    ; registers used.
    ;------------------------------
    PUSH    EBP
    MOV     EBP, ESP

    PUSH    EAX
    PUSH    EBX
    PUSH    ECX
    PUSH    ESI
    PUSH    EDI

    ;------------------
    ; Do stuff
    ;------------------
    mGetString [EBP + 20], [EBP + 16], [EBP + 12], [EBP + 8]

    ;------------------------------
    ; Set the direction flag to start at the end of the user int
    ; For each byte, load it into the accumulator, convert it to
    ; it's numeric value, and add it to the running total.
    ;------------------------------
    
    MOV     EBX, [EBP + 8]
    MOV     ESI, [EBP + 16]
    MOV     ECX, [EBX]

    CLD

    MOV     EBX, 0
_loop:
    LODSB
    SUB     AL, '0' ; start of ascii integers

    PUSH    EAX
    MOV     EAX, 10
    MUL     EBX
    MOV     EBX, EAX
    POP     EAX

    MOVZX   EAX, AL
    ADD     EBX, EAX

    LOOP    _loop

    CLD ; DONT FORGET TO CLEAR THE DIRECTION FLAG
    
    ;------------------------------
    ; Display the value to confirm it's working
    ;------------------------------
    MOV     EAX, EBX
    CALL    WriteDec

    ;------------------------------
    ; Tear down the call stack and restore registers
    ;------------------------------

    POP     EDI
    POP     ESI
    POP     ECX
    POP     EBX
    POP     EAX

    POP     EBP
    RET 20

ReadVal ENDP

; ---------------------------------------------------------------------------------
; Name:
;
; Description:
;
; Preconditions:
;
; Postconditions:
;
; Recieves:
;
; Returns:
;
; ---------------------------------------------------------------------------------
WriteVal PROC

    RET

WriteVal ENDP

END main
