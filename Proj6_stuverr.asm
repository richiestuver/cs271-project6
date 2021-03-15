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

    MOV     EDX, string_addr
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

    list                BYTE    "Your list is: ", 0
    avg                 BYTE    "The average is: ", 0
    sum                 BYTE    "The sum is: ", 0
    farewell            BYTE    "Thanks, it's been fun. Bye!", 0

    userInts            DWORD   10 DUP(?)

    userString          BYTE    33 DUP(0) ; large enough for user to enter up to 32 chars
    lenUserString       DWORD   ? ; 32 bit signed integer is 10 digits long

    tempString          BYTE    12 DUP(0)

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
    MOV     ECX, LENGTHOF userInts
    MOV     ESI, offset userInts
    MOV     EBX, TYPE userInts
_get_ints:
    ;------------------------------
    ; Pass in arguments to ReadVal
    ;------------------------------
    PUSH    offset error            ; [EBP + 28]
    PUSH    ESI                     ; [EBP + 24]
    PUSH    offset prompt           ; [EBP + 20]
    push    offset userString       ; [EBP + 16]
    push    SIZEOF userString       ; [EBP + 12]
    push    offset lenUserString    ; [EBP + 8]
    CALL    ReadVal

    ADD     ESI, EBX ; increment to next address to write the value.
    LOOP    _get_ints

    ;------------------------------
    ; This next section deals with 
    ; writing the list of values to console.
    ;------------------------------
    MOV     ECX, LENGTHOF userInts
    MOV     ESI, offset userInts
    MOV     EBX, TYPE userInts
    MOV     EDX, 0 ; store the sum of userInts.
    ;------------------------------
    ; Write a title to the screen
    ;------------------------------
    mDisplayString offset list

_loop_list:
    ;------------------------------
    ; Pass in arguments to WriteVal
    ;------------------------------
    MOV     EAX, [ESI]
    PUSH    lengthof tempstring     ; [EBP + 16]
    PUSH    offset tempString       ; [EBP + 12]  
    PUSH    EAX                     ; [EBP + 8]
    CALL    writeVal

    ;------------------------------
    ; Handle loop management - incrment necessary counters and 
    ; execute conditional branch to see if we are done
    ;------------------------------
    ADD     EDX, EAX ; keep a running total of the sum
    
    ADD     ESI, EBX ; increment by size of data type (dword)

    DEC     ECX
    CMP     ECX, 0   ; break out of loop
    JE      _continue 

    ;------------------------------
    ; Formatting for printing list. Don't append comma or space to 
    ; the last item
    ;------------------------------
    MOV     AL, 44 ; comma
    CALL    WriteChar

    MOV     AL, 32 ; space
    CALL    WriteChar

    JMP     _loop_list
 
 _continue:

    CALL    CrLF

    ;------------------------------
    ; Display the sum stored in EDX
    ;------------------------------
    mDisplayString offset sum

    ;------------------------------
    ; Pass in arguments to WriteVal
    ;------------------------------
    PUSH    lengthof tempstring     ; [EBP + 16]
    PUSH    offset tempString       ; [EBP + 12]  
    PUSH    EDX                     ; [EBP + 8]
    CALL    writeVal
    
    CALL    CrLF
    
    ;------------------------------
    ; Display the avg 
    ;------------------------------
    mDisplayString offset avg

    ;------------------------------
    ; Calculate the average
    ;------------------------------
    mov     eax, edx
    mov     ebx, lengthof userInts
    CDQ     ; convert dword to quadword for idiv
    IDIV    ebx
    
    ;------------------------------
    ; Pass in arguments to WriteVal
    ;------------------------------
    PUSH    lengthof tempstring     ; [EBP + 16]
    PUSH    offset tempString       ; [EBP + 12]  
    PUSH    EAX                     ; [EBP + 8]
    CALL    writeVal
    
    CALL    CrLF
    CALL    CrLF

    ;------------------------------
    ; Say goodbye, it's the end of the program
    ;------------------------------

    mDisplayString offset farewell

    Invoke ExitProcess,0	; exit to operating system
main ENDP

; (insert additional procedures here)

; ---------------------------------------------------------------------------------
; Name: ReadVal
;
; Description: Gets user input as a string of ascii digits. Converts digits to 
;               numeric SDWORD. Validates the input. Writes the SDWORD to memory.
;
; Preconditions: pass in 6x 4byte data types == 24 bytes
;
; Postconditions: all registers saved and restored. 
;
; Recieves: 1) addr for error message offset    [EBP + 28]
;           2) addr to store user DWORD         [EBP + 24]
;           3) addr of prompt string            [EBP + 20]
;           4) addr to store user string        [EBP + 16]
;           5) size of user string              [EBP + 12]
;           6) addr to store bytes read         [EBP + 8]
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

_start:
    ;------------------
    ; Do stuff
    ;------------------
    mGetString [EBP + 20], [EBP + 16], [EBP + 12], [EBP + 8]

    ;------------------
    ; Setup registers with the values needed to perform the string 
    ; traversal.
    ;------------------
    
    MOV     EBX, [EBP + 8]  ; addr of length of user input
    MOV     ESI, [EBP + 16] ; address of user input
    MOV     ECX, [EBX]      ; value of length of user input
    
    CLD
    MOV     EBX, +0 ; store running total for the integer value

    LODSB
    ;------------------------------
    ; If the first char is a sign, check if it's negative.
    ; if the sign is positive, skip to the next char in the array.
    ;------------------------------
_check_pos:
    CMP     AL, '+'
    JNE     _check_neg
    DEC     ECX
    JMP     _loop

_check_neg:
    CMP     AL, '-'
    JNE     _check_low
    DEC     ECX
    JMP     _loop

_loop:
    LODSB
    ;------------------------------
    ; DO A COMPARISON HERE FOR ILLEGAL CHARS. if char is not in [48, 57]
    ; then it is not a valid integer
    ;------------------------------
_check_low:
    CMP     AL, '0'
    JL      _error
    JMP     _check_high

_check_high:
    CMP     AL, '9'
    JG      _error

    ;------------------------------
    ; load the char into the accumulator, convert it to
    ; it's numeric value, and add it to the running total.
    ; after this loop exits, the final int value is in EBX
    ;------------------------------
_calculateTotal:
    SUB     AL, '0' ; start of ascii integers
    PUSH    EAX
    MOV     EAX, +10
    IMUL    EBX
    JNO     _proceed
    POP     EAX
    JMP     _error
_proceed:
    MOV     EBX, EAX
    POP     EAX

    MOVZX   EAX, AL
    ADD     EBX, EAX
    ;------------------------------
    ; Check for overflow. A postive number larger than 2,147,483,647 will not fit.
    ;------------------------------
    JO      _error

    LOOP    _loop

    CLD ; DONT FORGET TO CLEAR THE DIRECTION FLAG
    

    ;------------------------------
    ; if the sign is negative, negate. 
    ;------------------------------
    PUSH    EBX
    MOV     EBX, [EBP + 8]  ; addr of length of user input
    MOV     ESI, [EBP + 16]
    LODSB   
    POP     EBX

    CMP     AL, '-'
    JNE     _continue
    NEG     EBX
    
_continue:
    ;------------------------------
    ; Store the integer in the given output memory address
    ;------------------------------

    MOV     EDI, [EBP + 24]
    MOV     [EDI], EBX

    JMP     _tearDown

_error:
    MOV     edx, [EBP + 28]
    CALL    WriteString
    CALL    CrLf
    JMP     _start

    ;------------------------------
    ; Tear down the call stack and restore registers
    ;------------------------------
_tearDown:
    POP     EDI
    POP     ESI
    POP     ECX
    POP     EBX
    POP     EAX

    POP     EBP
    RET     24

ReadVal ENDP

; ---------------------------------------------------------------------------------
; Name: writeVal
;
; Description: Takes in an SDWORD and converts it into an ascii representation
;
; Preconditions: pass in SDWORD by value and offset to store byte array for processing
;                   and the address of an array to use for temp storage
;
; Postconditions: all registers saved and restored
;
; Recieves: 
;           length of temp string for processing [EBP + 16]
;           memory address to store string in while working [EBP + 12]
;           SDWORD (4) [EBP + 8]
;
; Returns: sdword converted to ascii and written to output
;
; ---------------------------------------------------------------------------------
WriteVal PROC
    ;------------------------------
    ; Setup the call stack and preserve any 
    ; registers used.
    ;------------------------------

    PUSH    EBP
    MOV     EBP, ESP

    PUSHAD

    ;------------------------------
    ; clear the temporary string by replacing it's contents with NULL (0)
    ; if you don't do this, bad things happen.
    ;------------------------------

    MOV     EDI, [EBP + 12] ; write string here
    MOV     EAX, 0; null terminator
    MOV     ECX, [EBP + 16] ; string length
_clear_string:
    STOSB
    LOOP    _clear_string
    
    ;------------------------------
    ; loop through the string buffer provided and write out the remainder
    ; of division by 10 to each location as an ascii digit. the string buffer
    ; holds the ascii representation "backwards"
    ;------------------------------

    MOV     EDI, [EBP + 12] ; write string here
    MOV     EAX, [EBP + 8] ; signed dword
    MOV     ECX, 0 ; count the string length

    ;------------------------------
    ; check for negativity by seeing if the dword is less than 0. 
    ; if negative, negate it and work only with the complement
    ;------------------------------

    CMP     EAX, 0
    JGE     _convert_to_string
    NEG     EAX

_convert_to_string:

    CDQ     ; convert dword to quadword for idiv
    MOV     EBX, 10
    IDIV    EBX     ; remainder in EDX
    
    ADD     EDX, 48 ; make it an ascii char
    PUSH    EAX

    MOV     AL, DL
    STOSB

    POP     EAX
    INC     ECX

    CMP     EAX, 0
    JNE     _convert_to_string
    
    ;------------------------------
    ; check for negativity by seeing if the dword is less than 0. 
    ; if negative, append a "-" sign for display purposes.
    ;------------------------------

    MOV     EAX, [EBP + 8]
    CMP     EAX, 0
    JGE     _not_negative
    PUSH    EAX
    MOV     AL, '-'
    STOSB
    POP     EAX
    INC     ECX

_not_negative:
    ;------------------------------
    ; Loop back down through the string arr and reverse the string in memory
    ; the next few lines set ESI and EDI to be same address and then inc/dec them
    ; in opposite directions. this is so we can decrement down ESI and get the 
    ; chars in reverse and add them to next successive slot in EDI.
    ;------------------------------
    STD
    MOV     esi, EDI
    DEC     ESI
    INC     EDI
    PUSH    EDI

_reverse:
    LODSB   ; char is in AL
    MOV     [EDI], AL
    INC     EDI ; add to the next available spot in the string buffer as we decrement ESI
    LOOP    _reverse

    CLD     ; NEVER FORGET
    POP     EDI
    MOV     ESI, EDI
    mDisplayString ESI

    ;------------------------------
    ; Tear down the call stack and restore registers
    ;------------------------------

    POPAD

    POP     EBP
    RET     12

WriteVal ENDP

END main
