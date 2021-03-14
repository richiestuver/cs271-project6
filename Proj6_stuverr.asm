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
    PUSH    ECX
    PUSH    EDX

    MOV     EDX, prompt_addr
    CALL    WriteString
    CALL    CrLf

    MOV     EDX, write_addr
    MOV     ECX, buffer_val
    CALL    ReadString ; address of user string now in EDX, lenght in EAX
    MOV     [bytes_read_addr], EAX

    POP     EDX
    POP     ECX
    POP     EAX

ENDM

mDisplayString MACRO string_addr

ENDM

; (insert constant definitions here)

.data

; (insert variable definitions here)
    intro               BYTE    "Project 6: The final project - by Richie Stuver", 13,10,13,10,
                                "Enter 10 signed decimal integers small enough to fit into a 32bit register.",
                                "The program will display the list assume well assume the sum and rounded average.",13,10,13,10,0
    prompt              BYTE    "Please enter a signed number: ",0

    userInts            DWORD   10 DUP(?)

    userString          BYTE    11 DUP(0) ; 32 bit signed integer is 10 digits long
    lenUserString       DWORD   ?


.code
main PROC

; (insert executable instructions here)

    mGetString offset prompt, offset userString, sizeof userString, offset lenUserString

    MOV     EDX, offset userString
    CALL    WriteString

    Invoke ExitProcess,0	; exit to operating system
main ENDP

; (insert additional procedures here)

END main
