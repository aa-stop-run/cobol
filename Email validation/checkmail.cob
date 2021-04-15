      ******************************************************************
      * Author: aa-stop-run
      * Date: 15/04/2021
      * Purpose: E-mail validation module
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHECKMAIL.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CLASS ValidUser IS
           "-_."
           "0123456789"
           "abcdefghijklmnopqrstuvwxyz"
           "ABCDEFGHIJKLMNOPQRSTUVWXYZ" SPACE.
           CLASS ValidDominio IS
           "-."
           "abcdefghijklmnopqrstuvwxyz"
           "ABCDEFGHIJKLMNOPQRSTUVWXYZ" SPACE.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DOMINIOS ASSIGN TO "dom"
                   ORGANIZATION IS INDEXED
                   ACCESS IS DYNAMIC
                   Record Key DOMINIO-VAL
                   FILE STATUS FSTATUS.
       DATA DIVISION.
       FILE SECTION.
       FD DOMINIOS.
       01  DOMINIOS-RECORD.
           03  DOMINIO-VAL PIC X(20).
               88  READ-STATUS             VALUE HIGH-VALUES.
           03  FSTATUS     PIC 9(02)   VALUE ZEROS.
       WORKING-STORAGE SECTION.
       01  WS-EMAIL        PIC X(40)   VALUE SPACES.
       01  WS-EMAIL1       PIC X(40)   VALUE SPACES.
       01  WS-USER         PIC X(40)   VALUE SPACES.
       01  WS-USER1        PIC X(40)   VALUE SPACES.
       01  WS-DOMINIO      PIC X(40)   VALUE SPACES.
       01  AT-COUNT        PIC 9(01)   VALUE ZEROS.
       01  PT-COUNT        PIC 9(01)   VALUE ZEROS.
       01  SP-COUNT        PIC X(40)   VALUE ZEROS.
       01  WS-DOM          PIC X(40)   VALUE SPACES.
       01  WS-DOM-1        PIC X(20)   VALUE SPACES.
       01  WS-DOM-2        PIC X(20)   VALUE SPACES.
       01  WS-DOM-3        PIC X(20)   VALUE SPACES.
       01  WS-DOM-4        PIC X(20)   VALUE SPACES.
       01  WS-DOM-5        PIC X(20)   VALUE SPACES.
       01  WS-DOM-6        PIC X(20)   VALUE SPACES.
.      01  FLAG-CHECK      PIC X(01)   VALUE "N".
           88  FLAG-TRUE               VALUE "S".
           88  FLAG-FALSE              VALUE "N".
       01  WS-DOMINIOS-FILE.
           88  WS-READ-STATUS             VALUE HIGH-VALUES.
           03  WS-DOMINIO-VAL PIC X(20).
           03  WS-FSTATUS     PIC 9(02)   VALUE ZEROS.
       LINKAGE SECTION.
       01  PROG-EMAIL           PIC X(40).
       01  PROG-FLAG            PIC X(01).
       01  MENSAGEM             PIC X(50).
       PROCEDURE DIVISION USING PROG-EMAIL, PROG-FLAG, MENSAGEM.
           MAIN-PROCEDURE.
           SET FLAG-TRUE TO TRUE
           MOVE PROG-EMAIL TO WS-EMAIL
      *************************************************************************
      *    Converts the email string into uppercase                           *
      *************************************************************************
           MOVE FUNCTION UPPER-CASE (WS-EMAIL) TO WS-EMAIL
      *************************************************************************
      *    Removes initial spaces from the string                             *
      *************************************************************************
           MOVE FUNCTION TRIM (WS-EMAIL) TO WS-EMAIL
      *************************************************************************
      *    Checks the existance of one @                                      *
      *************************************************************************
           MOVE ZEROS TO AT-COUNT
           INSPECT WS-EMAIL TALLYING AT-COUNT FOR ALL "@"
                IF AT-COUNT NOT EQUAL 1 THEN
                    SET FLAG-FALSE TO TRUE
                    MOVE FLAG-CHECK TO PROG-FLAG
                    MOVE "The e-mail must have contain a '@'"
                    TO MENSAGEM
                    EXIT PROGRAM
                END-IF
      *************************************************************************
      *    Splits the string into two strings delimited by the @              *
      *************************************************************************
           MOVE SPACES TO WS-USER WS-DOMINIO
           UNSTRING WS-EMAIL DELIMITED BY "@" INTO
               WS-USER
               WS-DOMINIO
      *************************************************************************
      *    Checks if the user string as the valid characters                  *
      *************************************************************************
               IF WS-USER NOT ValidUser THEN
                   MOVE "The e-mail as invalid characters."
                   TO MENSAGEM
                   SET FLAG-FALSE TO TRUE
                   MOVE FLAG-CHECK TO PROG-FLAG
                   EXIT PROGRAM
               END-IF
      *************************************************************************
      *    Checks if the domain string as the valid characters                *
      *************************************************************************
               IF WS-DOMINIO NOT ValidDominio THEN
                   MOVE "The e-mail as invalid characters."
                   TO MENSAGEM
                   SET FLAG-FALSE TO TRUE
                   MOVE FLAG-CHECK TO PROG-FLAG
                   EXIT PROGRAM
               END-IF
      *************************************************************************
      *    Checks if the user starts with a '.'                               *
      *************************************************************************
           MOVE WS-USER TO WS-USER1
           MOVE SPACES TO WS-DOM WS-DOM-1
           UNSTRING WS-USER DELIMITED BY "." INTO WS-DOM WS-DOM-1
               IF WS-DOM EQUALS SPACES THEN
                   MOVE "E-mail should not start with a '.'"
                   TO MENSAGEM
                   SET FLAG-FALSE TO TRUE
                   MOVE FLAG-CHECK TO PROG-FLAG
                   EXIT PROGRAM
               END-IF
      *************************************************************************
      *    Checks if the user ends with a '.'                                 *
      *************************************************************************
           MOVE SPACES TO WS-DOM WS-DOM-1
           MOVE TRIM(REVERSE (WS-USER1)) TO WS-DOM-2
           UNSTRING WS-DOM-2 DELIMITED BY "." INTO WS-DOM WS-DOM-1
                IF WS-DOM EQUALS SPACES THEN
                  MOVE "E-mail username should not end with a '.'"
                   TO MENSAGEM
                   SET FLAG-FALSE TO TRUE
                   MOVE FLAG-CHECK TO PROG-FLAG
                   EXIT PROGRAM
               END-IF
      *************************************************************************
      *    Checks if email as spaces in the string                            *
      *************************************************************************
           MOVE SPACES TO SP-COUNT
           UNSTRING WS-EMAIL DELIMITED BY " " INTO SP-COUNT
               IF SP-COUNT NOT EQUALS WS-EMAIL THEN
                 MOVE "E-mail can't have any spaces."
                   TO MENSAGEM
                   SET FLAG-FALSE TO TRUE
                   MOVE FLAG-CHECK TO PROG-FLAG
                   EXIT PROGRAM
               END-IF
      *************************************************************************
      *    Checks if email string has, at least, a '.' before the domain      *
      *************************************************************************
           MOVE ZEROS TO PT-COUNT
           INSPECT WS-DOMINIO TALLYING PT-COUNT FOR ALL "."
               IF PT-COUNT = 0 THEN
               MOVE "E-mail must have a '.' before domain name."
                   TO MENSAGEM
                   SET FLAG-FALSE TO TRUE
                   MOVE FLAG-CHECK TO PROG-FLAG
                   EXIT PROGRAM
               END-IF
      *************************************************************************
      *    Checks if the domain name is valid, from a file with domain names. *
      *************************************************************************
           MOVE SPACES TO WS-DOM-3 WS-DOM-4 WS-DOM-5
           MOVE REVERSE (WS-DOMINIO) TO WS-DOMINIO
           MOVE FUNCTION TRIM (WS-DOMINIO) TO WS-DOMINIO
           UNSTRING WS-DOMINIO DELIMITED BY "." INTO WS-DOM-5 WS-DOM-6
           MOVE REVERSE (WS-DOM-5) TO WS-DOM-5
           MOVE TRIM (WS-DOM-5) TO WS-DOM-5
           MOVE WS-DOM-5 TO DOMINIO-VAL
           OPEN INPUT DOMINIOS
           READ DOMINIOS
               INVALID KEY
               MOVE "Domain name is not valid."
               TO MENSAGEM
               SET FLAG-FALSE TO TRUE
               MOVE FLAG-CHECK TO PROG-FLAG
               EXIT PROGRAM
           END-READ
           CLOSE DOMINIOS
      *************************************************************************
      *    Checks if it has characters before the '.'.                        *
      *************************************************************************
           IF WS-DOM-6 EQUALS SPACES THEN
               MOVE "Tem de inserir caracteres antes do ponto. [ENTER]"
                   TO MENSAGEM
                   SET FLAG-FALSE TO TRUE
                   MOVE FLAG-CHECK TO PROG-FLAG
                   EXIT PROGRAM
               END-IF
           EXIT PROGRAM.
