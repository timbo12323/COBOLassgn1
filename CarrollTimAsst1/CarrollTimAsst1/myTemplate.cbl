       IDENTIFICATION DIVISION.
       PROGRAM-ID.       TEMPLATE.
       AUTHOR.           TIM CARROLL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F01-STUDENT-FILE ASSIGN TO 'SENIOR.DAT'
                                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT F02-PRINT-FILE   ASSIGN TO 'SENIOR.OUT'
                                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  F01-STUDENT-FILE
           RECORD CONTAINS 43 CHARACTERS
           DATA RECORD IS F01-STUDENT-RECORD.


       01  F01-STUDENT-RECORD.
           05  F01-STU-NAME            PIC X(25).
           05  F01-STU-CREDITS         PIC 9(3).
           05  F01-STU-MAJOR           PIC X(15).

      
       FD  F02-PRINT-FILE
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS F02-PRINT-LINE-RECORD.
       01  F02-PRINT-LINE-RECORD       PIC X(132).

       WORKING-STORAGE SECTION.
       01  W01-DATA-REMAINS-SWITCH PIC X(2)    VALUE SPACES.

       PROCEDURE DIVISION.
         
           STOP RUN
           .