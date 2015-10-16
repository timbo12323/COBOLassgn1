       IDENTIFICATION DIVISION.
       PROGRAM-ID.       TEMPLATE.
       AUTHOR.           TIM CARROLL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F01-EMPLOYEE-FILE ASSIGN TO 'ASST1.DAT'
                                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT F02-PRINT-FILE   ASSIGN TO 'ASST1.OUT'
                                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
      * This is the definition of the input file
       FD  F01-EMPLOYEE-FILE
           RECORD CONTAINS 30 CHARACTERS
           DATA RECORD IS F01-EMPLOYEE-IN.
       01  F01-EMPLOYEE-IN.
           05  F01-EMP-NAME        PIC X(18).
           05  F01-SSN             PIC 9(9).
           05  F01-GROSS-PAY       PIC 9(3).

      * The definition of the output file
       FD  F02-PRINT-FILE
           RECORD CONTAINS 71 CHARACTERS
           DATA RECORD IS F02-PRINT-LINE-RECORD.
       01  F02-PRINT-LINE-RECORD   PIC X(71).
       WORKING-STORAGE SECTION.
       01  W01-DATA-REMAINS-SWITCH PIC X(2)    VALUE SPACES.
      * need to add up to 71
       01  W02-TITLE-LINE.
           05                      PIC X(17)   VALUE SPACES.   
           05                      PIC X(33)   VALUE 'Tim Carroll by COBOL ASSIGNMENT 1'.     
           05                      PIC X(17)   VALUE SPACES.
       
       01  W03-HEADINGS-LINE.
           05                      PIC X(2)    VALUE SPACES.
           05                      PIC X(13)   VALUE 'EMPLOYEE NAME'.
           05                      PIC X(20)   VALUE SPACES.
           05                      PIC X(4)    VALUE '$100'.
           05                      PIC X(3)    VALUE SPACES.
           05                      PIC X(3)    VALUE '$50'.
           05                      PIC X(2)    VALUE SPACES.
           05                      PIC X(3)    VALUE '$20'.
           05                      PIC X(2)    VALUE SPACES.
           05                      PIC X(3)    VALUE '$10'.
           05                      PIC X(3)    VALUE SPACES.
           05                      PIC X(2)    VALUE '$5'.
           05                      PIC X(3)    VALUE SPACES.
           05                      PIC X(2)    VALUE '$1'.
           05                      PIC X(3)    VALUE SPACES.
           05                      PIC X(3)    VALUE 'PAY'.
     
           
       01  WO4-DETAIL-LINE.
           05                      PIC X(2)    VALUE SPACES.
           05  W04-EMP-NAME        PIC X(18).
           05                      PIC X(2)    VALUE SPACES.
           05  W04-EMP-SSN         PIC 9(9).
           05                      PIC X(7)    VALUE SPACES.
           05  W04-PRINT-100S      PIC 9.
           05                      PIC X(5)    VALUE SPACES.
           05  W04-PRINT-50S       PIC 9.
           05                      PIC X(4)    VALUE SPACES.
           05  W04-PRINT-20S       PIC 9.
           05                      PIC X(4)    VALUE SPACES.
           05  W04-PRINT-10S       PIC 9.
           05                      PIC X(4)    VALUE SPACES.
           05  W04-PRINT-5S        PIC 9.
           05                      PIC X(4)    VALUE SPACES.
           05  W04-PRINT-1S        PIC 9.
           05                      PIC X(3)    VALUE SPACES.
           05  W04-EMP-PAY         PIC 9(3).
           05  W04-CALC-TEMP       PIC V99.
       
       
       01  W05-FOOTER-LINE .
           05                      PIC X(13)   VALUE 'End of Report'.
           05                      PIC X(54)    VALUE SPACES.
       
       PROCEDURE DIVISION.
       
       PERFORM 100-OPEN-FILES
       PERFORM 200-WRITE-HEADING-LINES
       PERFORM 300-PROCESS-RECORDS
               UNTIL W01-DATA-REMAINS-SWITCH = 'NO'
       PERFORM 400-WRITE-FOOTER
       PERFORM 500-CLOSE-FILES
       .
           

      * Start of OPEN-FILES paragraph.
       100-OPEN-FILES.
           OPEN INPUT F01-EMPLOYEE-FILE
               OUTPUT F02-PRINT-FILE
      * Prime read
           READ F01-EMPLOYEE-FILE
               AT END MOVE 'NO' TO W01-DATA-REMAINS-SWITCH
           END-READ
           .
      * End of OPEN-FILES paragraph.
        
      * This paragraph writes the headings for the report.  
       200-WRITE-HEADING-LINES.
           MOVE W02-TITLE-LINE TO F02-PRINT-LINE-RECORD
           WRITE F02-PRINT-LINE-RECORD
           MOVE W03-HEADINGS-LINE TO F02-PRINT-LINE-RECORD
           WRITE F02-PRINT-LINE-RECORD
           .
      * End of WRITE-HEADING-LINES paragraph.
      
      * Start of PROCESS-RECORDS paragraph which writes the data.
       300-PROCESS-RECORDS.
           MOVE F01-EMP-NAME TO W04-EMP-NAME
           MOVE F01-SSN TO W04-EMP-SSN
           MOVE F01-GROSS-PAY TO W04-EMP-PAY
           PERFORM 310-DO-CALCULATIONS
           MOVE WO4-DETAIL-LINE TO F02-PRINT-LINE-RECORD
           WRITE F02-PRINT-LINE-RECORD
           READ F01-EMPLOYEE-FILE
               AT END MOVE 'NO' TO W01-DATA-REMAINS-SWITCH
           END-READ
           .
      * End of PROCESS-RECORDS paragraph.
        
      * This paragraph to determain the number of bills distributed to employee.  
       310-DO-CALCULATIONS.
      * # of $100 bills.
           COMPUTE W04-PRINT-100S = F01-GROSS-PAY / 100.
      * # of $50 bills.     
           COMPUTE W04-CALC-TEMP = F01-GROSS-PAY / 100. 
           COMPUTE W04-PRINT-50S = W04-CALC-TEMP * 100 / 50.
           COMPUTE F01-GROSS-PAY = W04-PRINT-50S * 50.
      * # of $20 bills.
           COMPUTE W04-CALC-TEMP = F01-GROSS-PAY / 100 - W04-CALC-TEMP.
           COMPUTE W04-PRINT-20S = W04-CALC-TEMP * 100 / 20.
           COMPUTE F01-GROSS-PAY = W04-PRINT-20S * 20.
      * # of $10 bills.     
           COMPUTE W04-CALC-TEMP = F01-GROSS-PAY / 100 - W04-CALC-TEMP.
           COMPUTE W04-PRINT-10S = W04-CALC-TEMP * 100 / 10.
           COMPUTE F01-GROSS-PAY = W04-PRINT-10S * 10.
      * # of $5 bills.     
           COMPUTE W04-CALC-TEMP = F01-GROSS-PAY / 100 - W04-CALC-TEMP.
           COMPUTE W04-PRINT-5S = W04-CALC-TEMP * 100 / 5.
           COMPUTE F01-GROSS-PAY = W04-PRINT-5S * 5.
      * # of $1 coins or bills.     
           COMPUTE W04-CALC-TEMP = F01-GROSS-PAY / 100 - W04-CALC-TEMP.
           COMPUTE W04-PRINT-1S = W04-CALC-TEMP * 100 / 1.
           COMPUTE F01-GROSS-PAY = W04-PRINT-1S * 1.
  
      * End of DO-CALCULATIONS paragraph. 
       
      * Paragraph that writes 'End of Report'. 
       400-WRITE-FOOTER.
           MOVE W05-FOOTER-LINE TO F02-PRINT-LINE-RECORD
           WRITE F02-PRINT-LINE-RECORD
           .
      * End of WRITE-FOOTER paragraph. 
          
       
      * Closing files 
       500-CLOSE-FILES.
           CLOSE F01-EMPLOYEE-FILE
                 F02-PRINT-FILE
           STOP RUN
           .
      * End of CLOSE-FILES paragraph.


