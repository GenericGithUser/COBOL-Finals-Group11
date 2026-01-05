       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG3PROB4.
       AUTHOR. GIAN-CARYL-SHEINE-EGIAN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFILE ASSIGN TO "C:\dos\cobol\outfile3.txt".
       DATA DIVISION.
       FILE SECTION.
       FD OUTFILE.
       01 OUTREC.
           05 FILLER PIC X(80).
       WORKING-STORAGE SECTION.
       01 HD01-REC.
           05 FILLER PIC X(28) VALUE SPACES.
           05 FILLER PIC X(25) VALUE 
               "BROWNOUT ELECTRIC COMPANY".
           05 FILLER PIC X(27) VALUE SPACES.
       01 HD02-REC.
           05 FILLER PIC X(33) VALUE SPACES.
           05 FILLER PIC X(14) VALUE "BILLING REPORT".
           05 FILLER PIC X(33) VALUE SPACES.
       01 SPACER.
           05 FILLER PIC X(80) VALUE SPACES.
       01 COLHD1-REC.
           05 FILLER PIC X(1) VALUE SPACES.
           05 FILLER PIC X(11) VALUE "Account No.".
           05 FILLER PIC X(1) VALUE SPACES.
           05 FILLER PIC X(13) VALUE "Customer Name".
           05 FILLER PIC X(4) VALUE SPACES.
           05 FILLER PIC X(12) VALUE "Account Type".
           05 FILLER PIC X(2) VALUE SPACES.
           05 FILLER PIC X(8) VALUE "Kwh Used".
           05 FILLER PIC X(1) VALUE SPACES.
           05 FILLER PIC X(14) VALUE "System Charges".
           05 FILLER PIC X(1) VALUE SPACES.
           05 FILLER PIC X(11) VALUE "Total  Bill".
       01 REC-OUT.
           05 FILLER PIC X(1) VALUE SPACES.
           05 ACC-NO-OUT PIC X(10).
           05 FILLER PIC X(1) VALUE SPACES.
           05 CUS-NAM-OUT PIC X(20).
           05 ACC-TYP-OUT PIC X(11).
           05 KW-USED-OUT PIC Z(6).
           05 FILLER PIC X(2) VALUE SPACES.
           05 SYS-CHAR-OUT PIC Z,ZZZ.ZZ.
           05 FILLER PIC X(4) VALUE SPACES.
           05 TOT-BIL-OUT PIC ZZ,ZZZ.ZZ.
       01 REC-IN.
           05 ACC-NO-IN PIC X(10).
           05 CUS-NAM-IN PIC X(25).
           05 PREV-RED-IN PIC 9(6).
           05 CUR-RED-IN PIC 9(6).
           05 KW-USED-IN PIC 9(6).
           05 ACC-COD-IN PIC A.
           05 ACC-TYP-IN PIC X(11).
           05 AR-COD-IN PIC 9.
           05 SYS-CHARG-IN PIC 9(4)V99. 
           05 HIGH-CUS-KWH PIC X(25).
           05 TOT-BIL-IN PIC 9(7)V99.

       01 DISPLAY-OUT.
           05 PREV-RED-OUT PIC Z(6).
           05 CUR-RED-OUT PIC Z(6).

       01 INIT-FLAGS.
           05 END-PROG PIC 9 VALUE 0.
           05 VALID-FLAG PIC 9 VALUE 0.
           05 ANS PIC X.
           05 CHK-ANS PIC 9 VALUE 0.
           05 CUR-HIGH-KWH PIC 9(6).
           05 REC PIC 9 VALUE 0.
           05 PRICE-PER-KWH PIC 99.
           05 ELEC-BILL PIC 9(6)V99.
           05 SYS-CHAR-PER PIC 9(4)V99.
           
       SCREEN SECTION. 
       01 CLRSCR.
           05 BLANK SCREEN.

       PROCEDURE DIVISION.
           PERFORM MAIN-RTN.
       MAIN-RTN.
           DISPLAY CLRSCR.
           OPEN OUTPUT OUTFILE.
           PERFORM INIT-PRINT-RTN THRU INIT-PRINT-END.
           PERFORM PROCESS-RTN THRU PROCESS-END UNTIL CHK-ANS = 1.
           PERFORM FINISH-RTN THRU FINISH-END.
           STOP RUN.

       INIT-PRINT-RTN.
           WRITE OUTREC FROM HD01-REC.
           WRITE OUTREC FROM HD02-REC AFTER ADVANCING 2 LINES. 
           WRITE OUTREC FROM COLHD1-REC AFTER ADVANCING 1 LINE.
       INIT-PRINT-END.
           EXIT.    

       PROCESS-RTN.
           DISPLAY CLRSCR.
           DISPLAY (1, 1) HD01-REC.
           DISPLAY (2, 1) HD02-REC.
           DISPLAY (3, 1) SPACER.
           DISPLAY (4, 1) "Account Number: ".
           ACCEPT (4, 40) ACC-NO-IN.
           DISPLAY (4, 40) ACC-NO-IN.

           DISPLAY (5, 1) "Customer Name: ".
           ACCEPT (5, 40) CUS-NAM-IN.
           DISPLAY (5, 40) CUS-NAM-IN.

           DISPLAY (6, 1) "Previous Reading: ".
           ACCEPT (6, 40) PREV-RED-IN.
           MOVE PREV-RED-IN TO PREV-RED-OUT.
           DISPLAY (6, 40) PREV-RED-OUT.

           DISPLAY (7, 1) "Current Reading: ".
           ACCEPT (7, 40) CUR-RED-IN.
           MOVE CUR-RED-IN TO CUR-RED-OUT.
           DISPLAY (7, 40) CUR-RED-OUT.

           PERFORM KWH-RTN. 

           DISPLAY (9, 1) "Account Code: ".
           PERFORM ACC-COD-RTN.
           MOVE 0 TO VALID-FLAG.

           DISPLAY (11, 1) "Area Code: "
           PERFORM AR-COD-RTN.
           MOVE 0 TO VALID-FLAG.

           COMPUTE TOT-BIL-IN = ELEC-BILL + SYS-CHARG-IN.
           MOVE TOT-BIL-IN TO TOT-BIL-OUT.
           DISPLAY (13, 1) "Total Bill: ".
           DISPLAY (13, 40) TOT-BIL-OUT.
           
           PERFORM PRINT-RTN THRU PRINT-END.
           DISPLAY (14, 1) "Input Another Record (Y/N)?: ".
           PERFORM ANS-CHK-RTN UNTIL VALID-FLAG = 1.
       PROCESS-END. 
           EXIT.
      
       KWH-RTN.
           COMPUTE KW-USED-IN = CUR-RED-IN - PREV-RED-IN.
           MOVE KW-USED-IN TO KW-USED-OUT.
           DISPLAY (8, 1) "KwH Used: "
           DISPLAY (8, 40) KW-USED-OUT.

           IF REC = 0
               ADD 1 TO REC
               MOVE KW-USED-IN TO CUR-HIGH-KWH
               MOVE CUS-NAM-IN TO HIGH-CUS-KWH
           ELSE
               ADD 1 TO REC
               IF KW-USED-IN GREATER THAN CUR-HIGH-KWH
                   MOVE KW-USED-IN TO CUR-HIGH-KWH
                   MOVE CUS-NAM-IN TO HIGH-CUS-KWH
               END-IF
           END-IF.

       KWH-END.
           EXIT.


       ACC-COD-RTN.
           ACCEPT (9, 40) ACC-COD-IN.
           
           EVALUATE ACC-COD-IN
               WHEN "R"
               WHEN "r"
                   MOVE 1 TO VALID-FLAG
                   MOVE "Residential" TO ACC-TYP-IN
                   MOVE 14 TO PRICE-PER-KWH
              WHEN "C"
              WHEN "c"
                  MOVE 1 TO VALID-FLAG
                  MOVE "Commercial" TO ACC-TYP-IN
                  MOVE 28 TO PRICE-PER-KWH
              WHEN "I"
              WHEN "i"
                  MOVE 1 TO VALID-FLAG
                  MOVE "Industrial" TO ACC-TYP-IN
                  MOVE 42 TO PRICE-PER-KWH
              WHEN OTHER
                  MOVE 0 TO VALID-FLAG
           END-EVALUATE.

           IF VALID-FLAG = 1
               DISPLAY (9, 40) ACC-COD-IN
               DISPLAY (10, 1) "Account Type: "
               DISPLAY (10, 40) ACC-TYP-IN

               COMPUTE ELEC-BILL = KW-USED-IN * PRICE-PER-KWH
           ELSE 
               PERFORM ACC-COD-RTN THRU ACC-COD-END 
                   UNTIL VALID-FLAG = 1
           END-IF.
       ACC-COD-END.
           EXIT.

       AR-COD-RTN.
           ACCEPT (11, 40) AR-COD-IN.

           EVALUATE AR-COD-IN
               WHEN 1
                   MOVE 1 TO VALID-FLAG
                   MOVE 0.03 TO SYS-CHAR-PER
               WHEN 2 
                   MOVE 1 TO VALID-FLAG
                   MOVE 0.05 TO SYS-CHAR-PER
               WHEN 3
                   MOVE 1 TO VALID-FLAG
                   MOVE 0.07 TO SYS-CHAR-PER
               WHEN OTHER
                    MOVE 0 TO VALID-FLAG
           END-EVALUATE.

           IF VALID-FLAG = 1
               DISPLAY (11, 40) AR-COD-IN
               COMPUTE SYS-CHARG-IN = ELEC-BILL * SYS-CHAR-PER
               MOVE SYS-CHARG-IN TO SYS-CHAR-OUT
               DISPLAY (12, 1) "System Charges: "
               DISPLAY (12, 40) SYS-CHAR-OUT
           ELSE
               PERFORM AR-COD-RTN THRU AR-COD-END UNTIL VALID-FLAG = 1
           END-IF.         
       AR-COD-END.
           EXIT.

       ANS-CHK-RTN.
           ACCEPT (14, 40) ANS.
           EVALUATE ANS
               WHEN "Y" 
               WHEN "y"
                   MOVE 1 TO VALID-FLAG
                   PERFORM PROCESS-RTN THRU PROCESS-END
               WHEN "N" 
               WHEN "n"
                   MOVE 1 TO VALID-FLAG
                   MOVE 1 TO CHK-ANS
               WHEN OTHER 
                   MOVE 0 TO VALID-FLAG    
           END-EVALUATE.
       ANS-CHK-END.
           EXIT.    
       
       PRINT-RTN.
           MOVE ACC-NO-IN TO ACC-NO-OUT.
           MOVE CUS-NAM-IN TO CUS-NAM-OUT.
           MOVE ACC-TYP-IN TO ACC-TYP-OUT.
           
           WRITE OUTREC FROM REC-OUT AFTER ADVANCING 1 LINE.
       PRINT-END.
           EXIT.

       FINISH-RTN.
           DISPLAY (14, 1) 
               "Customer with the highest number of Kwh Used: ".
           DISPLAY (14, 47) HIGH-CUS-KWH.

           CLOSE OUTFILE.    
       FINISH-END.
           EXIT.
