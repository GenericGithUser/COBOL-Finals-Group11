       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG3PROB3.
       AUTHOR. GIAN-CARYL-SHEINE-EGIAN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFILE ASSIGN TO "C:\dos\cobol\outfile2.txt".
       DATA DIVISION.
       FILE SECTION.
       FD OUTFILE.
       01 OUTREC.
           05 FILLER PIC X(80).
       WORKING-STORAGE SECTION.
       01 HD01-REC.
           05 FILLER PIC X(32) VALUE SPACES.
           05 FILLER PIC X(16) VALUE 
               "China Trust Bank".
           05 FILLER PIC X(32) VALUE SPACES.
       01 HD02-REC.
           05 FILLER PIC X(35) VALUE SPACES.
           05 FILLER PIC X(11) VALUE "Makati City".
           05 FILLER PIC X(34) VALUE SPACES.
       01 HD03-REC.
           05 FILLER PIC X(30) VALUE SPACES.
           05 FILLER PIC X(20) VALUE "Customer’s Account".
           05 FILLER PIC X(30) VALUE SPACES.
       01 SPACER.
           05 FILLER PIC X(80) VALUE SPACES.
       01 COLHD1-REC.
           05 FILLER PIC X(2) VALUE SPACES.
           05 FILLER PIC X(11) VALUE "Account No.".
           05 FILLER PIC X(2) VALUE SPACES.
           05 FILLER PIC X(12) VALUE "Account Name".
           05 FILLER PIC X(5) VALUE SPACES.
           05 FILLER PIC X(16) VALUE "Transaction Name".
           05 FILLER PIC X(5) VALUE SPACES.
           05 FILLER PIC X(17) VALUE "Account Type Name".
           05 FILLER PIC X(3) VALUE SPACES.
           05 FILLER PIC X(7) VALUE "Balance".
       01 REC-OUT.
           05 ACC-NO-OUT PIC 9(10).
           05 FILLER PIC X(3) VALUE SPACES.
           05 ACC-NAME-OUT PIC X(25).
           05 TRANSAC-NAME-OUT PIC X(12).
           05 FILLER PIC X(4) VALUE SPACES.
           05 ACC-TYP-NAM-OUT PIC X(10).
           05 BALANCE-OUT PIC ZZZ,ZZZ,ZZ9.99.
      
       01 REC-IN.
           05 ACC-NO-IN PIC 9(10).
           05 ACC-NAME-IN PIC X(25).
           05 GEN-CODE-IN PIC X.
           05 GEN-NAME-IN PIC X(6).
           05 TRANSAC-TYPE-IN PIC X.
           05 TRANSAC-NAME-IN PIC X(12).
           05 AMO-IN PIC X(9).
           05 AMO-ACT PIC 9(7)V99. 
           05 ACC-TYP-IN PIC X.
           05 ACC-TYP-NAM-IN PIC X(15).
           05 INI-DEP-IN PIC X(9).
           05 INI-DEP-ACT PIC 9(7)V99. 
           05 BALANCE-IN PIC 9(9)V99.
           05 BR-COD-IN PIC X(3).
           05 BR-NAME-IN PIC X(15).

       01 DISPLAY-REC.
           05 AMO-OUT PIC Z,ZZZ,ZZ9.99.
           05 INI-DEP-OUT PIC Z,ZZZ,ZZ9.99.
           05 BR-COD-OUT PIC X(3).

       01 INIT-FLAGS.
           05 VALID-FLAG PIC 9 VALUE 0.
           05 ANS PIC X.
           05 CHK-ANS PIC 9 VALUE 0.
           
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
           WRITE OUTREC FROM HD02-REC AFTER ADVANCING 1 LINE. 
           WRITE OUTREC FROM HD03-REC AFTER ADVANCING 2 LINES.
           WRITE OUTREC FROM COLHD1-REC AFTER ADVANCING 1 LINE.
       INIT-PRINT-END.
           EXIT.

       PROCESS-RTN.
           DISPLAY CLRSCR.
           DISPLAY (1, 1) HD01-REC.
           DISPLAY (2, 1) HD02-REC.
           DISPLAY (3, 1) HD03-REC.
           DISPLAY(4, 1) SPACER.
           DISPLAY (5, 1) "Account Number: ".
           ACCEPT (5, 40) ACC-NO-IN.
           DISPLAY (5, 40) ACC-NO-IN.
           DISPLAY (6, 1) "Account Name: ".
           ACCEPT (6, 40) ACC-NAME-IN.
           DISPLAY (6, 40) ACC-NAME-IN.

           DISPLAY (7, 1) "Gender Code: ".
           PERFORM GEND-RTN.
           MOVE 0 TO VALID-FLAG.

           DISPLAY (9, 1) "Transaction Type: "
           PERFORM TRANS-RTN.
           MOVE 0 TO VALID-FLAG.

           DISPLAY (11, 1) "Amount: "
           ACCEPT (11, 40) AMO-IN.
           MOVE FUNCTION NUMVAL(AMO-IN) TO AMO-ACT.
           MOVE AMO-ACT TO AMO-OUT.
           DISPLAY (11, 40) AMO-OUT.

           DISPLAY (12, 1) "Account Type: ".
           PERFORM ACC-TYP-RTN.
           MOVE 0 TO VALID-FLAG.

           DISPLAY (14, 1) "Initial Deposit: ".
           ACCEPT (14, 40) INI-DEP-IN.
           MOVE FUNCTION NUMVAL(INI-DEP-IN) TO INI-DEP-ACT.
           MOVE INI-DEP-ACT TO INI-DEP-OUT.
           DISPLAY (14, 40) INI-DEP-OUT.

           DISPLAY (15, 1) "Balance: ".
           PERFORM BAL-RTN THRU BAL-END.
           
           DISPLAY (16, 1) "Branch Code: ".
           PERFORM BRN-RTN.
           MOVE 0 TO VALID-FLAG.
           
           PERFORM PRINT-RTN THRU PRINT-END.
           DISPLAY (18, 1) "INPUT ANOTHER RECORD(Y/N)?: "
           PERFORM ANS-CHK-RTN UNTIL VALID-FLAG = 1.
           MOVE 0 TO VALID-FLAG.
       PROCESS-END.
           EXIT.

       GEND-RTN.
           ACCEPT (7, 40) GEN-CODE-IN.
           EVALUATE GEN-CODE-IN
               WHEN "M"
               WHEN "m"
                   MOVE 1 TO VALID-FLAG
                   MOVE "Male" TO GEN-NAME-IN
               WHEN "F"
               WHEN "f"
                   MOVE 1 TO VALID-FLAG
                   MOVE "Female" TO GEN-NAME-IN
               WHEN OTHER
                   MOVE 0 TO VALID-FLAG
           END-EVALUATE.
           
           IF VALID-FLAG = 1
               DISPLAY (7, 40) GEN-CODE-IN
               DISPLAY (8, 1) "Gender Name: "
               DISPLAY (8, 40) GEN-NAME-IN
           ELSE
               PERFORM GEND-RTN THRU GEND-END UNTIL VALID-FLAG = 1
           END-IF.    
       GEND-END.
           EXIT.

       TRANS-RTN.
           ACCEPT (9, 40) TRANSAC-TYPE-IN.
           EVALUATE TRANSAC-TYPE-IN
               WHEN "D"
               WHEN "d"
                   MOVE 1 TO VALID-FLAG
                   MOVE "Deposit" TO TRANSAC-NAME-IN
               WHEN "W"
               WHEN "w"
                   MOVE 1 TO VALID-FLAG
                   MOVE "Withdrawal" TO TRANSAC-NAME-IN
              WHEN OTHER
                   MOVE 0 TO VALID-FLAG
           END-EVALUATE.

           IF VALID-FLAG = 1
               DISPLAY (9, 40) TRANSAC-TYPE-IN
               DISPLAY (10, 1) "Transaction Name: "
               DISPLAY (10, 40) TRANSAC-NAME-IN
           ELSE 
               PERFORM TRANS-RTN THRU TRANS-END UNTIL VALID-FLAG = 1
           END-IF.    
       TRANS-END.
           EXIT.

       ACC-TYP-RTN.
           ACCEPT (12, 40) ACC-TYP-IN.
           EVALUATE ACC-TYP-IN
               WHEN "S"
               WHEN "s"
                   MOVE 1 TO VALID-FLAG
                   MOVE "Savings" TO ACC-TYP-NAM-IN
               WHEN "C"
               WHEN "c"
                   MOVE 1 TO VALID-FLAG
                   MOVE "Checking" TO ACC-TYP-NAM-IN
               WHEN "D"
               WHEN "d"
                   MOVE 1 TO VALID-FLAG
                   MOVE "Dollar" TO ACC-TYP-NAM-IN
               WHEN OTHER
                  MOVE 0 TO VALID-FLAG
           END-EVALUATE.

           IF VALID-FLAG = 1
               DISPLAY (12, 40) ACC-TYP-IN
               DISPLAY (13, 1) "Account Type Name: "
               DISPLAY (13, 40) ACC-TYP-NAM-IN
           ELSE 
               PERFORM ACC-TYP-RTN THRU ACC-TYP-END
                   UNTIL VALID-FLAG = 1
           END-IF.
       ACC-TYP-END.   
           EXIT.

       BAL-RTN.
           IF TRANSAC-TYPE-IN = "W" OR TRANSAC-TYPE-IN = "w"
               IF INI-DEP-ACT LESS THAN AMO-ACT
                   MOVE INI-DEP-ACT TO BALANCE-IN
                   DISPLAY (15, 60) "Transaction Failed"
               ELSE
                   COMPUTE BALANCE-IN = INI-DEP-ACT - AMO-ACT
               END-IF
           ELSE 
               COMPUTE BALANCE-IN = INI-DEP-ACT + AMO-ACT
           END-IF.    
           MOVE BALANCE-IN TO BALANCE-OUT.
           DISPLAY (15, 40) BALANCE-OUT.    
       BAL-END.
           EXIT.
       
       BRN-RTN.
           ACCEPT (16, 40) BR-COD-IN.
           MOVE FUNCTION UPPER-CASE(BR-COD-IN) TO BR-COD-OUT.
           EVALUATE BR-COD-OUT
               WHEN "PAR"
                   MOVE 1 TO VALID-FLAG
                   MOVE "Paranaque" TO BR-NAME-IN
               WHEN "PAS"
                   MOVE 1 TO VALID-FLAG
                   MOVE "Pasay" TO BR-NAME-IN
               WHEN "MAN" 
                   MOVE 1 TO VALID-FLAG
                   MOVE "Mandaluyong" TO BR-NAME-IN
               WHEN "SME"
                   MOVE 1 TO VALID-FLAG
                   MOVE "Sta. Mesa" TO BR-NAME-IN
               WHEN "SJA"
                   MOVE 1 TO VALID-FLAG
                   MOVE "San Juan" TO BR-NAME-IN
               WHEN OTHER
                   MOVE 0 TO VALID-FLAG
           END-EVALUATE.

           IF VALID-FLAG = 1
               DISPLAY (16, 40) BR-COD-OUT
               DISPLAY (17, 1) "Branch Name: "
               DISPLAY (17, 40) BR-NAME-IN
           ELSE
               PERFORM BRN-RTN THRU BRN-END UNTIL VALID-FLAG = 1
           END-IF.       
       BRN-END.
           EXIT.

       ANS-CHK-RTN.
           ACCEPT (18, 40) ANS.
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
           MOVE ACC-NAME-IN TO ACC-NAME-OUT.
           MOVE TRANSAC-NAME-IN TO TRANSAC-NAME-OUT.
           MOVE ACC-TYP-NAM-IN TO ACC-TYP-NAM-OUT.
           DISPLAY (18, 1) REC-OUT.
           WRITE OUTREC FROM REC-OUT AFTER ADVANCING 1 LINE.
       PRINT-END.
           EXIT.

       FINISH-RTN.
           CLOSE OUTFILE.
       FINISH-END.
           EXIT.