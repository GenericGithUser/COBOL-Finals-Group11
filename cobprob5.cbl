       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG3PROB5.
       AUTHOR. GIAN-CARYL-SHEINE-EGIAN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFILE ASSIGN TO "C:\dos\cobol\outfile4.txt".
       DATA DIVISION.
       FILE SECTION.
       FD OUTFILE.
       01 OUTREC.
           05 FILLER PIC X(80).
       WORKING-STORAGE SECTION.
       01 HD01-REC.
           05 FILLER PIC X(23) VALUE SPACES.
           05 FILLER PIC X(34) VALUE 
               "Professional Regulation Commission".
           05 FILLER PIC X(23) VALUE SPACES.
       01 HD02-REC.
           05 FILLER PIC X(24) VALUE SPACES.
           05 FILLER PIC X(33) VALUE 
               "IT Professional Board Exam Result".
           05 FILLER PIC X(23) VALUE SPACES.
       01 SPACER.
           05 FILLER PIC X(80) VALUE SPACES.
       01 COLHD1-REC.
           05 FILLER PIC X(5) VALUE SPACES.
           05 FILLER PIC X(8) VALUE "Examinee".
           05 FILLER PIC X(3) VALUE SPACES.
           05 FILLER PIC X(8) VALUE "Examinee".
           05 FILLER PIC X(7) VALUE SPACES.
           05 FILLER PIC X(13) VALUE "Date of Birth".
           05 FILLER PIC X(8) VALUE SPACES.
           05 FILLER PIC X(10) VALUE "University".
           05 FILLER PIC X(4) VALUE SPACES.
           05 FILLER PIC X(6) VALUE "Course".
           05 FILLER PIC X(4) VALUE SPACES.
           05 FILLER PIC X(7) VALUE "Remarks".
           
       01 COLHD2-REC.
           05 FILLER PIC X(7) VALUE SPACES.
           05 FILLER PIC X(3) VALUE "No.".
           05 FILLER PIC X(8) VALUE SPACES.
           05 FILLER PIC X(4) VALUE "Name".
           05 FILLER PIC X(30) VALUE SPACES.
           05 FILLER PIC X(4) VALUE "Name".
           05 FILLER PIC X(5) VALUE SPACES.
           05 FILLER PIC X(8) VALUE "Name".
       01 REC-OUT.
           05 FILLER PIC X(1) VALUE SPACES.
           05 EXA-NO-OUT PIC 9(10).
           05 FILLER PIC X(3) VALUE SPACES.
           05 EXA-NAM-OUT PIC X(20).
           05 DOB-OUT PIC X(20).
           05 UNAME-OUT PIC X(5).
           05 C-NAME-OUT PIC X(4).
           05 FILLER PIC X(3) VALUE SPACES.
           05 REMARK-OUT PIC X(6).
       01 TOTPAS.
           05 FILLER PIC X(21) VALUE "TOTAL NO. OF PASSED: ".
           05 TOTPAS-OUT PIC 99.    
       01 TOTFAL.
           05 FILLER PIC X(21) VALUE "TOTAL NO. OF FAILED: ".
           05 TOTFAL-OUT PIC 99.
       01 REC-IN.
           05 EXA-NO-IN PIC 9(10).
           05 EXA-NAM-IN PIC X(20).
           05 DOB-IN PIC X(20).
           05 UCODE-IN PIC 9.
           05 UNAME-IN PIC X(5).
           05 C-CODE-IN PIC 9.
           05 C-NAME-IN PIC X(4).
           05 TOT-NO-ITEM-IN PIC 9(3).
           05 TEST-REST-IN PIC 99.
           05 REMARK-IN PIC X(6).

       01 INIT-FLAGS.
           05 END-PROG PIC 9 VALUE 0.
           05 VALID-FLAG PIC 9 VALUE 0.
           05 ANS PIC X.
           05 CHK-ANS PIC 9 VALUE 0.
           05 PASS-PER PIC 9(3)V99.
           05 PASS-SCORE PIC 9(3).
           05 TP-CTR PIC 99 VALUE 0.
           05 TF-CTR PIC 99 VALUE 0.
           
       SCREEN SECTION. 
       01 CLRSCR.
           05 BLANK SCREEN.        

       PROCEDURE DIVISION.
           PERFORM MAIN-RTN.

       MAIN-RTN.
           DISPLAY CLRSCR
           OPEN OUTPUT OUTFILE.
           PERFORM INIT-PRINT-RTN THRU INIT-PRINT-END.
           PERFORM PROCESS-RTN THRU PROCESS-END UNTIL CHK-ANS = 1.
           PERFORM FINISH-RTN THRU FINISH-END.
           STOP RUN.

       INIT-PRINT-RTN.
           WRITE OUTREC FROM HD01-REC.
           WRITE OUTREC FROM HD02-REC AFTER ADVANCING 2 LINES. 
           WRITE OUTREC FROM COLHD1-REC AFTER ADVANCING 1 LINE.
           WRITE OUTREC FROM COLHD2-REC AFTER ADVANCING 1 LINE.
       INIT-PRINT-END.
           EXIT.       

       PROCESS-RTN.
           DISPLAY CLRSCR.
           DISPLAY (1, 1) HD01-REC.
           DISPLAY (2, 1) HD02-REC.
           DISPLAY (3, 1) SPACER.
           DISPLAY (4, 1) "Examinee Number: ".
           ACCEPT (4, 40) EXA-NO-IN.
           DISPLAY (4, 40) EXA-NO-IN.
           DISPLAY (5, 1) "Examinee Name: ".
           ACCEPT (5, 40) EXA-NAM-IN.
           DISPLAY (5, 40) EXA-NAM-IN.
           DISPLAY (6, 1) "Date of Birth: ".
           ACCEPT (6, 40) DOB-IN.
           DISPLAY (6, 40) DOB-IN.

           DISPLAY (7, 1) "University Code: ".
           PERFORM U-COD-RTN.
           MOVE 0 TO VALID-FLAG.

           DISPLAY (9, 1) "Course Code: ".
           PERFORM C-CODE-RTN.
           MOVE 0 TO VALID-FLAG.

           DISPLAY (11, 1) "Total No. of Items: ".
           ACCEPT (11, 40) TOT-NO-ITEM-IN.
           DISPLAY (11, 40) TOT-NO-ITEM-IN.

           DISPLAY (12, 1) "Test Result (Score): ".
           ACCEPT (12, 40) TEST-REST-IN.

           PERFORM REMARK-RTN.
           PERFORM PRINT-RTN THRU PRINT-END.
           DISPLAY (14, 1) "Input Another Record (Y/N)?".
           PERFORM ANS-CHK-RTN UNTIL VALID-FLAG = 1.
       PROCESS-END.
           EXIT.
       
       U-COD-RTN.
           ACCEPT (7, 40) UCODE-IN.

           EVALUATE UCODE-IN
               WHEN 1
                   MOVE 1 TO VALID-FLAG
                   MOVE "UP" TO UNAME-IN
               WHEN 2
                   MOVE 1 TO VALID-FLAG
                   MOVE "PUP" TO UNAME-IN
               WHEN 3
                   MOVE 1 TO VALID-FLAG
                   MOVE "DLSU" TO UNAME-IN
               WHEN 4
                   MOVE 1 TO VALID-FLAG
                   MOVE "ADMU" TO UNAME-IN
               WHEN 5
                   MOVE 1 TO VALID-FLAG
                   MOVE "MAPUA" TO UNAME-IN
               WHEN OTHER
                   MOVE 0 TO VALID-FLAG
           END-EVALUATE.

           IF VALID-FLAG = 1
               DISPLAY (7, 40) UCODE-IN
               DISPLAY (8, 1) "University Name: "
               DISPLAY (8, 40) UNAME-IN 
           ELSE
               PERFORM U-COD-RTN THRU U-COD-END UNTIL VALID-FLAG = 1
           END-IF.        
       U-COD-END.
           EXIT.
       
       C-CODE-RTN.
           ACCEPT (9, 40) C-CODE-IN.

           EVALUATE C-CODE-IN
               WHEN 1 
                   MOVE 1 TO VALID-FLAG
                   MOVE "BSIT" TO C-NAME-IN
                   MOVE 0.60 TO PASS-PER
               WHEN 2 
                   MOVE 1 TO VALID-FLAG
                   MOVE "BSCS" TO C-NAME-IN
                   MOVE 0.70 TO PASS-PER
               WHEN 3 
                   MOVE 1 TO VALID-FLAG
                   MOVE "BSIS" TO C-NAME-IN
                   MOVE 0.50 TO PASS-PER
               WHEN OTHER
                   MOVE 0 TO VALID-FLAG
                   MOVE 0 TO PASS-PER
           END-EVALUATE.

           IF VALID-FLAG = 1
               DISPLAY (9, 40) C-CODE-IN
               DISPLAY (10, 1) "Course Name: "
               DISPLAY (10, 40) C-NAME-IN
           ELSE
               PERFORM C-CODE-RTN THRU C-CODE-END UNTIL VALID-FLAG = 1
           END-IF.         
       C-CODE-END.
           EXIT.

       REMARK-RTN.
           COMPUTE PASS-SCORE = TOT-NO-ITEM-IN * PASS-PER. 
           IF TEST-REST-IN >= PASS-SCORE
               MOVE "PASSED" TO REMARK-IN
               ADD 1 TO TP-CTR
           ELSE 
               MOVE "FAILED" TO REMARK-IN
               ADD 1 TO TF-CTR
           END-IF.
           DISPLAY (13, 1) "Remarks: ".
           DISPLAY (13, 40) REMARK-IN.    
       REMARK-END.
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
           MOVE EXA-NO-IN TO EXA-NO-OUT.
           MOVE EXA-NAM-IN TO EXA-NAM-OUT.
           MOVE DOB-IN TO DOB-OUT.
           MOVE UNAME-IN TO UNAME-OUT.
           MOVE C-NAME-IN TO C-NAME-OUT.
           MOVE REMARK-IN TO REMARK-OUT.    
           
           WRITE OUTREC FROM REC-OUT AFTER ADVANCING 1 LINE.
       PRINT-END.
           EXIT.    

       FINISH-RTN.
           MOVE TP-CTR TO TOTPAS-OUT.
           DISPLAY (15, 1) TOTPAS.
           MOVE TF-CTR TO TOTFAL-OUT.
           DISPLAY (16, 1) TOTFAL.

           CLOSE OUTFILE.
       FINISH-END.
           EXIT.