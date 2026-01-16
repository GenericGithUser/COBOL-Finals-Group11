       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG3PROB2.
       AUTHOR. GIAN-CARYL-SHEINE-EGIAN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFILE ASSIGN TO "C:\dos\cobol\outfile.txt".
       DATA DIVISION.
       FILE SECTION.
       FD OUTFILE.
       01 OUTREC.
           05 FILLER PIC X(80).
       WORKING-STORAGE SECTION.
       01 HD01-REC.
           05 FILLER PIC X(19) VALUE SPACES.
           05 FILLER PIC X(41) VALUE 
               "Polytechnic University of the Philippines".
           05 FILLER PIC X(20) VALUE SPACES.
       01 HD02-REC.
           05 FILLER PIC X(31) VALUE SPACES.
           05 FILLER PIC X(17) VALUE "Sta. Mesa, Manila".
           05 FILLER PIC X(32) VALUE SPACES.
       01 HD03-REC.
           05 FILLER PIC X(24) VALUE SPACES.
           05 FILLER PIC X(32) VALUE "Student's Statement of Account".
           05 FILLER PIC X(24) VALUE SPACES.
       01 SPACER.
           05 FILLER PIC X(80) VALUE SPACES.
       01 COLHD1-REC.
           05 FILLER PIC X(2) VALUE SPACES.
           05 FILLER PIC X(11) VALUE "Student No.".
           05 FILLER PIC X(2) VALUE SPACES.
           05 FILLER PIC X(12) VALUE "Student Name".
           05 FILLER PIC X(3) VALUE SPACES.
           05 FILLER PIC X(21) VALUE "Student Type Name".
           05 FILLER PIC X(1) VALUE SPACES.
           05 FILLER PIC X(4) VALUE "Year".
           05 FILLER PIC X(3) VALUE SPACES.
           05 FILLER PIC X(20) VALUE "Total Amount of Fees".
       01 REC-OUT.
           05 FILLER PIC X(2) VALUE SPACES.
           05 SNO-OUT PIC 9(10).
           05 FILLER PIC X(3) VALUE SPACES.
           05 SNAME-OUT PIC X(25).
           05 STU-TYPE-NAME-OUT PIC X(10).
           05 FILLER PIC X(3) VALUE SPACES.
           05 YEAR-OUT PIC 9.
           05 FILLER PIC X(6) VALUE SPACES.
           05 TOTAL-FEE-OUT PIC ZZ,ZZ9.99.
           05 FILLER PIC X(2) VALUE SPACES.
       01 DATA-REC.
           05 SNO-IN PIC 9(10).
           05 SNAME-IN PIC X(25).
           05 C-CODE-IN PIC 9.
           05 C-NAME-IN PIC X(27).
           05 YEAR-IN PIC 9.
           05 SEC-IN PIC 9.
           05 STU-TYPE-CODE-IN PIC X.
           05 STU-TYPE-NAME-IN PIC X(10).
           05 T-FEE-IN PIC X(10).
           05 T-FEE-ACT PIC 9(4)V99.
           05 SC-FEE-IN PIC X(9).
           05 SC-FEE-ACT PIC 9(3)V99.
           05 LAB-FEE-IN PIC X(9).
           05 LAB-FEE-ACT PIC 9(3)V99.
           05 M-FEE-IN PIC X(9).
           05 M-FEE-ACT PIC 9(3)V99.
           05 TOTAL-FEE PIC 9(5)V99 VALUE ZERO.
       01 DISPLAY-OUT.
           05 T-FEE-OUT PIC ZZ,ZZ9.99.
           05 SC-FEE-OUT PIC Z,ZZ9.99.
           05 LAB-FEE-OUT PIC Z,ZZ9.99.
           05 M-FEE-OUT PIC Z,ZZ9.99.
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
           DISPLAY (4, 1)SPACER.
           DISPLAY (5, 1) "Student No: ".
           ACCEPT (5, 40) SNO-IN.
           MOVE SNO-IN TO SNO-OUT.
           DISPLAY (5, 40) SNO-OUT.
           DISPLAY (6, 1) "Student Name: ".
           ACCEPT (6, 40) SNAME-IN.
           MOVE SNAME-IN TO SNAME-OUT.
           DISPLAY (6, 40) SNAME-OUT.
           DISPLAY (7, 1) "Course Code: ".
           PERFORM COURSE-RTN.
           MOVE 0 TO VALID-FLAG.
           DISPLAY (9, 1) "Year: "
           PERFORM YR-CHK-RTN.
           MOVE 0 TO VALID-FLAG.
           DISPLAY (10, 1) "Section: "
           ACCEPT (10, 40) SEC-IN.
           DISPLAY (10, 40) SEC-IN.
           DISPLAY (11, 1) "Student Type: ".
           PERFORM STU-TYPE-RTN.
           MOVE 0 TO VALID-FLAG.

           DISPLAY (13, 1) "Tuition Fee: ".
           ACCEPT (13, 40) T-FEE-IN.
           MOVE FUNCTION NUMVAL(T-FEE-IN) TO T-FEE-ACT.
           MOVE T-FEE-ACT TO T-FEE-OUT.
           DISPLAY (13, 40) T-FEE-OUT.
           ADD T-FEE-ACT TO TOTAL-FEE.

           DISPLAY (14, 1) "Student Council Fee: ".
           ACCEPT (14, 40) SC-FEE-IN.
           MOVE FUNCTION NUMVAL(SC-FEE-IN) TO SC-FEE-ACT.
           MOVE SC-FEE-ACT TO SC-FEE-OUT.
           DISPLAY (14, 40) SC-FEE-OUT.
           ADD SC-FEE-ACT TO TOTAL-FEE.

           DISPLAY (15, 1) "Laboratory Fee: ".
           ACCEPT (15, 40) LAB-FEE-IN.
           MOVE FUNCTION NUMVAL(LAB-FEE-IN) TO LAB-FEE-ACT.
           MOVE LAB-FEE-ACT TO LAB-FEE-OUT.
           DISPLAY (15, 40) LAB-FEE-OUT.
           ADD LAB-FEE-ACT TO TOTAL-FEE.

           DISPLAY (16, 1) "Miscellaneous Fee: ".
           ACCEPT (16, 40) M-FEE-IN.
           MOVE FUNCTION NUMVAL(M-FEE-IN) TO M-FEE-ACT.
           MOVE M-FEE-ACT TO M-FEE-OUT.
           DISPLAY (16, 40) M-FEE-OUT.
           ADD M-FEE-ACT TO TOTAL-FEE.

           MOVE TOTAL-FEE TO TOTAL-FEE-OUT.
           DISPLAY (17, 1) "Total Amount of Fees: ".
           DISPLAY (17, 40) TOTAL-FEE-OUT.
           PERFORM PRINT-RTN THRU PRINT-END.
           DISPLAY (18, 1) "INPUT ANOTHER RECORD (Y/N)".
           PERFORM ANS-CHK-RTN UNTIL VALID-FLAG = 1.
       PROCESS-END.
           EXIT.
       COURSE-RTN.
           ACCEPT (7, 40) C-CODE-IN.
           EVALUATE C-CODE-IN
               WHEN 1
                   MOVE 1 TO VALID-FLAG
                   MOVE "Accounting" TO C-NAME-IN
               WHEN 2
                   MOVE 1 TO VALID-FLAG
                   MOVE "Arts" TO C-NAME-IN
               WHEN 3
                   MOVE 1 TO VALID-FLAG
                   MOVE "Business" TO C-NAME-IN
               WHEN 4 
                   MOVE 1 TO VALID-FLAG
                   MOVE "Computer Science/Info. Tech" TO C-NAME-IN
               WHEN 5
                   MOVE 1 TO VALID-FLAG 
                   MOVE "Eduction" TO C-NAME-IN
               WHEN 6
                   MOVE 1 TO VALID-FLAG
                   MOVE "Engineering" TO C-NAME-IN 
               WHEN OTHER
                   MOVE 0 TO VALID-FLAG
           END-EVALUATE.

           IF VALID-FLAG = 1
               DISPLAY (7, 40) C-CODE-IN
               DISPLAY (8, 1) "Course Name: "
               DISPLAY (8, 40) C-NAME-IN
           ELSE
               PERFORM COURSE-RTN THRU COURSE-END UNTIL VALID-FLAG = 1
           END-IF.
                  
           
       COURSE-END.
           EXIT.

       YR-CHK-RTN.
           ACCEPT (9, 40) YEAR-IN.
           IF C-NAME-IN = "Engineering              "
               IF YEAR-IN >= 1 AND YEAR-IN <= 5
                   MOVE 1 TO VALID-FLAG
               ELSE
                   MOVE 0 TO VALID-FLAG
               END-IF    
           ELSE
               IF YEAR-IN >= 1 AND YEAR-IN <= 4
                   MOVE 1 TO VALID-FLAG
               ELSE 
                   MOVE 0 TO VALID-FLAG
               END-IF
           END-IF 

           IF VALID-FLAG = 1
               DISPLAY (9, 40) YEAR-IN
           ELSE   
               PERFORM YR-CHK-RTN THRU YR-CHK-END UNTIL VALID-FLAG = 1
           END-IF.   
       YR-CHK-END.
           EXIT.

       STU-TYPE-RTN.
           ACCEPT (11, 40) STU-TYPE-CODE-IN.
           EVALUATE STU-TYPE-CODE-IN
               WHEN "R"
               WHEN "r"
                   MOVE 1 TO VALID-FLAG
                   MOVE "Regular" TO STU-TYPE-NAME-IN
               WHEN "I"
               WHEN "i"
                   MOVE 1 TO VALID-FLAG
                   MOVE "Irregular" TO STU-TYPE-NAME-IN
               WHEN OTHER
                   MOVE 0 TO VALID-FLAG
           END-EVALUATE.

           IF VALID-FLAG = 1
               DISPLAY (11, 40) STU-TYPE-CODE-IN
               DISPLAY (12, 1) "Student Type Name: " 
               DISPLAY (12, 40) STU-TYPE-NAME-IN
           ELSE
               PERFORM STU-TYPE-RTN THRU STU-TYPE-END 
                   UNTIL VALID-FLAG = 1
           END-IF.
       STU-TYPE-END.
           EXIT.

       ANS-CHK-RTN.
           ACCEPT (18, 40) ANS.
           EVALUATE ANS
               WHEN "Y" 
               WHEN "y"
                   MOVE 1 TO VALID-FLAG
                   MOVE 0 TO TOTAL-FEE
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
           MOVE SNO-IN TO SNO-OUT.
           MOVE SNAME-IN TO SNAME-OUT.
           MOVE YEAR-IN TO YEAR-OUT.
           MOVE STU-TYPE-NAME-IN TO STU-TYPE-NAME-OUT.
           WRITE OUTREC FROM REC-OUT AFTER ADVANCING 1 LINE.    
       PRINT-END.
           EXIT.
           
       FINISH-RTN.
           CLOSE OUTFILE.
       FINISH-END.
           EXIT.