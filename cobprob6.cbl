       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG3PROB6.
       AUTHOR. GIAN-CARYL-SHEINE-EGIAN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFILE ASSIGN TO "C:\dos\cobol\outfile5.txt".
       DATA DIVISION.
       FILE SECTION.
       FD OUTFILE.
       01 OUTREC.
           05 FILLER PIC X(90).
       WORKING-STORAGE SECTION.
       01 HD01-REC.
           05 FILLER PIC X(25) VALUE SPACES.
           05 FILLER PIC X(41) VALUE 
               "Polytechnic University of the Philippines".
           05 FILLER PIC X(24) VALUE SPACES.
       01 HD02-REC.
           05 FILLER PIC X(36) VALUE SPACES.
           05 FILLER PIC X(17) VALUE 
               "Sta. Mesa, Manila".
           05 FILLER PIC X(35) VALUE SPACES.
       01 HD03-REC.
           05 FILLER PIC X(36) VALUE SPACES.
           05 FILLER PIC X(17) VALUE 
               "Population Report".
           05 FILLER PIC X(35) VALUE SPACES.
       01 HD04-REC.
           05 FILLER PIC X(38) VALUE SPACES.
           05 FILLER PIC X(14) VALUE 
               "First Semester".
           05 FILLER PIC X(38) VALUE SPACES.
       01 HD05-REC.
           05 FILLER PIC X(40) VALUE SPACES.
           05 FILLER PIC X(9) VALUE 
               "2010-2011".
           05 FILLER PIC X(39) VALUE SPACES.    
       01 SPACER.
           05 FILLER PIC X(80) VALUE SPACES.  

       01 COLHD1-REC.
           05 FILLER PIC X(2) VALUE SPACES.
           05 FILLER PIC X(13) VALUE "Location Name".
           05 FILLER PIC X(1) VALUE SPACES.
           05 FILLER PIC X(20) VALUE "Total No. of Courses".
           05 FILLER PIC X(1) VALUE SPACES.
           05 FILLER PIC X(30) VALUE "Total No. of Enrolled Students".
           05 FILLER PIC X(1) VALUE SPACES.
           05 FILLER PIC X(18) VALUE "Total No. Faculty ".
           05 FILLER PIC X(2) VALUE SPACES.

       01 LAR-BRA-STU.
           05 LAR-STU-TITLE PIC X(34) VALUE 
               "Largest No. of Enrolled Students: ".
           05 LARGEST-STU-POP PIC ZZZ,ZZ9.
           05 LAR-STU-BRA-TITLE PIC X(12) VALUE "Branch Name:".
           05 BRA-NAM-LARGES PIC X(20).

       01 LAR-BRA-FAC.
           05 LAR-FAC-TITLE PIC X(24) VALUE "Largest No. of Faculty: ".
           05 LARGEST-FAC-POP PIC ZZZ,ZZ9.
           05 LAR-FAC-BRA-TITLE PIC X(12) VALUE "Branch Name:".
           05 BRA-NAM-LARGEF PIC X(20).

       01 REC-OUT.
           05 FILLER PIC X(4) VALUE SPACES.
           05 LOC-NAM-OUT PIC X(20).
           05 TOTNO-COR-OUT PIC 99.
           05 FILLER PIC X(18) VALUE SPACES.
           05 TOTNO-ENSTU-OUT PIC ZZZ,ZZ9.
           05 FILLER PIC X(18) VALUE SPACES.
           05 TOTNO-FAC-OUT PIC ZZZ,ZZ9.
           
       01 REC-IN.
           05 LOC-BRA-COD-IN PIC 99.
           05 LOC-NAM-IN PIC X(20).
           05 TOTNO-COR-IN PIC 99.
           05 TOTNO-FAC-REG-IN PIC 9(3).
           05 TOTNO-FAC-PT-IN PIC 9(3).
           05 TOTNO-FAC-IN PIC 9(4).
           05 TOTNO-ENSTU-IN PIC 9(6).
           05 TOTNO-REGSTU-IN PIC 9(4).
           05 TOTNO-IRREGSTU-IN PIC 9(3).

       01 DIS-REC.
           05 TOTNOD-FAC-REG-IN PIC ZZ9.
           05 TOTNOD-FAC-PT-IN PIC ZZ9.

           05 TOTNOD-REGSTU-IN PIC Z,ZZ9.
           05 TOTNOD-IRREGSTU-IN PIC ZZ9.

       01 INIT-FLAGS.
           05 END-PROG PIC 9 VALUE 0.
           05 VALID-FLAG PIC 9 VALUE 0.
           05 ANS PIC X.
           05 CHK-ANS PIC 9 VALUE 0.
           05 REC PIC 99 VALUE 0.
           05 LARGEST-STU-POP-IN PIC 9(6).
           05 LARGEST-FAC-POP-IN PIC 9(6).
           05 BRA-NAM-LARGES-IN PIC X(20).
           05 BRA-NAM-LARGEF-IN PIC X(20).

           
       SCREEN SECTION. 
       01 CLRSCR.
           05 BLANK SCREEN.            

       PROCEDURE DIVISION.
           PERFORM MAIN-RTN.
       MAIN-RTN.
           DISPLAY CLRSCR.
           OPEN OUTPUT OUTFILE.
           PERFORM INIT-PRINT-RTN THRU INIT-PRINT-RTN.
           PERFORM PROCESS-RTN THRU PROCESS-END UNTIL CHK-ANS = 1.
           PERFORM FINISH-RTN THRU FINISH-END.
           STOP RUN.

       INIT-PRINT-RTN.
           WRITE OUTREC FROM HD01-REC.
           WRITE OUTREC FROM HD02-REC AFTER ADVANCING 3 LINES. 
           WRITE OUTREC FROM HD03-REC AFTER ADVANCING 1 LINES.
           WRITE OUTREC FROM HD04-REC AFTER ADVANCING 1 LINES.
           WRITE OUTREC FROM HD05-REC AFTER ADVANCING 2 LINES.
           WRITE OUTREC FROM COLHD1-REC AFTER ADVANCING 1 LINE.
       INIT-PRINT-END.
           EXIT.     

       PROCESS-RTN.
           DISPLAY CLRSCR.
           DISPLAY (1, 1) HD01-REC.
           DISPLAY (2, 1) HD02-REC.
           DISPLAY (3, 1) SPACER.
           DISPLAY (4, 1) SPACER.
           DISPLAY (5, 1) HD03-REC.
           DISPLAY (6, 1) HD04-REC.
           DISPLAY (7, 1) HD05-REC.
           DISPLAY (8, 1) SPACER.

           DISPLAY (9, 1) "Location Branch Code: ".
           PERFORM BRA-COD-RTN.
           MOVE 0 TO VALID-FLAG.

           DISPLAY (11, 1) "Total No. of Courses Offered: ".
           ACCEPT (11, 40) TOTNO-COR-IN.
           DISPLAY (11, 40) TOTNO-COR-IN.

           DISPLAY (12, 1) "Total No. of Regular Faculty: ".
           ACCEPT (12, 40) TOTNO-FAC-REG-IN.
           MOVE TOTNO-FAC-REG-IN TO TOTNOD-FAC-REG-IN.
           DISPLAY (12, 40) TOTNOD-FAC-REG-IN.
           ADD TOTNO-FAC-REG-IN TO TOTNO-FAC-IN.

           DISPLAY (13, 1) "Total No. of Part Time Faculty: ".
           ACCEPT (13, 40) TOTNO-FAC-PT-IN.
           MOVE TOTNO-FAC-PT-IN TO TOTNOD-FAC-PT-IN.
           DISPLAY (13, 40) TOTNOD-FAC-PT-IN.
           ADD TOTNO-FAC-PT-IN TO TOTNO-FAC-IN.

           DISPLAY (14, 1) "Total No. of Enrolled Students: ".
           ACCEPT (14, 40) TOTNO-ENSTU-IN.
           MOVE TOTNO-ENSTU-IN TO TOTNO-ENSTU-OUT.
           DISPLAY (14, 40) TOTNO-ENSTU-OUT.

           DISPLAY (15, 1) "Total No. of Regular Students: ".
           ACCEPT (15, 40) TOTNO-REGSTU-IN.
           MOVE TOTNO-REGSTU-IN TO TOTNOD-REGSTU-IN.
           DISPLAY (15, 40) TOTNOD-REGSTU-IN.

           COMPUTE TOTNO-IRREGSTU-IN = TOTNO-ENSTU-IN - TOTNO-REGSTU-IN.
           DISPLAY (16, 1) "Total No. of Irregular Students: ".
           MOVE TOTNO-IRREGSTU-IN TO TOTNOD-IRREGSTU-IN.
           DISPLAY (16, 40) TOTNOD-IRREGSTU-IN.

           PERFORM LARGEST-RTN THRU LARGEST-END.
           PERFORM PRINT-RTN THRU PRINT-END.

           MOVE 0 TO VALID-FLAG.

           DISPLAY (17, 1) "Input Another Record (Y/N)?".
           PERFORM ANS-CHK-RTN UNTIL VALID-FLAG = 1.

       PROCESS-END.
           EXIT.

       BRA-COD-RTN.
           ACCEPT (9, 40) LOC-BRA-COD-IN.
           EVALUATE LOC-BRA-COD-IN
               WHEN 1
                   MOVE 1 TO VALID-FLAG
                   MOVE "PUP Main" TO LOC-NAM-IN
              WHEN 2 
                  MOVE 1 TO VALID-FLAG
                  MOVE "PUP Commonwealth" TO LOC-NAM-IN
              WHEN 3 
                  MOVE 1 TO VALID-FLAG
                  MOVE "PUP Sta. Rosa" TO LOC-NAM-IN
              WHEN 4
                  MOVE 1 TO VALID-FLAG
                  MOVE "PUP Taguig" TO LOC-NAM-IN
              WHEN OTHER
                 MOVE 0 TO VALID-FLAG
           END-EVALUATE.

           IF VALID-FLAG = 1
               DISPLAY (9, 40) LOC-BRA-COD-IN
               DISPLAY (10, 1) "Location Name: "
               DISPLAY (10, 40) LOC-NAM-IN
           ELSE
               PERFORM BRA-COD-RTN THRU BRA-COD-END 
                   UNTIL VALID-FLAG = 1
           END-IF.          
       BRA-COD-END.
           EXIT.

       LARGEST-RTN.
           IF REC = 0
               MOVE LOC-NAM-IN TO BRA-NAM-LARGES-IN
               MOVE TOTNO-ENSTU-IN TO LARGEST-STU-POP-IN
               MOVE LOC-NAM-IN TO BRA-NAM-LARGEF-IN
               MOVE TOTNO-FAC-IN TO LARGEST-FAC-POP-IN
           ELSE 
               IF TOTNO-ENSTU-IN > LARGEST-STU-POP-IN
                   MOVE TOTNO-ENSTU-IN TO LARGEST-STU-POP-IN
                   MOVE LOC-NAM-IN TO BRA-NAM-LARGES-IN
               END-IF
               IF TOTNO-FAC-IN > LARGEST-FAC-POP-IN
                   MOVE TOTNO-FAC-IN TO LARGEST-FAC-POP-IN
                   MOVE LOC-NAM-IN TO BRA-NAM-LARGEF-IN
               END-IF
           END-IF. 
           ADD 1 TO REC.   
       LARGEST-END.
           EXIT.
       
       ANS-CHK-RTN.
           ACCEPT (17, 40) ANS.
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
           MOVE LOC-NAM-IN TO LOC-NAM-OUT.
           MOVE TOTNO-COR-IN TO TOTNO-COR-OUT.
           MOVE TOTNO-FAC-IN TO TOTNO-FAC-OUT.

           WRITE OUTREC FROM REC-OUT AFTER ADVANCING 1 LINE.
       PRINT-END.
           EXIT.

       FINISH-RTN.
           MOVE LARGEST-STU-POP-IN TO LARGEST-STU-POP.
           MOVE BRA-NAM-LARGES-IN TO BRA-NAM-LARGES.
           MOVE LARGEST-FAC-POP-IN TO LARGEST-FAC-POP
           MOVE BRA-NAM-LARGEF-IN TO BRA-NAM-LARGEF.

           DISPLAY (18, 1) LAR-STU-TITLE.
           DISPLAY (18, 40) LARGEST-STU-POP.
           DISPLAY (19, 1) LAR-STU-BRA-TITLE.
           DISPLAY (19, 40) BRA-NAM-LARGES.

           DISPLAY (20, 1) LAR-FAC-TITLE.
           DISPLAY (20, 40) LARGEST-FAC-POP.
           DISPLAY (21, 1) SPACER.
           DISPLAY (22, 1) LAR-FAC-BRA-TITLE.
           DISPLAY (22, 40) BRA-NAM-LARGEF.

           CLOSE OUTFILE.
       FINISH-END.
           EXIT.
