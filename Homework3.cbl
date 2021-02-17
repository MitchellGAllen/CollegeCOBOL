       IDENTIFICATION DIVISION.
       PROGRAM-ID.     Homework2.
       AUTHOR.         Mitchell A, Adam M, Michael L.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLASS-FILE   ASSIGN TO 'N:\INPUT.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE  ASSIGN TO 'N:\OUTPUT.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  CLASS-FILE
           RECORD CONTAINS 37 CHARACTERS
           DATA RECORD IS CLASS-RECORD.
       01  CLASS-RECORD.
           05  FIRST-NAME-IN     PIC X(10).
           05  LAST-NAME-IN      PIC X(15).
           05  GRADES-IN         OCCURS 4 TIMES.
               10  GRADE-IN      PIC 999.
       FD  OUTPUT-FILE
           RECORD CONTAINS 47
           DATA RECORD IS OUT-PRINT.
       01  OUT-PRINT          PIC X(47).
       
       WORKING-STORAGE SECTION.
       01  DATA-REMAINS-SWITCH PIC xx          VALUE SPACES.
      *Used to determine when to continue reading the file
       
       01  CURVE-FLAG PIC xx                   VALUE SPACES.
      *Used to determine if the averages needed curved

       01  TABLE-SIZE PIC 99                   VALUE ZEROS.
      *Used to handle tables of varing sizes by counting it as the table is filled

       01  AVG-CHK PIC 99                      VALUE ZEROS.
      *Control variable for checking if the average needs curved

       01  COUNTER PIC 99                        VALUE ZEROS.
      *Used as a way to access each element in the table

       01  CLASS-TABLE.
           05  STUDENT           OCCURS 10 TIMES.
               10  FIRST-NAME    PIC X(10).
               10  LAST-NAME     PIC X(15).
               10  GRADES        OCCURS 4 TIMES.
                   15  GRADE     PIC 999.
               10  AVG           PIC 999    VALUE ZEROS.
      *Table used for storing the students        

       01  PRINT-HEAD.
	   05  FILLER	        PIC x(10)		VALUE 'First Name'.
	   05  FILLER	        PIC x(6)		VALUE ' |    '.
	   05  FILLER	        PIC x(9)		VALUE 'Last Name'.
	   05  FILLER	        PIC x(6)	 	VALUE '    | '.
	   05  FILLER	        PIC x(3)		VALUE 'Avg'.
	   05  FILLER	        PIC x(3)		VALUE ' | '.
	   05  FILLER	        PIC x(10)		VALUE 'Curved Avg'.
      *Standard heading output format 
       
       01  PRINT-DETAILS.
	      05  FIRST-NAME-OUT   PIC x(10).
	      05  FILLER		   PIC x(3)		VALUE ' | '.
	      05  LAST-NAME-OUT	   PIC x(15).
	      05  FILLER		   PIC x(3)		VALUE ' | '.
	      05  AVG-OUT		   PIC 9(3).
	      05  FILLER		   PIC x(3)		VALUE ' | '.
          05  FILLER           PIC x(3)     VALUE SPACES.
	      05  C-AVG-OUT	       PIC 9(3).
	      05  FILLER		   PIC x(4)		VALUE SPACES.
      *Standard details formatting 
     

       PROCEDURE DIVISION.

       VALIDATION-MAIN.
       	  OPEN INPUT CLASS-FILE
	       OUTPUT OUTPUT-FILE.
	   READ CLASS-FILE
	       AT END MOVE 'NO' TO DATA-REMAINS-SWITCH
	   END-READ.
          PERFORM INPUT-PARAGRAPH
               UNTIL DATA-REMAINS-SWITCH = 'NO'.
	   CLOSE CLASS-FILE.
          PERFORM PROCESS-PARAGRAPH TABLE-SIZE TIMES.
          PERFORM WRITE-HEADING.
          MOVE 0 TO COUNTER.
          PERFORM OUTPUT-PARAGRAPH TABLE-SIZE TIMES.
          CLOSE OUTPUT-FILE.
	   STOP RUN.

       INPUT-PARAGRAPH.
      *Takes the input and moves it into the table for later processing
          ADD 1 TO TABLE-SIZE.
          MOVE FIRST-NAME-IN TO FIRST-NAME(TABLE-SIZE).
          MOVE LAST-NAME-IN TO LAST-NAME(TABLE-SIZE).
          MOVE GRADE-IN(1) TO GRADE(TABLE-SIZE, 1).
          MOVE GRADE-IN(2) TO GRADE(TABLE-SIZE, 2).
          MOVE GRADE-IN(3) TO GRADE(TABLE-SIZE, 3).
          MOVE GRADE-IN(4) TO GRADE(TABLE-SIZE, 4).
          ADD  GRADE(TABLE-SIZE, 1) 
               GRADE(TABLE-SIZE, 2) 
               GRADE(TABLE-SIZE, 3)
               GRADE(TABLE-SIZE, 4) 
          GIVING AVG(TABLE-SIZE).                                        
          DIVIDE AVG(TABLE-SIZE) BY 4 GIVING AVG(TABLE-SIZE).
          READ CLASS-FILE 
               AT END MOVE 'NO' TO DATA-REMAINS-SWITCH
          END-READ.

       PROCESS-PARAGRAPH.
      *Calculates the average for every student
          ADD 1 TO COUNTER.
          IF AVG(COUNTER) IS GREATER THAN 84
             ADD 1 TO AVG-CHK.
          ENDIF.
       
       WRITE-HEADING.
      *Standard heading paragraph
          MOVE PRINT-HEAD TO OUT-PRINT.
          WRITE OUT-PRINT.
       
       OUTPUT-PARAGRAPH.
      *Checks the AVG-CHK variable for if curving is needed then does appropriate output writing
          ADD 1 TO COUNTER.
          IF AVG-CHK IS LESS THAN 3
             MOVE AVG(COUNTER) TO C-AVG-OUT
             ADD 10 TO C-AVG-OUT
          ELSE
             MOVE AVG(COUNTER) TO C-AVG-OUT.
          MOVE FIRST-NAME(COUNTER) TO FIRST-NAME-OUT.
          MOVE LAST-NAME(COUNTER) TO LAST-NAME-OUT.
          MOVE AVG(COUNTER) TO AVG-OUT.
          MOVE PRINT-DETAILS TO OUT-PRINT.
          WRITE OUT-PRINT.