       IDENTIFICATION DIVISION.
       PROGRAM-ID.     Homework2.
       AUTHOR.         Mitchell A, Adam M, Michael L.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CAR-SALES-FILE   ASSIGN TO 'N:\INPUT.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT GOOD-FILE        ASSIGN TO 'N:\GOOD.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BAD-FILE         ASSIGN TO 'N:\BAD.TXT' 
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CAR-SALES-FILE
           RECORD CONTAINS 67 CHARACTERS
           DATA RECORD IS CAR-SALES-RECORD.
       01  CAR-SALES-RECORD.
           05 LOCATION         PIC X(11).
           05 BRANCH           PIC 9(4).
           05 SALESPERSON      PIC X(10).
           05 CUSTOMER         PIC X(10).
           05 SALE-DATE.
               07 MONTH        PIC 99.
               07 DAY-IN       PIC 99.
               07 YEAR         PIC 99.
           05 SALE-AMOUNT      PIC 9(6).
           05 COMMISSION       PIC 9(3).
           05 CAR-MODEL        PIC X(13).
           05 CAR-YEAR         PIC 9(4).

       FD  GOOD-FILE
           RECORD CONTAINS 130 CHARACTERS
           DATA RECORD IS GOOD-PRINT.
       01  GOOD-PRINT          PIC x(130).

       FD BAD-FILE
	       RECORD CONTAINS 130 CHARACTERS
	       DATA RECORD IS BAD-PRINT.
       01  BAD-PRINT           PIC x(130).
           
       WORKING-STORAGE SECTION.
       01  DATA-REMAINS-SWITCH PIC xx          VALUE SPACES.
      *Used to determine when to continue reading the file

       01  E-MSG-CONTROL       PIC 9.
           88  ALL-CLEAR   		       VALUE IS 0.
           88  MISSING     		       VALUE IS 1.
           88  NON-NUM     		       VALUE IS 2.
           88  BAD-COMM    		       VALUE IS 3.
           88  BAD-YEAR    		       VALUE IS 4.
           88  BAD-DATE    		       VALUE IS 5.
      *Was having trouble getting these to work and ran out of time to fix them 
        
       01  E-MSG-PRINT-LINE.
           05  E-MSG-PRINT     PIC x(55).
           05  FILLER          PIC x(75)       VALUE SPACES.
      *The spaces are used to fill the rest of the bad-print variable    

       01  PRINTV-HEAD.
           05  FILLER          PIC x(8)        VALUE 'Location'.
           05  FILLER          PIC xxx         VALUE SPACES.
           05  FILLER          PIC x(3)        VALUE ' | '.
           05  FILLER          PIC x(6)        VALUE 'Branch'.
           05  FILLER          PIC x(3)        VALUE ' | '.
           05  FILLER          PIC x(11)       VALUE 'Salesperson'.
           05  FILLER          PIC x(3)        VALUE ' | '.
           05  FILLER          PIC x(13)       VALUE 'Customer Name'.
           05  FILLER          PIC x(3)        VALUE ' | '.
           05  FILLER          PIC x(9)        VALUE 'Sale Date'.
           05  FILLER          PIC x(3)        VALUE ' | '.
           05  FILLER          PIC x(11)       VALUE 'Sale Amount'.
           05  FILLER          PIC x(3)        VALUE ' | '.
           05  FILLER          PIC x(15)       VALUE 'Commission Rate'.
           05  FILLER          PIC x(3)        VALUE ' | '.
           05  FILLER          PIC x(9)        VALUE 'Car Model'.
           05  FILLER          PIC x(4)        VALUE SPACES.
           05  FILLER          PIC x(3)        VALUE ' | '.
           05  FILLER          PIC x(8)        VALUE 'Car Year'.
      *Heading line for both good and bad outputs
       
       01  PRINTV-DETAILS.
           05  LOC-OUT         PIC x(11).
           05  FILLER          PIC x(4)        VALUE ' |  '.
           05  BRNCH-OUT       PIC 9(4).
           05  FILLER          PIC x(4)        VALUE '  | '.
           05  SALE-PER-OUT    PIC x(10).
           05  FILLER          PIC x(4)        VALUE '  | '.
           05  CUST-OUT        PIC x(10).
           05  FILLER          PIC x(3)        VALUE SPACES.
           05  FILLER          PIC x(3)        VALUE ' | '.
           05  SALE-DATE-OUT.
                 07  MONTH-OUT PIC 99.
                 07  FILLER    PIC x           VALUE '-'.
                 07  DAY-OUT   PIC 99.
                 07  FILLER    PIC x           VALUE '-'.
                 07  YEAR-OUT  PIC 99.
           05  FILLER          PIC x           VALUE SPACES.
           05  FILLER          PIC x(3)        VALUE ' | '.
           05  SALE-AMNT-OUT   PIC $$$$$$9.
           05  FILLER          PIC x(4)        VALUE SPACES.
           05  FILLER          PIC x(3)        VALUE ' | '.
           05  FILLER          PIC x           VALUE '%'.
           05  COMM-OUT        PIC 999.
           05  FILLER          PIC x(11)       VALUE SPACES.
           05  FILLER          PIC x(3)        VALUE ' | '.
           05  CAR-MDL-OUT     PIC x(13).
           05  FILLER          PIC x(3)        VALUE ' | '.
           05  CAR-YR-OUT      PIC 9(4).
           05  FILLER          PIC x(4)        VALUE SPACES.
      *Details formatting for both good and bad outputs
       

       PROCEDURE DIVISION.

       VALIDATION-MAIN.
       	  OPEN INPUT  CAR-SALES-FILE
	           OUTPUT GOOD-FILE
	           OUTPUT BAD-FILE.
	      READ CAR-SALES-FILE
	           AT END MOVE 'NO' TO DATA-REMAINS-SWITCH
	      END-READ.
	      PERFORM WRITE-HEADING.
          PERFORM ERROR-CHK
	          UNTIL DATA-REMAINS-SWITCH = 'NO'.
	      CLOSE CAR-SALES-FILE
	            GOOD-FILE
		        BAD-FILE.
	   STOP RUN.

       ERROR-CHK.
       MOVE 0 TO E-MSG-CONTROL.
      *Initializes the control variable for the error message

	   IF LOCATION EQUAL TO SPACES OR 
          SALESPERSON EQUAL TO SPACES OR
          CUSTOMER EQUAL TO SPACES OR 
          CAR-MODEL EQUAL TO SPACES
               MOVE 1 TO E-MSG-CONTROL
               PERFORM ERROR-MESSAGE
	   END-IF.
      *Checks for the first error and calls the error message paragraph after setting the control variable

	   IF BRANCH EQUAL TO ZERO OR 
          SALE-DATE  EQUAL TO ZERO OR 
          SALE-AMOUNT EQUAL TO ZERO OR 
          COMMISSION  EQUAL TO ZERO OR 
          CAR-YEAR EQUAL TO ZERO
            MOVE 1 TO E-MSG-CONTROL
            PERFORM ERROR-MESSAGE
	   END-IF.
      *Checks for the second error

	   IF BRANCH IS NOT NUMERIC OR 
          SALE-DATE IS NOT NUMERIC OR 
          SALE-AMOUNT IS NOT NUMERIC OR 
          COMMISSION IS NOT NUMERIC OR 
          CAR-YEAR IS NOT NUMERIC
            MOVE 2 TO E-MSG-CONTROL
            PERFORM ERROR-MESSAGE
	   END-IF.
      *And the third error

	   IF COMMISSION IS LESS THAN ZERO OR GREATER THAN 100
            MOVE 3 TO E-MSG-CONTROL
            PERFORM ERROR-MESSAGE
	   END-IF.

	   IF CAR-YEAR IS LESS THAN 1930 OR GREATER THAN 2016
            MOVE 4 TO E-MSG-CONTROL
            PERFORM ERROR-MESSAGE
	   END-IF.

	   EVALUATE TRUE
	       WHEN MONTH EQUALS 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12
	       IF DAY-IN IS GREATER THAN 31
                MOVE 5 TO E-MSG-CONTROL
                PERFORM ERROR-MESSAGE
      	        END-IF

	       WHEN MONTH EQUALS 4 OR 6 OR 9 OR 11
	       IF DAY-IN IS GREATER THAN 30
                MOVE 5 TO E-MSG-CONTROL
                PERFORM ERROR-MESSAGE
	       END-IF

	       WHEN MONTH EQUALS 2
           IF DAY-IN IS GREATER THAN 29
                MOVE 5 TO E-MSG-CONTROL
                PERFORM ERROR-MESSAGE
	       END-IF

	       WHEN MONTH IS LESS THAN 1 OR GREATER THAN 12
                MOVE 5 TO E-MSG-CONTROL
                PERFORM ERROR-MESSAGE
	   END-EVALUATE.
      *Used to condense all of the day error checks in one statement
       
	   PERFORM MOVE-DETAILS.

           IF E-MSG-CONTROL NOT EQUAL TO 0
                MOVE PRINTV-HEAD TO BAD-PRINT
                WRITE BAD-PRINT
      *Sets the header after the error messages have been printed
	            MOVE PRINTV-DETAILS TO BAD-PRINT
                WRITE BAD-PRINT
                MOVE SPACES TO BAD-PRINT
                WRITE BAD-PRINT
      *Prints an empty line for formatting purposes         
	       ELSE
                MOVE PRINTV-DETAILS TO GOOD-PRINT
                WRITE GOOD-PRINT
	       END-IF.

       READ CAR-SALES-FILE
	      AT END MOVE 'NO' TO DATA-REMAINS-SWITCH
       END-READ.
      *Reads the next input

       ERROR-MESSAGE.
      *When called will print the error message based on the control variable
       EVALUATE TRUE
            WHEN E-MSG-CONTROL EQUAL TO 1
            MOVE 'ERROR - INFORMATION MISSING FROM INPUT RECORD.      '
            TO E-MSG-PRINT                                              
 
            WHEN E-MSG-CONTROL EQUAL TO 2
            MOVE 'ERROR - NON-NUMERIC DATA ENTERED FOR NUMERIC FIELD. '
            TO E-MSG-PRINT                                              

            WHEN E-MSG-CONTROL EQUAL TO 3
            MOVE 'ERROR - COMMISSION RATE MUST BE BETWEEN 0% AND 100%.'
            TO E-MSG-PRINT                                              

            WHEN E-MSG-CONTROL EQUAL TO 4
            MOVE 'ERROR - CAR YEAR MUST BE AT LEAST 1930.             '
            TO E-MSG-PRINT                                              

            WHEN E-MSG-CONTROL EQUAL TO 5
            MOVE 'ERROR - INVALID DATE OF SALE.                       '
            TO E-MSG-PRINT                                              
       END-EVALUATE.
       MOVE E-MSG-PRINT-LINE TO BAD-PRINT.
       WRITE BAD-PRINT.

       WRITE-HEADING.
	   MOVE PRINTV-HEAD TO GOOD-PRINT.
       WRITE GOOD-PRINT.
      *Writes the heading for the good input

       MOVE-DETAILS.
      *Used to move the inputs to either good or bad output
        MOVE LOCATION TO LOC-OUT.
        MOVE BRANCH TO BRNCH-OUT.
        MOVE SALESPERSON TO SALE-PER-OUT.
        MOVE CUSTOMER TO CUST-OUT.
        MOVE MONTH TO MONTH-OUT.
        MOVE DAY-IN TO DAY-OUT.
        MOVE YEAR TO YEAR-OUT.
        MOVE SALE-AMOUNT TO SALE-AMNT-OUT.
        MOVE COMMISSION TO COMM-OUT.
        MOVE CAR-MODEL TO CAR-MDL-OUT.
        MOVE CAR-YEAR TO CAR-YR-OUT.