       IDENTIFICATION DIVISION.
       PROGRAM-ID.         Homework1.
       AUTHOR.             Mitchell A, Mike L, Adam M.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE        ASSIGN TO 'N:\input.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRINT1-FILE          ASSIGN TO 'N:\bad.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRINT2-FILE          ASSIGN TO 'N:\output.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
               
       DATA DIVISION.
       FILE SECTION.
           FD CUSTOMER-FILE
           RECORD CONTAINS 70 CHARACTERS
           DATA RECORD IS CUSTOMER-IN.
       01  CUSTOMER-IN.
           05  FIRST-NAME              PIC x(12).
           05  LAST-NAME               PIC x(12).
           05  STREET-ADDRESS          PIC x(15).
           05  CITY                    PIC x(13).
           05  STATE                   PIC x(2).
           05  ACC-NUM                 PIC 9(6).
           05  P-M-R                   PIC 9(5).
           05  C-M-R                   PIC 9(5).
      *CUSTOMER-FILE is the input file    
      *P-M-R is PREVIOUS-METER-READING, C-M-R IS CURRENT-METER-READING
       
           FD PRINT1-FILE.
           RECORD CONTAINS 63 CHARACTERS
           DATA RECORD IS PRINT1-LINE.
       01  PRINT1-LINE.                PIC x(63).
      *PRINT1-OUT is the output record used to store bad records from CUSTOMER-IN
       
           FD PRINT2-FILE.
           RECORD CONTAINS 75 CHARACTERS
           DATA RECORD IS PRINT2-LINE.
       01  PRINT2-LINE.                PIC x(75).
      *PRINT2-OUT is the output record for the processed data
                                       
       WORKING-STORAGE SECTION.         
       01  DATA-REMAINS-SWITCH         PIC xx		VALUE SPACES.
      *Tracks whether data remains to be read from file

       01  PRINT1-HEADING
           05  FILLER     PIC x(14)      VALUE 'ACCOUNT NUMBER'.
           05  FILLER     PIC xxx        VALUE ' | '.
           05  FILLER     PIC x(21)      VALUE 'CURRENT METER READING'.
           05  FILLER     PIC xxx        VALUE ' | '.   
           05  FILLER     PIC x(22)      VALUE 'PREVIOUS METER READING'.
      *PRINT1-HEADING is used to create the header for the output file used to store bad records
      *Header should look like "ACCOUNT NUMBER | CURRENT METER READING | PREVIOUS METER READING"

       01  PRINT1-DETAILS.
           05  FILLER                     PIC x(4)         VALUE SPACES.
           05  ACCOUNT-NUMBER-OUT1        PIC 9(6).
           05  FILLER                     PIC x(5)         VALUE SPACES.
           05  FILLER                     PIC x            VALUE '|'.
           05  FILLER                     PIC x(9)         VALUE SPACES.
           05  CURRENT-METER-READING-OUT  PIC 9(5).
           05  FILLER                     PIC x(9)         VALUE SPACES.
           05  FILLER                     PIC x            VALUE '|'.
           05  FILLER                     PIC x(10)        VALUE SPACES.
           05  PREVIOUS-METER-READING-OUT PIC 9(5).
      *PRINT1-DETAILS is used to print the information below the header to the output file used to store bad records
      *Should look like "    000000     |         00000         |          00000"


       01  PRINT2-HEADING.
           05  FILLER            PIC x(9)        VALUE 'LAST NAME'.     
           05  FILLER            PIC x(6)        VALUE '    | '.
           05  FILLER            PIC x(10)       VALUE 'FIRST NAME'.    
           05  FILLER            PIC x(5)        VALUE '   | '.
           05  FILLER            PIC x(14)       VALUE 'ACCOUNT NUMBER'.
           05  FILLER            PIC xxx         VALUE ' | '.
           05  FILLER            PIC x(14)       VALUE 'STREET ADDRESS'.
           05  FILLER            PIC x(4)        VALUE '  | '.
           05  FILLER            PIC x(10)       VALUE 'UNITS USED'.   
      *PRINT2-HEADING is used to create the header for the output file used to print the processed records
      *Header should look like "LAST NAME    | FIRST NAME   | ACCOUNT NUMBER | STREET ADDRESS  | UNITS USED"

       01  PRINT2-DETAILS.
           05  LAST-NAME-OUT           PIC x(12).
           05  FILLER                  PIC xxx          VALUE SPACES.
           05  FIRST-NAME-OUT          PIC x(12).
           05  FILLER                  PIC xx           VALUE ' |'.
           05  FILLER                  PIC x(5)         VALUE SPACES.
           05  ACCOUNT-NUMBER-OUT2     PIC 9(6).
           05  FILLER                  PIC x(5)         VALUE SPACES.
           05  FILLER                  PIC xx           VALUE '| '.
           05  STREET-ADDRESS-OUT      PIC x(15).
           05  FILLER                  PIC x(5)         VALUE ' |   '.
           05  UNITS-USED              PIC x(5).
      *PRINT2-DETAILS is sued to print the information below the header to the output file used to print the processed records
      *Should look like "abcdefghijkl | abcdefghijkl |     000000     | abcdefghijklmno |   00000"
      *UNITS-USED is CURRENT-METER-READING minus PREVIOUS-METER-READING         

       PROCEDURE DIVISION.
       PREPARE-CUSTOMER-REPORT.
           OPEN INPUT CUSTOMER-FILE
                OUTPUT PRINT1-FILE
                OUTPUT PRINT2-FILE.
           READ CUSTOMER-FILE
               AT END MOVE 'NO' TO DATA-REMAINS-SWITCH
           END-READ.
           PERFORM WRITE-HEADING.
           PERFORM PROCESS-RECORD.
               UNTIL DATA-REMAINS-SWITCH = 'NO'.
      *Will perform PROCESS-RECORD until there are no more records
           CLOSE CUSTOMER-FILE
                 PRINT1-FILE
                 PRINT2-FILE.
           STOP RUN.
                    
       WRITE-HEADING.
           MOVE PRINT1-HEADING TO PRINT1-LINE.
           WRITE PRINT1-LINE.
           MOVE PRINT2-HEADING TO PRINT2-LINE.
           WRITE PRINT2-LINE.
      *Writing the headers for both outputs
                      
       PROCESS-RECORD.
           IF ACC-NUM IS NUMERIC AND P-M-R IS NUMERIC AND C-M-R IS 
           NUMERIC 
      *Not certain if doing the line continuation correctly    
      *Checks if the data is good then processes the data if it is
               MOVE LAST-NAME TO LAST-NAME-OUT.
               MOVE FIRST-NAME TO FIRST-NAME-OUT.
               MOVE ACC-NUM TO ACCOUNT-NUMBER-OUT2.
               MOVE STREET-ADDRESS TO STREET-ADDRESS-OUT.
               SUBTRACT P-M-R BY C-M-R GIVING UNITS-USED.
           ELSE
      *If the data is bad its filtered into the bad record output file
              MOVE ACC-NUM TO ACCOUNT-NUMBER-OUT1.
              MOVE C-M-R TO CURRENT-METER-READING-OUT.
              MOVE P-M-R TO PREVIOUS-METER-READING-OUT.
           END-IF.
           READ CUSTOMER-FILE
               AT END MOVE 'NO' TO DATA-REMAINS-SWITCH
           END-READ.
      *Reads the file again in case of multiple records being processed