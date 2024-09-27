       *> A disassembler for the hack assembly language.
       *> Compile with cobc -x disassembler.cbl
       *> Run with ./disassembler & enter a valid .hack when prompted
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISASSEMBLER.
       AUTHOR. WYATT SHANAHAN.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT HACK-FILE ASSIGN USING FILE-NAME
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ASM-FILE ASSIGN USING ASM-NAME
              ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD HACK-FILE.
       01 HACK-RECORD     PIC X(16).*> read as alpha to allow A and C
       
       FD ASM-FILE.
       01 ASM-RECORD      PIC X(80).
       
       WORKING-STORAGE SECTION.
       77 EOF               PIC X VALUE 'N'. *> EOF FLAG
       
       01 FILE-INPUT.
          05 FILE-NAME      PIC X(80). *> STORE FILE NAME
          05 NAME-LEN       PIC 99  VALUE 0. *>STORE LEN OF FILE NAME
          05 START-POS      PIC 99. *> STORE WHERE FILE EXT STARTS
          05 EXT            PIC XXXXX. *>STORE EXTENSION
          05 BASE-NAME      PIC X(75). *>STORE NAME W/O EXT
          05 ASM-NAME       PIC X(79).
       
       01 COMP-TABLE.
          05 COMP-ELEMENT OCCURS 16 TIMES.
             10 COMP-BINARY      PIC X(6).
             10 COMP-RESULT      PIC X(2).
             
       01 COMP-TABLE.
          05 COMP-ELEMENT OCCURS 18 TIMES.
             10 COMP-BIN  PIC X(6). *> binary representation
             10 COMP-ASM  PIC X(7). *> ASM TRANSLATION

       
       *> continue...
       PROCEDURE DIVISION.
       100-MAIN.
          PERFORM 110-FILENAME.
          PERFORM 120-CONSTRUCT-ARRAYS.
          PERFORM 200-PROCESS.
          PERFORM 300-TERMINATION.
          STOP RUN.
          
       110-FILENAME.
          DISPLAY "Enter the HACK file name (with .hack extension): "
              WITH NO ADVANCING.
          ACCEPT FILE-NAME.
          INSPECT FILE-NAME TALLYING NAME-LEN
              FOR CHARACTERS BEFORE INITIAL SPACE.
          COMPUTE START-POS = NAME-LEN - 4. *> FIND START POS OF EXT
          MOVE FILE-NAME (START-POS:5) TO EXT.
          IF EXT = ".hack"
              MOVE FILE-NAME (1:START-POS - 1) TO BASE-NAME,
              MOVE FUNCTION CONCATENATE(FUNCTION TRIM(BASE-NAME),
                     ".asm") TO ASM-NAME,
          ELSE
              DISPLAY "ERROR: File must have a .hack extension.",
              STOP RUN,
          END-IF.
          
       120-CONSTRUCT-ARRAYS. *> manage array construction process
          PERFORM 130-COMP-ARRAY.
          *>PERFORM 140-DEST-ARRAY.
          *> PERFORM 150-JUMP-ARRAY.

       130-COMP-ARRAY. *> build comp array
          MOVE '101010' TO COMP-BIN (1).   
          MOVE '0' TO COMP-ASM (1).
          MOVE '111111' TO COMP-BIN (2).   
          MOVE '1' TO COMP-ASM (2).
          MOVE '111010' TO COMP-BIN (3).   
          MOVE '-1' TO COMP-ASM (3).
          MOVE '001100' TO COMP-BIN (4).   
          MOVE 'D' TO COMP-ASM (4).
          MOVE '110000' TO COMP-BIN (5).   
          MOVE 'A,M' TO COMP-ASM (5).
          MOVE '001101' TO COMP-BIN (6).   
          MOVE '!D' TO COMP-ASM (6).
          MOVE '110001' TO COMP-BIN (7).   
          MOVE '!A,!M' TO COMP-ASM (7).
          MOVE '001111' TO COMP-BIN (8).   
          MOVE '-D' TO COMP-ASM (8).
          MOVE '110011' TO COMP-BIN (9).   
          MOVE '-A,-M' TO COMP-ASM (9).
          MOVE '011111' TO COMP-BIN (10).  
          MOVE 'D+1' TO COMP-ASM (10).
          MOVE '110111' TO COMP-BIN (11).
          MOVE 'A+1,M+1' TO COMP-ASM (11).
          MOVE '001110' TO COMP-BIN (12).
          MOVE 'D-1' TO COMP-ASM (12).
          MOVE '110010' TO COMP-BIN (13).  
          MOVE 'A-1,M-1' TO COMP-ASM (13).
          MOVE '000010' TO COMP-BIN (14).
          MOVE 'D+A,D+M' TO COMP-ASM (14).
          MOVE '010011' TO COMP-BIN (15).
          MOVE 'D-A,D-M' TO COMP-ASM (15).
          MOVE '000111' TO COMP-BIN (16).
          MOVE 'A-D,M-D' TO COMP-ASM (16).
          MOVE '000000' TO COMP-BIN (17).
          MOVE 'D&A,D&M' TO COMP-ASM (17).
          MOVE '010101' TO COMP-BIN (18).
          MOVE 'D|A,D|M' TO COMP-ASM (18).

       
       200-PROCESS.
          OPEN INPUT HACK-FILE.
          OPEN OUTPUT ASM-FILE.
          DISPLAY "TO BE WRITTEN".
       
       300-TERMINATION.
          CLOSE HACK-FILE.
          CLOSE ASM-FILE.
  
       *> todo: 
       *> define conversion tables
       *> read files, check if a or C-INSTRUCTION
       *> processes for a and C-INSTRUCTION
       
       
       
          *>OPEN INPUT HACK-FILE.
          *>OPEN OUTPUT ASM-FILE.
          
          *>PERFORM UNTIL EOF
            *> READ HACK-FILE INTO HACK-RECORD
               *> AT END SET EOF TO TRUE
             *>END-READ
       
            *> IF HACK-RECORD(1:1) = "0"
         *>       PERFORM PROCESS-A-INSTRUCTION
         *>    ELSE
         *>       PERFORM PROCESS-C-INSTRUCTION
         *>    END-IF
       *>
       *>        END-PERFORM.
       *>
       *>        CLOSE HACK-FILE.
       *>      CLOSE ASM-FILE.
       *>    STOP RUN.
