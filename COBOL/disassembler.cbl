       *> A disassembler for the hack assembly language.
       *> Compile with cobc -x disassembler.cbl
       *> Run with ./disassembler & enter a valid .hack when prompted
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISASSEMBLER.
       AUTHOR. WYATT SHANAHAN.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT HACK-FILE ASSIGN TO DYNAMIC-FILE
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ASM-FILE ASSIGN TO DYNAMIC-ASM
              ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD HACK-FILE.
       01 HACK-RECORD     PIC X(16).*> read as alpha to allow A and C
       
       FD ASM-FILE.
       01 ASM-RECORD      PIC X(80).
       
       WORKING-STORAGE SECTION.
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
       
       *> continue...
       PROCEDURE DIVISION.
       100-MAIN.
          PERFORM 110-FILENAME.
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
              SET ENVIRONMENT "DYNAMIC-FILE" TO FILE-NAME,
              MOVE FILE-NAME (1:START-POS - 1) TO BASE-NAME,
              MOVE FUNCTION CONCATENATE(FUNCTION TRIM(BASE-NAME),
                     ".asm") TO ASM-NAME,
              SET ENVIRONMENT "DYNAMIC-ASM" TO ASM-NAME,
          ELSE
              DISPLAY "ERROR: File must have a .hack extension.",
              STOP RUN,
          END-IF.


  
       *> todo: open files
       *> define conversion tables
       *> read files, check if a or C-INSTRUCTION
       *> processes for a and C-INSTRUCTION
       *> close file
       
       
       
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
