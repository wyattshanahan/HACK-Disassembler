# HACK Disassemblers
A series of disassemblers written for the HACK assembly language.

 

The HACK assembly language is used by the HACK computer found in Nand2Tetris by Nisan and Schocken. 

It can be found at www.nand2tetris.org and in the book "The Elements of Computing Systems."

# Current languages supported

- C#
- Lua
- PHP (CLI-based)
- Python
- Ruby
- Swift
- TCL

# How to use

This program will convert the raw binary from the .hack file into a .asm file of the same base name containing fully functional hack assembly.

Each disassembler file requires a .hack file containing translated assembly code. It will disassemble the code into a .asm file of the same name.

Each program takes 2 arguments, the program name and the .hack file. For example, in Lua you would use ``lua disassembler.lua Neg.hack`` to disassemble Neg.hack. Please see the readme file in each directory (if available) for more information about executing the disassembler in that particular language.

The samples directory contains several example .hack files to run through the disassemblers. There is also an assembly .asm file for each .hack file to show what the output should look like.

# Upcoming Language Support

- COBOL
- JavaScript
- Pascal
