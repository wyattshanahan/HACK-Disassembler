# HACK Disassemblers
A series of disassemblers written for the HACK assembly language.

 

The HACK assembly language is used by the HACK computer found in Nand2Tetris by Nisan and Schocken. 

It can be found at www.nand2tetris.org and in the book "The Elements of Computing Systems."

# Current languages supported

- Lua
- Python

# How to use

Each disassembler file requires a .hack file containing translated assembly code. It will disassemble the code into a .asm file of the same name.

Each program takes 2 arguments, the program name and the .hack file. For example, in Lua you would use ``lua disassembler.lua Neg.hack`` to disassemble Neg.hack.

The samples directory contains several example .hack files to run through the disassemblers. There is also an assembly .asm file for each .hack file to show what the output should look like.
