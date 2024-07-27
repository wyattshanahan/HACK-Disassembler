# Mono C# Hack Disassembler Directions

This program was written for use with mono C#. It is compatible with other C# implementations, but may require changing the directions below. Please consult the documentation for your C# implementation.

Use ``mcs -out:disassembler.exe disassembler.cs`` to compile the program.

Use ``mono disassembler.exe <filename.hack>`` to execute the program.

### An Example Execution

For example, if I were to be executing the program for a file called sample.hack, I'd do the following:

1. Compile the application with ``mcs -out:disassembler.exe disassembler.cs``
2. Call the executable, giving the file as an argument, with ``mono disassembler.exe sample.hack`` 
