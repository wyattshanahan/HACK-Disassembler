using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static void Main(string[] args)
    {
        // Check number of args, if none, throw error and exit
      if (args.Length == 0)
      {
          Console.WriteLine("ERROR: No input file provided. Use mono disassembler.exe <fileName.hack>.");
          return; // exit execution
      }
      // set filename as first arg
      string fileName = args[0];
      // Check if the filename contains ".hack" and if the file exists. if not, throw error and exit
      if (!fileName.Contains(".hack") || !File.Exists(fileName))
      {
        Console.WriteLine("File does not exist or is not a .hack file.");
        return;
      }
      // Read lines from file and store in lines
      List<string> lines = new List<string>(File.ReadAllLines(fileName));
      // List to store HACK instructions after translation
      List<string> hackList = new List<string>();
      // Computation dictionary
      Dictionary<string, string> compTable = new Dictionary<string, string>
      {
          { "101010", "0" },
          { "111111", "1" },
          { "111010", "-1" },
          { "001100", "D" },
          { "110000", "A,M" },
          { "001101", "!D" },
          { "110001", "!A,!M" },
          { "001111", "-D" },
          { "110011", "-A,-M" },
          { "011111", "D+1" },
          { "110111", "A+1,M+1" },
          { "001110", "D-1" },
          { "110010", "A-1,M-1" },
          { "000010", "D+A,D+M" },
          { "010011", "D-A,D-M" },
          { "000111", "A-D,M-D" },
          { "000000", "D&A,D&M" },
          { "010101", "D|A,D|M" }
      };

      // Destination table
      Dictionary<string, string> destTable = new Dictionary<string, string>
      {
          { "000", "" },
          { "001", "M=" },
          { "010", "D=" },
          { "011", "DM=" },
          { "100", "A=" },
          { "101", "AM=" },
          { "110", "AD=" },
          { "111", "ADM=" }
      };

      // Jump table
      Dictionary<string, string> jumpTable = new Dictionary<string, string>
      {
          { "000", "" },
          { "001", ";JGT" },
          { "010", ";JEQ" },
          { "011", ";JGE" },
          { "100", ";JLT" },
          { "101", ";JNE" },
          { "110", ";JLE" },
          { "111", ";JMP" }
      };

      // test: Accessing and printing elements
      Console.WriteLine("Destination for '101': " + destTable["101"]); // Outputs: AM=
      Console.WriteLine("Jump for '010': " + jumpTable["010"]);       // Outputs: ;JEQ
      foreach (var line in lines)
      {
          Console.WriteLine($"Key: {line}");
      }
    }
}

/*
TODO:
- loop lines in lines
- check if A or C
- translate lines
- append to hackList
- write hacklist to file with name
- print success or not
*/
