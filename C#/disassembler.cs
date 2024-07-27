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
      // create output file name
      string outFile = fileName.Replace(".hack", ".asm");
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
      foreach (var line in lines)
      {
        if (line[0] == '0') // if first char is a 0, then this is an 'A' instruction
        {
          string binStr = line.Substring(1, 15); // splice to grab chars 1-15 (binary substring)
          string val = Convert.ToInt32(binStr, 2).ToString(); // convert to base 10, then to str
          string instruction = "@" + val; // build instruction
          hackList.Add(instruction); // add instruction to list
        }
        else if (line[0] == '1') // if first char is a 1, then this is a 'C' instruction
        {
          int aBit = int.Parse(line[3].ToString()); // get a-bit, then convert to int via string
          string compBits = line.Substring(4, 6); // get comp bits
          string destBits = line.Substring(10, 3); // get dest bits
          string jmpBits = line.Substring(13, 3); // get jmp bits
          compTable.TryGetValue(compBits, out string comp);
          string[] compUnfiltered = comp.Split(','); // split comp at comma
          comp = compUnfiltered[aBit]; // filter comp using abit
          destTable.TryGetValue(destBits, out string dest); // lookup in dest table
          jumpTable.TryGetValue(jmpBits, out string jmp); // lookup in jump table
          string instruction = dest + comp + jmp; // build instruction
          hackList.Add(instruction); // add instruction to list
        }
        else // if not C or A instruction, then throw error and exit
        {
          Console.WriteLine("ERROR: Invalid instruction.");
          return;
        }
        // write to file after replacing .hack with .asm for the new file name
        using (StreamWriter file = new StreamWriter(outFile))
        {
            // Example of writing some text to the file
          foreach (var i in hackList) // iterate through the translated lines in hackList
          {
            file.WriteLine(i); // write line to file
          }
        }
      }
      if (File.Exists(outFile))
      {
        Console.WriteLine("File written to " + outFile);
      }
      else
      {
        Console.WriteLine("ERROR: File " + outFile + " not exist.");
      }
    }
}
