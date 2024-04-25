//  This program converts Hack binary into Hack Assembly Language.
// To run, first compile with: swiftc -o disassembler disassembler.swift
// Then run with: ./disassembler <fileName.hack>

import Foundation
// Check if command-line argument is provided. if none, print error and exit
if CommandLine.arguments.count < 2 {
    print("ERROR: No input file provided. Use \(CommandLine.arguments[0]) <fileName.hack>")
    exit(1)
}
// check if appropriate file extension, if not then throw error and exit
if !CommandLine.arguments[1].contains(".hack"){
  print("ERROR: Input file must be a .hack file.")
  exit(1)
}
// use fileManager to check if exists - if not, throw error and exit
let fileManager = FileManager.default
if !fileManager.fileExists(atPath: CommandLine.arguments[1]) {
  print("ERROR: File does not exist.")
  exit(1)
}
else {
  let lines = try String(contentsOfFile: CommandLine.arguments[1]).components(separatedBy: .newlines) // read file into lines
  // store hack instructions
  var hackList = [String]()
  
    // Computation Dictionary
  let compTable: [String: String] = [
      "101010": "0",
      "111111": "1",
      "111010": "-1",
      "001100": "D",
      "110000": "A,M",
      "001101": "!D",
      "110001": "!A,!M",
      "001111": "-D",
      "110011": "-A,-M",
      "011111": "D+1",
      "110111": "A+1,M+1",
      "001110": "D-1",
      "110010": "A-1,M-1",
      "000010": "D+A,D+M",
      "010011": "D-A,D-M",
      "000111": "A-D,M-D",
      "000000": "D&A,D&M",
      "010101": "D|A,D|M"
  ]
  
  // Destination Dictionary
  let destTable: [String: String] = [
      "000": "",
      "001": "M=",
      "010": "D=",
      "011": "DM=",
      "100": "A=",
      "101": "AM=",
      "110": "AD=",
      "111": "ADM="
  ]
  
  // Jump Dictionary
  let jumpTable: [String: String] = [
      "000": "",
      "001": ";JGT",
      "010": ";JEQ",
      "011": ";JGE",
      "100": ";JLT",
      "101": ";JNE",
      "110": ";JLE",
      "111": ";JMP"
  ]
  // process lines
  for line in lines {
    if (line.first == "0"){ // detect if A instruction
      // convert to decimal, then build instruction and add to hackList
      if let decimalValue = Int(String(line[line.index(line.startIndex, offsetBy: 1)..<line.index(line.startIndex, offsetBy: 16)]), radix: 2) {
        let instruction = "@" + String(decimalValue) + "\n"
        hackList.append(instruction)
      }
    }
    else if (line.first == "1"){ // detect if C instruciton
      print ("C bit")
    }
    else{
      print ("ERROR: Unable to parse file.")
      exit(1)
    }
    print(line)
  }
}
// todo: C instruct, write to file, exit
print ("Hello, World!")
