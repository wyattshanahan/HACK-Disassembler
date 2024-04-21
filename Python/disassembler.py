#  This program converts Hack binary into Hack Assembly Language.
# To run, type: python disassembler.py <fileName.hack>
# if on a Linux system, you may need to run it with: python3 disassembler.py <fileName.hack>

import sys
from os.path import exists as file_exists

if len(sys.argv) < 2:  # check that argument/input file provided
  print(
      "ERROR: No input file provided. Use python disassembler.py <fileName.hack>"
  )
  exit(1)
# Check if appropriate file extension
if ".hack" not in sys.argv[1]:
  print("ERROR: Input file must be a Hack file.")
  exit(1)
else:
  if file_exists(sys.argv[1]): # if file exists, then read lines from it
    file = open(sys.argv[1], 'r')
    Lines = file.readlines()

    # List to be used for storing HACK instructions
    hackList = []

    # Computation Dictionary
    compTable = {
        '101010': '0',
        '111111': '1',
        '111010': '-1',
        '001100': 'D',
        '110000': 'A,M',
        '001101': '!D',
        '110001': '!A,!M',
        '001111': '-D',
        '110011': '-A,-M',
        '011111': 'D+1',
        '110111': 'A+1,M+1',
        '001110': 'D-1',
        '110010': 'A-1,M-1',
        '000010': 'D+A,D+M',
        '010011': 'D-A,D-M',
        '000111': 'A-D,M-D',
        '000000': 'D&A,D&M',
        '010101': 'D|A,D|M'
    }

    # Destination Dictionary
    destTable = {
        '000': '',
        '001': 'M=',
        '010': 'D=',
        '011': 'DM=',
        '100': 'A=',
        '101': 'AM=',
        '110': 'AD=',
        '111': 'ADM='
    }

    # Jump Dictionary
    jumpTable = {
        '000': '',
        '001': ';JGT',
        '010': ';JEQ',
        '011': ';JGE',
        '100': ';JLT',
        '101': ';JNE',
        '110': ';JLE',
        '111': ';JMP'
    }

    # Loop through all assembly lines in the HACK file
    for line in Lines:
      if line[0] == '0': # if first bit is 0, then it is an A instruction
        val = str(int(line[1:16], 2))  # Get rest of string, convert to decimal, then to string
        instruction = '@' + val + '\n'  # build instruction
        hackList.append(instruction) # add instruction to list

      elif line[0] == '1': # if first bit is 1, then this is a C instruction
        aBit = line[3] # splice for retrieving the A bits
        compBits = line[4:10] # splice to get compBits
        destBits = line[10:13] #splice to get destbits
        jmpBits = line[13:16] # splice out jumpbits
        dest = destTable[destBits] # get destination string from destTable
        compUnfiltered = compTable[compBits] #grab unfiltered computation from compTable
        comp = compUnfiltered.split(',')[int(aBit)] # use aBit to determine the comp instruction
        jmp = jumpTable[jmpBits] # grab jump location from jumpTable
        instruction = dest + comp + jmp + '\n' # build instruction
        hackList.append(instruction) # add instruction to list
        
    file = open(sys.argv[1].replace('.hack', '.asm'), 'w') # create and open file
    file.writelines(hackList) # write to file
    print("File written to " + sys.argv[1].replace('.hack', '.asm'))
    file.close()

  else: # if file does not exist, then print error
    print("ERROR: File " + sys.argv[1] + " does not exist.")
    exit(1)
