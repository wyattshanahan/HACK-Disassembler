#  This program converts Hack binary into Hack Assembly Language.
# To run, type: tclsh disassembler.tcl <fileName.hack>
if {[llength $argv] == 0} { #check if arguments provided, exit if none
  puts "ERROR: No input file provided. Use tclsh disassembler.tcl <fileName.hack>"
  exit 1
}

# Check if .hack file provided, exit if not
if {[string first ".hack" [lindex $argv 0]] < 0} {
    puts "ERROR: Input file must be a Hack file."
    exit 1
}

if {[file exists [lindex $argv 0]]} { # if file exists, then open and read lines
    set file [open [lindex $argv 0] r]
    set lines [split [read $file] "\n"]
    close $file # close file, data stored in $lines
} else { # if not found, then throw error and exit
    puts "ERROR: File does not exist."
    exit 1
}

set outList [list] # use for storing converted assembly code

# Computation Dictionary
set compTable [dict create \
    "101010" "0" \
    "111111" "1" \
    "111010" "-1" \
    "001100" "D" \
    "110000" "A,M" \
    "001101" "!D" \
    "110001" "!A,!M" \
    "001111" "-D" \
    "110011" "-A,-M" \
    "011111" "D+1" \
    "110111" "A+1,M+1" \
    "001110" "D-1" \
    "110010" "A-1,M-1" \
    "000010" "D+A,D+M" \
    "010011" "D-A,D-M" \
    "000111" "A-D,M-D" \
    "000000" "D&A,D&M" \
    "010101" "D|A,D|M"]

# Destination Dictionary
set destTable [dict create \
    "000" "" \
    "001" "M=" \
    "010" "D=" \
    "011" "DM=" \
    "100" "A=" \
    "101" "AM=" \
    "110" "AD=" \
    "111" "ADM="]

# Jump Dictionary
set jumpTable [dict create \
    "000" "" \
    "001" ";JGT" \
    "010" ";JEQ" \
    "011" ";JGE" \
    "100" ";JLT" \
    "101" ";JNE" \
    "110" ";JLE" \
    "111" ";JMP"]

puts "Hello, World!"

puts [llength $argv]

#process lines, convert, add to outarray, write to file, and display output message
