#  This program converts Hack binary into Hack Assembly Language.
# To run, type: ruby disassembler.rb <fileName.hack>

if ARGV.empty? # check if any argument/file provided
  puts 'ERROR: No input file provided. Use ruby disassembler.rb <fileName.hack>'
  exit(1) # exit with error
end

if ARGV[0] && !ARGV[0].include?(".hack") # check if file is a .hack file
  puts "ERROR: Input file must be a .hack file"
  exit(1) # exit with error
end

compTable = { # declare compTable
  '101010' => '0',
  '111111' => '1',
  '111010' => '-1',
  '001100' => 'D',
  '110000' => 'A,M',
  '001101' => '!D',
  '110001' => '!A,!M',
  '001111' => '-D',
  '110011' => '-A,-M',
  '011111' => 'D+1',
  '110111' => 'A+1,M+1',
  '001110' => 'D-1',
  '110010' => 'A-1,M-1',
  '000010' => 'D+A,D+M',
  '010011' => 'D-A,D-M',
  '000111' => 'A-D,M-D',
  '000000' => 'D&A,D&M',
  '010101' => 'D|A,D|M'
}

destTable = { # declare destTable
  '000' => '',
  '001' => 'M=',
  '010' => 'D=',
  '011' => 'DM=',
  '100' => 'A=',
  '101' => 'AM=',
  '110' => 'AD=',
  '111' => 'ADM='
  }

jumpTable = { # declare jumpTable
  '000' => '',
  '001' => ';JGT',
  '010' => ';JEQ',
  '011' => ';JGE',
  '100' => ';JLT',
  '101' => ';JNE',
  '110' => ';JLE',
  '111' => ';JMP'
}

rawData = File.readlines(ARGV[0]) # read lines from the file and store as rawData
outputArray = []

rawData.each do |line| # iterate over each line
  if line[0] == '0' # if 0, then this is an A instruction
    val = line[1, 15].to_i(2).to_s # convert to decimal
    instruction = "@" + val + "\n" # build instruction
    outputArray << instruction # add instruction to output array
  end
  if line[0] == '1' # if 1, then this is a C instruction
    aBit = line[3]
    compBits = line[4..9].to_s # grab comp bits from line
    destBits = line[10..12].to_s # grab destination bits from line 
    jmpBits = line[13..16].to_s # grab jump bits from line
    dest = destTable[destBits.to_s] # look destbits up in destTable
    jmp = jumpTable[jmpBits].to_s # look jump bits up in jumpTable
    compUnfiltered = compTable[compBits].to_s # look up comp in comptable
    comp = compUnfiltered.split(',')[aBit.to_i].to_s # filter comp to find operation
    instruction = dest + comp + jmp + "\n" # build instruction
    outputArray << instruction # add instruction to output array
  end
  if line[0] != '0' && line[0] != '1' #if no dest bit and unable to detect A or C function
    puts 'ERROR: Unable to parse data'
    exit(1)
  end
end

file = File.open(ARGV[0].sub('.hack', '.asm'), 'w') do |file| # create file switching .hack to .asm
  file.puts outputArray # write array to file
  puts 'File written to ' + ARGV[0].sub('.hack','.asm') # output file name
end
