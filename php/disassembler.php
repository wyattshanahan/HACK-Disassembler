<?
/* This program converts Hack binary into Hack Assembly Language.
To run, type: php disassembler.php <fileName.hack>

This program is intended for use in PHP CLI*/

function output($message)  /*function to output to console*/
{
    $message = date("H:i:s") . " - $message ".PHP_EOL;
    print($message);
    flush();
}

if ($argc < 2){ // check if any arguments provided, exit if not
  output("ERROR: No input file provided. Use php disassembler.php <input file>");
  die("");
}

if (strpos($argv[1],".hack")==false){ // check if valid file type provided as argument
  output("ERROR: Input file must be a .hack file");
  die("");
}

$compTable = [ // create lookup table
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
];

$destTable = [ // create lookup table
  '000' => '',
  '001' => 'M=',
  '010' => 'D=',
  '011' => 'DM=',
  '100' => 'A=',
  '101' => 'AM=',
  '110' => 'AD=',
  '111' => 'ADM='
];

$jumpTable = [ // create lookup table
  '000' => '',
  '001' => ';JGT',
  '010' => ';JEQ',
  '011' => ';JGE',
  '100' => ';JLT',
  '101' => ';JNE',
  '110' => ';JLE',
  '111' => ';JMP'
];

$rawData = file($argv[1]); //open/file
$outputArray = []; //define array to store translated instructions
$i = 1; //counter for data processing
foreach($rawData as $line){ // iterate/operate on each line
  if ($line[0] == '0'){ // if 0, then A instruction
    $val = substr($line,1,15); // splice line to get destination
    $val = bindec($val); // convert to binary
    $val = strval($val); // convert back to string
    $instruction = '@' . $val . "\n"; // build instruction
    $outputArray[]=$instruction; // add instruction to array
  }
  else if($line[0] == '1'){ // else if 1, then C instruction
    $aBit = $line[3];
    $compBits = substr($line, 4, 6); // Extracts characters from index 4 to 9
    $destBits = substr($line, 10, 3); // Extracts characters from index 10 to 12
    $jmpBits = substr($line, 13, 3); // Extracts characters from index 13 to 15
    $dest = $destTable[$destBits]; // convert destbits
    $jmp = $jumpTable[$jmpBits]; // convert jumpbits
    $compUnfiltered = $compTable[$compBits]; // get compbits
    $compUnfilteredParts = explode(',', $compUnfiltered); // explode compbits
    $aBitIndex = intval($aBit); // convert to int
    $comp = $compUnfilteredParts[$aBitIndex]; // find specific comp value
    $instruction = $dest . $comp . $jmp . "\n"; // build instruction
    $outputArray[]=$instruction; // add instruction to array
  }
  else{ // if no dest bit and unable to detect A or C function
    output("ERROR: Unable to parse line $i");
    die("");
  }
  $i = $i + 1; // increment line counter
}
// Replace the extension .hack with .asm in the file name
$filename = str_replace('.hack', '.asm', $argv[1]);

// Open the file in write mode
$outfile = fopen($filename, 'w');
// Write the lines from $outputArray to the file
foreach ($outputArray as $line) {
    fwrite($outfile, $line);
}
fclose($outfile); // close file
output("File written to $filename"); // output success message
?>
