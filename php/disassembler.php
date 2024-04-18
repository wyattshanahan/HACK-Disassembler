<?
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
/* write file opening and check file is .hack*/
/* read lines*/

$compTable = [
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

$destTable = [
  '000' => '',
  '001' => 'M=',
  '010' => 'D=',
  '011' => 'DM=',
  '100' => 'A=',
  '101' => 'AM=',
  '110' => 'AD=',
  '111' => 'ADM='
];

$jumpTable = [
  '000' => '',
  '001' => ';JGT',
  '010' => ';JEQ',
  '011' => ';JGE',
  '100' => ';JLT',
  '101' => ';JNE',
  '110' => ';JLE',
  '111' => ';JMP'
];
output("Hello, world!");
?>
