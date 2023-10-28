-- disassembler for the HACK assembly language
-- Written by Wyatt Shanahan, 2023

-- To use: this program takes 1 argument of example.hack, where example.hack is your hack assembly file.
-- To call: lua disassembler.lua example.hack
-- Check if correct number of arguments passed in
-- if not, then we send a message explaining how to call and exit
if #arg ~= 1 then
  print("INVALID. USE: lua disassembler.lua example.hack")
  os.exit(1)
end

local extension = string.sub(arg[1], -5)
-- check if file is a .hack file
if extension ~= ".hack" then
  print("INVALID: Incorrect file extension")
  os.exit(1)
  -- if a .hack file, then check if the file exists and can be opened
elseif extension == ".hack" then
  local file = io.open(arg[1], "r")
  if file then -- if file opens
    local checkError = 0
  elseif file ~= true then -- if file fails to open
    print("ERROR: File failed to open.")
    os.exit(1)
  end
  --create empty list to store HACK instructions
  local hackList = {}
  -- Computation dictionary
  local compTable = {
    ['101010'] = '0',
    ['111111'] = '1',
    ['111010'] = '-1',
    ['001100'] = 'D',
    ['110000'] = 'A,M',
    ['001101'] = '!D',
    ['110001'] = '!A,!M',
    ['001111'] = '-D',
    ['110011'] = '-A,-M',
    ['011111'] = 'D+1',
    ['110111'] = 'A+1,M+1',
    ['001110'] = 'D-1',
    ['110010'] = 'A-1,M-1',
    ['000010'] = 'D+A,D+M',
    ['010011'] = 'D-A,D-M',
    ['000111'] = 'A-D,M-D',
    ['000000'] = 'D&A,D&M',
    ['010101'] = 'D|A,D|M'
  }
  -- Destination dictionary
  local destTable = {
    ['000'] = '',
    ['001'] = 'M=',
    ['010'] = 'D=',
    ['011'] = 'DM=',
    ['100'] = 'A=',
    ['101'] = 'AM=',
    ['110'] = 'AD=',
    ['111'] = 'ADM='
  }
  -- Jump dictionary
  local jumpTable = {
    ['000'] = '',
    ['001'] = ';JGT',
    ['010'] = ';JEQ',
    ['011'] = ';JGE',
    ['100'] = ';JLT',
    ['101'] = ';JNE',
    ['110'] = ';JLE',
    ['111'] = ';JMP'
  }
  -- iterate over each line of the .hack file
  for line in file:lines() do
    -- operationCode tells us if it's an A or C instructions
    local operationCode = string.sub(line,1,1)

    
    -- if operationCode is 0, then it's an A instruction
    if operationCode == '0' then
      -- convert binary to decimal, then convert that to a string
      local val = tostring(tonumber(string.sub(line,2,16)))
      --concatenate to crate instruction
      local instruction = '@'..val
      -- insert into hacklist
      table.insert(hackList,instruction)
      --print(instruction)

    -- if operation code is 1, then it's a C instruction
    elseif operationCode == '1' then
      --create strings for aBit, compBits, destBits, and jmpBits
      --increase aBit by 1 since lua does not use 0 indexing
      local aBit = string.sub(line,4,4)
      local compBits = string.sub(line,5,10)
      local destBits = string.sub(line,11,13)
      local jmpBits = string.sub(line,14,16)
      -- return HACK instruction from destTable using destBits
      dest = destTable[destBits]
      -- return HACK instruction from compTable using compBits
       compUnfiltered = tostring(compTable[compBits])
      -- initialise/clear comp
      comp = ""
      -- filter compUnfiltered to find the actual computation
      -- use the a bit if a comma found
      if string.find(compUnfiltered, ",")then
        aZero,aOne = compUnfiltered:match("([^,]+),([^,]+)")
        if (aBit == "0") then
          comp = aZero
        elseif (aBit == "1") then
          comp = aOne
        end
        -- if no comma, then just use compUnfiltered
      elseif (string.find(compUnfiltered,",") ~= true) then
        comp = compUnfiltered
      end
      -- return HACK instruction from jumpTable using jmpBits
      jmp = tostring(jumpTable[jmpBits])
      -- concatenate instruction and add to hackList
      instruction = dest..comp..jmp
      table.insert(hackList,instruction)
    end
    -- create the asm file and output text
      local newfile = string.sub(arg[1], 1, -5)
      newfile = newfile .. 'asm'
      local writefile = io.open(newfile, "w")
      for i, value in ipairs(hackList) do
          writefile:write(value .. "\n")
    end
    --close the file
    writefile:close()
  end
end
