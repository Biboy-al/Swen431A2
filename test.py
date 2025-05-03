#!/usr/bin/python3

import os 


def readFile(fileName):
    f = open(fileName)
    readFile = f.read()
    return readFile

def extract_number(filename):
    return int(''.join(filter(str.isdigit, filename)))

pathInput = "input"
pathExpected = "expected"
outputFile = "output.txt"


dirInput = sorted([file for file in os.listdir(pathInput) if file.endswith(".txt")],
    key=extract_number
)

dirExcpected = sorted([file for file in os.listdir(pathExpected) if file.endswith(".txt")],
    key=extract_number
)

counter = 0
listOfFile = []
answer = []

for i in range(len(dirInput)):

    currInputFile = dirInput[i]
    currExcpectedFile = dirExcpected[i]


    os.system("ghc ws.hs")
    os.system("./ws " +pathInput + "/" + currInputFile)

    expcted = readFile(pathExpected + "/" + currExcpectedFile)
    actualOutput = readFile(outputFile)

    # print("Checking Input file "+ currInputFile + ":")
    # print("Checking Excpted file "+ currExcpectedFile + ":")

    # print(expcted)
    # print(actualOutput)
    listOfFile.append(currInputFile);
    if expcted == actualOutput:
        #print("true")
        answer.append("[V]");
        counter += 1
    else:
        answer.append("[X]");
        #print("false")

print("Test Cases")
for i in range (len(listOfFile)):
    print("     "+listOfFile[i] +" : " + answer[i])

print("(", len(dirInput) ," / ", counter , ") tests passed")