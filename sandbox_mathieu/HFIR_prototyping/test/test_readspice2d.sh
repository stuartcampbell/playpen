#!/bin/bash

# Clean up any old executable
rm read_test


echo "Compiling the test executable..."
g++ -O0 -g3 -o read_test test_readspice2d.cpp ../src/readspice2d.cpp -I ../inc -lPocoXML 
echo

echo "Running the tests..."
echo "--------------------"
./read_test
echo "--------------------"
