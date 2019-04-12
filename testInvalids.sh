#!/bin/bash

echo "Testing invalid syntax" > invalids.out

for filename in tests/invalids/*.gt; do
    echo "Testing $filename" >> invalids.out
    ./Goat $filename >> invalids.out
    echo "" >> invalids.out
done
