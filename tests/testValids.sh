#!/bin/bash

echo "Running valid test" > valids.out
for filename in *.gt; do
    echo "Testing $filename" >> valids.out
    ./Goat -p $filename >> valids.out
    echo "" >> valids.out
done
