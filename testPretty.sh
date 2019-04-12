#!/bin/bash

echo "Testing pretty printing" > pretty.out
for filename in tests/*Ugly.gt; do
    echo $filename >> pretty.out
    ./Goat -p $filename >> pretty.out
    echo "" >> pretty.out
done
