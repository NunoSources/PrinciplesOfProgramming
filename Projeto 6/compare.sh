#!/bin/bash

# Comparar dois ficheiros linha a linha
DIFF=$(diff our_output.txt expected_output.txt -q)

if ["$DIFF" != ""] 
then
    echo "Ficheiros Iguais!"
else
    echo "Ficheiros Diferentes!"
fi
