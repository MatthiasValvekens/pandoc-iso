#!/bin/bash


if [ "$#" -ne 2 ]; then
    echo "Usage: build-draft INFILE OUTFILE"
    exit 1
fi


infile=/inputs/$1
outfile=/outputs/$2

pandoc --filter=/usr/bin/pandoc-iso \
    --citeproc --csl=/var/pandoc/biblio.csl \
    --reference-doc /var/pandoc/reference.docx \
    -f markdown-latex_macros -t docx \
    --no-highlight \
    -o $outfile $infile
