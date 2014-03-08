#!/bin/bash

# Use texcount to get the total word count (sum of text and headers),
# and append it to a file with the date.

DATE=`date "+%Y-%m-%d"`

WORDS=`texcount -sum -merge $1 2> /dev/null| grep "Sum count" | awk '{ print $3 }'`

PAGES=`pdfinfo -meta $2 | grep "Pages" | awk '{ print $2 }'`

echo "${DATE} ${WORDS} ${PAGES}"
