#!/bin/bash

# Use texcount to get the total word count (sum of text and headers),
# and append it to a file with the date.

DATE=`date "+%Y-%m-%d"`

COUNT=`texcount -sum -merge $1 2> /dev/null| grep "Sum count" | awk '{ print $3 }'`

echo "${DATE} ${COUNT}"
