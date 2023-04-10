#! /bin/bash

# get a current date
DATE=$(date +%Y-%m-%d)

# get a date one week ago
DATE=$(date -d "$DATE - 7 days" +%Y-%m-%d)

# print it
echo "Starting sync for $DATE"

# make a sync command (run julia fits_sync $DATE)
CMD="julia /home/chris/Julia/fits_sync.jl $DATE"

# print the command
echo $CMD

# run the command
$CMD