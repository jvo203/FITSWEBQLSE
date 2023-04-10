#! /bin/bash

# get a current date
DATE=$(date +%Y-%m-%d)

# get a date one week ago
DATE=$(date -d "$DATE - 7 days" +%Y-%m-%d)

# print it
echo "Starting sync for $DATE"
