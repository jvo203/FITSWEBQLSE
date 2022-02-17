#!/usr/bin/env bash
set -e
pid=$1

logfile="memory_usage.csv"
start=$(date +%s)

printf """elapsed time [s]"",""rss""\n" > "$logfile"

# get the process' memory usage and run until `ps` fails which it will do when
# the pid cannot be found any longer

while mem=$(ps -o rss= -p "$pid"); do
    time=$(date +%s)

    # print the time since starting the program followed by its memory usage
    printf "%d %s\n" $((time-start)) "$mem" >> "$logfile"

    # sleep for a tenth of a second
    sleep .1
done

printf "Find the log at %s\n" "$logfile"

# ps -m -o pid,vsz,rss,%mem,command
