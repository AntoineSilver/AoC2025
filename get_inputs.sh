#!/bin/bash

# Load the session cookie (user id for personalized input)
. ./.env

YEAR=2025
NBDAY=12

DAY=1
while [ $DAY -le $NBDAY ]; do
    echo "Downloading input for Day $DAY..."
    if [ $DAY -le 9 ]; then
        curl -s -b "session=${SESSION_COOKIE}" "https://adventofcode.com/${YEAR}/day/${DAY}/input" -o "./input/Day0${DAY}.txt"
    else
        curl -s -b "session=${SESSION_COOKIE}" "https://adventofcode.com/${YEAR}/day/${DAY}/input" -o "./input/Day${DAY}.txt"
    fi
    if [ $? -eq 0 ]; then
        echo "Successfully downloaded input for Day $DAY."
    else
        echo "Failed to download input for Day $DAY."
    fi

    DAY=$((DAY+1))
done

echo "All inputs downloaded."
