#!/bin/sh

# call n instances of ./bencode_write.native on the same file

N=$1
FILE=$2

echo "call script $N times on file $FILE"
for i in `seq $N` ; do
    ./bencode_write.native "$FILE" &
done

wait
