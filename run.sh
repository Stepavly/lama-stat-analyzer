make all
lamac -b Sort.lama
./byterun/byterun Sort.bc > result.txt
rm Sort.bc