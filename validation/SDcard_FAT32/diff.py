#!/usr/bin/env python3

"""
This script will search for difference between 2 files.
It basically is print what should print the diff unix command....

./diff.py keep file_1 file_2

"""


import os
import sys


if len(sys.argv) < 2:
    raise Exception("Not enough arguments. Need 2 files as inputs")

file_1 = sys.argv[1]
file_2 = sys.argv[2]

if not(os.path.isfile(file_1)):
    raise Exception(file_1, "is not a file.")
if not(os.path.isfile(file_2)):
    raise Exception(file_2, "is not a file.")

with open(file_1, "rb") as binfile:
    bytes_in = []
    while True:
        tmp = binfile.read(1)
        if not tmp:
            break
        bytes_in.append(tmp)

with open(file_2, "rb") as binfile:
    bytes_out = []
    while True:
        tmp = binfile.read(1)
        if not tmp:
            break
        bytes_out.append(tmp)

# Check sizes
success = True
size = 0
if len(bytes_in) != len(bytes_out):
    success = False
    print("Files have a different length:")
    print(" -", file_1    + ":", len(bytes_in),  "bytes.")
    print(" -", file_2 + ":", len(bytes_out), "bytes.")
    print("Still checking bytes (size to check is the shortest file)")
    size = min(len(bytes_in), len(bytes_out))
else:
    size = len(bytes_in)

# Check content
for i in range(size):
    if bytes_out[i] != bytes_in[i]:
        success = False
        print("ERROR: data read with SDcard_FAT32 module isn't the same has the original file (bytes {:d} (0x{:X}))".format(i, i))
        break
    if success == False and i == size-1:
        print("Files are the same, but output of vhdl sim is just too short (not enough time to read the whole file?)")

# Test for this file over
if success:
    print("All tests performed successfully.")
