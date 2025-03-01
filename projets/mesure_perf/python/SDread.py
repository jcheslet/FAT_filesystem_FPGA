#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Script to read SD card from computer's usb
Default port : /dev/sdb
"""

import serial
import time
#from PIL import Image
from matplotlib import pyplot as plt
from matplotlib import image  as img

sdcard = open('/dev/sdb', 'rb')

def p_hex_string(s):
    for c in s:
        print(hex(c), end=" ")
    print("")

# fd_dataReadFromSD = open("./dataRead", 'wb')

data = bytes([i%256 for i in range(512)])

nb_success=0
nb_failure=0
r_sdcard = data

# for i in range(50000):
i = 0
while i < 7700:# r_sdcard==data:
    r_sdcard = sdcard.read(512)
    # fd_dataReadFromSD.write(r_sdcard)
    if i==32 or i==(32+7583)or i==(32+7584)or i==(32+7585):
        p_hex_string(r_sdcard[0:512//2])
        p_hex_string(r_sdcard[512//2:])

    # if r_sdcard==data:
    #     if i%1e3==0:
    #         print(i, " : Success!")
    #     nb_success+=1
    # else:
    #     print(i, " : Failure!")
    #     nb_failure += 1
    #     p_hex_string(r_sdcard[0:512//2])
    #     p_hex_string(r_sdcard[512//2:])
    i += 1

print("Nb success: ", nb_success, ". Nb failure: ", nb_failure)

sdcard.close()
# fd_dataReadFromSD.close()
