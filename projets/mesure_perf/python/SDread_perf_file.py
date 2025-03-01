#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Script to plot latency of SDcard from data files

"""

import time
import sys
import os.path
#from PIL import Image
from matplotlib import pyplot as plt
from matplotlib import image  as img


SDcard_available = ["16MB_canon",
                    "256MB_dane-elec",
                    "2GB_kingston",
                    "4GB_kingston",
                    "8GB_class10",
                    "32GB_transcend"]
sd_ID = ""

if len(sys.argv) > 1:
    sd_ID = SDcard_available[int(sys.argv[1])]
else:
    sd_ID = SDcard_available[0]

print("SDcard:", sd_ID)

read = True
mode = "read"
if len(sys.argv) > 2:
    if sys.argv[2] == 'w':
        read = False
        mode = "write"

print("Mode:", mode)



data = []
total_clk_cycles = 0
cnt = 0
with open('./perf/'+sd_ID+'/'+mode+'_latency3.txt', 'r' ) as latency:
    while True:
        buf = latency.readline()
        if not buf:
            break
        clk_cycles = int(buf)/100
        data.append(clk_cycles)
        total_clk_cycles += int(buf)
        cnt += 1

print(mode, "timing done.")
print("total clock cycle : ", total_clk_cycles, "i.e.", round(total_clk_cycles/1e8,3), "s")
print("total sectors:", cnt, end=". ")
print("average clock cycles per sector:", total_clk_cycles//cnt,  "i.e.", round(total_clk_cycles/(cnt*100), 2), "us")
print(mode, "speed:", round((cnt*512)/(total_clk_cycles/1e8)/1e6, 3),  "Mo/s")

plt.plot(data, linestyle = 'none', marker = 'x', color='b')


# data = []
# total_clk_cycles = 0
# cnt = 0
# with open('./perf/'+sdcard+'/read_latency1_no_PR.txt', 'r' ) as w_latency:
#     while True:
#         buf = w_latency.readline()
#         if not buf:
#             break
#         clk_cycles = int(buf)/100
#         data.append(clk_cycles)
#         total_clk_cycles += int(buf)
#         cnt += 1

# print("No PRE_ERASE:")
# print("Write timing done.")
# print("Total clock cycle : ", total_clk_cycles, "i.e.", round(total_clk_cycles/1e8,3), "s")
# print("Total sectors read:", cnt, end=". ")
# print("Average clock cycles per sector:", total_clk_cycles//cnt,  "i.e.", round(total_clk_cycles/(cnt*100), 2), "us")
# print("Read speed:", round((cnt*512)/(total_clk_cycles/1e8)/1e6, 3),  "Mo/s")

# plt.plot(data, linestyle = 'none', marker = '+', color='r')


# plt.savefig("/home/jeremy/vhdl_FAT/figures/"+sdcard+"/comparaison1.png")

# plt.show()
