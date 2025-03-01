#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Script to read latency of SD card from serial port, received from FPGA
Default port : /dev/ttyUSB1

"""
import serial
import time
import sys
import os.path
#from PIL import Image
from matplotlib import pyplot as plt
from matplotlib import image  as img

# sdcard = open('/dev/sdb', 'rb' )

sdcard = serial.Serial('/dev/ttyUSB1', baudrate=921600)

time.sleep(0.1)
if sdcard.in_waiting >0:
    sdcard.read(sdcard.in_waiting)

nb_sector = 0
sector_to_cnt = 0
data = []
total_clk_cycles = 0
cnt_consecutive_high_latency = 0

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

if sd_ID == "16MB_canon":
    sector_to_cnt = 28740                       #  16 MB
elif sd_ID == "256MB_dane-elec":
    sector_to_cnt = 2**18+2**17+2**16+2**15     # 256 MB
elif sd_ID == "2GB_kingston":
    sector_to_cnt = 2**21+2**20+2**19+2**18     #   2 GB (reality: 4,022,272 sectors)
elif sd_ID == "4GB_kingston":
    sector_to_cnt = 2**22+2**21+2**20           #   4 GB
elif sd_ID == "8GB_class10":
    sector_to_cnt = 2**23+2**22+2**21           #   8 GB
elif sd_ID == "32GB_transcend":
    sector_to_cnt = 2**25+2**24+2**23+2**22     #  32 GB (reality: 63,278,013 sectors)
else:
    raise Exception("No scard selected")

progress_bar_size = 100
start_time = time.time()

while nb_sector < sector_to_cnt:
    if sdcard.in_waiting>0:
        r_sdcard = sdcard.read(3)
        # print(r_sdcard[0], r_sdcard[1], r_sdcard[1]*256, r_sdcard[2], r_sdcard[2]*256*256)
        if r_sdcard[2] >= 1:
            cnt_consecutive_high_latency += 1
            if cnt_consecutive_high_latency==10:
                raise Exception("Too much consecutive high latency. It seems the 3-byte data is not well read.")
        else:
            cnt_consecutive_high_latency = 0
        
        clk_cycles = 256*(256*r_sdcard[2] + r_sdcard[1]) + r_sdcard[0]
        total_clk_cycles += clk_cycles
        data.append(clk_cycles)
        nb_sector += 1
        # Print progress
        if (nb_sector % (progress_bar_size**2)) == 0: #(nb_sector % (sector_to_cnt//(progress_bar_size**2)))
            print("["+ "#"*(nb_sector//(sector_to_cnt//progress_bar_size)), end="")                             # print advancement
            print(" "*(progress_bar_size - nb_sector//(sector_to_cnt//progress_bar_size) - 1) + "]", end=" ")       # print spaces and "stop" bracket
            time_per_symbole = (time.time() - start_time) / (nb_sector / (sector_to_cnt/progress_bar_size))     # Calculate time per "percentage"
            ETR = int(time_per_symbole * (progress_bar_size - (nb_sector /(sector_to_cnt/progress_bar_size))))  # Caculate ETR
            ETR_str = ""
            if ETR >= 3600:
                ETR_str = str(ETR//3600) + "h"
                ETR = ETR % 3600
            if ETR >= 60:
                ETR_str += str(ETR//60) + "m"
                ETR = ETR % 60
            ETR_str += str(ETR) + "s"
            print("ETR=", ETR_str +" "*9, sep="", end='\r', flush=True)                                         # print ETR



sdcard.close()

print("")
print("Data received.")
print("Total clock cycles:", total_clk_cycles, "i.e.", round(total_clk_cycles/1e8,3), "s")
print("Total sectors read:", nb_sector, end=". ")
print("Average clock cycles per sector:", total_clk_cycles//nb_sector,  "i.e.", round(total_clk_cycles/(nb_sector*100), 2), "us")
# print("Writing speed:", round((nb_sector*512)/(total_clk_cycles/1e8)/1e6, 3),  "Mo/s")
print("Reading speed:", round((nb_sector*512)/(total_clk_cycles/1e8)/1e6, 3),  "Mo/s")

nb = 1

while os.path.isfile("./perf/"+sd_ID+"/read_latency"+str(nb)+".txt"):
    nb += 1

with open("./perf/"+sd_ID+"/read_latency"+str(nb)+".txt", 'w') as fd:
    for i in data:
        fd.write(str(i)+"\n")


plt.plot(data, linestyle = 'none', marker = '+')
plt.savefig("/home/jeremy/vhdl_FAT/figures/"+sd_ID+"/read_latency"+str(nb)+".png")
print("read_latency"+str(nb)+".png saved.")
if sector_to_cnt < 2**20:
    plt.savefig("/home/jeremy/vhdl_FAT/figures/"+sd_ID+"/read_latency"+str(nb)+".svg") 
    print("read_latency"+str(nb)+".svg saved.")

print("Done!")

# plt.show()
