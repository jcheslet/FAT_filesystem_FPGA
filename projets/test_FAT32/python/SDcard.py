#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Script to interact with the SDcard and the FPGA's architecture from serial port, received from FPGA
Default port : /dev/ttyUSB1

"""
import serial
import time
#from PIL import Image
from matplotlib import pyplot as plt
from matplotlib import image  as img

# sdcard = open('/dev/sdb', 'rb' )

sdcard = serial.Serial('/dev/ttyUSB1', baudrate=921600)

time.sleep(0.1)

if sdcard.in_waiting >0:
    sdcard.read(sdcard.in_waiting)

# cnt = 0
# data = []
# total_clk_cycles = 0
# cnt_consecutive_high_latency = 0

def nop():
    # Send command
    sdcard.write(b"\x00")
    
    # Check ACK
    r=sdcard.read(1)
    if r!=b"\xA0":
        print(str(r))
        raise Exception("NOP instruction didn't went well.")


def read_reg(addr):
    # Send command
    sdcard.write(b"\x02")
    # Send address
    if type(addr)==int:
        sdcard.write(addr.to_bytes(2, 'little'))
    else:
        sdcard.write(addr[::-1])

    # Check ACK
    r = sdcard.read(1)
    if r!=b"\xA2":
        print("read ack:", str(r))
        raise Exception("Read instruction didn't went well.")
    
    # Get data
    r=sdcard.read(4)[::-1]
    return r

    
def write_reg(addr, data):
    # Send command
    sdcard.write(b"\x03")
    # Send address
    if type(addr)==int:
        sdcard.write(addr.to_bytes(2, 'little'))
    else:
        sdcard.write(addr[::-1])
    # Send data
    if type(data)==int:
        sdcard.write(data.to_bytes(4, 'little'))
    else:
        sdcard.write(data[::-1])
    
    # Check ACK
    r = sdcard.read(1)
    if r!=b"\xA3":
        print(str(r))
        raise Exception("Write instruction didn't went well.")

def print_reg(addr, size=4):
    r=read_reg(addr)
    s=""
    for i in r[4-size:]:
        n = hex(i)
        if len(n)==3:
            n = "0x0" + n[2]
        s += n + " "
    return s

# def read_stream(stream, nb):

# def write_stream(stream, nb, data):

run = True

print("Test version Wrapper:", print_reg(b"\xEF\xFF"))
print("Test version FAT32  :", print_reg(b"\xFF\xFF"))

#-------------   Boot record   -------------

print("\n\n---------------------------------------------------")
print("- Boot Record parameters")
print("---------------------------------------------------")

print("Jump code                   :", print_reg(b"\xF1\x00"))
print("OEM name                    :", print_reg(b"\xF1\x01"), print_reg(b"\xF1\x02"))
print("Bytes_per_sectors           :", print_reg(b"\xF1\x03"))
print("sectors_per_cluster         :", print_reg(b"\xF1\x04"))
print("reserved_sectors            :", print_reg(b"\xF1\x05"))
print("nb_of_FAT                   :", print_reg(b"\xF1\x06"))
print("media_descriptor            :", print_reg(b"\xF1\x07"))
print("sectors_per_track           :", print_reg(b"\xF1\x08"))
print("nb_of_heads                 :", print_reg(b"\xF1\x09"))
print("nb_of_hidden_sectors        :", print_reg(b"\xF1\x0A"))
print("nb_of_sectors_in_partition  :", print_reg(b"\xF1\x0B"))
print("nb_of_sectors_per_FAT       :", print_reg(b"\xF1\x0C"))
print("active_FAT                  :", print_reg(b"\xF1\x0D"))
print("root_directory_cluster      :", print_reg(b"\xF1\x0E"))
print("sector_nb_file_system_info  :", print_reg(b"\xF1\x0F"))
print("sector_nb_backup_boot_sector:", print_reg(b"\xF1\x10"))
print("logical_drive_nb            :", print_reg(b"\xF1\x11"))
print("extended_signature          :", print_reg(b"\xF1\x12"))
print("serial_nb_of_partition      :", print_reg(b"\xF1\x13"))
print("volume_name_of_partition    :", print_reg(b"\xF1\x14"), print_reg(b"\xF1\x15"), print_reg(b"\xF1\x16"))
print("FAT_name                    :", print_reg(b"\xF1\x17"), print_reg(b"\xF1\x18"))
print("boot_signature              :", print_reg(b"\xF1\x19"))
print("BR_nb_free_cluster          :", print_reg(b"\xF1\x1A"))
print("recent_cluster              :", print_reg(b"\xF1\x1B"))
print("")
print("Module state           :", print_reg(b"\xFF\x03"))
print("Boot Record state      :", print_reg(b"\xFF\x04"))
print("Boot Record error state:", print_reg(b"\xFF\x05"))
# print("BR_bytes_cnt           :", print_reg(b"\xF1\x80"))
# print("BR_block_num           :", print_reg(b"\xF1\x81"))
# print("BR_loc_buffer          :", print_reg(b"\xF1\x82"))
# print("BR_address             :", print_reg(b"\xF1\x83"))


#-------------   FAT check   -------------
print("\n\n---------------------------------------------------")
print("- FAT check")
print("---------------------------------------------------")

print("FAT_nb_fat_check      :", print_reg(b"\xF2\x00"))
print("FAT_check_sector_done :", print_reg(b"\xF2\x01"))
print("FAT_difference        :", print_reg(b"\xF2\x02"))
print("FAT_read_buffer_cnt   :", print_reg(b"\xF2\x03"))
print("FAT_1_buffer          :", print_reg(b"\xF2\x04"))
print("FAT_all_sector_read   :", print_reg(b"\xF2\x05"))
print("FAT_nb_correct       :", print_reg(b"\xF2\x06"))
print("FAT_nb_diff          :", print_reg(b"\xF2\x07"))
print("")
print("FAT_cluster_table_size     :", print_reg(b"\xF2\x40"))
print("FAT_free_clusters_start    :", print_reg(b"\xF2\x41"))
print("FAT_free_clusters_cnt      :", print_reg(b"\xF2\x42"))
print("FAT_free_clusters_chaining :", print_reg(b"\xF2\x43"))
print("FAT_nb_free_clusters       :", print_reg(b"\xF2\x44"))
print("FAT_chain_to_store         :", print_reg(b"\xF2\x45"), print_reg(b"\xF2\x46"))
print("FAT_new_chain_to_store     :", print_reg(b"\xF2\x47"))
print("FAT_cluster                :", print_reg(b"\xF2\x48"))
print("FAT_bytes_cnt              :", print_reg(b"\xF2\x49"))
print("FAT_cluster_cnt            :", print_reg(b"\xF2\x4A"))
print("FAT_new_cluster            :", print_reg(b"\xF2\x4B"))
print("FAT_new_chain_buffer       :", print_reg(b"\xF2\x4C"), print_reg(b"\xF2\x4D"))
print("FAT_chain_buffer           :", print_reg(b"\xF2\x4E"), print_reg(b"\xF2\x4F"))
print("FAT_ordering               :", print_reg(b"\xF2\x50"))
print("FAT_free_cluster_table_ptr :", print_reg(b"\xF2\x51"))
print("FAT_new_chain_inserted     :", print_reg(b"\xF2\x52"))
print("")
print("FAT state       :", print_reg(b"\xFF\x06"))
print("FAT_block_num   :", print_reg(b"\xF2\x80"))
print("FAT_block_num_1 :", print_reg(b"\xF2\x81"))
print("FAT_block_num_k :", print_reg(b"\xF2\x82"))
print("FAT_op_buff     :", print_reg(b"\xF2\x83"))
print("FAT_loc_buffer  :", print_reg(b"\xF2\x84"))
print("FAT_address     :", print_reg(b"\xF2\x85"))
print("FAT_multiple    :", print_reg(b"\xF2\x86"))


#-------------   SDcard signals   -------------

print("\n\n---------------------------------------------------")
print("- SDcard control signals")
print("---------------------------------------------------")

print("SD_nb_blocks:", print_reg(b"\xFA\x00"))
print("SD_block_num:", print_reg(b"\xFA\x01"))
print("SD_op_buff  :", print_reg(b"\xFA\x02"))
print("SD_read_blk :", print_reg(b"\xFA\x03"))
print("SD_write_blk:", print_reg(b"\xFA\x04"))
print("SD_erase_blk:", print_reg(b"\xFA\x05"))
print("SD_multiple :", print_reg(b"\xFA\x06"))
print("SD_buffer   :", print_reg(b"\xFA\x09"))
print("SD_address  :", print_reg(b"\xFA\x0A"))
print("SD_write    :", print_reg(b"\xFA\x0B"))
print("SD_data_in  :", print_reg(b"\xFA\x0C"))
print("SD_data_out :", print_reg(b"\xFA\x0D"))
print("SD_busy     :", print_reg(b"\xFA\x07"))
print("SD_err_code :", print_reg(b"\xFA\x0E"))
print("Card detect :", print_reg(b"\xFA\x0F"))


sens = 1
value = 1
# while run:
#     write_reg(0, value)
#     if value==2**15:
#         sens=0
#     elif value==1:
#         sens=1
#     # print_reg(b"\x00\x00")

#     if sens:
#         value*=2
#     else:
#         value//=2

    # time.sleep(2)

print("Done!")

sdcard.close()

