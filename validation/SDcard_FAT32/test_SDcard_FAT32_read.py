#!/usr/bin/env python3

"""
This script will provide the following tests:
    - Reading files from the FAT image using a VHDL simulation
    - Check if contents are the same

./test_SDcard_FAT32_read.py keep --seed 99 --image_size 64 --all_check

"""

import subprocess
import os
import sys
import random
import time

def run(cmd, verbose=False):
    p = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, bufsize=-1)
    output, error = p.communicate()
    if verbose:
        print(output.decode(), end="", flush=True)
    if p.returncode != 0:
        print("="*60)
        print("ERROR in command\n    {}".format(cmd))
        print("----------- stdout ------------")
        print(output.decode(), end="", flush=True)
        print("----------- stderr ------------")
        print(error.decode(),  end="", flush=True)
        quit(1)

def time_string(time):
    time_str = ""
    if time >= 3600:
        time_str = str(round(time//3600)) + "h"
        time = time % 3600
    if time >= 60:
        time_str += str(round(time//60)) + "m"
        time = time % 60
    time_str += str(round(time)) + "s"
    return time_str

#--------------------------------------------------
#                 SCRIPT PARAMETERS
#--------------------------------------------------
#
# Use a seed to remember the FAT image generated if some errors occurs
#
if "--seed" in sys.argv:
    SEED = int(sys.argv[sys.argv.index("--seed")+1])
else:
    SEED = 2
    SEED = int(''.join([str(i) for i in os.urandom(4)]))
random.seed(SEED)
print("SEED:", SEED)


#
# Get the FAT image size in MB
#
if "--image_size" in sys.argv:
    IMAGE_SIZE = int(sys.argv[sys.argv.index("--image_size")+1])
else:
    IMAGE_SIZE = 64 # Mo


#
# Determinate if we test read of write
#
if "--write" in sys.argv:
    rw_mode = 1
else:
    rw_mode = 0

#
# Check some file of every files
#
if "--all_check" in sys.argv:
    ALL_CHECK = True
else:
    ALL_CHECK = False


#
# Get the name of the subdirectory where the test is held. Default: seed of the randomizer
#
if "--subtest" in sys.argv:
    sub_dir = sys.argv[sys.argv.index("--subtest")+1]
else:
    sub_dir = str(SEED)


#
# Get the directory where all the file for the test will be created. Default: /tmp/FAT_script/'sub_dir'/
#
if "--working_dir" in sys.argv:
    working_dir = sys.argv[sys.argv.index("--working_dir")+1]
else:
    working_dir = "/tmp/FAT_script/" + sub_dir + "/"

dev_dir       = "/home/jeremy/vhdl_FAT/"            # git 'root' directory, where all the sources can be found
data_dir      = working_dir + "data/"               # directory where files writted onto the FAT image are stored
split_FAT_dir = working_dir + "random_dir/"         # directory which is used to generated VHDL simulation files from the FAT image
print("Doing tests in directory:", working_dir)



#--------------------------------------------------
#                   INITIALISATION
#--------------------------------------------------
#
# Create working directory to /tmp
#
if not os.path.exists("/tmp/FAT_script/"):
    run(["mkdir", "/tmp/FAT_script/"])
if os.path.exists(working_dir):
    raise Exception("Destination exists... remove it first !")
run(["mkdir", working_dir])
run(["mkdir", data_dir])


start_time = time.time()
#
# Create a FAT image with random file
#
print("Creating FAT image " + "(path="+working_dir+"FAT_img)" + " of " + str(IMAGE_SIZE) + " MB...")
run([dev_dir+"projets/test_FAT32/python/create_FAT_image.py", "--path", working_dir, "--size", "{}".format(IMAGE_SIZE), "--seed", "{}".format(SEED)])

FAT_creation_time = time.time() - start_time
print("Time to create FAT image:", time_string(FAT_creation_time))

# get file create on the SD image
file_on_SD = [i for i in os.listdir(data_dir) if os.path.isfile(data_dir+i)]



#--------------------------------------------------
#                   SIMULATION
#--------------------------------------------------
#
# Split the FAT image in multiple file in a directory (for simulation)
#
run([dev_dir+"validation/raw_access/split_image.py", working_dir+"FAT.img", split_FAT_dir])

if "--manual_sim" in sys.argv:
    print("Done.")
    exit(0)

if ALL_CHECK:
    nb_file_to_check = len(file_on_SD)
    print(file_on_SD)
else:
    nb_file_to_check = 1 # random.randint(1, len(file_on_SD))
    while nb_file_to_check > len(file_on_SD):
        nb_file_to_check -= 1

file_checked = []

for iterator in range(nb_file_to_check):

    #
    # pick a file to read
    #
    if rw_mode==0:
        if ALL_CHECK:
            filename = file_on_SD[iterator]
            file_path = data_dir + filename
        else:
            filename = random.choice(file_on_SD)
            while filename in [i[0] for i in file_checked]:     # DON'T USE THIS IF WE CHECK AN 'IMPORTANT' PERCENTAGE OF FILES
                filename = random.choice(file_on_SD)
            file_path = data_dir + filename
        print("File to check:", filename)
    else:
        filename = "100000001.dat"
        file_path = working_dir+filename
        print("File to write:", filename)


    # #
    # # GHL simulation
    # #
    file_to_analyse = filename.replace('.', '').upper()
    if rw_mode == 1:
        file_analysed = working_dir + "write_" + filename
    else:
        file_analysed = working_dir + "read_" + filename
    nb_data_to_analyse = 512*8*5
    # print("File to analyse:", file_to_analyse)
    # print("Results in analysed:", file_analysed)

    print("Running ghdl...")
    run(["ghdl", "-a", dev_dir+"validation/raw_access/SDcard_raw_access_simmodel_V2.vhd", dev_dir+"HDL/SDcard_FAT32.vhd", dev_dir+"HDL/SDcard_FAT32_file.vhd", dev_dir+"testbenches/tb_SDcard_FAT32.vhd", dev_dir+"HDL/FIFO_Generic.vhd", dev_dir+"tools/random_gen_v1.vhd"])
    run(["ghdl", "-e", "tb_SDcard_FAT32"])

    if rw_mode == 1:
        run(["ghdl", "-r", "tb_SDcard_FAT32", "--stop-time=5ms", "-gIMAGE_DIRNAME="+split_FAT_dir, "-gFILE_TO_ANALYSE="+file_to_analyse, "-gFILE_ANALYSED="+file_analysed, "-gRW_MODE='1'", "-gNB_DATA_TO_OP="+str(nb_data_to_analyse)])
    else:
        run(["ghdl", "-r", "tb_SDcard_FAT32", "--stop-time=5ms", "-gIMAGE_DIRNAME="+split_FAT_dir, "-gFILE_TO_ANALYSE="+file_to_analyse, "-gFILE_ANALYSED="+file_analysed, "-gRW_MODE='0'", "-gNB_DATA_TO_OP="+str(nb_data_to_analyse)])
            
    print("Simulation done.")

    #
    # Recreate the FAT image
    #
    if rw_mode==1:
        if "keep" in sys.argv:
            run([dev_dir+"validation/raw_access/split_image.py", split_FAT_dir, working_dir+"FAT_out.img", "keep"])
        else:
            run([dev_dir+"validation/raw_access/split_image.py", split_FAT_dir, working_dir+"FAT_out.img"])
        run(["mtype", "-i", working_dir+"FAT_out.img", "::"+filename, ">", file_path])


    #--------------------------------------------------
    #                   START TESTS
    #--------------------------------------------------
    #
    # Read data from files
    #
    # File created by this script
    # file_path = data_dir+"00000411.dat" ## 251, 254 is also nice              # for temporary tests
    with open(file_path, "rb") as binfile:
        bytes_in = []
        while True:
            tmp = binfile.read(1)
            if not tmp:
                break
            bytes_in.append(tmp)
    
    # Data return by the module for the previous file
    filename_out = file_analysed #file_analysed # working_dir+"file_out.dat"    # for temporary tests
    with open(filename_out, "rb") as binfile:
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
        print(" -", file_path    + ":", len(bytes_in),  "bytes.")
        print(" -", filename_out + ":", len(bytes_out), "bytes.")
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

    file_checked.append(tuple([filename, success]))

#--------------------------------------------------
#                 END OF TESTS
#--------------------------------------------------
print("Tests over. Done in", time_string(time.time() - FAT_creation_time - start_time) + ".")
print("Summarize results:")

global_success = True
for i in file_checked:
    print(i[0] + ":", end=' ')
    if i[1]:
        print("success.")
    else:
        global_success = False
        print("failure.")

if global_success:
    print("All tests performed successfully!")
    if not "keep" in sys.argv:
        run(["rm", "-rf", working_dir])
else:
    print("Some errors occurred :(")
