#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Script to create a FAT image with a random number of file and with some "hole" in the FAT

./create_FAT_image.py [--path FAT_path] [--size image_size] [--seed nb]

"""

import os.path
import sys
import subprocess
import random

def create_FAT_image(fatimage, size=2**6, volume_name="A name"):
    subprocess.run(["dd", "if=/dev/zero", "of="+fatimage, "bs=1M", "count="+str(size)])
    loop = subprocess.check_output(["losetup", "--find", "--show", fatimage])
    loop = "/dev/loop" + str(loop[9]-48)
    print(loop)
    out_fd = open(fatimage + ".txt", "w") # [:fatimage.rfind('/')+1]+"log/"+fatimage[fatimage.rfind('/')+1:len(fatimage)-4]
    subprocess.run(["mkfs.fat", loop, "-F32", "-s8", "-n", volume_name, "-v"], stdout=out_fd)
    out_fd.close()
    subprocess.run(["losetup", "-d", loop])


def mcopy(fatimage, source, target=""):
    subprocess.run(["mcopy", "-i", fatimage, source, "::"+target])


def mdir(fatimage):
    subprocess.run(["mdir", "-i", fatimage])


def mdel(fatimage, filename):
    subprocess.run(["mdel", "-i", fatimage, filename])


def mshowfat(fatimage, filename=""):
    subprocess.run(["mshowfat", "-i", fatimage, "::"+filename])

def mmd(fatimage, directory_name=""):
    subprocess.run(["mshowfat", "-i", fatimage, directory_name])


# Path and Name of the FAT image
FAT_img = "FAT_img"
if "--path" in sys.argv:
        FAT_img = sys.argv[sys.argv.index("--path")+1]
        if not FAT_img.endswith('.img'): 
            FAT_img += "FAT.img"
else:
    FAT_img = "/home/jeremy/vhdl_FAT/format_parameters/" + FAT_img
    fat_id = 0
    while os.path.isfile(FAT_img+str(fat_id)+".img"):
        fat_id += 1
    FAT_img = FAT_img+str(fat_id)+".img"

# Size of the FAT image
if "--size" in sys.argv:
    image_size = int(sys.argv[sys.argv.index("--size")+1])
else:
    image_size = 64


# Seeding, random if not argument to set it
if "--seed" in sys.argv:
    seed = int(sys.argv[sys.argv.index("--seed")+1])
else:
    # seed = random.randrange(sys.maxsize)
    seed = int(''.join([str(i) for i in os.urandom(4)]))
random.seed(seed)
print("Seed:", seed)


create_FAT_image(FAT_img, image_size, str(seed)[0:8])
print("----> Partition created :", FAT_img)

current_free_size = 2**20 * (image_size-image_size*8//100)  # we keep 8% of free space (for FATS,...)
file_id = 0

CLUSTER_SIZE = 512 * 8
MAX_FREE_CLUSTERS = 100 * CLUSTER_SIZE 

while current_free_size > MAX_FREE_CLUSTERS:

    # add files
    nb_file = random.randint(512, 10000)
    # print("Add", nb_file,"files.")
    files = []

    if "--path" in sys.argv:
        file_path =  sys.argv[sys.argv.index("--path")+1] + "data/"
    else:
        file_path = "/home/jeremy/vhdl_FAT/format_parameters/data/"

    for i in range(nb_file):
        # print("File", i,":", end=" ")
        while os.path.isfile(file_path + "0" * (8-len(str(file_id)))+str(file_id)+".dat"):
            file_id += 1
        filename = "0" * (8-len(str(file_id))) + str(file_id) + ".dat"

        file_size = random.randint(1, current_free_size//10)
        # print(filename, "size:", file_size)
        with open(file_path + filename, 'wb' ) as fd:
            for i in range(file_size):
                fd.write(bytes([random.randint(0, 255)]))

        # print("Start mcopy")
        mcopy(FAT_img, file_path + filename)
        files.append(tuple([filename, file_size]))
        # files.append(filename)
        current_free_size -= file_size
        # print("File copied. Remaining size:", current_free_size)
        if current_free_size <= MAX_FREE_CLUSTERS:
            # print("FAT is full")
            break

    # print("Files copy done.")
    # mdir(FAT_img)

    # remove files
    nb_file_deleted = len(files) // 2
    # print("Remove", nb_file_deleted,"files:", end=" ")
    for i in range(nb_file_deleted):
        file_to_delete = random.choice(files)
        subprocess.run(["rm", "-rf", "{}".format(file_path+file_to_delete[0])])
        files.remove(file_to_delete)
        mdel(FAT_img, file_to_delete[0])
        current_free_size += file_to_delete[1]
        # print(file_to_delete, end=", ")
    print("")


print("Operations over...\nPrinting tree structure...")
mdir(FAT_img)

print("File in FAT:")
mshowfat(FAT_img) # :: (root)
for i in files:
    mshowfat(FAT_img, i[0])


