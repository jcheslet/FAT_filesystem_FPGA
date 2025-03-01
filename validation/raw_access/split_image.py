#!/usr/bin/env python3

# This scripts splits an image file into a directory of smaller files so the
# image is easier to access from a VHDL simulation file. It also performs the
# reverse operation
#
# Script behavior is determined by first argument (file or directory)
#
#  split_image.py <source_image> <dest_dir>
#       creates the directory of chunks. The destination directory must not
#       exist. It will be created by the script. The image file size must be
#       a multiple of 512.
#     
#  split_image.py <source_dir> <dest_image>
#       reconstructs an image from a chunk directory. If the destination
#       already exists, it will not be overwritten and the script will produce
#       an error
#


MAIN_FILE_NAME  = "main.nfo"
SLICE_FILE_NAME = "ISO_{:08X}.dat"
CHUNK_SIZE      = 1024*1024

import sys
import os

source_name = sys.argv[1]
dest_name   = sys.argv[2]

if os.path.exists(dest_name):
    raise Exception("Destination exists... remove it first !")

if os.path.isdir(source_name):
    #################################
    # convert dir to image file
    #################################

    # read info from split image
    with open(os.path.join(source_name, MAIN_FILE_NAME), "r") as mainf:
        # info file first contains the number of 512-b blocks
        fsize = int(mainf.readline().strip())*512

        # then read chunk filenames
        chunknames = mainf.readlines()

    with open(dest_name, 'wb') as dest_img:
        for filename in chunknames:
            filename = filename.strip()
            if len(filename)==0:
                # This is to skip the last empty line of the info file
                # the last laine is mandatory to be understood by the VHDL model
                continue
            with open(os.path.join(source_name, filename), 'rb') as chunkf:
                # No need to specify chunk size, the read function gets the whole file
                # it is then easier to manage the last chunk which is not likely to have
                # the same size as others
                data = chunkf.read()
            dest_img.write(data)

else:
    ###############################
    # convert image file to dir
    ###############################

    # first get some details related to image size
    fsize  = os.path.getsize(source_name)
    blocks = fsize//512
    chunks = (fsize + CHUNK_SIZE - 1)//CHUNK_SIZE

    if blocks*512 != fsize:
        raise Exception("Image file is not a multiple of block size (512)")

    os.mkdir(dest_name)
    with open(os.path.join(dest_name, MAIN_FILE_NAME), "w") as mainf:
        # write the info file along the creation of image chunks
        # starts with number of 512-b blocks
        mainf.write(str(blocks)+'\n')
        with open(source_name, 'rb') as sourcef:
            for chunknum in range(chunks):
                # For each CHUNK_SIZE block, write new filename and copy data to new chunk
                mainf.write(SLICE_FILE_NAME.format(chunknum*2048) + '\n')
                data = sourcef.read(CHUNK_SIZE)
                with open(os.path.join(dest_name, SLICE_FILE_NAME.format(chunknum*2048)), 'wb') as dest:
                    dest.write(data)

