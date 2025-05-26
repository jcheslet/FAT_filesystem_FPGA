# FAT filesystem management on FPGA (PL only)

## Overview

This project aims to develop a VHDL module to manage a FAT filesystem, specifically FAT32 filesystem, to read/write on SD card images on FPGA.

The goal of this FAT filesystem management module on FPGA is to provide access to organized SD card storage on FPGA only board.

Speciafically designed for FAT32 filesystem, this module interacts with one SD card, from initializion to ejection.
It was originally made ensure real-time operations with the SD card, but note that latencies depends and varies between SD card and technologies. In short, old sd card techonology is more stable and reliable but slower.


## Key Features

- **Reading** a file (similar to `cat`).
- **Writing** in a new file (necessarily create it).
- **Simultaneous multiple files** operations with simple **scheduling**.
- **Folder** creation and navigation (`mdkir`, `cd`).
- **Safe ejection** to ensure data integrity.
- **FIFO based interface** to transfer data to FAT module.

## Recommendations & Limitations
- This module is optimized for SD cards with minimal fragmentation.
- It is recommended to use a freshly formatted SD card with 64 or 128 sectors per cluster for optimal performance.
- Current implementation does not handle files larger than 2GB.


## SD Card Module Usage

### Reading Data
1. **Open a File**: First, open a file in read mode.
2. **Read Data**: If the file opens successfully, data can be read from the file manager and puts its content in a FIFO. The file manager will automatically read the file to feed the FIFO as soon as possible.
3. **Close a File**: Closing a file in read mode typically takes 2 clock cycles. However, it may take longer if the file manager is currently reading sectors from the SD card, as these operations cannot be canceled.

### Writing Data
1. **Open a File**: First, open a file in write mode.
2. **Avoid Fragmentation**: To avoid fragmentation, provide a coherent value to `reserv_clust` when opening the file. A value close to the file size is ideal but may not be possible if the partition is already fragmented.
3. **Write Data**: If the file is created successfully, data can be written to the file manager FIFO, which will then be written to the SD card when it contains enough data.
4. **Close a File**: Closing a file in write mode takes some time as it needs to empty buffers, write the chain of used clusters in the FAT, and update file information (file size and first cluster).

### Handling Simultaneous writing and file closing
- If data is being written to a file at the same moment the user closes it (either manually or by ejecting the card), the data will still be written to the file.

### Creating Directories
Creating a directory does not automatically open it.

To create a file in a new directory:
1. Successfully create the directory
2. Manually open it
3. If no issues are encountered, the file can be created.

### Ejecting the card
- When ejecting the card, all file data inputs are cut off from the user's perspective, as if the user had manually closed all files.
- From the module's perspective, all remaining open files are requested to close simultaneously. The module will close written files one by one until all files are closed.
- The module then recopies FAT 1 into FAT 2 to ensure partition integrity. This step may take some time depending on the card quality.
- Main module informs user with a 1-bit signal when the SD card can be safely ejected.

## Development

*To ease the use of the module for very beginner in VHDL/FPGA design, it was a constraint to describe the architecture in a single entity*

### Main Entity Generics

| **Generic Name**         | **Type**          | **Default Value** | **Description**                                                                                      |
|--------------------------|-----------|-------------------|--------------------------------------------------------------------------------------------------------------|
| `CLK_FREQ_HZ`            | integer           | 100000000         | Clock frequency (Hz).                                                                                |
| `DIRNAME`                | string            | "simulation only" | Path of SD. For simulation only.                                                                     |
| `HIGH_SPEED_IF`          | boolean           | False             | When True, runs SD card bus at 50MHz (might not be reliable).                                        |
| `LITTLE_ENDIAN`          | boolean           | True              | When multiple bytes per word, indicates which byte is stored at the lower address.                   |
| `MAX_FILES`              | integer: 1~       | 1                 | Number of files that can be opened simultaneously.                                                   |
| `WORD_SIZE_PW2`          | integer: 0~       | 4                 | Data size (power of 2).                                                                              |
| `FIFO_DEPTH`             | integer: 4096~    | 4096              | Maximum size of internal FIFO for reading (for each file)                                            |
| `NB_MULTIPLE_OP`         | integer: 0~256    | 4                 | Number of sectors read/written when processing file multi-operations.                                |
| `FORMAT_CLUSTER_SIZE`    | integer: 1~8      | 7                 | Power of 2 indicating the size of the cluster (default: 7 => 7 * 512 bytes, i.e., 64KB per cluster). |
| `TREE_STRUCTURE_MAX`     | integer: 0~       | 3                 | Maximum number of subfolders from the root directory.                                                |


### Resource Utilization
- The module utilizes 1 BRAM + 2*MAX_FILES BRAM with the default configuration.
- Increasing the FIFO depth will also increase the number of BRAM used per file.

## Known bugs

#### Sdcard_FAT32:
- Issues with opening directories after card ejection (`cd`). Works with fresh bitstream with issues.
- Files cannot be created if no space is available in the directory (need to append the directory).

#### Sdcard_FAT32_file:
- No limitation for the number of reserved fragments, potentially overwriting the first fragment.


## Warnings

- **Filesystem Compatibility**: Specifically designed for FAT32 filesystem but compatible with other FAT filesystems upon minor changes.
- **SD card HDL**: The SD card management architecture is accessible upon request.


## Feature track 

| **Feature**              | **Written (VHDL)** | **Validated (simulation)** | **Valited (on board)** |
|--------------------------|--------------------|----------------------------|------------------------|
| Reading (file)           | ✔️                 | ✔️                         | ✔️                     |
| Writing (create file)    | ✔️                 | ✔️                         | ✔️                     |
| Append (file)            |                    |                            |                        |
| Erase (file/folder)      |                    |                            |                        |
| Multi-file               | ✔️                 | ✔️                         | ✔️                     |
| Multi-operation          | ✔️                 | ✔️                         | ✔️                     |
| Folder creation (`mkdir`)| ✔️                 |                            |                        |
| Folder navidation (`cd`) | ✔️                 | ✔️                         |                        |
| list directory (`ls`)    | -                  |                            |                        |
| Folder append            | -                  |                            |                        |
| Format                   |                    |                            |                        |
| Eject                    | ✔️                 | ✔️                         | ✔️                     |
| Reset                    | ✔️                 |                            | ✔️                     |


_Multi-operation is a mode to accelerate reading and writting that use SD card internal structure._

### Others todos

- [ ] Complete README and usage documentation
- [ ] Rework main FSM of the main module (seperate file descriptor check)
- [ ] Add directory append (store more files if limits it)
- [ ] Add formatting and file erase functions
- [ ] Verify reset behavior
- [ ] Fragment main file to many sub-modules...
- [ ] Improve scheduling: add prioritize, patterns,..., to optimize multiple operations (read/write)
- [ ] Add performances comparison table to compare with raw read/write on SD card, with multiple SD cards for comparison.


## Disclaimer

This project is few years old. Please note that codes, comments and documentation quality might be low and/or outdated.

While efforts were made to ensure functionality and clarity, there may be areas that require improvement or further optimization.

I intend to update and clean the project when I have the opportunity.
