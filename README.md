# FAT filesystem management on FPGA (PL only)

## Main Functionalities

- [x] Reading
- [x] Writing
- [x] Navigation in Folders
- [x] Creation of Folders
- [x] Safe Ejection
- [x] Multiple Files Operations Simultaneously
- [ ] Append File(s)
- [ ] Erase File / Folder
- [ ] List Function (ls)
- [ ] Format SD Card

*Description in a single entity was a constraint.*

## SD Card Module Usage

### Limitations and Recommendations
- This module is optimized for SD cards with minimal fragmentation.
- It is recommended to use a freshly formatted SD card with 64 or 128 sectors per cluster for optimal performance.

### Resource Utilization
- The module utilizes 1 BRAM + 2*MAX_FILES BRAM with the default configuration.
- Increasing the FIFO depth will also increase the number of BRAM used per file.

### Reading Data
1. **Open a File**: First, open a file in read mode.
2. **Read Data**: If the file opens successfully, data can be read from the file manager FIFO when it contains data. The file manager will automatically read the file to feed the FIFO as soon as possible.
3. **Close a File**: Closing a file in read mode typically takes 2 clock cycles. However, it may take longer if the file manager is currently reading sectors from the SD card, as these operations cannot be canceled.

### Writing Data
1. **Open a File**: First, open a file in write mode.
2. **Avoid Fragmentation**: To avoid fragmentation, provide a value to `reserv_clust` when opening the file. A value close to the file size is ideal but may not be possible if the partition is already fragmented.
3. **Write Data**: If the file is created successfully, data can be written to the file manager FIFO, which will then be written to the SD card when it contains enough data (defined by generics).
4. **Close a File**: Closing a file in write mode takes some time as it needs to empty buffers, write the chain of used clusters in the FAT, and update file information (file size and first cluster).

### Handling Simultaneous Operations
- If data is being written to a file at the same moment the user closes it (either manually or by ejecting the card), the data will still be written to the file.

### Creating Directories
- Creating a directory does not automatically open it. To create a file in a new directory, successfully create the directory first, then manually open it. If no issues are encountered, the file can be created.

### Ejecting the Card
- When ejecting the card, all file data inputs are cut off from the user's perspective, as if the user had manually closed the files.
- From the module's perspective, all remaining open files are requested to close simultaneously. The module will close written files one by one until all files are closed.
- The module then recopies FAT 1 into FAT 2 to ensure partition integrity. This step may take some time, depending on the card quality.



## TODO

- [ ] Complete README and usage documentation
- [ ] Rework main FSM of the main module
- [ ] Verify reset behavior

## Warnings

- **Filesystem Compatibility**: Specifically designed for FAT32 filesystem but compatible with other FAT filesystems upon minor changes.
- **Project Status**: This is an old project temporarily committed for historical purposes. It will be updated during my free time.
- **SD card HDL**: The SD card management architecture is accessible upon request.
