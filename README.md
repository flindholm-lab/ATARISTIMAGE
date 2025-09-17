# Atari .ST File Manager

![Atari .ST File Manager GUI](https://via.placeholder.com/400x250?text=Atari+.ST+File+Manager+GUI)

## Overview
A Lazarus/Free Pascal tool for managing files and directories on Atari ST disk images (`.st` files). It allows you to add, remove, create, and extract files and directories from FAT12-formatted disk images, commonly used for Atari ST emulation.

---

## Features
- **Open and Create Disk Images**: Open existing `.st` images or create new 720KB images.
- **File Operations**: Add, remove, and save files to/from the disk image.
- **Directory Operations**: Create and remove directories within the image.
- **Navigation**: Browse directories, go back to parent directories, and double-click to enter subdirectories.
- **Free Space Display**: Visual feedback on utilized and free space in the image.
- **Pending Operations**: Preview changes before writing them to the image.

---

## Requirements
- Lazarus 4 (or Free Pascal with LCL). For building.
- Tested on Windows.

---

## Usage

### 1. Open or Create an Image
- Click **"Open Image"** to load an existing `.st` file.
- Click **"Create 720KB Image"** to create a new blank 720KB disk image.

### 2. Navigate the Disk Image
- The current directory path is displayed at the top.
- Double-click a directory to enter it.
- Click **"Back"** to return to the parent directory.

### 3. Add Files
- Click **"Add File"** to select files from your computer.
- Added files are marked with `*A*` in the list and will be written to the image when you click **"Write Image"**.

### 4. Create Directories
- Click **"Create Directory"** and enter a name (max 8 characters).
- New directories are marked with `*A*` in the list.

### 5. Remove Files or Directories
- Select a file or directory and click **"Remove File"** or **"Remove Directory"**.
- Removed items are marked with `*D*` in the list.

### 6. Save Files to Your Computer
- Select a file and click **"Save File"** to extract it from the image.

### 7. Write Changes to the Image
- Click **"Write Image"** to apply all pending changes (additions, deletions, and new directories) to the disk image.
- You will be prompted to save the modified image.

---

## How It Works

### FAT12 Filesystem Support
- The tool reads and writes FAT12-formatted disk images, including:
  - Boot sector parsing.
  - FAT table management.
  - Directory entry handling.
  - Cluster allocation and deallocation.

### File Operations
- **Adding Files**: Files are written to the image, and the FAT is updated to allocate clusters.
- **Removing Files**: Files are marked as deleted in the directory entry, and their clusters are freed in the FAT.
- **Creating Directories**: New directories are created with `.` and `..` entries, and the FAT is updated.

### Free Space Calculation
- The tool calculates free space based on the image size (360KB, 720KB, or 800KB) and displays the utilized space percentage.

---

## Example Workflow

### Adding Files to an Image
1. Open an existing `.st` image or create a new one.
2. Click **"Add File"** and select files from your computer.
3. Click **"Write Image"** to save the changes.

### Creating a Directory
1. Navigate to the desired directory.
2. Click **"Create Directory"** and enter a name.
3. Click **"Write Image"** to create the directory.

### Extracting a File
1. Navigate to the file you want to extract.
2. Select the file and click **"Save File"**.
3. Choose a location on your computer to save the file.

---

## Notes
- **Backup Your Images**: Always back up your disk images before making changes.
- **File Size Limits**: The tool enforces the original image size to prevent expansion.
- **FAT12 Limitations**: File and directory names are limited to 8 characters (plus 3-character extension).

---

## Troubleshooting

| Issue                          | Solution                                                                 |
|--------------------------------|--------------------------------------------------------------------------|
| "Directory not found"          | Ensure the directory path is correct and the image is valid.            |
| "File not found"               | Verify the file exists in the current directory.                        |
| "No free directory entry"      | The directory is full; try removing files or creating a subdirectory.  |
| "Not enough free clusters"     | The image is full; delete files or use a larger image.                  |
| "Invalid directory name"       | Directory names must be 8 characters or less.                          |

---

## Future Improvements
- Drag-and-drop support for adding files.
- Undo/redo functionality for pending operations.
- Improved error handling and user feedback.

---

## License
MIT
