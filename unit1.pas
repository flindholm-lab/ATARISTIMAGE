unit Unit1;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLType, ExtCtrls, ComCtrls, StrUtils, Math, DateUtils, LazFileUtils;

type
  { TForm1 }
  TForm1 = class(TForm)
    // UI Components
    ButtonExit: TButton;
    ButtonRemoveDir: TButton;
    ButtonCreateDir: TButton;
    ButtonSaveFile: TButton;
    ButtonAddFile: TButton;
    ButtonOpenImage: TButton;
    ButtonRemoveFile: TButton;
    ButtonWriteImage: TButton;
    Label1: TLabel;
    LabelFreeSpace: TLabel;
    ListBoxFiles: TListBox;
    OpenDialog: TOpenDialog;
    ProgressBarFreeSpace: TProgressBar;
    SaveDialog: TSaveDialog;
    ButtonBack: TButton;
    LabelCurrentPath: TLabel;
    ButtonCreate720: TButton;

    // Event Handlers
    procedure ButtonAddFileClick(Sender: TObject);
    procedure ButtonCreateDirClick(Sender: TObject);
    procedure ButtonExitClick(Sender: TObject);
    procedure ButtonOpenImageClick(Sender: TObject);
    procedure ButtonRemoveDirClick(Sender: TObject);
    procedure ButtonRemoveFileClick(Sender: TObject);
    procedure ButtonWriteImageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonBackClick(Sender: TObject);
    procedure ListBoxFilesDblClick(Sender: TObject);
    procedure ButtonSaveFileClick(Sender: TObject);
    procedure ButtonCreate720Click(Sender: TObject);

  private
    // Lists for tracking files, directories, and pending operations
    FFilePaths: TStringList;          // Files to be added to the image
    FFilesToDelete: TStringList;      // Files marked for deletion
    FDisplayItems: TStringList;       // Items displayed in the current directory
    FNewDirectories: TStringList;     // Directories to be created
    FDirsToDelete: TStringList;       // Directories marked for deletion
    FImagePath: string;               // Path to the currently open image file
    FCurrentPath: string;             // Current path within the image

    // FAT12 filesystem layout variables
    FRootDirStart: Int64;             // Start position of the root directory
    FDataAreaStart: Int64;            // Start position of the data area
    FSectorsPerCluster: Byte;         // Sectors per cluster
    FBytesPerSector: Word;            // Bytes per sector
    FBytesPerCluster: Word;           // Bytes per cluster
    FFatTableStart: Int64;            // Start position of the FAT table
    FFatTableSize: Int64;             // Size of the FAT table in bytes
    FNumberOfFATs: Byte;              // Number of FAT copies
    FSectorsPerFAT: Word;             // Sectors per FAT
    FSingleFATSize: Int64;            // Size of a single FAT in bytes

    // Core Procedures and Functions
    procedure UpdateFileListDisplay;  // Updates the file list UI
    procedure ClearFileList;          // Clears the file list
    procedure ResetState;             // Resets the application state
    function FindDirectoryInPath(const AImageFile: TFileStream; const APath: string): Int64;
      // Finds the cluster of a directory given its path
    function FindFileInDirectory(const AImageFile: TFileStream; const ADirCluster: Int64; const AFileName: string): Int64;
      // Finds a file in a directory and returns its directory entry position
    function FindFreeDirectoryEntry(const AImageFile: TFileStream; const ADirCluster: Int64): Int64;
      // Finds a free directory entry for a new file or directory
    function FindFreeClusters(const AImageFile: TFileStream; const ACount: Int64): TStringList;
      // Finds free clusters for file allocation
    procedure WriteFileToImage(const AImageFile: TFileStream; const AFileName: string; const ASourceFile: TFileStream);
      // Writes a file to the image
    procedure GetFATDetails(const AImageFile: TFileStream);
      // Reads and parses the FAT12 boot sector and calculates layout
    procedure DeleteFileFromImage(const AImageFile: TFileStream; const ADirEntryPos: Int64; const AFileName: string);
      // Deletes a file from the image and updates the FAT
    function DateTimeToFatTime(const ADateTime: TDateTime): Word;
      // Converts a TDateTime to FAT time format
    function DateTimeToFatDate(const ADateTime: TDateTime): Word;
      // Converts a TDateTime to FAT date format
    function FatTimeToDateTime(const AFatTime: Word): TDateTime;
      // Converts FAT time to TDateTime
    function FatDateToDateTime(const AFatDate: Word): TDateTime;
      // Converts FAT date to TDateTime
    procedure ReadSTImage(const AFileName: String; const APath: string);
      // Reads the contents of a directory in the image
    function GetNextCluster(const AImageFile: TFileStream; const ACluster: Int64): Int64;
      // Gets the next cluster in a file's cluster chain
    function GetTempFile(const APath, APrefix: string): string;
      // Generates a unique temporary filename
    procedure WriteFAT12EntryAllCopies(const AImageFile: TFileStream; Cluster: Word; Value: Word);
      // Writes a FAT12 entry to all FAT copies
    function ReadFAT12Entry(const AImageFile: TFileStream; Cluster: Word): Word;
      // Reads a FAT12 entry
    function GetFreeSpace(const AImageFile: TFileStream): Word;
      // Calculates free space in the image
    procedure UpdateFreeSpaceDisplay;
      // Updates the free space display
    procedure CreateDirectoryInImage(const AImageFile: TFileStream; const DirName: string);
      // Creates a new directory in the image

    // Helper Functions
    function IsDirectoryEntryValid(const DirEntry: array of Byte): Boolean;
      // Checks if a directory entry is valid
    function IsDirectory(const DirEntry: array of Byte): Boolean;
      // Checks if a directory entry represents a directory
    function IsDeleted(const DirEntry: array of Byte): Boolean;
      // Checks if a directory entry is marked as deleted
    function IsEndOfDirectory(const DirEntry: array of Byte): Boolean;
      // Checks if a directory entry marks the end of the directory
    function ExtractFileNameFromEntry(const DirEntry: array of Byte): string;
      // Extracts the filename from a directory entry
    procedure ValidateDirectoryName(const DirName: string);
      // Validates a directory name
  public
  end;

var
  Form1: TForm1;

const
  // FAT12 filesystem constants
  FAT12_EOC = $FFF;                  // End-of-cluster marker
  FAT12_FREE = $000;                 // Free cluster marker
  FAT12_DELETED = $E5;               // Deleted file marker
  FAT12_DIR_ENTRY_SIZE = 32;         // Size of a directory entry in bytes
  FAT12_BOOT_SECTOR_SIZE = 512;      // Size of a boot sector in bytes
  FAT12_MAX_DIR_NAME_LEN = 8;        // Maximum directory name length
  FAT12_MAX_EXT_LEN = 3;             // Maximum file extension length

  // Error messages
  ERR_DIR_NOT_FOUND = 'Directory not found: %s';
  ERR_FILE_NOT_FOUND = 'File not found in directory: %s';
  ERR_NO_FREE_DIR_ENTRY = 'No free directory entry found.';
  ERR_NOT_ENOUGH_CLUSTERS = 'Not enough free clusters to write file.';
  ERR_INVALID_DIR_NAME = 'Invalid directory name.';

implementation

{$R *.lfm}

{------------------------------------------------------------------
  Function: GetTempFile
  Purpose:  Generates a unique temporary filename in the specified path
  Parameters:
    APath   - Path where the temporary file will be created
    APrefix - Prefix for the temporary filename
  Returns:  A unique temporary filename
 ------------------------------------------------------------------}
function TForm1.GetTempFile(const APath, APrefix: string): string;
var
  BaseName: string;
  I: Integer;
begin
  BaseName := APrefix + FormatDateTime('yyyymmddhhnnss', Now) + '-';
  I := 0;
  while True do
  begin
    Result := APath + BaseName + IntToStr(I);
    if not FileExists(Result) then Exit;
    Inc(I);
  end;
end;

{------------------------------------------------------------------
  Function: DateTimeToFatTime
  Purpose:  Converts a TDateTime to FAT time format (16-bit)
  Parameters:
    ADateTime - The datetime to convert
  Returns:  FAT time value
 ------------------------------------------------------------------}
function TForm1.DateTimeToFatTime(const ADateTime: TDateTime): Word;
var
  H, M, S: Word;
begin
  DecodeTime(ADateTime, H, M, S, S);
  Result := (H shl 11) or (M shl 5) or (S div 2);
end;

{------------------------------------------------------------------
  Function: DateTimeToFatDate
  Purpose:  Converts a TDateTime to FAT date format (16-bit)
  Parameters:
    ADateTime - The datetime to convert
  Returns:  FAT date value
 ------------------------------------------------------------------}
function TForm1.DateTimeToFatDate(const ADateTime: TDateTime): Word;
var
  Y, M, D: Word;
begin
  DecodeDate(ADateTime, Y, M, D);
  Result := ((Y - 1980) shl 9) or (M shl 5) or D;
end;

{------------------------------------------------------------------
  Function: FatTimeToDateTime
  Purpose:  Converts a FAT time value to TDateTime
  Parameters:
    AFatTime - FAT time value to convert
  Returns:  TDateTime value
 ------------------------------------------------------------------}
function TForm1.FatTimeToDateTime(const AFatTime: Word): TDateTime;
var
  H, M, S: Word;
begin
  H := AFatTime shr 11;
  M := (AFatTime shr 5) and $3F;
  S := (AFatTime and $1F) * 2;
  Result := EncodeTime(H, M, S, 0);
end;

{------------------------------------------------------------------
  Function: FatDateToDateTime
  Purpose:  Converts a FAT date value to TDateTime
  Parameters:
    AFatDate - FAT date value to convert
  Returns:  TDateTime value
 ------------------------------------------------------------------}
function TForm1.FatDateToDateTime(const AFatDate: Word): TDateTime;
var
  Y, M, D: Word;
begin
  Y := (AFatDate shr 9) + 1980;
  M := (AFatDate shr 5) and $0F;
  D := AFatDate and $1F;
  Result := EncodeDate(Y, M, D);
end;

{------------------------------------------------------------------
  Procedure: ResetState
  Purpose:  Resets the application state to default values
 ------------------------------------------------------------------}
procedure TForm1.ResetState;
begin
  FFilePaths.Clear;
  FFilesToDelete.Clear;
  FDisplayItems.Clear;
  FImagePath := '';
  FCurrentPath := '';
  UpdateFileListDisplay;
end;

{------------------------------------------------------------------
  Procedure: ButtonAddFileClick
  Purpose:  Event handler for the "Add File" button
            Opens a file dialog to select files to add to the image
 ------------------------------------------------------------------}
procedure TForm1.ButtonAddFileClick(Sender: TObject);
begin
  OpenDialog.Options := [ofAllowMultiSelect, ofFileMustExist];
  OpenDialog.Filter := 'All Files (*.*)|*.*';
  if OpenDialog.Execute then
  begin
    FFilePaths.AddStrings(OpenDialog.Files);
    UpdateFileListDisplay;
  end;
end;

{------------------------------------------------------------------
  Procedure: ButtonCreateDirClick
  Purpose:  Event handler for the "Create Directory" button
            Prompts the user for a directory name and adds it to the pending list
 ------------------------------------------------------------------}
procedure TForm1.ButtonCreateDirClick(Sender: TObject);
var
  DirName: string;
begin
  DirName := InputBox('Create Directory', 'Enter directory name (max 8 chars):', '');
  if (DirName = '') or (Length(DirName) > FAT12_MAX_DIR_NAME_LEN) then
  begin
    ShowMessage(ERR_INVALID_DIR_NAME);
    Exit;
  end;
  DirName := UpperCase(DirName);
  if FNewDirectories.IndexOf(DirName) = -1 then
    FNewDirectories.Add(DirName);
  UpdateFileListDisplay;
end;

{------------------------------------------------------------------
  Procedure: ButtonExitClick
  Purpose:  Event handler for the "Exit" button
            Closes the application
 ------------------------------------------------------------------}
procedure TForm1.ButtonExitClick(Sender: TObject);
begin
  Close;
end;

{------------------------------------------------------------------
  Procedure: ValidateDirectoryName
  Purpose:  Validates a directory name according to FAT12 rules
  Parameters:
    DirName - Directory name to validate
  Raises:   Exception if the name is invalid
 ------------------------------------------------------------------}
procedure TForm1.ValidateDirectoryName(const DirName: string);
begin
  if (DirName = '') or (Length(DirName) > FAT12_MAX_DIR_NAME_LEN) then
    raise Exception.Create(ERR_INVALID_DIR_NAME);
end;

{------------------------------------------------------------------
  Function: IsDirectoryEntryValid
  Purpose:  Checks if a directory entry is valid (not empty or deleted)
  Parameters:
    DirEntry - Directory entry to check
  Returns:  True if the entry is valid, False otherwise
 ------------------------------------------------------------------}
function TForm1.IsDirectoryEntryValid(const DirEntry: array of Byte): Boolean;
begin
  Result := (DirEntry[0] <> $00) and (DirEntry[0] <> FAT12_DELETED) and ((DirEntry[11] and $08) = 0);
end;

{------------------------------------------------------------------
  Function: IsDirectory
  Purpose:  Checks if a directory entry represents a directory
  Parameters:
    DirEntry - Directory entry to check
  Returns:  True if the entry is a directory, False otherwise
 ------------------------------------------------------------------}
function TForm1.IsDirectory(const DirEntry: array of Byte): Boolean;
begin
  Result := (DirEntry[11] and $10) <> 0;
end;

{------------------------------------------------------------------
  Function: IsDeleted
  Purpose:  Checks if a directory entry is marked as deleted
  Parameters:
    DirEntry - Directory entry to check
  Returns:  True if the entry is deleted, False otherwise
 ------------------------------------------------------------------}
function TForm1.IsDeleted(const DirEntry: array of Byte): Boolean;
begin
  Result := DirEntry[0] = FAT12_DELETED;
end;

{------------------------------------------------------------------
  Function: IsEndOfDirectory
  Purpose:  Checks if a directory entry marks the end of the directory
  Parameters:
    DirEntry - Directory entry to check
  Returns:  True if the entry marks the end, False otherwise
 ------------------------------------------------------------------}
function TForm1.IsEndOfDirectory(const DirEntry: array of Byte): Boolean;
begin
  Result := DirEntry[0] = $00;
end;

{------------------------------------------------------------------
  Function: ExtractFileNameFromEntry
  Purpose:  Extracts the filename from a directory entry
  Parameters:
    DirEntry - Directory entry to extract from
  Returns:  The filename (with extension if present)
 ------------------------------------------------------------------}
function TForm1.ExtractFileNameFromEntry(const DirEntry: array of Byte): string;
var
  I: Integer;
  FileName, FileExt: string;
begin
  FileName := '';
  for I := 0 to FAT12_MAX_DIR_NAME_LEN - 1 do
    FileName := FileName + AnsiChar(DirEntry[I]);
  FileName := Trim(FileName);

  FileExt := '';
  for I := 8 to 10 do
    FileExt := FileExt + AnsiChar(DirEntry[I]);
  FileExt := Trim(FileExt);

  if FileExt <> '' then
    Result := FileName + '.' + FileExt
  else
    Result := FileName;
end;

{------------------------------------------------------------------
  Procedure: CreateDirectoryInImage
  Purpose:  Creates a new directory in the image
  Parameters:
    AImageFile - The image file stream
    DirName    - Name of the directory to create
 ------------------------------------------------------------------}
procedure TForm1.CreateDirectoryInImage(const AImageFile: TFileStream; const DirName: string);
var
  DirEntry: array[0..FAT12_DIR_ENTRY_SIZE - 1] of Byte;
  DirEntryPos, ParentCluster: Int64;
  DirClusterList: TStringList;
  NewCluster: Word;
  FatPos: Int64;
  FatByte1, FatByte2: Byte;
  FatValue: Word;
  I: Integer;
begin
  GetFATDetails(AImageFile);
  ParentCluster := FindDirectoryInPath(AImageFile, FCurrentPath);
  DirEntryPos := FindFreeDirectoryEntry(AImageFile, ParentCluster);
  if DirEntryPos = -1 then Exit;

  // Find a free cluster for the new directory
  DirClusterList := FindFreeClusters(AImageFile, 1);
  if DirClusterList.Count = 0 then Exit;
  NewCluster := StrToInt(DirClusterList[0]);

  // Mark the cluster as end-of-chain in the FAT
  WriteFAT12EntryAllCopies(AImageFile, NewCluster, FAT12_EOC);

  // Update both FAT copies
  for I := 0 to 1 do
  begin
    FatPos := FFatTableStart + (I * FFatTableSize div 2) + (NewCluster * 3) div 2;
    AImageFile.Position := FatPos;
    AImageFile.Read(FatByte1, 1);
    AImageFile.Read(FatByte2, 1);
    FatValue := Word(FatByte1) or (Word(FatByte2) shl 8);
    if (NewCluster mod 2) = 0 then
      FatValue := (FatValue and $F000) or $0FFF
    else
      FatValue := (FatValue and $000F) or ($0FFF shl 4);
    FatByte1 := FatValue and $FF;
    FatByte2 := FatValue shr 8;
    AImageFile.Position := FatPos;
    AImageFile.Write(FatByte1, 1);
    AImageFile.Write(FatByte2, 1);
  end;

  // Write . and .. entries to the new directory cluster
  AImageFile.Position := FDataAreaStart + (NewCluster - 2) * FBytesPerCluster;

  // . entry (current directory)
  FillChar(DirEntry, SizeOf(DirEntry), $20);
  Move('.       ', DirEntry[0], 8);
  DirEntry[11] := $10;
  FillChar(DirEntry[12], 10, 0);
  I := DateTimeToFatTime(Now);
  Move(I, DirEntry[22], 2);
  I := DateTimeToFatDate(Now);
  Move(I, DirEntry[24], 2);
  Move(NewCluster, DirEntry[26], 2);
  FillChar(DirEntry[28], 4, 0);
  AImageFile.Write(DirEntry, FAT12_DIR_ENTRY_SIZE);

  // .. entry (parent directory)
  FillChar(DirEntry, SizeOf(DirEntry), $20);
  Move('..      ', DirEntry[0], 8);
  DirEntry[11] := $10;
  FillChar(DirEntry[12], 10, 0);
  I := DateTimeToFatTime(Now);
  Move(I, DirEntry[22], 2);
  I := DateTimeToFatDate(Now);
  Move(I, DirEntry[24], 2);
  Move(ParentCluster, DirEntry[26], 2);
  FillChar(DirEntry[28], 4, 0);
  AImageFile.Write(DirEntry, FAT12_DIR_ENTRY_SIZE);

  // Write directory entry in parent directory
  FillChar(DirEntry, SizeOf(DirEntry), $20);
  Move(PAnsiChar(UpperCase(DirName))^, DirEntry[0], Min(FAT12_MAX_DIR_NAME_LEN, Length(DirName)));
  DirEntry[11] := $10;
  FillChar(DirEntry[12], 10, 0);
  I := DateTimeToFatTime(Now);
  Move(I, DirEntry[22], 2);
  I := DateTimeToFatDate(Now);
  Move(I, DirEntry[24], 2);
  Move(NewCluster, DirEntry[26], 2);
  FillChar(DirEntry[28], 4, 0);
  AImageFile.Position := DirEntryPos;
  AImageFile.Write(DirEntry, FAT12_DIR_ENTRY_SIZE);

  DirClusterList.Free;
end;

{------------------------------------------------------------------
  Procedure: ButtonOpenImageClick
  Purpose:  Event handler for the "Open Image" button
            Opens an ST image file and reads its contents
 ------------------------------------------------------------------}
procedure TForm1.ButtonOpenImageClick(Sender: TObject);
begin
  OpenDialog.Options := [ofFileMustExist];
  OpenDialog.Filter := 'ST Image Files (*.st)|*.st|All Files (*.*)|*.*';
  if OpenDialog.Execute then
  begin
    ResetState;
    FImagePath := OpenDialog.FileName;
    ListBoxFiles.Items.Add('Reading image file...');
    ReadSTImage(FImagePath, FCurrentPath);
    UpdateFileListDisplay;
    UpdateFreeSpaceDisplay;
  end;
end;

{------------------------------------------------------------------
  Procedure: ButtonRemoveDirClick
  Purpose:  Event handler for the "Remove Directory" button
            Marks the selected directory for deletion
 ------------------------------------------------------------------}
procedure TForm1.ButtonRemoveDirClick(Sender: TObject);
var
  SelectedIndex: Integer;
  DirName: string;
begin
  SelectedIndex := ListBoxFiles.ItemIndex;
  if SelectedIndex < 0 then Exit;
  DirName := ListBoxFiles.Items[SelectedIndex];
  if not DirName.EndsWith('\') then
  begin
    ShowMessage('Selected item is not a directory.');
    Exit;
  end;
  // Remove trailing '\' and any *A* or *D* suffix
  DirName := TrimRight(DirName);
  DirName := StringReplace(DirName, '\ *A*', '', [rfReplaceAll]);
  DirName := StringReplace(DirName, '\ *D*', '', [rfReplaceAll]);
  DirName := StringReplace(DirName, '\', '', [rfReplaceAll]);
  if FDirsToDelete.IndexOf(DirName) = -1 then
    FDirsToDelete.Add(DirName)
  else
    FDirsToDelete.Delete(FDirsToDelete.IndexOf(DirName));
  UpdateFileListDisplay;
end;

{------------------------------------------------------------------
  Procedure: ButtonRemoveFileClick
  Purpose:  Event handler for the "Remove File" button
            Marks the selected file for deletion
 ------------------------------------------------------------------}
procedure TForm1.ButtonRemoveFileClick(Sender: TObject);
var
  SelectedIndex: Integer;
  FileName: string;
  Index: Integer;
begin
  SelectedIndex := ListBoxFiles.ItemIndex;
  if SelectedIndex >= 0 then
  begin
    FileName := ListBoxFiles.Items[SelectedIndex];
    if FileName.EndsWith(' *A*') then
      FileName := Copy(FileName, 1, Length(FileName) - 4)
    else if FileName.EndsWith(' *D*') then
      FileName := Copy(FileName, 1, Length(FileName) - 4);
    Index := FFilesToDelete.IndexOf(FileName);
    if Index <> -1 then
      FFilesToDelete.Delete(Index)
    else
      FFilesToDelete.Add(FileName);
    UpdateFileListDisplay;
  end;
end;

{------------------------------------------------------------------
  Procedure: ButtonWriteImageClick
  Purpose:  Event handler for the "Write Image" button
            Writes all pending changes to the image file
 ------------------------------------------------------------------}
procedure TForm1.ButtonWriteImageClick(Sender: TObject);
var
  SaveFileName: string;
  TempFileName: string;
  TempImageFile, OriginalImageFile, SourceFile: TFileStream;
  DirEntryPos: Int64;
  I: Integer;
  OriginalSize: Int64;
begin
  SaveDialog.Filter := 'ST Image Files (*.st)|*.st|All Files (*.*)|*.*';
  if SaveDialog.Execute then
  begin
    SaveFileName := SaveDialog.FileName;
    TempFileName := GetTempFile(ExtractFilePath(SaveFileName), 'fat-temp-');

    // Copy original image to a temporary file
    OriginalImageFile := TFileStream.Create(FImagePath, fmOpenRead);
    try
      OriginalSize := OriginalImageFile.Size;
      TempImageFile := TFileStream.Create(TempFileName, fmCreate);
      try
        TempImageFile.CopyFrom(OriginalImageFile, 0);
      finally
        TempImageFile.Free;
      end;
    finally
      OriginalImageFile.Free;
    end;

    // Reopen temp image for read/write
    TempImageFile := TFileStream.Create(TempFileName, fmOpenReadWrite);
    try
      GetFATDetails(TempImageFile);

      // Create new directories
      for I := 0 to FNewDirectories.Count - 1 do
        CreateDirectoryInImage(TempImageFile, FNewDirectories[I]);
      FNewDirectories.Clear;

      // Write new files
      for I := 0 to FFilePaths.Count - 1 do
      begin
        if FFilesToDelete.IndexOf(UpperCase(ExtractFileName(FFilePaths[I]))) = -1 then
        begin
          SourceFile := TFileStream.Create(FFilePaths[I], fmOpenRead);
          try
            WriteFileToImage(TempImageFile, FFilePaths[I], SourceFile);
          finally
            SourceFile.Free;
          end;
        end;
      end;

      // Delete directories
      for I := 0 to FDirsToDelete.Count - 1 do
      begin
        DirEntryPos := FindFileInDirectory(TempImageFile, FindDirectoryInPath(TempImageFile, FCurrentPath), FDirsToDelete[I]);
        if DirEntryPos <> -1 then
          DeleteFileFromImage(TempImageFile, DirEntryPos, FDirsToDelete[I])
        else
          ShowMessage(Format(ERR_DIR_NOT_FOUND, [FDirsToDelete[I]]));
      end;
      FDirsToDelete.Clear;

      // Delete files
      for I := 0 to FFilesToDelete.Count - 1 do
      begin
        DirEntryPos := FindFileInDirectory(TempImageFile, FindDirectoryInPath(TempImageFile, FCurrentPath), FFilesToDelete[I]);
        if DirEntryPos <> -1 then
          DeleteFileFromImage(TempImageFile, DirEntryPos, FFilesToDelete[I])
        else
          ShowMessage(Format(ERR_FILE_NOT_FOUND, [FFilesToDelete[I]]));
      end;

      // Enforce original file size to prevent expansion
      TempImageFile.Size := OriginalSize;
    finally
      TempImageFile.Free;
    end;

    // Replace original image with the modified temporary file
    if FileExists(SaveFileName) then DeleteFile(SaveFileName);
    RenameFile(TempFileName, SaveFileName);
    ShowMessage('Changes successfully written to image.');

    // Clear pending lists after writing
    FNewDirectories.Clear;
    FFilePaths.Clear;
    FDirsToDelete.Clear;
    FFilesToDelete.Clear;

    // Reload image
    FImagePath := SaveFileName;
    ReadSTImage(FImagePath, FCurrentPath);
    UpdateFileListDisplay;
    UpdateFreeSpaceDisplay;
  end;
end;

{------------------------------------------------------------------
  Procedure: ButtonCreate720Click
  Purpose:  Event handler for the "Create 720KB Image" button
            Creates a new empty 720KB ST image file
 ------------------------------------------------------------------}
procedure TForm1.ButtonCreate720Click(Sender: TObject);
const
  TOTAL_SIZE = 737280;               // Total size in bytes (720KB)
  BYTES_PER_SECTOR = 512;            // Bytes per sector
  SECTORS_PER_CLUSTER = 2;           // Sectors per cluster
  RESERVED_SECTORS = 1;              // Reserved sectors
  NUMBER_OF_FATS = 2;                // Number of FAT copies
  ROOT_ENTRIES = 112;                // Root directory entries
  TOTAL_SECTORS = 1440;              // Total sectors
  SECTORS_PER_FAT = 3;               // Sectors per FAT
var
  SaveFileName: string;
  TempFileName: string;
  ImageFile: TFileStream;
  BootSector: TBytes;
  FAT: array[0..1535] of Byte;
  RootDir: array[0..3583] of Byte;
  DataAreaSize: Int64;
  DataArea: TBytes;
begin
  SaveDialog.Filter := 'ST Image Files (*.st)|*.st|All Files (*.*)|*.*';
  SaveDialog.FileName := 'empty720kb.st';
  if SaveDialog.Execute then
  begin
    SaveFileName := SaveDialog.FileName;
    TempFileName := GetTempFile(ExtractFilePath(SaveFileName), 'fat-temp-');
    try
      ImageFile := TFileStream.Create(TempFileName, fmCreate);
      try
        // Create boot sector
        SetLength(BootSector, BYTES_PER_SECTOR);
        FillChar(BootSector[0], SizeOf(BootSector), 0);
        BootSector[0] := $E9;
        BootSector[1] := $00;
        BootSector[2] := $4E;
        BootSector[3] := $4E;
        BootSector[4] := $4E;
        BootSector[5] := $4E;
        BootSector[6] := $4E;
        BootSector[7] := $4E;
        BootSector[8] := $9C;
        BootSector[9] := $16;
        BootSector[10] := $81;
        BootSector[11] := BYTES_PER_SECTOR and $FF;
        BootSector[12] := BYTES_PER_SECTOR shr 8;
        BootSector[13] := SECTORS_PER_CLUSTER;
        BootSector[14] := RESERVED_SECTORS and $FF;
        BootSector[15] := RESERVED_SECTORS shr 8;
        BootSector[16] := NUMBER_OF_FATS;
        BootSector[17] := ROOT_ENTRIES and $FF;
        BootSector[18] := ROOT_ENTRIES shr 8;
        BootSector[19] := TOTAL_SECTORS and $FF;
        BootSector[20] := TOTAL_SECTORS shr 8;
        BootSector[21] := $F9;       // Media Descriptor
        BootSector[22] := SECTORS_PER_FAT and $FF;
        BootSector[23] := SECTORS_PER_FAT shr 8;
        BootSector[24] := $09;       // Drive Number
        BootSector[26] := $02;       // Number of Heads
        ImageFile.Write(BootSector[0], BYTES_PER_SECTOR);

        // Create FAT tables
        FillChar(FAT, SizeOf(FAT), 0);
        FAT[0] := $F9;
        FAT[1] := $FF;
        FAT[2] := $FF;
        ImageFile.Write(FAT, SizeOf(FAT));
        ImageFile.Write(FAT, SizeOf(FAT)); // Second FAT copy

        // Create empty root directory
        FillChar(RootDir, SizeOf(RootDir), 0);
        ImageFile.Write(RootDir, SizeOf(RootDir));

        // Create empty data area
        DataAreaSize := TOTAL_SIZE - ImageFile.Position;
        SetLength(DataArea, DataAreaSize);
        FillChar(DataArea[0], DataAreaSize, 0);
        ImageFile.Write(DataArea[0], DataAreaSize);
      finally
        ImageFile.Free;
      end;

      // Save the new image
      if FileExists(SaveFileName) then DeleteFile(SaveFileName);
      RenameFile(TempFileName, SaveFileName);
      ShowMessage('720 KB image created successfully!');
    except
      on E: Exception do
        ShowMessage('Error creating image: ' + E.Message);
    end;

    // Load the new image
    ResetState;
    FImagePath := SaveFileName;
    ReadSTImage(FImagePath, FCurrentPath);
    UpdateFileListDisplay;
  end;
end;

{------------------------------------------------------------------
  Procedure: FormCreate
  Purpose:  Event handler for form creation
            Initializes all string lists and sets dialog titles
 ------------------------------------------------------------------}
procedure TForm1.FormCreate(Sender: TObject);
begin
  FFilePaths := TStringList.Create;
  FFilesToDelete := TStringList.Create;
  FDisplayItems := TStringList.Create;
  FNewDirectories := TStringList.Create;
  FDirsToDelete := TStringList.Create;
  FImagePath := '';
  FCurrentPath := '';
  OpenDialog.Title := 'Select Files or Image';
  SaveDialog.Title := 'Save Image As...';
end;

{------------------------------------------------------------------
  Procedure: FormDestroy
  Purpose:  Event handler for form destruction
            Frees all allocated resources
 ------------------------------------------------------------------}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  FFilePaths.Free;
  FFilesToDelete.Free;
  FDisplayItems.Free;
  FNewDirectories.Free;
  FDirsToDelete.Free;
end;

{------------------------------------------------------------------
  Procedure: ButtonBackClick
  Purpose:  Event handler for the "Back" button
            Navigates to the parent directory
 ------------------------------------------------------------------}
procedure TForm1.ButtonBackClick(Sender: TObject);
var
  P: Integer;
begin
  if FCurrentPath = '' then Exit;
  if FCurrentPath.EndsWith('\') then
    FCurrentPath := Copy(FCurrentPath, 1, Length(FCurrentPath) - 1);
  P := LastDelimiter('\', FCurrentPath);
  if P > 0 then
    FCurrentPath := Copy(FCurrentPath, 1, P)
  else
    FCurrentPath := '';
  ListBoxFiles.Items.Add('Navigating back...');
  ReadSTImage(FImagePath, FCurrentPath);
  UpdateFileListDisplay;
end;

{------------------------------------------------------------------
  Procedure: ListBoxFilesDblClick
  Purpose:  Event handler for double-clicking a file in the list
            Navigates into directories or goes back if ".." is clicked
 ------------------------------------------------------------------}
procedure TForm1.ListBoxFilesDblClick(Sender: TObject);
var
  SelectedItem: string;
begin
  if ListBoxFiles.ItemIndex >= 0 then
  begin
    SelectedItem := ListBoxFiles.Items[ListBoxFiles.ItemIndex];
    if (SelectedItem = '..') or (SelectedItem = '..\') then
      ButtonBackClick(nil)
    else if EndsText('\', SelectedItem) then
    begin
      SelectedItem := Copy(SelectedItem, 1, Length(SelectedItem) - 1);
      if FCurrentPath = '' then
        FCurrentPath := SelectedItem + '\'
      else
        FCurrentPath := FCurrentPath + SelectedItem + '\';
      ListBoxFiles.Items.Add('Navigating into ' + SelectedItem);
      ReadSTImage(FImagePath, FCurrentPath);
      UpdateFileListDisplay;
    end;
  end;
end;

{------------------------------------------------------------------
  Procedure: UpdateFileListDisplay
  Purpose:  Updates the file list display with current directory contents
            and pending operations
 ------------------------------------------------------------------}
procedure TForm1.UpdateFileListDisplay;
var
  I: Integer;
  FileName: string;
begin
  ListBoxFiles.Clear;

  // Show files/directories from the disk
  if FDisplayItems.Count > 0 then
  begin
    for I := 0 to FDisplayItems.Count - 1 do
    begin
      FileName := UpperCase(FDisplayItems[I]);
      if FFilesToDelete.IndexOf(FileName) <> -1 then
        ListBoxFiles.Items.Add(FileName + ' *D*')
      else if FDirsToDelete.IndexOf(StringReplace(FileName, '\', '', [rfReplaceAll])) <> -1 then
        ListBoxFiles.Items.Add(FileName + ' *D*')
      else
        ListBoxFiles.Items.Add(FileName);
    end;
  end
  else
    ListBoxFiles.Items.Add('Directory is empty.');

  // Show pending file additions
  for I := 0 to FFilePaths.Count - 1 do
  begin
    FileName := UpperCase(ExtractFileName(FFilePaths[I]));
    if FDisplayItems.IndexOf(FileName) = -1 then
      ListBoxFiles.Items.Add(FileName + ' *A*');
  end;

  // Show pending directory additions
  for I := 0 to FNewDirectories.Count - 1 do
  begin
    FileName := FNewDirectories[I] + '\';
    if FDisplayItems.IndexOf(FileName) = -1 then
      ListBoxFiles.Items.Add(FileName + ' *A*');
  end;

  // Update current path label
  LabelCurrentPath.Caption := 'Current Path: ' + FCurrentPath;
end;

{------------------------------------------------------------------
  Procedure: ClearFileList
  Purpose:  Clears the file list display
 ------------------------------------------------------------------}
procedure TForm1.ClearFileList;
begin
  FDisplayItems.Clear;
  UpdateFileListDisplay;
end;

{------------------------------------------------------------------
  Function: GetNextCluster
  Purpose:  Gets the next cluster in a file's cluster chain
  Parameters:
    AImageFile - The image file stream
    ACluster   - Current cluster
  Returns:  Next cluster in the chain
 ------------------------------------------------------------------}
function TForm1.GetNextCluster(const AImageFile: TFileStream; const ACluster: Int64): Int64;
var
  FatPos: Int64;
  FatByte1, FatByte2: Byte;
  FatValue: Word;
begin
  FatPos := FFatTableStart + (ACluster * 3) div 2;
  AImageFile.Position := FatPos;
  AImageFile.Read(FatByte1, 1);
  AImageFile.Read(FatByte2, 1);
  FatValue := Word(FatByte1) or (Word(FatByte2) shl 8);
  if (ACluster mod 2) = 0 then
    Result := (FatValue and $0FFF)
  else
    Result := (FatValue shr 4);
end;

{------------------------------------------------------------------
  Function: FindDirectoryInPath
  Purpose:  Finds the cluster of a directory given its path
  Parameters:
    AImageFile - The image file stream
    APath      - Path to the directory
  Returns:  Cluster number of the directory, or -1 if not found
 ------------------------------------------------------------------}
function TForm1.FindDirectoryInPath(const AImageFile: TFileStream; const APath: string): Int64;
var
  PathParts: TStringList;
  I, J: Integer;
  DirFound: Boolean;
  DirCluster, CurrentCluster: Int64;
  DirEntry: array[0..FAT12_DIR_ENTRY_SIZE - 1] of Byte;
  DirName: string;
begin
  Result := 0;
  if APath = '' then Exit;

  PathParts := TStringList.Create;
  try
    PathParts.Delimiter := '\';
    PathParts.StrictDelimiter := True;
    PathParts.DelimitedText := APath;

    DirCluster := 0; // Start at root directory
    for I := 0 to PathParts.Count - 1 do
    begin
      if PathParts[I] = '' then Continue;
      DirFound := False;
      CurrentCluster := DirCluster;

      while True do
      begin
        // Position to the current cluster
        if CurrentCluster = 0 then
          AImageFile.Position := FRootDirStart
        else
          AImageFile.Position := FDataAreaStart + (CurrentCluster - 2) * FBytesPerCluster;

        // Search through directory entries
        while True do
        begin
          if AImageFile.Read(DirEntry, FAT12_DIR_ENTRY_SIZE) <> FAT12_DIR_ENTRY_SIZE then Break;
          if IsEndOfDirectory(DirEntry) then Break;
          if IsDeleted(DirEntry) or not IsDirectoryEntryValid(DirEntry) then Continue;

          DirName := ExtractFileNameFromEntry(DirEntry);
          if IsDirectory(DirEntry) and (UpperCase(DirName) = UpperCase(PathParts[I])) then
          begin
            DirCluster := Word(DirEntry[$1A]) + (Word(DirEntry[$1B]) shl 8);
            DirFound := True;
            Break;
          end;
        end;

        if DirFound then Break;
        if CurrentCluster = 0 then Break;
        CurrentCluster := GetNextCluster(AImageFile, CurrentCluster);
        if CurrentCluster >= $FF8 then Break;
      end;

      if not DirFound then
      begin
        Result := -1;
        Exit;
      end;
    end;

    Result := DirCluster;
  finally
    PathParts.Free;
  end;
end;

{------------------------------------------------------------------
  Procedure: ReadSTImage
  Purpose:  Reads the contents of a directory in the image
  Parameters:
    AFileName - Path to the image file
    APath     - Path within the image to read
 ------------------------------------------------------------------}
procedure TForm1.ReadSTImage(const AFileName: String; const APath: string);
var
  ImageFile: TFileStream;
  DirStartCluster: Int64;
  CurrentCluster: Int64;
  DirEntry: array[0..FAT12_DIR_ENTRY_SIZE - 1] of Byte;
  FileName: string;
  J: Integer;
begin
  FDisplayItems.Clear;
  ImageFile := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    GetFATDetails(ImageFile);

    if APath = '' then
    begin
      DirStartCluster := 0; // Root directory
      FDisplayItems.Clear;
    end else
    begin
      DirStartCluster := FindDirectoryInPath(ImageFile, APath);
      if DirStartCluster = -1 then
      begin
        ListBoxFiles.Items.Add(Format(ERR_DIR_NOT_FOUND, [APath]));
        Exit;
      end;
      FDisplayItems.Add('..'); // Add parent directory link
    end;

    CurrentCluster := DirStartCluster;
    while True do
    begin
      // Position to the current cluster
      if CurrentCluster = 0 then
        ImageFile.Position := FRootDirStart
      else
        ImageFile.Position := FDataAreaStart + (CurrentCluster - 2) * FBytesPerCluster;

      // Read directory entries
      while True do
      begin
        if ImageFile.Read(DirEntry, FAT12_DIR_ENTRY_SIZE) <> FAT12_DIR_ENTRY_SIZE then Break;
        if IsEndOfDirectory(DirEntry) then Break;
        if IsDeleted(DirEntry) or not IsDirectoryEntryValid(DirEntry) then Continue;

        FileName := ExtractFileNameFromEntry(DirEntry);
        if IsDirectory(DirEntry) and ((FileName = '.') or (FileName = '..')) then
          Continue;

        if IsDirectory(DirEntry) then
          FDisplayItems.Add(FileName + '\')
        else
          FDisplayItems.Add(FileName);
      end;

      if DirStartCluster = 0 then Break;
      CurrentCluster := GetNextCluster(ImageFile, CurrentCluster);
      if CurrentCluster >= $FF8 then Break;
    end;
  finally
    ImageFile.Free;
  end;
end;

{------------------------------------------------------------------
  Function: FindFileInDirectory
  Purpose:  Finds a file in a directory and returns its directory entry position
  Parameters:
    AImageFile - The image file stream
    ADirCluster - Cluster of the directory to search
    AFileName   - Name of the file to find
  Returns:  Position of the file's directory entry, or -1 if not found
 ------------------------------------------------------------------}
function TForm1.FindFileInDirectory(const AImageFile: TFileStream; const ADirCluster: Int64; const AFileName: string): Int64;
var
  CurrentCluster: Int64;
  DirEntry: array[0..FAT12_DIR_ENTRY_SIZE - 1] of Byte;
  FileName: string;
  J: Integer;
begin
  Result := -1;
  CurrentCluster := ADirCluster;
  while True do
  begin
    // Position to the current cluster
    if CurrentCluster = 0 then
      AImageFile.Position := FRootDirStart
    else
      AImageFile.Position := FDataAreaStart + (CurrentCluster - 2) * FBytesPerCluster;

    // Search through directory entries
    while True do
    begin
      if AImageFile.Read(DirEntry, FAT12_DIR_ENTRY_SIZE) <> FAT12_DIR_ENTRY_SIZE then Break;
      if IsEndOfDirectory(DirEntry) then Break;
      if IsDeleted(DirEntry) then Continue;

      FileName := ExtractFileNameFromEntry(DirEntry);
      if UpperCase(FileName) = UpperCase(AFileName) then
      begin
        Result := AImageFile.Position - FAT12_DIR_ENTRY_SIZE;
        Exit;
      end;
    end;

    if ADirCluster = 0 then Break;
    CurrentCluster := GetNextCluster(AImageFile, CurrentCluster);
    if CurrentCluster >= $FF8 then Break;
  end;
end;

{------------------------------------------------------------------
  Function: FindFreeDirectoryEntry
  Purpose:  Finds a free directory entry for a new file or directory
  Parameters:
    AImageFile - The image file stream
    ADirCluster - Cluster of the directory to search
  Returns:  Position of a free directory entry, or -1 if none found
 ------------------------------------------------------------------}
function TForm1.FindFreeDirectoryEntry(const AImageFile: TFileStream; const ADirCluster: Int64): Int64;
var
  CurrentCluster: Int64;
  DirEntry: array[0..FAT12_DIR_ENTRY_SIZE - 1] of Byte;
begin
  Result := -1;
  CurrentCluster := ADirCluster;
  while True do
  begin
    // Position to the current cluster
    if CurrentCluster = 0 then
      AImageFile.Position := FRootDirStart
    else
      AImageFile.Position := FDataAreaStart + (CurrentCluster - 2) * FBytesPerCluster;

    // Search for a free entry
    while True do
    begin
      if AImageFile.Read(DirEntry, FAT12_DIR_ENTRY_SIZE) <> FAT12_DIR_ENTRY_SIZE then Break;
      if IsEndOfDirectory(DirEntry) or IsDeleted(DirEntry) then
      begin
        Result := AImageFile.Position - FAT12_DIR_ENTRY_SIZE;
        Exit;
      end;
    end;

    if ADirCluster = 0 then Break;
    CurrentCluster := GetNextCluster(AImageFile, CurrentCluster);
    if CurrentCluster >= $FF8 then Break;
  end;
end;

{------------------------------------------------------------------
  Function: FindFreeClusters
  Purpose:  Finds free clusters for file allocation
  Parameters:
    AImageFile - The image file stream
    ACount     - Number of clusters needed
  Returns:  List of free cluster numbers
 ------------------------------------------------------------------}
function TForm1.FindFreeClusters(const AImageFile: TFileStream; const ACount: Int64): TStringList;
var
  CurrentCluster, FreeClusterCount: Int64;
  Val: Word;
begin
  Result := TStringList.Create;
  CurrentCluster := 2; // First allocatable cluster
  FreeClusterCount := 0;
  while FreeClusterCount < ACount do
  begin
    Val := ReadFAT12Entry(AImageFile, CurrentCluster);
    if Val = FAT12_FREE then
    begin
      Result.Add(IntToStr(CurrentCluster));
      Inc(FreeClusterCount);
    end;
    Inc(CurrentCluster);
    // Guard against infinite loop on damaged FAT
    if FFatTableStart + (CurrentCluster * 3) div 2 >= FFatTableStart + FSingleFATSize - 2 then
      Break;
  end;
end;

{------------------------------------------------------------------
  Procedure: WriteFileToImage
  Purpose:  Writes a file to the image and updates the FAT
  Parameters:
    AImageFile - The image file stream
    AFileName  - Name of the file to write
    ASourceFile - Source file stream
 ------------------------------------------------------------------}
procedure TForm1.WriteFileToImage(const AImageFile: TFileStream; const AFileName: string; const ASourceFile: TFileStream);
var
  DirEntryPos, DirCluster: Int64;
  FileClusters: TStringList;
  FileSizeInClusters: Int64;
  I: Integer;
  Buffer: array[0..4095] of Byte;
  BytesRead: Int64;
  DirEntry: array[0..FAT12_DIR_ENTRY_SIZE - 1] of Byte;
  FileNameOnly, FileExt: string;
  StartCluster: Word;
  FileTime, FileDate: Word;
begin
  // Calculate how many clusters are needed
  FileSizeInClusters := ASourceFile.Size div FBytesPerCluster;
  if (ASourceFile.Size mod FBytesPerCluster) <> 0 then Inc(FileSizeInClusters);

  // Find the directory to write to
  DirCluster := FindDirectoryInPath(AImageFile, FCurrentPath);
  DirEntryPos := FindFreeDirectoryEntry(AImageFile, DirCluster);
  if DirEntryPos = -1 then
  begin
    ShowMessage(ERR_NO_FREE_DIR_ENTRY);
    Exit;
  end;

  // Find free clusters
  FileClusters := FindFreeClusters(AImageFile, FileSizeInClusters);
  if FileClusters.Count < FileSizeInClusters then
  begin
    ShowMessage(ERR_NOT_ENOUGH_CLUSTERS);
    FileClusters.Free;
    Exit;
  end;

  // Write file data to clusters
  ASourceFile.Position := 0;
  for I := 0 to FileClusters.Count - 1 do
  begin
    BytesRead := ASourceFile.Read(Buffer, FBytesPerCluster);
    AImageFile.Position := FDataAreaStart + (StrToInt(FileClusters[I]) - 2) * FBytesPerCluster;
    AImageFile.Write(Buffer[0], BytesRead);
  end;

  // Update FAT chain
  for I := 0 to FileClusters.Count - 1 do
  begin
    if I < FileClusters.Count - 1 then
      WriteFAT12EntryAllCopies(AImageFile, StrToInt(FileClusters[I]), StrToInt(FileClusters[I + 1]))
    else
      WriteFAT12EntryAllCopies(AImageFile, StrToInt(FileClusters[I]), FAT12_EOC);
  end;

  // Create directory entry
  FillChar(DirEntry, SizeOf(DirEntry), 0);
  FileNameOnly := UpperCase(ExtractFileName(AFileName));
  FileExt := '';
  I := Pos('.', FileNameOnly);
  if I > 0 then
  begin
    FileExt := Copy(FileNameOnly, I + 1, Length(FileNameOnly));
    FileNameOnly := Copy(FileNameOnly, 1, I - 1);
  end;

  // Set filename and extension
  FillChar(DirEntry[0], FAT12_MAX_DIR_NAME_LEN, $20);
  FillChar(DirEntry[8], FAT12_MAX_EXT_LEN, $20);
  Move(PAnsiChar(FileNameOnly)^, DirEntry[0], Min(FAT12_MAX_DIR_NAME_LEN, Length(FileNameOnly)));
  Move(PAnsiChar(FileExt)^, DirEntry[8], Min(FAT12_MAX_EXT_LEN, Length(FileExt)));

  // Set file attributes and timestamps
  DirEntry[11] := $20; // Archive bit
  Move(ASourceFile.Size, DirEntry[28], 4); // File size
  StartCluster := StrToInt(FileClusters[0]);
  Move(StartCluster, DirEntry[26], 2); // Starting cluster

  FileTime := DateTimeToFatTime(Now);
  FileDate := DateTimeToFatDate(Now);
  Move(FileTime, DirEntry[22], 2); // Time
  Move(FileDate, DirEntry[24], 2); // Date

  // Write directory entry
  AImageFile.Position := DirEntryPos;
  AImageFile.Write(DirEntry, FAT12_DIR_ENTRY_SIZE);

  FileClusters.Free;
end;

{------------------------------------------------------------------
  Procedure: DeleteFileFromImage
  Purpose:  Deletes a file from the image and updates the FAT
  Parameters:
    AImageFile   - The image file stream
    ADirEntryPos - Position of the file's directory entry
    AFileName    - Name of the file to delete
 ------------------------------------------------------------------}
procedure TForm1.DeleteFileFromImage(const AImageFile: TFileStream; const ADirEntryPos: Int64; const AFileName: string);
var
  DeletedMarker: Byte;
  DirEntry: array[0..FAT12_DIR_ENTRY_SIZE - 1] of Byte;
  StartCluster: Word;
  OldCluster: Int64;
  Curr, Next2: Word;
begin
  // Read the directory entry
  AImageFile.Position := ADirEntryPos;
  AImageFile.Read(DirEntry, FAT12_DIR_ENTRY_SIZE);

  // Mark the first byte as deleted
  DeletedMarker := FAT12_DELETED;
  AImageFile.Position := ADirEntryPos;
  AImageFile.Write(DeletedMarker, 1);

  // Clear the file size
  FillChar(DirEntry[28], 4, 0);
  AImageFile.Position := ADirEntryPos + 28;
  AImageFile.Write(DirEntry[28], 4);

  // Clear the starting cluster
  FillChar(DirEntry[26], 2, 0);
  AImageFile.Position := ADirEntryPos + 26;
  AImageFile.Write(DirEntry[26], 2);

  // Get the starting cluster of the file
  Move(DirEntry[26], StartCluster, 2);
  OldCluster := StartCluster;
  Curr := StartCluster;

  // Clear the FAT chain for the file
  while (Curr >= 2) and (Curr < $FF8) do
  begin
    Next2 := ReadFAT12Entry(AImageFile, Curr);
    WriteFAT12EntryAllCopies(AImageFile, Curr, FAT12_FREE);
    Curr := Next2;
  end;
end;

{------------------------------------------------------------------
  Procedure: GetFATDetails
  Purpose:  Reads and parses the FAT12 boot sector and calculates layout
  Parameters:
    AImageFile - The image file stream
 ------------------------------------------------------------------}
procedure TForm1.GetFATDetails(const AImageFile: TFileStream);
var
  BootSector: array[0..FAT12_BOOT_SECTOR_SIZE - 1] of Byte;
  ReservedSectors, RootEntries, TotalSectors, SectorsPerFAT: Word;
  RootDirSectors: Int64;
begin
  AImageFile.Position := 0;
  AImageFile.Read(BootSector, FAT12_BOOT_SECTOR_SIZE);

  // Parse boot sector
  FBytesPerSector := Word(BootSector[$0B]) + (Word(BootSector[$0C]) shl 8);
  FSectorsPerCluster := BootSector[$0D];
  ReservedSectors := Word(BootSector[$0E]) + (Word(BootSector[$0F]) shl 8);
  FNumberOfFATs := BootSector[$10];
  RootEntries := Word(BootSector[$11]) + (Word(BootSector[$12]) shl 8);
  TotalSectors := Word(BootSector[$13]) + (Word(BootSector[$14]) shl 8);
  FSectorsPerFAT := Word(BootSector[$16]) + (Word(BootSector[$17]) shl 8);

  // Calculate FAT and data area positions
  FSingleFATSize := FSectorsPerFAT * FBytesPerSector;
  FFatTableStart := ReservedSectors * FBytesPerSector;
  FFatTableSize := FNumberOfFATs * FSingleFATSize;
  FRootDirStart := (ReservedSectors + (FNumberOfFATs * FSectorsPerFAT)) * FBytesPerSector;

  RootDirSectors := ((RootEntries * FAT12_DIR_ENTRY_SIZE) + (FBytesPerSector - 1)) div FBytesPerSector;
  FDataAreaStart := FRootDirStart + (RootDirSectors * FBytesPerSector);
  FBytesPerCluster := FSectorsPerCluster * FBytesPerSector;
end;

{------------------------------------------------------------------
  Function: ReadFAT12Entry
  Purpose:  Reads a FAT12 entry
  Parameters:
    AImageFile - The image file stream
    Cluster    - Cluster number to read
  Returns:  Value of the FAT entry
 ------------------------------------------------------------------}
function TForm1.ReadFAT12Entry(const AImageFile: TFileStream; Cluster: Word): Word;
var
  posFAT: Int64;
  b1, b2: Byte;
  val: Word;
begin
  posFAT := FFatTableStart + (Cluster * 3) div 2;
  AImageFile.Position := posFAT;
  AImageFile.Read(b1, 1);
  AImageFile.Read(b2, 1);
  val := Word(b1) or (Word(b2) shl 8);
  if (Cluster and 1) = 0 then
    Result := val and $0FFF
  else
    Result := val shr 4;
end;

{------------------------------------------------------------------
  Procedure: WriteFAT12EntryAllCopies
  Purpose:  Writes a FAT12 entry to all FAT copies
  Parameters:
    AImageFile - The image file stream
    Cluster    - Cluster number to write
    Value      - Value to write
 ------------------------------------------------------------------}
procedure TForm1.WriteFAT12EntryAllCopies(const AImageFile: TFileStream; Cluster: Word; Value: Word);
var
  fatIdx: Integer;
  posFAT, base: Int64;
  b1, b2: Byte;
  val: Word;
begin
  Value := Value and $0FFF; // Ensure it's a 12-bit value
  for fatIdx := 0 to FNumberOfFATs - 1 do
  begin
    base := FFatTableStart + fatIdx * FSingleFATSize;
    posFAT := base + (Cluster * 3) div 2;
    AImageFile.Position := posFAT;
    AImageFile.Read(b1, 1);
    AImageFile.Read(b2, 1);
    val := Word(b1) or (Word(b2) shl 8);
    if (Cluster and 1) = 0 then
      val := (val and $F000) or (Value and $0FFF)
    else
      val := (val and $000F) or ((Value and $0FFF) shl 4);
    b1 := val and $FF;
    b2 := val shr 8;
    AImageFile.Position := posFAT;
    AImageFile.Write(b1, 1);
    AImageFile.Write(b2, 1);
  end;
end;

{------------------------------------------------------------------
  Function: GetFreeSpace
  Purpose:  Counts the number of free clusters in the image, using the
            correct total clusters for the loaded image size.
  Parameters:
    AImageFile - The image file stream
  Returns:   Number of free clusters
 ------------------------------------------------------------------}
function TForm1.GetFreeSpace(const AImageFile: TFileStream): Word;
var
  CurrentCluster: Word;
  Val: Word;
  FreeClusters: Word;
  FileSize: Int64;
  TotalClusters: Word;
begin
  FreeClusters := 0;
  FileSize := AImageFile.Size;

  // Set the max cluster to check based on image size
      if FileSize = 368640 then       // 360KB
        TotalClusters := 343
      else if FileSize = 409600 then  // 400KB
        TotalClusters := 577
      else if FileSize = 737280 then  // 720KB
        TotalClusters := 614
      else if FileSize = 819200 then  // 800KB
        TotalClusters := 1154
      else
        TotalClusters := 343;        // Default to 360KB if unknown

  // Iterate through all possible clusters for this image size
  for CurrentCluster := 2 to TotalClusters + 1 do  // +1 because clusters start at 2
  begin
    Val := ReadFAT12Entry(AImageFile, CurrentCluster);
    if Val = 0 then
      Inc(FreeClusters);
  end;

  Result := FreeClusters;
end;

{------------------------------------------------------------------
  Procedure: UpdateFreeSpaceDisplay
  Purpose:  Updates the free space display in the UI using hardcoded total clusters
            for common Atari ST image sizes (360KB, 720KB, 800KB).
 ------------------------------------------------------------------}
procedure TForm1.UpdateFreeSpaceDisplay;
var
  ImageFile: TFileStream;
  FreeClusters: Word;
  TotalClusters: Word;
  UtilizedPercent: Double;
  FileSize: Int64;
begin
  if FImagePath = '' then Exit;

  ImageFile := TFileStream.Create(FImagePath, fmOpenRead or fmShareDenyNone);
  try
    FileSize := ImageFile.Size;
    // Bug in calculation, only correct for 720KB might be free FreeClusters calc that is off.
    // Set total clusters based on image size
    if FileSize = 368640 then       // 360KB
      TotalClusters := 343
    else if FileSize = 409600 then  // 400KB
      TotalClusters := 577
    else if FileSize = 737280 then  // 720KB
      TotalClusters := 614
    else if FileSize = 819200 then  // 800KB
      TotalClusters := 1154
    else
      TotalClusters := 343;        // Default to 360KB if unknown
    //ShowMessage(Format('Total clusters for this image: %d', [TotalClusters]));
    //ShowMessage(Format('Total filesize for this image: %d', [FileSize]));
    GetFATDetails(ImageFile);
    FreeClusters := GetFreeSpace(ImageFile);
    //ShowMessage(Format('Total FreeClusters for this image: %d', [FreeClusters]));

    // Calculate utilized space percentage
    if TotalClusters > 0 then
    begin
      UtilizedPercent := 100 - (FreeClusters / TotalClusters) * 100;
      ProgressBarFreeSpace.Position := Round(UtilizedPercent);
      //LabelFreeSpace.Caption := Format('Utilized space: %.1f%% (%d/%d clusters used)',
      //  [UtilizedPercent, TotalClusters - FreeClusters, TotalClusters]);
      LabelFreeSpace.Caption := Format('Utilized space: %.1f%%',[UtilizedPercent]);
    end
    else
    begin
      ProgressBarFreeSpace.Position := 0;
      //LabelFreeSpace.Caption := 'Utilized space: 0% (0/0 clusters used)';
      LabelFreeSpace.Caption := 'Utilized space: 0%';
    end;
  finally
    ImageFile.Free;
  end;
end;


{------------------------------------------------------------------
  Procedure: ButtonSaveFileClick
  Purpose:  Event handler for the "Save File" button
            Saves a file from the image to the local filesystem
 ------------------------------------------------------------------}
procedure TForm1.ButtonSaveFileClick(Sender: TObject);
var
  SelectedIndex: Integer;
  FileNameInList: string;
  FileNameInImage: string;
  LocalFileName: string;
  ImageFile: TFileStream;
  FileEntryPos: Int64;
  DirEntry: array[0..FAT12_DIR_ENTRY_SIZE - 1] of Byte;
  StartCluster: Word;
  FileSize: Int64;
  CurrentCluster: Word;
  FileData: TFileStream;
  Buffer: array[0..4095] of Byte;
  BytesToRead: Int64;
  BytesRead: Int64;
begin
  SelectedIndex := ListBoxFiles.ItemIndex;
  if SelectedIndex < 0 then Exit;
  FileNameInList := ListBoxFiles.Items[SelectedIndex];
  if FileNameInList.EndsWith(' *A*') or FileNameInList.EndsWith(' *D*') then
  begin
    ShowMessage('This file is either newly added or marked for deletion. Please write the image first before saving.');
    Exit;
  end;
  if FileNameInList.EndsWith('\') then
  begin
    ShowMessage('Cannot save a directory.');
    Exit;
  end;
  if FImagePath = '' then
  begin
    ShowMessage('No image file is open.');
    Exit;
  end;
  FileNameInImage := FileNameInList;
  SaveDialog.Filter := 'All Files (*.*)|*.*';
  SaveDialog.FileName := FileNameInImage;
  if not SaveDialog.Execute then Exit;
  LocalFileName := SaveDialog.FileName;
  try
    ImageFile := TFileStream.Create(FImagePath, fmOpenRead);
    try
      GetFATDetails(ImageFile);
      FileEntryPos := FindFileInDirectory(ImageFile, FindDirectoryInPath(ImageFile, FCurrentPath), FileNameInImage);
      if FileEntryPos = -1 then
      begin
        ShowMessage(Format(ERR_FILE_NOT_FOUND, [FileNameInImage]));
        Exit;
      end;
      ImageFile.Position := FileEntryPos;
      ImageFile.Read(DirEntry, FAT12_DIR_ENTRY_SIZE);
      // Read file size (little-endian)
      FileSize := DirEntry[28] or
                  (Int64(DirEntry[29]) shl 8) or
                  (Int64(DirEntry[30]) shl 16) or
                  (Int64(DirEntry[31]) shl 24);
      Move(DirEntry[26], StartCluster, 2);

      // Handle empty files
      if FileSize = 0 then
      begin
        FileData := TFileStream.Create(LocalFileName, fmCreate);
        FileData.Free;
        Exit;
      end;

      // Save file data
      FileData := TFileStream.Create(LocalFileName, fmCreate);
      try
        CurrentCluster := StartCluster;
        while (CurrentCluster > 1) and (CurrentCluster < $FF8) and (FileSize > 0) do
        begin
          BytesToRead := FBytesPerCluster;
          if FileSize < FBytesPerCluster then
            BytesToRead := FileSize;
          ImageFile.Position := FDataAreaStart + (CurrentCluster - 2) * FBytesPerCluster;
          BytesRead := ImageFile.Read(Buffer[0], BytesToRead);
          if BytesRead > 0 then
            FileData.Write(Buffer[0], BytesRead);
          Dec(FileSize, BytesRead);
          CurrentCluster := GetNextCluster(ImageFile, CurrentCluster);
        end;
      finally
        FileData.Free;
      end;
    finally
      ImageFile.Free;
    end;
    ShowMessage('File saved successfully: ' + LocalFileName);
  except
    on E: Exception do
      ShowMessage('Error: ' + E.Message);
  end;
end;

end.

