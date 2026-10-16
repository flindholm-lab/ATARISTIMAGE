unit Unit1;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLType, ExtCtrls, ComCtrls, StrUtils, Math, DateUtils, LazFileUtils,
  FileUtil, Menus, zipper, unit_clustermap, unit_msa, unit_fileviewer, unit_dragout,
  unit_depacker;

type
  { TDiskItemInfo represents a single file or directory in the Atari disk image }
  TDiskItemInfo = record
    DisplayName: string;
    CleanName: string;
    Size: Int64;
    SizeStr: string;
    StartCluster: Word;
    ClusterStr: string;
    DateTimeStr: string;
    AttrStr: string;
    StatusStr: string;
    IsDir: Boolean;
    IsDeleted: Boolean;
    IsHidden: Boolean;
    IsStagedAdd: Boolean;
    IsMarkedDelete: Boolean;
  end;

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
    ButtonCreate720: TButton;
    ButtonClusterMap: TButton;
    ButtonAddFolder: TButton;
    ButtonExtractAll: TButton;
    ButtonViewFile: TButton;
    ButtonBack: TButton;
    ButtonSetLabel: TButton;
    Label1: TLabel;
    LabelVolume: TLabel;
    LabelFreeSpace: TLabel;
    LabelCurrentPath: TLabel;
    ListViewFiles: TListView;
    OpenDialog: TOpenDialog;
    ProgressBarFreeSpace: TProgressBar;
    SaveDialog: TSaveDialog;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    MainMenu1: TMainMenu;
    MenuItemFile: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemNew720: TMenuItem;
    MenuItemWrite: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    MenuItemSep1: TMenuItem;
    MenuItemExtractFile: TMenuItem;
    MenuItemExtractDepacked: TMenuItem;
    MenuItemExtractAll: TMenuItem;
    MenuItemSep2: TMenuItem;
    MenuItemConvertMSA: TMenuItem;
    MenuItemSep3: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemAddFile: TMenuItem;
    MenuItemAddFolder: TMenuItem;
    MenuItemCreateDir: TMenuItem;
    MenuItemSep4: TMenuItem;
    MenuItemDelete: TMenuItem;
    MenuItemRemoveDir: TMenuItem;
    MenuItemSetLabel: TMenuItem;
    MenuItemTools: TMenuItem;
    MenuItemViewFile: TMenuItem;
    MenuItemClusterMap: TMenuItem;

    // Event Handlers
    procedure ButtonAddFileClick(Sender: TObject);
    procedure ButtonAddFolderClick(Sender: TObject);
    procedure ButtonCreateDirClick(Sender: TObject);
    procedure ButtonExitClick(Sender: TObject);
    procedure ButtonOpenImageClick(Sender: TObject);
    procedure ButtonRemoveDirClick(Sender: TObject);
    procedure ButtonRemoveFileClick(Sender: TObject);
    procedure ButtonWriteImageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonBackClick(Sender: TObject);
    procedure ListViewFilesClick(Sender: TObject);
    procedure ListViewFilesDblClick(Sender: TObject);
    procedure ListViewFilesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure ListViewFilesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ListViewFilesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ListViewFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListViewFilesCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ListViewFilesCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonSaveFileClick(Sender: TObject);
    procedure MenuItemExtractDepackedClick(Sender: TObject);
    procedure ButtonCreate720Click(Sender: TObject);
    procedure ButtonClusterMapClick(Sender: TObject);
    procedure ButtonExtractAllClick(Sender: TObject);
    procedure ButtonViewFileClick(Sender: TObject);
    procedure ButtonSetLabelClick(Sender: TObject);
    procedure MenuItemConvertMSAClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormShow(Sender: TObject);

  private
    // Lists for tracking files, directories, and pending operations
    FFilePaths: TStringList;          // Files to be added to the image (host paths)
    FFileDestDirs: TStringList;       // Target directory inside the image for each file in FFilePaths
    FFilesToDelete: TStringList;      // Files marked for deletion
    FDisplayItems: TStringList;       // Items displayed in the current directory
    FNewDirectories: TStringList;     // Directories to be created
    FDirsToDelete: TStringList;       // Directories marked for deletion
    FDiskItems: array of TDiskItemInfo; // Detailed items currently in directory
    FDiskItemsRaw: array of TDiskItemInfo; // Detailed items on disk in current directory
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
    FTotalClusters: Word;             // Total allocatable data clusters
    FRootEntries: Word;               // Number of root directory entries
    FVolumeLabel: string;             // Atari ST volume label (from root directory Attr = $08)
    FDiskSerial: LongWord;            // 24-bit Atari disk serial number (from boot sector $08..$0A)
    FIsMSAImage: Boolean;             // True if currently loaded from/saving to MSA
    FOriginalMSAPath: string;         // Original host path of the MSA file
    FTempSTPath: string;              // Temporary ST image file when working with MSA
    FTempZipExtractedPath: string;    // Temporary ST/MSA image file extracted from ZIP
    FOriginalZipPath: string;         // Original host path of the ZIP archive
    FDragStartPoint: TPoint;          // Mouse point where drag-out starts
    FDragPending: Boolean;            // Flag indicating left button pressed on an item
    FDragActive: Boolean;             // Flag indicating active outgoing OLE drag operation
    FDragSourcePath: string;          // Full host path of currently dragged-out file

    // Core Procedures and Functions
    function GetSelectedFileName(out AItemText, ACleanName: string; out AIsDir, AIsStaged, AIsDeleted, AIsMarkedDelete: Boolean): Boolean;
    procedure AddDiskItemToListView(const AInfo: TDiskItemInfo);
    function FormatSizeStr(Size: Int64; IsDir: Boolean): string;
    function FormatFATDateTime(DateVal, TimeVal: Word): string;
    function FormatFATAttr(Attr: Byte): string;

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
    procedure WriteFileToImage(const AImageFile: TFileStream; const AFileName: string; const ASourceFile: TFileStream; const ADestPath: string);
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
    procedure CreateDirectoryInImage(const AImageFile: TFileStream; const ADirPath: string);
      // Creates a new directory in the image
    procedure ScanDirectoryRecursive(const AHostPath, ATargetRelPath: string);
      // Recursively scans host directory and stages all files/subdirs
    function SanitizeDOSName(const AName: string; IsDir: Boolean): string;
      // Sanitizes a name to conform to Atari TOS FAT12 8.3 constraints
    function IsDirectChild(const AParentPath, AFullPath: string; out AChildName: string): Boolean;
      // Checks if AFullPath is a direct child of AParentPath
    procedure CreateBlank720KBImage(const ATargetFile: string);
      // Creates a clean, standard 720KB Atari ST floppy image
    function LoadDiskImage(const AFileName: string): Boolean;
    function LoadZipArchive(const AZipPath: string): Boolean;
    function SelectDiskImageFromZip(const AZipPath: string; MatchingEntries: TStrings): Integer;
    procedure AddFolderToZipper(const ABaseDir, ARelPath: string; AZip: TZipper);
    procedure ZipListDblClick(Sender: TObject);
    procedure ZipListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ExtractDiskDirectory(const AImageFile: TFileStream; const ADirCluster: Int64; const AHostPath: string; var AFileCount: Integer; var AByteCount: Int64);
    procedure WriteVolumeLabelToImage(const AImageFile: TFileStream; const ALabel: string);
    function ExtractFileToStream(const AFileName: string; AOutStream: TStream; out AStartCluster: Word): Boolean;
    function ExtractFileToTemp(const AFileName: string): string;

    // Helper Functions
    function IsDirectoryEntryValid(const DirEntry: array of Byte): Boolean;
      // Checks if a directory entry is valid
    function IsDirectory(const DirEntry: array of Byte): Boolean;
      // Checks if a directory entry represents a directory
    function IsHidden(const DirEntry: array of Byte): Boolean;
      // Checks if a directory entry is marked as hidden
    function IsDeleted(const DirEntry: array of Byte): Boolean;
      // Checks if a directory entry is marked as deleted
    function IsEndOfDirectory(const DirEntry: array of Byte): Boolean;
      // Checks if a directory entry marks the end of the directory
    function ExtractFileNameFromEntry(const DirEntry: array of Byte): string;
      // Extracts the filename from a directory entry
    function CleanFileName(const AItemName: string): string;
      // Strips display tags (*d*, *h*, *D*, *A*) from item name
    function FindDeletedFileInDirectory(const AImageFile: TFileStream; const ADirCluster: Int64; const AFileName: string): Int64;
      // Finds a deleted directory entry matching filename
    procedure ValidateDirectoryName(const DirName: string);
      // Validates a directory name
    procedure UpdateDeleteButtonState;
      // Updates the Delete/Undelete button caption based on current selection
    procedure UndeleteSelectedFile;
      // Restores a previously deleted file and re-links its FAT chain
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

{$IFDEF WINDOWS}
procedure DragAcceptFiles(hWnd: HWND; bAccept: LongBool); stdcall; external 'shell32.dll' name 'DragAcceptFiles';
{$ENDIF}

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
  Function: GetSelectedFileName
  Purpose:  Retrieves detailed info about the currently selected item in ListViewFiles
 ------------------------------------------------------------------}
function TForm1.GetSelectedFileName(out AItemText, ACleanName: string; out AIsDir, AIsStaged, AIsDeleted, AIsMarkedDelete: Boolean): Boolean;
var
  Idx: Integer;
begin
  Result := False;
  AItemText := '';
  ACleanName := '';
  AIsDir := False;
  AIsStaged := False;
  AIsDeleted := False;
  AIsMarkedDelete := False;

  if (ListViewFiles = nil) or (ListViewFiles.Selected = nil) then Exit;
  Idx := ListViewFiles.Selected.Index;
  if (Idx < 0) or (Idx > High(FDiskItems)) then Exit;

  AItemText := FDiskItems[Idx].DisplayName;
  ACleanName := FDiskItems[Idx].CleanName;
  AIsDir := FDiskItems[Idx].IsDir;
  AIsStaged := FDiskItems[Idx].IsStagedAdd;
  AIsDeleted := FDiskItems[Idx].IsDeleted;
  AIsMarkedDelete := FDiskItems[Idx].IsMarkedDelete;
  Result := True;
end;

{------------------------------------------------------------------
  Function: FormatSizeStr
  Purpose:  Formats a byte size for display in detailed file list
 ------------------------------------------------------------------}
function TForm1.FormatSizeStr(Size: Int64; IsDir: Boolean): string;
begin
  if IsDir then
    Result := '<DIR>'
  else if Size < 1024 then
    Result := Format('%d B', [Size])
  else if Size < 1024 * 1024 then
    Result := Format('%.1f KB', [Size / 1024.0])
  else
    Result := Format('%.2f MB', [Size / (1024.0 * 1024.0)]);
end;

{------------------------------------------------------------------
  Function: FormatFATDateTime
  Purpose:  Converts 16-bit FAT Date and Time words to standard date/time string
 ------------------------------------------------------------------}
function TForm1.FormatFATDateTime(DateVal, TimeVal: Word): string;
var
  Y, M, D, H, N: Word;
begin
  if (DateVal = 0) and (TimeVal = 0) then Exit('-');
  Y := 1980 + ((DateVal shr 9) and $7F);
  M := (DateVal shr 5) and $0F;
  D := DateVal and $1F;
  H := (TimeVal shr 11) and $1F;
  N := (TimeVal shr 5) and $3F;
  Result := Format('%.4d-%.2d-%.2d %.2d:%.2d', [Y, M, D, H, N]);
end;

{------------------------------------------------------------------
  Function: FormatFATAttr
  Purpose:  Converts FAT attribute byte to human-readable string (D A R H S)
 ------------------------------------------------------------------}
function TForm1.FormatFATAttr(Attr: Byte): string;
var
  S: string;
begin
  S := '';
  if (Attr and $10) <> 0 then S := S + 'D' else S := S + '-';
  if (Attr and $20) <> 0 then S := S + 'A' else S := S + '-';
  if (Attr and $01) <> 0 then S := S + 'R' else S := S + '-';
  if (Attr and $02) <> 0 then S := S + 'H' else S := S + '-';
  if (Attr and $04) <> 0 then S := S + 'S' else S := S + '-';
  Result := S;
end;

{------------------------------------------------------------------
  Procedure: AddDiskItemToListView
  Purpose:  Adds a TDiskItemInfo record to ListViewFiles columns
 ------------------------------------------------------------------}
procedure TForm1.AddDiskItemToListView(const AInfo: TDiskItemInfo);
var
  Item: TListItem;
begin
  Item := ListViewFiles.Items.Add;
  Item.Caption := AInfo.CleanName;
  Item.SubItems.Add(AInfo.SizeStr);
  Item.SubItems.Add(AInfo.ClusterStr);
  Item.SubItems.Add(AInfo.DateTimeStr);
  Item.SubItems.Add(AInfo.AttrStr);
  Item.SubItems.Add(AInfo.StatusStr);
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
  FFileDestDirs.Clear;
  FNewDirectories.Clear;
  FFilesToDelete.Clear;
  FDirsToDelete.Clear;
  FDisplayItems.Clear;
  FImagePath := '';
  FCurrentPath := '';
  FVolumeLabel := '';
  FDiskSerial := 0;
  Self.Caption := 'Atari .ST Image File Manager';
  ProgressBarFreeSpace.Position := 0;
  LabelFreeSpace.Caption := 'Utilized space: 0%';
  UpdateFileListDisplay;
end;

{------------------------------------------------------------------
  Function: SanitizeDOSName
  Purpose:  Sanitizes host file/directory name to conform to Atari TOS FAT12 8.3 rules
 ------------------------------------------------------------------}
function TForm1.SanitizeDOSName(const AName: string; IsDir: Boolean): string;
const
  ValidChars = ['A'..'Z', '0'..'9', '$', '%', '''', '-', '_', '@', '~', '!', '(', ')', '#', '&', '^'];
var
  CleanBase, Ext: string;
  I, DotPos: Integer;
  Ch: Char;
begin
  if IsDir then
  begin
    CleanBase := '';
    for I := 1 to Length(AName) do
    begin
      Ch := UpCase(AName[I]);
      if Ch in ValidChars then
        CleanBase := CleanBase + Ch
      else if (Ch = '.') or (Ch = ' ') or (Ch = '+') or (Ch = '=') then
        CleanBase := CleanBase + '_';
    end;
    if Length(CleanBase) > FAT12_MAX_DIR_NAME_LEN then
      CleanBase := Copy(CleanBase, 1, FAT12_MAX_DIR_NAME_LEN);
    if CleanBase = '' then
      CleanBase := 'DIR';
    Result := CleanBase;
  end
  else
  begin
    DotPos := LastDelimiter('.', AName);
    if DotPos > 0 then
    begin
      CleanBase := Copy(AName, 1, DotPos - 1);
      Ext := Copy(AName, DotPos + 1, Length(AName));
    end
    else
    begin
      CleanBase := AName;
      Ext := '';
    end;

    Result := '';
    for I := 1 to Length(CleanBase) do
    begin
      Ch := UpCase(CleanBase[I]);
      if Ch in ValidChars then
        Result := Result + Ch
      else if (Ch = ' ') or (Ch = '.') or (Ch = '+') or (Ch = '=') then
        Result := Result + '_';
    end;
    if Length(Result) > FAT12_MAX_DIR_NAME_LEN then
      Result := Copy(Result, 1, FAT12_MAX_DIR_NAME_LEN);
    if Result = '' then
      Result := 'FILE';

    if Ext <> '' then
    begin
      CleanBase := '';
      for I := 1 to Length(Ext) do
      begin
        Ch := UpCase(Ext[I]);
        if Ch in ValidChars then
          CleanBase := CleanBase + Ch;
      end;
      if Length(CleanBase) > FAT12_MAX_EXT_LEN then
        CleanBase := Copy(CleanBase, 1, FAT12_MAX_EXT_LEN);
      if CleanBase <> '' then
        Result := Result + '.' + CleanBase;
    end;
  end;
end;

{------------------------------------------------------------------
  Function: IsDirectChild
  Purpose:  Checks if AFullPath is an immediate child of AParentPath
 ------------------------------------------------------------------}
function TForm1.IsDirectChild(const AParentPath, AFullPath: string; out AChildName: string): Boolean;
var
  Remainder: string;
begin
  Result := False;
  AChildName := '';

  if AParentPath = '' then
  begin
    Remainder := AFullPath;
    if Remainder.EndsWith('\') then
      Remainder := Copy(Remainder, 1, Length(Remainder) - 1);
    if (Remainder <> '') and (Pos('\', Remainder) = 0) then
    begin
      AChildName := Remainder;
      Result := True;
    end;
  end
  else
  begin
    if StartsText(AParentPath, AFullPath) then
    begin
      Remainder := Copy(AFullPath, Length(AParentPath) + 1, Length(AFullPath));
      if Remainder.EndsWith('\') then
        Remainder := Copy(Remainder, 1, Length(Remainder) - 1);
      if (Remainder <> '') and (Pos('\', Remainder) = 0) then
      begin
        AChildName := Remainder;
        Result := True;
      end;
    end;
  end;
end;

{------------------------------------------------------------------
  Procedure: ScanDirectoryRecursive
  Purpose:  Recursively scans host folder and stages all subdirectories and files
 ------------------------------------------------------------------}
procedure TForm1.ScanDirectoryRecursive(const AHostPath, ATargetRelPath: string);
var
  SearchRec: TSearchRec;
  HostItemPath: string;
  DosName: string;
  SubTargetRelPath: string;
begin
  if FNewDirectories.IndexOf(ATargetRelPath) = -1 then
    FNewDirectories.Add(ATargetRelPath);

  if FindFirst(AppendPathDelim(AHostPath) + '*', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
          Continue;

        HostItemPath := AppendPathDelim(AHostPath) + SearchRec.Name;

        if (SearchRec.Attr and faDirectory) <> 0 then
        begin
          DosName := SanitizeDOSName(SearchRec.Name, True);
          SubTargetRelPath := ATargetRelPath + DosName + '\';
          ScanDirectoryRecursive(HostItemPath, SubTargetRelPath);
        end
        else
        begin
          FFilePaths.Add(HostItemPath);
          FFileDestDirs.Add(ATargetRelPath);
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

{------------------------------------------------------------------
  Procedure: ButtonAddFileClick
  Purpose:  Event handler for the "Add File" button
            Opens a file dialog to select files to add to the image
 ------------------------------------------------------------------}
procedure TForm1.ButtonAddFileClick(Sender: TObject);
var
  I: Integer;
  TargetName: string;
begin
  OpenDialog.Options := [ofAllowMultiSelect, ofFileMustExist];
  OpenDialog.Filter := 'All Files (*.*)|*.*';
  if OpenDialog.Execute then
  begin
    for I := 0 to OpenDialog.Files.Count - 1 do
    begin
      if DirectoryExists(OpenDialog.Files[I]) then
      begin
        TargetName := SanitizeDOSName(ExtractFileName(ExcludeTrailingPathDelimiter(OpenDialog.Files[I])), True);
        ScanDirectoryRecursive(OpenDialog.Files[I], FCurrentPath + TargetName + '\');
      end
      else
      begin
        FFilePaths.Add(OpenDialog.Files[I]);
        FFileDestDirs.Add(FCurrentPath);
      end;
    end;
    UpdateFileListDisplay;
  end;
end;

{------------------------------------------------------------------
  Procedure: ButtonAddFolderClick
  Purpose:  Event handler for the "Add Folder(s)" button
            Opens a folder browser dialog to add a directory tree recursively
 ------------------------------------------------------------------}
procedure TForm1.ButtonAddFolderClick(Sender: TObject);
var
  SelectedDir, TargetName: string;
begin
  SelectDirectoryDialog.Title := 'Select Folder to Add (Recursive)';
  if SelectDirectoryDialog.Execute then
  begin
    SelectedDir := ExcludeTrailingPathDelimiter(SelectDirectoryDialog.FileName);
    if (SelectedDir <> '') and DirectoryExists(SelectedDir) then
    begin
      TargetName := SanitizeDOSName(ExtractFileName(SelectedDir), True);
      ScanDirectoryRecursive(SelectedDir, FCurrentPath + TargetName + '\');
      UpdateFileListDisplay;
    end;
  end;
end;

{------------------------------------------------------------------
  Procedure: ButtonCreateDirClick
  Purpose:  Event handler for the "Create Directory" button
            Prompts the user for a directory name and adds it to the pending list
 ------------------------------------------------------------------}
procedure TForm1.ButtonCreateDirClick(Sender: TObject);
var
  DirName, TargetRelPath: string;
begin
  DirName := InputBox('Create Directory', 'Enter directory name (max 8 chars):', '');
  if DirName = '' then Exit;
  DirName := SanitizeDOSName(DirName, True);
  TargetRelPath := FCurrentPath + DirName + '\';
  if FNewDirectories.IndexOf(TargetRelPath) = -1 then
    FNewDirectories.Add(TargetRelPath);
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
  Function: IsHidden
  Purpose:  Checks if a directory entry is marked as hidden
 ------------------------------------------------------------------}
function TForm1.IsHidden(const DirEntry: array of Byte): Boolean;
begin
  Result := (DirEntry[11] and $02) <> 0;
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
  begin
    if (I = 0) and (DirEntry[0] = FAT12_DELETED) then
      FileName := '_'
    else
      FileName := FileName + AnsiChar(DirEntry[I]);
  end;
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
  Function: CleanFileName
  Purpose:  Strips display tags (*d*, *h*, *D*, *A*) from item name
 ------------------------------------------------------------------}
function TForm1.CleanFileName(const AItemName: string): string;
var
  S: string;
begin
  S := AItemName;
  S := StringReplace(S, ' *d*', '', [rfReplaceAll, rfIgnoreCase]);
  S := StringReplace(S, ' *h*', '', [rfReplaceAll, rfIgnoreCase]);
  S := StringReplace(S, ' *D*', '', [rfReplaceAll, rfIgnoreCase]);
  S := StringReplace(S, ' *A*', '', [rfReplaceAll, rfIgnoreCase]);
  Result := Trim(S);
end;

{------------------------------------------------------------------
  Procedure: CreateDirectoryInImage
  Purpose:  Creates a new directory in the image
  Parameters:
    AImageFile - The image file stream
    DirName    - Name of the directory to create
 ------------------------------------------------------------------}
procedure TForm1.CreateDirectoryInImage(const AImageFile: TFileStream; const ADirPath: string);
var
  DirEntry: array[0..FAT12_DIR_ENTRY_SIZE - 1] of Byte;
  DirEntryPos, ParentCluster: Int64;
  DirClusterList: TStringList;
  NewCluster: Word;
  TimeWord, DateWord, ParentClusterWord: Word;
  CleanPath, ParentPath, DirName: string;
  LastSlash: Integer;
  ClusterBuffer: array of Byte;
begin
  GetFATDetails(AImageFile);

  CleanPath := ExcludeTrailingPathDelimiter(ADirPath);
  LastSlash := LastDelimiter('\', CleanPath);
  if LastSlash > 0 then
  begin
    ParentPath := Copy(CleanPath, 1, LastSlash);
    DirName := Copy(CleanPath, LastSlash + 1, Length(CleanPath));
  end
  else
  begin
    ParentPath := '';
    DirName := CleanPath;
  end;

  ParentCluster := FindDirectoryInPath(AImageFile, ParentPath);
  if ParentCluster = -1 then Exit;

  DirEntryPos := FindFreeDirectoryEntry(AImageFile, ParentCluster);
  if DirEntryPos = -1 then Exit;

  // Find a free cluster for the new directory
  DirClusterList := FindFreeClusters(AImageFile, 1);
  if DirClusterList.Count = 0 then Exit;
  NewCluster := StrToInt(DirClusterList[0]);

  // Mark the cluster as end-of-chain in all FAT copies
  WriteFAT12EntryAllCopies(AImageFile, NewCluster, FAT12_EOC);

  // Zero-initialize the entire newly allocated cluster to prevent ghost entries
  SetLength(ClusterBuffer, FBytesPerCluster);
  FillChar(ClusterBuffer[0], FBytesPerCluster, 0);
  AImageFile.Position := FDataAreaStart + (NewCluster - 2) * FBytesPerCluster;
  AImageFile.Write(ClusterBuffer[0], FBytesPerCluster);

  TimeWord := DateTimeToFatTime(Now);
  DateWord := DateTimeToFatDate(Now);

  // . entry (points to current directory cluster)
  AImageFile.Position := FDataAreaStart + (NewCluster - 2) * FBytesPerCluster;
  FillChar(DirEntry, SizeOf(DirEntry), $20);
  Move('.       ', DirEntry[0], 8);
  DirEntry[11] := $10;
  FillChar(DirEntry[12], 10, 0);
  Move(TimeWord, DirEntry[22], 2);
  Move(DateWord, DirEntry[24], 2);
  Move(NewCluster, DirEntry[26], 2);
  FillChar(DirEntry[28], 4, 0);
  AImageFile.Write(DirEntry, FAT12_DIR_ENTRY_SIZE);

  // .. entry (points to parent directory cluster)
  FillChar(DirEntry, SizeOf(DirEntry), $20);
  Move('..      ', DirEntry[0], 8);
  DirEntry[11] := $10;
  FillChar(DirEntry[12], 10, 0);
  Move(TimeWord, DirEntry[22], 2);
  Move(DateWord, DirEntry[24], 2);
  if ParentCluster = 0 then
    ParentClusterWord := 0
  else
    ParentClusterWord := Word(ParentCluster);
  Move(ParentClusterWord, DirEntry[26], 2);
  FillChar(DirEntry[28], 4, 0);
  AImageFile.Write(DirEntry, FAT12_DIR_ENTRY_SIZE);

  // Write directory entry in parent directory
  FillChar(DirEntry, SizeOf(DirEntry), $20);
  Move(PAnsiChar(UpperCase(DirName))^, DirEntry[0], Min(FAT12_MAX_DIR_NAME_LEN, Length(DirName)));
  DirEntry[11] := $10;
  FillChar(DirEntry[12], 10, 0);
  Move(TimeWord, DirEntry[22], 2);
  Move(DateWord, DirEntry[24], 2);
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
  OpenDialog.Filter := 'Atari ST Disk Images (*.st;*.msa;*.zip)|*.st;*.msa;*.zip|ST Image Files (*.st)|*.st|MSA Image Files (*.msa)|*.msa|ZIP Archives (*.zip)|*.zip|All Files (*.*)|*.*';
  if OpenDialog.Execute then
  begin
    LoadDiskImage(OpenDialog.FileName);
  end;
end;

{------------------------------------------------------------------
  Procedure: ButtonRemoveDirClick
  Purpose:  Event handler for the "Remove Directory" button
            Marks the selected directory for deletion
 ------------------------------------------------------------------}
procedure TForm1.ButtonRemoveDirClick(Sender: TObject);
var
  RawName, DirName, TargetDir: string;
  IsDir, IsStaged, IsDel, IsMarkedDel: Boolean;
  I: Integer;
begin
  if not GetSelectedFileName(RawName, DirName, IsDir, IsStaged, IsDel, IsMarkedDel) then Exit;
  if IsDel then
  begin
    ShowMessage('This directory is already deleted.');
    Exit;
  end;

  if not IsDir or not DirName.EndsWith('\') then
  begin
    ShowMessage('Selected item is not a directory.');
    Exit;
  end;

  // Case 1: Staged directory addition (*A*) -> Cancel addition of dir and all nested contents
  if IsStaged then
  begin
    TargetDir := FCurrentPath + DirName;
    for I := FNewDirectories.Count - 1 downto 0 do
    begin
      if StartsText(TargetDir, FNewDirectories[I]) then
        FNewDirectories.Delete(I);
    end;
    for I := FFilePaths.Count - 1 downto 0 do
    begin
      if (I < FFileDestDirs.Count) and StartsText(TargetDir, FFileDestDirs[I]) then
      begin
        FFilePaths.Delete(I);
        FFileDestDirs.Delete(I);
      end;
    end;
    UpdateFileListDisplay;
    Exit;
  end;

  // Case 2: On-disk directory -> toggle deletion marking
  DirName := StringReplace(DirName, '\', '', [rfReplaceAll]);
  if FDirsToDelete.IndexOf(DirName) = -1 then
    FDirsToDelete.Add(DirName)
  else
    FDirsToDelete.Delete(FDirsToDelete.IndexOf(DirName));
  UpdateFileListDisplay;
end;

{------------------------------------------------------------------
  Procedure: ButtonRemoveFileClick
  Purpose:  Event handler for the Delete/Undelete button
            Toggles deletion for active files, undeletes deleted
            files (*d*), or cancels staged additions (*A*)
 ------------------------------------------------------------------}
procedure TForm1.ButtonRemoveFileClick(Sender: TObject);
var
  RawName, FileName: string;
  IsDir, IsStaged, IsDel, IsMarkedDel: Boolean;
  Index, I, SavedIdx: Integer;
begin
  if not GetSelectedFileName(RawName, FileName, IsDir, IsStaged, IsDel, IsMarkedDel) then
  begin
    ShowMessage('Please select a file first.');
    Exit;
  end;

  if (FileName = '..') or (FileName = '..\') then
    Exit;

  // Case 1: Deleted file on disk (*d*) -> Undelete it directly
  if IsDel then
  begin
    UndeleteSelectedFile;
    Exit;
  end;

  if IsDir or FileName.EndsWith('\') then
  begin
    ShowMessage('Selected item is a directory. Use "Remove Dir" instead.');
    Exit;
  end;

  // Case 2: Staged file addition (*A*) -> Cancel addition
  if IsStaged then
  begin
    Index := -1;
    for I := 0 to FFilePaths.Count - 1 do
    begin
      if SameText(ExtractFileName(FFilePaths[I]), FileName) and
         (I < FFileDestDirs.Count) and SameText(FFileDestDirs[I], FCurrentPath) then
      begin
        Index := I;
        Break;
      end;
    end;
    if Index <> -1 then
    begin
      FFilePaths.Delete(Index);
      if Index < FFileDestDirs.Count then
        FFileDestDirs.Delete(Index);
    end;
    UpdateFileListDisplay;
    Exit;
  end;

  // Case 3: Toggle deletion marking for active files on disk (*D*)
  SavedIdx := -1;
  if ListViewFiles.Selected <> nil then
    SavedIdx := ListViewFiles.Selected.Index;

  Index := FFilesToDelete.IndexOf(FileName);
  if Index <> -1 then
    FFilesToDelete.Delete(Index)
  else
    FFilesToDelete.Add(FileName);

  UpdateFileListDisplay;
  if (SavedIdx >= 0) and (SavedIdx < ListViewFiles.Items.Count) then
  begin
    ListViewFiles.Items[SavedIdx].Selected := True;
    ListViewFiles.Items[SavedIdx].Focused := True;
  end;
  UpdateDeleteButtonState;
end;

{------------------------------------------------------------------
  Procedure: ButtonWriteImageClick
  Purpose:  Event handler for the "Write Image" button
            Writes all pending changes to the image file
 ------------------------------------------------------------------}
{------------------------------------------------------------------
  Procedure: CreateBlank720KBImage
  Purpose:  Generates a clean standard 720KB Atari ST disk image
 ------------------------------------------------------------------}
procedure TForm1.CreateBlank720KBImage(const ATargetFile: string);
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
  ImageFile: TFileStream;
  BootSector: TBytes;
  FAT: array[0..1535] of Byte;
  RootDir: array[0..3583] of Byte;
  DataAreaSize: Int64;
  DataArea: TBytes;
begin
  ImageFile := TFileStream.Create(ATargetFile, fmCreate);
  try
    // Create boot sector
    SetLength(BootSector, BYTES_PER_SECTOR);
    FillChar(BootSector[0], BYTES_PER_SECTOR, 0);
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
  I, J: Integer;
  OriginalSize: Int64;
  TempStr, FileDestDir: string;
begin
  SaveDialog.Filter := 'Atari .ST Image (*.st)|*.st|Magic Shadow Archiver (*.msa)|*.msa|All Files (*.*)|*.*';
  if FIsMSAImage and (FOriginalMSAPath <> '') then
    SaveDialog.FileName := ExtractFileName(FOriginalMSAPath)
  else if (FOriginalZipPath <> '') and (FTempZipExtractedPath <> '') then
    SaveDialog.FileName := ExtractFileName(FTempZipExtractedPath)
  else if FImagePath <> '' then
    SaveDialog.FileName := ExtractFileName(FImagePath)
  else
    SaveDialog.FileName := 'new_disk.st';

  if SaveDialog.Execute then
  begin
    SaveFileName := SaveDialog.FileName;
    TempFileName := GetTempFile(ExtractFilePath(SaveFileName), 'fat-temp-');

    if (FImagePath <> '') and FileExists(FImagePath) then
    begin
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
    end
    else
    begin
      // No existing image opened: automatically initialize a standard 720KB ST image
      CreateBlank720KBImage(TempFileName);
      OriginalSize := 737280;
    end;

    // Reopen temp image for read/write
    TempImageFile := TFileStream.Create(TempFileName, fmOpenReadWrite);
    try
      GetFATDetails(TempImageFile);

      // Sort new directories by path depth (length) so parent directories are created first
      for I := 0 to FNewDirectories.Count - 2 do
        for J := I + 1 to FNewDirectories.Count - 1 do
          if Length(FNewDirectories[I]) > Length(FNewDirectories[J]) then
          begin
            TempStr := FNewDirectories[I];
            FNewDirectories[I] := FNewDirectories[J];
            FNewDirectories[J] := TempStr;
          end;

      // Create new directories
      for I := 0 to FNewDirectories.Count - 1 do
        CreateDirectoryInImage(TempImageFile, FNewDirectories[I]);
      FNewDirectories.Clear;

      // Write new files
      for I := 0 to FFilePaths.Count - 1 do
      begin
        if (Trim(FFilePaths[I]) = '') or (not FileExists(FFilePaths[I])) then
          Continue;

        if FFilesToDelete.IndexOf(UpperCase(ExtractFileName(FFilePaths[I]))) = -1 then
        begin
          if I < FFileDestDirs.Count then
            FileDestDir := FFileDestDirs[I]
          else
            FileDestDir := FCurrentPath;

          SourceFile := TFileStream.Create(FFilePaths[I], fmOpenRead or fmShareDenyNone);
          try
            WriteFileToImage(TempImageFile, ExtractFileName(FFilePaths[I]), SourceFile, FileDestDir);
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

      // Write / update Atari ST volume label in root directory
      if FVolumeLabel <> '' then
        WriteVolumeLabelToImage(TempImageFile, FVolumeLabel);

      // Enforce original file size to prevent expansion
      TempImageFile.Size := OriginalSize;
    finally
      TempImageFile.Free;
    end;

    // Handle MSA compression vs raw ST saving
    if SameText(ExtractFileExt(SaveFileName), '.msa') then
    begin
      if FileExists(SaveFileName) then SysUtils.DeleteFile(SaveFileName);
      if not PackSTToMSA(TempFileName, SaveFileName, 9, 2) then
      begin
        ShowMessage('Error: Failed to encode compressed MSA disk image.');
        Exit;
      end;
      SysUtils.DeleteFile(TempFileName);
      FIsMSAImage := True;
      FOriginalMSAPath := SaveFileName;
    end
    else
    begin
      if FileExists(SaveFileName) then SysUtils.DeleteFile(SaveFileName);
      RenameFile(TempFileName, SaveFileName);
      FIsMSAImage := False;
      FOriginalMSAPath := '';
    end;
    ShowMessage('Changes successfully written to image.');

    // Clear pending lists after writing
    FNewDirectories.Clear;
    FFilePaths.Clear;
    FFileDestDirs.Clear;
    FDirsToDelete.Clear;
    FFilesToDelete.Clear;

    // Reload image
    if (FTempZipExtractedPath <> '') and FileExists(FTempZipExtractedPath) then
    begin
      SysUtils.DeleteFile(FTempZipExtractedPath);
      RemoveDir(ExtractFilePath(FTempZipExtractedPath));
      FTempZipExtractedPath := '';
      FOriginalZipPath := '';
    end;
    FImagePath := SaveFileName;
    if FIsMSAImage then
      LoadDiskImage(SaveFileName)
    else
    begin
      ReadSTImage(FImagePath, FCurrentPath);
      UpdateFileListDisplay;
      UpdateFreeSpaceDisplay;
      Self.Caption := 'Atari .ST Image File Manager - ' + ExtractFileName(FImagePath);
    end;
  end;
end;

{------------------------------------------------------------------
  Procedure: ButtonCreate720Click
  Purpose:  Event handler for the "Create 720KB Image" button
            Creates a new empty 720KB ST image file
 ------------------------------------------------------------------}
procedure TForm1.ButtonCreate720Click(Sender: TObject);
var
  SaveFileName: string;
  TempFileName: string;
begin
  SaveDialog.Filter := 'ST Image Files (*.st)|*.st|All Files (*.*)|*.*';
  SaveDialog.FileName := 'empty720kb.st';
  if SaveDialog.Execute then
  begin
    SaveFileName := SaveDialog.FileName;
    TempFileName := GetTempFile(ExtractFilePath(SaveFileName), 'fat-temp-');
    try
      CreateBlank720KBImage(TempFileName);
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
    UpdateFreeSpaceDisplay;
  end;
end;

{------------------------------------------------------------------
  Procedure: ButtonClusterMapClick
  Purpose:  Event handler for the "Cluster Map" button
            Opens the Cluster Map and Sector Hex Editor
 ------------------------------------------------------------------}
procedure TForm1.ButtonClusterMapClick(Sender: TObject);
var
  RawName, FileName: string;
  IsDir, IsStaged, IsDel, IsMarkedDel: Boolean;
  ImageFile: TFileStream;
  DirCluster, DirEntryPos: Int64;
  DirEntry: array[0..31] of Byte;
  StartCluster, CurrCluster: Word;
  ClusterList: array of Word;
begin
  if (FImagePath = '') or not FileExists(FImagePath) then
  begin
    ShowMessage('Please open or write an image first.');
    Exit;
  end;

  SetLength(ClusterList, 0);

  if GetSelectedFileName(RawName, FileName, IsDir, IsStaged, IsDel, IsMarkedDel) then
  begin
    if not IsDir and not IsStaged and not FileName.EndsWith('\') then
    begin
      ImageFile := TFileStream.Create(FImagePath, fmOpenRead or fmShareDenyNone);
      try
        GetFATDetails(ImageFile);
        if FCurrentPath = '' then
          DirCluster := 0
        else
          DirCluster := FindDirectoryInPath(ImageFile, FCurrentPath);

        DirEntryPos := FindFileInDirectory(ImageFile, DirCluster, FileName);
        if DirEntryPos <> -1 then
        begin
          ImageFile.Position := DirEntryPos;
          ImageFile.Read(DirEntry[0], 32);
          StartCluster := DirEntry[26] or (DirEntry[27] shl 8);
          CurrCluster := StartCluster;
          while (CurrCluster >= 2) and (CurrCluster <= FTotalClusters + 1) do
          begin
            SetLength(ClusterList, Length(ClusterList) + 1);
            ClusterList[High(ClusterList)] := CurrCluster;
            CurrCluster := GetNextCluster(ImageFile, CurrCluster);
            if CurrCluster >= $FF8 then Break;
          end;
        end;
      finally
        ImageFile.Free;
      end;
    end;
  end;

  if Length(ClusterList) > 0 then
    FormClusterMap.SetHighlightedClusters(ClusterList)
  else
    FormClusterMap.ClearHighlight;

  FormClusterMap.LoadImage(FImagePath);
  FormClusterMap.ShowModal;

  // Refresh display after potential FAT or sector edits
  ReadSTImage(FImagePath, FCurrentPath);
  UpdateFileListDisplay;
  UpdateFreeSpaceDisplay;
end;

{------------------------------------------------------------------
  Procedure: UndeleteSelectedFile
  Purpose:  Restores a previously deleted file (*d*) and re-links its FAT chain
 ------------------------------------------------------------------}
procedure TForm1.UndeleteSelectedFile;
var
  SelectedItem, CleanName, FileNameOnly, FileExt: string;
  IsDir, IsStaged, IsDel, IsMarkedDel: Boolean;
  NewFirstCharStr, RestoredFileName: string;
  ImageFile: TFileStream;
  DirCluster, DirEntryPos: Int64;
  DirEntry: array[0..FAT12_DIR_ENTRY_SIZE - 1] of Byte;
  StartCluster: Word;
  FileSize: Int64;
  ClustersNeeded, I: Integer;
  CurrCluster, NextVal: Word;
  OverwriteWarning: Boolean;
  FirstChar: AnsiChar;
begin
  if FImagePath = '' then
  begin
    ShowMessage('No image file is open.');
    Exit;
  end;

  if not GetSelectedFileName(SelectedItem, CleanName, IsDir, IsStaged, IsDel, IsMarkedDel) then
  begin
    ShowMessage('Please select a deleted file (marked with *d*) to undelete.');
    Exit;
  end;

  if not IsDel then
  begin
    ShowMessage('Selected item is not a deleted file. Only files marked with *d* can be undeleted.');
    Exit;
  end;

  CleanName := CleanFileName(SelectedItem);
  if CleanName.EndsWith('\') then
  begin
    ShowMessage('Undeleting directories is not supported.');
    Exit;
  end;

  // Prompt the user for the restored filename or first character
  NewFirstCharStr := InputBox('Undelete File',
    'The first character of deleted file "' + CleanName + '" is missing.' + sLineBreak +
    'Enter the restored filename or first character:', CleanName);
  NewFirstCharStr := Trim(NewFirstCharStr);
  if NewFirstCharStr = '' then Exit;

  if Length(NewFirstCharStr) = 1 then
    RestoredFileName := UpperCase(NewFirstCharStr) + Copy(CleanName, 2, Length(CleanName))
  else
    RestoredFileName := UpperCase(NewFirstCharStr);

  FirstChar := AnsiChar(RestoredFileName[1]);
  if (FirstChar = AnsiChar(#$E5)) or (FirstChar = ' ') or (FirstChar = '?') then
  begin
    ShowMessage('Invalid first character for filename.');
    Exit;
  end;

  ImageFile := TFileStream.Create(FImagePath, fmOpenReadWrite or fmShareDenyNone);
  try
    GetFATDetails(ImageFile);
    DirCluster := FindDirectoryInPath(ImageFile, FCurrentPath);

    // Locate the deleted directory entry in the image
    DirEntryPos := FindDeletedFileInDirectory(ImageFile, DirCluster, CleanName);
    if DirEntryPos = -1 then
    begin
      ShowMessage('Could not locate the deleted directory entry in the image.');
      Exit;
    end;

    // Read the 32-byte directory entry
    ImageFile.Position := DirEntryPos;
    ImageFile.Read(DirEntry, FAT12_DIR_ENTRY_SIZE);

    // Extract start cluster and file size
    Move(DirEntry[26], StartCluster, 2);
    FileSize := DirEntry[28] or
                (Int64(DirEntry[29]) shl 8) or
                (Int64(DirEntry[30]) shl 16) or
                (Int64(DirEntry[31]) shl 24);

    if FBytesPerCluster > 0 then
      ClustersNeeded := (FileSize + FBytesPerCluster - 1) div FBytesPerCluster
    else
      ClustersNeeded := 0;

    // Check if clusters are available or were overwritten
    OverwriteWarning := False;
    if (ClustersNeeded > 0) and (StartCluster >= 2) and (StartCluster <= FTotalClusters + 1) then
    begin
      for I := 0 to ClustersNeeded - 1 do
      begin
        CurrCluster := StartCluster + I;
        if CurrCluster > FTotalClusters + 1 then
        begin
          OverwriteWarning := True;
          Break;
        end;
        NextVal := ReadFAT12Entry(ImageFile, CurrCluster);
        if (NextVal <> FAT12_FREE) and (NextVal <> CurrCluster + 1) and
           (not ((I = ClustersNeeded - 1) and (NextVal >= $FF8))) then
        begin
          OverwriteWarning := True;
          Break;
        end;
      end;

      if OverwriteWarning then
      begin
        if MessageDlg('Cluster Conflict',
          'Warning: Some disk clusters formerly belonging to this file may have been reallocated.' + sLineBreak +
          'Restoring this file may result in corrupted or conflicting data.' + sLineBreak +
          'Do you want to proceed with undeleting anyway?',
          mtWarning, [mbYes, mbNo], 0) <> mrYes then
        begin
          Exit;
        end;
      end;

      // Re-link clusters in the FAT table
      for I := 0 to ClustersNeeded - 1 do
      begin
        CurrCluster := StartCluster + I;
        if CurrCluster <= FTotalClusters + 1 then
        begin
          if I < ClustersNeeded - 1 then
            WriteFAT12EntryAllCopies(ImageFile, CurrCluster, CurrCluster + 1)
          else
            WriteFAT12EntryAllCopies(ImageFile, CurrCluster, FAT12_EOC);
        end;
      end;
    end;

    // Format new 8.3 filename into DirEntry
    FileNameOnly := RestoredFileName;
    FileExt := '';
    I := Pos('.', FileNameOnly);
    if I > 0 then
    begin
      FileExt := Copy(FileNameOnly, I + 1, Length(FileNameOnly));
      FileNameOnly := Copy(FileNameOnly, 1, I - 1);
    end;
    FillChar(DirEntry[0], FAT12_MAX_DIR_NAME_LEN, $20);
    FillChar(DirEntry[8], FAT12_MAX_EXT_LEN, $20);
    Move(PAnsiChar(FileNameOnly)^, DirEntry[0], Min(FAT12_MAX_DIR_NAME_LEN, Length(FileNameOnly)));
    Move(PAnsiChar(FileExt)^, DirEntry[8], Min(FAT12_MAX_EXT_LEN, Length(FileExt)));
    DirEntry[0] := Byte(FirstChar);

    // Write directory entry back to image
    ImageFile.Position := DirEntryPos;
    ImageFile.Write(DirEntry, FAT12_DIR_ENTRY_SIZE);
  finally
    ImageFile.Free;
  end;

  ShowMessage('File "' + RestoredFileName + '" has been successfully undeleted!');
  ReadSTImage(FImagePath, FCurrentPath);
  UpdateFileListDisplay;
  UpdateFreeSpaceDisplay;
  UpdateDeleteButtonState;
end;

{------------------------------------------------------------------
  Procedure: ListViewFilesClick
  Purpose:  Updates button captions when a file item is clicked
 ------------------------------------------------------------------}
procedure TForm1.ListViewFilesClick(Sender: TObject);
begin
  UpdateDeleteButtonState;
end;

{------------------------------------------------------------------
  Procedure: ListViewFilesSelectItem
  Purpose:  Updates button captions when selection changes (mouse/keys)
 ------------------------------------------------------------------}
procedure TForm1.ListViewFilesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  UpdateDeleteButtonState;
end;

{------------------------------------------------------------------
  Procedure: UpdateDeleteButtonState
  Purpose:  Toggles button caption between 'Delete' and 'Undelete'
            depending on whether the selected item is deleted or marked
 ------------------------------------------------------------------}
procedure TForm1.UpdateDeleteButtonState;
var
  RawName, CleanName: string;
  IsDir, IsStaged, IsDel, IsMarkedDel: Boolean;
begin
  if GetSelectedFileName(RawName, CleanName, IsDir, IsStaged, IsDel, IsMarkedDel) then
  begin
    if IsDel or IsMarkedDel then
      ButtonRemoveFile.Caption := 'Undelete'
    else
      ButtonRemoveFile.Caption := 'Delete';
  end
  else
    ButtonRemoveFile.Caption := 'Delete';
end;

{------------------------------------------------------------------
  Procedure: FormCreate
  Purpose:  Event handler for form creation
            Initializes all string lists and sets dialog titles
 ------------------------------------------------------------------}
procedure TForm1.FormCreate(Sender: TObject);
begin
  AllowDropFiles := True;
  {$IFDEF WINDOWS}
  DragAcceptFiles(Handle, True);
  DragAcceptFiles(ListViewFiles.Handle, True);
  {$ENDIF}
  FFilePaths := TStringList.Create;
  FFileDestDirs := TStringList.Create;
  FFilesToDelete := TStringList.Create;
  FDisplayItems := TStringList.Create;
  FNewDirectories := TStringList.Create;
  FDirsToDelete := TStringList.Create;
  SetLength(FDiskItems, 0);
  SetLength(FDiskItemsRaw, 0);
  FImagePath := '';
  FCurrentPath := '';
  FVolumeLabel := '';
  FDiskSerial := 0;
  FIsMSAImage := False;
  FOriginalMSAPath := '';
  FTempSTPath := '';
  FTempZipExtractedPath := '';
  FOriginalZipPath := '';
  FDragPending := False;
  FDragActive := False;
  FDragSourcePath := '';
  OpenDialog.Title := 'Select Files or Image';
  SelectDirectoryDialog.Title := 'Select Folder to Add (Recursive)';
  SaveDialog.Title := 'Save Image As...';
  UpdateDeleteButtonState;
end;

{------------------------------------------------------------------
  Procedure: FormShow
  Purpose:  Event handler when the form is shown
            Ensures drag-and-drop handles are registered with the OS
 ------------------------------------------------------------------}
procedure TForm1.FormShow(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  DragAcceptFiles(Handle, True);
  DragAcceptFiles(ListViewFiles.Handle, True);
  {$ENDIF}
  if (ParamCount >= 1) and FileExists(ParamStr(1)) and (FImagePath = '') then
    LoadDiskImage(ParamStr(1));
end;

{------------------------------------------------------------------
  Procedure: FormDestroy
  Purpose:  Event handler for form destruction
            Frees all allocated resources
 ------------------------------------------------------------------}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  if (FTempSTPath <> '') and FileExists(FTempSTPath) then
    SysUtils.DeleteFile(FTempSTPath);
  if (FTempZipExtractedPath <> '') and FileExists(FTempZipExtractedPath) then
  begin
    SysUtils.DeleteFile(FTempZipExtractedPath);
    RemoveDir(ExtractFilePath(FTempZipExtractedPath));
  end;
  FFilePaths.Free;
  FFileDestDirs.Free;
  FFilesToDelete.Free;
  FDisplayItems.Free;
  FNewDirectories.Free;
  FDirsToDelete.Free;
end;

{------------------------------------------------------------------
  Procedure: FormDropFiles
  Purpose:  Event handler for dragging and dropping files or folders
            from the host operating system into STIMAGE
 ------------------------------------------------------------------}
procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  I: Integer;
  ItemPath, Ext, TargetName: string;
begin
  if Length(FileNames) = 0 then Exit;
  if FDragActive then Exit; // Prevent drop when dragging out from the tool

  // Case 1: If a single .st, .msa, or .zip disk image is dropped, open it
  if (Length(FileNames) = 1) then
  begin
    ItemPath := FileNames[0];
    Ext := LowerCase(ExtractFileExt(ItemPath));
    if (Ext = '.st') or (Ext = '.msa') or (Ext = '.zip') then
    begin
      if (FFilePaths.Count > 0) or (FNewDirectories.Count > 0) or (FFilesToDelete.Count > 0) or (FDirsToDelete.Count > 0) then
      begin
        if MessageDlg('Open Disk Image',
          'You have pending changes staged. Opening a new image will discard them.' + sLineBreak +
          'Do you want to discard changes and open ' + ExtractFileName(ItemPath) + '?',
          mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
          Exit;
      end;
      LoadDiskImage(ItemPath);
      Exit;
    end;
  end;

  // Case 2: Process dropped files and folders
  for I := 0 to High(FileNames) do
  begin
    ItemPath := FileNames[I];
    if Trim(ItemPath) = '' then Continue;

    // Ignore files originating from our own drag-out
    if (FDragSourcePath <> '') and SameText(ExpandFileName(ItemPath), FDragSourcePath) then
      Continue;
    if Pos('stimage_drag', LowerCase(ItemPath)) > 0 then
      Continue;

    if DirectoryExists(ItemPath) then
    begin
      // Dropped a folder: recursively add directory tree into FCurrentPath
      TargetName := SanitizeDOSName(ExtractFileName(ExcludeTrailingPathDelimiter(ItemPath)), True);
      if TargetName <> '' then
        ScanDirectoryRecursive(ItemPath, FCurrentPath + TargetName + '\');
    end
    else if FileExists(ItemPath) then
    begin
      // Dropped a file: stage into FCurrentPath
      FFilePaths.Add(ItemPath);
      FFileDestDirs.Add(FCurrentPath);
    end;
  end;

  UpdateFileListDisplay;
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
  if (FImagePath <> '') and FileExists(FImagePath) then
    ReadSTImage(FImagePath, FCurrentPath)
  else
    FDisplayItems.Clear;
  UpdateFileListDisplay;
end;

{------------------------------------------------------------------
  Procedure: ListViewFilesDblClick
  Purpose:  Event handler for double-clicking a file in the list
            Navigates into directories or goes back if ".." is clicked
 ------------------------------------------------------------------}
procedure TForm1.ListViewFilesDblClick(Sender: TObject);
var
  RawItem, SelectedItem: string;
  IsDir, IsStaged, IsDel, IsMarkedDel: Boolean;
begin
  if not GetSelectedFileName(RawItem, SelectedItem, IsDir, IsStaged, IsDel, IsMarkedDel) then Exit;

  if (SelectedItem = '..') or (SelectedItem = '..\') then
    ButtonBackClick(nil)
  else if IsDir or EndsText('\', SelectedItem) then
  begin
    if IsDel then
    begin
      ShowMessage('Cannot enter a deleted directory.');
      Exit;
    end;
    if not SelectedItem.EndsWith('\') then
      SelectedItem := SelectedItem + '\';
    FCurrentPath := FCurrentPath + SelectedItem;
    if (FImagePath <> '') and FileExists(FImagePath) then
      ReadSTImage(FImagePath, FCurrentPath)
    else
      FDisplayItems.Clear;
    UpdateFileListDisplay;
  end
  else
  begin
    // Double-clicking a file opens it in the built-in File Viewer / Hex Inspector
    ButtonViewFileClick(Sender);
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
  CleanName, ChildName, HostFile: string;
  Info: TDiskItemInfo;
begin
  ListViewFiles.Items.BeginUpdate;
  try
    ListViewFiles.Items.Clear;
    SetLength(FDiskItems, 0);

    // 1. Parent directory link '..' in subdirectories
    if FCurrentPath <> '' then
    begin
      FillChar(Info, SizeOf(Info), 0);
      Info.DisplayName := '..';
      Info.CleanName := '..';
      Info.IsDir := True;
      Info.SizeStr := '<DIR>';
      Info.ClusterStr := '-';
      Info.DateTimeStr := '-';
      Info.AttrStr := 'D----';
      Info.StatusStr := 'Parent';
      SetLength(FDiskItems, Length(FDiskItems) + 1);
      FDiskItems[High(FDiskItems)] := Info;
      AddDiskItemToListView(Info);
    end;

    // 2. Entries on disk (from FDiskItemsRaw)
    for I := 0 to High(FDiskItemsRaw) do
    begin
      Info := FDiskItemsRaw[I];
      CleanName := Info.CleanName;
      if (FFilesToDelete.IndexOf(CleanName) <> -1) or
         (FDirsToDelete.IndexOf(StringReplace(CleanName, '\', '', [rfReplaceAll])) <> -1) then
      begin
        Info.IsMarkedDelete := True;
        Info.StatusStr := 'Marked [D]';
        Info.DisplayName := CleanName + ' *D*';
      end
      else if Info.IsDeleted then
      begin
        Info.StatusStr := 'Deleted [d]';
        Info.DisplayName := CleanName + ' *d*';
      end
      else if Info.IsHidden then
      begin
        Info.StatusStr := 'Hidden';
        Info.DisplayName := CleanName + ' *h*';
      end
      else
      begin
        Info.StatusStr := 'OK';
        Info.DisplayName := CleanName;
      end;
      SetLength(FDiskItems, Length(FDiskItems) + 1);
      FDiskItems[High(FDiskItems)] := Info;
      AddDiskItemToListView(Info);
    end;

    // 3. Staged directory additions (*A*)
    for I := 0 to FNewDirectories.Count - 1 do
    begin
      if IsDirectChild(FCurrentPath, FNewDirectories[I], ChildName) then
      begin
        FillChar(Info, SizeOf(Info), 0);
        Info.CleanName := ChildName + '\';
        Info.DisplayName := Info.CleanName + ' *A*';
        Info.IsDir := True;
        Info.IsStagedAdd := True;
        Info.SizeStr := '<DIR>';
        Info.ClusterStr := 'New';
        Info.DateTimeStr := FormatDateTime('yyyy-mm-dd hh:nn', Now);
        Info.AttrStr := 'D-A--';
        Info.StatusStr := 'Staged [A]';
        SetLength(FDiskItems, Length(FDiskItems) + 1);
        FDiskItems[High(FDiskItems)] := Info;
        AddDiskItemToListView(Info);
      end;
    end;

    // 4. Staged file additions (*A*)
    for I := 0 to FFilePaths.Count - 1 do
    begin
      if (I < FFileDestDirs.Count) and SameText(FFileDestDirs[I], FCurrentPath) then
      begin
        HostFile := FFilePaths[I];
        FillChar(Info, SizeOf(Info), 0);
        Info.CleanName := UpperCase(ExtractFileName(HostFile));
        Info.DisplayName := Info.CleanName + ' *A*';
        Info.IsDir := False;
        Info.IsStagedAdd := True;
        if FileExists(HostFile) then
        begin
          Info.Size := FileUtil.FileSize(HostFile);
          Info.SizeStr := FormatSizeStr(Info.Size, False);
          Info.DateTimeStr := FormatDateTime('yyyy-mm-dd hh:nn', FileDateToDateTime(FileAge(HostFile)));
        end
        else
        begin
          Info.Size := 0;
          Info.SizeStr := '0 B';
          Info.DateTimeStr := '-';
        end;
        Info.ClusterStr := 'New';
        Info.AttrStr := '-A---';
        Info.StatusStr := 'Staged [A]';
        SetLength(FDiskItems, Length(FDiskItems) + 1);
        FDiskItems[High(FDiskItems)] := Info;
        AddDiskItemToListView(Info);
      end;
    end;
  finally
    ListViewFiles.Items.EndUpdate;
  end;

  // Update current path and volume label
  LabelCurrentPath.Caption := 'Current Path: ' + FCurrentPath;
  if FVolumeLabel <> '' then
    LabelVolume.Caption := 'Volume: ' + FVolumeLabel
  else
    LabelVolume.Caption := 'Volume: [None]';
  UpdateDeleteButtonState;
end;

{------------------------------------------------------------------
  Procedure: ClearFileList
  Purpose:  Clears the file list display
 ------------------------------------------------------------------}
procedure TForm1.ClearFileList;
begin
  ListViewFiles.Items.Clear;
  SetLength(FDiskItems, 0);
  SetLength(FDiskItemsRaw, 0);
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
  FileName, ItemName: string;
  EntryIsDir, EntryIsDel, EntryIsHid: Boolean;
  EntryStartClust: Word;
  EntrySize: Int64;
  EntrySizeStr, EntryDateStr, EntryAttrStr: string;
  J: Integer;
begin
  FDisplayItems.Clear;
  SetLength(FDiskItemsRaw, 0);
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
        // If it's a pending staged directory, don't show an error
        if FNewDirectories.IndexOf(APath) <> -1 then
        begin
          FDisplayItems.Clear;
          Exit;
        end;
        ShowMessage(Format(ERR_DIR_NOT_FOUND, [APath]));
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

        // Skip volume labels in file list, but store label if in root directory
        if (DirEntry[11] and $08) <> 0 then
        begin
          if DirStartCluster = 0 then
          begin
            SetLength(FileName, 11);
            Move(DirEntry[0], FileName[1], 11);
            FVolumeLabel := Trim(FileName);
          end;
          Continue;
        end;

        FileName := ExtractFileNameFromEntry(DirEntry);
        if IsDirectory(DirEntry) and ((FileName = '.') or (FileName = '..')) then
          Continue;

        EntryIsDir := IsDirectory(DirEntry);
        EntryIsDel := IsDeleted(DirEntry);
        EntryIsHid := IsHidden(DirEntry);

        ItemName := FileName;
        if EntryIsDir then
          ItemName := ItemName + '\';

        if EntryIsDel then
          ItemName := ItemName + ' *d*';
        if EntryIsHid then
          ItemName := ItemName + ' *h*';

        FDisplayItems.Add(ItemName);

        EntryStartClust := Word(DirEntry[$1A]) or (Word(DirEntry[$1B]) shl 8);
        if EntryIsDir then
        begin
          EntrySize := 0;
          EntrySizeStr := '<DIR>';
        end
        else
        begin
          EntrySize := Int64(DirEntry[$1C]) or (Int64(DirEntry[$1D]) shl 8) or
                       (Int64(DirEntry[$1E]) shl 16) or (Int64(DirEntry[$1F]) shl 24);
          EntrySizeStr := FormatSizeStr(EntrySize, False);
        end;
        EntryDateStr := FormatFATDateTime(Word(DirEntry[$18]) or (Word(DirEntry[$19]) shl 8),
                                          Word(DirEntry[$16]) or (Word(DirEntry[$17]) shl 8));
        EntryAttrStr := FormatFATAttr(DirEntry[$0B]);

        SetLength(FDiskItemsRaw, Length(FDiskItemsRaw) + 1);
        FDiskItemsRaw[High(FDiskItemsRaw)].CleanName := FileName;
        if EntryIsDir then
          FDiskItemsRaw[High(FDiskItemsRaw)].CleanName := FDiskItemsRaw[High(FDiskItemsRaw)].CleanName + '\';
        FDiskItemsRaw[High(FDiskItemsRaw)].DisplayName := ItemName;
        FDiskItemsRaw[High(FDiskItemsRaw)].IsDir := EntryIsDir;
        FDiskItemsRaw[High(FDiskItemsRaw)].IsDeleted := EntryIsDel;
        FDiskItemsRaw[High(FDiskItemsRaw)].IsHidden := EntryIsHid;
        FDiskItemsRaw[High(FDiskItemsRaw)].IsStagedAdd := False;
        FDiskItemsRaw[High(FDiskItemsRaw)].IsMarkedDelete := False;
        FDiskItemsRaw[High(FDiskItemsRaw)].StartCluster := EntryStartClust;
        FDiskItemsRaw[High(FDiskItemsRaw)].Size := EntrySize;
        FDiskItemsRaw[High(FDiskItemsRaw)].SizeStr := EntrySizeStr;
        FDiskItemsRaw[High(FDiskItemsRaw)].ClusterStr := IntToStr(EntryStartClust);
        FDiskItemsRaw[High(FDiskItemsRaw)].DateTimeStr := EntryDateStr;
        FDiskItemsRaw[High(FDiskItemsRaw)].AttrStr := EntryAttrStr;
        FDiskItemsRaw[High(FDiskItemsRaw)].StatusStr := 'OK';
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
  Function: FindDeletedFileInDirectory
  Purpose:  Finds a deleted file in a directory and returns its entry position
 ------------------------------------------------------------------}
function TForm1.FindDeletedFileInDirectory(const AImageFile: TFileStream; const ADirCluster: Int64; const AFileName: string): Int64;
var
  CurrentCluster: Int64;
  DirEntry: array[0..FAT12_DIR_ENTRY_SIZE - 1] of Byte;
  EntryName, SearchName: string;
begin
  Result := -1;
  CurrentCluster := ADirCluster;
  SearchName := UpperCase(AFileName);
  while True do
  begin
    if CurrentCluster = 0 then
      AImageFile.Position := FRootDirStart
    else
      AImageFile.Position := FDataAreaStart + (CurrentCluster - 2) * FBytesPerCluster;

    while True do
    begin
      if AImageFile.Read(DirEntry, FAT12_DIR_ENTRY_SIZE) <> FAT12_DIR_ENTRY_SIZE then Break;
      if IsEndOfDirectory(DirEntry) then Break;
      if IsDeleted(DirEntry) then
      begin
        EntryName := UpperCase(ExtractFileNameFromEntry(DirEntry));
        if (EntryName = SearchName) or
           ((Length(EntryName) > 1) and (Length(SearchName) > 1) and
            (Copy(EntryName, 2, Length(EntryName)) = Copy(SearchName, 2, Length(SearchName)))) then
        begin
          Result := AImageFile.Position - FAT12_DIR_ENTRY_SIZE;
          Exit;
        end;
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
  CurrentCluster, LastCluster: Int64;
  DirEntry: array[0..FAT12_DIR_ENTRY_SIZE - 1] of Byte;
  FreeList: TStringList;
  NewSubCluster: Word;
  ClusterBuffer: array of Byte;
begin
  Result := -1;
  CurrentCluster := ADirCluster;
  LastCluster := ADirCluster;

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

    if ADirCluster = 0 then Break; // Root directory has fixed size, cannot grow
    LastCluster := CurrentCluster;
    CurrentCluster := GetNextCluster(AImageFile, CurrentCluster);
    if CurrentCluster >= $FF8 then Break;
  end;

  // If a subdirectory ran out of entries, allocate a new cluster to expand it
  if (ADirCluster <> 0) and (LastCluster > 1) then
  begin
    FreeList := FindFreeClusters(AImageFile, 1);
    try
      if FreeList.Count > 0 then
      begin
        NewSubCluster := StrToInt(FreeList[0]);
        // Extend directory cluster chain
        WriteFAT12EntryAllCopies(AImageFile, LastCluster, NewSubCluster);
        WriteFAT12EntryAllCopies(AImageFile, NewSubCluster, FAT12_EOC);

        // Zero out the newly allocated directory cluster
        SetLength(ClusterBuffer, FBytesPerCluster);
        FillChar(ClusterBuffer[0], FBytesPerCluster, 0);
        AImageFile.Position := FDataAreaStart + (NewSubCluster - 2) * FBytesPerCluster;
        AImageFile.Write(ClusterBuffer[0], FBytesPerCluster);

        // First 32-byte slot in this new cluster is our free directory entry
        Result := FDataAreaStart + (NewSubCluster - 2) * FBytesPerCluster;
      end;
    finally
      FreeList.Free;
    end;
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
  MaxCluster: Int64;
begin
  Result := TStringList.Create;
  CurrentCluster := 2; // First allocatable cluster
  FreeClusterCount := 0;

  if FTotalClusters > 0 then
    MaxCluster := FTotalClusters + 1
  else if FSingleFATSize > 3 then
    MaxCluster := (FSingleFATSize * 2 div 3) - 1
  else
    MaxCluster := $FF7;

  while (FreeClusterCount < ACount) and (CurrentCluster <= MaxCluster) do
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
    AImageFile  - The image file stream
    AFileName   - Name of the file to write
    ASourceFile - Source file stream
    ADestPath   - Destination directory path within the image
 ------------------------------------------------------------------}
procedure TForm1.WriteFileToImage(const AImageFile: TFileStream; const AFileName: string; const ASourceFile: TFileStream; const ADestPath: string);
var
  DirEntryPos, DirCluster: Int64;
  FileClusters: TStringList;
  FileSizeInClusters: Int64;
  I, DotPos: Integer;
  Buffer: array[0..4095] of Byte;
  BytesRead: Int64;
  DirEntry: array[0..FAT12_DIR_ENTRY_SIZE - 1] of Byte;
  Sanitized, FileNameOnly, FileExt: string;
  StartCluster: Word;
  FileTime, FileDate: Word;
begin
  // Calculate how many clusters are needed
  FileSizeInClusters := ASourceFile.Size div FBytesPerCluster;
  if (ASourceFile.Size mod FBytesPerCluster) <> 0 then Inc(FileSizeInClusters);

  // Find the directory to write to
  DirCluster := FindDirectoryInPath(AImageFile, ADestPath);
  if DirCluster = -1 then
  begin
    ShowMessage(Format(ERR_DIR_NOT_FOUND, [ADestPath]));
    Exit;
  end;

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

  // Update FAT chain in all copies
  for I := 0 to FileClusters.Count - 1 do
  begin
    if I < FileClusters.Count - 1 then
      WriteFAT12EntryAllCopies(AImageFile, StrToInt(FileClusters[I]), StrToInt(FileClusters[I + 1]))
    else
      WriteFAT12EntryAllCopies(AImageFile, StrToInt(FileClusters[I]), FAT12_EOC);
  end;

  // Create directory entry
  FillChar(DirEntry, SizeOf(DirEntry), 0);
  Sanitized := SanitizeDOSName(ExtractFileName(AFileName), False);
  DotPos := Pos('.', Sanitized);
  if DotPos > 0 then
  begin
    FileNameOnly := Copy(Sanitized, 1, DotPos - 1);
    FileExt := Copy(Sanitized, DotPos + 1, Length(Sanitized));
  end
  else
  begin
    FileNameOnly := Sanitized;
    FileExt := '';
  end;

  // Set filename and extension (padded with spaces)
  FillChar(DirEntry[0], FAT12_MAX_DIR_NAME_LEN, $20);
  FillChar(DirEntry[8], FAT12_MAX_EXT_LEN, $20);
  Move(PAnsiChar(FileNameOnly)^, DirEntry[0], Min(FAT12_MAX_DIR_NAME_LEN, Length(FileNameOnly)));
  Move(PAnsiChar(FileExt)^, DirEntry[8], Min(FAT12_MAX_EXT_LEN, Length(FileExt)));

  // Set file attributes and timestamps
  DirEntry[11] := $20; // Archive bit
  Move(ASourceFile.Size, DirEntry[28], 4); // File size
  if FileClusters.Count > 0 then
    StartCluster := StrToInt(FileClusters[0])
  else
    StartCluster := 0;
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
  Curr, Next2: Word;
  MaxLoops: Integer;
begin
  StartCluster := 0;
  // Read the directory entry
  AImageFile.Position := ADirEntryPos;
  AImageFile.Read(DirEntry, FAT12_DIR_ENTRY_SIZE);

  // Extract starting cluster BEFORE modifying or clearing DirEntry
  Move(DirEntry[26], StartCluster, 2);
  Curr := StartCluster;

  // Clear the FAT chain for the file in all FAT copies
  MaxLoops := 0;
  while (Curr >= 2) and (Curr < $FF8) and (MaxLoops < 8192) do
  begin
    Inc(MaxLoops);
    Next2 := ReadFAT12Entry(AImageFile, Curr);
    WriteFAT12EntryAllCopies(AImageFile, Curr, FAT12_FREE);
    if (Next2 = Curr) or (Next2 = 0) then Break;
    Curr := Next2;
  end;

  // Mark the first byte as deleted in the directory entry
  // Note: We preserve starting cluster (byte 26) and file size (byte 28)
  // so that undelete utilities can reconstruct the file.
  DeletedMarker := FAT12_DELETED;
  AImageFile.Position := ADirEntryPos;
  AImageFile.Write(DeletedMarker, 1);
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
  DataSectors: Int64;
  MaxClustersInFile, MaxClustersInFAT: Int64;
begin
  AImageFile.Position := 0;
  AImageFile.Read(BootSector, FAT12_BOOT_SECTOR_SIZE);

  // Parse boot sector
  FBytesPerSector := Word(BootSector[$0B]) + (Word(BootSector[$0C]) shl 8);
  if FBytesPerSector = 0 then FBytesPerSector := 512;
  FSectorsPerCluster := BootSector[$0D];
  if FSectorsPerCluster = 0 then FSectorsPerCluster := 2;
  ReservedSectors := Word(BootSector[$0E]) + (Word(BootSector[$0F]) shl 8);
  FNumberOfFATs := BootSector[$10];
  RootEntries := Word(BootSector[$11]) + (Word(BootSector[$12]) shl 8);
  FRootEntries := RootEntries;
  TotalSectors := Word(BootSector[$13]) + (Word(BootSector[$14]) shl 8);
  if TotalSectors = 0 then
    TotalSectors := AImageFile.Size div FBytesPerSector;
  FSectorsPerFAT := Word(BootSector[$16]) + (Word(BootSector[$17]) shl 8);

  // Read Atari ST 24-bit disk serial number at offset $08..$0A of boot sector
  FDiskSerial := (LongWord(BootSector[8]) shl 16) or
                 (LongWord(BootSector[9]) shl 8) or
                 LongWord(BootSector[10]);

  // Calculate FAT and data area positions
  FSingleFATSize := FSectorsPerFAT * FBytesPerSector;
  FFatTableStart := ReservedSectors * FBytesPerSector;
  FFatTableSize := FNumberOfFATs * FSingleFATSize;
  FRootDirStart := (ReservedSectors + (FNumberOfFATs * FSectorsPerFAT)) * FBytesPerSector;

  RootDirSectors := ((RootEntries * FAT12_DIR_ENTRY_SIZE) + (FBytesPerSector - 1)) div FBytesPerSector;
  FDataAreaStart := FRootDirStart + (RootDirSectors * FBytesPerSector);
  FBytesPerCluster := FSectorsPerCluster * FBytesPerSector;

  // Calculate total data clusters dynamically from filesystem layout
  DataSectors := TotalSectors - (ReservedSectors + (FNumberOfFATs * FSectorsPerFAT) + RootDirSectors);
  if (DataSectors > 0) and (FSectorsPerCluster > 0) then
    FTotalClusters := DataSectors div FSectorsPerCluster
  else
    FTotalClusters := 0;

  // Cap by actual image file size if image is truncated or non-standard
  if (FBytesPerCluster > 0) and (AImageFile.Size > FDataAreaStart) then
  begin
    MaxClustersInFile := (AImageFile.Size - FDataAreaStart) div FBytesPerCluster;
    if (MaxClustersInFile > 0) and (MaxClustersInFile < FTotalClusters) then
      FTotalClusters := MaxClustersInFile;
  end;

  // Cap by FAT table capacity (FAT12 uses 1.5 bytes per cluster entry)
  if FSingleFATSize > 3 then
  begin
    MaxClustersInFAT := (FSingleFATSize * 2 div 3) - 2;
    if (MaxClustersInFAT > 0) and (MaxClustersInFAT < FTotalClusters) then
      FTotalClusters := MaxClustersInFAT;
  end;
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
begin
  FreeClusters := 0;
  if FTotalClusters = 0 then
    GetFATDetails(AImageFile);

  // Iterate through all valid data clusters
  for CurrentCluster := 2 to FTotalClusters + 1 do
  begin
    Val := ReadFAT12Entry(AImageFile, CurrentCluster);
    if Val = FAT12_FREE then
      Inc(FreeClusters);
  end;

  Result := FreeClusters;
end;

{------------------------------------------------------------------
  Procedure: UpdateFreeSpaceDisplay
  Purpose:  Updates the free space display in the UI dynamically
            from FAT cluster analysis.
 ------------------------------------------------------------------}
procedure TForm1.UpdateFreeSpaceDisplay;
var
  ImageFile: TFileStream;
  FreeClusters: Word;
  UtilizedPercent: Double;
begin
  if FImagePath = '' then Exit;

  ImageFile := TFileStream.Create(FImagePath, fmOpenRead or fmShareDenyNone);
  try
    GetFATDetails(ImageFile);
    FreeClusters := GetFreeSpace(ImageFile);

    // Calculate utilized space percentage
    if FTotalClusters > 0 then
    begin
      UtilizedPercent := 100.0 - (Double(FreeClusters) / Double(FTotalClusters)) * 100.0;
      if UtilizedPercent < 0 then UtilizedPercent := 0;
      if UtilizedPercent > 100 then UtilizedPercent := 100;
      ProgressBarFreeSpace.Position := Round(UtilizedPercent);
      LabelFreeSpace.Caption := Format('Utilized space: %.1f%% (%d/%d clusters)',
        [UtilizedPercent, FTotalClusters - FreeClusters, FTotalClusters]);
    end
    else
    begin
      ProgressBarFreeSpace.Position := 0;
      LabelFreeSpace.Caption := 'Utilized space: 0%';
    end;
  finally
    ImageFile.Free;
  end;
end;


{------------------------------------------------------------------
  Function: LoadDiskImage
  Purpose:  Loads a disk image (.st or .msa), handling MSA decompression if needed
 ------------------------------------------------------------------}
function TForm1.LoadDiskImage(const AFileName: string): Boolean;
var
  Ext: string;
  TempFile: string;
begin
  Result := False;
  if not FileExists(AFileName) then Exit;

  Ext := LowerCase(ExtractFileExt(AFileName));
  if Ext = '.zip' then
  begin
    Result := LoadZipArchive(AFileName);
    Exit;
  end;

  // Clean up any old temporary ST file
  if (FTempSTPath <> '') and FileExists(FTempSTPath) then
  begin
    SysUtils.DeleteFile(FTempSTPath);
    FTempSTPath := '';
  end;

  // Clean up old temporary file from ZIP if loading a different file
  if (FTempZipExtractedPath <> '') and (AFileName <> FTempZipExtractedPath) and FileExists(FTempZipExtractedPath) then
  begin
    SysUtils.DeleteFile(FTempZipExtractedPath);
    RemoveDir(ExtractFilePath(FTempZipExtractedPath));
    FTempZipExtractedPath := '';
    FOriginalZipPath := '';
  end;

  ResetState;

  if Ext = '.msa' then
  begin
    TempFile := GetTempFile(GetTempDir, 'st-msa-') + '.st';
    if not UnpackMSAToST(AFileName, TempFile) then
    begin
      ShowMessage('Error: Failed to unpack MSA disk image.' + sLineBreak +
        'Please verify that the file is a valid Magic Shadow Archiver image.');
      Exit;
    end;
    FIsMSAImage := True;
    FOriginalMSAPath := AFileName;
    FTempSTPath := TempFile;
    FImagePath := TempFile;
  end
  else
  begin
    FIsMSAImage := False;
    FOriginalMSAPath := '';
    FTempSTPath := '';
    FImagePath := AFileName;
  end;

  ReadSTImage(FImagePath, FCurrentPath);
  UpdateFileListDisplay;
  UpdateFreeSpaceDisplay;

  // Update window caption to reflect loaded file
  if FOriginalZipPath <> '' then
    Self.Caption := 'Atari .ST Image File Manager - ' + ExtractFileName(FOriginalZipPath) + ' [' + ExtractFileName(FTempZipExtractedPath) + ']'
  else if FIsMSAImage and (FOriginalMSAPath <> '') then
    Self.Caption := 'Atari .ST Image File Manager - ' + ExtractFileName(FOriginalMSAPath)
  else
    Self.Caption := 'Atari .ST Image File Manager - ' + ExtractFileName(FImagePath);

  Result := True;
end;

{------------------------------------------------------------------
  Function: ExtractFileToStream
  Purpose:  Extracts a file from the disk image into a stream
 ------------------------------------------------------------------}
function TForm1.ExtractFileToStream(const AFileName: string; AOutStream: TStream; out AStartCluster: Word): Boolean;
var
  CleanName: string;
  ImageFile: TFileStream;
  DirCluster, DirEntryPos: Int64;
  DirEntry: array[0..FAT12_DIR_ENTRY_SIZE - 1] of Byte;
  FileSize, BytesToRead, BytesRead: Int64;
  CurrCluster: Word;
  Buffer: array[0..4095] of Byte;
begin
  Result := False;
  AStartCluster := 0;
  if (FImagePath = '') or not FileExists(FImagePath) then Exit;

  CleanName := CleanFileName(AFileName);
  ImageFile := TFileStream.Create(FImagePath, fmOpenRead or fmShareDenyNone);
  try
    GetFATDetails(ImageFile);
    if FCurrentPath = '' then
      DirCluster := 0
    else
      DirCluster := FindDirectoryInPath(ImageFile, FCurrentPath);
    if DirCluster = -1 then Exit;

    DirEntryPos := FindFileInDirectory(ImageFile, DirCluster, CleanName);
    if DirEntryPos = -1 then
      DirEntryPos := FindDeletedFileInDirectory(ImageFile, DirCluster, CleanName);
    if DirEntryPos = -1 then Exit;

    ImageFile.Position := DirEntryPos;
    ImageFile.Read(DirEntry, FAT12_DIR_ENTRY_SIZE);
    FileSize := DirEntry[28] or
                (Int64(DirEntry[29]) shl 8) or
                (Int64(DirEntry[30]) shl 16) or
                (Int64(DirEntry[31]) shl 24);
    AStartCluster := DirEntry[26] or (DirEntry[27] shl 8);

    if FileSize = 0 then
    begin
      Result := True;
      Exit;
    end;

    CurrCluster := AStartCluster;
    while (CurrCluster >= 2) and (CurrCluster < $FF8) and (FileSize > 0) do
    begin
      BytesToRead := FBytesPerCluster;
      if FileSize < FBytesPerCluster then
        BytesToRead := FileSize;
      ImageFile.Position := FDataAreaStart + (CurrCluster - 2) * FBytesPerCluster;
      BytesRead := ImageFile.Read(Buffer[0], BytesToRead);
      if BytesRead > 0 then
        AOutStream.Write(Buffer[0], BytesRead);
      Dec(FileSize, BytesRead);
      CurrCluster := GetNextCluster(ImageFile, CurrCluster);
    end;
    Result := True;
  finally
    ImageFile.Free;
  end;
end;

{------------------------------------------------------------------
  Function: ExtractFileToTemp
  Purpose:  Extracts a file to a temporary staging folder for dragging out
 ------------------------------------------------------------------}
function TForm1.ExtractFileToTemp(const AFileName: string): string;
var
  CleanName, TempDir, OutPath: string;
  Fs, SrcFs: TFileStream;
  DummyCluster: Word;
  I: Integer;
begin
  Result := '';
  CleanName := CleanFileName(AFileName);
  if (CleanName = '..') or (CleanName = '..\') or CleanName.EndsWith('\') then Exit;

  TempDir := IncludeTrailingPathDelimiter(GetTempDir) + 'stimage_drag';
  ForceDirectories(TempDir);
  OutPath := TempDir + PathDelim + CleanName;
  if FileExists(OutPath) then SysUtils.DeleteFile(OutPath);

  // If file is staged (*A*), copy directly from host source file
  if AFileName.EndsWith(' *A*') then
  begin
    for I := 0 to FFilePaths.Count - 1 do
    begin
      if SameText(ExtractFileName(FFilePaths[I]), CleanName) and
         (I < FFileDestDirs.Count) and SameText(FFileDestDirs[I], FCurrentPath) then
      begin
        if FileExists(FFilePaths[I]) then
        begin
          Fs := TFileStream.Create(OutPath, fmCreate);
          try
            SrcFs := TFileStream.Create(FFilePaths[I], fmOpenRead or fmShareDenyNone);
            try
              Fs.CopyFrom(SrcFs, SrcFs.Size);
              Result := OutPath;
            finally
              SrcFs.Free;
            end;
          finally
            Fs.Free;
          end;
        end;
        Exit;
      end;
    end;
  end;

  Fs := TFileStream.Create(OutPath, fmCreate);
  try
    if ExtractFileToStream(AFileName, Fs, DummyCluster) then
      Result := OutPath;
  finally
    Fs.Free;
  end;
end;

{------------------------------------------------------------------
  Procedure: ExtractDiskDirectory
  Purpose:  Recursively extracts all files and folders from a disk directory
 ------------------------------------------------------------------}
procedure TForm1.ExtractDiskDirectory(const AImageFile: TFileStream; const ADirCluster: Int64; const AHostPath: string; var AFileCount: Integer; var AByteCount: Int64);
type
  TDiskEntryRec = record
    Name: string;
    IsDir: Boolean;
    StartCluster: Word;
    FileSize: Int64;
  end;
var
  Entries: array of TDiskEntryRec;
  Count: Integer;
  CurrentCluster: Int64;
  DirEntry: array[0..FAT12_DIR_ENTRY_SIZE - 1] of Byte;
  EntryName, SubHostDir, OutFilePath: string;
  I: Integer;
  OutFs: TFileStream;
  CurrFileCluster: Word;
  BytesToRead, BytesRead, FileBytesRemaining: Int64;
  Buffer: array[0..4095] of Byte;
begin
  ForceDirectories(AHostPath);
  Count := 0;
  SetLength(Entries, 0);

  CurrentCluster := ADirCluster;
  while True do
  begin
    if CurrentCluster = 0 then
      AImageFile.Position := FRootDirStart
    else
      AImageFile.Position := FDataAreaStart + (CurrentCluster - 2) * FBytesPerCluster;

    while True do
    begin
      if AImageFile.Read(DirEntry, FAT12_DIR_ENTRY_SIZE) <> FAT12_DIR_ENTRY_SIZE then Break;
      if IsEndOfDirectory(DirEntry) then Break;
      if IsDeleted(DirEntry) or ((DirEntry[11] and $08) <> 0) then Continue;

      EntryName := ExtractFileNameFromEntry(DirEntry);
      if (EntryName = '.') or (EntryName = '..') then Continue;

      SetLength(Entries, Count + 1);
      Entries[Count].Name := EntryName;
      Entries[Count].IsDir := IsDirectory(DirEntry);
      Entries[Count].StartCluster := DirEntry[26] or (DirEntry[27] shl 8);
      Entries[Count].FileSize := DirEntry[28] or
                                 (Int64(DirEntry[29]) shl 8) or
                                 (Int64(DirEntry[30]) shl 16) or
                                 (Int64(DirEntry[31]) shl 24);
      Inc(Count);
    end;

    if ADirCluster = 0 then Break;
    CurrentCluster := GetNextCluster(AImageFile, CurrentCluster);
    if CurrentCluster >= $FF8 then Break;
  end;

  for I := 0 to Count - 1 do
  begin
    if Entries[I].IsDir then
    begin
      SubHostDir := IncludeTrailingPathDelimiter(AHostPath) + Entries[I].Name;
      ExtractDiskDirectory(AImageFile, Entries[I].StartCluster, SubHostDir, AFileCount, AByteCount);
    end
    else
    begin
      OutFilePath := IncludeTrailingPathDelimiter(AHostPath) + Entries[I].Name;
      OutFs := TFileStream.Create(OutFilePath, fmCreate);
      try
        FileBytesRemaining := Entries[I].FileSize;
        CurrFileCluster := Entries[I].StartCluster;
        while (CurrFileCluster >= 2) and (CurrFileCluster < $FF8) and (FileBytesRemaining > 0) do
        begin
          BytesToRead := FBytesPerCluster;
          if FileBytesRemaining < FBytesPerCluster then
            BytesToRead := FileBytesRemaining;
          AImageFile.Position := FDataAreaStart + (CurrFileCluster - 2) * FBytesPerCluster;
          BytesRead := AImageFile.Read(Buffer[0], BytesToRead);
          if BytesRead > 0 then
            OutFs.Write(Buffer[0], BytesRead);
          Dec(FileBytesRemaining, BytesRead);
          CurrFileCluster := GetNextCluster(AImageFile, CurrFileCluster);
        end;
      finally
        OutFs.Free;
      end;
      Inc(AFileCount);
      Inc(AByteCount, Entries[I].FileSize);
    end;
  end;
end;

{------------------------------------------------------------------
  Procedure: ButtonExtractAllClick
  Purpose:  Event handler for "Extract All" / Export entire disk image
 ------------------------------------------------------------------}
{------------------------------------------------------------------
  Procedure: ZipListDblClick
  Purpose:  Event handler for double-clicking an image in the ZIP selector
 ------------------------------------------------------------------}
procedure TForm1.ZipListDblClick(Sender: TObject);
begin
  if (Sender is TListBox) then
  begin
    if (TListBox(Sender).ItemIndex >= 0) and (TListBox(Sender).Parent is TCustomForm) then
      TCustomForm(TListBox(Sender).Parent).ModalResult := mrOK;
  end;
end;

{------------------------------------------------------------------
  Procedure: ZipListKeyDown
  Purpose:  Event handler for keyboard interaction in the ZIP selector
 ------------------------------------------------------------------}
procedure TForm1.ZipListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Sender is TListBox) then
  begin
    if (TListBox(Sender).ItemIndex >= 0) and (TListBox(Sender).Parent is TCustomForm) then
    begin
      Key := 0;
      TCustomForm(TListBox(Sender).Parent).ModalResult := mrOK;
    end;
  end;
end;

{------------------------------------------------------------------
  Function: SelectDiskImageFromZip
  Purpose:  Displays a modal dialog allowing the user to select one of
            multiple disk images found in a ZIP archive
 ------------------------------------------------------------------}
function TForm1.SelectDiskImageFromZip(const AZipPath: string; MatchingEntries: TStrings): Integer;
var
  Dlg: TForm;
  LblPrompt: TLabel;
  ListBox: TListBox;
  BtnOk, BtnCancel: TButton;
  BtnPnl: TPanel;
  I: Integer;
begin
  Result := -1;
  Dlg := TForm.CreateNew(nil);
  try
    Dlg.Caption := 'Select Disk Image from ZIP Archive';
    Dlg.Position := poScreenCenter;
    Dlg.Width := 520;
    Dlg.Height := 360;
    Dlg.BorderStyle := bsDialog;
    Dlg.KeyPreview := True;

    LblPrompt := TLabel.Create(Dlg);
    LblPrompt.Parent := Dlg;
    LblPrompt.SetBounds(16, 12, 488, 30);
    LblPrompt.Caption := 'Multiple Atari ST disk images were found in "' + ExtractFileName(AZipPath) + '".' +
      sLineBreak + 'Please select which disk image you want to open:';

    BtnPnl := TPanel.Create(Dlg);
    BtnPnl.Parent := Dlg;
    BtnPnl.Align := alBottom;
    BtnPnl.Height := 46;
    BtnPnl.BevelOuter := bvNone;

    BtnCancel := TButton.Create(Dlg);
    BtnCancel.Parent := BtnPnl;
    BtnCancel.Caption := 'Cancel';
    BtnCancel.ModalResult := mrCancel;
    BtnCancel.Cancel := True;
    BtnCancel.SetBounds(BtnPnl.Width - 96, 10, 80, 26);
    BtnCancel.Anchors := [akRight, akBottom];

    BtnOk := TButton.Create(Dlg);
    BtnOk.Parent := BtnPnl;
    BtnOk.Caption := 'Open';
    BtnOk.ModalResult := mrOK;
    BtnOk.Default := True;
    BtnOk.SetBounds(BtnPnl.Width - 186, 10, 80, 26);
    BtnOk.Anchors := [akRight, akBottom];

    ListBox := TListBox.Create(Dlg);
    ListBox.Parent := Dlg;
    ListBox.SetBounds(16, 48, 488, Dlg.ClientHeight - 48 - BtnPnl.Height);
    ListBox.Anchors := [akTop, akLeft, akRight, akBottom];
    ListBox.TabOrder := 0;
    ListBox.OnDblClick := @ZipListDblClick;
    ListBox.OnKeyDown := @ZipListKeyDown;

    for I := 0 to MatchingEntries.Count - 1 do
      ListBox.Items.AddObject(MatchingEntries[I], MatchingEntries.Objects[I]);

    if ListBox.Items.Count > 0 then
      ListBox.ItemIndex := 0;

    if Dlg.ShowModal = mrOK then
    begin
      if (ListBox.ItemIndex >= 0) and (ListBox.ItemIndex < ListBox.Items.Count) then
        Result := Integer(PtrUInt(ListBox.Items.Objects[ListBox.ItemIndex]));
    end;
  finally
    Dlg.Free;
  end;
end;

{------------------------------------------------------------------
  Function: LoadZipArchive
  Purpose:  Inspects a ZIP archive for .st / .msa disk images.
            Auto-opens single image or prompts user if multiple exist.
 ------------------------------------------------------------------}
function TForm1.LoadZipArchive(const AZipPath: string): Boolean;
var
  UnZip: TUnZipper;
  I, ChosenEntryIndex: Integer;
  MatchingEntries: TStringList;
  EntryName, Ext, TempStageDir, ExtractedFilePath: string;
  EntrySizeKB: Int64;
begin
  Result := False;
  if not FileExists(AZipPath) then Exit;

  MatchingEntries := TStringList.Create;
  UnZip := TUnZipper.Create;
  try
    UnZip.FileName := AZipPath;
    try
      UnZip.Examine;
    except
      on E: Exception do
      begin
        ShowMessage('Error: Failed to read ZIP archive:' + sLineBreak + E.Message);
        Exit;
      end;
    end;

    for I := 0 to UnZip.Entries.Count - 1 do
    begin
      if UnZip.Entries[I].IsDirectory then Continue;
      EntryName := UnZip.Entries[I].ArchiveFileName;
      Ext := LowerCase(ExtractFileExt(EntryName));
      if (Ext = '.st') or (Ext = '.msa') then
      begin
        EntrySizeKB := (UnZip.Entries[I].Size + 1023) div 1024;
        MatchingEntries.AddObject(Format('%s (%d KB)', [EntryName, EntrySizeKB]), TObject(PtrUInt(I)));
      end;
    end;

    if MatchingEntries.Count = 0 then
    begin
      ShowMessage('No Atari ST disk images (.st, .msa) were found inside this ZIP archive:' + sLineBreak +
        ExtractFileName(AZipPath));
      Exit;
    end
    else if MatchingEntries.Count = 1 then
    begin
      ChosenEntryIndex := Integer(PtrUInt(MatchingEntries.Objects[0]));
    end
    else
    begin
      ChosenEntryIndex := SelectDiskImageFromZip(AZipPath, MatchingEntries);
      if ChosenEntryIndex < 0 then Exit; // User cancelled
    end;

    // Clean up previous temp zip image if any
    if (FTempZipExtractedPath <> '') and FileExists(FTempZipExtractedPath) then
    begin
      SysUtils.DeleteFile(FTempZipExtractedPath);
      RemoveDir(ExtractFilePath(FTempZipExtractedPath));
      FTempZipExtractedPath := '';
    end;

    // Extract chosen disk image entry into a dedicated temp directory
    TempStageDir := IncludeTrailingPathDelimiter(GetTempDir) + 'stimage_zip_' + FormatDateTime('yyyymmddhhnnsszzz', Now) + DirectorySeparator;
    ForceDirectories(TempStageDir);
    ExtractedFilePath := TempStageDir + ExtractFileName(UnZip.Entries[ChosenEntryIndex].ArchiveFileName);

    UnZip.OutputPath := TempStageDir;
    UnZip.Flat := True;
    try
      UnZip.UnZipFile(UnZip.Entries[ChosenEntryIndex].ArchiveFileName);
    except
      on E: Exception do
      begin
        ShowMessage('Error: Failed to extract disk image from ZIP:' + sLineBreak + E.Message);
        Exit;
      end;
    end;

    if not FileExists(ExtractedFilePath) then
    begin
      ShowMessage('Error: Extracted disk image not found at:' + sLineBreak + ExtractedFilePath);
      Exit;
    end;

    FTempZipExtractedPath := ExtractedFilePath;
    FOriginalZipPath := AZipPath;

    // Now load the extracted ST or MSA image
    Result := LoadDiskImage(ExtractedFilePath);
  finally
    UnZip.Free;
    MatchingEntries.Free;
  end;
end;

{------------------------------------------------------------------
  Procedure: AddFolderToZipper
  Purpose:  Recursively scans a local directory and adds all contained
            files to a TZipper instance with relative paths
 ------------------------------------------------------------------}
procedure TForm1.AddFolderToZipper(const ABaseDir, ARelPath: string; AZip: TZipper);
var
  SR: TSearchRec;
  DiskFile, ArchiveRel: string;
begin
  if FindFirst(IncludeTrailingPathDelimiter(ABaseDir + ARelPath) + '*', faAnyFile, SR) = 0 then
  try
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then Continue;
      if (SR.Attr and faDirectory) <> 0 then
      begin
        AddFolderToZipper(ABaseDir, ARelPath + SR.Name + '/', AZip);
      end
      else
      begin
        DiskFile := IncludeTrailingPathDelimiter(ABaseDir) + ARelPath + SR.Name;
        ArchiveRel := ARelPath + SR.Name;
        ArchiveRel := StringReplace(ArchiveRel, '\', '/', [rfReplaceAll]);
        AZip.Entries.AddFileEntry(DiskFile, ArchiveRel);
      end;
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
end;

{------------------------------------------------------------------
  Procedure: ButtonExtractAllClick
  Purpose:  Event handler for "Extract All" / Export entire disk image.
            Prompts user whether to export to a Folder or ZIP Archive.
 ------------------------------------------------------------------}
procedure TForm1.ButtonExtractAllClick(Sender: TObject);
var
  TargetDir, TargetZipPath, TempZipPath, TempStagingDir, DefZipName: string;
  ImageFile: TFileStream;
  FileCount: Integer;
  ByteCount: Int64;
  Choice: TModalResult;
  ZipArchive: TZipper;
  SrcStream, DstStream: TFileStream;
begin
  if (FImagePath = '') or not FileExists(FImagePath) then
  begin
    ShowMessage('Please open a disk image first.');
    Exit;
  end;

  Choice := QuestionDlg('Export All Files',
    'How would you like to export all files from the disk image?' + sLineBreak + sLineBreak +
    '• Folder: Extract all files and directories into a selected folder' + sLineBreak +
    '• ZIP Archive: Package all files and directories into a compressed .zip file',
    mtConfirmation,
    [mrYes, 'Folder...', mrNo, 'ZIP Archive...', mrCancel, 'Cancel'],
    0);

  if (Choice <> mrYes) and (Choice <> mrNo) then Exit;

  if Choice = mrYes then
  begin
    SelectDirectoryDialog.Title := 'Select Destination Directory to Extract Disk';
    if not SelectDirectoryDialog.Execute then Exit;
    TargetDir := SelectDirectoryDialog.FileName;
    if TargetDir = '' then Exit;

    FileCount := 0;
    ByteCount := 0;
    ImageFile := TFileStream.Create(FImagePath, fmOpenRead or fmShareDenyNone);
    try
      GetFATDetails(ImageFile);
      ExtractDiskDirectory(ImageFile, 0, TargetDir, FileCount, ByteCount);
      ShowMessage(Format('Disk extraction complete!' + sLineBreak +
        'Extracted %d file(s) (%d bytes) to:' + sLineBreak + '%s',
        [FileCount, ByteCount, TargetDir]));
    finally
      ImageFile.Free;
    end;
  end
  else if Choice = mrNo then
  begin
    if FOriginalZipPath <> '' then
      DefZipName := ChangeFileExt(ExtractFileName(FTempZipExtractedPath), '') + '_export.zip'
    else if FIsMSAImage and (FOriginalMSAPath <> '') then
      DefZipName := ChangeFileExt(ExtractFileName(FOriginalMSAPath), '') + '.zip'
    else if FImagePath <> '' then
      DefZipName := ChangeFileExt(ExtractFileName(FImagePath), '') + '.zip'
    else
      DefZipName := 'disk_export.zip';

    SaveDialog.Title := 'Export Disk Files to ZIP Archive';
    SaveDialog.Filter := 'ZIP Archive (*.zip)|*.zip|All Files (*.*)|*.*';
    SaveDialog.FileName := DefZipName;
    if not SaveDialog.Execute then Exit;
    TargetZipPath := SaveDialog.FileName;
    if TargetZipPath = '' then Exit;
    if LowerCase(ExtractFileExt(TargetZipPath)) <> '.zip' then
      TargetZipPath := TargetZipPath + '.zip';

    // Stage all files into a temporary directory in GetTempDir
    TempStagingDir := IncludeTrailingPathDelimiter(GetTempDir) + 'stimage_stage_' + FormatDateTime('yyyymmddhhnnsszzz', Now) + DirectorySeparator;
    TempZipPath := IncludeTrailingPathDelimiter(GetTempDir) + 'stimage_export_' + FormatDateTime('yyyymmddhhnnsszzz', Now) + '.zip';
    ForceDirectories(TempStagingDir);

    FileCount := 0;
    ByteCount := 0;
    ImageFile := TFileStream.Create(FImagePath, fmOpenRead or fmShareDenyNone);
    try
      GetFATDetails(ImageFile);
      ExtractDiskDirectory(ImageFile, 0, TempStagingDir, FileCount, ByteCount);
    finally
      ImageFile.Free;
    end;

    if FileCount = 0 then
    begin
      DeleteDirectory(TempStagingDir, False);
      ShowMessage('The disk image contains no files to export.');
      Exit;
    end;

    try
      ZipArchive := TZipper.Create;
      try
        ZipArchive.FileName := TempZipPath;
        AddFolderToZipper(TempStagingDir, '', ZipArchive);
        ZipArchive.ZipAllFiles;
      finally
        ZipArchive.Free;
      end;

      // Copy created temp zip to final destination (handles network shares / GVFS safely)
      if FileExists(TargetZipPath) then SysUtils.DeleteFile(TargetZipPath);
      if not CopyFile(TempZipPath, TargetZipPath, True) then
      begin
        SrcStream := TFileStream.Create(TempZipPath, fmOpenRead or fmShareDenyNone);
        try
          DstStream := TFileStream.Create(TargetZipPath, fmCreate);
          try
            DstStream.CopyFrom(SrcStream, 0);
          finally
            DstStream.Free;
          end;
        finally
          SrcStream.Free;
        end;
      end;

      ShowMessage(Format('Export to ZIP complete!' + sLineBreak +
        'Packaged %d file(s) (%d bytes uncompressed) into:' + sLineBreak + '%s',
        [FileCount, ByteCount, TargetZipPath]));
    finally
      if FileExists(TempZipPath) then SysUtils.DeleteFile(TempZipPath);
      DeleteDirectory(TempStagingDir, False);
    end;
  end;
end;

{------------------------------------------------------------------
  Procedure: WriteVolumeLabelToImage
  Purpose:  Writes or updates the Atari ST GEMDOS volume label in the Root Directory
 ------------------------------------------------------------------}
procedure TForm1.WriteVolumeLabelToImage(const AImageFile: TFileStream; const ALabel: string);
var
  DirEntry: array[0..FAT12_DIR_ENTRY_SIZE - 1] of Byte;
  I: Integer;
  CleanLabel: string;
  LabelPos: Int64;
  FoundExisting: Boolean;
  TimeWord, DateWord: Word;
begin
  GetFATDetails(AImageFile);
  CleanLabel := UpperCase(Trim(ALabel));
  if Length(CleanLabel) > 11 then
    CleanLabel := Copy(CleanLabel, 1, 11);

  LabelPos := -1;
  FoundExisting := False;

  // Search root directory for existing volume label entry or free slot
  AImageFile.Position := FRootDirStart;
  for I := 0 to FRootEntries - 1 do
  begin
    if AImageFile.Read(DirEntry, FAT12_DIR_ENTRY_SIZE) <> FAT12_DIR_ENTRY_SIZE then Break;
    if (DirEntry[11] and $08) <> 0 then
    begin
      LabelPos := AImageFile.Position - FAT12_DIR_ENTRY_SIZE;
      FoundExisting := True;
      Break;
    end
    else if (LabelPos = -1) and ((DirEntry[0] = 0) or (DirEntry[0] = FAT12_DELETED)) then
    begin
      LabelPos := AImageFile.Position - FAT12_DIR_ENTRY_SIZE;
    end;
  end;

  if CleanLabel = '' then
  begin
    // If removing volume label and existing entry was found, mark it deleted
    if FoundExisting and (LabelPos <> -1) then
    begin
      AImageFile.Position := LabelPos;
      DirEntry[0] := FAT12_DELETED;
      AImageFile.Write(DirEntry[0], 1);
    end;
    Exit;
  end;

  if LabelPos = -1 then
  begin
    ShowMessage('Root directory full, cannot write volume label.');
    Exit;
  end;

  // Format 11-char volume label entry
  FillChar(DirEntry, FAT12_DIR_ENTRY_SIZE, 0);
  FillChar(DirEntry[0], 11, $20);
  Move(PAnsiChar(CleanLabel)^, DirEntry[0], Length(CleanLabel));
  DirEntry[11] := $08; // ATTR_VOLUME_ID

  TimeWord := DateTimeToFatTime(Now);
  DateWord := DateTimeToFatDate(Now);
  Move(TimeWord, DirEntry[22], 2);
  Move(DateWord, DirEntry[24], 2);

  AImageFile.Position := LabelPos;
  AImageFile.Write(DirEntry, FAT12_DIR_ENTRY_SIZE);
end;

{------------------------------------------------------------------
  Procedure: ButtonSetLabelClick
  Purpose:  Event handler for setting/changing the Atari ST volume label
 ------------------------------------------------------------------}
procedure TForm1.ButtonSetLabelClick(Sender: TObject);
var
  NewLabel: string;
  ImageFile: TFileStream;
begin
  if (FImagePath = '') or not FileExists(FImagePath) then
  begin
    ShowMessage('Please open or create a disk image first.');
    Exit;
  end;

  NewLabel := InputBox('Set Atari Volume Label',
    'Enter volume label for disk (up to 11 characters):', FVolumeLabel);
  NewLabel := UpperCase(Trim(NewLabel));
  if Length(NewLabel) > 11 then
    NewLabel := Copy(NewLabel, 1, 11);

  FVolumeLabel := NewLabel;
  ImageFile := TFileStream.Create(FImagePath, fmOpenReadWrite or fmShareDenyNone);
  try
    WriteVolumeLabelToImage(ImageFile, FVolumeLabel);
  finally
    ImageFile.Free;
  end;

  if FVolumeLabel <> '' then
    LabelVolume.Caption := 'Volume: ' + FVolumeLabel
  else
    LabelVolume.Caption := 'Volume: [None]';

  ShowMessage('Volume label updated to: ' + FVolumeLabel);
end;

{------------------------------------------------------------------
  Procedure: ButtonViewFileClick
  Purpose:  Event handler for the built-in File Viewer / Hex Inspector
 ------------------------------------------------------------------}
procedure TForm1.ButtonViewFileClick(Sender: TObject);
var
  RawName, CleanName, HostPath: string;
  IsDir, IsStaged, IsDel, IsMarkedDel: Boolean;
  MemStream: TMemoryStream;
  StartClust: Word;
  I: Integer;
begin
  if not GetSelectedFileName(RawName, CleanName, IsDir, IsStaged, IsDel, IsMarkedDel) then
  begin
    ShowMessage('Please select a file to view.');
    Exit;
  end;

  if (CleanName = '..') or (CleanName = '..\') or IsDir or CleanName.EndsWith('\') then
  begin
    ShowMessage('Please select a file, not a directory.');
    Exit;
  end;

  // Case 1: Pending staged file (*A*) - load from host disk
  if IsStaged then
  begin
    HostPath := '';
    for I := 0 to FFilePaths.Count - 1 do
    begin
      if SameText(ExtractFileName(FFilePaths[I]), CleanName) and
         (I < FFileDestDirs.Count) and SameText(FFileDestDirs[I], FCurrentPath) then
      begin
        HostPath := FFilePaths[I];
        Break;
      end;
    end;
    if (HostPath <> '') and FileExists(HostPath) then
    begin
      FormFileViewer.LoadFromHostFile(HostPath, CleanName, 0);
      FormFileViewer.ShowModal;
      Exit;
    end;
  end;

  // Case 2: File on disk (including deleted or hidden files)
  if (FImagePath = '') or not FileExists(FImagePath) then
  begin
    ShowMessage('No disk image is loaded.');
    Exit;
  end;

  MemStream := TMemoryStream.Create;
  try
    StartClust := 0;
    if ExtractFileToStream(RawName, MemStream, StartClust) then
    begin
      MemStream.Position := 0;
      FormFileViewer.LoadFromStream(MemStream, CleanName, StartClust);
      FormFileViewer.ShowModal;
    end
    else
      ShowMessage('Unable to read file contents from disk image.');
  finally
    MemStream.Free;
  end;
end;

{------------------------------------------------------------------
  Procedure: ListViewFilesMouseDown
  Purpose:  Captures start coordinate for dragging files out of STIMAGE
 ------------------------------------------------------------------}
procedure TForm1.ListViewFilesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FDragStartPoint := Point(X, Y);
    FDragPending := (ListViewFiles.GetItemAt(X, Y) <> nil);
  end;
end;

{------------------------------------------------------------------
  Procedure: ListViewFilesMouseMove
  Purpose:  Initiates OLE drag-and-drop export when dragging a file
 ------------------------------------------------------------------}
procedure TForm1.ListViewFilesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Item: TListItem;
  Idx: Integer;
  TempHostPath: string;
begin
  if (ssLeft in Shift) and FDragPending and not FDragActive then
  begin
    if (Abs(X - FDragStartPoint.X) > 5) or (Abs(Y - FDragStartPoint.Y) > 5) then
    begin
      FDragPending := False;
      Item := ListViewFiles.Selected;
      if Item = nil then
        Item := ListViewFiles.GetItemAt(FDragStartPoint.X, FDragStartPoint.Y);
      if Item = nil then Exit;

      Idx := Item.Index;
      if (Idx < 0) or (Idx > High(FDiskItems)) then Exit;

      if FDiskItems[Idx].IsDir or (FDiskItems[Idx].CleanName = '..') then Exit;

      TempHostPath := ExtractFileToTemp(FDiskItems[Idx].DisplayName);
      if (TempHostPath <> '') and FileExists(TempHostPath) then
      begin
        FDragActive := True;
        FDragSourcePath := ExpandFileName(TempHostPath);
        try
          StartDragDropFiles([TempHostPath]);
        finally
          FDragActive := False;
          FDragSourcePath := '';
          Application.ProcessMessages;
        end;
      end;
    end;
  end;
end;

{------------------------------------------------------------------
  Procedure: MenuItemConvertMSAClick
  Purpose:  Converts between uncompressed .ST and compressed .MSA disk images
 ------------------------------------------------------------------}
procedure TForm1.MenuItemConvertMSAClick(Sender: TObject);
var
  SrcFile, DstFile, Ext: string;
begin
  OpenDialog.Title := 'Select Disk Image to Convert (.ST or .MSA)';
  OpenDialog.Filter := 'Atari Disk Images (*.st;*.msa)|*.st;*.msa|ST Image (*.st)|*.st|MSA Image (*.msa)|*.msa|All Files (*.*)|*.*';
  if not OpenDialog.Execute then Exit;
  SrcFile := OpenDialog.FileName;
  if not FileExists(SrcFile) then Exit;

  Ext := LowerCase(ExtractFileExt(SrcFile));
  if Ext = '.msa' then
  begin
    SaveDialog.Title := 'Convert MSA to Standard ST Disk Image';
    SaveDialog.Filter := 'ST Image (*.st)|*.st|All Files (*.*)|*.*';
    SaveDialog.FileName := ChangeFileExt(ExtractFileName(SrcFile), '.st');
    if not SaveDialog.Execute then Exit;
    DstFile := SaveDialog.FileName;
    if FileExists(DstFile) then SysUtils.DeleteFile(DstFile);

    if UnpackMSAToST(SrcFile, DstFile) then
      ShowMessage('Conversion complete!' + sLineBreak + 'Unpacked MSA image to: ' + DstFile)
    else
      ShowMessage('Error: Failed to convert MSA image to ST.');
  end
  else if Ext = '.st' then
  begin
    SaveDialog.Title := 'Convert ST to Compressed MSA Disk Image';
    SaveDialog.Filter := 'MSA Image (*.msa)|*.msa|All Files (*.*)|*.*';
    SaveDialog.FileName := ChangeFileExt(ExtractFileName(SrcFile), '.msa');
    if not SaveDialog.Execute then Exit;
    DstFile := SaveDialog.FileName;
    if FileExists(DstFile) then SysUtils.DeleteFile(DstFile);

    if PackSTToMSA(SrcFile, DstFile, 9, 2) then
      ShowMessage('Conversion complete!' + sLineBreak + 'Packed ST image to MSA: ' + DstFile)
    else
      ShowMessage('Error: Failed to convert ST image to MSA.');
  end
  else
  begin
    ShowMessage('Please select a file with .st or .msa extension.');
  end;
end;

{------------------------------------------------------------------
  Procedure: ButtonSaveFileClick
  Purpose:  Event handler for extracting/saving the selected file
 ------------------------------------------------------------------}
procedure TForm1.ButtonSaveFileClick(Sender: TObject);
var
  RawName, CleanName, LocalFileName: string;
  IsDir, IsStaged, IsDel, IsMarkedDel: Boolean;
  Fs: TFileStream;
  DummyCluster: Word;
begin
  if not GetSelectedFileName(RawName, CleanName, IsDir, IsStaged, IsDel, IsMarkedDel) then
  begin
    ShowMessage('Please select a file to save/extract.');
    Exit;
  end;

  if IsStaged then
  begin
    ShowMessage('This file is newly added. Please write the image first before saving.');
    Exit;
  end;

  if (CleanName = '..') or (CleanName = '..\') or IsDir or CleanName.EndsWith('\') then
  begin
    ShowMessage('Cannot save a directory with this function. Use "Extract All to Folder" instead.');
    Exit;
  end;

  if (FImagePath = '') or not FileExists(FImagePath) then
  begin
    ShowMessage('No image file is open.');
    Exit;
  end;

  SaveDialog.Filter := 'All Files (*.*)|*.*';
  SaveDialog.FileName := CleanName;
  if not SaveDialog.Execute then Exit;
  LocalFileName := SaveDialog.FileName;

  Fs := TFileStream.Create(LocalFileName, fmCreate);
  try
    if ExtractFileToStream(RawName, Fs, DummyCluster) then
      ShowMessage('File extracted successfully: ' + LocalFileName)
    else
      ShowMessage('Failed to extract file.');
  finally
    Fs.Free;
  end;
end;

{------------------------------------------------------------------
  Procedure: MenuItemExtractDepackedClick
  Purpose:  Extracts and depacks the selected file using detected packer
 ------------------------------------------------------------------}
procedure TForm1.MenuItemExtractDepackedClick(Sender: TObject);
var
  RawName, CleanName, LocalFileName, PackerName: string;
  IsDir, IsStaged, IsDel, IsMarkedDel: Boolean;
  Fs: TFileStream;
  MemStream: TMemoryStream;
  DummyCluster: Word;
  InBuf, OutBuf: TBytes;
  I: Integer;
  HostPath: string;
begin
  if not GetSelectedFileName(RawName, CleanName, IsDir, IsStaged, IsDel, IsMarkedDel) then
  begin
    ShowMessage('Please select a file to depack and extract.');
    Exit;
  end;

  if (CleanName = '..') or (CleanName = '..\') or IsDir or CleanName.EndsWith('\') then
  begin
    ShowMessage('Cannot depack a directory. Please select a file.');
    Exit;
  end;

  SetLength(InBuf, 0);

  // Load file data (staged from host or from disk image)
  if IsStaged then
  begin
    HostPath := '';
    for I := 0 to FFilePaths.Count - 1 do
    begin
      if SameText(ExtractFileName(FFilePaths[I]), CleanName) and
         (I < FFileDestDirs.Count) and SameText(FFileDestDirs[I], FCurrentPath) then
      begin
        HostPath := FFilePaths[I];
        Break;
      end;
    end;
    if (HostPath <> '') and FileExists(HostPath) then
    begin
      Fs := TFileStream.Create(HostPath, fmOpenRead or fmShareDenyNone);
      try
        SetLength(InBuf, Fs.Size);
        if Fs.Size > 0 then
          Fs.ReadBuffer(InBuf[0], Fs.Size);
      finally
        Fs.Free;
      end;
    end
    else
    begin
      ShowMessage('Staged host file not found.');
      Exit;
    end;
  end
  else
  begin
    if (FImagePath = '') or not FileExists(FImagePath) then
    begin
      ShowMessage('No image file is open.');
      Exit;
    end;

    MemStream := TMemoryStream.Create;
    try
      if not ExtractFileToStream(RawName, MemStream, DummyCluster) then
      begin
        ShowMessage('Failed to read file from disk image.');
        Exit;
      end;
      SetLength(InBuf, MemStream.Size);
      if MemStream.Size > 0 then
      begin
        MemStream.Position := 0;
        MemStream.ReadBuffer(InBuf[0], MemStream.Size);
      end;
    finally
      MemStream.Free;
    end;
  end;

  if Length(InBuf) = 0 then
  begin
    ShowMessage('Selected file is empty.');
    Exit;
  end;

  // Detect packer signature
  PackerName := DetectPackerSignature(@InBuf[0], Length(InBuf));
  if (PackerName = '') or (PackerName = 'None') then
  begin
    ShowMessage('No supported Atari packer signature detected in "' + CleanName + '".');
    Exit;
  end;

  Screen.Cursor := crHourGlass;
  try
    if not DepackBuffer(@InBuf[0], Length(InBuf), OutBuf, PackerName) then
    begin
      ShowMessage('Decompression failed for: ' + PackerName + sLineBreak +
        'The file may be corrupted or compressed with an incompatible variant.');
      Exit;
    end;
  finally
    Screen.Cursor := crDefault;
  end;

  SaveDialog.Title := 'Save Depacked File As...';
  SaveDialog.Filter := 'All Files (*.*)|*.*';
  SaveDialog.FileName := CleanName;
  if not SaveDialog.Execute then Exit;
  LocalFileName := SaveDialog.FileName;

  Fs := TFileStream.Create(LocalFileName, fmCreate);
  try
    Fs.WriteBuffer(OutBuf[0], Length(OutBuf));
    ShowMessage(Format('Successfully depacked and extracted!' + sLineBreak +
      'Packer: %s' + sLineBreak +
      'Decompressed: %s bytes (Original: %s bytes)' + sLineBreak +
      'Saved to: %s',
      [PackerName,
       FormatFloat('#,##0', Length(OutBuf)),
       FormatFloat('#,##0', Length(InBuf)),
       LocalFileName]));
  finally
    Fs.Free;
  end;
end;

{------------------------------------------------------------------
  Procedure: FormKeyDown
  Purpose:  Global hotkeys for disk actions:
            Ctrl+O: Open Image, Ctrl+S: Write Image, Ctrl+N: New 720KB,
            Backspace / Alt+Up: Go back, Delete: Delete/Undelete
 ------------------------------------------------------------------}
procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = VK_O) then
  begin
    ButtonOpenImageClick(nil);
    Key := 0;
    Exit;
  end;

  if (ssCtrl in Shift) and (Key = VK_S) then
  begin
    ButtonWriteImageClick(nil);
    Key := 0;
    Exit;
  end;

  if (ssCtrl in Shift) and (Key = VK_N) then
  begin
    ButtonCreate720Click(nil);
    Key := 0;
    Exit;
  end;

  if (Key = VK_BACK) or ((ssAlt in Shift) and (Key = VK_UP)) then
  begin
    if FCurrentPath <> '' then
    begin
      ButtonBackClick(nil);
      Key := 0;
      Exit;
    end;
  end;

  if Key = VK_DELETE then
  begin
    if (ListViewFiles.Selected <> nil) then
    begin
      ButtonRemoveFileClick(nil);
      Key := 0;
      Exit;
    end;
  end;
end;

{------------------------------------------------------------------
  Procedure: ListViewFilesKeyDown
  Purpose:  Keyboard navigation inside the file list:
            Enter: open dir or view file, Delete: delete/undelete,
            Backspace / Alt+Up: parent dir
 ------------------------------------------------------------------}
procedure TForm1.ListViewFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    ListViewFilesDblClick(nil);
    Key := 0;
    Exit;
  end;

  if Key = VK_DELETE then
  begin
    ButtonRemoveFileClick(nil);
    Key := 0;
    Exit;
  end;

  if (Key = VK_BACK) or ((ssAlt in Shift) and (Key = VK_UP)) then
  begin
    if FCurrentPath <> '' then
    begin
      ButtonBackClick(nil);
      Key := 0;
      Exit;
    end;
  end;
end;

{------------------------------------------------------------------
  Procedure: ListViewFilesCustomDrawItem
  Purpose:  Color-codes file entries:
            - Directories: bold blue
            - Staged additions (*A*): bold green
            - Deleted files (*d*): red with strikeout
            - Marked deletions (*D*): maroon with strikeout
            - Hidden files (*h*): italic purple
 ------------------------------------------------------------------}
procedure TForm1.ListViewFilesCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Idx: Integer;
begin
  DefaultDraw := True;
  if Item = nil then Exit;
  Idx := Item.Index;
  if (Idx < 0) or (Idx > High(FDiskItems)) then Exit;

  if FDiskItems[Idx].IsDir then
  begin
    Sender.Canvas.Font.Style := [fsBold];
    if not (cdsSelected in State) then
      Sender.Canvas.Font.Color := clBlue;
  end
  else if FDiskItems[Idx].IsStagedAdd then
  begin
    Sender.Canvas.Font.Style := [fsBold];
    if not (cdsSelected in State) then
      Sender.Canvas.Font.Color := clGreen;
  end
  else if FDiskItems[Idx].IsDeleted then
  begin
    Sender.Canvas.Font.Style := [fsStrikeOut];
    if not (cdsSelected in State) then
      Sender.Canvas.Font.Color := clRed;
  end
  else if FDiskItems[Idx].IsMarkedDelete then
  begin
    Sender.Canvas.Font.Style := [fsStrikeOut];
    if not (cdsSelected in State) then
      Sender.Canvas.Font.Color := clMaroon;
  end
  else if FDiskItems[Idx].IsHidden then
  begin
    Sender.Canvas.Font.Style := [fsItalic];
    if not (cdsSelected in State) then
      Sender.Canvas.Font.Color := clPurple;
  end
  else
  begin
    Sender.Canvas.Font.Style := [];
    if not (cdsSelected in State) then
      Sender.Canvas.Font.Color := clWindowText;
  end;
end;

{------------------------------------------------------------------
  Procedure: ListViewFilesCustomDrawSubItem
  Purpose:  Applies consistent font styles and colors to sub-item columns
 ------------------------------------------------------------------}
procedure TForm1.ListViewFilesCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  DefaultDraw := True;
  ListViewFilesCustomDrawItem(Sender, Item, State, DefaultDraw);
end;

end.

