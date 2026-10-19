unit Unit1;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLType, ExtCtrls, ComCtrls, StrUtils, Math, DateUtils, LazFileUtils,
  FileUtil, Menus, zipper, unit_clustermap, unit_msa, unit_fileviewer, unit_dragout,
  unit_depacker, unit_virusscanner, unit_newimage, unit_archivevault,
  unit_dim, unit_search;

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
    PanelQuickToolbar: TPanel;
    ButtonToolOpen: TButton;
    ButtonToolNew: TButton;
    ButtonToolWrite: TButton;
    BevelSep1: TBevel;
    ButtonToolAddFile: TButton;
    ButtonToolAddFolder: TButton;
    ButtonToolCreateDir: TButton;
    ButtonToolExtract: TButton;
    BevelSep2: TBevel;
    ButtonToolView: TButton;
    ButtonToolClusterMap: TButton;
    ButtonToolVirusScan: TButton;

    PanelHeader: TPanel;
    ButtonNavUp: TButton;
    ButtonNavRoot: TButton;
    PanelPathBox: TPanel;
    LabelCurrentPath: TLabel;
    LabelVolume: TLabel;
    ButtonSetLabel: TButton;
    LabelFreeSpace: TLabel;
    ProgressBarFreeSpace: TProgressBar;

    StatusBar1: TStatusBar;

    PanelToolbar: TPanel;
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
    ButtonVirusScan: TButton;
    ButtonBack: TButton;

    ListViewFiles: TListView;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    MainMenu1: TMainMenu;
    MenuItemFile: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemNew720: TMenuItem;
    MenuItemWrite: TMenuItem;
    MenuItemCloseImage: TMenuItem;
    MenuItemSep1: TMenuItem;
    MenuItemExtractFile: TMenuItem;
    MenuItemExtractDepacked: TMenuItem;
    MenuItemExtractAll: TMenuItem;
    MenuItemSep2: TMenuItem;
    MenuItemConvertMSA: TMenuItem;
    MenuItemConvertDIM: TMenuItem;
    MenuItemSep3: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemFind: TMenuItem;
    MenuItemSepEdit0: TMenuItem;
    MenuItemRename: TMenuItem;
    MenuItemAttributes: TMenuItem;
    MenuItemAttrReadOnly: TMenuItem;
    MenuItemAttrHidden: TMenuItem;
    MenuItemAttrArchive: TMenuItem;
    MenuItemAttrSep: TMenuItem;
    MenuItemAttrProperties: TMenuItem;
    MenuItemSepEdit1: TMenuItem;
    MenuItemAddFile: TMenuItem;
    MenuItemAddFolder: TMenuItem;
    MenuItemCreateDir: TMenuItem;
    MenuItemSep4: TMenuItem;
    MenuItemDelete: TMenuItem;
    MenuItemRemoveDir: TMenuItem;
    MenuItemSepEdit2: TMenuItem;
    MenuItemSetLabel: TMenuItem;
    MenuItemView: TMenuItem;
    MenuItemQuickToolbar: TMenuItem;
    MenuItemButtonToolbar: TMenuItem;
    MenuItemSepView1: TMenuItem;
    MenuItemViewFile: TMenuItem;
    MenuItemSepView2: TMenuItem;
    MenuItemBrowseArchive: TMenuItem;
    MenuItemCloseArchive: TMenuItem;
    MenuItemBack: TMenuItem;
    MenuItemTools: TMenuItem;
    MenuItemClusterMap: TMenuItem;
    MenuItemSepTools: TMenuItem;
    MenuItemVirusScanner: TMenuItem;
    MenuItemSepTools2: TMenuItem;
    MenuItemMakePCBoot: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemAbout: TMenuItem;
    PopupMenuFiles: TPopupMenu;
    MenuItemPopupOpenArchive: TMenuItem;
    MenuItemPopupSep0: TMenuItem;
    MenuItemPopupView: TMenuItem;
    MenuItemPopupFind: TMenuItem;
    MenuItemPopupSep1: TMenuItem;
    MenuItemPopupRename: TMenuItem;
    MenuItemPopupAttributes: TMenuItem;
    MenuItemPopupAttrReadOnly: TMenuItem;
    MenuItemPopupAttrHidden: TMenuItem;
    MenuItemPopupAttrArchive: TMenuItem;
    MenuItemPopupAttrSep: TMenuItem;
    MenuItemPopupAttrProperties: TMenuItem;
    MenuItemPopupSepAttr: TMenuItem;
    MenuItemPopupExtract: TMenuItem;
    MenuItemPopupExtractDepacked: TMenuItem;
    MenuItemPopupSep2: TMenuItem;
    MenuItemPopupClusterMap: TMenuItem;
    MenuItemPopupSep3: TMenuItem;
    MenuItemPopupDelete: TMenuItem;

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
    procedure ButtonNavRootClick(Sender: TObject);
    procedure ListViewFilesClick(Sender: TObject);
    procedure ListViewFilesDblClick(Sender: TObject);
    procedure ListViewFilesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure ListViewFilesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ListViewFilesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ListViewFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListViewFilesColumnClick(Sender: TObject; Column: TListColumn);
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
    procedure MenuItemVirusScannerClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormShow(Sender: TObject);
    procedure PopupMenuFilesPopup(Sender: TObject);
    procedure MenuItemPopupOpenArchiveClick(Sender: TObject);
    procedure MenuItemPopupDeleteClick(Sender: TObject);
    procedure MenuItemCloseImageClick(Sender: TObject);
    procedure MenuItemCloseArchiveClick(Sender: TObject);
    procedure MenuItemQuickToolbarClick(Sender: TObject);
    procedure MenuItemButtonToolbarClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MenuItemConvertDIMClick(Sender: TObject);
    procedure MenuItemFindClick(Sender: TObject);
    procedure MenuItemRenameClick(Sender: TObject);
    procedure MenuItemAttrReadOnlyClick(Sender: TObject);
    procedure MenuItemAttrHiddenClick(Sender: TObject);
    procedure MenuItemAttrArchiveClick(Sender: TObject);
    procedure MenuItemAttrPropertiesClick(Sender: TObject);
    procedure MenuItemMakePCBootClick(Sender: TObject);

  public
    procedure SetButtonToolbarVisible(AVisible: Boolean);
    procedure UpdateLayout;
    procedure UpdateStatusBar;
    procedure SortDiskItems;
    procedure RefreshListViewFromDiskItems;
    procedure PopulateFileIcons(AImageList: TImageList);
    function GetFileIconIndex(const AFileName: string; IsDir: Boolean): Integer;

  private
    FSortColumn: Integer;             // Current sort column index (0..5)
    FSortAscending: Boolean;          // True = ascending, False = descending
    ImageListFiles: TImageList;       // 16x16 file type icons
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
    FSectorsPerTrack: Word;           // Sectors per track (e.g. 9, 10, 11, 18)
    FSides: Word;                     // Number of sides (1 or 2)
    FTracks: Word;                    // Number of tracks (e.g. 80)
    FIsMSAImage: Boolean;             // True if currently loaded from/saving to MSA
    FOriginalMSAPath: string;         // Original host path of the MSA file
    FIsDIMImage: Boolean;             // True if currently loaded from/saving to FastCopy DIM
    FOriginalDIMPath: string;         // Original host path of the DIM file
    FOriginalSTPath: string;          // Original host path of single-sided interleaved ST file
    FTempSTPath: string;              // Temporary ST image file when working with MSA or de-interleaved ST
    FTempZipExtractedPath: string;    // Temporary ST/MSA image file extracted from ZIP
    FOriginalZipPath: string;         // Original host path of the ZIP archive
    FDragStartPoint: TPoint;          // Mouse point where drag-out starts
    FDragPending: Boolean;            // Flag indicating left button pressed on an item
    FDragActive: Boolean;             // Flag indicating active outgoing OLE drag operation
    FDragSourcePath: string;          // Full host path of currently dragged-out file
    FIsModified: Boolean;             // True if unwritten direct edits (undelete, rename, attr, etc.) were made

    // Archive navigation state
    FInArchiveMode: Boolean;          // True when currently browsing inside an archive
    FArchiveVault: TArchiveVault;     // Archive handler (ZIP, LZH, LHA, ARC, ZOO)
    FArchiveSourceFileName: string;   // Name of archive file on disk, e.g. "HDDR771C.LZH"
    FArchiveTempFile: string;         // Host temp file of extracted archive
    FArchiveInnerPath: string;        // Current path inside archive, e.g. "AUTO/" or ""
    FArchiveChildren: array of TArchiveItem; // Items currently displayed from archive
    FArchiveChildCount: Integer;
    procedure OpenArchiveFromDisk(const AArchiveFileName: string);
    procedure CloseArchiveMode;
    procedure UpdateArchiveFileListDisplay;

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
    procedure CreateBlankDiskImage(const ATargetFile: string; ASides, ASectorsPerTrack, ATracks, ARootEntries, ASectorsPerCluster: Integer);
    procedure CreateBlank720KBImage(const ATargetFile: string);
      // Creates a clean, standard 720KB Atari ST floppy image
    function LoadDiskImage(const AFileName: string): Boolean;
    function LoadZipArchive(const AZipPath: string): Boolean;
    function SelectDiskImageFromZip(const AZipPath: string; MatchingEntries: TStrings): Integer;
    procedure AddFolderToZipper(const ABaseDir, ARelPath: string; AZip: TZipper);
    procedure ZipListDblClick(Sender: TObject);
    procedure ZipListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ExtractDiskDirectory(const AImageFile: TFileStream; const ADirCluster: Int64; const AHostPath: string; var AFileCount: Integer; var AByteCount: Int64; AVisitedDirs: TList = nil; ADepth: Integer = 0);
    procedure WriteVolumeLabelToImage(const AImageFile: TFileStream; const ALabel: string);
    function ExtractFileToStream(const AFileName: string; AOutStream: TStream; out AStartCluster: Word): Boolean;
    function ExtractFileToTemp(const AFileName: string): string;
    procedure FreeClusterChain(const AImageFile: TFileStream; StartCluster: Word);
    procedure DeleteDirectoryRecursive(const AImageFile: TFileStream; const ADirCluster: Word);
    function CanCloseOrOpenImage: Boolean;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    function CheckAndDeinterleaveSingleSidedST(const ASourceFile: string; out ADeinterleavedFile: string): Boolean;
    function ScoreDirectoryAtOffset(const AStream: TStream; AOffset: Int64; AMaxEntries: Integer): Integer;
    function DiscoverDirectoryContent(const AStream: TStream; out ARootDirStart: Int64; out ARootDirEntries: Word): Integer;
    procedure DeduceSubdirectoryGeometry(const AStream: TStream; ARootDirStart: Int64; var ARootDirEntries: Word;
      var ASectorsPerCluster: Byte; var ADataAreaStart: Int64);

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
    procedure RenameSelectedItem;
    procedure ToggleSelectedAttribute(AttrBit: Byte);
    procedure EditSelectedAttributes;
    procedure MakePCCompatibleBootSector;
    procedure OpenSearchDialog;
    procedure HandleSearchNavigate(const ADirPath, AFileName: string);
    procedure HandleSearchView(const ADirPath, AFileName: string);
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

  if (ListViewFiles = nil) then Exit;
  if (ListViewFiles.Selected = nil) and (ListViewFiles.ItemIndex >= 0) and (ListViewFiles.ItemIndex < ListViewFiles.Items.Count) then
    ListViewFiles.Selected := ListViewFiles.Items[ListViewFiles.ItemIndex];
  if (ListViewFiles.Selected = nil) then Exit;
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
  Item.ImageIndex := GetFileIconIndex(AInfo.CleanName, AInfo.IsDir);
  Item.SubItems.Add(AInfo.SizeStr);
  Item.SubItems.Add(AInfo.ClusterStr);
  Item.SubItems.Add(AInfo.DateTimeStr);
  Item.SubItems.Add(AInfo.AttrStr);
  Item.SubItems.Add(AInfo.StatusStr);
end;

function GetFullItemPath(const ADir, AName: string): string;
begin
  if (ADir = '') or (ADir = '\') then
    Result := AName
  else if (Length(ADir) > 0) and (ADir[Length(ADir)] = '\') then
    Result := ADir + AName
  else
    Result := ADir + '\' + AName;
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
  H, M, S, MS: Word;
begin
  DecodeTime(ADateTime, H, M, S, MS);
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
  CloseArchiveMode;
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
  FSectorsPerTrack := 9;
  FSides := 2;
  FTracks := 80;
  FIsDIMImage := False;
  FOriginalDIMPath := '';
  FIsModified := False;
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
  FileName, FileExt, CleanPart: string;
  C: AnsiChar;
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

  // Preserve standard DOS '.' and '..' special entries
  if (FileName = '.') and (FileExt = '') then Exit('.');
  if (FileName = '..') and (FileExt = '') then Exit('..');

  // Sanitize FileName
  CleanPart := '';
  for I := 1 to Length(FileName) do
  begin
    C := FileName[I];
    if C in ['/', '\', ':', '*', '?', '"', '<', '>', '|', ';', ',', #0..#31, #127] then
      CleanPart := CleanPart + '_'
    else
      CleanPart := CleanPart + C;
  end;
  FileName := CleanPart;

  // Sanitize FileExt
  CleanPart := '';
  for I := 1 to Length(FileExt) do
  begin
    C := FileExt[I];
    if C in ['/', '\', ':', '*', '?', '"', '<', '>', '|', ';', ',', #0..#31, #127] then
      CleanPart := CleanPart + '_'
    else
      CleanPart := CleanPart + C;
  end;
  FileExt := CleanPart;

  // Disallow names that are or start with '..'
  if (FileName = '..') or (FileName = '.') then
    FileName := '_' + FileName;

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

  // Prevent creating duplicate folder entries if folder already exists
  if FindFileInDirectory(AImageFile, ParentCluster, DirName) <> -1 then
    Exit;

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
  if not CanCloseOrOpenImage then Exit;
  OpenDialog.Options := [ofFileMustExist];
  OpenDialog.Filter := 'Atari ST Disk Images (*.st;*.msa;*.dim;*.zip)|*.st;*.msa;*.dim;*.zip|ST Image Files (*.st)|*.st|MSA Image Files (*.msa)|*.msa|FastCopy Pro DIM Files (*.dim)|*.dim|ZIP Archives (*.zip)|*.zip|All Files (*.*)|*.*';
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
  TargetDir := GetFullItemPath(FCurrentPath, StringReplace(DirName, '\', '', [rfReplaceAll]));
  if FDirsToDelete.IndexOf(TargetDir) = -1 then
    FDirsToDelete.Add(TargetDir)
  else
    FDirsToDelete.Delete(FDirsToDelete.IndexOf(TargetDir));
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
  RawName, FileName, TargetFull: string;
  IsDir, IsStaged, IsDel, IsMarkedDel: Boolean;
  Index, I, SavedIdx: Integer;
begin
  if FInArchiveMode then
  begin
    CloseArchiveMode;
    Exit;
  end;

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

  TargetFull := GetFullItemPath(FCurrentPath, FileName);
  Index := FFilesToDelete.IndexOf(TargetFull);
  if Index <> -1 then
    FFilesToDelete.Delete(Index)
  else
    FFilesToDelete.Add(TargetFull);

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
  Procedure: CreateBlankDiskImage
  Purpose:  Generates a clean Atari ST GEMDOS FAT12 floppy disk image
            supporting 360KB, 720KB, 800KB, 880KB, and 1.44MB formats
 ------------------------------------------------------------------}
procedure TForm1.CreateBlankDiskImage(const ATargetFile: string; ASides, ASectorsPerTrack, ATracks, ARootEntries, ASectorsPerCluster: Integer);
var
  TotalSectors, ReservedSectors, SectorsPerFAT: Integer;
  TotalSize, FATBytes, DataAreaSize: Int64;
  MediaByte: Byte;
  ImageFile: TFileStream;
  BootSector: array[0..511] of Byte;
  FATBuffer: array of Byte;
  RootDirBuffer: array of Byte;
  ZeroChunk: array[0..65535] of Byte;
  BytesRemaining, WriteChunk: Int64;
  SerialNum: LongWord;
begin
  TotalSectors := ASides * ASectorsPerTrack * ATracks;
  TotalSize := Int64(TotalSectors) * 512;
  ReservedSectors := 1;
  FSectorsPerTrack := ASectorsPerTrack;
  FSides := ASides;
  FTracks := ATracks;

  // Determine media descriptor and Sectors Per FAT based on format
  if ASides = 1 then
  begin
    MediaByte := $F8; // Single-sided 360 KB
    SectorsPerFAT := 2;
  end
  else if ASectorsPerTrack >= 18 then
  begin
    MediaByte := $F0; // High Density 1.44 MB
    SectorsPerFAT := 5;
  end
  else
  begin
    MediaByte := $F9; // Double-sided (720 KB, 800 KB, 880 KB)
    if ASectorsPerTrack = 11 then
      SectorsPerFAT := 4
    else
      SectorsPerFAT := 3;
  end;

  // Generate random 24-bit disk serial number
  Randomize;
  SerialNum := Random($1000000);

  // Initialize and populate Boot Sector (512 bytes)
  FillChar(BootSector[0], 512, 0);
  // Standard 68000 BRA opcode ($60 $1C) to skip BPB
  BootSector[$00] := $60;
  BootSector[$01] := $1C;
  // OEM / Loader signature (6 bytes)
  BootSector[$02] := Ord('S');
  BootSector[$03] := Ord('T');
  BootSector[$04] := Ord('I');
  BootSector[$05] := Ord('M');
  BootSector[$06] := Ord('A');
  BootSector[$07] := Ord('G');
  // 24-bit serial number (bytes 8..10)
  BootSector[$08] := (SerialNum shr 16) and $FF;
  BootSector[$09] := (SerialNum shr 8) and $FF;
  BootSector[$0A] := SerialNum and $FF;
  // BPB - Bytes per sector ($0B..$0C: 512)
  BootSector[$0B] := $00;
  BootSector[$0C] := $02;
  // Sectors per cluster ($0D)
  BootSector[$0D] := ASectorsPerCluster and $FF;
  // Reserved sectors ($0E..$0F: 1)
  BootSector[$0E] := ReservedSectors and $FF;
  BootSector[$0F] := (ReservedSectors shr 8) and $FF;
  // Number of FATs ($10: 2)
  BootSector[$10] := 2;
  // Root directory entries ($11..$12)
  BootSector[$11] := ARootEntries and $FF;
  BootSector[$12] := (ARootEntries shr 8) and $FF;
  // Total sectors on volume ($13..$14)
  BootSector[$13] := TotalSectors and $FF;
  BootSector[$14] := (TotalSectors shr 8) and $FF;
  // Media descriptor byte ($15)
  BootSector[$15] := MediaByte;
  // Sectors per FAT ($16..$17)
  BootSector[$16] := SectorsPerFAT and $FF;
  BootSector[$17] := (SectorsPerFAT shr 8) and $FF;
  // Sectors per track ($18..$19)
  BootSector[$18] := ASectorsPerTrack and $FF;
  BootSector[$19] := (ASectorsPerTrack shr 8) and $FF;
  // Number of sides / heads ($1A..$1B)
  BootSector[$1A] := ASides and $FF;
  BootSector[$1B] := (ASides shr 8) and $FF;
  // Hidden sectors ($1C..$1D: 0)
  BootSector[$1C] := 0;
  BootSector[$1D] := 0;
  // RTS instruction at offset $1E ($4E $75)
  BootSector[$1E] := $4E;
  BootSector[$1F] := $75;

  ImageFile := TFileStream.Create(ATargetFile, fmCreate);
  try
    // 1. Write Boot Sector
    ImageFile.WriteBuffer(BootSector[0], 512);

    // 2. Write FAT Copies (FAT 1 and FAT 2)
    FATBytes := Int64(SectorsPerFAT) * 512;
    SetLength(FATBuffer, FATBytes);
    FillChar(FATBuffer[0], FATBytes, 0);
    // Cluster 0 and 1 initial FAT12 marker:
    // Cluster 0 = MediaByte | $0F00, Cluster 1 = $0FFF -> [MediaByte, $FF, $FF]
    FATBuffer[0] := MediaByte;
    FATBuffer[1] := $FF;
    FATBuffer[2] := $FF;

    ImageFile.WriteBuffer(FATBuffer[0], FATBytes); // FAT 1
    ImageFile.WriteBuffer(FATBuffer[0], FATBytes); // FAT 2

    // 3. Write Empty Root Directory
    SetLength(RootDirBuffer, ARootEntries * 32);
    FillChar(RootDirBuffer[0], Length(RootDirBuffer), 0);
    ImageFile.WriteBuffer(RootDirBuffer[0], Length(RootDirBuffer));

    // 4. Fill Data Area with zeroes to reach exact target size
    FillChar(ZeroChunk[0], SizeOf(ZeroChunk), 0);
    DataAreaSize := TotalSize - ImageFile.Position;
    BytesRemaining := DataAreaSize;
    while BytesRemaining > 0 do
    begin
      WriteChunk := Min(BytesRemaining, SizeOf(ZeroChunk));
      ImageFile.WriteBuffer(ZeroChunk[0], WriteChunk);
      Dec(BytesRemaining, WriteChunk);
    end;
  finally
    ImageFile.Free;
  end;
end;

{------------------------------------------------------------------
  Procedure: CreateBlank720KBImage
  Purpose:  Generates a clean standard 720KB Atari ST disk image
 ------------------------------------------------------------------}
procedure TForm1.CreateBlank720KBImage(const ATargetFile: string);
begin
  CreateBlankDiskImage(ATargetFile, 2, 9, 80, 112, 2);
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
  DirEntryPos, DirCluster: Int64;
  I, J: Integer;
  OriginalSize: Int64;
  TempStr, FileDestDir: string;
  TargetFile, FileDir, DelName, TargetDir, ParentDir, TempOutFile, BakFileName: string;
  HasBackup: Boolean;
begin
  SaveDialog.Filter := 'Atari .ST Image (*.st)|*.st|Magic Shadow Archiver (*.msa)|*.msa|FastCopy Pro Image (*.dim)|*.dim|All Files (*.*)|*.*';
  if FIsMSAImage and (FOriginalMSAPath <> '') then
    SaveDialog.FileName := ExtractFileName(FOriginalMSAPath)
  else if FIsDIMImage and (FOriginalDIMPath <> '') then
    SaveDialog.FileName := ExtractFileName(FOriginalDIMPath)
  else if FOriginalSTPath <> '' then
    SaveDialog.FileName := ExtractFileName(FOriginalSTPath)
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

      // 1. Delete marked files first so replacing a file frees the slot and does not skip adding
      for I := 0 to FFilesToDelete.Count - 1 do
      begin
        TargetFile := FFilesToDelete[I];
        FileDir := ExtractFilePath(TargetFile);
        DelName := ExtractFileName(TargetFile);
        DirCluster := FindDirectoryInPath(TempImageFile, FileDir);
        if DirCluster <> -1 then
        begin
          DirEntryPos := FindFileInDirectory(TempImageFile, DirCluster, DelName);
          if DirEntryPos <> -1 then
            DeleteFileFromImage(TempImageFile, DirEntryPos, DelName);
        end;
      end;
      FFilesToDelete.Clear;

      // 2. Delete marked directories
      for I := 0 to FDirsToDelete.Count - 1 do
      begin
        TargetDir := FDirsToDelete[I];
        ParentDir := ExtractFilePath(TargetDir);
        DelName := ExtractFileName(TargetDir);
        if DelName = '' then DelName := TargetDir;
        DirCluster := FindDirectoryInPath(TempImageFile, ParentDir);
        if DirCluster <> -1 then
        begin
          DirEntryPos := FindFileInDirectory(TempImageFile, DirCluster, DelName);
          if DirEntryPos <> -1 then
            DeleteFileFromImage(TempImageFile, DirEntryPos, DelName);
        end;
      end;
      FDirsToDelete.Clear;

      // 3. Sort new directories by path depth (length) so parent directories are created first
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

      // 4. Write new files
      for I := 0 to FFilePaths.Count - 1 do
      begin
        if (Trim(FFilePaths[I]) = '') or (not FileExists(FFilePaths[I])) then
          Continue;

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
      FFilePaths.Clear;
      FFileDestDirs.Clear;

      // Write / update Atari ST volume label in root directory
      if FVolumeLabel <> '' then
        WriteVolumeLabelToImage(TempImageFile, FVolumeLabel);

      // Enforce original file size to prevent expansion
      TempImageFile.Size := OriginalSize;
    finally
      TempImageFile.Free;
    end;

    // Handle MSA compression vs raw ST saving
    BakFileName := SaveFileName + '.bak';
    if FileExists(BakFileName) then SysUtils.DeleteFile(BakFileName);

    if SameText(ExtractFileExt(SaveFileName), '.msa') then
    begin
      TempOutFile := TempFileName + '.msa';
      if FileExists(TempOutFile) then SysUtils.DeleteFile(TempOutFile);
      if not PackSTToMSA(TempFileName, TempOutFile, FSectorsPerTrack, FSides) then
      begin
        SysUtils.DeleteFile(TempFileName);
        ShowMessage('Error: Failed to encode compressed MSA disk image.');
        Exit;
      end;
      SysUtils.DeleteFile(TempFileName);
      TempFileName := TempOutFile;
      FIsMSAImage := True;
      FIsDIMImage := False;
      FOriginalMSAPath := SaveFileName;
      FOriginalDIMPath := '';
    end
    else if SameText(ExtractFileExt(SaveFileName), '.dim') then
    begin
      TempOutFile := TempFileName + '.dim';
      if FileExists(TempOutFile) then SysUtils.DeleteFile(TempOutFile);
      if not PackSTToDIM(TempFileName, TempOutFile, FSectorsPerTrack, FSides, FTracks) then
      begin
        SysUtils.DeleteFile(TempFileName);
        ShowMessage('Error: Failed to encode FastCopy DIM disk image.');
        Exit;
      end;
      SysUtils.DeleteFile(TempFileName);
      TempFileName := TempOutFile;
      FIsDIMImage := True;
      FIsMSAImage := False;
      FOriginalDIMPath := SaveFileName;
      FOriginalMSAPath := '';
    end
    else
    begin
      FIsMSAImage := False;
      FIsDIMImage := False;
      FOriginalMSAPath := '';
      FOriginalDIMPath := '';
    end;

    // Atomic replacement with backup rollback on failure
    HasBackup := False;
    if FileExists(SaveFileName) then
    begin
      HasBackup := RenameFile(SaveFileName, BakFileName);
      if not HasBackup then
        HasBackup := CopyFile(PChar(SaveFileName), PChar(BakFileName), False);
    end;

    if not RenameFile(TempFileName, SaveFileName) then
    begin
      if not CopyFile(PChar(TempFileName), PChar(SaveFileName), False) then
      begin
        if HasBackup and FileExists(BakFileName) then
          RenameFile(BakFileName, SaveFileName);
        ShowMessage('Error: Failed to write to image: ' + SaveFileName);
        Exit;
      end;
      SysUtils.DeleteFile(TempFileName);
    end;

    // Successful write: clean up backup
    if HasBackup and FileExists(BakFileName) then
      SysUtils.DeleteFile(BakFileName);

    ShowMessage('Changes successfully written to image.');
    FIsModified := False;

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
    if FIsMSAImage or FIsDIMImage then
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
  Purpose:  Event handler for creating a new Atari ST disk image
            Prompts user with choice of format (360KB, 720KB, 800KB, 880KB, 1.44MB)
 ------------------------------------------------------------------}
procedure TForm1.ButtonCreate720Click(Sender: TObject);
var
  Sides, Spt, Tracks, RootEntries, Spc: Integer;
  DefFileName, FormatName, SaveFileName, TempFileName: string;
begin
  if not CanCloseOrOpenImage then Exit;
  FormNewImage := TFormNewImage.Create(Application);
  try
    if FormNewImage.ShowModal <> mrOK then
      Exit;
    FormNewImage.GetSelectedFormat(Sides, Spt, Tracks, RootEntries, Spc, DefFileName, FormatName);
  finally
    FormNewImage.Free;
    FormNewImage := nil;
  end;

  SaveDialog.Title := 'Create ' + FormatName + ' Image';
  SaveDialog.Filter := 'ST Image Files (*.st)|*.st|All Files (*.*)|*.*';
  SaveDialog.FileName := DefFileName;
  if not SaveDialog.Execute then Exit;

  SaveFileName := SaveDialog.FileName;
  if LowerCase(ExtractFileExt(SaveFileName)) <> '.st' then
    SaveFileName := SaveFileName + '.st';

  TempFileName := GetTempFile(ExtractFilePath(SaveFileName), 'fat-temp-');
  try
    CreateBlankDiskImage(TempFileName, Sides, Spt, Tracks, RootEntries, Spc);
    if FileExists(SaveFileName) then SysUtils.DeleteFile(SaveFileName);
    RenameFile(TempFileName, SaveFileName);
    ShowMessage(Format('%s disk image created successfully!' + sLineBreak + '%s', [FormatName, SaveFileName]));
  except
    on E: Exception do
    begin
      if FileExists(TempFileName) then SysUtils.DeleteFile(TempFileName);
      ShowMessage('Error creating disk image: ' + E.Message);
      Exit;
    end;
  end;

  // Load the newly created image into the UI
  ResetState;
  LoadDiskImage(SaveFileName);
end;

{------------------------------------------------------------------
  Procedure: ButtonClusterMapClick
  Purpose:  Event handler for the "Cluster Map" button / "Locate in Cluster Map"
            Opens the Cluster Map and Sector Hex Editor with clusters highlighted
 ------------------------------------------------------------------}
procedure TForm1.ButtonClusterMapClick(Sender: TObject);
var
  RawName, FileName, CleanItem: string;
  IsDir, IsStaged, IsDel, IsMarkedDel: Boolean;
  ImageFile: TFileStream;
  DirCluster, DirEntryPos: Int64;
  DirEntry: array[0..31] of Byte;
  StartCluster, CurrCluster: Word;
  ClusterList: array of Word;
  LoopGuard: Integer;
begin
  if FInArchiveMode then
  begin
    ShowMessage('Cluster mapping is only available for floppy disk files, not inside archive containers.');
    Exit;
  end;

  if (FImagePath = '') or not FileExists(FImagePath) then
  begin
    ShowMessage('Please open or write an image first.');
    Exit;
  end;

  SetLength(ClusterList, 0);

  if GetSelectedFileName(RawName, FileName, IsDir, IsStaged, IsDel, IsMarkedDel) then
  begin
    if not IsStaged and (FileName <> '..') and (FileName <> '..\') then
    begin
      CleanItem := CleanFileName(FileName);
      if CleanItem.EndsWith('\') then
        CleanItem := Copy(CleanItem, 1, Length(CleanItem) - 1);

      ImageFile := TFileStream.Create(FImagePath, fmOpenRead or fmShareDenyNone);
      try
        GetFATDetails(ImageFile);
        if FCurrentPath = '' then
          DirCluster := 0
        else
          DirCluster := FindDirectoryInPath(ImageFile, FCurrentPath);

        DirEntryPos := -1;
        if DirCluster <> -1 then
        begin
          DirEntryPos := FindFileInDirectory(ImageFile, DirCluster, CleanItem);
          if (DirEntryPos = -1) and IsDel then
            DirEntryPos := FindDeletedFileInDirectory(ImageFile, DirCluster, CleanItem);
        end;

        if DirEntryPos <> -1 then
        begin
          ImageFile.Position := DirEntryPos;
          ImageFile.Read(DirEntry[0], 32);
          StartCluster := DirEntry[26] or (DirEntry[27] shl 8);
          CurrCluster := StartCluster;
          LoopGuard := 0;
          while (CurrCluster >= 2) and (CurrCluster <= FTotalClusters + 1) do
          begin
            Inc(LoopGuard);
            if LoopGuard > FTotalClusters + 2 then Break;
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
    if DirCluster = -1 then
    begin
      ShowMessage(Format(ERR_DIR_NOT_FOUND, [FCurrentPath]));
      Exit;
    end;

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
        if NextVal <> FAT12_FREE then
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
    FIsModified := True;
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
  UpdateStatusBar;
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
  if FInArchiveMode then
  begin
    ButtonRemoveFile.Caption := 'Close Archive';
    MenuItemDelete.Caption := 'Close &Archive (Return to Disk)';
    Exit;
  end;

  if GetSelectedFileName(RawName, CleanName, IsDir, IsStaged, IsDel, IsMarkedDel) then
  begin
    if IsDel or IsMarkedDel then
    begin
      ButtonRemoveFile.Caption := 'Undelete';
      MenuItemDelete.Caption := '&Undelete File';
    end
    else
    begin
      ButtonRemoveFile.Caption := 'Delete';
      MenuItemDelete.Caption := '&Delete File';
    end;
  end
  else
  begin
    ButtonRemoveFile.Caption := 'Delete';
    MenuItemDelete.Caption := '&Delete / Undelete File';
  end;
end;

function TForm1.CanCloseOrOpenImage: Boolean;
begin
  Result := True;
  if (FFilePaths.Count > 0) or (FNewDirectories.Count > 0) or
     (FFilesToDelete.Count > 0) or (FDirsToDelete.Count > 0) or FIsModified then
  begin
    if MessageDlg('Unsaved Changes',
      'You have pending changes staged or modified in the current image.' + sLineBreak +
      'Opening, creating, or closing an image will discard any unwritten changes.' + sLineBreak + sLineBreak +
      'Do you want to discard changes and continue?',
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Result := False;
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CanCloseOrOpenImage;
end;

{------------------------------------------------------------------
  Procedure: FormCreate
  Purpose:  Event handler for form creation
            Initializes all string lists and sets dialog titles
 ------------------------------------------------------------------}
procedure TForm1.FormCreate(Sender: TObject);
begin
  AllowDropFiles := True;
  OnCloseQuery := @FormCloseQuery;
  FIsModified := False;
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
  FIsDIMImage := False;
  FOriginalDIMPath := '';
  FOriginalSTPath := '';
  FTempSTPath := '';
  FTempZipExtractedPath := '';
  FOriginalZipPath := '';
  FDragPending := False;
  FDragActive := False;
  FDragSourcePath := '';
  FArchiveVault := TArchiveVault.Create;
  FInArchiveMode := False;
  FArchiveSourceFileName := '';
  FArchiveTempFile := '';
  FArchiveInnerPath := '';
  SetLength(FArchiveChildren, 0);
  FArchiveChildCount := 0;
  OpenDialog.Title := 'Select Files or Image';
  SelectDirectoryDialog.Title := 'Select Folder to Add (Recursive)';
  SaveDialog.Title := 'Save Image As...';
  Menu := MainMenu1;
  UpdateDeleteButtonState;
  SetButtonToolbarVisible(False);

  // Initialize sorting and file type icons
  FSortColumn := 0;
  FSortAscending := True;
  ImageListFiles := TImageList.Create(Self);
  PopulateFileIcons(ImageListFiles);
  ListViewFiles.SmallImages := ImageListFiles;
  ListViewFiles.AutoWidthLastColumn := True;
  UpdateLayout;
  UpdateStatusBar;
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
  UpdateLayout;
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
  if FInArchiveMode then
  begin
    FInArchiveMode := False;
    if (FArchiveTempFile <> '') and FileExists(FArchiveTempFile) then
      SysUtils.DeleteFile(FArchiveTempFile);
  end;
  FreeAndNil(FArchiveVault);
  if (FTempSTPath <> '') and FileExists(FTempSTPath) then
    SysUtils.DeleteFile(FTempSTPath);
  if (FTempZipExtractedPath <> '') and FileExists(FTempZipExtractedPath) then
  begin
    SysUtils.DeleteFile(FTempZipExtractedPath);
    RemoveDir(ExtractFilePath(FTempZipExtractedPath));
  end;
  FreeAndNil(FFilePaths);
  FreeAndNil(FFileDestDirs);
  FreeAndNil(FFilesToDelete);
  FreeAndNil(FDisplayItems);
  FreeAndNil(FNewDirectories);
  FreeAndNil(FDirsToDelete);
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
    if (Ext = '.st') or (Ext = '.msa') or (Ext = '.zip') or (Ext = '.dim') then
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
  Procedure: OpenArchiveFromDisk
  Purpose:  Extracts an archive (.zip, .lzh, .lha, .lza, .arc, .pak, .zoo)
            from the floppy disk to a temporary location and mounts it
            in TArchiveVault so it can be navigated like a folder
 ------------------------------------------------------------------}
procedure TForm1.OpenArchiveFromDisk(const AArchiveFileName: string);
var
  CleanName, TempFile: string;
begin
  CleanName := CleanFileName(AArchiveFileName);
  if not IsArchiveFileName(CleanName) then
  begin
    ShowMessage('File is not a supported archive format (.zip, .lzh, .lha, .lza, .arc, .pak, .zoo).');
    Exit;
  end;

  Screen.Cursor := crHourGlass;
  try
    TempFile := ExtractFileToTemp(AArchiveFileName);
  finally
    Screen.Cursor := crDefault;
  end;

  if (TempFile = '') or not FileExists(TempFile) then
  begin
    ShowMessage('Failed to extract archive from floppy disk image.');
    Exit;
  end;

  if FArchiveVault = nil then
    FArchiveVault := TArchiveVault.Create;

  Screen.Cursor := crHourGlass;
  try
    if not FArchiveVault.LoadArchive(TempFile) then
    begin
      ShowMessage('Failed to parse archive: ' + CleanName + sLineBreak +
        'The archive format was not recognized or the file may be corrupt.');
      SysUtils.DeleteFile(TempFile);
      Exit;
    end;
  finally
    Screen.Cursor := crDefault;
  end;

  if FArchiveVault.ItemCount = 0 then
  begin
    ShowMessage('Archive is empty: ' + CleanName);
    SysUtils.DeleteFile(TempFile);
    Exit;
  end;

  FInArchiveMode := True;
  FArchiveSourceFileName := CleanName;
  FArchiveTempFile := TempFile;
  FArchiveInnerPath := '';
  UpdateArchiveFileListDisplay;
end;

{------------------------------------------------------------------
  Procedure: CloseArchiveMode
  Purpose:  Exits archive folder navigation and returns to the disk image
 ------------------------------------------------------------------}
procedure TForm1.CloseArchiveMode;
begin
  if not FInArchiveMode then Exit;
  FInArchiveMode := False;
  FArchiveInnerPath := '';
  FArchiveSourceFileName := '';
  if FArchiveVault <> nil then
    FArchiveVault.Clear;
  if (FArchiveTempFile <> '') and FileExists(FArchiveTempFile) then
  begin
    SysUtils.DeleteFile(FArchiveTempFile);
    FArchiveTempFile := '';
  end;
  if (FImagePath <> '') and FileExists(FImagePath) then
    ReadSTImage(FImagePath, FCurrentPath)
  else
    FDisplayItems.Clear;
  UpdateFileListDisplay;
end;

{------------------------------------------------------------------
  Procedure: UpdateArchiveFileListDisplay
  Purpose:  Populates the ListViewFiles with files and subdirectories
            located inside the currently navigated archive
 ------------------------------------------------------------------}
procedure TForm1.UpdateArchiveFileListDisplay;
var
  Info: TDiskItemInfo;
  I, MaxItems: Integer;
begin
  if not FInArchiveMode or (FArchiveVault = nil) then Exit;

  ListViewFiles.Items.BeginUpdate;
  try
    ListViewFiles.Items.Clear;
    SetLength(FDiskItems, 0);

    // 1. Parent folder link '..'
    FillChar(Info, SizeOf(Info), 0);
    Info.DisplayName := '..';
    Info.CleanName := '..';
    Info.IsDir := True;
    Info.SizeStr := '<DIR>';
    if FArchiveInnerPath = '' then
      Info.ClusterStr := '[Disk]'
    else
      Info.ClusterStr := '[Up]';
    Info.DateTimeStr := '-';
    Info.AttrStr := 'D----';
    Info.StatusStr := 'Parent';
    SetLength(FDiskItems, Length(FDiskItems) + 1);
    FDiskItems[High(FDiskItems)] := Info;

    // 2. Query children from vault
    MaxItems := FArchiveVault.ItemCount * 2 + 32;
    SetLength(FArchiveChildren, MaxItems);
    FArchiveChildCount := 0;
    FArchiveVault.GetChildren(FArchiveInnerPath, FArchiveChildren, FArchiveChildCount);

    for I := 0 to FArchiveChildCount - 1 do
    begin
      FillChar(Info, SizeOf(Info), 0);
      Info.DisplayName := FArchiveChildren[I].DisplayName;
      Info.CleanName := FArchiveChildren[I].DisplayName;
      Info.IsDir := FArchiveChildren[I].IsDir;
      if Info.IsDir then
      begin
        Info.Size := 0;
        Info.SizeStr := '<DIR>';
        Info.AttrStr := 'D----';
        Info.ClusterStr := '<DIR>';
        Info.StatusStr := 'Directory';
      end
      else
      begin
        Info.Size := FArchiveChildren[I].OriginalSize;
        Info.SizeStr := FormatSizeStr(Info.Size, False);
        Info.AttrStr := '-----';
        Info.ClusterStr := FArchiveChildren[I].MethodStr;
        Info.StatusStr := 'In Archive';
      end;
      Info.DateTimeStr := FArchiveChildren[I].DateTimeStr;

      SetLength(FDiskItems, Length(FDiskItems) + 1);
      FDiskItems[High(FDiskItems)] := Info;
    end;
  finally
    ListViewFiles.Items.EndUpdate;
  end;

  SortDiskItems;
  RefreshListViewFromDiskItems;

  if FArchiveInnerPath = '' then
    LabelCurrentPath.Caption := 'Archive: ' + FArchiveSourceFileName + ' [Root]'
  else
    LabelCurrentPath.Caption := 'Archive: ' + FArchiveSourceFileName + ' / ' + FArchiveInnerPath;

  LabelVolume.Caption := 'Archive: ' + UpperCase(ExtractFileExt(FArchiveSourceFileName));
  UpdateDeleteButtonState;
end;

{------------------------------------------------------------------
  Procedure: ButtonBackClick
  Purpose:  Event handler for the "Back" button
            Navigates to the parent directory or exits archive
 ------------------------------------------------------------------}
procedure TForm1.ButtonBackClick(Sender: TObject);
var
  P: Integer;
begin
  if FInArchiveMode then
  begin
    if FArchiveInnerPath = '' then
      CloseArchiveMode
    else
    begin
      if FArchiveInnerPath.EndsWith('/') then
        FArchiveInnerPath := Copy(FArchiveInnerPath, 1, Length(FArchiveInnerPath) - 1);
      P := LastDelimiter('/', FArchiveInnerPath);
      if P > 0 then
        FArchiveInnerPath := Copy(FArchiveInnerPath, 1, P)
      else
        FArchiveInnerPath := '';
      UpdateArchiveFileListDisplay;
    end;
    Exit;
  end;

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
            Navigates into directories/archives or goes back if ".." is clicked
 ------------------------------------------------------------------}
procedure TForm1.ListViewFilesDblClick(Sender: TObject);
var
  RawItem, SelectedItem: string;
  IsDir, IsStaged, IsDel, IsMarkedDel: Boolean;
  P: Integer;
begin
  if FInArchiveMode then
  begin
    if not GetSelectedFileName(RawItem, SelectedItem, IsDir, IsStaged, IsDel, IsMarkedDel) then Exit;

    if (SelectedItem = '..') or (SelectedItem = '..\') then
    begin
      if FArchiveInnerPath = '' then
        CloseArchiveMode
      else
      begin
        if FArchiveInnerPath.EndsWith('/') then
          FArchiveInnerPath := Copy(FArchiveInnerPath, 1, Length(FArchiveInnerPath) - 1);
        P := LastDelimiter('/', FArchiveInnerPath);
        if P > 0 then
          FArchiveInnerPath := Copy(FArchiveInnerPath, 1, P)
        else
          FArchiveInnerPath := '';
        UpdateArchiveFileListDisplay;
      end;
      Exit;
    end;

    if IsDir then
    begin
      FArchiveInnerPath := FArchiveInnerPath + SelectedItem + '/';
      UpdateArchiveFileListDisplay;
    end
    else
    begin
      ButtonViewFileClick(Sender);
    end;
    Exit;
  end;

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
    // If it's an archive file, open it and navigate into it like a folder!
    if IsArchiveFileName(SelectedItem) and not IsDel and not IsStaged then
    begin
      OpenArchiveFromDisk(RawItem);
      Exit;
    end;

    // Double-clicking a regular file opens it in the built-in File Viewer / Hex Inspector
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
  CleanName, ChildName, HostFile, TargetFull, TargetDirFull: string;
  Info: TDiskItemInfo;
begin
  if FInArchiveMode then
  begin
    UpdateArchiveFileListDisplay;
    Exit;
  end;

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
    end;

    // 2. Entries on disk (from FDiskItemsRaw)
    for I := 0 to High(FDiskItemsRaw) do
    begin
      Info := FDiskItemsRaw[I];
      CleanName := Info.CleanName;
      TargetFull := GetFullItemPath(FCurrentPath, CleanName);
      TargetDirFull := GetFullItemPath(FCurrentPath, StringReplace(CleanName, '\', '', [rfReplaceAll]));
      if (FFilesToDelete.IndexOf(TargetFull) <> -1) or
         (FDirsToDelete.IndexOf(TargetDirFull) <> -1) then
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
      else if not Info.IsDir and IsArchiveFileName(CleanName) then
      begin
        Info.StatusStr := 'Archive';
        Info.DisplayName := CleanName;
      end
      else
      begin
        Info.StatusStr := 'OK';
        Info.DisplayName := CleanName;
      end;
      SetLength(FDiskItems, Length(FDiskItems) + 1);
      FDiskItems[High(FDiskItems)] := Info;
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
      end;
    end;
  finally
    ListViewFiles.Items.EndUpdate;
  end;

  SortDiskItems;
  RefreshListViewFromDiskItems;

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
  LoopGuard, MaxEntriesInCluster, EntryIdx: Integer;
  DirTerminated: Boolean;
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
      LoopGuard := 0;

      while True do
      begin
        Inc(LoopGuard);
        if LoopGuard > 4096 then Break;

        if CurrentCluster = 0 then
        begin
          AImageFile.Position := FRootDirStart;
          MaxEntriesInCluster := FRootEntries;
        end
        else
        begin
          if (CurrentCluster < 2) or (CurrentCluster >= $FF8) then Break;
          AImageFile.Position := FDataAreaStart + (CurrentCluster - 2) * FBytesPerCluster;
          if FBytesPerCluster >= FAT12_DIR_ENTRY_SIZE then
            MaxEntriesInCluster := FBytesPerCluster div FAT12_DIR_ENTRY_SIZE
          else
            MaxEntriesInCluster := 16;
        end;

        DirTerminated := False;
        for EntryIdx := 0 to MaxEntriesInCluster - 1 do
        begin
          if AImageFile.Read(DirEntry, FAT12_DIR_ENTRY_SIZE) <> FAT12_DIR_ENTRY_SIZE then
          begin
            DirTerminated := True;
            Break;
          end;
          if IsEndOfDirectory(DirEntry) then
          begin
            DirTerminated := True;
            Break;
          end;
          if IsDeleted(DirEntry) or not IsDirectoryEntryValid(DirEntry) then Continue;

          DirName := ExtractFileNameFromEntry(DirEntry);
          if IsDirectory(DirEntry) and (UpperCase(DirName) = UpperCase(PathParts[I])) then
          begin
            DirCluster := Word(DirEntry[$1A]) + (Word(DirEntry[$1B]) shl 8);
            DirFound := True;
            Break;
          end;
        end;

        if DirFound or DirTerminated then Break;
        if CurrentCluster = 0 then Break;
        CurrentCluster := GetNextCluster(AImageFile, CurrentCluster);
        if (CurrentCluster < 2) or (CurrentCluster >= $FF8) then Break;
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
  J, LoopGuard, MaxEntriesInCluster, EntryIdx: Integer;
  DirTerminated: Boolean;
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
    LoopGuard := 0;
    while True do
    begin
      Inc(LoopGuard);
      if LoopGuard > 4096 then Break;

      if CurrentCluster = 0 then
      begin
        ImageFile.Position := FRootDirStart;
        MaxEntriesInCluster := FRootEntries;
      end
      else
      begin
        if (CurrentCluster < 2) or (CurrentCluster >= $FF8) then Break;
        ImageFile.Position := FDataAreaStart + (CurrentCluster - 2) * FBytesPerCluster;
        if FBytesPerCluster >= FAT12_DIR_ENTRY_SIZE then
          MaxEntriesInCluster := FBytesPerCluster div FAT12_DIR_ENTRY_SIZE
        else
          MaxEntriesInCluster := 16;
      end;

      DirTerminated := False;
      for EntryIdx := 0 to MaxEntriesInCluster - 1 do
      begin
        if ImageFile.Read(DirEntry, FAT12_DIR_ENTRY_SIZE) <> FAT12_DIR_ENTRY_SIZE then
        begin
          DirTerminated := True;
          Break;
        end;
        if IsEndOfDirectory(DirEntry) then
        begin
          DirTerminated := True;
          Break;
        end;

        // Skip volume labels in file list, but store label if in root directory
        // Real volume label: bit $08 is set AND it is NOT an LFN entry ($0F)
        if ((DirEntry[11] and $08) <> 0) and ((DirEntry[11] and $0F) <> $0F) then
        begin
          if DirStartCluster = 0 then
          begin
            SetLength(FileName, 11);
            Move(DirEntry[0], FileName[1], 11);
            FVolumeLabel := Trim(FileName);
          end;
          Continue;
        end;
        if (DirEntry[11] and $0F) = $0F then Continue; // Skip LFN entries

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
        if not EntryIsDir and IsArchiveFileName(FileName) then
          FDiskItemsRaw[High(FDiskItemsRaw)].StatusStr := 'Archive'
        else
          FDiskItemsRaw[High(FDiskItemsRaw)].StatusStr := 'OK';
      end;

      if DirTerminated then Break;
      if DirStartCluster = 0 then Break;
      CurrentCluster := GetNextCluster(ImageFile, CurrentCluster);
      if (CurrentCluster < 2) or (CurrentCluster >= $FF8) then Break;
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
  CurrentCluster, NextClust: Int64;
  DirEntry: array[0..FAT12_DIR_ENTRY_SIZE - 1] of Byte;
  FileName: string;
  LoopGuard, MaxEntriesInCluster, EntryIdx: Integer;
  DirTerminated: Boolean;
begin
  Result := -1;
  CurrentCluster := ADirCluster;
  LoopGuard := 0;
  while True do
  begin
    Inc(LoopGuard);
    if LoopGuard > 4096 then Break;

    // Position to the current cluster and set entry limits
    if CurrentCluster = 0 then
    begin
      AImageFile.Position := FRootDirStart;
      MaxEntriesInCluster := FRootEntries;
    end
    else
    begin
      if (CurrentCluster < 2) or (CurrentCluster >= $FF8) then Break;
      AImageFile.Position := FDataAreaStart + (CurrentCluster - 2) * FBytesPerCluster;
      if FBytesPerCluster >= FAT12_DIR_ENTRY_SIZE then
        MaxEntriesInCluster := FBytesPerCluster div FAT12_DIR_ENTRY_SIZE
      else
        MaxEntriesInCluster := 16;
    end;

    DirTerminated := False;
    for EntryIdx := 0 to MaxEntriesInCluster - 1 do
    begin
      if AImageFile.Read(DirEntry, FAT12_DIR_ENTRY_SIZE) <> FAT12_DIR_ENTRY_SIZE then
      begin
        DirTerminated := True;
        Break;
      end;
      if IsEndOfDirectory(DirEntry) then
      begin
        DirTerminated := True;
        Break;
      end;
      if IsDeleted(DirEntry) or ((DirEntry[11] and $0F) = $0F) then Continue;

      FileName := ExtractFileNameFromEntry(DirEntry);
      if UpperCase(FileName) = UpperCase(AFileName) then
      begin
        Result := AImageFile.Position - FAT12_DIR_ENTRY_SIZE;
        Exit;
      end;
    end;

    if DirTerminated then Break;
    if ADirCluster = 0 then Break;
    NextClust := GetNextCluster(AImageFile, CurrentCluster);
    if (NextClust < 2) or (NextClust >= $FF8) or (NextClust = CurrentCluster) then Break;
    CurrentCluster := NextClust;
  end;
end;

{------------------------------------------------------------------
  Function: FindDeletedFileInDirectory
  Purpose:  Finds a deleted file in a directory and returns its entry position
 ------------------------------------------------------------------}
function TForm1.FindDeletedFileInDirectory(const AImageFile: TFileStream; const ADirCluster: Int64; const AFileName: string): Int64;
var
  CurrentCluster, NextClust: Int64;
  DirEntry: array[0..FAT12_DIR_ENTRY_SIZE - 1] of Byte;
  EntryName, SearchName: string;
  LoopGuard, MaxEntriesInCluster, EntryIdx: Integer;
  DirTerminated: Boolean;
begin
  Result := -1;
  CurrentCluster := ADirCluster;
  SearchName := UpperCase(AFileName);
  LoopGuard := 0;
  while True do
  begin
    Inc(LoopGuard);
    if LoopGuard > 4096 then Break;

    if CurrentCluster = 0 then
    begin
      AImageFile.Position := FRootDirStart;
      MaxEntriesInCluster := FRootEntries;
    end
    else
    begin
      if (CurrentCluster < 2) or (CurrentCluster >= $FF8) then Break;
      AImageFile.Position := FDataAreaStart + (CurrentCluster - 2) * FBytesPerCluster;
      if FBytesPerCluster >= FAT12_DIR_ENTRY_SIZE then
        MaxEntriesInCluster := FBytesPerCluster div FAT12_DIR_ENTRY_SIZE
      else
        MaxEntriesInCluster := 16;
    end;

    DirTerminated := False;
    for EntryIdx := 0 to MaxEntriesInCluster - 1 do
    begin
      if AImageFile.Read(DirEntry, FAT12_DIR_ENTRY_SIZE) <> FAT12_DIR_ENTRY_SIZE then
      begin
        DirTerminated := True;
        Break;
      end;
      if IsEndOfDirectory(DirEntry) then
      begin
        DirTerminated := True;
        Break;
      end;
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

    if DirTerminated then Break;
    if ADirCluster = 0 then Break;
    NextClust := GetNextCluster(AImageFile, CurrentCluster);
    if (NextClust < 2) or (NextClust >= $FF8) or (NextClust = CurrentCluster) then Break;
    CurrentCluster := NextClust;
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
  CurrentCluster, LastCluster, NextClust: Int64;
  DirEntry: array[0..FAT12_DIR_ENTRY_SIZE - 1] of Byte;
  FreeList: TStringList;
  NewSubCluster: Word;
  ClusterBuffer: array of Byte;
  LoopGuard, MaxEntriesInCluster, EntryIdx: Integer;
begin
  Result := -1;
  CurrentCluster := ADirCluster;
  LastCluster := ADirCluster;
  LoopGuard := 0;

  while True do
  begin
    Inc(LoopGuard);
    if LoopGuard > 4096 then Break;

    // Position to the current cluster and enforce strict entry bounds
    if CurrentCluster = 0 then
    begin
      AImageFile.Position := FRootDirStart;
      MaxEntriesInCluster := FRootEntries;
    end
    else
    begin
      if (CurrentCluster < 2) or (CurrentCluster >= $FF8) then Break;
      AImageFile.Position := FDataAreaStart + (CurrentCluster - 2) * FBytesPerCluster;
      if FBytesPerCluster >= FAT12_DIR_ENTRY_SIZE then
        MaxEntriesInCluster := FBytesPerCluster div FAT12_DIR_ENTRY_SIZE
      else
        MaxEntriesInCluster := 16;
    end;

    // Search for a free entry within this cluster only
    for EntryIdx := 0 to MaxEntriesInCluster - 1 do
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
    NextClust := GetNextCluster(AImageFile, CurrentCluster);
    if (NextClust < 2) or (NextClust >= $FF8) or (NextClust = CurrentCluster) then Break;
    CurrentCluster := NextClust;
  end;

  // If a subdirectory ran out of entries after checking all its clusters, allocate a new cluster to expand it
  if (ADirCluster <> 0) and (LastCluster >= 2) then
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

  Sanitized := SanitizeDOSName(ExtractFileName(AFileName), False);

  // Check if a file with this sanitized 8.3 name already exists in the destination
  DirEntryPos := FindFileInDirectory(AImageFile, DirCluster, Sanitized);
  if DirEntryPos <> -1 then
  begin
    // Replace existing file cleanly by freeing its clusters first (prevents duplicate names and leaks)
    DeleteFileFromImage(AImageFile, DirEntryPos, Sanitized);
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

procedure TForm1.FreeClusterChain(const AImageFile: TFileStream; StartCluster: Word);
var
  Curr, NextClust: Word;
  MaxLoops: Integer;
begin
  Curr := StartCluster;
  MaxLoops := 0;
  while (Curr >= 2) and (Curr < $FF8) and (MaxLoops < 8192) do
  begin
    Inc(MaxLoops);
    NextClust := ReadFAT12Entry(AImageFile, Curr);
    WriteFAT12EntryAllCopies(AImageFile, Curr, FAT12_FREE);
    if (NextClust = Curr) or (NextClust < 2) or (NextClust >= $FF8) then Break;
    Curr := NextClust;
  end;
end;

procedure TForm1.DeleteDirectoryRecursive(const AImageFile: TFileStream; const ADirCluster: Word);
var
  CurrCluster, NextClust: Word;
  DirEntry: array[0..FAT12_DIR_ENTRY_SIZE - 1] of Byte;
  EntryIdx, MaxEntries: Integer;
  ChildClust: Word;
  EntryName: string;
  LoopGuard: Integer;
begin
  if (ADirCluster < 2) or (ADirCluster >= $FF8) then Exit;

  CurrCluster := ADirCluster;
  LoopGuard := 0;
  while (CurrCluster >= 2) and (CurrCluster < $FF8) do
  begin
    Inc(LoopGuard);
    if LoopGuard > 4096 then Break;

    AImageFile.Position := FDataAreaStart + (CurrCluster - 2) * FBytesPerCluster;
    if FBytesPerCluster >= FAT12_DIR_ENTRY_SIZE then
      MaxEntries := FBytesPerCluster div FAT12_DIR_ENTRY_SIZE
    else
      MaxEntries := 16;

    for EntryIdx := 0 to MaxEntries - 1 do
    begin
      if AImageFile.Read(DirEntry, FAT12_DIR_ENTRY_SIZE) <> FAT12_DIR_ENTRY_SIZE then Break;
      if IsEndOfDirectory(DirEntry) then Break;
      if IsDeleted(DirEntry) or ((DirEntry[11] and $08) <> 0) then Continue;

      EntryName := ExtractFileNameFromEntry(DirEntry);
      if (EntryName = '.') or (EntryName = '..') then Continue;

      ChildClust := Word(DirEntry[26]) or (Word(DirEntry[27]) shl 8);
      if ChildClust >= 2 then
      begin
        if IsDirectory(DirEntry) then
          DeleteDirectoryRecursive(AImageFile, ChildClust)
        else
          FreeClusterChain(AImageFile, ChildClust);
      end;
    end;

    NextClust := ReadFAT12Entry(AImageFile, CurrCluster);
    if (NextClust < 2) or (NextClust >= $FF8) or (NextClust = CurrCluster) then Break;
    CurrCluster := NextClust;
  end;

  // Free directory's own cluster chain
  FreeClusterChain(AImageFile, ADirCluster);
end;

{------------------------------------------------------------------
  Procedure: DeleteFileFromImage
  Purpose:  Deletes a file or directory from the image and updates the FAT
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
begin
  StartCluster := 0;
  // Read the directory entry
  AImageFile.Position := ADirEntryPos;
  AImageFile.Read(DirEntry, FAT12_DIR_ENTRY_SIZE);

  // Extract starting cluster BEFORE modifying or clearing DirEntry
  Move(DirEntry[26], StartCluster, 2);

  if StartCluster >= 2 then
  begin
    if IsDirectory(DirEntry) then
      DeleteDirectoryRecursive(AImageFile, StartCluster)
    else
      FreeClusterChain(AImageFile, StartCluster);
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
  BpbScore, ScanScore: Integer;
  ScannedOffset: Int64;
  ScannedEntries: Word;
begin
  AImageFile.Position := 0;
  FillChar(BootSector[0], FAT12_BOOT_SECTOR_SIZE, 0);
  if AImageFile.Size >= FAT12_BOOT_SECTOR_SIZE then
    AImageFile.Read(BootSector, FAT12_BOOT_SECTOR_SIZE);

  // Parse boot sector
  FBytesPerSector := Word(BootSector[$0B]) + (Word(BootSector[$0C]) shl 8);
  if FBytesPerSector <> 512 then FBytesPerSector := 512;
  FSectorsPerCluster := BootSector[$0D];
  if not (FSectorsPerCluster in [1, 2, 4, 8]) then FSectorsPerCluster := 2;
  ReservedSectors := Word(BootSector[$0E]) + (Word(BootSector[$0F]) shl 8);
  if (ReservedSectors < 1) or (ReservedSectors > 32) then ReservedSectors := 1;
  FNumberOfFATs := BootSector[$10];
  if not (FNumberOfFATs in [1, 2]) then FNumberOfFATs := 2;
  RootEntries := Word(BootSector[$11]) + (Word(BootSector[$12]) shl 8);
  if (RootEntries < 16) or (RootEntries > 1024) then RootEntries := 112;
  FRootEntries := RootEntries;
  TotalSectors := Word(BootSector[$13]) + (Word(BootSector[$14]) shl 8);
  if TotalSectors = 0 then
    TotalSectors := AImageFile.Size div FBytesPerSector;
  FSectorsPerFAT := Word(BootSector[$16]) + (Word(BootSector[$17]) shl 8);
  if (FSectorsPerFAT < 1) or (FSectorsPerFAT > 32) then FSectorsPerFAT := 5;

  // Read Atari ST 24-bit disk serial number at offset $08..$0A of boot sector
  FDiskSerial := (LongWord(BootSector[8]) shl 16) or
                 (LongWord(BootSector[9]) shl 8) or
                 LongWord(BootSector[10]);

  // Read and validate disk geometry
  if not (FSides in [1, 2]) then
  begin
    FSides := Word(BootSector[$1A]) + (Word(BootSector[$1B]) shl 8);
    if not (FSides in [1, 2]) then
    begin
      if TotalSectors <= 720 then FSides := 1 else FSides := 2;
    end;
  end;

  if not (FSectorsPerTrack in [8..24]) then
  begin
    FSectorsPerTrack := Word(BootSector[$18]) + (Word(BootSector[$19]) shl 8);
    if not (FSectorsPerTrack in [8..24]) then
    begin
      if (TotalSectors > 0) and (FSides > 0) then
      begin
        if TotalSectors mod (FSides * 9) = 0 then FSectorsPerTrack := 9
        else if TotalSectors mod (FSides * 10) = 0 then FSectorsPerTrack := 10
        else if TotalSectors mod (FSides * 11) = 0 then FSectorsPerTrack := 11
        else if TotalSectors mod (FSides * 18) = 0 then FSectorsPerTrack := 18
        else FSectorsPerTrack := 9;
      end
      else
        FSectorsPerTrack := 9;
    end;
  end;

  if (FTracks < 40) or (FTracks > 85) then
  begin
    if (FSides * FSectorsPerTrack > 0) and (TotalSectors > 0) then
      FTracks := TotalSectors div (FSides * FSectorsPerTrack)
    else
      FTracks := 80;
    if (FTracks < 40) or (FTracks > 85) then
      FTracks := 80;
  end;

  // Standard BPB calculation
  FSingleFATSize := FSectorsPerFAT * FBytesPerSector;
  FFatTableStart := ReservedSectors * FBytesPerSector;
  FFatTableSize := FNumberOfFATs * FSingleFATSize;
  FRootDirStart := (ReservedSectors + (FNumberOfFATs * FSectorsPerFAT)) * FBytesPerSector;

  // Validate BPB root directory score
  BpbScore := -1;
  if (FRootDirStart >= 512) and (FRootDirStart + (Int64(FRootEntries) * FAT12_DIR_ENTRY_SIZE) <= AImageFile.Size) and
     (ReservedSectors in [1..10]) and (FNumberOfFATs in [1..2]) and (FSectorsPerFAT in [1..32]) and
     (FRootEntries >= 16) and (FRootEntries <= 1024) and ((FRootEntries mod 16) = 0) then
  begin
    BpbScore := ScoreDirectoryAtOffset(AImageFile, FRootDirStart, Min(FRootEntries, 16));
  end;

  // Fallback sliding-window discovery if BPB is non-compliant, missing, or has 0 valid files while another sector does
  if (BpbScore <= 0) or (FRootEntries = 0) or (FSectorsPerFAT = 0) or (ReservedSectors = 0) then
  begin
    ScanScore := DiscoverDirectoryContent(AImageFile, ScannedOffset, ScannedEntries);
    if (ScanScore > BpbScore) and (ScanScore >= 1) then
    begin
      FRootDirStart := ScannedOffset;
      FRootEntries := ScannedEntries;
      if FNumberOfFATs < 1 then FNumberOfFATs := 2;
      ReservedSectors := 1;
      FSectorsPerFAT := Max(1, (FRootDirStart - 512) div (FNumberOfFATs * FBytesPerSector));
      FSingleFATSize := FSectorsPerFAT * FBytesPerSector;
      FFatTableStart := ReservedSectors * FBytesPerSector;
      FFatTableSize := FNumberOfFATs * FSingleFATSize;
    end;
  end;

  RootDirSectors := ((Int64(FRootEntries) * FAT12_DIR_ENTRY_SIZE) + (FBytesPerSector - 1)) div FBytesPerSector;
  FDataAreaStart := FRootDirStart + (RootDirSectors * FBytesPerSector);

  // If BPB was non-compliant, run subdirectory cluster consensus to refine dataAreaStart and sectorsPerCluster
  if BpbScore <= 0 then
  begin
    DeduceSubdirectoryGeometry(AImageFile, FRootDirStart, FRootEntries, FSectorsPerCluster, FDataAreaStart);
    RootDirSectors := ((Int64(FRootEntries) * FAT12_DIR_ENTRY_SIZE) + (FBytesPerSector - 1)) div FBytesPerSector;
  end;

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

  // Clamp by FAT12 architectural limit (max 4084 data clusters before $FF7-$FFF markers)
  if FTotalClusters > 4084 then
    FTotalClusters := 4084;
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
  FreeBytes, TotalBytes: Int64;
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
      LabelFreeSpace.Caption := Format('%.0f%%', [UtilizedPercent]);

      FreeBytes := Int64(FreeClusters) * FBytesPerCluster;
      TotalBytes := Int64(FTotalClusters) * FBytesPerCluster;
      if (StatusBar1 <> nil) and (StatusBar1.Panels.Count >= 4) then
        StatusBar1.Panels[2].Text := 'Free: ' + FormatSizeStr(FreeBytes, False) + ' / ' + FormatSizeStr(TotalBytes, False);
    end
    else
    begin
      ProgressBarFreeSpace.Position := 0;
      LabelFreeSpace.Caption := '0%';
      if (StatusBar1 <> nil) and (StatusBar1.Panels.Count >= 4) then
        StatusBar1.Panels[2].Text := 'Free: 0 B';
    end;
  finally
    ImageFile.Free;
  end;
  UpdateStatusBar;
end;


{------------------------------------------------------------------
  Function: ScoreDirectoryAtOffset
  Purpose:  Scores a potential directory table at a byte offset in an ST image.
            Valid entries with GEMDOS/MS-DOS 8.3 names and cluster <= 4000
            yield positive points. Corrupted entries, control chars, or null
            padding yield negative score (-1).
 ------------------------------------------------------------------}
function TForm1.ScoreDirectoryAtOffset(const AStream: TStream; AOffset: Int64; AMaxEntries: Integer): Integer;
var
  EntriesToCheck, I, J: Integer;
  Entry: array[0..31] of Byte;
  FirstByte, Attr: Byte;
  IsValidFirst, HasInvalidChars: Boolean;
  StartClust: Word;
begin
  Result := -1;
  if AOffset + (Int64(AMaxEntries) * 32) > AStream.Size then
    EntriesToCheck := (AStream.Size - AOffset) div 32
  else
    EntriesToCheck := AMaxEntries;

  if EntriesToCheck <= 0 then Exit;

  Result := 0;
  AStream.Position := AOffset;

  for I := 0 to EntriesToCheck - 1 do
  begin
    if AStream.Read(Entry, 32) <> 32 then Break;
    FirstByte := Entry[0];
    if FirstByte = 0 then Continue;

    // Check valid first byte
    IsValidFirst := (FirstByte = $E5) or (FirstByte = $05) or
                    ((FirstByte >= Ord('A')) and (FirstByte <= Ord('Z'))) or
                    ((FirstByte >= Ord('a')) and (FirstByte <= Ord('z'))) or
                    ((FirstByte >= Ord('0')) and (FirstByte <= Ord('9'))) or
                    (Char(FirstByte) in ['_', '^', '$', '~', '!', '#', '%', '&', '-', '{', '}', '(', ')', '@', '''']) or
                    ((FirstByte >= 128) and (FirstByte <= 254) and (FirstByte <> $F8) and (FirstByte <> $F9));

    if not IsValidFirst then Exit(-1); // Garbage penalty

    Attr := Entry[11];
    if (Attr and $C0) <> 0 then Exit(-1); // Invalid attribute bits

    // Check 11 characters of 8.3 name: no nulls, no control characters
    HasInvalidChars := False;
    for J := 0 to 10 do
    begin
      if Entry[J] = 0 then
      begin
        HasInvalidChars := True;
        Break;
      end;
      if (Entry[J] < 32) and (Entry[J] <> 5) then
      begin
        HasInvalidChars := True;
        Break;
      end;
    end;
    if HasInvalidChars then Exit(-1);

    StartClust := Word(Entry[26]) or (Word(Entry[27]) shl 8);
    if StartClust > 4000 then Exit(-1);

    Inc(Result);
  end;
end;

{------------------------------------------------------------------
  Function: DiscoverDirectoryContent
  Purpose:  Scans through sectors 1..30 (offset 512..15360) using a sliding
            window to find the root directory with the highest validity score.
 ------------------------------------------------------------------}
function TForm1.DiscoverDirectoryContent(const AStream: TStream; out ARootDirStart: Int64; out ARootDirEntries: Word): Integer;
var
  Offset: Int64;
  BestScore, Score: Integer;
  BestOffset: Int64;
  ExpectedEntries: Word;
begin
  if AStream.Size <= 368640 then
  begin
    BestOffset := 2560; // Sector 5 (360KB single-sided default)
    ExpectedEntries := 64;
  end
  else
  begin
    BestOffset := 3584; // Sector 7 (720KB double-sided default)
    ExpectedEntries := 112;
  end;

  BestScore := 0;

  // Scan through first 30 sectors (offsets 512..15360)
  Offset := 512;
  while (Offset < 15360) and (Offset + 512 <= AStream.Size) do
  begin
    Score := ScoreDirectoryAtOffset(AStream, Offset, 16);
    if Score > BestScore then
    begin
      BestScore := Score;
      BestOffset := Offset;
      if Offset < 3000 then
        ExpectedEntries := 64
      else
        ExpectedEntries := 112;
    end;
    Inc(Offset, 512);
  end;

  ARootDirStart := BestOffset;
  ARootDirEntries := ExpectedEntries;
  Result := BestScore;
end;

{------------------------------------------------------------------
  Procedure: DeduceSubdirectoryGeometry
  Purpose:  Scans the disk for '.' and '..' directory headers to reach a
            consensus vote on the true DataAreaStart and SectorsPerCluster.
 ------------------------------------------------------------------}
procedure TForm1.DeduceSubdirectoryGeometry(const AStream: TStream; ARootDirStart: Int64; var ARootDirEntries: Word;
  var ASectorsPerCluster: Byte; var ADataAreaStart: Int64);
type
  TSubDirInfo = record
    Cluster: Word;
    Offset: Int64;
  end;
var
  SubDirs: array of TSubDirInfo;
  SubDirCount: Integer;
  Offset: Int64;
  Buf: array[0..63] of Byte;
  StartClust: Word;
  TestSPC: Integer;
  ImpliedDAS: Int64;
  BestSPC: Integer;
  BestDAS: Int64;
  MaxAgreement, CurAgreement: Integer;
  I, J: Integer;
  RootDirSectors: Int64;
begin
  SubDirCount := 0;
  SetLength(SubDirs, 0);

  // Scan for '.' and '..' directory signature
  Offset := 512;
  while (Offset < AStream.Size - 64) and (Offset < 200000) do
  begin
    AStream.Position := Offset;
    if AStream.Read(Buf, 64) <> 64 then Break;

    // Check entry 0: name is '.', attr is $10 (directory)
    // and entry 1: name is '..', attr is $10 (directory)
    if (Buf[0] = $2E) and (Buf[1] = $20) and (Buf[11] = $10) and
       (Buf[32] = $2E) and (Buf[33] = $2E) and (Buf[43] = $10) then
    begin
      StartClust := Word(Buf[26]) or (Word(Buf[27]) shl 8);
      if (StartClust >= 2) and (StartClust < 4096) then
      begin
        Inc(SubDirCount);
        SetLength(SubDirs, SubDirCount);
        SubDirs[SubDirCount - 1].Cluster := StartClust;
        SubDirs[SubDirCount - 1].Offset := Offset;
      end;
    end;
    Inc(Offset, 512);
  end;

  if SubDirCount > 0 then
  begin
    BestSPC := ASectorsPerCluster;
    BestDAS := ADataAreaStart;
    MaxAgreement := 0;

    for TestSPC in [1, 2, 4] do
    begin
      for I := 0 to SubDirCount - 1 do
      begin
        ImpliedDAS := SubDirs[I].Offset - Int64(SubDirs[I].Cluster - 2) * TestSPC * 512;
        if (ImpliedDAS >= ARootDirStart) and ((ImpliedDAS mod 512) = 0) then
        begin
          CurAgreement := 0;
          for J := 0 to SubDirCount - 1 do
          begin
            if SubDirs[J].Offset - Int64(SubDirs[J].Cluster - 2) * TestSPC * 512 = ImpliedDAS then
              Inc(CurAgreement);
          end;
          if CurAgreement > MaxAgreement then
          begin
            MaxAgreement := CurAgreement;
            BestSPC := TestSPC;
            BestDAS := ImpliedDAS;
          end;
        end;
      end;
    end;

    ASectorsPerCluster := BestSPC;
    ADataAreaStart := BestDAS;
    if ADataAreaStart > ARootDirStart then
    begin
      RootDirSectors := (ADataAreaStart - ARootDirStart) div 512;
      ARootDirEntries := RootDirSectors * 16;
    end;
  end;
end;

{------------------------------------------------------------------
  Function: CheckAndDeinterleaveSingleSidedST
  Purpose:  Detects single-sided ST images (sides=1 in BPB, or single-sided
            sector counts, or faked double-sided containers) that were dumped
            interleaved in a double-sided drive. Extracts Head 0 from each cylinder
            to produce a clean single-sided ST disk image where all directories
            and files are directly readable.
 ------------------------------------------------------------------}
function TForm1.CheckAndDeinterleaveSingleSidedST(const ASourceFile: string; out ADeinterleavedFile: string): Boolean;
var
  InStream, OutStream: TFileStream;
  BootSector: array[0..FAT12_BOOT_SECTOR_SIZE - 1] of Byte;
  BytesPerSector, SectorsPerTrack, Sides, TotalSectors: Word;
  TrackSize, CylinderSize, TrackCount, T: Integer;
  TrackBuf: TBytes;
  TempFile: string;
  ShouldDeinterleave: Boolean;
  UseSpt: Integer;
  AsIsRootStart: Int64;
  AsIsRootEntries: Word;
  AsIsScore: Integer;
  CandSPT, BestSPT, CandScore, BestCandScore: Integer;
  CandStream: TMemoryStream;
begin
  Result := False;
  ADeinterleavedFile := '';
  if not FileExists(ASourceFile) then Exit;

  InStream := TFileStream.Create(ASourceFile, fmOpenRead or fmShareDenyNone);
  try
    // Only candidates with double-sided container sizes (>= 500KB) can be faked double-sided containers
    if InStream.Size < 500000 then Exit;

    FillChar(BootSector[0], FAT12_BOOT_SECTOR_SIZE, 0);
    if InStream.Size >= FAT12_BOOT_SECTOR_SIZE then
      InStream.Read(BootSector, FAT12_BOOT_SECTOR_SIZE);

    BytesPerSector := Word(BootSector[$0B]) + (Word(BootSector[$0C]) shl 8);
    if BytesPerSector <> 512 then BytesPerSector := 512;
    SectorsPerTrack := Word(BootSector[$18]) + (Word(BootSector[$19]) shl 8);
    Sides := Word(BootSector[$1A]) + (Word(BootSector[$1B]) shl 8);
    TotalSectors := Word(BootSector[$13]) + (Word(BootSector[$14]) shl 8);

    ShouldDeinterleave := False;
    UseSpt := 9;

    // Case 1: BPB specifically says 1 side and total sectors <= 820
    if (Sides = 1) and (SectorsPerTrack in [9..11]) and (TotalSectors > 0) and (TotalSectors <= 820) then
    begin
      ShouldDeinterleave := True;
      UseSpt := SectorsPerTrack;
    end
    else
    begin
      // Case 2: Heuristic fallback from diskUtils.ts
      // Only test deinterleaving if the as-is layout is unreadable (score <= 0)
      AsIsScore := DiscoverDirectoryContent(InStream, AsIsRootStart, AsIsRootEntries);
      if AsIsScore <= 0 then
      begin
        BestCandScore := 0;
        BestSPT := 9;
        for CandSPT in [9, 10, 11] do
        begin
          TrackSize := CandSPT * BytesPerSector;
          CylinderSize := TrackSize * 2;
          TrackCount := InStream.Size div CylinderSize;
          if TrackCount >= 40 then
          begin
            CandStream := TMemoryStream.Create;
            try
              SetLength(TrackBuf, TrackSize);
              // Read first 10 tracks to test directory area
              for T := 0 to Min(TrackCount - 1, 10) do
              begin
                InStream.Position := Int64(T) * CylinderSize;
                InStream.ReadBuffer(TrackBuf[0], TrackSize);
                CandStream.WriteBuffer(TrackBuf[0], TrackSize);
              end;
              CandScore := DiscoverDirectoryContent(CandStream, AsIsRootStart, AsIsRootEntries);
              if CandScore > BestCandScore then
              begin
                BestCandScore := CandScore;
                BestSPT := CandSPT;
              end;
            finally
              CandStream.Free;
            end;
          end;
        end;

        if (BestCandScore > 0) and (BestCandScore > AsIsScore) then
        begin
          ShouldDeinterleave := True;
          UseSpt := BestSPT;
        end;
      end;
    end;

    if ShouldDeinterleave then
    begin
      TrackSize := UseSpt * BytesPerSector;
      CylinderSize := TrackSize * 2;
      TrackCount := InStream.Size div CylinderSize;
      if TrackCount >= 40 then
      begin
        TempFile := GetTempFile(GetTempDir, 'st-deint-') + '.st';
        OutStream := TFileStream.Create(TempFile, fmCreate);
        try
          SetLength(TrackBuf, TrackSize);
          for T := 0 to TrackCount - 1 do
          begin
            InStream.Position := Int64(T) * CylinderSize;
            InStream.ReadBuffer(TrackBuf[0], TrackSize);
            OutStream.WriteBuffer(TrackBuf[0], TrackSize);
          end;

          // Ensure boot sector in deinterleaved image reflects single-sided geometry
          if OutStream.Size >= FAT12_BOOT_SECTOR_SIZE then
          begin
            OutStream.Position := 0;
            OutStream.Read(BootSector, FAT12_BOOT_SECTOR_SIZE);
            BootSector[$1A] := 1; // 1 side
            BootSector[$1B] := 0;
            BootSector[$18] := UseSpt and $FF;
            BootSector[$19] := (UseSpt shr 8) and $FF;
            TotalSectors := Word(BootSector[$13]) + (Word(BootSector[$14]) shl 8);
            if (TotalSectors = 0) or (TotalSectors > 820) then
            begin
              TotalSectors := UseSpt * 80;
              BootSector[$13] := TotalSectors and $FF;
              BootSector[$14] := (TotalSectors shr 8) and $FF;
            end;
            OutStream.Position := 0;
            OutStream.Write(BootSector, FAT12_BOOT_SECTOR_SIZE);
          end;

          ADeinterleavedFile := TempFile;
          Result := True;
        finally
          OutStream.Free;
        end;
      end;
    end;
  finally
    InStream.Free;
  end;
end;

{------------------------------------------------------------------
  Function: LoadDiskImage
  Purpose:  Loads a disk image (.st or .msa), handling MSA decompression if needed
 ------------------------------------------------------------------}
function TForm1.LoadDiskImage(const AFileName: string): Boolean;
var
  Ext: string;
  TempFile, DeintTempFile: string;
  VirusThreatName: string;
  IsBootExec: Boolean;
  Spt, Sds, StTrack, EndTrack: Integer;
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
    if GetMSAInfo(AFileName, Spt, Sds, StTrack, EndTrack) then
    begin
      FSectorsPerTrack := Spt;
      FSides := Sds;
      FTracks := (EndTrack - StTrack) + 1;
    end;

    TempFile := GetTempFile(GetTempDir, 'st-msa-') + '.st';
    if not UnpackMSAToST(AFileName, TempFile) then
    begin
      ShowMessage('Error: Failed to unpack MSA disk image.' + sLineBreak +
        'Please verify that the file is a valid Magic Shadow Archiver image.');
      Exit;
    end;
    FIsMSAImage := True;
    FIsDIMImage := False;
    FOriginalMSAPath := AFileName;
    FOriginalDIMPath := '';
    FOriginalSTPath := '';

    if CheckAndDeinterleaveSingleSidedST(TempFile, DeintTempFile) then
    begin
      SysUtils.DeleteFile(TempFile);
      FTempSTPath := DeintTempFile;
      FImagePath := DeintTempFile;
    end
    else
    begin
      FTempSTPath := TempFile;
      FImagePath := TempFile;
    end;
  end
  else if (Ext = '.dim') or IsDIMFile(AFileName) then
  begin
    if GetDIMInfo(AFileName, Spt, Sds, StTrack, EndTrack) then
    begin
      FSectorsPerTrack := Spt;
      FSides := Sds;
      FTracks := (EndTrack - StTrack) + 1;
    end;

    TempFile := GetTempFile(GetTempDir, 'st-dim-') + '.st';
    if not UnpackDIMToST(AFileName, TempFile) then
    begin
      ShowMessage('Error: Failed to unpack FastCopy DIM disk image.' + sLineBreak +
        'Please verify that the file is a valid FastCopy Pro disk image.');
      Exit;
    end;
    FIsDIMImage := True;
    FIsMSAImage := False;
    FOriginalDIMPath := AFileName;
    FOriginalMSAPath := '';
    FOriginalSTPath := '';

    if CheckAndDeinterleaveSingleSidedST(TempFile, DeintTempFile) then
    begin
      SysUtils.DeleteFile(TempFile);
      FTempSTPath := DeintTempFile;
      FImagePath := DeintTempFile;
    end
    else
    begin
      FTempSTPath := TempFile;
      FImagePath := TempFile;
    end;
  end
  else
  begin
    FIsMSAImage := False;
    FIsDIMImage := False;
    FOriginalMSAPath := '';
    FOriginalDIMPath := '';
    if CheckAndDeinterleaveSingleSidedST(AFileName, TempFile) then
    begin
      FOriginalSTPath := AFileName;
      FTempSTPath := TempFile;
      FImagePath := TempFile;
    end
    else
    begin
      FOriginalSTPath := '';
      FTempSTPath := '';
      FImagePath := AFileName;
    end;
  end;

  ReadSTImage(FImagePath, FCurrentPath);
  UpdateFileListDisplay;
  UpdateFreeSpaceDisplay;

  // Update window caption to reflect loaded file
  if FOriginalZipPath <> '' then
    Self.Caption := 'Atari .ST Image File Manager - ' + ExtractFileName(FOriginalZipPath) + ' [' + ExtractFileName(FTempZipExtractedPath) + ']'
  else if FIsMSAImage and (FOriginalMSAPath <> '') then
    Self.Caption := 'Atari .ST Image File Manager - ' + ExtractFileName(FOriginalMSAPath)
  else if FIsDIMImage and (FOriginalDIMPath <> '') then
    Self.Caption := 'Atari .ST Image File Manager - ' + ExtractFileName(FOriginalDIMPath)
  else if FOriginalSTPath <> '' then
    Self.Caption := 'Atari .ST Image File Manager - ' + ExtractFileName(FOriginalSTPath)
  else
    Self.Caption := 'Atari .ST Image File Manager - ' + ExtractFileName(FImagePath);

  // Real-time Anti-Virus Boot Sector check
  if QuickCheckBootSector(FImagePath, VirusThreatName, IsBootExec) then
  begin
    if MessageDlg('Atari ST Anti-Virus Guard',
      Format('WARNING: Virus threat detected in Boot Sector: %s!' + sLineBreak + sLineBreak +
             'Would you like to open the Anti-Virus Scanner now to inspect and disinfect the disk?',
             [VirusThreatName]),
      mtWarning, [mbYes, mbNo], 0) = mrYes then
    begin
      FormVirusScanner.OpenForDisk(FImagePath, True);
    end;
  end;

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
  FileSize, BytesToRead, BytesRead, MaxPossibleSize: Int64;
  CurrCluster: Word;
  Buffer: array[0..4095] of Byte;
  LoopGuard: Integer;
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

    if (AStartCluster < 2) or (AStartCluster >= $FF8) then
      Exit;

    MaxPossibleSize := Int64(FTotalClusters) * FBytesPerCluster;
    if FileSize > MaxPossibleSize then
      FileSize := MaxPossibleSize;

    CurrCluster := AStartCluster;
    LoopGuard := 0;
    while (CurrCluster >= 2) and (CurrCluster < $FF8) and (FileSize > 0) do
    begin
      Inc(LoopGuard);
      if LoopGuard > 4096 then Break;

      BytesToRead := FBytesPerCluster;
      if FileSize < FBytesPerCluster then
        BytesToRead := FileSize;
      ImageFile.Position := FDataAreaStart + (Int64(CurrCluster) - 2) * FBytesPerCluster;
      BytesRead := ImageFile.Read(Buffer[0], BytesToRead);
      if BytesRead <= 0 then Break;
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
  CleanName, TempDir, OutPath, FullTempDir, FullOutDir: string;
  Fs, SrcFs: TFileStream;
  DummyCluster: Word;
  I: Integer;
begin
  Result := '';
  CleanName := CleanFileName(AFileName);
  CleanName := ExtractFileName(CleanName);
  if (CleanName = '') or (CleanName = '.') or (CleanName = '..') then Exit;

  TempDir := IncludeTrailingPathDelimiter(GetTempDir) + 'stimage_drag';
  ForceDirectories(TempDir);
  OutPath := IncludeTrailingPathDelimiter(TempDir) + CleanName;
  FullTempDir := IncludeTrailingPathDelimiter(ExpandFileName(TempDir));
  FullOutDir := IncludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(OutPath)));
  if not SameText(FullTempDir, FullOutDir) then Exit;

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
procedure TForm1.ExtractDiskDirectory(const AImageFile: TFileStream; const ADirCluster: Int64; const AHostPath: string; var AFileCount: Integer; var AByteCount: Int64; AVisitedDirs: TList; ADepth: Integer);
type
  TDiskEntryRec = record
    Name: string;
    IsDir: Boolean;
    StartCluster: Word;
    FileSize: Int64;
  end;
var
  Entries: array of TDiskEntryRec;
  Count, MaxEntries, EntryIndex: Integer;
  CurrentCluster: Int64;
  DirEntry: array[0..FAT12_DIR_ENTRY_SIZE - 1] of Byte;
  EntryName, SubHostDir, OutFilePath, FullHostDir, FullOutDir: string;
  I: Integer;
  OutFs: TFileStream;
  CurrFileCluster: Word;
  BytesToRead, BytesRead, FileBytesRemaining, MaxPossibleSize: Int64;
  Buffer: array[0..4095] of Byte;
  Visited: TList;
  OwnsVisited: Boolean;
  DirLoopCount, FileLoopCount: Integer;
  ReachedEnd: Boolean;
begin
  if ADepth > 32 then Exit;

  OwnsVisited := (AVisitedDirs = nil);
  if OwnsVisited then
    Visited := TList.Create
  else
    Visited := AVisitedDirs;

  try
    if Visited.IndexOf(Pointer(PtrUInt(ADirCluster))) <> -1 then Exit;
    Visited.Add(Pointer(PtrUInt(ADirCluster)));

    if not ForceDirectories(AHostPath) then Exit;

    Count := 0;
    SetLength(Entries, 0);

    CurrentCluster := ADirCluster;
    DirLoopCount := 0;
    ReachedEnd := False;
    while not ReachedEnd do
    begin
      Inc(DirLoopCount);
      if DirLoopCount > 4096 then Break;

      if CurrentCluster = 0 then
      begin
        AImageFile.Position := FRootDirStart;
        MaxEntries := FRootEntries;
      end
      else
      begin
        if (CurrentCluster < 2) or (CurrentCluster >= $FF8) then Break;
        AImageFile.Position := FDataAreaStart + (CurrentCluster - 2) * FBytesPerCluster;
        MaxEntries := FBytesPerCluster div FAT12_DIR_ENTRY_SIZE;
      end;

      EntryIndex := 0;
      while EntryIndex < MaxEntries do
      begin
        Inc(EntryIndex);
        if AImageFile.Read(DirEntry, FAT12_DIR_ENTRY_SIZE) <> FAT12_DIR_ENTRY_SIZE then
        begin
          ReachedEnd := True;
          Break;
        end;
        if IsEndOfDirectory(DirEntry) then
        begin
          ReachedEnd := True;
          Break;
        end;
        if IsDeleted(DirEntry) or ((DirEntry[11] and $08) <> 0) then Continue;

        EntryName := ExtractFileNameFromEntry(DirEntry);
        if (EntryName = '') or (EntryName = '.') or (EntryName = '..') then Continue;

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

    FullHostDir := IncludeTrailingPathDelimiter(ExpandFileName(AHostPath));

    for I := 0 to Count - 1 do
    begin
      if Entries[I].IsDir then
      begin
        if (Entries[I].StartCluster >= 2) and (Entries[I].StartCluster < $FF8) then
        begin
          SubHostDir := IncludeTrailingPathDelimiter(AHostPath) + Entries[I].Name;
          FullOutDir := IncludeTrailingPathDelimiter(ExpandFileName(SubHostDir));
          if StartsText(FullHostDir, FullOutDir) then
            ExtractDiskDirectory(AImageFile, Entries[I].StartCluster, SubHostDir, AFileCount, AByteCount, Visited, ADepth + 1);
        end;
      end
      else
      begin
        OutFilePath := IncludeTrailingPathDelimiter(AHostPath) + Entries[I].Name;
        FullOutDir := IncludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(OutFilePath)));
        if not StartsText(FullHostDir, FullOutDir) then Continue;

        OutFs := TFileStream.Create(OutFilePath, fmCreate);
        try
          FileBytesRemaining := Entries[I].FileSize;
          MaxPossibleSize := Int64(FTotalClusters) * FBytesPerCluster;
          if FileBytesRemaining > MaxPossibleSize then
            FileBytesRemaining := MaxPossibleSize;

          CurrFileCluster := Entries[I].StartCluster;
          FileLoopCount := 0;
          while (CurrFileCluster >= 2) and (CurrFileCluster < $FF8) and (FileBytesRemaining > 0) do
          begin
            Inc(FileLoopCount);
            if FileLoopCount > 4096 then Break;

            BytesToRead := FBytesPerCluster;
            if FileBytesRemaining < FBytesPerCluster then
              BytesToRead := FileBytesRemaining;
            AImageFile.Position := FDataAreaStart + (Int64(CurrFileCluster) - 2) * FBytesPerCluster;
            BytesRead := AImageFile.Read(Buffer[0], BytesToRead);
            if BytesRead <= 0 then Break;
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
  finally
    if OwnsVisited then
      Visited.Free;
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
      if (Ext = '.st') or (Ext = '.msa') or (Ext = '.dim') then
      begin
        EntrySizeKB := (UnZip.Entries[I].Size + 1023) div 1024;
        MatchingEntries.AddObject(Format('%s (%d KB)', [EntryName, EntrySizeKB]), TObject(PtrUInt(I)));
      end;
    end;

    if MatchingEntries.Count = 0 then
    begin
      ShowMessage('No Atari ST disk images (.st, .msa, .dim) were found inside this ZIP archive:' + sLineBreak +
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
    else if FOriginalSTPath <> '' then
      DefZipName := ChangeFileExt(ExtractFileName(FOriginalSTPath), '') + '.zip'
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
    if ((DirEntry[11] and $08) <> 0) and ((DirEntry[11] and $0F) <> $0F) then
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
      FIsModified := True;
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
  FIsModified := True;
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

  FIsModified := True;

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

  // Case 0: Viewing file from inside an archive container
  if FInArchiveMode then
  begin
    MemStream := TMemoryStream.Create;
    try
      Screen.Cursor := crHourGlass;
      try
        if FArchiveVault.ExtractEntryToStream(FArchiveInnerPath + CleanName, MemStream) then
        begin
          MemStream.Position := 0;
          FormFileViewer.LoadFromStream(MemStream, CleanName, 0);
          FormFileViewer.ShowModal;
        end
        else
          ShowMessage('Unable to extract file "' + CleanName + '" from archive for viewing.');
      finally
        Screen.Cursor := crDefault;
      end;
    finally
      MemStream.Free;
    end;
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
  Procedure: MenuItemVirusScannerClick
  Purpose:  Opens the Atari ST Anti-Virus Scanner & Disinfection Tool
 ------------------------------------------------------------------}
procedure TForm1.MenuItemVirusScannerClick(Sender: TObject);
begin
  if (FImagePath = '') or not FileExists(FImagePath) then
  begin
    ShowMessage('Please open an Atari ST disk image first.');
    Exit;
  end;
  FormVirusScanner.OpenForDisk(FImagePath, True);
end;

{------------------------------------------------------------------
  Procedure: ListViewFilesMouseDown
  Purpose:  Captures start coordinate for dragging files out of STIMAGE
 ------------------------------------------------------------------}
procedure TForm1.ListViewFilesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HitItem: TListItem;
begin
  if Button = mbLeft then
  begin
    FDragStartPoint := Point(X, Y);
    FDragPending := (ListViewFiles.GetItemAt(X, Y) <> nil);
  end
  else if Button = mbRight then
  begin
    HitItem := ListViewFiles.GetItemAt(X, Y);
    if HitItem <> nil then
      ListViewFiles.Selected := HitItem;
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

      if FInArchiveMode then
      begin
        if not FArchiveVault.ExtractEntryToTempFile(FArchiveInnerPath + FDiskItems[Idx].CleanName, TempHostPath) then
          Exit;
      end
      else
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
  Procedure: MenuItemConvertDIMClick
  Purpose:  Converts between uncompressed .ST and FastCopy .DIM disk images
 ------------------------------------------------------------------}
procedure TForm1.MenuItemConvertDIMClick(Sender: TObject);
var
  SrcFile, DstFile, Ext: string;
begin
  OpenDialog.Title := 'Select Disk Image to Convert (.ST or .DIM)';
  OpenDialog.Filter := 'Atari Disk Images (*.st;*.dim)|*.st;*.dim|FastCopy DIM Image (*.dim)|*.dim|ST Image (*.st)|*.st|All Files (*.*)|*.*';
  if not OpenDialog.Execute then Exit;
  SrcFile := OpenDialog.FileName;
  if not FileExists(SrcFile) then Exit;

  Ext := LowerCase(ExtractFileExt(SrcFile));
  if (Ext = '.dim') or IsDIMFile(SrcFile) then
  begin
    SaveDialog.Title := 'Convert FastCopy DIM to Standard ST Disk Image';
    SaveDialog.Filter := 'ST Image (*.st)|*.st|All Files (*.*)|*.*';
    SaveDialog.FileName := ChangeFileExt(ExtractFileName(SrcFile), '.st');
    if not SaveDialog.Execute then Exit;
    DstFile := SaveDialog.FileName;
    if FileExists(DstFile) then SysUtils.DeleteFile(DstFile);

    if UnpackDIMToST(SrcFile, DstFile) then
      ShowMessage('Conversion complete!' + sLineBreak + 'Unpacked FastCopy DIM image to: ' + DstFile)
    else
      ShowMessage('Error: Failed to convert FastCopy DIM image to ST.');
  end
  else if Ext = '.st' then
  begin
    SaveDialog.Title := 'Convert ST to FastCopy DIM Disk Image';
    SaveDialog.Filter := 'FastCopy DIM Image (*.dim)|*.dim|All Files (*.*)|*.*';
    SaveDialog.FileName := ChangeFileExt(ExtractFileName(SrcFile), '.dim');
    if not SaveDialog.Execute then Exit;
    DstFile := SaveDialog.FileName;
    if FileExists(DstFile) then SysUtils.DeleteFile(DstFile);

    if PackSTToDIM(SrcFile, DstFile, 9, 2, 80) then
      ShowMessage('Conversion complete!' + sLineBreak + 'Packed ST image to FastCopy DIM: ' + DstFile)
    else
      ShowMessage('Error: Failed to convert ST image to FastCopy DIM.');
  end
  else
  begin
    ShowMessage('Please select either a .DIM or .ST disk image file.');
  end;
end;

procedure TForm1.MenuItemFindClick(Sender: TObject);
begin
  OpenSearchDialog;
end;

procedure TForm1.MenuItemRenameClick(Sender: TObject);
begin
  RenameSelectedItem;
end;

procedure TForm1.MenuItemAttrReadOnlyClick(Sender: TObject);
begin
  ToggleSelectedAttribute($01);
end;

procedure TForm1.MenuItemAttrHiddenClick(Sender: TObject);
begin
  ToggleSelectedAttribute($02);
end;

procedure TForm1.MenuItemAttrArchiveClick(Sender: TObject);
begin
  ToggleSelectedAttribute($20);
end;

procedure TForm1.MenuItemAttrPropertiesClick(Sender: TObject);
begin
  EditSelectedAttributes;
end;

procedure TForm1.MenuItemMakePCBootClick(Sender: TObject);
begin
  MakePCCompatibleBootSector;
end;

procedure TForm1.OpenSearchDialog;
begin
  if (FImagePath = '') or (not FileExists(FImagePath)) then
  begin
    ShowMessage('Please open a disk image first before searching.');
    Exit;
  end;

  FormSearch.OnNavigateToFile := @HandleSearchNavigate;
  FormSearch.OnViewFile := @HandleSearchView;
  FormSearch.OpenForSearch(FImagePath, FCurrentPath);
end;

procedure TForm1.HandleSearchNavigate(const ADirPath, AFileName: string);
var
  I: Integer;
  TargetName: string;
begin
  if ADirPath <> FCurrentPath then
  begin
    FCurrentPath := ADirPath;
    ReadSTImage(FImagePath, FCurrentPath);
    UpdateFileListDisplay;
    UpdateStatusBar;
  end;

  TargetName := StringReplace(AFileName, '\', '', [rfReplaceAll]);
  for I := 0 to ListViewFiles.Items.Count - 1 do
  begin
    if SameText(ListViewFiles.Items[I].Caption, TargetName) or
       SameText(ListViewFiles.Items[I].Caption, TargetName + '\') then
    begin
      ListViewFiles.Items[I].Selected := True;
      ListViewFiles.Items[I].Focused := True;
      ListViewFiles.Items[I].MakeVisible(False);
      Break;
    end;
  end;
end;

procedure TForm1.HandleSearchView(const ADirPath, AFileName: string);
var
  SavedPath: string;
begin
  SavedPath := FCurrentPath;
  try
    HandleSearchNavigate(ADirPath, AFileName);
    ButtonViewFileClick(nil);
  finally
    if FCurrentPath <> SavedPath then
    begin
      FCurrentPath := SavedPath;
      ReadSTImage(FImagePath, FCurrentPath);
      UpdateFileListDisplay;
    end;
  end;
end;

procedure TForm1.RenameSelectedItem;
var
  RawName, OldName, NewName, BaseName, ExtName: string;
  IsDir, IsStaged, IsDel, IsMarkedDel: Boolean;
  ImageFile: TFileStream;
  DirCluster, DirEntryPos, ExistingPos: Int64;
  FormattedName: array[0..10] of Byte;
  DotPos, I: Integer;
begin
  if FInArchiveMode then
  begin
    ShowMessage('Renaming files inside archives is not supported.');
    Exit;
  end;

  if not GetSelectedFileName(RawName, OldName, IsDir, IsStaged, IsDel, IsMarkedDel) then
  begin
    ShowMessage('Please select a file or folder to rename.');
    Exit;
  end;

  if (OldName = '..') or (OldName = '..\') or (OldName = '.') then Exit;

  if IsDel then
  begin
    ShowMessage('Cannot rename a deleted file. Undelete it first.');
    Exit;
  end;

  // Strip trailing backslash for display and prompt
  OldName := StringReplace(OldName, '\', '', [rfReplaceAll]);

  NewName := InputBox('Rename File / Directory', 'Enter new name for "' + OldName + '":', OldName);
  NewName := Trim(NewName);
  if (NewName = '') or SameText(NewName, OldName) then Exit;

  // Sanitize and validate 8.3 filename
  for I := 1 to Length(NewName) do
  begin
    if NewName[I] in ['/', '\', ':', '*', '?', '"', '<', '>', '|', ';', ','] then
    begin
      ShowMessage('Error: Filename contains invalid characters (' + NewName[I] + ').');
      Exit;
    end;
  end;

  NewName := UpperCase(NewName);
  DotPos := Pos('.', NewName);
  if DotPos > 0 then
  begin
    BaseName := Copy(NewName, 1, DotPos - 1);
    ExtName := Copy(NewName, DotPos + 1, Length(NewName));
  end
  else
  begin
    BaseName := NewName;
    ExtName := '';
  end;

  if Length(BaseName) > 8 then BaseName := Copy(BaseName, 1, 8);
  if Length(ExtName) > 3 then ExtName := Copy(ExtName, 1, 3);

  if ExtName <> '' then
    NewName := BaseName + '.' + ExtName
  else
    NewName := BaseName;

  if SameText(NewName, OldName) then Exit;

  // Case 1: Staged file addition
  if IsStaged then
  begin
    ShowMessage('Rename the file on your computer before adding, or write changes first.');
    Exit;
  end;

  // Case 2: On-disk file or directory
  if (FImagePath = '') or (not FileExists(FImagePath)) then Exit;

  ImageFile := TFileStream.Create(FImagePath, fmOpenReadWrite or fmShareDenyNone);
  try
    GetFATDetails(ImageFile);
    DirCluster := FindDirectoryInPath(ImageFile, FCurrentPath);
    if DirCluster = -1 then
    begin
      ShowMessage(Format(ERR_DIR_NOT_FOUND, [FCurrentPath]));
      Exit;
    end;

    // Check if new name already exists
    ExistingPos := FindFileInDirectory(ImageFile, DirCluster, NewName);
    if ExistingPos <> -1 then
    begin
      ShowMessage(Format('Error: An item named "%s" already exists in this folder.', [NewName]));
      Exit;
    end;

    // Find original entry
    DirEntryPos := FindFileInDirectory(ImageFile, DirCluster, OldName);
    if DirEntryPos = -1 then
    begin
      ShowMessage(Format(ERR_FILE_NOT_FOUND, [OldName]));
      Exit;
    end;

    // Format new 8.3 name into 11 bytes space-padded
    FillChar(FormattedName, 11, $20);
    Move(PAnsiChar(BaseName)^, FormattedName[0], Length(BaseName));
    if ExtName <> '' then
      Move(PAnsiChar(ExtName)^, FormattedName[8], Length(ExtName));

    // Update directory entry on disk
    ImageFile.Position := DirEntryPos;
    ImageFile.WriteBuffer(FormattedName, 11);
    FIsModified := True;
  finally
    ImageFile.Free;
  end;

  // Refresh display
  ReadSTImage(FImagePath, FCurrentPath);
  UpdateFileListDisplay;

  // Find and select the renamed item
  for I := 0 to ListViewFiles.Items.Count - 1 do
  begin
    if SameText(ListViewFiles.Items[I].Caption, NewName) or
       SameText(ListViewFiles.Items[I].Caption, NewName + '\') then
    begin
      ListViewFiles.Items[I].Selected := True;
      ListViewFiles.Items[I].Focused := True;
      ListViewFiles.Items[I].MakeVisible(False);
      Break;
    end;
  end;

  UpdateStatusBar;
end;

procedure TForm1.ToggleSelectedAttribute(AttrBit: Byte);
var
  RawName, CleanName: string;
  IsDir, IsStaged, IsDel, IsMarkedDel: Boolean;
  ImageFile: TFileStream;
  DirCluster, DirEntryPos: Int64;
  CurrentAttr, NewAttr: Byte;
  I: Integer;
  TargetName: string;
begin
  if FInArchiveMode then
  begin
    ShowMessage('Cannot modify attributes of files inside archives.');
    Exit;
  end;

  if not GetSelectedFileName(RawName, CleanName, IsDir, IsStaged, IsDel, IsMarkedDel) then
  begin
    ShowMessage('Please select a file or directory first.');
    Exit;
  end;

  if (CleanName = '..') or (CleanName = '..\') or (CleanName = '.') then Exit;

  if IsDel then
  begin
    ShowMessage('Cannot change attributes of a deleted file.');
    Exit;
  end;

  if IsStaged then
  begin
    ShowMessage('Save the image first to change attributes of newly added files.');
    Exit;
  end;

  TargetName := StringReplace(CleanName, '\', '', [rfReplaceAll]);

  if (FImagePath = '') or (not FileExists(FImagePath)) then Exit;

  ImageFile := TFileStream.Create(FImagePath, fmOpenReadWrite or fmShareDenyNone);
  try
    GetFATDetails(ImageFile);
    DirCluster := FindDirectoryInPath(ImageFile, FCurrentPath);
    if DirCluster = -1 then
    begin
      ShowMessage(Format(ERR_DIR_NOT_FOUND, [FCurrentPath]));
      Exit;
    end;
    DirEntryPos := FindFileInDirectory(ImageFile, DirCluster, TargetName);
    if DirEntryPos = -1 then
    begin
      ShowMessage(Format(ERR_FILE_NOT_FOUND, [TargetName]));
      Exit;
    end;

    // Read attribute byte at offset 11 of entry
    ImageFile.Position := DirEntryPos + 11;
    ImageFile.ReadBuffer(CurrentAttr, 1);

    // Toggle bit
    NewAttr := CurrentAttr xor AttrBit;

    // Write back
    ImageFile.Position := DirEntryPos + 11;
    ImageFile.WriteBuffer(NewAttr, 1);
    FIsModified := True;
  finally
    ImageFile.Free;
  end;

  // Refresh
  ReadSTImage(FImagePath, FCurrentPath);
  UpdateFileListDisplay;

  // Restore selection
  for I := 0 to ListViewFiles.Items.Count - 1 do
  begin
    if SameText(ListViewFiles.Items[I].Caption, CleanName) or
       SameText(ListViewFiles.Items[I].Caption, CleanName + '\') then
    begin
      ListViewFiles.Items[I].Selected := True;
      ListViewFiles.Items[I].Focused := True;
      Break;
    end;
  end;
  UpdateStatusBar;
end;

procedure TForm1.EditSelectedAttributes;
var
  RawName, CleanName: string;
  IsDir, IsStaged, IsDel, IsMarkedDel: Boolean;
  ImageFile: TFileStream;
  DirCluster, DirEntryPos: Int64;
  Attr: Byte;
  AttrStr: string;
  NewAttrStr: string;
  NewAttr: Byte;
  I: Integer;
begin
  if FInArchiveMode then
  begin
    ShowMessage('Cannot modify attributes of files inside archives.');
    Exit;
  end;

  if not GetSelectedFileName(RawName, CleanName, IsDir, IsStaged, IsDel, IsMarkedDel) then
  begin
    ShowMessage('Please select a file or folder first.');
    Exit;
  end;

  if (CleanName = '..') or (CleanName = '..\') or (CleanName = '.') then Exit;

  if IsDel then
  begin
    ShowMessage('Cannot edit attributes of a deleted file.');
    Exit;
  end;

  if IsStaged then
  begin
    ShowMessage('Save the image first to edit attributes of newly added files.');
    Exit;
  end;

  CleanName := StringReplace(CleanName, '\', '', [rfReplaceAll]);

  if (FImagePath = '') or not FileExists(FImagePath) then Exit;

  ImageFile := TFileStream.Create(FImagePath, fmOpenReadWrite or fmShareDenyNone);
  try
    GetFATDetails(ImageFile);
    DirCluster := FindDirectoryInPath(ImageFile, FCurrentPath);
    if DirCluster = -1 then
    begin
      ShowMessage(Format(ERR_DIR_NOT_FOUND, [FCurrentPath]));
      Exit;
    end;
    DirEntryPos := FindFileInDirectory(ImageFile, DirCluster, CleanName);
    if DirEntryPos = -1 then
    begin
      ShowMessage(Format(ERR_FILE_NOT_FOUND, [CleanName]));
      Exit;
    end;

    ImageFile.Position := DirEntryPos + 11;
    ImageFile.ReadBuffer(Attr, 1);

    AttrStr := '';
    if (Attr and $01) <> 0 then AttrStr := AttrStr + 'R';
    if (Attr and $02) <> 0 then AttrStr := AttrStr + 'H';
    if (Attr and $20) <> 0 then AttrStr := AttrStr + 'A';

    NewAttrStr := InputBox('Edit Attributes for "' + CleanName + '"',
      'Enter attribute letters to enable (R = Read-Only, H = Hidden, A = Archive):', AttrStr);
    NewAttrStr := UpperCase(Trim(NewAttrStr));

    // Preserve directory bit and other system bits
    NewAttr := Attr and not ($01 or $02 or $20);
    if Pos('R', NewAttrStr) > 0 then NewAttr := NewAttr or $01;
    if Pos('H', NewAttrStr) > 0 then NewAttr := NewAttr or $02;
    if Pos('A', NewAttrStr) > 0 then NewAttr := NewAttr or $20;

    ImageFile.Position := DirEntryPos + 11;
    ImageFile.WriteBuffer(NewAttr, 1);
    FIsModified := True;
  finally
    ImageFile.Free;
  end;

  ReadSTImage(FImagePath, FCurrentPath);
  UpdateFileListDisplay;

  for I := 0 to ListViewFiles.Items.Count - 1 do
  begin
    if SameText(ListViewFiles.Items[I].Caption, CleanName) or
       SameText(ListViewFiles.Items[I].Caption, CleanName + '\') then
    begin
      ListViewFiles.Items[I].Selected := True;
      ListViewFiles.Items[I].Focused := True;
      Break;
    end;
  end;
  UpdateStatusBar;
end;

procedure TForm1.MakePCCompatibleBootSector;
var
  FS: TFileStream;
  BootSec: array[0..511] of Byte;
  BytesPerSec, SecPerFAT, TotalSec, ReservedSec, RootEntries, SecPerTrack, Sides: Word;
  SecPerClust, NumFATs, MediaByte: Byte;
  I: Integer;
  BootSum, OriginalBootSum: Word;
begin
  if (FImagePath = '') or not FileExists(FImagePath) then
  begin
    ShowMessage('Please open a disk image first.');
    Exit;
  end;

  if FInArchiveMode then
  begin
    ShowMessage('Cannot modify boot sector while browsing an archive.');
    Exit;
  end;

  if MessageDlg('Make PC-Compatible Boot Sector',
    'This tool installs a standard MS-DOS / PC compatible BIOS Parameter Block (BPB)' + sLineBreak +
    'with standard x86 jump instruction ($EB $3C $90), "MSDOS5.0" OEM string, and $55AA signature.' + sLineBreak + sLineBreak +
    'This allows modern Windows, Linux, and Mac systems to read the disk seamlessly,' + sLineBreak +
    'while remaining 100% compatible with Atari ST TOS.' + sLineBreak + sLineBreak +
    'Do you want to proceed?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  FS := TFileStream.Create(FImagePath, fmOpenReadWrite or fmShareDenyNone);
  try
    if FS.Size < 512 then
    begin
      ShowMessage('Error: Image size is less than 512 bytes.');
      Exit;
    end;

    FS.Position := 0;
    FS.ReadBuffer(BootSec, 512);

    // Check if original boot sector contains an executable Atari ST bootloader
    OriginalBootSum := 0;
    for I := 0 to 255 do
      Inc(OriginalBootSum, (Word(BootSec[I * 2]) shl 8) or BootSec[I * 2 + 1]);

    if (OriginalBootSum = $1234) or (BootSec[0] = $60) then
    begin
      if MessageDlg('Executable Bootloader Detected',
        'Warning: This disk image appears to contain an executable Atari ST bootloader.' + sLineBreak +
        'Applying a standard PC boot sector will overwrite the Atari boot program (e.g. game or demo loader).' + sLineBreak + sLineBreak +
        'Do you want to proceed and replace it anyway?',
        mtWarning, [mbYes, mbNo], 0) <> mrYes then
        Exit;
    end;

    // Read current BPB values
    Move(BootSec[$0B], BytesPerSec, 2);
    SecPerClust := BootSec[$0D];
    Move(BootSec[$0E], ReservedSec, 2);
    NumFATs := BootSec[$10];
    Move(BootSec[$11], RootEntries, 2);
    Move(BootSec[$13], TotalSec, 2);
    MediaByte := BootSec[$15];
    Move(BootSec[$16], SecPerFAT, 2);
    Move(BootSec[$18], SecPerTrack, 2);
    Move(BootSec[$1A], Sides, 2);

    // Validate and fallback to standard geometry if unformatted/invalid
    if BytesPerSec <> 512 then BytesPerSec := 512;
    if SecPerClust = 0 then
    begin
      if TotalSec <= 720 then SecPerClust := 1 else SecPerClust := 2;
    end;
    if ReservedSec = 0 then ReservedSec := 1;
    if NumFATs = 0 then NumFATs := 2;
    if RootEntries = 0 then
    begin
      if TotalSec <= 720 then RootEntries := 64 else RootEntries := 112;
    end;
    if TotalSec = 0 then
    begin
      TotalSec := Word(Min(65535, FS.Size div BytesPerSec));
    end;
    if (MediaByte < $F0) then
    begin
      if TotalSec <= 720 then MediaByte := $F7 else MediaByte := $F9;
    end;
    if SecPerFAT = 0 then
    begin
      if TotalSec <= 720 then
        SecPerFAT := 2
      else
      begin
        // Calculate needed FAT12 sectors: 1.5 bytes per cluster
        SecPerFAT := Word((((Int64(TotalSec) div Max(1, SecPerClust)) + 2) * 3 div 2 + 511) div 512);
        if SecPerFAT < 2 then SecPerFAT := 2;
        if SecPerFAT > 9 then SecPerFAT := 5;
      end;
    end;
    if SecPerTrack = 0 then SecPerTrack := 9;
    if Sides = 0 then
    begin
      if TotalSec <= 720 then Sides := 1 else Sides := 2;
    end;

    // 1. Jump instruction (JMP SHORT $3C; NOP -> lands at offset $3E)
    BootSec[0] := $EB;
    BootSec[1] := $3C;
    BootSec[2] := $90;

    // 2. OEM Name: "MSDOS5.0"
    Move(PAnsiChar('MSDOS5.0')^, BootSec[3], 8);

    // 3. BPB
    Move(BytesPerSec, BootSec[$0B], 2);
    BootSec[$0D] := SecPerClust;
    Move(ReservedSec, BootSec[$0E], 2);
    BootSec[$10] := NumFATs;
    Move(RootEntries, BootSec[$11], 2);
    Move(TotalSec, BootSec[$13], 2);
    BootSec[$15] := MediaByte;
    Move(SecPerFAT, BootSec[$16], 2);
    Move(SecPerTrack, BootSec[$18], 2);
    Move(Sides, BootSec[$1A], 2);
    FillChar(BootSec[$1C], 8, 0); // Hidden sectors + 32-bit total sectors = 0

    // 4. Extended BPB
    BootSec[$24] := $00; // Physical drive 0
    BootSec[$25] := $00; // Reserved
    BootSec[$26] := $29; // Extended boot signature
    // Serial number at $27..$2A (if 0, generate random)
    if (BootSec[$27] = 0) and (BootSec[$28] = 0) and (BootSec[$29] = 0) and (BootSec[$2A] = 0) then
    begin
      BootSec[$27] := Byte(Random(256));
      BootSec[$28] := Byte(Random(256));
      BootSec[$29] := Byte(Random(256));
      BootSec[$2A] := Byte(Random(256));
    end;
    // Volume label at $2B..$35 (11 bytes)
    if FVolumeLabel <> '' then
      Move(PAnsiChar(PadRight(FVolumeLabel, 11))^, BootSec[$2B], 11)
    else
      Move(PAnsiChar('NO NAME    ')^, BootSec[$2B], 11);

    // File System Type: "FAT12   "
    Move(PAnsiChar('FAT12   ')^, BootSec[$36], 8);

    // 5. Clear remainder of boot code up to signature with dummy code
    FillChar(BootSec[$3E], 510 - $3E, 0);
    BootSec[$3E] := $FA; // x86 CLI
    BootSec[$3F] := $F4; // x86 HLT
    BootSec[$40] := $CB; // x86 RETF

    // 6. MS-DOS Boot Signature: $55 $AA at offset 510..511
    BootSec[510] := $55;
    BootSec[511] := $AA;

    // 7. Ensure Atari TOS does NOT treat this as an executable boot sector
    // TOS sums 256 16-bit big-endian words. If sum = $1234, TOS executes it!
    BootSum := 0;
    for I := 0 to 255 do
      Inc(BootSum, (Word(BootSec[I * 2]) shl 8) or BootSec[I * 2 + 1]);

    if BootSum = $1234 then
    begin
      Inc(BootSec[508]);
    end;

    // Write updated Sector 0
    FS.Position := 0;
    FS.WriteBuffer(BootSec, 512);

    FIsModified := True;

    GetFATDetails(FS);
  finally
    FS.Free;
  end;

  UpdateFreeSpaceDisplay;
  ShowMessage('Boot sector updated successfully!' + sLineBreak + sLineBreak +
    'The disk image now features a standard PC/TOS dual-compatible BPB' + sLineBreak +
    'with valid OEM identification, extended parameters, and $55AA boot signature.');
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

  if FInArchiveMode then
  begin
    SaveDialog.Filter := 'All Files (*.*)|*.*';
    SaveDialog.FileName := CleanName;
    if not SaveDialog.Execute then Exit;
    LocalFileName := SaveDialog.FileName;

    Fs := TFileStream.Create(LocalFileName, fmCreate);
    try
      Screen.Cursor := crHourGlass;
      try
        if FArchiveVault.ExtractEntryToStream(FArchiveInnerPath + CleanName, Fs) then
          ShowMessage('File extracted successfully from archive:' + sLineBreak + LocalFileName)
        else
          ShowMessage('Failed to extract file from archive.');
      finally
        Screen.Cursor := crDefault;
      end;
    finally
      Fs.Free;
    end;
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

  if FInArchiveMode then
  begin
    MemStream := TMemoryStream.Create;
    try
      Screen.Cursor := crHourGlass;
      try
        if not FArchiveVault.ExtractEntryToStream(FArchiveInnerPath + CleanName, MemStream) then
        begin
          ShowMessage('Failed to extract file from archive.');
          Exit;
        end;
        SetLength(InBuf, MemStream.Size);
        if MemStream.Size > 0 then
        begin
          MemStream.Position := 0;
          MemStream.ReadBuffer(InBuf[0], MemStream.Size);
        end;
      finally
        Screen.Cursor := crDefault;
      end;
    finally
      MemStream.Free;
    end;
  end
  else if IsStaged then
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

  if not IsPackerSupported(PackerName) then
  begin
    ShowMessage(Format('File "%s" appears to be packed with "%s", but decompression for this specific packer format is not implemented.',
      [CleanName, PackerName]));
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
  if Key = VK_F1 then
  begin
    MenuItemAboutClick(nil);
    Key := 0;
    Exit;
  end;

  if Key = VK_F3 then
  begin
    ButtonViewFileClick(nil);
    Key := 0;
    Exit;
  end;

  if Key = VK_F4 then
  begin
    ButtonClusterMapClick(nil);
    Key := 0;
    Exit;
  end;

  if Key = VK_F6 then
  begin
    MenuItemVirusScannerClick(nil);
    Key := 0;
    Exit;
  end;

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

  if (ssCtrl in Shift) and (Key = VK_E) then
  begin
    ButtonSaveFileClick(nil);
    Key := 0;
    Exit;
  end;

  if (Key = VK_F) and (ssCtrl in Shift) then
  begin
    Key := 0;
    OpenSearchDialog;
    Exit;
  end;

  if Key = VK_F2 then
  begin
    if (ListViewFiles.Selected <> nil) then
    begin
      Key := 0;
      RenameSelectedItem;
      Exit;
    end;
  end;

  if (Key = VK_BACK) or ((ssAlt in Shift) and (Key = VK_UP)) then
  begin
    if FInArchiveMode or (FCurrentPath <> '') then
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
            F2: rename, Ctrl+F: find, Backspace / Alt+Up: parent dir
 ------------------------------------------------------------------}
procedure TForm1.ListViewFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_F) and (ssCtrl in Shift) then
  begin
    Key := 0;
    OpenSearchDialog;
    Exit;
  end;

  if Key = VK_F2 then
  begin
    Key := 0;
    RenameSelectedItem;
    Exit;
  end;

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
    if FInArchiveMode or (FCurrentPath <> '') then
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

  if FInArchiveMode then
  begin
    if FDiskItems[Idx].IsDir then
    begin
      Sender.Canvas.Font.Style := [fsBold];
      if not (cdsSelected in State) then
        Sender.Canvas.Font.Color := clBlue;
    end
    else
    begin
      Sender.Canvas.Font.Style := [];
      if not (cdsSelected in State) then
        Sender.Canvas.Font.Color := $00703800; // Deep teal / cyan for archive entries
    end;
    Exit;
  end;

  if FDiskItems[Idx].IsDir then
  begin
    Sender.Canvas.Font.Style := [fsBold];
    if not (cdsSelected in State) then
      Sender.Canvas.Font.Color := clBlue;
  end
  else if FDiskItems[Idx].StatusStr = 'Archive' then
  begin
    Sender.Canvas.Font.Style := [fsBold];
    if not (cdsSelected in State) then
      Sender.Canvas.Font.Color := clTeal;
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

{------------------------------------------------------------------
  Procedure: PopupMenuFilesPopup
  Purpose:  Dynamically sets menu item captions and availability
            for the file list right-click context menu
 ------------------------------------------------------------------}
procedure TForm1.PopupMenuFilesPopup(Sender: TObject);
var
  RawName, CleanName: string;
  IsDir, IsStaged, IsDel, IsMarkedDel: Boolean;
  HasSelection: Boolean;
begin
  if FInArchiveMode then
  begin
    MenuItemPopupOpenArchive.Caption := '&Close Archive (Return to Disk)';
    MenuItemPopupOpenArchive.Visible := True;
    MenuItemPopupOpenArchive.Enabled := True;
    MenuItemPopupSep0.Visible := True;

    HasSelection := GetSelectedFileName(RawName, CleanName, IsDir, IsStaged, IsDel, IsMarkedDel);
    if not HasSelection or (CleanName = '..') or (CleanName = '..\') then
    begin
      MenuItemPopupView.Enabled := False;
      MenuItemPopupExtract.Enabled := False;
      MenuItemPopupExtractDepacked.Enabled := False;
      MenuItemPopupClusterMap.Enabled := False;
      MenuItemPopupDelete.Enabled := False;
      MenuItemPopupDelete.Caption := '&Delete';
      Exit;
    end;

    MenuItemPopupView.Enabled := not IsDir;
    MenuItemPopupExtract.Enabled := not IsDir;
    MenuItemPopupExtractDepacked.Enabled := not IsDir;
    MenuItemPopupClusterMap.Enabled := False;
    MenuItemPopupDelete.Enabled := False;
    MenuItemPopupDelete.Caption := '&Delete';
    Exit;
  end;

  HasSelection := GetSelectedFileName(RawName, CleanName, IsDir, IsStaged, IsDel, IsMarkedDel);

  MenuItemPopupFind.Enabled := (FImagePath <> '') and FileExists(FImagePath);

  if not HasSelection or (CleanName = '..') or (CleanName = '..\') then
  begin
    MenuItemPopupOpenArchive.Visible := False;
    MenuItemPopupSep0.Visible := False;
    MenuItemPopupView.Enabled := False;
    MenuItemPopupRename.Enabled := False;
    MenuItemPopupAttributes.Enabled := False;
    MenuItemPopupExtract.Enabled := False;
    MenuItemPopupExtractDepacked.Enabled := False;
    MenuItemPopupClusterMap.Enabled := False;
    MenuItemPopupDelete.Enabled := False;
    MenuItemPopupDelete.Caption := '&Delete';
    Exit;
  end;

  // Rename and Attributes
  MenuItemPopupRename.Enabled := not IsDel and not FInArchiveMode;
  MenuItemPopupAttributes.Enabled := not IsDel and not IsStaged and not FInArchiveMode;

  if (ListViewFiles.Selected <> nil) and (ListViewFiles.Selected.SubItems.Count >= 4) then
  begin
    MenuItemPopupAttrArchive.Checked := (Length(ListViewFiles.Selected.SubItems[3]) >= 2) and (ListViewFiles.Selected.SubItems[3][2] = 'A');
    MenuItemPopupAttrReadOnly.Checked := (Length(ListViewFiles.Selected.SubItems[3]) >= 3) and (ListViewFiles.Selected.SubItems[3][3] = 'R');
    MenuItemPopupAttrHidden.Checked := (Length(ListViewFiles.Selected.SubItems[3]) >= 4) and (ListViewFiles.Selected.SubItems[3][4] = 'H');

    MenuItemAttrArchive.Checked := MenuItemPopupAttrArchive.Checked;
    MenuItemAttrReadOnly.Checked := MenuItemPopupAttrReadOnly.Checked;
    MenuItemAttrHidden.Checked := MenuItemPopupAttrHidden.Checked;
  end;

  // Open archive option
  if not IsDir and not CleanName.EndsWith('\') and IsArchiveFileName(CleanName) and not IsStaged and not IsDel then
  begin
    MenuItemPopupOpenArchive.Caption := '&Browse / Open Archive as Folder';
    MenuItemPopupOpenArchive.Visible := True;
    MenuItemPopupOpenArchive.Enabled := True;
    MenuItemPopupSep0.Visible := True;
  end
  else
  begin
    MenuItemPopupOpenArchive.Visible := False;
    MenuItemPopupSep0.Visible := False;
  end;

  // View: Only files
  MenuItemPopupView.Enabled := not IsDir and not CleanName.EndsWith('\');

  // Extract: Only files (directories cannot be single-extracted with ButtonSaveFile)
  MenuItemPopupExtract.Enabled := not IsDir and not CleanName.EndsWith('\') and not IsStaged;

  // Extract & Depack: Only files
  MenuItemPopupExtractDepacked.Enabled := not IsDir and not CleanName.EndsWith('\');

  // Locate in Cluster Map: Files/dirs that reside on disk (not staged)
  MenuItemPopupClusterMap.Enabled := not IsStaged and (FImagePath <> '') and FileExists(FImagePath);

  // Delete / Undelete
  MenuItemPopupDelete.Enabled := True;
  if IsDir or CleanName.EndsWith('\') then
  begin
    if IsStaged then
      MenuItemPopupDelete.Caption := 'Cancel Staged Directory'
    else
      MenuItemPopupDelete.Caption := '&Remove Directory';
  end
  else
  begin
    if IsDel then
      MenuItemPopupDelete.Caption := '&Undelete File'
    else if IsMarkedDel then
      MenuItemPopupDelete.Caption := 'Cancel Deletion'
    else if IsStaged then
      MenuItemPopupDelete.Caption := 'Cancel Staged File'
    else
      MenuItemPopupDelete.Caption := '&Delete File';
  end;
end;

{------------------------------------------------------------------
  Procedure: MenuItemPopupOpenArchiveClick
  Purpose:  Context menu handler for opening an archive container as a
            folder or closing it to return to the disk image
 ------------------------------------------------------------------}
procedure TForm1.MenuItemPopupOpenArchiveClick(Sender: TObject);
var
  RawName, CleanName: string;
  IsDir, IsStaged, IsDel, IsMarkedDel: Boolean;
begin
  if FInArchiveMode then
  begin
    CloseArchiveMode;
    Exit;
  end;

  if GetSelectedFileName(RawName, CleanName, IsDir, IsStaged, IsDel, IsMarkedDel) then
  begin
    if not IsDir and IsArchiveFileName(CleanName) and not IsStaged and not IsDel then
      OpenArchiveFromDisk(RawName);
  end;
end;

{------------------------------------------------------------------
  Procedure: MenuItemPopupDeleteClick
  Purpose:  Context menu delete handler: routes to file delete/undelete
            or directory removal based on selected item type
 ------------------------------------------------------------------}
procedure TForm1.MenuItemPopupDeleteClick(Sender: TObject);
var
  RawName, CleanName: string;
  IsDir, IsStaged, IsDel, IsMarkedDel: Boolean;
begin
  if not GetSelectedFileName(RawName, CleanName, IsDir, IsStaged, IsDel, IsMarkedDel) then Exit;
  if IsDir or CleanName.EndsWith('\') then
    ButtonRemoveDirClick(Sender)
  else
    ButtonRemoveFileClick(Sender);
end;

{------------------------------------------------------------------
  Procedure: UpdateLayout
  Purpose:  Dynamically positions and scales the file list, header indicators,
            and bottom button toolbar to ensure clean alignment at all window sizes
 ------------------------------------------------------------------}
procedure TForm1.UpdateLayout;
var
  FixedWidth, Remainder: Integer;
begin
  if ListViewFiles = nil then Exit;

  // Auto-expand columns to completely fill ListViewFiles without any blank space right of Status
  if ListViewFiles.Columns.Count >= 6 then
  begin
    // Sum of standard fixed columns: Size (80) + Cluster (55) + Date/Time (110) + Attr (50) = 295
    FixedWidth := ListViewFiles.Columns[1].Width +
                  ListViewFiles.Columns[2].Width +
                  ListViewFiles.Columns[3].Width +
                  ListViewFiles.Columns[4].Width;

    Remainder := ListViewFiles.ClientWidth - FixedWidth;
    if Remainder < 265 then
    begin
      ListViewFiles.Columns[0].Width := 180;
      ListViewFiles.Columns[5].Width := 85;
    end
    else
    begin
      ListViewFiles.Columns[5].Width := 85;
      ListViewFiles.Columns[0].Width := Remainder - 85;
    end;
  end;

  UpdateStatusBar;
end;

{------------------------------------------------------------------
  Procedure: FormResize
  Purpose:  Event handler triggered when the window is resized or maximized
 ------------------------------------------------------------------}
procedure TForm1.FormResize(Sender: TObject);
begin
  UpdateLayout;
end;

{------------------------------------------------------------------
  Procedure: SetButtonToolbarVisible
  Purpose:  Toggles visibility of the classic bottom button toolbar
            and resizes ListViewFiles accordingly (default is OFF)
 ------------------------------------------------------------------}
procedure TForm1.SetButtonToolbarVisible(AVisible: Boolean);
begin
  PanelToolbar.Visible := AVisible;
  MenuItemButtonToolbar.Checked := AVisible;
  UpdateLayout;
end;

{------------------------------------------------------------------
  Procedure: MenuItemButtonToolbarClick
  Purpose:  Toggles the classic bottom button toolbar from View menu
 ------------------------------------------------------------------}
procedure TForm1.MenuItemButtonToolbarClick(Sender: TObject);
begin
  SetButtonToolbarVisible(MenuItemButtonToolbar.Checked);
end;

{------------------------------------------------------------------
  Procedure: MenuItemCloseImageClick
  Purpose:  Closes the current disk image and returns to empty state
 ------------------------------------------------------------------}
procedure TForm1.MenuItemCloseImageClick(Sender: TObject);
begin
  if not CanCloseOrOpenImage then Exit;
  ResetState;
end;

{------------------------------------------------------------------
  Procedure: MenuItemCloseArchiveClick
  Purpose:  Exits archive folder navigation and returns to disk root
 ------------------------------------------------------------------}
procedure TForm1.MenuItemCloseArchiveClick(Sender: TObject);
begin
  if FInArchiveMode then
    CloseArchiveMode;
end;

{------------------------------------------------------------------
  Procedure: MenuItemAboutClick
  Purpose:  Displays the application information dialog
 ------------------------------------------------------------------}
procedure TForm1.MenuItemAboutClick(Sender: TObject);
begin
  MessageDlg('About Atari .ST Image File Manager',
    'Atari .ST Image File Manager v2.0' + LineEnding +
    'Advanced Atari ST Floppy Disk Image & Archive Utility' + LineEnding + LineEnding +
    'Features:' + LineEnding +
    '  * Multi-Format Floppy Disk Creator (360 KB - 1.44 MB)' + LineEnding +
    '  * FAT12 Disk Image Management (.ST & .MSA formats)' + LineEnding +
    '  * File Archival, Extraction & Integrated Decompressors' + LineEnding +
    '  * Archive Vault: Browse compressed files as folders' + LineEnding +
    '  * Cluster Map & Low-Level Sector Hex Inspector' + LineEnding +
    '  * Integrated Atari ST Boot-Sector & File Virus Scanner',
    mtInformation, [mbOK], 0);
end;

{------------------------------------------------------------------
  Procedure: ButtonNavRootClick
  Purpose:  Jumps directly to the disk root or exits archive vault
 ------------------------------------------------------------------}
procedure TForm1.ButtonNavRootClick(Sender: TObject);
begin
  if FInArchiveMode then
  begin
    CloseArchiveMode;
    Exit;
  end;

  if FCurrentPath <> '' then
  begin
    FCurrentPath := '';
    if (FImagePath <> '') and FileExists(FImagePath) then
      ReadSTImage(FImagePath, FCurrentPath)
    else
      FDisplayItems.Clear;
    UpdateFileListDisplay;
  end;
end;

{------------------------------------------------------------------
  Procedure: MenuItemQuickToolbarClick
  Purpose:  Toggles visibility of the top Quick Action Toolbar
 ------------------------------------------------------------------}
procedure TForm1.MenuItemQuickToolbarClick(Sender: TObject);
begin
  PanelQuickToolbar.Visible := MenuItemQuickToolbar.Checked;
  UpdateLayout;
end;

{------------------------------------------------------------------
  Procedure: ListViewFilesColumnClick
  Purpose:  Sorts the file list ascending/descending by clicked column
 ------------------------------------------------------------------}
procedure TForm1.ListViewFilesColumnClick(Sender: TObject; Column: TListColumn);
begin
  if Column = nil then Exit;
  if FSortColumn = Column.Index then
    FSortAscending := not FSortAscending
  else
  begin
    FSortColumn := Column.Index;
    FSortAscending := True;
  end;
  SortDiskItems;
  RefreshListViewFromDiskItems;
end;

{------------------------------------------------------------------
  Procedure: SortDiskItems
  Purpose:  Sorts the in-memory FDiskItems list by FSortColumn
            keeping the parent directory link '..' at index 0
 ------------------------------------------------------------------}
procedure TForm1.SortDiskItems;
const
  BaseCaptions: array[0..5] of string = ('Name', 'Size', 'Cluster', 'Date / Time', 'Attr', 'Status');
var
  C, StartIdx, I, J: Integer;
  Temp: TDiskItemInfo;

  function CompareItems(const A, B: TDiskItemInfo): Integer;
  begin
    // Directories always sort before files
    if A.IsDir and not B.IsDir then
      Exit(-1);
    if not A.IsDir and B.IsDir then
      Exit(1);

    case FSortColumn of
      0: Result := CompareText(A.CleanName, B.CleanName);
      1:
        begin
          if A.Size < B.Size then Result := -1
          else if A.Size > B.Size then Result := 1
          else Result := CompareText(A.CleanName, B.CleanName);
        end;
      2:
        begin
          if A.StartCluster < B.StartCluster then Result := -1
          else if A.StartCluster > B.StartCluster then Result := 1
          else Result := CompareText(A.CleanName, B.CleanName);
        end;
      3:
        begin
          Result := CompareText(A.DateTimeStr, B.DateTimeStr);
          if Result = 0 then
            Result := CompareText(A.CleanName, B.CleanName);
        end;
      4:
        begin
          Result := CompareText(A.AttrStr, B.AttrStr);
          if Result = 0 then
            Result := CompareText(A.CleanName, B.CleanName);
        end;
      5:
        begin
          Result := CompareText(A.StatusStr, B.StatusStr);
          if Result = 0 then
            Result := CompareText(A.CleanName, B.CleanName);
        end;
    else
      Result := CompareText(A.CleanName, B.CleanName);
    end;

    if not FSortAscending then
      Result := -Result;
  end;

begin
  // Update column header indicator glyphs
  if ListViewFiles <> nil then
  begin
    for C := 0 to Min(5, ListViewFiles.Columns.Count - 1) do
    begin
      if C = FSortColumn then
      begin
        if FSortAscending then
          ListViewFiles.Columns[C].Caption := BaseCaptions[C] + ' ▲'
        else
          ListViewFiles.Columns[C].Caption := BaseCaptions[C] + ' ▼';
      end
      else
        ListViewFiles.Columns[C].Caption := BaseCaptions[C];
    end;
  end;

  if Length(FDiskItems) <= 1 then Exit;

  // Keep parent directory link '..' pinned at index 0
  StartIdx := 0;
  if (FDiskItems[0].CleanName = '..') or (FDiskItems[0].DisplayName = '..') then
    StartIdx := 1;

  if StartIdx >= High(FDiskItems) then Exit;

  // Stable insertion sort
  for I := StartIdx + 1 to High(FDiskItems) do
  begin
    Temp := FDiskItems[I];
    J := I - 1;
    while (J >= StartIdx) and (CompareItems(FDiskItems[J], Temp) > 0) do
    begin
      FDiskItems[J + 1] := FDiskItems[J];
      Dec(J);
    end;
    FDiskItems[J + 1] := Temp;
  end;
end;

{------------------------------------------------------------------
  Procedure: RefreshListViewFromDiskItems
  Purpose:  Populates ListViewFiles from the sorted FDiskItems array
 ------------------------------------------------------------------}
procedure TForm1.RefreshListViewFromDiskItems;
var
  I: Integer;
begin
  if ListViewFiles = nil then Exit;
  ListViewFiles.Items.BeginUpdate;
  try
    ListViewFiles.Items.Clear;
    for I := 0 to High(FDiskItems) do
      AddDiskItemToListView(FDiskItems[I]);
  finally
    ListViewFiles.Items.EndUpdate;
  end;
  UpdateLayout;
  if (ListViewFiles.Items.Count > 0) and (ListViewFiles.Selected = nil) then
  begin
    ListViewFiles.ItemIndex := 0;
    ListViewFiles.Selected := ListViewFiles.Items[0];
  end;
end;

{------------------------------------------------------------------
  Procedure: PopulateFileIcons
  Purpose:  Generates sharp 16x16 pixel-art icons for file types:
            0: Folder, 1: ST Executable, 2: Archive, 3: Graphic/Pic,
            4: Audio, 5: Doc/Text, 6: Disk Image, 7: Generic File
 ------------------------------------------------------------------}
procedure TForm1.PopulateFileIcons(AImageList: TImageList);
var
  Bmp: TBitmap;

  procedure InitBmp;
  begin
    Bmp.Canvas.Brush.Color := clFuchsia;
    Bmp.Canvas.FillRect(0, 0, 16, 16);
  end;

  procedure AddBmp;
  begin
    AImageList.AddMasked(Bmp, clFuchsia);
  end;

begin
  if AImageList = nil then Exit;
  AImageList.Clear;
  AImageList.Width := 16;
  AImageList.Height := 16;

  Bmp := TBitmap.Create;
  try
    Bmp.SetSize(16, 16);

    // 0: Folder (Manila yellow folder with tab)
    InitBmp;
    Bmp.Canvas.Pen.Color := $002070A0;
    Bmp.Canvas.Brush.Color := $003090D0;
    Bmp.Canvas.Rectangle(2, 3, 8, 6);
    Bmp.Canvas.Brush.Color := $0040B8F8;
    Bmp.Canvas.Rectangle(2, 5, 14, 14);
    Bmp.Canvas.Pen.Color := $0070D0FF;
    Bmp.Canvas.Line(3, 6, 13, 6);
    AddBmp;

    // 1: ST Executable (PRG/TOS/TTP - retro window with GEM header and play symbol)
    InitBmp;
    Bmp.Canvas.Pen.Color := $00404040;
    Bmp.Canvas.Brush.Color := clWhite;
    Bmp.Canvas.Rectangle(2, 2, 14, 14);
    Bmp.Canvas.Pen.Color := $00904000;
    Bmp.Canvas.Brush.Color := $00C06000;
    Bmp.Canvas.Rectangle(3, 3, 13, 6);
    Bmp.Canvas.Pen.Color := $00209020;
    Bmp.Canvas.Brush.Color := $0030D030;
    Bmp.Canvas.Polygon([Point(5, 7), Point(5, 12), Point(11, 9)]);
    AddBmp;

    // 2: Archive (ZIP/LZH/ARC/MSA - cardboard package with vertical strap)
    InitBmp;
    Bmp.Canvas.Pen.Color := $00205070;
    Bmp.Canvas.Brush.Color := $004080A8;
    Bmp.Canvas.Rectangle(2, 3, 14, 14);
    Bmp.Canvas.Pen.Color := $001080D0;
    Bmp.Canvas.Brush.Color := $0020A0F0;
    Bmp.Canvas.Rectangle(7, 3, 9, 14);
    Bmp.Canvas.Pen.Color := $00205070;
    Bmp.Canvas.Line(2, 8, 13, 8);
    AddBmp;

    // 3: Picture / Graphic (PI1/NEO/PC1 - photo frame with sun & mountain)
    InitBmp;
    Bmp.Canvas.Pen.Color := $00606060;
    Bmp.Canvas.Brush.Color := clWhite;
    Bmp.Canvas.Rectangle(2, 2, 14, 14);
    Bmp.Canvas.Pen.Color := $00D09030;
    Bmp.Canvas.Brush.Color := $00F0C060;
    Bmp.Canvas.Rectangle(3, 3, 13, 9);
    Bmp.Canvas.Pen.Color := $0000B0E0;
    Bmp.Canvas.Brush.Color := $0000D0FF;
    Bmp.Canvas.Ellipse(10, 4, 12, 6);
    Bmp.Canvas.Pen.Color := $00308020;
    Bmp.Canvas.Brush.Color := $0040B030;
    Bmp.Canvas.Polygon([Point(3, 13), Point(7, 8), Point(10, 11), Point(13, 13)]);
    AddBmp;

    // 4: Audio / Sound (MOD/SND/YM - musical notes)
    InitBmp;
    Bmp.Canvas.Pen.Color := $00802080;
    Bmp.Canvas.Brush.Color := $00A02090;
    Bmp.Canvas.Ellipse(3, 10, 7, 13);
    Bmp.Canvas.Line(6, 4, 6, 11);
    Bmp.Canvas.Ellipse(9, 8, 13, 11);
    Bmp.Canvas.Line(12, 3, 12, 9);
    Bmp.Canvas.Pen.Width := 2;
    Bmp.Canvas.Line(6, 4, 12, 3);
    Bmp.Canvas.Pen.Width := 1;
    AddBmp;

    // 5: Document / Text (TXT/DOC/1ST - page sheet with folded corner and lines)
    InitBmp;
    Bmp.Canvas.Pen.Color := $00808080;
    Bmp.Canvas.Brush.Color := clWhite;
    Bmp.Canvas.Rectangle(3, 2, 13, 14);
    Bmp.Canvas.Pen.Color := $00A0A0A0;
    Bmp.Canvas.Brush.Color := $00E0E0E0;
    Bmp.Canvas.Polygon([Point(10, 2), Point(13, 5), Point(10, 5)]);
    Bmp.Canvas.Pen.Color := $00606060;
    Bmp.Canvas.Line(5, 6, 11, 6);
    Bmp.Canvas.Line(5, 8, 11, 8);
    Bmp.Canvas.Line(5, 10, 9, 10);
    AddBmp;

    // 6: Disk Image (ST/MSA/DIM - Atari 3.5" floppy disk)
    InitBmp;
    Bmp.Canvas.Pen.Color := $00803010;
    Bmp.Canvas.Brush.Color := $00B04018;
    Bmp.Canvas.Rectangle(2, 2, 14, 14);
    Bmp.Canvas.Pen.Color := $00808080;
    Bmp.Canvas.Brush.Color := $00D0D0D0;
    Bmp.Canvas.Rectangle(5, 2, 11, 7);
    Bmp.Canvas.Pen.Color := $00B0B0B0;
    Bmp.Canvas.Brush.Color := clWhite;
    Bmp.Canvas.Rectangle(4, 8, 12, 13);
    AddBmp;

    // 7: Generic File (clean document sheet)
    InitBmp;
    Bmp.Canvas.Pen.Color := $00707070;
    Bmp.Canvas.Brush.Color := clWhite;
    Bmp.Canvas.Rectangle(3, 2, 13, 14);
    Bmp.Canvas.Pen.Color := $00B0B0B0;
    Bmp.Canvas.Line(5, 6, 11, 6);
    Bmp.Canvas.Line(5, 9, 11, 9);
    AddBmp;

  finally
    Bmp.Free;
  end;
end;

{------------------------------------------------------------------
  Function: GetFileIconIndex
  Purpose:  Returns ImageList index (0..7) based on file type / extension
 ------------------------------------------------------------------}
function TForm1.GetFileIconIndex(const AFileName: string; IsDir: Boolean): Integer;
var
  Ext: string;
begin
  if IsDir or (AFileName = '..') or (AFileName = '..\') or AFileName.EndsWith('\') then
    Exit(0);

  Ext := UpperCase(ExtractFileExt(CleanFileName(AFileName)));

  // 1: ST Executables & Libraries (PRG, TOS, TTP, APP, GTP, ACC, CPX, OVL, SLB, DRV, SYS)
  if (Ext = '.PRG') or (Ext = '.TOS') or (Ext = '.TTP') or (Ext = '.APP') or
     (Ext = '.GTP') or (Ext = '.ACC') or (Ext = '.CPX') or (Ext = '.OVL') or
     (Ext = '.SLB') or (Ext = '.DRV') or (Ext = '.SYS') then
    Exit(1);

  // 2: Archives (ZIP, LZH, LHA, ARC, ZOO, ARJ, TAR, GZ, TGZ, 7Z, PAK, CHZ, CAB, RAR, SIT, BZ2, XZ)
  if (Ext = '.ZIP') or (Ext = '.LZH') or (Ext = '.LHA') or (Ext = '.ARC') or
     (Ext = '.ZOO') or (Ext = '.ARJ') or (Ext = '.TAR') or (Ext = '.GZ') or
     (Ext = '.TGZ') or (Ext = '.7Z') or (Ext = '.PAK') or (Ext = '.CHZ') or
     (Ext = '.CAB') or (Ext = '.RAR') or (Ext = '.SIT') or (Ext = '.BZ2') or
     (Ext = '.XZ') then
    Exit(2);

  // 3: Graphics & Pictures (PI1-3, PC1-3, NEO, SPU, SPC, SPS, ART, DOO, PBX, PAC, TN1-3, TNY, IMG, IFF, ILBM, LBM, BMP, GIF, DEG, MUR, ANM, GPH, ICN, JPG, JPEG, PNG, TGA, TIF, TIFF)
  if (Ext = '.PI1') or (Ext = '.PI2') or (Ext = '.PI3') or
     (Ext = '.PC1') or (Ext = '.PC2') or (Ext = '.PC3') or
     (Ext = '.NEO') or (Ext = '.SPU') or (Ext = '.SPC') or (Ext = '.SPS') or
     (Ext = '.ART') or (Ext = '.DOO') or (Ext = '.PBX') or (Ext = '.PAC') or
     (Ext = '.TN1') or (Ext = '.TN2') or (Ext = '.TN3') or (Ext = '.TNY') or
     (Ext = '.IMG') or (Ext = '.IFF') or (Ext = '.ILBM') or (Ext = '.LBM') or
     (Ext = '.BMP') or (Ext = '.GIF') or (Ext = '.DEG') or (Ext = '.MUR') or
     (Ext = '.ANM') or (Ext = '.GPH') or (Ext = '.ICN') or (Ext = '.JPG') or
     (Ext = '.JPEG') or (Ext = '.PNG') or (Ext = '.TGA') or (Ext = '.TIF') or
     (Ext = '.TIFF') then
    Exit(3);

  // 4: Audio & Tracker Music (MOD, SND, YM, MID, MIDI, WAV, SMP, THK, SID, DUD, SNG, AVR, AIF, AIFF, MP3, OGG, FLAC, XM, IT, S3M, MED, OKT, HIP, HIPC, AON, MUS)
  if (Ext = '.MOD') or (Ext = '.SND') or (Ext = '.YM') or (Ext = '.MID') or
     (Ext = '.MIDI') or (Ext = '.WAV') or (Ext = '.SMP') or (Ext = '.THK') or
     (Ext = '.SID') or (Ext = '.DUD') or (Ext = '.SNG') or (Ext = '.AVR') or
     (Ext = '.AIF') or (Ext = '.AIFF') or (Ext = '.MP3') or (Ext = '.OGG') or
     (Ext = '.FLAC') or (Ext = '.XM') or (Ext = '.IT') or (Ext = '.S3M') or
     (Ext = '.MED') or (Ext = '.OKT') or (Ext = '.HIP') or (Ext = '.HIPC') or
     (Ext = '.AON') or (Ext = '.MUS') then
    Exit(4);

  // 5: Documents, Source Code & Config (TXT, DOC, 1ST, ME, INF, CFG, INI, ASC, BAT, BAS, GFA, C, CPP, CC, H, PAS, INC, S, ASM, DIZ, NFO, MD, README, HLP, MAN, LOG, TEX, RTF, HTM, HTML, XML, JSON, PRJ)
  if (Ext = '.TXT') or (Ext = '.DOC') or (Ext = '.1ST') or (Ext = '.ME') or
     (Ext = '.INF') or (Ext = '.CFG') or (Ext = '.INI') or (Ext = '.ASC') or
     (Ext = '.BAT') or (Ext = '.BAS') or (Ext = '.GFA') or (Ext = '.C') or
     (Ext = '.CPP') or (Ext = '.CC') or (Ext = '.H') or (Ext = '.PAS') or
     (Ext = '.INC') or (Ext = '.S') or (Ext = '.ASM') or (Ext = '.DIZ') or
     (Ext = '.NFO') or (Ext = '.MD') or (Ext = '.README') or (Ext = '.HLP') or
     (Ext = '.MAN') or (Ext = '.LOG') or (Ext = '.TEX') or (Ext = '.RTF') or
     (Ext = '.HTM') or (Ext = '.HTML') or (Ext = '.XML') or (Ext = '.JSON') or
     (Ext = '.PRJ') then
    Exit(5);

  // 6: Disk Images & Cartridge Dumps (ST, MSA, DIM, STX, DSK, IPF, CTR, RAW)
  if (Ext = '.ST') or (Ext = '.MSA') or (Ext = '.DIM') or (Ext = '.STX') or
     (Ext = '.DSK') or (Ext = '.IPF') or (Ext = '.CTR') or (Ext = '.RAW') then
    Exit(6);

  // 7: Generic File
  Result := 7;
end;

{------------------------------------------------------------------
  Procedure: UpdateStatusBar
  Purpose:  Populates the 4 panels of StatusBar1:
            Panel 0: Format / Geometry
            Panel 1: File/Directory counts & selection
            Panel 2: Free capacity / Used
            Panel 3: Ready / Modified state
 ------------------------------------------------------------------}
procedure TForm1.UpdateStatusBar;
var
  I, FileCount, DirCount: Integer;
  FormatStr, FileStr, CapStr, StatusStr: string;
  SelectedCaption: string;
begin
  if StatusBar1 = nil then Exit;

  // 1. Format / Geometry Panel
  if FInArchiveMode then
    FormatStr := 'Archive: ' + UpperCase(ExtractFileExt(FArchiveSourceFileName)) + ' (' + ExtractFileName(FArchiveSourceFileName) + ')'
  else if FImagePath <> '' then
  begin
    if FIsMSAImage then
      FormatStr := '[MSA] '
    else if FIsDIMImage then
      FormatStr := '[DIM] '
    else
      FormatStr := '[ST] ';

    if FTotalClusters >= 2800 then
      FormatStr := FormatStr + 'Atari 1.44 MB HD (2S, 80T, 18SPT)'
    else if FTotalClusters >= 1700 then
      FormatStr := FormatStr + 'Atari 880 KB Extended (2S, 80T, 11SPT)'
    else if FTotalClusters >= 1500 then
      FormatStr := FormatStr + 'Atari 800 KB Demo (2S, 80T, 10SPT)'
    else if FTotalClusters >= 1400 then
      FormatStr := FormatStr + 'Atari 720 KB Standard (2S, 80T, 9SPT)'
    else if FTotalClusters >= 700 then
      FormatStr := FormatStr + 'Atari 360 KB SS (1S, 80T, 9SPT)'
    else
      FormatStr := FormatStr + 'Atari Custom Format (' + IntToStr(FTotalClusters) + ' clusters)';
  end
  else
    FormatStr := 'No Disk Image Loaded';

  // 2. File / Directory Counts & Selection Details
  FileCount := 0;
  DirCount := 0;
  for I := 0 to High(FDiskItems) do
  begin
    if (FDiskItems[I].CleanName = '..') or (FDiskItems[I].DisplayName = '..') then
      Continue;
    if FDiskItems[I].IsDir then
      Inc(DirCount)
    else
      Inc(FileCount);
  end;

  if (ListViewFiles <> nil) and (ListViewFiles.Selected <> nil) then
  begin
    SelectedCaption := ListViewFiles.Selected.Caption;
    if ListViewFiles.Selected.SubItems.Count > 0 then
      FileStr := 'Sel: ' + SelectedCaption + ' (' + ListViewFiles.Selected.SubItems[0] + ')'
    else
      FileStr := 'Sel: ' + SelectedCaption;
  end
  else
  begin
    if (FileCount = 0) and (DirCount = 0) then
      FileStr := 'Empty directory'
    else
      FileStr := Format('%d file(s), %d folder(s)', [FileCount, DirCount]);
  end;

  // 3. Free Space & Capacity
  if (StatusBar1.Panels.Count >= 4) and (StatusBar1.Panels[2].Text <> '') then
    CapStr := StatusBar1.Panels[2].Text
  else if (FImagePath <> '') and (FTotalClusters > 0) then
    CapStr := 'Free: ' + IntToStr(FTotalClusters) + ' clusters'
  else
    CapStr := 'Free: -';

  // 4. Status / State Indicator
  if (FFilePaths.Count > 0) or (FNewDirectories.Count > 0) or (FFilesToDelete.Count > 0) or (FDirsToDelete.Count > 0) then
    StatusStr := '● Modified'
  else if FInArchiveMode then
    StatusStr := '● Vault'
  else if FImagePath <> '' then
    StatusStr := '● Ready'
  else
    StatusStr := 'Idle';

  if StatusBar1.Panels.Count >= 4 then
  begin
    StatusBar1.Panels[0].Text := FormatStr;
    StatusBar1.Panels[1].Text := FileStr;
    StatusBar1.Panels[2].Text := CapStr;
    StatusBar1.Panels[3].Text := StatusStr;
  end;
end;

end.

