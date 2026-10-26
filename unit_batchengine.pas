unit unit_batchengine;

{$mode objfpc}{$H+}

{ ------------------------------------------------------------------
  unit_batchengine - Headless Batch Disk Image Conversion & Extraction Engine.

  Features:
  - Batch extraction of Atari ST floppy disk images (.st, .msa, .dim, .zip)
    to subfolders (e.g. image.st extracted to Destination\image\...).
  - Batch packaging of disk image contents directly into .zip archives.
  - In-flight depacking of 35+ Atari ST executable & data packers
    (Atomik, Ice, Automation, Pompey, Sentry, Fire, SpeedPacker, etc.).
  - Cross-conversion between image formats (.st <-> .msa, .dim).
  - Batch generation of disk catalog reports (HTML, CSV, JSON, TXT).
  - Clean separation from GUI: works headlessly in test runners, CLI,
    and GUI form wrappers with progress/log callbacks.
  ------------------------------------------------------------------ }

interface

uses
  Classes, SysUtils, Math, zipper,
  unit_fat12, unit_msa, unit_dim, unit_depacker, unit_catalog;

type
  { Supported batch processing actions }
  TBatchAction = (
    baExtractFolders,       // Extract files into <DestDir>\<ImageBaseName>\...
    baExtractZips,          // Extract files into <DestDir>\<ImageBaseName>.zip
    baConvertToST,          // Convert image (.msa/.dim/.zip) to .st raw image
    baConvertToMSA,         // Convert image (.st/.dim/.zip) to .msa compressed image
    baConvertToDIM,         // Convert image (.st/.msa/.zip) to .dim FastCopy Pro image
    baExportCatalogHTML,    // Generate <ImageBaseName>_catalog.html
    baExportCatalogCSV,     // Generate <ImageBaseName>_catalog.csv
    baExportCatalogJSON,    // Generate <ImageBaseName>_catalog.json
    baExportCatalogText     // Generate <ImageBaseName>_catalog.txt
  );

  { Status of an individual item in a batch }
  TBatchItemStatus = (
    bisPending,
    bisProcessing,
    bisSuccess,
    bisSkipped,
    bisError
  );

  { Configuration options for batch execution }
  TBatchConvertOptions = record
    Action: TBatchAction;
    OutputDir: string;          // Destination directory (if UseSourceDir is False)
    UseSourceDir: Boolean;       // If True, outputs to same folder as input file
    DepackFiles: Boolean;        // Depack Atomik, Ice, Automation, etc. upon extraction
    OverwriteExisting: Boolean;  // Overwrite existing files/dirs
    IncludeDeleted: Boolean;     // Include deleted files if recoverable
  end;

  { Result summary for a processed disk image }
  TBatchItemResult = record
    InputFile: string;
    OutputFile: string;          // Output folder, zip path, converted image path, or catalog path
    Status: TBatchItemStatus;
    ErrorMessage: string;
    FilesProcessed: Integer;
    FilesDepacked: Integer;
    BytesProcessed: Int64;
    DurationMs: Int64;
  end;

  { Callback types for progress reporting and logging }
  TBatchProgressProc = procedure(const CurrentFile: string; FileIndex, TotalFiles: Integer;
    const StatusMsg: string; var Abort: Boolean) of object;
  TBatchLogProc = procedure(const Msg: string) of object;

{ Returns human-readable description of a batch action }
function BatchActionToString(Action: TBatchAction): string;

{ Normalizes and safely creates a target subfolder for an image }
function GetImageOutputSubfolder(const InputImagePath, BaseOutputDir: string): string;

{ Recursively adds all files in ABaseDir + ARelPath to a TZipper instance }
function AddDirectoryToZipper(const ABaseDir, ARelPath: string; AZip: TZipper): Integer;

{ Extracts all files from a FAT12 image stream to a target filesystem directory.
  If DepackFiles is True, any compressed file recognized by unit_depacker is depacked. }
function ExtractFat12StreamToFolder(Stream: TStream; const DestFolder: string;
  DepackFiles, OverwriteExisting: Boolean; out FilesExtracted, FilesDepacked: Integer;
  out TotalBytes: Int64; OnLog: TBatchLogProc = nil): Boolean;

{ Extracts all files from an image file (.st, .msa, .dim) to a target folder }
function ExtractImageToFolder(const InputFile, DestFolder: string;
  DepackFiles, OverwriteExisting: Boolean; out FilesExtracted, FilesDepacked: Integer;
  out TotalBytes: Int64; OnLog: TBatchLogProc = nil): Boolean;

{ Extracts all files from an image file directly into a .zip archive }
function ExtractImageToZip(const InputFile, DestZipPath: string;
  DepackFiles, OverwriteExisting: Boolean; out FilesExtracted, FilesDepacked: Integer;
  out TotalBytes: Int64; OnLog: TBatchLogProc = nil): Boolean;

{ Converts an image file to another format (.st, .msa, .dim) }
function ConvertImageFile(const InputFile, DestFile: string; const TargetExt: string;
  OverwriteExisting: Boolean; OnLog: TBatchLogProc = nil): Boolean;

{ Scans a directory for supported disk images (.st, .msa, .dim, .zip) }
function CollectSupportedImagesInDir(const DirPath: string; Recursive: Boolean;
  OutList: TStrings): Integer;

{ High-level single-item processor adhering to TBatchConvertOptions.
  Handles .st, .msa, .dim, and .zip (extracting contained disk images). }
function ProcessBatchImage(const InputFile: string; const Options: TBatchConvertOptions;
  out ResultInfo: TBatchItemResult; OnProgress: TBatchProgressProc = nil;
  OnLog: TBatchLogProc = nil): Boolean;

{ Safe file and directory helpers }
function SafeCopyFile(const Src, Dst: string): Boolean;
function SafeDeleteDir(const DirPath: string): Boolean;
function GetFileSizeBytes(const APath: string): Int64;

implementation

function SafeCopyFile(const Src, Dst: string): Boolean;
var
  SrcStream, DstStream: TFileStream;
begin
  Result := False;
  try
    SrcStream := TFileStream.Create(Src, fmOpenRead or fmShareDenyNone);
    try
      DstStream := TFileStream.Create(Dst, fmCreate);
      try
        DstStream.CopyFrom(SrcStream, SrcStream.Size);
        Result := True;
      finally
        DstStream.Free;
      end;
    finally
      SrcStream.Free;
    end;
  except
    Result := False;
  end;
end;

function SafeDeleteDir(const DirPath: string): Boolean;
var
  SR: TSearchRec;
  P: string;
begin
  Result := True;
  P := IncludeTrailingPathDelimiter(DirPath);
  if FindFirst(P + '*', faAnyFile, SR) = 0 then
  try
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then Continue;
      if (SR.Attr and faDirectory) <> 0 then
        SafeDeleteDir(P + SR.Name)
      else
        SysUtils.DeleteFile(P + SR.Name);
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
  RemoveDir(DirPath);
end;

function GetFileSizeBytes(const APath: string): Int64;
var
  FS: TFileStream;
begin
  Result := 0;
  if not FileExists(APath) then Exit;
  try
    FS := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
    try
      Result := FS.Size;
    finally
      FS.Free;
    end;
  except
    Result := 0;
  end;
end;

function BatchActionToString(Action: TBatchAction): string;
begin
  case Action of
    baExtractFolders:    Result := 'Extract to Subfolders';
    baExtractZips:       Result := 'Extract to ZIP Archives';
    baConvertToST:       Result := 'Convert to Raw ST (.st)';
    baConvertToMSA:      Result := 'Convert to MSA Compressed (.msa)';
    baConvertToDIM:      Result := 'Convert to FastCopy Pro (.dim)';
    baExportCatalogHTML: Result := 'Export Catalog (HTML)';
    baExportCatalogCSV:  Result := 'Export Catalog (CSV)';
    baExportCatalogJSON: Result := 'Export Catalog (JSON)';
    baExportCatalogText: Result := 'Export Catalog (TXT)';
  else
    Result := 'Unknown Action';
  end;
end;

function GetImageOutputSubfolder(const InputImagePath, BaseOutputDir: string): string;
var
  BaseName: string;
begin
  BaseName := ChangeFileExt(ExtractFileName(InputImagePath), '');
  if BaseOutputDir <> '' then
    Result := IncludeTrailingPathDelimiter(BaseOutputDir) + BaseName
  else
    Result := IncludeTrailingPathDelimiter(ExtractFilePath(InputImagePath)) + BaseName;
end;

function AddDirectoryToZipper(const ABaseDir, ARelPath: string; AZip: TZipper): Integer;
var
  SR: TSearchRec;
  DiskFile, ArchiveRel: string;
  FullSearchDir: string;
begin
  Result := 0;
  FullSearchDir := IncludeTrailingPathDelimiter(ABaseDir + ARelPath);
  if FindFirst(FullSearchDir + '*', faAnyFile, SR) = 0 then
  try
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then Continue;
      if (SR.Attr and faDirectory) <> 0 then
      begin
        Result := Result + AddDirectoryToZipper(ABaseDir, ARelPath + SR.Name + '/', AZip);
      end
      else
      begin
        DiskFile := IncludeTrailingPathDelimiter(ABaseDir) + ARelPath + SR.Name;
        ArchiveRel := ARelPath + SR.Name;
        ArchiveRel := StringReplace(ArchiveRel, '\', '/', [rfReplaceAll]);
        AZip.Entries.AddFileEntry(DiskFile, ArchiveRel);
        Inc(Result);
      end;
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
end;

function ExtractFat12StreamToFolder(Stream: TStream; const DestFolder: string;
  DepackFiles, OverwriteExisting: Boolean; out FilesExtracted, FilesDepacked: Integer;
  out TotalBytes: Int64; OnLog: TBatchLogProc = nil): Boolean;
var
  BootSec: array[0..511] of Byte;
  Bpb: TFatBpb;
  Layout: TFatLayout;
  Entries: TFat12EntryArray;
  I: Integer;
  Entry: TFat12EntryInfo;
  NormalizedRelDir, FullTargetDir, FullTargetFile: string;
  RawBytes, OutBytes: TBytes;
  PackerName: string;
  FS: TFileStream;
  WasDepacked: Boolean;
begin
  Result := False;
  FilesExtracted := 0;
  FilesDepacked := 0;
  TotalBytes := 0;

  if (Stream = nil) or (Stream.Size < 512) then
  begin
    if Assigned(OnLog) then OnLog('Error: Stream is empty or shorter than 512 bytes.');
    Exit;
  end;

  Stream.Position := 0;
  Stream.ReadBuffer(BootSec[0], 512);
  Fat12ParseBpb(@BootSec[0], 512, Stream.Size, Bpb);
  Fat12ComputeLayout(Bpb, Stream.Size, Layout);

  Entries := Fat12ScanAllEntries(Stream, Bpb, Layout, False);

  if not ForceDirectories(DestFolder) then
  begin
    if Assigned(OnLog) then OnLog('Error: Failed to create target directory: ' + DestFolder);
    Exit;
  end;

  for I := 0 to High(Entries) do
  begin
    Entry := Entries[I];

    // Normalize path separators
    NormalizedRelDir := Entry.DirPath;
    NormalizedRelDir := StringReplace(NormalizedRelDir, '/', DirectorySeparator, [rfReplaceAll]);
    NormalizedRelDir := StringReplace(NormalizedRelDir, '\', DirectorySeparator, [rfReplaceAll]);
    while (Length(NormalizedRelDir) > 0) and (NormalizedRelDir[1] = DirectorySeparator) do
      Delete(NormalizedRelDir, 1, 1);

    if NormalizedRelDir <> '' then
      FullTargetDir := IncludeTrailingPathDelimiter(DestFolder) + NormalizedRelDir
    else
      FullTargetDir := DestFolder;

    if Entry.IsDir then
    begin
      // Create subdirectory
      FullTargetDir := IncludeTrailingPathDelimiter(FullTargetDir) + Entry.FileName;
      ForceDirectories(FullTargetDir);
      Continue;
    end;

    // It's a file
    ForceDirectories(FullTargetDir);
    FullTargetFile := IncludeTrailingPathDelimiter(FullTargetDir) + Entry.FileName;

    if FileExists(FullTargetFile) and not OverwriteExisting then
    begin
      if Assigned(OnLog) then OnLog('  Skipped existing: ' + Entry.FullPath);
      Continue;
    end;

    // Read file payload from cluster chain
    RawBytes := Fat12ReadFileData(Stream, Layout, Entry.StartCluster, Entry.FileSize);
    WasDepacked := False;

    // Attempt depacking if requested
    if DepackFiles and (Length(RawBytes) >= 16) then
    begin
      SetLength(OutBytes, 0);
      PackerName := '';
      try
        if DepackBuffer(PByte(RawBytes), Length(RawBytes), OutBytes, PackerName) and
           (Length(OutBytes) > 0) then
        begin
          RawBytes := OutBytes;
          WasDepacked := True;
          Inc(FilesDepacked);
          if Assigned(OnLog) then
            OnLog(Format('  Extracted & Depacked: %s (%s, %d -> %d bytes)',
              [Entry.FullPath, PackerName, Entry.FileSize, Length(RawBytes)]));
        end;
      except
        on E: Exception do
        begin
          if Assigned(OnLog) then
            OnLog(Format('  Warning: Depacking failed for %s (%s): %s', [Entry.FullPath, PackerName, E.Message]));
        end;
      end;
    end;

    if not WasDepacked and Assigned(OnLog) then
      OnLog(Format('  Extracted: %s (%d bytes)', [Entry.FullPath, Length(RawBytes)]));

    // Write file to disk
    try
      FS := TFileStream.Create(FullTargetFile, fmCreate);
      try
        if Length(RawBytes) > 0 then
          FS.WriteBuffer(RawBytes[0], Length(RawBytes));
      finally
        FS.Free;
      end;

      // Preserve authentic Atari ST file timestamp
      if Entry.DateTime > 0 then
      begin
        try
          FileSetDate(FullTargetFile, DateTimeToFileDate(Entry.DateTime));
        except
          // Ignore timestamp errors on host filesystems that do not support setting dates
        end;
      end;

      Inc(FilesExtracted);
      TotalBytes := TotalBytes + Length(RawBytes);
    except
      on E: Exception do
      begin
        if Assigned(OnLog) then
          OnLog(Format('  Error writing %s: %s', [FullTargetFile, E.Message]));
      end;
    end;
  end;

  Result := True;
end;

function ExtractImageToFolder(const InputFile, DestFolder: string;
  DepackFiles, OverwriteExisting: Boolean; out FilesExtracted, FilesDepacked: Integer;
  out TotalBytes: Int64; OnLog: TBatchLogProc = nil): Boolean;
var
  Ext: string;
  FS: TFileStream;
  MemStream: TMemoryStream;
begin
  Result := False;
  FilesExtracted := 0;
  FilesDepacked := 0;
  TotalBytes := 0;

  if not FileExists(InputFile) then
  begin
    if Assigned(OnLog) then OnLog('Error: Input file does not exist: ' + InputFile);
    Exit;
  end;

  Ext := LowerCase(ExtractFileExt(InputFile));

  if Ext = '.st' then
  begin
    FS := TFileStream.Create(InputFile, fmOpenRead or fmShareDenyNone);
    try
      Result := ExtractFat12StreamToFolder(FS, DestFolder, DepackFiles, OverwriteExisting,
        FilesExtracted, FilesDepacked, TotalBytes, OnLog);
    finally
      FS.Free;
    end;
  end
  else if Ext = '.msa' then
  begin
    MemStream := TMemoryStream.Create;
    try
      if Assigned(OnLog) then OnLog('Decompressing MSA image to memory stream...');
      if not UnpackMSAToStream(InputFile, MemStream) then
      begin
        if Assigned(OnLog) then OnLog('Error: Failed to unpack MSA image: ' + InputFile);
        Exit;
      end;
      Result := ExtractFat12StreamToFolder(MemStream, DestFolder, DepackFiles, OverwriteExisting,
        FilesExtracted, FilesDepacked, TotalBytes, OnLog);
    finally
      MemStream.Free;
    end;
  end
  else if Ext = '.dim' then
  begin
    MemStream := TMemoryStream.Create;
    try
      if Assigned(OnLog) then OnLog('Decompressing DIM image to memory stream...');
      if not UnpackDIMToStream(InputFile, MemStream) then
      begin
        if Assigned(OnLog) then OnLog('Error: Failed to unpack DIM image: ' + InputFile);
        Exit;
      end;
      Result := ExtractFat12StreamToFolder(MemStream, DestFolder, DepackFiles, OverwriteExisting,
        FilesExtracted, FilesDepacked, TotalBytes, OnLog);
    finally
      MemStream.Free;
    end;
  end
  else
  begin
    if Assigned(OnLog) then OnLog('Error: Unsupported image extension: ' + Ext);
  end;
end;

function ExtractImageToZip(const InputFile, DestZipPath: string;
  DepackFiles, OverwriteExisting: Boolean; out FilesExtracted, FilesDepacked: Integer;
  out TotalBytes: Int64; OnLog: TBatchLogProc = nil): Boolean;
var
  TempStagingDir: string;
  Zip: TZipper;
  ItemCount: Integer;
begin
  Result := False;
  FilesExtracted := 0;
  FilesDepacked := 0;
  TotalBytes := 0;

  if FileExists(DestZipPath) and not OverwriteExisting then
  begin
    if Assigned(OnLog) then OnLog('Skipped existing ZIP archive: ' + DestZipPath);
    Result := True;
    Exit;
  end;

  TempStagingDir := IncludeTrailingPathDelimiter(GetTempDir) + 'stimage_stage_' +
    FormatDateTime('yyyymmddhhnnsszzz', Now) + '_' + IntToStr(Random(10000)) + DirectorySeparator;

  if not ForceDirectories(TempStagingDir) then
  begin
    if Assigned(OnLog) then OnLog('Error: Failed to create temp staging directory: ' + TempStagingDir);
    Exit;
  end;

  try
    if not ExtractImageToFolder(InputFile, TempStagingDir, DepackFiles, True,
      FilesExtracted, FilesDepacked, TotalBytes, OnLog) then
    begin
      Exit;
    end;

    if FileExists(DestZipPath) then
      SysUtils.DeleteFile(DestZipPath);

    ForceDirectories(ExtractFilePath(DestZipPath));

    Zip := TZipper.Create;
    try
      Zip.FileName := DestZipPath;
      ItemCount := AddDirectoryToZipper(TempStagingDir, '', Zip);
      if Assigned(OnLog) then
        OnLog(Format('Creating ZIP archive with %d file(s): %s', [ItemCount, DestZipPath]));
      Zip.ZipAllFiles;
      Result := FileExists(DestZipPath);
    finally
      Zip.Free;
    end;
  finally
    SafeDeleteDir(TempStagingDir);
  end;
end;

function ConvertImageFile(const InputFile, DestFile: string; const TargetExt: string;
  OverwriteExisting: Boolean; OnLog: TBatchLogProc = nil): Boolean;
var
  SrcExt, DstExt, TempSTPath: string;
  BootSec: array[0..511] of Byte;
  FS: TFileStream;
  Bpb: TFatBpb;
  SPT, Sides, Tracks: Integer;
begin
  Result := False;
  if not FileExists(InputFile) then
  begin
    if Assigned(OnLog) then OnLog('Error: Source file not found: ' + InputFile);
    Exit;
  end;

  if FileExists(DestFile) and not OverwriteExisting then
  begin
    if Assigned(OnLog) then OnLog('Skipped existing target: ' + DestFile);
    Result := True;
    Exit;
  end;

  SrcExt := LowerCase(ExtractFileExt(InputFile));
  DstExt := LowerCase(TargetExt);
  if (DstExt <> '') and (DstExt[1] <> '.') then DstExt := '.' + DstExt;

  ForceDirectories(ExtractFilePath(DestFile));

  // 1. Target is .st
  if DstExt = '.st' then
  begin
    if SrcExt = '.st' then
    begin
      // Direct copy if paths differ
      if ExpandFileName(InputFile) <> ExpandFileName(DestFile) then
      begin
        if Assigned(OnLog) then OnLog(Format('Copying .st image: %s -> %s', [InputFile, DestFile]));
        Result := SafeCopyFile(InputFile, DestFile);
      end
      else
        Result := True;
    end
    else if SrcExt = '.msa' then
    begin
      if Assigned(OnLog) then OnLog(Format('Unpacking MSA to ST: %s -> %s', [InputFile, DestFile]));
      Result := UnpackMSAToST(InputFile, DestFile);
    end
    else if SrcExt = '.dim' then
    begin
      if Assigned(OnLog) then OnLog(Format('Unpacking DIM to ST: %s -> %s', [InputFile, DestFile]));
      Result := UnpackDIMToST(InputFile, DestFile);
    end;
    Exit;
  end;

  // 2. Target is .msa
  if DstExt = '.msa' then
  begin
    if SrcExt = '.msa' then
    begin
      if ExpandFileName(InputFile) <> ExpandFileName(DestFile) then
        Result := SafeCopyFile(InputFile, DestFile)
      else
        Result := True;
      Exit;
    end;

    // If source is .dim, convert to temp .st first
    if SrcExt = '.dim' then
    begin
      TempSTPath := IncludeTrailingPathDelimiter(GetTempDir) + 'temp_conv_' +
        FormatDateTime('yyyymmddhhnnsszzz', Now) + '.st';
      try
        if not UnpackDIMToST(InputFile, TempSTPath) then Exit;
        Result := ConvertImageFile(TempSTPath, DestFile, '.msa', OverwriteExisting, OnLog);
      finally
        if FileExists(TempSTPath) then SysUtils.DeleteFile(TempSTPath);
      end;
      Exit;
    end;

    // Source is .st -> pack to .msa
    SPT := 9;
    Sides := 2;
    try
      FS := TFileStream.Create(InputFile, fmOpenRead or fmShareDenyNone);
      try
        if FS.Size >= 512 then
        begin
          FS.ReadBuffer(BootSec[0], 512);
          Fat12ParseBpb(@BootSec[0], 512, FS.Size, Bpb);
          if (Bpb.SectorsPerTrack >= 8) and (Bpb.SectorsPerTrack <= 24) then
            SPT := Bpb.SectorsPerTrack;
          if (Bpb.Sides >= 1) and (Bpb.Sides <= 2) then
            Sides := Bpb.Sides;
        end;
      finally
        FS.Free;
      end;
    except
      SPT := 9;
      Sides := 2;
    end;

    if Assigned(OnLog) then
      OnLog(Format('Compressing ST to MSA (%d sectors/track, %d side(s)): %s -> %s',
        [SPT, Sides, InputFile, DestFile]));
    Result := PackSTToMSA(InputFile, DestFile, SPT, Sides);
    Exit;
  end;

  // 3. Target is .dim
  if DstExt = '.dim' then
  begin
    if SrcExt = '.dim' then
    begin
      if ExpandFileName(InputFile) <> ExpandFileName(DestFile) then
        Result := SafeCopyFile(InputFile, DestFile)
      else
        Result := True;
      Exit;
    end;

    if SrcExt = '.msa' then
    begin
      TempSTPath := IncludeTrailingPathDelimiter(GetTempDir) + 'temp_conv_' +
        FormatDateTime('yyyymmddhhnnsszzz', Now) + '.st';
      try
        if not UnpackMSAToST(InputFile, TempSTPath) then Exit;
        Result := ConvertImageFile(TempSTPath, DestFile, '.dim', OverwriteExisting, OnLog);
      finally
        if FileExists(TempSTPath) then SysUtils.DeleteFile(TempSTPath);
      end;
      Exit;
    end;

    // Source is .st -> pack to .dim
    SPT := 9;
    Sides := 2;
    Tracks := 80;
    try
      FS := TFileStream.Create(InputFile, fmOpenRead or fmShareDenyNone);
      try
        if FS.Size >= 512 then
        begin
          FS.ReadBuffer(BootSec[0], 512);
          Fat12ParseBpb(@BootSec[0], 512, FS.Size, Bpb);
          if (Bpb.SectorsPerTrack >= 8) and (Bpb.SectorsPerTrack <= 24) then
            SPT := Bpb.SectorsPerTrack;
          if (Bpb.Sides >= 1) and (Bpb.Sides <= 2) then
            Sides := Bpb.Sides;
          if (SPT > 0) and (Sides > 0) and (FS.Size >= 512) then
            Tracks := Round(FS.Size / (512 * SPT * Sides));
          if (Tracks < 40) or (Tracks > 85) then Tracks := 80;
        end;
      finally
        FS.Free;
      end;
    except
      SPT := 9;
      Sides := 2;
      Tracks := 80;
    end;

    if Assigned(OnLog) then
      OnLog(Format('Packaging ST to FastCopy Pro DIM (%d spt, %d sides, %d tracks): %s -> %s',
        [SPT, Sides, Tracks, InputFile, DestFile]));
    Result := PackSTToDIM(InputFile, DestFile, SPT, Sides, Tracks);
    Exit;
  end;

  if Assigned(OnLog) then OnLog('Error: Unrecognized destination format: ' + DstExt);
end;

function CollectSupportedImagesInDir(const DirPath: string; Recursive: Boolean;
  OutList: TStrings): Integer;
var
  SR: TSearchRec;
  Ext, TargetDir: string;
begin
  Result := 0;
  if (OutList = nil) or not DirectoryExists(DirPath) then Exit;

  TargetDir := IncludeTrailingPathDelimiter(DirPath);
  if FindFirst(TargetDir + '*', faAnyFile, SR) = 0 then
  try
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then Continue;
      if (SR.Attr and faDirectory) <> 0 then
      begin
        if Recursive then
          Result := Result + CollectSupportedImagesInDir(TargetDir + SR.Name, True, OutList);
      end
      else
      begin
        Ext := LowerCase(ExtractFileExt(SR.Name));
        if (Ext = '.st') or (Ext = '.msa') or (Ext = '.dim') or (Ext = '.zip') then
        begin
          OutList.Add(TargetDir + SR.Name);
          Inc(Result);
        end;
      end;
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
end;

function ProcessSingleDiskImage(const DiskImagePath, BaseOutputDir: string;
  const Options: TBatchConvertOptions; out ResultInfo: TBatchItemResult;
  OnLog: TBatchLogProc): Boolean;
var
  TargetDest, TargetExt: string;
  FilesExt, FilesDep: Integer;
  BytesProc: Int64;
  Catalog: TDiskCatalog;
  FS: TFileStream;
  MemStream: TMemoryStream;
  ImageExt: string;
begin
  Result := False;
  ResultInfo.InputFile := DiskImagePath;
  ResultInfo.Status := bisProcessing;
  ResultInfo.FilesProcessed := 0;
  ResultInfo.FilesDepacked := 0;
  ResultInfo.BytesProcessed := 0;

  case Options.Action of
    baExtractFolders:
    begin
      TargetDest := GetImageOutputSubfolder(DiskImagePath, BaseOutputDir);
      ResultInfo.OutputFile := TargetDest;
      if Assigned(OnLog) then
        OnLog(Format('Extracting disk image to subfolder: %s', [TargetDest]));

      Result := ExtractImageToFolder(DiskImagePath, TargetDest, Options.DepackFiles,
        Options.OverwriteExisting, FilesExt, FilesDep, BytesProc, OnLog);

      ResultInfo.FilesProcessed := FilesExt;
      ResultInfo.FilesDepacked := FilesDep;
      ResultInfo.BytesProcessed := BytesProc;
    end;

    baExtractZips:
    begin
      TargetDest := IncludeTrailingPathDelimiter(BaseOutputDir) +
        ChangeFileExt(ExtractFileName(DiskImagePath), '') + '.zip';
      ResultInfo.OutputFile := TargetDest;
      if Assigned(OnLog) then
        OnLog(Format('Extracting disk image to ZIP archive: %s', [TargetDest]));

      Result := ExtractImageToZip(DiskImagePath, TargetDest, Options.DepackFiles,
        Options.OverwriteExisting, FilesExt, FilesDep, BytesProc, OnLog);

      ResultInfo.FilesProcessed := FilesExt;
      ResultInfo.FilesDepacked := FilesDep;
      ResultInfo.BytesProcessed := BytesProc;
    end;

    baConvertToST, baConvertToMSA, baConvertToDIM:
    begin
      case Options.Action of
        baConvertToST:  TargetExt := '.st';
        baConvertToMSA: TargetExt := '.msa';
        baConvertToDIM: TargetExt := '.dim';
      end;

      TargetDest := IncludeTrailingPathDelimiter(BaseOutputDir) +
        ChangeFileExt(ExtractFileName(DiskImagePath), '') + TargetExt;
      ResultInfo.OutputFile := TargetDest;

      Result := ConvertImageFile(DiskImagePath, TargetDest, TargetExt,
        Options.OverwriteExisting, OnLog);

      if Result and FileExists(TargetDest) then
      begin
        ResultInfo.FilesProcessed := 1;
        ResultInfo.BytesProcessed := GetFileSizeBytes(TargetDest);
      end;
    end;

    baExportCatalogHTML, baExportCatalogCSV, baExportCatalogJSON, baExportCatalogText:
    begin
      case Options.Action of
        baExportCatalogHTML: TargetExt := '.html';
        baExportCatalogCSV:  TargetExt := '.csv';
        baExportCatalogJSON: TargetExt := '.json';
        baExportCatalogText: TargetExt := '.txt';
      end;

      TargetDest := IncludeTrailingPathDelimiter(BaseOutputDir) +
        ChangeFileExt(ExtractFileName(DiskImagePath), '') + '_catalog' + TargetExt;
      ResultInfo.OutputFile := TargetDest;

      if FileExists(TargetDest) and not Options.OverwriteExisting then
      begin
        if Assigned(OnLog) then OnLog('Skipped existing catalog: ' + TargetDest);
        Result := True;
        Exit;
      end;

      Catalog := Default(TDiskCatalog);
      ImageExt := LowerCase(ExtractFileExt(DiskImagePath));
      if ImageExt = '.st' then
      begin
        FS := TFileStream.Create(DiskImagePath, fmOpenRead or fmShareDenyNone);
        try
          if BuildDiskCatalog(FS, DiskImagePath, '', 0, 9, 2, 80, Catalog, Options.IncludeDeleted) then
            Result := ExportCatalogToFile(Catalog, TargetDest);
        finally
          FS.Free;
        end;
      end
      else if ImageExt = '.msa' then
      begin
        MemStream := TMemoryStream.Create;
        try
          if UnpackMSAToStream(DiskImagePath, MemStream) then
          begin
            if BuildDiskCatalog(MemStream, DiskImagePath, '', 0, 9, 2, 80, Catalog, Options.IncludeDeleted) then
              Result := ExportCatalogToFile(Catalog, TargetDest);
          end;
        finally
          MemStream.Free;
        end;
      end
      else if ImageExt = '.dim' then
      begin
        MemStream := TMemoryStream.Create;
        try
          if UnpackDIMToStream(DiskImagePath, MemStream) then
          begin
            if BuildDiskCatalog(MemStream, DiskImagePath, '', 0, 9, 2, 80, Catalog, Options.IncludeDeleted) then
              Result := ExportCatalogToFile(Catalog, TargetDest);
          end;
        finally
          MemStream.Free;
        end;
      end;

      if Result then
      begin
        ResultInfo.FilesProcessed := Catalog.TotalFiles;
        ResultInfo.BytesProcessed := Catalog.TotalFileBytes;
        if Assigned(OnLog) then
          OnLog(Format('Exported disk catalog (%d files) to: %s', [Catalog.TotalFiles, TargetDest]));
      end;
    end;
  end;

  if Result then
    ResultInfo.Status := bisSuccess
  else
  begin
    ResultInfo.Status := bisError;
    ResultInfo.ErrorMessage := 'Failed to process disk image';
  end;
end;

function ProcessBatchImage(const InputFile: string; const Options: TBatchConvertOptions;
  out ResultInfo: TBatchItemResult; OnProgress: TBatchProgressProc = nil;
  OnLog: TBatchLogProc = nil): Boolean;
var
  Ext, BaseOutputDir: string;
  StartTime: QWord;
  UnZip: TUnZipper;
  I: Integer;
  EntryName, EntryExt, TempZipDir, ExtractedDiskPath: string;
  DiskEntriesFound: Integer;
  SubResult: TBatchItemResult;
  Abort: Boolean;
begin
  StartTime := GetTickCount64;
  Result := False;
  Abort := False;

  ResultInfo.InputFile := InputFile;
  ResultInfo.OutputFile := '';
  ResultInfo.Status := bisProcessing;
  ResultInfo.ErrorMessage := '';
  ResultInfo.FilesProcessed := 0;
  ResultInfo.FilesDepacked := 0;
  ResultInfo.BytesProcessed := 0;
  ResultInfo.DurationMs := 0;

  if not FileExists(InputFile) then
  begin
    ResultInfo.Status := bisError;
    ResultInfo.ErrorMessage := 'File not found: ' + InputFile;
    if Assigned(OnLog) then OnLog('Error: ' + ResultInfo.ErrorMessage);
    Exit;
  end;

  if Options.UseSourceDir or (Options.OutputDir = '') then
    BaseOutputDir := ExtractFilePath(InputFile)
  else
    BaseOutputDir := Options.OutputDir;

  Ext := LowerCase(ExtractFileExt(InputFile));

  // If the input file is a .ZIP archive, inspect it for disk images (.st, .msa, .dim)
  if Ext = '.zip' then
  begin
    if Assigned(OnLog) then OnLog('Scanning ZIP container: ' + ExtractFileName(InputFile));
    UnZip := TUnZipper.Create;
    try
      UnZip.FileName := InputFile;
      try
        UnZip.Examine;
      except
        on E: Exception do
        begin
          ResultInfo.Status := bisError;
          ResultInfo.ErrorMessage := 'Corrupt ZIP archive: ' + E.Message;
          if Assigned(OnLog) then OnLog('Error: ' + ResultInfo.ErrorMessage);
          Exit;
        end;
      end;

      DiskEntriesFound := 0;
      TempZipDir := IncludeTrailingPathDelimiter(GetTempDir) + 'stimage_zipbatch_' +
        FormatDateTime('yyyymmddhhnnsszzz', Now) + '_' + IntToStr(Random(10000)) + DirectorySeparator;
      ForceDirectories(TempZipDir);
      try
        UnZip.OutputPath := TempZipDir;
        UnZip.Flat := True;

        for I := 0 to UnZip.Entries.Count - 1 do
        begin
          if UnZip.Entries[I].IsDirectory then Continue;
          EntryName := UnZip.Entries[I].ArchiveFileName;
          EntryExt := LowerCase(ExtractFileExt(EntryName));

          if (EntryExt = '.st') or (EntryExt = '.msa') or (EntryExt = '.dim') then
          begin
            Inc(DiskEntriesFound);
            if Assigned(OnLog) then
              OnLog(Format('  Extracting contained disk image: %s', [EntryName]));

            ExtractedDiskPath := TempZipDir + ExtractFileName(EntryName);
            try
              UnZip.UnZipFile(EntryName);
            except
              on E: Exception do
              begin
                if Assigned(OnLog) then
                  OnLog('  Error unzipping entry: ' + E.Message);
                Continue;
              end;
            end;

            if FileExists(ExtractedDiskPath) then
            begin
              if ProcessSingleDiskImage(ExtractedDiskPath, BaseOutputDir, Options, SubResult, OnLog) then
              begin
                Result := True;
                ResultInfo.FilesProcessed := ResultInfo.FilesProcessed + SubResult.FilesProcessed;
                ResultInfo.FilesDepacked := ResultInfo.FilesDepacked + SubResult.FilesDepacked;
                ResultInfo.BytesProcessed := ResultInfo.BytesProcessed + SubResult.BytesProcessed;
                if ResultInfo.OutputFile = '' then
                  ResultInfo.OutputFile := SubResult.OutputFile;
              end;
              SysUtils.DeleteFile(ExtractedDiskPath);
            end;
          end;
        end;

        if DiskEntriesFound = 0 then
        begin
          ResultInfo.Status := bisSkipped;
          ResultInfo.ErrorMessage := 'No Atari disk images (.st, .msa, .dim) found in ZIP';
          if Assigned(OnLog) then OnLog('Notice: ' + ResultInfo.ErrorMessage);
          Exit;
        end;
      finally
        SafeDeleteDir(TempZipDir);
      end;
    finally
      UnZip.Free;
    end;
  end
  else
  begin
    // Standard disk image (.st, .msa, .dim)
    Result := ProcessSingleDiskImage(InputFile, BaseOutputDir, Options, ResultInfo, OnLog);
  end;

  ResultInfo.DurationMs := GetTickCount64 - StartTime;
  if Result then
    ResultInfo.Status := bisSuccess
  else if ResultInfo.Status <> bisSkipped then
    ResultInfo.Status := bisError;

  if Assigned(OnProgress) then
    OnProgress(InputFile, 1, 1, ResultInfo.ErrorMessage, Abort);
end;

end.
