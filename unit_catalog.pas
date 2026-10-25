unit unit_catalog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, StrUtils, unit_fat12, unit_depacker;

type
  { Detailed entry in the disk catalog }
  TDiskCatalogEntry = record
    DirPath: string;
    FileName: string;
    FullPath: string;
    FileSize: Int64;
    StartCluster: Word;
    ClusterCount: Integer;
    DateTime: TDateTime;
    DateTimeStr: string;
    Attr: Byte;
    AttrStr: string;
    IsDir: Boolean;
    IsDeleted: Boolean;
    PackerName: string;
    CRC32: LongWord;
    CRC32Str: string;
  end;

  { Complete disk catalog including geometry, filesystem metrics, and file listing }
  TDiskCatalog = record
    ImagePath: string;
    ImageFileName: string;
    ImageSize: Int64;
    ImageFormat: string;
    VolumeLabel: string;
    DiskSerial: LongWord;
    DiskSerialStr: string;
    Tracks: Integer;
    Sides: Integer;
    SectorsPerTrack: Integer;
    BytesPerSector: Integer;
    SectorsPerCluster: Integer;
    ClusterBytes: Integer;
    TotalClusters: Integer;
    UsedClusters: Integer;
    FreeClusters: Integer;
    TotalSpaceBytes: Int64;
    FreeSpaceBytes: Int64;
    UsedSpaceBytes: Int64;
    IsBootExecutable: Boolean;
    BootChecksum: Word;
    BootSectorStatus: string;
    Entries: array of TDiskCatalogEntry;
    TotalFiles: Integer;
    TotalDirs: Integer;
    TotalFileBytes: Int64;
    TotalPackedFiles: Integer;
  end;

{ Builds a comprehensive disk catalog from a stream }
function BuildDiskCatalog(AStream: TStream; const AImagePath, AVolumeLabel: string;
  ADiskSerial: LongWord; ASectorsPerTrack, ASides, ATracks: Integer;
  var Catalog: TDiskCatalog; IncludeDeleted: Boolean = False): Boolean;

{ Exporters }
function ExportCatalogToCSV(const Catalog: TDiskCatalog; const OutFileName: string): Boolean;
function ExportCatalogToText(const Catalog: TDiskCatalog; const OutFileName: string): Boolean;
function ExportCatalogToJSON(const Catalog: TDiskCatalog; const OutFileName: string): Boolean;
function ExportCatalogToHTML(const Catalog: TDiskCatalog; const OutFileName: string): Boolean;
function ExportCatalogToFile(const Catalog: TDiskCatalog; const OutFileName: string): Boolean;
function FormatBytes(Bytes: Int64): string;

implementation

function FormatBytes(Bytes: Int64): string;
begin
  if Bytes < 1024 then
    Result := Format('%d B', [Bytes])
  else if Bytes < 1024 * 1024 then
    Result := Format('%.1f KB (%s B)', [Bytes / 1024.0, FormatFloat('#,##0', Bytes)])
  else
    Result := Format('%.2f MB (%s B)', [Bytes / (1024.0 * 1024.0), FormatFloat('#,##0', Bytes)]);
end;

function EscapeJSON(const S: string): string;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    case C of
      '"':  Result := Result + '\"';
      '\':  Result := Result + '\\';
      #8:   Result := Result + '\b';
      #9:   Result := Result + '\t';
      #10:  Result := Result + '\n';
      #12:  Result := Result + '\f';
      #13:  Result := Result + '\r';
      else
        if Ord(C) < 32 then
          Result := Result + '\u' + IntToHex(Ord(C), 4)
        else
          Result := Result + C;
    end;
  end;
end;

function EscapeHTML(const S: string): string;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    case C of
      '&': Result := Result + '&amp;';
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '"': Result := Result + '&quot;';
      '''': Result := Result + '&#39;';
      else Result := Result + C;
    end;
  end;
end;

function BuildDiskCatalog(AStream: TStream; const AImagePath, AVolumeLabel: string;
  ADiskSerial: LongWord; ASectorsPerTrack, ASides, ATracks: Integer;
  var Catalog: TDiskCatalog; IncludeDeleted: Boolean = False): Boolean;
var
  BootSec: array[0..511] of Byte;
  Bpb: TFatBpb;
  Layout: TFatLayout;
  RawEntries: TFat12EntryArray;
  I, C, Val: Integer;
  FileData: TBytes;
  SavedPos: Int64;
  Packer: string;
  CRC: LongWord;
begin
  Result := False;
  Catalog := Default(TDiskCatalog);
  if (AStream = nil) or (AStream.Size < 512) then Exit;

  SavedPos := AStream.Position;
  try
    Catalog.ImagePath := AImagePath;
    Catalog.ImageFileName := ExtractFileName(AImagePath);
    Catalog.ImageSize := AStream.Size;
    Catalog.VolumeLabel := Trim(AVolumeLabel);
    if Catalog.VolumeLabel = '' then Catalog.VolumeLabel := 'NO NAME';
    Catalog.DiskSerial := ADiskSerial;
    Catalog.DiskSerialStr := '$' + IntToHex(ADiskSerial, 6);

    // Read Sector 0
    AStream.Position := 0;
    FillChar(BootSec, 512, 0);
    AStream.Read(BootSec[0], 512);

    Fat12ParseBpb(@BootSec[0], 512, AStream.Size, Bpb);
    Fat12ComputeLayout(Bpb, AStream.Size, Layout);

    // Format description
    if (ASectorsPerTrack > 0) then Catalog.SectorsPerTrack := ASectorsPerTrack else Catalog.SectorsPerTrack := Bpb.SectorsPerTrack;
    if (ASides > 0) then Catalog.Sides := ASides else Catalog.Sides := Bpb.Sides;
    if (ATracks > 0) then Catalog.Tracks := ATracks else Catalog.Tracks := 80;
    if Catalog.SectorsPerTrack = 0 then Catalog.SectorsPerTrack := 9;
    if Catalog.Sides = 0 then Catalog.Sides := 2;

    if (Catalog.Sides = 1) and (Catalog.SectorsPerTrack = 9) then
      Catalog.ImageFormat := 'Atari ST 360 KB (Single-Sided)'
    else if (Catalog.Sides = 2) and (Catalog.SectorsPerTrack = 9) then
      Catalog.ImageFormat := 'Atari ST 720 KB (Double-Sided)'
    else if (Catalog.Sides = 2) and (Catalog.SectorsPerTrack in [10, 11]) then
      Catalog.ImageFormat := Format('Atari ST Extended %d KB (%d Sectors/Track)', [Catalog.Tracks * Catalog.Sides * Catalog.SectorsPerTrack * 512 div 1024, Catalog.SectorsPerTrack])
    else if (Catalog.Sides = 2) and (Catalog.SectorsPerTrack = 18) then
      Catalog.ImageFormat := 'Atari ST 1.44 MB (High-Density)'
    else
      Catalog.ImageFormat := Format('Atari ST Custom Floppy (%d Tracks, %d Sides, %d Sectors)', [Catalog.Tracks, Catalog.Sides, Catalog.SectorsPerTrack]);

    Catalog.BytesPerSector := Layout.BytesPerCluster div Bpb.SectorsPerCluster;
    Catalog.SectorsPerCluster := Bpb.SectorsPerCluster;
    Catalog.ClusterBytes := Integer(Layout.BytesPerCluster);
    Catalog.TotalClusters := Layout.TotalClusters;

    // Boot sector execution state
    Catalog.BootChecksum := AtariBootChecksum(@BootSec[0], 512);
    Catalog.IsBootExecutable := (Catalog.BootChecksum = ATARI_BOOT_EXEC_SUM);
    if Catalog.IsBootExecutable then
      Catalog.BootSectorStatus := 'Executable ($1234 Checksum Validated)'
    else if (BootSec[0] = $EB) and (BootSec[2] = $90) then
      Catalog.BootSectorStatus := 'PC / MS-DOS Compatible'
    else
      Catalog.BootSectorStatus := 'Standard Non-executable';

    // Count FAT allocations
    Catalog.UsedClusters := 0;
    for C := 2 to Layout.TotalClusters + 1 do
    begin
      Val := Fat12ReadEntry(AStream, Layout.FatTableStart, C);
      if Val <> FAT12_ENTRY_FREE then
        Inc(Catalog.UsedClusters);
    end;
    Catalog.FreeClusters := Max(0, Catalog.TotalClusters - Catalog.UsedClusters);
    Catalog.TotalSpaceBytes := Int64(Catalog.TotalClusters) * Layout.BytesPerCluster;
    Catalog.UsedSpaceBytes := Int64(Catalog.UsedClusters) * Layout.BytesPerCluster;
    Catalog.FreeSpaceBytes := Int64(Catalog.FreeClusters) * Layout.BytesPerCluster;

    // Scan all directory entries
    RawEntries := Fat12ScanAllEntries(AStream, Bpb, Layout, IncludeDeleted);
    SetLength(Catalog.Entries, Length(RawEntries));

    Catalog.TotalFiles := 0;
    Catalog.TotalDirs := 0;
    Catalog.TotalFileBytes := 0;
    Catalog.TotalPackedFiles := 0;

    for I := 0 to High(RawEntries) do
    begin
      Catalog.Entries[I].DirPath := RawEntries[I].DirPath;
      Catalog.Entries[I].FileName := RawEntries[I].FileName;
      Catalog.Entries[I].FullPath := RawEntries[I].FullPath;
      Catalog.Entries[I].FileSize := RawEntries[I].FileSize;
      Catalog.Entries[I].StartCluster := RawEntries[I].StartCluster;
      Catalog.Entries[I].ClusterCount := RawEntries[I].ClusterCount;
      Catalog.Entries[I].DateTime := RawEntries[I].DateTime;
      Catalog.Entries[I].DateTimeStr := RawEntries[I].DateTimeStr;
      Catalog.Entries[I].Attr := RawEntries[I].Attr;
      Catalog.Entries[I].AttrStr := RawEntries[I].AttrStr;
      Catalog.Entries[I].IsDir := RawEntries[I].IsDir;
      Catalog.Entries[I].IsDeleted := RawEntries[I].IsDeleted;

      if RawEntries[I].IsDir then
      begin
        Inc(Catalog.TotalDirs);
        Catalog.Entries[I].PackerName := '-';
        Catalog.Entries[I].CRC32 := 0;
        Catalog.Entries[I].CRC32Str := '--------';
      end
      else
      begin
        Inc(Catalog.TotalFiles);
        Inc(Catalog.TotalFileBytes, RawEntries[I].FileSize);

        // Read file bytes to check packer and calculate CRC32
        FileData := Fat12ReadFileData(AStream, Layout, RawEntries[I].StartCluster, RawEntries[I].FileSize);
        if Length(FileData) > 0 then
        begin
          CRC := CalculateCRC32(@FileData[0], Length(FileData));
          Catalog.Entries[I].CRC32 := CRC;
          Catalog.Entries[I].CRC32Str := IntToHex(CRC, 8);

          Packer := DetectPackerSignature(@FileData[0], Length(FileData));
          Catalog.Entries[I].PackerName := Packer;
          if (Packer <> 'None') and (Pos('Atari ST Executable', Packer) = 0) then
            Inc(Catalog.TotalPackedFiles);
        end
        else
        begin
          Catalog.Entries[I].CRC32 := 0;
          Catalog.Entries[I].CRC32Str := '00000000';
          Catalog.Entries[I].PackerName := 'None';
        end;
      end;
    end;

    Result := True;
  finally
    AStream.Position := SavedPos;
  end;
end;

function ExportCatalogToCSV(const Catalog: TDiskCatalog; const OutFileName: string): Boolean;
var
  SL: TStringList;
  I: Integer;
  Line: string;
  EntryType: string;
begin
  Result := False;
  SL := TStringList.Create;
  try
    SL.Add('# STIMAGE Atari ST Disk Catalog');
    SL.Add(Format('# Image: %s (%s)', [Catalog.ImageFileName, FormatBytes(Catalog.ImageSize)]));
    SL.Add(Format('# Format: %s', [Catalog.ImageFormat]));
    SL.Add(Format('# Volume Label: %s | Serial: %s', [Catalog.VolumeLabel, Catalog.DiskSerialStr]));
    SL.Add(Format('# Capacity: Total %s, Used %s, Free %s', [FormatBytes(Catalog.TotalSpaceBytes), FormatBytes(Catalog.UsedSpaceBytes), FormatBytes(Catalog.FreeSpaceBytes)]));
    SL.Add(Format('# Boot Sector: %s (Checksum $%s)', [Catalog.BootSectorStatus, IntToHex(Catalog.BootChecksum, 4)]));
    SL.Add(Format('# Summary: %d Files (%d Packed), %d Directories, %s Total Bytes', [Catalog.TotalFiles, Catalog.TotalPackedFiles, Catalog.TotalDirs, FormatFloat('#,##0', Catalog.TotalFileBytes)]));
    SL.Add('#');
    SL.Add('"Path","FileName","FullPath","Type","SizeBytes","SizeFormatted","StartCluster","ClusterCount","DateTime","Attributes","Packer","CRC32"');

    for I := 0 to High(Catalog.Entries) do
    begin
      if Catalog.Entries[I].IsDir then EntryType := 'Directory'
      else if Catalog.Entries[I].IsDeleted then EntryType := 'Deleted File'
      else EntryType := 'File';

      Line := Format('"%s","%s","%s","%s",%d,"%s",%d,%d,"%s","%s","%s","%s"', [
        Catalog.Entries[I].DirPath,
        Catalog.Entries[I].FileName,
        Catalog.Entries[I].FullPath,
        EntryType,
        Catalog.Entries[I].FileSize,
        FormatFloat('#,##0', Catalog.Entries[I].FileSize),
        Catalog.Entries[I].StartCluster,
        Catalog.Entries[I].ClusterCount,
        Catalog.Entries[I].DateTimeStr,
        Catalog.Entries[I].AttrStr,
        Catalog.Entries[I].PackerName,
        Catalog.Entries[I].CRC32Str
      ]);
      SL.Add(Line);
    end;

    SL.SaveToFile(OutFileName);
    Result := True;
  finally
    SL.Free;
  end;
end;

function ExportCatalogToText(const Catalog: TDiskCatalog; const OutFileName: string): Boolean;
var
  SL: TStringList;
  I: Integer;
  PathCol, SizeCol, ClustCol, DateCol, AttrCol, PackerCol, CRCCol: string;
begin
  Result := False;
  SL := TStringList.Create;
  try
    SL.Add('========================================================================================================================');
    SL.Add(' STIMAGE - ATARI ST DISK CATALOG REPORT');
    SL.Add('========================================================================================================================');
    SL.Add(Format(' Image File:      %s (%s)', [Catalog.ImageFileName, FormatBytes(Catalog.ImageSize)]));
    SL.Add(Format(' Full Path:       %s', [Catalog.ImagePath]));
    SL.Add(Format(' Disk Format:     %s', [Catalog.ImageFormat]));
    SL.Add(Format(' Geometry:        %d Tracks, %d Sides, %d Sectors/Track (%d Bytes/Sector)', [Catalog.Tracks, Catalog.Sides, Catalog.SectorsPerTrack, Catalog.BytesPerSector]));
    SL.Add(Format(' Volume Label:    %-12s  |  Disk Serial Number: %s', [Catalog.VolumeLabel, Catalog.DiskSerialStr]));
    SL.Add(Format(' Cluster Layout:  %d Clusters Total (%d Bytes/Cluster)  |  %d Used  |  %d Free', [Catalog.TotalClusters, Catalog.ClusterBytes, Catalog.UsedClusters, Catalog.FreeClusters]));
    SL.Add(Format(' Space Usage:     Total: %s  |  Used: %s  |  Free: %s', [FormatBytes(Catalog.TotalSpaceBytes), FormatBytes(Catalog.UsedSpaceBytes), FormatBytes(Catalog.FreeSpaceBytes)]));
    SL.Add(Format(' Boot Sector:     %s (Checksum: $%s)', [Catalog.BootSectorStatus, IntToHex(Catalog.BootChecksum, 4)]));
    SL.Add(Format(' Disk Contents:   %d File(s) (%d Compressed), %d Folder(s), %s Bytes Total', [Catalog.TotalFiles, Catalog.TotalPackedFiles, Catalog.TotalDirs, FormatFloat('#,##0', Catalog.TotalFileBytes)]));
    SL.Add('========================================================================================================================');
    SL.Add(' Full Path                               Size            Cluster     Date & Time          Attr    Packer                 CRC32   ');
    SL.Add('------------------------------------------------------------------------------------------------------------------------');

    for I := 0 to High(Catalog.Entries) do
    begin
      PathCol := Catalog.Entries[I].FullPath;
      if Catalog.Entries[I].IsDir then PathCol := PathCol + '\';
      if Length(PathCol) > 38 then
        PathCol := Copy(PathCol, 1, 35) + '...'
      else
        PathCol := PadRight(PathCol, 38);

      if Catalog.Entries[I].IsDir then
        SizeCol := PadLeft('<DIR>', 14)
      else
        SizeCol := PadLeft(FormatFloat('#,##0 B', Catalog.Entries[I].FileSize), 14);

      ClustCol := PadLeft(Format('#%d', [Catalog.Entries[I].StartCluster]), 10);
      DateCol := PadRight(Catalog.Entries[I].DateTimeStr, 20);
      AttrCol := PadRight(Catalog.Entries[I].AttrStr, 7);

      PackerCol := Catalog.Entries[I].PackerName;
      if Length(PackerCol) > 21 then
        PackerCol := Copy(PackerCol, 1, 18) + '...'
      else
        PackerCol := PadRight(PackerCol, 21);

      CRCCol := Catalog.Entries[I].CRC32Str;

      SL.Add(Format(' %s  %s  %s  %s %s %s  %s', [PathCol, SizeCol, ClustCol, DateCol, AttrCol, PackerCol, CRCCol]));
    end;

    SL.Add('========================================================================================================================');
    SL.Add(Format(' End of Catalog: %d Total Item(s) Cataloged', [Length(Catalog.Entries)]));
    SL.Add('========================================================================================================================');

    SL.SaveToFile(OutFileName);
    Result := True;
  finally
    SL.Free;
  end;
end;

function ExportCatalogToJSON(const Catalog: TDiskCatalog; const OutFileName: string): Boolean;
var
  SL: TStringList;
  I: Integer;
  Comma: string;
begin
  Result := False;
  SL := TStringList.Create;
  try
    SL.Add('{');
    SL.Add('  "disk": {');
    SL.Add(Format('    "image_name": "%s",', [EscapeJSON(Catalog.ImageFileName)]));
    SL.Add(Format('    "image_path": "%s",', [EscapeJSON(Catalog.ImagePath)]));
    SL.Add(Format('    "image_size": %d,', [Catalog.ImageSize]));
    SL.Add(Format('    "image_format": "%s",', [EscapeJSON(Catalog.ImageFormat)]));
    SL.Add(Format('    "volume_label": "%s",', [EscapeJSON(Catalog.VolumeLabel)]));
    SL.Add(Format('    "serial_number": "%s",', [Catalog.DiskSerialStr]));
    SL.Add(Format('    "tracks": %d,', [Catalog.Tracks]));
    SL.Add(Format('    "sides": %d,', [Catalog.Sides]));
    SL.Add(Format('    "sectors_per_track": %d,', [Catalog.SectorsPerTrack]));
    SL.Add(Format('    "bytes_per_sector": %d,', [Catalog.BytesPerSector]));
    SL.Add(Format('    "sectors_per_cluster": %d,', [Catalog.SectorsPerCluster]));
    SL.Add(Format('    "cluster_bytes": %d,', [Catalog.ClusterBytes]));
    SL.Add(Format('    "total_clusters": %d,', [Catalog.TotalClusters]));
    SL.Add(Format('    "used_clusters": %d,', [Catalog.UsedClusters]));
    SL.Add(Format('    "free_clusters": %d,', [Catalog.FreeClusters]));
    SL.Add(Format('    "total_space_bytes": %d,', [Catalog.TotalSpaceBytes]));
    SL.Add(Format('    "used_space_bytes": %d,', [Catalog.UsedSpaceBytes]));
    SL.Add(Format('    "free_space_bytes": %d,', [Catalog.FreeSpaceBytes]));
    SL.Add(Format('    "boot_executable": %s,', [BoolToStr(Catalog.IsBootExecutable, 'true', 'false')]));
    SL.Add(Format('    "boot_checksum": "0x%s",', [IntToHex(Catalog.BootChecksum, 4)]));
    SL.Add(Format('    "boot_status": "%s",', [EscapeJSON(Catalog.BootSectorStatus)]));
    SL.Add(Format('    "total_files": %d,', [Catalog.TotalFiles]));
    SL.Add(Format('    "total_directories": %d,', [Catalog.TotalDirs]));
    SL.Add(Format('    "total_file_bytes": %d,', [Catalog.TotalFileBytes]));
    SL.Add(Format('    "total_packed_files": %d', [Catalog.TotalPackedFiles]));
    SL.Add('  },');
    SL.Add('  "files": [');

    for I := 0 to High(Catalog.Entries) do
    begin
      if I = High(Catalog.Entries) then Comma := '' else Comma := ',';
      SL.Add('    {');
      SL.Add(Format('      "dir_path": "%s",', [EscapeJSON(Catalog.Entries[I].DirPath)]));
      SL.Add(Format('      "file_name": "%s",', [EscapeJSON(Catalog.Entries[I].FileName)]));
      SL.Add(Format('      "full_path": "%s",', [EscapeJSON(Catalog.Entries[I].FullPath)]));
      SL.Add(Format('      "is_dir": %s,', [BoolToStr(Catalog.Entries[I].IsDir, 'true', 'false')]));
      SL.Add(Format('      "is_deleted": %s,', [BoolToStr(Catalog.Entries[I].IsDeleted, 'true', 'false')]));
      SL.Add(Format('      "file_size": %d,', [Catalog.Entries[I].FileSize]));
      SL.Add(Format('      "start_cluster": %d,', [Catalog.Entries[I].StartCluster]));
      SL.Add(Format('      "cluster_count": %d,', [Catalog.Entries[I].ClusterCount]));
      SL.Add(Format('      "date_time": "%s",', [EscapeJSON(Catalog.Entries[I].DateTimeStr)]));
      SL.Add(Format('      "attributes": "%s",', [EscapeJSON(Catalog.Entries[I].AttrStr)]));
      SL.Add(Format('      "packer": "%s",', [EscapeJSON(Catalog.Entries[I].PackerName)]));
      SL.Add(Format('      "crc32": "%s"', [Catalog.Entries[I].CRC32Str]));
      SL.Add('    }' + Comma);
    end;

    SL.Add('  ]');
    SL.Add('}');

    SL.SaveToFile(OutFileName);
    Result := True;
  finally
    SL.Free;
  end;
end;

function ExportCatalogToHTML(const Catalog: TDiskCatalog; const OutFileName: string): Boolean;
var
  SL: TStringList;
  I: Integer;
  TypeBadge, PackerBadge: string;
begin
  Result := False;
  SL := TStringList.Create;
  try
    SL.Add('<!DOCTYPE html>');
    SL.Add('<html lang="en">');
    SL.Add('<head>');
    SL.Add('  <meta charset="UTF-8">');
    SL.Add('  <meta name="viewport" content="width=device-width, initial-scale=1.0">');
    SL.Add(Format('  <title>Disk Catalog - %s</title>', [EscapeHTML(Catalog.ImageFileName)]));
    SL.Add('  <style>');
    SL.Add('    body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif; background: #121418; color: #E0E4EC; margin: 0; padding: 24px; line-height: 1.5; }');
    SL.Add('    .container { max-width: 1200px; margin: 0 auto; }');
    SL.Add('    .header { background: #1A1D24; border: 1px solid #2A2E38; border-radius: 8px; padding: 20px 24px; margin-bottom: 24px; }');
    SL.Add('    .header h1 { margin: 0 0 12px 0; font-size: 22px; color: #58A6FF; }');
    SL.Add('    .grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(220px, 1fr)); gap: 16px; margin-top: 16px; }');
    SL.Add('    .card { background: #222630; border-radius: 6px; padding: 12px 16px; border: 1px solid #303644; }');
    SL.Add('    .card .label { font-size: 11px; text-transform: uppercase; color: #8B949E; letter-spacing: 0.5px; }');
    SL.Add('    .card .value { font-size: 16px; font-weight: 600; color: #F0F6FC; margin-top: 4px; }');
    SL.Add('    table { width: 100%; border-collapse: collapse; background: #1A1D24; border: 1px solid #2A2E38; border-radius: 8px; overflow: hidden; font-size: 13px; }');
    SL.Add('    th { background: #222630; color: #8B949E; text-align: left; padding: 10px 14px; font-weight: 600; border-bottom: 1px solid #303644; }');
    SL.Add('    td { padding: 9px 14px; border-bottom: 1px solid #222630; }');
    SL.Add('    tr:hover { background: #20242D; }');
    SL.Add('    .mono { font-family: "SFMono-Regular", Consolas, "Liberation Mono", Menlo, monospace; }');
    SL.Add('    .badge { display: inline-block; padding: 2px 8px; border-radius: 4px; font-size: 11px; font-weight: 600; }');
    SL.Add('    .badge-file { background: #1F385C; color: #58A6FF; }');
    SL.Add('    .badge-dir { background: #403014; color: #D29922; }');
    SL.Add('    .badge-del { background: #4C1D1D; color: #F85149; }');
    SL.Add('    .badge-pack { background: #234228; color: #3FB950; }');
    SL.Add('    .footer { text-align: center; margin-top: 24px; color: #8B949E; font-size: 12px; }');
    SL.Add('  </style>');
    SL.Add('</head>');
    SL.Add('<body>');
    SL.Add('  <div class="container">');
    SL.Add('    <div class="header">');
    SL.Add(Format('      <h1>Atari ST Disk Catalog: %s</h1>', [EscapeHTML(Catalog.ImageFileName)]));
    SL.Add(Format('      <div>Format: <strong>%s</strong> | Label: <strong>%s</strong> | Serial: <strong>%s</strong></div>', [EscapeHTML(Catalog.ImageFormat), EscapeHTML(Catalog.VolumeLabel), Catalog.DiskSerialStr]));
    SL.Add('      <div class="grid">');
    SL.Add(Format('        <div class="card"><div class="label">Total Files</div><div class="value">%d (%d Packed)</div></div>', [Catalog.TotalFiles, Catalog.TotalPackedFiles]));
    SL.Add(Format('        <div class="card"><div class="label">Total Folders</div><div class="value">%d</div></div>', [Catalog.TotalDirs]));
    SL.Add(Format('        <div class="card"><div class="label">Total File Bytes</div><div class="value">%s</div></div>', [FormatBytes(Catalog.TotalFileBytes)]));
    SL.Add(Format('        <div class="card"><div class="label">Free Space</div><div class="value">%s</div></div>', [FormatBytes(Catalog.FreeSpaceBytes)]));
    SL.Add(Format('        <div class="card"><div class="label">Boot Sector</div><div class="value">%s</div></div>', [EscapeHTML(Catalog.BootSectorStatus)]));
    SL.Add('      </div>');
    SL.Add('    </div>');

    SL.Add('    <table>');
    SL.Add('      <thead>');
    SL.Add('        <tr>');
    SL.Add('          <th>Type</th>');
    SL.Add('          <th>Full Path</th>');
    SL.Add('          <th>Size</th>');
    SL.Add('          <th>Cluster</th>');
    SL.Add('          <th>Date &amp; Time</th>');
    SL.Add('          <th>Attr</th>');
    SL.Add('          <th>Packer</th>');
    SL.Add('          <th>CRC32</th>');
    SL.Add('        </tr>');
    SL.Add('      </thead>');
    SL.Add('      <tbody>');

    for I := 0 to High(Catalog.Entries) do
    begin
      if Catalog.Entries[I].IsDir then
        TypeBadge := '<span class="badge badge-dir">DIR</span>'
      else if Catalog.Entries[I].IsDeleted then
        TypeBadge := '<span class="badge badge-del">DEL</span>'
      else
        TypeBadge := '<span class="badge badge-file">FILE</span>';

      if (Catalog.Entries[I].PackerName <> 'None') and (Catalog.Entries[I].PackerName <> '-') and
         (Pos('Atari ST Executable', Catalog.Entries[I].PackerName) = 0) then
        PackerBadge := Format('<span class="badge badge-pack">%s</span>', [EscapeHTML(Catalog.Entries[I].PackerName)])
      else
        PackerBadge := EscapeHTML(Catalog.Entries[I].PackerName);

      SL.Add('        <tr>');
      SL.Add(Format('          <td>%s</td>', [TypeBadge]));
      SL.Add(Format('          <td class="mono"><strong>%s</strong></td>', [EscapeHTML(Catalog.Entries[I].FullPath)]));
      if Catalog.Entries[I].IsDir then
        SL.Add('          <td>&mdash;</td>')
      else
        SL.Add(Format('          <td class="mono">%s</td>', [FormatFloat('#,##0 B', Catalog.Entries[I].FileSize)]));
      SL.Add(Format('          <td class="mono">#%d</td>', [Catalog.Entries[I].StartCluster]));
      SL.Add(Format('          <td>%s</td>', [EscapeHTML(Catalog.Entries[I].DateTimeStr)]));
      SL.Add(Format('          <td class="mono">%s</td>', [EscapeHTML(Catalog.Entries[I].AttrStr)]));
      SL.Add(Format('          <td>%s</td>', [PackerBadge]));
      SL.Add(Format('          <td class="mono">%s</td>', [Catalog.Entries[I].CRC32Str]));
      SL.Add('        </tr>');
    end;

    SL.Add('      </tbody>');
    SL.Add('    </table>');
    SL.Add('    <div class="footer">Generated by STIMAGE &bull; Atari ST Disk Image Manager</div>');
    SL.Add('  </div>');
    SL.Add('</body>');
    SL.Add('</html>');

    SL.SaveToFile(OutFileName);
    Result := True;
  finally
    SL.Free;
  end;
end;

function ExportCatalogToFile(const Catalog: TDiskCatalog; const OutFileName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(OutFileName));
  if Ext = '.csv' then
    Result := ExportCatalogToCSV(Catalog, OutFileName)
  else if Ext = '.json' then
    Result := ExportCatalogToJSON(Catalog, OutFileName)
  else if (Ext = '.html') or (Ext = '.htm') then
    Result := ExportCatalogToHTML(Catalog, OutFileName)
  else
    Result := ExportCatalogToText(Catalog, OutFileName);
end;

end.
