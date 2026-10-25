unit unit_fat12;

{$mode objfpc}{$H+}

{ ------------------------------------------------------------------
  unit_fat12 - shared, GUI-free FAT12 / BIOS Parameter Block logic.

  Every unit that previously carried its own copy of the BPB parsing
  (unit1, unit_clustermap, unit_virusscanner, unit_search) and of the
  FAT12 entry packing now uses this single implementation.

  All arithmetic on sector / cluster / byte offsets is done in Int64 so
  that corrupt or oversized images can never trigger range or overflow
  errors, and every routine tolerates short or nil buffers.
  ------------------------------------------------------------------ }

interface

uses
  Classes, SysUtils, Math;

const
  FAT12_SECTOR_BYTES  = 512;   // Atari ST / PC floppy sector size
  FAT12_DIRENT_BYTES  = 32;    // Size of one directory entry
  FAT12_ENTRY_FREE    = $000;
  FAT12_ENTRY_BAD     = $FF7;
  FAT12_ENTRY_EOC_MIN = $FF8;  // First end-of-chain marker
  FAT12_ENTRY_EOC     = $FFF;
  FAT12_MAX_CLUSTERS  = 4084;  // Max data clusters of a FAT12 volume
  ATARI_BOOT_EXEC_SUM = $1234; // Word-sum that makes TOS execute a boot sector
  FAT12_ATTR_READONLY = $01;
  FAT12_ATTR_HIDDEN   = $02;
  FAT12_ATTR_SYSTEM   = $04;
  FAT12_ATTR_VOLUME   = $08;
  FAT12_ATTR_DIR      = $10;
  FAT12_ATTR_ARCHIVE  = $20;

type
  { Sanitised BIOS Parameter Block.  Out-of-range values are replaced with
    safe defaults; IsPlausible tells whether the raw values needed that. }
  TFatBpb = record
    BytesPerSector: Integer;
    SectorsPerCluster: Integer;
    ReservedSectors: Integer;
    NumberOfFATs: Integer;
    RootEntries: Integer;
    TotalSectors: Integer;     // BPB value, or derived from image size when 0
    MediaDescriptor: Byte;
    SectorsPerFAT: Integer;
    SectorsPerTrack: Integer;  // raw BPB value (may be 0 / invalid)
    Sides: Integer;            // raw BPB value (may be 0 / invalid)
    HiddenSectors: Integer;
    DiskSerial: LongWord;      // Atari 24-bit serial (boot sector $08..$0A)
    IsPlausible: Boolean;      // True when no fallback values had to be used
  end;

  { Byte offsets and sizes derived from a TFatBpb. }
  TFatLayout = record
    FatTableStart: Int64;
    SingleFATSize: Int64;
    FatTableSize: Int64;
    RootDirStart: Int64;
    RootDirSectors: Int64;
    DataAreaStart: Int64;
    BytesPerCluster: Int64;
    TotalClusters: Integer;
  end;

  { Detailed information for a file or directory entry found in a FAT12 volume }
  TFat12EntryInfo = record
    FileName: string;
    DirPath: string;
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
  end;
  TFat12EntryArray = array of TFat12EntryInfo;

{ Little-endian readers.  Reading outside Len returns 0. }
function ReadLE16(const P: PByte; Len, Offset: Integer): Word;
function ReadLE32(const P: PByte; Len, Offset: Integer): LongWord;

{ Standard IEEE 802.3 32-bit CRC }
function CalculateCRC32(const Data: PByte; Length: Integer): LongWord;

{ Formats 8.3 filename from a 32-byte FAT12 directory entry }
function Fat12FormatFileName(const DirEntry: PByte): string;

{ Formats attribute byte to human-readable string (e.g. "[DIR]", "[R.H.A]") }
function Fat12FormatAttrString(Attr: Byte): string;

{ Converts FAT date and time words to TDateTime (safe against invalid values) }
function Fat12ParseDateTime(FatDate, FatTime: Word): TDateTime;

{ 16-bit big-endian word sum of a 512-byte boot sector, modulo $10000.
  A result of ATARI_BOOT_EXEC_SUM marks the sector as executable. }
function AtariBootChecksum(const Sector: PByte; Size: Integer = 512): Word;

{ Parses and sanitises the BPB of a boot sector.  Sector may be nil or
  shorter than 512 bytes; missing bytes are treated as zero. }
procedure Fat12ParseBpb(const Sector: PByte; SectorLen: Integer;
  ImageSize: Int64; out Bpb: TFatBpb);

{ Writes the BPB fields (offsets $0B..$1D and the Atari serial) into Sector.
  Sector must have at least 512 bytes. }
procedure Fat12WriteBpb(Sector: PByte; SectorLen: Integer; const Bpb: TFatBpb);

{ Applies the three capacity limits shared by every FAT12 consumer:
  image file size, FAT table capacity and the FAT12 architectural maximum. }
function Fat12ClampClusters(RawClusters, ImageSize, DataAreaStart,
  BytesPerCluster, SingleFATSize: Int64): Integer;

{ Derives all offsets (FAT, root directory, data area) from a BPB. }
procedure Fat12ComputeLayout(const Bpb: TFatBpb; ImageSize: Int64;
  out L: TFatLayout);

{ Byte offset of the first byte of a data cluster (clusters start at 2). }
function Fat12ClusterOffset(const L: TFatLayout; Cluster: Integer): Int64;

{ In-memory FAT access.  Out-of-range reads return FAT12_ENTRY_EOC and
  out-of-range writes are ignored. }
function Fat12GetEntry(const Fat: PByte; FatLen, Cluster: Integer): Integer;
procedure Fat12SetEntry(Fat: PByte; FatLen, Cluster, Value: Integer);

{ Stream FAT access.  A short read returns FAT12_ENTRY_EOC. }
function Fat12ReadEntry(Stream: TStream; FatStart: Int64; Cluster: Integer): Integer;
procedure Fat12WriteEntryAllCopies(Stream: TStream; FatStart, SingleFATSize: Int64;
  NumFATs, Cluster, Value: Integer);

{ Recursively scans all directory entries from a FAT12 image stream }
function Fat12ScanAllEntries(Stream: TStream; const Bpb: TFatBpb;
  const Layout: TFatLayout; IncludeDeleted: Boolean = False): TFat12EntryArray;

{ Reads the complete byte content of a file from its cluster chain }
function Fat12ReadFileData(Stream: TStream; const Layout: TFatLayout;
  StartCluster: Word; FileSize: Int64): TBytes;

implementation

function ReadLE16(const P: PByte; Len, Offset: Integer): Word;
begin
  Result := 0;
  if (P = nil) or (Offset < 0) or (Offset + 2 > Len) then Exit;
  Result := Word(P[Offset]) or (Word(P[Offset + 1]) shl 8);
end;

function ReadLE32(const P: PByte; Len, Offset: Integer): LongWord;
begin
  Result := 0;
  if (P = nil) or (Offset < 0) or (Offset + 4 > Len) then Exit;
  Result := LongWord(P[Offset]) or (LongWord(P[Offset + 1]) shl 8) or
            (LongWord(P[Offset + 2]) shl 16) or (LongWord(P[Offset + 3]) shl 24);
end;

function AtariBootChecksum(const Sector: PByte; Size: Integer): Word;
var
  Sum: LongWord;
  I: Integer;
begin
  Result := 0;
  if (Sector = nil) or (Size < 512) then Exit;
  Sum := 0;
  for I := 0 to 255 do
    Sum := (Sum + ((LongWord(Sector[I * 2]) shl 8) or LongWord(Sector[I * 2 + 1]))) and $FFFF;
  Result := Word(Sum);
end;

procedure Fat12ParseBpb(const Sector: PByte; SectorLen: Integer;
  ImageSize: Int64; out Bpb: TFatBpb);
var
  Len: Integer;
  RawBPS, RawRes, RawRoot, RawSPF: Integer;
  RawSPC, RawFATs: Byte;
  Derived: Int64;
begin
  FillChar(Bpb, SizeOf(Bpb), 0);
  Len := SectorLen;
  if Sector = nil then Len := 0;
  if Len > FAT12_SECTOR_BYTES then Len := FAT12_SECTOR_BYTES;

  RawBPS  := ReadLE16(Sector, Len, $0B);
  if Len > $0D then RawSPC := Sector[$0D] else RawSPC := 0;
  RawRes  := ReadLE16(Sector, Len, $0E);
  if Len > $10 then RawFATs := Sector[$10] else RawFATs := 0;
  RawRoot := ReadLE16(Sector, Len, $11);
  RawSPF  := ReadLE16(Sector, Len, $16);

  Bpb.IsPlausible := True;

  Bpb.BytesPerSector := RawBPS;
  if Bpb.BytesPerSector <> FAT12_SECTOR_BYTES then
  begin
    Bpb.BytesPerSector := FAT12_SECTOR_BYTES;
    Bpb.IsPlausible := False;
  end;

  Bpb.SectorsPerCluster := RawSPC;
  if not (RawSPC in [1, 2, 4, 8]) then
  begin
    Bpb.SectorsPerCluster := 2;
    Bpb.IsPlausible := False;
  end;

  Bpb.ReservedSectors := RawRes;
  if (RawRes < 1) or (RawRes > 32) then
  begin
    Bpb.ReservedSectors := 1;
    Bpb.IsPlausible := False;
  end;

  Bpb.NumberOfFATs := RawFATs;
  if not (RawFATs in [1, 2]) then
  begin
    Bpb.NumberOfFATs := 2;
    Bpb.IsPlausible := False;
  end;

  Bpb.RootEntries := RawRoot;
  if (RawRoot < 16) or (RawRoot > 1024) then
  begin
    Bpb.RootEntries := 112;
    Bpb.IsPlausible := False;
  end;

  Bpb.TotalSectors := ReadLE16(Sector, Len, $13);
  if Bpb.TotalSectors = 0 then
  begin
    Derived := ImageSize div Bpb.BytesPerSector;
    if Derived < 0 then Derived := 0;
    if Derived > High(Integer) then Derived := High(Integer);
    Bpb.TotalSectors := Integer(Derived);
  end;

  if Len > $15 then Bpb.MediaDescriptor := Sector[$15];

  Bpb.SectorsPerFAT := RawSPF;
  if (RawSPF < 1) or (RawSPF > 32) then
  begin
    Bpb.SectorsPerFAT := 5;
    Bpb.IsPlausible := False;
  end;

  Bpb.SectorsPerTrack := ReadLE16(Sector, Len, $18);
  Bpb.Sides           := ReadLE16(Sector, Len, $1A);
  Bpb.HiddenSectors   := ReadLE16(Sector, Len, $1C);

  if Len > $0A then
    Bpb.DiskSerial := (LongWord(Sector[8]) shl 16) or
                      (LongWord(Sector[9]) shl 8) or LongWord(Sector[10]);
end;

procedure Fat12WriteBpb(Sector: PByte; SectorLen: Integer; const Bpb: TFatBpb);
  procedure Put16(Offset: Integer; Value: Integer);
  begin
    Sector[Offset]     := Byte(Value and $FF);
    Sector[Offset + 1] := Byte((Value shr 8) and $FF);
  end;
begin
  if (Sector = nil) or (SectorLen < FAT12_SECTOR_BYTES) then Exit;
  Sector[8]  := Byte((Bpb.DiskSerial shr 16) and $FF);
  Sector[9]  := Byte((Bpb.DiskSerial shr 8) and $FF);
  Sector[10] := Byte(Bpb.DiskSerial and $FF);
  Put16($0B, Bpb.BytesPerSector);
  Sector[$0D] := Byte(Bpb.SectorsPerCluster and $FF);
  Put16($0E, Bpb.ReservedSectors);
  Sector[$10] := Byte(Bpb.NumberOfFATs and $FF);
  Put16($11, Bpb.RootEntries);
  Put16($13, Bpb.TotalSectors);
  Sector[$15] := Bpb.MediaDescriptor;
  Put16($16, Bpb.SectorsPerFAT);
  Put16($18, Bpb.SectorsPerTrack);
  Put16($1A, Bpb.Sides);
  Put16($1C, Bpb.HiddenSectors);
end;

function Fat12ClampClusters(RawClusters, ImageSize, DataAreaStart,
  BytesPerCluster, SingleFATSize: Int64): Integer;
var
  R, MaxInFile, MaxInFAT: Int64;
begin
  R := RawClusters;
  if R < 0 then R := 0;

  // Cap by the real image size (truncated or non-standard images)
  if (BytesPerCluster > 0) and (ImageSize > DataAreaStart) then
  begin
    MaxInFile := (ImageSize - DataAreaStart) div BytesPerCluster;
    if (MaxInFile > 0) and (MaxInFile < R) then R := MaxInFile;
  end;

  // Cap by FAT table capacity (FAT12 uses 1.5 bytes per cluster entry)
  if SingleFATSize > 3 then
  begin
    MaxInFAT := (SingleFATSize * 2 div 3) - 2;
    if (MaxInFAT > 0) and (MaxInFAT < R) then R := MaxInFAT;
  end;

  // FAT12 architectural limit
  if R > FAT12_MAX_CLUSTERS then R := FAT12_MAX_CLUSTERS;
  Result := Integer(R);
end;

procedure Fat12ComputeLayout(const Bpb: TFatBpb; ImageSize: Int64;
  out L: TFatLayout);
var
  BPS, DataSectors: Int64;
begin
  FillChar(L, SizeOf(L), 0);
  BPS := Bpb.BytesPerSector;
  if BPS <= 0 then BPS := FAT12_SECTOR_BYTES;

  L.BytesPerCluster := Int64(Bpb.SectorsPerCluster) * BPS;
  L.SingleFATSize   := Int64(Bpb.SectorsPerFAT) * BPS;
  L.FatTableStart   := Int64(Bpb.ReservedSectors) * BPS;
  L.FatTableSize    := Int64(Bpb.NumberOfFATs) * L.SingleFATSize;
  L.RootDirStart    := L.FatTableStart + L.FatTableSize;
  L.RootDirSectors  := ((Int64(Bpb.RootEntries) * FAT12_DIRENT_BYTES) + (BPS - 1)) div BPS;
  L.DataAreaStart   := L.RootDirStart + L.RootDirSectors * BPS;

  DataSectors := Int64(Bpb.TotalSectors) -
    (Int64(Bpb.ReservedSectors) + Int64(Bpb.NumberOfFATs) * Bpb.SectorsPerFAT + L.RootDirSectors);
  if (DataSectors > 0) and (Bpb.SectorsPerCluster > 0) then
    L.TotalClusters := Fat12ClampClusters(DataSectors div Bpb.SectorsPerCluster,
      ImageSize, L.DataAreaStart, L.BytesPerCluster, L.SingleFATSize)
  else
    L.TotalClusters := 0;
end;

function Fat12ClusterOffset(const L: TFatLayout; Cluster: Integer): Int64;
begin
  if Cluster < 2 then
    Result := L.DataAreaStart
  else
    Result := L.DataAreaStart + Int64(Cluster - 2) * L.BytesPerCluster;
end;

function Fat12GetEntry(const Fat: PByte; FatLen, Cluster: Integer): Integer;
var
  Off: Int64;
  V: Word;
begin
  Result := FAT12_ENTRY_EOC;
  if (Fat = nil) or (Cluster < 0) then Exit;
  Off := (Int64(Cluster) * 3) div 2;
  if Off + 1 >= FatLen then Exit;
  V := Word(Fat[Off]) or (Word(Fat[Off + 1]) shl 8);
  if (Cluster and 1) = 0 then
    Result := V and $0FFF
  else
    Result := V shr 4;
end;

procedure Fat12SetEntry(Fat: PByte; FatLen, Cluster, Value: Integer);
var
  Off: Int64;
  V: Word;
begin
  if (Fat = nil) or (Cluster < 0) then Exit;
  Off := (Int64(Cluster) * 3) div 2;
  if Off + 1 >= FatLen then Exit;
  Value := Value and $0FFF;
  V := Word(Fat[Off]) or (Word(Fat[Off + 1]) shl 8);
  if (Cluster and 1) = 0 then
    V := (V and $F000) or Word(Value)
  else
    V := (V and $000F) or Word(Value shl 4);
  Fat[Off]     := Byte(V and $FF);
  Fat[Off + 1] := Byte(V shr 8);
end;

function Fat12ReadEntry(Stream: TStream; FatStart: Int64; Cluster: Integer): Integer;
var
  SavedPos: Int64;
  B: array[0..1] of Byte;
  V: Word;
begin
  Result := FAT12_ENTRY_EOC;
  if (Stream = nil) or (Cluster < 0) then Exit;
  SavedPos := Stream.Position;
  try
    B[0] := 0;
    B[1] := 0;
    Stream.Position := FatStart + (Int64(Cluster) * 3) div 2;
    if Stream.Read(B[0], 2) <> 2 then Exit;
    V := Word(B[0]) or (Word(B[1]) shl 8);
    if (Cluster and 1) = 0 then
      Result := V and $0FFF
    else
      Result := V shr 4;
  finally
    Stream.Position := SavedPos;
  end;
end;

procedure Fat12WriteEntryAllCopies(Stream: TStream; FatStart, SingleFATSize: Int64;
  NumFATs, Cluster, Value: Integer);
var
  SavedPos: Int64;
  I: Integer;
  Pos: Int64;
  B: array[0..1] of Byte;
  V: Word;
begin
  if (Stream = nil) or (Cluster < 0) then Exit;
  SavedPos := Stream.Position;
  try
    Value := Value and $0FFF;
    for I := 0 to NumFATs - 1 do
    begin
      Pos := FatStart + Int64(I) * SingleFATSize + (Int64(Cluster) * 3) div 2;
      B[0] := 0;
      B[1] := 0;
      Stream.Position := Pos;
      if Stream.Read(B[0], 2) <> 2 then Continue; // FAT copy lies outside the image
      V := Word(B[0]) or (Word(B[1]) shl 8);
      if (Cluster and 1) = 0 then
        V := (V and $F000) or Word(Value)
      else
        V := (V and $000F) or Word(Value shl 4);
      B[0] := Byte(V and $FF);
      B[1] := Byte(V shr 8);
      Stream.Position := Pos;
      Stream.WriteBuffer(B[0], 2);
    end;
  finally
    Stream.Position := SavedPos;
  end;
end;

var
  CRC32Table: array[0..255] of LongWord;
  CRC32TableInit: Boolean = False;

procedure InitCRC32Table;
var
  C: LongWord;
  I, J: Integer;
begin
  for I := 0 to 255 do
  begin
    C := LongWord(I);
    for J := 1 to 8 do
    begin
      if (C and 1) <> 0 then
        C := (C shr 1) xor $EDB88320
      else
        C := C shr 1;
    end;
    CRC32Table[I] := C;
  end;
  CRC32TableInit := True;
end;

function CalculateCRC32(const Data: PByte; Length: Integer): LongWord;
var
  CRC: LongWord;
  I: Integer;
begin
  if not CRC32TableInit then InitCRC32Table;
  CRC := $FFFFFFFF;
  if (Data <> nil) and (Length > 0) then
  begin
    for I := 0 to Length - 1 do
      CRC := (CRC shr 8) xor CRC32Table[(CRC and $FF) xor Data[I]];
  end;
  Result := CRC xor $FFFFFFFF;
end;

function Fat12FormatFileName(const DirEntry: PByte): string;
var
  BaseName, Ext: string;
  I: Integer;
  B: Byte;
begin
  Result := '';
  if DirEntry = nil then Exit;

  BaseName := '';
  for I := 0 to 7 do
  begin
    B := DirEntry[I];
    if (I = 0) and (B = $E5) then
      BaseName := BaseName + '_'
    else if (I = 0) and (B = $05) then
      BaseName := BaseName + Chr($E5)
    else if B > 32 then
      BaseName := BaseName + Chr(B);
  end;
  BaseName := TrimRight(BaseName);

  Ext := '';
  for I := 8 to 10 do
  begin
    B := DirEntry[I];
    if B > 32 then
      Ext := Ext + Chr(B);
  end;
  Ext := TrimRight(Ext);

  if Ext <> '' then
    Result := BaseName + '.' + Ext
  else
    Result := BaseName;
end;

function Fat12FormatAttrString(Attr: Byte): string;
begin
  if (Attr and FAT12_ATTR_DIR) <> 0 then
    Exit('[DIR]');
  Result := '[';
  if (Attr and FAT12_ATTR_READONLY) <> 0 then Result := Result + 'R' else Result := Result + '.';
  if (Attr and FAT12_ATTR_HIDDEN) <> 0 then Result := Result + 'H' else Result := Result + '.';
  if (Attr and FAT12_ATTR_SYSTEM) <> 0 then Result := Result + 'S' else Result := Result + '.';
  if (Attr and FAT12_ATTR_ARCHIVE) <> 0 then Result := Result + 'A' else Result := Result + '.';
  Result := Result + ']';
end;

function Fat12ParseDateTime(FatDate, FatTime: Word): TDateTime;
var
  Y, M, D, H, Min, Sec: Word;
  TimeVal: TDateTime;
begin
  Y := (FatDate shr 9) + 1980;
  M := (FatDate shr 5) and $0F;
  D := FatDate and $1F;
  H := (FatTime shr 11) and $1F;
  Min := (FatTime shr 5) and $3F;
  Sec := (FatTime and $1F) * 2;

  if not TryEncodeDate(Y, M, D, Result) then
    Result := 0;
  if (Result > 0) and TryEncodeTime(H, Min, Sec, 0, TimeVal) then
    Result := Result + TimeVal;
end;

function Fat12ReadFileData(Stream: TStream; const Layout: TFatLayout; StartCluster: Word; FileSize: Int64): TBytes;
var
  CurrCluster, NextCluster: Integer;
  BytesToRead, BytesRemaining: Int64;
  Offset: Int64;
  PosOut: Int64;
  LoopGuard: Integer;
  ClusterBytes: Int64;
begin
  SetLength(Result, 0);
  if (Stream = nil) or (FileSize <= 0) or (StartCluster < 2) then Exit;
  if FileSize > 32 * 1024 * 1024 then Exit; // Safety cap

  SetLength(Result, FileSize);
  BytesRemaining := FileSize;
  CurrCluster := StartCluster;
  PosOut := 0;
  LoopGuard := 0;
  ClusterBytes := Layout.BytesPerCluster;
  if ClusterBytes <= 0 then Exit;

  while (BytesRemaining > 0) and (CurrCluster >= 2) and (CurrCluster < FAT12_ENTRY_EOC_MIN) do
  begin
    Inc(LoopGuard);
    if LoopGuard > 4096 then Break;

    Offset := Fat12ClusterOffset(Layout, CurrCluster);
    if (Offset < Layout.DataAreaStart) or (Offset >= Stream.Size) then Break;

    BytesToRead := Min(BytesRemaining, ClusterBytes);
    Stream.Position := Offset;
    if Stream.Read(Result[PosOut], BytesToRead) <> BytesToRead then Break;

    Inc(PosOut, BytesToRead);
    Dec(BytesRemaining, BytesToRead);

    NextCluster := Fat12ReadEntry(Stream, Layout.FatTableStart, CurrCluster);
    if (NextCluster = CurrCluster) or (NextCluster < 2) then Break;
    CurrCluster := NextCluster;
  end;

  if PosOut < FileSize then
    SetLength(Result, PosOut);
end;

function Fat12ScanAllEntries(Stream: TStream; const Bpb: TFatBpb;
  const Layout: TFatLayout; IncludeDeleted: Boolean = False): TFat12EntryArray;
type
  TDirQueueItem = record
    Cluster: Word;
    DirPath: string;
  end;
var
  Queue: array of TDirQueueItem;
  QueueHead, QueueTail: Integer;
  Visited: array of Word;
  VisitedCount: Integer;

  function IsVisited(C: Word): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to VisitedCount - 1 do
      if Visited[I] = C then Exit(True);
    Result := False;
  end;

  procedure MarkVisited(C: Word);
  begin
    SetLength(Visited, VisitedCount + 1);
    Visited[VisitedCount] := C;
    Inc(VisitedCount);
  end;

  procedure Enqueue(ACluster: Word; const ADirPath: string);
  begin
    SetLength(Queue, QueueTail + 1);
    Queue[QueueTail].Cluster := ACluster;
    Queue[QueueTail].DirPath := ADirPath;
    Inc(QueueTail);
  end;

  function CountClusters(AStartClust: Word): Integer;
  var
    C, NextC, Guard: Integer;
  begin
    Result := 0;
    C := AStartClust;
    Guard := 0;
    while (C >= 2) and (C < FAT12_ENTRY_EOC_MIN) do
    begin
      Inc(Result);
      Inc(Guard);
      if Guard > 4096 then Break;
      NextC := Fat12ReadEntry(Stream, Layout.FatTableStart, C);
      if (NextC < 2) or (NextC = C) then Break;
      C := NextC;
    end;
  end;

var
  Item: TDirQueueItem;
  DirEntry: array[0..31] of Byte;
  EntryName: string;
  FileSize: Int64;
  StartClust: Word;
  Attr: Byte;
  DateWord, TimeWord: Word;
  EntryDate: TDateTime;
  IsDir, IsDel: Boolean;
  Offset: Int64;
  CurrCluster, NextCluster, LoopGuard: Integer;
  MaxEntries, EntryIdx: Integer;
  Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  if (Stream = nil) or (Layout.BytesPerCluster <= 0) then Exit;

  QueueHead := 0;
  QueueTail := 0;
  VisitedCount := 0;
  SetLength(Queue, 0);
  SetLength(Visited, 0);

  // Enqueue root directory (Cluster 0)
  Enqueue(0, '');

  while QueueHead < QueueTail do
  begin
    Item := Queue[QueueHead];
    Inc(QueueHead);

    if Item.Cluster = 0 then
    begin
      // 1. Scan Root Directory
      Offset := Layout.RootDirStart;
      if Offset + Int64(Bpb.RootEntries) * 32 > Stream.Size then
        MaxEntries := Max(0, Integer((Stream.Size - Offset) div 32))
      else
        MaxEntries := Bpb.RootEntries;

      for EntryIdx := 0 to MaxEntries - 1 do
      begin
        Stream.Position := Offset + Int64(EntryIdx) * 32;
        if Stream.Read(DirEntry[0], 32) <> 32 then Break;
        if DirEntry[0] = 0 then Break; // End of root directory

        IsDel := (DirEntry[0] = $E5);
        if IsDel and not IncludeDeleted then Continue;

        Attr := DirEntry[11];
        if (Attr and FAT12_ATTR_VOLUME) <> 0 then Continue; // Skip Volume label

        EntryName := Fat12FormatFileName(@DirEntry[0]);
        if (EntryName = '') or (EntryName = '.') or (EntryName = '..') then Continue;

        IsDir := (Attr and FAT12_ATTR_DIR) <> 0;
        StartClust := ReadLE16(@DirEntry[0], 32, 26);
        FileSize := ReadLE32(@DirEntry[0], 32, 28);
        DateWord := ReadLE16(@DirEntry[0], 32, 24);
        TimeWord := ReadLE16(@DirEntry[0], 32, 22);
        EntryDate := Fat12ParseDateTime(DateWord, TimeWord);

        SetLength(Result, Count + 1);
        Result[Count].FileName := EntryName;
        Result[Count].DirPath := Item.DirPath;
        if Item.DirPath <> '' then
          Result[Count].FullPath := Item.DirPath + EntryName
        else
          Result[Count].FullPath := EntryName;
        Result[Count].FileSize := FileSize;
        Result[Count].StartCluster := StartClust;
        Result[Count].DateTime := EntryDate;
        if EntryDate > 0 then
          Result[Count].DateTimeStr := FormatDateTime('yyyy-mm-dd hh:nn:ss', EntryDate)
        else
          Result[Count].DateTimeStr := '----';
        Result[Count].Attr := Attr;
        Result[Count].AttrStr := Fat12FormatAttrString(Attr);
        Result[Count].IsDir := IsDir;
        Result[Count].IsDeleted := IsDel;
        if IsDir then
          Result[Count].ClusterCount := 1
        else
          Result[Count].ClusterCount := CountClusters(StartClust);
        Inc(Count);

        if IsDir and (StartClust >= 2) and (StartClust < FAT12_ENTRY_EOC_MIN) and not IsVisited(StartClust) then
        begin
          MarkVisited(StartClust);
          if Item.DirPath <> '' then
            Enqueue(StartClust, Item.DirPath + EntryName + '\')
          else
            Enqueue(StartClust, EntryName + '\');
        end;
      end;
    end
    else
    begin
      // 2. Scan Subdirectory cluster chain
      CurrCluster := Item.Cluster;
      LoopGuard := 0;
      MaxEntries := Integer(Layout.BytesPerCluster div 32);

      while (CurrCluster >= 2) and (CurrCluster < FAT12_ENTRY_EOC_MIN) do
      begin
        Inc(LoopGuard);
        if LoopGuard > 4096 then Break;

        Offset := Fat12ClusterOffset(Layout, CurrCluster);
        if (Offset < Layout.DataAreaStart) or (Offset + Layout.BytesPerCluster > Stream.Size) then Break;

        for EntryIdx := 0 to MaxEntries - 1 do
        begin
          Stream.Position := Offset + Int64(EntryIdx) * 32;
          if Stream.Read(DirEntry[0], 32) <> 32 then Break;
          if DirEntry[0] = 0 then Break; // End of this directory

          IsDel := (DirEntry[0] = $E5);
          if IsDel and not IncludeDeleted then Continue;

          Attr := DirEntry[11];
          if (Attr and FAT12_ATTR_VOLUME) <> 0 then Continue;

          EntryName := Fat12FormatFileName(@DirEntry[0]);
          if (EntryName = '') or (EntryName = '.') or (EntryName = '..') then Continue;

          IsDir := (Attr and FAT12_ATTR_DIR) <> 0;
          StartClust := ReadLE16(@DirEntry[0], 32, 26);
          FileSize := ReadLE32(@DirEntry[0], 32, 28);
          DateWord := ReadLE16(@DirEntry[0], 32, 24);
          TimeWord := ReadLE16(@DirEntry[0], 32, 22);
          EntryDate := Fat12ParseDateTime(DateWord, TimeWord);

          SetLength(Result, Count + 1);
          Result[Count].FileName := EntryName;
          Result[Count].DirPath := Item.DirPath;
          if Item.DirPath <> '' then
            Result[Count].FullPath := Item.DirPath + EntryName
          else
            Result[Count].FullPath := EntryName;
          Result[Count].FileSize := FileSize;
          Result[Count].StartCluster := StartClust;
          Result[Count].DateTime := EntryDate;
          if EntryDate > 0 then
            Result[Count].DateTimeStr := FormatDateTime('yyyy-mm-dd hh:nn:ss', EntryDate)
          else
            Result[Count].DateTimeStr := '----';
          Result[Count].Attr := Attr;
          Result[Count].AttrStr := Fat12FormatAttrString(Attr);
          Result[Count].IsDir := IsDir;
          Result[Count].IsDeleted := IsDel;
          if IsDir then
            Result[Count].ClusterCount := 1
          else
            Result[Count].ClusterCount := CountClusters(StartClust);
          Inc(Count);

          if IsDir and (StartClust >= 2) and (StartClust < FAT12_ENTRY_EOC_MIN) and not IsVisited(StartClust) then
          begin
            MarkVisited(StartClust);
            if Item.DirPath <> '' then
              Enqueue(StartClust, Item.DirPath + EntryName + '\')
            else
              Enqueue(StartClust, EntryName + '\');
          end;
        end;

        NextCluster := Fat12ReadEntry(Stream, Layout.FatTableStart, CurrCluster);
        if (NextCluster < 2) or (NextCluster = CurrCluster) then Break;
        CurrCluster := NextCluster;
      end;
    end;
  end;
end;

end.
