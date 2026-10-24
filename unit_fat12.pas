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
  Classes, SysUtils;

const
  FAT12_SECTOR_BYTES  = 512;   // Atari ST / PC floppy sector size
  FAT12_DIRENT_BYTES  = 32;    // Size of one directory entry
  FAT12_ENTRY_FREE    = $000;
  FAT12_ENTRY_BAD     = $FF7;
  FAT12_ENTRY_EOC_MIN = $FF8;  // First end-of-chain marker
  FAT12_ENTRY_EOC     = $FFF;
  FAT12_MAX_CLUSTERS  = 4084;  // Max data clusters of a FAT12 volume
  ATARI_BOOT_EXEC_SUM = $1234; // Word-sum that makes TOS execute a boot sector

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

{ Little-endian readers.  Reading outside Len returns 0. }
function ReadLE16(const P: PByte; Len, Offset: Integer): Word;
function ReadLE32(const P: PByte; Len, Offset: Integer): LongWord;

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
  B: array[0..1] of Byte;
  V: Word;
begin
  Result := FAT12_ENTRY_EOC;
  if (Stream = nil) or (Cluster < 0) then Exit;
  B[0] := 0;
  B[1] := 0;
  Stream.Position := FatStart + (Int64(Cluster) * 3) div 2;
  if Stream.Read(B[0], 2) <> 2 then Exit;
  V := Word(B[0]) or (Word(B[1]) shl 8);
  if (Cluster and 1) = 0 then
    Result := V and $0FFF
  else
    Result := V shr 4;
end;

procedure Fat12WriteEntryAllCopies(Stream: TStream; FatStart, SingleFATSize: Int64;
  NumFATs, Cluster, Value: Integer);
var
  I: Integer;
  Pos: Int64;
  B: array[0..1] of Byte;
  V: Word;
begin
  if (Stream = nil) or (Cluster < 0) then Exit;
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
end;

end.
