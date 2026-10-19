unit unit_archivevault;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, StrUtils, zipper, LazFileUtils;

type
  TArchiveFormat = (afUnknown, afZip, afLha, afArc, afZoo);

  TArchiveItem = record
    FullPath: string;       // Normalized path, e.g. "hddrv771.pck/auto/hddriver.prg"
    DisplayName: string;    // Name of file or directory, e.g. "hddriver.prg"
    IsDir: Boolean;
    OriginalSize: Int64;
    CompressedSize: Int64;
    DateTimeStr: string;
    MethodStr: string;      // e.g. "-lh5-", "Deflate", "Stored"
    DataOffset: Int64;
    HeaderOffset: Int64;
  end;

  { TArchiveVault handles browsing and extracting files from archive files (.zip, .lzh, .lha, .arc, .zoo) }
  TArchiveVault = class
  private
    FSourceFile: string;
    FFormat: TArchiveFormat;
    FItems: array of TArchiveItem;
    function ParseZip: Boolean;
    function ParseLha: Boolean;
    function ParseArc: Boolean;
    function ParseZoo: Boolean;
    function GetItemCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function LoadArchive(const AFilePath: string): Boolean;
    function GetItem(Index: Integer): TArchiveItem;
    procedure GetChildren(const AInnerPath: string; out AChildren: array of TArchiveItem; out ACount: Integer);
    function ExtractEntryToStream(const AFullPath: string; AOutStream: TStream): Boolean;
    function ExtractEntryToTempFile(const AFullPath: string; out ATempPath: string): Boolean;
    property Format: TArchiveFormat read FFormat;
    property SourceFile: string read FSourceFile;
    property ItemCount: Integer read GetItemCount;
  end;

function IsArchiveFileName(const AFileName: string): Boolean;
function DetectArchiveFormat(const AFileName: string; AStream: TStream): TArchiveFormat;
function NormalizeArchivePath(const APath: string): string;

implementation

{ Bit Stream Reader for LH5 Decompression }
type
  TBitReader = class
  private
    FStream: TStream;
    FBitBuf: LongWord;
    FBits: Integer;
  public
    constructor Create(AStream: TStream);
    function PeekBits(N: Integer): LongWord;
    function ReadBits(N: Integer): LongWord;
    function ReadBit: LongWord;
  end;

constructor TBitReader.Create(AStream: TStream);
begin
  FStream := AStream;
  FBitBuf := 0;
  FBits := 0;
end;

function TBitReader.PeekBits(N: Integer): LongWord;
var
  B: Byte;
begin
  if N = 0 then Exit(0);
  while FBits < N do
  begin
    B := 0;
    if FStream.Position < FStream.Size then
      FStream.Read(B, 1);
    FBitBuf := FBitBuf or (LongWord(B) shl (24 - FBits));
    Inc(FBits, 8);
  end;
  Result := (FBitBuf shr (32 - N)) and ((1 shl N) - 1);
end;

function TBitReader.ReadBits(N: Integer): LongWord;
begin
  Result := PeekBits(N);
  FBitBuf := (FBitBuf shl N) and $FFFFFFFF;
  Dec(FBits, N);
end;

function TBitReader.ReadBit: LongWord;
begin
  Result := ReadBits(1);
end;

type
  TTreeArray = array of Word;

procedure BuildTree(const CodeLens: array of Byte; var Tree: TTreeArray);
var
  Allocated, NextEntry, CodeLen, I, EndOffset, NewNodes: Integer;
  Remaining: Boolean;
begin
  SetLength(Tree, 1024);
  for I := 0 to High(Tree) do Tree[I] := $8000;
  Allocated := 1;
  NextEntry := 0;
  CodeLen := 0;
  while True do
  begin
    NewNodes := (Allocated - NextEntry) * 2;
    if Allocated + NewNodes > Length(Tree) then
      SetLength(Tree, Allocated + NewNodes + 128);
    EndOffset := Allocated;
    while NextEntry < EndOffset do
    begin
      Tree[NextEntry] := Allocated;
      Inc(Allocated, 2);
      Inc(NextEntry);
    end;
    Inc(CodeLen);
    Remaining := False;
    for I := 0 to High(CodeLens) do
    begin
      if CodeLens[I] = CodeLen then
      begin
        Tree[NextEntry] := Word(I) or $8000;
        Inc(NextEntry);
      end
      else if CodeLens[I] > CodeLen then
        Remaining := True;
    end;
    if not Remaining then Break;
  end;
end;

function ReadFromTree(Reader: TBitReader; const Tree: TTreeArray): Integer;
var
  Code: Word;
  Bit: LongWord;
begin
  Code := Tree[0];
  while (Code and $8000) = 0 do
  begin
    Bit := Reader.ReadBit;
    Code := Tree[Code + Bit];
  end;
  Result := Code and $7FFF;
end;

function ReadLenVal(Reader: TBitReader): Integer;
var
  L: Integer;
begin
  L := Reader.ReadBits(3);
  if L = 7 then
  begin
    while Reader.ReadBit <> 0 do
      Inc(L);
  end;
  Result := L;
end;

function DecompressLH5(AInStream: TStream; ACompressedSize, AOriginalSize: Int64; AOutStream: TStream): Boolean;
var
  Reader: TBitReader;
  RingBuf: array[0..8191] of Byte;
  RingPos: Integer;
  BytesOut: Int64;
  BlockRem, N, I, Skip, Code, MatchLen, Bits, Offset, Start, J: Integer;
  TempLens: array[0..18] of Byte;
  TempTree: TTreeArray;
  CodeLens: array[0..509] of Byte;
  CodeTree: TTreeArray;
  OffLens: array[0..15] of Byte;
  OffTree: TTreeArray;
  C: Integer;
  B: Byte;
begin
  Result := False;
  FillChar(RingBuf, SizeOf(RingBuf), 32); // Space character ($20)
  RingPos := 0;
  BytesOut := 0;
  Reader := TBitReader.Create(AInStream);
  try
    while BytesOut < AOriginalSize do
    begin
      BlockRem := Reader.ReadBits(16);
      if (BlockRem = 0) and (BytesOut >= AOriginalSize) then Break;

      // 1. Temp tree
      N := Reader.ReadBits(5);
      if N > Length(TempLens) then Exit(False);
      FillChar(TempLens, SizeOf(TempLens), 0);
      if N = 0 then
      begin
        C := Reader.ReadBits(5);
        SetLength(TempTree, 64);
        for I := 0 to High(TempTree) do TempTree[I] := Word(C) or $8000;
      end
      else
      begin
        I := 0;
        while (I < N) and (I <= High(TempLens)) do
        begin
          TempLens[I] := ReadLenVal(Reader);
          if I = 2 then
          begin
            Skip := Reader.ReadBits(2);
            for J := 1 to Skip do
            begin
              Inc(I);
              if (I < N) and (I <= High(TempLens)) then TempLens[I] := 0;
            end;
          end;
          Inc(I);
        end;
        BuildTree(Slice(TempLens, N), TempTree);
      end;

      // 2. Code tree
      N := Reader.ReadBits(9);
      if N > Length(CodeLens) then Exit(False);
      FillChar(CodeLens, SizeOf(CodeLens), 0);
      if N = 0 then
      begin
        C := Reader.ReadBits(9);
        SetLength(CodeTree, 1024);
        for I := 0 to High(CodeTree) do CodeTree[I] := Word(C) or $8000;
      end
      else
      begin
        I := 0;
        while (I < N) and (I <= High(CodeLens)) do
        begin
          C := ReadFromTree(Reader, TempTree);
          if C <= 2 then
          begin
            if C = 0 then Skip := 1
            else if C = 1 then Skip := Reader.ReadBits(4) + 3
            else Skip := Reader.ReadBits(9) + 20;
            for J := 1 to Skip do
            begin
              if (I < N) and (I <= High(CodeLens)) then CodeLens[I] := 0;
              Inc(I);
            end;
          end
          else
          begin
            CodeLens[I] := C - 2;
            Inc(I);
          end;
        end;
        BuildTree(Slice(CodeLens, N), CodeTree);
      end;

      // 3. Offset tree
      N := Reader.ReadBits(4);
      if N > Length(OffLens) then Exit(False);
      FillChar(OffLens, SizeOf(OffLens), 0);
      if N = 0 then
      begin
        C := Reader.ReadBits(4);
        SetLength(OffTree, 64);
        for I := 0 to High(OffTree) do OffTree[I] := Word(C) or $8000;
      end
      else
      begin
        for I := 0 to Min(N - 1, High(OffLens)) do
          OffLens[I] := ReadLenVal(Reader);
        BuildTree(Slice(OffLens, N), OffTree);
      end;

      // 4. Decode block commands
      for I := 1 to BlockRem do
      begin
        if BytesOut >= AOriginalSize then Break;
        Code := ReadFromTree(Reader, CodeTree);
        if Code < 256 then
        begin
          B := Byte(Code);
          AOutStream.Write(B, 1);
          RingBuf[RingPos] := B;
          RingPos := (RingPos + 1) mod 8192;
          Inc(BytesOut);
        end
        else
        begin
          MatchLen := Code - 256 + 3;
          Bits := ReadFromTree(Reader, OffTree);
          if Bits = 0 then Offset := 0
          else if Bits = 1 then Offset := 1
          else Offset := Reader.ReadBits(Bits - 1) + (1 shl (Bits - 1));

          Start := (RingPos + 8192 - Offset - 1) mod 8192;
          for J := 0 to MatchLen - 1 do
          begin
            B := RingBuf[(Start + J) mod 8192];
            AOutStream.Write(B, 1);
            RingBuf[RingPos] := B;
            RingPos := (RingPos + 1) mod 8192;
            Inc(BytesOut);
            if BytesOut >= AOriginalSize then Break;
          end;
        end;
      end;
    end;
    Result := (BytesOut = AOriginalSize);
  finally
    Reader.Free;
  end;
end;

function NormalizeArchivePath(const APath: string): string;
begin
  Result := StringReplace(APath, '\', '/', [rfReplaceAll]);
  while (Length(Result) > 0) and (Result[1] = '/') do
    Delete(Result, 1, 1);
  while (Length(Result) > 0) and (Result[Length(Result)] = '/') do
    Delete(Result, Length(Result), 1);
end;

function IsArchiveFileName(const AFileName: string): Boolean;
var
  Ext: string;
begin
  Ext := UpperCase(ExtractFileExt(AFileName));
  if (Ext = '.ZIP') or (Ext = '.LZH') or (Ext = '.LHA') or (Ext = '.LZA') or
     (Ext = '.ARC') or (Ext = '.PAK') or (Ext = '.ZOO') or (Ext = '.LZS') or (Ext = '.LZ4') then
    Result := True
  else
    Result := False;
end;

function DetectArchiveFormat(const AFileName: string; AStream: TStream): TArchiveFormat;
var
  Ext: string;
  Magic: array[0..7] of Byte;
  SavedPos: Int64;
  ReadBytes: Integer;
  Tag: LongWord;
begin
  Result := afUnknown;
  Ext := UpperCase(ExtractFileExt(AFileName));

  SavedPos := AStream.Position;
  try
    FillChar(Magic, SizeOf(Magic), 0);
    ReadBytes := AStream.Read(Magic[0], SizeOf(Magic));
    if ReadBytes >= 2 then
    begin
      // ZIP check: PK\x03\x04 or PK\x01\x02
      if (Magic[0] = $50) and (Magic[1] = $4B) then
        Exit(afZip);

      // ZOO check: tag $FDC4A7DC at offset 0
      if ReadBytes >= 4 then
      begin
        Tag := Magic[0] or (LongWord(Magic[1]) shl 8) or (LongWord(Magic[2]) shl 16) or (LongWord(Magic[3]) shl 24);
        if Tag = $FDC4A7DC then
          Exit(afZoo);
      end;

      // LHA check: Method at offset 2..6 (-lh or -lz)
      if ReadBytes >= 7 then
      begin
        if (Magic[2] = Ord('-')) and (Magic[3] = Ord('l')) and
           ((Magic[4] = Ord('h')) or (Magic[4] = Ord('z'))) then
          Exit(afLha);
      end;

      // ARC check: $1A followed by method 1..9
      if (Magic[0] = $1A) and (Magic[1] in [1..9]) then
        Exit(afArc);
    end;
  finally
    AStream.Position := SavedPos;
  end;

  // Fallback to extension if magic was ambiguous
  if (Ext = '.ZIP') then Result := afZip
  else if (Ext = '.LZH') or (Ext = '.LHA') or (Ext = '.LZA') or (Ext = '.LZS') or (Ext = '.LZ4') then Result := afLha
  else if (Ext = '.ARC') or (Ext = '.PAK') then Result := afArc
  else if (Ext = '.ZOO') then Result := afZoo;
end;

{ TArchiveVault }

constructor TArchiveVault.Create;
begin
  inherited Create;
  Clear;
end;

destructor TArchiveVault.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TArchiveVault.Clear;
begin
  FSourceFile := '';
  FFormat := afUnknown;
  SetLength(FItems, 0);
end;

function TArchiveVault.GetItemCount: Integer;
begin
  Result := Length(FItems);
end;

function TArchiveVault.GetItem(Index: Integer): TArchiveItem;
begin
  if (Index >= 0) and (Index < Length(FItems)) then
    Result := FItems[Index]
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function TArchiveVault.LoadArchive(const AFilePath: string): Boolean;
var
  Fs: TFileStream;
begin
  Clear;
  Result := False;
  if not FileExists(AFilePath) then Exit;

  FSourceFile := AFilePath;
  Fs := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyNone);
  try
    FFormat := DetectArchiveFormat(AFilePath, Fs);
  finally
    Fs.Free;
  end;

  case FFormat of
    afZip: Result := ParseZip;
    afLha: Result := ParseLha;
    afArc: Result := ParseArc;
    afZoo: Result := ParseZoo;
    else Result := False;
  end;
end;

function TArchiveVault.ParseZip: Boolean;
var
  UnZip: TUnZipper;
  I: Integer;
  EntryPath, NormPath, DispName: string;
  SlashPos: Integer;
  Item: TArchiveItem;
begin
  Result := False;
  UnZip := TUnZipper.Create;
  try
    UnZip.FileName := FSourceFile;
    UnZip.Examine;
    SetLength(FItems, 0);

    for I := 0 to UnZip.Entries.Count - 1 do
    begin
      EntryPath := UnZip.Entries[I].ArchiveFileName;
      NormPath := NormalizeArchivePath(EntryPath);
      if NormPath = '' then Continue;

      FillChar(Item, SizeOf(Item), 0);
      Item.FullPath := NormPath;
      SlashPos := LastDelimiter('/', NormPath);
      if SlashPos > 0 then
        DispName := Copy(NormPath, SlashPos + 1, Length(NormPath))
      else
        DispName := NormPath;

      Item.DisplayName := DispName;
      Item.IsDir := UnZip.Entries[I].IsDirectory or EntryPath.EndsWith('/');
      Item.OriginalSize := UnZip.Entries[I].Size;
      Item.CompressedSize := UnZip.Entries[I].Size;
      Item.DateTimeStr := FormatDateTime('yyyy-mm-dd hh:nn', UnZip.Entries[I].DateTime);
      Item.MethodStr := 'Deflate';
      Item.DataOffset := 0;
      Item.HeaderOffset := I; // store entry index

      SetLength(FItems, Length(FItems) + 1);
      FItems[High(FItems)] := Item;
    end;
    Result := True;
  except
    Result := False;
  end;
  UnZip.Free;
end;

function TArchiveVault.ParseLha: Boolean;
var
  Fs: TFileStream;
  HSize: Byte;
  HSizeW: Word;
  MethodBytes: array[0..4] of Char;
  MethodStr: string;
  CompSize, OrigSize: LongWord;
  Level, NameLen: Byte;
  NameBuf: array[0..255] of Char;
  NameStr, NormPath, DispName: string;
  SlashPos: Integer;
  Item: TArchiveItem;
  NextHeaderPos: Int64;
  DosTime, DosDate: Word;
  DtVal: TDateTime;
begin
  Result := False;
  SetLength(FItems, 0);
  Fs := TFileStream.Create(FSourceFile, fmOpenRead or fmShareDenyNone);
  try
    while Fs.Position < Fs.Size do
    begin
      Item.HeaderOffset := Fs.Position;
      HSize := 0;
      if Fs.Read(HSize, 1) <> 1 then Break;
      if HSize = 0 then Break; // End of archive

      // Check for Level 2 header (HSize is first byte of 2-byte word)
      Fs.Position := Item.HeaderOffset + 2;
      FillChar(MethodBytes, SizeOf(MethodBytes), 0);
      if Fs.Read(MethodBytes[0], 5) <> 5 then Break;
      MethodStr := MethodBytes;

      Fs.Position := Item.HeaderOffset + 7;
      Fs.Read(CompSize, 4);
      Fs.Read(OrigSize, 4);

      // Read DOS date/time at offset 15
      Fs.Position := Item.HeaderOffset + 15;
      Fs.Read(DosTime, 2);
      Fs.Read(DosDate, 2);

      Fs.Position := Item.HeaderOffset + 20;
      Level := 0;
      Fs.Read(Level, 1);

      if Level = 2 then
      begin
        Fs.Position := Item.HeaderOffset;
        Fs.Read(HSizeW, 2);
        NextHeaderPos := Item.HeaderOffset + HSizeW + CompSize;
        Item.DataOffset := Item.HeaderOffset + HSizeW;
        NameStr := 'file_' + IntToStr(Length(FItems) + 1);
      end
      else
      begin
        // Level 0 or Level 1
        NameLen := 0;
        Fs.Position := Item.HeaderOffset + 21;
        Fs.Read(NameLen, 1);
        FillChar(NameBuf, SizeOf(NameBuf), 0);
        if NameLen > 0 then
          Fs.Read(NameBuf[0], Min(NameLen, SizeOf(NameBuf) - 1));
        NameStr := NameBuf;

        NextHeaderPos := Item.HeaderOffset + HSize + 2 + CompSize;
        Item.DataOffset := Item.HeaderOffset + HSize + 2;
      end;

      NormPath := NormalizeArchivePath(NameStr);
      if NormPath = '' then
        NormPath := 'file_' + IntToStr(Length(FItems) + 1);

      SlashPos := LastDelimiter('/', NormPath);
      if SlashPos > 0 then
        DispName := Copy(NormPath, SlashPos + 1, Length(NormPath))
      else
        DispName := NormPath;

      Item.FullPath := NormPath;
      Item.DisplayName := DispName;
      Item.IsDir := (MethodStr = '-lhd-') or NameStr.EndsWith('/') or NameStr.EndsWith('\');
      Item.OriginalSize := OrigSize;
      Item.CompressedSize := CompSize;
      Item.MethodStr := MethodStr;

      try
        // Decode FAT/DOS date time
        DtVal := EncodeDate(((DosDate shr 9) and $7F) + 1980,
                            Max(1, Min(12, (DosDate shr 5) and $0F)),
                            Max(1, Min(31, DosDate and $1F))) +
                 EncodeTime(Min(23, (DosTime shr 11) and $1F),
                            Min(59, (DosTime shr 5) and $3F),
                            Min(59, (DosTime and $1F) * 2), 0);
        Item.DateTimeStr := FormatDateTime('yyyy-mm-dd hh:nn', DtVal);
      except
        Item.DateTimeStr := '-';
      end;

      SetLength(FItems, Length(FItems) + 1);
      FItems[High(FItems)] := Item;

      if NextHeaderPos <= Fs.Position then Break;
      Fs.Position := NextHeaderPos;
    end;
    Result := (Length(FItems) > 0);
  finally
    Fs.Free;
  end;
end;

function TArchiveVault.ParseArc: Boolean;
var
  Fs: TFileStream;
  Marker, Method: Byte;
  NameBuf: array[0..12] of Char;
  NameStr, NormPath, DispName: string;
  CompSize, OrigSize: LongWord;
  DosDate, DosTime: Word;
  Item: TArchiveItem;
  SlashPos: Integer;
begin
  Result := False;
  SetLength(FItems, 0);
  Fs := TFileStream.Create(FSourceFile, fmOpenRead or fmShareDenyNone);
  try
    while Fs.Position + 29 <= Fs.Size do
    begin
      Item.HeaderOffset := Fs.Position;
      Marker := 0;
      if Fs.Read(Marker, 1) <> 1 then Break;
      if Marker <> $1A then Break; // ARC magic marker

      Method := 0;
      Fs.Read(Method, 1);
      if (Method = 0) or (Method > 20) then Break; // End of archive or invalid

      FillChar(NameBuf, SizeOf(NameBuf), 0);
      Fs.Read(NameBuf[0], 13);
      NameStr := Trim(NameBuf);
      if NameStr = '' then Break;

      Fs.Read(CompSize, 4);
      Fs.Read(DosDate, 2);
      Fs.Read(DosTime, 2);
      Fs.Position := Fs.Position + 2; // Skip CRC (2 bytes)
      Fs.Read(OrigSize, 4);

      Item.DataOffset := Item.HeaderOffset + 29;
      NormPath := NormalizeArchivePath(NameStr);
      SlashPos := LastDelimiter('/', NormPath);
      if SlashPos > 0 then
        DispName := Copy(NormPath, SlashPos + 1, Length(NormPath))
      else
        DispName := NormPath;

      Item.FullPath := NormPath;
      Item.DisplayName := DispName;
      Item.IsDir := False;
      Item.OriginalSize := OrigSize;
      Item.CompressedSize := CompSize;
      Item.MethodStr := 'ARC M' + IntToStr(Method);
      Item.DateTimeStr := '-';

      SetLength(FItems, Length(FItems) + 1);
      FItems[High(FItems)] := Item;

      Fs.Position := Item.DataOffset + CompSize;
    end;
    Result := (Length(FItems) > 0);
  finally
    Fs.Free;
  end;
end;

function TArchiveVault.ParseZoo: Boolean;
var
  Fs: TFileStream;
  Tag, DirOffset, NextDir, DataOff, OrigSize, CompSize: LongWord;
  NameBuf: array[0..13] of Char;
  NameStr, NormPath, DispName: string;
  PackType: Word;
  Item: TArchiveItem;
  SlashPos, Guard: Integer;
begin
  Result := False;
  SetLength(FItems, 0);
  Fs := TFileStream.Create(FSourceFile, fmOpenRead or fmShareDenyNone);
  try
    if Fs.Size < 32 then Exit;
    Fs.Position := 20;
    Fs.Read(Tag, 4);
    if Tag <> $FDC4A7DC then
    begin
      Fs.Position := 0;
      Fs.Read(Tag, 4);
      if Tag <> $FDC4A7DC then Exit;
    end;

    Fs.Position := 24;
    Fs.Read(DirOffset, 4);
    NextDir := DirOffset;
    Guard := 0;

    while (NextDir > 0) and (NextDir + 43 < Fs.Size) and (Guard < 4096) do
    begin
      Inc(Guard);
      Fs.Position := NextDir;
      Fs.Read(Tag, 4);
      if Tag <> $FDC4A7DC then Break;

      Item.HeaderOffset := NextDir;
      Fs.Position := NextDir + 6;
      Fs.Read(NextDir, 4);
      Fs.Read(DataOff, 4);
      Fs.Position := NextDir + 18; // Read sizes
      Fs.Position := Item.HeaderOffset + 18;
      Fs.Position := Item.HeaderOffset + 20;
      Fs.Read(OrigSize, 4);
      Fs.Read(CompSize, 4);

      FillChar(NameBuf, SizeOf(NameBuf), 0);
      Fs.Position := Item.HeaderOffset + 28;
      Fs.Read(NameBuf[0], 13);
      NameStr := Trim(NameBuf);

      Fs.Position := Item.HeaderOffset + 5;
      Fs.Read(PackType, 2);

      Item.DataOffset := DataOff;
      NormPath := NormalizeArchivePath(NameStr);
      SlashPos := LastDelimiter('/', NormPath);
      if SlashPos > 0 then
        DispName := Copy(NormPath, SlashPos + 1, Length(NormPath))
      else
        DispName := NormPath;

      Item.FullPath := NormPath;
      Item.DisplayName := DispName;
      Item.IsDir := False;
      Item.OriginalSize := OrigSize;
      Item.CompressedSize := CompSize;
      Item.MethodStr := 'ZOO T' + IntToStr(PackType);
      Item.DateTimeStr := '-';

      SetLength(FItems, Length(FItems) + 1);
      FItems[High(FItems)] := Item;
    end;
    Result := (Length(FItems) > 0);
  finally
    Fs.Free;
  end;
end;

procedure TArchiveVault.GetChildren(const AInnerPath: string; out AChildren: array of TArchiveItem; out ACount: Integer);
var
  Prefix, Remainder, ChildName: string;
  SlashPos, I, J: Integer;
  Found: Boolean;
  ItemRec: TArchiveItem;
begin
  ACount := 0;
  Prefix := NormalizeArchivePath(AInnerPath);
  if (Prefix <> '') and not Prefix.EndsWith('/') then
    Prefix := Prefix + '/';

  for I := 0 to High(FItems) do
  begin
    if (Prefix <> '') and not StartsText(Prefix, FItems[I].FullPath) then
      Continue;

    if Prefix <> '' then
      Remainder := Copy(FItems[I].FullPath, Length(Prefix) + 1, Length(FItems[I].FullPath))
    else
      Remainder := FItems[I].FullPath;

    if Remainder = '' then Continue;

    SlashPos := Pos('/', Remainder);
    if SlashPos > 0 then
    begin
      // It's a directory
      ChildName := Copy(Remainder, 1, SlashPos - 1);
      Found := False;
      for J := 0 to ACount - 1 do
      begin
        if AChildren[J].IsDir and SameText(AChildren[J].DisplayName, ChildName) then
        begin
          Found := True;
          Break;
        end;
      end;
      if not Found and (ACount < Length(AChildren)) then
      begin
        FillChar(ItemRec, SizeOf(ItemRec), 0);
        ItemRec.DisplayName := ChildName;
        ItemRec.FullPath := Prefix + ChildName + '/';
        ItemRec.IsDir := True;
        ItemRec.OriginalSize := 0;
        ItemRec.CompressedSize := 0;
        ItemRec.DateTimeStr := '-';
        ItemRec.MethodStr := '<DIR>';
        AChildren[ACount] := ItemRec;
        Inc(ACount);
      end;
    end
    else
    begin
      // It's a file
      if ACount < Length(AChildren) then
      begin
        ItemRec := FItems[I];
        ItemRec.DisplayName := Remainder;
        AChildren[ACount] := ItemRec;
        Inc(ACount);
      end;
    end;
  end;
end;

function TArchiveVault.ExtractEntryToStream(const AFullPath: string; AOutStream: TStream): Boolean;
var
  NormTarget, TempDir, ExtractedFile: string;
  I: Integer;
  TargetIndex: Integer;
  Fs: TFileStream;
  UnZip: TUnZipper;
  Buffer: array[0..4095] of Byte;
  BytesToRead, Chunk: Int64;
  Ctrl, Val: Byte;
  K: Integer;
begin
  Result := False;
  NormTarget := NormalizeArchivePath(AFullPath);
  TargetIndex := -1;

  for I := 0 to High(FItems) do
  begin
    if SameText(NormalizeArchivePath(FItems[I].FullPath), NormTarget) then
    begin
      TargetIndex := I;
      Break;
    end;
  end;

  if TargetIndex = -1 then Exit;

  case FFormat of
    afZip:
    begin
      // Use TUnZipper to extract specific file to temporary staging folder
      TempDir := IncludeTrailingPathDelimiter(GetTempDir) + 'stimage_zip_extr_' + FormatDateTime('hhnnsszzz', Now) + PathDelim;
      ForceDirectories(TempDir);
      try
        UnZip := TUnZipper.Create;
        try
          UnZip.FileName := FSourceFile;
          UnZip.OutputPath := TempDir;
          UnZip.Flat := True;
          UnZip.UnZipFile(FItems[TargetIndex].FullPath);
        finally
          UnZip.Free;
        end;

        ExtractedFile := TempDir + ExtractFileName(StringReplace(FItems[TargetIndex].FullPath, '/', PathDelim, [rfReplaceAll]));
        if FileExists(ExtractedFile) then
        begin
          Fs := TFileStream.Create(ExtractedFile, fmOpenRead or fmShareDenyNone);
          try
            AOutStream.CopyFrom(Fs, Fs.Size);
            Result := True;
          finally
            Fs.Free;
            SysUtils.DeleteFile(ExtractedFile);
          end;
        end;
      finally
        RemoveDir(TempDir);
      end;
    end;

    afLha:
    begin
      Fs := TFileStream.Create(FSourceFile, fmOpenRead or fmShareDenyNone);
      try
        Fs.Position := FItems[TargetIndex].DataOffset;
        if (FItems[TargetIndex].MethodStr = '-lh0-') or (FItems[TargetIndex].MethodStr = '-lz4-') then
        begin
          // Uncompressed stored
          BytesToRead := FItems[TargetIndex].OriginalSize;
          while BytesToRead > 0 do
          begin
            Chunk := Min(BytesToRead, SizeOf(Buffer));
            Fs.ReadBuffer(Buffer[0], Chunk);
            AOutStream.WriteBuffer(Buffer[0], Chunk);
            Dec(BytesToRead, Chunk);
          end;
          Result := True;
        end
        else if (FItems[TargetIndex].MethodStr = '-lh5-') or (FItems[TargetIndex].MethodStr = '-lh4-') then
        begin
          // Decompress using native LH5 decoder
          Result := DecompressLH5(Fs, FItems[TargetIndex].CompressedSize, FItems[TargetIndex].OriginalSize, AOutStream);
        end
        else
        begin
          // Method not supported; do not report success on raw compressed dump
          Result := False;
        end;
      finally
        Fs.Free;
      end;
    end;

    afArc:
    begin
      Fs := TFileStream.Create(FSourceFile, fmOpenRead or fmShareDenyNone);
      try
        Fs.Position := FItems[TargetIndex].DataOffset;
        if FItems[TargetIndex].MethodStr = 'ARC M1' then
        begin
          // Stored
          BytesToRead := FItems[TargetIndex].OriginalSize;
          while BytesToRead > 0 do
          begin
            Chunk := Min(BytesToRead, SizeOf(Buffer));
            Fs.ReadBuffer(Buffer[0], Chunk);
            AOutStream.WriteBuffer(Buffer[0], Chunk);
            Dec(BytesToRead, Chunk);
          end;
          Result := True;
        end
        else if (FItems[TargetIndex].MethodStr = 'ARC M2') or (FItems[TargetIndex].MethodStr = 'ARC M3') then
        begin
          // RLE Decompression
          BytesToRead := 0;
          while (BytesToRead < FItems[TargetIndex].OriginalSize) and (Fs.Position < FItems[TargetIndex].DataOffset + FItems[TargetIndex].CompressedSize) do
          begin
            if Fs.Read(Ctrl, 1) <> 1 then Break;
            if Ctrl = 0 then
            begin
              if Fs.Read(Val, 1) = 1 then
              begin
                AOutStream.Write(Val, 1);
                Inc(BytesToRead);
              end;
            end
            else
            begin
              if Fs.Read(Val, 1) = 1 then
              begin
                for K := 1 to Ctrl do
                begin
                  if BytesToRead >= FItems[TargetIndex].OriginalSize then Break;
                  AOutStream.Write(Val, 1);
                  Inc(BytesToRead);
                end;
              end;
            end;
          end;
          Result := (BytesToRead = FItems[TargetIndex].OriginalSize);
        end
        else
        begin
          // Method not supported; do not report success on raw compressed dump
          Result := False;
        end;
      finally
        Fs.Free;
      end;
    end;

    afZoo:
    begin
      Fs := TFileStream.Create(FSourceFile, fmOpenRead or fmShareDenyNone);
      try
        Fs.Position := FItems[TargetIndex].DataOffset;
        BytesToRead := Min(FItems[TargetIndex].OriginalSize, FItems[TargetIndex].CompressedSize);
        while BytesToRead > 0 do
        begin
          Chunk := Min(BytesToRead, SizeOf(Buffer));
          Fs.ReadBuffer(Buffer[0], Chunk);
          AOutStream.WriteBuffer(Buffer[0], Chunk);
          Dec(BytesToRead, Chunk);
        end;
        Result := True;
      finally
        Fs.Free;
      end;
    end;
  end;
end;

function TArchiveVault.ExtractEntryToTempFile(const AFullPath: string; out ATempPath: string): Boolean;
var
  TempDir, OutPath, Ext: string;
  Fs: TFileStream;
begin
  Result := False;
  ATempPath := '';
  TempDir := IncludeTrailingPathDelimiter(GetTempDir) + 'stimage_archive_extract';
  ForceDirectories(TempDir);

  Ext := ExtractFileExt(AFullPath);
  OutPath := TempDir + PathDelim + ExtractFileName(StringReplace(AFullPath, '/', PathDelim, [rfReplaceAll]));
  if FileExists(OutPath) then SysUtils.DeleteFile(OutPath);

  Fs := TFileStream.Create(OutPath, fmCreate);
  try
    if ExtractEntryToStream(AFullPath, Fs) then
    begin
      ATempPath := OutPath;
      Result := True;
    end;
  finally
    Fs.Free;
  end;
end;

end.
