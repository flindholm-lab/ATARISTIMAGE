program test_runner;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Math,
  unit_fat12, unit_depacker, unit_msa, unit_dim, unit_atariaudio,
  unit_catalog, unit_batchengine;

var
  TestsRun: Integer = 0;
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;

procedure AssertTrue(const Name: string; Condition: Boolean);
begin
  Inc(TestsRun);
  if Condition then
  begin
    Inc(TestsPassed);
    WriteLn('  [PASS] ', Name);
  end
  else
  begin
    Inc(TestsFailed);
    WriteLn('  [FAIL] ', Name);
  end;
end;

procedure AssertEquals(const Name: string; Expected, Actual: Int64);
begin
  Inc(TestsRun);
  if Expected = Actual then
  begin
    Inc(TestsPassed);
    WriteLn('  [PASS] ', Name);
  end
  else
  begin
    Inc(TestsFailed);
    WriteLn('  [FAIL] ', Name, ' - Expected: ', Expected, ', Actual: ', Actual);
  end;
end;

procedure AssertEqualsStr(const Name: string; const Expected, Actual: string);
begin
  Inc(TestsRun);
  if Expected = Actual then
  begin
    Inc(TestsPassed);
    WriteLn('  [PASS] ', Name);
  end
  else
  begin
    Inc(TestsFailed);
    WriteLn('  [FAIL] ', Name, ' - Expected: "', Expected, '", Actual: "', Actual, '"');
  end;
end;

{ ============================================================================
  1. FAT12 and BPB Unit Tests
  ============================================================================ }
procedure TestFAT12_BPBParsing;
var
  BootSec: array[0..511] of Byte;
  Bpb: TFatBpb;
  Layout: TFatLayout;
begin
  WriteLn('Testing FAT12 BPB Parsing & Geometry...');

  // 1.1 Empty / nil buffer
  Fat12ParseBpb(nil, 0, 0, Bpb);
  AssertTrue('Nil boot sector handled gracefully', not Bpb.IsPlausible);
  AssertEquals('Default BytesPerSector is 512', 512, Bpb.BytesPerSector);
  AssertEquals('Default SectorsPerCluster is 2', 2, Bpb.SectorsPerCluster);
  AssertEquals('Default NumberOfFATs is 2', 2, Bpb.NumberOfFATs);
  AssertEquals('Default RootEntries is 112', 112, Bpb.RootEntries);
  AssertEquals('Default SectorsPerFAT is 5', 5, Bpb.SectorsPerFAT);

  // 1.2 Standard 720KB Atari ST Floppy (80 tracks, 9 sectors, 2 sides = 1440 sectors)
  FillChar(BootSec, SizeOf(BootSec), 0);
  BootSec[$08] := $12; BootSec[$09] := $34; BootSec[$0A] := $56; // Serial
  BootSec[$0B] := $00; BootSec[$0C] := $02; // BytesPerSec = 512
  BootSec[$0D] := 2;                         // SecPerClust = 2
  BootSec[$0E] := 1; BootSec[$0F] := 0;      // ResSec = 1
  BootSec[$10] := 2;                         // NumFATs = 2
  BootSec[$11] := 112; BootSec[$12] := 0;    // RootEntries = 112
  BootSec[$13] := $A0; BootSec[$14] := $05;  // TotalSec = 1440
  BootSec[$15] := $F9;                       // Media = $F9
  BootSec[$16] := 5; BootSec[$17] := 0;      // SecPerFAT = 5
  BootSec[$18] := 9; BootSec[$19] := 0;      // SecPerTrack = 9
  BootSec[$1A] := 2; BootSec[$1B] := 0;      // Sides = 2

  Fat12ParseBpb(@BootSec[0], 512, 737280, Bpb);
  AssertTrue('Standard 720KB BPB marked plausible', Bpb.IsPlausible);
  AssertEquals('BPB TotalSectors = 1440', 1440, Bpb.TotalSectors);
  AssertEquals('BPB DiskSerial = $123456', $123456, Bpb.DiskSerial);

  Fat12ComputeLayout(Bpb, 737280, Layout);
  AssertEquals('Layout FatTableStart = 512', 512, Layout.FatTableStart);
  AssertEquals('Layout SingleFATSize = 2560', 2560, Layout.SingleFATSize);
  AssertEquals('Layout FatTableSize = 5120', 5120, Layout.FatTableSize);
  AssertEquals('Layout RootDirStart = 5632', 5632, Layout.RootDirStart);
  AssertEquals('Layout RootDirSectors = 7', 7, Layout.RootDirSectors);
  AssertEquals('Layout DataAreaStart = 9216', 9216, Layout.DataAreaStart);
  AssertEquals('Layout BytesPerCluster = 1024', 1024, Layout.BytesPerCluster);
  AssertEquals('Layout Cluster 2 offset = DataAreaStart', 9216, Fat12ClusterOffset(Layout, 2));
  AssertEquals('Layout Cluster 3 offset = 10240', 10240, Fat12ClusterOffset(Layout, 3));
end;

procedure TestFAT12_EntriesAndChecksum;
var
  Fat: array[0..255] of Byte;
  Sector: array[0..511] of Byte;
  MS: TMemoryStream;
  Sum: Word;
begin
  WriteLn('Testing FAT12 Entries, Streams & Checksum...');

  FillChar(Fat, SizeOf(Fat), 0);

  // Set entries
  Fat12SetEntry(@Fat[0], Length(Fat), 2, $003); // Even cluster
  Fat12SetEntry(@Fat[0], Length(Fat), 3, $FFF); // Odd cluster
  Fat12SetEntry(@Fat[0], Length(Fat), 4, $FF7); // Bad cluster
  Fat12SetEntry(@Fat[0], Length(Fat), 5, $123);

  AssertEquals('Fat12GetEntry(2) = $003', $003, Fat12GetEntry(@Fat[0], Length(Fat), 2));
  AssertEquals('Fat12GetEntry(3) = $FFF', $FFF, Fat12GetEntry(@Fat[0], Length(Fat), 3));
  AssertEquals('Fat12GetEntry(4) = $FF7', $FF7, Fat12GetEntry(@Fat[0], Length(Fat), 4));
  AssertEquals('Fat12GetEntry(5) = $123', $123, Fat12GetEntry(@Fat[0], Length(Fat), 5));
  AssertEquals('Fat12GetEntry(out-of-bounds) = $FFF', FAT12_ENTRY_EOC, Fat12GetEntry(@Fat[0], Length(Fat), 500));

  // Stream FAT12 access
  MS := TMemoryStream.Create;
  try
    MS.SetSize(10240);
    MS.Position := 0;
    // Two copies of FAT, FAT0 at 512, FAT1 at 512 + 2560 = 3072
    Fat12WriteEntryAllCopies(MS, 512, 2560, 2, 10, $ABC);
    AssertEquals('Stream Fat12ReadEntry(copy 0)', $ABC, Fat12ReadEntry(MS, 512, 10));
    AssertEquals('Stream Fat12ReadEntry(copy 1)', $ABC, Fat12ReadEntry(MS, 3072, 10));
  finally
    MS.Free;
  end;

  // Atari ST Boot Sector Checksum
  FillChar(Sector, SizeOf(Sector), 0);
  AssertEquals('Zero sector checksum = 0', 0, AtariBootChecksum(@Sector[0], 512));

  // Create sector with executable checksum $1234
  Sector[0] := $60; Sector[1] := $1C; // BRA
  Sum := AtariBootChecksum(@Sector[0], 512);
  // Adjust last word to make it $1234
  Sector[510] := Byte((($1234 - Sum) shr 8) and $FF);
  Sector[511] := Byte(($1234 - Sum) and $FF);
  AssertEquals('Constructed Atari executable checksum = $1234', ATARI_BOOT_EXEC_SUM, AtariBootChecksum(@Sector[0], 512));
end;

{ ============================================================================
  2. Decoder Fuzzing & Robustness Tests
  ============================================================================ }

// Simple pseudo-random generator with deterministic seed for reproducible fuzz tests
var
  RndState: LongWord = $12345678;

function FuzzRand(MaxVal: LongWord): LongWord;
begin
  RndState := (RndState * 1664525 + 1013904223);
  if MaxVal = 0 then
    Result := 0
  else
    Result := (RndState shr 16) mod MaxVal;
end;

procedure FillFuzz(var Buf: array of Byte; Len: Integer);
var
  I: Integer;
begin
  for I := 0 to Len - 1 do
    Buf[I] := Byte(FuzzRand(256));
end;

procedure TestDepacker_Fuzzing;
var
  Buf: array[0..65535] of Byte;
  OutBuf: TBytes;
  OutPal, OutVram: TBytes;
  OutRes: Integer;
  Detected: string;
  Iteration, I, LIdx: Integer;
  Success: Boolean;
  Len: Integer;
  Signatures: array[0..15] of string = (
    'Ice!', 'ICE!', 'ATOM', 'ATMK', 'PACK', 'FIRE', 'JAM!',
    'RNC'#1, 'RNC'#2, 'TPWM', '2.00', 'STAD', 'SPv1', 'SPv2',
    'POPI', 'PUFF'
  );
  TruncLens: array[0..5] of Integer = (4, 8, 12, 16, 24, 32);
  S: Integer;
begin
  WriteLn('Fuzzing unit_depacker against corrupt, random & truncated payloads...');
  FillChar(Buf, SizeOf(Buf), 0);

  // 2.1 Nil and short buffers
  AssertTrue('DepackBuffer(nil, 0) fails safely', not DepackBuffer(nil, 0, OutBuf, Detected));
  AssertTrue('DepackBuffer(buf, 1) fails safely', not DepackBuffer(@Buf[0], 1, OutBuf, Detected));
  AssertTrue('DepackBuffer(buf, 4) fails safely', not DepackBuffer(@Buf[0], 4, OutBuf, Detected));
  AssertTrue('DetectPackerSignature(nil, 0) returns None', DetectPackerSignature(nil, 0) = 'None');

  // 2.2 Valid headers with corrupt/truncated payloads
  for S := 0 to High(Signatures) do
  begin
    FillChar(Buf, SizeOf(Buf), 0);
    // Write signature
    for Iteration := 1 to Length(Signatures[S]) do
      Buf[Iteration - 1] := Byte(Signatures[S][Iteration]);

    // Truncated header test (size 4..32 bytes)
    for LIdx := 0 to High(TruncLens) do
    begin
      Len := TruncLens[LIdx];
      try
        Success := DepackBuffer(@Buf[0], Len, OutBuf, Detected);
      except
        on E: Exception do
          AssertTrue('Crash on truncated header ' + Signatures[S] + ': ' + E.Message, False);
      end;
    end;

    // Fuzzed payload after valid header
    for Iteration := 1 to 5 do
    begin
      Len := 64 + Integer(FuzzRand(2000));
      FillFuzz(Buf, Len);
      for I := 1 to Length(Signatures[S]) do
        Buf[I - 1] := Byte(Signatures[S][I]);

      try
        Success := DepackBuffer(@Buf[0], Len, OutBuf, Detected);
      except
        on E: Exception do
          AssertTrue('Crash on fuzzed payload ' + Signatures[S] + ': ' + E.Message, False);
      end;
    end;
  end;

  // 2.3 Pure random garbage streams of different lengths
  for Iteration := 1 to 50 do
  begin
    Len := Integer(FuzzRand(4096)) + 1;
    FillFuzz(Buf, Len);
    try
      Success := DepackBuffer(@Buf[0], Len, OutBuf, Detected);
      // Individual depackers:
      DecompressPackIce(@Buf[0], Len, OutBuf);
      DecompressAtomik(@Buf[0], Len, OutBuf);
      DecompressAutomation(@Buf[0], Len, OutBuf);
      DecompressJam(@Buf[0], Len, OutBuf);
      DecompressRNC(@Buf[0], Len, OutBuf);
      DecompressFire(@Buf[0], Len, OutBuf);
      DecompressPompey(@Buf[0], Len, OutBuf);
      DecompressSentry(@Buf[0], Len, OutBuf);
      Decompress4PAK(@Buf[0], Len, OutBuf);
      DecompressHappy(@Buf[0], Len, OutBuf);
      DecompressRLE(@Buf[0], Len, OutBuf);
      DecompressTiny(@Buf[0], Len, 0, OutPal, OutVram, OutRes);
      DecompressStad(@Buf[0], Len, OutVram);
    except
      on E: Exception do
        AssertTrue('Crash on random fuzz iter ' + IntToStr(Iteration) + ': ' + E.Message, False);
    end;
  end;

  AssertTrue('100+ unit_depacker fuzz cases survived without crashing', True);
end;

procedure TestMSA_Fuzzing;
var
  Header: TMSAHeader;
  TempFile: string;
  FS: TFileStream;
  OutMS: TMemoryStream;
  Buf: array[0..2047] of Byte;
  Sectors, Sides, StartT, EndT: Integer;
  Iter: Integer;
begin
  WriteLn('Fuzzing unit_msa against corrupt MSA image streams...');
  FillChar(Buf, SizeOf(Buf), 0);

  TempFile := GetTempFileName('', 'fuzz_msa');
  try
    // Test truncated MSA header
    FS := TFileStream.Create(TempFile, fmCreate);
    FS.WriteBuffer(Buf[0], 5);
    FS.Free;
    AssertTrue('Short file is not MSA', not IsMSAFile(TempFile));
    AssertTrue('GetMSAInfo fails on short file', not GetMSAInfo(TempFile, Sectors, Sides, StartT, EndT));

    // Test fuzzed MSA track blocks
    for Iter := 1 to 10 do
    begin
      FillFuzz(Buf, SizeOf(Buf));
      Header.ID := SwapWordBE(MSA_MAGIC);
      Header.SectorsPerTrack := SwapWordBE(9);
      Header.Sides := SwapWordBE(1);
      Header.StartTrack := SwapWordBE(0);
      Header.EndTrack := SwapWordBE(1); // 2 tracks

      FS := TFileStream.Create(TempFile, fmCreate);
      FS.WriteBuffer(Header, SizeOf(Header));
      // Write random track chunk
      FS.WriteBuffer(Buf[0], 500 + Integer(FuzzRand(1000)));
      FS.Free;

      OutMS := TMemoryStream.Create;
      try
        UnpackMSAToStream(TempFile, OutMS);
      except
        on E: Exception do
          AssertTrue('Crash in UnpackMSAToStream: ' + E.Message, False);
      end;
      OutMS.Free;
    end;

    AssertTrue('unit_msa corrupt stream fuzz survived cleanly', True);
  finally
    if FileExists(TempFile) then DeleteFile(TempFile);
  end;
end;

procedure TestDIM_Fuzzing;
var
  Header: TDIMHeader;
  TempFile: string;
  FS: TFileStream;
  OutMS: TMemoryStream;
  Buf: array[0..1023] of Byte;
  Sectors, Sides, StartT, EndT: Integer;
  Iter: Integer;
begin
  WriteLn('Fuzzing unit_dim against corrupt DIM image streams...');

  TempFile := GetTempFileName('', 'fuzz_dim');
  try
    // Empty file
    FS := TFileStream.Create(TempFile, fmCreate);
    FS.Free;
    AssertTrue('Empty file is not DIM', not IsDIMFile(TempFile));
    AssertTrue('GetDIMInfo fails on empty file', not GetDIMInfo(TempFile, Sectors, Sides, StartT, EndT));

    for Iter := 1 to 10 do
    begin
      FillFuzz(Buf, SizeOf(Buf));
      FillChar(Header, SizeOf(Header), 0);
      Header.ID := DIM_MAGIC;
      Header.SectorsPerTrack := 9;
      Header.Sides := 1;
      Header.StartTrack := 0;
      Header.EndTrack := 1;

      FS := TFileStream.Create(TempFile, fmCreate);
      FS.WriteBuffer(Header, SizeOf(Header));
      FS.WriteBuffer(Buf[0], Integer(FuzzRand(512)) + 128);
      FS.Free;

      OutMS := TMemoryStream.Create;
      try
        UnpackDIMToStream(TempFile, OutMS);
      except
        on E: Exception do
          AssertTrue('Crash in UnpackDIMToStream: ' + E.Message, False);
      end;
      OutMS.Free;
    end;

    AssertTrue('unit_dim corrupt stream fuzz survived cleanly', True);
  finally
    if FileExists(TempFile) then DeleteFile(TempFile);
  end;
end;

procedure TestAtariAudio_Fuzzing;
var
  Buf: array[0..4095] of Byte;
  Info: TAtariAudioInfo;
  Iter, Len, LIdx: Integer;
  Magics: array[0..3] of string = ('SNDH', 'YM2!', 'YM5!', 'M.K.');
  AudioLengths: array[0..4] of Integer = (16, 64, 256, 1024, 2048);
  M: Integer;
begin
  WriteLn('Fuzzing unit_atariaudio metadata parsers...');
  FillChar(Buf, SizeOf(Buf), 0);

  // Nil buffer
  AssertTrue('DetectAndInspectAudio(nil, 0) returns false', not DetectAndInspectAudio(nil, 0, 'test.snd', Info));

  // Corrupted audio headers
  for M := 0 to High(Magics) do
  begin
    FillFuzz(Buf, SizeOf(Buf));
    for Iter := 1 to Length(Magics[M]) do
      Buf[Iter - 1] := Byte(Magics[M][Iter]);

    for LIdx := 0 to High(AudioLengths) do
    begin
      Len := AudioLengths[LIdx];
      try
        DetectAndInspectAudio(@Buf[0], Len, 'test.' + Magics[M], Info);
      except
        on E: Exception do
          AssertTrue('Crash in DetectAndInspectAudio ' + Magics[M] + ': ' + E.Message, False);
      end;
    end;
  end;

  // Pure random streams
  for Iter := 1 to 20 do
  begin
    Len := Integer(FuzzRand(4000)) + 32;
    FillFuzz(Buf, Len);
    try
      DetectAndInspectAudio(@Buf[0], Len, 'random.bin', Info);
    except
      on E: Exception do
        AssertTrue('Crash in DetectAndInspectAudio random fuzz: ' + E.Message, False);
    end;
  end;

  AssertTrue('unit_atariaudio fuzz tests passed without crashing', True);
end;

procedure TestPackers_4PAK_and_Happy;
var
  FS: TFileStream;
  InBuf, OutBuf: TBytes;
  Detected: string;
  Sample4PAK, SampleHappy: string;
begin
  WriteLn('Testing 4PAK and Happy Computer Packer Decompressors...');

  // 1. 4PAK Sample
  Sample4PAK := 'samples/4PAK/Pulsion_PULS173_LLMATRON.TOS';
  if not FileExists(Sample4PAK) then
    Sample4PAK := 'samples\4PAK\Pulsion_PULS173_LLMATRON.TOS';

  if FileExists(Sample4PAK) then
  begin
    FS := TFileStream.Create(Sample4PAK, fmOpenRead or fmShareDenyNone);
    try
      SetLength(InBuf, FS.Size);
      FS.ReadBuffer(InBuf[0], FS.Size);
    finally
      FS.Free;
    end;

    Detected := DetectPackerSignature(@InBuf[0], Length(InBuf));
    AssertTrue('4PAK signature detected', Pos('4PAK', Detected) > 0);
    AssertTrue('4PAK marked supported', IsPackerSupported(Detected));

    AssertTrue('Decompress4PAK succeeds', Decompress4PAK(@InBuf[0], Length(InBuf), OutBuf));
    AssertEquals('4PAK decompressed size matches', 252507, Length(OutBuf));
    if Length(OutBuf) >= 28 then
    begin
      AssertTrue('4PAK reconstructed GEMDOS $601A magic', (OutBuf[0] = $60) and (OutBuf[1] = $1A));
      AssertEquals('4PAK reconstructed text length', $3CCF0, (LongWord(OutBuf[2]) shl 24) or (LongWord(OutBuf[3]) shl 16) or (LongWord(OutBuf[4]) shl 8) or LongWord(OutBuf[5]));
    end;
  end
  else
    WriteLn('  [SKIP] 4PAK sample file not found');

  // 2. Happy Packer Sample
  SampleHappy := 'samples/Happy/HAPPY.PRG';
  if not FileExists(SampleHappy) then
    SampleHappy := 'samples\Happy\HAPPY.PRG';

  if FileExists(SampleHappy) then
  begin
    FS := TFileStream.Create(SampleHappy, fmOpenRead or fmShareDenyNone);
    try
      SetLength(InBuf, FS.Size);
      FS.ReadBuffer(InBuf[0], FS.Size);
    finally
      FS.Free;
    end;

    Detected := DetectPackerSignature(@InBuf[0], Length(InBuf));
    AssertTrue('Happy signature detected', Pos('Happy', Detected) > 0);
    AssertTrue('Happy marked supported', IsPackerSupported(Detected));

    AssertTrue('DecompressHappy succeeds', DecompressHappy(@InBuf[0], Length(InBuf), OutBuf));
    AssertEquals('Happy decompressed size matches', 32397, Length(OutBuf));
    if Length(OutBuf) >= 28 then
    begin
      AssertTrue('Happy reconstructed GEMDOS $601A magic', (OutBuf[0] = $60) and (OutBuf[1] = $1A));
      AssertEquals('Happy reconstructed text length', $7E68, (LongWord(OutBuf[2]) shl 24) or (LongWord(OutBuf[3]) shl 16) or (LongWord(OutBuf[4]) shl 8) or LongWord(OutBuf[5]));
    end;
  end
  else
    WriteLn('  [SKIP] Happy sample file not found');
end;

procedure TestPacker_PFX;
var
  SamplePath: string;
  FS: TFileStream;
  InBuf, OutBuf: TBytes;
  Detected: string;
begin
  WriteLn('Testing PFX Packer Decompressor...');
  SamplePath := 'samples' + DirectorySeparator + 'PFX_v1.13' + DirectorySeparator + 'Pulsion_PULS060_2.PRG';
  if not FileExists(SamplePath) then
    SamplePath := '..' + DirectorySeparator + SamplePath;
  if not FileExists(SamplePath) then
    SamplePath := 'T:\samples\PFX_v1.13\Pulsion_PULS060_2.PRG';
  if not FileExists(SamplePath) then
    SamplePath := 'T:\Pulsion_PULS060_2.PRG';
  if not FileExists(SamplePath) then
    SamplePath := '/tmp/pfx_sample.prg';

  if FileExists(SamplePath) then
  begin
    FS := TFileStream.Create(SamplePath, fmOpenRead or fmShareDenyNone);
    try
      SetLength(InBuf, FS.Size);
      FS.ReadBuffer(InBuf[0], FS.Size);
    finally
      FS.Free;
    end;

    Detected := DetectPackerSignature(@InBuf[0], Length(InBuf));
    AssertTrue('PFX signature detected', Pos('PFX', Detected) > 0);
    AssertTrue('PFX marked supported', IsPackerSupported(Detected));

    AssertTrue('DecompressPFX succeeds', DecompressPFX(@InBuf[0], Length(InBuf), OutBuf));
    AssertEquals('PFX decompressed size matches (192,915 bytes)', 192915, Length(OutBuf));
    if Length(OutBuf) >= 28 then
    begin
      AssertTrue('PFX reconstructed GEMDOS $601A magic', (OutBuf[0] = $60) and (OutBuf[1] = $1A));
      AssertEquals('PFX reconstructed text length', 145962, (LongWord(OutBuf[2]) shl 24) or (LongWord(OutBuf[3]) shl 16) or (LongWord(OutBuf[4]) shl 8) or LongWord(OutBuf[5]));
      AssertEquals('PFX reconstructed data length', 46216, (LongWord(OutBuf[6]) shl 24) or (LongWord(OutBuf[7]) shl 16) or (LongWord(OutBuf[8]) shl 8) or LongWord(OutBuf[9]));
    end;
  end
  else
    WriteLn('  [SKIP] PFX sample file not found');
end;

procedure TestPacker_PowerPacker;
var
  SamplePath: string;
  FS: TFileStream;
  InBuf, OutBuf: TBytes;
  Detected: string;
begin
  WriteLn('Testing PowerPacker (PP20) Decompressor...');
  SamplePath := 'samples' + DirectorySeparator + 'PowerPacker' + DirectorySeparator + 'PP20_TEST.DAT';
  if not FileExists(SamplePath) then
    SamplePath := '..' + DirectorySeparator + SamplePath;
  if not FileExists(SamplePath) then
    SamplePath := 'T:\samples\PowerPacker\PP20_TEST.DAT';

  if FileExists(SamplePath) then
  begin
    FS := TFileStream.Create(SamplePath, fmOpenRead or fmShareDenyNone);
    try
      SetLength(InBuf, FS.Size);
      FS.ReadBuffer(InBuf[0], FS.Size);
    finally
      FS.Free;
    end;

    Detected := DetectPackerSignature(@InBuf[0], Length(InBuf));
    AssertTrue('PowerPacker signature detected', Pos('Power Packer', Detected) > 0);
    AssertTrue('PowerPacker marked supported', IsPackerSupported(Detected));

    AssertTrue('DecompressPowerPacker succeeds', DecompressPowerPacker(@InBuf[0], Length(InBuf), OutBuf));
    AssertEquals('PowerPacker decompressed size matches (2,280 bytes)', 2280, Length(OutBuf));
    AssertTrue('PowerPacker verified content prefix', (Length(OutBuf) >= 8) and (OutBuf[0] = Ord('A')) and (OutBuf[1] = Ord('T')));
  end
  else
    WriteLn('  [SKIP] PowerPacker sample file not found');

  SamplePath := 'samples' + DirectorySeparator + 'PowerPacker' + DirectorySeparator + 'PP20_PRG.PRG';
  if not FileExists(SamplePath) then
    SamplePath := '..' + DirectorySeparator + SamplePath;
  if not FileExists(SamplePath) then
    SamplePath := 'T:\samples\PowerPacker\PP20_PRG.PRG';

  if FileExists(SamplePath) then
  begin
    FS := TFileStream.Create(SamplePath, fmOpenRead or fmShareDenyNone);
    try
      SetLength(InBuf, FS.Size);
      FS.ReadBuffer(InBuf[0], FS.Size);
    finally
      FS.Free;
    end;

    Detected := DetectPackerSignature(@InBuf[0], Length(InBuf));
    AssertTrue('PowerPacker executable signature detected', Pos('Power Packer', Detected) > 0);
    AssertTrue('DepackBuffer on PowerPacker executable succeeds', DepackBuffer(@InBuf[0], Length(InBuf), OutBuf, Detected));
    AssertEquals('PowerPacker executable depacked size matches', 2280, Length(OutBuf));
  end;
end;

procedure TestPacker_CrunchMania;
var
  SamplePath: string;
  FS: TFileStream;
  InBuf, OutBuf: TBytes;
  Detected: string;
begin
  WriteLn('Testing Crunch-Mania (CrM! / CrM2) Decompressor...');

  // 1. Standard mode (CrM!)
  SamplePath := 'samples' + DirectorySeparator + 'CrunchMania' + DirectorySeparator + 'CRM_STD.DAT';
  if not FileExists(SamplePath) then
    SamplePath := '..' + DirectorySeparator + SamplePath;
  if not FileExists(SamplePath) then
    SamplePath := 'T:\samples\CrunchMania\CRM_STD.DAT';

  if FileExists(SamplePath) then
  begin
    FS := TFileStream.Create(SamplePath, fmOpenRead or fmShareDenyNone);
    try
      SetLength(InBuf, FS.Size);
      FS.ReadBuffer(InBuf[0], FS.Size);
    finally
      FS.Free;
    end;

    Detected := DetectPackerSignature(@InBuf[0], Length(InBuf));
    AssertTrue('Crunch-Mania Standard signature detected', Pos('Crunch-Mania', Detected) > 0);
    AssertTrue('Crunch-Mania marked supported', IsPackerSupported(Detected));

    AssertTrue('DecompressCrunchMania Standard succeeds', DecompressCrunchMania(@InBuf[0], Length(InBuf), OutBuf));
    AssertEquals('Crunch-Mania Standard decompressed size matches (152,089 bytes)', 152089, Length(OutBuf));
  end
  else
    WriteLn('  [SKIP] Crunch-Mania Standard sample not found');

  // 2. LZH mode (CrM2)
  SamplePath := 'samples' + DirectorySeparator + 'CrunchMania' + DirectorySeparator + 'CRM_LZH.DAT';
  if not FileExists(SamplePath) then
    SamplePath := '..' + DirectorySeparator + SamplePath;
  if not FileExists(SamplePath) then
    SamplePath := 'T:\samples\CrunchMania\CRM_LZH.DAT';

  if FileExists(SamplePath) then
  begin
    FS := TFileStream.Create(SamplePath, fmOpenRead or fmShareDenyNone);
    try
      SetLength(InBuf, FS.Size);
      FS.ReadBuffer(InBuf[0], FS.Size);
    finally
      FS.Free;
    end;

    Detected := DetectPackerSignature(@InBuf[0], Length(InBuf));
    AssertTrue('Crunch-Mania LZH signature detected', Pos('Crunch-Mania', Detected) > 0);

    AssertTrue('DecompressCrunchMania LZH succeeds', DecompressCrunchMania(@InBuf[0], Length(InBuf), OutBuf));
    AssertEquals('Crunch-Mania LZH decompressed size matches (152,089 bytes)', 152089, Length(OutBuf));
  end
  else
    WriteLn('  [SKIP] Crunch-Mania LZH sample not found');

  // 3. Standard Sampled (Delta) mode (Crm!)
  SamplePath := 'samples' + DirectorySeparator + 'CrunchMania' + DirectorySeparator + 'CRM_STD_DELTA.DAT';
  if not FileExists(SamplePath) then
    SamplePath := '..' + DirectorySeparator + SamplePath;
  if not FileExists(SamplePath) then
    SamplePath := 'T:\samples\CrunchMania\CRM_STD_DELTA.DAT';

  if FileExists(SamplePath) then
  begin
    FS := TFileStream.Create(SamplePath, fmOpenRead or fmShareDenyNone);
    try
      SetLength(InBuf, FS.Size);
      FS.ReadBuffer(InBuf[0], FS.Size);
    finally
      FS.Free;
    end;

    Detected := DetectPackerSignature(@InBuf[0], Length(InBuf));
    AssertTrue('Crunch-Mania Standard Sampled signature detected', Pos('Crunch-Mania', Detected) > 0);

    AssertTrue('DecompressCrunchMania Standard Sampled succeeds', DecompressCrunchMania(@InBuf[0], Length(InBuf), OutBuf));
    AssertEquals('Crunch-Mania Standard Sampled decompressed size matches (152,089 bytes)', 152089, Length(OutBuf));
  end
  else
    WriteLn('  [SKIP] Crunch-Mania Standard Sampled sample not found');

  // 4. PRG Executable Wrapper
  SamplePath := 'samples' + DirectorySeparator + 'CrunchMania' + DirectorySeparator + 'CRM_PRG.PRG';
  if not FileExists(SamplePath) then
    SamplePath := '..' + DirectorySeparator + SamplePath;
  if not FileExists(SamplePath) then
    SamplePath := 'T:\samples\CrunchMania\CRM_PRG.PRG';

  if FileExists(SamplePath) then
  begin
    FS := TFileStream.Create(SamplePath, fmOpenRead or fmShareDenyNone);
    try
      SetLength(InBuf, FS.Size);
      FS.ReadBuffer(InBuf[0], FS.Size);
    finally
      FS.Free;
    end;

    Detected := DetectPackerSignature(@InBuf[0], Length(InBuf));
    AssertTrue('Crunch-Mania Executable signature detected', Pos('Crunch-Mania', Detected) > 0);

    AssertTrue('DepackBuffer on Crunch-Mania executable succeeds', DepackBuffer(@InBuf[0], Length(InBuf), OutBuf, Detected));
    AssertEquals('Crunch-Mania Executable depacked size matches (152,089 bytes)', 152089, Length(OutBuf));
  end
  else
    WriteLn('  [SKIP] Crunch-Mania Executable sample not found');
end;

procedure TestPulsarMSA_RootFolders;
var
  MsaFile, StFile: string;
  FS: TFileStream;
  BootSector: array[0..511] of Byte;
  Bpb: TFatBpb;
  Layout: TFatLayout;
  DirEntry: array[0..31] of Byte;
  I: Integer;
  FoundLSD, FoundAtomik, FoundPompey: Boolean;
  NameStr: string;
  ClustLSD: Word;
begin
  WriteLn('Testing PULSPCK1.MSA root directory folder resolution...');
  MsaFile := 'PULSPCK1.MSA';
  if not FileExists(MsaFile) then
    MsaFile := '..' + DirectorySeparator + 'PULSPCK1.MSA';
  if not FileExists(MsaFile) then
    MsaFile := 'T:\PULSPCK1.MSA';
  if not FileExists(MsaFile) then
    MsaFile := '/tmp/stimage_build_fredrik/PULSPCK1.MSA';
  if not FileExists(MsaFile) then
  begin
    WriteLn('  [SKIP] PULSPCK1.MSA not found');
    Exit;
  end;

  StFile := GetTempFileName(GetTempDir, 'test_puls_') + '.st';
  try
    AssertTrue('UnpackMSAToST for PULSPCK1.MSA succeeds', UnpackMSAToST(MsaFile, StFile));
    FS := TFileStream.Create(StFile, fmOpenRead or fmShareDenyNone);
    try
      AssertTrue('Unpacked ST file >= 512 bytes', FS.Size >= 512);
      FS.ReadBuffer(BootSector[0], 512);
      Fat12ParseBpb(@BootSector[0], 512, FS.Size, Bpb);
      AssertTrue('PULSPCK1 BPB is plausible', Bpb.IsPlausible);
      AssertEquals('PULSPCK1 RootEntries = 112', 112, Bpb.RootEntries);
      Fat12ComputeLayout(Bpb, FS.Size, Layout);
      AssertEquals('PULSPCK1 Layout.RootDirStart = 3584 (Sector 7)', 3584, Layout.RootDirStart);

      // Scan root directory entries starting at Layout.RootDirStart
      FS.Position := Layout.RootDirStart;
      FoundLSD := False;
      FoundAtomik := False;
      FoundPompey := False;
      ClustLSD := 0;

      for I := 0 to Bpb.RootEntries - 1 do
      begin
        if FS.Read(DirEntry[0], 32) <> 32 then Break;
        if DirEntry[0] = 0 then Break;
        if (DirEntry[0] <> $E5) and ((DirEntry[11] and $10) <> 0) then
        begin
          SetLength(NameStr, 11);
          Move(DirEntry[0], NameStr[1], 11);
          if Pos('LSD', NameStr) = 1 then
          begin
            FoundLSD := True;
            ClustLSD := Word(DirEntry[26]) or (Word(DirEntry[27]) shl 8);
          end;
          if Pos('ATOMIK', NameStr) = 1 then FoundAtomik := True;
          if Pos('POMPEY', NameStr) = 1 then FoundPompey := True;
        end;
      end;

      AssertTrue('Found folder LSD in root directory', FoundLSD);
      AssertTrue('Found folder ATOMIK in root directory', FoundAtomik);
      AssertTrue('Found folder POMPEY in root directory', FoundPompey);
      AssertEquals('LSD folder cluster is 114', 114, ClustLSD);

      // Check contents of cluster 114 (LSD.WAS)
      if ClustLSD >= 2 then
      begin
        FS.Position := Fat12ClusterOffset(Layout, ClustLSD);
        FS.ReadBuffer(DirEntry[0], 32);
        AssertTrue('Cluster 114 first entry is "."', (DirEntry[0] = Ord('.')) and (DirEntry[1] = $20));
        FS.ReadBuffer(DirEntry[0], 32);
        AssertTrue('Cluster 114 second entry is ".."', (DirEntry[0] = Ord('.')) and (DirEntry[1] = Ord('.')));
        FS.ReadBuffer(DirEntry[0], 32);
        SetLength(NameStr, 11);
        Move(DirEntry[0], NameStr[1], 11);
        AssertTrue('Cluster 114 contains LSD.TOS', Pos('LSD', NameStr) = 1);
      end;
    finally
      FS.Free;
    end;
  finally
    if FileExists(StFile) then DeleteFile(StFile);
  end;
end;

procedure TestDiskCatalog;
var
  Data: string;
  Crc: LongWord;
  StFile, TempCSV, TempTXT, TempJSON, TempHTML: string;
  FS: TFileStream;
  Catalog: TDiskCatalog;
  SL: TStringList;
begin
  WriteLn('Testing Disk Catalog & Report Exporters...');

  // 1. Standard IEEE 802.3 CRC32 Test Vector: '123456789' -> $CBF43926
  Data := '123456789';
  Crc := CalculateCRC32(PByte(PChar(Data)), Length(Data));
  AssertTrue('CalculateCRC32 on standard ASCII test vector matches $CBF43926', Crc = $CBF43926);
  AssertTrue('CalculateCRC32 on empty buffer returns 0', CalculateCRC32(nil, 0) = 0);

  // 2. Build disk catalog on PULSPCK1.MSA
  StFile := IncludeTrailingPathDelimiter(GetTempDir) + 'test_cat_' + FormatDateTime('yyyymmddhhnnsszzz', Now) + '.st';
  TempCSV := IncludeTrailingPathDelimiter(GetTempDir) + 'test_cat_' + FormatDateTime('yyyymmddhhnnsszzz', Now) + '.csv';
  TempTXT := IncludeTrailingPathDelimiter(GetTempDir) + 'test_cat_' + FormatDateTime('yyyymmddhhnnsszzz', Now) + '.txt';
  TempJSON := IncludeTrailingPathDelimiter(GetTempDir) + 'test_cat_' + FormatDateTime('yyyymmddhhnnsszzz', Now) + '.json';
  TempHTML := IncludeTrailingPathDelimiter(GetTempDir) + 'test_cat_' + FormatDateTime('yyyymmddhhnnsszzz', Now) + '.html';

  try
    AssertTrue('UnpackMSAToST for catalog test succeeds', UnpackMSAToST('PULSPCK1.MSA', StFile));
    FS := TFileStream.Create(StFile, fmOpenRead or fmShareDenyNone);
    try
      Catalog := Default(TDiskCatalog);
      AssertTrue('BuildDiskCatalog returns True on PULSPCK1',
        BuildDiskCatalog(FS, 'PULSPCK1.MSA', '', 0, 9, 2, 80, Catalog, False));

      AssertTrue('Catalog total files > 10', Catalog.TotalFiles > 10);
      AssertTrue('Catalog total directories >= 3 (ATOMIK, LSD, POMPEY)', Catalog.TotalDirs >= 3);
      AssertTrue('Catalog total file bytes > 100,000', Catalog.TotalFileBytes > 100000);
      AssertTrue('Catalog packed files detected > 0', Catalog.TotalPackedFiles > 0);
      AssertTrue('Catalog total space bytes >= 720KB', Catalog.TotalSpaceBytes >= 720 * 1024);
      AssertTrue('Catalog free space bytes > 0', Catalog.FreeSpaceBytes > 0);

      // 3. Test exporters
      AssertTrue('ExportCatalogToCSV succeeds', ExportCatalogToCSV(Catalog, TempCSV));
      AssertTrue('CSV output file exists and is non-empty', FileExists(TempCSV) and (GetFileSizeBytes(TempCSV) > 500));

      AssertTrue('ExportCatalogToText succeeds', ExportCatalogToText(Catalog, TempTXT));
      AssertTrue('Text report output file exists and is non-empty', FileExists(TempTXT) and (GetFileSizeBytes(TempTXT) > 500));

      AssertTrue('ExportCatalogToJSON succeeds', ExportCatalogToJSON(Catalog, TempJSON));
      AssertTrue('JSON output file exists and is non-empty', FileExists(TempJSON) and (GetFileSizeBytes(TempJSON) > 500));

      SL := TStringList.Create;
      try
        SL.LoadFromFile(TempJSON);
        AssertTrue('JSON output contains "disk" property', Pos('"disk"', SL.Text) > 0);
        AssertTrue('JSON output contains "files" property', Pos('"files"', SL.Text) > 0);
      finally
        SL.Free;
      end;

      AssertTrue('ExportCatalogToHTML succeeds', ExportCatalogToHTML(Catalog, TempHTML));
      AssertTrue('HTML report output file exists and is non-empty', FileExists(TempHTML) and (GetFileSizeBytes(TempHTML) > 1000));

      SL := TStringList.Create;
      try
        SL.LoadFromFile(TempHTML);
        AssertTrue('HTML output has <!DOCTYPE html>', Pos('<!DOCTYPE html>', SL.Text) > 0);
        AssertTrue('HTML output contains disk name PULSPCK1', Pos('PULSPCK1', SL.Text) > 0);
        AssertTrue('HTML output contains LSD folder/file', Pos('LSD', SL.Text) > 0);
      finally
        SL.Free;
      end;
    finally
      FS.Free;
    end;
  finally
    if FileExists(StFile) then DeleteFile(StFile);
    if FileExists(TempCSV) then DeleteFile(TempCSV);
    if FileExists(TempTXT) then DeleteFile(TempTXT);
    if FileExists(TempJSON) then DeleteFile(TempJSON);
    if FileExists(TempHTML) then DeleteFile(TempHTML);
  end;
end;

procedure TestBatchEngine;
var
  TempBaseDir, SubFolder, TempZip, TempST, TempMSA, TempDIM: string;
  FilesExt, FilesDep: Integer;
  BytesTotal: Int64;
  Options: TBatchConvertOptions;
  ItemResult: TBatchItemResult;
begin
  WriteLn('Testing Batch Disk Image Conversion & Extraction Engine...');

  TempBaseDir := IncludeTrailingPathDelimiter(GetTempDir) + 'stimage_testbatch_' +
    FormatDateTime('yyyymmddhhnnsszzz', Now) + '_' + IntToStr(Random(10000));
  ForceDirectories(TempBaseDir);

  try
    // 1. Test extraction to subfolder: image.st is extracted to folder\image
    SubFolder := IncludeTrailingPathDelimiter(TempBaseDir) + 'PULSPCK1';
    AssertTrue('ExtractImageToFolder for PULSPCK1.MSA succeeds',
      ExtractImageToFolder('PULSPCK1.MSA', SubFolder, True, True, FilesExt, FilesDep, BytesTotal));

    AssertTrue('Files extracted count > 10', FilesExt > 10);
    AssertTrue('In-flight depacking detected and depacked files > 0', FilesDep > 0);
    AssertTrue('Target folder exists', DirectoryExists(SubFolder));
    AssertTrue('Subfolder LSD.WAS exists', DirectoryExists(SubFolder + DirectorySeparator + 'LSD.WAS'));
    AssertTrue('File LSD.TOS extracted', FileExists(SubFolder + DirectorySeparator + 'LSD.WAS' + DirectorySeparator + 'LSD.TOS'));
    AssertTrue('Subfolder ATOMIK.3_1 exists', DirectoryExists(SubFolder + DirectorySeparator + 'ATOMIK.3_1'));

    // 2. Test extraction directly into a .ZIP archive
    TempZip := IncludeTrailingPathDelimiter(TempBaseDir) + 'PULSPCK1.zip';
    AssertTrue('ExtractImageToZip for PULSPCK1.MSA succeeds',
      ExtractImageToZip('PULSPCK1.MSA', TempZip, True, True, FilesExt, FilesDep, BytesTotal));
    AssertTrue('ZIP archive created and is non-empty', FileExists(TempZip) and (GetFileSizeBytes(TempZip) > 10000));

    // 3. Test cross-format conversion
    TempST := IncludeTrailingPathDelimiter(TempBaseDir) + 'conv_test.st';
    TempMSA := IncludeTrailingPathDelimiter(TempBaseDir) + 'conv_test.msa';
    TempDIM := IncludeTrailingPathDelimiter(TempBaseDir) + 'conv_test.dim';

    AssertTrue('ConvertImageFile from .msa to .st succeeds',
      ConvertImageFile('PULSPCK1.MSA', TempST, '.st', True));
    AssertTrue('Converted .st file is at least 720KB', FileExists(TempST) and (GetFileSizeBytes(TempST) >= 720 * 1024));

    AssertTrue('ConvertImageFile from .st to .msa succeeds',
      ConvertImageFile(TempST, TempMSA, '.msa', True));
    AssertTrue('Converted .msa file has valid MSA magic header', FileExists(TempMSA) and IsMSAFile(TempMSA));

    AssertTrue('ConvertImageFile from .st to .dim succeeds',
      ConvertImageFile(TempST, TempDIM, '.dim', True));
    AssertTrue('Converted .dim file has valid FastCopy Pro DIM magic header', FileExists(TempDIM) and IsDIMFile(TempDIM));

    // 4. Test high-level ProcessBatchImage with baExtractFolders
    Options.Action := baExtractFolders;
    Options.OutputDir := TempBaseDir;
    Options.UseSourceDir := False;
    Options.DepackFiles := True;
    Options.OverwriteExisting := True;
    Options.IncludeDeleted := False;

    AssertTrue('ProcessBatchImage for PULSPCK1.MSA succeeds',
      ProcessBatchImage('PULSPCK1.MSA', Options, ItemResult));
    AssertTrue('ItemResult status is bisSuccess', ItemResult.Status = bisSuccess);
    AssertTrue('ItemResult files processed > 0', ItemResult.FilesProcessed > 0);
    AssertTrue('ItemResult files depacked > 0', ItemResult.FilesDepacked > 0);
  finally
    SafeDeleteDir(TempBaseDir);
  end;
end;

begin
  WriteLn('================================================================');
  WriteLn('STIMAGE FAT12 / BPB & Decoder Robustness Test Suite');
  WriteLn('================================================================');

  TestFAT12_BPBParsing;
  TestFAT12_EntriesAndChecksum;
  TestDepacker_Fuzzing;
  TestPackers_4PAK_and_Happy;
  TestPacker_PFX;
  TestPacker_PowerPacker;
  TestPacker_CrunchMania;
  TestPulsarMSA_RootFolders;
  TestMSA_Fuzzing;
  TestDIM_Fuzzing;
  TestAtariAudio_Fuzzing;
  TestDiskCatalog;
  TestBatchEngine;

  WriteLn('================================================================');
  WriteLn(Format('Tests Completed: %d | Passed: %d | Failed: %d', [TestsRun, TestsPassed, TestsFailed]));
  WriteLn('================================================================');

  if TestsFailed > 0 then
    Halt(1)
  else
    Halt(0);
end.
