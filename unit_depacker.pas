unit unit_depacker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  TBytes = SysUtils.TBytes;

// Detection & Information
function DetectPackerSignature(const Buffer: PByte; Size: Integer): string; overload;
function DetectPackerSignature(const Bytes: TBytes): string; overload;
function DetectPackerSignature(Stream: TStream): string; overload;
function IsPackerSupported(const PackerName: string): Boolean;

// Universal Depacker Dispatcher
function DepackBuffer(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes; out DetectedPacker: string): Boolean;
function DepackStream(InStream: TStream; OutStream: TStream; out DetectedPacker: string): Boolean;

// Specialized Decompressors
function DecompressPackIce(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
function DecompressAtomik(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
function DecompressAutomation(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
function DecompressJam(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
function DecompressRNC(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
function DecompressThunderV1(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
function DecompressThunderV2(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
function DecompressJek(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
function DecompressDegasElite(const InBuf: PByte; InSize: Integer; DefaultRes: Integer; out OutBuf: TBytes): Boolean;
function DecompressRLE(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
function DecompressFire(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
function DecompressTiny(const InBuf: PByte; InSize: Integer; DefaultRes: Integer; out OutPalette: TBytes; out OutVRAM: TBytes; out OutRes: Integer): Boolean;
function DecompressStad(const InBuf: PByte; InSize: Integer; out OutScreen: TBytes): Boolean;
function DecompressStosSquash(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
function DecompressStosCompact(const InBuf: PByte; InSize: Integer; out OutPalette: TBytes; out OutVRAM: TBytes; out OutRes: Integer): Boolean;

implementation

// -----------------------------------------------------------------------------
// Bit and Byte Utilities
// -----------------------------------------------------------------------------

function ReadBE16(const P: PByte; Offset: Integer): Word; inline;
begin
  Result := (Word(P[Offset]) shl 8) or Word(P[Offset + 1]);
end;

function ReadBE32(const P: PByte; Offset: Integer): LongWord; inline;
begin
  Result := (LongWord(P[Offset]) shl 24) or
            (LongWord(P[Offset + 1]) shl 16) or
            (LongWord(P[Offset + 2]) shl 8) or
            LongWord(P[Offset + 3]);
end;

function CheckLong(const P: PByte; Size: Integer; Offset: Integer; Val: LongWord): Boolean; inline;
begin
  if (Offset < 0) or (Offset + 4 > Size) then Exit(False);
  Result := ReadBE32(P, Offset) = Val;
end;

function CheckWord(const P: PByte; Size: Integer; Offset: Integer; Val: Word): Boolean; inline;
begin
  if (Offset < 0) or (Offset + 2 > Size) then Exit(False);
  Result := ReadBE16(P, Offset) = Val;
end;

function CheckString(const P: PByte; Size: Integer; Offset: Integer; const S: string): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  if (Offset < 0) or (Offset + L > Size) then Exit(False);
  for I := 1 to L do
    if P[Offset + I - 1] <> Byte(S[I]) then Exit(False);
  Result := True;
end;

function CheckLastLongString(const P: PByte; Size: Integer; const S: string): Boolean;
begin
  if Size < 4 then Exit(False);
  Result := CheckString(P, Size, Size - 4, S);
end;

function FindEmbeddedSig(const P: PByte; Size: Integer; const Sig: string; StartOffset: Integer = 0): Integer;
var
  I, J, SigLen, MaxScan: Integer;
  Matched: Boolean;
begin
  Result := -1;
  SigLen := Length(Sig);
  if (SigLen = 0) or (Size < SigLen) then Exit;
  MaxScan := Size - SigLen;
  for I := StartOffset to MaxScan do
  begin
    Matched := True;
    for J := 1 to SigLen do
    begin
      if P[I + J - 1] <> Byte(Sig[J]) then
      begin
        Matched := False;
        Break;
      end;
    end;
    if Matched then Exit(I);
  end;
end;

function BufferToAscii(const P: PByte; Size: Integer; MaxLen: Integer): string;
var
  I, L: Integer;
  B: Byte;
begin
  L := Min(Size, MaxLen);
  SetLength(Result, L);
  for I := 0 to L - 1 do
  begin
    B := P[I];
    if (B >= 32) and (B <= 126) then
      Result[I + 1] := Char(B)
    else
      Result[I + 1] := ' ';
  end;
end;

// -----------------------------------------------------------------------------
// Packer Signature Detection
// -----------------------------------------------------------------------------

function DetectPackerSignature(const Buffer: PByte; Size: Integer): string;
var
  AsciiContext, TailAscii: string;
  Idx: Integer;
  ULen, PLen, AbsEnd, AbsLitOff, LitLen, AbsLitStart: LongWord;
  DummyOut: TBytes;
begin
  Result := 'None';
  if (Buffer = nil) or (Size < 4) then Exit;

  // 1. 4PAK
  if CheckLong(Buffer, Size, 58, $4E714EF9) then
    Exit('4PAK Executable Packer');

  // 2. Automation Packer
  if CheckString(Buffer, Size, 0, 'LSD!') then
    Exit('Automation Packer v2.x/v3.x / JAM / LSD Raw Data');
  if CheckString(Buffer, Size, 0, 'LSD$') or CheckString(Buffer, Size, 0, 'AUTM') then
    Exit('Automation Packer v2.x/v3.x Raw Data');
  if CheckString(Buffer, Size, 0, 'LSDC') then
    Exit('Automation Chunked Raw Data');
  if CheckString(Buffer, Size, 0, 'AU5!') then
    Exit('Automation Packer v5.01 Raw Data');
  if CheckString(Buffer, Size, 0, 'AU5C') then
    Exit('Automation Packer v5.01 Chunked Raw Data');
  if CheckLong(Buffer, Size, 482, $2E337200) then
    Exit('Automation Packer v2.3r Executable');
  if CheckLong(Buffer, Size, 482, $2E353100) or CheckLong(Buffer, Size, 482, $2E357200) then
    Exit('Automation Packer v2.51 Executable');
  if CheckString(Buffer, Size, 836, 'AU5!') then
    Exit('Automation Packer v5.01 Executable');

  // 3. ATOM Packer
  if CheckString(Buffer, Size, 0, 'ATOM') then
    Exit('Atomik / Thunder Raw Data');
  if CheckString(Buffer, Size, 0, 'ATM5') then
    Exit('Atomik Cruncher v3.5 Raw Data');
  if CheckString(Buffer, Size, 516, 'ATOM') then
    Exit('Atomik Cruncher v3.1 Executable Packer');
  if CheckString(Buffer, Size, 538, 'ATOM') then
    Exit('Atomik Cruncher v3.3 Executable Packer');
  if CheckString(Buffer, Size, 564, '/BMT') then
    Exit('Atomik Cruncher v3.3 BMT Executable Packer');
  if CheckLong(Buffer, Size, 336, $1B3250F0) or CheckString(Buffer, Size, 634, '3.5 ') then
    Exit('Atomik Cruncher v3.5 Executable Packer');

  // 4. BRAS
  if CheckString(Buffer, Size, 30, 'BRAS') then
    Exit('BRAS Executable Packer');

  // 5. Bytekiller
  if CheckLastLongString(Buffer, Size, 'JPM!') then
    Exit('Bytekiller (JPM Data) Raw Data');
  if CheckLong(Buffer, Size, 72, $0007FD00) then
    Exit('Bytekiller v2 Executable Packer');
  if CheckLong(Buffer, Size, 72, $224A7053) then
    Exit('Bytekiller v3 Executable Packer');
  if CheckLong(Buffer, Size, 28, $487A00AA) then
    Exit('Bytekiller (Russ Payne) Executable Packer');
  if CheckLong(Buffer, Size, 190, $4EFA0020) then
    Exit('Bytekiller (JPM Prog) Executable Packer');

  // 6. DC Squish Packer
  if CheckString(Buffer, Size, 40, 'Squi') then
  begin
    if CheckString(Buffer, Size, 58, '10') then Exit('DC Squish Packer v1.0 Executable');
    if CheckString(Buffer, Size, 58, '12') then Exit('DC Squish Packer v1.2 Executable');
    if CheckString(Buffer, Size, 58, '14') then Exit('DC Squish Packer v1.4 Executable');
    if CheckString(Buffer, Size, 58, '21') then Exit('DC Squish Packer v2.1 Executable');
    Exit('DC Squish Packer Executable');
  end;

  // 7. Gollum Packer
  if CheckLong(Buffer, Size, 192, $42401018) then
    Exit('Gollum Packer (Non-Huffman) Executable');
  if CheckLong(Buffer, Size, 192, $22572C7A) then
    Exit('Gollum Packer (Huffman) Executable');

  // 8. Gremlin Packer
  if CheckLastLongString(Buffer, Size, 'GUS*') then
    Exit('Gremlin Packer Raw Data');

  // 9. Happy Packer
  if CheckString(Buffer, Size, 52, 'EASY') then
    Exit('Happy Packer Executable');

    // Pack-Ice executable banner check (e.g. MENU70, MENU71, MENU72, INTRO)
  if (Size >= 64) and (FindEmbeddedSig(Buffer, Min(Size, 256), 'Pack-Ice') >= 0) then
    Exit('Pack-Ice v2.1 Executable Packer');
// 10. ICE Packer / Superior
  if CheckWord(Buffer, Size, 28, 008) and (CheckString(Buffer, Size, 30, 'Pack-Ice') or CheckString(Buffer, Size, 30, 'Ice!')) then
    Exit('Pack-Ice v2.0 Executable Packer');
  if CheckWord(Buffer, Size, 28, 006) and CheckString(Buffer, Size, 30, 'Ice!') then
    Exit('Pack-Ice v2.0 Executable Packer');
  if CheckLastLongString(Buffer, Size, 'Ice!') then
    Exit('ICE Packer v1.1 Raw Data');
  if CheckString(Buffer, Size, 0, 'Ice!') then
    Exit('Pack-Ice v2.0/v2.2 Raw Data');
  if CheckString(Buffer, Size, 0, 'ICE!') then
    Exit('Pack-Ice v2.3/v2.4 Raw Data');
  if CheckString(Buffer, Size, 0, 'TSM!') then
    Exit('Pack-Ice v2.4 (TSM!) Raw Data');
  if CheckLong(Buffer, Size, 70, $610000A0) then
    Exit('Pack-Ice v1.1 Executable Packer');
  if CheckString(Buffer, Size, 452, 'Ice!') then
    Exit('Pack-Ice v2.2 Executable Packer');
  if CheckString(Buffer, Size, 448, 'ICE!') then
    Exit('Pack-Ice v2.3 Executable Packer');
  if CheckString(Buffer, Size, 442, 'ICE!') then
    Exit('Pack-Ice v2.4 Executable Packer');
  if CheckLong(Buffer, Size, 28, $41FA0220) then
    Exit('Superior PE Executable Packer');

  // 11. IMP Packer
  if CheckString(Buffer, Size, 0, 'IMP!') then
    Exit('IMP Packer Raw Data');

  // 12. Ivory Dragon Packer
  if CheckLastLongString(Buffer, Size, 'PAWN') then
    Exit('Ivory Dragon Packer Raw Data');
  if CheckLong(Buffer, Size, 96, $610000B2) then
    Exit('Ivory Dragon Packer Executable');

  // 13. FIRE Packer
  if (Size >= 40) and (FindEmbeddedSig(Buffer, Min(Size, 256), 'Pack-Fire') >= 0) then
    Exit('FIRE Packer v2.0 Executable');
  if CheckLastLongString(Buffer, Size, 'Fire') then
    Exit('FIRE Packer v1.0 Raw Data');
  if CheckString(Buffer, Size, 0, 'FIRE') then
    Exit('FIRE Packer v2.0 Raw Data');
  if CheckString(Buffer, Size, 36, 'ire ') then
    Exit('FIRE Packer v1.0 Executable');
  if CheckString(Buffer, Size, 36, 'ire!') then
    Exit('FIRE Packer v2.0 Executable');

  // 14. J Packer
  if CheckString(Buffer, Size, 576, 'cker') then
    Exit('J Packer Executable');

  // 15. JAM Packer
  if CheckString(Buffer, Size, 0, 'LZH!') then
    Exit('JAM LZH Raw Data');
  if CheckString(Buffer, Size, 0, 'LZW!') then
    Exit('JAM LZW Raw Data');
  if CheckString(Buffer, Size, 672, 'JAM ') or CheckString(Buffer, Size, 674, 'JAM ') or CheckString(Buffer, Size, 706, 'JAM ') then
    Exit('JAM Packer v1 Executable');
  if CheckString(Buffer, Size, 1170, 'LSD!') then
    Exit('JAM Packer v2 JAM3 Executable');
  if CheckString(Buffer, Size, 1170, 'LZH!') then
    Exit('JAM Packer LZH JAM3 Executable');
  if CheckString(Buffer, Size, 758, 'LSD!') then
    Exit('JAM Packer v2 JAM4 Executable');
  if CheckString(Buffer, Size, 1194, 'LZH!') then
    Exit('JAM Packer LZH JAM4 Executable');
  if CheckString(Buffer, Size, 544, 'LZW!') then
    Exit('JAM Packer LZW JAM4 Executable');
  if CheckString(Buffer, Size, 712, 'Ice!') then
    Exit('JAM Packer ICE JAM4 Executable');

  // Embedded JAM Packer check (before GEMDOS C startup matches JEK)
  for Idx := 0 to Min(Size - 12, 4096) do
  begin
    if Buffer[Idx] = Byte('L') then
    begin
      if CheckString(Buffer, Size, Idx, 'LSD!') then Exit('JAM Packer v2 JAM Executable');
      if CheckString(Buffer, Size, Idx, 'LZH!') then Exit('JAM Packer LZH Executable');
      if CheckString(Buffer, Size, Idx, 'LZW!') then Exit('JAM Packer LZW Executable');
    end;
  end;

  // 16. JEK Packer
  if CheckLong(Buffer, Size, 28, $2A6F0004) and CheckLong(Buffer, Size, 32, $226D0010) then
    Exit('JEK Packer / Byte Killer Executable');
  if CheckLastLongString(Buffer, Size, 'JEK!') then
    Exit('JEK Packer Raw Data');
  if CheckString(Buffer, Size, 638, 'JEK ') then
    Exit('JEK Packer v1.2d Executable');
  if CheckString(Buffer, Size, 646, 'JEK ') then
    Exit('JEK Packer v1.3d Executable');

  // 17. Le Crunch Packer
  if CheckString(Buffer, Size, 0, 'LeCr') then
    Exit('Le Crunch Packer Raw Data');

  // 18. LSD Packer
  if CheckLastLongString(Buffer, Size, 'END!') or CheckLastLongString(Buffer, Size, 'LSD!') then
    Exit('LSD Packer Raw Data');
  if CheckString(Buffer, Size, 638, 'PLEA') or CheckString(Buffer, Size, 638, 'PACK') then
    Exit('LSD Packer Executable');
  if CheckString(Buffer, Size, 650, '1.2 ') then
    Exit('LSD Packer v1.2 Executable');

  // 19. MPack Packer
  if CheckLong(Buffer, Size, 230, $4AC04440) then
    Exit('MPack Packer Type 1 Executable');
  if CheckLong(Buffer, Size, 228, $4AC04440) then
    Exit('MPack Packer Type 2 Executable');
  if CheckLong(Buffer, Size, 230, $7800284A) then
    Exit('MPack Packer Type 3 Executable');

  // 20. PA Packer
  if CheckLong(Buffer, Size, 102, $425D51CF) then
    Exit('PA Packer Executable');

  // 21. PFX Packer
  if CheckString(Buffer, Size, 422, '-lz5') then Exit('PFX Packer v1.13 Executable');
  if CheckString(Buffer, Size, 564, '-lz5') then Exit('PFX Packer v1.13/v1.6 Executable');
  if CheckString(Buffer, Size, 586, '-lz5') then Exit('PFX Packer v1.8/v2.1 Executable');
  if CheckString(Buffer, Size, 1052, '-lz5') then Exit('PFX Packer v2.1 (Anti-Virus) Executable');

  // 22. Pompey Packer
  if CheckLastLongString(Buffer, Size, 'POPI') or CheckLastLongString(Buffer, Size, 'PUFF') then
    Exit('Pompey Packer Raw Data');
  if CheckLong(Buffer, Size, 158, $00847604) then Exit('Pompey Packer v1.5 Executable');
  if CheckLong(Buffer, Size, 158, $60000084) then Exit('Pompey Packer v1.9 Executable');
  if CheckLong(Buffer, Size, 158, $340460F0) then Exit('Pompey Packer v1.9 Var 1 Executable');
  if CheckLong(Buffer, Size, 158, $3F3C000A) then Exit('Pompey Packer v1.9 Var 2 Executable');
  if CheckLong(Buffer, Size, 158, $76046022) then Exit('Pompey Packer v2.3 Executable');
  if CheckLong(Buffer, Size, 158, $65647201) then Exit('Pompey Packer v2.6 Executable');
  if CheckLong(Buffer, Size, 158, $60E67205) then Exit('Pompey Packer v3.0 Executable');

  // 23. Power Packer
  if CheckString(Buffer, Size, 0, 'PP20') then
    Exit('Power Packer v2.0 Raw Data');

  // 24. QPak Packer
  if CheckString(Buffer, Size, 4, '2-JM') then Exit('QPak Packer v2 Raw Data');
  if CheckString(Buffer, Size, 52, '2-JM') then Exit('QPak Packer v2 Executable');

  // 25. Rob Northen Compression (RNC)
  if CheckLong(Buffer, Size, 0, $524E4301) then Exit('Rob Northen Compression (Method 1) Raw Data');
  if CheckLong(Buffer, Size, 0, $524E4302) then Exit('Rob Northen Compression (Method 2) Raw Data');
  if CheckLong(Buffer, Size, 146, $0C54601A) then Exit('RNC Copylock Executable');
  if CheckLong(Buffer, Size, 642, $524E4302) then Exit('RNC2 Executable');

  // 26. Sentry Packer
  if CheckLastLongString(Buffer, Size, 'Snt2') then Exit('Sentry Packer v2.05 Raw Data');
  if CheckString(Buffer, Size, 522, '2.05') then Exit('Sentry Packer v2.05 Executable');
  if CheckString(Buffer, Size, 526, '2.11') then Exit('Sentry Packer v2.11 Executable');

  // 27. Speedpacker
  if CheckString(Buffer, Size, 0, 'SP20') then Exit('Speedpacker v2.0 Raw Data');
  if CheckString(Buffer, Size, 0, 'SPv3') then Exit('Speedpacker v3.0 Raw Data');
  if CheckString(Buffer, Size, 542, 'SP20') then Exit('Speedpacker v2.0 Executable');
  if CheckLong(Buffer, Size, 28, $4E417633) then Exit('Speedpacker v3.0 Var 1 Executable');
  if CheckString(Buffer, Size, 1816, 'SPv3') then Exit('Speedpacker v3.0 Var 2 Executable');
  if CheckString(Buffer, Size, 30, 'SPv3') then Exit('Speedpacker v3.0 Var 3 Executable');

  // 28. STOS Packer
  if CheckLong(Buffer, Size, 68, $0007FD00) then Exit('STOS Executable Packer');

  // 29. Thunder Packer
  if CheckString(Buffer, Size, 422, 'ATOM') then Exit('Thunder V1 Executable Packer');
  if CheckString(Buffer, Size, 480, 'ATOM') then Exit('Thunder V1.1 Executable Packer');
  if CheckString(Buffer, Size, 452, 'ATOM') then Exit('Thunder V2 Executable Packer');

  // 30. Vic2 Packer
  if CheckString(Buffer, Size, 0, 'Vic2') then Exit('Vic2 Packer Raw Data');

  // 31. Degas Elite (Graphics)
  if CheckWord(Buffer, Size, 0, $8000) or CheckWord(Buffer, Size, 0, $8001) or CheckWord(Buffer, Size, 0, $8002) then
    Exit('Degas Elite Compressed Image');

  // 32. STOS Picture Compactor
  if CheckLong(Buffer, Size, 0, $06071963) then
    Exit('STOS Picture Compactor');

  // 33. STAD Monochrome Picture
  if CheckString(Buffer, Size, 0, 'pM85') or CheckString(Buffer, Size, 0, 'pM86') then
    Exit('STAD Monochrome Picture');

  // 34. STOS Squashed Data / Picture
  if (Size >= 16) and (ReadBE32(Buffer, Size - 4) > 0) and (ReadBE32(Buffer, Size - 4) <= 2097152) then
  begin
    if DecompressStosSquash(Buffer, Size, DummyOut) then
    begin
      if (Length(DummyOut) >= 4) and (ReadBE32(@DummyOut[0], 0) = $06071963) then
        Exit('STOS Squashed Picture')
      else
        Exit('STOS Squashed Data');
    end;
  end;

  // 32. Heuristics fallback scanning on first 8192 bytes
  AsciiContext := BufferToAscii(Buffer, Size, 8192);

  if (Pos('DPAK', AsciiContext) > 0) or (Pos('NEW_DPAK', AsciiContext) > 0) or (Pos('XERUD', AsciiContext) > 0) then
    Exit('Mike Watson (Xerud) NEW_DPAK');
  if (Pos('JAM Packer', AsciiContext) > 0) or (Pos('The JAM Packer', AsciiContext) > 0) or (Pos('JAM PACKER', AsciiContext) > 0) then
    Exit('The JAM Packer Executable');
  if (Pos('ATOMIC', AsciiContext) > 0) or (Pos('THUNDER', AsciiContext) > 0) then
    Exit('JPM Thunder Packer / ATOMIC');

  if (Pos('ATM5', AsciiContext) > 0) or (Pos('ATM3', AsciiContext) > 0) or
     (Pos('ATOMIK CRUNCHER', AsciiContext) > 0) or (Pos('ATOM', AsciiContext) > 0) then
  begin
    Idx := Pos('ATOM', AsciiContext);
    if (Idx > 0) and (Idx - 1 + 12 <= Size) then
    begin
      ULen := ReadBE32(Buffer, Idx - 1 + 4);
      PLen := ReadBE32(Buffer, Idx - 1 + 8);
      if (ULen > 0) and (ULen < 16777216) and (PLen > 0) and (Idx - 1 + PLen <= Size) then
      begin
        AbsEnd := Idx - 1 + PLen;
        AbsLitOff := AbsEnd - 4;
        if (AbsLitOff >= Idx - 1 + 12) and (AbsLitOff + 4 <= Size) then
        begin
          LitLen := ReadBE32(Buffer, AbsLitOff);
          AbsLitStart := AbsLitOff - LitLen;
          if (AbsLitStart >= Idx - 1 + 12) and (AbsLitStart <= AbsEnd) then
            Exit('Thunder V2 Executable Packer');
        end;
        if DecompressThunderV1(@Buffer[Idx - 1], Size - (Idx - 1), DummyOut) then
          Exit('Thunder V1 Executable Packer');
        if DecompressAtomik(@Buffer[Idx - 1], Size - (Idx - 1), DummyOut) then
          Exit('Atomik Cruncher Executable Packer');
      end;
    end
    else if (Pos('ATM5', AsciiContext) > 0) or (Pos('ATM3', AsciiContext) > 0) or
            (Pos('ATOMIK CRUNCHER', AsciiContext) > 0) then
    begin
      Exit('Atomik Cruncher Executable Packer');
    end;
  end;

  if Pos('AUTOMATION PACKER V2.3', AsciiContext) > 0 then Exit('Automation 2.3 Packer');
  if (Pos('AUTOMATION PACKER', AsciiContext) > 0) or (Pos('AUTOMATION COMPACTER', AsciiContext) > 0) then
    Exit('Automation Compacter');
  if (Pos('POMPEY PIRATES', AsciiContext) > 0) then Exit('Pompey Pirates Packer');
  if (Pos('JEK PACKER', AsciiContext) > 0) or (Pos('JEK!', AsciiContext) > 0) then Exit('JEK Packer / Byte Killer');

  // Tail-end scanning fallback
  if Size > 1024 then
    TailAscii := BufferToAscii(@Buffer[Size - 1024], 1024, 1024)
  else
    TailAscii := AsciiContext;

  if Pos('JPM!', TailAscii) > 0 then Exit('Bytekiller (JPM Data) Raw Data');
  if Pos('GUS*', TailAscii) > 0 then Exit('Gremlin Packer Raw Data');
  if (Pos('JEK!', TailAscii) > 0) or (Pos('JEK PACKER', TailAscii) > 0) then Exit('JEK Packer / Byte Killer');

  // Uncompressed files have no packer signature
  Result := 'None';
end;

function DetectPackerSignature(const Bytes: TBytes): string;
begin
  if Length(Bytes) = 0 then Exit('None');
  Result := DetectPackerSignature(@Bytes[0], Length(Bytes));
end;

function DetectPackerSignature(Stream: TStream): string;
var
  Buf: TBytes;
  SavedPos, BytesToRead: Int64;
begin
  if (Stream = nil) or (Stream.Size = 0) then Exit('None');
  SavedPos := Stream.Position;
  try
    Stream.Position := 0;
    BytesToRead := Min(Stream.Size, 16384);
    SetLength(Buf, BytesToRead);
    Stream.ReadBuffer(Buf[0], BytesToRead);
    Result := DetectPackerSignature(@Buf[0], Length(Buf));
  finally
    Stream.Position := SavedPos;
  end;
end;

function IsPackerSupported(const PackerName: string): Boolean;
var
  P: string;
begin
  Result := False;
  if (PackerName = '') or (PackerName = 'None') then Exit(False);
  P := UpperCase(PackerName);
  if Pos('UNSUPPORTED', P) > 0 then Exit(False);
  if Pos('ATARI ST EXECUTABLE', P) > 0 then Exit(False);
  if Pos('UNKNOWN', P) > 0 then Exit(False);

  if (Pos('PACK-ICE', P) > 0) or (Pos('ICE PACKER', P) > 0) or
     (Pos('ATOMIK', P) > 0) or
     (Pos('AUTOMATION', P) > 0) or
     (Pos('JAM', P) > 0) or
     (Pos('ROB NORTHEN', P) > 0) or (Pos('RNC', P) > 0) or
     (Pos('THUNDER', P) > 0) or
     (Pos('JEK', P) > 0) or (Pos('BYTEKILLER', P) > 0) or (Pos('BYTE KILLER', P) > 0) or
     (Pos('DEGAS', P) > 0) or
     (Pos('FIRE', P) > 0) or
     (Pos('STOS', P) > 0) or (Pos('STAD', P) > 0) or (Pos('TINY', P) > 0) or (Pos('SQUASH', P) > 0) or
     (Pos('COMPACTOR', P) > 0) then
    Result := True;
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// Backward Stream, Bit Reader, VLC and Pack-Ice Decompressor
// -----------------------------------------------------------------------------

type
  TBackwardInputStream = class
  private
    FBuffer: PByte;
    FCurrentOffset: Integer;
    FEndOffset: Integer;
  public
    constructor Create(ABuffer: PByte; AStartOffset, AEndOffset: Integer);
    function Eof: Boolean; inline;
    function ReadByte: Byte; inline;
    function ReadBE16: Word;
    function ReadBE32: LongWord;
  end;

constructor TBackwardInputStream.Create(ABuffer: PByte; AStartOffset, AEndOffset: Integer);
begin
  inherited Create;
  FBuffer := ABuffer;
  FCurrentOffset := AEndOffset;
  FEndOffset := AStartOffset;
end;

function TBackwardInputStream.Eof: Boolean;
begin
  Result := FCurrentOffset <= FEndOffset;
end;

function TBackwardInputStream.ReadByte: Byte;
begin
  if FCurrentOffset <= FEndOffset then
    raise Exception.Create('BackwardInputStream EOF');
  Dec(FCurrentOffset);
  Result := FBuffer[FCurrentOffset];
end;

function TBackwardInputStream.ReadBE16: Word;
var
  B0, B1: Byte;
begin
  B0 := ReadByte;
  B1 := ReadByte;
  Result := (Word(B1) shl 8) or B0;
end;

function TBackwardInputStream.ReadBE32: LongWord;
var
  B0, B1, B2, B3: Byte;
begin
  B0 := ReadByte;
  B1 := ReadByte;
  B2 := ReadByte;
  B3 := ReadByte;
  Result := (LongWord(B3) shl 24) or (LongWord(B2) shl 16) or (LongWord(B1) shl 8) or B0;
end;

type
  TBackwardOutputStream = class
  private
    FBuffer: PByte;
    FStartOffset: Integer;
    FCurrentOffset: Integer;
    FEndOffset: Integer;
  public
    constructor Create(ABuffer: PByte; AStartOffset, AEndOffset: Integer);
    function Eof: Boolean; inline;
    procedure WriteByte(Val: Byte); inline;
    procedure Copy(Distance, Count: Integer; DefaultChar: Integer = -1);
  end;

constructor TBackwardOutputStream.Create(ABuffer: PByte; AStartOffset, AEndOffset: Integer);
begin
  inherited Create;
  FBuffer := ABuffer;
  FStartOffset := AStartOffset;
  FCurrentOffset := AEndOffset;
  FEndOffset := AEndOffset;
end;

function TBackwardOutputStream.Eof: Boolean;
begin
  Result := FCurrentOffset <= FStartOffset;
end;

procedure TBackwardOutputStream.WriteByte(Val: Byte);
begin
  if FCurrentOffset <= FStartOffset then
    raise Exception.Create('BackwardOutputStream overflow');
  Dec(FCurrentOffset);
  FBuffer[FCurrentOffset] := Val;
end;

procedure TBackwardOutputStream.Copy(Distance, Count: Integer; DefaultChar: Integer);
var
  I, PrevCount: Integer;
begin
  if Distance <= 0 then raise Exception.Create('Invalid distance');
  if FStartOffset + Count > FCurrentOffset then raise Exception.Create('Copy count overflow');

  if DefaultChar >= 0 then
  begin
    PrevCount := 0;
    if FCurrentOffset + Distance > FEndOffset then
    begin
      PrevCount := Min(Count, FCurrentOffset + Distance - FEndOffset);
      for I := 1 to PrevCount do
      begin
        Dec(FCurrentOffset);
        FBuffer[FCurrentOffset] := Byte(DefaultChar);
      end;
    end;
    for I := PrevCount + 1 to Count do
    begin
      Dec(FCurrentOffset);
      FBuffer[FCurrentOffset] := FBuffer[FCurrentOffset + Distance];
    end;
  end
  else
  begin
    if FCurrentOffset + Distance > FEndOffset then raise Exception.Create('Copy distance overflow');
    for I := 1 to Count do
    begin
      Dec(FCurrentOffset);
      FBuffer[FCurrentOffset] := FBuffer[FCurrentOffset + Distance];
    end;
  end;
end;

type
  TMSBBitReader = class
  private
    FStream: TBackwardInputStream;
    FBufContent: LongWord;
    FBufLength: Integer;
  public
    constructor Create(AStream: TBackwardInputStream);
    procedure Reset(ABufContent: LongWord = 0; ABufLength: Integer = 0);
    function ReadBits(Count: Integer): LongWord;
    function ReadBitsBE32(Count: Integer): LongWord;
  end;

constructor TMSBBitReader.Create(AStream: TBackwardInputStream);
begin
  inherited Create;
  FStream := AStream;
  FBufContent := 0;
  FBufLength := 0;
end;

procedure TMSBBitReader.Reset(ABufContent: LongWord; ABufLength: Integer);
begin
  FBufContent := ABufContent;
  FBufLength := ABufLength;
end;

function TMSBBitReader.ReadBits(Count: Integer): LongWord;
var
  Ret: LongWord;
  MaxCount: Integer;
begin
  Ret := 0;
  while Count > 0 do
  begin
    if FBufLength = 0 then
    begin
      FBufContent := FStream.ReadByte;
      FBufLength := 8;
    end;
    MaxCount := Min(Count, FBufLength);
    Dec(FBufLength, MaxCount);
    Ret := (Ret shl MaxCount) or ((FBufContent shr FBufLength) and ((LongWord(1) shl MaxCount) - 1));
    Dec(Count, MaxCount);
  end;
  Result := Ret;
end;

function TMSBBitReader.ReadBitsBE32(Count: Integer): LongWord;
var
  Ret: LongWord;
  MaxCount: Integer;
begin
  Ret := 0;
  while Count > 0 do
  begin
    if FBufLength = 0 then
    begin
      FBufContent := FStream.ReadBE32;
      FBufLength := 32;
    end;
    MaxCount := Min(Count, FBufLength);
    Dec(FBufLength, MaxCount);
    Ret := (Ret shl MaxCount) or ((FBufContent shr FBufLength) and ((LongWord(1) shl MaxCount) - 1));
    Dec(Count, MaxCount);
  end;
  Result := Ret;
end;

type
  TVLC = class
  public
    BitLengths: array of Integer;
    Offsets: array of Integer;
    constructor Create(const Args: array of Integer);
    function Decode(ReadBitsFn: TMSBBitReader; Base: Integer): Integer;
    function DecodeCascade(ReadBitsFn: TMSBBitReader): Integer;
    function DecodeBE32(ReadBitsFn: TMSBBitReader; Base: Integer): Integer;
    function DecodeCascadeBE32(ReadBitsFn: TMSBBitReader): Integer;
  end;

constructor TVLC.Create(const Args: array of Integer);
var
  I, L, Val: Integer;
begin
  inherited Create;
  SetLength(BitLengths, Length(Args));
  SetLength(Offsets, Length(Args));
  L := 0;
  for I := 0 to High(Args) do
  begin
    Val := Args[I];
    if Val < 0 then
    begin
      BitLengths[I] := -Val;
      Offsets[I] := 0;
      L := 1 shl (-Val);
    end
    else
    begin
      BitLengths[I] := Val;
      Offsets[I] := L;
      L := L + (1 shl Val);
    end;
  end;
end;

function TVLC.Decode(ReadBitsFn: TMSBBitReader; Base: Integer): Integer;
begin
  Result := Offsets[Base] + Integer(ReadBitsFn.ReadBits(BitLengths[Base]));
end;

function TVLC.DecodeCascade(ReadBitsFn: TMSBBitReader): Integer;
var
  I, N, Len, Tmp: Integer;
begin
  N := Length(BitLengths);
  for I := 0 to N - 1 do
  begin
    Len := BitLengths[I];
    Tmp := Integer(ReadBitsFn.ReadBits(Len));
    if (I = N - 1) or (Tmp <> (1 shl Len) - 1) then
      Exit(Offsets[I] - I + Tmp);
  end;
  raise Exception.Create('VLC Cascade Error');
end;

function TVLC.DecodeBE32(ReadBitsFn: TMSBBitReader; Base: Integer): Integer;
begin
  Result := Offsets[Base] + Integer(ReadBitsFn.ReadBitsBE32(BitLengths[Base]));
end;

function TVLC.DecodeCascadeBE32(ReadBitsFn: TMSBBitReader): Integer;
var
  I, N, Len, Tmp: Integer;
begin
  N := Length(BitLengths);
  for I := 0 to N - 1 do
  begin
    Len := BitLengths[I];
    Tmp := Integer(ReadBitsFn.ReadBitsBE32(Len));
    if (I = N - 1) or (Tmp <> (1 shl Len) - 1) then
      Exit(Offsets[I] - I + Tmp);
  end;
  raise Exception.Create('VLC Cascade Error');
end;


function DecompressPackIceStream(const PackedData: PByte; StartOffset, PackedSize, RawSize, InitialSrcIdx: Integer; IsOld, UseBytes: Boolean; out OutBuf: TBytes): Boolean;
var
  InStream: TBackwardInputStream;
  OutStream: TBackwardOutputStream;
  BitReader: TMSBBitReader;
  LitVlc, CountBaseVlc, CountVlc, DistBaseVlc, DistVlc: TVLC;
  Val, Tmp: LongWord;
  Count, LitLength, CountBase, CountVal, DistBase, Distance: Integer;
begin
  Result := False;
  SetLength(OutBuf, 0);
  try
    InStream := TBackwardInputStream.Create(PackedData, StartOffset, InitialSrcIdx);
    BitReader := TMSBBitReader.Create(InStream);
    try
      if UseBytes then
        Val := InStream.ReadByte
      else
        Val := InStream.ReadBE32;

      Tmp := Val;
      Count := 0;
      while Tmp <> 0 do
      begin
        Tmp := (Tmp shl 1) and $FFFFFFFF;
        Inc(Count);
      end;
      if Count > 0 then Dec(Count);
      if Count > 0 then
      begin
        if UseBytes then
          BitReader.Reset(Val shr (32 - Count), Count - 24)
        else
          BitReader.Reset(Val shr (32 - Count), Count);
      end;

      SetLength(OutBuf, RawSize);
      OutStream := TBackwardOutputStream.Create(@OutBuf[0], 0, RawSize);
      try
        if IsOld then
          LitVlc := TVLC.Create([1, 2, 2, 3, 10])
        else
          LitVlc := TVLC.Create([1, 2, 2, 3, 8, 15]);

        CountBaseVlc := TVLC.Create([1, 1, 1, 1]);
        CountVlc := TVLC.Create([0, 0, 1, 2, 10]);
        DistBaseVlc := TVLC.Create([1, 1]);
        DistVlc := TVLC.Create([5, 8, 12]);
        try
          while True do
          begin
            if ((UseBytes and (BitReader.ReadBits(1) = 1)) or ((not UseBytes) and (BitReader.ReadBitsBE32(1) = 1))) then
            begin
              if UseBytes then
                LitLength := LitVlc.DecodeCascade(BitReader) + 1
              else
                LitLength := LitVlc.DecodeCascadeBE32(BitReader) + 1;

              for Count := 1 to LitLength do
                OutStream.WriteByte(InStream.ReadByte);
            end;
            if OutStream.Eof then Break;

            if UseBytes then
            begin
              CountBase := CountBaseVlc.DecodeCascade(BitReader);
              CountVal := CountVlc.Decode(BitReader, CountBase) + 2;
              if CountVal = 2 then
              begin
                if BitReader.ReadBits(1) = 1 then
                  Distance := Integer(BitReader.ReadBits(9)) + $40
                else
                  Distance := Integer(BitReader.ReadBits(6));
                Inc(Distance, CountVal - 1);
              end
              else
              begin
                DistBase := DistBaseVlc.DecodeCascade(BitReader);
                if DistBase < 2 then DistBase := DistBase xor 1;
                Distance := DistVlc.Decode(BitReader, DistBase);
                if Distance <> 0 then Inc(Distance, CountVal - 1) else Distance := 1;
              end;
            end
            else
            begin
              CountBase := CountBaseVlc.DecodeCascadeBE32(BitReader);
              CountVal := CountVlc.DecodeBE32(BitReader, CountBase) + 2;
              if CountVal = 2 then
              begin
                if BitReader.ReadBitsBE32(1) = 1 then
                  Distance := Integer(BitReader.ReadBitsBE32(9)) + $40
                else
                  Distance := Integer(BitReader.ReadBitsBE32(6));
                Inc(Distance, CountVal);
              end
              else
              begin
                DistBase := DistBaseVlc.DecodeCascadeBE32(BitReader);
                if DistBase < 2 then DistBase := DistBase xor 1;
                Distance := DistVlc.DecodeBE32(BitReader, DistBase);
                Inc(Distance, CountVal);
              end;
            end;

            OutStream.Copy(Distance, CountVal);
          end;
          Result := True;
        finally
          LitVlc.Free;
          CountBaseVlc.Free;
          CountVlc.Free;
          DistBaseVlc.Free;
          DistVlc.Free;
        end;
      finally
        OutStream.Free;
      end;
    finally
      BitReader.Free;
      InStream.Free;
    end;
  except
    Result := False;
  end;
end;

function IsValidOutput(const OutBuf: TBytes): Boolean;
begin
  Result := False;
  if Length(OutBuf) < 28 then Exit;
  if (OutBuf[0] = $60) and (OutBuf[1] in [$1A, $1C, $00]) then Exit(True);
  if (Length(OutBuf) = 32000) or (Length(OutBuf) = 32034) or (Length(OutBuf) = 32066) or (Length(OutBuf) = 32128) then Exit(True);
end;

function DecompressPackIce(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
const
  IceDeltas: array[0..4] of Integer = (0, -2, 2, -4, 4);
var
  SearchOffsets: array of Integer;
  PossibleEnds: array of Integer;
  TrialOrigSizes: array of Integer;
  Ts, Ds, Ss, PackedSize, OrigSize: LongWord;
  PrgEnd, Off, I, Delta, DIdx, SrcIdx, Op, Mode, ImmVal, EndIdx: Integer;
  IsIce21, UseBytes, IsOld: Boolean;
begin
  Result := False;
  SetLength(OutBuf, 0);
  if (InBuf = nil) or (InSize < 12) then Exit;

  // Phase 1: Raw ICE!, Ice! and TSM! signatures
  SetLength(SearchOffsets, 0);
  for Off := 0 to InSize - 12 do
  begin
    if (((InBuf[Off] = Byte('I')) and
        (((InBuf[Off+1] = Byte('C')) and (InBuf[Off+2] = Byte('E')) and (InBuf[Off+3] = Byte('!'))) or
         ((InBuf[Off+1] = Byte('c')) and (InBuf[Off+2] = Byte('e')) and (InBuf[Off+3] = Byte('!'))))) or
        ((InBuf[Off] = Byte('T')) and (InBuf[Off+1] = Byte('S')) and (InBuf[Off+2] = Byte('M')) and (InBuf[Off+3] = Byte('!')))) then
    begin
      SetLength(SearchOffsets, Length(SearchOffsets) + 1);
      SearchOffsets[High(SearchOffsets)] := Off;
    end;
  end;

  for I := 0 to High(SearchOffsets) do
  begin
    Off := SearchOffsets[I];
    IsIce21 := (InBuf[Off+1] = Byte('c'));
    PackedSize := ReadBE32(InBuf, Off + 4);
    OrigSize := ReadBE32(InBuf, Off + 8);
    if (PackedSize > 0) and (PackedSize <= LongWord(InSize)) and (OrigSize > 0) and (OrigSize <= $800000) then
    begin
      for DIdx := 0 to High(IceDeltas) do
      begin
        Delta := IceDeltas[DIdx];
        for IsOld := False to True do
          for UseBytes := False to True do
          begin
            // 1. Try bitstream ending at Off + PackedSize + Delta with data start at Off + 12
            EndIdx := Off + Integer(PackedSize) + Delta;
            if (EndIdx > Off + 12) and (EndIdx <= InSize) then
            begin
              if DecompressPackIceStream(InBuf, Off + 12, InSize, OrigSize, EndIdx, IsOld, UseBytes, OutBuf) then
              begin
                if (Off = 0) or IsValidOutput(OutBuf) then Exit(True);
              end;
            end;

            // 2. Try bitstream ending at Off + PackedSize + 12 + Delta with data start at Off
            EndIdx := Off + Integer(PackedSize) + 12 + Delta;
            if (EndIdx > Off) and (EndIdx <= InSize) then
            begin
              if DecompressPackIceStream(InBuf, Off, InSize, OrigSize, EndIdx, IsOld, UseBytes, OutBuf) then
              begin
                if (Off = 0) or IsValidOutput(OutBuf) then Exit(True);
              end;
            end;

            // 3. Try bitstream ending at Off + PackedSize + Delta with data start at Off
            EndIdx := Off + Integer(PackedSize) + Delta;
            if (EndIdx > Off) and (EndIdx <= InSize) then
            begin
              if DecompressPackIceStream(InBuf, Off, InSize, OrigSize, EndIdx, IsOld, UseBytes, OutBuf) then
              begin
                if (Off = 0) or IsValidOutput(OutBuf) then Exit(True);
              end;
            end;
          end;
      end;
    end;
  end;

  // Phase 2: PRG executables without raw ICE header
  if (InSize >= 28) and (InBuf[0] = $60) and (InBuf[1] in [$1A, $1C, $00]) then
  begin
    Ts := ReadBE32(InBuf, 2);
    Ds := ReadBE32(InBuf, 6);
    Ss := ReadBE32(InBuf, 14);

    SetLength(PossibleEnds, 0);
    if (Ts + Ds + Ss > 0) and (28 + Ts + Ds + Ss <= LongWord(InSize)) then
    begin
      SetLength(PossibleEnds, Length(PossibleEnds) + 1);
      PossibleEnds[High(PossibleEnds)] := 28 + Ts + Ds + Ss;
    end;
    if (Ts + Ds > 0) and (28 + Ts + Ds <= LongWord(InSize)) then
    begin
      SetLength(PossibleEnds, Length(PossibleEnds) + 1);
      PossibleEnds[High(PossibleEnds)] := 28 + Ts + Ds;
    end;
    if (Ts > 0) and (28 + Ts <= LongWord(InSize)) then
    begin
      SetLength(PossibleEnds, Length(PossibleEnds) + 1);
      PossibleEnds[High(PossibleEnds)] := 28 + Ts;
    end;
    SetLength(PossibleEnds, Length(PossibleEnds) + 1);
    PossibleEnds[High(PossibleEnds)] := InSize;

    // Scan assembly for immediate sizes
    SetLength(TrialOrigSizes, 0);
    for I := 28 to Min(InSize - 6, 4096) do
    begin
      Op := InBuf[I];
      Mode := InBuf[I + 1];
      if (Op >= $20) and (Op <= $2E) and ((Mode = $3C) or (Mode = $7C)) then
      begin
        ImmVal := Integer(ReadBE32(InBuf, I + 2));
        if (ImmVal > 1000) and (ImmVal <= $800000) then
        begin
          SetLength(TrialOrigSizes, Length(TrialOrigSizes) + 1);
          TrialOrigSizes[High(TrialOrigSizes)] := ImmVal;
        end;
      end;
    end;

    for EndIdx := 0 to High(PossibleEnds) do
    begin
      PrgEnd := PossibleEnds[EndIdx];
      if PrgEnd >= 8 then
      begin
        ImmVal := Integer(ReadBE32(InBuf, PrgEnd - 4));
        if (ImmVal > 1000) and (ImmVal <= $800000) then
        begin
          SetLength(TrialOrigSizes, Length(TrialOrigSizes) + 1);
          TrialOrigSizes[High(TrialOrigSizes)] := ImmVal;
        end;
        ImmVal := Integer(ReadBE32(InBuf, PrgEnd - 8));
        if (ImmVal > 1000) and (ImmVal <= $800000) then
        begin
          SetLength(TrialOrigSizes, Length(TrialOrigSizes) + 1);
          TrialOrigSizes[High(TrialOrigSizes)] := ImmVal;
        end;
        if PrgEnd >= 9 then
        begin
          ImmVal := Integer(ReadBE32(InBuf, PrgEnd - 5));
          if (ImmVal > 1000) and (ImmVal <= $800000) then
          begin
            SetLength(TrialOrigSizes, Length(TrialOrigSizes) + 1);
            TrialOrigSizes[High(TrialOrigSizes)] := ImmVal;
          end;
        end;
      end;

      for I := 0 to High(TrialOrigSizes) do
      begin
        OrigSize := TrialOrigSizes[I];
        if (OrigSize <= 1000) or (OrigSize > $800000) then Continue;

        // Try old/new, useBytes
        for IsOld := False to True do
          for UseBytes := False to True do
          begin
            // Sweep deltas near PrgEnd
            for Delta := -8 to 8 do
            begin
              SrcIdx := PrgEnd + Delta;
              if (SrcIdx >= 12) and (SrcIdx <= InSize) then
              begin
                if DecompressPackIceStream(InBuf, 0, InSize, OrigSize, SrcIdx, IsOld, UseBytes, OutBuf) and IsValidOutput(OutBuf) then
                  Exit(True);
              end;
            end;
          end;
      end;
    end;
  end;
end;


// Rob Northen Compression (RNC Propack Method 1 & 2)
// -----------------------------------------------------------------------------

function DecompressRNCMethod2(const Data: PByte; InSize: Integer; OutSize: Integer; out OutBuf: TBytes): Boolean;
var
  OutPos, InPos, BitBuf, BitsLeft: Integer;

  function GetBit: Byte;
  begin
    if BitsLeft = 0 then
    begin
      if InPos >= InSize then Exit(0);
      BitBuf := Data[InPos];
      Inc(InPos);
      BitsLeft := 8;
    end;
    Result := (BitBuf shr 7) and 1;
    BitBuf := (BitBuf shl 1) and $FF;
    Dec(BitsLeft);
  end;

  function GetByte: Byte;
  begin
    if InPos < InSize then
    begin
      Result := Data[InPos];
      Inc(InPos);
    end
    else
      Result := 0;
  end;

  function GetBits(N: Integer): LongWord;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 1 to N do
      Result := (Result shl 1) or GetBit;
  end;

  function GetOffset: Integer;
  var
    B1, B2: Byte;
  begin
    if GetBit = 0 then
    begin
      B1 := GetByte;
      B2 := GetByte;
      Result := ((B1 shl 8) or B2) + 1;
    end
    else
    begin
      B1 := GetByte;
      B2 := GetByte;
      Result := (((B1 shl 8) or B2) or $10000) + 1;
    end;
  end;

var
  Len, OffsetVal, I: Integer;
begin
  Result := False;
  SetLength(OutBuf, OutSize);
  OutPos := 0;
  InPos := 18;
  BitBuf := 0;
  BitsLeft := 0;

  GetBits(2); // Skip initial 2 bits

  while (OutPos < OutSize) and (InPos < InSize) do
  begin
    if GetBit = 0 then
    begin
      OutBuf[OutPos] := GetByte;
      Inc(OutPos);
    end
    else if GetBit = 0 then
    begin
      Len := 4 + GetBit;
      if GetBit = 1 then
      begin
        Len := (Len - 1) * 2 + GetBit;
        if Len = 9 then
        begin
          Len := (GetBits(4) + 3) * 4;
          for I := 1 to Len do
          begin
            if OutPos >= OutSize then Break;
            OutBuf[OutPos] := GetByte;
            Inc(OutPos);
          end;
          Continue;
        end;
      end;
      OffsetVal := GetOffset;
      for I := 1 to Len do
      begin
        if (OutPos >= OutSize) or (OutPos - OffsetVal < 0) then Break;
        OutBuf[OutPos] := OutBuf[OutPos - OffsetVal];
        Inc(OutPos);
      end;
    end
    else if GetBit = 0 then
    begin
      OffsetVal := GetByte + 1;
      Len := 2;
      for I := 1 to Len do
      begin
        if (OutPos >= OutSize) or (OutPos - OffsetVal < 0) then Break;
        OutBuf[OutPos] := OutBuf[OutPos - OffsetVal];
        Inc(OutPos);
      end;
    end
    else if GetBit = 0 then
    begin
      OffsetVal := GetOffset;
      Len := 3;
      for I := 1 to Len do
      begin
        if (OutPos >= OutSize) or (OutPos - OffsetVal < 0) then Break;
        OutBuf[OutPos] := OutBuf[OutPos - OffsetVal];
        Inc(OutPos);
      end;
    end
    else if GetBit = 0 then
    begin
      Len := GetByte + 8;
      if Len = 8 then
      begin
        GetBit;
        Continue;
      end;
      OffsetVal := GetOffset;
      for I := 1 to Len do
      begin
        if (OutPos >= OutSize) or (OutPos - OffsetVal < 0) then Break;
        OutBuf[OutPos] := OutBuf[OutPos - OffsetVal];
        Inc(OutPos);
      end;
    end
    else
    begin
      Len := GetByte + 8;
      OffsetVal := GetOffset;
      for I := 1 to Len do
      begin
        if (OutPos >= OutSize) or (OutPos - OffsetVal < 0) then Break;
        OutBuf[OutPos] := OutBuf[OutPos - OffsetVal];
        Inc(OutPos);
      end;
    end;
  end;

  Result := (OutPos = OutSize) and (OutSize > 0);
end;

type
  TRNCHufEntry = record
    Depth: Byte;
    Code: Word;
  end;
  TRNCHufTable = array[0..15] of TRNCHufEntry;

function Ror16(Key: Word): Word; inline;
begin
  if (Key and 1) <> 0 then
    Result := $8000 or (Key shr 1)
  else
    Result := Key shr 1;
end;

function InverseBits(Value: LongWord; Count: Integer): Word; inline;
var
  Res: Word;
  I: Integer;
begin
  Res := 0;
  for I := 1 to Count do
  begin
    Res := Res shl 1;
    if (Value and 1) <> 0 then
      Res := Res or 1;
    Value := Value shr 1;
  end;
  Result := Res;
end;

function CalculateRNCCRC16(const Buffer: PByte; ALength: Integer): Word;
var
  CRC: Word;
  I, J: Integer;
  B: Byte;
begin
  CRC := 0;
  for I := 0 to ALength - 1 do
  begin
    B := Buffer[I];
    CRC := CRC xor B;
    for J := 0 to 7 do
    begin
      if (CRC and 1) <> 0 then
        CRC := (CRC shr 1) xor $A001
      else
        CRC := CRC shr 1;
    end;
  end;
  Result := CRC;
end;

function ExecuteRNC1Unpack(const Data: PByte; InSize: Integer; OutSize: Integer; AKey: Word; out OutBuf: TBytes): Boolean;
var
  InPos, InEnd, OutPos, Processed, LoopGuard: Integer;
  BitBuf: LongWord;
  BitCount: Integer;
  CurKey, InitialKey: Word;
  RawTable, LenTable, PosTable: TRNCHufTable;
  Subchunks, SC, DataLen, MatchOffset, MatchCount, I: Integer;
  Hi, Mid, Lo: Byte;
  B: Byte;

  function Peek(Offset: Integer): Byte; inline;
  var
    Idx: Integer;
  begin
    Idx := InPos + Offset;
    if (Idx >= 0) and (Idx < InEnd) then
      Result := Data[Idx]
    else
      Result := 0;
  end;

  function ReadByte: Byte; inline;
  begin
    if InPos < InEnd then
    begin
      Result := Data[InPos];
      Inc(InPos);
    end
    else
    begin
      Inc(InPos); // Always advance to guarantee forward progress and avoid EOF hang
      Result := 0;
    end;
  end;

  function ReadM1TokenByte: Byte; inline;
  begin
    Result := Peek(0);
    Inc(InPos); // Always advance to guarantee forward progress and avoid EOF hang
  end;

  function ReadBitsM1(Count: Integer): LongWord;
  var
    Bits, PrevBits: LongWord;
    B1, B2, LoByte, HiByte: Byte;
    I: Integer;
  begin
    Bits := 0;
    PrevBits := 1;
    for I := 1 to Count do
    begin
      if BitCount = 0 then
      begin
        B1 := ReadM1TokenByte;
        B2 := ReadM1TokenByte;
        LoByte := Peek(0);
        HiByte := Peek(1);
        BitBuf := (LongWord(HiByte) shl 24) or (LongWord(LoByte) shl 16) or (LongWord(B2) shl 8) or LongWord(B1);
        BitCount := 16;
      end;

      if (BitBuf and 1) <> 0 then
        Bits := Bits or PrevBits;

      BitBuf := BitBuf shr 1;
      PrevBits := PrevBits shl 1;
      Dec(BitCount);
    end;
    Result := Bits;
  end;

  procedure MakeHufTable(out Table: TRNCHufTable);
  var
    LeafNodes, I, BitsCount: Integer;
    BitDepths: array[0..15] of Byte;
    Val, DivVal: LongWord;
  begin
    FillChar(Table, SizeOf(Table), 0);
    FillChar(BitDepths, SizeOf(BitDepths), 0);
    LeafNodes := ReadBitsM1(5);
    if LeafNodes = 0 then Exit;
    if LeafNodes > 16 then LeafNodes := 16;

    for I := 0 to LeafNodes - 1 do
      BitDepths[I] := ReadBitsM1(4);

    Val := 0;
    DivVal := $80000000;
    for BitsCount := 1 to 16 do
    begin
      for I := 0 to LeafNodes - 1 do
      begin
        if BitDepths[I] = BitsCount then
        begin
          Table[I].Depth := BitsCount;
          Table[I].Code := InverseBits(Val div DivVal, BitsCount);
          Inc(Val, DivVal);
        end;
      end;
      DivVal := DivVal shr 1;
    end;
  end;

  function DecodeTableData(const Table: TRNCHufTable; out DecVal: Integer): Boolean;
  var
    I: Integer;
    Mask: LongWord;
  begin
    for I := 0 to 15 do
    begin
      if Table[I].Depth > 0 then
      begin
        Mask := (LongWord(1) shl Table[I].Depth) - 1;
        if Table[I].Code = (BitBuf and Mask) then
        begin
          ReadBitsM1(Table[I].Depth);
          if I < 2 then
            DecVal := I
          else
            DecVal := Integer(ReadBitsM1(I - 1)) or (1 shl (I - 1));
          Exit(True);
        end;
      end;
    end;
    DecVal := 0;
    Result := False; // EOF or invalid symbol
  end;

begin
  Result := False;
  SetLength(OutBuf, OutSize);
  OutPos := 0;
  Processed := 0;
  InPos := 18; // Skip 18-byte RNC header
  InEnd := InSize;
  BitBuf := 0;
  BitCount := 0;
  InitialKey := AKey;
  CurKey := AKey;
  LoopGuard := 0;

  // Skip 2 bits (bit 0 = lock flag, bit 1 = encryption flag)
  ReadBitsM1(2);

  while (Processed < OutSize) and (InPos < InEnd + 16) do
  begin
    Inc(LoopGuard);
    if LoopGuard > 20000 then Break;

    CurKey := InitialKey;
    MakeHufTable(RawTable);
    MakeHufTable(LenTable);
    MakeHufTable(PosTable);

    Subchunks := ReadBitsM1(16);
    if Subchunks <= 0 then Break;

    for SC := 0 to Subchunks - 1 do
    begin
      if not DecodeTableData(RawTable, DataLen) then Exit(False);
      Inc(Processed, DataLen);

      if DataLen > 0 then
      begin
        for I := 1 to DataLen do
        begin
          B := ReadByte;
          if OutPos < OutSize then
          begin
            OutBuf[OutPos] := (CurKey xor B) and $FF;
            Inc(OutPos);
          end;
        end;

        CurKey := Ror16(CurKey);

        // Reload lookahead into bit buffer after reading raw literal bytes
        Hi := Peek(2);
        Mid := Peek(1);
        Lo := Peek(0);
        BitBuf := (((LongWord(Hi) shl 16) or (LongWord(Mid) shl 8) or LongWord(Lo)) shl BitCount) or
                  (BitBuf and ((LongWord(1) shl BitCount) - 1));
      end;

      if SC < Subchunks - 1 then
      begin
        if not DecodeTableData(LenTable, MatchOffset) then Exit(False);
        Inc(MatchOffset, 1);

        if not DecodeTableData(PosTable, MatchCount) then Exit(False);
        Inc(MatchCount, 2);

        Inc(Processed, MatchCount);

        for I := 1 to MatchCount do
        begin
          if (OutPos < OutSize) and (OutPos - MatchOffset >= 0) then
          begin
            OutBuf[OutPos] := OutBuf[OutPos - MatchOffset];
            Inc(OutPos);
          end
          else
            Break;
        end;
      end;

      if Processed >= OutSize then Break;
    end;
  end;

  Result := (OutPos = OutSize);
end;

function DecompressRNCMethod1(const Data: PByte; InSize: Integer; OutSize: Integer; out OutBuf: TBytes): Boolean;
var
  ExpectedCRC: Word;
  EncFlag: Byte;
begin
  Result := False;
  SetLength(OutBuf, 0);
  if (Data = nil) or (InSize < 18) or (OutSize <= 0) then Exit(False);

  ExpectedCRC := (Word(Data[12]) shl 8) or Data[13];
  EncFlag := (Data[18] shr 1) and 1; // Bit 1 of byte 18

  // If encryption flag is set, try standard Atari ST key $04D2 (1234) first
  if EncFlag <> 0 then
  begin
    if ExecuteRNC1Unpack(Data, InSize, OutSize, $04D2, OutBuf) then
    begin
      if (ExpectedCRC = 0) or (CalculateRNCCRC16(@OutBuf[0], OutSize) = ExpectedCRC) then
        Exit(True);
    end;
  end;

  // Try unencrypted / key 0
  if ExecuteRNC1Unpack(Data, InSize, OutSize, 0, OutBuf) then
  begin
    if (ExpectedCRC = 0) or (CalculateRNCCRC16(@OutBuf[0], OutSize) = ExpectedCRC) then
      Exit(True);
  end;

  // Fallback: If enc flag is set, try $04D2 even if CRC didn't match exactly
  if EncFlag <> 0 then
  begin
    if ExecuteRNC1Unpack(Data, InSize, OutSize, $04D2, OutBuf) then
      Exit(True);
  end;

  Result := (Length(OutBuf) = OutSize) and (OutSize > 0);
end;

function DecompressRNC(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
var
  RNCStart, Method: Integer;
  OutSize: LongWord;
  ExpectedCRC: Word;
begin
  Result := False;
  SetLength(OutBuf, 0);
  if (InBuf = nil) or (InSize < 18) then Exit;

  RNCStart := FindEmbeddedSig(InBuf, InSize, 'RNC'#1);
  if RNCStart < 0 then
    RNCStart := FindEmbeddedSig(InBuf, InSize, 'RNC'#2);
  if (RNCStart < 0) or (RNCStart + 18 > InSize) then Exit;

  Method := InBuf[RNCStart + 3];
  OutSize := ReadBE32(InBuf, RNCStart + 4);
  if (OutSize = 0) or (OutSize > 16 * 1024 * 1024) then Exit;

  ExpectedCRC := (Word(InBuf[RNCStart + 12]) shl 8) or InBuf[RNCStart + 13];

  if Method = 2 then
    Result := DecompressRNCMethod2(@InBuf[RNCStart], InSize - RNCStart, OutSize, OutBuf)
  else if Method = 1 then
    Result := DecompressRNCMethod1(@InBuf[RNCStart], InSize - RNCStart, OutSize, OutBuf);

  if Result and (ExpectedCRC <> 0) and (Length(OutBuf) = OutSize) then
  begin
    if CalculateRNCCRC16(@OutBuf[0], OutSize) <> ExpectedCRC then
    begin
      SetLength(OutBuf, 0);
      Result := False;
    end;
  end;
end;

// -----------------------------------------------------------------------------
// Atomik Cruncher v3.1 & v3.5
// -----------------------------------------------------------------------------

function DecompressAtomik(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
var
  OrigSize, PackedSize, DataOffset: LongWord;
  AtmPos, SearchIdx, K, I, J: Integer;
  IsPRG, IsV31: Boolean;
  Sig: LongWord;
  DstIdx, SrcIdx, BlockSize, BlockTarget, LengthBits, MatchOffset: Integer;
  OffsetIdx, LenVal, MatchLen, QIdx: Integer;
  PicCnt, Cmd, Mask, SubVal: Byte;
  SpecialOffsetTabFull, ShortCharTabFull: Boolean;
  SpecialOffset: Byte;
  SpecialOffsetTab: array[0..6] of Integer;
  ShortCharTab: array[0..15] of Byte;
  BitTable, OffsetTable: array[0..7] of Integer;
  Plane0, Plane1, Plane2, Plane3, DataByte: Word;
  PicOffset, P: Integer;

  function ReadByte: Byte;
  begin
    if SrcIdx <= DataOffset then Exit(0);
    Dec(SrcIdx);
    Result := InBuf[SrcIdx];
  end;

  function GetBits(BitLen: Integer): Integer;
  var
    Tmp: Integer;
  begin
    Tmp := 0;
    while BitLen > 0 do
    begin
      Tmp := (Tmp shl 1);
      Mask := Mask shr 1;
      if Mask = 0 then
      begin
        Cmd := ReadByte;
        Mask := $80;
      end;
      if (Cmd and Mask) <> 0 then
        Tmp := Tmp or 1;
      Dec(BitLen);
    end;
    Result := Tmp;
  end;

begin
  Result := False;
  SetLength(OutBuf, 0);
  if (InBuf = nil) or (InSize < 12) then Exit;

  AtmPos := -1;
  IsPRG := False;
  IsV31 := False;
  OrigSize := 0;
  PackedSize := 0;
  DataOffset := 0;

  // Phase 1: Check Data Header ('ATM5', 'ATM3', 'ATOM')
  for SearchIdx := 0 to Min(InSize - 12, 4096) do
  begin
    if InBuf[SearchIdx] = Byte('A') then
    begin
      Sig := ReadBE32(InBuf, SearchIdx);
      if (Sig = $41544D35) or (Sig = $41544D33) or (Sig = $41544F4D) then
      begin
        OrigSize := ReadBE32(InBuf, SearchIdx + 4);
        PackedSize := ReadBE32(InBuf, SearchIdx + 8);
        if (OrigSize > 0) and (OrigSize < 16777216) and (PackedSize > 0) and (PackedSize < 16777216) then
        begin
          AtmPos := SearchIdx;
          DataOffset := SearchIdx + 12;
          if Sig = $41544D33 then IsV31 := True;
          Break;
        end;
      end;
    end;
  end;

  // Phase 2: Check Packed PRG Executable ('ATOMIK')
  if AtmPos = -1 then
  begin
    for SearchIdx := 0 to Min(InSize - 38, 8192) do
    begin
      if CheckString(InBuf, InSize, SearchIdx, 'ATOMIK') then
      begin
        for K := 0 to 19 do
        begin
          if (SearchIdx + K + 2 < InSize) and
             (InBuf[SearchIdx + K] = Byte('3')) and
             (InBuf[SearchIdx + K + 1] = Byte('.')) and
             (InBuf[SearchIdx + K + 2] = Byte('1')) then
          begin
            IsV31 := True;
            Break;
          end;
        end;

        if IsV31 then
        begin
          OrigSize := ReadBE32(InBuf, SearchIdx + 22);
          PackedSize := ReadBE32(InBuf, SearchIdx + 26);
          DataOffset := SearchIdx + 30;
        end
        else
        begin
          OrigSize := ReadBE32(InBuf, SearchIdx + 30);
          PackedSize := ReadBE32(InBuf, SearchIdx + 34);
          DataOffset := SearchIdx + 38;
        end;

        if (OrigSize > 0) and (OrigSize < 16777216) and (PackedSize > 0) and (PackedSize < 16777216) then
        begin
          AtmPos := SearchIdx;
          IsPRG := True;
          Break;
        end;
      end;
    end;
  end;

  if (AtmPos = -1) or (OrigSize = 0) then Exit;

  SetLength(OutBuf, OrigSize);
  DstIdx := OrigSize;
  SrcIdx := DataOffset + PackedSize;
  if SrcIdx > InSize then SrcIdx := InSize;

  PicCnt := 0;
  Cmd := 0;
  if IsPRG then
    Cmd := ReadByte
  else
  begin
    PicCnt := ReadByte;
    Cmd := ReadByte;
  end;

  while (Cmd = 0) and (SrcIdx > DataOffset) do
  begin
    if not IsPRG then PicCnt := Cmd;
    Cmd := ReadByte;
  end;

  Mask := $80;
  while ((Cmd and 1) = 0) and (Mask > 0) do
  begin
    Cmd := Cmd shr 1;
    Mask := Mask shr 1;
  end;
  Cmd := Cmd shr 1;

  OffsetTable[0] := 0;    OffsetTable[1] := 32;   OffsetTable[2] := 96;   OffsetTable[3] := 352;
  OffsetTable[4] := 864;  OffsetTable[5] := 1888; OffsetTable[6] := 3936; OffsetTable[7] := 8032;

  if IsV31 then
  begin
    BitTable[0] := 5; BitTable[1] := 6; BitTable[2] := 8; BitTable[3] := 9;
    BitTable[4] := 10; BitTable[5] := 11; BitTable[6] := 12; BitTable[7] := 13;
  end
  else
  begin
    BitTable[0] := 1; BitTable[1] := 2; BitTable[2] := 4; BitTable[3] := 5;
    BitTable[4] := 6; BitTable[5] := 7; BitTable[6] := 8; BitTable[7] := 9;
  end;

  try
    while (DstIdx > 0) and (SrcIdx > DataOffset) do
    begin
      SpecialOffsetTabFull := False;
      ShortCharTabFull := False;

      if not IsV31 then
      begin
        if GetBits(1) = 1 then
        begin
          SpecialOffset := ReadByte;
          K := 1;
          for I := 0 to 6 do
          begin
            if K = SpecialOffset then Inc(K, 2);
            SpecialOffsetTab[I] := K;
            Inc(K, 2);
          end;
          SpecialOffsetTabFull := True;
        end;

        if GetBits(1) = 1 then
        begin
          for I := 0 to 15 do
            ShortCharTab[I] := ReadByte;
          ShortCharTabFull := True;
        end;
      end;

      LengthBits := ReadByte;
      BlockSize := (Integer(ReadByte) shl 8) or ReadByte;
      if (BlockSize = 0) and (SrcIdx <= DataOffset) then Break;
      if DstIdx < BlockSize then BlockSize := DstIdx;
      BlockTarget := DstIdx - BlockSize;

      while (DstIdx > BlockTarget) and (SrcIdx > DataOffset) do
      begin
        if GetBits(1) = 0 then
        begin
          Dec(DstIdx);
          OutBuf[DstIdx] := ReadByte;
        end
        else
        begin
          LenVal := 0;
          while GetBits(1) = 0 do
          begin
            if LenVal = LengthBits then
            begin
              if IsV31 then
              begin
                if GetBits(1) = 1 then
                  Inc(LenVal, 1)
                else
                begin
                  K := GetBits(4);
                  if K = 0 then K := GetBits(8) + 15;
                  Inc(LenVal, K);
                end;
              end
              else
              begin
                K := GetBits(4);
                if K = 0 then
                begin
                  K := GetBits(8);
                  if K = 0 then K := GetBits(14) + 255;
                  Inc(K, 15);
                end;
                Inc(LenVal, K);
              end;
              Break;
            end;
            Inc(LenVal);
          end;

          if LenVal = 0 then
          begin
            if GetBits(1) = 1 then
            begin
              K := GetBits(4);
              Dec(DstIdx);
              if ShortCharTabFull then
                OutBuf[DstIdx] := ShortCharTab[K]
              else
              begin
                SubVal := K;
                if SubVal > 7 then Inc(SubVal, $F0);
                if DstIdx + 1 < OrigSize then
                  OutBuf[DstIdx] := Byte((Integer(OutBuf[DstIdx + 1]) - SubVal) and $FF)
                else
                  OutBuf[DstIdx] := Byte((-SubVal) and $FF);
              end;
              Continue;
            end
            else
            begin
              MatchLen := 2;
              OffsetIdx := GetBits(2);
            end;
          end
          else
          begin
            MatchLen := LenVal + 2;
            OffsetIdx := GetBits(3);
          end;

          if OffsetIdx > 7 then Exit(False);

          if IsV31 then
            MatchOffset := OffsetTable[OffsetIdx] + GetBits(BitTable[OffsetIdx])
          else
          begin
            if SpecialOffsetTabFull then
            begin
              K := GetBits(BitTable[OffsetIdx]) shl 4;
              I := GetBits(3);
              if I = 7 then
              begin
                if GetBits(1) = 1 then
                  K := K or ((GetBits(3) * 2) and $FF)
                else
                  K := K or SpecialOffset;
              end
              else if (I >= 0) and (I < 7) then
                K := K or SpecialOffsetTab[I];
              MatchOffset := OffsetTable[OffsetIdx] + K;
            end
            else
              MatchOffset := OffsetTable[OffsetIdx] + GetBits(BitTable[OffsetIdx] + 4);
          end;

          QIdx := DstIdx + MatchOffset + 1;
          if DstIdx - BlockTarget < MatchLen then
            MatchLen := DstIdx - BlockTarget;

          while MatchLen > 0 do
          begin
            Dec(DstIdx);
            Dec(QIdx);
            if (QIdx >= 0) and (QIdx < OrigSize) then
              OutBuf[DstIdx] := OutBuf[QIdx]
            else
              OutBuf[DstIdx] := 0;
            Dec(MatchLen);
          end;
        end;
      end;
    end;

    // Chunky to Planar screen decrunch
    while (PicCnt > 0) and (Length(OutBuf) > 32000) do
    begin
      PicOffset := ReadBE32(@OutBuf[0], Length(OutBuf) - 4);
      SetLength(OutBuf, Length(OutBuf) - 4);
      if PicOffset > Length(OutBuf) - 32000 then PicOffset := Length(OutBuf) - 32000;
      if PicOffset < 0 then PicOffset := 0;

      P := PicOffset;
      for I := 1 to 4000 do
      begin
        Plane0 := 0; Plane1 := 0; Plane2 := 0; Plane3 := 0;
        for J := 0 to 7 do
        begin
          DataByte := OutBuf[P + J];
          Plane0 := Plane0 shl 2; Plane1 := Plane1 shl 2; Plane2 := Plane2 shl 2; Plane3 := Plane3 shl 2;
          Inc(Plane3, DataByte and 1); DataByte := DataByte shr 1;
          Inc(Plane2, DataByte and 1); DataByte := DataByte shr 1;
          Inc(Plane1, DataByte and 1); DataByte := DataByte shr 1;
          Inc(Plane0, DataByte and 1);
          Inc(Plane3, DataByte and 2); DataByte := DataByte shr 1;
          Inc(Plane2, DataByte and 2); DataByte := DataByte shr 1;
          Inc(Plane1, DataByte and 2); DataByte := DataByte shr 1;
          Inc(Plane0, DataByte and 2);
        end;
        OutBuf[P] := (Plane0 shr 8) and $FF; Inc(P); OutBuf[P] := Plane0 and $FF; Inc(P);
        OutBuf[P] := (Plane1 shr 8) and $FF; Inc(P); OutBuf[P] := Plane1 and $FF; Inc(P);
        OutBuf[P] := (Plane2 shr 8) and $FF; Inc(P); OutBuf[P] := Plane2 and $FF; Inc(P);
        OutBuf[P] := (Plane3 shr 8) and $FF; Inc(P); OutBuf[P] := Plane3 and $FF; Inc(P);
      end;
      Dec(PicCnt);
    end;

    Result := (DstIdx = 0) and (Length(OutBuf) > 0);
  except
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------
// Automation Packer v2.x / v2.5
// -----------------------------------------------------------------------------

function DecompressAutomation(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
const
  LowerTable: array[0..3] of Integer = (10, 3, 2, 2);
  UpperTable: array[0..3] of Integer = (14, 7, 4, 1);
  RepeatBits: array[0..4] of Integer = (10, 2, 1, 0, 0);
  RepeatAdj:  array[0..4] of Integer = (10, 6, 4, 3, 2);
  OffsetBits: array[0..2] of Integer = (11, 4, 7);
  OffsetAdj:  array[0..2] of Integer = (288, 0, 32);
var
  DataStart, A0, A1, A2, EndA0, D1, D2, D3, D4, D5, OffsetVal, MaskVal, I: Integer;
  BitBuf, CarryBit: Byte;
  TestWord: SmallInt;

  function GetBit: Byte;
  var
    NextCarry: Byte;
  begin
    CarryBit := (BitBuf shr 7) and 1;
    BitBuf := (BitBuf shl 1) and $FF;
    if BitBuf = 0 then
    begin
      Dec(A0);
      if (A0 < 0) or (A0 >= InSize) then raise Exception.Create('Automation OOB');
      BitBuf := InBuf[A0];
      NextCarry := (BitBuf shr 7) and 1;
      BitBuf := ((BitBuf shl 1) or CarryBit) and $FF;
      CarryBit := NextCarry;
    end;
    Result := CarryBit;
  end;

  function GetBits(NumBits: Integer; InitialVal: Integer = 0): Integer;
  var
    K: Integer;
  begin
    Result := InitialVal;
    for K := 1 to NumBits do
      Result := ((Result shl 1) or GetBit) and $FFFF;
  end;

begin
  Result := False;
  SetLength(OutBuf, 0);
  if (InBuf = nil) or (InSize < 12) then Exit;

  DataStart := -1;
  for I := 0 to Min(8192, InSize - 12) do
  begin
    if (I = 0) or
       CheckString(InBuf, InSize, I, 'LSD!') or
       CheckString(InBuf, InSize, I, 'LSD$') or
       CheckString(InBuf, InSize, I, 'AUTM') or
       CheckString(InBuf, InSize, I, 'AU5!') or
       CheckString(InBuf, InSize, I, 'AU5C') or
       CheckString(InBuf, InSize, I, 'LSDC') then
    begin
      D5 := ReadBE32(InBuf, I + 4);
      D1 := ReadBE32(InBuf, I + 8);
      if (D1 > 0) and (D5 >= D1) and (D5 < 32 * 1024 * 1024) then
      begin
        if (I + 8 + D1 <= InSize + 4) and (I + 8 + D1 >= InSize - 1024) then
        begin
          DataStart := I;
          Break;
        end;
      end;
    end;
  end;

  if DataStart = -1 then Exit;

  D5 := ReadBE32(InBuf, DataStart + 4); // Unpacked size
  if (D5 = 0) or (D5 > 32 * 1024 * 1024) then Exit;

  SetLength(OutBuf, D5);
  A1 := D5;

  A0 := DataStart + 8 + Integer(ReadBE32(InBuf, DataStart + 8)) - 4;
  Dec(A0, 2);
  if (A0 < 0) or (A0 + 2 > InSize) then Exit;

  TestWord := SmallInt(ReadBE16(InBuf, A0));
  if TestWord < 0 then Dec(A0, 1);

  Dec(A0);
  if (A0 < 0) or (A0 >= InSize) then Exit;
  BitBuf := InBuf[A0];
  CarryBit := 0;

  EndA0 := DataStart + 12;

  try
    while True do
    begin
      if GetBit <> 0 then
      begin
        D1 := 0;
        if GetBit <> 0 then
        begin
          D3 := 3;
          while True do
          begin
            MaskVal := (1 shl LowerTable[D3]) - 1;
            D1 := GetBits(LowerTable[D3], 0);
            if (D3 <> 0) and (D1 = MaskVal) then
              Dec(D3)
            else
              Break;
          end;
          Inc(D1, UpperTable[D3]);
        end;

        for I := 0 to D1 do
        begin
          Dec(A0);
          Dec(A1);
          if (A1 < 0) or (A0 < 0) or (A0 >= InSize) then Exit(False);
          OutBuf[A1] := InBuf[A0];
        end;
      end;

      if A0 <= EndA0 then Break;

      D2 := 3;
      while True do
      begin
        if GetBit = 0 then Break;
        Dec(D2);
        if D2 < 0 then Break;
      end;

      Inc(D2);
      if RepeatBits[D2] <> 0 then
        D1 := GetBits(RepeatBits[D2], 0)
      else
        D1 := 0;
      Inc(D1, RepeatAdj[D2]);

      if D1 = 2 then
      begin
        if GetBit = 0 then
        begin
          D3 := 5;
          D4 := 0;
        end
        else
        begin
          D3 := 8;
          D4 := 64;
        end;
        OffsetVal := GetBits(D3 + 1, 0) + D4;
      end
      else
      begin
        D3 := 1;
        while True do
        begin
          if GetBit = 0 then Break;
          Dec(D3);
          if D3 < 0 then Break;
        end;
        Inc(D3);
        OffsetVal := GetBits(OffsetBits[D3] + 1, 0) + OffsetAdj[D3];
      end;

      A2 := A1 + OffsetVal + D1;
      if A2 > Length(OutBuf) then A2 := Length(OutBuf);

      for I := 1 to D1 do
      begin
        Dec(A1);
        Dec(A2);
        if (A1 < 0) or (A2 < 0) or (A2 >= Length(OutBuf)) then
        begin
          SetLength(OutBuf, 0);
          Exit(False);
        end;
        OutBuf[A1] := OutBuf[A2];
      end;
    end;

    Result := True;
  except
    SetLength(OutBuf, 0);
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------
// JAM Packer (LSD, LZH with Dynamic Huffman, LZW)
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// Dynamic Huffman & JAM Decompressor
// -----------------------------------------------------------------------------

// DYNAMIC HUFFMAN
const
  MaxHuffCount = 314;

type
  TDynNode = record
    Freq: Integer;
    Index: Integer;
    Parent: Integer;
    LeftLeaf: Integer;
    RightLeaf: Integer;
  end;

  TDynamicHuffman = class
  private
    FCount: Integer;
    FNodes: array[0..MaxHuffCount * 2 - 2] of TDynNode;
    FCodeMap: array[0..MaxHuffCount * 2 - 2] of Integer;
  public
    constructor Create;
    procedure Reset;
    function Decode(BitReader: TMSBBitReader): Integer;
    procedure Update(Code: Integer);
    procedure Halve;
    function GetMaxFrequency: Integer;
  end;

constructor TDynamicHuffman.Create;
begin
  inherited Create;
  Reset;
end;

procedure TDynamicHuffman.Reset;
var
  I, J, L, R: Integer;
begin
  FCount := MaxHuffCount;
  for I := 0 to FCount - 1 do
  begin
    FNodes[I].Freq := 1;
    FNodes[I].Index := I + (MaxHuffCount - FCount) * 2;
    FNodes[I].Parent := MaxHuffCount * 2 - FCount + (I shr 1);
    FNodes[I].LeftLeaf := 0;
    FNodes[I].RightLeaf := 0;
    FCodeMap[I + (MaxHuffCount - FCount) * 2] := I;
  end;
  for I := MaxHuffCount * 2 - FCount to MaxHuffCount * 2 - 2 do
  begin
    J := (I - (MaxHuffCount * 2 - FCount)) * 2;
    if J >= FCount then L := J + (MaxHuffCount - FCount) * 2 else L := J;
    if J + 1 >= FCount then R := J + 1 + (MaxHuffCount - FCount) * 2 else R := J + 1;
    FNodes[I].Freq := FNodes[L].Freq + FNodes[R].Freq;
    FNodes[I].Index := I;
    FNodes[I].Parent := MaxHuffCount + (I shr 1);
    FNodes[I].LeftLeaf := L;
    FNodes[I].RightLeaf := R;
    FCodeMap[I] := I;
  end;
end;

function TDynamicHuffman.Decode(BitReader: TMSBBitReader): Integer;
var
  Code: Integer;
begin
  if FCount <= 1 then Exit(0);
  Code := MaxHuffCount * 2 - 2;
  while Code >= MaxHuffCount do
  begin
    if BitReader.ReadBits(1) = 1 then
      Code := FNodes[Code].RightLeaf
    else
      Code := FNodes[Code].LeftLeaf;
  end;
  Result := Code;
end;

procedure TDynamicHuffman.Update(Code: Integer);
var
  Index, DestIndex, Freq, DestCode, TempIndex, TempCodeMap, TempParent: Integer;
  ParentA, ParentB: Integer;
begin
  if Code >= FCount then raise Exception.Create('Huffman Code OOB');
  if FCount = 1 then
  begin
    FNodes[0].Freq := 1;
    Exit;
  end;

  while Code <> MaxHuffCount * 2 - 2 do
  begin
    Inc(FNodes[Code].Freq);
    Index := FNodes[Code].Index;
    DestIndex := Index;
    Freq := FNodes[Code].Freq;

    while (DestIndex <> MaxHuffCount * 2 - 2) and (Freq > FNodes[FCodeMap[DestIndex + 1]].Freq) do
      Inc(DestIndex);

    if Index <> DestIndex then
    begin
      DestCode := FCodeMap[DestIndex];

      TempIndex := FNodes[Code].Index;
      FNodes[Code].Index := FNodes[DestCode].Index;
      FNodes[DestCode].Index := TempIndex;

      TempCodeMap := FCodeMap[Index];
      FCodeMap[Index] := FCodeMap[DestIndex];
      FCodeMap[DestIndex] := TempCodeMap;

      ParentA := FNodes[Code].Parent;
      ParentB := FNodes[DestCode].Parent;

      if FNodes[ParentA].LeftLeaf = Code then
      begin
        if FNodes[ParentB].LeftLeaf = DestCode then
        begin
          FNodes[ParentA].LeftLeaf := DestCode;
          FNodes[ParentB].LeftLeaf := Code;
        end
        else
        begin
          FNodes[ParentA].LeftLeaf := DestCode;
          FNodes[ParentB].RightLeaf := Code;
        end;
      end
      else
      begin
        if FNodes[ParentB].LeftLeaf = DestCode then
        begin
          FNodes[ParentA].RightLeaf := DestCode;
          FNodes[ParentB].LeftLeaf := Code;
        end
        else
        begin
          FNodes[ParentA].RightLeaf := DestCode;
          FNodes[ParentB].RightLeaf := Code;
        end;
      end;

      TempParent := FNodes[Code].Parent;
      FNodes[Code].Parent := FNodes[DestCode].Parent;
      FNodes[DestCode].Parent := TempParent;
    end;
    Code := FNodes[Code].Parent;
  end;
  Inc(FNodes[Code].Freq);
end;

procedure TDynamicHuffman.Halve;
var
  I, J, K, L, R, Freq, C, DestCode, TempIndex, TempParent, TempMap: Integer;
begin
  if FCount <= 1 then Exit;

  J := (MaxHuffCount - FCount) * 2;
  for I := (MaxHuffCount - FCount) * 2 to MaxHuffCount * 2 - 2 do
  begin
    if J >= MaxHuffCount * 2 - FCount then Break;
    if FCodeMap[I] < MaxHuffCount then
    begin
      FNodes[FCodeMap[I]].Index := J;
      Inc(J);
    end;
  end;

  for I := 0 to FCount - 1 do
  begin
    FNodes[I].Freq := (FNodes[I].Freq + 1) shr 1;
    FNodes[I].Parent := MaxHuffCount + (FNodes[I].Index shr 1);
    FCodeMap[FNodes[I].Index] := I;
  end;

  J := (MaxHuffCount - FCount) * 2;
  for I := MaxHuffCount * 2 - FCount to MaxHuffCount * 2 - 2 do
  begin
    L := FCodeMap[J];
    R := FCodeMap[J + 1];
    Freq := FNodes[L].Freq + FNodes[R].Freq;
    FNodes[I].Freq := Freq;
    FNodes[I].Index := I;
    FNodes[I].Parent := MaxHuffCount + (I shr 1);
    FNodes[I].LeftLeaf := L;
    FNodes[I].RightLeaf := R;
    FCodeMap[I] := I;

    K := I;
    while (K > 0) and (Freq < FNodes[FCodeMap[K - 1]].Freq) do
    begin
      C := FCodeMap[K];
      DestCode := FCodeMap[K - 1];

      TempIndex := FNodes[C].Index;
      FNodes[C].Index := FNodes[DestCode].Index;
      FNodes[DestCode].Index := TempIndex;

      TempParent := FNodes[C].Parent;
      FNodes[C].Parent := FNodes[DestCode].Parent;
      FNodes[DestCode].Parent := TempParent;

      TempMap := FCodeMap[K];
      FCodeMap[K] := FCodeMap[K - 1];
      FCodeMap[K - 1] := TempMap;
      Dec(K);
    end;
    Inc(J, 2);
  end;
end;

function TDynamicHuffman.GetMaxFrequency: Integer;
begin
  Result := FNodes[MaxHuffCount * 2 - 2].Freq;
end;

// =============================================================================
// MAIN DECOMPRESSORS
// =============================================================================

function DecompressInternalV2(const PackedData: PByte; RawSize, PackedSize: Integer): TBytes;
var
  InStream: TBackwardInputStream;
  OutStream: TBackwardOutputStream;
  BitReader: TMSBBitReader;
  LitVlc, CountBaseVlc, CountVlc, DistBaseVlc, DistVlc: TVLC;
  LitLength, CountBase, CountVal, DistBase, Distance: Integer;
  Tmp, I, J: Integer;
begin
  InStream := TBackwardInputStream.Create(PackedData, 12, PackedSize);
  BitReader := TMSBBitReader.Create(InStream);
  try
    if (InStream.ReadBE16 and $8000) <> 0 then
      InStream.ReadByte;
    Tmp := InStream.ReadByte;
    I := 1; J := 7;
    while I < $80 do
    begin
      if (Tmp and I) <> 0 then
      begin
        BitReader.Reset(Tmp shr (8 - J), J);
        Break;
      end;
      I := I shl 1;
      Dec(J);
    end;

    SetLength(Result, RawSize);
    OutStream := TBackwardOutputStream.Create(@Result[0], 0, RawSize);
    try
      LitVlc := TVLC.Create([1, 2, 2, 3, 10]);
      CountBaseVlc := TVLC.Create([1, 1, 1, 1]);
      CountVlc := TVLC.Create([0, 0, 1, 2, 10]);
      DistBaseVlc := TVLC.Create([1, 1]);
      DistVlc := TVLC.Create([5, 8, 12]);
      try
        while True do
        begin
          if BitReader.ReadBits(1) = 1 then
          begin
            LitLength := LitVlc.DecodeCascade(BitReader) + 1;
            for I := 1 to LitLength do
              OutStream.WriteByte(InStream.ReadByte);
          end;
          if OutStream.Eof then Break;

          CountBase := CountBaseVlc.DecodeCascade(BitReader);
          CountVal := CountVlc.Decode(BitReader, CountBase) + 2;
          Distance := 0;
          if CountVal = 2 then
          begin
            if BitReader.ReadBits(1) = 1 then
              Distance := Integer(BitReader.ReadBits(9)) + $40
            else
              Distance := Integer(BitReader.ReadBits(6));
          end
          else
          begin
            DistBase := DistBaseVlc.DecodeCascade(BitReader);
            if DistBase < 2 then DistBase := DistBase xor 1;
            Distance := DistVlc.Decode(BitReader, DistBase);
          end;
          Inc(Distance, CountVal);
          OutStream.Copy(Distance, CountVal);
        end;
      finally
        LitVlc.Free;
        CountBaseVlc.Free;
        CountVlc.Free;
        DistBaseVlc.Free;
        DistVlc.Free;
      end;
    finally
      OutStream.Free;
    end;
  finally
    BitReader.Free;
    InStream.Free;
  end;
end;

function DecompressInternalLZH(const PackedData: PByte; RawSize, PackedSize: Integer): TBytes;
var
  InStream: TBackwardInputStream;
  OutStream: TBackwardOutputStream;
  BitReader: TMSBBitReader;
  Decoder: TDynamicHuffman;
  Vlc: TVLC;
  Symbol, DistBase, Distance, CountVal: Integer;
begin
  InStream := TBackwardInputStream.Create(PackedData, 12, PackedSize);
  BitReader := TMSBBitReader.Create(InStream);
  try
    SetLength(Result, RawSize);
    OutStream := TBackwardOutputStream.Create(@Result[0], 0, RawSize);
    try
      Decoder := TDynamicHuffman.Create;
      Vlc := TVLC.Create([5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 9, 9, 9, 10]);
      try
        while not OutStream.Eof do
        begin
          Symbol := Decoder.Decode(BitReader);
          if Decoder.GetMaxFrequency = $8000 then
            Decoder.Halve;
          Decoder.Update(Symbol);
          if Symbol < 256 then
            OutStream.WriteByte(Symbol)
          else
          begin
            DistBase := BitReader.ReadBits(4);
            Distance := Vlc.Decode(BitReader, DistBase) + 1;
            CountVal := Symbol - 253;
            OutStream.Copy(Distance, CountVal, $20);
          end;
        end;
      finally
        Vlc.Free;
        Decoder.Free;
      end;
    finally
      OutStream.Free;
    end;
  finally
    BitReader.Free;
    InStream.Free;
  end;
end;

function DecompressJam(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
var
  Off: Integer;
  IsLsd, IsLzh, IsLzw: Boolean;
  RawSize, PackedSize: LongWord;
begin
  Result := False;
  SetLength(OutBuf, 0);
  if (InBuf = nil) or (InSize < 12) then Exit;

  for Off := 0 to InSize - 12 do
  begin
    if InBuf[Off] = Byte('L') then
    begin
      IsLsd := (InBuf[Off+1] = Byte('S')) and (InBuf[Off+2] = Byte('D')) and (InBuf[Off+3] = Byte('!'));
      IsLzh := (InBuf[Off+1] = Byte('Z')) and (InBuf[Off+2] = Byte('H')) and (InBuf[Off+3] = Byte('!'));
      IsLzw := (InBuf[Off+1] = Byte('Z')) and (InBuf[Off+2] = Byte('W')) and (InBuf[Off+3] = Byte('!'));
      if IsLsd or IsLzh or IsLzw then
      begin
        RawSize := ReadBE32(InBuf, Off + 4);
        PackedSize := ReadBE32(InBuf, Off + 8);
        if IsLsd then Inc(PackedSize, 4)
        else if IsLzh then Inc(PackedSize, 12);

        if (PackedSize > 0) and (PackedSize <= LongWord(InSize)) and (Off <= InSize - Integer(PackedSize)) and (RawSize > 0) and (RawSize <= $800000) then
        begin
          try
            if IsLsd then
            begin
              OutBuf := DecompressInternalV2(@InBuf[Off], RawSize, PackedSize);
              if Length(OutBuf) = RawSize then Exit(True);
            end
            else if IsLzh then
            begin
              OutBuf := DecompressInternalLZH(@InBuf[Off], RawSize, PackedSize);
              if Length(OutBuf) = RawSize then Exit(True);
            end;
          except
          end;
        end;
      end;
    end;
  end;
end;


// Thunder v1 & v2 Decompressor
// -----------------------------------------------------------------------------

function DecompressThunderV1(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
var
  AtomOffset, A2, PackedEnd, A4, Count, LengthVal, OffsetVal, MatchPtr, I: Integer;
  UncompressedLen, PackedLen: LongWord;
  D4, D1, D2, NextByte, CarryVal: Word;
begin
  Result := False;
  SetLength(OutBuf, 0);
  if (InBuf = nil) or (InSize < 12) then Exit;

  AtomOffset := FindEmbeddedSig(InBuf, InSize, 'ATOM');
  if (AtomOffset < 0) or (AtomOffset + 12 > InSize) then Exit;

  UncompressedLen := ReadBE32(InBuf, AtomOffset + 4);
  PackedLen := ReadBE32(InBuf, AtomOffset + 8);
  if (UncompressedLen = 0) or (UncompressedLen > 16777216) then Exit;

  A2 := AtomOffset + 12;
  PackedEnd := Min(InSize, A2 + Integer(PackedLen));
  SetLength(OutBuf, UncompressedLen);
  A4 := 0;
  D1 := 0;

  try
    while (A2 < PackedEnd) and (A4 < Integer(UncompressedLen)) do
    begin
      if A2 + 1 >= InSize then Break;
      D4 := ReadBE16(InBuf, A2);
      Inc(A2, 2);

      if (D4 and $4000) = 0 then
      begin
        if (D4 and $8000) <> 0 then
        begin
          D4 := D4 and $7FFF;
          if A2 >= InSize then Break;
          D1 := InBuf[A2];
          Inc(A2);
        end;
        Inc(D4, $2000);
        while True do
        begin
          CarryVal := D4 and 1;
          D4 := D4 shr 1;
          if D4 = 0 then Break;
          if A4 >= Length(OutBuf) then Break;
          if CarryVal <> 0 then
          begin
            OutBuf[A4] := D1;
            Inc(A4);
          end
          else
          begin
            if A2 >= InSize then Break;
            OutBuf[A4] := InBuf[A2];
            Inc(A4);
            Inc(A2);
          end;
        end;
      end
      else if (D4 and $8000) = 0 then
      begin
        D4 := D4 and $BFFF;
        if A2 >= InSize then Break;
        D2 := InBuf[A2];
        Inc(A2);
        Count := D4 + 1;
        for I := 1 to Count do
        begin
          if A4 >= Length(OutBuf) then Break;
          OutBuf[A4] := D2;
          Inc(A4);
        end;
      end
      else
      begin
        D4 := D4 and $3FFF;
        if A2 >= InSize then Break;
        NextByte := InBuf[A2];
        Inc(A2);
        LengthVal := (D4 shr 8) + 1;
        OffsetVal := ((D4 and $FF) shl 8) or NextByte;
        MatchPtr := A4 - OffsetVal;
        if MatchPtr < 0 then Exit(False);

        for I := 1 to LengthVal do
        begin
          if A4 >= Length(OutBuf) then Break;
          OutBuf[A4] := OutBuf[MatchPtr];
          Inc(A4);
          Inc(MatchPtr);
        end;
      end;
    end;

    Result := (A4 = Length(OutBuf)) and (A4 > 0);
    if not Result then
      SetLength(OutBuf, 0);
  except
    SetLength(OutBuf, 0);
    Result := False;
  end;
end;

function DecompressThunderV2(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
var
  AtomOffset, A2, A3, A0, A4, I, D1Val, D2Val, D0Val, D2Offset: Integer;
  UncompressedLen, PackedEndOff, AbsPackedEnd, AbsLitLenOff, LitLen, AbsLitStart: LongWord;
  D7: Word;

  function U3: Boolean;
  var
    Carry: Boolean;
    W: Word;
  begin
    Carry := (D7 and $8000) <> 0;
    D7 := (D7 shl 1) and $FFFF;
    if D7 = 0 then
    begin
      if A2 + 1 >= InSize then Exit(False);
      W := ReadBE16(InBuf, A2);
      Inc(A2, 2);
      Carry := (W and $8000) <> 0;
      D7 := ((W shl 1) or 1) and $FFFF;
    end;
    Result := Carry;
  end;

  function U14(CountBits: Integer): Integer;
  var
    D2: Integer;
    K: Integer;
  begin
    D2 := 0;
    for K := 1 to CountBits do
      D2 := (D2 shl 1) or (Byte(U3) and 1);
    Result := D2;
  end;

  function U4: Integer;
  var
    D1, K: Integer;
  begin
    D1 := 0;
    for K := 1 to 4 do
      D1 := (D1 shl 1) or (Byte(U3) and 1);
    if D1 > 0 then
      Result := U14(D1)
    else
      Result := 0;
  end;

  procedure CopyMatch(OffsetVal, LengthVal: Integer);
  var
    Src, K: Integer;
  begin
    if OffsetVal <= 0 then Exit;
    Src := A4 - OffsetVal;
    if Src < 0 then Exit;
    for K := 1 to LengthVal do
    begin
      if A4 >= Length(OutBuf) then Break;
      OutBuf[A4] := OutBuf[Src];
      Inc(A4);
      Inc(Src);
    end;
  end;

begin
  Result := False;
  SetLength(OutBuf, 0);
  if (InBuf = nil) or (InSize < 12) then Exit;

  AtomOffset := FindEmbeddedSig(InBuf, InSize, 'ATOM');
  if (AtomOffset < 0) or (AtomOffset + 12 > InSize) then Exit;

  UncompressedLen := ReadBE32(InBuf, AtomOffset + 4);
  PackedEndOff := ReadBE32(InBuf, AtomOffset + 8);
  if (UncompressedLen = 0) or (UncompressedLen > 16777216) then Exit;

  AbsPackedEnd := AtomOffset + PackedEndOff;
  if AbsPackedEnd > LongWord(InSize) then Exit;

  AbsLitLenOff := AbsPackedEnd - 4;
  LitLen := ReadBE32(InBuf, AbsLitLenOff);
  AbsLitStart := AbsLitLenOff - LitLen;
  if AbsLitStart < LongWord(AtomOffset + 12) then Exit;

  A2 := AtomOffset + 12;
  A3 := AbsLitStart;
  A0 := AbsLitStart;
  SetLength(OutBuf, UncompressedLen);
  A4 := 0;
  D7 := $8000;

  try
    while (A2 <= A3) and (A4 < Integer(UncompressedLen)) do
    begin
      if not U3 then
      begin
        if A0 >= Integer(AbsPackedEnd) then Break;
        OutBuf[A4] := InBuf[A0];
        Inc(A4);
        Inc(A0);
      end
      else
      begin
        if not U3 then
        begin
          D1Val := U14(3);
          if D1Val = 1 then
          begin
            for I := 1 to 10 do
            begin
              if (A0 >= Integer(AbsPackedEnd)) or (A4 >= Length(OutBuf)) then Break;
              OutBuf[A4] := InBuf[A0];
              Inc(A4);
              Inc(A0);
            end;
          end
          else if D1Val = 0 then
          begin
            if A4 < Length(OutBuf) then
            begin
              OutBuf[A4] := 0;
              Inc(A4);
            end;
          end
          else
          begin
            D2Val := U14(D1Val);
            D2Offset := U4;
            CopyMatch(D2Offset, D2Val + 1);
          end;
        end
        else
        begin
          if not U3 then
          begin
            D1Val := U14(4);
            if D1Val = 1 then
            begin
              if A0 - 1 >= Integer(AbsLitStart) then
              begin
                OutBuf[A4] := InBuf[A0 - 1];
                Inc(A4);
              end;
            end
            else if D1Val < 1 then
            begin
              D2Val := U4;
              if D2Val = 0 then
              begin
                D0Val := U4;
                D2Offset := U4;
                CopyMatch(D2Offset, D0Val + 1);
              end
              else
              begin
                D1Val := 0;
                if not U3 then
                begin
                  if A0 < Integer(AbsPackedEnd) then
                  begin
                    D1Val := InBuf[A0];
                    Inc(A0);
                  end;
                end;
                for I := 1 to D2Val + 1 do
                begin
                  if A4 >= Length(OutBuf) then Break;
                  OutBuf[A4] := D1Val;
                  Inc(A4);
                end;
              end;
            end
            else
            begin
              D2Val := U14(D1Val);
              CopyMatch(D2Val, 3);
            end;
          end
          else
          begin
            if A0 >= Integer(AbsPackedEnd) then Break;
            D2Val := InBuf[A0];
            Inc(A0);
            CopyMatch(D2Val, 2);
          end;
        end;
      end;
    end;

    Result := (A4 = Length(OutBuf)) and (A4 > 0);
    if not Result then
      SetLength(OutBuf, 0);
  except
    SetLength(OutBuf, 0);
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------
// JEK Packer / Byte Killer
// -----------------------------------------------------------------------------

function DecompressJek(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
var
  Candidates: array of Integer;
  CandIdx, ActualEnd, OffsetVal, OutPtr, CmdVal, CountVal, OffVal, SrcVal, I, P, RelocBytes: Integer;
  XorKey, D0Val, CarryVal, WVal: LongWord;
  TextSize, DataSize, SymSize, TextLen, DataLen, SymLen, TotalExp, TotalWithReloc, FirstReloc: LongWord;
  End1, End2, MaxBufSize, ResultLen, PrgStart: Integer;
  TempBuf, RawSlice: TBytes;
  FallbackBuf: TBytes;

  function ReadLong: LongWord;
  begin
    if OffsetVal < 4 then raise Exception.Create('Jek EOF');
    Dec(OffsetVal, 4);
    Result := ReadBE32(InBuf, OffsetVal);
  end;

  function GetBit: LongWord;
  begin
    CarryVal := D0Val and 1;
    D0Val := D0Val shr 1;
    if D0Val = 0 then
    begin
      WVal := ReadLong xor XorKey;
      CarryVal := WVal and 1;
      D0Val := (WVal shr 1) or $80000000;
    end;
    Result := CarryVal;
  end;

  function GetBits(N: Integer): LongWord;
  var
    K: Integer;
  begin
    Result := 0;
    for K := 1 to N do
      Result := (Result shl 1) or GetBit;
  end;

begin
  Result := False;
  SetLength(OutBuf, 0);
  SetLength(FallbackBuf, 0);
  if (InBuf = nil) or (InSize < 12) then Exit;

  try
    // Gather candidate stream end positions
    SetLength(Candidates, 0);
    if (InSize >= 28) and (InBuf[0] = $60) and (InBuf[1] in [$1A, $1C, $00]) then
    begin
      TextSize := ReadBE32(InBuf, 2);
      DataSize := ReadBE32(InBuf, 6);
      SymSize  := ReadBE32(InBuf, 14);
      if (TextSize < 16777216) and (DataSize < 16777216) and (SymSize < 16777216) then
      begin
        End1 := 28 + Integer(TextSize) + Integer(DataSize);
        End2 := 28 + Integer(TextSize) + Integer(DataSize) + Integer(SymSize);
        if (End1 > 28) and (End1 <= InSize) then
        begin
          SetLength(Candidates, Length(Candidates) + 1);
          Candidates[High(Candidates)] := End1;
        end;
        if (End2 > 28) and (End2 <= InSize) and (End2 <> End1) then
        begin
          SetLength(Candidates, Length(Candidates) + 1);
          Candidates[High(Candidates)] := End2;
        end;
      end;
    end;
    SetLength(Candidates, Length(Candidates) + 1);
    Candidates[High(Candidates)] := InSize;

    for CandIdx := 0 to High(Candidates) do
    begin
      ActualEnd := Candidates[CandIdx];
      if ActualEnd < 8 then Continue;

      OffsetVal := ActualEnd - 4;
      XorKey := ReadBE32(InBuf, OffsetVal);
      D0Val := 0;

      MaxBufSize := 4 * 1024 * 1024;
      SetLength(TempBuf, MaxBufSize);
      OutPtr := MaxBufSize;

      try
        while OutPtr > 0 do
        begin
          if GetBit = 0 then
          begin
            if GetBit = 0 then
            begin
              CountVal := GetBits(3) + 1;
              for I := 1 to CountVal do
              begin
                Dec(OutPtr);
                if OutPtr >= 0 then TempBuf[OutPtr] := GetBits(8);
              end;
            end
            else
            begin
              OffVal := GetBits(8);
              SrcVal := OutPtr + OffVal;
              for I := 1 to 2 do
              begin
                Dec(OutPtr);
                Dec(SrcVal);
                if (OutPtr >= 0) and (SrcVal >= 0) and (SrcVal < MaxBufSize) then
                  TempBuf[OutPtr] := TempBuf[SrcVal]
                else
                  TempBuf[OutPtr] := 0;
              end;
            end;
          end
          else
          begin
            CmdVal := GetBits(2);
            CountVal := 0;
            OffVal := 0;

            if CmdVal = 0 then
            begin
              OffVal := GetBits(9);
              CountVal := 3;
            end
            else if CmdVal = 1 then
            begin
              OffVal := GetBits(10);
              CountVal := 4;
            end
            else if CmdVal = 2 then
            begin
              CountVal := GetBits(8) + 1;
              OffVal := GetBits(12);
            end
            else if CmdVal = 3 then
            begin
              CountVal := GetBits(8) + 9;
              for I := 1 to CountVal do
              begin
                Dec(OutPtr);
                if OutPtr >= 0 then TempBuf[OutPtr] := GetBits(8);
              end;
              Continue;
            end;

            SrcVal := OutPtr + OffVal;
            for I := 1 to CountVal do
            begin
              Dec(OutPtr);
              Dec(SrcVal);
              if (OutPtr >= 0) and (SrcVal >= 0) and (SrcVal < MaxBufSize) then
                TempBuf[OutPtr] := TempBuf[SrcVal]
              else
                TempBuf[OutPtr] := 0;
            end;
          end;
        end;
      except
        // Graceful exit upon hitting end of compressed bitstream
      end;

      if OutPtr < MaxBufSize then
      begin
        ResultLen := MaxBufSize - OutPtr;
        SetLength(RawSlice, ResultLen);
        Move(TempBuf[OutPtr], RawSlice[0], ResultLen);

        // Scan for embedded GEMDOS/TOS executable header ($601A)
        PrgStart := -1;
        TotalWithReloc := 0;
        for I := 0 to ResultLen - 28 do
        begin
          if (RawSlice[I] = $60) and (RawSlice[I+1] = $1A) then
          begin
            TextLen := ReadBE32(@RawSlice[I], 2);
            DataLen := ReadBE32(@RawSlice[I], 6);
            SymLen  := ReadBE32(@RawSlice[I], 14);
            if (TextLen < 16777216) and (DataLen < 16777216) and (SymLen < 16777216) then
            begin
              TotalExp := 28 + TextLen + DataLen + SymLen;
              if (TotalExp > 0) and (TotalExp <= LongWord(ResultLen - I + 512)) then
              begin
                P := I + TotalExp;
                TotalWithReloc := Min(LongWord(ResultLen - I), TotalExp);
                if P + 4 <= ResultLen then
                begin
                  FirstReloc := ReadBE32(@RawSlice[0], P);
                  if FirstReloc = 0 then
                    Inc(TotalWithReloc, 4)
                  else
                  begin
                    RelocBytes := 4;
                    while (P + RelocBytes < ResultLen) and (RawSlice[P + RelocBytes] <> 0) do
                      Inc(RelocBytes);
                    if P + RelocBytes < ResultLen then Inc(RelocBytes);
                    Inc(TotalWithReloc, RelocBytes);
                  end;
                end;
                TotalWithReloc := Min(LongWord(ResultLen - I), TotalWithReloc);
                PrgStart := I;
                Break;
              end;
            end;
          end;
        end;

        if PrgStart <> -1 then
        begin
          SetLength(OutBuf, TotalWithReloc);
          Move(RawSlice[PrgStart], OutBuf[0], TotalWithReloc);
          Exit(True);
        end
        else if (ResultLen >= 1024) and (Length(FallbackBuf) = 0) then
        begin
          FallbackBuf := RawSlice;
        end;
      end;
    end;

    if Length(FallbackBuf) > 0 then
    begin
      OutBuf := FallbackBuf;
      Exit(True);
    end;
  except
    SetLength(OutBuf, 0);
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------
// RLE / DEGAS Elite Decompressor
// -----------------------------------------------------------------------------

function DecompressDegasElite(const InBuf: PByte; InSize: Integer; DefaultRes: Integer; out OutBuf: TBytes): Boolean;
var
  ResWord: Word;
  ResMode: Integer;
  NumPlanes, BytesPerPlane, BytesPerTile, BytesPerScanline, Scanlines: Integer;
  Y, Z, PlaneX, TileOffset, Count, I: Integer;
  InPos: Integer;
  Ctrl, Val: Byte;
  RawPlane: array[0..79] of Byte;
  LinePtr: PByte;
begin
  Result := False;
  SetLength(OutBuf, 0);
  if (InBuf = nil) or (InSize < 34) then Exit;

  ResWord := (Word(InBuf[0]) shl 8) or Word(InBuf[1]);
  // In DEGAS Elite:
  // $8000 = Low Res (0)
  // $8001 = Med Res (1)
  // $8002 = High Res (2)
  // Bit 15 indicates compressed format or color cycling.
  if (ResWord and $8000 <> 0) or (ResWord in [0, 1, 2]) then
    ResMode := ResWord and $0003
  else
    ResMode := DefaultRes and $0003;

  case ResMode of
    0: // Low Res: 320x200, 4 planes, 16 colors
    begin
      NumPlanes := 4;
      BytesPerPlane := 40;
      BytesPerTile := 8;
      BytesPerScanline := 160;
      Scanlines := 200;
    end;
    1: // Med Res: 640x200, 2 planes, 4 colors
    begin
      NumPlanes := 2;
      BytesPerPlane := 80;
      BytesPerTile := 4;
      BytesPerScanline := 160;
      Scanlines := 200;
    end;
    2: // High Res: 640x400, 1 plane, 2 colors (monochrome)
    begin
      NumPlanes := 1;
      BytesPerPlane := 80;
      BytesPerTile := 2;
      BytesPerScanline := 80;
      Scanlines := 400;
    end;
  else
    Exit(False);
  end;

  // Allocate 32,034 bytes standard uncompressed DEGAS image (.PI1, .PI2, .PI3)
  SetLength(OutBuf, 32034);
  FillChar(OutBuf[0], 32034, 0);

  // Resolution word in big-endian (0, 1, or 2)
  OutBuf[0] := 0;
  OutBuf[1] := Byte(ResMode);

  // Copy 16-color palette (32 bytes)
  Move(InBuf[2], OutBuf[2], 32);

  InPos := 34; // Start of compressed RLE data

  // Decode scanlines
  for Y := 0 to Scanlines - 1 do
  begin
    LinePtr := @OutBuf[34 + Y * BytesPerScanline];

    for Z := 0 to NumPlanes - 1 do
    begin
      FillChar(RawPlane, SizeOf(RawPlane), 0);
      PlaneX := 0;

      while (PlaneX < BytesPerPlane) and (InPos < InSize) do
      begin
        Ctrl := InBuf[InPos];
        Inc(InPos);

        if Ctrl < 128 then
        begin
          // Literal run: Ctrl + 1 bytes
          Count := Ctrl + 1;
          if PlaneX + Count > BytesPerPlane then
            Count := BytesPerPlane - PlaneX;

          for I := 0 to Count - 1 do
          begin
            if InPos < InSize then
            begin
              RawPlane[PlaneX] := InBuf[InPos];
              Inc(InPos);
            end
            else
              RawPlane[PlaneX] := 0;
            Inc(PlaneX);
          end;
        end
        else
        begin
          // Repeat run: 257 - Ctrl bytes
          Count := 257 - Ctrl;
          if PlaneX + Count > BytesPerPlane then
            Count := BytesPerPlane - PlaneX;

          if InPos < InSize then
          begin
            Val := InBuf[InPos];
            Inc(InPos);
          end
          else
            Val := 0;

          for I := 0 to Count - 1 do
          begin
            RawPlane[PlaneX] := Val;
            Inc(PlaneX);
          end;
        end;
      end;

      // Interleave RawPlane into LinePtr
      if NumPlanes = 1 then
      begin
        // Monochrome high-res: no interleaving needed
        Move(RawPlane[0], LinePtr[0], BytesPerPlane);
      end
      else
      begin
        TileOffset := Z * 2;
        PlaneX := 0;
        while PlaneX < BytesPerPlane do
        begin
          LinePtr[TileOffset] := RawPlane[PlaneX];
          LinePtr[TileOffset + 1] := RawPlane[PlaneX + 1];
          Inc(PlaneX, 2);
          Inc(TileOffset, BytesPerTile);
        end;
      end;
    end;
  end;

  Result := True;
end;

function DecompressRLE(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
begin
  Result := DecompressDegasElite(InBuf, InSize, 0, OutBuf);
end;

// -----------------------------------------------------------------------------
// FIRE Packer v2.0 Decompressor
// -----------------------------------------------------------------------------

function DecompressFire(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
var
  PayloadStart, PayloadEnd: Integer;
  UnpackedSize: LongWord;
  InPos: Integer;
  OutPos: Integer;
  BitBuf: Byte;
  Extend: Byte;
  Off: Integer;
  PLen, ULen: LongWord;

  function GetBit: Byte; inline;
  var
    NewByte: Byte;
  begin
    Result := (BitBuf shr 7) and 1;
    BitBuf := (BitBuf shl 1) and $FF;
    Extend := Result;
    if BitBuf = 0 then
    begin
      if InPos <= PayloadStart then
        Exit(0);
      Dec(InPos);
      NewByte := InBuf[InPos];
      Result := (NewByte shr 7) and 1;
      BitBuf := ((NewByte shl 1) or Extend) and $FF;
      Extend := Result;
    end;
  end;

  function ReadBits(Count: Integer): LongWord;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 1 to Count do
      Result := (Result shl 1) or GetBit;
  end;

  function ReadDistance: LongWord;
  var
    Idx: LongWord;
  begin
    Idx := ReadBits(2);
    case Idx of
      0: Result := ReadBits(4);
      1: Result := ReadBits(8) + 16;
      2: Result := ReadBits(12) + 272;
    else
      Result := ReadBits(16) + 4368;
    end;
  end;

  function ReadOptDistance(out Dist: LongWord): Boolean;
  var
    Idx: LongWord;
  begin
    Idx := ReadBits(2);
    if Idx = 0 then
    begin
      Dist := 0;
      Exit(False);
    end;
    Dec(Idx);
    case Idx of
      0: Dist := ReadBits(4);
      1: Dist := ReadBits(8) + 16;
    else
      Dist := ReadBits(12) + 272;
    end;
    Result := True;
  end;

  procedure CopyMatch(Len: Integer; Dist: LongWord);
  var
    Offset: LongWord;
    K: Integer;
  begin
    Offset := Dist + LongWord(Len);
    for K := 1 to Len do
    begin
      if (OutPos <= 0) or (OutPos + Integer(Offset) > Integer(UnpackedSize)) then
        Exit;
      Dec(OutPos);
      OutBuf[OutPos] := OutBuf[OutPos + Offset];
    end;
  end;

var
  Bit1, Bit2, B1, B2: Byte;
  LitCount, Val: LongWord;
  I: Integer;
  Mode, SubMode: LongWord;
  Dist, LengthVal: LongWord;
  HasDist: Boolean;
  LastByte: Byte;
  Shift: LongWord;
  HeaderFound: Boolean;
begin
  Result := False;
  SetLength(OutBuf, 0);
  if (InBuf = nil) or (InSize < 16) then Exit;

  HeaderFound := False;
  PayloadStart := 0;
  PayloadEnd := 0;
  UnpackedSize := 0;

  // 1. Direct 'FIRE' header at offset 0 (Raw data)
  if (InBuf[0] = Byte('F')) and (InBuf[1] = Byte('I')) and 
     (InBuf[2] = Byte('R')) and (InBuf[3] = Byte('E')) then
  begin
    PLen := ReadBE32(InBuf, 4);
    ULen := ReadBE32(InBuf, 8);
    if (PLen >= 12) and (ULen > 0) and (ULen <= 16 * 1024 * 1024) then
    begin
      PayloadStart := 12;
      PayloadEnd := Min(Integer(PLen), InSize);
      UnpackedSize := ULen;
      HeaderFound := True;
    end;
  end;

  // 2. Embedded 'FIRE' header (e.g. in Executables like Pack-Fire)
  if not HeaderFound then
  begin
    for Off := 0 to Min(InSize - 16, 4096) do
    begin
      if (InBuf[Off] = Byte('F')) and (InBuf[Off + 1] = Byte('I')) and 
         (InBuf[Off + 2] = Byte('R')) and (InBuf[Off + 3] = Byte('E')) then
      begin
        PLen := ReadBE32(InBuf, Off + 4);
        ULen := ReadBE32(InBuf, Off + 8);
        if (PLen >= 12) and (ULen > 0) and (ULen <= 16 * 1024 * 1024) and 
           (Off + Integer(PLen) <= InSize + 16) then
        begin
          PayloadStart := Off + 12;
          PayloadEnd := Min(Off + Integer(PLen), InSize);
          UnpackedSize := ULen;
          HeaderFound := True;
          Break;
        end;
      end;
    end;
  end;

  if not HeaderFound then Exit(False);

  SetLength(OutBuf, UnpackedSize);
  OutPos := UnpackedSize;
  InPos := PayloadEnd;

  // Initial bitstream priming (68k: move.b -(a5), d7)
  Dec(InPos);
  BitBuf := InBuf[InPos];
  Extend := 0;

  while OutPos > 0 do
  begin
    Bit1 := GetBit;
    if Bit1 <> 0 then
    begin
      // Literals run
      Bit2 := GetBit;
      if Bit2 = 0 then
        LitCount := 1
      else
      begin
        Val := ReadBits(2);
        if Val <> 3 then
          LitCount := Val + 1 + 1
        else
        begin
          Val := ReadBits(2);
          if Val <> 3 then
            LitCount := Val + 4 + 1
          else
          begin
            Val := ReadBits(3);
            if Val <> 7 then
              LitCount := Val + 7 + 1
            else
            begin
              Val := ReadBits(10);
              LitCount := Val + 14 + 1;
            end;
          end;
        end;
      end;

      for I := 1 to LitCount do
      begin
        if (InPos <= PayloadStart) or (OutPos <= 0) then
          Exit(False);
        Dec(InPos);
        Dec(OutPos);
        OutBuf[OutPos] := InBuf[InPos];
      end;
    end;

    if OutPos <= 0 then Break;

    Mode := ReadBits(2);
    case Mode of
      0:
      begin
        SubMode := ReadBits(2);
        if SubMode = 0 then
        begin
          // RLE of last written byte
          Dist := ReadDistance;
          if Dist > 0 then
          begin
            if OutPos >= Integer(UnpackedSize) then Exit(False);
            LastByte := OutBuf[OutPos];
            for I := 0 to Dist do
            begin
              if OutPos <= 0 then Exit(False);
              Dec(OutPos);
              OutBuf[OutPos] := LastByte;
            end;
          end;
        end
        else
        begin
          case SubMode of
            1: LengthVal := ReadBits(4) + 5;
            2: LengthVal := ReadBits(6) + 21;
          else
            LengthVal := ReadBits(8) + 85;
          end;
          Dist := ReadDistance;
          CopyMatch(LengthVal, Dist);
        end;
      end;

      1:
      begin
        Dist := ReadBits(8);
        CopyMatch(2, Dist);
      end;

      2:
      begin
        HasDist := ReadOptDistance(Dist);
        if not HasDist then
        begin
          if OutPos <= 0 then Exit(False);
          Dec(OutPos);
          OutBuf[OutPos] := 0;
        end
        else
          CopyMatch(3, Dist);
      end;

      3:
      begin
        B1 := GetBit;
        if B1 = 0 then
        begin
          Shift := ReadBits(3);
          if OutPos <= 0 then Exit(False);
          Dec(OutPos);
          OutBuf[OutPos] := Byte(1 shl Shift);
        end
        else
        begin
          B2 := GetBit;
          if B2 = 0 then
          begin
            HasDist := ReadOptDistance(Dist);
            if not HasDist then
            begin
              if OutPos >= Integer(UnpackedSize) then Exit(False);
              LastByte := OutBuf[OutPos];
              if OutPos <= 0 then Exit(False);
              Dec(OutPos);
              OutBuf[OutPos] := LastByte;
            end
            else
              CopyMatch(4, Dist);
          end
          else
          begin
            HasDist := ReadOptDistance(Dist);
            if not HasDist then
            begin
              if OutPos <= 0 then Exit(False);
              Dec(OutPos);
              OutBuf[OutPos] := $FF;
            end
            else
              CopyMatch(5, Dist);
          end;
        end;
      end;
    end;
  end;

  Result := (OutPos = 0);
end;

// -----------------------------------------------------------------------------
// STOS Squasher Decompressor (unsquash)
// -----------------------------------------------------------------------------
function DecompressStosSquash(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
var
  Offset, UncompressedSize, Checksum, BitBuf, NextWord, Chk: LongWord;
  OutPtr, Count, Dist, Val, Mode, NBits, I: Integer;

  function GetBit: Byte;
  begin
    Result := BitBuf and 1;
    BitBuf := BitBuf shr 1;
    if BitBuf = 0 then
    begin
      if Offset < 4 then Exit(0);
      Dec(Offset, 4);
      NextWord := ReadBE32(InBuf, Offset);
      Chk := Chk xor NextWord;
      Result := NextWord and 1;
      BitBuf := (NextWord shr 1) or $80000000;
    end;
  end;

  function GetBits(N: Integer): LongWord;
  var
    J: Integer;
  begin
    Result := 0;
    for J := 1 to N do
      Result := (Result shl 1) or GetBit;
  end;

begin
  Result := False;
  SetLength(OutBuf, 0);
  if (InBuf = nil) or (InSize < 16) then Exit;

  Offset := InSize;
  Dec(Offset, 4);
  UncompressedSize := ReadBE32(InBuf, Offset);
  if (UncompressedSize = 0) or (UncompressedSize > 10 * 1024 * 1024) then Exit;

  Dec(Offset, 4);
  Checksum := ReadBE32(InBuf, Offset);
  Dec(Offset, 4);
  BitBuf := ReadBE32(InBuf, Offset);

  Chk := BitBuf xor Checksum;

  SetLength(OutBuf, UncompressedSize);
  OutPtr := UncompressedSize;

  try
    while OutPtr > 0 do
    begin
      if GetBit = 0 then
      begin
        if GetBit = 1 then
        begin
          Dist := GetBits(8);
          for I := 1 to 2 do
          begin
            Dec(OutPtr);
            if (OutPtr + Dist >= Integer(UncompressedSize)) then Exit(False);
            OutBuf[OutPtr] := OutBuf[OutPtr + Dist];
          end;
        end
        else
        begin
          Count := GetBits(3);
          for I := 0 to Count do
          begin
            Val := GetBits(8);
            Dec(OutPtr);
            OutBuf[OutPtr] := Byte(Val);
          end;
        end;
      end
      else
      begin
        Mode := GetBits(2);
        if Mode < 2 then
        begin
          NBits := 9 + Mode;
          Count := Mode + 2;
          Dist := GetBits(NBits);
          for I := 0 to Count do
          begin
            Dec(OutPtr);
            if (OutPtr + Dist >= Integer(UncompressedSize)) then Exit(False);
            OutBuf[OutPtr] := OutBuf[OutPtr + Dist];
          end;
        end
        else if Mode = 2 then
        begin
          Count := GetBits(8);
          Dist := GetBits(12);
          for I := 0 to Count do
          begin
            Dec(OutPtr);
            if (OutPtr + Dist >= Integer(UncompressedSize)) then Exit(False);
            OutBuf[OutPtr] := OutBuf[OutPtr + Dist];
          end;
        end
        else
        begin
          Count := GetBits(8) + 8;
          for I := 0 to Count do
          begin
            Val := GetBits(8);
            Dec(OutPtr);
            OutBuf[OutPtr] := Byte(Val);
          end;
        end;
      end;
    end;
    Result := (OutPtr = 0);
  except
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------
// STOS Picture Compactor Decompressor ($06071963)
// -----------------------------------------------------------------------------
function DecompressStosCompact(const InBuf: PByte; InSize: Integer; out OutPalette: TBytes; out OutVRAM: TBytes; out OutRes: Integer): Boolean;
var
  Res, XBlk, YBlk, LinesPerBlk, NumPlanes, LineStride, TileStride: Integer;
  OffMask1, OffMask2, A4, A5, A6: LongWord;
  D0, D1, D2, D3: Integer;
  Plane, PlaneOffset, By, Bx, BlockYOffset, BlockXOffset, TileBase: Integer;
  ByteIdx, Line, Dest: Integer;
begin
  Result := False;
  SetLength(OutPalette, 0);
  SetLength(OutVRAM, 0);
  OutRes := 0;

  if (InBuf = nil) or (InSize < $46) then Exit;
  if ReadBE32(InBuf, 0) <> $06071963 then Exit;

  Res := ReadBE16(InBuf, 4);
  OutRes := Res;
  XBlk := ReadBE16(InBuf, 10);
  YBlk := ReadBE16(InBuf, 12);
  LinesPerBlk := ReadBE16(InBuf, 16);
  OffMask1 := ReadBE32(InBuf, 20);
  OffMask2 := ReadBE32(InBuf, 24);

  if (OffMask1 >= LongWord(InSize)) or (OffMask2 >= LongWord(InSize)) then Exit;

  // Extract 16 palette words (32 bytes) at offset $26
  SetLength(OutPalette, 32);
  Move(InBuf[$26], OutPalette[0], 32);

  // Screen size: 32000 bytes
  SetLength(OutVRAM, 32000);
  FillChar(OutVRAM[0], 32000, 0);

  if Res = 0 then
  begin
    NumPlanes := 4;
    TileStride := 8;
  end
  else if Res = 1 then
  begin
    NumPlanes := 2;
    TileStride := 4;
  end
  else
  begin
    NumPlanes := 1;
    TileStride := 2;
  end;
  LineStride := 160;

  A4 := $46;
  A5 := OffMask1;
  A6 := OffMask2;

  D0 := 7;
  D1 := 7;

  if A5 >= LongWord(InSize) then Exit(False);
  D2 := InBuf[A5];
  Inc(A5);
  if A4 >= LongWord(InSize) then Exit(False);
  D3 := InBuf[A4];
  Inc(A4);

  if A6 < LongWord(InSize) then
    if (InBuf[A6] and (1 shl D1)) <> 0 then
    begin
      if A5 < LongWord(InSize) then
      begin
        D2 := InBuf[A5];
        Inc(A5);
      end;
    end;
  Dec(D1);
  if D1 < 0 then
  begin
    D1 := 7;
    Inc(A6);
  end;

  try
    for Plane := 0 to NumPlanes - 1 do
    begin
      PlaneOffset := Plane * 2;
      for By := 0 to YBlk - 1 do
      begin
        BlockYOffset := By * (LinesPerBlk * LineStride);
        for Bx := 0 to XBlk - 1 do
        begin
          BlockXOffset := Bx * TileStride;
          TileBase := BlockYOffset + BlockXOffset + PlaneOffset;

          for ByteIdx := 0 to 1 do
          begin
            for Line := 0 to LinesPerBlk - 1 do
            begin
              if (D2 and (1 shl D0)) <> 0 then
              begin
                if A4 < LongWord(InSize) then
                begin
                  D3 := InBuf[A4];
                  Inc(A4);
                end;
              end;
              Dest := TileBase + Line * LineStride + ByteIdx;
              if (Dest >= 0) and (Dest < 32000) then
                OutVRAM[Dest] := Byte(D3);

              Dec(D0);
              if D0 < 0 then
              begin
                D0 := 7;
                if A6 < LongWord(InSize) then
                  if (InBuf[A6] and (1 shl D1)) <> 0 then
                  begin
                    if A5 < LongWord(InSize) then
                    begin
                      D2 := InBuf[A5];
                      Inc(A5);
                    end;
                  end;
                Dec(D1);
                if D1 < 0 then
                begin
                  D1 := 7;
                  Inc(A6);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------
// STAD Monochrome Decompressor (pM85 / pM86)
// -----------------------------------------------------------------------------
function DecompressStad(const InBuf: PByte; InSize: Integer; out OutScreen: TBytes): Boolean;
var
  IsVert: Boolean;
  IdByte, PackByte, SpecialByte, CurrByte, RunVal: Byte;
  RunCount, InIdx, OutIdx, X, Y: Integer;
  RawBuf: TBytes;
begin
  Result := False;
  SetLength(OutScreen, 0);
  if (InBuf = nil) or (InSize < 7) then Exit;

  if CheckString(InBuf, InSize, 0, 'pM85') then
    IsVert := False
  else if CheckString(InBuf, InSize, 0, 'pM86') then
    IsVert := True
  else
    Exit(False);

  IdByte := InBuf[4];
  PackByte := InBuf[5];
  SpecialByte := InBuf[6];

  SetLength(RawBuf, 32000);
  FillChar(RawBuf[0], 32000, 0);
  InIdx := 7;
  OutIdx := 0;

  try
    while (InIdx < InSize) and (OutIdx < 32000) do
    begin
      CurrByte := InBuf[InIdx];
      Inc(InIdx);

      if CurrByte = IdByte then
      begin
        if InIdx >= InSize then Break;
        RunCount := InBuf[InIdx] + 1;
        Inc(InIdx);
        while (RunCount > 0) and (OutIdx < 32000) do
        begin
          RawBuf[OutIdx] := PackByte;
          Inc(OutIdx);
          Dec(RunCount);
        end;
      end
      else if CurrByte = SpecialByte then
      begin
        if InIdx + 1 >= InSize then Break;
        RunVal := InBuf[InIdx];
        Inc(InIdx);
        RunCount := InBuf[InIdx];
        Inc(InIdx);
        while (RunCount > 0) and (OutIdx < 32000) do
        begin
          RawBuf[OutIdx] := RunVal;
          Inc(OutIdx);
          Dec(RunCount);
        end;
      end
      else
      begin
        RawBuf[OutIdx] := CurrByte;
        Inc(OutIdx);
      end;
    end;

    SetLength(OutScreen, 32000);
    if IsVert then
    begin
      for X := 0 to 79 do
        for Y := 0 to 399 do
          OutScreen[Y * 80 + X] := RawBuf[X * 400 + Y];
    end
    else
    begin
      Move(RawBuf[0], OutScreen[0], 32000);
    end;

    Result := True;
  except
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------
// Tiny Stuff Decompressor (.TN1, .TN2, .TN3, .TNY)
// -----------------------------------------------------------------------------
function DecompressTiny(const InBuf: PByte; InSize: Integer; DefaultRes: Integer;
  out OutPalette: TBytes; out OutVRAM: TBytes; out OutRes: Integer): Boolean;
var
  Res, Offset, CtrlStreamSize, A5, A5End, A4, A1, D6, D4: Integer;
  Ctrl, Count: Integer;
  IsRepeat: Boolean;
  ValWord: Word;
  I: Integer;

  procedure Advance;
  begin
    Inc(A1, 2);
    Inc(A1, 158);
    if A1 >= D6 then
    begin
      Inc(A1, -31992);
      if A1 >= D4 then
        Inc(A1, -158);
    end;
  end;

begin
  Result := False;
  SetLength(OutPalette, 0);
  SetLength(OutVRAM, 0);
  OutRes := 0;

  if (InBuf = nil) or (InSize < 37) then Exit;

  Res := InBuf[0];
  Offset := 1;
  if Res > 2 then
    Inc(Offset, 4);

  if Offset + 32 + 4 > InSize then Exit;

  // Palette: 32 bytes (16 words)
  SetLength(OutPalette, 32);
  Move(InBuf[Offset], OutPalette[0], 32);
  Inc(Offset, 32);

  // Control stream size
  CtrlStreamSize := (Word(InBuf[Offset]) shl 8) or Word(InBuf[Offset + 1]);
  Inc(Offset, 4); // size (2 bytes) + 2 padding bytes

  if Offset + CtrlStreamSize > InSize then Exit;

  A5 := Offset;
  A5End := Offset + CtrlStreamSize;
  A4 := A5End;

  SetLength(OutVRAM, 32000);
  FillChar(OutVRAM[0], 32000, 0);
  A1 := 0;
  D6 := 32000;
  D4 := 160;

  try
    while A5 < A5End do
    begin
      Ctrl := InBuf[A5];
      Inc(A5);

      if Ctrl = 0 then
      begin
        if A5 + 1 >= A5End then Break;
        Count := (Word(InBuf[A5]) shl 8) or Word(InBuf[A5 + 1]);
        Inc(A5, 2);
        IsRepeat := True;
      end
      else if Ctrl = 1 then
      begin
        if A5 + 1 >= A5End then Break;
        Count := (Word(InBuf[A5]) shl 8) or Word(InBuf[A5 + 1]);
        Inc(A5, 2);
        IsRepeat := False;
      end
      else if Ctrl >= 128 then
      begin
        Count := 256 - Ctrl;
        IsRepeat := False;
      end
      else
      begin
        Count := Ctrl;
        IsRepeat := True;
      end;

      if IsRepeat then
      begin
        if A4 + 1 >= InSize then Break;
        ValWord := (Word(InBuf[A4]) shl 8) or Word(InBuf[A4 + 1]);
        Inc(A4, 2);
        for I := 0 to Count - 1 do
        begin
          if A1 >= 32000 then Break;
          OutVRAM[A1] := Byte((ValWord shr 8) and $FF);
          OutVRAM[A1 + 1] := Byte(ValWord and $FF);
          Advance;
        end;
      end
      else
      begin
        for I := 0 to Count - 1 do
        begin
          if A4 + 1 >= InSize then Break;
          ValWord := (Word(InBuf[A4]) shl 8) or Word(InBuf[A4 + 1]);
          Inc(A4, 2);
          if A1 >= 32000 then Break;
          OutVRAM[A1] := Byte((ValWord shr 8) and $FF);
          OutVRAM[A1 + 1] := Byte(ValWord and $FF);
          Advance;
        end;
      end;
    end;

    if DefaultRes in [0, 1, 2] then
      OutRes := DefaultRes
    else if Res <= 2 then
      OutRes := Res
    else
      OutRes := 0;

    Result := True;
  except
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------
// Master Depacker Dispatcher
// -----------------------------------------------------------------------------

function DepackBuffer(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes; out DetectedPacker: string): Boolean;
var
  DummyPal, DummyVRAM: TBytes;
  DummyRes: Integer;
begin
  Result := False;
  SetLength(OutBuf, 0);
  DetectedPacker := 'None';
  if (InBuf = nil) or (InSize < 4) then Exit;

  try
    DetectedPacker := DetectPackerSignature(InBuf, InSize);

    // 1. Pack-Ice
  if Pos('Pack-Ice', DetectedPacker) > 0 then
  begin
    if DecompressPackIce(InBuf, InSize, OutBuf) then Exit(True);
    if DecompressJam(InBuf, InSize, OutBuf) then begin DetectedPacker := 'JAM Packer Executable'; Exit(True); end;
    if DecompressJek(InBuf, InSize, OutBuf) then begin DetectedPacker := 'JEK Packer Executable'; Exit(True); end;
    if DecompressAutomation(InBuf, InSize, OutBuf) then begin DetectedPacker := 'Automation Packer Executable'; Exit(True); end;
  end;

  // 2. Atomik Cruncher
  if Pos('Atomik', DetectedPacker) > 0 then
  begin
    if DecompressAtomik(InBuf, InSize, OutBuf) then Exit(True);
    if DecompressThunderV2(InBuf, InSize, OutBuf) then begin DetectedPacker := 'Thunder V2 Executable'; Exit(True); end;
    if DecompressThunderV1(InBuf, InSize, OutBuf) then begin DetectedPacker := 'Thunder V1 Executable'; Exit(True); end;
  end;

  // 3. Automation Packer
  if Pos('Automation', DetectedPacker) > 0 then
  begin
    if DecompressJam(InBuf, InSize, OutBuf) then begin DetectedPacker := 'JAM Packer Executable'; Exit(True); end;
    if DecompressAutomation(InBuf, InSize, OutBuf) then Exit(True);
    if DecompressPackIce(InBuf, InSize, OutBuf) then begin DetectedPacker := 'Pack-Ice Executable'; Exit(True); end;
  end;

  // 4. JAM Packer
  if Pos('JAM', DetectedPacker) > 0 then
  begin
    if DecompressJam(InBuf, InSize, OutBuf) then Exit(True);
    if DecompressPackIce(InBuf, InSize, OutBuf) then begin DetectedPacker := 'Pack-Ice Executable'; Exit(True); end;
    if DecompressAutomation(InBuf, InSize, OutBuf) then begin DetectedPacker := 'Automation Packer Executable'; Exit(True); end;
  end;

  // 5. Rob Northen Compression
  if (Pos('Rob Northen', DetectedPacker) > 0) or (Pos('RNC', DetectedPacker) > 0) then
  begin
    if DecompressRNC(InBuf, InSize, OutBuf) then Exit(True);
  end;

  // 6. Thunder Packer
  if Pos('Thunder', DetectedPacker) > 0 then
  begin
    if DecompressThunderV2(InBuf, InSize, OutBuf) then Exit(True);
    if DecompressThunderV1(InBuf, InSize, OutBuf) then Exit(True);
    if DecompressAtomik(InBuf, InSize, OutBuf) then begin DetectedPacker := 'Atomik Cruncher Executable'; Exit(True); end;
  end;

  // 7. JEK Packer / Byte Killer
  if (Pos('JEK', DetectedPacker) > 0) or (Pos('Bytekiller', DetectedPacker) > 0) or (Pos('Byte Killer', DetectedPacker) > 0) then
  begin
    if DecompressJam(InBuf, InSize, OutBuf) then begin DetectedPacker := 'JAM Packer Executable'; Exit(True); end;
    if DecompressJek(InBuf, InSize, OutBuf) then Exit(True);
    if DecompressPackIce(InBuf, InSize, OutBuf) then begin DetectedPacker := 'Pack-Ice Executable'; Exit(True); end;
    if DecompressAutomation(InBuf, InSize, OutBuf) then begin DetectedPacker := 'Automation Packer Executable'; Exit(True); end;
  end;

  // 8. Degas Elite RLE
  if Pos('Degas', DetectedPacker) > 0 then
  begin
    if DecompressRLE(InBuf, InSize, OutBuf) then Exit(True);
  end;

  // 9. FIRE Packer
  if (Pos('FIRE', UpperCase(DetectedPacker)) > 0) or (Pos('PACK-FIRE', UpperCase(DetectedPacker)) > 0) then
  begin
    if DecompressFire(InBuf, InSize, OutBuf) then Exit(True);
  end;

  // 10. STOS Squasher / Picture Compactor
  if (Pos('STOS', UpperCase(DetectedPacker)) > 0) or (Pos('SQUASH', UpperCase(DetectedPacker)) > 0) or (Pos('COMPACTOR', UpperCase(DetectedPacker)) > 0) then
  begin
    if DecompressStosSquash(InBuf, InSize, OutBuf) then
    begin
      if (Length(OutBuf) >= 4) and (ReadBE32(@OutBuf[0], 0) = $06071963) then
      begin
        if DecompressStosCompact(@OutBuf[0], Length(OutBuf), DummyPal, DummyVRAM, DummyRes) then
        begin
          SetLength(OutBuf, 32034);
          OutBuf[0] := 0; OutBuf[1] := Byte(DummyRes);
          Move(DummyPal[0], OutBuf[2], 32);
          Move(DummyVRAM[0], OutBuf[34], 32000);
          DetectedPacker := 'STOS Packed Picture';
        end;
      end;
      Exit(True);
    end;

    if CheckLong(InBuf, InSize, 0, $06071963) then
    begin
      if DecompressStosCompact(InBuf, InSize, DummyPal, DummyVRAM, DummyRes) then
      begin
        SetLength(OutBuf, 32034);
        OutBuf[0] := 0; OutBuf[1] := Byte(DummyRes);
        Move(DummyPal[0], OutBuf[2], 32);
        Move(DummyVRAM[0], OutBuf[34], 32000);
        DetectedPacker := 'STOS Picture Compactor';
        Exit(True);
      end;
    end;
  end;

  // 11. STAD Monochrome Picture
  if (Pos('STAD', UpperCase(DetectedPacker)) > 0) or CheckString(InBuf, InSize, 0, 'pM85') or CheckString(InBuf, InSize, 0, 'pM86') then
  begin
    if DecompressStad(InBuf, InSize, OutBuf) then Exit(True);
  end;

  // 12. Tiny Stuff Image
  if Pos('TINY', UpperCase(DetectedPacker)) > 0 then
  begin
    if DecompressTiny(InBuf, InSize, 0, DummyPal, DummyVRAM, DummyRes) then
    begin
      SetLength(OutBuf, 32034);
      OutBuf[0] := 0; OutBuf[1] := Byte(DummyRes);
      Move(DummyPal[0], OutBuf[2], 32);
      Move(DummyVRAM[0], OutBuf[34], 32000);
      Exit(True);
    end;
  end;

  // Generic fallback tries if specific packer failed or was unknown
  if DecompressPackIce(InBuf, InSize, OutBuf) then
  begin
    DetectedPacker := 'Pack-Ice (Auto-detected)';
    Exit(True);
  end;

  if DecompressJam(InBuf, InSize, OutBuf) then
  begin
    DetectedPacker := 'JAM Packer (Auto-detected)';
    Exit(True);
  end;

  if DecompressFire(InBuf, InSize, OutBuf) then
  begin
    DetectedPacker := 'FIRE Packer (Auto-detected)';
    Exit(True);
  end;

  if DecompressJek(InBuf, InSize, OutBuf) then
  begin
    DetectedPacker := 'JEK Packer (Auto-detected)';
    Exit(True);
  end;

  if DecompressAutomation(InBuf, InSize, OutBuf) then
  begin
    DetectedPacker := 'Automation Packer (Auto-detected)';
    Exit(True);
  end;

  if DecompressAtomik(InBuf, InSize, OutBuf) then
  begin
    DetectedPacker := 'Atomik Cruncher (Auto-detected)';
    Exit(True);
  end;

  if DecompressRNC(InBuf, InSize, OutBuf) then
  begin
    DetectedPacker := 'Rob Northen Compression (Auto-detected)';
    Exit(True);
  end;

  SetLength(OutBuf, 0);
  except
    SetLength(OutBuf, 0);
    Result := False;
  end;
end;


function DepackStream(InStream: TStream; OutStream: TStream; out DetectedPacker: string): Boolean;
var
  InBuf, OutBuf: TBytes;
  SavedPos: Int64;
begin
  Result := False;
  DetectedPacker := 'None';
  if (InStream = nil) or (OutStream = nil) or (InStream.Size = 0) then Exit;

  SavedPos := InStream.Position;
  try
    InStream.Position := 0;
    SetLength(InBuf, InStream.Size);
    InStream.ReadBuffer(InBuf[0], InStream.Size);

    if DepackBuffer(@InBuf[0], Length(InBuf), OutBuf, DetectedPacker) then
    begin
      OutStream.Size := 0;
      OutStream.WriteBuffer(OutBuf[0], Length(OutBuf));
      OutStream.Position := 0;
      Result := True;
    end;
  finally
    InStream.Position := SavedPos;
  end;
end;

end.
