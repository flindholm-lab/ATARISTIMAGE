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
function DecompressRLE(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;

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
  IsExec, IsSwapped: Boolean;
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

  // 10. ICE Packer / Superior
  if CheckLastLongString(Buffer, Size, 'Ice!') then
    Exit('ICE Packer v1.1 Raw Data');
  if CheckString(Buffer, Size, 0, 'Ice!') then
    Exit('Pack-Ice v2.0/v2.2 Raw Data');
  if CheckString(Buffer, Size, 0, 'ICE!') then
    Exit('Pack-Ice v2.3/v2.4 Raw Data');
  if CheckLong(Buffer, Size, 70, $610000A0) then
    Exit('Pack-Ice v1.1 Executable Packer');
  if CheckWord(Buffer, Size, 54, $2E3C) then
    Exit('Pack-Ice v2.0 Executable Packer');
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

  // 32. Heuristics fallback scanning on first 8192 bytes
  AsciiContext := BufferToAscii(Buffer, Size, 8192);

  if Pos('THE MEDWAY BOYS', AsciiContext) > 0 then Exit('The Medway Boys Packer');
  if Pos('ICE!', AsciiContext) > 0 then Exit('Pack-Ice v2.4 Executable Packer');
  if Pos('Ice!', AsciiContext) > 0 then Exit('Pack-Ice v2.1 Executable Packer');
  if Pos('Pack-Ice', AsciiContext) > 0 then Exit('Pack-Ice v2.1 Executable Packer');
  if (Pos('DPAK', AsciiContext) > 0) or (Pos('NEW_DPAK', AsciiContext) > 0) or (Pos('XERUD', AsciiContext) > 0) then
    Exit('Mike Watson (Xerud) NEW_DPAK');
  if (Pos('JAM Packer', AsciiContext) > 0) or (Pos('The JAM Packer', AsciiContext) > 0) or (Pos('JAM PACKER', AsciiContext) > 0) then
    Exit('The JAM Packer Executable');
  if (Pos('ATOMIC', AsciiContext) > 0) or (Pos('THUNDER', AsciiContext) > 0) then
    Exit('JPM Thunder Packer / ATOMIC');

  if (Pos('ATOM', AsciiContext) > 0) or (Pos('ATM5', AsciiContext) > 0) or
     (Pos('ATM3', AsciiContext) > 0) or (Pos('Atomik', AsciiContext) > 0) then
  begin
    Idx := Pos('ATOM', AsciiContext);
    if (Idx > 0) and (Idx - 1 + 12 <= Size) then
    begin
      ULen := ReadBE32(Buffer, Idx - 1 + 4);
      PLen := ReadBE32(Buffer, Idx - 1 + 8);
      if (ULen > 0) and (ULen < 16777216) and (Idx - 1 + PLen <= Size) then
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
        Exit('Atomik Cruncher Executable Packer');
      end;
    end;
    Exit('Atomik Cruncher Executable Packer');
  end;

  if Pos('AUTOMATION PACKER V2.3', AsciiContext) > 0 then Exit('Automation 2.3 Packer');
  if (Pos('AUTOMATION PACKER', AsciiContext) > 0) or (Pos('Automation', AsciiContext) > 0) or (Pos('COMPACTER', AsciiContext) > 0) then
    Exit('Automation Compacter');
  if (Pos('POMPEY PIRATES', AsciiContext) > 0) or (Pos('POMPEY', AsciiContext) > 0) then Exit('Pompey Pirates Packer');
  if (Pos('JEK PACKER', AsciiContext) > 0) or (Pos('JEK!', AsciiContext) > 0) then Exit('JEK Packer / Byte Killer');

  // Tail-end scanning fallback
  if Size > 1024 then
    TailAscii := BufferToAscii(@Buffer[Size - 1024], 1024, 1024)
  else
    TailAscii := AsciiContext;

  if Pos('THE MEDWAY BOYS', TailAscii) > 0 then Exit('The Medway Boys Packer');
  if Pos('JPM!', TailAscii) > 0 then Exit('Bytekiller (JPM Data) Raw Data');
  if Pos('GUS*', TailAscii) > 0 then Exit('Gremlin Packer Raw Data');
  if (Pos('JEK!', TailAscii) > 0) or (Pos('JEK PACKER', TailAscii) > 0) then Exit('JEK Packer / Byte Killer');

  // Atari ST Executable Header fallback (BRA $1A / $1C)
  IsExec := (Buffer[0] = $60) and ((Buffer[1] = $1A) or (Buffer[1] = $1C) or (Buffer[1] = $00));
  IsSwapped := (Size >= 4) and (Buffer[0] = $1A) and (Buffer[1] = $00) and (Buffer[3] = $61);
  if IsExec or IsSwapped then
    Exit('Atari ST Executable');
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

// -----------------------------------------------------------------------------
// Pack-Ice Bit Reader and Decompressor
// -----------------------------------------------------------------------------

type
  TIceBitReader = class
  public
    function GetBit: Byte; virtual; abstract;
    function GetBits(N: Integer): LongWord;
    function ReadRawByte: Byte; virtual; abstract;
  end;

function TIceBitReader.GetBits(N: Integer): LongWord;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to N do
    Result := (Result shl 1) or GetBit;
end;

type
  TIceBitReader24 = class(TIceBitReader)
  private
    FBuffer: PByte;
    FSrcIdx: Integer;
    FD7: Byte;
  public
    constructor Create(ABuffer: PByte; AStartIdx: Integer);
    function GetBit: Byte; override;
    function ReadRawByte: Byte; override;
  end;

constructor TIceBitReader24.Create(ABuffer: PByte; AStartIdx: Integer);
begin
  inherited Create;
  FBuffer := ABuffer;
  FSrcIdx := AStartIdx;
  if FSrcIdx > 0 then
  begin
    Dec(FSrcIdx);
    FD7 := FBuffer[FSrcIdx];
  end
  else
    FD7 := 0;
end;

function TIceBitReader24.GetBit: Byte;
var
  NewByte: Byte;
begin
  Result := (FD7 shr 7) and 1;
  FD7 := (FD7 shl 1) and $FF;
  if FD7 = 0 then
  begin
    if FSrcIdx <= 0 then raise Exception.Create('PackIce bitstream OOB');
    Dec(FSrcIdx);
    NewByte := FBuffer[FSrcIdx];
    Result := (NewByte shr 7) and 1;
    FD7 := ((NewByte shl 1) or 1) and $FF;
  end;
end;

function TIceBitReader24.ReadRawByte: Byte;
begin
  if FSrcIdx <= 0 then raise Exception.Create('PackIce raw byte OOB');
  Dec(FSrcIdx);
  Result := FBuffer[FSrcIdx];
end;

type
  TIceBitReader21 = class(TIceBitReader)
  private
    FBuffer: PByte;
    FSrcIdx: Integer;
    FD7: LongWord;
  public
    constructor Create(ABuffer: PByte; AStartIdx: Integer);
    function GetBit: Byte; override;
    function ReadRawByte: Byte; override;
  end;

constructor TIceBitReader21.Create(ABuffer: PByte; AStartIdx: Integer);
begin
  inherited Create;
  FBuffer := ABuffer;
  FSrcIdx := AStartIdx;
  if FSrcIdx >= 4 then
  begin
    Dec(FSrcIdx, 4);
    FD7 := ReadBE32(FBuffer, FSrcIdx);
  end
  else
    FD7 := 0;
end;

function TIceBitReader21.GetBit: Byte;
var
  NewWord: LongWord;
begin
  Result := (FD7 shr 31) and 1;
  FD7 := (FD7 shl 1);
  if FD7 = 0 then
  begin
    if FSrcIdx < 4 then raise Exception.Create('PackIce bitstream OOB');
    Dec(FSrcIdx, 4);
    NewWord := ReadBE32(FBuffer, FSrcIdx);
    Result := (NewWord shr 31) and 1;
    FD7 := (NewWord shl 1) or 1;
  end;
end;

function TIceBitReader21.ReadRawByte: Byte;
begin
  if FSrcIdx <= 0 then raise Exception.Create('PackIce raw byte OOB');
  Dec(FSrcIdx);
  Result := FBuffer[FSrcIdx];
end;

function DecompressPackIceInternal(const Buffer: PByte; TotalSize: Integer;
  StartPos: Integer; IsIce21: Boolean; OrigSize: Integer; InitialSrcIdx: Integer;
  out OutBuf: TBytes): Boolean;
const
  LenBits: array[0..5] of Integer = (1, 2, 2, 3, 8, 15);
  MaxLen: array[0..5] of Integer = (1, 3, 3, 7, 255, 32767);
  LiteralOffset: array[0..5] of Integer = (1, 2, 5, 8, 15, 270);
  ExtraBits1: array[0..4] of Integer = (0, 0, 1, 2, 10);
  SldOffset1: array[0..4] of Integer = (0, 1, 2, 4, 8);
  ExtraBits2: array[0..2] of Integer = (8, 5, 12);
  SldOffset2: array[0..2] of Integer = (32, 0, 288);
var
  BR: TIceBitReader;
  PIdx, TablePos, TablePos2, Len, LenRaw, PosVal, CopyDistance, SrcCopy: Integer;
  I, J, K, PicIdx, PicIters: Integer;
  D0, D1, D2, D3, D4: Word;
begin
  Result := False;
  SetLength(OutBuf, 0);
  if (OrigSize <= 0) or (OrigSize > 16 * 1024 * 1024) then Exit;

  SetLength(OutBuf, OrigSize);
  PIdx := OrigSize;

  if IsIce21 then
    BR := TIceBitReader21.Create(@Buffer[StartPos], InitialSrcIdx)
  else
    BR := TIceBitReader24.Create(@Buffer[StartPos], InitialSrcIdx);

  try
    try
      while PIdx > 0 do
      begin
        if BR.GetBit = 1 then
        begin
          // Literal Copy
          TablePos := 0;
          Len := 0;
          while TablePos < 6 do
          begin
            Len := BR.GetBits(LenBits[TablePos]);
            if (Len <> MaxLen[TablePos]) or (TablePos = 5) then
            begin
              Inc(Len, LiteralOffset[TablePos]);
              Break;
            end;
            Inc(TablePos);
          end;

          if PIdx < Len then Len := PIdx;
          for I := 1 to Len do
          begin
            Dec(PIdx);
            OutBuf[PIdx] := BR.ReadRawByte;
          end;
        end;

        if PIdx <= 0 then Break;

        // Match Copy
        TablePos := 0;
        while (TablePos < 4) and (BR.GetBit = 1) do Inc(TablePos);
        LenRaw := SldOffset1[TablePos] + BR.GetBits(ExtraBits1[TablePos]);
        PosVal := 0;

        if LenRaw <> 0 then
        begin
          TablePos2 := 0;
          while (TablePos2 < 2) and (BR.GetBit = 1) do Inc(TablePos2);
          PosVal := SldOffset2[TablePos2] + BR.GetBits(ExtraBits2[TablePos2]);
          if (not IsIce21) and (PosVal <> 0) then
            Inc(PosVal, LenRaw);
        end
        else
        begin
          if BR.GetBit = 1 then
            PosVal := 64 + BR.GetBits(9)
          else
            PosVal := BR.GetBits(6);
        end;

        Len := LenRaw + 2;
        if IsIce21 then
          CopyDistance := PosVal + Len
        else
          CopyDistance := PosVal + 1;

        if PIdx < Len then Len := PIdx;
        for I := 1 to Len do
        begin
          Dec(PIdx);
          SrcCopy := PIdx + CopyDistance;
          if (PIdx < 0) or (SrcCopy >= OrigSize) then Exit(False);
          OutBuf[PIdx] := OutBuf[SrcCopy];
        end;
      end;

      // Picture Chunky Decrunch (optional)
      if BR.GetBit = 1 then
      begin
        PicIdx := OrigSize;
        PicIters := Min(4000, OrigSize div 8);
        for I := 1 to PicIters do
        begin
          D0 := 0; D1 := 0; D2 := 0; D3 := 0;
          for J := 1 to 4 do
          begin
            Dec(PicIdx, 2);
            if PicIdx < 0 then Exit(False);
            D4 := (Word(OutBuf[PicIdx]) shl 8) or OutBuf[PicIdx + 1];
            for K := 1 to 4 do
            begin
              D0 := ((D0 shl 1) or ((D4 shr 15) and 1)); D4 := (D4 shl 1) and $FFFF;
              D1 := ((D1 shl 1) or ((D4 shr 15) and 1)); D4 := (D4 shl 1) and $FFFF;
              D2 := ((D2 shl 1) or ((D4 shr 15) and 1)); D4 := (D4 shl 1) and $FFFF;
              D3 := ((D3 shl 1) or ((D4 shr 15) and 1)); D4 := (D4 shl 1) and $FFFF;
            end;
          end;
          OutBuf[PicIdx]     := (D0 shr 8) and $FF;
          OutBuf[PicIdx + 1] := D0 and $FF;
          OutBuf[PicIdx + 2] := (D1 shr 8) and $FF;
          OutBuf[PicIdx + 3] := D1 and $FF;
          OutBuf[PicIdx + 4] := (D2 shr 8) and $FF;
          OutBuf[PicIdx + 5] := D2 and $FF;
          OutBuf[PicIdx + 6] := (D3 shr 8) and $FF;
          OutBuf[PicIdx + 7] := D3 and $FF;
        end;
      end;

      Result := True;
    except
      Result := False;
    end;
  finally
    BR.Free;
  end;
end;

function DecompressPackIce(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
var
  SearchOffsets: array of Integer;
  IsIce21: Boolean;
  I, Off, Delta: Integer;
  PackedSize, OrigSize: LongWord;
begin
  Result := False;
  SetLength(OutBuf, 0);
  if (InBuf = nil) or (InSize < 12) then Exit;

  SetLength(SearchOffsets, 0);
  // Scan for 'ICE!' or 'Ice!'
  Off := 0;
  while Off <= InSize - 12 do
  begin
    if ((InBuf[Off] = Byte('I')) and (InBuf[Off+1] = Byte('C')) and (InBuf[Off+2] = Byte('E')) and (InBuf[Off+3] = Byte('!'))) or
       ((InBuf[Off] = Byte('I')) and (InBuf[Off+1] = Byte('c')) and (InBuf[Off+2] = Byte('e')) and (InBuf[Off+3] = Byte('!'))) then
    begin
      SetLength(SearchOffsets, Length(SearchOffsets) + 1);
      SearchOffsets[High(SearchOffsets)] := Off;
    end;
    Inc(Off);
  end;

  for I := 0 to High(SearchOffsets) do
  begin
    Off := SearchOffsets[I];
    IsIce21 := (InBuf[Off+1] = Byte('c'));
    PackedSize := ReadBE32(InBuf, Off + 4);
    OrigSize := ReadBE32(InBuf, Off + 8);

    if (PackedSize > 0) and (PackedSize <= LongWord(InSize)) and (OrigSize > 0) and (OrigSize < 16777216) then
    begin
      for Delta := 0 to 4 do
      begin
        if DecompressPackIceInternal(InBuf, InSize, Off + 12, IsIce21, OrigSize, PackedSize - Delta, OutBuf) then
          Exit(True);
        if DecompressPackIceInternal(InBuf, InSize, Off, IsIce21, OrigSize, PackedSize + 12 - Delta, OutBuf) then
          Exit(True);
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------
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
    Result := BitBuf and 1;
    BitBuf := BitBuf shr 1;
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

  Result := (OutPos > 0);
end;

type
  TRNCHuffTree = record
    Lengths: array of Byte;
    Codes: array of Word;
    MaxLen: Integer;
    NumSymbols: Integer;
  end;

function DecompressRNCMethod1(const Data: PByte; InSize: Integer; OutSize: Integer; out OutBuf: TBytes): Boolean;
var
  OutPos, InPos, BitBuf, BitsLeft: Integer;

  function GetBit: Byte;
  begin
    if BitsLeft = 0 then
    begin
      if InPos + 1 >= InSize then Exit(0);
      BitBuf := Data[InPos] or (Word(Data[InPos + 1]) shl 8);
      Inc(InPos, 2);
      BitsLeft := 16;
    end;
    Result := BitBuf and 1;
    BitBuf := BitBuf shr 1;
    Dec(BitsLeft);
  end;

  function GetBits(N: Integer): LongWord;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 1 to N do
      Result := (Result shl 1) or GetBit;
  end;

  function BuildHuff(NumSymbols: Integer): TRNCHuffTree;
  var
    Idx, Len, Count, Extra, I, Bits, Code: Integer;
    BLCount: array[0..32] of Word;
    NextCode: array[0..32] of Word;
  begin
    Result.NumSymbols := NumSymbols;
    SetLength(Result.Lengths, NumSymbols);
    SetLength(Result.Codes, NumSymbols);
    Idx := 0;
    while Idx < NumSymbols do
    begin
      Len := 0;
      while GetBit = 0 do Inc(Len);
      Inc(Len);
      Count := GetBits(4);
      if Count = 15 then
      repeat
        Extra := GetBits(4);
        Inc(Count, Extra);
      until Extra <> 15;
      Inc(Count);
      for I := 1 to Count do
      begin
        if Idx < NumSymbols then
        begin
          Result.Lengths[Idx] := Len;
          Inc(Idx);
        end;
      end;
    end;

    Result.MaxLen := 1;
    for I := 0 to NumSymbols - 1 do
      if Result.Lengths[I] > Result.MaxLen then Result.MaxLen := Result.Lengths[I];

    FillChar(BLCount, SizeOf(BLCount), 0);
    for I := 0 to NumSymbols - 1 do
      if Result.Lengths[I] > 0 then Inc(BLCount[Result.Lengths[I]]);

    Code := 0;
    BLCount[0] := 0;
    FillChar(NextCode, SizeOf(NextCode), 0);
    for Bits := 1 to Result.MaxLen do
    begin
      Code := (Code + BLCount[Bits - 1]) shl 1;
      NextCode[Bits] := Code;
    end;

    for I := 0 to NumSymbols - 1 do
    begin
      Len := Result.Lengths[I];
      if Len > 0 then
      begin
        Result.Codes[I] := NextCode[Len];
        Inc(NextCode[Len]);
      end;
    end;
  end;

  function DecodeSymbol(const Tree: TRNCHuffTree): Integer;
  var
    Code, Len, I: Integer;
  begin
    Result := 0;
    Code := 0;
    for Len := 1 to Tree.MaxLen do
    begin
      Code := (Code shl 1) or GetBit;
      for I := 0 to Tree.NumSymbols - 1 do
      begin
        if (Tree.Lengths[I] = Len) and (Tree.Codes[I] = Code) then
        begin
          if I < 2 then Exit(I);
          Exit(GetBits(I - 1) or (1 shl (I - 1)));
        end;
      end;
    end;
  end;

var
  RawTree, LenTree, PosTree: TRNCHuffTree;
  Subchunks, RawLen, OffsetVal, LengthVal, I: Integer;
begin
  Result := False;
  SetLength(OutBuf, OutSize);
  OutPos := 0;
  InPos := 18;
  BitBuf := 0;
  BitsLeft := 0;

  try
    while (OutPos < OutSize) and (InPos < InSize) do
    begin
      RawTree := BuildHuff(16);
      LenTree := BuildHuff(64);
      PosTree := BuildHuff(64);
      Subchunks := GetBits(16);

      while (Subchunks > 0) and (OutPos < OutSize) do
      begin
        Dec(Subchunks);
        RawLen := DecodeSymbol(RawTree);
        while (RawLen > 0) and (OutPos < OutSize) and (InPos < InSize) do
        begin
          OutBuf[OutPos] := Data[InPos];
          Inc(OutPos);
          Inc(InPos);
          Dec(RawLen);
        end;
        if (Subchunks > 0) and (OutPos < OutSize) then
        begin
          OffsetVal := DecodeSymbol(LenTree) + 1;
          LengthVal := DecodeSymbol(PosTree) + 2;
          for I := 1 to LengthVal do
          begin
            if (OutPos >= OutSize) or (OutPos - OffsetVal < 0) then Break;
            OutBuf[OutPos] := OutBuf[OutPos - OffsetVal];
            Inc(OutPos);
          end;
        end;
      end;
    end;
    Result := (OutPos > 0);
  except
    Result := False;
  end;
end;

function DecompressRNC(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
var
  RNCStart, Method: Integer;
  OutSize: LongWord;
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

  if Method = 2 then
    Result := DecompressRNCMethod2(@InBuf[RNCStart], InSize - RNCStart, OutSize, OutBuf)
  else if Method = 1 then
    Result := DecompressRNCMethod1(@InBuf[RNCStart], InSize - RNCStart, OutSize, OutBuf);
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

    Result := True;
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
        if (A1 < 0) or (A2 < 0) or (A2 >= Length(OutBuf)) then Exit(False);
        OutBuf[A1] := OutBuf[A2];
      end;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------
// JAM Packer (LSD, LZH with Dynamic Huffman, LZW)
// -----------------------------------------------------------------------------

type
  TDynHuffNode = record
    Freq: Word;
    Index: Word;
    Parent: Word;
    LeftLeaf: Word;
    RightLeaf: Word;
  end;

function DecompressJamLZH(const PackedData: PByte; PackedSize: Integer; RawSize: Integer; out OutBuf: TBytes): Boolean;
const
  MaxCount = 314;
  VlcLengths: array[0..15] of Integer = (5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 9, 9, 9, 10);
  VlcOffsets: array[0..15] of Integer = (0, 32, 64, 128, 192, 256, 384, 512, 640, 768, 1024, 1280, 1536, 2048, 2560, 3072);
var
  Nodes: array[0..MaxCount * 2 - 2] of TDynHuffNode;
  CodeMap: array[0..MaxCount * 2 - 2] of Word;
  InPos, OutPos: Integer;
  BitBuf, BitLen: Integer;

  procedure ResetTree;
  var
    I, J, L, R: Integer;
  begin
    for I := 0 to MaxCount - 1 do
    begin
      Nodes[I].Freq := 1;
      Nodes[I].Index := I;
      Nodes[I].Parent := MaxCount + (I shr 1);
      Nodes[I].LeftLeaf := 0;
      Nodes[I].RightLeaf := 0;
      CodeMap[I] := I;
    end;
    for I := MaxCount to MaxCount * 2 - 2 do
    begin
      J := (I - MaxCount) shl 1;
      L := J;
      R := J + 1;
      Nodes[I].Freq := Nodes[L].Freq + Nodes[R].Freq;
      Nodes[I].Index := I;
      Nodes[I].Parent := MaxCount + (I shr 1);
      Nodes[I].LeftLeaf := L;
      Nodes[I].RightLeaf := R;
      CodeMap[I] := I;
    end;
  end;

  function ReadByte: Byte;
  begin
    if InPos <= 12 then raise Exception.Create('Jam LZH OOB');
    Dec(InPos);
    Result := PackedData[InPos];
  end;

  function ReadBits(Count: Integer): Integer;
  var
    Ret, MaxC: Integer;
  begin
    Ret := 0;
    while Count > 0 do
    begin
      if BitLen = 0 then
      begin
        BitBuf := ReadByte;
        BitLen := 8;
      end;
      MaxC := Min(Count, BitLen);
      Dec(BitLen, MaxC);
      Ret := (Ret shl MaxC) or ((BitBuf shr BitLen) and ((1 shl MaxC) - 1));
      Dec(Count, MaxC);
    end;
    Result := Ret;
  end;

  function DecodeSymbol: Word;
  var
    Code: Word;
  begin
    Code := MaxCount * 2 - 2;
    while Code >= MaxCount do
    begin
      if ReadBits(1) = 1 then
        Code := Nodes[Code].RightLeaf
      else
        Code := Nodes[Code].LeftLeaf;
    end;
    Result := Code;
  end;

  procedure UpdateTree(Code: Word);
  var
    ParentIdx, DestCode, TempParent: Word;
  begin
    while Code <> MaxCount * 2 - 2 do
    begin
      Inc(Nodes[Code].Freq);
      Code := Nodes[Code].Parent;
    end;
    Inc(Nodes[Code].Freq);
  end;

var
  Symbol, Count, DistBase, Distance, I: Integer;
begin
  Result := False;
  SetLength(OutBuf, RawSize);
  InPos := PackedSize;
  OutPos := RawSize;
  BitBuf := 0;
  BitLen := 0;
  ResetTree;

  try
    while OutPos > 0 do
    begin
      Symbol := DecodeSymbol;
      UpdateTree(Symbol);
      if Symbol < 256 then
      begin
        Dec(OutPos);
        OutBuf[OutPos] := Symbol;
      end
      else
      begin
        DistBase := ReadBits(4);
        Distance := VlcOffsets[DistBase] + ReadBits(VlcLengths[DistBase]) + 1;
        Count := Symbol - 253;
        for I := 1 to Count do
        begin
          Dec(OutPos);
          if OutPos + Distance < RawSize then
            OutBuf[OutPos] := OutBuf[OutPos + Distance]
          else
            OutBuf[OutPos] := $20;
        end;
      end;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function DecompressJam(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
var
  Off: Integer;
  RawSize, PackedSize: LongWord;
  Dummy: TBytes;
begin
  Result := False;
  SetLength(OutBuf, 0);
  if (InBuf = nil) or (InSize < 12) then Exit;

  // Scan for LSD!, LZH!, LZW!
  for Off := 0 to Min(InSize - 12, 4096) do
  begin
    if (InBuf[Off] = Byte('L')) and
       ((CheckString(InBuf, InSize, Off, 'LSD!') or
         CheckString(InBuf, InSize, Off, 'LZH!') or
         CheckString(InBuf, InSize, Off, 'LZW!'))) then
    begin
      RawSize := ReadBE32(InBuf, Off + 4);
      PackedSize := ReadBE32(InBuf, Off + 8);
      if (RawSize > 0) and (RawSize < 16777216) and (PackedSize > 0) and (Off + PackedSize <= InSize + 12) then
      begin
        if CheckString(InBuf, InSize, Off, 'LSD!') then
        begin
          if DecompressAutomation(@InBuf[Off], PackedSize + 12, OutBuf) then
            Exit(True);
        end
        else if CheckString(InBuf, InSize, Off, 'LZH!') then
        begin
          if DecompressJamLZH(@InBuf[Off], PackedSize + 12, RawSize, OutBuf) then
            Exit(True);
        end;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------
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

    Result := (A4 > 0);
    if Result and (A4 < Length(OutBuf)) then
      SetLength(OutBuf, A4);
  except
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

    Result := (A4 > 0);
  except
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------
// JEK Packer / Byte Killer
// -----------------------------------------------------------------------------

function DecompressJek(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
var
  ActualEnd, OffsetVal, OutPtr, CmdVal, CountVal, OffVal, SrcVal, I: Integer;
  XorKey, D0Val, CarryVal, WVal: LongWord;
  MaxBufSize: Integer;
  TempBuf: TBytes;

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
  if (InBuf = nil) or (InSize < 12) then Exit;

  ActualEnd := InSize;
  OffsetVal := ActualEnd - 4;
  XorKey := ReadBE32(InBuf, OffsetVal);
  D0Val := 0;

  MaxBufSize := Min(4 * 1024 * 1024, InSize * 8 + 65536);
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
    SetLength(OutBuf, MaxBufSize - OutPtr);
    Move(TempBuf[OutPtr], OutBuf[0], Length(OutBuf));
    Result := (Length(OutBuf) > 0);
  end;
end;

// -----------------------------------------------------------------------------
// RLE / Degas Elite Decompressor
// -----------------------------------------------------------------------------

function DecompressRLE(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes): Boolean;
var
  InPos, OutPos, HeaderVal, Count, I: Integer;
  B, ValByte: Byte;
begin
  Result := False;
  SetLength(OutBuf, 0);
  if (InBuf = nil) or (InSize = 0) then Exit;

  SetLength(OutBuf, InSize * 4 + 32768);
  InPos := 0;
  OutPos := 0;

  // Skip 2-byte Degas Elite resolution header if present ($8000, $8001, $8002)
  if (InSize >= 2) and (InBuf[0] = $80) and (InBuf[1] in [0, 1, 2]) then
    InPos := 2;

  while InPos < InSize do
  begin
    B := InBuf[InPos];
    Inc(InPos);
    if B > 127 then
      HeaderVal := B - 256
    else
      HeaderVal := B;

    if (HeaderVal >= 0) and (HeaderVal <= 127) then
    begin
      Count := HeaderVal + 1;
      for I := 1 to Count do
      begin
        if InPos >= InSize then Break;
        if OutPos >= Length(OutBuf) then SetLength(OutBuf, Length(OutBuf) * 2);
        OutBuf[OutPos] := InBuf[InPos];
        Inc(OutPos);
        Inc(InPos);
      end;
    end
    else if (HeaderVal >= -127) and (HeaderVal <= -1) then
    begin
      if InPos >= InSize then Break;
      ValByte := InBuf[InPos];
      Inc(InPos);
      Count := -HeaderVal + 1;
      for I := 1 to Count do
      begin
        if OutPos >= Length(OutBuf) then SetLength(OutBuf, Length(OutBuf) * 2);
        OutBuf[OutPos] := ValByte;
        Inc(OutPos);
      end;
    end;
  end;

  SetLength(OutBuf, OutPos);
  Result := (OutPos > 0);
end;

// -----------------------------------------------------------------------------
// Master Depacker Dispatcher
// -----------------------------------------------------------------------------

function DepackBuffer(const InBuf: PByte; InSize: Integer; out OutBuf: TBytes; out DetectedPacker: string): Boolean;
begin
  Result := False;
  SetLength(OutBuf, 0);
  DetectedPacker := 'None';
  if (InBuf = nil) or (InSize < 4) then Exit;

  DetectedPacker := DetectPackerSignature(InBuf, InSize);

  // 1. Pack-Ice
  if Pos('Pack-Ice', DetectedPacker) > 0 then
  begin
    if DecompressPackIce(InBuf, InSize, OutBuf) then Exit(True);
  end;

  // 2. Atomik Cruncher
  if Pos('Atomik', DetectedPacker) > 0 then
  begin
    if DecompressAtomik(InBuf, InSize, OutBuf) then Exit(True);
  end;

  // 3. Automation Packer
  if Pos('Automation', DetectedPacker) > 0 then
  begin
    if DecompressAutomation(InBuf, InSize, OutBuf) then Exit(True);
  end;

  // 4. JAM Packer
  if Pos('JAM', DetectedPacker) > 0 then
  begin
    if DecompressJam(InBuf, InSize, OutBuf) then Exit(True);
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
  end;

  // 7. JEK Packer / Byte Killer
  if (Pos('JEK', DetectedPacker) > 0) or (Pos('Bytekiller', DetectedPacker) > 0) then
  begin
    if DecompressJek(InBuf, InSize, OutBuf) then Exit(True);
  end;

  // 8. Degas Elite RLE
  if Pos('Degas', DetectedPacker) > 0 then
  begin
    if DecompressRLE(InBuf, InSize, OutBuf) then Exit(True);
  end;

  // Generic fallback tries for files with generic executable or data flags
  if DecompressPackIce(InBuf, InSize, OutBuf) then
  begin
    DetectedPacker := 'Pack-Ice (Auto-detected)';
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

  if DecompressAutomation(InBuf, InSize, OutBuf) then
  begin
    DetectedPacker := 'Automation Packer (Auto-detected)';
    Exit(True);
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
