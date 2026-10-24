unit unit_ataripic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Math, unit_depacker;

type
  TAtariPicFormat = (
    apfUnknown,
    apfDegasPI1,       // DEGAS Low Res (320x200, 16 colors)
    apfDegasPI2,       // DEGAS Med Res (640x200, 4 colors)
    apfDegasPI3,       // DEGAS High Res (640x400, 2 colors monochrome)
    apfDegasPC1,       // DEGAS Elite Compressed Low Res (RLE)
    apfDegasPC2,       // DEGAS Elite Compressed Med Res (RLE)
    apfDegasPC3,       // DEGAS Elite Compressed High Res (RLE)
    apfNeoChrome,      // NeoChrome (.NEO) (320x200, 16 colors)
    apfSpectrum512SPU, // Spectrum 512 Uncompressed (.SPU) (320x200, 512 colors per-line palettes)
    apfSpectrum512SPC, // Spectrum 512 Compressed (.SPC) (320x200, 512 colors)
    apfSpectrum512SPS, // Spectrum 512 Smooshed (.SPS) (320x200, 512 colors)
    apfRawScreen,      // Raw 32,000 or 32,032 bytes Atari ST screen dump
    apfArtDirector,    // Art Director (.ART) (320x200, 16 colors)
    apfDoodle,         // Doodle (.DOO) (640x400, monochrome)
    apfQuantumPaint,   // Quantum Paintbox (.PBX) (320x200, 16 colors)
    apfTinyStuff,      // Tiny Stuff (.TN1, .TN2, .TN3, .TNY)
    apfStadPac,        // STAD Monochrome Packed (.PAC - pM85/pM86)
    apfStosCompactPac, // STOS Picture Compactor / Squashed (.PAC)
    apfGemImg,         // GEM Monochrome Image (.IMG)
    apfTitusPC1,       // Titus / Extended Compressed Picture (.PC1)
    apfTitusSQ,        // Titus SQZ Compressed Screen (.SQ)
    apfMurMural        // C.O.L.R. Object Editor Mural (.MUR) (320x200, 16 colors)
  );

const
  apfDuckTalesPC1: TAtariPicFormat = apfTitusPC1;


{ Detects if the given buffer/filename represents a supported Atari ST picture }
function DetectAtariPicFormat(const InBuf: PByte; InSize: Integer; const FileName: string): TAtariPicFormat;

{ Returns human-readable name of the format }
function GetAtariPicFormatName(Format: TAtariPicFormat): string;

{ Decodes an Atari ST picture from buffer into a standard Graphics.TBitmap }
function DecodeAtariPicture(const InBuf: PByte; InSize: Integer; const FileName: string;
  out OutBitmap: Graphics.TBitmap; out FormatDesc: string;
  out OutWidth, OutHeight, OutColors: Integer): Boolean;

implementation

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

function CheckLong(const P: PByte; Size: Integer; Offset: Integer; Val: LongWord): Boolean; inline;
begin
  if (Offset < 0) or (Offset + 4 > Size) then Exit(False);
  Result := ReadBE32(P, Offset) = Val;
end;

function HasSTEBits(const PaletteBuf: PByte; NumColors: Integer): Boolean;
var
  I: Integer;
  W: Word;
begin
  for I := 0 to NumColors - 1 do
  begin
    W := ReadBE16(PaletteBuf, I * 2);
    // In Atari STE, bit 11 is R LSB, bit 7 is G LSB, bit 3 is B LSB
    if (W and $0888) <> 0 then
      Exit(True);
  end;
  Result := False;
end;

function ConvertAtariColorWord(ColorWord: Word; IsSTE: Boolean): TColor;
var
  R, G, B: Byte;
begin
  if IsSTE then
  begin
    // 4-bit per channel: bits 8..10 are ST R2..R0, bit 11 is STE R_LSB
    R := (((ColorWord shr 8) and $07) shl 1) or ((ColorWord shr 11) and $01);
    G := (((ColorWord shr 4) and $07) shl 1) or ((ColorWord shr 7) and $01);
    B := ((ColorWord and $07) shl 1) or ((ColorWord shr 3) and $01);
    Result := RGBToColor((R * 255) div 15, (G * 255) div 15, (B * 255) div 15);
  end
  else
  begin
    // Standard ST: 3-bit per channel (0..7)
    R := (ColorWord shr 8) and $07;
    G := (ColorWord shr 4) and $07;
    B := ColorWord and $07;
    Result := RGBToColor((R * 255) div 7, (G * 255) div 7, (B * 255) div 7);
  end;
end;

function GetDefaultSTPalette(Index: Integer): TColor;
begin
  case Index and 15 of
    0:  Result := RGBToColor(255, 255, 255); // White
    1:  Result := RGBToColor(0, 0, 0);       // Black
    2:  Result := RGBToColor(255, 0, 0);     // Red
    3:  Result := RGBToColor(0, 255, 0);     // Green
    4:  Result := RGBToColor(0, 0, 255);     // Blue
    5:  Result := RGBToColor(0, 255, 255);   // Cyan
    6:  Result := RGBToColor(255, 255, 0);   // Yellow
    7:  Result := RGBToColor(255, 0, 255);   // Magenta
    8:  Result := RGBToColor(170, 170, 170); // Low White
    9:  Result := RGBToColor(85, 85, 85);    // Gray
    10: Result := RGBToColor(255, 85, 85);   // Light Red
    11: Result := RGBToColor(85, 255, 85);   // Light Green
    12: Result := RGBToColor(85, 85, 255);   // Light Blue
    13: Result := RGBToColor(85, 255, 255);  // Light Cyan
    14: Result := RGBToColor(255, 255, 85);  // Light Yellow
    15: Result := RGBToColor(255, 85, 255);  // Light Magenta
  else
    Result := clBlack;
  end;
end;

function GetAtariPicFormatName(Format: TAtariPicFormat): string;
begin
  case Format of
    apfDegasPI1:       Result := 'DEGAS Low Res (.PI1)';
    apfDegasPI2:       Result := 'DEGAS Med Res (.PI2)';
    apfDegasPI3:       Result := 'DEGAS High Res (.PI3)';
    apfDegasPC1:       Result := 'DEGAS Elite RLE Low Res (.PC1)';
    apfDegasPC2:       Result := 'DEGAS Elite RLE Med Res (.PC2)';
    apfDegasPC3:       Result := 'DEGAS Elite RLE High Res (.PC3)';
    apfNeoChrome:      Result := 'NeoChrome Image (.NEO)';
    apfSpectrum512SPU: Result := 'Spectrum 512 Image (.SPU)';
    apfSpectrum512SPC: Result := 'Spectrum 512 Compressed Image (.SPC)';
    apfSpectrum512SPS: Result := 'Spectrum 512 Smooshed Image (.SPS)';
    apfRawScreen:      Result := 'Raw Atari ST Screen Dump (320x200)';
    apfArtDirector:    Result := 'Art Director Image (.ART)';
    apfDoodle:         Result := 'Doodle High Res (.DOO)';
    apfQuantumPaint:   Result := 'Quantum Paintbox (.PBX)';
    apfTinyStuff:      Result := 'Tiny Stuff Compressed Image (.TNY)';
    apfStadPac:        Result := 'STAD Monochrome Image (.PAC)';
    apfStosCompactPac: Result := 'STOS Packed Picture (.PAC)';
    apfGemImg:         Result := 'GEM Monochrome Image (.IMG)';
    apfTitusPC1:       Result := 'Compressed Low Res Picture (.PC1)';
    apfTitusSQ:        Result := 'Titus Compressed Screen (.SQ)';
    apfMurMural:        Result := 'C.O.L.R. Object Editor Mural (.MUR)';
  else
    Result := 'Unknown Picture Format';
  end;
end;

function DetectAtariPicFormat(const InBuf: PByte; InSize: Integer; const FileName: string): TAtariPicFormat;
var
  Ext: string;
  ResWord: Word;
  UncompVal: LongWord;
begin
  Result := apfUnknown;
  if (InBuf = nil) or (InSize < 7) then Exit;

  Ext := UpperCase(ExtractFileExt(FileName));

  // 1a. Check DEGAS Elite magic resolution words ($8000, $8001, $8002)
  if InSize >= 34 then
  begin
    ResWord := ReadBE16(InBuf, 0);
    if (ResWord = $8000) then Exit(apfDegasPC1);
    if (ResWord = $8001) then Exit(apfDegasPC2);
    if (ResWord = $8002) then Exit(apfDegasPC3);
  end;

  // 1b. Check Titus Compressed Picture (.PC1 Method 1 Huffman or Method 2 LZW)
  if (InSize >= 10) then
  begin
    if (InBuf[0] in [1, 2]) and (InBuf[1] = 0) then
    begin
      UncompVal := LongWord(InBuf[2]) or (LongWord(InBuf[3]) shl 8) or (LongWord(InBuf[4]) shl 16) or (LongWord(InBuf[5]) shl 24);
      if (Ext = '.PC1') or ((UncompVal >= 1000) and (UncompVal <= 500000)) then
        Exit(apfTitusPC1);
    end;
  end;

  // 1c. Check Titus SQ Compressed Screen (.SQ / .SQZ / .SQH)
  if (InSize >= 8) and ((Ext = '.SQ') or (Ext = '.SQZ') or (Ext = '.SQH')) then
  begin
    UncompVal := ((LongWord(InBuf[0]) and $0F) shl 16) or (LongWord(InBuf[3]) shl 8) or LongWord(InBuf[2]);
    if (UncompVal = 32000) or (UncompVal = 32032) or (UncompVal = 40000) or (UncompVal = 64000) then
      Exit(apfTitusSQ);
  end;

  // Generic check for Titus SQ (header: out_len and tree_size)
  if (InSize >= 16) and (InBuf[1] in [$84, $04, $80, $00]) then
  begin
    UncompVal := ((LongWord(InBuf[0]) and $0F) shl 16) or (LongWord(InBuf[3]) shl 8) or LongWord(InBuf[2]);
    if (UncompVal = 32000) or (UncompVal = 32032) or (UncompVal = 40000) then
    begin
      ResWord := Word(InBuf[4]) or (Word(InBuf[5]) shl 8); // TreeSize
      if (ResWord > 0) and (ResWord < 4000) and (6 + ResWord < InSize) then
        Exit(apfTitusSQ);
    end;
  end;

  // 1d. Check DEGAS Elite RLE extensions
  if (Ext = '.PC1') then Exit(apfDegasPC1);
  if (Ext = '.PC2') then Exit(apfDegasPC2);
  if (Ext = '.PC3') then Exit(apfDegasPC3);

  // 2. Check STAD Monochrome (.PAC) - 'pM85' or 'pM86'
  if CheckString(InBuf, InSize, 0, 'pM85') or CheckString(InBuf, InSize, 0, 'pM86') then
    Exit(apfStadPac);

  // 3. Check STOS Picture Compactor ($06071963)
  if CheckLong(InBuf, InSize, 0, $06071963) then
    Exit(apfStosCompactPac);

  // 4. Check STOS / STAD by .PAC extension
  if (Ext = '.PAC') then
  begin
    if CheckString(InBuf, InSize, 0, 'pM85') or CheckString(InBuf, InSize, 0, 'pM86') then
      Exit(apfStadPac)
    else
      Exit(apfStosCompactPac);
  end;

  // 5. Check Tiny Stuff (.TN1, .TN2, .TN3, .TNY)
  if (Ext = '.TN1') or (Ext = '.TN2') or (Ext = '.TN3') or (Ext = '.TNY') then
    Exit(apfTinyStuff);

  // 6. Check Art Director (.ART)
  if (Ext = '.ART') then Exit(apfArtDirector);

  // 7. Check Doodle (.DOO)
  if (Ext = '.DOO') then Exit(apfDoodle);

  // 8. Check Quantum Paintbox (.PBX)
  if (Ext = '.PBX') or CheckString(InBuf, InSize, 0, 'PBX') then Exit(apfQuantumPaint);

  // 9a. Check Spectrum 512 Uncompressed (.SPU) - 51,104 or 51,200 bytes
  if (InSize = 51104) or (InSize = 51200) or ((Ext = '.SPU') and (InSize >= 51104)) then
    Exit(apfSpectrum512SPU);

  // 9b. Check Spectrum 512 Compressed (.SPC) and Smooshed (.SPS)
  if (InSize >= 12) and (InBuf[0] = Byte('S')) and (InBuf[1] = Byte('P')) then
  begin
    if Ext = '.SPS' then
      Exit(apfSpectrum512SPS)
    else
      Exit(apfSpectrum512SPC);
  end;
  if (Ext = '.SPC') and (InSize >= 12) then Exit(apfSpectrum512SPC);
  if (Ext = '.SPS') and (InSize >= 12) then Exit(apfSpectrum512SPS);

  // 10. Check NeoChrome (.NEO) - 32,128 bytes
  if (Ext = '.NEO') or (InSize = 32128) then
  begin
    if InSize = 32128 then
    begin
      ResWord := ReadBE16(InBuf, 2);
      if ResWord <= 2 then Exit(apfNeoChrome);
    end;
  end;

  // 11. Check DEGAS Uncompressed (.PI1, .PI2, .PI3) - 32,034 bytes (or 32,066)
  if (InSize = 32034) or (InSize = 32066) or (Ext = '.PI1') or (Ext = '.PI2') or (Ext = '.PI3') then
  begin
    ResWord := ReadBE16(InBuf, 0);
    if (ResWord = 0) or (Ext = '.PI1') then Exit(apfDegasPI1);
    if (ResWord = 1) or (Ext = '.PI2') then Exit(apfDegasPI2);
    if (ResWord = 2) or (Ext = '.PI3') then Exit(apfDegasPI3);
  end;

  // 12. Check C.O.L.R. Object Editor Mural (.MUR)
  if (Ext = '.MUR') and (InSize >= 32000) then
    Exit(apfMurMural);

  // 13. Check Raw Atari ST Screen dumps (32,000 or 32,032 bytes)
  if (InSize = 32000) or (InSize = 32032) then
  begin
    Exit(apfRawScreen);
  end;

  // 14. Check GEM Image (.IMG)
  if (Ext = '.IMG') or ((InSize >= 16) and (ReadBE16(InBuf, 0) = 1) and (ReadBE16(InBuf, 2) >= 8) and (ReadBE16(InBuf, 4) in [1, 4])) then
  begin
    Exit(apfGemImg);
  end;
end;

{ Decodes Low Resolution 4-plane Atari ST screen with variable dimensions }
procedure DecodeLowResScreenEx(const BitmapBuf: PByte; const Palette: array of TColor;
  OutBmp: Graphics.TBitmap; AWidth, AHeight: Integer);
var
  Y, Block, X, Shift, ColorIdx, PixelX, LineOffset, BlockOffset: Integer;
  P0, P1, P2, P3: Word;
begin
  OutBmp.SetSize(AWidth, AHeight);
  OutBmp.PixelFormat := pf24bit;
  for Y := 0 to AHeight - 1 do
  begin
    LineOffset := Y * (AWidth div 2);
    for Block := 0 to (AWidth div 16) - 1 do
    begin
      BlockOffset := LineOffset + Block * 8;
      P0 := ReadBE16(BitmapBuf, BlockOffset);
      P1 := ReadBE16(BitmapBuf, BlockOffset + 2);
      P2 := ReadBE16(BitmapBuf, BlockOffset + 4);
      P3 := ReadBE16(BitmapBuf, BlockOffset + 6);

      for X := 0 to 15 do
      begin
        Shift := 15 - X;
        ColorIdx := ((P0 shr Shift) and 1) or
                    (((P1 shr Shift) and 1) shl 1) or
                    (((P2 shr Shift) and 1) shl 2) or
                    (((P3 shr Shift) and 1) shl 3);

        PixelX := Block * 16 + X;
        OutBmp.Canvas.Pixels[PixelX, Y] := Palette[ColorIdx];
      end;
    end;
  end;
end;

{ Decodes 320x200 Low Resolution 4-plane Atari ST screen }
procedure DecodeLowResScreen(const BitmapBuf: PByte; const Palette: array of TColor;
  OutBmp: Graphics.TBitmap);
begin
  DecodeLowResScreenEx(BitmapBuf, Palette, OutBmp, 320, 200);
end;

{ Decodes 640x200 Medium Resolution 2-plane Atari ST screen (doubled vertically to 640x400 for square pixels) }
procedure DecodeMedResScreen(const BitmapBuf: PByte; const Palette: array of TColor;
  OutBmp: Graphics.TBitmap);
var
  Y, Block, X, Shift, ColorIdx, PixelX, LineOffset, BlockOffset: Integer;
  P0, P1: Word;
  Col: TColor;
begin
  OutBmp.SetSize(640, 400);
  OutBmp.PixelFormat := pf24bit;
  for Y := 0 to 199 do
  begin
    LineOffset := Y * 160;
    for Block := 0 to 39 do
    begin
      BlockOffset := LineOffset + Block * 4;
      P0 := ReadBE16(BitmapBuf, BlockOffset);
      P1 := ReadBE16(BitmapBuf, BlockOffset + 2);

      for X := 0 to 15 do
      begin
        Shift := 15 - X;
        ColorIdx := ((P0 shr Shift) and 1) or
                    (((P1 shr Shift) and 1) shl 1);

        Col := Palette[ColorIdx and 3];
        PixelX := Block * 16 + X;
        OutBmp.Canvas.Pixels[PixelX, Y * 2] := Col;
        OutBmp.Canvas.Pixels[PixelX, Y * 2 + 1] := Col;
      end;
    end;
  end;
end;

{ Decodes 640x400 High Resolution 1-plane Atari ST screen }
procedure DecodeHighResScreen(const BitmapBuf: PByte; PaperCol, InkCol: TColor;
  OutBmp: Graphics.TBitmap);
var
  Y, Block, X, Shift, PixelX, LineOffset, BlockOffset: Integer;
  P0: Word;
begin
  OutBmp.SetSize(640, 400);
  OutBmp.PixelFormat := pf24bit;

  for Y := 0 to 399 do
  begin
    LineOffset := Y * 80;
    for Block := 0 to 39 do
    begin
      BlockOffset := LineOffset + Block * 2;
      P0 := ReadBE16(BitmapBuf, BlockOffset);

      for X := 0 to 15 do
      begin
        Shift := 15 - X;
        PixelX := Block * 16 + X;
        if ((P0 shr Shift) and 1) = 0 then
          OutBmp.Canvas.Pixels[PixelX, Y] := PaperCol
        else
          OutBmp.Canvas.Pixels[PixelX, Y] := InkCol;
      end;
    end;
  end;
end;

const
  { Standard desktop GEM / C.O.L.R. Object Editor Mural 16-color palette (ST 3-bit RGB) }
  COLR_MURAL_PALETTE: array[0..15] of Word = (
    $0777, $0000, $0555, $0444, $0333, $0222, $0111, $0770,
    $0660, $0550, $0440, $0330, $0220, $0110, $0007, $0000
  );

{ Decompresses Spectrum 512 Compressed (.SPC) image and palette streams }
function DecompressSpectrum512SPC(const InBuf: PByte; InSize: Integer;
  out OutScreen: TBytes; out OutPal: TBytes): Boolean;
var
  DataLen, ColorLen: LongWord;
  InPos, DestPos: Integer;
  Val: ShortInt;
  B: Byte;
  Count: Integer;
  BufImage: array[0..31839] of Byte;
  DestW, SrcW: Integer;
  PalInPos, PalDestIdx: Integer;
  PalCount, ColorIdx: Integer;
  ActiveMask: Word;
  ColorWord: Word;
begin
  Result := False;
  SetLength(OutScreen, 0);
  SetLength(OutPal, 0);

  if (InBuf = nil) or (InSize < 12) then Exit;

  // Header verification: 'SP' signature ($53, $50)
  if (InBuf[0] <> Byte('S')) or (InBuf[1] <> Byte('P')) then Exit;

  DataLen := ReadBE32(InBuf, 4);
  ColorLen := ReadBE32(InBuf, 8);

  if (DataLen = 0) or (ColorLen = 0) then Exit;
  if 12 + Int64(DataLen) + Int64(ColorLen) > InSize then Exit;

  // 1. Decompress pixel data via RLE into BufImage (31,840 bytes)
  FillChar(BufImage, SizeOf(BufImage), 0);
  InPos := 12;
  DestPos := 0;

  while (DestPos < 31840) and (InPos < 12 + Integer(DataLen)) do
  begin
    Val := ShortInt(InBuf[InPos]);
    Inc(InPos);

    if Val >= 0 then
    begin
      Count := Val + 1;
      if DestPos + Count > 31840 then
        Count := 31840 - DestPos;
      if InPos + Count > 12 + Integer(DataLen) then
        Count := (12 + Integer(DataLen)) - InPos;
      if Count > 0 then
      begin
        Move(InBuf[InPos], BufImage[DestPos], Count);
        Inc(InPos, Count);
        Inc(DestPos, Count);
      end
      else
        Break;
    end
    else
    begin
      Count := -Val + 2;
      if DestPos + Count > 31840 then
        Count := 31840 - DestPos;
      if InPos < 12 + Integer(DataLen) then
      begin
        B := InBuf[InPos];
        Inc(InPos);
        FillChar(BufImage[DestPos], Count, B);
        Inc(DestPos, Count);
      end
      else
        Break;
    end;
  end;

  // 2. Interleave planar BufImage into standard 32,000-byte VRAM screen buffer
  SetLength(OutScreen, 32000);
  FillChar(OutScreen[0], 32000, 0);

  DestW := 80; // Starting at scanline 1 (offset 160 bytes = 80 words)
  SrcW := 0;
  while (DestW < 16000) and (SrcW + 1 < 7960) do
  begin
    OutScreen[DestW * 2]     := BufImage[SrcW * 2];
    OutScreen[DestW * 2 + 1] := BufImage[SrcW * 2 + 1];
    OutScreen[DestW * 2 + 2] := BufImage[7960 + SrcW * 2];
    OutScreen[DestW * 2 + 3] := BufImage[7960 + SrcW * 2 + 1];
    OutScreen[DestW * 2 + 4] := BufImage[15920 + SrcW * 2];
    OutScreen[DestW * 2 + 5] := BufImage[15920 + SrcW * 2 + 1];
    OutScreen[DestW * 2 + 6] := BufImage[23880 + SrcW * 2];
    OutScreen[DestW * 2 + 7] := BufImage[23880 + SrcW * 2 + 1];
    Inc(DestW, 4);
    Inc(SrcW, 1);
  end;

  // 3. Decompress palette (19,104 bytes = 597 palettes * 16 words * 2 bytes)
  SetLength(OutPal, 19104);
  FillChar(OutPal[0], 19104, 0);

  PalInPos := 12 + Integer(DataLen);
  PalDestIdx := 0;

  for PalCount := 1 to 597 do
  begin
    if PalInPos + 1 >= 12 + Integer(DataLen) + Integer(ColorLen) then
      Break;

    ActiveMask := ReadBE16(InBuf, PalInPos);
    Inc(PalInPos, 2);

    for ColorIdx := 0 to 15 do
    begin
      if ((ActiveMask and (1 shl ColorIdx)) <> 0) and (ColorIdx in [1..14]) then
      begin
        if PalInPos + 1 < 12 + Integer(DataLen) + Integer(ColorLen) then
        begin
          ColorWord := ReadBE16(InBuf, PalInPos);
          Inc(PalInPos, 2);
        end
        else
          ColorWord := 0;
      end
      else
        ColorWord := 0;

      OutPal[PalDestIdx * 2] := Byte((ColorWord shr 8) and $FF);
      OutPal[PalDestIdx * 2 + 1] := Byte(ColorWord and $FF);
      Inc(PalDestIdx);
    end;
  end;

  Result := True;
end;

{ Decompresses Spectrum 512 Smooshed (.SPS) image and palette streams }
function DecompressSpectrum512SPS(const InBuf: PByte; InSize: Integer;
  out OutScreen: TBytes; out OutPal: TBytes): Boolean;
var
  DataLen, ColorLen: LongWord;
  InPos, DestPos: Integer;
  ValByte, B: Byte;
  Count, I: Integer;
  BufImage: array[0..31839] of Byte;
  DestW, SrcW: Integer;
  PalInPos, PalDestIdx: Integer;
  PalCount, ColorIdx: Integer;
  BitArray: array of Boolean;
  TotalBits, BitPos: Integer;
  ActiveCols: array[1..14] of Boolean;
  ColorVal: Word;
begin
  Result := False;
  SetLength(OutScreen, 0);
  SetLength(OutPal, 0);

  if (InBuf = nil) or (InSize < 12) then Exit;

  if (InBuf[0] <> Byte('S')) or (InBuf[1] <> Byte('P')) then Exit;

  DataLen := ReadBE32(InBuf, 4);
  ColorLen := ReadBE32(InBuf, 8);

  if (DataLen = 0) or (ColorLen = 0) then Exit;
  if 12 + Int64(DataLen) + Int64(ColorLen) > InSize then Exit;

  // 1. Decompress pixel data via SPS RLE into BufImage (31,840 bytes)
  FillChar(BufImage, SizeOf(BufImage), 0);
  InPos := 12;
  DestPos := 0;

  while (DestPos < 31840) and (InPos < 12 + Integer(DataLen)) do
  begin
    ValByte := InBuf[InPos];
    Inc(InPos);

    if ValByte <= 127 then
    begin
      Count := Integer(ValByte) + 3;
      if DestPos + Count > 31840 then
        Count := 31840 - DestPos;
      if InPos < 12 + Integer(DataLen) then
      begin
        B := InBuf[InPos];
        Inc(InPos);
        FillChar(BufImage[DestPos], Count, B);
        Inc(DestPos, Count);
      end
      else
        Break;
    end
    else
    begin
      Count := Integer(ValByte) - 127;
      if DestPos + Count > 31840 then
        Count := 31840 - DestPos;
      if InPos + Count > 12 + Integer(DataLen) then
        Count := (12 + Integer(DataLen)) - InPos;
      if Count > 0 then
      begin
        Move(InBuf[InPos], BufImage[DestPos], Count);
        Inc(InPos, Count);
        Inc(DestPos, Count);
      end
      else
        Break;
    end;
  end;

  // 2. Interleave planar BufImage into standard 32,000-byte VRAM screen buffer
  SetLength(OutScreen, 32000);
  FillChar(OutScreen[0], 32000, 0);

  DestW := 80;
  SrcW := 0;
  while (DestW < 16000) and (SrcW + 1 < 7960) do
  begin
    OutScreen[DestW * 2]     := BufImage[SrcW * 2];
    OutScreen[DestW * 2 + 1] := BufImage[SrcW * 2 + 1];
    OutScreen[DestW * 2 + 2] := BufImage[7960 + SrcW * 2];
    OutScreen[DestW * 2 + 3] := BufImage[7960 + SrcW * 2 + 1];
    OutScreen[DestW * 2 + 4] := BufImage[15920 + SrcW * 2];
    OutScreen[DestW * 2 + 5] := BufImage[15920 + SrcW * 2 + 1];
    OutScreen[DestW * 2 + 6] := BufImage[23880 + SrcW * 2];
    OutScreen[DestW * 2 + 7] := BufImage[23880 + SrcW * 2 + 1];
    Inc(DestW, 4);
    Inc(SrcW, 1);
  end;

  // 3. Decompress palette bitstream
  SetLength(OutPal, 19104);
  FillChar(OutPal[0], 19104, 0);

  PalInPos := 12 + Integer(DataLen);
  TotalBits := Integer(ColorLen) * 8;
  SetLength(BitArray, TotalBits);

  for I := 0 to Integer(ColorLen) - 1 do
  begin
    B := InBuf[PalInPos + I];
    BitArray[I * 8 + 0] := (B and $80) <> 0;
    BitArray[I * 8 + 1] := (B and $40) <> 0;
    BitArray[I * 8 + 2] := (B and $20) <> 0;
    BitArray[I * 8 + 3] := (B and $10) <> 0;
    BitArray[I * 8 + 4] := (B and $08) <> 0;
    BitArray[I * 8 + 5] := (B and $04) <> 0;
    BitArray[I * 8 + 6] := (B and $02) <> 0;
    BitArray[I * 8 + 7] := (B and $01) <> 0;
  end;

  BitPos := 0;
  PalDestIdx := 0;

  for PalCount := 1 to 597 do
  begin
    if BitPos + 14 > TotalBits then Break;

    for ColorIdx := 1 to 14 do
    begin
      ActiveCols[ColorIdx] := BitArray[BitPos];
      Inc(BitPos);
    end;

    // Color 0 is always 0
    OutPal[PalDestIdx * 2] := 0;
    OutPal[PalDestIdx * 2 + 1] := 0;
    Inc(PalDestIdx);

    for ColorIdx := 1 to 14 do
    begin
      if ActiveCols[ColorIdx] then
      begin
        if BitPos + 9 > TotalBits then Break;

        ColorVal := 0;
        if BitArray[BitPos + 0] then ColorVal := ColorVal or 1024; // R bit 2
        if BitArray[BitPos + 1] then ColorVal := ColorVal or 512;  // R bit 1
        if BitArray[BitPos + 2] then ColorVal := ColorVal or 256;  // R bit 0
        if BitArray[BitPos + 3] then ColorVal := ColorVal or 64;   // G bit 2
        if BitArray[BitPos + 4] then ColorVal := ColorVal or 32;   // G bit 1
        if BitArray[BitPos + 5] then ColorVal := ColorVal or 16;   // G bit 0
        if BitArray[BitPos + 6] then ColorVal := ColorVal or 4;    // B bit 2
        if BitArray[BitPos + 7] then ColorVal := ColorVal or 2;    // B bit 1
        if BitArray[BitPos + 8] then ColorVal := ColorVal or 1;    // B bit 0
        Inc(BitPos, 9);
      end
      else
        ColorVal := 0;

      OutPal[PalDestIdx * 2] := Byte((ColorVal shr 8) and $FF);
      OutPal[PalDestIdx * 2 + 1] := Byte(ColorVal and $FF);
      Inc(PalDestIdx);
    end;

    // Color 15 is always 0
    OutPal[PalDestIdx * 2] := 0;
    OutPal[PalDestIdx * 2 + 1] := 0;
    Inc(PalDestIdx);
  end;

  Result := True;
end;

{ Decodes 320x200 Spectrum 512 image (SPU, SPC, or SPS) with authentic hardware raster timing }
procedure DecodeSpectrum512Screen(const ScreenBuf: PByte; const PalBuf: PByte; OutBmp: Graphics.TBitmap);
var
  Y, Block, X, Shift, ColorIdx, PixelX, LineOffset, BlockOffset, PalIdx: Integer;
  P0, P1, P2, P3: Word;
  LinePalOffset: Integer;
  LinePal: array[0..47] of TColor;
  PalWord: Word;
  C, Calcul: Integer;
  Col: TColor;
begin
  OutBmp.SetSize(320, 200);
  OutBmp.PixelFormat := pf24bit;

  // Scanline 0 is blank/black in Spectrum 512
  for PixelX := 0 to 319 do
    OutBmp.Canvas.Pixels[PixelX, 0] := clBlack;

  // Scanlines 1..199
  for Y := 1 to 199 do
  begin
    LineOffset := Y * 160;
    LinePalOffset := (Y - 1) * 96;

    // Precalculate 48 colors for line Y (3 palettes of 16 colors)
    for PalIdx := 0 to 2 do
    begin
      for C := 0 to 15 do
      begin
        PalWord := ReadBE16(PalBuf, LinePalOffset + PalIdx * 32 + C * 2);
        LinePal[PalIdx * 16 + C] := ConvertAtariColorWord(PalWord, True);
      end;
    end;

    for Block := 0 to 19 do
    begin
      BlockOffset := LineOffset + Block * 8;
      P0 := ReadBE16(ScreenBuf, BlockOffset);
      P1 := ReadBE16(ScreenBuf, BlockOffset + 2);
      P2 := ReadBE16(ScreenBuf, BlockOffset + 4);
      P3 := ReadBE16(ScreenBuf, BlockOffset + 6);

      for X := 0 to 15 do
      begin
        PixelX := Block * 16 + X;
        Shift := 15 - X;
        ColorIdx := ((P0 shr Shift) and 1) or
                    (((P1 shr Shift) and 1) shl 1) or
                    (((P2 shr Shift) and 1) shl 2) or
                    (((P3 shr Shift) and 1) shl 3);

        // Hardware-accurate Spectrum 512 raster interrupt color switching formula
        Calcul := 10 * ColorIdx;
        if Odd(ColorIdx) then
          Dec(Calcul, 5)
        else
          Inc(Calcul);

        if (PixelX >= Calcul) and (PixelX < Calcul + 160) then
          Col := LinePal[16 + ColorIdx]
        else if PixelX >= Calcul + 160 then
          Col := LinePal[32 + ColorIdx]
        else
          Col := LinePal[ColorIdx];

        OutBmp.Canvas.Pixels[PixelX, Y] := Col;
      end;
    end;
  end;
end;

procedure DecodeSpectrum512(const InBuf: PByte; OutBmp: Graphics.TBitmap);
begin
  DecodeSpectrum512Screen(InBuf, InBuf + 32000, OutBmp);
end;

{ Decodes GEM Monochrome .IMG picture }
function DecodeGemImage(const InBuf: PByte; InSize: Integer; OutBmp: Graphics.TBitmap;
  out OutW, OutH: Integer): Boolean;
var
  Version, HeaderLengthWords, NumPlanes, PatternLength, W, H, HeaderBSize: Integer;
  RowBytes, DstLen, Src, Dst, Head, RepeatCount, P, R, CopyCount, C, RunCount, LineRepeat, I, LastLineStart: Integer;
  Val: Byte;
  Decompressed: TBytes;
  Pattern: array[0..255] of Byte;
  Y, X, ByteIdx, BitShift: Integer;
  Col: TColor;
begin
  Result := False;
  OutW := 0;
  OutH := 0;
  if (InBuf = nil) or (InSize < 16) then Exit;

  Version := ReadBE16(InBuf, 0);
  HeaderLengthWords := ReadBE16(InBuf, 2);
  NumPlanes := ReadBE16(InBuf, 4);
  PatternLength := ReadBE16(InBuf, 6);
  W := ReadBE16(InBuf, 12);
  H := ReadBE16(InBuf, 14);

  if (W <= 0) or (H <= 0) or (W > 4096) or (H > 4096) then Exit;
  HeaderBSize := HeaderLengthWords * 2;
  if InSize < HeaderBSize then Exit;

  RowBytes := (W + 7) div 8;
  DstLen := H * RowBytes;
  SetLength(Decompressed, DstLen);
  FillChar(Decompressed[0], DstLen, 0);

  Src := HeaderBSize;
  Dst := 0;

  while (Dst < DstLen) and (Src < InSize) do
  begin
    Head := InBuf[Src];
    Inc(Src);

    if Head = 0 then
    begin
      // Pattern run
      if Src >= InSize then Break;
      RepeatCount := InBuf[Src];
      Inc(Src);
      if PatternLength > 256 then PatternLength := 256;
      for P := 0 to PatternLength - 1 do
      begin
        if Src < InSize then
        begin
          Pattern[P] := InBuf[Src];
          Inc(Src);
        end
        else
          Pattern[P] := 0;
      end;

      for R := 0 to RepeatCount - 1 do
      begin
        for P := 0 to PatternLength - 1 do
        begin
          if Dst < DstLen then
          begin
            Decompressed[Dst] := Pattern[P];
            Inc(Dst);
          end;
        end;
      end;
    end
    else if Head = $80 then
    begin
      // Literal run
      if Src >= InSize then Break;
      CopyCount := InBuf[Src];
      Inc(Src);
      for C := 0 to CopyCount - 1 do
      begin
        if (Src < InSize) and (Dst < DstLen) then
        begin
          Decompressed[Dst] := InBuf[Src];
          Inc(Src);
          Inc(Dst);
        end;
      end;
    end
    else if (Head and $80) <> 0 then
    begin
      // Solid run
      RunCount := Head and $7F;
      if (Head and 1) <> 0 then Val := $FF else Val := $00;
      for R := 0 to RunCount - 1 do
      begin
        if Dst < DstLen then
        begin
          Decompressed[Dst] := Val;
          Inc(Dst);
        end;
      end;
    end
    else
    begin
      // Vertical duplication of previous line
      LineRepeat := Head;
      if (LineRepeat > 0) and (Dst >= RowBytes) then
      begin
        LastLineStart := Dst - RowBytes;
        for R := 0 to LineRepeat - 1 do
        begin
          for I := 0 to RowBytes - 1 do
          begin
            if Dst < DstLen then
            begin
              Decompressed[Dst] := Decompressed[LastLineStart + I];
              Inc(Dst);
            end;
          end;
        end;
      end;
    end;
  end;

  OutBmp.SetSize(W, H);
  OutBmp.PixelFormat := pf24bit;

  for Y := 0 to H - 1 do
  begin
    for X := 0 to W - 1 do
    begin
      ByteIdx := Y * RowBytes + (X div 8);
      BitShift := 7 - (X mod 8);
      if ByteIdx < DstLen then
      begin
        if ((Decompressed[ByteIdx] shr BitShift) and 1) <> 0 then
          Col := clBlack
        else
          Col := clWhite;
      end
      else
        Col := clWhite;
      OutBmp.Canvas.Pixels[X, Y] := Col;
    end;
  end;

  OutW := W;
  OutH := H;
  Result := True;
end;

type
  TDTHuffNode = record
    Left, Right: SmallInt;
  end;

function DecodeTitusPC1(const InBuf: PByte; InSize: Integer;
  out OutScreen: TBytes; out OutPal: array of TColor): Boolean;
var
  Method: Word;
  UncompressedSize: LongWord;
  Pos: Integer;
  TreeSize: SmallInt;
  Tree: array of TDTHuffNode;
  BitBuf: Byte;
  BitCnt: Integer;
  Node: SmallInt;
  Val: Integer;
  StreamBuf: TBytes;
  StreamLen: Integer;

  NextCode, NBits, MaxCode: Integer;
  ForceReload: Boolean;
  LZWBlock: array[0..15] of Byte;
  LZWBlockLen: Integer;
  LZWBitOff, LZWBitLim: Integer;
  Prefix: array[0..5002] of Word;
  Suffix: array[0..5002] of Byte;
  Stack: array[0..5002] of Byte;
  StackPtr: Integer;
  Code, OldCode, InCode: Integer;
  FinChar: Byte;
  D6, D5, ByteIdx, BitIdx: Integer;
  D0Val, D7Val, D0Len, D6Shift, D1Val: Integer;
  I: Integer;

  function LZWReadCode: Integer;
  var
    J: Integer;
  begin
    if ForceReload or (LZWBitOff >= LZWBitLim) or (NextCode > MaxCode) then
    begin
      if NextCode > MaxCode then
      begin
        Inc(NBits);
        if NBits = 12 then
          MaxCode := 4096
        else
          MaxCode := (1 shl NBits) - 1;
      end;
      ForceReload := False;
      LZWBlockLen := 0;
      for J := 0 to NBits - 1 do
      begin
        if Pos < InSize then
        begin
          LZWBlock[LZWBlockLen] := InBuf[Pos];
          Inc(Pos);
          Inc(LZWBlockLen);
        end
        else
          Break;
      end;
      if LZWBlockLen = 0 then Exit(-1);
      LZWBitOff := 0;
      LZWBitLim := LZWBlockLen * 8 - (NBits - 1);
    end;

    D6 := LZWBitOff;
    D5 := NBits;
    ByteIdx := D6 shr 3;
    BitIdx := D6 and 7;
    if ByteIdx >= LZWBlockLen then Exit(-1);

    D0Val := LZWBlock[ByteIdx];
    Inc(ByteIdx);
    D7Val := D0Val shr BitIdx;
    D0Len := 8 - BitIdx;
    Dec(D5, D0Len);
    D6Shift := D0Len;

    if D5 >= 8 then
    begin
      if ByteIdx < LZWBlockLen then
      begin
        D1Val := LZWBlock[ByteIdx];
        Inc(ByteIdx);
        D7Val := D7Val or (D1Val shl D6Shift);
        Inc(D6Shift, 8);
        Dec(D5, 8);
      end;
    end;

    if D5 > 0 then
    begin
      if ByteIdx < LZWBlockLen then
      begin
        D7Val := D7Val or ((LZWBlock[ByteIdx] and ((1 shl D5) - 1)) shl D6Shift);
      end;
    end;

    Inc(LZWBitOff, NBits);
    Result := D7Val;
  end;

var
  SPos: Integer;
  Red, Green, Blue: array[0..15] of Byte;
  Ctrl: TBytes;
  CtrlLen: Integer;
  B, Cnt, Rep: Byte;
  Plane, Col, Block, Bit, CIdx, Addr: Integer;
  D3, D0: Byte;
begin
  Result := False;
  SetLength(OutScreen, 0);
  if (InBuf = nil) or (InSize < 10) then Exit;

  Method := Word(InBuf[0]) or (Word(InBuf[1]) shl 8);
  UncompressedSize := LongWord(InBuf[2]) or (LongWord(InBuf[3]) shl 8) or
                      (LongWord(InBuf[4]) shl 16) or (LongWord(InBuf[5]) shl 24);

  if (UncompressedSize = 0) or (UncompressedSize > 1000000) then Exit;
  SetLength(StreamBuf, UncompressedSize);
  StreamLen := 0;

  if Method = 1 then
  begin
    Pos := 10;
    while (Pos < InSize) and (InBuf[Pos] <> 0) do Inc(Pos);
    Inc(Pos);
    if Pos + 2 > InSize then Exit;
    TreeSize := SmallInt(Word(InBuf[Pos]) or (Word(InBuf[Pos+1]) shl 8));
    Inc(Pos, 2);
    if (TreeSize <= 0) or (Pos + TreeSize * 4 > InSize) then Exit;

    SetLength(Tree, TreeSize);
    for I := 0 to TreeSize - 1 do
    begin
      Tree[I].Left := SmallInt(Word(InBuf[Pos]) or (Word(InBuf[Pos+1]) shl 8));
      Tree[I].Right := SmallInt(Word(InBuf[Pos+2]) or (Word(InBuf[Pos+3]) shl 8));
      Inc(Pos, 4);
    end;

    BitBuf := 0;
    BitCnt := 8;
    while (StreamLen < UncompressedSize) and (Pos < InSize) do
    begin
      Node := 0;
      while Node >= 0 do
      begin
        if BitCnt >= 8 then
        begin
          if Pos >= InSize then Break;
          BitBuf := InBuf[Pos];
          Inc(Pos);
          BitCnt := 0;
        end;
        Bit := (BitBuf shr BitCnt) and 1;
        Inc(BitCnt);
        if Bit = 0 then
          Node := Tree[Node].Left
        else
          Node := Tree[Node].Right;
      end;
      if Node < 0 then
      begin
        Val := -(Node + 1);
        if Val = 256 then Break;
        StreamBuf[StreamLen] := Byte(Val and $FF);
        Inc(StreamLen);
      end;
    end;
  end
  else if Method = 2 then
  begin
    Pos := 7;
    NBits := 9;
    MaxCode := 511;
    NextCode := $101;
    LZWBlockLen := 0;
    LZWBitOff := 0;
    LZWBitLim := 0;
    ForceReload := False;

    for I := 0 to 255 do
      Suffix[I] := Byte(I);
    FillChar(Prefix, SizeOf(Prefix), 0);

    Code := LZWReadCode;
    if Code = -1 then Exit;
    FinChar := Byte(Code and $FF);
    OldCode := FinChar;
    StreamBuf[StreamLen] := FinChar;
    Inc(StreamLen);
    StackPtr := 0;

    while StreamLen < UncompressedSize do
    begin
      Code := LZWReadCode;
      if Code = -1 then Break;
      if Code = $100 then
      begin
        NextCode := $100;
        ForceReload := True;
        NBits := 9;
        MaxCode := 511;
        Code := LZWReadCode;
        if Code = -1 then Break;
      end;

      InCode := Code;
      if Code >= NextCode then
      begin
        Stack[StackPtr] := FinChar;
        Inc(StackPtr);
        Code := OldCode;
      end;

      while Code >= $100 do
      begin
        Stack[StackPtr] := Suffix[Code];
        Inc(StackPtr);
        Code := Prefix[Code];
      end;

      FinChar := Suffix[Code] and $FF;
      Stack[StackPtr] := FinChar;
      Inc(StackPtr);

      while (StackPtr > 0) and (StreamLen < UncompressedSize) do
      begin
        Dec(StackPtr);
        StreamBuf[StreamLen] := Stack[StackPtr];
        Inc(StreamLen);
      end;

      if NextCode < 4096 then
      begin
        Prefix[NextCode] := OldCode;
        Suffix[NextCode] := FinChar;
        Inc(NextCode);
      end;
      OldCode := InCode;
    end;
  end
  else
    Exit(False);

  if StreamLen < 48 + 4 then Exit(False);

  SPos := 0;
  Move(StreamBuf[SPos], Red[0], 16); Inc(SPos, 16);
  Move(StreamBuf[SPos], Green[0], 16); Inc(SPos, 16);
  Move(StreamBuf[SPos], Blue[0], 16); Inc(SPos, 16);
  for I := 0 to 15 do
  begin
    OutPal[I] := RGBToColor(
      (Integer(Red[I] shr 5) * 255) div 7,
      (Integer(Green[I] shr 5) * 255) div 7,
      (Integer(Blue[I] shr 5) * 255) div 7
    );
  end;
  Inc(SPos, 4);

  SetLength(Ctrl, 4000);
  CtrlLen := 0;
  while (CtrlLen < 4000) and (SPos < StreamLen) do
  begin
    B := StreamBuf[SPos];
    Inc(SPos);
    if B = $80 then Continue;
    if B < $80 then
    begin
      Cnt := B + 1;
      while (Cnt > 0) and (SPos < StreamLen) and (CtrlLen < 4000) do
      begin
        Ctrl[CtrlLen] := StreamBuf[SPos];
        Inc(SPos);
        Inc(CtrlLen);
        Dec(Cnt);
      end;
    end
    else
    begin
      Cnt := (256 - B) + 1;
      if SPos < StreamLen then
      begin
        Rep := StreamBuf[SPos];
        Inc(SPos);
        while (Cnt > 0) and (CtrlLen < 4000) do
        begin
          Ctrl[CtrlLen] := Rep;
          Inc(CtrlLen);
          Dec(Cnt);
        end;
      end;
    end;
  end;
  Inc(SPos, 4);

  if CtrlLen < 4000 then Exit(False);

  SetLength(OutScreen, 32000);
  FillChar(OutScreen[0], 32000, 0);
  CIdx := 0;

  for Plane := 0 to 3 do
  begin
    for Col := 0 to 19 do
    begin
      for Block := 0 to 24 do
      begin
        D3 := Ctrl[CIdx]; Inc(CIdx);
        for Bit := 0 to 7 do
        begin
          Addr := (Block * 8 + Bit) * 160 + Col * 8 + Plane * 2;
          if ((D3 shr Bit) and 1) = 0 then
          begin
            if SPos < StreamLen then
            begin
              D0 := StreamBuf[SPos];
              Inc(SPos);
            end;
          end;
          OutScreen[Addr] := D0;
        end;
      end;

      for Block := 0 to 24 do
      begin
        D3 := Ctrl[CIdx]; Inc(CIdx);
        for Bit := 0 to 7 do
        begin
          Addr := (Block * 8 + Bit) * 160 + Col * 8 + Plane * 2 + 1;
          if ((D3 shr Bit) and 1) = 0 then
          begin
            if SPos < StreamLen then
            begin
              D0 := StreamBuf[SPos];
              Inc(SPos);
            end;
          end;
          OutScreen[Addr] := D0;
        end;
      end;
    end;
  end;

  Result := True;
end;

function DecodeDuckTalesPC1(const InBuf: PByte; InSize: Integer;
  out OutScreen: TBytes; out OutPal: array of TColor): Boolean;
begin
  Result := DecodeTitusPC1(InBuf, InSize, OutScreen, OutPal);
end;

{ Decodes Titus SQ compressed 4-plane screen (.SQ / .SQZ / .SQH) }
function DecodeTitusSQ(const InBuf: PByte; InSize: Integer;
  out OutScreen: TBytes; out OutPal: array of TColor; out OutW, OutH: Integer): Boolean;
const
  TitusDefaultPalWords: array[0..15] of Word = (
    $0FFF, $0000, $0688, $0455, $0FCC, $0F88, $0C02, $0A00,
    $0800, $0400, $0899, $09AA, $0ABB, $0BCC, $0CDD, $0DEE
  );
var
  B1, B3, B4: Byte;
  OutLen, TreeSize, OutPos: Integer;
  Node, BinTree, Count, J: Word;
  NodeL, Bit, LastVal: Byte;
  State, I, Y, Block, LineOffset, BlockOffset: Integer;
  RawOutput: TBytes;
  W: Word;
  R, G, B: Byte;
begin
  Result := False;
  SetLength(OutScreen, 0);
  OutW := 0;
  OutH := 0;
  if (InBuf = nil) or (InSize < 8) then Exit;

  B1 := InBuf[0];
  B3 := InBuf[2];
  B4 := InBuf[3];
  OutLen := ((Integer(B1) and $0F) shl 16) or (Integer(B4) shl 8) or Integer(B3);

  if (OutLen <= 0) or (OutLen > 500000) then Exit;

  TreeSize := (Word(InBuf[5]) shl 8) or Word(InBuf[4]);
  if (TreeSize <= 0) or (6 + TreeSize > InSize) then Exit;

  SetLength(RawOutput, OutLen);
  Node := 0;
  State := 0;
  OutPos := 0;
  LastVal := 0;

  for I := 6 + TreeSize to InSize - 1 do
  begin
    Bit := 128;
    while Bit >= 1 do
    begin
      if (InBuf[I] and Bit) <> 0 then
        Inc(Node);

      if 7 + Node * 2 >= InSize then Exit(False);
      BinTree := (Word(InBuf[7 + Node * 2]) shl 8) or Word(InBuf[6 + Node * 2]);

      if BinTree <= $7FFF then
        Node := BinTree shr 1
      else
      begin
        Node := BinTree and $7FFF;
        NodeL := Byte(Node and $FF);

        case State of
          0:
          begin
            if Node < $100 then
            begin
              LastVal := NodeL;
              if OutPos < OutLen then
              begin
                RawOutput[OutPos] := LastVal;
                Inc(OutPos);
              end;
            end
            else if NodeL = 0 then
              State := 1
            else if NodeL = 1 then
              State := 2
            else
            begin
              for J := 1 to NodeL do
              begin
                if OutPos < OutLen then
                begin
                  RawOutput[OutPos] := LastVal;
                  Inc(OutPos);
                end;
              end;
            end;
          end;
          1:
          begin
            for J := 1 to Node do
            begin
              if OutPos < OutLen then
              begin
                RawOutput[OutPos] := LastVal;
                Inc(OutPos);
              end;
            end;
            State := 0;
          end;
          2:
          begin
            Count := 256 * NodeL;
            State := 3;
          end;
          3:
          begin
            Inc(Count, NodeL);
            for J := 1 to Count do
            begin
              if OutPos < OutLen then
              begin
                RawOutput[OutPos] := LastVal;
                Inc(OutPos);
              end;
            end;
            State := 0;
          end;
        end;
        Node := 0;
      end;
      Bit := Bit shr 1;
    end;
  end;

  if OutPos < OutLen then Exit(False);

  // Set default Titus palette
  for I := 0 to 15 do
  begin
    W := TitusDefaultPalWords[I];
    R := (((W shr 8) and 7) * 2 + ((W shr 11) and 1)) * 255 div 15;
    G := (((W shr 4) and 7) * 2 + ((W shr 7) and 1)) * 255 div 15;
    B := ((W and 7) * 2 + ((W shr 3) and 1)) * 255 div 15;
    OutPal[I] := RGBToColor(R, G, B);
  end;

  OutW := 320;
  if OutLen >= 40000 then
    OutH := 250
  else
    OutH := 200;

  // Interleave planar scanlines into standard Atari ST format (160 bytes per scanline)
  SetLength(OutScreen, OutH * 160);
  FillChar(OutScreen[0], Length(OutScreen), 0);

  for Y := 0 to OutH - 1 do
  begin
    LineOffset := Y * 160;
    if LineOffset + 160 > OutLen then Break;

    for Block := 0 to 19 do
    begin
      BlockOffset := LineOffset + Block * 8;
      // Word 0 (Plane 0)
      OutScreen[BlockOffset]     := RawOutput[LineOffset + Block * 2];
      OutScreen[BlockOffset + 1] := RawOutput[LineOffset + Block * 2 + 1];
      // Word 1 (Plane 1)
      OutScreen[BlockOffset + 2] := RawOutput[LineOffset + 40 + Block * 2];
      OutScreen[BlockOffset + 3] := RawOutput[LineOffset + 40 + Block * 2 + 1];
      // Word 2 (Plane 2)
      OutScreen[BlockOffset + 4] := RawOutput[LineOffset + 80 + Block * 2];
      OutScreen[BlockOffset + 5] := RawOutput[LineOffset + 80 + Block * 2 + 1];
      // Word 3 (Plane 3)
      OutScreen[BlockOffset + 6] := RawOutput[LineOffset + 120 + Block * 2];
      OutScreen[BlockOffset + 7] := RawOutput[LineOffset + 120 + Block * 2 + 1];
    end;
  end;

  Result := True;
end;

function DecodeAtariPicture(const InBuf: PByte; InSize: Integer; const FileName: string;
  out OutBitmap: Graphics.TBitmap; out FormatDesc: string;
  out OutWidth, OutHeight, OutColors: Integer): Boolean;
var
  PicFmt: TAtariPicFormat;
  Pal: array[0..15] of TColor;
  DtPal: array[0..15] of TColor;
  DtScreen: TBytes;
  IsSTE, IsCompressed: Boolean;
  I, DefRes, PalOffset, ScreenOffset: Integer;
  UnpackedBuf, StadVRAM, StosPal, StosVRAM, TinyPal, TinyVRAM, UnsquashedBuf, PackedDepackBuf: TBytes;
  SpcScreen, SpcPal, SpsScreen, SpsPal: TBytes;
  StosRes, TinyRes: Integer;
  WorkBuf: PByte;
  WorkSize: Integer;
  DepackedPacker: string;
  GemW, GemH: Integer;
  Ext: string;
begin
  Result := False;
  OutBitmap := nil;
  FormatDesc := '';
  OutWidth := 0;
  OutHeight := 0;
  OutColors := 0;

  if (InBuf = nil) or (InSize < 7) then Exit;

  PicFmt := DetectAtariPicFormat(InBuf, InSize, FileName);
  if PicFmt = apfUnknown then Exit;

  IsCompressed := PicFmt in [apfDegasPC1, apfDegasPC2, apfDegasPC3];

  // Handle DEGAS Elite RLE Compressed Formats (.PC1, .PC2, .PC3)
  if IsCompressed then
  begin
    case PicFmt of
      apfDegasPC1: DefRes := 0;
      apfDegasPC2: DefRes := 1;
      apfDegasPC3: DefRes := 2;
    else
      DefRes := 0;
    end;

    if not DecompressDegasElite(InBuf, InSize, DefRes, UnpackedBuf) then
    begin
      // Fallback for Titus PC1 if misclassified as DEGAS
      if PicFmt = apfDegasPC1 then
      begin
        OutBitmap := Graphics.TBitmap.Create;
        try
          if DecodeTitusPC1(InBuf, InSize, DtScreen, DtPal) then
          begin
            for I := 0 to 15 do
              Pal[I] := DtPal[I];
            DecodeLowResScreen(@DtScreen[0], Pal, OutBitmap);
            FormatDesc := 'Compressed Low Res Picture (320x200, 16 Colors)';
            OutWidth := 320;
            OutHeight := 200;
            OutColors := 16;
            Exit(True);
          end;
        except
          FreeAndNil(OutBitmap);
        end;
      end;
      Exit(False);
    end;

    if Length(UnpackedBuf) < 32034 then Exit(False);
    WorkBuf := @UnpackedBuf[0];
    WorkSize := Length(UnpackedBuf);
    case ReadBE16(WorkBuf, 0) of
      0: PicFmt := apfDegasPI1;
      1: PicFmt := apfDegasPI2;
      2: PicFmt := apfDegasPI3;
    end;
  end
  else
  begin
    WorkBuf := InBuf;
    WorkSize := InSize;

    // Auto-depack if buffer is packed with an Atari packer (e.g. Pack-Ice/TSM!, Atomik, Automation, etc.)
    if (PicFmt in [apfDegasPI1, apfDegasPI2, apfDegasPI3, apfNeoChrome, apfRawScreen]) and
       ((WorkSize < 32000) or (DetectPackerSignature(WorkBuf, WorkSize) <> 'None')) then
    begin
      if DepackBuffer(WorkBuf, WorkSize, PackedDepackBuf, DepackedPacker) and (Length(PackedDepackBuf) >= 32000) then
      begin
        WorkBuf := @PackedDepackBuf[0];
        WorkSize := Length(PackedDepackBuf);
      end;
    end;
  end;

  OutBitmap := Graphics.TBitmap.Create;
  try
    case PicFmt of
      apfDegasPI1:
      begin
        IsSTE := HasSTEBits(WorkBuf + 2, 16);
        for I := 0 to 15 do
          Pal[I] := ConvertAtariColorWord(ReadBE16(WorkBuf, 2 + I * 2), IsSTE);

        DecodeLowResScreen(WorkBuf + 34, Pal, OutBitmap);
        if IsCompressed then
          FormatDesc := 'DEGAS Elite RLE Low Res (320x200, 16 Colors)'
        else
          FormatDesc := 'DEGAS Low Res (320x200, 16 Colors)';
        OutWidth := 320;
        OutHeight := 200;
        OutColors := 16;
        Result := True;
      end;

      apfDegasPI2:
      begin
        IsSTE := HasSTEBits(WorkBuf + 2, 4);
        for I := 0 to 3 do
          Pal[I] := ConvertAtariColorWord(ReadBE16(WorkBuf, 2 + I * 2), IsSTE);

        DecodeMedResScreen(WorkBuf + 34, Pal, OutBitmap);
        if IsCompressed then
          FormatDesc := 'DEGAS Elite RLE Med Res (640x200 / 640x400, 4 Colors)'
        else
          FormatDesc := 'DEGAS Med Res (640x200 / 640x400, 4 Colors)';
        OutWidth := 640;
        OutHeight := 400;
        OutColors := 4;
        Result := True;
      end;

      apfDegasPI3:
      begin
        Pal[0] := ConvertAtariColorWord(ReadBE16(WorkBuf, 2), False);
        Pal[1] := ConvertAtariColorWord(ReadBE16(WorkBuf, 4), False);
        if Pal[0] = Pal[1] then
        begin
          Pal[0] := clWhite;
          Pal[1] := clBlack;
        end;

        DecodeHighResScreen(WorkBuf + 34, Pal[0], Pal[1], OutBitmap);
        if IsCompressed then
          FormatDesc := 'DEGAS Elite RLE High Res (640x400, Monochrome)'
        else
          FormatDesc := 'DEGAS High Res (640x400, Monochrome)';
        OutWidth := 640;
        OutHeight := 400;
        OutColors := 2;
        Result := True;
      end;

      apfNeoChrome:
      begin
        // Palette starts at offset 4, screen at offset 128
        IsSTE := HasSTEBits(WorkBuf + 4, 16);
        for I := 0 to 15 do
          Pal[I] := ConvertAtariColorWord(ReadBE16(WorkBuf, 4 + I * 2), IsSTE);

        DecodeLowResScreen(WorkBuf + 128, Pal, OutBitmap);
        FormatDesc := 'NeoChrome Image (320x200, 16 Colors)';
        OutWidth := 320;
        OutHeight := 200;
        OutColors := 16;
        Result := True;
      end;

      apfSpectrum512SPU:
      begin
        if WorkSize < 51104 then Exit(False);
        DecodeSpectrum512Screen(WorkBuf, WorkBuf + 32000, OutBitmap);
        FormatDesc := 'Spectrum 512 Uncompressed (320x200, 512 Colors)';
        OutWidth := 320;
        OutHeight := 200;
        OutColors := 512;
        Result := True;
      end;

      apfSpectrum512SPC:
      begin
        if not DecompressSpectrum512SPC(WorkBuf, WorkSize, SpcScreen, SpcPal) then Exit(False);
        DecodeSpectrum512Screen(@SpcScreen[0], @SpcPal[0], OutBitmap);
        FormatDesc := 'Spectrum 512 Compressed (320x200, 512 Colors)';
        OutWidth := 320;
        OutHeight := 200;
        OutColors := 512;
        Result := True;
      end;

      apfSpectrum512SPS:
      begin
        if not DecompressSpectrum512SPS(WorkBuf, WorkSize, SpsScreen, SpsPal) then Exit(False);
        DecodeSpectrum512Screen(@SpsScreen[0], @SpsPal[0], OutBitmap);
        FormatDesc := 'Spectrum 512 Smooshed (320x200, 512 Colors)';
        OutWidth := 320;
        OutHeight := 200;
        OutColors := 512;
        Result := True;
      end;

      apfRawScreen:
      begin
        if WorkSize >= 32032 then
        begin
          // If 32,032 bytes, check if palette is at front or back
          if HasSTEBits(WorkBuf + 32000, 16) or (ReadBE16(WorkBuf + 32000, 0) <> 0) then
          begin
            IsSTE := HasSTEBits(WorkBuf + 32000, 16);
            for I := 0 to 15 do
              Pal[I] := ConvertAtariColorWord(ReadBE16(WorkBuf, 32000 + I * 2), IsSTE);
            DecodeLowResScreen(WorkBuf, Pal, OutBitmap);
          end
          else
          begin
            IsSTE := HasSTEBits(WorkBuf, 16);
            for I := 0 to 15 do
              Pal[I] := ConvertAtariColorWord(ReadBE16(WorkBuf, I * 2), IsSTE);
            DecodeLowResScreen(WorkBuf + 32, Pal, OutBitmap);
          end;
        end
        else
        begin
          // Raw 32,000 bytes with default palette
          for I := 0 to 15 do
            Pal[I] := GetDefaultSTPalette(I);
          DecodeLowResScreen(WorkBuf, Pal, OutBitmap);
        end;

        FormatDesc := 'Raw Atari ST Screen (320x200, 16 Colors)';
        OutWidth := 320;
        OutHeight := 200;
        OutColors := 16;
        Result := True;
      end;

      apfArtDirector:
      begin
        if WorkSize < 32032 then Exit(False);
        // Art Director (.ART) is standard 32,000 bytes screen data at offset 0,
        // followed by 32 bytes (16 words) Atari ST palette at offset 32000.
        // Some converters might prepend a 2-byte resolution word (32,034 bytes).
        if (WorkSize >= 32034) and (ReadBE16(WorkBuf, 0) in [0..2]) then
        begin
          ScreenOffset := 34;
          PalOffset := 2;
        end
        else
        begin
          ScreenOffset := 0;
          PalOffset := 32000;
        end;

        IsSTE := HasSTEBits(WorkBuf + PalOffset, 16);
        for I := 0 to 15 do
          Pal[I] := ConvertAtariColorWord(ReadBE16(WorkBuf, PalOffset + I * 2), IsSTE);
        DecodeLowResScreen(WorkBuf + ScreenOffset, Pal, OutBitmap);
        FormatDesc := 'Art Director (320x200, 16 Colors)';
        OutWidth := 320;
        OutHeight := 200;
        OutColors := 16;
        Result := True;
      end;

      apfMurMural:
      begin
        if WorkSize < 32000 then Exit(False);
        for I := 0 to 15 do
          Pal[I] := ConvertAtariColorWord(COLR_MURAL_PALETTE[I], False);
        DecodeLowResScreen(WorkBuf, Pal, OutBitmap);
        FormatDesc := 'C.O.L.R. Object Editor Mural (320x200, 16 Colors)';
        OutWidth := 320;
        OutHeight := 200;
        OutColors := 16;
        Result := True;
      end;

      apfDoodle:
      begin
        if WorkSize < 32000 then Exit(False);
        Pal[0] := clWhite;
        Pal[1] := clBlack;
        DecodeHighResScreen(WorkBuf, Pal[0], Pal[1], OutBitmap);
        FormatDesc := 'Doodle High Res (640x400, Monochrome)';
        OutWidth := 640;
        OutHeight := 400;
        OutColors := 2;
        Result := True;
      end;

      apfQuantumPaint:
      begin
        if WorkSize < 32032 then Exit(False);
        if CheckString(WorkBuf, WorkSize, 0, 'PBX') then
        begin
          PalOffset := 4;
          if WorkSize >= 32064 then
            ScreenOffset := 64
          else
            ScreenOffset := 32;
        end
        else
        begin
          PalOffset := 0;
          ScreenOffset := 32;
        end;
        IsSTE := HasSTEBits(WorkBuf + PalOffset, 16);
        for I := 0 to 15 do
          Pal[I] := ConvertAtariColorWord(ReadBE16(WorkBuf, PalOffset + I * 2), IsSTE);
        DecodeLowResScreen(WorkBuf + ScreenOffset, Pal, OutBitmap);
        FormatDesc := 'Quantum Paintbox (320x200, 16 Colors)';
        OutWidth := 320;
        OutHeight := 200;
        OutColors := 16;
        Result := True;
      end;

      apfTinyStuff:
      begin
        Ext := UpperCase(ExtractFileExt(FileName));
        if Ext = '.TN1' then DefRes := 0
        else if Ext = '.TN2' then DefRes := 1
        else if Ext = '.TN3' then DefRes := 2
        else DefRes := 0;

        if not DecompressTiny(WorkBuf, WorkSize, DefRes, TinyPal, TinyVRAM, TinyRes) then Exit(False);

        if TinyRes = 0 then
        begin
          IsSTE := HasSTEBits(@TinyPal[0], 16);
          for I := 0 to 15 do
            Pal[I] := ConvertAtariColorWord(ReadBE16(@TinyPal[0], I * 2), IsSTE);
          DecodeLowResScreen(@TinyVRAM[0], Pal, OutBitmap);
          FormatDesc := 'Tiny Stuff Low Res (320x200, 16 Colors)';
          OutWidth := 320;
          OutHeight := 200;
          OutColors := 16;
          Result := True;
        end
        else if TinyRes = 1 then
        begin
          IsSTE := HasSTEBits(@TinyPal[0], 4);
          for I := 0 to 3 do
            Pal[I] := ConvertAtariColorWord(ReadBE16(@TinyPal[0], I * 2), IsSTE);
          DecodeMedResScreen(@TinyVRAM[0], Pal, OutBitmap);
          FormatDesc := 'Tiny Stuff Med Res (640x200 / 640x400, 4 Colors)';
          OutWidth := 640;
          OutHeight := 400;
          OutColors := 4;
          Result := True;
        end
        else
        begin
          Pal[0] := clWhite;
          Pal[1] := clBlack;
          DecodeHighResScreen(@TinyVRAM[0], Pal[0], Pal[1], OutBitmap);
          FormatDesc := 'Tiny Stuff High Res (640x400, Monochrome)';
          OutWidth := 640;
          OutHeight := 400;
          OutColors := 2;
          Result := True;
        end;
      end;

      apfStadPac:
      begin
        if not DecompressStad(WorkBuf, WorkSize, StadVRAM) then Exit(False);
        Pal[0] := clWhite;
        Pal[1] := clBlack;
        DecodeHighResScreen(@StadVRAM[0], Pal[0], Pal[1], OutBitmap);
        FormatDesc := 'STAD Monochrome Image (640x400, Monochrome)';
        OutWidth := 640;
        OutHeight := 400;
        OutColors := 2;
        Result := True;
      end;

      apfStosCompactPac:
      begin
        if CheckLong(WorkBuf, WorkSize, 0, $06071963) then
        begin
          if not DecompressStosCompact(WorkBuf, WorkSize, StosPal, StosVRAM, StosRes) then Exit(False);
        end
        else
        begin
          // Try decompressing with STOS Squasher first
          if not DecompressStosSquash(WorkBuf, WorkSize, UnsquashedBuf) then Exit(False);
          if not DecompressStosCompact(@UnsquashedBuf[0], Length(UnsquashedBuf), StosPal, StosVRAM, StosRes) then Exit(False);
        end;

        if StosRes = 0 then
        begin
          IsSTE := HasSTEBits(@StosPal[0], 16);
          for I := 0 to 15 do
            Pal[I] := ConvertAtariColorWord(ReadBE16(@StosPal[0], I * 2), IsSTE);
          DecodeLowResScreen(@StosVRAM[0], Pal, OutBitmap);
          FormatDesc := 'STOS Packed Screen (320x200, 16 Colors)';
          OutWidth := 320;
          OutHeight := 200;
          OutColors := 16;
          Result := True;
        end
        else if StosRes = 1 then
        begin
          IsSTE := HasSTEBits(@StosPal[0], 4);
          for I := 0 to 3 do
            Pal[I] := ConvertAtariColorWord(ReadBE16(@StosPal[0], I * 2), IsSTE);
          DecodeMedResScreen(@StosVRAM[0], Pal, OutBitmap);
          FormatDesc := 'STOS Packed Screen (640x200 / 640x400, 4 Colors)';
          OutWidth := 640;
          OutHeight := 400;
          OutColors := 4;
          Result := True;
        end
        else
        begin
          Pal[0] := clWhite;
          Pal[1] := clBlack;
          DecodeHighResScreen(@StosVRAM[0], Pal[0], Pal[1], OutBitmap);
          FormatDesc := 'STOS Packed Screen (640x400, Monochrome)';
          OutWidth := 640;
          OutHeight := 400;
          OutColors := 2;
          Result := True;
        end;
      end;

      apfGemImg:
      begin
        if not DecodeGemImage(WorkBuf, WorkSize, OutBitmap, GemW, GemH) then Exit(False);
        FormatDesc := Format('GEM Monochrome Image (%dx%d, 1-bit)', [GemW, GemH]);
        OutWidth := GemW;
        OutHeight := GemH;
        OutColors := 2;
        Result := True;
      end;

      apfTitusPC1:
      begin
        if DecodeTitusPC1(WorkBuf, WorkSize, DtScreen, DtPal) then
        begin
          for I := 0 to 15 do
            Pal[I] := DtPal[I];
          DecodeLowResScreen(@DtScreen[0], Pal, OutBitmap);
          FormatDesc := 'Compressed Low Res Picture (320x200, 16 Colors)';
          OutWidth := 320;
          OutHeight := 200;
          OutColors := 16;
          Result := True;
        end
        else
        begin
          // Fallback to DEGAS Elite RLE if compressed stream was not Titus format
          if DecompressDegasElite(WorkBuf, WorkSize, 0, UnpackedBuf) and (Length(UnpackedBuf) >= 32034) then
          begin
            IsSTE := HasSTEBits(@UnpackedBuf[2], 16);
            for I := 0 to 15 do
              Pal[I] := ConvertAtariColorWord(ReadBE16(@UnpackedBuf[0], 2 + I * 2), IsSTE);
            DecodeLowResScreen(@UnpackedBuf[34], Pal, OutBitmap);
            FormatDesc := 'DEGAS Elite RLE Low Res (320x200, 16 Colors)';
            OutWidth := 320;
            OutHeight := 200;
            OutColors := 16;
            Result := True;
          end
          else
            Exit(False);
        end;
      end;

      apfTitusSQ:
      begin
        if DecodeTitusSQ(WorkBuf, WorkSize, DtScreen, DtPal, GemW, GemH) then
        begin
          for I := 0 to 15 do
            Pal[I] := DtPal[I];
          DecodeLowResScreenEx(@DtScreen[0], Pal, OutBitmap, GemW, GemH);
          FormatDesc := Format('Titus Compressed Screen (%dx%d, 16 Colors)', [GemW, GemH]);
          OutWidth := GemW;
          OutHeight := GemH;
          OutColors := 16;
          Result := True;
        end
        else
          Exit(False);
      end;
    end;
  except
    FreeAndNil(OutBitmap);
    Result := False;
  end;
end;

end.
