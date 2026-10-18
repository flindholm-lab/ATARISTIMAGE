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
    apfSpectrum512SPU, // Spectrum 512 (.SPU) (320x200, 512 colors per-line palettes)
    apfRawScreen,      // Raw 32,000 or 32,032 bytes Atari ST screen dump
    apfArtDirector,    // Art Director (.ART) (320x200, 16 colors)
    apfDoodle,         // Doodle (.DOO) (640x400, monochrome)
    apfQuantumPaint,   // Quantum Paintbox (.PBX) (320x200, 16 colors)
    apfTinyStuff,      // Tiny Stuff (.TN1, .TN2, .TN3, .TNY)
    apfStadPac,        // STAD Monochrome Packed (.PAC - pM85/pM86)
    apfStosCompactPac, // STOS Picture Compactor / Squashed (.PAC)
    apfGemImg          // GEM Monochrome Image (.IMG)
  );

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
    apfRawScreen:      Result := 'Raw Atari ST Screen Dump (320x200)';
    apfArtDirector:    Result := 'Art Director Image (.ART)';
    apfDoodle:         Result := 'Doodle High Res (.DOO)';
    apfQuantumPaint:   Result := 'Quantum Paintbox (.PBX)';
    apfTinyStuff:      Result := 'Tiny Stuff Compressed Image (.TNY)';
    apfStadPac:        Result := 'STAD Monochrome Image (.PAC)';
    apfStosCompactPac: Result := 'STOS Packed Picture (.PAC)';
    apfGemImg:         Result := 'GEM Monochrome Image (.IMG)';
  else
    Result := 'Unknown Picture Format';
  end;
end;

function DetectAtariPicFormat(const InBuf: PByte; InSize: Integer; const FileName: string): TAtariPicFormat;
var
  Ext: string;
  ResWord: Word;
begin
  Result := apfUnknown;
  if (InBuf = nil) or (InSize < 7) then Exit;

  Ext := UpperCase(ExtractFileExt(FileName));

  // 1. Check DEGAS Elite RLE extensions
  if (Ext = '.PC1') then Exit(apfDegasPC1);
  if (Ext = '.PC2') then Exit(apfDegasPC2);
  if (Ext = '.PC3') then Exit(apfDegasPC3);

  // 1b. Check DEGAS Elite magic resolution words ($8000, $8001, $8002)
  if InSize >= 34 then
  begin
    ResWord := ReadBE16(InBuf, 0);
    if (ResWord = $8000) then Exit(apfDegasPC1);
    if (ResWord = $8001) then Exit(apfDegasPC2);
    if (ResWord = $8002) then Exit(apfDegasPC3);
  end;

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

  // 9. Check Spectrum 512 (.SPU, .SPC, .SPS)
  if (Ext = '.SPU') or (Ext = '.SPC') or (Ext = '.SPS') or (InSize = 51200) then
  begin
    if InSize = 51200 then Exit(apfSpectrum512SPU);
  end;

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

  // 12. Check Raw Atari ST Screen dumps (32,000 or 32,032 bytes) or .MUR
  if (InSize = 32000) or (InSize = 32032) or (Ext = '.MUR') then
  begin
    Exit(apfRawScreen);
  end;

  // 13. Check GEM Image (.IMG)
  if (Ext = '.IMG') or ((InSize >= 16) and (ReadBE16(InBuf, 0) = 1) and (ReadBE16(InBuf, 2) >= 8) and (ReadBE16(InBuf, 4) in [1, 4])) then
  begin
    Exit(apfGemImg);
  end;
end;

{ Decodes 320x200 Low Resolution 4-plane Atari ST screen }
procedure DecodeLowResScreen(const BitmapBuf: PByte; const Palette: array of TColor;
  OutBmp: Graphics.TBitmap);
var
  Y, Block, X, Shift, ColorIdx, PixelX, LineOffset, BlockOffset: Integer;
  P0, P1, P2, P3: Word;
begin
  OutBmp.SetSize(320, 200);
  OutBmp.PixelFormat := pf24bit;

  for Y := 0 to 199 do
  begin
    LineOffset := Y * 160;
    for Block := 0 to 19 do
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

{ Decodes 320x200 Spectrum 512 image with 3 palettes per scanline }
procedure DecodeSpectrum512(const InBuf: PByte; OutBmp: Graphics.TBitmap);
var
  Y, Block, X, Shift, ColorIdx, PixelX, LineOffset, BlockOffset, PalIdx: Integer;
  P0, P1, P2, P3, PalWord: Word;
  PalOffset: Integer;
  Col: TColor;
begin
  OutBmp.SetSize(320, 200);
  OutBmp.PixelFormat := pf24bit;

  for Y := 0 to 199 do
  begin
    LineOffset := Y * 160;
    PalOffset := 32000 + Y * 96;

    for Block := 0 to 19 do
    begin
      BlockOffset := LineOffset + Block * 8;
      P0 := ReadBE16(InBuf, BlockOffset);
      P1 := ReadBE16(InBuf, BlockOffset + 2);
      P2 := ReadBE16(InBuf, BlockOffset + 4);
      P3 := ReadBE16(InBuf, BlockOffset + 6);

      for X := 0 to 15 do
      begin
        PixelX := Block * 16 + X;
        Shift := 15 - X;
        ColorIdx := ((P0 shr Shift) and 1) or
                    (((P1 shr Shift) and 1) shl 1) or
                    (((P2 shr Shift) and 1) shl 2) or
                    (((P3 shr Shift) and 1) shl 3);

        // Spectrum 512 horizontal raster beam palette distribution
        if PixelX < 10 then
          PalIdx := 0
        else if PixelX < 170 then
          PalIdx := 1
        else
          PalIdx := 2;

        PalWord := ReadBE16(InBuf, PalOffset + PalIdx * 32 + ColorIdx * 2);
        Col := ConvertAtariColorWord(PalWord, True);
        OutBmp.Canvas.Pixels[PixelX, Y] := Col;
      end;
    end;
  end;
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

function DecodeAtariPicture(const InBuf: PByte; InSize: Integer; const FileName: string;
  out OutBitmap: Graphics.TBitmap; out FormatDesc: string;
  out OutWidth, OutHeight, OutColors: Integer): Boolean;
var
  PicFmt: TAtariPicFormat;
  Pal: array[0..15] of TColor;
  IsSTE, IsCompressed: Boolean;
  I, DefRes, PalOffset, ScreenOffset: Integer;
  UnpackedBuf, StadVRAM, StosPal, StosVRAM, TinyPal, TinyVRAM, UnsquashedBuf: TBytes;
  StosRes, TinyRes: Integer;
  WorkBuf: PByte;
  WorkSize: Integer;
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

    if not DecompressDegasElite(InBuf, InSize, DefRes, UnpackedBuf) then Exit(False);
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
        DecodeSpectrum512(WorkBuf, OutBitmap);
        FormatDesc := 'Spectrum 512 Image (320x200, 512 Colors)';
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
        if WorkSize < 32034 then Exit(False);
        IsSTE := HasSTEBits(WorkBuf + 2, 16);
        for I := 0 to 15 do
          Pal[I] := ConvertAtariColorWord(ReadBE16(WorkBuf, 2 + I * 2), IsSTE);
        DecodeLowResScreen(WorkBuf + 34, Pal, OutBitmap);
        FormatDesc := 'Art Director (320x200, 16 Colors)';
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
    end;
  except
    FreeAndNil(OutBitmap);
    Result := False;
  end;
end;

end.
