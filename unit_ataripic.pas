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
    apfRawScreen       // Raw 32,000 or 32,032 bytes Atari ST screen dump
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
  if (InBuf = nil) or (InSize < 34) then Exit;

  Ext := UpperCase(ExtractFileExt(FileName));

  // 1. Check DEGAS Elite RLE extensions
  if (Ext = '.PC1') then Exit(apfDegasPC1);
  if (Ext = '.PC2') then Exit(apfDegasPC2);
  if (Ext = '.PC3') then Exit(apfDegasPC3);

  // 2. Check Spectrum 512 (.SPU)
  if (Ext = '.SPU') or (InSize = 51200) then
  begin
    if InSize = 51200 then Exit(apfSpectrum512SPU);
  end;

  // 3. Check NeoChrome (.NEO) - 32,128 bytes
  if (Ext = '.NEO') or (InSize = 32128) then
  begin
    if InSize = 32128 then
    begin
      ResWord := ReadBE16(InBuf, 2);
      if ResWord <= 2 then Exit(apfNeoChrome);
    end;
  end;

  // 4. Check DEGAS Uncompressed (.PI1, .PI2, .PI3) - 32,034 bytes (or 32,066)
  if (InSize = 32034) or (InSize = 32066) or (Ext = '.PI1') or (Ext = '.PI2') or (Ext = '.PI3') then
  begin
    ResWord := ReadBE16(InBuf, 0);
    if (ResWord = 0) or (Ext = '.PI1') then Exit(apfDegasPI1);
    if (ResWord = 1) or (Ext = '.PI2') then Exit(apfDegasPI2);
    if (ResWord = 2) or (Ext = '.PI3') then Exit(apfDegasPI3);
  end;

  // 5. Check Raw Atari ST Screen dumps (32,000 or 32,032 bytes)
  if (InSize = 32000) or (InSize = 32032) then
  begin
    Exit(apfRawScreen);
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

function DecodeAtariPicture(const InBuf: PByte; InSize: Integer; const FileName: string;
  out OutBitmap: Graphics.TBitmap; out FormatDesc: string;
  out OutWidth, OutHeight, OutColors: Integer): Boolean;
var
  PicFmt: TAtariPicFormat;
  Pal: array[0..15] of TColor;
  IsSTE: Boolean;
  I: Integer;
  UnpackedBuf: TBytes;
  WorkBuf: PByte;
  WorkSize: Integer;
begin
  Result := False;
  OutBitmap := nil;
  FormatDesc := '';
  OutWidth := 0;
  OutHeight := 0;
  OutColors := 0;

  if (InBuf = nil) or (InSize < 34) then Exit;

  PicFmt := DetectAtariPicFormat(InBuf, InSize, FileName);
  if PicFmt = apfUnknown then Exit;

  // Handle DEGAS Elite RLE Compressed Formats (.PC1, .PC2, .PC3)
  if PicFmt in [apfDegasPC1, apfDegasPC2, apfDegasPC3] then
  begin
    if not DecompressRLE(InBuf, InSize, UnpackedBuf) then Exit(False);
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
    end;
  except
    FreeAndNil(OutBitmap);
    Result := False;
  end;
end;

end.
