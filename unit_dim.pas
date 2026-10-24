unit unit_dim;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

const
  DIM_MAGIC = $4242; // 'BB' FastCopy Pro ID

type
  { FastCopy Pro 32-byte Disk Image Header }
  TDIMHeader = packed record
    ID: Word;               // $4242 ('BB' Big Endian / Little Endian in file as $42 $42)
    AutoDetect: Byte;       // 1 = auto configuration detected, 0 = user specified
    SectorMode: Byte;       // 0 = all sectors, 1 = only used sectors
    Reserved1: Word;        // Reserved (0)
    Sides: Byte;            // 0 = 1 side, 1 = 2 sides (add 1 for side count)
    Reserved2: Byte;        // Reserved (0)
    SectorsPerTrack: Byte;  // Sectors per track (9, 10, 11, etc.)
    Reserved3: Byte;        // Reserved (0)
    StartTrack: Byte;       // Starting track (0-based, usually 0)
    Reserved4: Byte;        // Reserved (0)
    EndTrack: Byte;         // Ending track (0-based, usually 79 or 81)
    Density: Byte;          // 0 = Double Density (DD), 1 = High Density (HD)
    BPB: array[0..17] of Byte; // 18 bytes copy of BIOS Parameter Block (BPB)
  end;

function IsDIMFile(const AFileName: string): Boolean;
function GetDIMInfo(const ADIMFileName: string; out ASectorsPerTrack, ASides, AStartTrack, AEndTrack: Integer): Boolean;
function UnpackDIMToStream(const ADIMFileName: string; AOutStream: TStream): Boolean;
function UnpackDIMToST(const ADIMFileName, ASTFileName: string): Boolean;
function PackSTToDIM(const ASTFileName, ADIMFileName: string; ASectorsPerTrack: Integer = 9; ASides: Integer = 2; ATracks: Integer = 80): Boolean;

implementation

function IsDIMFile(const AFileName: string): Boolean;
var
  FS: TFileStream;
  Header: TDIMHeader;
  Ext: string;
begin
  Result := False;
  if not FileExists(AFileName) then Exit;
  Ext := LowerCase(ExtractFileExt(AFileName));

  try
    FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      if FS.Size >= SizeOf(TDIMHeader) + 512 then
      begin
        FS.ReadBuffer(Header, SizeOf(Header));
        // Check for 'BB' ($4242) magic ID
        if (Header.ID = DIM_MAGIC) or
           ((PAnsiChar(@Header.ID)[0] = 'B') and (PAnsiChar(@Header.ID)[1] = 'B')) then
        begin
          Result := True;
        end
        else if (Ext = '.dim') and (FS.Size >= 368640 + 32) then
        begin
          // Fallback: file extension is .dim and size is an Atari ST floppy size + 32 header bytes
          Result := True;
        end;
      end;
    finally
      FS.Free;
    end;
  except
    Result := False;
  end;
end;

function GetDIMInfo(const ADIMFileName: string; out ASectorsPerTrack, ASides, AStartTrack, AEndTrack: Integer): Boolean;
var
  FS: TFileStream;
  Header: TDIMHeader;
  TotalTracks: Integer;
  PayloadSize: Int64;
begin
  Result := False;
  ASectorsPerTrack := 9;
  ASides := 2;
  AStartTrack := 0;
  AEndTrack := 79;

  if not FileExists(ADIMFileName) then Exit;

  try
    FS := TFileStream.Create(ADIMFileName, fmOpenRead or fmShareDenyNone);
    try
      if FS.Size >= SizeOf(TDIMHeader) then
      begin
        FS.ReadBuffer(Header, SizeOf(Header));
        if (Header.ID = DIM_MAGIC) or
           ((PAnsiChar(@Header.ID)[0] = 'B') and (PAnsiChar(@Header.ID)[1] = 'B')) then
        begin
          ASides := Header.Sides + 1;
          if (ASides < 1) or (ASides > 2) then ASides := 2;

          ASectorsPerTrack := Header.SectorsPerTrack;
          if (ASectorsPerTrack < 8) or (ASectorsPerTrack > 24) then
            ASectorsPerTrack := 9;

          AStartTrack := Header.StartTrack;
          AEndTrack := Header.EndTrack;
          if (AEndTrack < AStartTrack) or (AEndTrack > 85) then
            AEndTrack := 79;

          Result := True;
          Exit;
        end;
      end;

      // Headerless or alternative DIM: derive from payload file size
      PayloadSize := FS.Size - 32;
      if PayloadSize <= 0 then PayloadSize := FS.Size;

      if PayloadSize = 368640 then // 360 KB (80 trk, 1 side, 9 sec)
      begin
        ASides := 1;
        ASectorsPerTrack := 9;
        AEndTrack := 79;
        Result := True;
      end
      else if PayloadSize = 409600 then // 400 KB (80 trk, 1 side, 10 sec)
      begin
        ASides := 1;
        ASectorsPerTrack := 10;
        AEndTrack := 79;
        Result := True;
      end
      else if PayloadSize = 737280 then // 720 KB (80 trk, 2 side, 9 sec)
      begin
        ASides := 2;
        ASectorsPerTrack := 9;
        AEndTrack := 79;
        Result := True;
      end
      else if PayloadSize = 819200 then // 800 KB (80 trk, 2 side, 10 sec)
      begin
        ASides := 2;
        ASectorsPerTrack := 10;
        AEndTrack := 79;
        Result := True;
      end
      else if PayloadSize = 1474560 then // 1.44 MB (80 trk, 2 side, 18 sec)
      begin
        ASides := 2;
        ASectorsPerTrack := 18;
        AEndTrack := 79;
        Result := True;
      end
      else
      begin
        // Generic fallback calculation
        TotalTracks := PayloadSize div (2 * 9 * 512);
        if TotalTracks >= 70 then
        begin
          ASides := 2;
          ASectorsPerTrack := 9;
          AEndTrack := Max(79, TotalTracks - 1);
          Result := True;
        end;
      end;
    finally
      FS.Free;
    end;
  except
    Result := False;
  end;
end;

function UnpackDIMToStream(const ADIMFileName: string; AOutStream: TStream): Boolean;
var
  FS: TFileStream;
  Header: TDIMHeader;
  Buffer: array[0..65535] of Byte;
  BytesRead, TotalToCopy: Int64;
begin
  Result := False;
  if not FileExists(ADIMFileName) or (AOutStream = nil) then Exit;

  try
    FS := TFileStream.Create(ADIMFileName, fmOpenRead or fmShareDenyNone);
    try
      if FS.Size < SizeOf(TDIMHeader) + 512 then Exit;

      FS.ReadBuffer(Header, SizeOf(Header));
      // Raw sectors payload starts at offset 32 (SizeOf(TDIMHeader))
      FS.Position := SizeOf(TDIMHeader);

      TotalToCopy := FS.Size - SizeOf(TDIMHeader);
      while TotalToCopy > 0 do
      begin
        BytesRead := FS.Read(Buffer, Min(SizeOf(Buffer), TotalToCopy));
        if BytesRead <= 0 then Break;
        AOutStream.WriteBuffer(Buffer, BytesRead);
        Dec(TotalToCopy, BytesRead);
      end;
      Result := True;
    finally
      FS.Free;
    end;
  except
    Result := False;
  end;
end;

function UnpackDIMToST(const ADIMFileName, ASTFileName: string): Boolean;
var
  OutFS: TFileStream;
begin
  Result := False;
  try
    OutFS := TFileStream.Create(ASTFileName, fmCreate);
    try
      Result := UnpackDIMToStream(ADIMFileName, OutFS);
    finally
      OutFS.Free;
    end;
    if not Result and FileExists(ASTFileName) then
      SysUtils.DeleteFile(ASTFileName);
  except
    Result := False;
  end;
end;

function PackSTToDIM(const ASTFileName, ADIMFileName: string; ASectorsPerTrack: Integer = 9; ASides: Integer = 2; ATracks: Integer = 80): Boolean;
var
  InFS, OutFS: TFileStream;
  Header: TDIMHeader;
  Sector0: array[0..511] of Byte;
  Buffer: array[0..65535] of Byte;
  BytesRead: Integer;
  SecPerTrackBPB, SidesBPB, TotalSecBPB: Word;
begin
  Result := False;
  if not FileExists(ASTFileName) then Exit;

  try
    InFS := TFileStream.Create(ASTFileName, fmOpenRead or fmShareDenyNone);
    try
      if InFS.Size < 512 then Exit;

      // Read Sector 0 from ST image to extract BPB if valid
      InFS.Position := 0;
      InFS.ReadBuffer(Sector0, SizeOf(Sector0));

      // Auto-detect geometry from BPB if present
      SecPerTrackBPB := Sector0[$18] or (Sector0[$19] shl 8);
      SidesBPB := Sector0[$1A] or (Sector0[$1B] shl 8);
      TotalSecBPB := Sector0[$13] or (Sector0[$14] shl 8);

      if (SecPerTrackBPB >= 8) and (SecPerTrackBPB <= 24) and (SidesBPB in [1..2]) then
      begin
        ASectorsPerTrack := SecPerTrackBPB;
        ASides := SidesBPB;
        if (TotalSecBPB > 0) then
          ATracks := TotalSecBPB div (ASectorsPerTrack * ASides);
      end
      else
      begin
        // Infer from file size
        if InFS.Size = 368640 then begin ASides := 1; ASectorsPerTrack := 9; ATracks := 80; end
        else if InFS.Size = 409600 then begin ASides := 1; ASectorsPerTrack := 10; ATracks := 80; end
        else if InFS.Size = 737280 then begin ASides := 2; ASectorsPerTrack := 9; ATracks := 80; end
        else if InFS.Size = 819200 then begin ASides := 2; ASectorsPerTrack := 10; ATracks := 80; end
        else if InFS.Size = 1474560 then begin ASides := 2; ASectorsPerTrack := 18; ATracks := 80; end;
      end;

      // Initialize DIM Header
      FillChar(Header, SizeOf(Header), 0);
      Header.ID := DIM_MAGIC; // 'BB'
      Header.AutoDetect := 1;  // Automatic configuration
      Header.SectorMode := 0;  // All sectors included
      Header.Sides := Byte(Max(0, Min(1, ASides - 1)));
      Header.SectorsPerTrack := Byte(Max(8, Min(24, ASectorsPerTrack)));
      Header.StartTrack := 0;
      Header.EndTrack := Byte(Max(0, Min(85, ATracks - 1)));
      if ASectorsPerTrack >= 18 then
        Header.Density := 1 // High Density (HD)
      else
        Header.Density := 0; // Double Density (DD)

      // BPB in Atari/DOS boot sector is at offset $0B..$1C (18 bytes)
      Move(Sector0[$0B], Header.BPB[0], 18);

      OutFS := TFileStream.Create(ADIMFileName, fmCreate);
      try
        // 1. Write 32-byte DIM Header
        OutFS.WriteBuffer(Header, SizeOf(Header));

        // 2. Write raw ST image sector stream
        InFS.Position := 0;
        while InFS.Position < InFS.Size do
        begin
          BytesRead := InFS.Read(Buffer, SizeOf(Buffer));
          if BytesRead <= 0 then Break;
          OutFS.WriteBuffer(Buffer, BytesRead);
        end;

        Result := True;
      finally
        OutFS.Free;
      end;
    finally
      InFS.Free;
    end;
  except
    Result := False;
  end;
end;

end.
