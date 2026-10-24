unit unit_msa;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

const
  MSA_MAGIC = $0E0F;

type
  TMSAHeader = packed record
    ID: Word;               // $0E0F (Big Endian)
    SectorsPerTrack: Word;  // Usually 9, 10, or 11 (Big Endian)
    Sides: Word;            // 0 = 1 side, 1 = 2 sides (Big Endian)
    StartTrack: Word;       // Usually 0 (Big Endian)
    EndTrack: Word;         // Usually 79 (Big Endian)
  end;

function SwapWordBE(AValue: Word): Word;
function IsMSAFile(const AFileName: string): Boolean;
function GetMSAInfo(const AMSAFileName: string; out ASectorsPerTrack, ASides, AStartTrack, AEndTrack: Integer): Boolean;
function UnpackMSAToStream(const AMSAFileName: string; AOutStream: TStream): Boolean;
function UnpackMSAToST(const AMSAFileName, ASTFileName: string): Boolean;
function PackSTToMSA(const ASTFileName, AMSAFileName: string; ASectorsPerTrack: Integer = 9; ASides: Integer = 2): Boolean;

implementation

function SwapWordBE(AValue: Word): Word;
begin
  Result := ((AValue and $FF) shl 8) or ((AValue shr 8) and $FF);
end;

function IsMSAFile(const AFileName: string): Boolean;
var
  FS: TFileStream;
  Magic: Word;
begin
  Result := False;
  if not FileExists(AFileName) then Exit;
  try
    FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      if FS.Size >= 10 then
      begin
        FS.ReadBuffer(Magic, SizeOf(Magic));
        Result := (SwapWordBE(Magic) = MSA_MAGIC);
      end;
    finally
      FS.Free;
    end;
  except
    Result := False;
  end;
end;

function GetMSAInfo(const AMSAFileName: string; out ASectorsPerTrack, ASides, AStartTrack, AEndTrack: Integer): Boolean;
var
  FS: TFileStream;
  Hdr: TMSAHeader;
begin
  Result := False;
  ASectorsPerTrack := 9;
  ASides := 2;
  AStartTrack := 0;
  AEndTrack := 79;
  if not FileExists(AMSAFileName) then Exit;
  try
    FS := TFileStream.Create(AMSAFileName, fmOpenRead or fmShareDenyNone);
    try
      if FS.Size >= 10 then
      begin
        FS.ReadBuffer(Hdr, SizeOf(Hdr));
        if SwapWordBE(Hdr.ID) = MSA_MAGIC then
        begin
          ASectorsPerTrack := SwapWordBE(Hdr.SectorsPerTrack);
          ASides := SwapWordBE(Hdr.Sides) + 1;
          AStartTrack := SwapWordBE(Hdr.StartTrack);
          AEndTrack := SwapWordBE(Hdr.EndTrack);
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

function UnpackMSAToStream(const AMSAFileName: string; AOutStream: TStream): Boolean;
var
  FS: TFileStream;
  Hdr: TMSAHeader;
  SecPerTrack, SidesCount, StartTrk, EndTrk: Integer;
  ExpectedTrackSize: Integer;
  ExpectedTotalTracks, ExpectedTotalBytes: Int64;
  Trk, Side: Integer;
  TrackLenBE, TrackLen: Word;
  TrackData: array of Byte;
  TrackOut: array of Byte;
  ZeroBuf: array of Byte;
  InPos, InEnd, OutPos: Integer;
  B, DataByte: Byte;
  Count: Word;
  AvailBytes, BytesToRead, PadBytes: Int64;
  AnyTrackWritten: Boolean;
begin
  Result := False;
  if not FileExists(AMSAFileName) then Exit;

  FS := TFileStream.Create(AMSAFileName, fmOpenRead or fmShareDenyNone);
  try
    if FS.Size < 10 then Exit;
    FS.ReadBuffer(Hdr, SizeOf(Hdr));
    if SwapWordBE(Hdr.ID) <> MSA_MAGIC then Exit;

    SecPerTrack := SwapWordBE(Hdr.SectorsPerTrack);
    SidesCount := SwapWordBE(Hdr.Sides) + 1; // 0 -> 1 side, 1 -> 2 sides
    StartTrk := SwapWordBE(Hdr.StartTrack);
    EndTrk := SwapWordBE(Hdr.EndTrack);

    if (SecPerTrack < 1) or (SecPerTrack > 36) then SecPerTrack := 9;
    if (SidesCount < 1) or (SidesCount > 2) then SidesCount := 2;
    if (EndTrk < StartTrk) or (EndTrk > 90) then EndTrk := 79;

    ExpectedTrackSize := SecPerTrack * 512;
    AnyTrackWritten := False;

    for Trk := StartTrk to EndTrk do
    begin
      for Side := 0 to SidesCount - 1 do
      begin
        if FS.Position + 2 > FS.Size then Break;
        FS.ReadBuffer(TrackLenBE, 2);
        TrackLen := SwapWordBE(TrackLenBE);

        AvailBytes := FS.Size - FS.Position;
        if AvailBytes < 0 then AvailBytes := 0;

        if TrackLen = ExpectedTrackSize then
        begin
          // Uncompressed track
          SetLength(TrackData, ExpectedTrackSize);
          FillChar(TrackData[0], ExpectedTrackSize, 0);
          BytesToRead := Min(Int64(ExpectedTrackSize), AvailBytes);
          if BytesToRead > 0 then
            FS.ReadBuffer(TrackData[0], BytesToRead);
          AOutStream.WriteBuffer(TrackData[0], ExpectedTrackSize);
          AnyTrackWritten := True;
        end
        else
        begin
          // RLE compressed track
          BytesToRead := Min(Int64(TrackLen), AvailBytes);
          SetLength(TrackData, TrackLen);
          FillChar(TrackData[0], TrackLen, 0);
          if BytesToRead > 0 then
            FS.ReadBuffer(TrackData[0], BytesToRead);

          SetLength(TrackOut, ExpectedTrackSize);
          FillChar(TrackOut[0], ExpectedTrackSize, 0);
          OutPos := 0;
          InPos := 0;
          InEnd := BytesToRead;

          while (InPos < InEnd) and (OutPos < ExpectedTrackSize) do
          begin
            B := TrackData[InPos];
            Inc(InPos);
            if B = $E5 then
            begin
              if InPos >= InEnd then Break;
              DataByte := TrackData[InPos];
              Inc(InPos);
              if InPos + 1 >= InEnd then Break;
              Count := (Word(TrackData[InPos]) shl 8) or Word(TrackData[InPos + 1]);
              Inc(InPos, 2);

              if OutPos + Count > ExpectedTrackSize then
                Count := ExpectedTrackSize - OutPos;

              if Count > 0 then
              begin
                FillChar(TrackOut[OutPos], Count, DataByte);
                Inc(OutPos, Count);
              end;
            end
            else
            begin
              TrackOut[OutPos] := B;
              Inc(OutPos);
            end;
          end;

          AOutStream.WriteBuffer(TrackOut[0], ExpectedTrackSize);
          AnyTrackWritten := True;
        end;
      end;
      if FS.Position >= FS.Size then Break;
    end;

    // If archive ended early (e.g. truncated MSA image), pad remaining tracks up to expected disk geometry
    ExpectedTotalTracks := (EndTrk - StartTrk + 1);
    ExpectedTotalBytes := ExpectedTotalTracks * SidesCount * ExpectedTrackSize;
    if (AOutStream.Size > 0) and (AOutStream.Size < ExpectedTotalBytes) then
    begin
      SetLength(ZeroBuf, ExpectedTrackSize);
      FillChar(ZeroBuf[0], ExpectedTrackSize, 0);
      while AOutStream.Size < ExpectedTotalBytes do
      begin
        PadBytes := Min(Int64(ExpectedTrackSize), ExpectedTotalBytes - AOutStream.Size);
        AOutStream.WriteBuffer(ZeroBuf[0], PadBytes);
      end;
    end;

    Result := AnyTrackWritten or (AOutStream.Size > 0);
  finally
    FS.Free;
  end;
end;

function UnpackMSAToST(const AMSAFileName, ASTFileName: string): Boolean;
var
  OutFS: TFileStream;
begin
  Result := False;
  try
    OutFS := TFileStream.Create(ASTFileName, fmCreate);
    try
      Result := UnpackMSAToStream(AMSAFileName, OutFS);
    finally
      OutFS.Free;
    end;
  except
    Result := False;
  end;
end;

function PackSTToMSA(const ASTFileName, AMSAFileName: string; ASectorsPerTrack: Integer = 9; ASides: Integer = 2): Boolean;
var
  InFS: TFileStream;
  OutFS: TFileStream;
  Hdr: TMSAHeader;
  TrackSize: Integer;
  TotalTracks: Integer;
  Trk, Side: Integer;
  RawTrack: array of Byte;
  CompTrack: array of Byte;
  CompLen: Integer;
  TrackLenBE: Word;
  I, RunLen: Integer;
  CurByte: Byte;
begin
  Result := False;
  if not FileExists(ASTFileName) then Exit;

  InFS := TFileStream.Create(ASTFileName, fmOpenRead or fmShareDenyNone);
  try
    TrackSize := ASectorsPerTrack * 512;
    if (TrackSize <= 0) or (ASides <= 0) then Exit;

    TotalTracks := InFS.Size div (TrackSize * ASides);
    if TotalTracks < 1 then TotalTracks := 1;
    if TotalTracks > 84 then TotalTracks := 84;

    OutFS := TFileStream.Create(AMSAFileName, fmCreate);
    try
      // Write Header
      Hdr.ID := SwapWordBE(MSA_MAGIC);
      Hdr.SectorsPerTrack := SwapWordBE(Word(ASectorsPerTrack));
      Hdr.Sides := SwapWordBE(Word(ASides - 1));
      Hdr.StartTrack := SwapWordBE(0);
      Hdr.EndTrack := SwapWordBE(Word(TotalTracks - 1));
      OutFS.WriteBuffer(Hdr, SizeOf(Hdr));

      SetLength(RawTrack, TrackSize);
      // Safe allocation: track size + margin; loop stops immediately once CompLen >= TrackSize
      SetLength(CompTrack, TrackSize + 32);

      for Trk := 0 to TotalTracks - 1 do
      begin
        for Side := 0 to ASides - 1 do
        begin
          FillChar(RawTrack[0], TrackSize, 0);
          InFS.Read(RawTrack[0], TrackSize);

          // Compress RawTrack using MSA RLE
          CompLen := 0;
          I := 0;
          while I < TrackSize do
          begin
            // Stop compressing once output reaches TrackSize; raw track will be stored
            if CompLen >= TrackSize then
              Break;

            CurByte := RawTrack[I];
            RunLen := 1;
            while (I + RunLen < TrackSize) and (RawTrack[I + RunLen] = CurByte) and (RunLen < $FFFF) do
              Inc(RunLen);

            if CurByte = $E5 then
            begin
              // $E5 is always escaped in MSA
              CompTrack[CompLen] := $E5;
              CompTrack[CompLen + 1] := $E5;
              CompTrack[CompLen + 2] := (RunLen shr 8) and $FF;
              CompTrack[CompLen + 3] := RunLen and $FF;
              Inc(CompLen, 4);
              Inc(I, RunLen);
            end
            else if RunLen >= 4 then
            begin
              // Run of 4 or more identical bytes
              CompTrack[CompLen] := $E5;
              CompTrack[CompLen + 1] := CurByte;
              CompTrack[CompLen + 2] := (RunLen shr 8) and $FF;
              CompTrack[CompLen + 3] := RunLen and $FF;
              Inc(CompLen, 4);
              Inc(I, RunLen);
            end
            else
            begin
              // Literal byte
              CompTrack[CompLen] := CurByte;
              Inc(CompLen);
              Inc(I);
            end;
          end;

          // If compressed is smaller than raw, store compressed
          if CompLen < TrackSize then
          begin
            TrackLenBE := SwapWordBE(Word(CompLen));
            OutFS.WriteBuffer(TrackLenBE, 2);
            OutFS.WriteBuffer(CompTrack[0], CompLen);
          end
          else
          begin
            // Store uncompressed raw track
            TrackLenBE := SwapWordBE(Word(TrackSize));
            OutFS.WriteBuffer(TrackLenBE, 2);
            OutFS.WriteBuffer(RawTrack[0], TrackSize);
          end;
        end;
      end;
      Result := True;
    finally
      OutFS.Free;
    end;
  finally
    InFS.Free;
  end;
end;

end.
