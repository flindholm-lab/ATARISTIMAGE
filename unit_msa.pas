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

function UnpackMSAToStream(const AMSAFileName: string; AOutStream: TStream): Boolean;
var
  FS: TFileStream;
  Hdr: TMSAHeader;
  SecPerTrack, SidesCount, StartTrk, EndTrk: Integer;
  ExpectedTrackSize: Integer;
  Trk, Side: Integer;
  TrackLenBE, TrackLen: Word;
  TrackData: array of Byte;
  TrackOut: array of Byte;
  InPos, InEnd, OutPos: Integer;
  B, DataByte: Byte;
  Count: Word;
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

    for Trk := StartTrk to EndTrk do
    begin
      for Side := 0 to SidesCount - 1 do
      begin
        if FS.Position + 2 > FS.Size then Break;
        FS.ReadBuffer(TrackLenBE, 2);
        TrackLen := SwapWordBE(TrackLenBE);

        if TrackLen = ExpectedTrackSize then
        begin
          // Uncompressed track
          SetLength(TrackData, TrackLen);
          FS.ReadBuffer(TrackData[0], TrackLen);
          AOutStream.WriteBuffer(TrackData[0], TrackLen);
        end
        else
        begin
          // RLE compressed track
          SetLength(TrackData, TrackLen);
          if TrackLen > 0 then
            FS.ReadBuffer(TrackData[0], TrackLen);

          SetLength(TrackOut, ExpectedTrackSize);
          FillChar(TrackOut[0], ExpectedTrackSize, 0);
          OutPos := 0;
          InPos := 0;
          InEnd := TrackLen;

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
        end;
      end;
    end;
    Result := True;
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
  BytesRead: Integer;
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
      // Compressed track could at most be slightly larger if all single $E5 escapes
      SetLength(CompTrack, TrackSize * 2);

      for Trk := 0 to TotalTracks - 1 do
      begin
        for Side := 0 to ASides - 1 do
        begin
          FillChar(RawTrack[0], TrackSize, 0);
          BytesRead := InFS.Read(RawTrack[0], TrackSize);

          // Compress RawTrack using MSA RLE
          CompLen := 0;
          I := 0;
          while I < TrackSize do
          begin
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
