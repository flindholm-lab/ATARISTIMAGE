unit unit_atariaudio;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, StrUtils
  {$IFDEF MSWINDOWS}
  , mmsystem
  {$ENDIF}
  ;

type
  TAtariAudioFormat = (
    aafUnknown,
    aafSNDH,        // SNDH Atari ST Chiptune
    aafYM,          // YM2149 Chiptune (YM2..YM6)
    aafMOD,         // ProTracker / SoundTracker Module (.MOD)
    aafWAV,         // RIFF WAVE Audio (.WAV)
    aafAVR,         // Audio Visual Research (.AVR - 2BIT)
    aafAtariSample  // Atari ST Raw Sound Sample (.SND, .SPL, .SAM, .SMP, .DVS)
  );

  TAtariAudioInfo = record
    Format: TAtariAudioFormat;
    FormatName: string;
    Title: string;
    Author: string;
    Ripper: string;
    Channels: Integer;
    SampleRate: Integer;
    BitsPerSample: Integer;
    DurationSec: Double;
    Subtunes: Integer;
    Details: string;
    CanPlay: Boolean;
    SampleCount: Integer;
    SampleNames: array of string;
    WaveformData: TBytes;
  end;

{ Detects audio format and extracts full metadata and preview audio if applicable }
function DetectAndInspectAudio(const InBuf: PByte; InSize: Integer;
  const FileName: string; out Info: TAtariAudioInfo): Boolean;

{ Formats the metadata inspector block to display in Hex/Text view }
function FormatAudioInspectorHeader(const Info: TAtariAudioInfo; TotalFileSize: Integer): string;

{ Audio preview playback controls }
procedure PlayAudioBuffer(const WavBuf: TBytes);
procedure StopAudio;

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

function ReadLE16(const P: PByte; Offset: Integer): Word; inline;
begin
  Result := Word(P[Offset]) or (Word(P[Offset + 1]) shl 8);
end;

function ReadLE32(const P: PByte; Offset: Integer): LongWord; inline;
begin
  Result := LongWord(P[Offset]) or
            (LongWord(P[Offset + 1]) shl 8) or
            (LongWord(P[Offset + 2]) shl 16) or
            (LongWord(P[Offset + 3]) shl 24);
end;

function CheckFourCC(const P: PByte; Size: Integer; Offset: Integer; const S: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (Offset < 0) or (Offset + 4 > Size) then Exit;
  for I := 1 to 4 do
    if P[Offset + I - 1] <> Byte(S[I]) then Exit;
  Result := True;
end;

function ReadNullString(const P: PByte; Size: Integer; var Offset: Integer; MaxLen: Integer = 256): string;
var
  StartPos, EndPos: Integer;
begin
  Result := '';
  if (Offset < 0) or (Offset >= Size) then Exit;
  StartPos := Offset;
  EndPos := StartPos;
  while (EndPos < Size) and (P[EndPos] <> 0) and (EndPos - StartPos < MaxLen) do
    Inc(EndPos);

  SetString(Result, PChar(@P[StartPos]), EndPos - StartPos);
  Offset := EndPos;
  if (Offset < Size) and (P[Offset] = 0) then
    Inc(Offset);
  Result := Trim(Result);
end;

{ Builds a standard 44-byte RIFF PCM WAV header }
function BuildWavHeader(SampleRate, Channels, BitsPerSample, DataBytes: Integer): TBytes;
var
  ByteRate, BlockAlign: Integer;
begin
  SetLength(Result, 44);
  ByteRate := SampleRate * Channels * (BitsPerSample div 8);
  BlockAlign := Channels * (BitsPerSample div 8);

  // 'RIFF'
  Result[0] := Byte('R'); Result[1] := Byte('I'); Result[2] := Byte('F'); Result[3] := Byte('F');
  // ChunkSize (36 + DataBytes)
  PLongWord(@Result[4])^ := 36 + LongWord(DataBytes);
  // 'WAVE'
  Result[8] := Byte('W'); Result[9] := Byte('A'); Result[10] := Byte('V'); Result[11] := Byte('E');
  // 'fmt '
  Result[12] := Byte('f'); Result[13] := Byte('m'); Result[14] := Byte('t'); Result[15] := Byte(' ');
  // Subchunk1Size (16 for PCM)
  PLongWord(@Result[16])^ := 16;
  // AudioFormat (1 = PCM)
  PWord(@Result[20])^ := 1;
  // NumChannels
  PWord(@Result[22])^ := Word(Channels);
  // SampleRate
  PLongWord(@Result[24])^ := LongWord(SampleRate);
  // ByteRate
  PLongWord(@Result[28])^ := LongWord(ByteRate);
  // BlockAlign
  PWord(@Result[32])^ := Word(BlockAlign);
  // BitsPerSample
  PWord(@Result[34])^ := Word(BitsPerSample);
  // 'data'
  Result[36] := Byte('d'); Result[37] := Byte('a'); Result[38] := Byte('t'); Result[39] := Byte('a');
  // Subchunk2Size (DataBytes)
  PLongWord(@Result[40])^ := LongWord(DataBytes);
end;

function DetectAndInspectAudio(const InBuf: PByte; InSize: Integer;
  const FileName: string; out Info: TAtariAudioInfo): Boolean;
var
  Ext: string;
  Pos, TagPos, I: Integer;
  TagStr: string;
  NbFrames, LoopFrame, MasterClock, AudioDataSize: LongWord;
  FrameRate, Digidrums: Word;
  SndhSubtunes: Integer;
  AudioFormat, NumChannels, BitsPerSample: Word;
  SampleRate, ByteRate, BlockAlign: LongWord;
  ChunkID, ChunkSize: LongWord;
  WavHdr: TBytes;
  AvrRate, AvrSamples: LongWord;
  AvrMonoStereo, AvrRes, AvrSigned: Word;
  AvrName: string;
  SrcSample, DstSample: Byte;
  ModTag: string;
  NumPatterns, ActiveSamples: Integer;
  SampleLenWords: Word;
  SName: string;
begin
  Result := False;
  FillChar(Info, SizeOf(Info), 0);
  Info.Format := aafUnknown;
  SetLength(Info.SampleNames, 0);
  SetLength(Info.WaveformData, 0);

  if (InBuf = nil) or (InSize < 12) then Exit;
  Ext := UpperCase(ExtractFileExt(FileName));

  // -------------------------------------------------------------
  // 1. RIFF WAVE (.WAV)
  // -------------------------------------------------------------
  if (InSize >= 44) and CheckFourCC(InBuf, InSize, 0, 'RIFF') and CheckFourCC(InBuf, InSize, 8, 'WAVE') then
  begin
    Info.Format := aafWAV;
    Info.FormatName := 'RIFF WAVE Audio (.WAV)';
    Info.Channels := 1;
    Info.SampleRate := 22050;
    Info.BitsPerSample := 16;

    Pos := 12;
    while Pos + 8 <= InSize do
    begin
      ChunkSize := ReadLE32(InBuf, Pos + 4);
      if CheckFourCC(InBuf, InSize, Pos, 'fmt ') and (ChunkSize >= 16) and (Pos + 8 + 16 <= InSize) then
      begin
        AudioFormat := ReadLE16(InBuf, Pos + 8);
        NumChannels := ReadLE16(InBuf, Pos + 10);
        SampleRate := ReadLE32(InBuf, Pos + 12);
        ByteRate := ReadLE32(InBuf, Pos + 16);
        BlockAlign := ReadLE16(InBuf, Pos + 20);
        BitsPerSample := ReadLE16(InBuf, Pos + 22);

        Info.Channels := NumChannels;
        Info.SampleRate := SampleRate;
        Info.BitsPerSample := BitsPerSample;
        if AudioFormat = 1 then
          Info.Details := 'PCM Audio (Uncompressed)'
        else
          Info.Details := Format('Compressed Audio (Format Code $%X)', [AudioFormat]);
      end
      else if CheckFourCC(InBuf, InSize, Pos, 'data') then
      begin
        AudioDataSize := Min(ChunkSize, LongWord(InSize - (Pos + 8)));
        Info.SampleCount := AudioDataSize div Max(1, (Info.Channels * (Info.BitsPerSample div 8)));
        if Info.SampleRate > 0 then
          Info.DurationSec := Double(Info.SampleCount) / Double(Info.SampleRate);
      end;
      Inc(Pos, 8 + ((ChunkSize + 1) and (not 1)));
    end;

    // Direct playback supported for standard PCM
    Info.CanPlay := True;
    SetLength(Info.WaveformData, InSize);
    Move(InBuf[0], Info.WaveformData[0], InSize);
    Exit(True);
  end;

  // -------------------------------------------------------------
  // 2. Audio Visual Research (.AVR - "2BIT")
  // -------------------------------------------------------------
  if (InSize >= 128) and CheckFourCC(InBuf, InSize, 0, '2BIT') then
  begin
    Info.Format := aafAVR;
    Info.FormatName := 'Audio Visual Research Sample (.AVR)';
    AvrName := Trim(Copy(PChar(@InBuf[4]), 1, 8));
    Info.Title := AvrName;
    AvrMonoStereo := ReadBE16(InBuf, 12);
    if AvrMonoStereo = $FFFF then
      Info.Channels := 2
    else
      Info.Channels := 1;

    AvrRes := ReadBE16(InBuf, 14);
    Info.BitsPerSample := AvrRes;
    AvrSigned := ReadBE16(InBuf, 16);
    AvrRate := ReadBE32(InBuf, 22);
    Info.SampleRate := AvrRate;
    AvrSamples := ReadBE32(InBuf, 26);
    Info.SampleCount := AvrSamples;

    if AvrRate > 0 then
      Info.DurationSec := Double(AvrSamples) / Double(AvrRate);

    if AvrSigned = 1 then
      Info.Details := Format('Signed %d-bit PCM (%s)', [AvrRes, IfThen(Info.Channels = 2, 'Stereo', 'Mono')])
    else
      Info.Details := Format('Unsigned %d-bit PCM (%s)', [AvrRes, IfThen(Info.Channels = 2, 'Stereo', 'Mono')]);

    // Build playable WAV from AVR payload
    AudioDataSize := InSize - 128;
    if AudioDataSize > 0 then
    begin
      WavHdr := BuildWavHeader(Info.SampleRate, Info.Channels, Info.BitsPerSample, AudioDataSize);
      SetLength(Info.WaveformData, 44 + AudioDataSize);
      Move(WavHdr[0], Info.WaveformData[0], 44);

      if (AvrRes = 8) and (AvrSigned = 1) then
      begin
        // Convert signed 8-bit to unsigned 8-bit for WAV
        for I := 0 to AudioDataSize - 1 do
          Info.WaveformData[44 + I] := Byte(InBuf[128 + I] xor $80);
      end
      else
        Move(InBuf[128], Info.WaveformData[44], AudioDataSize);

      Info.CanPlay := True;
    end;
    Exit(True);
  end;

  // -------------------------------------------------------------
  // 3. Atari ST SNDH Chiptune (.SNDH / .SND with 'SNDH' tag)
  // -------------------------------------------------------------
  TagPos := -1;
  if CheckFourCC(InBuf, InSize, 12, 'SNDH') then
    TagPos := 12
  else
  begin
    for I := 0 to Min(InSize - 4, 32) do
    begin
      if CheckFourCC(InBuf, InSize, I, 'SNDH') then
      begin
        TagPos := I;
        Break;
      end;
    end;
  end;

  if TagPos >= 0 then
  begin
    Info.Format := aafSNDH;
    Info.FormatName := 'Atari ST SNDH Chiptune (.SNDH)';
    Info.Channels := 3; // 3 Yamaha YM-2149 PSG Channels
    Info.BitsPerSample := 4;
    Info.SampleRate := 50; // Standard VBL 50 Hz replay
    Info.Subtunes := 1;
    Info.Details := 'Yamaha YM-2149 PSG Chiptune';

    Pos := TagPos + 4;
    while Pos + 4 <= InSize do
    begin
      SetString(TagStr, PChar(@InBuf[Pos]), 4);
      Inc(Pos, 4);

      if TagStr = 'HDNT' then Break;

      if TagStr = 'TITL' then
        Info.Title := ReadNullString(InBuf, InSize, Pos)
      else if TagStr = 'COMM' then
        Info.Author := ReadNullString(InBuf, InSize, Pos)
      else if TagStr = 'RIPP' then
        Info.Ripper := ReadNullString(InBuf, InSize, Pos)
      else if TagStr = 'CONV' then
        ReadNullString(InBuf, InSize, Pos)
      else if TagStr = 'YEAR' then
        Info.Details := Info.Details + ' | Year: ' + ReadNullString(InBuf, InSize, Pos)
      else if (Length(TagStr) = 4) and (TagStr[1] = '#') and (TagStr[2] = '#') then
      begin
        SndhSubtunes := StrToIntDef(Copy(TagStr, 3, 2), 1);
        Info.Subtunes := Max(1, SndhSubtunes);
      end
      else if (TagStr = 'TC50') or (TagStr = 'TC10') or (TagStr = 'TC20') or (TagStr = 'VBL5') then
      begin
        if TagStr = 'TC50' then Info.SampleRate := 50;
        if TagStr = 'TC10' then Info.SampleRate := 100;
        if TagStr = 'TC20' then Info.SampleRate := 200;
        if TagStr = 'VBL5' then Info.SampleRate := 50;
        Info.Details := Info.Details + Format(' | Replay: %s (%d Hz)', [TagStr, Info.SampleRate]);
      end;
    end;

    Info.CanPlay := False; // Chiptune emulation requires 68000 CPU
    Exit(True);
  end;

  // -------------------------------------------------------------
  // 4. YM2149 Register Chiptune (.YM)
  // -------------------------------------------------------------
  if (InSize >= 34) and (CheckFourCC(InBuf, InSize, 0, 'YM2!') or
                         CheckFourCC(InBuf, InSize, 0, 'YM3!') or
                         CheckFourCC(InBuf, InSize, 0, 'YM3b') or
                         CheckFourCC(InBuf, InSize, 0, 'YM4!') or
                         CheckFourCC(InBuf, InSize, 0, 'YM5!') or
                         CheckFourCC(InBuf, InSize, 0, 'YM6!')) then
  begin
    Info.Format := aafYM;
    SetString(TagStr, PChar(@InBuf[0]), 4);
    Info.FormatName := Format('YM2149 Chiptune (%s)', [TagStr]);
    Info.Channels := 3;
    Info.BitsPerSample := 4;
    Info.SampleRate := 50;
    Info.Subtunes := 1;

    if CheckFourCC(InBuf, InSize, 0, 'YM5!') or CheckFourCC(InBuf, InSize, 0, 'YM6!') then
    begin
      NbFrames := ReadBE32(InBuf, 12);
      Digidrums := ReadBE16(InBuf, 20);
      MasterClock := ReadBE32(InBuf, 22);
      FrameRate := ReadBE16(InBuf, 26);
      LoopFrame := ReadBE32(InBuf, 28);

      if FrameRate > 0 then
        Info.SampleRate := FrameRate;
      if (Info.SampleRate > 0) and (NbFrames > 0) then
        Info.DurationSec := Double(NbFrames) / Double(Info.SampleRate);

      Pos := 34 + Digidrums * 4; // skip digidrum table if present
      Info.Title := ReadNullString(InBuf, InSize, Pos);
      Info.Author := ReadNullString(InBuf, InSize, Pos);
      Info.Details := Format('Atari ST YM2149 (%d frames, Clock: %s MHz, Loop: %d)',
        [NbFrames, FormatFloat('0.0', Double(MasterClock) / 1000000.0), LoopFrame]);
    end
    else
    begin
      Info.Details := 'Legacy YM register stream';
    end;

    Info.CanPlay := False;
    Exit(True);
  end;

  // -------------------------------------------------------------
  // 5. ProTracker / SoundTracker Module (.MOD)
  // -------------------------------------------------------------
  if InSize >= 1084 then
  begin
    SetString(ModTag, PChar(@InBuf[1080]), 4);
    NumChannels := 0;
    if (ModTag = 'M.K.') or (ModTag = 'M!K!') or (ModTag = 'FLT4') or (ModTag = '4CHN') then
      NumChannels := 4
    else if (ModTag = '6CHN') then
      NumChannels := 6
    else if (ModTag = '8CHN') or (ModTag = 'OCTA') or (ModTag = 'CD81') then
      NumChannels := 8;

    if NumChannels > 0 then
    begin
      Info.Format := aafMOD;
      Info.FormatName := Format('ProTracker / SoundTracker Module (%s)', [ModTag]);
      Info.Channels := NumChannels;
      Info.SampleRate := 125; // Default BPM 125
      Info.BitsPerSample := 8;
      Info.Title := Trim(Copy(PChar(@InBuf[0]), 1, 20));
      NumPatterns := InBuf[950];
      Info.Details := Format('%d-Channel Module (%d song positions)', [NumChannels, NumPatterns]);

      ActiveSamples := 0;
      SetLength(Info.SampleNames, 31);
      for I := 0 to 30 do
      begin
        Pos := 20 + I * 30;
        SName := Trim(Copy(PChar(@InBuf[Pos]), 1, 22));
        SampleLenWords := ReadBE16(InBuf, Pos + 22);
        if SampleLenWords > 0 then
        begin
          Inc(ActiveSamples);
          Info.SampleNames[I] := Format('%.2d: %s (%d bytes, Vol %d)',
            [I + 1, IfThen(SName <> '', SName, '[Sample]'), SampleLenWords * 2, InBuf[Pos + 25]]);
        end
        else
          Info.SampleNames[I] := '';
      end;
      SetLength(Info.SampleNames, 31);
      Info.SampleCount := ActiveSamples;
      Info.CanPlay := False;
      Exit(True);
    end;
  end;

  // -------------------------------------------------------------
  // 6. Atari ST Raw Sound Samples (.SND, .SPL, .SAM, .SMP, .DVS)
  // -------------------------------------------------------------
  if (Ext = '.SND') or (Ext = '.SPL') or (Ext = '.SAM') or (Ext = '.SMP') or (Ext = '.DVS') then
  begin
    Info.Format := aafAtariSample;
    Info.FormatName := Format('Atari ST Sound Sample (%s)', [Ext]);
    Info.Title := ChangeFileExt(ExtractFileName(FileName), '');
    Info.Channels := 1;
    Info.BitsPerSample := 8;
    Info.SampleRate := 8000; // Standard Atari ST 8 kHz replay rate
    Info.SampleCount := InSize;
    Info.DurationSec := Double(InSize) / 8000.0;
    Info.Details := Format('8-bit Unsigned PCM Sample (8,000 Hz, %s bytes)', [FormatFloat('#,##0', InSize)]);

    // Wrap directly into standard 44-byte RIFF WAV for immediate playback
    WavHdr := BuildWavHeader(8000, 1, 8, InSize);
    SetLength(Info.WaveformData, 44 + InSize);
    Move(WavHdr[0], Info.WaveformData[0], 44);
    Move(InBuf[0], Info.WaveformData[44], InSize);
    Info.CanPlay := True;
    Exit(True);
  end;
end;

function FormatAudioInspectorHeader(const Info: TAtariAudioInfo; TotalFileSize: Integer): string;
var
  DurationStr, SubtuneStr, RateStr, PlayStr: string;
  I, Count: Integer;
begin
  Result := '';
  if Info.Format = aafUnknown then Exit;

  if Info.DurationSec > 0 then
    DurationStr := Format('%.2f seconds (%02d:%02d)',
      [Info.DurationSec, Trunc(Info.DurationSec) div 60, Trunc(Info.DurationSec) mod 60])
  else
    DurationStr := 'Continuous / Track dependent';

  if Info.SampleRate > 0 then
  begin
    if Info.Format in [aafSNDH, aafYM] then
      RateStr := Format('%d Hz (VBL / Timer Frame Rate)', [Info.SampleRate])
    else if Info.Format = aafMOD then
      RateStr := Format('%d BPM (ProTracker Default Tempo)', [Info.SampleRate])
    else
      RateStr := Format('%s Hz', [FormatFloat('#,##0', Info.SampleRate)]);
  end
  else
    RateStr := 'Standard 50 Hz';

  if Info.Subtunes > 1 then
    SubtuneStr := Format('%d Subtunes (Default: #1)', [Info.Subtunes])
  else
    SubtuneStr := '1 Tune';

  if Info.CanPlay then
    PlayStr := 'Interactive Audio Playback Supported (Click ''▶ Play Audio'')'
  else
    PlayStr := 'Chiptune / Tracker format (Metadata Inspector View)';

  Result := '=== AUDIO & CHIPTUNE INSPECTOR ===' + sLineBreak +
            Format('Format:           %s' + sLineBreak, [Info.FormatName]);

  if Info.Title <> '' then
    Result := Result + Format('Title / Song:     %s' + sLineBreak, [Info.Title]);
  if Info.Author <> '' then
    Result := Result + Format('Composer/Author:  %s' + sLineBreak, [Info.Author]);
  if Info.Ripper <> '' then
    Result := Result + Format('Ripper / Credit:  %s' + sLineBreak, [Info.Ripper]);

  Result := Result +
            Format('Audio Channels:   %d %s' + sLineBreak +
                   'Replay / Rate:    %s' + sLineBreak +
                   'Bit Resolution:   %d-bit' + sLineBreak +
                   'Duration:         %s' + sLineBreak,
                   [Info.Channels,
                    IfThen(Info.Channels = 1, '(Mono)', IfThen(Info.Channels = 2, '(Stereo)', IfThen(Info.Channels = 3, '(Yamaha YM-2149 PSG)', '(Multi-channel Track)'))),
                    RateStr,
                    Info.BitsPerSample,
                    DurationStr]);

  if Info.Subtunes > 1 then
    Result := Result + Format('Subtunes:         %s' + sLineBreak, [SubtuneStr]);

  if Info.SampleCount > 0 then
    Result := Result + Format('Sample Count:     %s samples' + sLineBreak, [FormatFloat('#,##0', Info.SampleCount)]);

  if Info.Details <> '' then
    Result := Result + Format('Attributes:       %s' + sLineBreak, [Info.Details]);

  // List top samples if tracker module
  if (Info.Format = aafMOD) and (Length(Info.SampleNames) > 0) then
  begin
    Result := Result + 'Instrument Samples:' + sLineBreak;
    Count := 0;
    for I := 0 to High(Info.SampleNames) do
    begin
      if Info.SampleNames[I] <> '' then
      begin
        Result := Result + '  ' + Info.SampleNames[I] + sLineBreak;
        Inc(Count);
        if Count >= 12 then
        begin
          Result := Result + '  ... [additional samples omitted] ...' + sLineBreak;
          Break;
        end;
      end;
    end;
  end;

  Result := Result +
            Format('File Size:        %s bytes' + sLineBreak +
                   'Playback:         %s' + sLineBreak +
                   '==================================' + sLineBreak + sLineBreak,
                   [FormatFloat('#,##0', TotalFileSize), PlayStr]);
end;

procedure PlayAudioBuffer(const WavBuf: TBytes);
begin
  StopAudio;
  if Length(WavBuf) = 0 then Exit;
  {$IFDEF MSWINDOWS}
  PlaySound(PChar(@WavBuf[0]), 0, SND_MEMORY or SND_ASYNC or SND_NODEFAULT);
  {$ENDIF}
end;

procedure StopAudio;
begin
  {$IFDEF MSWINDOWS}
  PlaySound(nil, 0, 0);
  {$ENDIF}
end;

end.
