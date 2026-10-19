unit unit_fileviewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Math,
  unit_depacker, unit_ataripic, unit_atariaudio;

type
  { TFormFileViewer }
  TFormFileViewer = class(TForm)
    PanelTop: TPanel;
    LabelFileName: TLabel;
    LabelFileInfo: TLabel;
    ButtonHexMode: TButton;
    ButtonTextMode: TButton;
    ButtonImageMode: TButton;
    ButtonZoom: TButton;
    ButtonDepack: TButton;
    ButtonSaveCopy: TButton;
    ButtonSaveDepacked: TButton;
    ButtonPlayAudio: TButton;
    MemoContent: TMemo;
    ScrollBoxImage: TScrollBox;
    ImagePicture: TImage;
    SaveDialogExport: TSaveDialog;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonHexModeClick(Sender: TObject);
    procedure ButtonTextModeClick(Sender: TObject);
    procedure ButtonImageModeClick(Sender: TObject);
    procedure ButtonZoomClick(Sender: TObject);
    procedure ButtonDepackClick(Sender: TObject);
    procedure ButtonSaveCopyClick(Sender: TObject);
    procedure ButtonSaveDepackedClick(Sender: TObject);
    procedure ButtonPlayAudioClick(Sender: TObject);

  private
    FData: TBytes;
    FOriginalData: TBytes;
    FDepackedData: TBytes;
    FHasDepacked: Boolean;
    FIsDepackedView: Boolean;
    FDetectedPacker: string;
    FFileName: string;
    FStartCluster: Word;
    FCurrentMode: Integer; // 0 = Hex, 1 = Text, 2 = Picture

    FPictureBitmap: Graphics.TBitmap;
    FPictureFormatDesc: string;
    FPictureWidth: Integer;
    FPictureHeight: Integer;
    FPictureColors: Integer;
    FHasPicture: Boolean;
    FZoomMode: Integer; // 0 = 2x, 1 = 1x, 2 = Fit

    FAudioInfo: TAtariAudioInfo;
    FHasAudio: Boolean;
    FIsAudioPlaying: Boolean;

    procedure RenderHexView;
    procedure RenderTextView;
    procedure RenderImageView;
    procedure CheckAndDecodePicture(const AData: TBytes);
    procedure CheckAndDecodeAudio(const AData: TBytes);
    function DecodePRGHeader: string;
    function TryPerformDepack: Boolean;

  public
    procedure LoadFromStream(AStream: TStream; const ADisplayName: string; ACluster: Word);
    procedure LoadFromBuffer(const ABytes: TBytes; const ADisplayName: string; ACluster: Word);
    procedure LoadFromHostFile(const AHostPath, ADisplayName: string; ACluster: Word);
  end;

var
  FormFileViewer: TFormFileViewer;

implementation

uses
  unit_virusscanner;

{$R *.lfm}

function SwapLongBE(AValue: LongWord): LongWord;
begin
  Result := ((AValue and $FF) shl 24) or
            (((AValue shr 8) and $FF) shl 16) or
            (((AValue shr 16) and $FF) shl 8) or
            ((AValue shr 24) and $FF);
end;

{ TFormFileViewer }

procedure TFormFileViewer.FormCreate(Sender: TObject);
begin
  FCurrentMode := 0; // Default to Hex View
  FHasDepacked := False;
  FIsDepackedView := False;
  FDetectedPacker := 'None';
  FPictureBitmap := nil;
  FHasPicture := False;
  FZoomMode := 0; // Default to 2x zoom
  FHasAudio := False;
  FIsAudioPlaying := False;
end;

procedure TFormFileViewer.FormDestroy(Sender: TObject);
begin
  StopAudio;
  if FPictureBitmap <> nil then
  begin
    FPictureBitmap.Free;
    FPictureBitmap := nil;
  end;
end;

procedure TFormFileViewer.ButtonHexModeClick(Sender: TObject);
begin
  FCurrentMode := 0;
  ScrollBoxImage.Visible := False;
  ButtonZoom.Visible := False;
  MemoContent.Visible := True;
  RenderHexView;
end;

procedure TFormFileViewer.ButtonTextModeClick(Sender: TObject);
begin
  FCurrentMode := 1;
  ScrollBoxImage.Visible := False;
  ButtonZoom.Visible := False;
  MemoContent.Visible := True;
  RenderTextView;
end;

procedure TFormFileViewer.ButtonImageModeClick(Sender: TObject);
begin
  if not FHasPicture then Exit;
  FCurrentMode := 2;
  MemoContent.Visible := False;
  ScrollBoxImage.Visible := True;
  ButtonZoom.Visible := True;
  RenderImageView;
end;

procedure TFormFileViewer.ButtonZoomClick(Sender: TObject);
begin
  FZoomMode := (FZoomMode + 1) mod 3;
  RenderImageView;
end;

procedure TFormFileViewer.CheckAndDecodePicture(const AData: TBytes);
begin
  if FPictureBitmap <> nil then
  begin
    FPictureBitmap.Free;
    FPictureBitmap := nil;
  end;

  FHasPicture := False;
  FPictureFormatDesc := '';
  FPictureWidth := 0;
  FPictureHeight := 0;
  FPictureColors := 0;

  if Length(AData) >= 7 then
  begin
    if DecodeAtariPicture(@AData[0], Length(AData), FFileName, FPictureBitmap,
                          FPictureFormatDesc, FPictureWidth, FPictureHeight, FPictureColors) then
    begin
      FHasPicture := True;
    end;
  end;

  ButtonImageMode.Visible := FHasPicture;
  ButtonImageMode.Enabled := FHasPicture;
end;

procedure TFormFileViewer.CheckAndDecodeAudio(const AData: TBytes);
begin
  StopAudio;
  FIsAudioPlaying := False;
  FHasAudio := False;
  if Length(AData) >= 12 then
  begin
    FHasAudio := DetectAndInspectAudio(@AData[0], Length(AData), FFileName, FAudioInfo);
  end;

  if FHasAudio and FAudioInfo.CanPlay then
  begin
    ButtonPlayAudio.Caption := '▶ Play Audio';
    ButtonPlayAudio.Visible := True;
    ButtonPlayAudio.Enabled := True;
  end
  else
  begin
    ButtonPlayAudio.Visible := False;
    ButtonPlayAudio.Enabled := False;
  end;
end;

procedure TFormFileViewer.ButtonPlayAudioClick(Sender: TObject);
begin
  if not FHasAudio then Exit;

  if FIsAudioPlaying then
  begin
    StopAudio;
    FIsAudioPlaying := False;
    ButtonPlayAudio.Caption := '▶ Play Audio';
  end
  else
  begin
    if Length(FAudioInfo.WaveformData) > 0 then
    begin
      PlayAudioBuffer(FAudioInfo.WaveformData);
      FIsAudioPlaying := True;
      ButtonPlayAudio.Caption := '⏹ Stop Audio';
    end;
  end;
end;

procedure TFormFileViewer.RenderImageView;
var
  TargetW, TargetH: Integer;
begin
  if (not FHasPicture) or (FPictureBitmap = nil) then Exit;

  MemoContent.Visible := False;
  ScrollBoxImage.Visible := True;
  ButtonZoom.Visible := True;

  ImagePicture.Picture.Assign(FPictureBitmap);

  case FZoomMode of
    0: // 2x Scaling (Authentic monitor size)
    begin
      ButtonZoom.Caption := 'Zoom: 2x';
      ImagePicture.Align := alNone;
      TargetW := FPictureWidth * 2;
      TargetH := FPictureHeight * 2;
      ImagePicture.Width := TargetW;
      ImagePicture.Height := TargetH;
      ImagePicture.Left := Max(0, (ScrollBoxImage.ClientWidth - TargetW) div 2);
      ImagePicture.Top := Max(0, (ScrollBoxImage.ClientHeight - TargetH) div 2);
    end;

    1: // 1x Native Pixel Size
    begin
      ButtonZoom.Caption := 'Zoom: 1x';
      ImagePicture.Align := alNone;
      TargetW := FPictureWidth;
      TargetH := FPictureHeight;
      ImagePicture.Width := TargetW;
      ImagePicture.Height := TargetH;
      ImagePicture.Left := Max(0, (ScrollBoxImage.ClientWidth - TargetW) div 2);
      ImagePicture.Top := Max(0, (ScrollBoxImage.ClientHeight - TargetH) div 2);
    end;

    2: // Fit to Window
    begin
      ButtonZoom.Caption := 'Zoom: Fit';
      ImagePicture.Align := alClient;
    end;
  end;
end;

function TFormFileViewer.TryPerformDepack: Boolean;
var
  PackerName: string;
begin
  Result := False;
  if Length(FOriginalData) = 0 then Exit;

  if FHasDepacked and (Length(FDepackedData) > 0) then Exit(True);

  if not IsPackerSupported(FDetectedPacker) then
  begin
    ShowMessage('Decompression is not implemented for packer: ' + FDetectedPacker);
    Exit(False);
  end;

  Screen.Cursor := crHourGlass;
  try
    if DepackBuffer(@FOriginalData[0], Length(FOriginalData), FDepackedData, PackerName) then
    begin
      FHasDepacked := True;
      if (PackerName <> '') and (PackerName <> 'None') then
        FDetectedPacker := PackerName;
      Result := True;
    end
    else
    begin
      SetLength(FDepackedData, 0);
      ShowMessage('Decompression failed for: ' + FDetectedPacker + sLineBreak +
        'The file data may be corrupt or encoded with an unsupported variant.');
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormFileViewer.ButtonDepackClick(Sender: TObject);
var
  InfoStr: string;
begin
  if not FIsDepackedView then
  begin
    // Switch to depacked view
    if not TryPerformDepack then Exit;

    FIsDepackedView := True;
    FData := Copy(FDepackedData);
    ButtonDepack.Caption := 'View Packed';

    CheckAndDecodePicture(FData);
    CheckAndDecodeAudio(FData);

    InfoStr := Format('DEPACKED [%s] | Size: %s bytes (Packed: %s bytes, Ratio: %.1f%%)',
      [FDetectedPacker,
       FormatFloat('#,##0', Length(FDepackedData)),
       FormatFloat('#,##0', Length(FOriginalData)),
       (Double(Length(FOriginalData)) / Max(1.0, Double(Length(FDepackedData)))) * 100.0]);

    if FHasPicture then
      InfoStr := InfoStr + ' | Picture: ' + FPictureFormatDesc;

    if FHasAudio then
    begin
      if FAudioInfo.DurationSec > 0 then
        InfoStr := InfoStr + Format(' | Audio: %s (%.1fs)', [FAudioInfo.FormatName, FAudioInfo.DurationSec])
      else
        InfoStr := InfoStr + ' | Audio: ' + FAudioInfo.FormatName;
    end;

    LabelFileInfo.Caption := InfoStr;

    // If depacked into a picture and user was in picture view or it's clearly an image, render image
    if FHasPicture and (FCurrentMode = 2) then
      RenderImageView
    else if FCurrentMode = 0 then
      RenderHexView
    else if FCurrentMode = 1 then
      RenderTextView;
  end
  else
  begin
    // Switch back to packed view
    FIsDepackedView := False;
    FData := Copy(FOriginalData);
    ButtonDepack.Caption := 'View Depacked';

    CheckAndDecodePicture(FData);
    CheckAndDecodeAudio(FData);

    InfoStr := Format('PACKED [%s] | Size: %s bytes', [FDetectedPacker, FormatFloat('#,##0', Length(FOriginalData))]);
    if FStartCluster > 0 then
      InfoStr := InfoStr + Format(' | Cluster: #%d ($%.3X)', [FStartCluster, FStartCluster]);

    if FHasPicture then
      InfoStr := InfoStr + ' | Picture: ' + FPictureFormatDesc;

    if FHasAudio then
    begin
      if FAudioInfo.DurationSec > 0 then
        InfoStr := InfoStr + Format(' | Audio: %s (%.1fs)', [FAudioInfo.FormatName, FAudioInfo.DurationSec])
      else
        InfoStr := InfoStr + ' | Audio: ' + FAudioInfo.FormatName;
    end;

    LabelFileInfo.Caption := InfoStr;

    if FHasPicture and (FCurrentMode = 2) then
      RenderImageView
    else if FCurrentMode = 0 then
      RenderHexView
    else if FCurrentMode = 1 then
      RenderTextView;
  end;
end;

procedure TFormFileViewer.ButtonSaveCopyClick(Sender: TObject);
var
  FS: TFileStream;
  OutExt: string;
begin
  if Length(FData) = 0 then
  begin
    ShowMessage('File is empty.');
    Exit;
  end;

  if (FCurrentMode = 2) and FHasPicture and (FPictureBitmap <> nil) then
  begin
    SaveDialogExport.Title := 'Save Picture As...';
    SaveDialogExport.Filter := 'Windows Bitmap (*.bmp)|*.bmp|Original Atari Picture (*.*)|*.*';
    SaveDialogExport.FileName := ChangeFileExt(FFileName, '.bmp');
    if SaveDialogExport.Execute then
    begin
      OutExt := LowerCase(ExtractFileExt(SaveDialogExport.FileName));
      if OutExt = '.bmp' then
      begin
        FPictureBitmap.SaveToFile(SaveDialogExport.FileName);
        ShowMessage('Picture successfully exported as Windows Bitmap to:' + sLineBreak + SaveDialogExport.FileName);
      end
      else
      begin
        FS := TFileStream.Create(SaveDialogExport.FileName, fmCreate);
        try
          FS.WriteBuffer(FData[0], Length(FData));
          ShowMessage('Raw picture successfully saved to:' + sLineBreak + SaveDialogExport.FileName);
        finally
          FS.Free;
        end;
      end;
    end;
    Exit;
  end;

  SaveDialogExport.Title := 'Save Raw / Extracted File As...';
  SaveDialogExport.Filter := 'All Files (*.*)|*.*';
  SaveDialogExport.FileName := FFileName;
  if SaveDialogExport.Execute then
  begin
    FS := TFileStream.Create(SaveDialogExport.FileName, fmCreate);
    try
      FS.WriteBuffer(FData[0], Length(FData));
      ShowMessage('File successfully saved to:' + sLineBreak + SaveDialogExport.FileName);
    finally
      FS.Free;
    end;
  end;
end;

procedure TFormFileViewer.ButtonSaveDepackedClick(Sender: TObject);
var
  FS: TFileStream;
  OutName: string;
begin
  if Length(FOriginalData) = 0 then
  begin
    ShowMessage('File is empty.');
    Exit;
  end;

  if not TryPerformDepack then Exit;

  if Length(FDepackedData) = 0 then
  begin
    ShowMessage('Decompressed data is empty.');
    Exit;
  end;

  SaveDialogExport.Title := 'Save Depacked File As...';
  OutName := FFileName;
  SaveDialogExport.FileName := OutName;
  if SaveDialogExport.Execute then
  begin
    FS := TFileStream.Create(SaveDialogExport.FileName, fmCreate);
    try
      FS.WriteBuffer(FDepackedData[0], Length(FDepackedData));
      ShowMessage(Format('Depacked file successfully saved!' + sLineBreak +
        'Packer: %s' + sLineBreak +
        'Decompressed Size: %s bytes (Packed: %s bytes)' + sLineBreak +
        'Destination: %s',
        [FDetectedPacker,
         FormatFloat('#,##0', Length(FDepackedData)),
         FormatFloat('#,##0', Length(FOriginalData)),
         SaveDialogExport.FileName]));
    finally
      FS.Free;
    end;
  end;
end;

function TFormFileViewer.DecodePRGHeader: string;
var
  TextSize, DataSize, BssSize, SymsSize, Flags: LongWord;
  FlagsDesc, PackerLine, StatusDesc: string;
  CurPacker: string;
  IsSupp: Boolean;
  ThreatRes: TVirusScanResult;
  ThreatLine: string;
  BootSum: Word;
  BootState: string;
begin
  Result := '';
  if Length(FData) < 4 then Exit;

  // 0. ATARI ST BOOT SECTOR (512 bytes)
  if (Length(FData) = 512) and (Pos('BOOT_SECTOR', UpperCase(FFileName)) > 0) then
  begin
    BootSum := CalculateBootChecksum(@FData[0], 512);
    if BootSum = $1234 then
      BootState := 'Executable (Bootable $1234 Checksum Validated)'
    else
      BootState := 'Non-executable (Standard Data Disk)';

    if ScanBootSector(@FData[0], 512, ThreatRes) then
      ThreatLine := Format('Threat Status:    *** VIRUS DETECTED: %s ***' + sLineBreak +
                           'Description:      %s' + sLineBreak, [ThreatRes.Name, ThreatRes.Description])
    else
      ThreatLine := 'Threat Status:    Clean (No known virus signatures detected)' + sLineBreak;

    Result := '=== ATARI ST BOOT SECTOR (SECTOR 0) ===' + sLineBreak +
              Format('Boot Checksum:    $%.4X' + sLineBreak +
                     'Execution State:  %s' + sLineBreak +
                     '%s' +
                     '=======================================' + sLineBreak + sLineBreak,
                     [BootSum, BootState, ThreatLine]);
    Exit;
  end;

  // File Virus scan
  if ScanFileBuffer(@FData[0], Length(FData), FFileName, FStartCluster, ThreatRes) then
    ThreatLine := Format('Threat Status:    *** VIRUS ALERT: %s ***' + sLineBreak, [ThreatRes.Name])
  else
    ThreatLine := '';

  // Determine packer info
  CurPacker := FDetectedPacker;
  if ((CurPacker = '') or (CurPacker = 'None')) and (Length(FOriginalData) > 0) then
    CurPacker := DetectPackerSignature(@FOriginalData[0], Length(FOriginalData));
  if ((CurPacker = '') or (CurPacker = 'None')) and (Length(FData) > 0) then
    CurPacker := DetectPackerSignature(@FData[0], Length(FData));

  IsSupp := IsPackerSupported(CurPacker);

  // 1. ATARI ST GEMDOS EXECUTABLE (.PRG)
  if (Length(FData) >= 28) and (FData[0] = $60) and (FData[1] = $1A) then
  begin
    TextSize := SwapLongBE(PLongWord(@FData[2])^);
    DataSize := SwapLongBE(PLongWord(@FData[6])^);
    BssSize  := SwapLongBE(PLongWord(@FData[10])^);
    SymsSize := SwapLongBE(PLongWord(@FData[14])^);
    Flags    := SwapLongBE(PLongWord(@FData[22])^);

    FlagsDesc := '';
    if (Flags and $01) <> 0 then FlagsDesc := FlagsDesc + 'FastLoad ';
    if (Flags and $02) <> 0 then FlagsDesc := FlagsDesc + 'TT-RAM-Load ';
    if (Flags and $04) <> 0 then FlagsDesc := FlagsDesc + 'TT-RAM-Alloc ';
    if FlagsDesc = '' then FlagsDesc := 'None (Standard ST RAM)';

    if (CurPacker <> '') and (CurPacker <> 'None') and (Pos('Atari ST Executable', CurPacker) = 0) then
    begin
      if FIsDepackedView then
        StatusDesc := Format('DEPACKED from %s (Original: %s bytes, Ratio: %.1f%%)',
          [CurPacker,
           FormatFloat('#,##0', Length(FOriginalData)),
           (Double(Length(FOriginalData)) / Max(1.0, Double(Length(FDepackedData)))) * 100.0])
      else if IsSupp then
        StatusDesc := Format('%s [Decompression Supported]', [CurPacker])
      else
        StatusDesc := Format('%s [Decompression Unsupported]', [CurPacker]);

      PackerLine := Format('Packer / Version: %s' + sLineBreak, [StatusDesc]);
    end
    else
    begin
      PackerLine := 'Packer / Version: None (Standard Uncompressed GEMDOS Executable)' + sLineBreak;
    end;

    Result := '=== ATARI ST GEMDOS EXECUTABLE (.PRG) HEADER ===' + sLineBreak +
              Format('Magic:            $601A (Atari Executable)' + sLineBreak +
                     '%s' +
                     '%s' +
                     'TEXT Segment:     %s bytes' + sLineBreak +
                     'DATA Segment:     %s bytes' + sLineBreak +
                     'BSS Segment:      %s bytes' + sLineBreak +
                     'Symbol Table:     %s bytes' + sLineBreak +
                     'TOS Flags:        $%.8X (%s)' + sLineBreak +
                     '================================================' + sLineBreak + sLineBreak,
                     [PackerLine,
                      ThreatLine,
                      FormatFloat('#,##0', TextSize),
                      FormatFloat('#,##0', DataSize),
                      FormatFloat('#,##0', BssSize),
                      FormatFloat('#,##0', SymsSize),
                      Flags, Trim(FlagsDesc)]);
  end
  // 2. ATARI ST PICTURE FILE (Non-PRG picture)
  else if FHasPicture then
  begin
    if (CurPacker <> '') and (CurPacker <> 'None') and (Pos('Atari ST Executable', CurPacker) = 0) then
    begin
      if FIsDepackedView then
        StatusDesc := Format('DEPACKED from %s (Original: %s bytes, Ratio: %.1f%%)',
          [CurPacker,
           FormatFloat('#,##0', Length(FOriginalData)),
           (Double(Length(FOriginalData)) / Max(1.0, Double(Length(FDepackedData)))) * 100.0])
      else if IsSupp then
        StatusDesc := Format('%s [Decompression Supported]', [CurPacker])
      else
        StatusDesc := Format('%s [Decompression Unsupported]', [CurPacker]);

      PackerLine := Format('Packer / Version: %s' + sLineBreak, [StatusDesc]);
    end
    else
      PackerLine := '';

    Result := '=== ATARI ST PICTURE FILE HEADER ===' + sLineBreak +
              Format('Format:           %s' + sLineBreak +
                     '%s' +
                     'Dimensions:       %d x %d (%d colors)' + sLineBreak +
                     'File Size:        %s bytes' + sLineBreak +
                     '====================================' + sLineBreak + sLineBreak,
                     [FPictureFormatDesc, PackerLine, FPictureWidth, FPictureHeight, FPictureColors, FormatFloat('#,##0', Length(FData))]);
  end
  // 3. PACKED DATA FILE (Non-PRG / Raw Data file)
  else if (CurPacker <> '') and (CurPacker <> 'None') and (Pos('Atari ST Executable', CurPacker) = 0) then
  begin
    if FIsDepackedView then
      StatusDesc := Format('DEPACKED from %s (Original: %s bytes -> Depacked: %s bytes, Ratio: %.1f%%)',
        [CurPacker,
         FormatFloat('#,##0', Length(FOriginalData)),
         FormatFloat('#,##0', Length(FDepackedData)),
         (Double(Length(FOriginalData)) / Max(1.0, Double(Length(FDepackedData)))) * 100.0])
    else if IsSupp then
      StatusDesc := Format('%s [Decompression Supported]', [CurPacker])
    else
      StatusDesc := Format('%s [Decompression Unsupported]', [CurPacker]);

    Result := '=== PACKED DATA FILE HEADER ===' + sLineBreak +
              Format('Packer / Version: %s' + sLineBreak +
                     'Current Size:     %s bytes' + sLineBreak +
                     '================================' + sLineBreak + sLineBreak,
                     [StatusDesc, FormatFloat('#,##0', Length(FData))]);
  end;

  // 4. ATARI AUDIO & CHIPTUNE FILE
  if FHasAudio then
  begin
    Result := FormatAudioInspectorHeader(FAudioInfo, Length(FData)) + Result;
  end;
end;

procedure TFormFileViewer.RenderHexView;
const
  HexDigits: array[0..15] of Char = '0123456789ABCDEF';
var
  SL: TStringList;
  I, J, LineStart, LineEnd, TotalBytes: Integer;
  LineStr: string;
  B: Byte;
  HeaderInfo: string;
  HexPos, AscPos: Integer;
begin
  MemoContent.Lines.BeginUpdate;
  try
    MemoContent.Clear;
    HeaderInfo := DecodePRGHeader;
    if HeaderInfo <> '' then
      MemoContent.Lines.Text := HeaderInfo;

    TotalBytes := Length(FData);
    if TotalBytes = 0 then
    begin
      MemoContent.Lines.Add('[Empty File - 0 Bytes]');
      Exit;
    end;

    SL := TStringList.Create;
    try
      // Cap at 1MB for UI responsiveness if very large file/disk image
      TotalBytes := Min(TotalBytes, 1048576);
      SL.Capacity := ((TotalBytes + 15) div 16) + 16;

      // Pre-allocate template line:
      // Offset (8) + ':  ' (3) + 49 chars hex + ' |' (2) + 16 chars asc + '|' (1) = 79 chars
      SetLength(LineStr, 79);

      I := 0;
      while I < TotalBytes do
      begin
        LineStart := I;
        LineEnd := Min(LineStart + 16, TotalBytes);

        // Fill offset (8 hex digits)
        LineStr[1] := HexDigits[(LineStart shr 28) and $0F];
        LineStr[2] := HexDigits[(LineStart shr 24) and $0F];
        LineStr[3] := HexDigits[(LineStart shr 20) and $0F];
        LineStr[4] := HexDigits[(LineStart shr 16) and $0F];
        LineStr[5] := HexDigits[(LineStart shr 12) and $0F];
        LineStr[6] := HexDigits[(LineStart shr 8) and $0F];
        LineStr[7] := HexDigits[(LineStart shr 4) and $0F];
        LineStr[8] := HexDigits[LineStart and $0F];
        LineStr[9] := ':';
        LineStr[10] := ' ';
        LineStr[11] := ' ';

        // Clear hex and ascii area
        FillChar(LineStr[12], 49, ' ');
        LineStr[61] := ' ';
        LineStr[62] := '|';
        FillChar(LineStr[63], 16, ' ');
        LineStr[79] := '|';

        HexPos := 12;
        AscPos := 63;
        for J := LineStart to LineEnd - 1 do
        begin
          B := FData[J];
          LineStr[HexPos] := HexDigits[B shr 4];
          LineStr[HexPos + 1] := HexDigits[B and $0F];
          LineStr[HexPos + 2] := ' ';
          Inc(HexPos, 3);
          if (J - LineStart = 7) then
          begin
            LineStr[HexPos] := ' ';
            Inc(HexPos);
          end;

          if (B >= 32) and (B <= 126) then
            LineStr[AscPos] := Char(B)
          else
            LineStr[AscPos] := '.';
          Inc(AscPos);
        end;

        SL.Add(LineStr);
        Inc(I, 16);
      end;

      if Length(FData) > TotalBytes then
        SL.Add(Format(sLineBreak + '... [Display capped at %d MB for performance. Total file size: %s bytes] ...',
          [TotalBytes div (1024 * 1024), FormatFloat('#,##0', Length(FData))]));

      if HeaderInfo <> '' then
        MemoContent.Lines.AddStrings(SL)
      else
        MemoContent.Lines.Assign(SL);
    finally
      SL.Free;
    end;
  finally
    MemoContent.Lines.EndUpdate;
  end;
end;

procedure TFormFileViewer.RenderTextView;
var
  SL: TStringList;
  I, TotalBytes: Integer;
  LineStr: string;
  B: Byte;
  HeaderInfo: string;
begin
  MemoContent.Lines.BeginUpdate;
  try
    MemoContent.Clear;
    HeaderInfo := DecodePRGHeader;
    if HeaderInfo <> '' then
      MemoContent.Lines.Text := HeaderInfo;

    TotalBytes := Length(FData);
    if TotalBytes = 0 then
    begin
      MemoContent.Lines.Add('[Empty File - 0 Bytes]');
      Exit;
    end;

    SL := TStringList.Create;
    try
      // Allow full viewing of floppy image files (safety cap only at 16MB)
      TotalBytes := Min(TotalBytes, 16777216);
      LineStr := '';
      for I := 0 to TotalBytes - 1 do
      begin
        B := FData[I];
        if B = 10 then // LF
        begin
          SL.Add(LineStr);
          LineStr := '';
        end
        else if B = 13 then // CR
        begin
          // Skip or let LF handle line ending
        end
        else if (B >= 32) and (B <= 126) then
        begin
          LineStr := LineStr + Char(B);
        end
        else if (B = 9) then // Tab
        begin
          LineStr := LineStr + '    ';
        end
        else if (B >= 128) then
        begin
          // Basic Atari ST / Latin-1 mapping
          LineStr := LineStr + Char(B);
        end
        else
        begin
          LineStr := LineStr + '.';
        end;
      end;
      if LineStr <> '' then
        SL.Add(LineStr);

      if Length(FData) > TotalBytes then
        SL.Add(Format(sLineBreak + '... [Text capped at %d MB] ...', [TotalBytes div (1024 * 1024)]));

      if HeaderInfo <> '' then
        MemoContent.Lines.AddStrings(SL)
      else
        MemoContent.Lines.Assign(SL);
    finally
      SL.Free;
    end;
  finally
    MemoContent.Lines.EndUpdate;
  end;
end;

procedure TFormFileViewer.LoadFromBuffer(const ABytes: TBytes; const ADisplayName: string; ACluster: Word);
var
  InfoStr: string;
  CanDepack: Boolean;
  Ext: string;
begin
  FOriginalData := Copy(ABytes);
  SetLength(FDepackedData, 0);
  FHasDepacked := False;
  FIsDepackedView := False;
  FData := Copy(ABytes);
  FFileName := ADisplayName;
  FStartCluster := ACluster;

  Caption := 'File Viewer / Hex Inspector - ' + FFileName;
  LabelFileName.Caption := 'File: ' + FFileName;

  // Check if file is packed
  if Length(ABytes) > 0 then
    FDetectedPacker := DetectPackerSignature(@ABytes[0], Length(ABytes))
  else
    FDetectedPacker := 'None';

  CanDepack := IsPackerSupported(FDetectedPacker);

  ButtonDepack.Caption := 'View Depacked';
  ButtonDepack.Enabled := CanDepack;
  ButtonDepack.Visible := CanDepack;

  ButtonSaveDepacked.Enabled := CanDepack;
  ButtonSaveDepacked.Visible := CanDepack;

  // Check if file is an Atari ST picture
  CheckAndDecodePicture(ABytes);

  // Check if file is an Atari audio / chiptune
  CheckAndDecodeAudio(ABytes);

  InfoStr := Format('Size: %s bytes', [FormatFloat('#,##0', Length(FData))]);
  if FStartCluster > 0 then
    InfoStr := InfoStr + Format(' | Cluster: #%d ($%.3X)', [FStartCluster, FStartCluster]);

  if CanDepack then
    InfoStr := InfoStr + ' | Packer: ' + FDetectedPacker
  else if (FDetectedPacker <> '') and (FDetectedPacker <> 'None') then
    InfoStr := InfoStr + ' | Packer: ' + FDetectedPacker + ' (Depack unsupported)';

  if FHasPicture then
    InfoStr := InfoStr + ' | Picture: ' + FPictureFormatDesc;

  if FHasAudio then
  begin
    if FAudioInfo.DurationSec > 0 then
      InfoStr := InfoStr + Format(' | Audio: %s (%.1fs)', [FAudioInfo.FormatName, FAudioInfo.DurationSec])
    else
      InfoStr := InfoStr + ' | Audio: ' + FAudioInfo.FormatName;
  end;

  LabelFileInfo.Caption := InfoStr;

  // If file is an Atari picture, open directly in Picture View mode
  if FHasPicture then
  begin
    FCurrentMode := 2; // Picture View
    RenderImageView;
  end
  else
  begin
    FCurrentMode := 0; // Hex View
    ScrollBoxImage.Visible := False;
    ButtonZoom.Visible := False;
    MemoContent.Visible := True;
    RenderHexView;
  end;
end;

procedure TFormFileViewer.LoadFromStream(AStream: TStream; const ADisplayName: string; ACluster: Word);
var
  Buf: TBytes;
begin
  if AStream.Size > 0 then
  begin
    SetLength(Buf, AStream.Size);
    AStream.Position := 0;
    AStream.ReadBuffer(Buf[0], AStream.Size);
  end
  else
    SetLength(Buf, 0);

  LoadFromBuffer(Buf, ADisplayName, ACluster);
end;

procedure TFormFileViewer.LoadFromHostFile(const AHostPath, ADisplayName: string; ACluster: Word);
var
  FS: TFileStream;
begin
  if not FileExists(AHostPath) then Exit;
  FS := TFileStream.Create(AHostPath, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(FS, ADisplayName, ACluster);
  finally
    FS.Free;
  end;
end;

end.
