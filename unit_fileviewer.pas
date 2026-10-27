unit unit_fileviewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Math,
  StrUtils, LCLType,
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
    ButtonFind: TButton;
    CheckBoxAtariFont: TCheckBox;
    CheckBoxWordWrap: TCheckBox;
    MemoContent: TMemo;
    ScrollBoxImage: TScrollBox;
    ImagePicture: TImage;
    SaveDialogExport: TSaveDialog;

    // Search Panel components
    PanelSearch: TPanel;
    LabelFindPrompt: TLabel;
    EditSearch: TEdit;
    ButtonFindNext: TButton;
    ButtonFindPrev: TButton;
    CheckBoxMatchCase: TCheckBox;
    CheckBoxHexSearch: TCheckBox;
    LabelSearchStatus: TLabel;
    ButtonCloseSearch: TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure CheckBoxWordWrapChange(Sender: TObject);
    procedure CheckBoxAtariFontChange(Sender: TObject);
    procedure ButtonFindClick(Sender: TObject);
    procedure ButtonCloseSearchClick(Sender: TObject);
    procedure ButtonFindNextClick(Sender: TObject);
    procedure ButtonFindPrevClick(Sender: TObject);
    procedure EditSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
    FLastSearchOffset: Integer;

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

    procedure UpdateViewerLayout;
    procedure PerformTextSearch(ForwardDir: Boolean);
    procedure PerformHexSearch(ForwardDir: Boolean);
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
  unit_virusscanner, unit_virusreport;

{$R *.lfm}

function SwapLongBE(AValue: LongWord): LongWord;
begin
  Result := ((AValue and $FF) shl 24) or
            (((AValue shr 8) and $FF) shl 16) or
            (((AValue shr 16) and $FF) shl 8) or
            ((AValue shr 24) and $FF);
end;

{ TFormFileViewer }

const
  ATARI_ST_UTF8: array[0..255] of string = (
    { 00 } ' ',
    { 01 } '☺', '☻', '♥', '♦', '♣', '♠', '•', '◘', '    ', '',
    { 0B } '♂', '♀', '', '♫', '☼',
    { 10 } '►', '◄', '↕', '‼', '¶', '§', '▬', '↨', '↑', '↓', '→', '←', '∟', '↔', '▲', '▼',
    { 20 } ' ', '!', '"', '#', '$', '%', '&', '''', '(', ')', '*', '+', ',', '-', '.', '/',
    { 30 } '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?',
    { 40 } '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
    { 50 } 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '[', '\', ']', '^', '_',
    { 60 } '`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
    { 70 } 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '{', '|', '}', '~', '⌂',
    { 80 } 'Ç', 'ü', 'é', 'â', 'ä', 'à', 'å', 'ç', 'ê', 'ë', 'è', 'ï', 'î', 'ì', 'Ä', 'Å',
    { 90 } 'É', 'æ', 'Æ', 'ô', 'ö', 'ò', 'û', 'ù', 'ÿ', 'Ö', 'Ü', '¢', '£', '¥', 'ß', 'ƒ',
    { A0 } 'á', 'í', 'ó', 'ú', 'ñ', 'Ñ', 'ª', 'º', '¿', '⌐', '¬', '½', '¼', '¡', '«', '»',
    { B0 } '░', '▒', '▓', '│', '┤', '╡', '╢', '╖', '╕', '╣', '║', '╗', '╝', '╜', '╛', '┐',
    { C0 } '└', '┴', '┬', '├', '─', '┼', '╞', '╟', '╚', '╔', '╩', '╦', '╠', '═', '╬', '╧',
    { D0 } '╨', '╤', '╥', '╙', '╘', '╒', '╓', '╫', '╪', '┘', '┌', '█', '▄', '▌', '▐', '▀',
    { E0 } 'α', 'ß', 'Γ', 'π', 'Σ', 'σ', 'µ', 'τ', 'Φ', 'Θ', 'Ω', 'δ', '∞', '∅', '∈', '∩',
    { F0 } '≡', '±', '≥', '≤', '⌠', '⌡', '÷', '≈', '°', '•', '·', '√', 'ⁿ', '²', '³', ' '
  );

function AtariSTByteToUTF8(B: Byte): string;
begin
  Result := ATARI_ST_UTF8[B];
end;

procedure TFormFileViewer.UpdateViewerLayout;
var
  BottomMargin: Integer;
begin
  if PanelSearch.Visible then
    BottomMargin := PanelSearch.Height + 8
  else
    BottomMargin := 8;

  MemoContent.Height := Max(50, ClientHeight - MemoContent.Top - BottomMargin);
  ScrollBoxImage.Height := MemoContent.Height;
end;

procedure TFormFileViewer.FormResize(Sender: TObject);
begin
  UpdateViewerLayout;
end;

procedure TFormFileViewer.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_F) and (ssCtrl in Shift) then
  begin
    Key := 0;
    PanelSearch.Visible := True;
    UpdateViewerLayout;
    EditSearch.SetFocus;
    EditSearch.SelectAll;
    Exit;
  end;

  if Key = VK_F3 then
  begin
    Key := 0;
    if ssShift in Shift then
      ButtonFindPrevClick(nil)
    else
      ButtonFindNextClick(nil);
    Exit;
  end;

  if (Key = VK_ESCAPE) and PanelSearch.Visible then
  begin
    Key := 0;
    PanelSearch.Visible := False;
    UpdateViewerLayout;
    Exit;
  end;
end;

procedure TFormFileViewer.ButtonFindClick(Sender: TObject);
begin
  PanelSearch.Visible := not PanelSearch.Visible;
  UpdateViewerLayout;
  if PanelSearch.Visible then
  begin
    EditSearch.SetFocus;
    EditSearch.SelectAll;
  end;
end;

procedure TFormFileViewer.ButtonCloseSearchClick(Sender: TObject);
begin
  PanelSearch.Visible := False;
  UpdateViewerLayout;
end;

procedure TFormFileViewer.EditSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    Key := 0;
    if ssShift in Shift then
      ButtonFindPrevClick(nil)
    else
      ButtonFindNextClick(nil);
  end
  else if Key = VK_ESCAPE then
  begin
    Key := 0;
    PanelSearch.Visible := False;
    UpdateViewerLayout;
  end;
end;

procedure TFormFileViewer.PerformTextSearch(ForwardDir: Boolean);
var
  FullText, Pattern: string;
  StartPos, FoundPos, SearchLen: Integer;
  LineIdx, ColIdx, I: Integer;
begin
  Pattern := EditSearch.Text;
  if Pattern = '' then Exit;

  FullText := MemoContent.Text;
  if Length(FullText) = 0 then
  begin
    LabelSearchStatus.Caption := 'File is empty.';
    Exit;
  end;

  SearchLen := Length(Pattern);
  if not CheckBoxMatchCase.Checked then
  begin
    FullText := UpperCase(FullText);
    Pattern := UpperCase(Pattern);
  end;

  if ForwardDir then
  begin
    StartPos := MemoContent.SelStart + MemoContent.SelLength + 1;
    if StartPos < 1 then StartPos := 1;
    FoundPos := PosEx(Pattern, FullText, StartPos);

    if (FoundPos = 0) and (StartPos > 1) then
    begin
      FoundPos := PosEx(Pattern, FullText, 1);
      if FoundPos > 0 then
        LabelSearchStatus.Caption := 'Wrapped to start of file'
      else
        LabelSearchStatus.Caption := 'Text not found.';
    end;
  end
  else
  begin
    StartPos := MemoContent.SelStart;
    if StartPos > Length(FullText) - SearchLen + 1 then
      StartPos := Length(FullText) - SearchLen + 1;
    FoundPos := 0;
    for I := StartPos downto 1 do
    begin
      if Copy(FullText, I, SearchLen) = Pattern then
      begin
        FoundPos := I;
        Break;
      end;
    end;

    if (FoundPos = 0) and (StartPos < Length(FullText)) then
    begin
      for I := Length(FullText) - SearchLen + 1 downto StartPos + 1 do
      begin
        if Copy(FullText, I, SearchLen) = Pattern then
        begin
          FoundPos := I;
          LabelSearchStatus.Caption := 'Wrapped to end of file';
          Break;
        end;
      end;
      if FoundPos = 0 then
        LabelSearchStatus.Caption := 'Text not found.';
    end;
  end;

  if FoundPos > 0 then
  begin
    MemoContent.SelStart := FoundPos - 1;
    MemoContent.SelLength := SearchLen;
    MemoContent.SetFocus;

    LineIdx := 1;
    ColIdx := 1;
    for I := 1 to FoundPos - 1 do
    begin
      if FullText[I] = #10 then
      begin
        Inc(LineIdx);
        ColIdx := 1;
      end
      else if FullText[I] <> #13 then
        Inc(ColIdx);
    end;

    if Pos('Wrapped', LabelSearchStatus.Caption) = 0 then
      LabelSearchStatus.Caption := Format('Match found at line %d, col %d', [LineIdx, ColIdx]);
  end
  else
    LabelSearchStatus.Caption := 'Pattern not found.';
end;

procedure TFormFileViewer.PerformHexSearch(ForwardDir: Boolean);
var
  SearchPattern: TBytes;
  PatternStr, CleanHex, OffPrefix, MemoTxt: string;
  I, J, StartByte, FoundByte, DataLen, PatLen: Integer;
  Matched: Boolean;
  HexByteVal, FoundLineCharPos: Integer;
begin
  PatternStr := EditSearch.Text;
  if Trim(PatternStr) = '' then Exit;

  DataLen := Length(FData);
  if DataLen = 0 then
  begin
    LabelSearchStatus.Caption := 'File is empty.';
    Exit;
  end;

  if CheckBoxHexSearch.Checked then
  begin
    CleanHex := '';
    for I := 1 to Length(PatternStr) do
    begin
      if PatternStr[I] in ['0'..'9', 'A'..'F', 'a'..'f'] then
        CleanHex := CleanHex + PatternStr[I];
    end;
    if (Length(CleanHex) = 0) or ((Length(CleanHex) mod 2) <> 0) then
    begin
      LabelSearchStatus.Caption := 'Enter hex pairs (e.g. "60 1A").';
      Exit;
    end;
    PatLen := Length(CleanHex) div 2;
    SetLength(SearchPattern, PatLen);
    for I := 0 to PatLen - 1 do
    begin
      HexByteVal := StrToIntDef('$' + Copy(CleanHex, I * 2 + 1, 2), -1);
      if HexByteVal = -1 then
      begin
        LabelSearchStatus.Caption := 'Invalid hex characters.';
        Exit;
      end;
      SearchPattern[I] := Byte(HexByteVal);
    end;
  end
  else
  begin
    PatLen := Length(PatternStr);
    SetLength(SearchPattern, PatLen);
    for I := 0 to PatLen - 1 do
      SearchPattern[I] := Byte(PatternStr[I + 1]);
  end;

  if PatLen > DataLen then
  begin
    LabelSearchStatus.Caption := 'Pattern longer than file data.';
    Exit;
  end;

  FoundByte := -1;
  if ForwardDir then
  begin
    StartByte := FLastSearchOffset + 1;
    if (StartByte < 0) or (StartByte > DataLen - PatLen) then
      StartByte := 0;

    for I := StartByte to DataLen - PatLen do
    begin
      Matched := True;
      for J := 0 to PatLen - 1 do
      begin
        if CheckBoxHexSearch.Checked or CheckBoxMatchCase.Checked then
        begin
          if FData[I + J] <> SearchPattern[J] then
          begin
            Matched := False;
            Break;
          end;
        end
        else
        begin
          if UpCase(Char(FData[I + J])) <> UpCase(Char(SearchPattern[J])) then
          begin
            Matched := False;
            Break;
          end;
        end;
      end;
      if Matched then
      begin
        FoundByte := I;
        Break;
      end;
    end;

    if (FoundByte = -1) and (StartByte > 0) then
    begin
      for I := 0 to StartByte - 1 do
      begin
        Matched := True;
        for J := 0 to PatLen - 1 do
        begin
          if CheckBoxHexSearch.Checked or CheckBoxMatchCase.Checked then
          begin
            if FData[I + J] <> SearchPattern[J] then
            begin
              Matched := False;
              Break;
            end;
          end
          else
          begin
            if UpCase(Char(FData[I + J])) <> UpCase(Char(SearchPattern[J])) then
            begin
              Matched := False;
              Break;
            end;
          end;
        end;
        if Matched then
        begin
          FoundByte := I;
          LabelSearchStatus.Caption := 'Wrapped to start of file';
          Break;
        end;
      end;
    end;
  end
  else
  begin
    StartByte := FLastSearchOffset - 1;
    if StartByte > DataLen - PatLen then
      StartByte := DataLen - PatLen;
    if StartByte < 0 then
      StartByte := DataLen - PatLen;

    for I := StartByte downto 0 do
    begin
      Matched := True;
      for J := 0 to PatLen - 1 do
      begin
        if CheckBoxHexSearch.Checked or CheckBoxMatchCase.Checked then
        begin
          if FData[I + J] <> SearchPattern[J] then
          begin
            Matched := False;
            Break;
          end;
        end
        else
        begin
          if UpCase(Char(FData[I + J])) <> UpCase(Char(SearchPattern[J])) then
          begin
            Matched := False;
            Break;
          end;
        end;
      end;
      if Matched then
      begin
        FoundByte := I;
        Break;
      end;
    end;

    if (FoundByte = -1) and (StartByte < DataLen - PatLen) then
    begin
      for I := DataLen - PatLen downto StartByte + 1 do
      begin
        Matched := True;
        for J := 0 to PatLen - 1 do
        begin
          if CheckBoxHexSearch.Checked or CheckBoxMatchCase.Checked then
          begin
            if FData[I + J] <> SearchPattern[J] then
            begin
              Matched := False;
              Break;
            end;
          end
          else
          begin
            if UpCase(Char(FData[I + J])) <> UpCase(Char(SearchPattern[J])) then
            begin
              Matched := False;
              Break;
            end;
          end;
        end;
        if Matched then
        begin
          FoundByte := I;
          LabelSearchStatus.Caption := 'Wrapped to end of file';
          Break;
        end;
      end;
    end;
  end;

  if FoundByte <> -1 then
  begin
    FLastSearchOffset := FoundByte;
    if Pos('Wrapped', LabelSearchStatus.Caption) = 0 then
      LabelSearchStatus.Caption := Format('Match found at byte offset $%X (%d)', [FoundByte, FoundByte]);

    OffPrefix := IntToHex(FoundByte and not $0F, 8) + ':';
    MemoTxt := MemoContent.Text;
    FoundLineCharPos := Pos(OffPrefix, MemoTxt);
    if FoundLineCharPos > 0 then
    begin
      MemoContent.SelStart := FoundLineCharPos - 1;
      MemoContent.SelLength := 79;
      MemoContent.SetFocus;
    end;
  end
  else
    LabelSearchStatus.Caption := 'Pattern not found.';
end;

procedure TFormFileViewer.ButtonFindNextClick(Sender: TObject);
begin
  if FCurrentMode = 0 then
    PerformHexSearch(True)
  else if FCurrentMode = 1 then
    PerformTextSearch(True);
end;

procedure TFormFileViewer.ButtonFindPrevClick(Sender: TObject);
begin
  if FCurrentMode = 0 then
    PerformHexSearch(False)
  else if FCurrentMode = 1 then
    PerformTextSearch(False);
end;

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
  FLastSearchOffset := -1;
  CheckBoxWordWrap.Visible := False;
  CheckBoxWordWrap.Checked := False;
  CheckBoxAtariFont.Visible := False;
  CheckBoxAtariFont.Checked := True;
  PanelSearch.Visible := False;
  KeyPreview := True;
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

procedure TFormFileViewer.CheckBoxWordWrapChange(Sender: TObject);
begin
  if FCurrentMode = 1 then
  begin
    MemoContent.WordWrap := CheckBoxWordWrap.Checked;
    if CheckBoxWordWrap.Checked then
      MemoContent.ScrollBars := ssVertical
    else
      MemoContent.ScrollBars := ssBoth;
  end;
end;

procedure TFormFileViewer.CheckBoxAtariFontChange(Sender: TObject);
begin
  if FCurrentMode = 1 then
    RenderTextView;
end;

procedure TFormFileViewer.ButtonHexModeClick(Sender: TObject);
begin
  FCurrentMode := 0;
  ScrollBoxImage.Visible := False;
  ButtonZoom.Visible := False;
  CheckBoxWordWrap.Visible := False;
  CheckBoxAtariFont.Visible := False;
  ButtonFind.Visible := True;
  CheckBoxHexSearch.Visible := True;
  CheckBoxHexSearch.Enabled := True;
  MemoContent.Visible := True;
  MemoContent.WordWrap := False;
  MemoContent.ScrollBars := ssBoth;
  UpdateViewerLayout;
  RenderHexView;
end;

procedure TFormFileViewer.ButtonTextModeClick(Sender: TObject);
begin
  FCurrentMode := 1;
  ScrollBoxImage.Visible := False;
  ButtonZoom.Visible := False;
  MemoContent.Visible := True;
  CheckBoxWordWrap.Visible := True;
  CheckBoxAtariFont.Visible := True;
  ButtonFind.Visible := True;
  CheckBoxHexSearch.Visible := False;
  CheckBoxHexSearch.Checked := False;
  MemoContent.WordWrap := CheckBoxWordWrap.Checked;
  if CheckBoxWordWrap.Checked then
    MemoContent.ScrollBars := ssVertical
  else
    MemoContent.ScrollBars := ssBoth;
  UpdateViewerLayout;
  RenderTextView;
end;

procedure TFormFileViewer.ButtonImageModeClick(Sender: TObject);
begin
  if not FHasPicture then Exit;
  FCurrentMode := 2;
  MemoContent.Visible := False;
  CheckBoxWordWrap.Visible := False;
  CheckBoxAtariFont.Visible := False;
  ButtonFind.Visible := False;
  PanelSearch.Visible := False;
  ScrollBoxImage.Visible := True;
  ButtonZoom.Visible := True;
  UpdateViewerLayout;
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
    begin
      if ThreatRes.TargetType = vtProtector then
        ThreatLine := Format('Protection Status: [PROTECTOR / VACCINE] %s' + sLineBreak +
                             'Description:       %s' + sLineBreak, [ThreatRes.Name, ThreatRes.Description])
      else if ThreatRes.TargetType = vtCustomBoot then
        ThreatLine := Format('Bootloader Status: [CUSTOM BOOTLOADER] %s' + sLineBreak +
                             'Description:       %s' + sLineBreak, [ThreatRes.Name, ThreatRes.Description])
      else if Pos('unrecognized', LowerCase(ThreatRes.Name)) > 0 then
        ThreatLine := Format('Threat Status:    *** POTENTIAL VIRUS DETECTED (YELLOW WARNING): %s ***' + sLineBreak +
                             'Description:      %s' + sLineBreak, [ThreatRes.Name, ThreatRes.Description])
      else if Pos('suspicious', LowerCase(ThreatRes.Name)) > 0 then
      begin
        if HasFloppyWriteRoutine(ThreatRes) then
          ThreatLine := Format('Threat Status:    *** VIRUS DETECTED (RED ALERT / FLOPPY WRITE): %s ***' + sLineBreak +
                               'Description:      %s' + sLineBreak, [ThreatRes.Name, ThreatRes.Description])
        else
          ThreatLine := Format('Threat Status:    *** SUSPICIOUS BOOT (YELLOW CAUTION / NO WRITE): %s ***' + sLineBreak +
                               'Description:      %s' + sLineBreak, [ThreatRes.Name, ThreatRes.Description]);
      end
      else
        ThreatLine := Format('Threat Status:    *** VIRUS DETECTED: %s ***' + sLineBreak +
                             'Description:      %s' + sLineBreak, [ThreatRes.Name, ThreatRes.Description]);
    end
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
        else if CheckBoxAtariFont.Checked then
        begin
          LineStr := LineStr + AtariSTByteToUTF8(B);
        end
        else if (B >= 128) then
        begin
          // Basic Latin-1 mapping
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
    CheckBoxWordWrap.Visible := False;
    CheckBoxAtariFont.Visible := False;
    ButtonFind.Visible := False;
    PanelSearch.Visible := False;
    UpdateViewerLayout;
    RenderImageView;
  end
  else
  begin
    FCurrentMode := 0; // Hex View
    ScrollBoxImage.Visible := False;
    ButtonZoom.Visible := False;
    CheckBoxWordWrap.Visible := False;
    CheckBoxAtariFont.Visible := False;
    ButtonFind.Visible := True;
    CheckBoxHexSearch.Visible := True;
    CheckBoxHexSearch.Enabled := True;
    MemoContent.Visible := True;
    MemoContent.WordWrap := False;
    MemoContent.ScrollBars := ssBoth;
    UpdateViewerLayout;
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
