unit unit_batchconverter;

{$mode objfpc}{$H+}

{ ------------------------------------------------------------------
  unit_batchconverter - GUI Form for Batch Disk Image Converter & Extractor.

  Allows processing batches of Atari ST floppy disk images (.st, .msa, .dim)
  and .zip archives. Supports:
  - Extraction directly to subfolders: image.st -> folder\image\...
  - Extraction directly to ZIP archives: image.st -> folder\image.zip
  - Conversion between formats: .st <-> .msa, .dim
  - Automatic depacking of 35+ Atari ST packers upon extraction
  - Generation of HTML/CSV/JSON/TXT disk catalog reports
  - Recursive folder scanning, drag-and-drop, real-time logging, and stop support
  ------------------------------------------------------------------ }

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, unit_batchengine;

type
  { TFormBatchConverter }
  TFormBatchConverter = class(TForm)
    PanelHeader: TPanel;
    LabelHeaderTitle: TLabel;
    LabelHeaderSubtitle: TLabel;
    PanelMain: TPanel;
    GroupBoxFiles: TGroupBox;
    ListViewFiles: TListView;
    PanelListButtons: TPanel;
    ButtonAddFiles: TButton;
    ButtonAddFolder: TButton;
    ButtonRemove: TButton;
    ButtonClear: TButton;
    LabelQueueSummary: TLabel;
    GroupBoxOptions: TGroupBox;
    RadioGroupAction: TRadioGroup;
    GroupBoxDestination: TGroupBox;
    RadioOutputSource: TRadioButton;
    RadioOutputDir: TRadioButton;
    EditDestDir: TEdit;
    ButtonBrowseDest: TButton;
    CheckBoxDepack: TCheckBox;
    CheckBoxOverwrite: TCheckBox;
    GroupBoxProgress: TGroupBox;
    ProgressBarBatch: TProgressBar;
    LabelStatus: TLabel;
    MemoLog: TMemo;
    PanelFooter: TPanel;
    ButtonStart: TButton;
    ButtonStop: TButton;
    ButtonClearLog: TButton;
    ButtonClose: TButton;
    OpenDialogAdd: TOpenDialog;
    SelectDirDialog: TSelectDirectoryDialog;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure ButtonAddFilesClick(Sender: TObject);
    procedure ButtonAddFolderClick(Sender: TObject);
    procedure ButtonRemoveClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonBrowseDestClick(Sender: TObject);
    procedure RadioOutputSourceChange(Sender: TObject);
    procedure RadioOutputDirChange(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonClearLogClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);

  private
    FRunning: Boolean;
    FStopRequested: Boolean;
    procedure UpdateQueueSummary;
    procedure LogMessage(const Msg: string);
    procedure AddImageFileToQueue(const AFilePath: string);
    procedure SetControlsState(IsRunning: Boolean);
    procedure OnBatchLog(const Msg: string);
  public
    procedure AddFiles(const FileList: TStrings);
  end;

var
  FormBatchConverter: TFormBatchConverter;

implementation

{$R *.lfm}

function FormatSizeKB(Bytes: Int64): string;
begin
  if Bytes < 1024 then
    Result := Format('%d B', [Bytes])
  else if Bytes < 1024 * 1024 then
    Result := Format('%.1f KB', [Bytes / 1024.0])
  else
    Result := Format('%.2f MB', [Bytes / (1024.0 * 1024.0)]);
end;

{ TFormBatchConverter }

procedure TFormBatchConverter.FormCreate(Sender: TObject);
begin
  FRunning := False;
  FStopRequested := False;
  CheckBoxDepack.Checked := False;
  OpenDialogAdd.Filter := 'Atari ST Disk Images & Archives (*.st;*.msa;*.dim;*.zip)|*.st;*.msa;*.dim;*.zip|' +
                          'Standard ST Disk Images (*.st)|*.st|' +
                          'MSA Compressed Images (*.msa)|*.msa|' +
                          'FastCopy Pro DIM (*.dim)|*.dim|' +
                          'ZIP Archives (*.zip)|*.zip|' +
                          'All Files (*.*)|*.*';
  UpdateQueueSummary;
end;

procedure TFormBatchConverter.FormShow(Sender: TObject);
begin
  UpdateQueueSummary;
end;

procedure TFormBatchConverter.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FRunning then
  begin
    if MessageDlg('Batch Processing in Progress',
                  'A batch conversion job is currently running. Do you want to cancel and close?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      FStopRequested := True;
      CanClose := True;
    end
    else
      CanClose := False;
  end
  else
    CanClose := True;
end;

procedure TFormBatchConverter.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  I: Integer;
  SL: TStringList;
begin
  if FRunning then Exit;
  SL := TStringList.Create;
  try
    for I := 0 to High(FileNames) do
    begin
      if DirectoryExists(FileNames[I]) then
        CollectSupportedImagesInDir(FileNames[I], True, SL)
      else if FileExists(FileNames[I]) then
        SL.Add(FileNames[I]);
    end;
    AddFiles(SL);
  finally
    SL.Free;
  end;
end;

procedure TFormBatchConverter.AddImageFileToQueue(const AFilePath: string);
var
  Item: TListItem;
  Ext, Fmt: string;
  FS: TFileStream;
  SizeVal: Int64;
  I: Integer;
begin
  if not FileExists(AFilePath) then Exit;

  // Check if already in list
  for I := 0 to ListViewFiles.Items.Count - 1 do
  begin
    if SameText(ListViewFiles.Items[I].SubItems[2] + DirectorySeparator + ListViewFiles.Items[I].Caption, AFilePath) then
      Exit;
  end;

  Ext := LowerCase(ExtractFileExt(AFilePath));
  if Ext = '.st' then
    Fmt := 'ST'
  else if Ext = '.msa' then
    Fmt := 'MSA'
  else if Ext = '.dim' then
    Fmt := 'DIM'
  else if Ext = '.zip' then
    Fmt := 'ZIP'
  else
    Fmt := UpperCase(Copy(Ext, 2, Length(Ext)));

  SizeVal := 0;
  try
    FS := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyNone);
    try
      SizeVal := FS.Size;
    finally
      FS.Free;
    end;
  except
    SizeVal := 0;
  end;

  Item := ListViewFiles.Items.Add;
  Item.Caption := ExtractFileName(AFilePath);
  Item.SubItems.Add(Fmt);
  Item.SubItems.Add(FormatSizeKB(SizeVal));
  Item.SubItems.Add(ExtractFilePath(AFilePath));
  Item.SubItems.Add('Pending');
end;

procedure TFormBatchConverter.AddFiles(const FileList: TStrings);
var
  I: Integer;
begin
  if FileList = nil then Exit;
  ListViewFiles.Items.BeginUpdate;
  try
    for I := 0 to FileList.Count - 1 do
      AddImageFileToQueue(FileList[I]);
  finally
    ListViewFiles.Items.EndUpdate;
    UpdateQueueSummary;
  end;
end;

procedure TFormBatchConverter.UpdateQueueSummary;
begin
  LabelQueueSummary.Caption := Format('%d image(s) queued', [ListViewFiles.Items.Count]);
  ButtonStart.Enabled := not FRunning and (ListViewFiles.Items.Count > 0);
end;

procedure TFormBatchConverter.LogMessage(const Msg: string);
begin
  MemoLog.Lines.Add(Format('[%s] %s', [FormatDateTime('hh:nn:ss', Now), Msg]));
  MemoLog.SelStart := Length(MemoLog.Text);
end;

procedure TFormBatchConverter.OnBatchLog(const Msg: string);
begin
  LogMessage(Msg);
  Application.ProcessMessages;
end;

procedure TFormBatchConverter.SetControlsState(IsRunning: Boolean);
begin
  FRunning := IsRunning;
  ButtonStart.Enabled := not IsRunning and (ListViewFiles.Items.Count > 0);
  ButtonStop.Enabled := IsRunning;
  ButtonClose.Enabled := not IsRunning;
  ButtonAddFiles.Enabled := not IsRunning;
  ButtonAddFolder.Enabled := not IsRunning;
  ButtonRemove.Enabled := not IsRunning;
  ButtonClear.Enabled := not IsRunning;
  RadioGroupAction.Enabled := not IsRunning;
  RadioOutputSource.Enabled := not IsRunning;
  RadioOutputDir.Enabled := not IsRunning;
  EditDestDir.Enabled := not IsRunning and RadioOutputDir.Checked;
  ButtonBrowseDest.Enabled := not IsRunning and RadioOutputDir.Checked;
  CheckBoxDepack.Enabled := not IsRunning;
  CheckBoxOverwrite.Enabled := not IsRunning;
end;

procedure TFormBatchConverter.ButtonAddFilesClick(Sender: TObject);
var
  I: Integer;
begin
  if OpenDialogAdd.Execute then
  begin
    ListViewFiles.Items.BeginUpdate;
    try
      for I := 0 to OpenDialogAdd.Files.Count - 1 do
        AddImageFileToQueue(OpenDialogAdd.Files[I]);
    finally
      ListViewFiles.Items.EndUpdate;
      UpdateQueueSummary;
    end;
  end;
end;

procedure TFormBatchConverter.ButtonAddFolderClick(Sender: TObject);
var
  SL: TStringList;
begin
  if SelectDirDialog.Execute then
  begin
    SL := TStringList.Create;
    try
      LogMessage('Scanning directory: ' + SelectDirDialog.FileName);
      CollectSupportedImagesInDir(SelectDirDialog.FileName, True, SL);
      LogMessage(Format('Found %d supported disk image(s).', [SL.Count]));
      AddFiles(SL);
    finally
      SL.Free;
    end;
  end;
end;

procedure TFormBatchConverter.ButtonRemoveClick(Sender: TObject);
var
  I: Integer;
begin
  ListViewFiles.Items.BeginUpdate;
  try
    for I := ListViewFiles.Items.Count - 1 downto 0 do
    begin
      if ListViewFiles.Items[I].Selected then
        ListViewFiles.Items.Delete(I);
    end;
  finally
    ListViewFiles.Items.EndUpdate;
    UpdateQueueSummary;
  end;
end;

procedure TFormBatchConverter.ButtonClearClick(Sender: TObject);
begin
  ListViewFiles.Items.Clear;
  UpdateQueueSummary;
end;

procedure TFormBatchConverter.ButtonBrowseDestClick(Sender: TObject);
begin
  if SelectDirDialog.Execute then
    EditDestDir.Text := SelectDirDialog.FileName;
end;

procedure TFormBatchConverter.RadioOutputSourceChange(Sender: TObject);
begin
  EditDestDir.Enabled := False;
  ButtonBrowseDest.Enabled := False;
end;

procedure TFormBatchConverter.RadioOutputDirChange(Sender: TObject);
begin
  EditDestDir.Enabled := True;
  ButtonBrowseDest.Enabled := True;
end;

procedure TFormBatchConverter.ButtonClearLogClick(Sender: TObject);
begin
  MemoLog.Lines.Clear;
end;

procedure TFormBatchConverter.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormBatchConverter.ButtonStopClick(Sender: TObject);
begin
  if FRunning then
  begin
    FStopRequested := True;
    LogMessage('Cancellation requested by user. Halting after current image...');
    ButtonStop.Enabled := False;
  end;
end;

procedure TFormBatchConverter.ButtonStartClick(Sender: TObject);
var
  I: Integer;
  FullImagePath: string;
  Options: TBatchConvertOptions;
  ItemResult: TBatchItemResult;
  TotalProcessed, TotalSuccess, TotalErrors: Integer;
  TotalFilesExtracted, TotalFilesDepacked: Integer;
  OverallBytes: Int64;
  StartTime: QWord;
  ActionStr: string;
begin
  if ListViewFiles.Items.Count = 0 then
  begin
    ShowMessage('Please add at least one disk image or ZIP file to the queue.');
    Exit;
  end;

  if RadioOutputDir.Checked and (Trim(EditDestDir.Text) = '') then
  begin
    ShowMessage('Please select a destination folder or select "Output to same folder as source image".');
    EditDestDir.SetFocus;
    Exit;
  end;

  if RadioOutputDir.Checked and not DirectoryExists(EditDestDir.Text) then
  begin
    if not ForceDirectories(EditDestDir.Text) then
    begin
      ShowMessage('Error: Failed to create destination directory:' + sLineBreak + EditDestDir.Text);
      Exit;
    end;
  end;

  // Configure options
  case RadioGroupAction.ItemIndex of
    0: Options.Action := baExtractFolders;
    1: Options.Action := baExtractZips;
    2: Options.Action := baConvertToST;
    3: Options.Action := baConvertToMSA;
    4: Options.Action := baConvertToDIM;
    5: Options.Action := baExportCatalogHTML;
  else
    Options.Action := baExtractFolders;
  end;

  Options.OutputDir := Trim(EditDestDir.Text);
  Options.UseSourceDir := RadioOutputSource.Checked;
  Options.DepackFiles := CheckBoxDepack.Checked;
  Options.OverwriteExisting := CheckBoxOverwrite.Checked;
  Options.IncludeDeleted := False;

  ActionStr := BatchActionToString(Options.Action);
  LogMessage('================================================================');
  LogMessage(Format('Starting Batch Execution: %s (%d images)', [ActionStr, ListViewFiles.Items.Count]));
  if Options.UseSourceDir then
    LogMessage('Destination: Same folder as source images')
  else
    LogMessage('Destination: ' + Options.OutputDir);
  if Options.DepackFiles then
    LogMessage('Depacking: Enabled (Atomik, Ice, Automation, Pompey, Sentry, etc.)');
  LogMessage('================================================================');

  SetControlsState(True);
  FStopRequested := False;
  ProgressBarBatch.Min := 0;
  ProgressBarBatch.Max := ListViewFiles.Items.Count;
  ProgressBarBatch.Position := 0;

  TotalProcessed := 0;
  TotalSuccess := 0;
  TotalErrors := 0;
  TotalFilesExtracted := 0;
  TotalFilesDepacked := 0;
  OverallBytes := 0;
  StartTime := GetTickCount64;

  try
    for I := 0 to ListViewFiles.Items.Count - 1 do
    begin
      if FStopRequested then
      begin
        LogMessage('Batch operation cancelled by user.');
        Break;
      end;

      FullImagePath := IncludeTrailingPathDelimiter(ListViewFiles.Items[I].SubItems[2]) +
                       ListViewFiles.Items[I].Caption;

      LabelStatus.Caption := Format('Processing %d of %d: %s',
        [I + 1, ListViewFiles.Items.Count, ListViewFiles.Items[I].Caption]);
      ListViewFiles.Items[I].SubItems[3] := 'Processing...';
      ListViewFiles.ItemIndex := I;
      Application.ProcessMessages;

      LogMessage(Format('[%d/%d] Processing %s...', [I + 1, ListViewFiles.Items.Count, ListViewFiles.Items[I].Caption]));

      if ProcessBatchImage(FullImagePath, Options, ItemResult, nil, @OnBatchLog) then
      begin
        Inc(TotalSuccess);
        TotalFilesExtracted := TotalFilesExtracted + ItemResult.FilesProcessed;
        TotalFilesDepacked := TotalFilesDepacked + ItemResult.FilesDepacked;
        OverallBytes := OverallBytes + ItemResult.BytesProcessed;

        if ItemResult.FilesDepacked > 0 then
          ListViewFiles.Items[I].SubItems[3] := Format('Done (%d files, %d depacked)',
            [ItemResult.FilesProcessed, ItemResult.FilesDepacked])
        else if ItemResult.FilesProcessed > 0 then
          ListViewFiles.Items[I].SubItems[3] := Format('Done (%d files)', [ItemResult.FilesProcessed])
        else
          ListViewFiles.Items[I].SubItems[3] := 'Done';
      end
      else
      begin
        Inc(TotalErrors);
        if ItemResult.ErrorMessage <> '' then
          ListViewFiles.Items[I].SubItems[3] := 'Error: ' + ItemResult.ErrorMessage
        else
          ListViewFiles.Items[I].SubItems[3] := 'Error';
      end;

      Inc(TotalProcessed);
      ProgressBarBatch.Position := TotalProcessed;
      Application.ProcessMessages;
    end;
  finally
    SetControlsState(False);
    ProgressBarBatch.Position := ProgressBarBatch.Max;
    LabelStatus.Caption := Format('Completed: %d succeeded, %d failed in %.1fs',
      [TotalSuccess, TotalErrors, (GetTickCount64 - StartTime) / 1000.0]);

    LogMessage('----------------------------------------------------------------');
    LogMessage(Format('Batch execution finished in %.1f seconds.', [(GetTickCount64 - StartTime) / 1000.0]));
    LogMessage(Format('Summary: %d processed | %d succeeded | %d error(s)',
      [TotalProcessed, TotalSuccess, TotalErrors]));
    if (Options.Action in [baExtractFolders, baExtractZips]) then
    begin
      LogMessage(Format('Files extracted: %d | Files depacked: %d | Total payload: %s',
        [TotalFilesExtracted, TotalFilesDepacked, FormatSizeKB(OverallBytes)]));
    end;
    LogMessage('================================================================');

    ShowMessage(Format('Batch operation completed!' + sLineBreak + sLineBreak +
      '• Processed: %d image(s)' + sLineBreak +
      '• Succeeded: %d' + sLineBreak +
      '• Errors: %d' + sLineBreak +
      '• Extracted files: %d (%d depacked)',
      [TotalProcessed, TotalSuccess, TotalErrors, TotalFilesExtracted, TotalFilesDepacked]));
  end;
end;

end.
