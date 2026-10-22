unit unit_floppywriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Buttons, FileUtil, LazFileUtils, zipper,
  {$IFDEF MSWINDOWS}
  Windows, ShellAPI,
  {$ENDIF}
  unit_msa, unit_dim;

type
  { Progress callback: Cyl (0..79), Head (0..1), TrackIdx (1..TotalTracks), TotalTracks, var Abort }
  TFloppyProgress = procedure(ACyl, AHead, ATrackIdx, ATotalTracks: Integer; var AAbort: Boolean) of object;

  { TFormFloppyWriter }
  TFormFloppyWriter = class(TForm)
    PanelHeader: TPanel;
    LabelHeaderTitle: TLabel;
    LabelHeaderSubtitle: TLabel;
    PanelAdminBanner: TPanel;
    LabelAdminNotice: TLabel;
    ButtonRelaunchAdmin: TButton;

    PageControlFloppy: TPageControl;
    TabSheetWrite: TTabSheet;
    TabSheetRead: TTabSheet;

    // --- Write Tab Controls ---
    GroupBoxDrive: TGroupBox;
    LabelDriveSelect: TLabel;
    ComboBoxDrive: TComboBox;
    ButtonRefreshDrives: TButton;
    ButtonFormatDrive: TButton;
    LabelMediaStatus: TLabel;

    GroupBoxImage: TGroupBox;
    LabelImageFile: TLabel;
    EditSourceImage: TEdit;
    ButtonBrowseImage: TButton;
    LabelImageDetails: TLabel;
    LabelCompatibilityWarning: TLabel;

    GroupBoxWriteOptions: TGroupBox;
    CheckBoxVerify: TCheckBox;
    CheckBoxRepack720: TCheckBox;
    LabelUsbTapeTip: TLabel;

    GroupBoxWriteProgress: TGroupBox;
    LabelWriteStatus: TLabel;
    ProgressBarWrite: TProgressBar;
    ButtonWrite: TButton;
    ButtonCancelWrite: TButton;

    // --- Read Tab Controls ---
    GroupBoxReadDrive: TGroupBox;
    LabelReadDriveSelect: TLabel;
    ComboBoxReadDrive: TComboBox;
    ButtonRefreshReadDrives: TButton;
    LabelReadMediaStatus: TLabel;

    GroupBoxTargetImage: TGroupBox;
    LabelTargetFile: TLabel;
    EditTargetImage: TEdit;
    ButtonBrowseTarget: TButton;

    GroupBoxReadOptions: TGroupBox;
    CheckBoxOpenAfterRead: TCheckBox;
    CheckBoxRetryBadSectors: TCheckBox;

    GroupBoxReadProgress: TGroupBox;
    LabelReadStatus: TLabel;
    ProgressBarRead: TProgressBar;
    ButtonRead: TButton;
    ButtonCancelRead: TButton;

    // --- Bottom Panel ---
    PanelBottom: TPanel;
    ButtonClose: TButton;

    OpenDialogImage: TOpenDialog;
    SaveDialogImage: TSaveDialog;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonRelaunchAdminClick(Sender: TObject);

    // Write Tab
    procedure ButtonRefreshDrivesClick(Sender: TObject);
    procedure ComboBoxDriveChange(Sender: TObject);
    procedure ButtonBrowseImageClick(Sender: TObject);
    procedure EditSourceImageChange(Sender: TObject);
    procedure ButtonFormatDriveClick(Sender: TObject);
    procedure ButtonWriteClick(Sender: TObject);
    procedure ButtonCancelWriteClick(Sender: TObject);
    procedure CheckBoxRepack720Change(Sender: TObject);

    // Read Tab
    procedure ButtonRefreshReadDrivesClick(Sender: TObject);
    procedure ComboBoxReadDriveChange(Sender: TObject);
    procedure ButtonBrowseTargetClick(Sender: TObject);
    procedure ButtonReadClick(Sender: TObject);
    procedure ButtonCancelReadClick(Sender: TObject);

    procedure ButtonCloseClick(Sender: TObject);

  private
    FCurrentSourcePath: string;
    FSourceData: TBytes;
    FSourceFormatDesc: string;
    FSourceGeometryValid: Boolean;
    FSourceSectorsPerTrack: Integer;
    FSourceSides: Integer;
    FSourceTracks: Integer;

    FCanRepack720: Boolean;
    FRepackFileCount: Integer;
    FRepackDirCount: Integer;
    FRepackTotalBytes: Int64;
    FRepackClusters: Integer;

    FIsAdmin: Boolean;
    FAbortOperation: Boolean;
    FOperationInProgress: Boolean;

    procedure CheckAdminStatus;
    procedure PopulateDriveLists;
    procedure InspectTargetDrive(const ADriveLetter: string; ALabel: TLabel);
    function LoadAndAnalyzeImage(const AFilePath: string): Boolean;
    procedure OnWriteProgressUpdate(ACyl, AHead, ATrackIdx, ATotalTracks: Integer; var AAbort: Boolean);
    procedure OnReadProgressUpdate(ACyl, AHead, ATrackIdx, ATotalTracks: Integer; var AAbort: Boolean);
    procedure SetUIEnabled(AEnabled: Boolean);

  public
    procedure OpenForWriting(const AImagePath: string = '');
    procedure OpenForReading;
  end;

var
  FormFloppyWriter: TFormFloppyWriter;

// Low-Level Win32 Floppy IOCTL Constants
const
  FSCTL_LOCK_VOLUME               = $00090018;
  FSCTL_UNLOCK_VOLUME             = $0009001C;
  FSCTL_DISMOUNT_VOLUME           = $00090020;
  IOCTL_DISK_GET_DRIVE_GEOMETRY   = $00070000;

function CheckProcessIsAdmin: Boolean;
function QueryDriveFloppyGeometry(const ADrive: string; out ACylinders: Int64;
  out AHeads, ASpt, ASectorSize: DWORD; out AMediaName: string): Boolean;
function WriteAtariImageToFloppy(const ADrive: string; const AImageData: TBytes;
  AVerify: Boolean; AOnProgress: TFloppyProgress; var AAbort: Boolean; out AErrorMessage: string): Boolean;
function ReadFloppyToAtariImage(const ADrive: string; const AOutFilePath: string;
  ARetries: Integer; AOnProgress: TFloppyProgress; var AAbort: Boolean; out AErrorMessage: string): Boolean;
procedure LaunchWindowsFormatDialog(AHandle: HWND; ADriveLetter: Char);

implementation

uses
  unit1;

{$R *.lfm}

type
  TFnIsUserAnAdmin = function: BOOL; stdcall;
  TFnSHFormatDrive = function(hWnd: HWND; Drive: UINT; fmtID: UINT; Options: UINT): DWORD; stdcall;

{ CheckProcessIsAdmin }
function CheckProcessIsAdmin: Boolean;
{$IFDEF MSWINDOWS}
var
  H: HMODULE;
  pIsAdmin: TFnIsUserAnAdmin;
begin
  Result := False;
  H := LoadLibrary('shell32.dll');
  if H <> 0 then
  begin
    pIsAdmin := TFnIsUserAnAdmin(GetProcAddress(H, 'IsUserAnAdmin'));
    if Assigned(pIsAdmin) then
      Result := pIsAdmin();
    FreeLibrary(H);
  end;
end;
{$ELSE}
begin
  Result := True;
end;
{$ENDIF}

{ LaunchWindowsFormatDialog }
procedure LaunchWindowsFormatDialog(AHandle: HWND; ADriveLetter: Char);
{$IFDEF MSWINDOWS}
var
  H: HMODULE;
  pFormat: TFnSHFormatDrive;
  DriveNum: UINT;
begin
  DriveNum := Ord(UpCase(ADriveLetter)) - Ord('A');
  H := LoadLibrary('shell32.dll');
  if H <> 0 then
  begin
    pFormat := TFnSHFormatDrive(GetProcAddress(H, 'SHFormatDrive'));
    if Assigned(pFormat) then
      pFormat(AHandle, DriveNum, $FFFF, 0);
    FreeLibrary(H);
  end;
end;
{$ELSE}
begin
  ShowMessage('Windows Floppy Format utility is not available on this platform.');
end;
{$ENDIF}

{ QueryDriveFloppyGeometry }
function QueryDriveFloppyGeometry(const ADrive: string; out ACylinders: Int64;
  out AHeads, ASpt, ASectorSize: DWORD; out AMediaName: string): Boolean;
{$IFDEF MSWINDOWS}
var
  DevPath: string;
  hDevice: THandle;
  Geom: DISK_GEOMETRY;
  BytesRet: DWORD;
  Err: DWORD;
begin
  Result := False;
  ACylinders := 0;
  AHeads := 0;
  ASpt := 0;
  ASectorSize := 512;
  AMediaName := 'No disk or unknown media';

  if Length(ADrive) < 1 then Exit;
  DevPath := '\\.\' + Copy(ADrive, 1, 1) + ':';

  hDevice := CreateFile(PChar(DevPath), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil, OPEN_EXISTING, 0, 0);
  if hDevice = INVALID_HANDLE_VALUE then
  begin
    Err := GetLastError;
    if Err = 5 then
      AMediaName := 'Access Denied (Administrator rights required)'
    else if Err = 21 then
      AMediaName := 'Drive not ready (No disk inserted)'
    else
      AMediaName := Format('Drive not ready (Win32 Error %d)', [Err]);
    Exit;
  end;

  try
    FillChar(Geom, SizeOf(Geom), 0);
    if DeviceIoControl(hDevice, IOCTL_DISK_GET_DRIVE_GEOMETRY, nil, 0, @Geom, SizeOf(Geom), BytesRet, nil) then
    begin
      ACylinders := Geom.Cylinders.QuadPart;
      AHeads := Geom.TracksPerCylinder;
      ASpt := Geom.SectorsPerTrack;
      ASectorSize := Geom.BytesPerSector;
      if ASectorSize = 0 then ASectorSize := 512;

      if (ASpt = 9) and (ACylinders >= 80) and (AHeads = 2) then
        AMediaName := '720 KB Double-Density (80 Cylinders, 2 Heads, 9 SPT)'
      else if (ASpt = 18) and (ACylinders >= 80) and (AHeads = 2) then
        AMediaName := '1.44 MB High-Density (80 Cylinders, 2 Heads, 18 SPT)'
      else if (ASpt = 9) and (ACylinders >= 80) and (AHeads = 1) then
        AMediaName := '360 KB Single-Sided (80 Cylinders, 1 Head, 9 SPT)'
      else if (ACylinders > 0) and (ASpt > 0) then
        AMediaName := Format('Floppy Media: %d Cylinders, %d Heads, %d SPT (%d KB)',
          [ACylinders, AHeads, ASpt, (ACylinders * AHeads * ASpt * ASectorSize) div 1024])
      else
        AMediaName := 'Unformatted or unrecognized floppy disk';

      Result := True;
    end
    else
    begin
      Err := GetLastError;
      if Err = 21 then
        AMediaName := 'Drive empty (Please insert a floppy disk)'
      else
        AMediaName := Format('Unformatted disk or error querying media (%d)', [Err]);
    end;
  finally
    CloseHandle(hDevice);
  end;
end;
{$ELSE}
begin
  ACylinders := 80;
  AHeads := 2;
  ASpt := 9;
  ASectorSize := 512;
  AMediaName := 'Simulated 720 KB Floppy Drive';
  Result := True;
end;
{$ENDIF}

{ WriteAtariImageToFloppy }
function WriteAtariImageToFloppy(const ADrive: string; const AImageData: TBytes;
  AVerify: Boolean; AOnProgress: TFloppyProgress; var AAbort: Boolean; out AErrorMessage: string): Boolean;
{$IFDEF MSWINDOWS}
var
  DevPath: string;
  hDevice: THandle;
  BytesRet, BytesWritten, BytesRead, Err: DWORD;
  ImageSize: Integer;
  TotalCylinders, TotalHeads, SectorsPerTrack: Integer;
  BytesPerTrack, TotalTracks, TrackIndex: Integer;
  Cyl, Head: Integer;
  PhysicalOffset, ImageOffset: Int64;
  TrackBuf, VerifyBuf: array of Byte;
begin
  Result := False;
  AErrorMessage := '';

  ImageSize := Length(AImageData);
  if ImageSize = 737280 then
  begin
    TotalCylinders := 80;
    TotalHeads := 2;
    SectorsPerTrack := 9;
  end
  else if ImageSize = 1474560 then
  begin
    TotalCylinders := 80;
    TotalHeads := 2;
    SectorsPerTrack := 18;
  end
  else if ImageSize = 368640 then
  begin
    TotalCylinders := 80;
    TotalHeads := 1;
    SectorsPerTrack := 9;
  end
  else
  begin
    AErrorMessage := Format('Unsupported image size: %d bytes. USB floppy drives only support standard 720 KB (737,280 bytes) and 1.44 MB (1,474,560 bytes) layouts.', [ImageSize]);
    Exit;
  end;

  BytesPerTrack := SectorsPerTrack * 512;
  SetLength(TrackBuf, BytesPerTrack);
  SetLength(VerifyBuf, BytesPerTrack);

  DevPath := '\\.\' + Copy(ADrive, 1, 1) + ':';
  hDevice := CreateFile(PChar(DevPath), GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);

  if hDevice = INVALID_HANDLE_VALUE then
  begin
    Err := GetLastError;
    if Err = 5 then
      AErrorMessage := 'Access Denied: Writing raw sectors to a physical floppy drive requires Administrator privileges.' + sLineBreak +
                       'Please restart STIMAGE as Administrator.'
    else if Err = 21 then
      AErrorMessage := 'Drive not ready: Please ensure a formatted floppy disk is inserted into ' + ADrive
    else
      AErrorMessage := Format('Failed to open drive %s (Win32 Error %d).', [ADrive, Err]);
    Exit;
  end;

  try
    // 1. Lock the volume to prevent Windows from interfering with disk blocks
    DeviceIoControl(hDevice, FSCTL_LOCK_VOLUME, nil, 0, nil, 0, BytesRet, nil);

    // 2. Dismount the volume so Windows invalidates cached filesystem buffers
    DeviceIoControl(hDevice, FSCTL_DISMOUNT_VOLUME, nil, 0, nil, 0, BytesRet, nil);

    TotalTracks := TotalCylinders * 2; // Always 160 physical tracks for double-sided drives
    TrackIndex := 0;

    for Cyl := 0 to TotalCylinders - 1 do
    begin
      for Head := 0 to 1 do
      begin
        if AAbort then Break;

        PhysicalOffset := (Int64(Cyl) * 2 + Head) * BytesPerTrack;

        if TotalHeads = 1 then
        begin
          // Single-sided 360K image mapped to 720K DD disk: Side 0 has image tracks, Side 1 formatted empty
          if Head = 0 then
          begin
            ImageOffset := Int64(Cyl) * BytesPerTrack;
            Move(AImageData[ImageOffset], TrackBuf[0], BytesPerTrack);
          end
          else
          begin
            FillChar(TrackBuf[0], BytesPerTrack, $E5);
          end;
        end
        else
        begin
          ImageOffset := PhysicalOffset;
          Move(AImageData[ImageOffset], TrackBuf[0], BytesPerTrack);
        end;

        // Seek to track offset
        SetFilePointer(hDevice, PhysicalOffset, nil, FILE_BEGIN);

        // Write track
        BytesWritten := 0;
        if not WriteFile(hDevice, TrackBuf[0], BytesPerTrack, BytesWritten, nil) or (BytesWritten <> DWORD(BytesPerTrack)) then
        begin
          Err := GetLastError;
          AErrorMessage := Format('Write failed at Cylinder %d, Head %d (Win32 Error %d).' + sLineBreak +
            'If using a 1.44 MB disk for a 720 KB image, verify that the density hole is taped over.',
            [Cyl, Head, Err]);
          Exit;
        end;

        // Optional verify pass
        if AVerify then
        begin
          SetFilePointer(hDevice, PhysicalOffset, nil, FILE_BEGIN);
          BytesRead := 0;
          if not ReadFile(hDevice, VerifyBuf[0], BytesPerTrack, BytesRead, nil) or (BytesRead <> DWORD(BytesPerTrack)) then
          begin
            Err := GetLastError;
            AErrorMessage := Format('Verify read failed at Cylinder %d, Head %d (Win32 Error %d).', [Cyl, Head, Err]);
            Exit;
          end;

          if not CompareMem(@TrackBuf[0], @VerifyBuf[0], BytesPerTrack) then
          begin
            AErrorMessage := Format('Verification mismatch at Cylinder %d, Head %d: Disk media data differs from source image.', [Cyl, Head]);
            Exit;
          end;
        end;

        Inc(TrackIndex);
        if Assigned(AOnProgress) then
          AOnProgress(Cyl, Head, TrackIndex, TotalTracks, AAbort);
      end;
      if AAbort then Break;
    end;

    if AAbort then
    begin
      AErrorMessage := 'Write operation cancelled by user.';
      Exit;
    end;

    Result := True;
  finally
    // Unlock volume and close handle
    DeviceIoControl(hDevice, FSCTL_UNLOCK_VOLUME, nil, 0, nil, 0, BytesRet, nil);
    CloseHandle(hDevice);
  end;
end;
{$ELSE}
begin
  AErrorMessage := 'Physical floppy drive writes are only supported on Windows.';
  Result := False;
end;
{$ENDIF}

{ ReadFloppyToAtariImage }
function ReadFloppyToAtariImage(const ADrive: string; const AOutFilePath: string;
  ARetries: Integer; AOnProgress: TFloppyProgress; var AAbort: Boolean; out AErrorMessage: string): Boolean;
{$IFDEF MSWINDOWS}
var
  DevPath: string;
  hDevice: THandle;
  Geom: DISK_GEOMETRY;
  BytesRet, BytesRead, Err: DWORD;
  TotalCylinders, TotalHeads, SectorsPerTrack: Integer;
  BytesPerTrack, TotalTracks, TrackIndex: Integer;
  TotalImageBytes: Int64;
  ImageData: TBytes;
  Cyl, Head, RetryCount: Integer;
  PhysicalOffset: Int64;
  ReadSuccess: Boolean;
  FS: TFileStream;
begin
  Result := False;
  AErrorMessage := '';

  if Length(ADrive) < 1 then
  begin
    AErrorMessage := 'No floppy drive selected.';
    Exit;
  end;

  DevPath := '\\.\' + Copy(ADrive, 1, 1) + ':';
  hDevice := CreateFile(PChar(DevPath), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil, OPEN_EXISTING, 0, 0);

  if hDevice = INVALID_HANDLE_VALUE then
  begin
    Err := GetLastError;
    if Err = 5 then
      AErrorMessage := 'Access Denied: Reading physical floppy sectors requires Administrator privileges.'
    else if Err = 21 then
      AErrorMessage := 'Drive not ready: Please insert a floppy disk into ' + ADrive
    else
      AErrorMessage := Format('Failed to open drive %s (Win32 Error %d).', [ADrive, Err]);
    Exit;
  end;

  try
    FillChar(Geom, SizeOf(Geom), 0);
    if not DeviceIoControl(hDevice, IOCTL_DISK_GET_DRIVE_GEOMETRY, nil, 0, @Geom, SizeOf(Geom), BytesRet, nil) then
    begin
      AErrorMessage := 'Failed to detect media geometry. Ensure a valid floppy disk is inserted.';
      Exit;
    end;

    TotalCylinders := Geom.Cylinders.QuadPart;
    TotalHeads := Geom.TracksPerCylinder;
    SectorsPerTrack := Geom.SectorsPerTrack;
    if TotalCylinders < 80 then TotalCylinders := 80;
    if TotalHeads < 2 then TotalHeads := 2;
    if (SectorsPerTrack <> 9) and (SectorsPerTrack <> 18) then
      SectorsPerTrack := 9; // Fallback to standard 720K DD

    BytesPerTrack := SectorsPerTrack * 512;
    TotalTracks := TotalCylinders * TotalHeads;
    TotalImageBytes := Int64(TotalTracks) * BytesPerTrack;

    SetLength(ImageData, TotalImageBytes);

    // Lock volume for reading
    DeviceIoControl(hDevice, FSCTL_LOCK_VOLUME, nil, 0, nil, 0, BytesRet, nil);

    TrackIndex := 0;
    for Cyl := 0 to TotalCylinders - 1 do
    begin
      for Head := 0 to TotalHeads - 1 do
      begin
        if AAbort then Break;

        PhysicalOffset := (Int64(Cyl) * TotalHeads + Head) * BytesPerTrack;
        ReadSuccess := False;

        for RetryCount := 0 to ARetries do
        begin
          SetFilePointer(hDevice, PhysicalOffset, nil, FILE_BEGIN);
          BytesRead := 0;
          if ReadFile(hDevice, ImageData[PhysicalOffset], BytesPerTrack, BytesRead, nil) and (BytesRead = DWORD(BytesPerTrack)) then
          begin
            ReadSuccess := True;
            Break;
          end;
          Sleep(50);
        end;

        if not ReadSuccess then
        begin
          AErrorMessage := Format('Unrecoverable read error at Cylinder %d, Head %d.', [Cyl, Head]);
          Exit;
        end;

        Inc(TrackIndex);
        if Assigned(AOnProgress) then
          AOnProgress(Cyl, Head, TrackIndex, TotalTracks, AAbort);
      end;
      if AAbort then Break;
    end;

    if AAbort then
    begin
      AErrorMessage := 'Read operation cancelled by user.';
      Exit;
    end;

    // Save image to output .st file
    FS := TFileStream.Create(AOutFilePath, fmCreate);
    try
      if Length(ImageData) > 0 then
        FS.WriteBuffer(ImageData[0], Length(ImageData));
    finally
      FS.Free;
    end;

    Result := True;
  finally
    DeviceIoControl(hDevice, FSCTL_UNLOCK_VOLUME, nil, 0, nil, 0, BytesRet, nil);
    CloseHandle(hDevice);
  end;
end;
{$ELSE}
begin
  AErrorMessage := 'Physical floppy drive reading is only supported on Windows.';
  Result := False;
end;
{$ENDIF}

{ TFormFloppyWriter }

procedure TFormFloppyWriter.FormCreate(Sender: TObject);
begin
  FCurrentSourcePath := '';
  FSourceGeometryValid := False;
  FCanRepack720 := False;
  FOperationInProgress := False;
  FAbortOperation := False;

  CheckBoxRepack720.Checked := False;
  CheckBoxRepack720.Enabled := False;
  PageControlFloppy.ActivePageIndex := 0;
  CheckAdminStatus;
  PopulateDriveLists;
end;

procedure TFormFloppyWriter.FormShow(Sender: TObject);
begin
  CheckAdminStatus;
  PopulateDriveLists;
  if (FCurrentSourcePath <> '') and FileExists(FCurrentSourcePath) then
    LoadAndAnalyzeImage(FCurrentSourcePath);
end;

procedure TFormFloppyWriter.CheckAdminStatus;
begin
  FIsAdmin := CheckProcessIsAdmin;
  PanelAdminBanner.Visible := not FIsAdmin;
end;

procedure TFormFloppyWriter.ButtonRelaunchAdminClick(Sender: TObject);
{$IFDEF MSWINDOWS}
var
  Params: string;
begin
  if FCurrentSourcePath <> '' then
    Params := '"' + FCurrentSourcePath + '"'
  else
    Params := '';
  ShellExecute(0, 'runas', PChar(Application.ExeName), PChar(Params), nil, SW_SHOWNORMAL);
  Application.Terminate;
end;
{$ELSE}
begin
  ShowMessage('Administrator elevation is only supported on Windows.');
end;
{$ENDIF}

procedure TFormFloppyWriter.PopulateDriveLists;
var
  DriveList: TStringList;
  Drives: DWORD;
  I: Integer;
  DriveLetter: Char;
  DriveRoot: string;
  DriveTypeVal: UINT;
begin
  DriveList := TStringList.Create;
  try
    {$IFDEF MSWINDOWS}
    Drives := GetLogicalDrives;
    for I := 0 to 25 do
    begin
      if (Drives and (1 shl I)) <> 0 then
      begin
        DriveLetter := Chr(Ord('A') + I);
        DriveRoot := DriveLetter + ':\';
        DriveTypeVal := GetDriveType(PChar(DriveRoot));
        if (DriveTypeVal = DRIVE_REMOVABLE) or (I < 2) then // A:, B:, or any removable USB floppy
          DriveList.Add(DriveLetter + ':\');
      end;
    end;
    {$ELSE}
    DriveList.Add('A:\');
    DriveList.Add('B:\');
    {$ENDIF}

    if DriveList.Count = 0 then
      DriveList.Add('A:\');

    ComboBoxDrive.Items.Assign(DriveList);
    ComboBoxReadDrive.Items.Assign(DriveList);

    if ComboBoxDrive.ItemIndex < 0 then
      ComboBoxDrive.ItemIndex := 0;
    if ComboBoxReadDrive.ItemIndex < 0 then
      ComboBoxReadDrive.ItemIndex := 0;

    InspectTargetDrive(ComboBoxDrive.Text, LabelMediaStatus);
    InspectTargetDrive(ComboBoxReadDrive.Text, LabelReadMediaStatus);
  finally
    DriveList.Free;
  end;
end;

procedure TFormFloppyWriter.InspectTargetDrive(const ADriveLetter: string; ALabel: TLabel);
var
  Cyl: Int64;
  Heads, Spt, SecSize: DWORD;
  MediaName: string;
begin
  if ADriveLetter = '' then
  begin
    ALabel.Caption := 'Drive status: [No drive selected]';
    ALabel.Font.Color := clGrayText;
    Exit;
  end;

  if QueryDriveFloppyGeometry(ADriveLetter, Cyl, Heads, Spt, SecSize, MediaName) then
  begin
    ALabel.Caption := 'Inserted Disk: ' + MediaName;
    ALabel.Font.Color := clGreen;
  end
  else
  begin
    ALabel.Caption := 'Drive Status: ' + MediaName;
    ALabel.Font.Color := clRed;
  end;
end;

procedure TFormFloppyWriter.ButtonRefreshDrivesClick(Sender: TObject);
begin
  PopulateDriveLists;
end;

procedure TFormFloppyWriter.ButtonRefreshReadDrivesClick(Sender: TObject);
begin
  PopulateDriveLists;
end;

procedure TFormFloppyWriter.ComboBoxDriveChange(Sender: TObject);
begin
  InspectTargetDrive(ComboBoxDrive.Text, LabelMediaStatus);
end;

procedure TFormFloppyWriter.ComboBoxReadDriveChange(Sender: TObject);
begin
  InspectTargetDrive(ComboBoxReadDrive.Text, LabelReadMediaStatus);
end;

procedure TFormFloppyWriter.ButtonFormatDriveClick(Sender: TObject);
var
  DriveChar: Char;
begin
  if ComboBoxDrive.Text = '' then Exit;
  DriveChar := ComboBoxDrive.Text[1];
  LaunchWindowsFormatDialog(Self.Handle, DriveChar);
  InspectTargetDrive(ComboBoxDrive.Text, LabelMediaStatus);
end;

function TFormFloppyWriter.LoadAndAnalyzeImage(const AFilePath: string): Boolean;
var
  Ext, TempST, RepackReason: string;
  FS: TFileStream;
  ImageSize: Integer;
begin
  Result := False;
  FSourceGeometryValid := False;
  FCanRepack720 := False;
  CheckBoxRepack720.Checked := False;
  CheckBoxRepack720.Enabled := False;
  SetLength(FSourceData, 0);

  if not FileExists(AFilePath) then
  begin
    LabelImageDetails.Caption := 'Selected file does not exist.';
    LabelImageDetails.Font.Color := clRed;
    LabelCompatibilityWarning.Caption := '';
    ButtonWrite.Enabled := False;
    Exit;
  end;

  Ext := LowerCase(ExtractFileExt(AFilePath));

  try
    if Ext = '.msa' then
    begin
      TempST := IncludeTrailingPathDelimiter(GetTempDir) + 'msa_write_temp_' + FormatDateTime('hhnnsszzz', Now) + '.st';
      try
        if not UnpackMSAToST(AFilePath, TempST) or not FileExists(TempST) then
        begin
          LabelImageDetails.Caption := 'Error: Failed to decompress MSA image.';
          LabelImageDetails.Font.Color := clRed;
          ButtonWrite.Enabled := False;
          Exit;
        end;
        FS := TFileStream.Create(TempST, fmOpenRead or fmShareDenyNone);
        try
          SetLength(FSourceData, FS.Size);
          if FS.Size > 0 then
            FS.ReadBuffer(FSourceData[0], FS.Size);
        finally
          FS.Free;
        end;
      finally
        if FileExists(TempST) then SysUtils.DeleteFile(TempST);
      end;
      FSourceFormatDesc := 'Decompressed MSA Archive';
    end
    else if Ext = '.dim' then
    begin
      TempST := IncludeTrailingPathDelimiter(GetTempDir) + 'dim_write_temp_' + FormatDateTime('hhnnsszzz', Now) + '.st';
      try
        if not UnpackDIMToST(AFilePath, TempST) or not FileExists(TempST) then
        begin
          LabelImageDetails.Caption := 'Error: Failed to decompress FastCopy DIM image.';
          LabelImageDetails.Font.Color := clRed;
          ButtonWrite.Enabled := False;
          Exit;
        end;
        FS := TFileStream.Create(TempST, fmOpenRead or fmShareDenyNone);
        try
          SetLength(FSourceData, FS.Size);
          if FS.Size > 0 then
            FS.ReadBuffer(FSourceData[0], FS.Size);
        finally
          FS.Free;
        end;
      finally
        if FileExists(TempST) then SysUtils.DeleteFile(TempST);
      end;
      FSourceFormatDesc := 'Decompressed FastCopy DIM Archive';
    end
    else
    begin
      // Direct raw .st or .img file
      FS := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyNone);
      try
        SetLength(FSourceData, FS.Size);
        if FS.Size > 0 then
          FS.ReadBuffer(FSourceData[0], FS.Size);
      finally
        FS.Free;
      end;
      FSourceFormatDesc := 'Raw Atari ST Disk Image (.ST)';
    end;

    ImageSize := Length(FSourceData);

    // Check if the files and directory payload can fit on a standard 720 KB floppy
    FCanRepack720 := (Form1 <> nil) and
      Form1.AnalyzeDiskImageFor720KB(AFilePath, FRepackFileCount, FRepackDirCount, FRepackTotalBytes, FRepackClusters, RepackReason);

    if ImageSize = 737280 then
    begin
      FSourceGeometryValid := True;
      FSourceTracks := 80;
      FSourceSides := 2;
      FSourceSectorsPerTrack := 9;
      ButtonWrite.Enabled := True;
      CheckBoxRepack720.Checked := False;
      CheckBoxRepack720.Enabled := FCanRepack720;
      LabelImageDetails.Caption := Format('Format: 720 KB Double-Density (%s) - 80 Tracks, 2 Sides, 9 Sectors/Track', [FSourceFormatDesc]);
      LabelImageDetails.Font.Color := clGreen;
      LabelCompatibilityWarning.Caption := '✅ Fully compatible with standard 3.5" USB and PC internal floppy drives.';
      LabelCompatibilityWarning.Font.Color := clGreen;
      Result := True;
    end
    else if ImageSize = 1474560 then
    begin
      FSourceGeometryValid := True;
      FSourceTracks := 80;
      FSourceSides := 2;
      FSourceSectorsPerTrack := 18;
      ButtonWrite.Enabled := True;
      LabelImageDetails.Caption := Format('Format: 1.44 MB High-Density (%s) - 80 Tracks, 2 Sides, 18 Sectors/Track', [FSourceFormatDesc]);
      LabelImageDetails.Font.Color := clGreen;

      CheckBoxRepack720.Enabled := FCanRepack720;
      CheckBoxRepack720.Checked := False;

      if FCanRepack720 then
        LabelCompatibilityWarning.Caption := '✅ Compatible with 1.44 MB HD drives. (Optional: Check "Repack files into standard 720 KB image" to transfer to 720 KB media)'
      else
        LabelCompatibilityWarning.Caption := '✅ Fully compatible with High-Density 1.44 MB USB and internal floppy drives.';
      LabelCompatibilityWarning.Font.Color := clGreen;
      Result := True;
    end
    else if ImageSize = 368640 then
    begin
      FSourceGeometryValid := True;
      FSourceTracks := 80;
      FSourceSides := 1;
      FSourceSectorsPerTrack := 9;
      ButtonWrite.Enabled := True;
      CheckBoxRepack720.Enabled := FCanRepack720;
      CheckBoxRepack720.Checked := False;
      LabelImageDetails.Caption := Format('Format: 360 KB Single-Sided (%s) - 80 Tracks, 1 Side, 9 Sectors/Track', [FSourceFormatDesc]);
      LabelImageDetails.Font.Color := $00804000;
      if FCanRepack720 then
        LabelCompatibilityWarning.Caption := 'ℹ️ Single-sided 360 KB image. (Tip: Check "Repack files into standard 720 KB image" to write as standard 2-sided 720 KB floppy).'
      else
        LabelCompatibilityWarning.Caption := 'ℹ️ Single-sided image will be mapped onto Side 0 of a Double-Density diskette (supported by Atari TOS).';
      LabelCompatibilityWarning.Font.Color := $00804000;
      Result := True;
    end
    else
    begin
      // Non-standard disk size (e.g. < 720 KB, 800 KB, 880 KB, 820 KB, etc.)
      FSourceGeometryValid := False;

      if FCanRepack720 then
      begin
        CheckBoxRepack720.Enabled := True;
        CheckBoxRepack720.Checked := True;
        ButtonWrite.Enabled := True;

        if ImageSize = 819200 then
          LabelImageDetails.Caption := 'Format: Atari 800 KB Extended (80T, 2S, 10SPT) - Files Fit on 720 KB Floppy!'
        else if ImageSize = 901120 then
          LabelImageDetails.Caption := 'Format: Atari 880 KB Extended (80T, 2S, 11SPT) - Files Fit on 720 KB Floppy!'
        else if ImageSize = 839680 then
          LabelImageDetails.Caption := 'Format: Atari 820 KB Extended (82T, 2S, 10SPT) - Files Fit on 720 KB Floppy!'
        else if ImageSize < 737280 then
          LabelImageDetails.Caption := Format('Format: Non-Standard Image (%s bytes < 720 KB) - Files Fit on 720 KB Floppy!', [FormatFloat('#,##0', ImageSize)])
        else
          LabelImageDetails.Caption := Format('Format: Extended Disk Image (%s bytes) - Files Fit on 720 KB Floppy!', [FormatFloat('#,##0', ImageSize)]);

        LabelImageDetails.Font.Color := $00804000;
        LabelCompatibilityWarning.Caption := Format('💡 Non-standard geometry (%s bytes), but all %d file(s) and %d folder(s) (%s) fit on a standard 720 KB floppy.' + sLineBreak +
          '"Repack files into standard 720 KB image" is checked: A temporary 720 KB image will be transferred to disk.',
          [FormatFloat('#,##0', ImageSize), FRepackFileCount, FRepackDirCount, Form1.FormatSizeStr(FRepackTotalBytes, False)]);
        LabelCompatibilityWarning.Font.Color := clGreen;
        Result := True;
      end
      else
      begin
        CheckBoxRepack720.Checked := False;
        CheckBoxRepack720.Enabled := False;
        ButtonWrite.Enabled := False;

        LabelImageDetails.Caption := Format('Non-standard image size: %s bytes', [FormatFloat('#,##0', ImageSize)]);
        LabelImageDetails.Font.Color := clRed;
        if ImageSize > 737280 then
        begin
          if RepackReason <> '' then
            LabelCompatibilityWarning.Caption := '❌ ' + RepackReason
          else
            LabelCompatibilityWarning.Caption := '❌ Non-standard image size. Content exceeds 720 KB floppy capacity.';
        end
        else
        begin
          if RepackReason <> '' then
            LabelCompatibilityWarning.Caption := '❌ Non-standard disk image format: ' + RepackReason
          else
            LabelCompatibilityWarning.Caption := '❌ Unsupported disk size. USB floppy drives only support standard 720 KB and 1.44 MB sector layouts.';
        end;
        LabelCompatibilityWarning.Font.Color := clRed;
      end;
    end;
  except
    on E: Exception do
    begin
      LabelImageDetails.Caption := 'Error reading disk image: ' + E.Message;
      LabelImageDetails.Font.Color := clRed;
      LabelCompatibilityWarning.Caption := '';
      FCanRepack720 := False;
      CheckBoxRepack720.Checked := False;
      CheckBoxRepack720.Enabled := False;
      ButtonWrite.Enabled := False;
    end;
  end;
end;

procedure TFormFloppyWriter.ButtonBrowseImageClick(Sender: TObject);
begin
  OpenDialogImage.Title := 'Select Atari ST Disk Image to Write';
  OpenDialogImage.Filter := 'Atari Disk Images (*.st;*.msa;*.dim)|*.st;*.msa;*.dim|All Files (*.*)|*.*';
  if OpenDialogImage.Execute then
  begin
    EditSourceImage.Text := OpenDialogImage.FileName;
  end;
end;

procedure TFormFloppyWriter.EditSourceImageChange(Sender: TObject);
begin
  FCurrentSourcePath := EditSourceImage.Text;
  LoadAndAnalyzeImage(FCurrentSourcePath);
end;

procedure TFormFloppyWriter.SetUIEnabled(AEnabled: Boolean);
begin
  ButtonWrite.Enabled := AEnabled and (FSourceGeometryValid or (CheckBoxRepack720.Checked and FCanRepack720));
  CheckBoxRepack720.Enabled := AEnabled and (FCanRepack720 or FSourceGeometryValid);
  ButtonBrowseImage.Enabled := AEnabled;
  EditSourceImage.Enabled := AEnabled;
  ComboBoxDrive.Enabled := AEnabled;
  ButtonRefreshDrives.Enabled := AEnabled;
  ButtonFormatDrive.Enabled := AEnabled;
  CheckBoxVerify.Enabled := AEnabled;

  ButtonRead.Enabled := AEnabled;
  ComboBoxReadDrive.Enabled := AEnabled;
  ButtonRefreshReadDrives.Enabled := AEnabled;
  EditTargetImage.Enabled := AEnabled;
  ButtonBrowseTarget.Enabled := AEnabled;
  CheckBoxOpenAfterRead.Enabled := AEnabled;
  CheckBoxRetryBadSectors.Enabled := AEnabled;

  ButtonClose.Enabled := AEnabled;
  ButtonCancelWrite.Enabled := not AEnabled;
  ButtonCancelRead.Enabled := not AEnabled;
end;

procedure TFormFloppyWriter.OnWriteProgressUpdate(ACyl, AHead, ATrackIdx, ATotalTracks: Integer; var AAbort: Boolean);
begin
  ProgressBarWrite.Position := (ATrackIdx * 100) div ATotalTracks;
  LabelWriteStatus.Caption := Format('Writing Cylinder %d/80, Head %d (%d of %d tracks)...',
    [ACyl + 1, AHead, ATrackIdx, ATotalTracks]);
  Application.ProcessMessages;
  AAbort := FAbortOperation;
end;

procedure TFormFloppyWriter.OnReadProgressUpdate(ACyl, AHead, ATrackIdx, ATotalTracks: Integer; var AAbort: Boolean);
begin
  ProgressBarRead.Position := (ATrackIdx * 100) div ATotalTracks;
  LabelReadStatus.Caption := Format('Reading Cylinder %d/80, Head %d (%d of %d tracks)...',
    [ACyl + 1, AHead, ATrackIdx, ATotalTracks]);
  Application.ProcessMessages;
  AAbort := FAbortOperation;
end;

procedure TFormFloppyWriter.ButtonWriteClick(Sender: TObject);
var
  TargetDrive: string;
  ErrMsg: string;
  ConfirmMsg: string;
  WriteBuffer: TBytes;
  Temp720ST: string;
  RepackedFileCount, RepackedDirCount: Integer;
  RepackedBytesTotal: Int64;
  RepackStream: TFileStream;
  WriteSuccess: Boolean;
  ImageSize: Integer;
begin
  TargetDrive := ComboBoxDrive.Text;
  if TargetDrive = '' then
  begin
    ShowMessage('Please select a target floppy drive.');
    Exit;
  end;

  {$IFDEF MSWINDOWS}
  if not FIsAdmin then
  begin
    if MessageDlg('Administrator Privileges Required',
      'Writing raw sectors to physical floppy drives on Windows requires Administrator privileges.' + sLineBreak + sLineBreak +
      'Would you like to restart STIMAGE as Administrator now?',
      mtWarning, [mbYes, mbNo], 0) = mrYes then
    begin
      ButtonRelaunchAdminClick(nil);
      Exit;
    end
    else
      Exit;
  end;
  {$ENDIF}

  ImageSize := Length(FSourceData);

  // Check if repacking into standard 720 KB is requested
  if CheckBoxRepack720.Checked then
  begin
    if (Form1 = nil) or not Form1.AnalyzeDiskImageFor720KB(FCurrentSourcePath, FRepackFileCount, FRepackDirCount, FRepackTotalBytes, FRepackClusters, ErrMsg) then
    begin
      ShowMessage('Cannot repack disk image into standard 720 KB floppy:' + sLineBreak + sLineBreak + ErrMsg);
      Exit;
    end;

    ConfirmMsg := Format('Are you sure you want to write to physical floppy drive %s?' + sLineBreak + sLineBreak +
      'Source Image: %s (%s)' + sLineBreak +
      'Contents: %d file(s), %d folder(s) (%s)' + sLineBreak +
      'Mode: Repacking files into temporary standard 720 KB floppy layout' + sLineBreak +
      'Target Drive: %s' + sLineBreak + sLineBreak +
      'WARNING: All existing data on the floppy diskette will be permanently overwritten!',
      [TargetDrive, ExtractFileName(FCurrentSourcePath), FSourceFormatDesc,
       FRepackFileCount, FRepackDirCount, Form1.FormatSizeStr(FRepackTotalBytes, False), TargetDrive]);

    if MessageDlg('Confirm Physical Floppy Write', ConfirmMsg, mtWarning, [mbYes, mbNo], 0) <> mrYes then
      Exit;

    FAbortOperation := False;
    FOperationInProgress := True;
    SetUIEnabled(False);
    ProgressBarWrite.Position := 0;
    LabelWriteStatus.Caption := 'Creating temporary standard 720 KB image...';
    LabelWriteStatus.Font.Color := clNavy;
    Screen.Cursor := crHourGlass;
    Application.ProcessMessages;

    Temp720ST := IncludeTrailingPathDelimiter(GetTempDir) + 'repack720_' + FormatDateTime('hhnnsszzz', Now) + '.st';
    try
      if not Form1.RepackDiskImageTo720KB(FCurrentSourcePath, Temp720ST,
        RepackedFileCount, RepackedDirCount, RepackedBytesTotal, ErrMsg) or not FileExists(Temp720ST) then
      begin
        LabelWriteStatus.Caption := 'Error: Repack failed.';
        LabelWriteStatus.Font.Color := clRed;
        ShowMessage('Failed to create temporary 720 KB image:' + sLineBreak + ErrMsg);
        Exit;
      end;

      RepackStream := TFileStream.Create(Temp720ST, fmOpenRead or fmShareDenyNone);
      try
        SetLength(WriteBuffer, RepackStream.Size);
        if RepackStream.Size > 0 then
          RepackStream.ReadBuffer(WriteBuffer[0], RepackStream.Size);
      finally
        RepackStream.Free;
      end;

      if Length(WriteBuffer) <> 737280 then
      begin
        ShowMessage(Format('Internal error: Repacked image size is %d bytes instead of 737,280 bytes.', [Length(WriteBuffer)]));
        Exit;
      end;

      LabelWriteStatus.Caption := Format('Writing repacked 720 KB layout to drive %s...', [TargetDrive]);
      LabelWriteStatus.Font.Color := clNavy;
      Application.ProcessMessages;

      WriteSuccess := WriteAtariImageToFloppy(TargetDrive, WriteBuffer, CheckBoxVerify.Checked,
        @OnWriteProgressUpdate, FAbortOperation, ErrMsg);

      if WriteSuccess then
      begin
        LabelWriteStatus.Caption := '✅ Floppy write completed and verified successfully!';
        LabelWriteStatus.Font.Color := clGreen;
        ProgressBarWrite.Position := 100;
        MessageDlg('Floppy Write Complete',
          Format('Successfully transferred %d file(s) and %d folder(s) (%s) to 720 KB floppy in drive %s!' + sLineBreak + sLineBreak +
                 'The disk is formatted as standard Double-Density Atari ST FAT12 and is ready for use on any Atari ST or PC.',
                 [RepackedFileCount, RepackedDirCount, Form1.FormatSizeStr(RepackedBytesTotal, False), TargetDrive]),
          mtInformation, [mbOK], 0);
      end
      else
      begin
        LabelWriteStatus.Caption := 'Write failed: ' + ErrMsg;
        LabelWriteStatus.Font.Color := clRed;
        MessageDlg('Write Error', 'Floppy write failed:' + sLineBreak + sLineBreak + ErrMsg, mtError, [mbOK], 0);
      end;
    finally
      if FileExists(Temp720ST) then
        SysUtils.DeleteFile(Temp720ST);
      FOperationInProgress := False;
      SetUIEnabled(True);
      Screen.Cursor := crDefault;
      InspectTargetDrive(TargetDrive, LabelMediaStatus);
    end;
  end
  else
  begin
    // CheckBoxRepack720 is NOT checked: direct image write
    if not FSourceGeometryValid or (ImageSize = 0) then
    begin
      if FCanRepack720 then
      begin
        if MessageDlg('Non-Standard Image Format',
          'This disk image has a non-standard geometry which USB floppy drives cannot write directly.' + sLineBreak + sLineBreak +
          'Would you like to enable "Repack files into standard 720 KB image" to transfer its files to floppy?',
          mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          CheckBoxRepack720.Checked := True;
          ButtonWriteClick(Sender);
          Exit;
        end;
      end;
      ShowMessage('Please select a valid 720 KB or 1.44 MB Atari ST disk image.');
      Exit;
    end;

    ConfirmMsg := Format('Are you sure you want to write this image to physical floppy drive %s?' + sLineBreak + sLineBreak +
      'Source Image: %s (%s)' + sLineBreak +
      'Target Drive: %s' + sLineBreak + sLineBreak +
      'WARNING: All existing data on the floppy diskette will be permanently overwritten!',
      [TargetDrive, ExtractFileName(FCurrentSourcePath), FSourceFormatDesc, TargetDrive]);

    if MessageDlg('Confirm Physical Floppy Write', ConfirmMsg, mtWarning, [mbYes, mbNo], 0) <> mrYes then
      Exit;

    FAbortOperation := False;
    FOperationInProgress := True;
    SetUIEnabled(False);
    ProgressBarWrite.Position := 0;
    LabelWriteStatus.Caption := 'Initializing floppy drive write...';
    Screen.Cursor := crHourGlass;
    try
      if WriteAtariImageToFloppy(TargetDrive, FSourceData, CheckBoxVerify.Checked,
           @OnWriteProgressUpdate, FAbortOperation, ErrMsg) then
      begin
        ProgressBarWrite.Position := 100;
        LabelWriteStatus.Font.Color := clGreen;
        if CheckBoxVerify.Checked then
          LabelWriteStatus.Caption := 'Write and verify completed successfully!'
        else
          LabelWriteStatus.Caption := 'Write completed successfully!';
        MessageDlg('Floppy Write Complete',
          'The Atari ST disk image was successfully written to floppy drive ' + TargetDrive + '!' + sLineBreak +
          'You can now use this disk on your Atari ST computer.',
          mtInformation, [mbOK], 0);
      end
      else
      begin
        LabelWriteStatus.Font.Color := clRed;
        LabelWriteStatus.Caption := 'Write failed: ' + ErrMsg;
        MessageDlg('Floppy Write Failed', ErrMsg, mtError, [mbOK], 0);
      end;
    finally
      FOperationInProgress := False;
      SetUIEnabled(True);
      Screen.Cursor := crDefault;
      InspectTargetDrive(ComboBoxDrive.Text, LabelMediaStatus);
    end;
  end;
end;

procedure TFormFloppyWriter.ButtonCancelWriteClick(Sender: TObject);
begin
  FAbortOperation := True;
  LabelWriteStatus.Caption := 'Cancelling write...';
end;

procedure TFormFloppyWriter.ButtonBrowseTargetClick(Sender: TObject);
begin
  SaveDialogImage.Title := 'Save Dumped Floppy Disk Image As...';
  SaveDialogImage.Filter := 'Atari ST Disk Image (*.st)|*.st|All Files (*.*)|*.*';
  SaveDialogImage.DefaultExt := 'st';
  if SaveDialogImage.Execute then
  begin
    EditTargetImage.Text := SaveDialogImage.FileName;
  end;
end;

procedure TFormFloppyWriter.ButtonReadClick(Sender: TObject);
var
  TargetDrive, OutPath, ErrMsg: string;
  Retries: Integer;
begin
  TargetDrive := ComboBoxReadDrive.Text;
  if TargetDrive = '' then
  begin
    ShowMessage('Please select a source floppy drive.');
    Exit;
  end;

  OutPath := EditTargetImage.Text;
  if OutPath = '' then
  begin
    ButtonBrowseTargetClick(nil);
    OutPath := EditTargetImage.Text;
    if OutPath = '' then Exit;
  end;

  if CheckBoxRetryBadSectors.Checked then
    Retries := 3
  else
    Retries := 0;

  FAbortOperation := False;
  FOperationInProgress := True;
  SetUIEnabled(False);
  ProgressBarRead.Position := 0;
  LabelReadStatus.Caption := 'Starting floppy disk read...';
  Screen.Cursor := crHourGlass;
  try
    if ReadFloppyToAtariImage(TargetDrive, OutPath, Retries,
         @OnReadProgressUpdate, FAbortOperation, ErrMsg) then
    begin
      ProgressBarRead.Position := 100;
      LabelReadStatus.Font.Color := clGreen;
      LabelReadStatus.Caption := 'Floppy disk dump completed successfully!';

      if MessageDlg('Floppy Read Complete',
           Format('Successfully dumped floppy disk from %s to:' + sLineBreak + '%s' + sLineBreak + sLineBreak +
                  'Would you like to open this image in STIMAGE now?', [TargetDrive, OutPath]),
           mtInformation, [mbYes, mbNo], 0) = mrYes then
      begin
        Form1.LoadDiskImage(OutPath);
        Close;
      end;
    end
    else
    begin
      LabelReadStatus.Font.Color := clRed;
      LabelReadStatus.Caption := 'Read failed: ' + ErrMsg;
      MessageDlg('Floppy Read Failed', ErrMsg, mtError, [mbOK], 0);
    end;
  finally
    FOperationInProgress := False;
    SetUIEnabled(True);
    Screen.Cursor := crDefault;
    InspectTargetDrive(ComboBoxReadDrive.Text, LabelReadMediaStatus);
  end;
end;

procedure TFormFloppyWriter.ButtonCancelReadClick(Sender: TObject);
begin
  FAbortOperation := True;
  LabelReadStatus.Caption := 'Cancelling read...';
end;

procedure TFormFloppyWriter.ButtonCloseClick(Sender: TObject);
begin
  if FOperationInProgress then
  begin
    if MessageDlg('Operation in Progress', 'A floppy disk transfer is currently running. Do you want to cancel and exit?',
         mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      FAbortOperation := True;
      Close;
    end;
  end
  else
    Close;
end;

procedure TFormFloppyWriter.CheckBoxRepack720Change(Sender: TObject);
var
  ImageSize: Integer;
begin
  if FOperationInProgress then Exit;
  ImageSize := Length(FSourceData);

  if not FSourceGeometryValid then
  begin
    if CheckBoxRepack720.Checked then
    begin
      if FCanRepack720 then
      begin
        ButtonWrite.Enabled := True;
        LabelCompatibilityWarning.Caption := Format('💡 "Repack files into standard 720 KB image" is checked: %d file(s) and %d folder(s) (%s) will be transferred using a temporary 720 KB image.',
          [FRepackFileCount, FRepackDirCount, Form1.FormatSizeStr(FRepackTotalBytes, False)]);
        LabelCompatibilityWarning.Font.Color := clGreen;
      end
      else
      begin
        ButtonWrite.Enabled := False;
        LabelCompatibilityWarning.Caption := '❌ Cannot repack: Files exceed 720 KB floppy capacity.';
        LabelCompatibilityWarning.Font.Color := clRed;
      end;
    end
    else
    begin
      ButtonWrite.Enabled := False;
      LabelCompatibilityWarning.Caption := '❌ Non-standard image size. Check "Repack files into standard 720 KB image" to transfer to a 720 KB floppy.';
      LabelCompatibilityWarning.Font.Color := clRed;
    end;
  end
  else if ImageSize = 1474560 then
  begin
    if CheckBoxRepack720.Checked then
    begin
      if FCanRepack720 then
      begin
        LabelCompatibilityWarning.Caption := Format('💡 1.44 MB image will be repacked into a temporary 720 KB image (%d files, %d folders, %s) and written to a Double-Density floppy.',
          [FRepackFileCount, FRepackDirCount, Form1.FormatSizeStr(FRepackTotalBytes, False)]);
        LabelCompatibilityWarning.Font.Color := clGreen;
      end
      else
      begin
        ShowMessage('The files in this 1.44 MB image exceed 720 KB floppy capacity and cannot be repacked to 720 KB.');
        CheckBoxRepack720.Checked := False;
      end;
    end
    else
    begin
      LabelCompatibilityWarning.Caption := '✅ Fully compatible with High-Density 1.44 MB USB and internal floppy drives.';
      LabelCompatibilityWarning.Font.Color := clGreen;
    end;
    ButtonWrite.Enabled := True;
  end
  else if ImageSize = 368640 then
  begin
    if CheckBoxRepack720.Checked then
    begin
      LabelCompatibilityWarning.Caption := Format('💡 360 KB image will be repacked into a temporary standard 720 KB Double-Density image (%d files, %d folders, %s) before writing to floppy.',
        [FRepackFileCount, FRepackDirCount, Form1.FormatSizeStr(FRepackTotalBytes, False)]);
      LabelCompatibilityWarning.Font.Color := clGreen;
    end
    else
    begin
      LabelCompatibilityWarning.Caption := 'ℹ️ Single-sided 360 KB image will be mapped onto Side 0 of a Double-Density diskette (supported by Atari TOS).';
      LabelCompatibilityWarning.Font.Color := $00804000;
    end;
    ButtonWrite.Enabled := True;
  end
  else if ImageSize = 737280 then
  begin
    if CheckBoxRepack720.Checked then
    begin
      LabelCompatibilityWarning.Caption := Format('💡 Files (%d files, %d folders, %s) will be repacked into a pristine 720 KB image before writing.',
        [FRepackFileCount, FRepackDirCount, Form1.FormatSizeStr(FRepackTotalBytes, False)]);
      LabelCompatibilityWarning.Font.Color := clGreen;
    end
    else
    begin
      LabelCompatibilityWarning.Caption := '✅ Fully compatible with standard 3.5" USB and PC internal floppy drives.';
      LabelCompatibilityWarning.Font.Color := clGreen;
    end;
    ButtonWrite.Enabled := True;
  end
  else
  begin
    ButtonWrite.Enabled := FSourceGeometryValid or (CheckBoxRepack720.Checked and FCanRepack720);
  end;
end;

procedure TFormFloppyWriter.OpenForWriting(const AImagePath: string);
begin
  PageControlFloppy.ActivePageIndex := 0;
  if AImagePath <> '' then
  begin
    EditSourceImage.Text := AImagePath;
    FCurrentSourcePath := AImagePath;
    LoadAndAnalyzeImage(AImagePath);
  end;
  ShowModal;
end;

procedure TFormFloppyWriter.OpenForReading;
begin
  PageControlFloppy.ActivePageIndex := 1;
  ShowModal;
end;

end.
