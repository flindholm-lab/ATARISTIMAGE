unit unit_clustermap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Grids, Math;

const
  FAT12_BOOT_SECTOR_SIZE = 512;
  FAT12_FREE             = $000;
  FAT12_BAD              = $FF7;
  FAT12_EOC              = $FFF;

type
  { TFormClusterMap }
  TFormClusterMap = class(TForm)
    PageControlMain: TPageControl;
    TabSheetClusterMap: TTabSheet;
    TabSheetSectorEditor: TTabSheet;

    // Cluster Map components
    PanelClusterHeader: TPanel;
    LabelUtilizedTitle: TLabel;
    ProgressBarCluster: TProgressBar;
    LabelUsedCount: TLabel;
    LabelFreeCount: TLabel;

    PanelInspector: TPanel;
    LabelInspTitle: TLabel;
    LabelInspCluster: TLabel;
    LabelInspStatus: TLabel;
    LabelInspFAT: TLabel;
    LabelInspFile: TLabel;
    LabelInspSectors: TLabel;
    LabelInspOffset: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    ButtonViewInHex: TButton;
    ButtonSetFree: TButton;
    ButtonSetEOC: TButton;
    ButtonSetBad: TButton;
    ButtonSetCustomFAT: TButton;
    ButtonRefreshMap: TButton;
    ButtonClose1: TButton;

    PanelMapContainer: TPanel;
    PanelMapTitle: TPanel;
    LabelMapTitle: TLabel;
    LabelHoverInfo: TLabel;
    PanelLegend: TPanel;
    LabelLegend: TLabel;
    ScrollBoxMap: TScrollBox;
    PaintBoxClusters: TPaintBox;

    // Sector Hex Editor components
    PanelSectorNav: TPanel;
    LabelSectorPrompt: TLabel;
    EditSectorNum: TEdit;
    LabelMaxSector: TLabel;
    ButtonPrevSector: TButton;
    ButtonNextSector: TButton;
    ButtonBoot: TButton;
    ButtonFAT1: TButton;
    ButtonFAT2: TButton;
    ButtonRootDir: TButton;
    ButtonReadOnlyToggle: TButton;
    ButtonSaveSector: TButton;
    GridHex: TStringGrid;

    PanelEditBar: TPanel;
    LabelSelectedByte: TLabel;
    EditHexByte: TEdit;
    ButtonApplyByte: TButton;
    LabelFillPrompt: TLabel;
    EditFillByte: TEdit;
    ButtonFillSector: TButton;

    PanelSectorFooter: TPanel;
    LabelDiskOffset: TLabel;
    LabelSectorRegion: TLabel;
    LabelEditStatus: TLabel;

    // Event Handlers
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PageControlMainChange(Sender: TObject);

    // Cluster map events
    procedure PaintBoxClustersPaint(Sender: TObject);
    procedure PaintBoxClustersMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxClustersMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxClustersDblClick(Sender: TObject);
    procedure ButtonViewInHexClick(Sender: TObject);
    procedure ButtonSetFreeClick(Sender: TObject);
    procedure ButtonSetEOCClick(Sender: TObject);
    procedure ButtonSetBadClick(Sender: TObject);
    procedure ButtonSetCustomFATClick(Sender: TObject);
    procedure ButtonRefreshMapClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);

    // Sector hex editor events
    procedure EditSectorNumKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonPrevSectorClick(Sender: TObject);
    procedure ButtonNextSectorClick(Sender: TObject);
    procedure ButtonBootClick(Sender: TObject);
    procedure ButtonFAT1Click(Sender: TObject);
    procedure ButtonFAT2Click(Sender: TObject);
    procedure ButtonRootDirClick(Sender: TObject);
    procedure ButtonReadOnlyToggleClick(Sender: TObject);
    procedure ButtonSaveSectorClick(Sender: TObject);
    procedure GridHexSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure GridHexSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure EditHexByteKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonApplyByteClick(Sender: TObject);
    procedure ButtonFillSectorClick(Sender: TObject);

  private
    FImagePath: string;
    FBytesPerSector: Word;
    FSectorsPerCluster: Byte;
    FReservedSectors: Word;
    FNumberOfFATs: Byte;
    FSectorsPerFAT: Word;
    FRootEntries: Word;
    FTotalSectors: Word;
    FFatTableStart: Int64;
    FSingleFATSize: Int64;
    FRootDirStart: Int64;
    FRootDirSectors: Int64;
    FDataAreaStart: Int64;
    FBytesPerCluster: Int64;
    FTotalClusters: Int64;

    // Cluster grid layout
    FColCount: Integer;
    FRowCount: Integer;
    FCellSize: Integer;
    FCellStride: Integer;
    FSelectedCluster: Word;
    FHoveredCluster: Word;

    // In-memory data
    FFATEntries: array of Word;
    FClusterOwner: array of string;
    FSectorBuffer: array[0..511] of Byte;
    FCurrentSector: Int64;
    FSectorModified: Boolean;
    FReadOnly: Boolean;
    FSelectedByteOffset: Integer;
    FHighlightedClusters: array of Boolean;
    FVisitedDirs: array of Word;

    procedure InitHexGrid;
    procedure ReadFATDetails(const AImageFile: TFileStream);
    function ReadFAT12Entry(const AImageFile: TFileStream; Cluster: Word): Word;
    procedure WriteFAT12EntryAllCopies(const AImageFile: TFileStream; Cluster: Word; Value: Word);
    procedure ScanDirectoryForClusterOwners(const AImageFile: TFileStream; DirCluster: Int64; const CurrentPath: string);
    procedure UpdateInspector(ACluster: Word);
    procedure LoadSector(ASectorNum: Int64);
    procedure DisplaySectorData;
    procedure SaveSector;
    function GetSectorRegionDescription(ASectorNum: Int64): string;

  public
    procedure LoadImage(const AImagePath: string);
    procedure JumpToCluster(const ACluster: Word);
    procedure JumpToSector(const ASector: Int64);
    procedure SetHighlightedClusters(const AClusters: array of Word);
    procedure ClearHighlight;
  end;

var
  FormClusterMap: TFormClusterMap;

implementation

{$R *.lfm}

{ TFormClusterMap }

procedure TFormClusterMap.FormCreate(Sender: TObject);
begin
  FImagePath := '';
  FBytesPerSector := 512;
  FSectorsPerCluster := 2;
  FReservedSectors := 1;
  FNumberOfFATs := 2;
  FSectorsPerFAT := 5;
  FRootEntries := 112;
  FTotalSectors := 1440;
  FTotalClusters := 713;

  FColCount := 28;
  FRowCount := 26;
  FCellSize := 15;
  FCellStride := 18;
  FSelectedCluster := 2;
  FHoveredCluster := 0;

  FCurrentSector := 0;
  FSectorModified := False;
  FReadOnly := True;
  FSelectedByteOffset := 0;

  InitHexGrid;
end;

procedure TFormClusterMap.FormDestroy(Sender: TObject);
begin
  SetLength(FFATEntries, 0);
  SetLength(FClusterOwner, 0);
end;

procedure TFormClusterMap.FormShow(Sender: TObject);
begin
  if FImagePath <> '' then
    LoadImage(FImagePath);
end;

procedure TFormClusterMap.PageControlMainChange(Sender: TObject);
begin
  if PageControlMain.ActivePage = TabSheetSectorEditor then
  begin
    LoadSector(FCurrentSector);
  end
  else if PageControlMain.ActivePage = TabSheetClusterMap then
  begin
    PaintBoxClusters.Invalidate;
    UpdateInspector(FSelectedCluster);
  end;
end;

procedure TFormClusterMap.InitHexGrid;
var
  I: Integer;
begin
  GridHex.ColCount := 18;
  GridHex.RowCount := 33;
  GridHex.FixedCols := 1;
  GridHex.FixedRows := 1;
  GridHex.Cells[0, 0] := 'OFFSET';
  GridHex.ColWidths[0] := 55;
  for I := 0 to 15 do
  begin
    GridHex.Cells[I + 1, 0] := Format('%.2X', [I]);
    GridHex.ColWidths[I + 1] := 24;
  end;
  GridHex.Cells[17, 0] := 'ASCII';
  GridHex.ColWidths[17] := 145;
  for I := 0 to 31 do
    GridHex.Cells[0, I + 1] := Format('%.3X:', [I * 16]);
end;

procedure TFormClusterMap.ReadFATDetails(const AImageFile: TFileStream);
var
  BootSector: array[0..FAT12_BOOT_SECTOR_SIZE - 1] of Byte;
  DataSectors: Int64;
  MaxClustersInFile, MaxClustersInFAT: Int64;
begin
  AImageFile.Position := 0;
  AImageFile.Read(BootSector, FAT12_BOOT_SECTOR_SIZE);

  FBytesPerSector := Word(BootSector[$0B]) + (Word(BootSector[$0C]) shl 8);
  if FBytesPerSector <> 512 then FBytesPerSector := 512;
  FSectorsPerCluster := BootSector[$0D];
  if FSectorsPerCluster = 0 then FSectorsPerCluster := 2;
  FReservedSectors := Word(BootSector[$0E]) + (Word(BootSector[$0F]) shl 8);
  if FReservedSectors = 0 then FReservedSectors := 1;
  FNumberOfFATs := BootSector[$10];
  if FNumberOfFATs = 0 then FNumberOfFATs := 2;
  FRootEntries := Word(BootSector[$11]) + (Word(BootSector[$12]) shl 8);
  if FRootEntries = 0 then FRootEntries := 112;
  FTotalSectors := Word(BootSector[$13]) + (Word(BootSector[$14]) shl 8);
  if FTotalSectors = 0 then
    FTotalSectors := AImageFile.Size div FBytesPerSector;
  FSectorsPerFAT := Word(BootSector[$16]) + (Word(BootSector[$17]) shl 8);
  if FSectorsPerFAT = 0 then FSectorsPerFAT := 5;

  FSingleFATSize := FSectorsPerFAT * FBytesPerSector;
  FFatTableStart := FReservedSectors * FBytesPerSector;
  FRootDirStart := (FReservedSectors + (FNumberOfFATs * FSectorsPerFAT)) * FBytesPerSector;
  FRootDirSectors := ((FRootEntries * 32) + (FBytesPerSector - 1)) div FBytesPerSector;
  FDataAreaStart := FRootDirStart + (FRootDirSectors * FBytesPerSector);
  FBytesPerCluster := FSectorsPerCluster * FBytesPerSector;

  DataSectors := FTotalSectors - (FReservedSectors + (FNumberOfFATs * FSectorsPerFAT) + FRootDirSectors);
  if (DataSectors > 0) and (FSectorsPerCluster > 0) then
    FTotalClusters := DataSectors div FSectorsPerCluster
  else
    FTotalClusters := 0;

  if (FBytesPerCluster > 0) and (AImageFile.Size > FDataAreaStart) then
  begin
    MaxClustersInFile := (AImageFile.Size - FDataAreaStart) div FBytesPerCluster;
    if (MaxClustersInFile > 0) and (MaxClustersInFile < FTotalClusters) then
      FTotalClusters := MaxClustersInFile;
  end;

  if FSingleFATSize > 3 then
  begin
    MaxClustersInFAT := (FSingleFATSize * 2 div 3) - 2;
    if (MaxClustersInFAT > 0) and (MaxClustersInFAT < FTotalClusters) then
      FTotalClusters := MaxClustersInFAT;
  end;
end;

function TFormClusterMap.ReadFAT12Entry(const AImageFile: TFileStream; Cluster: Word): Word;
var
  fatPos: Int64;
  b1, b2: Byte;
  val: Word;
begin
  fatPos := FFatTableStart + (Int64(Cluster) * 3) div 2;
  AImageFile.Position := fatPos;
  AImageFile.Read(b1, 1);
  AImageFile.Read(b2, 1);
  val := Word(b1) or (Word(b2) shl 8);
  if (Cluster and 1) = 0 then
    Result := val and $0FFF
  else
    Result := (val shr 4) and $0FFF;
end;

procedure TFormClusterMap.WriteFAT12EntryAllCopies(const AImageFile: TFileStream; Cluster: Word; Value: Word);
var
  fatIdx: Integer;
  posFAT, base: Int64;
  b1, b2: Byte;
  val: Word;
begin
  Value := Value and $0FFF;
  for fatIdx := 0 to FNumberOfFATs - 1 do
  begin
    base := FFatTableStart + fatIdx * FSingleFATSize;
    posFAT := base + (Int64(Cluster) * 3) div 2;
    AImageFile.Position := posFAT;
    AImageFile.Read(b1, 1);
    AImageFile.Read(b2, 1);
    val := Word(b1) or (Word(b2) shl 8);
    if (Cluster and 1) = 0 then
      val := (val and $F000) or (Value and $0FFF)
    else
      val := (val and $000F) or ((Value and $0FFF) shl 4);
    b1 := Byte(val and $FF);
    b2 := Byte((val shr 8) and $FF);
    AImageFile.Position := posFAT;
    AImageFile.Write(b1, 1);
    AImageFile.Write(b2, 1);
  end;
end;

procedure TFormClusterMap.ScanDirectoryForClusterOwners(const AImageFile: TFileStream; DirCluster: Int64; const CurrentPath: string);
var
  EntriesToRead, I, V: Integer;
  DirEntry: array[0..31] of Byte;
  DirOffset, EntryPos: Int64;
  NameStr, ExtStr, FullName: string;
  Attr: Byte;
  StartCluster, CurrCluster: Word;
  NextClust: Int64;
  ChainIndex, DirChainIndex: Integer;
begin
  if DirCluster <> 0 then
  begin
    for V := 0 to High(FVisitedDirs) do
      if FVisitedDirs[V] = DirCluster then Exit;
    SetLength(FVisitedDirs, Length(FVisitedDirs) + 1);
    FVisitedDirs[High(FVisitedDirs)] := DirCluster;
  end;

  if DirCluster = 0 then
  begin
    EntriesToRead := FRootEntries;
    for I := 0 to EntriesToRead - 1 do
    begin
      EntryPos := FRootDirStart + I * 32;
      if EntryPos + 32 > AImageFile.Size then Break;
      AImageFile.Position := EntryPos;
      AImageFile.Read(DirEntry, 32);
      if DirEntry[0] = 0 then Break;
      if (DirEntry[0] = $E5) or ((DirEntry[11] and $08) <> 0) then Continue;
      Attr := DirEntry[11];
      NameStr := Trim(Copy(PAnsiChar(@DirEntry[0]), 1, 8));
      ExtStr := Trim(Copy(PAnsiChar(@DirEntry[8]), 1, 3));
      if ExtStr <> '' then FullName := NameStr + '.' + ExtStr else FullName := NameStr;
      Move(DirEntry[26], StartCluster, 2);

      if (Attr and $10 <> 0) then
      begin
        if (NameStr <> '.') and (NameStr <> '..') and (StartCluster >= 2) then
          ScanDirectoryForClusterOwners(AImageFile, StartCluster, CurrentPath + FullName + '\');
      end
      else
      begin
        CurrCluster := StartCluster;
        ChainIndex := 1;
        while (CurrCluster >= 2) and (CurrCluster <= FTotalClusters + 1) do
        begin
          if (CurrCluster < Length(FClusterOwner)) and (FClusterOwner[CurrCluster] = '') then
            FClusterOwner[CurrCluster] := CurrentPath + FullName + Format(' (#%d)', [ChainIndex]);
          if CurrCluster < Length(FFATEntries) then
            CurrCluster := FFATEntries[CurrCluster]
          else
            Break;
          Inc(ChainIndex);
          if (ChainIndex > FTotalClusters + 2) or (CurrCluster >= $FF8) then Break;
        end;
      end;
    end;
  end
  else
  begin
    CurrCluster := DirCluster;
    DirChainIndex := 0;
    while (CurrCluster >= 2) and (CurrCluster <= FTotalClusters + 1) do
    begin
      Inc(DirChainIndex);
      if DirChainIndex > FTotalClusters + 2 then Break;

      DirOffset := FDataAreaStart + (Int64(CurrCluster) - 2) * FBytesPerCluster;
      if DirOffset + FBytesPerCluster > AImageFile.Size then Break;
      EntriesToRead := FBytesPerCluster div 32;
      for I := 0 to EntriesToRead - 1 do
      begin
        EntryPos := DirOffset + I * 32;
        AImageFile.Position := EntryPos;
        AImageFile.Read(DirEntry, 32);
        if DirEntry[0] = 0 then Exit;
        if (DirEntry[0] = $E5) or ((DirEntry[11] and $08) <> 0) then Continue;
        Attr := DirEntry[11];
        NameStr := Trim(Copy(PAnsiChar(@DirEntry[0]), 1, 8));
        ExtStr := Trim(Copy(PAnsiChar(@DirEntry[8]), 1, 3));
        if ExtStr <> '' then FullName := NameStr + '.' + ExtStr else FullName := NameStr;
        Move(DirEntry[26], StartCluster, 2);

        if (Attr and $10 <> 0) then
        begin
          if (NameStr <> '.') and (NameStr <> '..') and (StartCluster >= 2) then
            ScanDirectoryForClusterOwners(AImageFile, StartCluster, CurrentPath + FullName + '\');
        end
        else
        begin
          NextClust := StartCluster;
          ChainIndex := 1;
          while (NextClust >= 2) and (NextClust <= FTotalClusters + 1) do
          begin
            if (NextClust < Length(FClusterOwner)) and (FClusterOwner[NextClust] = '') then
              FClusterOwner[NextClust] := CurrentPath + FullName + Format(' (#%d)', [ChainIndex]);
            if NextClust < Length(FFATEntries) then
              NextClust := FFATEntries[NextClust]
            else
              Break;
            Inc(ChainIndex);
            if (ChainIndex > FTotalClusters + 2) or (NextClust >= $FF8) then Break;
          end;
        end;
      end;
      if CurrCluster < Length(FFATEntries) then
        CurrCluster := FFATEntries[CurrCluster]
      else
        Break;
      if CurrCluster >= $FF8 then Break;
    end;
  end;
end;

procedure TFormClusterMap.LoadImage(const AImagePath: string);
var
  ImageFile: TFileStream;
  I: Integer;
  UsedClusters, FreeClusters: Integer;
  UtilPercent: Double;
begin
  FImagePath := AImagePath;
  SetLength(FVisitedDirs, 0);
  if not FileExists(FImagePath) then Exit;

  ImageFile := TFileStream.Create(FImagePath, fmOpenRead or fmShareDenyNone);
  try
    ReadFATDetails(ImageFile);

    SetLength(FFATEntries, FTotalClusters + 2);
    SetLength(FClusterOwner, FTotalClusters + 2);
    for I := 0 to High(FClusterOwner) do
      FClusterOwner[I] := '';

    UsedClusters := 0;
    FreeClusters := 0;

    for I := 0 to FTotalClusters - 1 do
    begin
      FFATEntries[2 + I] := ReadFAT12Entry(ImageFile, 2 + I);
      if FFATEntries[2 + I] = FAT12_FREE then
        Inc(FreeClusters)
      else
        Inc(UsedClusters);
    end;

    // Scan directory tree for file names mapped to clusters
    ScanDirectoryForClusterOwners(ImageFile, 0, '');

    // Update Header summary
    if FTotalClusters > 0 then
      UtilPercent := (Double(UsedClusters) / Double(FTotalClusters)) * 100.0
    else
      UtilPercent := 0.0;

    LabelUtilizedTitle.Caption := Format('Floppy Space Utilized: %.1f%%', [UtilPercent]);
    ProgressBarCluster.Max := FTotalClusters;
    ProgressBarCluster.Position := UsedClusters;
    LabelUsedCount.Caption := Format('%d / %d used', [UsedClusters, FTotalClusters]);
    LabelFreeCount.Caption := Format('%d free', [FreeClusters]);

    // Grid sizing
    FColCount := 28;
    FRowCount := (FTotalClusters + FColCount - 1) div FColCount;
    PaintBoxClusters.Width := FColCount * FCellStride + 10;
    PaintBoxClusters.Height := FRowCount * FCellStride + 10;

    if FSelectedCluster < 2 then FSelectedCluster := 2;
    if FSelectedCluster > FTotalClusters + 1 then FSelectedCluster := 2;
    UpdateInspector(FSelectedCluster);
  finally
    ImageFile.Free;
  end;

  PaintBoxClusters.Invalidate;
  LoadSector(FCurrentSector);
end;

procedure TFormClusterMap.UpdateInspector(ACluster: Word);
var
  FATVal: Word;
  StatusStr, FATStr, OwnerStr: string;
  StartSec, EndSec, PhysOffset: Int64;
begin
  if (ACluster < 2) or (ACluster > FTotalClusters + 1) then
  begin
    LabelInspCluster.Caption := 'Cluster: None';
    LabelInspStatus.Caption := 'Status: --';
    LabelInspFAT.Caption := 'FAT Value: --';
    LabelInspFile.Caption := 'File: --';
    LabelInspSectors.Caption := 'Sectors: --';
    LabelInspOffset.Caption := 'Offset: --';
    Exit;
  end;

  if ACluster < Length(FFATEntries) then
    FATVal := FFATEntries[ACluster]
  else
    FATVal := 0;

  if FATVal = FAT12_FREE then
  begin
    StatusStr := 'Free';
    FATStr := '$000 (Free)';
  end
  else if FATVal = FAT12_BAD then
  begin
    StatusStr := 'Bad Cluster';
    FATStr := '$FF7 (Bad)';
  end
  else if FATVal >= $FF8 then
  begin
    StatusStr := 'Allocated (End of File)';
    FATStr := Format('$%.3X (End of Chain)', [FATVal]);
  end
  else
  begin
    StatusStr := Format('Allocated -> Next: %d', [FATVal]);
    FATStr := Format('$%.3X (Points to #%d)', [FATVal, FATVal]);
  end;

  if (ACluster < Length(FClusterOwner)) and (FClusterOwner[ACluster] <> '') then
    OwnerStr := FClusterOwner[ACluster]
  else
    OwnerStr := '<None / Unreferenced>';

  StartSec := (FDataAreaStart div FBytesPerSector) + (Int64(ACluster) - 2) * FSectorsPerCluster;
  EndSec := StartSec + FSectorsPerCluster - 1;
  PhysOffset := StartSec * FBytesPerSector;

  LabelInspCluster.Caption := Format('Cluster: #%d', [ACluster]);
  LabelInspStatus.Caption := 'Status: ' + StatusStr;
  LabelInspFAT.Caption := 'FAT Value: ' + FATStr;
  LabelInspFile.Caption := 'File: ' + OwnerStr;
  LabelInspSectors.Caption := Format('Sectors: %d - %d', [StartSec, EndSec]);
  LabelInspOffset.Caption := Format('Offset: %s bytes', [FormatFloat('#,##0', PhysOffset)]);
end;

procedure TFormClusterMap.PaintBoxClustersPaint(Sender: TObject);
var
  I, Col, Row, X, Y: Integer;
  ClusterNum: Word;
  FATVal: Word;
  R: TRect;
begin
  with PaintBoxClusters.Canvas do
  begin
    Brush.Color := clBtnFace;
    FillRect(0, 0, PaintBoxClusters.Width, PaintBoxClusters.Height);

    if FTotalClusters <= 0 then Exit;

    for I := 0 to FTotalClusters - 1 do
    begin
      ClusterNum := 2 + I;
      Col := I mod FColCount;
      Row := I div FColCount;
      X := Col * FCellStride + 2;
      Y := Row * FCellStride + 2;
      R := Rect(X, Y, X + FCellSize, Y + FCellSize);

      if ClusterNum < Length(FFATEntries) then
        FATVal := FFATEntries[ClusterNum]
      else
        FATVal := 0;

      if (ClusterNum < Length(FHighlightedClusters)) and FHighlightedClusters[ClusterNum] then
      begin
        Brush.Color := $00FFFF; // Bright Yellow (Selected File)
        Pen.Color := clBlack;
        Pen.Width := 1;
      end
      else if FATVal = FAT12_FREE then
      begin
        Brush.Color := clWhite;
        Pen.Color := clBlack;
        Pen.Width := 1;
      end
      else if FATVal = FAT12_BAD then
      begin
        Brush.Color := $0000D0; // Red
        Pen.Color := clBlack;
        Pen.Width := 1;
      end
      else if FATVal >= $FF8 then
      begin
        Brush.Color := $1080E0; // Amber / Orange
        Pen.Color := clBlack;
        Pen.Width := 1;
      end
      else
      begin
        Brush.Color := $D06020; // Steel Blue
        Pen.Color := clBlack;
        Pen.Width := 1;
      end;

      Rectangle(R);

      // Highlight selected, hovered, or file-highlighted cluster border
      if (ClusterNum < Length(FHighlightedClusters)) and FHighlightedClusters[ClusterNum] then
      begin
        Pen.Color := $0080FF; // High contrast orange/gold border for file clusters
        Pen.Width := 2;
        Brush.Style := bsClear;
        Rectangle(X - 1, Y - 1, X + FCellSize + 1, Y + FCellSize + 1);
        Brush.Style := bsSolid;
      end;

      if ClusterNum = FSelectedCluster then
      begin
        Pen.Color := clRed;
        Pen.Width := 2;
        Brush.Style := bsClear;
        Rectangle(X - 1, Y - 1, X + FCellSize + 1, Y + FCellSize + 1);
        Brush.Style := bsSolid;
      end
      else if ClusterNum = FHoveredCluster then
      begin
        Pen.Color := clLime;
        Pen.Width := 2;
        Brush.Style := bsClear;
        Rectangle(X - 1, Y - 1, X + FCellSize + 1, Y + FCellSize + 1);
        Brush.Style := bsSolid;
      end;
    end;
  end;
end;

procedure TFormClusterMap.PaintBoxClustersMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Col, Row, Index: Integer;
  NewHover: Word;
  FATVal: Word;
  Stat: string;
begin
  Col := (X - 2) div FCellStride;
  Row := (Y - 2) div FCellStride;
  if (Col >= 0) and (Col < FColCount) and (Row >= 0) and (Row < FRowCount) then
  begin
    Index := Row * FColCount + Col;
    if Index < FTotalClusters then
    begin
      NewHover := 2 + Index;
      if NewHover <> FHoveredCluster then
      begin
        FHoveredCluster := NewHover;
        if FHoveredCluster < Length(FFATEntries) then
          FATVal := FFATEntries[FHoveredCluster]
        else
          FATVal := 0;

        if FATVal = 0 then Stat := 'Free'
        else if FATVal = $FF7 then Stat := 'Bad'
        else if FATVal >= $FF8 then Stat := 'End of File'
        else Stat := Format('Next -> %d', [FATVal]);

        LabelHoverInfo.Caption := Format('Cluster #%d | %s | %s',
          [FHoveredCluster, Stat, FClusterOwner[FHoveredCluster]]);
        PaintBoxClusters.Invalidate;
      end;
      Exit;
    end;
  end;

  if FHoveredCluster <> 0 then
  begin
    FHoveredCluster := 0;
    LabelHoverInfo.Caption := 'Hover cell';
    PaintBoxClusters.Invalidate;
  end;
end;

procedure TFormClusterMap.PaintBoxClustersMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Col, Row, Index: Integer;
begin
  Col := (X - 2) div FCellStride;
  Row := (Y - 2) div FCellStride;
  if (Col >= 0) and (Col < FColCount) and (Row >= 0) and (Row < FRowCount) then
  begin
    Index := Row * FColCount + Col;
    if Index < FTotalClusters then
    begin
      FSelectedCluster := 2 + Index;
      UpdateInspector(FSelectedCluster);
      PaintBoxClusters.Invalidate;
    end;
  end;
end;

procedure TFormClusterMap.PaintBoxClustersDblClick(Sender: TObject);
begin
  ButtonViewInHexClick(Sender);
end;

procedure TFormClusterMap.ButtonViewInHexClick(Sender: TObject);
var
  StartSector: Int64;
begin
  if (FSelectedCluster < 2) or (FSelectedCluster > FTotalClusters + 1) then Exit;
  StartSector := (FDataAreaStart div FBytesPerSector) + (Int64(FSelectedCluster) - 2) * FSectorsPerCluster;
  JumpToSector(StartSector);
  PageControlMain.ActivePage := TabSheetSectorEditor;
end;

procedure TFormClusterMap.ButtonSetFreeClick(Sender: TObject);
var
  ImageFile: TFileStream;
begin
  if (FSelectedCluster < 2) or (FSelectedCluster > FTotalClusters + 1) then Exit;
  if MessageDlg('Mark Cluster Free',
    Format('Are you sure you want to mark Cluster #%d as Free ($000)?' + sLineBreak +
           'This modifies the FAT table in the image.', [FSelectedCluster]),
    mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit;

  ImageFile := TFileStream.Create(FImagePath, fmOpenReadWrite or fmShareDenyNone);
  try
    WriteFAT12EntryAllCopies(ImageFile, FSelectedCluster, FAT12_FREE);
  finally
    ImageFile.Free;
  end;

  LoadImage(FImagePath);
end;

procedure TFormClusterMap.ButtonSetEOCClick(Sender: TObject);
var
  ImageFile: TFileStream;
begin
  if (FSelectedCluster < 2) or (FSelectedCluster > FTotalClusters + 1) then Exit;
  if MessageDlg('Mark End of File',
    Format('Are you sure you want to mark Cluster #%d as End of File ($FFF)?', [FSelectedCluster]),
    mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit;

  ImageFile := TFileStream.Create(FImagePath, fmOpenReadWrite or fmShareDenyNone);
  try
    WriteFAT12EntryAllCopies(ImageFile, FSelectedCluster, FAT12_EOC);
  finally
    ImageFile.Free;
  end;

  LoadImage(FImagePath);
end;

procedure TFormClusterMap.ButtonSetBadClick(Sender: TObject);
var
  ImageFile: TFileStream;
  NewVal: Word;
begin
  if (FSelectedCluster < 2) or (FSelectedCluster > FTotalClusters + 1) then Exit;

  if (FSelectedCluster < Length(FFATEntries)) and (FFATEntries[FSelectedCluster] = FAT12_BAD) then
    NewVal := FAT12_FREE
  else
    NewVal := FAT12_BAD;

  if NewVal = FAT12_BAD then
  begin
    if MessageDlg('Mark Cluster as Bad',
      Format('Are you sure you want to mark Cluster #%d as Bad ($FF7)?' + sLineBreak +
             'This writes $FF7 to both FAT tables in the image.', [FSelectedCluster]),
      mtWarning, [mbYes, mbNo], 0) <> mrYes then Exit;
  end
  else
  begin
    if MessageDlg('Unmark Bad Cluster',
      Format('Are you sure you want to restore Cluster #%d to Free ($000)?', [FSelectedCluster]),
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit;
  end;

  ImageFile := TFileStream.Create(FImagePath, fmOpenReadWrite or fmShareDenyNone);
  try
    WriteFAT12EntryAllCopies(ImageFile, FSelectedCluster, NewVal);
  finally
    ImageFile.Free;
  end;

  LoadImage(FImagePath);
end;

procedure TFormClusterMap.SetHighlightedClusters(const AClusters: array of Word);
var
  I: Integer;
begin
  SetLength(FHighlightedClusters, FTotalClusters + 3);
  FillChar(FHighlightedClusters[0], Length(FHighlightedClusters) * SizeOf(Boolean), 0);
  for I := 0 to High(AClusters) do
  begin
    if (AClusters[I] >= 2) and (AClusters[I] < Length(FHighlightedClusters)) then
      FHighlightedClusters[AClusters[I]] := True;
  end;
  if PaintBoxClusters <> nil then
    PaintBoxClusters.Invalidate;
end;

procedure TFormClusterMap.ClearHighlight;
begin
  SetLength(FHighlightedClusters, 0);
  if PaintBoxClusters <> nil then
    PaintBoxClusters.Invalidate;
end;

procedure TFormClusterMap.ButtonSetCustomFATClick(Sender: TObject);
var
  InputStr: string;
  ValInt: Integer;
  ImageFile: TFileStream;
begin
  if (FSelectedCluster < 2) or (FSelectedCluster > FTotalClusters + 1) then Exit;
  InputStr := InputBox('Set FAT Value',
    Format('Enter hex or decimal FAT value for Cluster #%d (e.g. 0, $FFF, or cluster #):', [FSelectedCluster]),
    Format('$%.3X', [FFATEntries[FSelectedCluster]]));
  InputStr := Trim(InputStr);
  if InputStr = '' then Exit;

  try
    if InputStr.StartsWith('$') or InputStr.StartsWith('0x') then
      ValInt := StrToInt(InputStr)
    else
      ValInt := StrToInt('$' + InputStr);
  except
    try
      ValInt := StrToInt(InputStr);
    except
      ShowMessage('Invalid number entered.');
      Exit;
    end;
  end;

  ValInt := ValInt and $0FFF;

  ImageFile := TFileStream.Create(FImagePath, fmOpenReadWrite or fmShareDenyNone);
  try
    WriteFAT12EntryAllCopies(ImageFile, FSelectedCluster, Word(ValInt));
  finally
    ImageFile.Free;
  end;

  LoadImage(FImagePath);
end;

procedure TFormClusterMap.ButtonRefreshMapClick(Sender: TObject);
begin
  LoadImage(FImagePath);
end;

procedure TFormClusterMap.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

{ Sector Hex Editor methods }

function TFormClusterMap.GetSectorRegionDescription(ASectorNum: Int64): string;
var
  ClustNum, SecInClust: Int64;
  OwnerName: string;
begin
  if ASectorNum = 0 then
    Result := 'Region: Boot Sector (Sector 0)'
  else if (ASectorNum >= 1) and (ASectorNum <= FSectorsPerFAT) then
    Result := Format('Region: FAT 1 (Sector %d of %d)', [ASectorNum, FSectorsPerFAT])
  else if (ASectorNum > FSectorsPerFAT) and (ASectorNum <= FSectorsPerFAT * 2) then
    Result := Format('Region: FAT 2 (Sector %d of %d)', [ASectorNum - FSectorsPerFAT, FSectorsPerFAT])
  else if (ASectorNum > FSectorsPerFAT * 2) and (ASectorNum < FDataAreaStart div FBytesPerSector) then
    Result := Format('Region: Root Directory (Sector %d of %d)',
      [ASectorNum - (FSectorsPerFAT * 2), FRootDirSectors])
  else
  begin
    ClustNum := 2 + (ASectorNum - (FDataAreaStart div FBytesPerSector)) div FSectorsPerCluster;
    SecInClust := (ASectorNum - (FDataAreaStart div FBytesPerSector)) mod FSectorsPerCluster;
    if (ClustNum < Length(FClusterOwner)) and (FClusterOwner[ClustNum] <> '') then
      OwnerName := ' | File: ' + FClusterOwner[ClustNum]
    else
      OwnerName := '';
    Result := Format('Region: Data Area -> Cluster %d (Sector %d of %d)%s',
      [ClustNum, SecInClust + 1, FSectorsPerCluster, OwnerName]);
  end;
end;

procedure TFormClusterMap.DisplaySectorData;
var
  R, C: Integer;
  B: Byte;
  AscStr: string;
begin
  GridHex.BeginUpdate;
  try
    for R := 0 to 31 do
    begin
      AscStr := '';
      for C := 0 to 15 do
      begin
        B := FSectorBuffer[R * 16 + C];
        GridHex.Cells[C + 1, R + 1] := Format('%.2X', [B]);
        if (B >= 32) and (B <= 126) then
          AscStr := AscStr + Char(B)
        else
          AscStr := AscStr + '.';
      end;
      GridHex.Cells[17, R + 1] := AscStr;
    end;
  finally
    GridHex.EndUpdate;
  end;
end;

procedure TFormClusterMap.LoadSector(ASectorNum: Int64);
var
  ImageFile: TFileStream;
  PhysOffset: Int64;
begin
  if FTotalSectors > 0 then
  begin
    if ASectorNum < 0 then ASectorNum := 0;
    if ASectorNum >= FTotalSectors then ASectorNum := FTotalSectors - 1;
  end;

  FCurrentSector := ASectorNum;
  PhysOffset := FCurrentSector * FBytesPerSector;

  if (FImagePath <> '') and FileExists(FImagePath) then
  begin
    ImageFile := TFileStream.Create(FImagePath, fmOpenRead or fmShareDenyNone);
    try
      if PhysOffset + FBytesPerSector <= ImageFile.Size then
      begin
        ImageFile.Position := PhysOffset;
        ImageFile.Read(FSectorBuffer, Min(SizeOf(FSectorBuffer), FBytesPerSector));
      end
      else
        FillChar(FSectorBuffer, 512, 0);
    finally
      ImageFile.Free;
    end;
  end
  else
    FillChar(FSectorBuffer, 512, 0);

  EditSectorNum.Text := IntToStr(FCurrentSector);
  LabelMaxSector.Caption := Format('/ %d', [FTotalSectors - 1]);
  LabelDiskOffset.Caption := Format('PHYSICAL DISK OFFSET: %s BYTES', [FormatFloat('#,##0', PhysOffset)]);
  LabelSectorRegion.Caption := GetSectorRegionDescription(FCurrentSector);

  FSectorModified := False;
  ButtonSaveSector.Enabled := False;

  DisplaySectorData;

  // Refresh byte editor info
  if (FSelectedByteOffset >= 0) and (FSelectedByteOffset < 512) then
  begin
    EditHexByte.Text := Format('%.2X', [FSectorBuffer[FSelectedByteOffset]]);
    LabelSelectedByte.Caption := Format('Byte at offset $%.3X (%d):', [FSelectedByteOffset, FSelectedByteOffset]);
  end;
end;

procedure TFormClusterMap.SaveSector;
var
  ImageFile: TFileStream;
  PhysOffset: Int64;
  Fat1Offset, Fat2Offset: Int64;
begin
  if FImagePath = '' then Exit;
  PhysOffset := FCurrentSector * FBytesPerSector;

  ImageFile := TFileStream.Create(FImagePath, fmOpenReadWrite or fmShareDenyNone);
  try
    ImageFile.Position := PhysOffset;
    ImageFile.Write(FSectorBuffer, Min(SizeOf(FSectorBuffer), FBytesPerSector));

    // If modifying FAT 1, mirror to FAT 2
    if (FCurrentSector >= 1) and (FCurrentSector <= FSectorsPerFAT) then
    begin
      Fat2Offset := PhysOffset + (FSectorsPerFAT * FBytesPerSector);
      ImageFile.Position := Fat2Offset;
      ImageFile.Write(FSectorBuffer, Min(SizeOf(FSectorBuffer), FBytesPerSector));
    end
    else if (FCurrentSector > FSectorsPerFAT) and (FCurrentSector <= FSectorsPerFAT * 2) then
    begin
      Fat1Offset := PhysOffset - (FSectorsPerFAT * FBytesPerSector);
      ImageFile.Position := Fat1Offset;
      ImageFile.Write(FSectorBuffer, Min(SizeOf(FSectorBuffer), FBytesPerSector));
    end;
  finally
    ImageFile.Free;
  end;

  FSectorModified := False;
  ButtonSaveSector.Enabled := False;
  LabelEditStatus.Caption := '✏️ SECTOR SAVED';
  ShowMessage(Format('Sector %d successfully saved to image!', [FCurrentSector]));

  // Reload FAT entries and cluster map if a FAT sector was updated
  if (FCurrentSector >= 1) and (FCurrentSector <= FSectorsPerFAT * 2) then
    LoadImage(FImagePath);
end;

procedure TFormClusterMap.EditSectorNumKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Sec: Int64;
begin
  if Key = 13 then // Enter
  begin
    try
      Sec := StrToInt64(Trim(EditSectorNum.Text));
      LoadSector(Sec);
    except
      ShowMessage('Invalid sector number.');
    end;
  end;
end;

procedure TFormClusterMap.ButtonPrevSectorClick(Sender: TObject);
begin
  if FCurrentSector > 0 then
    LoadSector(FCurrentSector - 1);
end;

procedure TFormClusterMap.ButtonNextSectorClick(Sender: TObject);
begin
  if FCurrentSector < FTotalSectors - 1 then
    LoadSector(FCurrentSector + 1);
end;

procedure TFormClusterMap.ButtonBootClick(Sender: TObject);
begin
  LoadSector(0);
end;

procedure TFormClusterMap.ButtonFAT1Click(Sender: TObject);
begin
  LoadSector(1);
end;

procedure TFormClusterMap.ButtonFAT2Click(Sender: TObject);
begin
  LoadSector(1 + FSectorsPerFAT);
end;

procedure TFormClusterMap.ButtonRootDirClick(Sender: TObject);
begin
  LoadSector(1 + FSectorsPerFAT * 2);
end;

procedure TFormClusterMap.ButtonReadOnlyToggleClick(Sender: TObject);
begin
  FReadOnly := not FReadOnly;
  if FReadOnly then
  begin
    ButtonReadOnlyToggle.Caption := '🔒 READ ONLY';
    LabelEditStatus.Caption := '🔒 READ ONLY MAP';
    PanelEditBar.Visible := False;
    ButtonSaveSector.Enabled := False;
    GridHex.Options := GridHex.Options - [goEditing];
  end
  else
  begin
    ButtonReadOnlyToggle.Caption := '✏️ EDIT MODE';
    LabelEditStatus.Caption := '✏️ EDIT MODE (Active)';
    PanelEditBar.Visible := True;
    ButtonSaveSector.Enabled := FSectorModified;
    GridHex.Options := GridHex.Options + [goEditing];
  end;
end;

procedure TFormClusterMap.ButtonSaveSectorClick(Sender: TObject);
begin
  SaveSector;
end;

procedure TFormClusterMap.GridHexSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
var
  ByteIdx: Integer;
begin
  CanSelect := True;
  if (aCol >= 1) and (aCol <= 16) and (aRow >= 1) and (aRow <= 32) then
  begin
    ByteIdx := (aRow - 1) * 16 + (aCol - 1);
    FSelectedByteOffset := ByteIdx;
    EditHexByte.Text := Format('%.2X', [FSectorBuffer[ByteIdx]]);
    LabelSelectedByte.Caption := Format('Byte at offset $%.3X (%d):', [ByteIdx, ByteIdx]);
  end;
end;

procedure TFormClusterMap.GridHexSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
var
  ByteIdx: Integer;
  ValInt: Integer;
  HexVal: string;
begin
  if FReadOnly then Exit;
  if (ACol >= 1) and (ACol <= 16) and (ARow >= 1) and (ARow <= 32) then
  begin
    ByteIdx := (ARow - 1) * 16 + (ACol - 1);
    HexVal := Trim(Value);
    if HexVal <> '' then
    begin
      try
        ValInt := StrToInt('$' + HexVal);
        if (ValInt >= 0) and (ValInt <= 255) then
        begin
          FSectorBuffer[ByteIdx] := Byte(ValInt);
          FSectorModified := True;
          ButtonSaveSector.Enabled := True;
          LabelEditStatus.Caption := '✏️ MODIFIED (Unsaved)';
          // Update ASCII column
          DisplaySectorData;
        end;
      except
        // Ignore parsing error while typing
      end;
    end;
  end;
end;

procedure TFormClusterMap.EditHexByteKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 13 then
    ButtonApplyByteClick(Sender);
end;

procedure TFormClusterMap.ButtonApplyByteClick(Sender: TObject);
var
  ValInt: Integer;
  Col, Row: Integer;
begin
  if (FSelectedByteOffset < 0) or (FSelectedByteOffset >= 512) then Exit;
  try
    ValInt := StrToInt('$' + Trim(EditHexByte.Text));
    if (ValInt < 0) or (ValInt > 255) then
    begin
      ShowMessage('Byte value must be between $00 and $FF.');
      Exit;
    end;
    FSectorBuffer[FSelectedByteOffset] := Byte(ValInt);
    FSectorModified := True;
    ButtonSaveSector.Enabled := True;
    LabelEditStatus.Caption := '✏️ MODIFIED (Unsaved)';

    Row := (FSelectedByteOffset div 16) + 1;
    Col := (FSelectedByteOffset mod 16) + 1;
    GridHex.Cells[Col, Row] := Format('%.2X', [ValInt]);
    DisplaySectorData;
  except
    ShowMessage('Invalid hexadecimal byte entered.');
  end;
end;

procedure TFormClusterMap.ButtonFillSectorClick(Sender: TObject);
var
  ValInt: Integer;
begin
  if FReadOnly then
  begin
    ShowMessage('Please switch to Edit Mode first.');
    Exit;
  end;
  try
    ValInt := StrToInt('$' + Trim(EditFillByte.Text));
    if (ValInt < 0) or (ValInt > 255) then
    begin
      ShowMessage('Byte value must be between $00 and $FF.');
      Exit;
    end;

    if MessageDlg('Fill Sector',
      Format('Are you sure you want to fill all 512 bytes of Sector %d with $%.2X?', [FCurrentSector, ValInt]),
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit;

    FillChar(FSectorBuffer, 512, Byte(ValInt));
    FSectorModified := True;
    ButtonSaveSector.Enabled := True;
    LabelEditStatus.Caption := '✏️ MODIFIED (Unsaved)';
    DisplaySectorData;
  except
    ShowMessage('Invalid fill byte.');
  end;
end;

procedure TFormClusterMap.JumpToCluster(const ACluster: Word);
var
  Sec: Int64;
begin
  FSelectedCluster := ACluster;
  Sec := (FDataAreaStart div FBytesPerSector) + (Int64(ACluster) - 2) * FSectorsPerCluster;
  JumpToSector(Sec);
end;

procedure TFormClusterMap.JumpToSector(const ASector: Int64);
begin
  LoadSector(ASector);
end;

end.
