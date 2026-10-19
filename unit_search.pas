unit unit_search;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Math, Masks;

type
  TSearchResult = record
    FileName: string;
    DirPath: string;
    Size: Int64;
    SizeStr: string;
    StartCluster: Word;
    ClusterStr: string;
    DateTimeStr: string;
    AttrStr: string;
    IsDir: Boolean;
    IsDeleted: Boolean;
  end;

  TSearchResultArray = array of TSearchResult;

  TOnNavigateToFileEvent = procedure(const ADirPath, AFileName: string) of object;
  TOnViewFileEvent = procedure(const ADirPath, AFileName: string) of object;

  { TFormSearch }
  TFormSearch = class(TForm)
    PanelTop: TPanel;
    LabelPrompt: TLabel;
    EditQuery: TEdit;
    ButtonSearch: TButton;
    CheckBoxRecursive: TCheckBox;
    CheckBoxCaseSensitive: TCheckBox;
    CheckBoxIncludeDeleted: TCheckBox;
    ListViewResults: TListView;
    PanelBottom: TPanel;
    LabelStatus: TLabel;
    ButtonGoTo: TButton;
    ButtonView: TButton;
    ButtonClose: TButton;

    procedure FormCreate(Sender: TObject);
    procedure ButtonSearchClick(Sender: TObject);
    procedure ButtonGoToClick(Sender: TObject);
    procedure ButtonViewClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ListViewResultsDblClick(Sender: TObject);
    procedure ListViewResultsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditQueryKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  private
    FImagePath: string;
    FCurrentPath: string;
    FResults: TSearchResultArray;
    FOnNavigateToFile: TOnNavigateToFileEvent;
    FOnViewFile: TOnViewFileEvent;

    // FAT12 parameters cached for search
    FBytesPerSector: Word;
    FSectorsPerCluster: Byte;
    FReservedSectors: Word;
    FNumberOfFATs: Byte;
    FRootEntries: Word;
    FTotalSectors: Word;
    FSectorsPerFAT: Word;
    FRootDirOffset: Int64;
    FRootDirSize: Int64;
    FDataAreaOffset: Int64;
    FClusterBytes: Int64;

    function MatchesQuery(const AFileName, AQuery: string; ACaseSensitive: Boolean): Boolean;
    function ReadFAT12Entry(AStream: TStream; ACluster: Word): Word;
    procedure ScanDirectory(AStream: TFileStream; const ADirPath: string; ADirCluster: Word;
      ARecursive, ACaseSensitive, AIncludeDeleted: Boolean; const AQuery: string);
    procedure ExecuteSearch;
    procedure AddResultToListView(const Item: TSearchResult);

  public
    property OnNavigateToFile: TOnNavigateToFileEvent read FOnNavigateToFile write FOnNavigateToFile;
    property OnViewFile: TOnViewFileEvent read FOnViewFile write FOnViewFile;
    procedure OpenForSearch(const AImagePath, AStartPath: string);
  end;

var
  FormSearch: TFormSearch;

implementation

{$R *.lfm}

function FormatSizeStr(Bytes: Int64): string;
begin
  if Bytes < 1024 then
    Result := Format('%d B', [Bytes])
  else if Bytes < 1024 * 1024 then
    Result := Format('%.1f KB', [Bytes / 1024.0])
  else
    Result := Format('%.2f MB', [Bytes / (1024.0 * 1024.0)]);
end;

function FormatFATAttr(Attr: Byte): string;
var
  S: string;
begin
  S := '';
  if (Attr and $10) <> 0 then S := S + 'D' else S := S + '-';
  if (Attr and $20) <> 0 then S := S + 'A' else S := S + '-';
  if (Attr and $01) <> 0 then S := S + 'R' else S := S + '-';
  if (Attr and $02) <> 0 then S := S + 'H' else S := S + '-';
  if (Attr and $04) <> 0 then S := S + 'S' else S := S + '-';
  Result := S;
end;

function FormatFATDateTime(DateWord, TimeWord: Word): string;
var
  Y, M, D, H, Min, Sec: Word;
begin
  if (DateWord = 0) and (TimeWord = 0) then
  begin
    Result := '-';
    Exit;
  end;
  Y := ((DateWord shr 9) and $7F) + 1980;
  M := (DateWord shr 5) and $0F;
  D := DateWord and $1F;
  H := (TimeWord shr 11) and $1F;
  Min := (TimeWord shr 5) and $3F;
  Sec := (TimeWord and $1F) * 2;
  Result := Format('%.4d-%.2d-%.2d %.2d:%.2d', [Y, M, D, H, Min]);
end;

{ TFormSearch }

procedure TFormSearch.FormCreate(Sender: TObject);
begin
  FImagePath := '';
  FCurrentPath := '\';
  SetLength(FResults, 0);
  LabelStatus.Caption := 'Ready to search.';
end;

procedure TFormSearch.OpenForSearch(const AImagePath, AStartPath: string);
begin
  FImagePath := AImagePath;
  if AStartPath <> '' then
    FCurrentPath := AStartPath
  else
    FCurrentPath := '\';

  SetLength(FResults, 0);
  ListViewResults.Items.Clear;
  LabelStatus.Caption := 'Enter search query and press Search (or Enter).';
  Show;
  BringToFront;
  if EditQuery.CanFocus then
  begin
    EditQuery.SetFocus;
    EditQuery.SelectAll;
  end;
end;

function TFormSearch.MatchesQuery(const AFileName, AQuery: string; ACaseSensitive: Boolean): Boolean;
var
  TargetName, TargetQuery: string;
begin
  if AQuery = '' then Exit(True);

  if ACaseSensitive then
  begin
    TargetName := AFileName;
    TargetQuery := AQuery;
  end
  else
  begin
    TargetName := UpperCase(AFileName);
    TargetQuery := UpperCase(AQuery);
  end;

  if (Pos('*', TargetQuery) > 0) or (Pos('?', TargetQuery) > 0) then
  begin
    try
      Result := MatchesMask(TargetName, TargetQuery);
    except
      Result := (Pos(TargetQuery, TargetName) > 0);
    end;
  end
  else
  begin
    Result := (Pos(TargetQuery, TargetName) > 0);
  end;
end;

function TFormSearch.ReadFAT12Entry(AStream: TStream; ACluster: Word): Word;
var
  FATOffset: Int64;
  B0, B1: Byte;
begin
  FATOffset := FReservedSectors * FBytesPerSector + ((ACluster * 3) div 2);
  AStream.Position := FATOffset;
  B0 := AStream.ReadByte;
  B1 := AStream.ReadByte;
  if (ACluster and 1) = 0 then
    Result := (Word(B0) or (Word(B1 and $0F) shl 8))
  else
    Result := ((Word(B0) shr 4) or (Word(B1) shl 4));
end;

procedure TFormSearch.ScanDirectory(AStream: TFileStream; const ADirPath: string; ADirCluster: Word;
  ARecursive, ACaseSensitive, AIncludeDeleted: Boolean; const AQuery: string);
var
  DirEntry: array[0..31] of Byte;
  BaseName, ExtName, CleanName: string;
  Attr: Byte;
  StartCluster: Word;
  FileSize: Int64;
  DateWord, TimeWord: Word;
  IsDir, IsDel: Boolean;
  Item: TSearchResult;
  CurrCluster: Word;
  ClusterOffset, MaxOffset: Int64;
  EntriesInCluster, E: Integer;
  SubDirsToScan: TStringList;
  SubDirClusters: array of Word;
  I: Integer;
begin
  SubDirsToScan := TStringList.Create;
  try
    if ADirCluster = 0 then
    begin
      // Root Directory
      AStream.Position := FRootDirOffset;
      MaxOffset := FRootDirOffset + FRootDirSize;
      while AStream.Position < MaxOffset do
      begin
        if AStream.Read(DirEntry, 32) < 32 then Break;
        if DirEntry[0] = $00 then Break; // End of directory

        IsDel := (DirEntry[0] = $E5);
        if IsDel and (not AIncludeDeleted) then Continue;

        Attr := DirEntry[11];
        if (Attr and $08) <> 0 then Continue; // Skip volume label

        IsDir := (Attr and $10) <> 0;

        // Extract 8.3 name
        SetString(BaseName, PAnsiChar(@DirEntry[0]), 8);
        BaseName := TrimRight(BaseName);
        SetString(ExtName, PAnsiChar(@DirEntry[8]), 3);
        ExtName := TrimRight(ExtName);

        if IsDel then
        begin
          if Length(BaseName) > 1 then
            BaseName := '?' + Copy(BaseName, 2, Length(BaseName) - 1)
          else
            BaseName := '?';
        end;

        if ExtName <> '' then
          CleanName := BaseName + '.' + ExtName
        else
          CleanName := BaseName;

        if (CleanName = '.') or (CleanName = '..') then Continue;

        Move(DirEntry[22], TimeWord, 2);
        Move(DirEntry[24], DateWord, 2);
        Move(DirEntry[26], StartCluster, 2);
        Move(DirEntry[28], FileSize, 4);

        if MatchesQuery(CleanName, AQuery, ACaseSensitive) then
        begin
          Item.FileName := CleanName;
          Item.DirPath := ADirPath;
          Item.Size := FileSize;
          if IsDir then
            Item.SizeStr := '<DIR>'
          else
            Item.SizeStr := FormatSizeStr(FileSize);
          Item.StartCluster := StartCluster;
          if IsDir then
            Item.ClusterStr := '<DIR>'
          else
            Item.ClusterStr := IntToStr(StartCluster);
          Item.DateTimeStr := FormatFATDateTime(DateWord, TimeWord);
          Item.AttrStr := FormatFATAttr(Attr);
          Item.IsDir := IsDir;
          Item.IsDeleted := IsDel;

          SetLength(FResults, Length(FResults) + 1);
          FResults[High(FResults)] := Item;
          AddResultToListView(Item);
        end;

        if IsDir and ARecursive and (StartCluster >= 2) and (not IsDel) then
        begin
          SubDirsToScan.Add(CleanName);
          SetLength(SubDirClusters, Length(SubDirClusters) + 1);
          SubDirClusters[High(SubDirClusters)] := StartCluster;
        end;
      end;
    end
    else
    begin
      // Subdirectory FAT chain
      CurrCluster := ADirCluster;
      while (CurrCluster >= 2) and (CurrCluster < $FF8) do
      begin
        ClusterOffset := FDataAreaOffset + Int64(CurrCluster - 2) * FClusterBytes;
        AStream.Position := ClusterOffset;
        EntriesInCluster := FClusterBytes div 32;

        for E := 0 to EntriesInCluster - 1 do
        begin
          if AStream.Read(DirEntry, 32) < 32 then Break;
          if DirEntry[0] = $00 then Break; // End of directory

          IsDel := (DirEntry[0] = $E5);
          if IsDel and (not AIncludeDeleted) then Continue;

          Attr := DirEntry[11];
          if (Attr and $08) <> 0 then Continue; // Skip volume label

          IsDir := (Attr and $10) <> 0;

          // Extract 8.3 name
          SetString(BaseName, PAnsiChar(@DirEntry[0]), 8);
          BaseName := TrimRight(BaseName);
          SetString(ExtName, PAnsiChar(@DirEntry[8]), 3);
          ExtName := TrimRight(ExtName);

          if IsDel then
          begin
            if Length(BaseName) > 1 then
              BaseName := '?' + Copy(BaseName, 2, Length(BaseName) - 1)
            else
              BaseName := '?';
          end;

          if ExtName <> '' then
            CleanName := BaseName + '.' + ExtName
          else
            CleanName := BaseName;

          if (CleanName = '.') or (CleanName = '..') then Continue;

          Move(DirEntry[22], TimeWord, 2);
          Move(DirEntry[24], DateWord, 2);
          Move(DirEntry[26], StartCluster, 2);
          Move(DirEntry[28], FileSize, 4);

          if MatchesQuery(CleanName, AQuery, ACaseSensitive) then
          begin
            Item.FileName := CleanName;
            Item.DirPath := ADirPath;
            Item.Size := FileSize;
            if IsDir then
              Item.SizeStr := '<DIR>'
            else
              Item.SizeStr := FormatSizeStr(FileSize);
            Item.StartCluster := StartCluster;
            if IsDir then
              Item.ClusterStr := '<DIR>'
            else
              Item.ClusterStr := IntToStr(StartCluster);
            Item.DateTimeStr := FormatFATDateTime(DateWord, TimeWord);
            Item.AttrStr := FormatFATAttr(Attr);
            Item.IsDir := IsDir;
            Item.IsDeleted := IsDel;

            SetLength(FResults, Length(FResults) + 1);
            FResults[High(FResults)] := Item;
            AddResultToListView(Item);
          end;

          if IsDir and ARecursive and (StartCluster >= 2) and (not IsDel) then
          begin
            SubDirsToScan.Add(CleanName);
            SetLength(SubDirClusters, Length(SubDirClusters) + 1);
            SubDirClusters[High(SubDirClusters)] := StartCluster;
          end;
        end;

        CurrCluster := ReadFAT12Entry(AStream, CurrCluster);
      end;
    end;

    // Recurse into subdirectories
    for I := 0 to SubDirsToScan.Count - 1 do
    begin
      ScanDirectory(AStream, ADirPath + SubDirsToScan[I] + '\', SubDirClusters[I],
        ARecursive, ACaseSensitive, AIncludeDeleted, AQuery);
    end;
  finally
    SubDirsToScan.Free;
  end;
end;

procedure TFormSearch.AddResultToListView(const Item: TSearchResult);
var
  LI: TListItem;
begin
  LI := ListViewResults.Items.Add;
  if Item.IsDeleted then
    LI.Caption := Item.FileName + ' (*d*)'
  else
    LI.Caption := Item.FileName;

  LI.SubItems.Add(Item.DirPath);
  LI.SubItems.Add(Item.SizeStr);
  LI.SubItems.Add(Item.ClusterStr);
  LI.SubItems.Add(Item.DateTimeStr);
  LI.SubItems.Add(Item.AttrStr);
end;

procedure TFormSearch.ExecuteSearch;
var
  FS: TFileStream;
  BootSec: array[0..511] of Byte;
  QueryStr, StartPath: string;
  StartCluster: Word;
  IsRec, IsCase, IsDel: Boolean;
begin
  QueryStr := Trim(EditQuery.Text);
  if (FImagePath = '') or (not FileExists(FImagePath)) then
  begin
    LabelStatus.Caption := 'Error: No disk image is currently loaded.';
    Exit;
  end;

  ListViewResults.Items.BeginUpdate;
  try
    ListViewResults.Items.Clear;
    SetLength(FResults, 0);

    FS := TFileStream.Create(FImagePath, fmOpenRead or fmShareDenyNone);
    try
      if FS.Size < 512 then
      begin
        LabelStatus.Caption := 'Error: Disk image is too small or invalid.';
        Exit;
      end;

      // Read Sector 0 (Boot Sector)
      FS.Position := 0;
      FS.ReadBuffer(BootSec, 512);

      Move(BootSec[$0B], FBytesPerSector, 2);
      FSectorsPerCluster := BootSec[$0D];
      Move(BootSec[$0E], FReservedSectors, 2);
      FNumberOfFATs := BootSec[$10];
      Move(BootSec[$11], FRootEntries, 2);
      Move(BootSec[$13], FTotalSectors, 2);
      Move(BootSec[$16], FSectorsPerFAT, 2);

      if FBytesPerSector = 0 then FBytesPerSector := 512;
      if FSectorsPerCluster = 0 then FSectorsPerCluster := 2;
      if FReservedSectors = 0 then FReservedSectors := 1;
      if FNumberOfFATs = 0 then FNumberOfFATs := 2;
      if FRootEntries = 0 then FRootEntries := 112;
      if FSectorsPerFAT = 0 then FSectorsPerFAT := 5;

      FRootDirOffset := (Int64(FReservedSectors) + Int64(FNumberOfFATs) * Int64(FSectorsPerFAT)) * Int64(FBytesPerSector);
      FRootDirSize := Int64(FRootEntries) * 32;
      FDataAreaOffset := FRootDirOffset + FRootDirSize;
      FClusterBytes := Int64(FSectorsPerCluster) * Int64(FBytesPerSector);

      IsRec := CheckBoxRecursive.Checked;
      IsCase := CheckBoxCaseSensitive.Checked;
      IsDel := CheckBoxIncludeDeleted.Checked;

      if IsRec then
      begin
        StartPath := '\';
        StartCluster := 0;
      end
      else
      begin
        StartPath := FCurrentPath;
        if StartPath = '\' then
          StartCluster := 0
        else
          StartCluster := 0; // fallback to root if specific cluster not passed
      end;

      ScanDirectory(FS, StartPath, StartCluster, IsRec, IsCase, IsDel, QueryStr);
    finally
      FS.Free;
    end;
  finally
    ListViewResults.Items.EndUpdate;
  end;

  if Length(FResults) = 0 then
    LabelStatus.Caption := Format('No matching files found for "%s".', [QueryStr])
  else if Length(FResults) = 1 then
    LabelStatus.Caption := Format('Found 1 matching file for "%s".', [QueryStr])
  else
    LabelStatus.Caption := Format('Found %d matching items for "%s".', [Length(FResults), QueryStr]);
end;

procedure TFormSearch.ButtonSearchClick(Sender: TObject);
begin
  ExecuteSearch;
end;

procedure TFormSearch.EditQueryKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 13 then // Enter
  begin
    Key := 0;
    ExecuteSearch;
  end
  else if Key = 27 then // Esc
  begin
    Key := 0;
    Close;
  end;
end;

procedure TFormSearch.ButtonGoToClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := ListViewResults.ItemIndex;
  if (Idx < 0) or (Idx >= Length(FResults)) then
  begin
    ShowMessage('Please select an item from the results list first.');
    Exit;
  end;

  if Assigned(FOnNavigateToFile) then
  begin
    FOnNavigateToFile(FResults[Idx].DirPath, FResults[Idx].FileName);
    Close;
  end;
end;

procedure TFormSearch.ButtonViewClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := ListViewResults.ItemIndex;
  if (Idx < 0) or (Idx >= Length(FResults)) then
  begin
    ShowMessage('Please select a file to view.');
    Exit;
  end;

  if Assigned(FOnViewFile) then
  begin
    FOnViewFile(FResults[Idx].DirPath, FResults[Idx].FileName);
  end;
end;

procedure TFormSearch.ListViewResultsDblClick(Sender: TObject);
begin
  ButtonGoToClick(Sender);
end;

procedure TFormSearch.ListViewResultsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 13 then // Enter -> Go to location
  begin
    Key := 0;
    ButtonGoToClick(Sender);
  end
  else if Key = 114 then // F3 -> View
  begin
    Key := 0;
    ButtonViewClick(Sender);
  end
  else if Key = 27 then // Esc
  begin
    Key := 0;
    Close;
  end;
end;

procedure TFormSearch.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

end.
