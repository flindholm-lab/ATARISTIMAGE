unit unit_virusreport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils;

type
  TVirusTargetType = (vtBootSector, vtFile, vtProtector, vtCustomBoot, vtNone);

  TVirusScanResult = record
    VirusFound: Boolean;
    TargetType: TVirusTargetType;
    Name: string;
    Description: string;
    Target: string;          // e.g. "Boot Sector (Sector 0)" or "File: \AUTO\MEDWAY.PRG"
    SourceFile: string;      // Scanned file name / archive member
    ActualImagePath: string; // Full path on disk (if standalone)
    ZipArchivePath: string;  // Non-empty if inside zip
    ZipEntryName: string;    // Entry path inside zip
    Offset: Integer;         // Byte offset within sector or file
    CanDisinfect: Boolean;
    Cluster: Word;           // If file, starting cluster
    FileSize: Int64;
    Disinfected: Boolean;
  end;

  TVirusScanResultArray = array of TVirusScanResult;

  TDiskScanSummary = record
    DiskImagePath: string;
    TotalFilesScanned: Integer;
    TotalFoldersScanned: Integer;
    BootChecksum: Word;
    BootIsExecutable: Boolean;
    Threats: TVirusScanResultArray;
  end;

  { Single detailed finding item in the report }
  TVirusReportItem = record
    SourceFile: string;
    Target: string;
    ThreatName: string;
    Category: string;        // 'Boot Sector', 'File Infector', 'Protector / Vaccine', 'Custom Bootloader'
    Status: string;          // e.g. 'Threat (Disinfectable)', 'Suspicious (Floppy Write)', 'Active / Protected'
    Severity: string;        // 'danger', 'warning', 'teal', 'cleaned', 'info', 'success'
    Offset: Integer;
    Cluster: Word;
    FileSize: Int64;
    CanDisinfect: Boolean;
    Disinfected: Boolean;
    Description: string;
    ZipArchivePath: string;
    ZipEntryName: string;
  end;

  { Complete virus scan report document }
  TVirusReport = record
    Title: string;
    ScanDate: TDateTime;
    ScanDateStr: string;
    TargetName: string;
    TargetPath: string;
    IsFolderScan: Boolean;
    BootOnly: Boolean;
    TotalImagesScanned: Integer;
    TotalFilesScanned: Integer;
    TotalFoldersScanned: Integer;
    ThreatCount: Integer;         // Active malware / replicating threats
    PotentialCount: Integer;      // Suspicious heuristics / unrec bootloaders
    ProtectorCount: Integer;      // Boot protectors / vaccines
    CustomBootCount: Integer;     // Standard custom game/demo loaders
    CleanedCount: Integer;        // Disinfected items
    OverallVerdict: string;       // 'CLEAN', 'PROTECTED', 'SUSPICIOUS', 'THREATS DETECTED'
    OverallVerdictColor: string;  // '#3FB950' (green), '#5EEAD4' (teal), '#E3B341' (amber), '#F85149' (red)
    BootChecksum: Word;
    BootChecksumStr: string;
    BootIsExecutable: Boolean;
    BootStatusStr: string;
    Items: array of TVirusReportItem;
  end;

{ Helper Functions }
function HasFloppyWriteRoutine(const TraitsOrDescription: string): Boolean; overload;
function HasFloppyWriteRoutine(const Threat: TVirusScanResult): Boolean; overload;
function FormatBytes(Bytes: Int64): string;
function EscapeJSON(const S: string): string;
function EscapeHTML(const S: string): string;

{ Builder }
function BuildVirusReport(const ASummary: TDiskScanSummary; const ATargetPath: string;
  AIsFolderScan, ABootOnly: Boolean; ATotalImagesProcessed: Integer = 0): TVirusReport;

{ Exporters }
function ExportVirusReportToHTML(const Report: TVirusReport; const OutFileName: string): Boolean;
function ExportVirusReportToCSV(const Report: TVirusReport; const OutFileName: string): Boolean;
function ExportVirusReportToJSON(const Report: TVirusReport; const OutFileName: string): Boolean;
function ExportVirusReportToText(const Report: TVirusReport; const OutFileName: string): Boolean;
function ExportVirusReportToFile(const Report: TVirusReport; const OutFileName: string): Boolean; overload;
function ExportVirusReportToFile(const ASummary: TDiskScanSummary; const ATargetPath, OutFileName: string;
  AIsFolderScan: Boolean = False; ABootOnly: Boolean = False; ATotalImagesProcessed: Integer = 0): Boolean; overload;

implementation

function FormatBytes(Bytes: Int64): string;
begin
  if Bytes < 1024 then
    Result := Format('%d B', [Bytes])
  else if Bytes < 1024 * 1024 then
    Result := Format('%.1f KB (%s B)', [Bytes / 1024.0, FormatFloat('#,##0', Bytes)])
  else
    Result := Format('%.2f MB (%s B)', [Bytes / (1024.0 * 1024.0), FormatFloat('#,##0', Bytes)]);
end;

function EscapeJSON(const S: string): string;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    case C of
      '"':  Result := Result + '\"';
      '\':  Result := Result + '\\';
      #8:   Result := Result + '\b';
      #9:   Result := Result + '\t';
      #10:  Result := Result + '\n';
      #12:  Result := Result + '\f';
      #13:  Result := Result + '\r';
      else
        if Ord(C) < 32 then
          Result := Result + '\u' + IntToHex(Ord(C), 4)
        else
          Result := Result + C;
    end;
  end;
end;

function EscapeHTML(const S: string): string;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    case C of
      '&': Result := Result + '&amp;';
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '"': Result := Result + '&quot;';
      '''': Result := Result + '&#39;';
      else Result := Result + C;
    end;
  end;
end;

function PadRight(const S: string; W: Integer): string;
begin
  Result := S;
  while Length(Result) < W do
    Result := Result + ' ';
end;

function PadLeft(const S: string; W: Integer): string;
begin
  Result := S;
  while Length(Result) < W do
    Result := ' ' + Result;
end;

function HasFloppyWriteRoutine(const TraitsOrDescription: string): Boolean;
begin
  Result := (Pos('Floppy Write', TraitsOrDescription) > 0) or
            (Pos('Rwabs Write', TraitsOrDescription) > 0) or
            (Pos('Sector Write', TraitsOrDescription) > 0) or
            (Pos('Floppy Controller Write', TraitsOrDescription) > 0) or
            (Pos('Protobt', TraitsOrDescription) > 0) or
            (Pos('floppy write', LowerCase(TraitsOrDescription)) > 0);
end;

function HasFloppyWriteRoutine(const Threat: TVirusScanResult): Boolean;
begin
  Result := HasFloppyWriteRoutine(Threat.Description) or
            HasFloppyWriteRoutine(Threat.Name);
end;

function BuildVirusReport(const ASummary: TDiskScanSummary; const ATargetPath: string;
  AIsFolderScan, ABootOnly: Boolean; ATotalImagesProcessed: Integer = 0): TVirusReport;
var
  I: Integer;
  T: TVirusScanResult;
  Item: TVirusReportItem;
  LowName: string;
begin
  Result := Default(TVirusReport);
  Result.Title := 'Atari ST Anti-Virus Scan Report';
  Result.ScanDate := Now;
  Result.ScanDateStr := FormatDateTime('yyyy-mm-dd hh:nn:ss', Result.ScanDate);
  Result.TargetPath := ATargetPath;
  Result.IsFolderScan := AIsFolderScan;
  Result.BootOnly := ABootOnly;

  if AIsFolderScan then
  begin
    Result.TargetName := ExtractFileName(ExcludeTrailingPathDelimiter(ATargetPath));
    if Result.TargetName = '' then Result.TargetName := ATargetPath;
    if ATotalImagesProcessed > 0 then
      Result.TotalImagesScanned := ATotalImagesProcessed
    else
      Result.TotalImagesScanned := 1;
  end
  else
  begin
    Result.TargetName := ExtractFileName(ATargetPath);
    if Result.TargetName = '' then Result.TargetName := 'Disk Image';
    Result.TotalImagesScanned := 1;
    Result.BootChecksum := ASummary.BootChecksum;
    Result.BootChecksumStr := '$' + IntToHex(ASummary.BootChecksum, 4);
    Result.BootIsExecutable := ASummary.BootIsExecutable;
    if ASummary.BootIsExecutable then
      Result.BootStatusStr := 'Executable Boot Block'
    else
      Result.BootStatusStr := 'Non-executable / Clean';
  end;

  Result.TotalFilesScanned := ASummary.TotalFilesScanned;
  Result.TotalFoldersScanned := ASummary.TotalFoldersScanned;

  SetLength(Result.Items, Length(ASummary.Threats));
  for I := 0 to High(ASummary.Threats) do
  begin
    T := ASummary.Threats[I];
    Item := Default(TVirusReportItem);

    if T.SourceFile <> '' then
      Item.SourceFile := T.SourceFile
    else if T.ActualImagePath <> '' then
      Item.SourceFile := ExtractFileName(T.ActualImagePath)
    else
      Item.SourceFile := Result.TargetName;

    Item.Target := T.Target;
    Item.ThreatName := T.Name;
    Item.Offset := T.Offset;
    Item.Cluster := T.Cluster;
    Item.FileSize := T.FileSize;
    Item.CanDisinfect := T.CanDisinfect;
    Item.Disinfected := T.Disinfected;
    Item.Description := T.Description;
    Item.ZipArchivePath := T.ZipArchivePath;
    Item.ZipEntryName := T.ZipEntryName;

    // Category mapping
    case T.TargetType of
      vtBootSector:  Item.Category := 'Boot Sector';
      vtProtector:   Item.Category := 'Protector / Vaccine';
      vtCustomBoot:  Item.Category := 'Custom Bootloader';
      vtFile:        Item.Category := 'File Infector';
      else           Item.Category := 'Unknown';
    end;

    // Status and Severity mapping
    LowName := LowerCase(T.Name);
    if T.Disinfected then
    begin
      Item.Status := 'Cleaned / Safe';
      Item.Severity := 'cleaned';
      Inc(Result.CleanedCount);
    end
    else if T.TargetType = vtProtector then
    begin
      Item.Status := 'Active / Protected';
      Item.Severity := 'teal';
      Inc(Result.ProtectorCount);
    end
    else if T.TargetType = vtCustomBoot then
    begin
      Item.Status := 'Standard / Bootable';
      Item.Severity := 'info';
      Inc(Result.CustomBootCount);
    end
    else if Pos('unrecognized', LowName) > 0 then
    begin
      Item.Status := 'Potential Threat';
      Item.Severity := 'warning';
      Inc(Result.PotentialCount);
    end
    else if Pos('suspicious', LowName) > 0 then
    begin
      if HasFloppyWriteRoutine(T) then
      begin
        Item.Status := 'Suspicious (Floppy Write)';
        Item.Severity := 'danger';
        Inc(Result.ThreatCount);
      end
      else
      begin
        Item.Status := 'Suspicious (Vector Hook)';
        Item.Severity := 'warning';
        Inc(Result.PotentialCount);
      end;
    end
    else
    begin
      if T.CanDisinfect then
        Item.Status := 'Threat (Disinfectable)'
      else
        Item.Status := 'Threat (Quarantined)';
      Item.Severity := 'danger';
      Inc(Result.ThreatCount);
    end;

    Result.Items[I] := Item;
  end;

  // Compute Overall Verdict
  if Result.ThreatCount > 0 then
  begin
    Result.OverallVerdict := 'THREATS DETECTED';
    Result.OverallVerdictColor := '#F85149';
  end
  else if Result.PotentialCount > 0 then
  begin
    Result.OverallVerdict := 'SUSPICIOUS / POTENTIAL THREATS';
    Result.OverallVerdictColor := '#E3B341';
  end
  else if Result.ProtectorCount > 0 then
  begin
    Result.OverallVerdict := 'PROTECTED / VACCINATED';
    Result.OverallVerdictColor := '#5EEAD4';
  end
  else
  begin
    Result.OverallVerdict := 'CLEAN';
    Result.OverallVerdictColor := '#3FB950';
  end;
end;

function ExportVirusReportToHTML(const Report: TVirusReport; const OutFileName: string): Boolean;
var
  SL: TStringList;
  I: Integer;
  Item: TVirusReportItem;
  BannerHTML, ModeStr, BootInfoHTML, CatBadge, StatusBadge, LocationStr, SearchBlob: string;
begin
  Result := False;
  SL := TStringList.Create;
  try
    SL.Add('<!DOCTYPE html>');
    SL.Add('<html lang="en">');
    SL.Add('<head>');
    SL.Add('  <meta charset="UTF-8">');
    SL.Add('  <meta name="viewport" content="width=device-width, initial-scale=1.0">');
    SL.Add(Format('  <title>Atari ST Anti-Virus Scan Report - %s</title>', [EscapeHTML(Report.TargetName)]));
    SL.Add('  <style>');
    SL.Add('    :root {');
    SL.Add('      --bg: #121418; --card-bg: #1A1D24; --card-sub: #222630; --border: #2A2E38; --border-sub: #303644;');
    SL.Add('      --text: #E0E4EC; --text-muted: #8B949E; --accent: #58A6FF; --danger: #F85149; --danger-bg: #3B1212;');
    SL.Add('      --danger-border: #7F1D1D; --warn: #E3B341; --warn-bg: #3D2D0A; --warn-border: #854D0E;');
    SL.Add('      --success: #3FB950; --success-bg: #102A1E; --success-border: #166534; --teal: #5EEAD4;');
    SL.Add('      --teal-bg: #0E3326; --teal-border: #115E59;');
    SL.Add('    }');
    SL.Add('    * { box-sizing: border-box; }');
    SL.Add('    body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif; background: var(--bg); color: var(--text); margin: 0; padding: 24px; line-height: 1.5; }');
    SL.Add('    .container { max-width: 1200px; margin: 0 auto; }');
    SL.Add('    .header { background: var(--card-bg); border: 1px solid var(--border); border-radius: 8px; padding: 24px; margin-bottom: 24px; }');
    SL.Add('    .header-top { display: flex; justify-content: space-between; align-items: flex-start; flex-wrap: wrap; gap: 12px; margin-bottom: 16px; }');
    SL.Add('    .header h1 { margin: 0; font-size: 22px; color: var(--accent); }');
    SL.Add('    .header-meta { color: var(--text-muted); font-size: 13px; margin-top: 6px; }');
    SL.Add('    .header-meta strong { color: var(--text); }');
    SL.Add('    .banner { display: flex; align-items: center; gap: 12px; padding: 14px 18px; border-radius: 6px; font-size: 14px; font-weight: 500; margin-bottom: 20px; }');
    SL.Add('    .banner-danger { background: var(--danger-bg); border: 1px solid var(--danger-border); color: #FCA5A5; }');
    SL.Add('    .banner-warn { background: var(--warn-bg); border: 1px solid var(--warn-border); color: #FDE047; }');
    SL.Add('    .banner-teal { background: var(--teal-bg); border: 1px solid var(--teal-border); color: var(--teal); }');
    SL.Add('    .banner-clean { background: var(--success-bg); border: 1px solid var(--success-border); color: #86EFAC; }');
    SL.Add('    .banner-icon { font-size: 20px; }');
    SL.Add('    .grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(160px, 1fr)); gap: 14px; margin-top: 16px; }');
    SL.Add('    .card { background: var(--card-sub); border-radius: 6px; padding: 12px 16px; border: 1px solid var(--border-sub); }');
    SL.Add('    .card .label { font-size: 11px; text-transform: uppercase; color: var(--text-muted); letter-spacing: 0.5px; }');
    SL.Add('    .card .value { font-size: 17px; font-weight: 700; color: #F0F6FC; margin-top: 4px; }');
    SL.Add('    .toolbar { display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 14px; margin-bottom: 16px; }');
    SL.Add('    .search-box { background: var(--card-bg); border: 1px solid var(--border); border-radius: 6px; color: var(--text); padding: 8px 14px; font-size: 13px; width: 340px; max-width: 100%; outline: none; }');
    SL.Add('    .search-box:focus { border-color: var(--accent); }');
    SL.Add('    .filter-pills { display: flex; gap: 8px; flex-wrap: wrap; }');
    SL.Add('    .filter-btn { background: var(--card-sub); border: 1px solid var(--border-sub); color: var(--text-muted); padding: 6px 12px; border-radius: 20px; font-size: 12px; font-weight: 600; cursor: pointer; }');
    SL.Add('    .filter-btn:hover { color: var(--text); border-color: var(--text-muted); }');
    SL.Add('    .filter-btn.active { background: #1F385C; color: var(--accent); border-color: #388BFD; }');
    SL.Add('    .filter-btn.active.btn-danger { background: #4C1D1D; color: #F85149; border-color: #F85149; }');
    SL.Add('    .filter-btn.active.btn-warn { background: #4D3800; color: #E3B341; border-color: #E3B341; }');
    SL.Add('    .filter-btn.active.btn-teal { background: #0E3326; color: var(--teal); border-color: var(--teal); }');
    SL.Add('    .filter-btn.active.btn-cleaned { background: #21262D; color: #8B949E; border-color: #8B949E; }');
    SL.Add('    .table-container { background: var(--card-bg); border: 1px solid var(--border); border-radius: 8px; overflow: hidden; }');
    SL.Add('    table { width: 100%; border-collapse: collapse; font-size: 13px; }');
    SL.Add('    th { background: var(--card-sub); color: var(--text-muted); text-align: left; padding: 12px 14px; font-weight: 600; border-bottom: 1px solid var(--border-sub); white-space: nowrap; }');
    SL.Add('    td { padding: 10px 14px; border-bottom: 1px solid #1F232B; vertical-align: middle; }');
    SL.Add('    tr:last-child td { border-bottom: none; }');
    SL.Add('    tr.finding-row:hover { background: #20242D; }');
    SL.Add('    .mono { font-family: "SFMono-Regular", Consolas, "Liberation Mono", Menlo, monospace; font-size: 12px; }');
    SL.Add('    .badge { display: inline-block; padding: 2px 8px; border-radius: 4px; font-size: 11px; font-weight: 600; white-space: nowrap; }');
    SL.Add('    .badge-boot { background: #1F385C; color: #58A6FF; }');
    SL.Add('    .badge-file { background: #3B2D54; color: #D2A8FF; }');
    SL.Add('    .badge-protector { background: #0E3326; color: #5EEAD4; }');
    SL.Add('    .badge-custom { background: #1B3C48; color: #79C0FF; }');
    SL.Add('    .badge-danger { background: #4C1D1D; color: #F85149; }');
    SL.Add('    .badge-warn { background: #4D3800; color: #E3B341; }');
    SL.Add('    .badge-success { background: #234228; color: #3FB950; }');
    SL.Add('    .badge-cleaned { background: #21262D; color: #8B949E; text-decoration: line-through; }');
    SL.Add('    .badge-info { background: #1F385C; color: #58A6FF; }');
    SL.Add('    .clean-empty-row { text-align: center; padding: 48px 24px; color: #3FB950; font-size: 15px; font-weight: 500; }');
    SL.Add('    .footer { text-align: center; margin-top: 24px; color: var(--text-muted); font-size: 12px; }');
    SL.Add('  </style>');
    SL.Add('</head>');
    SL.Add('<body>');
    SL.Add('  <div class="container">');
    SL.Add('    <div class="header">');
    SL.Add('      <div class="header-top">');
    SL.Add('        <div>');
    SL.Add(Format('          <h1>Atari ST Anti-Virus Scan Report: %s</h1>', [EscapeHTML(Report.TargetName)]));
    if Report.IsFolderScan then
      ModeStr := 'Folder / Directory Scan'
    else if Report.BootOnly then
      ModeStr := 'Single Disk Scan (Boot Sector Only)'
    else
      ModeStr := 'Single Disk Scan (Filesystem & Boot Sector)';

    SL.Add(Format('          <div class="header-meta">Mode: <strong>%s</strong> &bull; Scanned: <strong>%s</strong> &bull; Target: <span class="mono">%s</span></div>',
      [EscapeHTML(ModeStr), Report.ScanDateStr, EscapeHTML(Report.TargetPath)]));
    SL.Add('        </div>');
    SL.Add('      </div>');

    // Verdict Banner
    if Report.ThreatCount > 0 then
      BannerHTML := Format('<div class="banner banner-danger"><span class="banner-icon">🚨</span><div><strong>ALERT: %d Active Virus Threat(s) Detected!</strong> Remediation or boot sector disinfection recommended.</div></div>', [Report.ThreatCount])
    else if Report.PotentialCount > 0 then
      BannerHTML := Format('<div class="banner banner-warn"><span class="banner-icon">⚠️</span><div><strong>CAUTION: %d Potential Threat(s) / Suspicious Heuristics Detected.</strong> Review vector hooks and write routines.</div></div>', [Report.PotentialCount])
    else if Report.ProtectorCount > 0 then
      BannerHTML := Format('<div class="banner banner-teal"><span class="banner-icon">🛡️</span><div><strong>CLEAN &amp; PROTECTED: Active Boot Protector / Vaccine Installed.</strong> System vectors are protected against viral infection.</div></div>', [])
    else
      BannerHTML := '<div class="banner banner-clean"><span class="banner-icon">✅</span><div><strong>CLEAN: No Viruses or Malicious Signatures Detected.</strong> Disk boot sector and analyzed files are clean.</div></div>';

    SL.Add('      ' + BannerHTML);

    // KPI Cards
    SL.Add('      <div class="grid">');
    SL.Add(Format('        <div class="card"><div class="label">Overall Verdict</div><div class="value" style="color: %s;">%s</div></div>', [Report.OverallVerdictColor, Report.OverallVerdict]));
    SL.Add(Format('        <div class="card"><div class="label">Images Scanned</div><div class="value">%d</div></div>', [Report.TotalImagesScanned]));
    SL.Add(Format('        <div class="card"><div class="label">Files Checked</div><div class="value">%d</div></div>', [Report.TotalFilesScanned]));
    SL.Add(Format('        <div class="card"><div class="label">Malware Threats</div><div class="value" style="color: %s;">%d</div></div>', [IfThen(Report.ThreatCount > 0, '#F85149', '#F0F6FC'), Report.ThreatCount]));
    SL.Add(Format('        <div class="card"><div class="label">Suspicious / Caution</div><div class="value" style="color: %s;">%d</div></div>', [IfThen(Report.PotentialCount > 0, '#E3B341', '#F0F6FC'), Report.PotentialCount]));
    SL.Add(Format('        <div class="card"><div class="label">Boot Protectors</div><div class="value" style="color: %s;">%d</div></div>', [IfThen(Report.ProtectorCount > 0, '#5EEAD4', '#F0F6FC'), Report.ProtectorCount]));
    if Report.CleanedCount > 0 then
      SL.Add(Format('        <div class="card"><div class="label">Cleaned / Safe</div><div class="value" style="color: #3FB950;">%d</div></div>', [Report.CleanedCount]));
    if not Report.IsFolderScan then
    begin
      BootInfoHTML := Format('%s (%s)', [Report.BootChecksumStr, Report.BootStatusStr]);
      SL.Add(Format('        <div class="card"><div class="label">Boot Sector</div><div class="value" style="font-size: 13px;">%s</div></div>', [EscapeHTML(BootInfoHTML)]));
    end;
    SL.Add('      </div>');
    SL.Add('    </div>');

    // Toolbar (Search & Filter Pills)
    SL.Add('    <div class="toolbar">');
    SL.Add('      <input type="text" id="reportSearch" class="search-box" placeholder="Search by filename, target, threat name, traits..." onkeyup="applyFilter()">');
    SL.Add('      <div class="filter-pills">');
    SL.Add(Format('        <button class="filter-btn active" onclick="setFilter(''all'', this)">All (%d)</button>', [Length(Report.Items)]));
    SL.Add(Format('        <button class="filter-btn btn-danger" onclick="setFilter(''danger'', this)">Threats (%d)</button>', [Report.ThreatCount]));
    SL.Add(Format('        <button class="filter-btn btn-warn" onclick="setFilter(''warning'', this)">Suspicious (%d)</button>', [Report.PotentialCount]));
    SL.Add(Format('        <button class="filter-btn btn-teal" onclick="setFilter(''teal'', this)">Protectors (%d)</button>', [Report.ProtectorCount]));
    if Report.CleanedCount > 0 then
      SL.Add(Format('        <button class="filter-btn btn-cleaned" onclick="setFilter(''cleaned'', this)">Cleaned (%d)</button>', [Report.CleanedCount]));
    SL.Add('      </div>');
    SL.Add('    </div>');

    // Table
    SL.Add('    <div class="table-container">');
    SL.Add('      <table>');
    SL.Add('        <thead>');
    SL.Add('          <tr>');
    SL.Add('            <th>Scanned Source</th>');
    SL.Add('            <th>Target</th>');
    SL.Add('            <th>Threat / Item Name</th>');
    SL.Add('            <th>Category</th>');
    SL.Add('            <th>Status / Verdict</th>');
    SL.Add('            <th>Cluster / Offset</th>');
    SL.Add('            <th>Technical Details &amp; Traits</th>');
    SL.Add('          </tr>');
    SL.Add('        </thead>');
    SL.Add('        <tbody>');

    if Length(Report.Items) = 0 then
    begin
      SL.Add('          <tr>');
      SL.Add('            <td colspan="7" class="clean-empty-row">');
      SL.Add('              &#x2705; <strong>Clean Scan:</strong> No virus signatures, heuristic vector hooks, or suspicious executable boot blocks were detected.');
      SL.Add('            </td>');
      SL.Add('          </tr>');
    end
    else
    begin
      for I := 0 to High(Report.Items) do
      begin
        Item := Report.Items[I];

        // Badges
        case Item.Category of
          'Boot Sector':         CatBadge := '<span class="badge badge-boot">Boot Sector</span>';
          'File Infector':       CatBadge := '<span class="badge badge-file">File Infector</span>';
          'Protector / Vaccine': CatBadge := '<span class="badge badge-protector">Protector</span>';
          'Custom Bootloader':   CatBadge := '<span class="badge badge-custom">Custom Boot</span>';
          else                   CatBadge := Format('<span class="badge">%s</span>', [EscapeHTML(Item.Category)]);
        end;

        case Item.Severity of
          'danger':  StatusBadge := Format('<span class="badge badge-danger">%s</span>', [EscapeHTML(Item.Status)]);
          'warning': StatusBadge := Format('<span class="badge badge-warn">%s</span>', [EscapeHTML(Item.Status)]);
          'teal':    StatusBadge := Format('<span class="badge badge-protector">%s</span>', [EscapeHTML(Item.Status)]);
          'cleaned': StatusBadge := Format('<span class="badge badge-cleaned">%s</span>', [EscapeHTML(Item.Status)]);
          'info':    StatusBadge := Format('<span class="badge badge-info">%s</span>', [EscapeHTML(Item.Status)]);
          else       StatusBadge := Format('<span class="badge badge-success">%s</span>', [EscapeHTML(Item.Status)]);
        end;

        if Item.Cluster >= 2 then
          LocationStr := Format('Cluster #%d', [Item.Cluster])
        else if Item.Offset >= 0 then
          LocationStr := Format('Offset +$%X', [Item.Offset])
        else
          LocationStr := '&mdash;';

        SearchBlob := LowerCase(Item.SourceFile + ' ' + Item.Target + ' ' + Item.ThreatName + ' ' +
          Item.Category + ' ' + Item.Status + ' ' + Item.Description);

        SL.Add(Format('          <tr class="finding-row" data-severity="%s" data-search="%s">', [Item.Severity, EscapeHTML(SearchBlob)]));
        SL.Add(Format('            <td class="mono"><strong>%s</strong></td>', [EscapeHTML(Item.SourceFile)]));
        SL.Add(Format('            <td class="mono">%s</td>', [EscapeHTML(Item.Target)]));
        SL.Add(Format('            <td><strong>%s</strong></td>', [EscapeHTML(Item.ThreatName)]));
        SL.Add(Format('            <td>%s</td>', [CatBadge]));
        SL.Add(Format('            <td>%s</td>', [StatusBadge]));
        SL.Add(Format('            <td class="mono">%s</td>', [LocationStr]));
        SL.Add(Format('            <td>%s</td>', [EscapeHTML(Item.Description)]));
        SL.Add('          </tr>');
      end;

      SL.Add('          <tr id="noFilterMatches" style="display: none;">');
      SL.Add('            <td colspan="7" style="text-align: center; padding: 28px; color: var(--text-muted);">');
      SL.Add('              No findings match the current filter or search criteria.');
      SL.Add('            </td>');
      SL.Add('          </tr>');
    end;

    SL.Add('        </tbody>');
    SL.Add('      </table>');
    SL.Add('    </div>');

    // JavaScript
    SL.Add('    <script>');
    SL.Add('      let currentFilter = "all";');
    SL.Add('      function setFilter(filter, btn) {');
    SL.Add('        currentFilter = filter;');
    SL.Add('        document.querySelectorAll(".filter-btn").forEach(b => b.classList.remove("active"));');
    SL.Add('        if (btn) btn.classList.add("active");');
    SL.Add('        applyFilter();');
    SL.Add('      }');
    SL.Add('      function applyFilter() {');
    SL.Add('        const q = (document.getElementById("reportSearch").value || "").toLowerCase().trim();');
    SL.Add('        const rows = document.querySelectorAll("tr.finding-row");');
    SL.Add('        let visibleCount = 0;');
    SL.Add('        rows.forEach(row => {');
    SL.Add('          const sev = row.getAttribute("data-severity");');
    SL.Add('          const text = row.getAttribute("data-search") || "";');
    SL.Add('          const filterMatch = (currentFilter === "all") || (sev === currentFilter);');
    SL.Add('          const searchMatch = (q === "") || (text.indexOf(q) !== -1);');
    SL.Add('          if (filterMatch && searchMatch) {');
    SL.Add('            row.style.display = "";');
    SL.Add('            visibleCount++;');
    SL.Add('          } else {');
    SL.Add('            row.style.display = "none";');
    SL.Add('          }');
    SL.Add('        });');
    SL.Add('        const noMatch = document.getElementById("noFilterMatches");');
    SL.Add('        if (noMatch) noMatch.style.display = (visibleCount === 0 && rows.length > 0) ? "" : "none";');
    SL.Add('      }');
    SL.Add('    </script>');

    SL.Add(Format('    <div class="footer">Generated by STIMAGE &bull; Atari ST Anti-Virus Protection &amp; Guard &bull; %s</div>', [Report.ScanDateStr]));
    SL.Add('  </div>');
    SL.Add('</body>');
    SL.Add('</html>');

    SL.SaveToFile(OutFileName);
    Result := True;
  finally
    SL.Free;
  end;
end;

function ExportVirusReportToCSV(const Report: TVirusReport; const OutFileName: string): Boolean;
var
  SL: TStringList;
  I: Integer;
  Item: TVirusReportItem;
  ModeStr: string;

  function CsvEscape(const S: string): string;
  begin
    Result := '"' + StringReplace(S, '"', '""', [rfReplaceAll]) + '"';
  end;

begin
  Result := False;
  SL := TStringList.Create;
  try
    if Report.IsFolderScan then
      ModeStr := 'Folder Scan'
    else if Report.BootOnly then
      ModeStr := 'Single Disk (Boot Only)'
    else
      ModeStr := 'Single Disk (Full)';

    SL.Add('# STIMAGE Atari ST Anti-Virus Scan Report');
    SL.Add(Format('# Generated: %s', [Report.ScanDateStr]));
    SL.Add(Format('# Target: %s (%s)', [Report.TargetName, Report.TargetPath]));
    SL.Add(Format('# Mode: %s | Overall Verdict: %s', [ModeStr, Report.OverallVerdict]));
    SL.Add(Format('# Summary: Images=%d, Files=%d, Threats=%d, Suspicious=%d, Protectors=%d, Cleaned=%d',
      [Report.TotalImagesScanned, Report.TotalFilesScanned, Report.ThreatCount,
       Report.PotentialCount, Report.ProtectorCount, Report.CleanedCount]));
    SL.Add('"Source File","Target","Threat Name","Category","Status","Offset","Cluster","File Size","Can Disinfect","Disinfected","Details"');

    for I := 0 to High(Report.Items) do
    begin
      Item := Report.Items[I];
      SL.Add(Format('%s,%s,%s,%s,%s,%d,%d,%d,%s,%s,%s', [
        CsvEscape(Item.SourceFile),
        CsvEscape(Item.Target),
        CsvEscape(Item.ThreatName),
        CsvEscape(Item.Category),
        CsvEscape(Item.Status),
        Item.Offset,
        Item.Cluster,
        Item.FileSize,
        BoolToStr(Item.CanDisinfect, 'True', 'False'),
        BoolToStr(Item.Disinfected, 'True', 'False'),
        CsvEscape(Item.Description)
      ]));
    end;

    SL.SaveToFile(OutFileName);
    Result := True;
  finally
    SL.Free;
  end;
end;

function ExportVirusReportToJSON(const Report: TVirusReport; const OutFileName: string): Boolean;
var
  SL: TStringList;
  I: Integer;
  Item: TVirusReportItem;
  Comma, ModeStr: string;
begin
  Result := False;
  SL := TStringList.Create;
  try
    if Report.IsFolderScan then
      ModeStr := 'Folder Scan'
    else if Report.BootOnly then
      ModeStr := 'Single Disk (Boot Only)'
    else
      ModeStr := 'Single Disk (Full)';

    SL.Add('{');
    SL.Add('  "scanner": {');
    SL.Add(Format('    "title": "%s",', [EscapeJSON(Report.Title)]));
    SL.Add(Format('    "target_name": "%s",', [EscapeJSON(Report.TargetName)]));
    SL.Add(Format('    "target_path": "%s",', [EscapeJSON(Report.TargetPath)]));
    SL.Add(Format('    "scan_date": "%s",', [Report.ScanDateStr]));
    SL.Add(Format('    "scan_mode": "%s",', [EscapeJSON(ModeStr)]));
    SL.Add(Format('    "is_folder_scan": %s,', [BoolToStr(Report.IsFolderScan, 'true', 'false')]));
    SL.Add(Format('    "boot_only": %s,', [BoolToStr(Report.BootOnly, 'true', 'false')]));
    SL.Add(Format('    "total_images_scanned": %d,', [Report.TotalImagesScanned]));
    SL.Add(Format('    "total_files_scanned": %d,', [Report.TotalFilesScanned]));
    SL.Add(Format('    "total_folders_scanned": %d,', [Report.TotalFoldersScanned]));
    SL.Add(Format('    "threat_count": %d,', [Report.ThreatCount]));
    SL.Add(Format('    "potential_count": %d,', [Report.PotentialCount]));
    SL.Add(Format('    "protector_count": %d,', [Report.ProtectorCount]));
    SL.Add(Format('    "custom_boot_count": %d,', [Report.CustomBootCount]));
    SL.Add(Format('    "cleaned_count": %d,', [Report.CleanedCount]));
    SL.Add(Format('    "overall_verdict": "%s",', [EscapeJSON(Report.OverallVerdict)]));
    SL.Add(Format('    "boot_checksum": "%s",', [Report.BootChecksumStr]));
    SL.Add(Format('    "boot_executable": %s,', [BoolToStr(Report.BootIsExecutable, 'true', 'false')]));
    SL.Add(Format('    "boot_status": "%s"', [EscapeJSON(Report.BootStatusStr)]));
    SL.Add('  },');
    SL.Add('  "findings": [');

    for I := 0 to High(Report.Items) do
    begin
      Item := Report.Items[I];
      if I = High(Report.Items) then Comma := '' else Comma := ',';
      SL.Add('    {');
      SL.Add(Format('      "source_file": "%s",', [EscapeJSON(Item.SourceFile)]));
      SL.Add(Format('      "target": "%s",', [EscapeJSON(Item.Target)]));
      SL.Add(Format('      "threat_name": "%s",', [EscapeJSON(Item.ThreatName)]));
      SL.Add(Format('      "category": "%s",', [EscapeJSON(Item.Category)]));
      SL.Add(Format('      "status": "%s",', [EscapeJSON(Item.Status)]));
      SL.Add(Format('      "severity": "%s",', [EscapeJSON(Item.Severity)]));
      SL.Add(Format('      "offset": %d,', [Item.Offset]));
      SL.Add(Format('      "cluster": %d,', [Item.Cluster]));
      SL.Add(Format('      "file_size": %d,', [Item.FileSize]));
      SL.Add(Format('      "can_disinfect": %s,', [BoolToStr(Item.CanDisinfect, 'true', 'false')]));
      SL.Add(Format('      "disinfected": %s,', [BoolToStr(Item.Disinfected, 'true', 'false')]));
      SL.Add(Format('      "description": "%s"', [EscapeJSON(Item.Description)]));
      SL.Add('    }' + Comma);
    end;

    SL.Add('  ]');
    SL.Add('}');

    SL.SaveToFile(OutFileName);
    Result := True;
  finally
    SL.Free;
  end;
end;

function ExportVirusReportToText(const Report: TVirusReport; const OutFileName: string): Boolean;
var
  SL: TStringList;
  I: Integer;
  Item: TVirusReportItem;
  ModeStr, SourceCol, TargetCol, ThreatCol, CatCol, StatusCol, LocCol: string;
begin
  Result := False;
  SL := TStringList.Create;
  try
    if Report.IsFolderScan then
      ModeStr := 'Folder / Directory Scan'
    else if Report.BootOnly then
      ModeStr := 'Single Disk Scan (Boot Sector Only)'
    else
      ModeStr := 'Single Disk Scan (Full Filesystem & Boot Sector)';

    SL.Add('========================================================================================================================');
    SL.Add(' STIMAGE ATARI ST ANTI-VIRUS SCANNER REPORT');
    SL.Add('========================================================================================================================');
    SL.Add(Format(' Target Name       : %s', [Report.TargetName]));
    SL.Add(Format(' Target Path       : %s', [Report.TargetPath]));
    SL.Add(Format(' Scan Date & Time  : %s', [Report.ScanDateStr]));
    SL.Add(Format(' Scan Mode         : %s', [ModeStr]));
    SL.Add(Format(' Overall Verdict   : %s', [Report.OverallVerdict]));
    SL.Add(Format(' Images Processed  : %d', [Report.TotalImagesScanned]));
    SL.Add(Format(' Files Scanned     : %d', [Report.TotalFilesScanned]));
    SL.Add(Format(' Malware Threats   : %d', [Report.ThreatCount]));
    SL.Add(Format(' Suspicious Hooks  : %d', [Report.PotentialCount]));
    SL.Add(Format(' Boot Protectors   : %d', [Report.ProtectorCount]));
    SL.Add(Format(' Cleaned Items     : %d', [Report.CleanedCount]));
    if not Report.IsFolderScan then
      SL.Add(Format(' Boot Sector Check : %s (%s)', [Report.BootChecksumStr, Report.BootStatusStr]));
    SL.Add('========================================================================================================================');
    SL.Add(' FINDINGS & MALWARE DETECTION LOG');
    SL.Add('========================================================================================================================');
    SL.Add(' Scanned File           Target                 Threat Name            Category         Status             Location');
    SL.Add('------------------------------------------------------------------------------------------------------------------------');

    if Length(Report.Items) = 0 then
    begin
      SL.Add(' [CLEAN] No virus threats, suspicious heuristic hooks, or infected files were detected.');
    end
    else
    begin
      for I := 0 to High(Report.Items) do
      begin
        Item := Report.Items[I];

        SourceCol := Item.SourceFile;
        if Length(SourceCol) > 22 then
          SourceCol := Copy(SourceCol, 1, 19) + '...'
        else
          SourceCol := PadRight(SourceCol, 22);

        TargetCol := Item.Target;
        if Length(TargetCol) > 21 then
          TargetCol := Copy(TargetCol, 1, 18) + '...'
        else
          TargetCol := PadRight(TargetCol, 21);

        ThreatCol := Item.ThreatName;
        if Length(ThreatCol) > 22 then
          ThreatCol := Copy(ThreatCol, 1, 19) + '...'
        else
          ThreatCol := PadRight(ThreatCol, 22);

        CatCol := PadRight(Item.Category, 16);
        StatusCol := PadRight(Item.Status, 18);

        if Item.Cluster >= 2 then
          LocCol := Format('Clust #%d', [Item.Cluster])
        else if Item.Offset >= 0 then
          LocCol := Format('+$%X', [Item.Offset])
        else
          LocCol := '-';

        SL.Add(Format(' %s %s %s %s %s %s', [SourceCol, TargetCol, ThreatCol, CatCol, StatusCol, LocCol]));
        if Item.Description <> '' then
          SL.Add(Format('   -> Details: %s', [Item.Description]));
      end;
    end;

    SL.Add('========================================================================================================================');
    SL.Add(Format(' End of Virus Scan Report: %d Finding(s) Logged', [Length(Report.Items)]));
    SL.Add('========================================================================================================================');

    SL.SaveToFile(OutFileName);
    Result := True;
  finally
    SL.Free;
  end;
end;

function ExportVirusReportToFile(const Report: TVirusReport; const OutFileName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(OutFileName));
  if (Ext = '.html') or (Ext = '.htm') then
    Result := ExportVirusReportToHTML(Report, OutFileName)
  else if Ext = '.csv' then
    Result := ExportVirusReportToCSV(Report, OutFileName)
  else if Ext = '.json' then
    Result := ExportVirusReportToJSON(Report, OutFileName)
  else
    Result := ExportVirusReportToText(Report, OutFileName);
end;

function ExportVirusReportToFile(const ASummary: TDiskScanSummary; const ATargetPath, OutFileName: string;
  AIsFolderScan: Boolean = False; ABootOnly: Boolean = False; ATotalImagesProcessed: Integer = 0): Boolean;
var
  Rep: TVirusReport;
begin
  Rep := BuildVirusReport(ASummary, ATargetPath, AIsFolderScan, ABootOnly, ATotalImagesProcessed);
  Result := ExportVirusReportToFile(Rep, OutFileName);
end;

end.
