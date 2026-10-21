unit unit_virusscanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Math;

type
  TVirusTargetType = (vtBootSector, vtFile, vtProtector, vtCustomBoot, vtNone);

  TVirusScanResult = record
    VirusFound: Boolean;
    TargetType: TVirusTargetType;
    Name: string;
    Description: string;
    Target: string;       // e.g. "Boot Sector (Sector 0)" or "File: \AUTO\MEDWAY.PRG"
    SourceFile: string;   // Scanned file name / archive member
    ActualImagePath: string; // Full path on disk (if standalone)
    ZipArchivePath: string;  // Non-empty if inside zip
    ZipEntryName: string;    // Entry path inside zip
    Offset: Integer;      // Byte offset within sector or file
    CanDisinfect: Boolean;
    Cluster: Word;        // If file, starting cluster
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

  TScanProgressEvent = procedure(const CurrentItem: string; Percent: Integer; var Abort: Boolean) of object;

  { TFormVirusScanner }
  TFormVirusScanner = class(TForm)
    PanelTop: TPanel;
    LabelTitle: TLabel;
    LabelDiskPath: TLabel;
    LabelBootStatus: TLabel;
    LabelScanStatus: TLabel;
    CheckBoxBootOnly: TCheckBox;
    ListViewThreats: TListView;
    PanelBottom: TPanel;
    ProgressBarScan: TProgressBar;
    ButtonScan: TButton;
    ButtonScanFolder: TButton;
    ButtonCancelScan: TButton;
    ButtonDisinfectBoot: TButton;
    ButtonDisinfectAll: TButton;
    ButtonInspectHex: TButton;
    ButtonClose: TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ButtonScanClick(Sender: TObject);
    procedure ButtonScanFolderClick(Sender: TObject);
    procedure ButtonCancelScanClick(Sender: TObject);
    procedure ButtonDisinfectBootClick(Sender: TObject);
    procedure ButtonDisinfectAllClick(Sender: TObject);
    procedure ButtonInspectHexClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ListViewThreatsCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ListViewThreatsCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ListViewThreatsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    FCurrentDiskPath: string;
    FSummary: TDiskScanSummary;
    FCancelScan: Boolean;
    FFolderScanMode: Boolean;
    FCurrentFileIndex: Integer;
    FTotalFilesToScan: Integer;
    procedure UpdateLayout;
    procedure UpdateUI;
    procedure OnScanProgress(const CurrentItem: string; Percent: Integer; var Abort: Boolean);
    procedure PopulateThreatsList;
    procedure AddThreatResult(const AThreat: TVirusScanResult);
  public
    procedure OpenForDisk(const ADiskPath: string; AutoScan: Boolean = True);
  end;

var
  FormVirusScanner: TFormVirusScanner;

// Engine Functions
function CalculateBootChecksum(const Sector: PByte; Size: Integer = 512): Word;
function HasFloppyWriteRoutine(const TraitsOrDescription: string): Boolean; overload;
function HasFloppyWriteRoutine(const Threat: TVirusScanResult): Boolean; overload;
function AnalyzeBootSectorHeuristics(const SectorBytes: PByte; SectorSize: Integer; out ViralTraits: string): Boolean;
function ScanBootSector(const SectorBytes: PByte; SectorSize: Integer; out Threat: TVirusScanResult): Boolean;
function ScanFileBuffer(const Buffer: PByte; BufLen: Integer; const AFileName: string; ACluster: Word; out Threat: TVirusScanResult): Boolean;
function ScanDiskImage(const AImagePath: string; out Summary: TDiskScanSummary; OnProgress: TScanProgressEvent = nil; BootSectorOnly: Boolean = False): Boolean;
function DisinfectBootSector(const AImagePath: string): Boolean;
function DisinfectFile(const AImagePath: string; const AFileName: string; ACluster: Word): Boolean;
function QuickCheckBootSector(const AImagePath: string; out ThreatName: string; out IsExecutable: Boolean): Boolean;

implementation

uses
  unit_fileviewer, unit_depacker, unit_msa, unit_dim, zipper;

{$R *.lfm}

type
  TMaskedSig = record
    Name: string;
    Description: string;
    Bytes: array of Byte;
    Mask: string;
    FixedOffset: Integer; // -1 for sliding, >= 0 for fixed
    RequiredString: string;
  end;

function BufferContainsString(const Buffer: PByte; BufLen: Integer; const Needle: string): Boolean;
var
  NeedleLen, I, J: Integer;
  Match: Boolean;
begin
  Result := False;
  NeedleLen := Length(Needle);
  if (Buffer = nil) or (BufLen < NeedleLen) or (NeedleLen = 0) then Exit;

  for I := 0 to BufLen - NeedleLen do
  begin
    Match := True;
    for J := 1 to NeedleLen do
    begin
      if Buffer[I + J - 1] <> Byte(Needle[J]) then
      begin
        Match := False;
        Break;
      end;
    end;
    if Match then Exit(True);
  end;
end;

function BufferContainsText(const Buffer: PByte; BufLen: Integer; const Needle: string): Boolean;
var
  NeedleLen, I, J: Integer;
  Match: Boolean;
  UpNeedle: string;
begin
  Result := False;
  NeedleLen := Length(Needle);
  if (Buffer = nil) or (BufLen < NeedleLen) or (NeedleLen = 0) then Exit;
  UpNeedle := UpperCase(Needle);

  for I := 0 to BufLen - NeedleLen do
  begin
    Match := True;
    for J := 1 to NeedleLen do
    begin
      if UpCase(Char(Buffer[I + J - 1])) <> UpNeedle[J] then
      begin
        Match := False;
        Break;
      end;
    end;
    if Match then Exit(True);
  end;
end;

function FindMaskedPattern(const Buffer: PByte; BufLen: Integer; const Pattern: array of Byte; const Mask: string; FixedOffset: Integer = -1): Integer;
var
  PatLen, StartIdx, EndIdx, I, J: Integer;
  Match: Boolean;
begin
  Result := -1;
  PatLen := Length(Pattern);
  if (Buffer = nil) or (BufLen < PatLen) or (Length(Mask) < PatLen) then Exit;

  if FixedOffset >= 0 then
  begin
    StartIdx := FixedOffset;
    EndIdx := FixedOffset;
    if StartIdx + PatLen > BufLen then Exit;
  end
  else
  begin
    StartIdx := 0;
    EndIdx := BufLen - PatLen;
  end;

  for I := StartIdx to EndIdx do
  begin
    Match := True;
    for J := 0 to PatLen - 1 do
    begin
      if (J < Length(Mask)) and (Mask[J + 1] = 'x') then
      begin
        if Buffer[I + J] <> Pattern[J] then
        begin
          Match := False;
          Break;
        end;
      end;
    end;
    if Match then Exit(I);
  end;
end;

function CalculateBootChecksum(const Sector: PByte; Size: Integer = 512): Word;
var
  Sum: LongWord;
  I: Integer;
  Val: Word;
begin
  Sum := 0;
  if (Sector = nil) or (Size < 512) then Exit(0);
  for I := 0 to 255 do
  begin
    Val := (Word(Sector[I * 2]) shl 8) or Word(Sector[I * 2 + 1]);
    Sum := (Sum + Val) and $FFFF;
  end;
  Result := Word(Sum);
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

function AnalyzeBootSectorHeuristics(const SectorBytes: PByte; SectorSize: Integer; out ViralTraits: string): Boolean;
var
  TraitsList: TStringList;
  I, J, K, LookbackStart: Integer;
  B0, B1: Byte;
  W: Word;
  HasRwabsWrite: Boolean;
begin
  Result := False;
  ViralTraits := '';
  if (SectorBytes = nil) or (SectorSize < 512) then Exit;

  TraitsList := TStringList.Create;
  try
    // 1. Reset vector hooks / magic: $31415926 and $426 / $42A
    if BufferContainsString(SectorBytes, SectorSize, #$31#$41#$59#$26) then
      TraitsList.Add('Reset Magic Key ($31415926 resvalid)');

    for I := 2 to SectorSize - 4 do
    begin
      W := (Word(SectorBytes[I]) shl 8) or Word(SectorBytes[I + 1]);
      if (W = $042A) and (I >= 2) then
      begin
        B0 := SectorBytes[I - 2];
        B1 := SectorBytes[I - 1];
        if ((B0 in [$21, $23, $48, $43]) and ((B1 and $C0) = $C0)) or ((B0 = $04) and (B1 = $78)) then
        begin
          if TraitsList.IndexOf('Reset Vector Hook ($42A resvector)') = -1 then
            TraitsList.Add('Reset Vector Hook ($42A resvector)');
        end;
      end
      else if (W = $0426) and (I >= 2) then
      begin
        B0 := SectorBytes[I - 2];
        B1 := SectorBytes[I - 1];
        if (B0 in [$21, $23, $48, $43]) and ((B1 and $C0) = $C0) then
        begin
          if TraitsList.IndexOf('Reset Valid Hook ($426 resvalid)') = -1 then
            TraitsList.Add('Reset Valid Hook ($426 resvalid)');
        end;
      end;
    end;

    // 2. System Disk Vectors: hdv_rw ($476), hdv_bpb ($472), hdv_boot ($47A), hdv_mediach ($47E)
    for I := 2 to SectorSize - 4 do
    begin
      W := (Word(SectorBytes[I]) shl 8) or Word(SectorBytes[I + 1]);
      if (W = $0476) or (W = $0472) or (W = $047A) or (W = $047E) then
      begin
        B0 := SectorBytes[I - 2];
        B1 := SectorBytes[I - 1];
        if ((B0 in [$20, $22, $24, $26, $28, $2A, $2C, $2E]) and (B1 in [$78, $38, $79, $39])) or
           ((B0 in [$21, $23, $25, $27, $29, $2B, $2D, $2F]) and ((B1 and $C0) in [$C0, $80])) or
           ((B0 = $48) and (B1 = $78)) or ((B0 = $41) and (B1 = $F8)) then
        begin
          case W of
            $0476: if TraitsList.IndexOf('Disk R/W Vector Hook ($476 hdv_rw)') = -1 then TraitsList.Add('Disk R/W Vector Hook ($476 hdv_rw)');
            $0472: if TraitsList.IndexOf('Disk BPB Vector Hook ($472 hdv_bpb)') = -1 then TraitsList.Add('Disk BPB Vector Hook ($472 hdv_bpb)');
            $047A: if TraitsList.IndexOf('Boot Vector Hook ($47A hdv_boot)') = -1 then TraitsList.Add('Boot Vector Hook ($47A hdv_boot)');
            $047E: if TraitsList.IndexOf('Media Change Vector Hook ($47E hdv_mediach)') = -1 then TraitsList.Add('Media Change Vector Hook ($47E hdv_mediach)');
          end;
        end;
      end;
    end;

    // 3. Low OS Trap Vectors: Trap #1 ($84), Trap #13 ($B4), Trap #14 ($B8)
    for I := 2 to SectorSize - 4 do
    begin
      W := (Word(SectorBytes[I]) shl 8) or Word(SectorBytes[I + 1]);
      if (W = $0084) or (W = $00B4) or (W = $00B8) then
      begin
        B0 := SectorBytes[I - 2];
        B1 := SectorBytes[I - 1];
        if ((B0 in [$21, $23]) and ((B1 and $C0) = $C0)) or
           ((B0 in [$20, $22]) and (B1 = $78)) then
        begin
          case W of
            $0084: if TraitsList.IndexOf('GEMDOS Trap Vector Hook ($84 Trap #1)') = -1 then TraitsList.Add('GEMDOS Trap Vector Hook ($84 Trap #1)');
            $00B4: if TraitsList.IndexOf('BIOS Trap Vector Hook ($B4 Trap #13)') = -1 then TraitsList.Add('BIOS Trap Vector Hook ($B4 Trap #13)');
            $00B8: if TraitsList.IndexOf('XBIOS Trap Vector Hook ($B8 Trap #14)') = -1 then TraitsList.Add('XBIOS Trap Vector Hook ($B8 Trap #14)');
          end;
        end;
      end;
    end;

    // 4. Memory Residency: phystop ($42E) and memtop ($436)
    for I := 2 to SectorSize - 4 do
    begin
      W := (Word(SectorBytes[I]) shl 8) or Word(SectorBytes[I + 1]);
      if (W = $042E) or (W = $0436) then
      begin
        B0 := SectorBytes[I - 2];
        B1 := SectorBytes[I - 1];
        if ((B0 in [$20, $22, $24, $21, $23, $91, $93, $90, $92, $04, $53, $51]) and (B1 in [$78, $38, $F8, $B8, $C0, $C8])) or
           ((B0 = $21) and ((B1 and $C0) = $C0)) then
        begin
          if W = $042E then
          begin
            if TraitsList.IndexOf('RAM Top Manipulation ($42E phystop)') = -1 then
              TraitsList.Add('RAM Top Manipulation ($42E phystop)');
          end
          else
          begin
            if TraitsList.IndexOf('User RAM Manipulation ($436 memtop)') = -1 then
              TraitsList.Add('User RAM Manipulation ($436 memtop)');
          end;
        end;
      end;
    end;

    // 5. Replication / Sector Write Mechanisms
    for I := 4 to SectorSize - 2 do
    begin
      // Trap #14 ($4E $4E) - Function opcode must be pushed immediately prior to TRAP #14
      if (SectorBytes[I] = $4E) and (SectorBytes[I + 1] = $4E) then
      begin
        // Check if opcode immediately preceding TRAP #14 is #9 (Flopwr)
        // move.w #9, -(sp) -> $3F $3C $00 $09
        // pea 9.w          -> $48 $78 $00 $09
        // move.w d0, -(sp) with moveq #9, d0 -> $70 $09 $3F $00
        if ((SectorBytes[I - 4] = $3F) and (SectorBytes[I - 3] = $3C) and
            (SectorBytes[I - 2] = $00) and (SectorBytes[I - 1] = $09)) or
           ((SectorBytes[I - 4] = $48) and (SectorBytes[I - 3] = $78) and
            (SectorBytes[I - 2] = $00) and (SectorBytes[I - 1] = $09)) or
           ((I >= 6) and (SectorBytes[I - 4] = $70) and (SectorBytes[I - 3] = $09) and
            (SectorBytes[I - 2] = $3F) and (SectorBytes[I - 1] = $00)) then
        begin
          if TraitsList.IndexOf('Floppy Write Routine (XBIOS 9 Flopwr)') = -1 then
            TraitsList.Add('Floppy Write Routine (XBIOS 9 Flopwr)');
        end
        // Check if opcode immediately preceding TRAP #14 is #18 (Protobt) -> $12
        else if ((SectorBytes[I - 4] = $3F) and (SectorBytes[I - 3] = $3C) and
                 (SectorBytes[I - 2] = $00) and (SectorBytes[I - 1] = $12)) or
                ((SectorBytes[I - 4] = $48) and (SectorBytes[I - 3] = $78) and
                 (SectorBytes[I - 2] = $00) and (SectorBytes[I - 1] = $12)) or
                ((I >= 6) and (SectorBytes[I - 4] = $70) and (SectorBytes[I - 3] = $12) and
                 (SectorBytes[I - 2] = $3F) and (SectorBytes[I - 1] = $00)) then
        begin
          if TraitsList.IndexOf('Boot Sector Creation Routine (XBIOS 18 Protobt)') = -1 then
            TraitsList.Add('Boot Sector Creation Routine (XBIOS 18 Protobt)');
        end;
      end
      // Trap #13 ($4E $4D) - Function opcode #4 (Rwabs) must be pushed immediately prior to TRAP #13
      else if (SectorBytes[I] = $4E) and (SectorBytes[I + 1] = $4D) then
      begin
        // Check if opcode immediately preceding TRAP #13 is #4 (Rwabs)
        if ((SectorBytes[I - 4] = $3F) and (SectorBytes[I - 3] = $3C) and
            (SectorBytes[I - 2] = $00) and (SectorBytes[I - 1] = $04)) or
           ((SectorBytes[I - 4] = $48) and (SectorBytes[I - 3] = $78) and
            (SectorBytes[I - 2] = $00) and (SectorBytes[I - 1] = $04)) then
        begin
          // Check if rw parameter pushed right before opcode #4 is write mode (1 or 3)
          // Case 1: Immediate push: move.w #1/3, -(sp) -> $3F $3C $00 $01 / $03
          LookbackStart := Max(0, I - 16);
          HasRwabsWrite := False;
          for K := LookbackStart to I - 5 do
          begin
            if (SectorBytes[K] = $3F) and (SectorBytes[K + 1] = $3C) and
               (SectorBytes[K + 2] = $00) and (SectorBytes[K + 3] in [$01, $03]) then
            begin
              HasRwabsWrite := True;
              Break;
            end;
          end;

          // Case 2: Register push: move.w Dn, -(sp) -> $3F $00..$3F $07 right before opcode #4
          if not HasRwabsWrite and (I >= 6) and (SectorBytes[I - 6] = $3F) and ((SectorBytes[I - 5] and $F8) = $00) then
          begin
            LookbackStart := Max(0, I - 48);
            for K := LookbackStart to I - 7 do
            begin
              if (SectorBytes[K] in [$70, $72, $74, $76, $78, $7A, $7C, $7E]) and
                 (SectorBytes[K + 1] in [$01, $03]) then
              begin
                HasRwabsWrite := True;
                Break;
              end
              else if (SectorBytes[K] in [$30, $32, $34, $36, $38, $3A, $3C, $3E]) and
                      (SectorBytes[K + 1] = $3C) and (SectorBytes[K + 2] = $00) and
                      (SectorBytes[K + 3] in [$01, $03]) then
              begin
                HasRwabsWrite := True;
                Break;
              end;
            end;
          end;

          if HasRwabsWrite then
          begin
            if TraitsList.IndexOf('Disk Sector Write Routine (BIOS 4 Rwabs Write)') = -1 then
              TraitsList.Add('Disk Sector Write Routine (BIOS 4 Rwabs Write)');
          end;
        end;
      end;
    end;

    // Direct hardware FDC / DMA Floppy Controller write ($FFFF8604 / $FFFF8606)
    for I := 2 to SectorSize - 3 do
    begin
      W := (Word(SectorBytes[I]) shl 8) or Word(SectorBytes[I + 1]);
      if (W = $8604) or (W = $8606) then
      begin
        B0 := SectorBytes[I - 2];
        B1 := SectorBytes[I - 1];
        if ((B0 in [$11, $13, $31, $33, $41]) and ((B1 and $C0) in [$C0, $80])) or
           ((I >= 4) and (SectorBytes[I - 4] in [$31, $33]) and (SectorBytes[I - 3] = $FC)) then
        begin
          if TraitsList.IndexOf('Direct Hardware Floppy Controller Write ($FFFF8604)') = -1 then
            TraitsList.Add('Direct Hardware Floppy Controller Write ($FFFF8604)');
          Break;
        end;
      end;
    end;

    if TraitsList.Count > 0 then
    begin
      ViralTraits := '';
      for I := 0 to TraitsList.Count - 1 do
      begin
        if ViralTraits <> '' then ViralTraits := ViralTraits + '; ';
        ViralTraits := ViralTraits + TraitsList[I];
      end;
      Result := True;
    end;
  finally
    TraitsList.Free;
  end;
end;

function ScanBootSector(const SectorBytes: PByte; SectorSize: Integer; out Threat: TVirusScanResult): Boolean;
var
  BootSum: Word;
  IsExec: Boolean;
  MatchOff: Integer;
  I, K, TargetOff: Integer;
  IsDummy: Boolean;
  ViralTraits: string;
  Sigs: array[0..44] of TMaskedSig;
  SafeSigs: array[0..5] of TMaskedSig;
  GenericSigs: array[0..0] of TMaskedSig;
begin
  Result := False;
  Threat.VirusFound := False;
  Threat.TargetType := vtBootSector;
  Threat.Name := '';
  Threat.Description := '';
  Threat.Target := 'Boot Sector (Sector 0)';
  Threat.SourceFile := '';
  Threat.ActualImagePath := '';
  Threat.ZipArchivePath := '';
  Threat.ZipEntryName := '';
  Threat.Offset := -1;
  Threat.CanDisinfect := True;
  Threat.Cluster := 0;
  Threat.FileSize := 512;
  Threat.Disinfected := False;

  if (SectorBytes = nil) or (SectorSize < 512) then Exit;

  BootSum := CalculateBootChecksum(SectorBytes, 512);
  IsExec := (BootSum = $1234);

  // -------------------------------------------------------------------------
  // TIER 1: Specific Virus Signatures Database (27 Boot Sector Viruses)
  // -------------------------------------------------------------------------

  // 1. ACA Virus
  Sigs[0].Name := 'ACA Virus';
  Sigs[0].Description := 'Classic early resident boot infector that hooks trap vectors and identifies with OEM Label ACA.';
  Sigs[0].Bytes := [$60, $00, $00, $94, $41, $43, $41, $09];
  Sigs[0].Mask := 'xxxxxxxx';
  Sigs[0].FixedOffset := 0;
  Sigs[0].RequiredString := '';

  // 2. Zorro Virus
  Sigs[1].Name := 'Zorro Virus';
  Sigs[1].Description := 'Extremely rare but aggressive vector hijacking boot sector threat.';
  Sigs[1].Bytes := [$EB, $34, $90, $03, $60, $18, $4E, $F9, $C7, $11, $D6, $00];
  Sigs[1].Mask := 'xxxxxxxxxxxx';
  Sigs[1].FixedOffset := 0;
  Sigs[1].RequiredString := '';

  // 3. Freeze Virus
  Sigs[2].Name := 'Freeze Virus';
  Sigs[2].Description := 'Infects and locks up memory-resident pipelines upon cold boots.';
  Sigs[2].Bytes := [$48, $7A, $22, $78, $04, $2E, $93, $FC];
  Sigs[2].Mask := 'xxxxxxxx';
  Sigs[2].FixedOffset := -1;
  Sigs[2].RequiredString := '';

  // 4. CT Virus
  Sigs[3].Name := 'CT Virus';
  Sigs[3].Description := 'Memory resident threat targeting floppy sector operations with typical instruction loops.';
  Sigs[3].Bytes := [$20, $5F, $55, $88, $95, $CA, $22, $6A, $04, $36];
  Sigs[3].Mask := 'xxxxxxxxxx';
  Sigs[3].FixedOffset := -1;
  Sigs[3].RequiredString := '';

  // 5. Cookie Virus
  Sigs[4].Name := 'Cookie Virus';
  Sigs[4].Description := 'Famous resident boot payload demanding a cookie on execution.';
  Sigs[4].Bytes := [$49, $20, $57, $41, $4E, $54, $20, $41, $20, $43, $4F, $4F, $4B, $49, $45, $21]; // "I WANT A COOKIE!"
  Sigs[4].Mask := 'xxxxxxxxxxxxxxxx';
  Sigs[4].FixedOffset := -1;
  Sigs[4].RequiredString := '';

  // 6. Blot Virus
  Sigs[5].Name := 'Blot Virus';
  Sigs[5].Description := 'Destructive boot sector infector that corrupts BIOS parameter metadata.';
  Sigs[5].Bytes := [$12, $12, $34, $56, $21, $48, $00, $04, $31, $7C];
  Sigs[5].Mask := 'xxxxxxxxxx';
  Sigs[5].FixedOffset := -1;
  Sigs[5].RequiredString := '';

  // 7. Finland / Toubab Virus
  Sigs[6].Name := 'Finland / Toubab Virus';
  Sigs[6].Description := 'A variant featuring an inquisitive social greeting signature.';
  Sigs[6].Bytes := [$48, $69, $21, $52, $20, $55, $20, $6E, $69, $63, $65, $3F]; // "Hi!R U nice?"
  Sigs[6].Mask := 'xxxxxxxxxxxx';
  Sigs[6].FixedOffset := -1;
  Sigs[6].RequiredString := '';

  // 8. Directory Waster Virus
  Sigs[7].Name := 'Directory Waster Virus';
  Sigs[7].Description := 'Highly destructive sector threat replicating 14 times before wiping the root directory.';
  Sigs[7].Bytes := [$24, $12, $B4, $91, $67, $34, $41, $FA];
  Sigs[7].Mask := 'xxxxxxxx';
  Sigs[7].FixedOffset := -1;
  Sigs[7].RequiredString := '';

  // 9. Flying Chimp Virus
  Sigs[8].Name := 'Flying Chimp Virus';
  Sigs[8].Description := 'Resident system sector modifier replicating into high system space bounds.';
  Sigs[8].Bytes := [$4E, $5E, $41, $FA, $00, $06, $20, $50, $4E, $D0];
  Sigs[8].Mask := 'xxxxxxxxxx';
  Sigs[8].FixedOffset := -1;
  Sigs[8].RequiredString := '';

  // 10. Evil Nick Virus
  Sigs[9].Name := 'Evil Nick Virus';
  Sigs[9].Description := 'Infector containing a specific string gift message.';
  Sigs[9].Bytes := [$45, $56, $49, $4C, $20, $21, $20, $2D, $20, $41, $20, $47, $69, $66, $74]; // "EVIL ! - A Gift"
  Sigs[9].Mask := 'xxxxxxxxxxxxxxx';
  Sigs[9].FixedOffset := -1;
  Sigs[9].RequiredString := '';

  // 11. Screen Virus
  Sigs[10].Name := 'Screen Virus';
  Sigs[10].Description := 'Visual sector payload modifier hiding within standard boot spaces.';
  Sigs[10].Bytes := [$0C, $6C, $E6, $56, $00, $02, $67, $00];
  Sigs[10].Mask := 'xxxxxxxx';
  Sigs[10].FixedOffset := -1;
  Sigs[10].RequiredString := '';

  // 12. Oli Virus
  Sigs[11].Name := 'Oli Virus';
  Sigs[11].Description := 'System vector hook displaying custom verification installation messages.';
  Sigs[11].Bytes := [$4F, $4C, $49, $2D, $56, $49, $52, $55, $53]; // "OLI-VIRUS"
  Sigs[11].Mask := 'xxxxxxxxx';
  Sigs[11].FixedOffset := -1;
  Sigs[11].RequiredString := '';

  // 13. Muncher / Vire 87 Virus
  Sigs[12].Name := 'Muncher / Vire 87 Virus';
  Sigs[12].Description := 'Vintage 1987 floppy resident boot infector.';
  Sigs[12].Bytes := [$56, $49, $52, $45, $20, $38, $37]; // "VIRE 87"
  Sigs[12].Mask := 'xxxxxxx';
  Sigs[12].FixedOffset := -1;
  Sigs[12].RequiredString := '';

  // 14. Green Goblins Virus
  Sigs[13].Name := 'Green Goblins Virus';
  Sigs[13].Description := 'Retro game compilations sector interrupter displaying playful warnings.';
  Sigs[13].Bytes := [$47, $72, $65, $65, $6E, $20, $47, $6F, $62, $6C, $69, $6E]; // "Green Goblin"
  Sigs[13].Mask := 'xxxxxxxxxxxx';
  Sigs[13].FixedOffset := -1;
  Sigs[13].RequiredString := '';

  // 15. Virus Master
  Sigs[14].Name := 'Virus Master';
  Sigs[14].Description := 'Custom message boot payload targeting standard system operations.';
  Sigs[14].Bytes := [$74, $68, $65, $20, $76, $69, $72, $75, $73, $20, $6D, $61, $73, $74, $65, $72]; // "the virus master"
  Sigs[14].Mask := 'xxxxxxxxxxxxxxxx';
  Sigs[14].FixedOffset := -1;
  Sigs[14].RequiredString := '';

  // 16. Macumba Virus
  Sigs[15].Name := 'Macumba Virus';
  Sigs[15].Description := 'Destructive vector-hooking resident sector threat.';
  Sigs[15].Bytes := [$EB, $34, $90, $05, $60, $18, $4E, $F9, $01, $DE, $5C, $00];
  Sigs[15].Mask := 'xxxxxxxxxxxx';
  Sigs[15].FixedOffset := 0;
  Sigs[15].RequiredString := '';

  // 17. Pirate Trap Virus
  Sigs[16].Name := 'Pirate Trap Virus';
  Sigs[16].Description := 'Classic resident Trojan intercepting BIOS Trap routines with console warning string.';
  Sigs[16].Bytes := [$4E, $4E, $4E, $4E, $91, $04, $AB, $00];
  Sigs[16].Mask := 'xxxxxxxx';
  Sigs[16].FixedOffset := 4;
  Sigs[16].RequiredString := '*** The Pirate Trap ***';

  // 18. Signum Boot Virus
  Sigs[17].Name := 'Signum Boot Virus';
  Sigs[17].Description := 'Highly contagious system trap interceptor vector virus patching system variables.';
  Sigs[17].Bytes := [$41, $FA, $FF, $C4, $22, $79, $00, $00, $04, $C6, $D3, $FC];
  Sigs[17].Mask := 'xxxxxxxxxxxx';
  Sigs[17].FixedOffset := -1;
  Sigs[17].RequiredString := '';

  // 19. Ghost Boot Virus (Mouse)
  Sigs[18].Name := 'Ghost Boot Virus (Mouse)';
  Sigs[18].Description := 'Mouse/Ghost variant targeting memory bounds, intercepts timers, flashes borders.';
  Sigs[18].Bytes := [$48, $E7, $FF, $FE, $41, $FA, $00, $00, $20, $3C];
  Sigs[18].Mask := 'xxxxxx??xx';
  Sigs[18].FixedOffset := -1;
  Sigs[18].RequiredString := '';

  // 20. Ghost Boot Virus (GFA / Mouse)
  Sigs[19].Name := 'Ghost Boot Virus (GFA / Mouse)';
  Sigs[19].Description := 'Prevalent memory-resident boot block infector GFA variant intercepting disk vectors.';
  Sigs[19].Bytes := [$26, $3C, $00, $00, $00, $D6, $43, $F8, $01, $40];
  Sigs[19].Mask := 'xxxxxxxxxx';
  Sigs[19].FixedOffset := -1;
  Sigs[19].RequiredString := '';

  // 21. MAD Virus
  Sigs[20].Name := 'MAD Virus';
  Sigs[20].Description := 'Disruptive resident joke virus that triggers visual distortion and retro music.';
  Sigs[20].Bytes := [$48, $E7, $FF, $FE, $41, $FA, $00, $00, $20, $7C];
  Sigs[20].Mask := 'xxxxxx??xx';
  Sigs[20].FixedOffset := -1;
  Sigs[20].RequiredString := '';

  // 22. MAD Virus (Scrolltext / Repeating)
  Sigs[21].Name := 'MAD Virus (Scrolltext / Repeating)';
  Sigs[21].Description := 'Resident joke virus that flashes borders, rotates screens, and triggers a sound loop.';
  Sigs[21].Bytes := [$E3, $4B, $D5, $3D, $97, $8C];
  Sigs[21].Mask := 'xxxxxx';
  Sigs[21].FixedOffset := -1;
  Sigs[21].RequiredString := '';

  // 23. Signum / BPL Boot Virus Variant
  Sigs[22].Name := 'Signum / BPL Boot Virus Variant';
  Sigs[22].Description := 'Resident system vector and BIOS trap modifier using supervisor stack hijack loader.';
  Sigs[22].Bytes := [$41, $FA, $07, $7A, $43, $FA, $00, $06, $60, $00, $02, $6E];
  Sigs[22].Mask := 'xxxxxxxxxxxx';
  Sigs[22].FixedOffset := -1;
  Sigs[22].RequiredString := '';

  // 24. Kobold #2 Virus
  Sigs[23].Name := 'Kobold #2 Virus';
  Sigs[23].Description := 'Destructive resident boot infector which displays "KOBOLD#2 AKTIV!" and replicates via Rwabs sector 0 write.';
  Sigs[23].Bytes := [$4B, $4F, $42, $4F, $4C, $44, $23, $32, $20, $41, $4B, $54, $49, $56, $21]; // "KOBOLD#2 AKTIV!"
  Sigs[23].Mask := 'xxxxxxxxxxxxxxx';
  Sigs[23].FixedOffset := -1;
  Sigs[23].RequiredString := 'KOBOLD#2';

  // 25. Greenpeace Virus
  Sigs[24].Name := 'Greenpeace Virus';
  Sigs[24].Description := 'Eco-themed resident boot virus that displays scrolltexts and intercepts BIOS calls.';
  Sigs[24].Bytes := [$48, $E7, $FF, $FE, $41, $FA, $00, $4C, $20, $3C];
  Sigs[24].Mask := 'xxxxxxxxxx';
  Sigs[24].FixedOffset := -1;
  Sigs[24].RequiredString := '';

  // 26. SCA Boot Virus
  Sigs[25].Name := 'SCA Boot Virus';
  Sigs[25].Description := 'Classic boot-sector infector originally targeting Amiga architectures, written to ST.';
  Sigs[25].Bytes := [$53, $43, $41, $20, $56, $49, $52, $55, $53]; // "SCA VIRUS"
  Sigs[25].Mask := 'xxxxxxxxx';
  Sigs[25].FixedOffset := -1;
  Sigs[25].RequiredString := '';

  // 27. Medway Boys Boot Virus
  Sigs[26].Name := 'Medway Boys Boot Virus';
  Sigs[26].Description := 'System vector and trap infector developed by the Medway crew, hijacking loader sectors.';
  Sigs[26].Bytes := [$4D, $45, $44, $57, $41, $59, $20, $42, $4F, $59, $53]; // "MEDWAY BOYS"
  Sigs[26].Mask := 'xxxxxxxxxxx';
  Sigs[26].FixedOffset := -1;
  Sigs[26].RequiredString := '';

  // 28. Disk-Doctor / Cva Boot Virus
  Sigs[27].Name := 'Disk-Doctor / Cva Boot Virus';
  Sigs[27].Description := 'Widespread early European boot infector hooking Hdv_bpb vector, often disguised as Disk Doctor.';
  Sigs[27].Bytes := [$60, $1C, $00, $00, $2F, $08, $20, $79, $00, $00, $04, $C6];
  Sigs[27].Mask := 'xxxxxxxxxxxx';
  Sigs[27].FixedOffset := -1;
  Sigs[27].RequiredString := '';

  // 29. Kobold #1 Boot Virus (KVI)
  Sigs[28].Name := 'Kobold #1 Boot Virus (KVI)';
  Sigs[28].Description := 'Precursor to Kobold #2, resident boot sector infector hooking floppy read vectors.';
  Sigs[28].Bytes := [$60, $38, $4B, $4F, $42, $4F, $4C, $44]; // BRA + "KOBOLD"
  Sigs[28].Mask := 'xxxxxxxx';
  Sigs[28].FixedOffset := 0;
  Sigs[28].RequiredString := '';

  // 30. C'est La Vie Boot Virus
  Sigs[29].Name := 'C''est La Vie Boot Virus';
  Sigs[29].Description := 'French resident boot infector displaying "C''est la vie" and hijacking BIOS traps.';
  Sigs[29].Bytes := [$43, $27, $45, $53, $54, $20, $4C, $41, $20, $56, $49, $45]; // "C'EST LA VIE"
  Sigs[29].Mask := 'xxxxxxxxxxxx';
  Sigs[29].FixedOffset := -1;
  Sigs[29].RequiredString := '';

  // 31. NRP Boot Virus (No Real Problem)
  Sigs[30].Name := 'NRP Boot Virus (No Real Problem)';
  Sigs[30].Description := 'Highly prevalent European boot infector displaying "No Real Problem" on activation.';
  Sigs[30].Bytes := [$4E, $52, $50, $20, $56, $49, $52, $55, $53]; // "NRP VIRUS"
  Sigs[30].Mask := 'xxxxxxxxx';
  Sigs[30].FixedOffset := -1;
  Sigs[30].RequiredString := '';

  // 32. Anti-Ghost Boot Virus
  Sigs[31].Name := 'Anti-Ghost Boot Virus';
  Sigs[31].Description := 'Contagious resident boot sector that was intended to eradicate the Ghost virus, but became an infector itself.';
  Sigs[31].Bytes := [$41, $4E, $54, $49, $2D, $47, $48, $4F, $53, $54]; // "ANTI-GHOST"
  Sigs[31].Mask := 'xxxxxxxxxx';
  Sigs[31].FixedOffset := -1;
  Sigs[31].RequiredString := '';

  // 33. Sub-Rosa Boot Virus
  Sigs[32].Name := 'Sub-Rosa Boot Virus';
  Sigs[32].Description := 'French resident boot virus manipulating video base registers ($FFFF8201) and palette colors.';
  Sigs[32].Bytes := [$53, $55, $42, $2D, $52, $4F, $53, $41]; // "SUB-ROSA"
  Sigs[32].Mask := 'xxxxxxxx';
  Sigs[32].FixedOffset := -1;
  Sigs[32].RequiredString := '';

  // 34. Alcool / Alcoolique Boot Virus
  Sigs[33].Name := 'Alcool / Alcoolique Boot Virus';
  Sigs[33].Description := 'French boot sector virus inverting display orientations and colors after 10 disk accesses.';
  Sigs[33].Bytes := [$41, $4C, $43, $4F, $4F, $4C]; // "ALCOOL"
  Sigs[33].Mask := 'xxxxxx';
  Sigs[33].FixedOffset := -1;
  Sigs[33].RequiredString := '';

  // 35. Silly Boot Virus
  Sigs[34].Name := 'Silly Boot Virus';
  Sigs[34].Description := 'Classic 1987 reset-proof resident boot infector hooking Resvector ($42A).';
  Sigs[34].Bytes := [$23, $FC, $00, $00, $00, $00, $00, $00, $04, $2A];
  Sigs[34].Mask := 'xx????xxxx';
  Sigs[34].FixedOffset := -1;
  Sigs[34].RequiredString := 'SILLY';

  // 36. Vax / Vaxina Boot Virus
  Sigs[35].Name := 'Vax / Vaxina Boot Virus';
  Sigs[35].Description := 'Survives warm resets and resides above phystop, hooking floppy read/write operations.';
  Sigs[35].Bytes := [$56, $41, $58, $49, $4E, $41]; // "VAXINA"
  Sigs[35].Mask := 'xxxxxx';
  Sigs[35].FixedOffset := -1;
  Sigs[35].RequiredString := '';

  // 37. Whistle Soundchip Boot Virus
  Sigs[36].Name := 'Whistle Soundchip Boot Virus';
  Sigs[36].Description := 'Resident floppy infector playing audio whistle tones through the Yamaha YM2149 soundchip on seek.';
  Sigs[36].Bytes := [$13, $FC, $00, $07, $00, $FF, $88, $00];
  Sigs[36].Mask := 'xxxxxx??';
  Sigs[36].FixedOffset := -1;
  Sigs[36].RequiredString := 'WHISTLE';

  // 38. Mosquito Boot Virus
  Sigs[37].Name := 'Mosquito Boot Virus';
  Sigs[37].Description := 'Generates persistent high-pitched buzzing through the YM2149 sound generator.';
  Sigs[37].Bytes := [$4D, $4F, $53, $51, $55, $49, $54, $4F]; // "MOSQUITO"
  Sigs[37].Mask := 'xxxxxxxx';
  Sigs[37].FixedOffset := -1;
  Sigs[37].RequiredString := '';

  // 39. Bell Boot Virus
  Sigs[38].Name := 'Bell Boot Virus';
  Sigs[38].Description := 'Rings the console bell on each floppy track seek and spreads to uninfected disks.';
  Sigs[38].Bytes := [$1F, $3C, $00, $07, $4E, $41];
  Sigs[38].Mask := 'xxxxxx';
  Sigs[38].FixedOffset := -1;
  Sigs[38].RequiredString := 'BELL';

  // 40. Laxy 1 Boot Virus
  Sigs[39].Name := 'Laxy 1 Boot Virus';
  Sigs[39].Description := 'Deceptive resident boot infector masquerading as an immunization sector (Immunsektor-Täuschung).';
  Sigs[39].Bytes := [$4C, $41, $58, $59, $20, $31]; // "LAXY 1"
  Sigs[39].Mask := 'xxxxxx';
  Sigs[39].FixedOffset := -1;
  Sigs[39].RequiredString := '';

  // 41. Arnold / Rambo Boot Virus
  Sigs[40].Name := 'Arnold / Rambo Boot Virus';
  Sigs[40].Description := 'Resident boot sector infector spreading across floppy drives A and B on disk access.';
  Sigs[40].Bytes := [$41, $52, $4E, $4F, $4C, $44, $2F, $52, $41, $4D, $42, $4F]; // "ARNOLD/RAMBO"
  Sigs[40].Mask := 'xxxxxxxxxxxx';
  Sigs[40].FixedOffset := -1;
  Sigs[40].RequiredString := '';

  // 42. Screen Darkener Boot Virus (Bildschirmverdunkler)
  Sigs[41].Name := 'Screen Darkener Boot Virus (Bildschirmverdunkler)';
  Sigs[41].Description := 'Resident boot infector activating after 10 disk copies or accesses, darkening the display.';
  Sigs[41].Bytes := [$6C, $64, $41]; // "ldA"
  Sigs[41].Mask := 'xxx';
  Sigs[41].FixedOffset := -1;
  Sigs[41].RequiredString := 'Virus';

  // 43. Memory Corruptor Boot Virus (Speicherstörer)
  Sigs[42].Name := 'Memory Corruptor Boot Virus (Speicherstörer)';
  Sigs[42].Description := 'Resident boot sector infector spreading across drives A and B, disrupting and altering system memory bounds.';
  Sigs[42].Bytes := [$56, $78, $61]; // "Vxa"
  Sigs[42].Mask := 'xxx';
  Sigs[42].FixedOffset := -1;
  Sigs[42].RequiredString := 'Bi';

  // 44. VDU Boot Virus (VDU-Imune)
  Sigs[43].Name := 'VDU Boot Virus (VDU-Imune)';
  Sigs[43].Description := 'Floppy boot infector spreading across drives A and B, erasing standard boot sector loaders.';
  Sigs[43].Bytes := [$44, $65, $6E, $74]; // "Dent"
  Sigs[43].Mask := 'xxxx';
  Sigs[43].FixedOffset := -1;
  Sigs[43].RequiredString := 'gRC';

  // 45. Zoch Boot Virus
  Sigs[44].Name := 'Zoch Boot Virus';
  Sigs[44].Description := 'Resident floppy boot sector infector identified by "ZOCH". Replicates using Protobt (XBIOS 18) and Flopwr (XBIOS 9) via _dskbufp ($4C6).';
  Sigs[44].Bytes := [$0C, $A8, $5A, $4F, $43, $48, $00, $02]; // CMPI.L #'ZOCH', 2(A0)
  Sigs[44].Mask := 'xxxxxxxx';
  Sigs[44].FixedOffset := -1;
  Sigs[44].RequiredString := 'ZOCH';

  // -------------------------------------------------------------------------
  // TIER 1.5: Known Boot Sector Protectors and Vaccines
  // Checked BEFORE virus signatures so immunization stubs are not falsely flagged
  // -------------------------------------------------------------------------
  // Kobold Boot Vaccine / Protector (English & German releases)
  if (BufferContainsText(SectorBytes, 512, 'KOBOLD') and
      (BufferContainsText(SectorBytes, 512, 'PROTECT') or
       BufferContainsText(SectorBytes, 512, 'VACCINE') or
       BufferContainsText(SectorBytes, 512, 'SCHUTZ') or
       BufferContainsText(SectorBytes, 512, 'VIRENSCHUTZ'))) or
     BufferContainsText(SectorBytes, 512, 'KOBOLD-VIRUS PROTECTION') or
     BufferContainsText(SectorBytes, 512, 'KOBOLD-VIRENSCHUTZ') or
     ((SectorBytes[0] = $60) and (SectorBytes[1] = $38) and
      BufferContainsString(SectorBytes, 512, 'KOBOLD') and
      (SectorBytes[$3A] = $48) and (SectorBytes[$3B] = $7A) and
      (SectorBytes[$42] = $4E) and (SectorBytes[$43] = $41) and
      (SectorBytes[$46] = $4E) and (SectorBytes[$47] = $75)) then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'Kobold Vaccine / Boot Protector';
    if BufferContainsText(SectorBytes, 512, 'VIRENSCHUTZ') or BufferContainsText(SectorBytes, 512, 'SCHUTZ') then
      Threat.Description := 'Known benign German vaccine boot sector displaying "*** KOBOLD-Virenschutz ***". Immunizes floppy against Kobold virus infections.'
    else
      Threat.Description := 'Known benign vaccine boot sector displaying "*** KOBOLD-Virus Protection ***". Immunizes floppy against Kobold virus infections.';
    Threat.Offset := 0;
    Exit(True);
  end;

  if BufferContainsText(SectorBytes, 512, 'SAGROTAN') or
     BufferContainsText(SectorBytes, 512, 'KEIN VIRUS') or
     ((SectorBytes[506] = Byte('B')) and (SectorBytes[507] = Byte('o')) and (SectorBytes[508] = Byte('o')) and (SectorBytes[509] = Byte('t'))) then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'Sagrotan Boot Protector';
    Threat.Description := 'Resident boot protector & vaccine by Henrik Alt / ToXis. Protects Sector 0 against unauthorized writes.';
    Threat.Offset := 0;
    Exit(True);
  end;

  if BufferContainsText(SectorBytes, 512, 'ANTIDOTE') then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'Antidote Boot Protector';
    Threat.Description := 'Known memory-resident virus shield by Antidote. Intercepts viral propagation routines on boot.';
    Threat.Offset := 0;
    Exit(True);
  end;

  if BufferContainsText(SectorBytes, 512, 'VKILLER') or BufferContainsText(SectorBytes, 512, 'VACCINE') then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'VKiller Resident Vaccine';
    Threat.Description := 'Known resident boot protection installed by VKiller. Traps write attempts to floppy track 0.';
    Threat.Offset := 0;
    Exit(True);
  end;

  if BufferContainsText(SectorBytes, 512, 'UVK') or BufferContainsText(SectorBytes, 512, 'KARSMAKERS') or BufferContainsText(SectorBytes, 512, 'ULTIMATE VIRUS KILLER') then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'UVK Boot Guard / Vaccine';
    Threat.Description := 'Immunization record or boot guard installed by Richard Karsmakers'' Ultimate Virus Killer.';
    Threat.Offset := 0;
    Exit(True);
  end;

  if BufferContainsText(SectorBytes, 512, 'MEDWAY') and
     (BufferContainsText(SectorBytes, 512, 'PROTECT') or BufferContainsText(SectorBytes, 512, 'KILLER')) then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'Medway Boys Boot Protector';
    Threat.Description := 'Resident boot protection header created by the Medway Boys crew.';
    Threat.Offset := 0;
    Exit(True);
  end;

  if BufferContainsText(SectorBytes, 512, 'CHUBBY') or BufferContainsText(SectorBytes, 512, 'DISK DEFENDER') then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'Chubby Checker Boot Guard';
    Threat.Description := 'Resident boot sector validator / protection utility.';
    Threat.Offset := 0;
    Exit(True);
  end;

  if BufferContainsText(SectorBytes, 512, 'The Little Green Desktop') or
     BufferContainsText(SectorBytes, 512, 'lgd.fatal-design.com') then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'LGD Virus Sentinel / Protector';
    Threat.Description := 'The Little Green Desktop boot guard by TS & x_. Protects boot vectors and scans memory for Kobold #2, Ghost, etc.';
    Threat.Offset := 0;
    Exit(True);
  end;

  if BufferContainsText(SectorBytes, 512, 'D-Bug') or
     BufferContainsText(SectorBytes, 512, 'Cyrano Jones') or
     (BufferContainsText(SectorBytes, 512, 'Anti-Virus') and BufferContainsText(SectorBytes, 512, 'by reset')) then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'D-Bug Boot Protector';
    Threat.Description := 'Resident boot protector coded by Cyrano Jones / D-Bug. Guards boot vectors against viral infection.';
    Threat.Offset := 0;
    Exit(True);
  end;

  if BufferContainsText(SectorBytes, 512, 'FUZION') and
     (BufferContainsText(SectorBytes, 512, 'VIRUS KILLER') or BufferContainsText(SectorBytes, 512, 'VIRUS FOUND') or BufferContainsText(SectorBytes, 512, 'NO VIRUS')) then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'Fuzion Virus Killer';
    Threat.Description := 'Resident boot protector & virus killer created by the Fuzion cracking crew.';
    Threat.Offset := 0;
    Exit(True);
  end;

  if BufferContainsText(SectorBytes, 512, 'KILLER_BOOT') then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'Killer Boot Resident Vaccine';
    Threat.Description := 'Classic resident virus killer boot block scanning memory on startup.';
    Threat.Offset := 0;
    Exit(True);
  end;

  if BufferContainsText(SectorBytes, 512, 'UTILITY BOOT') and
     (BufferContainsText(SectorBytes, 512, 'SECTOR ONE') or BufferContainsText(SectorBytes, 512, 'ODC')) then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'Sector One Utility Boot Sentinel';
    Threat.Description := 'Memory protector and virus sanitizer by ODC / Sector One.';
    Threat.Offset := 0;
    Exit(True);
  end;

  if BufferContainsText(SectorBytes, 512, 'MAGNUM') and
     BufferContainsText(SectorBytes, 512, 'FREE OF VIRUSES') then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'Magnum Anti-Virus Boot Guard';
    Threat.Description := 'Resident boot guard validating system memory and floppy sectors against viruses.';
    Threat.Offset := 0;
    Exit(True);
  end;

  if BufferContainsText(SectorBytes, 512, 'FASTCOPY PRO') or
     (BufferContainsText(SectorBytes, 512, 'GUARDIAN') and BufferContainsText(SectorBytes, 512, 'NOT INFECTED')) or
     (BufferContainsText(SectorBytes, 512, 'Martin Backschat') and BufferContainsText(SectorBytes, 512, 'GUARDIAN')) then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'FastCopy Pro Guardian Vaccine';
    Threat.Description := 'Boot sector guardian & vaccine installed by FastCopy Pro / Martin Backschat.';
    Threat.Offset := 0;
    Exit(True);
  end;

  if BufferContainsText(SectorBytes, 512, 'SPECIAL FX') or
     BufferContainsText(SectorBytes, 512, 'SPECIAL-FX') or
     BufferContainsText(SectorBytes, 512, 'JAM-HOT') or
     (FindMaskedPattern(SectorBytes, 512, [$2F, $38, $04, $4E, $31, $FC, $00, $07, $82, $40], 'xxxxxxxxxx', 30) <> -1) or
     (FindMaskedPattern(SectorBytes, 512, [$2F, $38, $04, $4E, $42, $78, $82, $40, $21, $FC], 'xxxxxxxxxx', 30) <> -1) then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'Special FX / Jam-Hot Menu Sentinel';
    Threat.Description := 'Special FX / Superior scene compilation menu loader and virus-free greeting.';
    Threat.Offset := 0;
    Exit(True);
  end;

  // The Tripple H Killer Vaccine
  if BufferContainsText(SectorBytes, 512, 'TRIPPLE H KILLER') or
     BufferContainsText(SectorBytes, 512, 'TRIPLE H KILLER') or
     (BufferContainsText(SectorBytes, 512, 'MAST') and BufferContainsText(SectorBytes, 512, 'KILLER')) then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'Triple H Virus Killer Vaccine';
    Threat.Description := 'The Tripple H Killer resident boot vaccine by MAST / Triple H. Scans memory on startup and neutralizes boot viruses.';
    Threat.Offset := 0;
    Exit(True);
  end;

  // TDT Altair Anti-Virus / FastCopy III Guardian
  if BufferContainsText(SectorBytes, 512, 'TDT ALTAIR') or
     BufferContainsText(SectorBytes, 512, 'ALTAIR ANTI VIRUS') or
     (BufferContainsText(SectorBytes, 512, 'FastCopy') and BufferContainsText(SectorBytes, 512, 'ANTI VIRUS')) or
     (BufferContainsText(SectorBytes, 512, 'FastCopy') and BufferContainsText(SectorBytes, 512, 'RESET VIRUS')) then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'TDT Altair Anti-Virus Vaccine';
    Threat.Description := 'TDT Altair Anti Virus v3.00 installed via FastCopy III. Resident reset-proof boot vaccine purging memory-resident viruses.';
    Threat.Offset := 0;
    Exit(True);
  end;

  // Sentry Protector II by Eagle
  if BufferContainsText(SectorBytes, 512, 'SENTRY Protector') or
     (BufferContainsText(SectorBytes, 512, 'SENTRY') and BufferContainsText(SectorBytes, 512, 'Eagle')) or
     (BufferContainsText(SectorBytes, 512, 'SENTRY') and BufferContainsText(SectorBytes, 512, 'Virus in memory')) then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'Sentry Protector II Vaccine';
    Threat.Description := 'Sentry Protector II resident boot protector by Eagle. Scans system vectors and memory on boot, offering interactive virus removal.';
    Threat.Offset := 0;
    Exit(True);
  end;

  // Persistence Of Vision (P.O.V.) Virus Detector
  if BufferContainsText(SectorBytes, 512, 'P.O.V. VIRUS DETECTOR') or
     BufferContainsText(SectorBytes, 512, 'POV VIRUS DETECTOR') or
     (BufferContainsText(SectorBytes, 512, 'P.O.V.') and BufferContainsText(SectorBytes, 512, 'VIRUS')) or
     (BufferContainsText(SectorBytes, 512, 'FREE OF VIRUSES') and BufferContainsText(SectorBytes, 512, 'clear vector')) then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'P.O.V. Virus Detector / Sentinel';
    Threat.Description := 'Persistence Of Vision (P.O.V.) resident virus detector. Scans memory on boot for Ghost, Kobold #2, and reset vector tampering.';
    Threat.Offset := 0;
    Exit(True);
  end;

  // CT P.D.L. Anti-Virus (by Troic / CT Public Domain Library)
  if (BufferContainsText(SectorBytes, 512, 'CT P.D.L.') and
      (BufferContainsText(SectorBytes, 512, 'TI-VIRUS') or
       BufferContainsText(SectorBytes, 512, 'VIRUS') or
       BufferContainsText(SectorBytes, 512, 'virus in memory'))) or
     BufferContainsText(SectorBytes, 512, 'There is no virus in memory!') or
     (BufferContainsText(SectorBytes, 512, 'THE GHOST VIRUS IS IN YOUR MEMORY') and
      (BufferContainsText(SectorBytes, 512, 'RESET VECTOR') or BufferContainsText(SectorBytes, 512, 'R.S.T V.CTOR') or
       BufferContainsText(SectorBytes, 512, 'CT P.D.L.') or BufferContainsText(SectorBytes, 512, 'TROIC'))) then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'CT P.D.L. Anti-Virus Sentinel';
    Threat.Description := 'CT Public Domain Library resident anti-virus boot protector (coded by Troic). Checks memory for Ghost virus and reset vector tampering.';
    Threat.Offset := 0;
    Exit(True);
  end;

  // Anarchy's Virus Checker (by Anarchy demogroup)
  if BufferContainsText(SectorBytes, 512, 'Anarchy''s Virus Checker') or
     (BufferContainsText(SectorBytes, 512, 'Anarchy') and BufferContainsText(SectorBytes, 512, 'Virus Checker')) or
     (BufferContainsText(SectorBytes, 512, 'The Memory is Clean') and BufferContainsText(SectorBytes, 512, 'Ghost')) or
     (BufferContainsText(SectorBytes, 512, 'Reset Resident Code In Memory') and BufferContainsText(SectorBytes, 512, 'Virus')) then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'Anarchy Virus Checker Sentinel';
    Threat.Description := 'Anarchy demogroup resident anti-virus boot protector. Checks memory for Ghost virus and reset-resident code.';
    Threat.Offset := 0;
    Exit(True);
  end;

  // ST Guardian by Merlin (The Silicon Sorceror / MUG U.K. SVK)
  if BufferContainsText(SectorBytes, 512, 'ST Guardian') or
     BufferContainsText(SectorBytes, 512, 'ST GUARDIAN') or
     (BufferContainsText(SectorBytes, 512, 'Guardian') and BufferContainsText(SectorBytes, 512, 'Merlin')) then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'ST Guardian Boot Protector';
    Threat.Description := 'Resident boot protector and vaccine by Merlin (The Silicon Sorceror). Protects memory and boot sectors against infection.';
    Threat.Offset := 0;
    Exit(True);
  end;

  // GFA IMMUN by Donald P. Maple (Early GFA-BASIC Resident Vaccine)
  if BufferContainsText(SectorBytes, 512, 'GFA IMMUN') or
     (BufferContainsText(SectorBytes, 512, 'IMMUN') and BufferContainsText(SectorBytes, 512, 'Donald')) or
     BufferContainsText(SectorBytes, 512, 'Donald P. Maple') or
     BufferContainsText(SectorBytes, 512, 'Donald Maple') then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'GFA Immun Vaccine (Donald Maple)';
    Threat.Description := 'Resident floppy immunization and protector by Donald P. Maple. Prevents bootsector infection on the Atari ST.';
    Threat.Offset := 0;
    Exit(True);
  end;

  // Anti-Viren Kit II & M. Paulys Anti Virus (German Protectors)
  if BufferContainsText(SectorBytes, 512, 'KEIN BOOTSEKTORVIRUS') or
     BufferContainsText(SectorBytes, 512, 'Anti Viren Kit') or
     BufferContainsText(SectorBytes, 512, 'Kein Bootvirus vorhanden') or
     BufferContainsText(SectorBytes, 512, 'M. Paulys') or
     BufferContainsText(SectorBytes, 512, 'M.Paulys') then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'Anti-Viren Kit / M. Paulys Vaccine';
    Threat.Description := 'German resident boot vaccine (Anti-Viren Kit / M. Paulys). Validates boot sector integrity and blocks viral writes.';
    Threat.Offset := 0;
    Exit(True);
  end;

  // Virus-Report & LSD Anti-Virus 1.1
  if BufferContainsText(SectorBytes, 512, 'Virus-Report') or
     BufferContainsText(SectorBytes, 512, 'Virus Report') or
     BufferContainsText(SectorBytes, 512, 'LSD Anti Virus') or
     BufferContainsText(SectorBytes, 512, 'LSD Anti-Virus') or
     BufferContainsText(SectorBytes, 512, 'Neuter Booter') then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'Virus-Report / LSD Anti-Virus Sentinel';
    Threat.Description := 'Resident boot sentinel and immunization header (Virus-Report / LSD / Neuter Booter).';
    Threat.Offset := 0;
    Exit(True);
  end;

  // MUG U.K. Super Virus Killer Vaccine & NeoChrome Booter
  if (BufferContainsText(SectorBytes, 512, 'MUG U.K') or BufferContainsText(SectorBytes, 512, 'MUG UK')) and
     (BufferContainsText(SectorBytes, 512, 'NeoChrome') or BufferContainsText(SectorBytes, 512, 'Immunizer') or
      BufferContainsText(SectorBytes, 512, 'Super Virus') or BufferContainsText(SectorBytes, 512, 'Killer')) then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'MUG U.K. Super Virus Killer Vaccine';
    Threat.Description := 'Resident vaccine and booter installed by MUG U.K.''s Super Virus Killer.';
    Threat.Offset := 0;
    Exit(True);
  end;

  // M.S. Powell's Vkill Bootsector
  if BufferContainsText(SectorBytes, 512, 'Vkill') or
     (BufferContainsText(SectorBytes, 512, 'Powell') and BufferContainsText(SectorBytes, 512, 'Kill')) then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'M.S. Powell Vkill Vaccine';
    Threat.Description := 'Resident boot protection vaccine coded by M.S. Powell.';
    Threat.Offset := 0;
    Exit(True);
  end;

  // Boot-Viren Killer V1.0 by Marcus Klocke-Sewing (Scheibenkleister)
  if BufferContainsText(SectorBytes, 512, 'Boot-Viren Killer') or
     BufferContainsText(SectorBytes, 512, 'Boot-Viren-Killer') or
     BufferContainsText(SectorBytes, 512, 'Klocke-Sewing') or
     (BufferContainsText(SectorBytes, 512, 'geheilt!') and BufferContainsText(SectorBytes, 512, 'Boot-Viren')) or
     (BufferContainsText(SectorBytes, 512, 'geschützt') and BufferContainsText(SectorBytes, 512, 'Boot-Viren')) or
     (BufferContainsText(SectorBytes, 512, 'geschtzt') and BufferContainsText(SectorBytes, 512, 'Boot-Viren')) then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'Boot-Viren Killer Vaccine (Scheibenkleister)';
    Threat.Description := 'Resident boot protector and vaccine by Marcus Klocke-Sewing from the book "Scheibenkleister" (Claus Brod & Anton Stepper).';
    Threat.Offset := 0;
    Exit(True);
  end;

  // Virentod Boot Vaccine
  if BufferContainsText(SectorBytes, 512, 'VIRENTOD') then
  begin
    Threat.VirusFound := False;
    Threat.TargetType := vtProtector;
    Threat.Name := 'Virentod Boot Protector / Vaccine';
    Threat.Description := 'German resident boot sector vaccine and protector (Virentod). Immunizes floppy track 0 against unauthorized modifications.';
    Threat.Offset := 0;
    Exit(True);
  end;

  // -------------------------------------------------------------------------
  // Check Specific Virus Signatures Database (44 Boot Sector Viruses)
  // -------------------------------------------------------------------------
  for I := 0 to High(Sigs) do
  begin
    MatchOff := FindMaskedPattern(SectorBytes, 512, Sigs[I].Bytes, Sigs[I].Mask, Sigs[I].FixedOffset);
    if MatchOff <> -1 then
    begin
      if (Sigs[I].RequiredString <> '') and
         not BufferContainsString(SectorBytes, 512, Sigs[I].RequiredString) and
         not BufferContainsText(SectorBytes, 512, Sigs[I].RequiredString) then
        Continue;

      // Disregard Kobold #1 if sector is the Kobold protection vaccine (English or German)
      if (I = 28) and (BufferContainsText(SectorBytes, 512, 'PROTECT') or
                       BufferContainsText(SectorBytes, 512, 'VACCINE') or
                       BufferContainsText(SectorBytes, 512, 'SCHUTZ') or
                       BufferContainsText(SectorBytes, 512, 'VIRENSCHUTZ') or
                       ((SectorBytes[0] = $60) and (SectorBytes[1] = $38) and
                        (SectorBytes[$3A] = $48) and (SectorBytes[$3B] = $7A) and
                        (SectorBytes[$42] = $4E) and (SectorBytes[$43] = $41) and
                        (SectorBytes[$46] = $4E) and (SectorBytes[$47] = $75))) then
        Continue;

      Threat.VirusFound := True;
      Threat.Name := Sigs[I].Name;
      Threat.Description := Sigs[I].Description;
      Threat.Offset := MatchOff;
      Exit(True);
    end;
  end;

  // -------------------------------------------------------------------------
  // TIER 2: Known Safe (Whitelisted) Boot Sectors
  // -------------------------------------------------------------------------
  SafeSigs[0].Name := 'Rob Northen CopyLock';
  SafeSigs[0].Bytes := [$52, $6F, $62, $20, $4E, $6F, $72, $74, $68, $65, $6E]; // "Rob Northen"
  SafeSigs[0].Mask := 'xxxxxxxxxxx';
  SafeSigs[0].FixedOffset := -1;
  SafeSigs[0].RequiredString := '';

  SafeSigs[1].Name := 'FastCopy Formatter';
  SafeSigs[1].Bytes := [$46, $43, $4F, $50, $59]; // "FCOPY"
  SafeSigs[1].Mask := 'xxxxx';
  SafeSigs[1].FixedOffset := -1;
  SafeSigs[1].RequiredString := '';

  SafeSigs[2].Name := 'Automation Menu Loader';
  SafeSigs[2].Bytes := [$41, $55, $54, $4F, $4D, $41, $54, $49, $4F, $4E]; // "AUTOMATION"
  SafeSigs[2].Mask := 'xxxxxxxxxx';
  SafeSigs[2].FixedOffset := -1;
  SafeSigs[2].RequiredString := '';

  SafeSigs[3].Name := 'Benign TOS Boot Sector Jump';
  SafeSigs[3].Bytes := [$60, $00];
  SafeSigs[3].Mask := 'x?';
  SafeSigs[3].FixedOffset := 0;
  SafeSigs[3].RequiredString := '';

  SafeSigs[4].Name := 'Benign DOS Boot Sector Jump (Short)';
  SafeSigs[4].Bytes := [$EB, $00];
  SafeSigs[4].Mask := 'x?';
  SafeSigs[4].FixedOffset := 0;
  SafeSigs[4].RequiredString := '';

  SafeSigs[5].Name := 'Benign DOS Boot Sector Jump (Near)';
  SafeSigs[5].Bytes := [$E9, $00, $00];
  SafeSigs[5].Mask := 'x??';
  SafeSigs[5].FixedOffset := 0;
  SafeSigs[5].RequiredString := '';

  // Specific whitelisted loaders (Rob Northen, FastCopy, Automation)
  for I := 0 to 2 do
  begin
    if FindMaskedPattern(SectorBytes, 512, SafeSigs[I].Bytes, SafeSigs[I].Mask, SafeSigs[I].FixedOffset) <> -1 then
      Exit(False); // Whitelisted and verified safe!
  end;

  // Recognized Atari ST Scene Loaders & Crack Intros (Hotline, Elite, Pompey Pirates, Superior, Cynix, Replicants, Colin Reed)
  if BufferContainsText(SectorBytes, 512, 'HOTLINE') or
     BufferContainsText(SectorBytes, 512, 'ELITE!') or
     BufferContainsText(SectorBytes, 512, 'POMPEY') or
     BufferContainsText(SectorBytes, 512, 'CYNIX') or
     BufferContainsText(SectorBytes, 512, 'SUPERIOR') or
     BufferContainsText(SectorBytes, 512, 'REPLICANTS') or
     BufferContainsText(SectorBytes, 512, 'Colin Reed') or
     BufferContainsText(SectorBytes, 512, 'STANDARD CLEAR AUTOMATION BOOT') or
     BufferContainsText(SectorBytes, 512, 'Fungus The Bogeyman') or
     BufferContainsText(SectorBytes, 512, 'NeoDsk') or
     BufferContainsText(SectorBytes, 512, 'GFA USER') or
     BufferContainsText(SectorBytes, 512, 'desktop in medium resolution') or
     BufferContainsText(SectorBytes, 512, 'HI, REPLICANTS!') or
     BufferContainsText(SectorBytes, 512, 'PHALANX DEMO') or
     BufferContainsText(SectorBytes, 512, 'Aenigmatica') or
     BufferContainsText(SectorBytes, 512, 'Drum-U-Loada') or
     BufferContainsText(SectorBytes, 512, 'TIMELORD') or
     BufferContainsText(SectorBytes, 512, 'freezer.2_0') or
     BufferContainsText(SectorBytes, 512, 'DClick') or
     BufferContainsText(SectorBytes, 512, 'CopySTar') or
     BufferContainsText(SectorBytes, 512, '42 Crew') or
     BufferContainsText(SectorBytes, 512, 'AMIGA DEMO') or
     BufferContainsText(SectorBytes, 512, 'BIG DEMO') or
     BufferContainsText(SectorBytes, 512, 'Whattaheck') or
     BufferContainsText(SectorBytes, 512, 'Union Demo') or
     ((SectorBytes[0] = $60) and (SectorBytes[1] = $38) and BufferContainsString(SectorBytes, 512, 'CUBE')) then
    Exit(False);

  // Generic BRA / JMP instructions are only benign if the sector is NOT executable (checksum <> $1234)
  if not IsExec then
  begin
    for I := 3 to High(SafeSigs) do
    begin
      if FindMaskedPattern(SectorBytes, 512, SafeSigs[I].Bytes, SafeSigs[I].Mask, SafeSigs[I].FixedOffset) <> -1 then
        Exit(False);
    end;
  end;

  // Benign dummy boot stubs: code branches directly to RTS ($4E $75) or starts with RTS
  if (SectorBytes[0] = $4E) and (SectorBytes[1] = $75) then
  begin
    Threat.VirusFound := False;
    Exit(False); // Immediate RTS at offset 0
  end;

  if (SectorBytes[0] = $60) then
  begin
    if SectorBytes[1] <> 0 then
      TargetOff := 2 + ShortInt(SectorBytes[1])
    else
      TargetOff := 4 + SmallInt((Word(SectorBytes[2]) shl 8) or Word(SectorBytes[3]));

    if (TargetOff >= 2) and (TargetOff + 1 < 510) and
       (SectorBytes[TargetOff] = $4E) and (SectorBytes[TargetOff + 1] = $75) then
    begin
      Threat.VirusFound := False;
      Exit(False); // Benign dummy stub: BRA directly to RTS, no code executed
    end;
  end;

  // -------------------------------------------------------------------------
  // TIER 3: Generic Family Signatures
  // -------------------------------------------------------------------------
  GenericSigs[0].Name := 'Ghost / MAD Virus Family (Generic)';
  GenericSigs[0].Description := 'Mutated or obfuscated variant of the Ghost/MAD family using recognizable relocator stub.';
  GenericSigs[0].Bytes := [$48, $E7, $FF, $FE, $41, $FA];
  GenericSigs[0].Mask := 'xxxxxx';
  GenericSigs[0].FixedOffset := -1;
  GenericSigs[0].RequiredString := '';

  for I := 0 to High(GenericSigs) do
  begin
    MatchOff := FindMaskedPattern(SectorBytes, 512, GenericSigs[I].Bytes, GenericSigs[I].Mask, GenericSigs[I].FixedOffset);
    if MatchOff <> -1 then
    begin
      Threat.VirusFound := True;
      Threat.Name := GenericSigs[I].Name;
      Threat.Description := GenericSigs[I].Description;
      Threat.Offset := MatchOff;
      Exit(True);
    end;
  end;

  // -------------------------------------------------------------------------
  // TIER 4: Heuristic Behavioral Analysis for Executable Boot Sectors
  // -------------------------------------------------------------------------
  if IsExec then
  begin
    if AnalyzeBootSectorHeuristics(SectorBytes, 512, ViralTraits) then
    begin
      Threat.VirusFound := True;
      Threat.TargetType := vtBootSector;
      if HasFloppyWriteRoutine(ViralTraits) then
        Threat.Name := 'Suspicious Boot Sector (Floppy Write Routine / Infector)'
      else
        Threat.Name := 'Suspicious Boot Sector (Vector Hijack / Non-Replicating)';
      Threat.Description := 'Behavioral heuristic detection: Executable boot sector contains active viral routines (' +
                            ViralTraits + '). Matches resident boot virus behavior profile.';
      Threat.Offset := 0;
      Exit(True);
    end
    else
    begin
      Threat.VirusFound := False;
      Threat.TargetType := vtCustomBoot;
      Threat.Name := 'Custom Bootloader (Standard Bootable Disk)';
      Threat.Description := 'Executable boot sector (checksum $1234) contains standard non-resident loader code. No vector hooks, memory residency, or replication routines detected.';
      Threat.Offset := 0;
      Exit(True);
    end;
  end;
end;

function ScanFileBuffer(const Buffer: PByte; BufLen: Integer; const AFileName: string; ACluster: Word; out Threat: TVirusScanResult): Boolean;
var
  MatchOff, I: Integer;
  FileSigs: array[0..8] of TMaskedSig;
  DepackedBuf: TBytes;
  PackerSig, DepackPackerName: string;
begin
  Result := False;
  Threat.VirusFound := False;
  Threat.TargetType := vtFile;
  Threat.Name := '';
  Threat.Description := '';
  Threat.Target := 'File: \' + AFileName;
  Threat.SourceFile := '';
  Threat.ActualImagePath := '';
  Threat.ZipArchivePath := '';
  Threat.ZipEntryName := '';
  Threat.Offset := -1;
  Threat.CanDisinfect := True;
  Threat.Cluster := ACluster;
  Threat.FileSize := BufLen;
  Threat.Disinfected := False;

  if (Buffer = nil) or (BufLen <= 0) then Exit;

  // 1. Pluto File Virus
  FileSigs[0].Name := 'Pluto File Virus';
  FileSigs[0].Description := 'Virulent resident parasitical infector targeting assembly executable structures, appending signature PLUTO.';
  FileSigs[0].Bytes := [$50, $4C, $55, $54, $4F, $20, $56, $49, $52, $55, $53]; // "PLUTO VIRUS"
  FileSigs[0].Mask := 'xxxxxxxxxxx';
  FileSigs[0].FixedOffset := -1;

  // 2. Saddam File Virus
  FileSigs[1].Name := 'Saddam File Virus';
  FileSigs[1].Description := 'Destructive file infector appending offensive statements of Saddam Hussein and corrupting system memory.';
  FileSigs[1].Bytes := [$53, $41, $44, $44, $41, $4D, $20, $48, $55, $53, $53, $45, $49, $4E]; // "SADDAM HUSSEIN"
  FileSigs[1].Mask := 'xxxxxxxxxxxxxx';
  FileSigs[1].FixedOffset := -1;

  // 3. Byte Bandit File Infector
  FileSigs[2].Name := 'Byte Bandit File Infector';
  FileSigs[2].Description := 'File-level replication of the harmful Byte Bandit segment sequence.';
  FileSigs[2].Bytes := [$42, $59, $54, $45, $20, $42, $41, $4E, $44, $49, $54]; // "BYTE BANDIT"
  FileSigs[2].Mask := 'xxxxxxxxxxx';
  FileSigs[2].FixedOffset := -1;

  // 4. Ghost Linkvirus (TOS-Link)
  FileSigs[3].Name := 'Ghost Linkvirus (TOS-Link)';
  FileSigs[3].Description := 'File-infecting variant of the Ghost family appending code to GEMDOS .PRG and .TOS executables.';
  FileSigs[3].Bytes := [$47, $48, $4F, $53, $54, $2D, $4C, $49, $4E, $4B]; // "GHOST-LINK"
  FileSigs[3].Mask := 'xxxxxxxxxx';
  FileSigs[3].FixedOffset := -1;

  // 5. Hallo / Hello File Virus
  FileSigs[4].Name := 'Hallo / Hello File Virus';
  FileSigs[4].Description := 'Appends payload to GEMDOS executables and alters the Program Header text segment size.';
  FileSigs[4].Bytes := [$48, $41, $4C, $4C, $4F, $20, $56, $49, $52, $55, $53]; // "HALLO VIRUS"
  FileSigs[4].Mask := 'xxxxxxxxxxx';
  FileSigs[4].FixedOffset := -1;

  // 6. Twister File Virus
  FileSigs[5].Name := 'Twister File Virus';
  FileSigs[5].Description := 'Virulent file linkvirus spreading across executable applications and desk accessories.';
  FileSigs[5].Bytes := [$54, $57, $49, $53, $54, $45, $52, $20, $56, $49, $52, $55, $53]; // "TWISTER VIRUS"
  FileSigs[5].Mask := 'xxxxxxxxxxxxx';
  FileSigs[5].FixedOffset := -1;

  // 7. Spam File Virus
  FileSigs[6].Name := 'Spam File Virus';
  FileSigs[6].Description := 'Targets and patches AUTO folder executables to guarantee automatic execution on boot.';
  FileSigs[6].Bytes := [$53, $50, $41, $4D, $20, $56, $49, $52, $55, $53]; // "SPAM VIRUS"
  FileSigs[6].Mask := 'xxxxxxxxxx';
  FileSigs[6].FixedOffset := -1;

  // 8. Manfred / M-Virus
  FileSigs[7].Name := 'Manfred / M-Virus';
  FileSigs[7].Description := 'Modifies GEMDOS program headers to expand code size and prepend replication routines.';
  FileSigs[7].Bytes := [$4D, $41, $4E, $46, $52, $45, $44, $20, $56, $49, $52, $55, $53]; // "MANFRED VIRUS"
  FileSigs[7].Mask := 'xxxxxxxxxxxxx';
  FileSigs[7].FixedOffset := -1;

  // 9. Signum File Infector
  FileSigs[8].Name := 'Signum File Infector';
  FileSigs[8].Description := 'Companion file infector that replicates alongside the Signum boot sector strain.';
  FileSigs[8].Bytes := [$53, $49, $47, $4E, $55, $4D, $20, $46, $49, $4C, $45]; // "SIGNUM FILE"
  FileSigs[8].Mask := 'xxxxxxxxxxx';
  FileSigs[8].FixedOffset := -1;

  // Scan raw file bytes
  for I := 0 to High(FileSigs) do
  begin
    MatchOff := FindMaskedPattern(Buffer, BufLen, FileSigs[I].Bytes, FileSigs[I].Mask, FileSigs[I].FixedOffset);
    if MatchOff <> -1 then
    begin
      Threat.VirusFound := True;
      Threat.Name := FileSigs[I].Name;
      Threat.Description := FileSigs[I].Description;
      Threat.Offset := MatchOff;
      Exit(True);
    end;
  end;

  // If file is packed, decompress and scan unpacked contents
  try
    PackerSig := DetectPackerSignature(Buffer, BufLen);
    if (BufLen > 32) and IsPackerSupported(PackerSig) then
    begin
      if DepackBuffer(Buffer, BufLen, DepackedBuf, DepackPackerName) and (Length(DepackedBuf) > 0) then
      begin
        for I := 0 to High(FileSigs) do
        begin
          MatchOff := FindMaskedPattern(@DepackedBuf[0], Length(DepackedBuf), FileSigs[I].Bytes, FileSigs[I].Mask, FileSigs[I].FixedOffset);
          if MatchOff <> -1 then
          begin
            Threat.VirusFound := True;
            Threat.Name := FileSigs[I].Name + ' [In Packed Executable]';
            Threat.Description := FileSigs[I].Description + ' (Detected inside decompressed binary payload)';
            Threat.Offset := MatchOff;
            Exit(True);
          end;
        end;
      end;
    end;
  except
    // Safe fallback if depacking encounters invalid bitstream or arithmetic overflow
  end;
end;

function ReadFAT12Entry(const FATBuf: PByte; FATSize: Integer; Cluster: Word): Word;
var
  Off: Integer;
begin
  Off := (Cluster * 3) div 2;
  if (FATBuf = nil) or (Off + 1 >= FATSize) then Exit($FFF);
  if (Cluster and 1) = 0 then
    Result := (Word(FATBuf[Off]) or (Word(FATBuf[Off + 1] and $0F) shl 8))
  else
    Result := ((Word(FATBuf[Off]) shr 4) or (Word(FATBuf[Off + 1]) shl 4));
end;

function IsValidDOSChar(C: Byte): Boolean; inline;
begin
  // Valid printable GEMDOS filename characters: ASCII 33..126 excluding reserved chars
  Result := (C > 32) and (C < 127) and not (Char(C) in ['*', '?', '.', ',', ';', ':', '=', '+', '/', '"', '[', ']', '|', '\', '<', '>']);
end;

function IsValidDOSName(const P: PByte): Boolean;
var
  J: Integer;
  HasNonSpace: Boolean;
  B: Byte;
begin
  HasNonSpace := False;
  for J := 0 to 10 do
  begin
    B := P[J];
    if B = 32 then Continue;
    if not IsValidDOSChar(B) then Exit(False);
    HasNonSpace := True;
  end;
  Result := HasNonSpace;
end;

function ScanDiskImage(const AImagePath: string; out Summary: TDiskScanSummary; OnProgress: TScanProgressEvent = nil; BootSectorOnly: Boolean = False): Boolean;
var
  FS: TFileStream;
  BootSec: array[0..511] of Byte;
  BytesPerSec, SecPerClust, ResSec, NumFATs, RootEntries, SecPerFAT: Word;
  FatStart, RootDirStart, DataStart, RootDirSectors: Int64;
  ClusterBytes: Int64;
  MaxCluster: Word;
  FATBuffer: TBytes;
  BootThreat, FileThreat: TVirusScanResult;
  GlobalVisitedDirs: array of Word;
  FAborted: Boolean;

  procedure AddThreat(const AThreat: TVirusScanResult);
  var
    Len: Integer;
  begin
    Len := Length(Summary.Threats);
    SetLength(Summary.Threats, Len + 1);
    Summary.Threats[Len] := AThreat;
    Summary.Threats[Len].SourceFile := ExtractFileName(AImagePath);
    Summary.Threats[Len].ActualImagePath := AImagePath;
  end;

  function IsDirClusterVisited(C: Word): Boolean;
  var
    V: Integer;
  begin
    for V := 0 to High(GlobalVisitedDirs) do
      if GlobalVisitedDirs[V] = C then Exit(True);
    Exit(False);
  end;

  procedure MarkDirClusterVisited(C: Word);
  var
    Len: Integer;
  begin
    Len := Length(GlobalVisitedDirs);
    SetLength(GlobalVisitedDirs, Len + 1);
    GlobalVisitedDirs[Len] := C;
  end;

  procedure ScanDirectory(StartCluster: Word; const CurrentPath: string; Depth: Integer);
  var
    EntriesBuf: TBytes;
    NumEntries, I, J: Integer;
    EntryOff: Integer;
    Attr: Byte;
    RawName: string;
    FileClust: Word;
    FileSize: LongWord;
    FileBuf: TBytes;
    CurrClust: Word;
    ClustOff, ToRead, BytesRead: Int64;
    VisitedClusters: array of Word;
    AbortScan: Boolean;

    function IsClusterVisited(C: Word): Boolean;
    var
      V: Integer;
    begin
      for V := 0 to High(VisitedClusters) do
        if VisitedClusters[V] = C then Exit(True);
      Exit(False);
    end;

  begin
    if FAborted then Exit;
    if Depth > 8 then Exit;
    if Summary.TotalFoldersScanned >= 256 then Exit;

    // Read directory entries
    if StartCluster = 0 then
    begin
      // Root directory
      NumEntries := RootEntries;
      SetLength(EntriesBuf, NumEntries * 32);
      FS.Position := RootDirStart;
      FS.ReadBuffer(EntriesBuf[0], NumEntries * 32);
    end
    else
    begin
      // Subdirectory
      if (StartCluster < 2) or (StartCluster > MaxCluster) then Exit;
      if IsDirClusterVisited(StartCluster) then Exit;
      MarkDirClusterVisited(StartCluster);

      CurrClust := StartCluster;
      SetLength(EntriesBuf, 0);
      SetLength(VisitedClusters, 0);

      while (CurrClust >= 2) and (CurrClust <= MaxCluster) and not IsClusterVisited(CurrClust) do
      begin
        SetLength(VisitedClusters, Length(VisitedClusters) + 1);
        VisitedClusters[High(VisitedClusters)] := CurrClust;

        ClustOff := DataStart + (Int64(CurrClust) - 2) * ClusterBytes;
        if ClustOff + ClusterBytes <= FS.Size then
        begin
          I := Length(EntriesBuf);
          SetLength(EntriesBuf, I + ClusterBytes);
          FS.Position := ClustOff;
          FS.ReadBuffer(EntriesBuf[I], ClusterBytes);
        end
        else
          Break;

        CurrClust := ReadFAT12Entry(@FATBuffer[0], Length(FATBuffer), CurrClust);
      end;

      // Subdirectory structure validation:
      // In GEMDOS and FAT12, EVERY valid subdirectory MUST start with the '.' entry ($2E)
      // and have the subdirectory attribute ($10).
      if (Length(EntriesBuf) < 32) or (EntriesBuf[0] <> Byte('.')) or ((EntriesBuf[11] and $10) = 0) then
        Exit;

      Inc(Summary.TotalFoldersScanned);
      NumEntries := Length(EntriesBuf) div 32;
    end;

    for I := 0 to NumEntries - 1 do
    begin
      if FAborted then Exit;
      if Summary.TotalFilesScanned >= 2048 then Exit;

      EntryOff := I * 32;
      if (EntryOff + 32 > Length(EntriesBuf)) then Break;
      if (EntriesBuf[EntryOff] = $00) and (StartCluster = 0) then Break;
      if (EntriesBuf[EntryOff] = $00) or (EntriesBuf[EntryOff] = $E5) then Continue;

      // Skip '.' and '..' directory entries
      if EntriesBuf[EntryOff] = Byte('.') then Continue;

      Attr := EntriesBuf[EntryOff + 11];
      // Reserved attribute bits in FAT12/GEMDOS: bits 6 & 7 ($C0) are never used
      if (Attr and $C0) <> 0 then Continue;
      // Volume label: skip
      if (Attr and $08) <> 0 then Continue;

      // Validate 8.3 filename characters
      if not IsValidDOSName(@EntriesBuf[EntryOff]) then Continue;

      // Build 8.3 filename
      RawName := '';
      for J := 0 to 7 do
        if EntriesBuf[EntryOff + J] > 32 then
          RawName := RawName + Char(EntriesBuf[EntryOff + J]);
      if EntriesBuf[EntryOff + 8] > 32 then
      begin
        RawName := RawName + '.';
        for J := 8 to 10 do
          if EntriesBuf[EntryOff + J] > 32 then
            RawName := RawName + Char(EntriesBuf[EntryOff + J]);
      end;

      FileClust := Word(EntriesBuf[EntryOff + 26]) or (Word(EntriesBuf[EntryOff + 27]) shl 8);
      FileSize := LongWord(EntriesBuf[EntryOff + 28]) or
                  (LongWord(EntriesBuf[EntryOff + 29]) shl 8) or
                  (LongWord(EntriesBuf[EntryOff + 30]) shl 16) or
                  (LongWord(EntriesBuf[EntryOff + 31]) shl 24);

      if (Attr and $10) <> 0 then
      begin
        // Recurse into subdirectory
        if (FileClust >= 2) and (FileClust <= MaxCluster) and not IsDirClusterVisited(FileClust) then
          ScanDirectory(FileClust, CurrentPath + '\' + RawName, Depth + 1);
      end
      else
      begin
        // File!
        if (FileClust > MaxCluster) and (FileClust <> 0) then Continue;
        Inc(Summary.TotalFilesScanned);
        if Assigned(OnProgress) then
        begin
          AbortScan := False;
          OnProgress(CurrentPath + '\' + RawName, Min(100, Summary.TotalFilesScanned), AbortScan);
          if AbortScan then
          begin
            FAborted := True;
            Exit;
          end;
        end;

        if (FileSize > 0) and (FileClust >= 2) and (FileClust <= MaxCluster) then
        begin
          // Read full file bytes following cluster chain (safety capped at 8MB)
          FileSize := Min(FileSize, 8 * 1024 * 1024);
          SetLength(FileBuf, FileSize);
          BytesRead := 0;
          CurrClust := FileClust;
          SetLength(VisitedClusters, 0);

          while (CurrClust >= 2) and (CurrClust <= MaxCluster) and (BytesRead < FileSize) and not IsClusterVisited(CurrClust) do
          begin
            SetLength(VisitedClusters, Length(VisitedClusters) + 1);
            VisitedClusters[High(VisitedClusters)] := CurrClust;

            ClustOff := DataStart + (Int64(CurrClust) - 2) * ClusterBytes;
            ToRead := Min(ClusterBytes, Int64(FileSize) - BytesRead);
            if ClustOff + ToRead <= FS.Size then
            begin
              FS.Position := ClustOff;
              FS.ReadBuffer(FileBuf[BytesRead], ToRead);
              Inc(BytesRead, ToRead);
            end
            else
              Break;

            CurrClust := ReadFAT12Entry(@FATBuffer[0], Length(FATBuffer), CurrClust);
          end;

          if (BytesRead > 0) and ScanFileBuffer(@FileBuf[0], BytesRead, CurrentPath + '\' + RawName, FileClust, FileThreat) then
          begin
            AddThreat(FileThreat);
          end;
        end;
      end;
    end;
  end;

begin
  Result := False;
  Summary.DiskImagePath := AImagePath;
  Summary.TotalFilesScanned := 0;
  Summary.TotalFoldersScanned := 0;
  Summary.BootChecksum := 0;
  Summary.BootIsExecutable := False;
  SetLength(Summary.Threats, 0);
  SetLength(GlobalVisitedDirs, 0);
  FAborted := False;

  if not FileExists(AImagePath) then Exit;

  FS := TFileStream.Create(AImagePath, fmOpenRead or fmShareDenyNone);
  try
    if FS.Size < 512 then Exit;

    // 1. Scan Boot Sector
    FS.Position := 0;
    FS.ReadBuffer(BootSec[0], 512);

    Summary.BootChecksum := CalculateBootChecksum(@BootSec[0], 512);
    Summary.BootIsExecutable := (Summary.BootChecksum = $1234);

    if ScanBootSector(@BootSec[0], 512, BootThreat) then
      AddThreat(BootThreat);

    if BootSectorOnly then
    begin
      Result := True;
      Exit;
    end;

    // 2. Parse BPB Geometry
    BytesPerSec := Word(BootSec[$0B]) or (Word(BootSec[$0C]) shl 8);
    if BytesPerSec = 0 then BytesPerSec := 512;
    SecPerClust := BootSec[$0D];
    if SecPerClust = 0 then SecPerClust := 2;
    ResSec := Word(BootSec[$0E]) or (Word(BootSec[$0F]) shl 8);
    if ResSec = 0 then ResSec := 1;
    NumFATs := BootSec[$10];
    if NumFATs = 0 then NumFATs := 2;
    RootEntries := Word(BootSec[$11]) or (Word(BootSec[$12]) shl 8);
    if RootEntries = 0 then RootEntries := 112;
    SecPerFAT := Word(BootSec[$16]) or (Word(BootSec[$17]) shl 8);
    if SecPerFAT = 0 then SecPerFAT := 5;

    FatStart := Int64(ResSec) * BytesPerSec;
    RootDirStart := FatStart + Int64(NumFATs) * SecPerFAT * BytesPerSec;
    RootDirSectors := (Int64(RootEntries) * 32 + (BytesPerSec - 1)) div BytesPerSec;
    DataStart := RootDirStart + RootDirSectors * BytesPerSec;
    ClusterBytes := Int64(SecPerClust) * BytesPerSec;

    // Validate geometry sanity
    if (ClusterBytes <= 0) or (DataStart >= FS.Size) or (FatStart >= FS.Size) then
    begin
      Result := True; // Boot sector scanned, filesystem non-standard
      Exit;
    end;

    MaxCluster := 2 + (FS.Size - DataStart) div ClusterBytes;
    if MaxCluster > 4085 then MaxCluster := 4085;

    // Read First FAT Table into memory
    if (FatStart + Int64(SecPerFAT) * BytesPerSec) <= FS.Size then
    begin
      SetLength(FATBuffer, SecPerFAT * BytesPerSec);
      FS.Position := FatStart;
      FS.ReadBuffer(FATBuffer[0], SecPerFAT * BytesPerSec);

      // Recursively scan disk files
      ScanDirectory(0, '', 0);
    end;

    Result := True;
  finally
    FS.Free;
  end;
end;

function DisinfectBootSector(const AImagePath: string): Boolean;
var
  FS: TFileStream;
  BootSec: array[0..511] of Byte;
  CleanMarker: AnsiString;
  I: Integer;
begin
  Result := False;
  if not FileExists(AImagePath) then Exit;
  FS := TFileStream.Create(AImagePath, fmOpenReadWrite or fmShareDenyNone);
  try
    if FS.Size < 512 then Exit;
    FS.Position := 0;
    FS.ReadBuffer(BootSec[0], 512);

    // Step A: Patch initial entry branch instruction past BPB geometry block
    // $60 $1C is a branch instruction skipping to byte 30.
    BootSec[0] := $60;
    BootSec[1] := $1C;
    BootSec[2] := $00;
    BootSec[3] := $00;

    // Bytes 11..27: BPB geometry is preserved!

    // Step B: Put a safe "RTS" ($4E $75) exit instruction at offset 30.
    BootSec[30] := $4E;
    BootSec[31] := $75;

    // Step C: Nullify virus identifiers to avoid subsequent alerts
    CleanMarker := 'CLEANED DISK';
    for I := 1 to Length(CleanMarker) do
      BootSec[31 + I] := Byte(CleanMarker[I]);

    // Step D: Zero out residual code payload outside the BPB (offsets 44..509)
    for I := 44 to 509 do
      BootSec[I] := $00;

    // Step E: Reset execution checksum word at sector tail (offsets 510-511)
    BootSec[510] := $00;
    BootSec[511] := $00;

    FS.Position := 0;
    FS.WriteBuffer(BootSec[0], 512);
    Result := True;
  finally
    FS.Free;
  end;
end;

function DisinfectFile(const AImagePath: string; const AFileName: string; ACluster: Word): Boolean;
var
  FS: TFileStream;
  BootSec: array[0..511] of Byte;
  BytesPerSec, SecPerClust, ResSec, NumFATs, RootEntries, SecPerFAT: Word;
  FatStart, RootDirStart, DataStart, RootDirSectors, ClusterBytes: Int64;
  FATBuffer: TBytes;
  EntriesBuf, SubEntriesBuf: TBytes;
  NumEntries, I, J, EntryOff, FOff: Integer;
  EntryClust: Word;
  CurrClust, NextClust: Word;
  CleanName: string;
  RawName: string;
  MatchFound: Boolean;
  TotalClusters, C, S, SEntryOff: Integer;
  ClustPos: Int64;
  SubClust: Word;
begin
  Result := False;
  if not FileExists(AImagePath) then Exit;

  CleanName := UpperCase(ExtractFileName(AFileName));
  FS := TFileStream.Create(AImagePath, fmOpenReadWrite or fmShareDenyNone);
  try
    if FS.Size < 512 then Exit;
    FS.Position := 0;
    FS.ReadBuffer(BootSec[0], 512);

    BytesPerSec := Word(BootSec[$0B]) or (Word(BootSec[$0C]) shl 8);
    if BytesPerSec = 0 then BytesPerSec := 512;
    SecPerClust := BootSec[$0D];
    if SecPerClust = 0 then SecPerClust := 2;
    ResSec := Word(BootSec[$0E]) or (Word(BootSec[$0F]) shl 8);
    if ResSec = 0 then ResSec := 1;
    NumFATs := BootSec[$10];
    if NumFATs = 0 then NumFATs := 2;
    RootEntries := Word(BootSec[$11]) or (Word(BootSec[$12]) shl 8);
    if RootEntries = 0 then RootEntries := 112;
    SecPerFAT := Word(BootSec[$16]) or (Word(BootSec[$17]) shl 8);
    if SecPerFAT = 0 then SecPerFAT := 5;

    FatStart := Int64(ResSec) * BytesPerSec;
    RootDirStart := FatStart + Int64(NumFATs) * SecPerFAT * BytesPerSec;
    RootDirSectors := (Int64(RootEntries) * 32 + (BytesPerSec - 1)) div BytesPerSec;
    DataStart := RootDirStart + RootDirSectors * BytesPerSec;
    ClusterBytes := Int64(SecPerClust) * BytesPerSec;

    // Load FAT
    SetLength(FATBuffer, SecPerFAT * BytesPerSec);
    FS.Position := FatStart;
    FS.ReadBuffer(FATBuffer[0], SecPerFAT * BytesPerSec);

    // Read Root directory entries
    NumEntries := RootEntries;
    SetLength(EntriesBuf, NumEntries * 32);
    FS.Position := RootDirStart;
    FS.ReadBuffer(EntriesBuf[0], NumEntries * 32);

    MatchFound := False;
    for I := 0 to NumEntries - 1 do
    begin
      EntryOff := I * 32;
      if EntriesBuf[EntryOff] = $E5 then Continue;
      EntryClust := Word(EntriesBuf[EntryOff + 26]) or (Word(EntriesBuf[EntryOff + 27]) shl 8);

      RawName := '';
      for J := 0 to 7 do
        if EntriesBuf[EntryOff + J] > 32 then
          RawName := RawName + Char(EntriesBuf[EntryOff + J]);
      if EntriesBuf[EntryOff + 8] > 32 then
      begin
        RawName := RawName + '.';
        for J := 8 to 10 do
          if EntriesBuf[EntryOff + J] > 32 then
            RawName := RawName + Char(EntriesBuf[EntryOff + J]);
      end;

      if (ACluster >= 2) and (EntryClust = ACluster) then
        MatchFound := True
      else if (ACluster = 0) and (EntryClust = 0) and (UpperCase(RawName) = CleanName) then
        MatchFound := True;

      if MatchFound then
      begin
        // Mark directory entry as deleted ($E5)
        EntriesBuf[EntryOff] := $E5;
        FS.Position := RootDirStart + EntryOff;
        FS.WriteBuffer(EntriesBuf[EntryOff], 1);
        Break;
      end;
    end;

    // If not found in root and ACluster >= 2, search subdirectory clusters
    if not MatchFound and (ACluster >= 2) and (ClusterBytes > 0) then
    begin
      TotalClusters := (FS.Size - DataStart) div ClusterBytes;
      for C := 0 to TotalClusters - 1 do
      begin
        ClustPos := DataStart + Int64(C) * ClusterBytes;
        if ClustPos + ClusterBytes > FS.Size then Break;
        SetLength(SubEntriesBuf, ClusterBytes);
        FS.Position := ClustPos;
        FS.ReadBuffer(SubEntriesBuf[0], ClusterBytes);
        for S := 0 to (ClusterBytes div 32) - 1 do
        begin
          SEntryOff := S * 32;
          if SubEntriesBuf[SEntryOff] = $E5 then Continue;
          if SubEntriesBuf[SEntryOff] = $00 then Break;
          SubClust := Word(SubEntriesBuf[SEntryOff + 26]) or (Word(SubEntriesBuf[SEntryOff + 27]) shl 8);
          if SubClust = ACluster then
          begin
            SubEntriesBuf[SEntryOff] := $E5;
            FS.Position := ClustPos + SEntryOff;
            FS.WriteBuffer(SubEntriesBuf[SEntryOff], 1);
            MatchFound := True;
            Break;
          end;
        end;
        if MatchFound then Break;
      end;
    end;

    if MatchFound then
    begin
      // Free FAT cluster chain
      CurrClust := ACluster;
      while (CurrClust >= 2) and (CurrClust < $FF8) do
      begin
        NextClust := ReadFAT12Entry(@FATBuffer[0], Length(FATBuffer), CurrClust);

        // Zero FAT12 entry
        FOff := (CurrClust * 3) div 2;
        if (CurrClust and 1) = 0 then
        begin
          FATBuffer[FOff] := $00;
          FATBuffer[FOff + 1] := FATBuffer[FOff + 1] and $F0;
        end
        else
        begin
          FATBuffer[FOff] := FATBuffer[FOff] and $0F;
          FATBuffer[FOff + 1] := $00;
        end;

        CurrClust := NextClust;
      end;

      // Write FAT copies back
      for J := 0 to NumFATs - 1 do
      begin
        FS.Position := FatStart + Int64(J) * SecPerFAT * BytesPerSec;
        FS.WriteBuffer(FATBuffer[0], Length(FATBuffer));
      end;

      Result := True;
    end;
  finally
    FS.Free;
  end;
end;

function QuickCheckBootSector(const AImagePath: string; out ThreatName: string; out IsExecutable: Boolean): Boolean;
var
  FS: TFileStream;
  BootSec: array[0..511] of Byte;
  Threat: TVirusScanResult;
begin
  Result := False;
  ThreatName := '';
  IsExecutable := False;
  if not FileExists(AImagePath) then Exit;
  FS := TFileStream.Create(AImagePath, fmOpenRead or fmShareDenyNone);
  try
    if FS.Size < 512 then Exit;
    FS.Position := 0;
    FS.ReadBuffer(BootSec[0], 512);

    IsExecutable := (CalculateBootChecksum(@BootSec[0], 512) = $1234);
    if ScanBootSector(@BootSec[0], 512, Threat) then
    begin
      // Only treat as threat if it is an actual threat, not a protector/vaccine or standard custom loader
      if (Threat.TargetType in [vtBootSector, vtFile]) and Threat.VirusFound then
      begin
        ThreatName := Threat.Name;
        Result := True;
      end;
    end;
  finally
    FS.Free;
  end;
end;

procedure CollectSupportedFiles(const ADir: string; AList: TStringList; var ACancel: Boolean);
var
  SR: TSearchRec;
  Ext, TargetDir: string;
begin
  if ACancel then Exit;
  TargetDir := IncludeTrailingPathDelimiter(ADir);
  if FindFirst(TargetDir + '*', faAnyFile, SR) = 0 then
  begin
    try
      repeat
        if ACancel then Exit;
        if (SR.Name = '.') or (SR.Name = '..') then Continue;

        if (SR.Attr and faDirectory) <> 0 then
        begin
          CollectSupportedFiles(TargetDir + SR.Name, AList, ACancel);
        end
        else
        begin
          Ext := LowerCase(ExtractFileExt(SR.Name));
          if (Ext = '.st') or (Ext = '.msa') or (Ext = '.dim') or (Ext = '.zip') then
            AList.Add(TargetDir + SR.Name);
        end;
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
  end;
end;

{ TFormVirusScanner }

procedure TFormVirusScanner.UpdateLayout;
var
  FixedWidth, Remainder, FileColWidth, DescColWidth: Integer;
begin
  if ListViewThreats = nil then Exit;

  // Auto-expand columns to fill ListViewThreats without blank space right of Description
  if ListViewThreats.Columns.Count >= 6 then
  begin
    // Sum of standard fixed columns: Target (1) + Threat Name (2) + Category (3) + Action/Status (4)
    FixedWidth := ListViewThreats.Columns[1].Width +
                  ListViewThreats.Columns[2].Width +
                  ListViewThreats.Columns[3].Width +
                  ListViewThreats.Columns[4].Width;

    Remainder := ListViewThreats.ClientWidth - FixedWidth;
    if Remainder < 420 then
    begin
      ListViewThreats.Columns[0].Width := 180;
      ListViewThreats.Columns[5].Width := 240;
    end
    else
    begin
      // Distribute remainder space: approx 35% to Scanned File (min 180), rest to Description (min 240)
      FileColWidth := Round(Remainder * 0.35);
      if FileColWidth < 180 then FileColWidth := 180;
      if (Remainder - FileColWidth) < 240 then FileColWidth := Remainder - 240;
      DescColWidth := Remainder - FileColWidth;
      if DescColWidth < 200 then DescColWidth := 200;

      ListViewThreats.Columns[0].Width := FileColWidth;
      ListViewThreats.Columns[5].Width := DescColWidth;
    end;
  end;
end;

procedure TFormVirusScanner.FormResize(Sender: TObject);
begin
  UpdateLayout;
end;

procedure TFormVirusScanner.FormCreate(Sender: TObject);
begin
  FCurrentDiskPath := '';
  FFolderScanMode := False;
  FCancelScan := False;
  ProgressBarScan.Position := 0;
  ButtonCancelScan.Enabled := False;
  UpdateLayout;
end;

procedure TFormVirusScanner.OpenForDisk(const ADiskPath: string; AutoScan: Boolean);
begin
  FCurrentDiskPath := ADiskPath;
  FFolderScanMode := False;
  FCancelScan := False;
  ButtonCancelScan.Enabled := False;
  ButtonScanFolder.Enabled := True;

  if FCurrentDiskPath <> '' then
  begin
    LabelDiskPath.Caption := 'Disk Image: ' + ExtractFileName(FCurrentDiskPath);
    ButtonScan.Enabled := True;
  end
  else
  begin
    LabelDiskPath.Caption := 'Disk Image: [No Disk Loaded - Ready for Folder Scan]';
    LabelBootStatus.Caption := 'Boot Sector: N/A';
    ButtonScan.Enabled := False;
  end;

  UpdateUI;
  UpdateLayout;
  Show;
  BringToFront;

  if AutoScan and (FCurrentDiskPath <> '') then
    ButtonScanClick(nil);
end;

procedure TFormVirusScanner.UpdateUI;
var
  HasBootThreat, HasFloppyWriteBootThreat, HasCustomBoot, HasAnyThreat, OnlyPotentialThreats: Boolean;
  ThreatCount, ProtectorCount, CustomBootCount: Integer;
  FirstProtectorName: string;
  I: Integer;
begin
  HasBootThreat := False;
  HasFloppyWriteBootThreat := False;
  HasCustomBoot := False;
  HasAnyThreat := False;
  ThreatCount := 0;
  ProtectorCount := 0;
  CustomBootCount := 0;
  FirstProtectorName := '';
  OnlyPotentialThreats := True;

  for I := 0 to High(FSummary.Threats) do
  begin
    if (FSummary.Threats[I].TargetType = vtBootSector) and not FSummary.Threats[I].Disinfected then
    begin
      HasBootThreat := True;
      if HasFloppyWriteRoutine(FSummary.Threats[I]) then
        HasFloppyWriteBootThreat := True;
    end;

    if (FSummary.Threats[I].TargetType in [vtBootSector, vtFile]) and not FSummary.Threats[I].Disinfected then
    begin
      Inc(ThreatCount);
      if (Pos('unrecognized', LowerCase(FSummary.Threats[I].Name)) = 0) and
         ((Pos('suspicious', LowerCase(FSummary.Threats[I].Name)) = 0) or HasFloppyWriteRoutine(FSummary.Threats[I])) then
        OnlyPotentialThreats := False;
    end
    else if FSummary.Threats[I].TargetType = vtProtector then
    begin
      Inc(ProtectorCount);
      if FirstProtectorName = '' then
        FirstProtectorName := FSummary.Threats[I].Name;
    end
    else if FSummary.Threats[I].TargetType = vtCustomBoot then
    begin
      Inc(CustomBootCount);
      HasCustomBoot := True;
    end;
  end;

  HasAnyThreat := (ThreatCount > 0);

  if not FFolderScanMode then
  begin
    if FSummary.BootIsExecutable then
    begin
      if ProtectorCount > 0 then
      begin
        LabelBootStatus.Font.Color := $00804000; // Teal
        LabelBootStatus.Caption := Format('Boot Sector: Checksum $%.4X [PROTECTED / VACCINE: %s]', [FSummary.BootChecksum, FirstProtectorName]);
      end
      else if CustomBootCount > 0 then
      begin
        LabelBootStatus.Font.Color := RGBToColor(0, 100, 160); // Blue
        LabelBootStatus.Caption := Format('Boot Sector: Checksum $%.4X [STANDARD CUSTOM BOOTLOADER]', [FSummary.BootChecksum]);
      end
      else if HasBootThreat then
      begin
        if HasFloppyWriteBootThreat then
        begin
          LabelBootStatus.Font.Color := clRed;
          LabelBootStatus.Caption := Format('Boot Sector: Checksum $%.4X [SUSPICIOUS / INFECTED BOOT: FLOPPY WRITE - RED ALERT]', [FSummary.BootChecksum]);
        end
        else
        begin
          LabelBootStatus.Font.Color := RGBToColor(160, 110, 0); // Dark yellow / amber
          LabelBootStatus.Caption := Format('Boot Sector: Checksum $%.4X [SUSPICIOUS BOOT: VECTOR HOOK - YELLOW CAUTION]', [FSummary.BootChecksum]);
        end;
      end
      else
      begin
        LabelBootStatus.Font.Color := clGreen;
        LabelBootStatus.Caption := Format('Boot Sector: Checksum $%.4X [EXECUTABLE BOOT BLOCK]', [FSummary.BootChecksum]);
      end;
    end
    else
    begin
      LabelBootStatus.Font.Color := clWindowText;
      LabelBootStatus.Caption := Format('Boot Sector: Checksum $%.4X [Non-executable / Clean]', [FSummary.BootChecksum]);
    end;
  end;

  if HasAnyThreat then
  begin
    if OnlyPotentialThreats then
    begin
      LabelScanStatus.Font.Color := RGBToColor(180, 120, 0); // Amber / yellow
      LabelScanStatus.Caption := Format('CAUTION: %d Potential / Non-Replicating Threat(s) Detected!', [ThreatCount]);
    end
    else
    begin
      LabelScanStatus.Font.Color := clRed;
      LabelScanStatus.Caption := Format('ALERT: %d Threat(s) Found!', [ThreatCount]);
    end;
  end
  else if ProtectorCount > 0 then
  begin
    LabelScanStatus.Font.Color := $00804000; // Deep teal / green
    LabelScanStatus.Caption := Format('PROTECTED: %s Active (0 Threats Found)', [FirstProtectorName]);
  end
  else if CustomBootCount > 0 then
  begin
    LabelScanStatus.Font.Color := RGBToColor(0, 100, 160); // Muted navy / blue
    LabelScanStatus.Caption := 'INFORMATIONAL: Standard Bootable Disk (0 Threats Found)';
  end
  else if CheckBoxBootOnly.Checked and (FSummary.DiskImagePath <> '') then
  begin
    LabelScanStatus.Font.Color := clGreen;
    LabelScanStatus.Caption := 'CLEAN: Boot Sector Scanned (No Threats)';
  end
  else if FSummary.TotalFilesScanned > 0 then
  begin
    LabelScanStatus.Font.Color := clGreen;
    LabelScanStatus.Caption := Format('CLEAN: %d Files Scanned (No Threats)', [FSummary.TotalFilesScanned]);
  end
  else
  begin
    LabelScanStatus.Font.Color := clWindowText;
    LabelScanStatus.Caption := 'Ready to scan';
  end;

  if FFolderScanMode then
  begin
    ButtonDisinfectBoot.Enabled := False;
    ButtonDisinfectAll.Enabled := False;
  end
  else
  begin
    ButtonDisinfectBoot.Enabled := HasBootThreat or (ProtectorCount > 0) or (CustomBootCount > 0);
    ButtonDisinfectAll.Enabled := HasAnyThreat;
  end;
  ButtonInspectHex.Enabled := (ListViewThreats.Selected <> nil);
end;

procedure TFormVirusScanner.OnScanProgress(const CurrentItem: string; Percent: Integer; var Abort: Boolean);
begin
  if FFolderScanMode and (FTotalFilesToScan > 0) then
    LabelScanStatus.Caption := Format('Scanning [%d/%d]: %s',
      [FCurrentFileIndex + 1, FTotalFilesToScan, ExtractFileName(CurrentItem)])
  else
    LabelScanStatus.Caption := 'Scanning: ' + ExtractFileName(CurrentItem);
  ProgressBarScan.Position := Percent;
  Application.ProcessMessages;
  Abort := FCancelScan;
end;

procedure TFormVirusScanner.PopulateThreatsList;
var
  I: Integer;
  Item: TListItem;
  CatStr, StatusStr, FileDispName: string;
begin
  ListViewThreats.Items.BeginUpdate;
  try
    ListViewThreats.Items.Clear;
    for I := 0 to High(FSummary.Threats) do
    begin
      Item := ListViewThreats.Items.Add;
      FileDispName := FSummary.Threats[I].SourceFile;
      if FileDispName = '' then
      begin
        if FSummary.DiskImagePath <> '' then
          FileDispName := ExtractFileName(FSummary.DiskImagePath)
        else
          FileDispName := ExtractFileName(FCurrentDiskPath);
      end;
      Item.Caption := FileDispName;
      Item.SubItems.Add(FSummary.Threats[I].Target);
      Item.SubItems.Add(FSummary.Threats[I].Name);

      if FSummary.Threats[I].TargetType = vtBootSector then
        CatStr := 'Boot Sector'
      else if FSummary.Threats[I].TargetType = vtProtector then
        CatStr := 'Protector / Vaccine'
      else if FSummary.Threats[I].TargetType = vtCustomBoot then
        CatStr := 'Custom Bootloader'
      else
        CatStr := 'File Infector';
      Item.SubItems.Add(CatStr);

      if FSummary.Threats[I].TargetType = vtProtector then
        StatusStr := 'Active / Protected'
      else if FSummary.Threats[I].TargetType = vtCustomBoot then
        StatusStr := 'Standard / Bootable'
      else if FSummary.Threats[I].Disinfected then
        StatusStr := 'Cleaned / Safe'
      else if Pos('unrecognized', LowerCase(FSummary.Threats[I].Name)) > 0 then
        StatusStr := 'Potential Threat'
      else if Pos('suspicious', LowerCase(FSummary.Threats[I].Name)) > 0 then
      begin
        if HasFloppyWriteRoutine(FSummary.Threats[I]) then
          StatusStr := 'Suspicious (Floppy Write)'
        else
          StatusStr := 'Suspicious (Vector Hook)';
      end
      else if FSummary.Threats[I].CanDisinfect then
        StatusStr := 'Disinfectable'
      else
        StatusStr := 'Quarantined';
      Item.SubItems.Add(StatusStr);

      Item.SubItems.Add(FSummary.Threats[I].Description);
    end;
  finally
    ListViewThreats.Items.EndUpdate;
  end;
end;

procedure TFormVirusScanner.AddThreatResult(const AThreat: TVirusScanResult);
var
  Idx: Integer;
  Item: TListItem;
  CatStr, StatusStr, FileDispName: string;
begin
  Idx := Length(FSummary.Threats);
  SetLength(FSummary.Threats, Idx + 1);
  FSummary.Threats[Idx] := AThreat;

  FileDispName := AThreat.SourceFile;
  if FileDispName = '' then
  begin
    if FSummary.DiskImagePath <> '' then
      FileDispName := ExtractFileName(FSummary.DiskImagePath)
    else
      FileDispName := ExtractFileName(FCurrentDiskPath);
  end;

  Item := ListViewThreats.Items.Add;
  Item.Caption := FileDispName;
  Item.SubItems.Add(AThreat.Target);
  Item.SubItems.Add(AThreat.Name);

  if AThreat.TargetType = vtBootSector then
    CatStr := 'Boot Sector'
  else if AThreat.TargetType = vtProtector then
    CatStr := 'Protector / Vaccine'
  else if AThreat.TargetType = vtCustomBoot then
    CatStr := 'Custom Bootloader'
  else
    CatStr := 'File Infector';
  Item.SubItems.Add(CatStr);

  if AThreat.TargetType = vtProtector then
    StatusStr := 'Active / Protected'
  else if AThreat.TargetType = vtCustomBoot then
    StatusStr := 'Standard / Bootable'
  else if AThreat.Disinfected then
    StatusStr := 'Cleaned / Safe'
  else if Pos('unrecognized', LowerCase(AThreat.Name)) > 0 then
    StatusStr := 'Potential Threat'
  else if Pos('suspicious', LowerCase(AThreat.Name)) > 0 then
  begin
    if HasFloppyWriteRoutine(AThreat) then
      StatusStr := 'Suspicious (Floppy Write)'
    else
      StatusStr := 'Suspicious (Vector Hook)';
  end
  else if AThreat.CanDisinfect then
    StatusStr := 'Disinfectable'
  else
    StatusStr := 'Quarantined';
  Item.SubItems.Add(StatusStr);

  Item.SubItems.Add(AThreat.Description);
end;

procedure TFormVirusScanner.ButtonScanClick(Sender: TObject);
var
  I: Integer;
begin
  if (FCurrentDiskPath = '') or not FileExists(FCurrentDiskPath) then
  begin
    ShowMessage('No valid disk image is currently loaded to scan.');
    Exit;
  end;

  FFolderScanMode := False;
  FCancelScan := False;
  ButtonScan.Enabled := False;
  ButtonScanFolder.Enabled := False;
  ButtonCancelScan.Enabled := True;
  ButtonDisinfectBoot.Enabled := False;
  ButtonDisinfectAll.Enabled := False;
  ButtonInspectHex.Enabled := False;
  ProgressBarScan.Position := 0;
  LabelScanStatus.Caption := 'Scanning disk...';
  Screen.Cursor := crHourGlass;
  try
    ScanDiskImage(FCurrentDiskPath, FSummary, @OnScanProgress, CheckBoxBootOnly.Checked);
    for I := 0 to High(FSummary.Threats) do
    begin
      FSummary.Threats[I].SourceFile := ExtractFileName(FCurrentDiskPath);
      FSummary.Threats[I].ActualImagePath := FCurrentDiskPath;
    end;
    PopulateThreatsList;
    ProgressBarScan.Position := 100;
    UpdateUI;
  finally
    ButtonScan.Enabled := True;
    ButtonScanFolder.Enabled := True;
    ButtonCancelScan.Enabled := False;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormVirusScanner.ButtonScanFolderClick(Sender: TObject);
var
  Dlg: TSelectDirectoryDialog;
  ChosenDir: string;
  FileList: TStringList;
  FileIdx, EntryIdx, K: Integer;
  ActualThreatCount, PotentialThreatCount, ProtectorCount, CustomBootCount: Integer;
  CurFile, Ext, TempST, TempConvertedST, DiskToScan: string;
  TempSummary: TDiskScanSummary;
  TotalImagesProcessed, TotalFilesInsideImages: Integer;
  UnZip, UnZipSingle: TUnZipper;
  ZipEntryName, ZipEntryExt, TempZipStageDir, ExtractedFile: string;
begin
  Dlg := TSelectDirectoryDialog.Create(nil);
  try
    Dlg.Title := 'Select Directory to Scan for Atari ST Disk Images & Archives';
    if not Dlg.Execute then Exit;
    ChosenDir := Dlg.FileName;
  finally
    Dlg.Free;
  end;

  if (ChosenDir = '') or not DirectoryExists(ChosenDir) then Exit;

  FFolderScanMode := True;
  FCancelScan := False;
  ButtonScan.Enabled := False;
  ButtonScanFolder.Enabled := False;
  ButtonCancelScan.Enabled := True;
  ButtonDisinfectBoot.Enabled := False;
  ButtonDisinfectAll.Enabled := False;
  ButtonInspectHex.Enabled := False;

  LabelDiskPath.Caption := 'Folder: ' + ChosenDir;
  LabelBootStatus.Caption := 'Directory Scan in progress...';
  LabelScanStatus.Font.Color := clWindowText;
  LabelScanStatus.Caption := 'Searching for supported files (.st, .msa, .dim, .zip)...';
  ProgressBarScan.Position := 0;
  ListViewThreats.Items.Clear;
  SetLength(FSummary.Threats, 0);
  FSummary.TotalFilesScanned := 0;
  FSummary.TotalFoldersScanned := 0;
  FSummary.BootChecksum := 0;
  FSummary.BootIsExecutable := False;

  Screen.Cursor := crHourGlass;
  FileList := TStringList.Create;
  try
    CollectSupportedFiles(ChosenDir, FileList, FCancelScan);

    if FCancelScan then
    begin
      LabelScanStatus.Font.Color := clNavy;
      LabelScanStatus.Caption := 'Scan cancelled during directory discovery.';
      Exit;
    end;

    if FileList.Count = 0 then
    begin
      LabelScanStatus.Caption := 'No supported files (.st, .msa, .dim, .zip) found in folder.';
      ShowMessage('No supported files (.st, .msa, .dim, .zip) found in:' + sLineBreak + ChosenDir);
      Exit;
    end;

    TotalImagesProcessed := 0;
    TotalFilesInsideImages := 0;
    FTotalFilesToScan := FileList.Count;

    for FileIdx := 0 to FileList.Count - 1 do
    begin
      if FCancelScan then Break;
      FCurrentFileIndex := FileIdx;

      CurFile := FileList[FileIdx];
      ProgressBarScan.Position := (FileIdx * 100) div FileList.Count;
      LabelScanStatus.Font.Color := clWindowText;
      LabelScanStatus.Caption := Format('Scanning [%d/%d]: %s',
        [FileIdx + 1, FileList.Count, ExtractFileName(CurFile)]);
      Application.ProcessMessages;

      if FCancelScan then Break;

      Ext := LowerCase(ExtractFileExt(CurFile));

      try
        if Ext = '.st' then
        begin
          TempSummary.TotalFilesScanned := 0;
          SetLength(TempSummary.Threats, 0);
          if ScanDiskImage(CurFile, TempSummary, @OnScanProgress, CheckBoxBootOnly.Checked) then
          begin
            Inc(TotalImagesProcessed);
            Inc(TotalFilesInsideImages, TempSummary.TotalFilesScanned);
            for K := 0 to High(TempSummary.Threats) do
            begin
              TempSummary.Threats[K].SourceFile := ExtractFileName(CurFile);
              TempSummary.Threats[K].ActualImagePath := CurFile;
              AddThreatResult(TempSummary.Threats[K]);
            end;
          end;
        end
        else if Ext = '.msa' then
        begin
          TempST := IncludeTrailingPathDelimiter(GetTempDir) + 'st_msascan_' + FormatDateTime('hhnnsszzz', Now) + '_' + IntToStr(FileIdx) + '.st';
          try
            if UnpackMSAToST(CurFile, TempST) and FileExists(TempST) then
            begin
              TempSummary.TotalFilesScanned := 0;
              SetLength(TempSummary.Threats, 0);
              if ScanDiskImage(TempST, TempSummary, @OnScanProgress, CheckBoxBootOnly.Checked) then
              begin
                Inc(TotalImagesProcessed);
                Inc(TotalFilesInsideImages, TempSummary.TotalFilesScanned);
                for K := 0 to High(TempSummary.Threats) do
                begin
                  TempSummary.Threats[K].SourceFile := ExtractFileName(CurFile);
                  TempSummary.Threats[K].ActualImagePath := CurFile;
                  AddThreatResult(TempSummary.Threats[K]);
                end;
              end;
            end;
          finally
            if FileExists(TempST) then
              SysUtils.DeleteFile(TempST);
          end;
        end
        else if Ext = '.dim' then
        begin
          TempST := IncludeTrailingPathDelimiter(GetTempDir) + 'st_dimscan_' + FormatDateTime('hhnnsszzz', Now) + '_' + IntToStr(FileIdx) + '.st';
          try
            if UnpackDIMToST(CurFile, TempST) and FileExists(TempST) then
            begin
              TempSummary.TotalFilesScanned := 0;
              SetLength(TempSummary.Threats, 0);
              if ScanDiskImage(TempST, TempSummary, @OnScanProgress, CheckBoxBootOnly.Checked) then
              begin
                Inc(TotalImagesProcessed);
                Inc(TotalFilesInsideImages, TempSummary.TotalFilesScanned);
                for K := 0 to High(TempSummary.Threats) do
                begin
                  TempSummary.Threats[K].SourceFile := ExtractFileName(CurFile);
                  TempSummary.Threats[K].ActualImagePath := CurFile;
                  AddThreatResult(TempSummary.Threats[K]);
                end;
              end;
            end;
          finally
            if FileExists(TempST) then
              SysUtils.DeleteFile(TempST);
          end;
        end
        else if Ext = '.zip' then
        begin
          UnZip := TUnZipper.Create;
          try
            UnZip.FileName := CurFile;
            try
              UnZip.Examine;
            except
              UnZip.Entries.Clear;
            end;

            for EntryIdx := 0 to UnZip.Entries.Count - 1 do
            begin
              if FCancelScan then Break;
              if UnZip.Entries[EntryIdx].IsDirectory then Continue;

              ZipEntryName := UnZip.Entries[EntryIdx].ArchiveFileName;
              ZipEntryExt := LowerCase(ExtractFileExt(ZipEntryName));
              if (ZipEntryExt = '.st') or (ZipEntryExt = '.msa') or (ZipEntryExt = '.dim') then
              begin
                TempZipStageDir := IncludeTrailingPathDelimiter(GetTempDir) + 'st_zipscan_' + FormatDateTime('hhnnsszzz', Now) + '_' + IntToStr(EntryIdx) + PathDelim;
                ForceDirectories(TempZipStageDir);
                try
                  UnZipSingle := TUnZipper.Create;
                  try
                    UnZipSingle.FileName := CurFile;
                    UnZipSingle.OutputPath := TempZipStageDir;
                    UnZipSingle.Flat := True;
                    UnZipSingle.UnZipFile(ZipEntryName);
                  finally
                    UnZipSingle.Free;
                  end;

                  ExtractedFile := TempZipStageDir + ExtractFileName(StringReplace(ZipEntryName, '/', PathDelim, [rfReplaceAll]));
                  if FileExists(ExtractedFile) then
                  begin
                    DiskToScan := ExtractedFile;
                    TempConvertedST := '';
                    try
                      if ZipEntryExt = '.msa' then
                      begin
                        TempConvertedST := ExtractedFile + '.st';
                        if UnpackMSAToST(ExtractedFile, TempConvertedST) and FileExists(TempConvertedST) then
                          DiskToScan := TempConvertedST
                        else
                          DiskToScan := '';
                      end
                      else if ZipEntryExt = '.dim' then
                      begin
                        TempConvertedST := ExtractedFile + '.st';
                        if UnpackDIMToST(ExtractedFile, TempConvertedST) and FileExists(TempConvertedST) then
                          DiskToScan := TempConvertedST
                        else
                          DiskToScan := '';
                      end;

                      if (DiskToScan <> '') and FileExists(DiskToScan) then
                      begin
                        TempSummary.TotalFilesScanned := 0;
                        SetLength(TempSummary.Threats, 0);
                        if ScanDiskImage(DiskToScan, TempSummary, @OnScanProgress, CheckBoxBootOnly.Checked) then
                        begin
                          Inc(TotalImagesProcessed);
                          Inc(TotalFilesInsideImages, TempSummary.TotalFilesScanned);
                          for K := 0 to High(TempSummary.Threats) do
                          begin
                            TempSummary.Threats[K].SourceFile := ExtractFileName(CurFile) + ' [' + ExtractFileName(ZipEntryName) + ']';
                            TempSummary.Threats[K].ActualImagePath := '';
                            TempSummary.Threats[K].ZipArchivePath := CurFile;
                            TempSummary.Threats[K].ZipEntryName := ZipEntryName;
                            AddThreatResult(TempSummary.Threats[K]);
                          end;
                        end;
                      end;
                    finally
                      if (TempConvertedST <> '') and FileExists(TempConvertedST) then
                        SysUtils.DeleteFile(TempConvertedST);
                      SysUtils.DeleteFile(ExtractedFile);
                    end;
                  end;
                finally
                  RemoveDir(TempZipStageDir);
                end;
              end;
            end;
          finally
            UnZip.Free;
          end;
        end;
      except
        on E: Exception do
        begin
          // Safely catch I/O hiccups, unreadable shares or corrupted archives and proceed
        end;
      end;
    end;

    FSummary.TotalFilesScanned := TotalFilesInsideImages;

    ActualThreatCount := 0;
    PotentialThreatCount := 0;
    ProtectorCount := 0;
    CustomBootCount := 0;
    for K := 0 to High(FSummary.Threats) do
    begin
      if FSummary.Threats[K].TargetType = vtProtector then
        Inc(ProtectorCount)
      else if FSummary.Threats[K].TargetType = vtCustomBoot then
        Inc(CustomBootCount)
      else if (Pos('unrecognized', LowerCase(FSummary.Threats[K].Name)) > 0) or
              ((Pos('suspicious', LowerCase(FSummary.Threats[K].Name)) > 0) and not HasFloppyWriteRoutine(FSummary.Threats[K])) then
        Inc(PotentialThreatCount)
      else
        Inc(ActualThreatCount);
    end;

    if FCancelScan then
    begin
      LabelScanStatus.Font.Color := clNavy;
      if (ActualThreatCount + PotentialThreatCount > 0) or (ProtectorCount > 0) then
        LabelScanStatus.Caption := Format('Scan cancelled. %d images processed (%d threat(s), %d protector(s)).',
          [TotalImagesProcessed, ActualThreatCount + PotentialThreatCount, ProtectorCount])
      else
        LabelScanStatus.Caption := Format('Scan cancelled. %d images processed (0 threats detected).',
          [TotalImagesProcessed]);
      LabelBootStatus.Caption := Format('Folder Scan Cancelled (%d of %d files checked)',
        [TotalImagesProcessed, FileList.Count]);
    end
    else
    begin
      ProgressBarScan.Position := 100;
      if ProtectorCount > 0 then
        LabelBootStatus.Caption := Format('Folder Scan Complete: %d image(s) processed (%d protected)',
          [TotalImagesProcessed, ProtectorCount])
      else
        LabelBootStatus.Caption := Format('Folder Scan Complete: %d image(s) processed', [TotalImagesProcessed]);

      if ActualThreatCount > 0 then
      begin
        LabelScanStatus.Font.Color := clRed;
        if PotentialThreatCount > 0 then
          LabelScanStatus.Caption := Format('ALERT: %d Threat(s) and %d Potential Threat(s) Found across %d image(s)!',
            [ActualThreatCount, PotentialThreatCount, TotalImagesProcessed])
        else
          LabelScanStatus.Caption := Format('ALERT: %d Threat(s) Found across %d image(s)!',
            [ActualThreatCount, TotalImagesProcessed]);
      end
      else if PotentialThreatCount > 0 then
      begin
        LabelScanStatus.Font.Color := $000080FF; // Amber / orange
        LabelScanStatus.Caption := Format('CAUTION: %d Potential Threat(s) Detected across %d image(s)!',
          [PotentialThreatCount, TotalImagesProcessed]);
      end
      else if ProtectorCount > 0 then
      begin
        LabelScanStatus.Font.Color := $00804000; // Deep teal / green
        if CheckBoxBootOnly.Checked then
          LabelScanStatus.Caption := Format('CLEAN & PROTECTED: %d image boot sectors scanned (%d boot protector(s), 0 threats)',
            [TotalImagesProcessed, ProtectorCount])
        else
          LabelScanStatus.Caption := Format('CLEAN & PROTECTED: %d images scanned, %d files checked (%d boot protector(s), 0 threats)',
            [TotalImagesProcessed, TotalFilesInsideImages, ProtectorCount]);
      end
      else
      begin
        LabelScanStatus.Font.Color := clGreen;
        if CheckBoxBootOnly.Checked then
          LabelScanStatus.Caption := Format('CLEAN: %d image boot sectors scanned (No threats)',
            [TotalImagesProcessed])
        else
          LabelScanStatus.Caption := Format('CLEAN: %d images scanned, %d files checked (No threats)',
            [TotalImagesProcessed, TotalFilesInsideImages]);
      end;
    end;
  finally
    FileList.Free;
    ButtonScan.Enabled := (FCurrentDiskPath <> '') and FileExists(FCurrentDiskPath);
    ButtonScanFolder.Enabled := True;
    ButtonCancelScan.Enabled := False;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormVirusScanner.ButtonCancelScanClick(Sender: TObject);
begin
  FCancelScan := True;
  LabelScanStatus.Caption := 'Cancelling scan...';
  Application.ProcessMessages;
end;

procedure TFormVirusScanner.ButtonDisinfectBootClick(Sender: TObject);
var
  I, TargetIdx: Integer;
  HasUnrecBoot, HasProtector, HasCustomBoot: Boolean;
  BakPath, TargetDisk: string;
begin
  TargetIdx := -1;
  if FFolderScanMode then
  begin
    if ListViewThreats.Selected = nil then
    begin
      ShowMessage('Please select a threat in the list to disinfect.');
      Exit;
    end;
    TargetIdx := ListViewThreats.Selected.Index;
    if (TargetIdx < 0) or (TargetIdx > High(FSummary.Threats)) then Exit;
    TargetDisk := FSummary.Threats[TargetIdx].ActualImagePath;
    if (TargetDisk = '') or not FileExists(TargetDisk) or not SameText(ExtractFileExt(TargetDisk), '.st') then
    begin
      ShowMessage('Only standalone uncompressed .ST disk images can be disinfected in batch mode.');
      Exit;
    end;
  end
  else
  begin
    TargetDisk := FCurrentDiskPath;
  end;

  if (TargetDisk = '') or not FileExists(TargetDisk) then Exit;

  HasUnrecBoot := False;
  HasProtector := False;
  HasCustomBoot := False;
  if TargetIdx >= 0 then
  begin
    if FSummary.Threats[TargetIdx].TargetType = vtProtector then
      HasProtector := True
    else if FSummary.Threats[TargetIdx].TargetType = vtCustomBoot then
      HasCustomBoot := True
    else if (FSummary.Threats[TargetIdx].TargetType = vtBootSector) and
       (Pos('Unrecognized Executable', FSummary.Threats[TargetIdx].Name) > 0) then
      HasUnrecBoot := True;
  end
  else
  begin
    for I := 0 to High(FSummary.Threats) do
    begin
      if FSummary.Threats[I].TargetType = vtProtector then
        HasProtector := True
      else if FSummary.Threats[I].TargetType = vtCustomBoot then
        HasCustomBoot := True
      else if (FSummary.Threats[I].TargetType = vtBootSector) and
         (Pos('Unrecognized Executable', FSummary.Threats[I].Name) > 0) then
        HasUnrecBoot := True;
    end;
  end;

  if HasProtector then
  begin
    if MessageDlg('Remove Virus Protector / Vaccine',
      'The boot sector contains a known virus protector or vaccine.' + sLineBreak +
      'Disinfecting will remove this protection and reset Sector 0 to a standard non-executable boot block.' + sLineBreak + sLineBreak +
      'A backup (' + ExtractFileName(TargetDisk) + '.bak) will be created.' + sLineBreak +
      'Are you sure you want to proceed?',
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit;
  end
  else if HasCustomBoot or HasUnrecBoot then
  begin
    if MessageDlg('Custom Bootloader Warning',
      'Caution: The boot sector contains a custom executable loader (e.g. for games or demos).' + sLineBreak +
      'Disinfecting will disable the boot code and may prevent the disk from auto-booting!' + sLineBreak + sLineBreak +
      'A backup (' + ExtractFileName(TargetDisk) + '.bak) will be created.' + sLineBreak +
      'Are you sure you want to proceed?',
      mtWarning, [mbYes, mbNo], 0) <> mrYes then Exit;
  end
  else
  begin
    if MessageDlg('Disinfect Boot Sector',
      'Are you sure you want to disinfect the boot sector?' + sLineBreak +
      'A backup (' + ExtractFileName(TargetDisk) + '.bak) will be created.' + sLineBreak +
      'The BPB geometry will be kept intact, execution vectors demoted, and checksum cleared.',
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit;
  end;

  BakPath := TargetDisk + '.bak';
  CopyFile(TargetDisk, BakPath);

  if DisinfectBootSector(TargetDisk) then
  begin
    if TargetIdx >= 0 then
    begin
      FSummary.Threats[TargetIdx].Disinfected := True;
    end
    else
    begin
      for I := 0 to High(FSummary.Threats) do
      begin
        if (FSummary.Threats[I].TargetType in [vtBootSector, vtProtector, vtCustomBoot]) then
          FSummary.Threats[I].Disinfected := True;
      end;
      FSummary.BootChecksum := 0;
      FSummary.BootIsExecutable := False;
    end;
    PopulateThreatsList;
    UpdateUI;
    ShowMessage('Boot Sector successfully disinfected! The disk is now safe and non-executable.' + sLineBreak +
      'Backup saved to: ' + ExtractFileName(BakPath));
  end
  else
    ShowMessage('Error: Failed to disinfect boot sector.');
end;

procedure TFormVirusScanner.ButtonDisinfectAllClick(Sender: TObject);
var
  I: Integer;
  CleanedCount: Integer;
  HasUnrecBoot: Boolean;
  BakPath: string;
begin
  if (FCurrentDiskPath = '') or not FileExists(FCurrentDiskPath) then Exit;

  HasUnrecBoot := False;
  CleanedCount := 0;
  for I := 0 to High(FSummary.Threats) do
  begin
    if not FSummary.Threats[I].Disinfected and not (FSummary.Threats[I].TargetType in [vtProtector, vtCustomBoot]) then
    begin
      Inc(CleanedCount);
      if (FSummary.Threats[I].TargetType = vtBootSector) and
         (Pos('Unrecognized Executable', FSummary.Threats[I].Name) > 0) then
        HasUnrecBoot := True;
    end;
  end;

  if CleanedCount = 0 then
  begin
    ShowMessage('No active threats to disinfect (boot protectors, vaccines, and standard bootloaders are safe).');
    Exit;
  end;

  if HasUnrecBoot then
  begin
    if MessageDlg('Custom Bootloader Warning',
      'Caution: An unrecognized custom executable boot sector was detected.' + sLineBreak +
      'This is often a legitimate game or demo loader.' + sLineBreak +
      'Disinfecting all threats will erase this bootloader and may prevent the disk from booting!' + sLineBreak + sLineBreak +
      'A backup (' + ExtractFileName(FCurrentDiskPath) + '.bak) will be created.' + sLineBreak +
      'Do you want to proceed with disinfecting all threats?',
      mtWarning, [mbYes, mbNo], 0) <> mrYes then Exit;
  end
  else
  begin
    if MessageDlg('Disinfect All Threats',
      Format('Disinfect %d detected threat(s)?' + sLineBreak +
        'A backup (' + ExtractFileName(FCurrentDiskPath) + '.bak) will be created.' + sLineBreak +
        'Boot sector threats will be safely cleared and infected files will be removed.',
        [CleanedCount]),
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit;
  end;

  BakPath := FCurrentDiskPath + '.bak';
  CopyFile(FCurrentDiskPath, BakPath);

  CleanedCount := 0;
  for I := 0 to High(FSummary.Threats) do
  begin
    if (FSummary.Threats[I].TargetType = vtBootSector) and (FSummary.Threats[I].TargetType <> vtProtector) then
    begin
      if DisinfectBootSector(FCurrentDiskPath) then
      begin
        FSummary.Threats[I].Disinfected := True;
        Inc(CleanedCount);
      end;
    end
    else if (FSummary.Threats[I].TargetType = vtFile) and (FSummary.Threats[I].Cluster >= 2) then
    begin
      if DisinfectFile(FCurrentDiskPath, FSummary.Threats[I].Target, FSummary.Threats[I].Cluster) then
      begin
        FSummary.Threats[I].Disinfected := True;
        Inc(CleanedCount);
      end;
    end;
  end;

  FSummary.BootChecksum := 0;
  FSummary.BootIsExecutable := False;
  PopulateThreatsList;
  UpdateUI;
  ShowMessage(Format('Disinfection complete! %d threat(s) cleaned.' + sLineBreak +
    'Backup saved to: %s', [CleanedCount, ExtractFileName(BakPath)]));
end;

procedure TFormVirusScanner.ButtonInspectHexClick(Sender: TObject);
var
  Idx: Integer;
  FS: TFileStream;
  Buf: TBytes;
  Clust: Word;
  ImagePath, TempDir, ExtractedFile, TempST: string;
  UnZip: TUnZipper;
  NeedCleanupImage, NeedCleanupDir: Boolean;
begin
  if ListViewThreats.Selected = nil then Exit;
  Idx := ListViewThreats.Selected.Index;
  if (Idx < 0) or (Idx > High(FSummary.Threats)) then Exit;

  ImagePath := '';
  NeedCleanupImage := False;
  NeedCleanupDir := False;
  TempDir := '';
  ExtractedFile := '';
  TempST := '';

  if FSummary.Threats[Idx].ZipArchivePath <> '' then
  begin
    // Extract from ZIP archive into temporary directory
    TempDir := IncludeTrailingPathDelimiter(GetTempDir) + 'st_view_' + FormatDateTime('hhnnsszzz', Now) + PathDelim;
    ForceDirectories(TempDir);
    NeedCleanupDir := True;
    try
      UnZip := TUnZipper.Create;
      try
        UnZip.FileName := FSummary.Threats[Idx].ZipArchivePath;
        UnZip.OutputPath := TempDir;
        UnZip.Flat := True;
        UnZip.UnZipFile(FSummary.Threats[Idx].ZipEntryName);
      finally
        UnZip.Free;
      end;
      ExtractedFile := TempDir + ExtractFileName(StringReplace(FSummary.Threats[Idx].ZipEntryName, '/', PathDelim, [rfReplaceAll]));
      if FileExists(ExtractedFile) then
      begin
        if SameText(ExtractFileExt(ExtractedFile), '.msa') then
        begin
          TempST := ExtractedFile + '.st';
          if UnpackMSAToST(ExtractedFile, TempST) and FileExists(TempST) then
            ImagePath := TempST;
        end
        else if SameText(ExtractFileExt(ExtractedFile), '.dim') then
        begin
          TempST := ExtractedFile + '.st';
          if UnpackDIMToST(ExtractedFile, TempST) and FileExists(TempST) then
            ImagePath := TempST;
        end
        else
          ImagePath := ExtractedFile;
      end;
    except
      ImagePath := '';
    end;
  end
  else if FFolderScanMode then
  begin
    ImagePath := FSummary.Threats[Idx].ActualImagePath;
    if (ImagePath <> '') and FileExists(ImagePath) then
    begin
      if SameText(ExtractFileExt(ImagePath), '.msa') then
      begin
        TempST := IncludeTrailingPathDelimiter(GetTempDir) + 'st_view_' + FormatDateTime('hhnnsszzz', Now) + '.st';
        if UnpackMSAToST(ImagePath, TempST) and FileExists(TempST) then
        begin
          ImagePath := TempST;
          NeedCleanupImage := True;
        end;
      end
      else if SameText(ExtractFileExt(ImagePath), '.dim') then
      begin
        TempST := IncludeTrailingPathDelimiter(GetTempDir) + 'st_view_' + FormatDateTime('hhnnsszzz', Now) + '.st';
        if UnpackDIMToST(ImagePath, TempST) and FileExists(TempST) then
        begin
          ImagePath := TempST;
          NeedCleanupImage := True;
        end;
      end;
    end;
  end
  else
  begin
    ImagePath := FCurrentDiskPath;
  end;

  try
    if (ImagePath = '') or not FileExists(ImagePath) then
    begin
      ShowMessage('Hex inspection could not access the underlying disk image file.');
      Exit;
    end;

    if FSummary.Threats[Idx].TargetType in [vtBootSector, vtProtector, vtCustomBoot] then
    begin
      FS := TFileStream.Create(ImagePath, fmOpenRead or fmShareDenyNone);
      try
        SetLength(Buf, 512);
        FS.Position := 0;
        FS.ReadBuffer(Buf[0], 512);
        FormFileViewer.LoadFromBuffer(Buf, 'BOOT_SECTOR_0.BIN [' + FSummary.Threats[Idx].SourceFile + ']', 0);
        FormFileViewer.Show;
        FormFileViewer.BringToFront;
      finally
        FS.Free;
      end;
    end
    else if FSummary.Threats[Idx].TargetType = vtFile then
    begin
      Clust := FSummary.Threats[Idx].Cluster;
      if (Clust >= 2) then
      begin
        FormFileViewer.LoadFromHostFile(ImagePath, FSummary.Threats[Idx].Target, Clust);
        FormFileViewer.Show;
        FormFileViewer.BringToFront;
      end;
    end;
  finally
    if NeedCleanupImage and (ImagePath <> '') and FileExists(ImagePath) then
      SysUtils.DeleteFile(ImagePath);
    if NeedCleanupDir and (TempDir <> '') and DirectoryExists(TempDir) then
    begin
      if (ExtractedFile <> '') and FileExists(ExtractedFile) then
        SysUtils.DeleteFile(ExtractedFile);
      if (TempST <> '') and FileExists(TempST) then
        SysUtils.DeleteFile(TempST);
      RemoveDir(TempDir);
    end;
  end;
end;

procedure TFormVirusScanner.ButtonCloseClick(Sender: TObject);
begin
  FCancelScan := True;
  Close;
end;

procedure TFormVirusScanner.ListViewThreatsCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Idx: Integer;
begin
  DefaultDraw := True;
  if Item = nil then Exit;
  Idx := Item.Index;
  if (Idx < 0) or (Idx > High(FSummary.Threats)) then Exit;

  if not (cdsSelected in State) then
  begin
    if FSummary.Threats[Idx].Disinfected then
    begin
      Sender.Canvas.Brush.Color := clWindow;
      Sender.Canvas.Font.Color := clGrayText;
      Sender.Canvas.Font.Style := [];
    end
    else if FSummary.Threats[Idx].TargetType = vtProtector then
    begin
      Sender.Canvas.Brush.Color := clWindow;
      Sender.Canvas.Font.Color := $00804000; // Deep teal / green
      Sender.Canvas.Font.Style := [];
    end
    else if FSummary.Threats[Idx].TargetType = vtCustomBoot then
    begin
      Sender.Canvas.Brush.Color := clWindow;
      Sender.Canvas.Font.Color := RGBToColor(0, 100, 160); // Muted navy / blue
      Sender.Canvas.Font.Style := [];
    end
    else if Pos('suspicious', LowerCase(FSummary.Threats[Idx].Name)) > 0 then
    begin
      if HasFloppyWriteRoutine(FSummary.Threats[Idx]) then
      begin
        // RED: Suspicious Boot Sector with Floppy Write routine
        Sender.Canvas.Brush.Color := RGBToColor(255, 224, 224); // Light pastel red row background
        Sender.Canvas.Font.Color := RGBToColor(160, 0, 0);     // Dark bold red text
        Sender.Canvas.Font.Style := [fsBold];
      end
      else
      begin
        // YELLOW: Suspicious Boot Sector WITHOUT Floppy Write routine
        Sender.Canvas.Brush.Color := RGBToColor(255, 255, 204); // Light pastel yellow row background
        Sender.Canvas.Font.Color := RGBToColor(140, 95, 0);    // Dark golden amber text
        Sender.Canvas.Font.Style := [fsBold];
      end;
    end
    else if Pos('unrecognized', LowerCase(FSummary.Threats[Idx].Name)) > 0 then
    begin
      Sender.Canvas.Brush.Color := RGBToColor(255, 255, 225); // Very pale cream background
      Sender.Canvas.Font.Color := RGBToColor(180, 90, 0);     // Amber / brown text
      Sender.Canvas.Font.Style := [];
    end
    else if (FSummary.Threats[Idx].TargetType in [vtBootSector, vtFile]) then
    begin
      // Recognized virus threats -> RED
      Sender.Canvas.Brush.Color := RGBToColor(255, 224, 224); // Light pastel red row background
      Sender.Canvas.Font.Color := RGBToColor(160, 0, 0);     // Dark bold red text
      Sender.Canvas.Font.Style := [fsBold];
    end
    else
    begin
      Sender.Canvas.Brush.Color := clWindow;
      Sender.Canvas.Font.Color := clWindowText;
      Sender.Canvas.Font.Style := [];
    end;
  end
  else
  begin
    Sender.Canvas.Brush.Color := clHighlight;
    Sender.Canvas.Font.Color := clHighlightText;
    Sender.Canvas.Font.Style := [];
  end;
end;

procedure TFormVirusScanner.ListViewThreatsCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  ListViewThreatsCustomDrawItem(Sender, Item, State, DefaultDraw);
end;

procedure TFormVirusScanner.ListViewThreatsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  Idx: Integer;
begin
  ButtonInspectHex.Enabled := Selected and (Item <> nil);
  if FFolderScanMode and Selected and (Item <> nil) then
  begin
    Idx := Item.Index;
    if (Idx >= 0) and (Idx <= High(FSummary.Threats)) then
    begin
      ButtonDisinfectBoot.Enabled := (FSummary.Threats[Idx].TargetType in [vtBootSector, vtProtector, vtCustomBoot]) and
                                     (FSummary.Threats[Idx].ActualImagePath <> '') and
                                     FileExists(FSummary.Threats[Idx].ActualImagePath) and
                                     SameText(ExtractFileExt(FSummary.Threats[Idx].ActualImagePath), '.st');
    end;
  end;
end;

end.
