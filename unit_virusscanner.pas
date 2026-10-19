unit unit_virusscanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Math;

type
  TVirusTargetType = (vtBootSector, vtFile, vtNone);

  TVirusScanResult = record
    VirusFound: Boolean;
    TargetType: TVirusTargetType;
    Name: string;
    Description: string;
    Target: string;       // e.g. "Boot Sector (Sector 0)" or "File: \AUTO\MEDWAY.PRG"
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

  TScanProgressEvent = procedure(const CurrentItem: string; Percent: Integer) of object;

  { TFormVirusScanner }
  TFormVirusScanner = class(TForm)
    PanelTop: TPanel;
    LabelTitle: TLabel;
    LabelDiskPath: TLabel;
    LabelBootStatus: TLabel;
    LabelScanStatus: TLabel;
    ListViewThreats: TListView;
    PanelBottom: TPanel;
    ProgressBarScan: TProgressBar;
    ButtonScan: TButton;
    ButtonDisinfectBoot: TButton;
    ButtonDisinfectAll: TButton;
    ButtonInspectHex: TButton;
    ButtonClose: TButton;

    procedure FormCreate(Sender: TObject);
    procedure ButtonScanClick(Sender: TObject);
    procedure ButtonDisinfectBootClick(Sender: TObject);
    procedure ButtonDisinfectAllClick(Sender: TObject);
    procedure ButtonInspectHexClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ListViewThreatsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    FCurrentDiskPath: string;
    FSummary: TDiskScanSummary;
    procedure UpdateUI;
    procedure OnScanProgress(const CurrentItem: string; Percent: Integer);
    procedure PopulateThreatsList;
  public
    procedure OpenForDisk(const ADiskPath: string; AutoScan: Boolean = True);
  end;

var
  FormVirusScanner: TFormVirusScanner;

// Engine Functions
function CalculateBootChecksum(const Sector: PByte; Size: Integer = 512): Word;
function ScanBootSector(const SectorBytes: PByte; SectorSize: Integer; out Threat: TVirusScanResult): Boolean;
function ScanFileBuffer(const Buffer: PByte; BufLen: Integer; const AFileName: string; ACluster: Word; out Threat: TVirusScanResult): Boolean;
function ScanDiskImage(const AImagePath: string; out Summary: TDiskScanSummary; OnProgress: TScanProgressEvent = nil): Boolean;
function DisinfectBootSector(const AImagePath: string): Boolean;
function DisinfectFile(const AImagePath: string; const AFileName: string; ACluster: Word): Boolean;
function QuickCheckBootSector(const AImagePath: string; out ThreatName: string; out IsExecutable: Boolean): Boolean;

implementation

uses
  unit_fileviewer, unit_depacker;

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
  Sum: Word;
  I: Integer;
  Val: Word;
begin
  Sum := 0;
  if (Sector = nil) or (Size < 512) then Exit(0);
  for I := 0 to 255 do
  begin
    Val := (Word(Sector[I * 2]) shl 8) or Word(Sector[I * 2 + 1]);
    Sum := Word(Sum + Val);
  end;
  Result := Sum;
end;

function ScanBootSector(const SectorBytes: PByte; SectorSize: Integer; out Threat: TVirusScanResult): Boolean;
var
  BootSum: Word;
  IsExec: Boolean;
  MatchOff: Integer;
  I: Integer;
  Sigs: array[0..26] of TMaskedSig;
  SafeSigs: array[0..5] of TMaskedSig;
  GenericSigs: array[0..0] of TMaskedSig;
begin
  Result := False;
  Threat.VirusFound := False;
  Threat.TargetType := vtBootSector;
  Threat.Name := '';
  Threat.Description := '';
  Threat.Target := 'Boot Sector (Sector 0)';
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
  Sigs[12].FixedOffset := 0;
  Sigs[12].RequiredString := '';

  // 14. Green Goblins Virus
  Sigs[13].Name := 'Green Goblins Virus';
  Sigs[13].Description := 'Retro game compilations sector interrupter displaying playful warnings.';
  Sigs[13].Bytes := [$54, $68, $65, $20, $4C, $69, $74, $74, $6C, $65, $20, $47, $72, $65, $65, $6E]; // "The Little Green"
  Sigs[13].Mask := 'xxxxxxxxxxxxxxxx';
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
  Sigs[23].Description := 'Destructive loader partition virus which alters the root allocation structure.';
  Sigs[23].Bytes := [$4B, $4F, $42, $4F, $44, $23, $32, $20, $41, $4B, $54, $49, $56, $21]; // "KOBOLD#2 AKTIV!"
  Sigs[23].Mask := 'x?xxxx?xxxxxxxx';
  Sigs[23].FixedOffset := -1;
  Sigs[23].RequiredString := '';

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

  for I := 0 to High(Sigs) do
  begin
    MatchOff := FindMaskedPattern(SectorBytes, 512, Sigs[I].Bytes, Sigs[I].Mask, Sigs[I].FixedOffset);
    if MatchOff <> -1 then
    begin
      if (Sigs[I].RequiredString <> '') and not BufferContainsString(SectorBytes, 512, Sigs[I].RequiredString) then
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

  // Generic BRA / JMP instructions are only benign if the sector is NOT executable (checksum <> $1234)
  if not IsExec then
  begin
    for I := 3 to High(SafeSigs) do
    begin
      if FindMaskedPattern(SectorBytes, 512, SafeSigs[I].Bytes, SafeSigs[I].Mask, SafeSigs[I].FixedOffset) <> -1 then
        Exit(False);
    end;
  end;

  // -------------------------------------------------------------------------
  // TIER 3: Generic Family Signatures
  // -------------------------------------------------------------------------
  GenericSigs[0].Name := 'Ghost / MAD Virus Family (Generic)';
  GenericSigs[0].Description := 'Mutated or obfuscated variant of the Ghost/MAD family using recognizable register-dump stub.';
  GenericSigs[0].Bytes := [$48, $E7, $FF, $FE];
  GenericSigs[0].Mask := 'xxxx';
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
  // TIER 4: Unrecognized Executable Boot Sector
  // -------------------------------------------------------------------------
  if IsExec then
  begin
    Threat.VirusFound := True;
    Threat.Name := 'Unrecognized Executable Boot Sector';
    Threat.Description := 'The boot sector checksum validates to $1234, but its binary code matches no known safe OS configuration.';
    Threat.Offset := 0;
    Exit(True);
  end;
end;

function ScanFileBuffer(const Buffer: PByte; BufLen: Integer; const AFileName: string; ACluster: Word; out Threat: TVirusScanResult): Boolean;
var
  MatchOff, I: Integer;
  FileSigs: array[0..2] of TMaskedSig;
  DepackedBuf: TBytes;
  PackerSig, DepackPackerName: string;
begin
  Result := False;
  Threat.VirusFound := False;
  Threat.TargetType := vtFile;
  Threat.Name := '';
  Threat.Description := '';
  Threat.Target := 'File: \' + AFileName;
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

  {
  // 4. Medway Boys File Infector - commented out for now (gives too many false positives on legitimate Medway releases)
  FileSigs[3].Name := 'Medway Boys File Infector';
  FileSigs[3].Description := 'A virus developed by the Medway crew targeting executable application storage offsets.';
  FileSigs[3].Bytes := [$4D, $45, $44, $57, $41, $59, $20, $42, $4F, $59, $53]; // "MEDWAY BOYS"
  FileSigs[3].Mask := 'xxxxxxxxxxx';
  FileSigs[3].FixedOffset := -1;
  }

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

function ScanDiskImage(const AImagePath: string; out Summary: TDiskScanSummary; OnProgress: TScanProgressEvent = nil): Boolean;
var
  FS: TFileStream;
  BootSec: array[0..511] of Byte;
  BytesPerSec, SecPerClust, ResSec, NumFATs, RootEntries, SecPerFAT: Word;
  FatStart, RootDirStart, DataStart, RootDirSectors: Int64;
  ClusterBytes: Int64;
  FATBuffer: TBytes;
  BootThreat, FileThreat: TVirusScanResult;

  procedure AddThreat(const AThreat: TVirusScanResult);
  var
    Len: Integer;
  begin
    Len := Length(Summary.Threats);
    SetLength(Summary.Threats, Len + 1);
    Summary.Threats[Len] := AThreat;
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

    function IsClusterVisited(C: Word): Boolean;
    var
      V: Integer;
    begin
      for V := 0 to High(VisitedClusters) do
        if VisitedClusters[V] = C then Exit(True);
      Exit(False);
    end;

  begin
    if Depth > 16 then Exit;

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
      Inc(Summary.TotalFoldersScanned);
      CurrClust := StartCluster;
      SetLength(EntriesBuf, 0);
      SetLength(VisitedClusters, 0);

      while (CurrClust >= 2) and (CurrClust < $FF8) and not IsClusterVisited(CurrClust) do
      begin
        SetLength(VisitedClusters, Length(VisitedClusters) + 1);
        VisitedClusters[High(VisitedClusters)] := CurrClust;

        ClustOff := DataStart + Int64(CurrClust - 2) * ClusterBytes;
        if ClustOff + ClusterBytes <= FS.Size then
        begin
          I := Length(EntriesBuf);
          SetLength(EntriesBuf, I + ClusterBytes);
          FS.Position := ClustOff;
          FS.ReadBuffer(EntriesBuf[I], ClusterBytes);
        end;

        CurrClust := ReadFAT12Entry(@FATBuffer[0], Length(FATBuffer), CurrClust);
      end;
      NumEntries := Length(EntriesBuf) div 32;
    end;

    for I := 0 to NumEntries - 1 do
    begin
      EntryOff := I * 32;
      if (EntryOff + 32 > Length(EntriesBuf)) then Break;
      if (EntriesBuf[EntryOff] = $00) and (StartCluster = 0) then Break;
      if EntriesBuf[EntryOff] = $E5 then Continue; // Deleted

      Attr := EntriesBuf[EntryOff + 11];
      if (Attr and $08) <> 0 then Continue; // Volume Label

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

      if (RawName = '.') or (RawName = '..') then Continue;

      FileClust := Word(EntriesBuf[EntryOff + 26]) or (Word(EntriesBuf[EntryOff + 27]) shl 8);
      FileSize := LongWord(EntriesBuf[EntryOff + 28]) or
                  (LongWord(EntriesBuf[EntryOff + 29]) shl 8) or
                  (LongWord(EntriesBuf[EntryOff + 30]) shl 16) or
                  (LongWord(EntriesBuf[EntryOff + 31]) shl 24);

      if (Attr and $10) <> 0 then
      begin
        // Recurse into subdirectory
        if FileClust >= 2 then
          ScanDirectory(FileClust, CurrentPath + '\' + RawName, Depth + 1);
      end
      else
      begin
        // File!
        Inc(Summary.TotalFilesScanned);
        if Assigned(OnProgress) then
          OnProgress(CurrentPath + '\' + RawName, Min(100, Summary.TotalFilesScanned));

        if (FileSize > 0) and (FileClust >= 2) then
        begin
          // Read full file bytes following cluster chain (safety capped at 8MB)
          FileSize := Min(FileSize, 8 * 1024 * 1024);
          SetLength(FileBuf, FileSize);
          BytesRead := 0;
          CurrClust := FileClust;
          SetLength(VisitedClusters, 0);

          while (CurrClust >= 2) and (CurrClust < $FF8) and (BytesRead < FileSize) and not IsClusterVisited(CurrClust) do
          begin
            SetLength(VisitedClusters, Length(VisitedClusters) + 1);
            VisitedClusters[High(VisitedClusters)] := CurrClust;

            ClustOff := DataStart + Int64(CurrClust - 2) * ClusterBytes;
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
      ThreatName := Threat.Name;
      Result := True;
    end;
  finally
    FS.Free;
  end;
end;

{ TFormVirusScanner }

procedure TFormVirusScanner.FormCreate(Sender: TObject);
begin
  FCurrentDiskPath := '';
  ProgressBarScan.Position := 0;
end;

procedure TFormVirusScanner.OpenForDisk(const ADiskPath: string; AutoScan: Boolean);
begin
  FCurrentDiskPath := ADiskPath;
  LabelDiskPath.Caption := 'Disk Image: ' + ExtractFileName(FCurrentDiskPath);
  UpdateUI;
  Show;
  BringToFront;

  if AutoScan and (FCurrentDiskPath <> '') then
    ButtonScanClick(nil);
end;

procedure TFormVirusScanner.UpdateUI;
var
  HasBootThreat, HasAnyThreat: Boolean;
  I: Integer;
begin
  HasBootThreat := False;
  HasAnyThreat := (Length(FSummary.Threats) > 0);

  for I := 0 to High(FSummary.Threats) do
  begin
    if (FSummary.Threats[I].TargetType = vtBootSector) and not FSummary.Threats[I].Disinfected then
      HasBootThreat := True;
  end;

  if FSummary.BootIsExecutable then
    LabelBootStatus.Caption := Format('Boot Sector: Checksum $%.4X [EXECUTABLE BOOT BLOCK]', [FSummary.BootChecksum])
  else
    LabelBootStatus.Caption := Format('Boot Sector: Checksum $%.4X [Non-executable / Clean]', [FSummary.BootChecksum]);

  if HasAnyThreat then
  begin
    LabelScanStatus.Font.Color := clRed;
    LabelScanStatus.Caption := Format('ALERT: %d Threat(s) Found!', [Length(FSummary.Threats)]);
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

  ButtonDisinfectBoot.Enabled := HasBootThreat;
  ButtonDisinfectAll.Enabled := HasAnyThreat;
  ButtonInspectHex.Enabled := (ListViewThreats.Selected <> nil);
end;

procedure TFormVirusScanner.OnScanProgress(const CurrentItem: string; Percent: Integer);
begin
  LabelScanStatus.Caption := 'Scanning: ' + ExtractFileName(CurrentItem);
  ProgressBarScan.Position := Percent;
  Application.ProcessMessages;
end;

procedure TFormVirusScanner.PopulateThreatsList;
var
  I: Integer;
  Item: TListItem;
  CatStr, StatusStr: string;
begin
  ListViewThreats.Items.BeginUpdate;
  try
    ListViewThreats.Items.Clear;
    for I := 0 to High(FSummary.Threats) do
    begin
      Item := ListViewThreats.Items.Add;
      Item.Caption := FSummary.Threats[I].Target;
      Item.SubItems.Add(FSummary.Threats[I].Name);

      if FSummary.Threats[I].TargetType = vtBootSector then
        CatStr := 'Boot Sector'
      else
        CatStr := 'File Infector';
      Item.SubItems.Add(CatStr);

      if FSummary.Threats[I].Disinfected then
        StatusStr := 'Cleaned / Safe'
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

procedure TFormVirusScanner.ButtonScanClick(Sender: TObject);
begin
  if (FCurrentDiskPath = '') or not FileExists(FCurrentDiskPath) then
  begin
    ShowMessage('No valid disk image is currently loaded to scan.');
    Exit;
  end;

  ButtonScan.Enabled := False;
  ProgressBarScan.Position := 0;
  LabelScanStatus.Caption := 'Scanning disk...';
  Screen.Cursor := crHourGlass;
  try
    ScanDiskImage(FCurrentDiskPath, FSummary, @OnScanProgress);
    PopulateThreatsList;
    ProgressBarScan.Position := 100;
    UpdateUI;
  finally
    ButtonScan.Enabled := True;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormVirusScanner.ButtonDisinfectBootClick(Sender: TObject);
var
  I: Integer;
  HasUnrecBoot: Boolean;
  BakPath: string;
begin
  if (FCurrentDiskPath = '') or not FileExists(FCurrentDiskPath) then Exit;

  HasUnrecBoot := False;
  for I := 0 to High(FSummary.Threats) do
    if (FSummary.Threats[I].TargetType = vtBootSector) and
       (Pos('Unrecognized Executable', FSummary.Threats[I].Name) > 0) then
      HasUnrecBoot := True;

  if HasUnrecBoot then
  begin
    if MessageDlg('Custom Bootloader Warning',
      'Caution: The boot sector contains an unrecognized custom executable loader.' + sLineBreak +
      'This is often a legitimate custom loader for Atari games and demos.' + sLineBreak +
      'Disinfecting will disable the boot code and may prevent the disk from booting!' + sLineBreak + sLineBreak +
      'A backup (' + ExtractFileName(FCurrentDiskPath) + '.bak) will be created.' + sLineBreak +
      'Are you sure you want to proceed?',
      mtWarning, [mbYes, mbNo], 0) <> mrYes then Exit;
  end
  else
  begin
    if MessageDlg('Disinfect Boot Sector',
      'Are you sure you want to disinfect the boot sector?' + sLineBreak +
      'A backup (' + ExtractFileName(FCurrentDiskPath) + '.bak) will be created.' + sLineBreak +
      'The BPB geometry will be kept intact, execution vectors demoted, and checksum cleared.',
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit;
  end;

  BakPath := FCurrentDiskPath + '.bak';
  CopyFile(FCurrentDiskPath, BakPath);

  if DisinfectBootSector(FCurrentDiskPath) then
  begin
    for I := 0 to High(FSummary.Threats) do
    begin
      if FSummary.Threats[I].TargetType = vtBootSector then
        FSummary.Threats[I].Disinfected := True;
    end;
    FSummary.BootChecksum := 0;
    FSummary.BootIsExecutable := False;
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
  for I := 0 to High(FSummary.Threats) do
    if (FSummary.Threats[I].TargetType = vtBootSector) and
       (Pos('Unrecognized Executable', FSummary.Threats[I].Name) > 0) then
      HasUnrecBoot := True;

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
        [Length(FSummary.Threats)]),
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit;
  end;

  BakPath := FCurrentDiskPath + '.bak';
  CopyFile(FCurrentDiskPath, BakPath);

  CleanedCount := 0;
  for I := 0 to High(FSummary.Threats) do
  begin
    if FSummary.Threats[I].TargetType = vtBootSector then
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
  SizeToRead: Int64;
begin
  if ListViewThreats.Selected = nil then Exit;
  Idx := ListViewThreats.Selected.Index;
  if (Idx < 0) or (Idx > High(FSummary.Threats)) then Exit;

  if FSummary.Threats[Idx].TargetType = vtBootSector then
  begin
    // Load Sector 0 into File Viewer
    if FileExists(FCurrentDiskPath) then
    begin
      FS := TFileStream.Create(FCurrentDiskPath, fmOpenRead or fmShareDenyNone);
      try
        SetLength(Buf, 512);
        FS.Position := 0;
        FS.ReadBuffer(Buf[0], 512);
        FormFileViewer.LoadFromBuffer(Buf, 'BOOT_SECTOR_0.BIN', 0);
        FormFileViewer.Show;
        FormFileViewer.BringToFront;
      finally
        FS.Free;
      end;
    end;
  end
  else if FSummary.Threats[Idx].TargetType = vtFile then
  begin
    // Open the infected file in File Viewer
    Clust := FSummary.Threats[Idx].Cluster;
    SizeToRead := FSummary.Threats[Idx].FileSize;
    if (Clust >= 2) and FileExists(FCurrentDiskPath) then
    begin
      // Let FileViewer load or extract
      FormFileViewer.LoadFromHostFile(FCurrentDiskPath, FSummary.Threats[Idx].Target, Clust);
      FormFileViewer.Show;
      FormFileViewer.BringToFront;
    end;
  end;
end;

procedure TFormVirusScanner.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormVirusScanner.ListViewThreatsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  ButtonInspectHex.Enabled := Selected and (Item <> nil);
end;

end.
