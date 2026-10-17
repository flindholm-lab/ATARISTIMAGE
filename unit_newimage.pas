unit unit_newimage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  { TFormNewImage }
  TFormNewImage = class(TForm)
    LabelTitle: TLabel;
    RadioGroupFormat: TRadioGroup;
    GroupBoxDetails: TGroupBox;
    LabelCapacity: TLabel;
    LabelGeometry: TLabel;
    LabelFilesystem: TLabel;
    LabelCompatibility: TLabel;
    ButtonOK: TButton;
    ButtonCancel: TButton;

    procedure FormCreate(Sender: TObject);
    procedure RadioGroupFormatSelectionChanged(Sender: TObject);
  private
    procedure UpdateFormatSpecs;
  public
    procedure GetSelectedFormat(out ASides, ASectorsPerTrack, ATracks, ARootEntries, ASectorsPerCluster: Integer;
      out ADefFileName, AFormatName: string);
  end;

var
  FormNewImage: TFormNewImage;

implementation

{$R *.lfm}

procedure TFormNewImage.FormCreate(Sender: TObject);
begin
  RadioGroupFormat.ItemIndex := 0; // Default to 720 KB
  UpdateFormatSpecs;
end;

procedure TFormNewImage.RadioGroupFormatSelectionChanged(Sender: TObject);
begin
  UpdateFormatSpecs;
end;

procedure TFormNewImage.UpdateFormatSpecs;
begin
  case RadioGroupFormat.ItemIndex of
    0: // 720 KB
    begin
      LabelCapacity.Caption := 'Capacity: 720 KB (737,280 bytes)';
      LabelGeometry.Caption := 'Geometry: 80 Tracks, 2 Sides, 9 Sectors/Track (1,440 Total Sectors)';
      LabelFilesystem.Caption := 'Filesystem: FAT12, 112 Root Entries, 2 Sectors/Cluster (1,024 bytes)';
      LabelCompatibility.Caption := 'Compatibility: Standard format for all Atari ST, STE, and Mega ST models.';
    end;
    1: // 360 KB
    begin
      LabelCapacity.Caption := 'Capacity: 360 KB (368,640 bytes)';
      LabelGeometry.Caption := 'Geometry: 80 Tracks, 1 Side, 9 Sectors/Track (720 Total Sectors)';
      LabelFilesystem.Caption := 'Filesystem: FAT12, 64 Root Entries, 2 Sectors/Cluster (1,024 bytes)';
      LabelCompatibility.Caption := 'Compatibility: Single-sided format for early 520ST with single-sided drives.';
    end;
    2: // 800 KB
    begin
      LabelCapacity.Caption := 'Capacity: 800 KB (819,200 bytes)';
      LabelGeometry.Caption := 'Geometry: 80 Tracks, 2 Sides, 10 Sectors/Track (1,600 Total Sectors)';
      LabelFilesystem.Caption := 'Filesystem: FAT12, 112 Root Entries, 2 Sectors/Cluster (1,024 bytes)';
      LabelCompatibility.Caption := 'Compatibility: Popular extended format for demos, cracktros, and large games.';
    end;
    3: // 880 KB
    begin
      LabelCapacity.Caption := 'Capacity: 880 KB (901,120 bytes)';
      LabelGeometry.Caption := 'Geometry: 80 Tracks, 2 Sides, 11 Sectors/Track (1,760 Total Sectors)';
      LabelFilesystem.Caption := 'Filesystem: FAT12, 112 Root Entries, 2 Sectors/Cluster (1,024 bytes)';
      LabelCompatibility.Caption := 'Compatibility: Maximum standard Double-Density storage capacity.';
    end;
    4: // 1.44 MB
    begin
      LabelCapacity.Caption := 'Capacity: 1.44 MB (1,474,560 bytes)';
      LabelGeometry.Caption := 'Geometry: 80 Tracks, 2 Sides, 18 Sectors/Track (2,880 Total Sectors)';
      LabelFilesystem.Caption := 'Filesystem: FAT12, 224 Root Entries, 2 Sectors/Cluster (1,024 bytes)';
      LabelCompatibility.Caption := 'Compatibility: High-Density (HD) format for Mega STE, TT030, and Falcon030.';
    end;
  end;
end;

procedure TFormNewImage.GetSelectedFormat(out ASides, ASectorsPerTrack, ATracks, ARootEntries, ASectorsPerCluster: Integer;
  out ADefFileName, AFormatName: string);
begin
  ATracks := 80;
  ASectorsPerCluster := 2;

  case RadioGroupFormat.ItemIndex of
    1: // 360 KB
    begin
      ASides := 1;
      ASectorsPerTrack := 9;
      ARootEntries := 64;
      ADefFileName := 'empty360kb.st';
      AFormatName := '360 KB Single-Sided';
    end;
    2: // 800 KB
    begin
      ASides := 2;
      ASectorsPerTrack := 10;
      ARootEntries := 112;
      ADefFileName := 'empty800kb.st';
      AFormatName := '800 KB Extended (10 SPT)';
    end;
    3: // 880 KB
    begin
      ASides := 2;
      ASectorsPerTrack := 11;
      ARootEntries := 112;
      ADefFileName := 'empty880kb.st';
      AFormatName := '880 KB High Capacity (11 SPT)';
    end;
    4: // 1.44 MB
    begin
      ASides := 2;
      ASectorsPerTrack := 18;
      ARootEntries := 224;
      ADefFileName := 'empty1440kb.st';
      AFormatName := '1.44 MB High Density (HD)';
    end;
    else // 0: 720 KB
    begin
      ASides := 2;
      ASectorsPerTrack := 9;
      ARootEntries := 112;
      ADefFileName := 'empty720kb.st';
      AFormatName := '720 KB Standard Double-Sided';
    end;
  end;
end;

end.
