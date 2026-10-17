program stimage;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unit1, unit_clustermap, unit_msa, unit_fileviewer, unit_depacker,
  unit_ataripic, unit_virusscanner, unit_newimage, unit_archivevault
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  {$PUSH}{$WARN 5044 OFF}
  Application.MainFormOnTaskbar:=True;
  {$POP}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFormClusterMap, FormClusterMap);
  Application.CreateForm(TFormFileViewer, FormFileViewer);
  Application.CreateForm(TFormVirusScanner, FormVirusScanner);
  Application.Run;
end.

