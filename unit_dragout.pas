unit unit_dragout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF WINDOWS}
  , Windows, ActiveX, ShellApi
  {$ENDIF}
  ;

function StartDragDropFiles(const AFilePaths: array of string): Boolean;

implementation

{$IFDEF WINDOWS}

type
  { TDropFiles structure for CF_HDROP }
  TDropFiles = packed record
    pFiles: DWORD;
    pt: TPoint;
    fNC: BOOL;
    fWide: BOOL;
  end;

  { TEnumFormatEtc }
  TEnumFormatEtc = class(TInterfacedObject, IEnumFORMATETC)
  private
    FFormats: array of FORMATETC;
    FIndex: Integer;
  public
    constructor Create(const AFormats: array of FORMATETC);
    function Next(celt: ULONG; out rgelt: FORMATETC; pceltFetched: PULONG): HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppenum: IEnumFORMATETC): HResult; stdcall;
  end;

  { TSimpleDropSource }
  TSimpleDropSource = class(TInterfacedObject, IDropSource)
  public
    function QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: DWORD): HResult; stdcall;
    function GiveFeedback(dwEffect: DWORD): HResult; stdcall;
  end;

  { TSimpleDataObject }
  TSimpleDataObject = class(TInterfacedObject, IDataObject)
  private
    FHDROP: HGLOBAL;
    FCFPreferredDropEffect: UINT;
  public
    constructor Create(AHDROP: HGLOBAL);
    destructor Destroy; override;
    function GetData(const pformatetcIn: FORMATETC; out medium: STGMEDIUM): HResult; stdcall;
    function GetDataHere(const pformatetc: FORMATETC; out medium: STGMEDIUM): HResult; stdcall;
    function QueryGetData(const pformatetc: FORMATETC): HResult; stdcall;
    function GetCanonicalFormatEtc(const pformatectIn: FORMATETC; out pformatetcOut: FORMATETC): HResult; stdcall;
    function SetData(const pformatetc: FORMATETC; var medium: STGMEDIUM; fRelease: BOOL): HResult; stdcall;
    function EnumFormatEtc(dwDirection: DWORD; out ppenumFormatEtc: IEnumFORMATETC): HResult; stdcall;
    function DAdvise(const pformatetc: FORMATETC; advf: DWORD; const advsink: IAdviseSink; out dwConnection: DWORD): HResult; stdcall;
    function DUnadvise(dwConnection: DWORD): HResult; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumSTATDATA): HResult; stdcall;
  end;

{ TEnumFormatEtc }

constructor TEnumFormatEtc.Create(const AFormats: array of FORMATETC);
var
  I: Integer;
begin
  inherited Create;
  SetLength(FFormats, Length(AFormats));
  for I := 0 to High(AFormats) do
    FFormats[I] := AFormats[I];
  FIndex := 0;
end;

function TEnumFormatEtc.Next(celt: ULONG; out rgelt: FORMATETC; pceltFetched: PULONG): HResult; stdcall;
var
  Count: ULONG;
  Dest: PFORMATETC;
begin
  Count := 0;
  Dest := @rgelt;
  while (FIndex < Length(FFormats)) and (Count < celt) do
  begin
    Dest^ := FFormats[FIndex];
    Inc(Dest);
    Inc(FIndex);
    Inc(Count);
  end;
  if pceltFetched <> nil then
    pceltFetched^ := Count;
  if Count = celt then
    Result := S_OK
  else
    Result := S_FALSE;
end;

function TEnumFormatEtc.Skip(celt: ULONG): HResult; stdcall;
begin
  FIndex := FIndex + Integer(celt);
  if FIndex <= Length(FFormats) then
    Result := S_OK
  else
  begin
    FIndex := Length(FFormats);
    Result := S_FALSE;
  end;
end;

function TEnumFormatEtc.Reset: HResult; stdcall;
begin
  FIndex := 0;
  Result := S_OK;
end;

function TEnumFormatEtc.Clone(out ppenum: IEnumFORMATETC): HResult; stdcall;
var
  Cloned: TEnumFormatEtc;
begin
  Cloned := TEnumFormatEtc.Create(FFormats);
  Cloned.FIndex := FIndex;
  ppenum := Cloned;
  Result := S_OK;
end;

{ TSimpleDropSource }

function TSimpleDropSource.QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: DWORD): HResult; stdcall;
begin
  if fEscapePressed then
    Result := DRAGDROP_S_CANCEL
  else if (grfKeyState and (MK_LBUTTON or MK_RBUTTON)) = 0 then
    Result := DRAGDROP_S_DROP
  else
    Result := S_OK;
end;

function TSimpleDropSource.GiveFeedback(dwEffect: DWORD): HResult; stdcall;
begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;

{ TSimpleDataObject }

constructor TSimpleDataObject.Create(AHDROP: HGLOBAL);
begin
  inherited Create;
  FHDROP := AHDROP;
  FCFPreferredDropEffect := RegisterClipboardFormat('Preferred DropEffect');
end;

destructor TSimpleDataObject.Destroy;
begin
  if FHDROP <> 0 then
    GlobalFree(FHDROP);
  inherited Destroy;
end;

function TSimpleDataObject.GetData(const pformatetcIn: FORMATETC; out medium: STGMEDIUM): HResult; stdcall;
var
  Size: SIZE_T;
  Src, Dst: Pointer;
  NewMem: HGLOBAL;
  pEffect: PDWORD;
begin
  FillChar(medium, SizeOf(medium), 0);

  // 1. CF_HDROP
  if (pformatetcIn.cfFormat = CF_HDROP) and ((pformatetcIn.tymed and TYMED_HGLOBAL) <> 0) then
  begin
    Size := GlobalSize(FHDROP);
    NewMem := GlobalAlloc(GMEM_MOVEABLE or GMEM_SHARE, Size);
    if NewMem = 0 then Exit(E_OUTOFMEMORY);

    Src := GlobalLock(FHDROP);
    Dst := GlobalLock(NewMem);
    try
      Move(Src^, Dst^, Size);
    finally
      GlobalUnlock(FHDROP);
      GlobalUnlock(NewMem);
    end;

    medium.tymed := TYMED_HGLOBAL;
    medium.hGlobal := NewMem;
    medium.pUnkForRelease := nil;
    Exit(S_OK);
  end;

  // 2. Preferred DropEffect
  if (pformatetcIn.cfFormat = FCFPreferredDropEffect) and ((pformatetcIn.tymed and TYMED_HGLOBAL) <> 0) then
  begin
    NewMem := GlobalAlloc(GMEM_MOVEABLE or GMEM_SHARE, SizeOf(DWORD));
    if NewMem = 0 then Exit(E_OUTOFMEMORY);

    pEffect := PDWORD(GlobalLock(NewMem));
    try
      pEffect^ := DROPEFFECT_COPY;
    finally
      GlobalUnlock(NewMem);
    end;

    medium.tymed := TYMED_HGLOBAL;
    medium.hGlobal := NewMem;
    medium.pUnkForRelease := nil;
    Exit(S_OK);
  end;

  Result := DV_E_FORMATETC;
end;

function TSimpleDataObject.GetDataHere(const pformatetc: FORMATETC; out medium: STGMEDIUM): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TSimpleDataObject.QueryGetData(const pformatetc: FORMATETC): HResult; stdcall;
begin
  if (pformatetc.cfFormat = CF_HDROP) and ((pformatetc.tymed and TYMED_HGLOBAL) <> 0) then
    Result := S_OK
  else if (pformatetc.cfFormat = FCFPreferredDropEffect) and ((pformatetc.tymed and TYMED_HGLOBAL) <> 0) then
    Result := S_OK
  else
    Result := DV_E_FORMATETC;
end;

function TSimpleDataObject.GetCanonicalFormatEtc(const pformatectIn: FORMATETC; out pformatetcOut: FORMATETC): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TSimpleDataObject.SetData(const pformatetc: FORMATETC; var medium: STGMEDIUM; fRelease: BOOL): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TSimpleDataObject.EnumFormatEtc(dwDirection: DWORD; out ppenumFormatEtc: IEnumFORMATETC): HResult; stdcall;
var
  FmtList: array[0..1] of FORMATETC;
begin
  ppenumFormatEtc := nil;
  if dwDirection <> DATADIR_GET then
    Exit(E_NOTIMPL);

  FillChar(FmtList[0], SizeOf(FORMATETC), 0);
  FmtList[0].cfFormat := CF_HDROP;
  FmtList[0].ptd := nil;
  FmtList[0].dwAspect := DVASPECT_CONTENT;
  FmtList[0].lindex := -1;
  FmtList[0].tymed := TYMED_HGLOBAL;

  FillChar(FmtList[1], SizeOf(FORMATETC), 0);
  FmtList[1].cfFormat := FCFPreferredDropEffect;
  FmtList[1].ptd := nil;
  FmtList[1].dwAspect := DVASPECT_CONTENT;
  FmtList[1].lindex := -1;
  FmtList[1].tymed := TYMED_HGLOBAL;

  ppenumFormatEtc := TEnumFormatEtc.Create(FmtList);
  Result := S_OK;
end;

function TSimpleDataObject.DAdvise(const pformatetc: FORMATETC; advf: DWORD; const advsink: IAdviseSink; out dwConnection: DWORD): HResult; stdcall;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TSimpleDataObject.DUnadvise(dwConnection: DWORD): HResult; stdcall;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TSimpleDataObject.EnumDAdvise(out enumAdvise: IEnumSTATDATA): HResult; stdcall;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function CreateHDROP(const AFilePaths: array of string): HGLOBAL;
var
  TotalChars: Integer;
  I: Integer;
  WStr: WideString;
  Size: Integer;
  HMem: HGLOBAL;
  Ptr: PByte;
  DF: TDropFiles;
  DestWChar: PWideChar;
  NormPath: string;
begin
  Result := 0;
  if Length(AFilePaths) = 0 then Exit;

  TotalChars := 0;
  for I := 0 to High(AFilePaths) do
  begin
    NormPath := ExpandFileName(AFilePaths[I]);
    NormPath := StringReplace(NormPath, '/', '\', [rfReplaceAll]);
    TotalChars := TotalChars + Length(WideString(NormPath)) + 1;
  end;
  Inc(TotalChars); // Final double-null terminator

  Size := SizeOf(TDropFiles) + TotalChars * SizeOf(WideChar);
  HMem := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT or GMEM_SHARE, Size);
  if HMem = 0 then Exit;

  Ptr := PByte(GlobalLock(HMem));
  try
    FillChar(DF, SizeOf(DF), 0);
    DF.pFiles := SizeOf(TDropFiles);
    DF.fWide := True;
    Move(DF, Ptr^, SizeOf(TDropFiles));

    DestWChar := PWideChar(Ptr + SizeOf(TDropFiles));
    for I := 0 to High(AFilePaths) do
    begin
      NormPath := ExpandFileName(AFilePaths[I]);
      NormPath := StringReplace(NormPath, '/', '\', [rfReplaceAll]);
      WStr := WideString(NormPath);
      Move(PWideChar(WStr)^, DestWChar^, Length(WStr) * SizeOf(WideChar));
      Inc(DestWChar, Length(WStr));
      DestWChar^ := #0;
      Inc(DestWChar);
    end;
    DestWChar^ := #0; // Final terminating null
  finally
    GlobalUnlock(HMem);
  end;

  Result := HMem;
end;

function StartDragDropFiles(const AFilePaths: array of string): Boolean;
var
  HDROP: HGLOBAL;
  DataObj: IDataObject;
  DropSrc: IDropSource;
  Effect: DWORD;
  Res: HResult;
begin
  Result := False;
  if Length(AFilePaths) = 0 then Exit;

  HDROP := CreateHDROP(AFilePaths);
  if HDROP = 0 then Exit;

  try
    DataObj := TSimpleDataObject.Create(HDROP);
    DropSrc := TSimpleDropSource.Create;
    Effect := DROPEFFECT_COPY;

    Res := DoDragDrop(DataObj, DropSrc, DROPEFFECT_COPY, @Effect);
    Result := (Res = DRAGDROP_S_DROP);
  except
    Result := False;
  end;
end;

{$ELSE}

function StartDragDropFiles(const AFilePaths: array of string): Boolean;
begin
  Result := False;
end;

{$ENDIF}

initialization
{$IFDEF WINDOWS}
  OleInitialize(nil);
{$ENDIF}

finalization
{$IFDEF WINDOWS}
  OleUninitialize;
{$ENDIF}

end.
