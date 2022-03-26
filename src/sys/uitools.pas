unit UITools;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF Windows}
  Windows,
{$ENDIF}
  Graphics,
  Classes,
  SysUtils,
  Controls,
  ExtCtrls;

procedure ImageToForm(FormHandle: THandle; Image: TImage; TransparentColor: TColor);
function IsAeroEnabled: Boolean;
procedure SetControlMultilineLabel(Control: TWinControl);
function FindProcessWindows(ProcessID: LongWord; Handles: TList): Boolean;
procedure SetWindowIconForProcessId(const ProcessID: LongWord;
  const IconResourceName: string); overload;
procedure SetWindowIconForProcessId(const ProcessID: LongWord); overload;
function GetWindowTitle(const AWindowHandle: THandle): string;
function SetWindowTitle(const AWindowHandle: THandle; const NewTitle: string): Boolean;

implementation

uses
  Version;

type
  PFindWindowsStruct = ^TFindWindowsStruct;
  TFindWindowsStruct = record
    ProcessID: LongWord;
    HandleList: TList;
  end;

function EnumWindowsProc(WinHandle: THandle; lParam: LPARAM): LongBool; stdcall;
var
  dwProcessId: LongWord;
  
begin
  Result := False;
  dwProcessId := 0;
  if lParam <> 0 then
  begin
    GetWindowThreadProcessId(WinHandle, dwProcessId);
    with PFindWindowsStruct(lParam)^ do
	    if dwProcessID = ProcessID then
        HandleList.Add(Pointer(WinHandle));
    Result:= True;
  end;
end;

// https://delphi.developpez.com/faq/?page=Systeme-moins-Divers#Comment-recuperer-les-handles-des-fenetres-d-un-processus
function FindProcessWindows(ProcessID: LongWord; Handles: TList): Boolean;
var
  FindWindowsStruct: TFindWindowsStruct;
  
begin
  FindWindowsStruct.ProcessID := ProcessID;
  FindWindowsStruct.HandleList := Handles;
  Result := EnumWindows(@EnumWindowsProc, LongWord(@FindWindowsStruct));
end; 
	
function IsAeroEnabled: Boolean;
{$IFDEF Windows}
type
  TDwmIsCompositionEnabledFunc = function(out pfEnabled: BOOL): HRESULT; stdcall;

var
  IsEnabled: BOOL;
  ModuleHandle: HMODULE;
  DwmIsCompositionEnabledFunc: TDwmIsCompositionEnabledFunc;

begin
  Result := False;
  if IsWindowsVistaOrGreater then // Vista or Windows 7+
  begin
    ModuleHandle := LoadLibrary('dwmapi.dll');
    if ModuleHandle <> 0 then
    try
      DwmIsCompositionEnabledFunc := TDwmIsCompositionEnabledFunc(GetProcAddress(ModuleHandle, 'DwmIsCompositionEnabled'));
      if Assigned(DwmIsCompositionEnabledFunc) then
        if DwmIsCompositionEnabledFunc(IsEnabled) = S_OK then
          Result := IsEnabled;
    finally
      FreeLibrary(ModuleHandle);
    end;
  end;
{$ELSE}
begin
  Result := False;
{$ENDIF}
end;

// https://forum.lazarus.freepascal.org/index.php?topic=21018.0
procedure SetControlMultilineLabel(Control: TWinControl);
begin
  SetWindowLong(Control.Handle, GWL_STYLE,
    GetWindowLong(Control.Handle, GWL_STYLE) or BS_MULTILINE);
end;

procedure ImageToForm(FormHandle: THandle; Image: TImage; TransparentColor: TColor);
{$IFDEF WINDOWS}
var
  i, j, k : Integer;
  Region, Region2 : HRGN;

begin
  Region := CreateRectRgn(0, 0, 0, 0);
  for i := 0 to Image.Width - 1 do
  begin
    j := 0;
    while j < Image.Height - 1 do
    begin
      if Image.Canvas.Pixels[i, j] <> TransparentColor then
      begin
        k := j;
        while (Image.Canvas.Pixels[i, j] <> TransparentColor) and (j < Image.Height) do
          Inc(j);
        Region2 := CreateRectRgn(i, k, i + 1, j);
        CombineRgn(Region, Region, Region2, RGN_OR);
        DeleteObject(Region2);
      end;
      Inc(j);
    end;
  end;
  SetWindowRgn(FormHandle, Region, True);
{$ELSE}
begin
{$ENDIF}
end;

procedure SetWindowIconForProcessId(const ProcessID: LongWord;
  const IconResourceName: string); overload;
var
  WinHandles: TList;
  BigIcon,
  SmallIcon: HICON;
  WinHandle: THandle;
  i: Integer;

begin
  WinHandles := TList.Create;
  try
    // Load icons
    BigIcon := LoadImage(hInstance, PChar(IconResourceName), IMAGE_ICON,
      GetSystemMetrics(SM_CXICON), GetSystemMetrics(SM_CYICON), 0);
    SmallIcon := LoadImage(hInstance, PChar(IconResourceName), IMAGE_ICON,
      GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON), 0);

    // Set icons for all window associated to that ProcessID
    FindProcessWindows(ProcessID, WinHandles);
    for i:= 0 to WinHandles.Count - 1 do
    begin
      WinHandle := THandle(WinHandles[i]);
      SendMessage(WinHandle, WM_SETICON, ICON_BIG, LParam(BigIcon));
      SendMessage(WinHandle, WM_SETICON, ICON_SMALL, LParam(SmallIcon));
    end;
  finally
    WinHandles.Free;
  end;
end;

procedure SetWindowIconForProcessId(const ProcessID: LongWord); overload;
begin
  SetWindowIconForProcessId(ProcessID, 'MAINICON');
end;

function GetWindowTitle(const AWindowHandle: THandle): string;
var
  TitleLength: Integer;
  Title: string;

begin
  Result := EmptyStr;
  Title := EmptyStr;
  TitleLength := GetWindowTextLength(AWindowHandle);
  if TitleLength <> 0 then
  begin
    SetLength(Title, TitleLength);
    GetWindowText(AWindowHandle, PChar(Title), TitleLength + 1);
    Result := PChar(Title);
  end;
end;

function SetWindowTitle(const AWindowHandle: THandle;
  const NewTitle: string): Boolean;
begin
  Result := SetWindowText(AWindowHandle, PChar(NewTitle))
end;

end.

