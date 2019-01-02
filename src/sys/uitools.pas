unit UITools;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF Windows}
  Windows,
{$ENDIF}
  Classes,
  SysUtils,
  Controls;

function IsAeroEnabled: Boolean;
procedure SetControlMultilineLabel(Control: TWinControl);

implementation

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
  if (Win32MajorVersion >= 6) then // Vista or Windows 7+
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

end.

