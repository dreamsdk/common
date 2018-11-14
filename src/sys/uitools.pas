unit UITools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function IsAeroEnabled: Boolean;

implementation

{$IFDEF Windows}
uses
  Windows;
{$ENDIF}

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

end.

