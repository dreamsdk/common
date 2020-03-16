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

implementation

uses
  Version;
	
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

end.

