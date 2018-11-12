unit Version;

{$mode objfpc}{$H+}

interface

(*
  Building on the excellent vinfo.pas supplied by Paul Ishenin and available
  elsewhere on these Lazarus Forums
    - I hid the TVersionInfo class from the end user to simplify their (mine) number of required Uses...
    - Added defensive code to TVersionInfo if no build info is compiled into the exe
    - Deduced GetResourceStrings - works under Linux 64/GTK2 with Lazarus 0.9.30, but fails under
      Win XP 32bit/Lazarus 0.9.29 - suspecting my install as the lazresexplorer example also fails
      for me under Lazarus 0.9.29, but works with Lazarus 0.9.30

  Trawled through IDE source code, FPC source code and Lazarus supplied example program lasresexplorer
  to find the other defines and lookups...

  End user only needs to use VersionSupport - no other units necessary for their project.

  Jedi CodeFormatter seems to fail on the {$I %VARIABLE%} references, so sticking them all in here
  means end user code can be neatly formatted using Jedi CodeFormatter

  Other interesting includes I picked up in my travels are...
  //  {$I %HOME%} = User Home Directory
  //  {$I %FILE%} = Current pas file
  //  {$I %LINE%} = current line number

  Mike Thompson - mike.cornflake@gmail.com
  July 24 2011

  Adjustements by SiZiOUS - sizious(at)gmail(d0t)com
  Nov 5 2018
*)

uses
  Classes, SysUtils;

function GetFileDescription: string;
function GetFileVersion: string;
function GetProductName: string;
function GetProductVersion: string;
function GetCompiledDateTime: TDateTime;
function GetCompilerInfo: string;
function GetTargetInfo: string;
function GetOS: string;
function GetResourceStrings(oStringList: TStringList): Boolean;
function GetLCLVersion: string;
function GetWidgetSet: string;
function GetCompanyName: string;
function GetLegalCopyright: string;

const
  WIDGETSET_GTK        = 'GTK';
  WIDGETSET_GTK2       = 'GTK 2';
  WIDGETSET_WIN        = 'Win32/Win64';
  WIDGETSET_WINCE      = 'WinCE';
  WIDGETSET_CARBON     = 'Carbon';
  WIDGETSET_QT         = 'QT';
  WIDGETSET_fpGUI      = 'fpGUI';
  WIDGETSET_OTHER      = '(Other)';

implementation

Uses
  Resource, VersionTypes, VersionResource, LCLVersion, InterfaceBase,
  LCLPlatformDef, DateUtils;

Type
  TVersionInfo = Class
  private
    FBuildInfoAvailable: Boolean;
    FVersResource: TVersionResource;
    Function GetFixedInfo: TVersionFixedInfo;
    Function GetStringFileInfo: TVersionStringFileInfo;
    Function GetVarFileInfo: TVersionVarFileInfo;
  public
    Constructor Create;
    Destructor Destroy; override;

    Procedure Load(Instance: THandle);

    Property BuildInfoAvailable: Boolean Read FBuildInfoAvailable;

    Property FixedInfo: TVersionFixedInfo Read GetFixedInfo;
    Property StringFileInfo: TVersionStringFileInfo Read GetStringFileInfo;
    Property VarFileInfo: TVersionVarFileInfo Read GetVarFileInfo;
  End;

function GetWidgetSet: string;
begin
  case WidgetSet.LCLPlatform of
    lpGtk:   Result := WIDGETSET_GTK;
    lpGtk2:  Result := WIDGETSET_GTK2;
    lpWin32: Result := WIDGETSET_WIN;
    lpWinCE: Result := WIDGETSET_WINCE;
    lpCarbon:Result := WIDGETSET_CARBON;
    lpQT:    Result := WIDGETSET_QT;
    lpfpGUI: Result := WIDGETSET_fpGUI;
  else
    Result:=WIDGETSET_OTHER;
  end;
end;

Function GetCompilerInfo: String;
begin
  Result := {$I %FPCVERSION%};
end;

Function GetTargetInfo: String;
begin
  Result := {$I %FPCTARGETCPU%} + '/' + {$I %FPCTARGETOS%};
end;

Function GetOS: String;
Begin
  Result := {$I %FPCTARGETOS%};
End;

Function GetLCLVersion: String;
begin
  Result := lcl_version;
end;

function GetCompiledDateTime: TDateTime;
var
  sDate, sTime: string;
  dYYYY, dMM, dDD, tHH, tMM, tSS: Integer;

begin
  sDate := {$I %DATE%};
  dYYYY := StrToIntDef(Copy(sDate, 1, 4), 0);
  dMM := StrToIntDef(Copy(sDate, 6, 2), 0);
  dDD := StrToIntDef(Copy(sDate, 9, 2), 0);

  sTime := {$I %TIME%};
  tHH := StrToIntDef(Copy(sTime, 1, 2), 0);
  tMM := StrToIntDef(Copy(sTime, 4, 2), 0);
  tSS := StrToIntDef(Copy(sTime, 7, 2), 0);

  Result := EncodeDateTime(dYYYY, dMM, dDD, tHH, tMM, tSS, 0);
end;

{ Routines to expose TVersionInfo data }

Var
  FInfo: TVersionInfo;

Procedure CreateInfo;
Begin
  If Not Assigned(FInfo) Then
  Begin
    FInfo := TVersionInfo.Create;
    FInfo.Load(HINSTANCE);
  End;
End;

function GetVersionStringTable: TVersionStringTable;
begin
  Result := nil;
  CreateInfo;
  if FInfo.BuildInfoAvailable and (FInfo.StringFileInfo.Count > 0) then
    Result := FInfo.StringFileInfo[0];
end;

function GetVersionString(const Key: string): string;
var
  VersionStringTable: TVersionStringTable;

begin
  Result := '';
  try
    VersionStringTable := GetVersionStringTable;
    if Assigned(VersionStringTable) then
      Result := VersionStringTable.Values[Key];
  except
    Result := '';
  end;
end;

Function GetResourceStrings(oStringList: TStringList): Boolean;
Var
  i, j : Integer;
  oTable : TVersionStringTable;
begin
  CreateInfo;

  oStringList.Clear;
  Result := False;

  If FInfo.BuildInfoAvailable Then
  Begin
    Result := True;
    For i := 0 To FInfo.StringFileInfo.Count-1 Do
    Begin
      oTable := FInfo.StringFileInfo.Items[i];

      For j := 0 To oTable.Count-1 Do
        If Trim(oTable.ValuesByIndex[j])<>'' Then
          oStringList.Values[oTable.Keys[j]] := oTable.ValuesByIndex[j];
    end;
  end;
end;

Function ProductVersionToString(PV: TFileProductVersion): String;
Begin
  Result := Format('%d.%d.%d.%d', [PV[0], PV[1], PV[2], PV[3]]);
End;

Function GetProductVersion: String;
Begin
  CreateInfo;

  If FInfo.BuildInfoAvailable Then
    Result := ProductVersionToString(FInfo.FixedInfo.ProductVersion)
  Else
    Result := 'No build information available';
End;

function GetProductName: string;
begin
  Result := GetVersionString('ProductName');
end;

function GetFileDescription: string;
begin
  Result := GetVersionString('FileDescription');
end;

function GetCompanyName: string;
begin
  Result := GetVersionString('CompanyName');
end;

function GetLegalCopyright: string;
begin
  Result := GetVersionString('LegalCopyright');
end;

Function GetFileVersion: String;
Begin
  CreateInfo;

  If FInfo.BuildInfoAvailable Then
    Result := ProductVersionToString(FInfo.FixedInfo.FileVersion)
  Else
    Result := 'No build information available';
End;

{ TVersionInfo }

Function TVersionInfo.GetFixedInfo: TVersionFixedInfo;
Begin
  Result := FVersResource.FixedInfo;
End;

Function TVersionInfo.GetStringFileInfo: TVersionStringFileInfo;
Begin
  Result := FVersResource.StringFileInfo;
End;

Function TVersionInfo.GetVarFileInfo: TVersionVarFileInfo;
Begin
  Result := FVersResource.VarFileInfo;
End;

Constructor TVersionInfo.Create;
Begin
  Inherited Create;

  FVersResource := TVersionResource.Create;
  FBuildInfoAvailable := False;
End;

Destructor TVersionInfo.Destroy;
Begin
  FVersResource.Free;

  Inherited Destroy;
End;

Procedure TVersionInfo.Load(Instance: THandle);
Var
  Stream: TResourceStream;
  ResID: Integer;
  Res: TFPResourceHandle;

Begin
  FBuildInfoAvailable := False;
  ResID := 1;

{$IFDEF Windows}
  // Defensive code to prevent failure if no resource available...
  Res := FindResource(Instance, {%H-}PChar(PtrUInt(ResID)), PChar(RT_VERSION));
  If Res = 0 Then
    Exit;
{$ENDIF}

  Stream := TResourceStream.CreateFromID(Instance, ResID, PChar(RT_VERSION));
  Try
    FVersResource.SetCustomRawDataStream(Stream);

    // access some property to load from the stream
    FVersResource.FixedInfo;

    // clear the stream
    FVersResource.SetCustomRawDataStream(nil);

    FBuildInfoAvailable := True;
  Finally
    Stream.Free;
  End;
End;

Initialization
  FInfo := nil;

Finalization
  If Assigned(FInfo) Then
    FInfo.Free;

End.
