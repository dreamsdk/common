unit RefBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  EHomeDirectoryNotFound = class(Exception);

function GetBaseEnvironmentVariableName: string;
function GetConfigurationDirectory: TFileName;
function GetInstallationBaseDirectory: TFileName;
function GetMSysBaseDirectory: TFileName;

implementation

uses
{$IFDEF RELEASE}
{$IFDEF GUI}
  Dialogs,
  MsgDlg,
{$ENDIF}
{$ENDIF}
  SysTools,
  FirstRun,
  FSTools;

var
  InstallationBaseDirectory,
  MsysBaseDirectory,
  ConfigurationDirectory: TFileName;

function GetBaseEnvironmentVariableName: string;
const
  DREAMSDK_ENVIRONMENT_VARIABLE = {$IFDEF RELEASE}'DREAMSDK_HOME'{$ELSE}'DREAMSDK_HOME_DEBUG'{$ENDIF};

begin
  Result := DREAMSDK_ENVIRONMENT_VARIABLE;
end;

procedure RetrieveBaseDirectories;
const
  MSYS_BASE_DIRECTORY = 'msys\1.0\';
  SETTINGS_DIRECTORY = 'etc\dreamsdk\';

begin
  if IsEmpty(InstallationBaseDirectory) then
  begin
    InstallationBaseDirectory := GetEnvironmentVariable(GetBaseEnvironmentVariableName);
    if not DirectoryExists(InstallationBaseDirectory) then
      InstallationBaseDirectory := GetApplicationPath + '..\..\..\..\'; // Fail-safe: this points to the X:\DreamSDK\ directory

    if IsFirstRunMode then
      InstallationBaseDirectory := GetFirstRunInstallationDirectory;

    if not IsEmpty(InstallationBaseDirectory) then
      InstallationBaseDirectory := IncludeTrailingPathDelimiter(
        ExpandFileName(InstallationBaseDirectory));

    MsysBaseDirectory := InstallationBaseDirectory + MSYS_BASE_DIRECTORY;
    ConfigurationDirectory := MsysBaseDirectory + SETTINGS_DIRECTORY;

    if not DirectoryExists(MsysBaseDirectory) then
    begin
{$IFDEF RELEASE}
{$IFDEF GUI}
      MsgBoxDlg(0, 'DreamSDK Home directory is not found!', 'Fatal error', mtError, [mbOK]);
{$ENDIF}
{$ENDIF}
      raise EHomeDirectoryNotFound.Create('DreamSDK Home directory is not found!');
    end;
  end;
end;

function GetInstallationBaseDirectory: TFileName;
begin
  RetrieveBaseDirectories;
  Result := InstallationBaseDirectory;
end;

function GetMSysBaseDirectory: TFileName;
begin
  RetrieveBaseDirectories;
  Result := MsysBaseDirectory;
end;

function GetConfigurationDirectory: TFileName;
begin
  RetrieveBaseDirectories;
  Result := ConfigurationDirectory;
end;

initialization
  InstallationBaseDirectory := EmptyStr;
  MsysBaseDirectory := EmptyStr;
  ConfigurationDirectory := EmptyStr;

end.

