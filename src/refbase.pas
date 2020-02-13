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
  DREAMSDK_ENVIRONMENT_VARIABLE_RELEASE = 'DREAMSDK_HOME';
{$IFDEF DEBUG}
  DREAMSDK_ENVIRONMENT_VARIABLE_DEBUG = 'DREAMSDK_HOME_DEBUG';
{$ENDIF}

begin
  Result :=
{$IFDEF DEBUG}
    DREAMSDK_ENVIRONMENT_VARIABLE_DEBUG
{$ELSE}
    DREAMSDK_ENVIRONMENT_VARIABLE_RELEASE
{$ENDIF}
  ;

{$IFDEF DEBUG}
  if IsEmpty(GetEnvironmentVariable(Result)) then
    Result := DREAMSDK_ENVIRONMENT_VARIABLE_RELEASE;

  DebugLog('GetBaseEnvironmentVariableName: ' + Result);
{$ENDIF}
end;

procedure RetrieveBaseDirectories;
const
  MSYS_EXCEPTION_MESSAGE = 'DreamSDK Home directory is not found: "%s"';
  MSYS_BASE_DIRECTORY = 'msys\1.0\';
  SETTINGS_DIRECTORY = 'etc\dreamsdk\';

var
  MsysExceptionMessage: string;

begin
  if IsEmpty(InstallationBaseDirectory) then
  begin
    // Handling Installation Home Base Directory (i.e. X:\DreamSDK\)

    if IsFirstRunMode then
      // Special mode: First Run
      InstallationBaseDirectory := GetFirstRunInstallationDirectory
    else
    begin
      // Normal mode: Read from DREAMSDK_HOME environment variable
      InstallationBaseDirectory := GetEnvironmentVariable(GetBaseEnvironmentVariableName);
      if not IsEmpty(InstallationBaseDirectory)
        and (not DirectoryExists(InstallationBaseDirectory)) then
          InstallationBaseDirectory := GetApplicationPath + '..\..\..\..\'; // Fail-safe: this points to the X:\DreamSDK\ directory
    end;

    // If Home Base directory is not empty, then parse the path
    if not IsEmpty(InstallationBaseDirectory) then
      InstallationBaseDirectory := IncludeTrailingPathDelimiter(
        ExpandFileName(InstallationBaseDirectory));

    // Compute MSYS Base directory (i.e. '/')
    MsysBaseDirectory := InstallationBaseDirectory + MSYS_BASE_DIRECTORY;

    // Compute '/etc/dreamsdk' directory
    ConfigurationDirectory := MsysBaseDirectory + SETTINGS_DIRECTORY;

    // If the MSYS Base directory doesn't exist, then there is a issue somewhere!
    if not DirectoryExists(MsysBaseDirectory) then
    begin
      MsysExceptionMessage := Format(MSYS_EXCEPTION_MESSAGE, [MsysBaseDirectory]);
{$IFDEF RELEASE}
      // Release
{$IFDEF GUI}
      MsgBoxDlg(0, sError, MsysExceptionMessage, mtError, [mbOK]);
{$ELSE}
      WriteLn(MsysExceptionMessage);
{$ENDIF}
{$ELSE}
      // Debug
      raise EHomeDirectoryNotFound.Create(MsysExceptionMessage);
{$ENDIF}
    end; // DirectoryExists MsysBaseDirectory
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

