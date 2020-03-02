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
function IsDefinedInstallationBaseDirectoryVariable: Boolean;

implementation

uses
{$IFDEF GUI}
  Dialogs,
  MsgDlg,
{$ENDIF}
  SysTools,
  FSTools;

var
  InstallationBaseDirectory,
  MsysBaseDirectory,
  ConfigurationDirectory,
  CommandLineInstallationDirectory: TFileName;

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

function GetRawInstallationBaseDirectory: TFileName;
begin
  Result := GetEnvironmentVariable(GetBaseEnvironmentVariableName);
end;

function IsDefinedInstallationBaseDirectoryVariable: Boolean;
begin
  Result := not IsEmpty(GetRawInstallationBaseDirectory);
end;

procedure RetrieveBaseDirectories;
const
{$IFNDEF DISABLE_REFBASE_WARNING}
  MSYS_EXCEPTION_MESSAGE = 'DreamSDK Home directory is not found. ' +
    'InstallationBaseDirectory: "%s", ConfigurationDirectory: "%s", ' +
    'ConfigurationDirectory: "%s", CommandLineInstallationDirectory: "%s".';
{$ENDIF}
  MSYS_BASE_DIRECTORY = 'msys\1.0\';
  SETTINGS_DIRECTORY = 'etc\dreamsdk\';

{$IFNDEF DISABLE_REFBASE_WARNING}
var
  MsysExceptionMessage: string;
{$ENDIF}

begin
  if IsEmpty(InstallationBaseDirectory) then
  begin
{$IFDEF DEBUG}
    DebugLog('RetrieveBaseDirectories');
{$ENDIF}

    // Handling Installation Home Base Directory (i.e. X:\DreamSDK\)
    if DirectoryExists(CommandLineInstallationDirectory) then
    begin
      // Special mode: First Run
      InstallationBaseDirectory := CommandLineInstallationDirectory;
{$IFDEF DEBUG}
      DebugLog('  IsFirstRunMode: ' + InstallationBaseDirectory);
{$ENDIF}
    end
    else
    begin
      // Normal mode: Read from DREAMSDK_HOME environment variable
      InstallationBaseDirectory := GetRawInstallationBaseDirectory;
{$IFDEF DEBUG}
      DebugLog('  InstallationBaseDirectory from environment variable: ' + InstallationBaseDirectory);
{$ENDIF}

{$IFNDEF DISABLE_REFBASE_FAILSAFE}
      // Fail-safe: this points to the X:\DreamSDK\ directory
      if IsEmpty(InstallationBaseDirectory)
        or (not DirectoryExists(InstallationBaseDirectory)) then
          InstallationBaseDirectory := GetApplicationPath + '..\..\..\..\';
{$IFDEF DEBUG}
      DebugLog('  InstallationBaseDirectory fail-safe: ' + InstallationBaseDirectory);
{$ENDIF}
{$ENDIF}
    end; // InstallationBaseDirectory

    // If Home Base directory is not empty, then parse the path
    if not IsEmpty(InstallationBaseDirectory) then
      InstallationBaseDirectory := IncludeTrailingPathDelimiter(
        ExpandFileName(InstallationBaseDirectory));

{$IFDEF DEBUG}
      DebugLog('  InstallationBaseDirectory post-processing: ' + InstallationBaseDirectory);
{$ENDIF}

    if DirectoryExists(InstallationBaseDirectory) then
    begin
      // Compute MSYS Base directory (i.e. '/')
      MsysBaseDirectory := InstallationBaseDirectory + MSYS_BASE_DIRECTORY;
{$IFDEF DEBUG}
      DebugLog('  MsysBaseDirectory: ' + MsysBaseDirectory);
{$ENDIF}

      // Compute '/etc/dreamsdk' directory
      ConfigurationDirectory := MsysBaseDirectory + SETTINGS_DIRECTORY;
{$IFDEF DEBUG}
      DebugLog('  ConfigurationDirectory: ' + ConfigurationDirectory);
{$ENDIF}
    end;

{$IFNDEF DISABLE_REFBASE_WARNING}
    // If the MSYS Base directory doesn't exist, then there is a issue somewhere!
    if (not DirectoryExists(InstallationBaseDirectory))
      or (not DirectoryExists(MsysBaseDirectory))
      or (not DirectoryExists(ConfigurationDirectory)) then
    begin
      MsysExceptionMessage := Format(MSYS_EXCEPTION_MESSAGE, [
        InstallationBaseDirectory,
        MsysBaseDirectory,
        ConfigurationDirectory,
        CommandLineInstallationDirectory
      ]);

{$IFDEF GUI}
      MsgBoxDlg(0, sError, MsysExceptionMessage, mtError, [mbOK]);
{$ELSE}
      WriteLn(MsysExceptionMessage);
{$ENDIF}

{$IFDEF DEBUG}
      raise EHomeDirectoryNotFound.Create(MsysExceptionMessage);
{$ENDIF}
    end; // DirectoryExists MsysBaseDirectory
{$ENDIF}
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

{$IFNDEF DISABLE_REFBASE_HOME_DIR_OVERRIDE}
procedure ParseParameters;
const
  DIR_SWITCH = '--home-dir';

var
  i: Integer;
  Param: string;

begin
  for i := 1 to ParamCount do
  begin
    Param := LowerCase(ParamStr(i));
    if IsInString(DIR_SWITCH, Param) then
      CommandLineInstallationDirectory := ParamStr(i + 1);
  end;
end;
{$ENDIF}

initialization
  InstallationBaseDirectory := EmptyStr;
  MsysBaseDirectory := EmptyStr;
  ConfigurationDirectory := EmptyStr;
  CommandLineInstallationDirectory := EmptyStr;
{$IFNDEF DISABLE_REFBASE_HOME_DIR_OVERRIDE}
  ParseParameters;
{$ENDIF}

end.

