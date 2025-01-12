unit RefBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  EHomeDirectoryNotFound = class(Exception);

  TEnvironmentFoundationKind = (
    efkUndefined,
    efkMinGWMSYS,
    efkMinGW64MSYS2
  );

function DreamSdkPathToSystem(const UnixPathName: TFileName): TFileName;
function GetBaseEnvironmentFoundationKind: TEnvironmentFoundationKind;
function GetBaseEnvironmentVariableName: string;
function GetConfigurationDirectory: TFileName;
function GetConfigurationPartialPath: TFileName;
function GetInstallationBaseDirectory: TFileName;
function GetMSysBaseDirectory: TFileName;
function GetUserBinariesBaseDirectory: TFileName;
function GetWindowsToolchainBaseDirectory: TFileName;
function IsDefinedInstallationBaseDirectoryVariable: Boolean;
function SystemToDreamSdkPath(const SystemPathName: TFileName): TFileName;

implementation

uses
{$IFDEF DEBUG}
  TypInfo,
{$ENDIF}
{$IFDEF GUI}
  Interfaces,
  Dialogs,
  MsgDlg,
{$ENDIF}
  SysTools,
  FSTools;

const
  MSYS2_FLAVOUR = 'mingw64';
  MSYS_BASE_DIRECTORY = 'msys\1.0\';
  SETTINGS_DIRECTORY = 'etc\dreamsdk\';

var
  FoundationKind: TEnvironmentFoundationKind;
  InstallationBaseDirectory,
  MsysBaseDirectory,
  ConfigurationDirectory,
  WindowsToolchainBaseDirectory,
  UserBinariesBaseDirectory,
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
{$IFNDEF DISABLE_REFBASE_WARNING}
const
  MSYS_EXCEPTION_MESSAGE = 'DreamSDK Home directory is not found. ' +
    'InstallationBaseDirectory: "%s", ConfigurationDirectory: "%s", ' +
    'ConfigurationDirectory: "%s", CommandLineInstallationDirectory: "%s".';
{$ENDIF}  

  procedure DetectFoundationKind;
  begin
    FoundationKind := efkUndefined;
    if FileExists(InstallationBaseDirectory + 'usr\bin\pacman.exe') then
      FoundationKind := efkMinGW64MSYS2
    else if DirectoryExists(InstallationBaseDirectory + MSYS_BASE_DIRECTORY) then
      FoundationKind := efkMinGWMSYS;
{$IFDEF DEBUG}
    DebugLog('  Environment: ' + GetEnumName(TypeInfo(TEnvironmentFoundationKind), Ord(FoundationKind)));
{$ENDIF}
  end;

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
          InstallationBaseDirectory := Left(MSYS_BASE_DIRECTORY, GetApplicationPath);
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
      // Detect the foundation kind: if we are under MinGW/MSYS or MinGW-w64/MSYS2
      DetectFoundationKind;

      // Compute MSYS Base directory (i.e. '/')
      case FoundationKind of
        efkMinGWMSYS:
          MsysBaseDirectory := InstallationBaseDirectory + MSYS_BASE_DIRECTORY;
        efkMinGW64MSYS2:
          MsysBaseDirectory := InstallationBaseDirectory;
      end;
{$IFDEF DEBUG}
      DebugLog('  MsysBaseDirectory: ' + MsysBaseDirectory);
{$ENDIF}

      // Compute '/etc/dreamsdk' directory
      ConfigurationDirectory := MsysBaseDirectory + SETTINGS_DIRECTORY;
{$IFDEF DEBUG}
      DebugLog('  ConfigurationDirectory: ' + ConfigurationDirectory);
{$ENDIF}

      // Compute Windows toolchain directory
      WindowsToolchainBaseDirectory := InstallationBaseDirectory;
      if (FoundationKind = efkMinGW64MSYS2) then
        WindowsToolchainBaseDirectory := WindowsToolchainBaseDirectory + MSYS2_FLAVOUR + DirectorySeparator;
      WindowsToolchainBaseDirectory := WindowsToolchainBaseDirectory + 'bin' + DirectorySeparator;
{$IFDEF DEBUG}
      DebugLog('  WindowsToolchainBaseDirectory: ' + WindowsToolchainBaseDirectory);
{$ENDIF}

      // User binaries directory
      UserBinariesBaseDirectory := MsysBaseDirectory + 'bin';
      if (FoundationKind = efkMinGW64MSYS2) then
        UserBinariesBaseDirectory := MsysBaseDirectory + 'usr\bin';
      UserBinariesBaseDirectory := UserBinariesBaseDirectory + DirectorySeparator;
{$IFDEF DEBUG}
      DebugLog('  UserBinariesBaseDirectory: ' + UserBinariesBaseDirectory);
{$ENDIF}
    end; // InstallationBaseDirectory exists

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

function SystemToDreamSdkPath(const SystemPathName: TFileName): TFileName;
begin
  Result := StringReplace(SystemPathName, GetMSysBaseDirectory, EmptyStr, []);
  Result := SystemToUnixPath(Result);
end;

function DreamSdkPathToSystem(const UnixPathName: TFileName): TFileName;
begin
  Result := UnixPathToSystem(UnixPathName);
  if not DirectoryExists(Result) then
    Result := GetMSysBaseDirectory + Result;
end;

function GetConfigurationPartialPath: TFileName;
begin
  Result := SETTINGS_DIRECTORY;
  if GetBaseEnvironmentFoundationKind = efkMinGWMSYS then
    Result := MSYS_BASE_DIRECTORY + Result;
end;

function GetBaseEnvironmentFoundationKind: TEnvironmentFoundationKind;
begin
  RetrieveBaseDirectories;
  Result := FoundationKind;
end;

function GetWindowsToolchainBaseDirectory: TFileName;
begin
  RetrieveBaseDirectories;
  Result := WindowsToolchainBaseDirectory;
end;

function GetUserBinariesBaseDirectory: TFileName;
begin
  RetrieveBaseDirectories;
  Result := UserBinariesBaseDirectory;
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
  FoundationKind := Default(TEnvironmentFoundationKind);
  InstallationBaseDirectory := EmptyStr;
  MsysBaseDirectory := EmptyStr;
  ConfigurationDirectory := EmptyStr;
  CommandLineInstallationDirectory := EmptyStr;
  UserBinariesBaseDirectory := EmptyStr;
{$IFNDEF DISABLE_REFBASE_HOME_DIR_OVERRIDE}
  ParseParameters;
{$ENDIF}

end.

