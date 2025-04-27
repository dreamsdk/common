(*
================================================================================
  ____                         _____  ____   _____
 |    .  ___  ___  ___  _____ |   __||    . |  |  |
 |  |  ||  _|| -_|| .'||     ||__   ||  |  ||    -|
 |____/ |_|  |___||__,||_|_|_||_____||____/ |__|__|

================================================================================
DreamSDK Common Library - Reference Base Directories
================================================================================

This file is part of DreamSDK.

DreamSDK is free software: you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

DreamSDK is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
DreamSDK. If not, see <https://www.gnu.org/licenses/>.
*)
unit RefBase;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  EHomeDirectoryNotFound = class(Exception);

  (* Foundation type where the user is executing this install of DreamSDK *)
  TEnvironmentFoundationKind = (
    (* Not identified *)
    efkUndefined,
    (* Environment is legacy MinGW/MSYS, previously known as MinGW.org *)
    efkMinGWMSYS,
    (* Environment is modern MinGW-w64/MSYS2 *)
    efkMinGW64MSYS2
  );

function DreamSdkPathToSystem(const UnixPathName: TFileName): TFileName;

(* Indicates if this DreamSDK installation is running under MinGW/MSYS or
   MinGW-w64/MSYS2. This has a huge impact on everything as the capabilities
   and paths aren't the same between the two environments. *)
function GetBaseEnvironmentFoundationKind: TEnvironmentFoundationKind;

(* Get the environment variable name where this install of DreamSDK will try to
   read, typically DREAMSDK_HOME *)
function GetBaseEnvironmentVariableName: string;

(* Get the location of the DreamSDK configuration directory, "/etc/dreamsdk",
   in Windows physical path. Typically "C:\DreamSDK\msys\1.0\etc\dreamsdk\" on
   MinGW/MSYS or "C:\DreamSDK\etc\dreamsdk\" on MinGW-w64/MSYS2.
   Note: The trailing path delimiter is already included. *)
function GetConfigurationDirectory: TFileName;

(* Companion function of GetConfigurationDirectory(), just returns the partial
   path information, typically "msys\1.0\etc\dreamsdk\" on MinGW/MSYS or just
   "etc\dreamsdk\" on MinGW-w64/MSYS2. *)
function GetConfigurationPartialPath: TFileName;

(* The most important directory path, the installation path. This path is used
   everywhere, for all other computed paths. Typical value: "C:\DreamSDK\".
   Usually, this directory is retrieved from DREAMSDK_HOME env variable.
   Note: The trailing path delimiter is already included. *)
function GetBaseInstallationHomeDirectory: TFileName;

(* Returns the full path to the MSYS root directory.
   Usually "C:\DreamSDK\msys\1.0\" on MinGW/MSYS and "C:\DreamSDK\" on
   MinGW-w64/MSYS2.
   Note: The trailing path delimiter is already included. *)
function GetMSysBaseDirectory: TFileName;

(* Returns the full path of the user binaries, typically on MinGW/MSYS:
   "C:\DreamSDK\msys\1.0\bin\", on MinGW-w64/MSYS2: "C:\DreamSDK\usr\bin\".
   Note: The trailing path delimiter is already included. *)
function GetUserBinariesBaseDirectory: TFileName;

(* Get the path where the host toolchain (i.e., GCC) is located.
   Typically on MinGW/MSYS: "C:\DreamSDK\bin\" and "C:\DreamSDK\mingw64\bin\" on
   MinGW-w64/MSYS2.
   Note: The trailing path delimiter is already included. *)
function GetWindowsToolchainBaseDirectory: TFileName;

(* Indicate if the home directory path has been overloaded by using the
   --home-dir command-line switch. *)
function IsBaseInstallationHomeDirectoryOverloaded: Boolean;

(* Indicate if DREAMSDK_HOME (or DREAMSDK_HOME_DEBUG) is not empty. *)
function IsDefinedInstallationBaseDirectoryVariable: Boolean;

{$IFDEF DISABLE_REFBASE_HOME_DIR_AUTO_OVERRIDE}
(* Overload installation (home) directory manually. *)
procedure SetBaseInstallationHomeDirectory(const HomeDirectory: TFileName);
{$ENDIF}

function SystemToDreamSdkPath(const SystemPathName: TFileName): TFileName;

implementation

uses
  TypInfo,
{$IFDEF GUI}
  Interfaces,
  Dialogs,
  MsgDlg,
{$ENDIF}
  SysTools,
  StrTools,
  FSTools;

const
  DIR_SWITCH = 'home-dir';
  MSYS2_FLAVOUR = 'mingw64';
  MSYS_BASE_DIRECTORY = 'msys\1.0\';
  SETTINGS_DIRECTORY = 'etc\dreamsdk\';

var
{$IFDEF GUI}
  ErrorMessageDisplayed: Boolean = False;
{$ENDIF}
  FoundationKind: TEnvironmentFoundationKind = efkUndefined;
  InstallationBaseDirectory: TFileName = '';
  MsysBaseDirectory: TFileName = '';
  ConfigurationDirectory: TFileName = '';
  WindowsToolchainBaseDirectory: TFileName = '';
  UserBinariesBaseDirectory: TFileName = '';
  CommandLineInstallationDirectory: TFileName = '';

{$IFNDEF DISABLE_REFBASE_WARNING}
resourcestring
  MSYS_EXCEPTION_MESSAGE_COMMAND_LINE_INSTALLATION_DIRECTORY = 'CommandLineInstallationDirectory: "%s"' + sLineBreak;
  MSYS_EXCEPTION_MESSAGE = 'Critical Exception: ' +
    'DreamSDK Home directory was not found. Cannot continue.' + sLineBreak +
    sLineBreak +
    '---' + sLineBreak +
    'FoundationKind: %s' + sLineBreak +
    'BaseEnvironmentVariableName: %s' + sLineBreak +
    'InstallationBaseDirectory: "%s"' + sLineBreak +
    'MsysBaseDirectory: "%s"' + sLineBreak +
    'ConfigurationDirectory: "%s"' + sLineBreak +
    '%s' +
    '---';
{$ENDIF}

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

procedure DetectFoundationKind;
begin
  FoundationKind := efkUndefined;
  if DirectoryExists(InstallationBaseDirectory) then
  begin
    if FileExists(InstallationBaseDirectory + 'usr\bin\pacman.exe') then
      FoundationKind := efkMinGW64MSYS2
    else if DirectoryExists(InstallationBaseDirectory + MSYS_BASE_DIRECTORY) then
      FoundationKind := efkMinGWMSYS;
  end;
end;

procedure RetrieveBaseDirectories;
{$IFNDEF DISABLE_REFBASE_WARNING}
var
  MsysExceptionMessage,
  MsysExceptionMessageCommandLineInstallationDirectory: string;
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
      // Fail-safe: this points to the "X:\DreamSDK\" directory
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

    (* Starting from this point, InstallationBaseDirectory is usable, and should
       something like "C:\DreamSDK\". *)


{$IFDEF DEBUG}
      DebugLog('  InstallationBaseDirectory post-processing: ' + InstallationBaseDirectory);
{$ENDIF}

    if DirectoryExists(InstallationBaseDirectory) then
    begin
      // Detect the foundation kind: if we are under MinGW/MSYS or MinGW-w64/MSYS2
      DetectFoundationKind;

{$IFDEF DEBUG}
      DebugLog(Format('  Environment: %s', [
        GetEnumName(TypeInfo(TEnvironmentFoundationKind), Ord(FoundationKind))
      ]));
{$ENDIF}

      // Compute MSYS Base directory (i.e. '/' in MSYS)
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
      MsysExceptionMessageCommandLineInstallationDirectory := EmptyStr;
      if not IsEmpty(CommandLineInstallationDirectory) then
        MsysExceptionMessageCommandLineInstallationDirectory := Format(
          MSYS_EXCEPTION_MESSAGE_COMMAND_LINE_INSTALLATION_DIRECTORY, [
            CommandLineInstallationDirectory
          ]);

      MsysExceptionMessage := Format(MSYS_EXCEPTION_MESSAGE, [
        GetEnumName(TypeInfo(TEnvironmentFoundationKind), Ord(FoundationKind)),
        GetBaseEnvironmentVariableName,
        InstallationBaseDirectory,
        MsysBaseDirectory,
        ConfigurationDirectory,
        MsysExceptionMessageCommandLineInstallationDirectory
      ]);

{$IFDEF GUI}
      if not ErrorMessageDisplayed then
      begin
        MsgBoxDlg(0, PChar(Format('%s (%s)', [sError, ApplicationName])),
          MsysExceptionMessage, mtError, [mbOK]);
{$IFDEF RELEASE}
        Halt(255);
{$ENDIF}
        ErrorMessageDisplayed := True;
      end;
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

function GetBaseInstallationHomeDirectory: TFileName;
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

function IsBaseInstallationHomeDirectoryOverloaded: Boolean;
begin
  Result := not IsEmpty(CommandLineInstallationDirectory);
end;

{$IFNDEF DISABLE_REFBASE_HOME_DIR_AUTO_OVERRIDE}

procedure ParseParameters;
var
  LogContext: TLogMessageContext;
  i: Integer;
  Param: string;

begin
  LogContext := LogMessageEnter({$I %FILE%}, {$I %CURRENTROUTINE%});
  try

    for i := 1 to ParamCount do
    begin
      Param := LowerCase(ParamStr(i));
      LogMessage(LogContext, Format('Parsing parameter: "%s"', [Param]));
      if IsInString(DIR_SWITCH, Param) then
      begin
        CommandLineInstallationDirectory := ParamStr(i + 1);
        LogMessage(LogContext, Format('Parameter "%s" parsed successfully. Value: "%s"', [
          Param,
          CommandLineInstallationDirectory
        ]));
      end;
    end;

  finally
    LogMessageExit(LogContext);
  end;
end;

{$ELSE}

procedure SetBaseInstallationHomeDirectory(const HomeDirectory: TFileName);
begin
  CommandLineInstallationDirectory :=
    ExcludeTrailingPathDelimiter(ParseInputFileSystemObject(HomeDirectory));
end;

{$ENDIF}

initialization
{$IFNDEF DISABLE_REFBASE_HOME_DIR_AUTO_OVERRIDE}
  ParseParameters;
{$ENDIF}

end.

