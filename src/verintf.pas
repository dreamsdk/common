unit VerIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TModuleVersion = packed record
    FileDescription: string;
    FileVersion: string;
    BuildDateTime: string;
    ProductVersion: string;
  end;

const
  INVALID_VERSION = '(##INVALID##)';

function ExtractCurrentModuleVersion(UnknownValueIfEmpty: string): TModuleVersion;
function ExtractModuleVersion(const FileName: TFileName;
  UnknownValueIfEmpty: string): TModuleVersion;
function GetRegisteredVersion(const FileName: TFileName): string;
function IsGetModuleVersionCommand: Boolean;
function IsVersionValid(const Version: string): Boolean;
function LoadModuleVersion(const FileName: TFileName; const ProcessId: Integer): TModuleVersion;
procedure SaveModuleVersion; overload;
procedure SaveModuleVersion(const FileName: TFileName; const ProcessId: Integer); overload;
function RetrieveVersion(Executable, CommandLine, StartTag, EndTag: string): string; overload;
function RetrieveVersion(Executable, CommandLine, StartTag, EndTag: string;
  EnableRegister: Boolean): string; overload;
function RetrieveVersion(Executable, CommandLine, StartTag, EndTag: string;
  EnableRegister, UseShellRunner: Boolean): string; overload;
function RetrieveVersionWithFind(FindTargetFileName: TFileName;
  Tag: string): string; overload;
function RetrieveVersionWithFind(FindTargetFileName: TFileName;
  StartTag, EndTag: string): string; overload;
function RetrieveVersionWithFind(FindTargetFileName: TFileName;
  Tag: string; EnableRegister: Boolean): string; overload;
function RetrieveVersionWithFind(FindTargetFileName: TFileName;
  StartTag, EndTag: string; EnableRegister: Boolean): string; overload;
procedure SetRegisteredVersion(const FileName: TFileName; const Version: string);

implementation

uses
  IniFiles,
  RefBase,
  SysTools,
  Runner,
  RunTools,
  Version,
  FSTools;

const
  GREP_FILE_SYSTEM_LOCATION = MSYS_BASE_DIRECTORY + 'bin\grep.exe';
  GET_MODULE_VERSION_SWITCH = '--internal-get-module-version';
  VERSION_REGISTRY_FILE_SYSTEM_LOCATION = 'versions.conf';

var
  GrepFileName: TFileName;
  ComSpecFileName: TFileName;
  VersionInformationRegistry: TIniFile;
  ShellRunner: TDreamcastSoftwareDevelopmentKitRunner;

function GetRegisteredVersion(const FileName: TFileName): string;
begin
  Result := VersionInformationRegistry.ReadString('Versions',
    GetFileHash(FileName), EmptyStr);
end;

procedure SetRegisteredVersion(const FileName: TFileName; const Version: string);
begin
{$IFDEF DEBUG}
  WriteLn('SetRegisteredVersion: ', FileName, ': ', Version);
{$ENDIF}
  VersionInformationRegistry.WriteString('Versions',
    GetFileHash(FileName), Version);
end;

function IsGetModuleVersionCommand: Boolean;
var
  i: Integer;
  Param: string;

begin
  Result := False;
  for i := 1 to ParamCount do
  begin
    Param := ParamStr(i);
    if LowerCase(Param) = GET_MODULE_VERSION_SWITCH then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function GetDumpVersionFileName(const FileName: TFileName;
  ProcessId: Integer): TFileName;
var
  PathName: TFileName;

begin
  PathName := ExtractFilePath(FileName);
  Result := Format('%s.dump-version-%d.tmp', [PathName, ProcessId]);
end;

procedure SaveModuleVersion;
begin
  SaveModuleVersion(ParamStr(0), GetProcessID);
end;

procedure SaveModuleVersion(const FileName: TFileName;
  const ProcessId: Integer);
var
  StringList: TStringList;
  DumpFileName: TFileName;

begin
{$IFDEF DEBUG}
  DebugLog('SaveModuleVersion: ' + FileName + ', ProcessId: '
    + IntToStr(ProcessId));
{$ENDIF}
  DumpFileName := GetDumpVersionFileName(FileName, ProcessId);
{$IFDEF DEBUG}
  DebugLog('  DumpFileName: ' + DumpFileName);
{$ENDIF}
  StringList := TStringList.Create;
  try
    StringList.Add(GetFileDescription);
    StringList.Add(GetFileVersion);
    StringList.Add(FormatDateTime(STRING_DATE_FORMAT, GetCompiledDateTime));
    StringList.Add(GetProductVersion);
    StringList.SaveToFile(DumpFileName);
  finally
    StringList.Free;
  end;
{$IFDEF DEBUG}
  DebugLog('SaveModuleVersion is done!');
{$ENDIF}
end;

function LoadModuleVersion(const FileName: TFileName;
  const ProcessId: Integer): TModuleVersion;
var
  StringList: TStringList;
  DumpFileName: TFileName;

begin
  Result.FileDescription := '';
  Result.FileVersion := '';
  Result.BuildDateTime := '';
  Result.ProductVersion := '';
  DumpFileName := GetDumpVersionFileName(FileName, ProcessId);
  if FileExists(DumpFileName) then
  begin
    StringList := TStringList.Create;
    try
      StringList.LoadFromFile(DumpFileName);
      Result.FileDescription := StringList[0];
      Result.FileVersion := StringList[1];
      Result.BuildDateTime := StringList[2];
      Result.ProductVersion := StringList[3];
    finally
      StringList.Free;
    end;
    DeleteFile(DumpFileName);
  end;
end;

function IsVersionValid(const Version: string): Boolean;
begin
  Result := not IsEmpty(Version) and not IsInString(INVALID_VERSION, Version);
end;

function RetrieveVersion(Executable, CommandLine, StartTag,
  EndTag: string): string;
begin
  Result := RetrieveVersion(Executable, CommandLine, StartTag, EndTag,
    True, True);
end;

function RetrieveVersion(Executable, CommandLine, StartTag, EndTag: string;
  EnableRegister: Boolean): string; overload;
begin
  Result := RetrieveVersion(Executable, CommandLine, StartTag, EndTag,
    EnableRegister, True);
end;

function RetrieveVersion(Executable, CommandLine, StartTag,
  EndTag: string; EnableRegister, UseShellRunner: Boolean): string;
var
  Buffer: string;
  UseRegister: Boolean;
  ExecutableFileName: TFileName;
  FileLocations: TStringList;

begin
  Result := EmptyStr;
  Buffer := EmptyStr;
  UseRegister := False;

  if EnableRegister then
  begin
    // By default, executable is a physical filename
    ExecutableFileName := Executable;

    if not FileExists(ExecutableFileName) then
    begin
      // Try to find the executable passed in parameter in PATH
      FileLocations := TStringList.Create;
      try
        if GetFileLocationsInSystemPath(ExecutableFileName, FileLocations) then
          ExecutableFileName := FileLocations[0];
      finally
        FileLocations.Free;
      end;
    end;

    // We can register if executable is found
    UseRegister := FileExists(ExecutableFileName);

    // Get the version if we can
    if UseRegister then
      Result := GetRegisteredVersion(ExecutableFileName);
  end;

  if IsEmpty(Result) then
  begin
    try
      if UseShellRunner and ShellRunner.Healthy then
      begin
        // Execute the command from the Shell (bash)
        ShellRunner.StartShellCommand(
          Format('%s %s', [SystemToUnixPath(Executable), CommandLine]),
          Buffer
        );
      end
      else
        // Execute the command from the Windows Prompt (cmd)
        Buffer := Run(Executable, CommandLine);

      // Extract the version into StartTag/EndTag
      Result := Buffer;
      if not IsEmpty(EndTag) then
        Result := ExtractStr(StartTag, EndTag, Buffer);
      Result := Trim(Result);

      // If Result is nil/NULL, then it's an invalid version...
      if Pointer(Result) = nil then
        raise Exception.Create('Unable to retrieve version');

      // Save version if we can
      if UseRegister and (not IsEmpty(Result)) then
        SetRegisteredVersion(ExecutableFileName, Result);
    except
      Result := INVALID_VERSION;
    end;
  end;
end;

function RetrieveVersionWithFind(FindTargetFileName: TFileName;
  Tag: string): string; overload;
begin
  Result := RetrieveVersionWithFind(FindTargetFileName, Tag, EmptyStr, True);
end;

function RetrieveVersionWithFind(FindTargetFileName: TFileName;
  Tag: string; EnableRegister: Boolean): string; overload;
begin
  Result := RetrieveVersionWithFind(FindTargetFileName, Tag, EmptyStr, EnableRegister);
end;

function RetrieveVersionWithFind(FindTargetFileName: TFileName; StartTag,
  EndTag: string): string;
begin
  Result := RetrieveVersionWithFind(FindTargetFileName, StartTag, EndTag, True);
end;

function RetrieveVersionWithFind(FindTargetFileName: TFileName; StartTag,
  EndTag: string; EnableRegister: Boolean): string;
var
  CommandLine: string;

begin
  Result := EmptyStr;

  if EnableRegister then
    Result := GetRegisteredVersion(FindTargetFileName);

  if IsEmpty(Result) then
  begin
    if (not IsWindowsVistaOrGreater) and FileExists(GrepFileName) then
    begin
      // Windows XP
      CommandLine := Format('/c type "%s" | "%s" --text "%s" ', [FindTargetFileName,
        GrepFileName, StartTag]);
      Result := RetrieveVersion(ComSpecFileName, CommandLine, StartTag, EndTag, False, False);
    end
    else
    begin
      // Windows Vista+
      CommandLine := Format('"%s" %s', [StartTag, FindTargetFileName]);
      Result := RetrieveVersion('find', CommandLine, StartTag, EndTag, False, False);
    end;

    if EnableRegister and (not IsEmpty(Result)) then
      SetRegisteredVersion(FindTargetFileName, Result);
  end;

  if IsEmpty(Result) then
    Result := INVALID_VERSION;

{$IFDEF DEBUG}
  DebugLog(Format('RetrieveVersionWithFind: FileName="%s", Version="%s"', [
    FindTargetFileName, Result]))
{$ENDIF}
end;

function ProcessModuleVersion(ModuleVersion: TModuleVersion;
  UnknownValueIfEmpty: string): TModuleVersion;
begin
  if not SameText(UnknownValueIfEmpty, EmptyStr) then
  begin
    if SameText(ModuleVersion.BuildDateTime, EmptyStr) then
      ModuleVersion.BuildDateTime := UnknownValueIfEmpty;
    if SameText(ModuleVersion.FileDescription, EmptyStr) then
      ModuleVersion.FileDescription := UnknownValueIfEmpty;
    if SameText(ModuleVersion.FileVersion, EmptyStr) then
      ModuleVersion.FileVersion := UnknownValueIfEmpty;
    if SameText(ModuleVersion.ProductVersion, EmptyStr) then
      ModuleVersion.ProductVersion := UnknownValueIfEmpty;
  end;
  Result := ModuleVersion;
end;

function ExtractModuleVersion(const FileName: TFileName;
  UnknownValueIfEmpty: string): TModuleVersion;
var
  ModuleProcessId: Integer;
  ModuleVersion: TModuleVersion;

begin
  ModuleVersion.BuildDateTime := EmptyStr;
  ModuleVersion.FileDescription := EmptyStr;
  ModuleVersion.FileVersion := EmptyStr;
  ModuleVersion.ProductVersion := EmptyStr;
  if FileExists(FileName) then
  begin
    ModuleProcessId := 0;
    Run(FileName, GET_MODULE_VERSION_SWITCH, ModuleProcessId);
    ModuleVersion := LoadModuleVersion(FileName, ModuleProcessId);
  end;
  Result := ProcessModuleVersion(ModuleVersion, UnknownValueIfEmpty);
end;

function ExtractCurrentModuleVersion(UnknownValueIfEmpty: string): TModuleVersion;
var
  ModuleVersion: TModuleVersion;

begin
  ModuleVersion.FileDescription := GetFileDescription;
  ModuleVersion.FileVersion := GetFileVersion;
  ModuleVersion.ProductVersion := GetProductVersion;
  ModuleVersion.BuildDateTime := FormatDateTime(STRING_DATE_FORMAT, GetCompiledDateTime);
  Result := ProcessModuleVersion(ModuleVersion, UnknownValueIfEmpty);
end;

initialization
  ShellRunner := TDreamcastSoftwareDevelopmentKitRunner.Create(True);
  GrepFileName := GetInstallationBaseDirectory + GREP_FILE_SYSTEM_LOCATION;
  ComSpecFileName := GetEnvironmentVariable('COMSPEC');
  if DirectoryExists(GetConfigurationDirectory) then
    VersionInformationRegistry := TIniFile.Create(
      GetConfigurationDirectory + VERSION_REGISTRY_FILE_SYSTEM_LOCATION);

finalization
  if Assigned(ShellRunner) then
    ShellRunner.Free;
  if Assigned(VersionInformationRegistry) then
    VersionInformationRegistry.Free;

end.

