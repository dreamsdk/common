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
function IsGetModuleVersionCommand: Boolean;
function LoadModuleVersion(const FileName: TFileName; const ProcessId: Integer): TModuleVersion;
procedure SaveModuleVersion; overload;
procedure SaveModuleVersion(const FileName: TFileName; const ProcessId: Integer); overload;
function RetrieveVersion(Executable, CommandLine, StartTag, EndTag: string): string;
function RetrieveVersionWithFind(FindTargetFileName: TFileName; StartTag, EndTag: string): string;
function IsVersionValid(const Version: string): Boolean;

implementation

uses
  SysTools, Version, FirstRun;

const
  GREP_FILE_SYSTEM_LOCATION = 'msys\1.0\bin\grep.exe';
  GET_MODULE_VERSION_SWITCH = '--internal-get-module-version';

var
  GrepFileName: TFileName;
  ComSpecFileName: TFileName;

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
  DumpFileName := GetDumpVersionFileName(FileName, ProcessId);
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
  Result := not IsInString(INVALID_VERSION, Version);
end;

function RetrieveVersion(Executable, CommandLine, StartTag,
  EndTag: string): string;
var
  Buffer: string;

begin
  try
    Buffer := Run(Executable, CommandLine);
    Result := Trim(ExtractStr(StartTag, EndTag, Buffer));
  except
    Result := INVALID_VERSION;
  end;
end;

function RetrieveVersionWithFind(FindTargetFileName: TFileName; StartTag,
  EndTag: string): string;
var
  CommandLine: string;

begin
  if FileExists(GrepFileName) then
  begin
    CommandLine := Format('/c type "%s" | "%s" --text "%s" ', [FindTargetFileName,
      GrepFileName, StartTag]);
    Result := RetrieveVersion('cmd', CommandLine, StartTag, EndTag);
  end
  else
  begin
    CommandLine := Format('"%s" %s', [StartTag, FindTargetFileName]);
    Result := RetrieveVersion('find', CommandLine, StartTag, EndTag);
  end;

  if (Result = '') then
    Result := INVALID_VERSION;
end;

function GetInstallationBaseDirectory: TFileName;
begin
  Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('DREAMSDK_HOME'));
  if IsFirstRunMode then
    Result := IncludeTrailingPathDelimiter(GetFirstRunInstallationDirectory);
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
  GrepFileName := GetInstallationBaseDirectory + GREP_FILE_SYSTEM_LOCATION;
  ComSpecFileName := GetEnvironmentVariable('COMSPEC');

end.

