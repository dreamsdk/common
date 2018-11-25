unit VerIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  GET_MODULE_VERSION_SWITCH = '--internal-get-module-version';

type
  TModuleVersion = packed record
    FileDescription: string;
    FileVersion: string;
    BuildDateTime: string;
    ProductVersion: string;
  end;

const
  INVALID_VERSION = '(##INVALID##)';

function LoadModuleVersion(const FileName: TFileName; const ProcessId: Integer): TModuleVersion;
procedure SaveModuleVersion(const FileName: TFileName; const ProcessId: Integer);
function RetrieveVersion(Executable, CommandLine, StartTag, EndTag: string): string;
function RetrieveVersionWithFind(FindTargetFileName: TFileName; StartTag, EndTag: string): string;
function IsVersionValid(const Version: string): Boolean;

implementation

uses
  SysTools, Version;

var
  GrepFileName: TFileName;
  ComSpecFileName: TFileName;

function GetDumpVersionFileName(const FileName: TFileName;
  ProcessId: Integer): TFileName;
var
  PathName: TFileName;

begin
  PathName := ExtractFilePath(FileName);
  Result := Format('%s.dump-version-%d.tmp', [PathName, ProcessId]);
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

initialization
  GrepFileName := IncludeTrailingPathDelimiter(GetEnvironmentVariable('DREAMSDK_HOME'))
    + 'msys\1.0\bin\grep.exe';
  ComSpecFileName := GetEnvironmentVariable('COMSPEC');

end.

