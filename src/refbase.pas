unit RefBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  EHomeDirectoryNotFound = class(Exception);

function GetBaseEnvironmentVariableName: string;
function GetInstallationBaseDirectory: TFileName;
function GetMSysBaseDirectory: TFileName;

implementation

uses
  FirstRun, FSTools;

function GetBaseEnvironmentVariableName: string;
const
  DREAMSDK_ENVIRONMENT_VARIABLE = {$IFDEF RELEASE}'DREAMSDK_HOME'{$ELSE}'DREAMSDK_HOME_DEBUG'{$ENDIF};

begin
  Result := DREAMSDK_ENVIRONMENT_VARIABLE;
end;

function GetInstallationBaseDirectory: TFileName;
begin
  Result := GetEnvironmentVariable(GetBaseEnvironmentVariableName);
  if not DirectoryExists(Result) then
    Result := GetApplicationPath + '..\..\'; // Fail-safe

  if IsFirstRunMode then
    Result := GetFirstRunInstallationDirectory;

  if Result <> EmptyStr then
    Result := IncludeTrailingPathDelimiter(Result);

  if not DirectoryExists(Result) then
    raise EHomeDirectoryNotFound.Create('DreamSDK Home directory not found!');
end;

function GetMSysBaseDirectory: TFileName;
begin
  Result := GetInstallationBaseDirectory + 'msys\1.0\';
end;

end.

