unit CBVerInf;

interface

uses
  Classes,
  SysUtils,
  FSTools;

type
  TCodeBlocksVersion = (
    cbvUndefined,   // C::B is not installed/detected
    cbvUnknown,     // C::B is installed but version is unknown
    cbv1712,        // C::B 17.12 (x86 only)
    cbv2003x86,     // C::B 20.03 (x86)
    cbv2003x64,     // C::B 20.03 (x64)
    cbv2503x86,     // C::B 25.03 (x86)
    cbv2503x64      // C::B 25.03 (x64)
  );

const
  CODEBLOCKS_MAX_SUPPORTED_VERSION = 5;

function GetCodeBlocksVersion(InstallationDirectory: TFileName;
  const ExpandInstallationDirectory: Boolean = True): TCodeBlocksVersion;

function GetCodeBlocksSupportedVersions: TStringArray;

function CodeBlocksVersionToString(const CodeBlocksVersion: TCodeBlocksVersion): string;

function IsCodeBlocksFilesPatched(InstallationDirectory: TFileName): Boolean;

{$IFDEF ENABLE_CBTOOLS_SAVE_CB_VERSION}
procedure DeleteCodeBlocksVersionFileFromInstallationDirectory(
  InstallationDirectory: TFileName);
procedure SaveCodeBlocksVersionToInstallationDirectory(
  const CodeBlocksVersion: TCodeBlocksVersion; InstallationDirectory: TFileName);
{$ENDIF}

implementation

uses
  MD5,
  SysTools,
  Version;

type
  TCodeBlocksSupportedVersion = record
    MD5HashString: string;
    Version: TCodeBlocksVersion;
  end;

const
  CODEBLOCKS_ORIGINAL_CHECKER_FILE = 'codeblocks.dll';
  CODEBLOCKS_SUPPORTED_HASHES: array [0..CODEBLOCKS_MAX_SUPPORTED_VERSION - 1] of TCodeBlocksSupportedVersion = (
    (
      MD5HashString: '1575beba73a3ea34465fad9f55fd098a';
      Version: cbv1712
    ),
    (
      MD5HashString: 'ccd554fe3c7b01f2dab8d08c3485bf2a';
      Version: cbv2003x86
    ),
    (
      MD5HashString: '998040f792a0c36c85490b384ae1d3f0';
      Version: cbv2003x64
    ),
    (
      MD5HashString: '9e133d4fe0f436f15a7154340ff89e34';
      Version: cbv2503x86
    ),
    (
      MD5HashString: '40e98457e3540c9613308ce8b5439d68';
      Version: cbv2503x64
    )
  );

function CodeBlocksVersionToString(const CodeBlocksVersion: TCodeBlocksVersion): string;
begin
  Result := EmptyStr;
  case CodeBlocksVersion of
    cbvUndefined:
      Result := '(Undefined)';
    cbvUnknown:
      Result := '(Unknown)';
    cbv1712:
      Result := '17.12';
    cbv2003x86:
      Result := '20.03 (x86)';
    cbv2003x64:
      Result := '20.03 (x64)';
    cbv2503x86:
      Result := '25.03 (x86)';
    cbv2503x64:
      Result := '25.03 (x64)';
  end;
end;

function GetCodeBlocksSupportedVersions: TStringArray;
begin
  Result := Default(TStringArray);
  SetLength(Result, 3);
  Result[0] := '17.12';
  Result[1] := '20.03';
  Result[2] := '25.03';
end;

function GetCodeBlocksPatchedVersionFileName(InstallationDirectory: TFileName): TFileName;
const
  CODEBLOCKS_VERSION_FILE = 'dreamsdk.bin';

begin
  Result := IncludeTrailingPathDelimiter(InstallationDirectory)
    + CODEBLOCKS_VERSION_FILE;
end;

function IsCodeBlocksFilesPatched(InstallationDirectory: TFileName): Boolean;
var
  PatchedVersionFileName: TFileName;

begin
  PatchedVersionFileName := GetCodeBlocksPatchedVersionFileName(InstallationDirectory);
  Result := FileExists(PatchedVersionFileName);
end;

function GetCodeBlocksVersion(InstallationDirectory: TFileName;
  const ExpandInstallationDirectory: Boolean = True): TCodeBlocksVersion;
var
  i: Integer;
  CheckerFileName: TFileName;
  Buffer,
  CheckerFileHash: string;
  FileInfo: TCodeBlocksSupportedVersion;
  PatchedVersionFileName: TFileName;

begin
  Result := cbvUndefined; // C::B is not installed/detected

  if ExpandInstallationDirectory then
    InstallationDirectory := ParseInputFileSystemObject(InstallationDirectory);

  // Check if this C::B install has already been patched
  PatchedVersionFileName := GetCodeBlocksPatchedVersionFileName(InstallationDirectory);
  if FileExists(PatchedVersionFileName) then
  begin
    Buffer := LoadFileToString(PatchedVersionFileName);
    try
      // In that case, C::B has been patched so we read the version from a special file
      Result := TCodeBlocksVersion(StrToInt(Buffer));
    except
      // Silent exception if not possible to parse the file...
      Result := cbvUndefined; // Still undefined
    end;
  end;

  // Try to determine the C::B version by parsing the hash of "codeblocks.dll"
  if (Result = cbvUndefined) then
  begin
    CheckerFileName := IncludeTrailingPathDelimiter(InstallationDirectory)
      + CODEBLOCKS_ORIGINAL_CHECKER_FILE;

    if FileExists(CheckerFileName) then
    begin
      Result := cbvUnknown; // C::B is installed but version is unknown (atm)
      CheckerFileHash := LowerCase(MD5Print(MD5File(CheckerFileName)));
      i := Low(CODEBLOCKS_SUPPORTED_HASHES);
      while (Result = cbvUnknown) and (i <= High(CODEBLOCKS_SUPPORTED_HASHES)) do
      begin
        FileInfo := CODEBLOCKS_SUPPORTED_HASHES[i];
        if (FileInfo.MD5HashString = CheckerFileHash) then
          Result := FileInfo.Version; // we found the C::B version and it's supported!
        Inc(i);
      end;
    end;
  end;
end;

{$IFDEF ENABLE_CBTOOLS_SAVE_CB_VERSION}

procedure SaveCodeBlocksVersionToInstallationDirectory(
  const CodeBlocksVersion: TCodeBlocksVersion; InstallationDirectory: TFileName);
var
  PatchedVersionFileName: TFileName;

begin
  PatchedVersionFileName := GetCodeBlocksPatchedVersionFileName(InstallationDirectory);
  SaveStringToFile(IntToStr(Integer(CodeBlocksVersion)), PatchedVersionFileName);
end;

procedure DeleteCodeBlocksVersionFileFromInstallationDirectory(
  InstallationDirectory: TFileName);
var
  PatchedVersionFileName: TFileName;

begin
  PatchedVersionFileName := GetCodeBlocksPatchedVersionFileName(InstallationDirectory);
  KillFile(PatchedVersionFileName);
end;

{$ENDIF}

end.

