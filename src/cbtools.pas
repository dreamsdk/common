unit CBTools;
  
interface

uses
  Classes, SysUtils, FSTools;

type
  TCodeBlocksVersion = (cbvUndefined, cbv1712, cbv2003x86, cbv2003x64);

function GetCodeBlocksVersion(InstallationDirectory: TFileName;
  const ExpandInstallationDirectory: Boolean = True): TCodeBlocksVersion;
function CodeBlocksVersionToString(const CodeBlocksVersion: TCodeBlocksVersion): string;
procedure ConvertCodeBlocksConfigurationFileNamesToUsers(
  ConfigurationFileNames: TFileList; AvailableUsers: TStringList);
procedure GetCodeBlocksAvailableConfigurationFileNames(
  ConfigurationFileNames: TFileList);
procedure GetCodeBlocksAvailableUsers(AvailableUsers: TStringList);
function GetCodeBlocksDefaultInstallationDirectory: TFileName;

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
  CODEBLOCKS_ORIGINAL_FILE_CHECKER = 'codeblocks.dll';
  CODEBLOCKS_SUPPORTED_HASHES: array [0..2] of TCodeBlocksSupportedVersion = (
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
    )
);

function CodeBlocksVersionToString(const CodeBlocksVersion: TCodeBlocksVersion): string;
begin
  Result := EmptyStr;
  case CodeBlocksVersion of
    cbvUndefined:
      Result := '(Unknown)';
    cbv1712:
      Result := '17.12';
    cbv2003x86:
      Result := '20.03 (x86)';
    cbv2003x64:
      Result := '20.03 (x64)';
  end;
end;

function GetCodeBlocksDefaultInstallationDirectory: TFileName;
const
  DEFAULT_CODEBLOCKS_DIR_64 = '%ProgramFiles(x86)%\CodeBlocks';
  DEFAULT_CODEBLOCKS_DIR_32 = '%ProgramFiles%\CodeBlocks';

var
  IsDirectory32Exist,
  IsDirectory64Exist: Boolean;

begin
  IsDirectory32Exist := DirectoryExists(ParseInputFileSystemObject(DEFAULT_CODEBLOCKS_DIR_32));
  IsDirectory64Exist := DirectoryExists(ParseInputFileSystemObject(DEFAULT_CODEBLOCKS_DIR_64));

  Result := DEFAULT_CODEBLOCKS_DIR_32;
  if IsWindows64 then
  begin
    Result := DEFAULT_CODEBLOCKS_DIR_64;
    // If for some reason, the user have a 64-bit OS but installed the C::B 32-bit release
    if not IsDirectory64Exist and IsDirectory32Exist then
      Result := DEFAULT_CODEBLOCKS_DIR_32;
  end;
end;

procedure GetCodeBlocksAvailableConfigurationFileNames(
  ConfigurationFileNames: TFileList);
const
  DEFAULT_CODEBLOCKS_CONFIGURATION_FILE = '%sCodeBlocks\default.conf';

var
  UsersAppData: TStringList;
  i: Integer;
  CodeBlocksConfigurationFileName: TFileName;

begin
  UsersAppData := TStringList.Create;
  try
    GetAppDataListFromUsers(UsersAppData);

    for i := 0 to UsersAppData.Count - 1 do
    begin
      CodeBlocksConfigurationFileName := Format(
        DEFAULT_CODEBLOCKS_CONFIGURATION_FILE,
        [ IncludeTrailingPathDelimiter(UsersAppData[i]) ]
      );
{$IFDEF DEBUG}
      Write(CodeBlocksConfigurationFileName, ' ... ');
{$ENDIF}
      if FileExists(CodeBlocksConfigurationFileName) then
      begin
{$IFDEF DEBUG}
        WriteLn('exist!');
{$ENDIF}
        ConfigurationFileNames.Add(CodeBlocksConfigurationFileName);
      end
{$IFDEF DEBUG}
      else
        WriteLn('doesn''t exist!')
{$ENDIF};
    end;

  finally
    UsersAppData.Free;
  end;
end;

procedure ConvertCodeBlocksConfigurationFileNamesToUsers(
  ConfigurationFileNames: TFileList; AvailableUsers: TStringList);
var
  i: Integer;
  UsersDirectory: TFileName;
  CurrentUserName: string;

begin
  if Assigned(ConfigurationFileNames) and Assigned(AvailableUsers) then
  begin
    AvailableUsers.Clear;
    UsersDirectory := GetUsersDirectory;
    for i := 0 to ConfigurationFileNames.Count - 1 do
    begin
      CurrentUserName := ExtractStr(UsersDirectory, DirectorySeparator,
	    ConfigurationFileNames[i]);
      AvailableUsers.Add(GetFriendlyUserName(CurrentUserName));
    end;
  end;
end;

procedure GetCodeBlocksAvailableUsers(AvailableUsers: TStringList);
var
  Buffer: TFileList;

begin
  Buffer := TFileList.Create;
  try
    GetCodeBlocksAvailableConfigurationFileNames(Buffer);
    ConvertCodeBlocksConfigurationFileNamesToUsers(Buffer, AvailableUsers);
  finally
    Buffer.Free;
  end;
end;

function GetCodeBlocksVersion(InstallationDirectory: TFileName;
  const ExpandInstallationDirectory: Boolean = True): TCodeBlocksVersion;
var
  i: Integer;
  CheckerFileName: TFileName;
  CheckerFileHash: string;
  FileInfo: TCodeBlocksSupportedVersion;

begin
  Result := cbvUndefined;

  if ExpandInstallationDirectory then
    InstallationDirectory := ParseInputFileSystemObject(InstallationDirectory);

  CheckerFileName := IncludeTrailingPathDelimiter(InstallationDirectory)
    + CODEBLOCKS_ORIGINAL_FILE_CHECKER;

  if FileExists(CheckerFileName) then
  begin
    CheckerFileHash := LowerCase(MD5Print(MD5File(CheckerFileName)));
    i := Low(CODEBLOCKS_SUPPORTED_HASHES);
    while (Result = cbvUndefined) and (i <= High(CODEBLOCKS_SUPPORTED_HASHES)) do
    begin
      FileInfo := CODEBLOCKS_SUPPORTED_HASHES[i];
      if (FileInfo.MD5HashString = CheckerFileHash) then
        Result := FileInfo.Version;
      Inc(i);
    end;
  end;
end;

end.
