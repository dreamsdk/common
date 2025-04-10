unit CBTools;
  
interface

uses
  Classes,
  SysUtils,
  FSTools,
  SysTools;

const
  CODEBLOCKS_PATCHER_ERROR_TAG = 'Error: ';
  CODEBLOCKS_PATCHER_ERROR_SEPARATOR = '#||#';

type
  TCodeBlocksVersion = (
    cbvUndefined,   // C::B is not installed/detected
    cbvUnknown,     // C::B is installed but version is unknown
    cbv1712,        // C::B 17.12 (x86 only)
    cbv2003x86,     // C::B 20.03 (x86)
    cbv2003x64      // C::B 20.03 (x64)
  );

function GetCodeBlocksVersion(InstallationDirectory: TFileName;
  const ExpandInstallationDirectory: Boolean = True): TCodeBlocksVersion;

function CodeBlocksVersionToString(const CodeBlocksVersion: TCodeBlocksVersion): string;

(*procedure ConvertCodeBlocksConfigurationFileNamesToUsers(
  ConfigurationFileNames: TFileList; AvailableUsers: TStringList);*)

function GetCodeBlocksAvailableConfigurationFileNames(
  out AvailableUsers: TWindowsUserAccountInformationArray;
  ConfigurationFileNames: TFileList): Boolean;

function GetCodeBlocksConfigurationFileNames(
  out AvailableUsers: TWindowsUserAccountInformationArray;
  AvailableConfigurationFileNames: TFileList;
  out NotConfiguredUsers: TWindowsUserAccountInformationArray;
  MissingConfigurationFileNames: TFileList): Boolean;

function GetCodeBlocksAvailableUsers(
  out AvailableUsers: TWindowsUserAccountInformationArray): Boolean;

function GetCodeBlocksDefaultInstallationDirectory: TFileName;

procedure InitializeCodeBlocksProfiles;

{$IFDEF ENABLE_CBTOOLS_SAVE_CB_VERSION}
procedure DeleteCodeBlocksVersionFileFromInstallationDirectory(
  InstallationDirectory: TFileName);
procedure SaveCodeBlocksVersionToInstallationDirectory(
  const CodeBlocksVersion: TCodeBlocksVersion; InstallationDirectory: TFileName);
{$ENDIF}

implementation

uses
  MD5,
  Version;

type
  TCodeBlocksSupportedVersion = record
    MD5HashString: string;
    Version: TCodeBlocksVersion;
  end;

const
  CODEBLOCKS_VERSION_FILE = 'dreamsdk.bin';
  CODEBLOCKS_ORIGINAL_CHECKER_FILE = 'codeblocks.dll';
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
      Result := '(Undefined)';
    cbvUnknown:
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
  DEFAULT_CODEBLOCKS_DIR = '%ProgramFiles%\CodeBlocks';
  DEFAULT_CODEBLOCKS_DIR_64 = '%ProgramW6432%\CodeBlocks';
  DEFAULT_CODEBLOCKS_DIR_32_ON_64 = '%ProgramFiles(x86)%\CodeBlocks';

var
  Directory,
  Directory32On64: TFileName;
  IsDirectoryExist,
  IsDirectoryExist32On64: Boolean;

begin
  Directory := ParseInputFileSystemObject(DEFAULT_CODEBLOCKS_DIR);
  Directory32On64 := EmptyStr;
  if IsWindows64 then
  begin
    Directory := ParseInputFileSystemObject(DEFAULT_CODEBLOCKS_DIR_64);
    Directory32On64 := ParseInputFileSystemObject(DEFAULT_CODEBLOCKS_DIR_32_ON_64);
  end;

{$IFDEF DEBUG}
  WriteLn('Directory: ', Directory, ', Directory32On64: ', Directory32On64);
{$ENDIF}

  IsDirectoryExist := DirectoryExists(Directory);
  IsDirectoryExist32On64 := DirectoryExists(Directory32On64);

{$IFDEF DEBUG}
  WriteLn('IsDirectoryExist: ', IsDirectoryExist, ', IsDirectoryExist32On64: ', IsDirectoryExist32On64);
{$ENDIF}

  Result := DEFAULT_CODEBLOCKS_DIR;
  if IsWindows64 then
    Result := DEFAULT_CODEBLOCKS_DIR_64;
  // If for some reason, the user have a 64-bit OS but installed the C::B 32-bit release
  if (not IsDirectoryExist) and IsWindows64 and IsDirectoryExist32On64 then
    Result := DEFAULT_CODEBLOCKS_DIR_32_ON_64;
{$IFDEF DEBUG}
  WriteLn('> GetCodeBlocksDefaultInstallationDirectory: ', Result);
{$ENDIF}
end;

function GetCodeBlocksAvailableConfigurationFileNames(
  out AvailableUsers: TWindowsUserAccountInformationArray;
  ConfigurationFileNames: TFileList): Boolean;
var
  Dummy1: TWindowsUserAccountInformationArray;
  Dummy2: TFileList;

begin
{$IFDEF DEBUG}
  DebugLog('GetCodeBlocksAvailableConfigurationFileNames');
{$ENDIF}
  Dummy2 := TFileList.Create;
  try
    Result := GetCodeBlocksConfigurationFileNames(AvailableUsers,
      ConfigurationFileNames, Dummy1, Dummy2);
  finally
    Dummy2.Free;
  end;
end;

function GetCodeBlocksConfigurationFileNames(
  out AvailableUsers: TWindowsUserAccountInformationArray;
  AvailableConfigurationFileNames: TFileList;
  out NotConfiguredUsers: TWindowsUserAccountInformationArray;
  MissingConfigurationFileNames: TFileList
): Boolean;
const
  DEFAULT_CODEBLOCKS_CONFIGURATION_FILE = '%sCodeBlocks\default.conf';

var
  CurrentUser: TWindowsUserAccountInformation;
  UsersList: TWindowsUserAccountInformationArray;
  i,
  ConfiguredUsersMaxCount,
  NotConfiguredUsersMaxCount: Integer;
  CodeBlocksConfigurationFileName: TFileName;

begin
{$IFDEF DEBUG}
  DebugLog('GetCodeBlocksConfigurationFileNames');
{$ENDIF}

  Result := GetUserList(UsersList);

  // Initialize the size of the both arrays
  ConfiguredUsersMaxCount := 0;
  AvailableUsers := Default(TWindowsUserAccountInformationArray);
  SetLength(AvailableUsers, Length(UsersList));

  NotConfiguredUsersMaxCount := 0;
  NotConfiguredUsers := Default(TWindowsUserAccountInformationArray);
  SetLength(NotConfiguredUsers, Length(UsersList));

{$IFDEF DEBUG}
  DebugLog(Format('  Account Count: %d', [Length(UsersList)]));
  DebugLog('  Configuration File Names:');
{$ENDIF}

  for i := 0 to Length(UsersList) - 1 do
  begin
    CurrentUser := UsersList[i];

    CodeBlocksConfigurationFileName := Format(
      DEFAULT_CODEBLOCKS_CONFIGURATION_FILE, [
        IncludeTrailingPathDelimiter(CurrentUser.RoamingAppDataPath)
      ]);

{$IFDEF DEBUG}
    Write('    "', CodeBlocksConfigurationFileName, '" ... ');
{$ENDIF}

    if FileExists(CodeBlocksConfigurationFileName) then
    begin
{$IFDEF DEBUG}
      WriteLn('exist!');
{$ENDIF}
      AvailableConfigurationFileNames.Add(CodeBlocksConfigurationFileName);
      AvailableUsers[ConfiguredUsersMaxCount] := CurrentUser;
      Inc(ConfiguredUsersMaxCount);
    end
    else
    begin
{$IFDEF DEBUG}
      WriteLn('doesn''t exist!')
{$ENDIF};
      MissingConfigurationFileNames.Add(CodeBlocksConfigurationFileName);
      NotConfiguredUsers[NotConfiguredUsersMaxCount] := CurrentUser;
      Inc(NotConfiguredUsersMaxCount);
    end;
  end;

  SetLength(AvailableUsers, ConfiguredUsersMaxCount);
  SetLength(NotConfiguredUsers, NotConfiguredUsersMaxCount);
end;

(*
procedure ConvertCodeBlocksConfigurationFileNamesToUsers(
  ConfigurationFileNames: TFileList; AvailableUsers: TStringList);
var
  i: Integer;
  UsersDirectory: TFileName;
  CurrentUserName,
  FriendlyUserName: string;

begin
{$IFDEF DEBUG}
  DebugLog('ConvertCodeBlocksConfigurationFileNamesToUsers');
{$ENDIF}

  if Assigned(ConfigurationFileNames) and Assigned(AvailableUsers) then
  begin
{$IFDEF DEBUG}
    DebugLog(Format('  Configuration File Names Count: %d', [ConfigurationFileNames.Count]));
{$ENDIF}

    AvailableUsers.Clear;
    UsersDirectory := GetUsersRootDirectory;
    for i := 0 to ConfigurationFileNames.Count - 1 do
    begin
      CurrentUserName := ExtractStr(UsersDirectory, DirectorySeparator,
        ConfigurationFileNames[i]);
{$IFDEF DEBUG}
      DebugLog(Format('    [%d/%d] CurrentUserName: %s', [i,
        ConfigurationFileNames.Count, CurrentUserName]));
{$ENDIF}

      FriendlyUserName := GetFriendlyUserName(CurrentUserName);
{$IFDEF DEBUG}
      DebugLog(Format('    [%d/%d] FriendlyUserName: %s', [i,
        ConfigurationFileNames.Count, FriendlyUserName]));
{$ENDIF}

      AvailableUsers.Add(FriendlyUserName);
    end;
  end;
end;
*)

function GetCodeBlocksAvailableUsers(
  out AvailableUsers: TWindowsUserAccountInformationArray): Boolean;
var
  ConfigurationFileNames: TFileList;

begin
{$IFDEF DEBUG}
  DebugLog('GetCodeBlocksAvailableUsers');
{$ENDIF}

  ConfigurationFileNames := TFileList.Create;
  try
    Result := GetCodeBlocksAvailableConfigurationFileNames(
      AvailableUsers, ConfigurationFileNames);
    // ConvertCodeBlocksConfigurationFileNamesToUsers(Buffer, AvailableUsers);
  finally
    ConfigurationFileNames.Free;
  end;
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
  PatchedVersionFileName := IncludeTrailingPathDelimiter(InstallationDirectory)
    + CODEBLOCKS_VERSION_FILE;
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

procedure InitializeCodeBlocksProfiles;
var
  NotConfiguredUsers,
  Unused1: TWindowsUserAccountInformationArray;
  MissingConfigurationFileNames,
  Unused2: TFileList;
  i: Integer;
  MissingConfigurationFileName,
  MissingConfigurationPathName: TFileName;

begin
{$IFDEF DEBUG}
  WriteLn('InitializeProfiles');
{$ENDIF}
  MissingConfigurationFileNames := TFileList.Create;
  Unused2 := TFileList.Create;
  try
    GetCodeBlocksConfigurationFileNames(Unused1, Unused2, NotConfiguredUsers,
      MissingConfigurationFileNames);
    for i := 0 to MissingConfigurationFileNames.Count - 1 do
    begin
      MissingConfigurationFileName := MissingConfigurationFileNames[i];

{$IFDEF DEBUG}
      WriteLn('  + ', MissingConfigurationFileName);
{$ENDIF}

      MissingConfigurationPathName := ExtractFilePath(MissingConfigurationFileName);

      ForceDirectories(MissingConfigurationPathName);
      SaveStringToFile(
        '<?xml version="1.0" encoding="utf-8" standalone="yes" ?>' +
        '<CodeBlocksConfig version="1">' +
        '</CodeBlocksConfig>',
        MissingConfigurationFileName
      );
    end;
  finally
    Unused2.Free;
    MissingConfigurationFileNames.Free;
  end;
end;

{$IFDEF ENABLE_CBTOOLS_SAVE_CB_VERSION}

procedure SaveCodeBlocksVersionToInstallationDirectory(
  const CodeBlocksVersion: TCodeBlocksVersion; InstallationDirectory: TFileName);
var
  PatchedVersionFileName: TFileName;

begin
  PatchedVersionFileName := IncludeTrailingPathDelimiter(InstallationDirectory)
    + CODEBLOCKS_VERSION_FILE;
  SaveStringToFile(IntToStr(Integer(CodeBlocksVersion)), PatchedVersionFileName);
end;

procedure DeleteCodeBlocksVersionFileFromInstallationDirectory(
  InstallationDirectory: TFileName);
var
  PatchedVersionFileName: TFileName;

begin
  PatchedVersionFileName := IncludeTrailingPathDelimiter(InstallationDirectory)
    + CODEBLOCKS_VERSION_FILE;
  KillFile(PatchedVersionFileName);
end;

{$ENDIF}

end.
