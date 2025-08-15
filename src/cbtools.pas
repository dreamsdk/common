unit CBTools;
  
interface

uses
  Classes,
  SysUtils,
  FSTools,
  SysTools,
  CBVerInf;

const
  CODEBLOCKS_PATCHER_ERROR_TAG = 'Error: ';
  CODEBLOCKS_PATCHER_ERROR_SEPARATOR = '#||#';

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

implementation

uses
  Version;

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
  finally
    ConfigurationFileNames.Free;
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

end.
