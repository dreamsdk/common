unit CBTools;
  
interface

uses
  Classes, SysUtils, FSTools;

procedure ConvertCodeBlocksConfigurationFileNamesToUsers(
  ConfigurationFileNames: TFileList; AvailableUsers: TStringList);
procedure GetCodeBlocksAvailableConfigurationFileNames(
  ConfigurationFileNames: TFileList);
procedure GetCodeBlocksAvailableUsers(AvailableUsers: TStringList);

implementation

uses
  SysTools;

const
  DEFAULT_CODEBLOCKS_CONFIGURATION_FILE = '%sCodeBlocks\default.conf';

procedure GetCodeBlocksAvailableConfigurationFileNames(
  ConfigurationFileNames: TFileList);
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

end.
